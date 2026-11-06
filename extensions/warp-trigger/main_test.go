package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"net"
	"os"
	"reflect"
	"strings"
	"testing"
	"time"
)

func ptr(v float64) *float64 { return &v }
func speedEvent(before, after, ts any) event {
	return event{From: before, To: after, TS: ts, Data: map[string]any{"hash": "engine-ecu", "field": "speed"}}
}
func baseline() state { return state{Armed: true, BaselineSpeed: ptr(44), BaselineTS: ptr(100000)} }

func TestPolicy(t *testing.T) {
	s, play := decide(speedEvent("44", "44", 100000.), "44", "ready-to-drive", state{}, 100)
	if play || !s.Armed {
		t.Fatal("first observation must only seed")
	}
	s, play = decide(speedEvent("44", "50", 100200.), "50", "ready-to-drive", s, 100.2)
	if play {
		t.Fatal("exact threshold must not trigger")
	}
	s, play = decide(speedEvent("50", "50.1", 100400.), "50.1", "ready-to-drive", s, 100.4)
	if !play || s.Armed || s.LastTrigger != 100.4 {
		t.Fatal("upward crossing must play once")
	}
	_, play = decide(speedEvent("50.1", "52", 100600.), "52", "ready-to-drive", s, 100.6)
	if play {
		t.Fatal("must not replay above threshold")
	}
}

func TestInvalidObservationsClearBaseline(t *testing.T) {
	cases := []struct {
		name    string
		e       event
		current string
		now     float64
	}{
		{"stale", speedEvent("44", "51", 100100.), "51", 102},
		{"future", speedEvent("44", "51", 100300.), "51", 100},
		{"gap", speedEvent("44", "51", 102000.), "51", 102},
		{"out of order", speedEvent("44", "51", 99999.), "51", 100},
		{"predecessor mismatch", speedEvent("49", "51", 100100.), "51", 100.1},
		{"speed mismatch", speedEvent("44", "51", 100100.), "52", 100.1},
		{"invalid speed", speedEvent("44", "NaN", 100100.), "NaN", 100.1},
		{"missing timestamp", speedEvent("44", "51", nil), "51", 100.1},
		{"boolean timestamp", speedEvent("44", "51", true), "51", 100.1},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			s, play := decide(c.e, c.current, "ready-to-drive", baseline(), c.now)
			if play || s.Armed || s.BaselineTS != nil || s.BaselineSpeed != nil {
				t.Fatal(s, play)
			}
		})
	}
}

func TestRearmCooldownAndParked(t *testing.T) {
	for _, vehicle := range []string{"parked", "stand-by", ""} {
		_, play := decide(speedEvent("44", "51", 100100.), "51", vehicle, baseline(), 100.1)
		if play {
			t.Fatal("non-driving trigger")
		}
	}
	s := baseline()
	s.LastTrigger = 90
	s, play := decide(speedEvent("44", "51", 100100.), "51", "ready-to-drive", s, 100.1)
	if play || s.Armed || s.LastTrigger != 90 {
		t.Fatal("cooldown bypass")
	}
	s, play = decide(speedEvent("51", "45", 100200.), "45", "ready-to-drive", s, 100.2)
	if play || !s.Armed {
		t.Fatal("rearm at 45")
	}
	s, play = decide(speedEvent("45", "51", 100400.), "51", "ready-to-drive", s, 135.1)
	if play {
		t.Fatal("stale observation bypass")
	}
	s = baseline()
	s.LastTrigger = 55.0
	_, play = decide(speedEvent("44", "51", 100100.), "51", "ready-to-drive", s, 100.1)
	if !play {
		t.Fatal("45 second cooldown did not expire")
	}
}

func TestPythonParity(t *testing.T) {
	path := os.Getenv("WARP_POLICY_CASES")
	if path == "" {
		t.Skip("set WARP_POLICY_CASES for generated Python comparison")
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	var cases []struct {
		Event   event   `json:"event"`
		Current string  `json:"current"`
		Vehicle string  `json:"vehicle"`
		State   state   `json:"state"`
		Now     float64 `json:"now"`
		Want    state   `json:"want"`
		Play    bool    `json:"play"`
	}
	if err := json.Unmarshal(data, &cases); err != nil {
		t.Fatal(err)
	}
	for i, c := range cases {
		s, play := decide(c.Event, c.Current, c.Vehicle, c.State, c.Now)
		if !reflect.DeepEqual(s, c.Want) || play != c.Play {
			t.Fatalf("case %d: got %+v/%t want %+v/%t", i, s, play, c.Want, c.Play)
		}
	}
}

func TestReplies(t *testing.T) {
	for _, text := range []string{"$-2\r\n", "$65537\r\n", "$1\r\naXX", "-ERR bad\r\n", "!bad\r\n"} {
		if _, err := readReply(bufio.NewReader(strings.NewReader(text))); err == nil {
			t.Fatal("accepted", text)
		}
	}
	v, err := readReply(bufio.NewReader(strings.NewReader("$-1\r\n")))
	if err != nil || v != nil {
		t.Fatal(v, err)
	}
}

func TestWholeActionDeadline(t *testing.T) {
	server, client := net.Pipe()
	defer server.Close()
	defer client.Close()
	client.SetDeadline(time.Now().Add(30 * time.Millisecond))
	r := redisConn{conn: client, reader: bufio.NewReader(client)}
	start := time.Now()
	if _, err := r.command("PING"); err == nil {
		t.Fatal("missing deadline")
	}
	if time.Since(start) > time.Second {
		t.Fatal("blocked action")
	}
}

func TestHandlePersistsBeforePublish(t *testing.T) {
	server, client := net.Pipe()
	defer client.Close()
	r := redisConn{conn: client, reader: bufio.NewReader(client)}
	client.SetDeadline(time.Now().Add(time.Second))
	done := make(chan error, 1)
	now := float64(time.Now().UnixMilli()) / 1000
	saved := state{Armed: true, BaselineSpeed: ptr(44), BaselineTS: ptr(now*1000 - 200)}
	raw, _ := json.Marshal(saved)
	go func() {
		defer server.Close()
		rd := bufio.NewReader(server)
		expected := [][]string{{"HGET", "engine-ecu", "speed"}, {"HGET", "vehicle", "state"}, {"GET", "test:state"}, {"SET", "test:state"}, {"PUBLISH", "test:music", "play:240"}}
		replies := []string{"51", "ready-to-drive", string(raw)}
		for i, want := range expected {
			header, err := rd.ReadString('\n')
			if err != nil {
				done <- err
				return
			}
			var n int
			fmt.Sscanf(header, "*%d", &n)
			args := make([]string, n)
			for j := range args {
				value, err := readReply(rd)
				if err != nil {
					done <- err
					return
				}
				args[j], _ = value.(string)
			}
			if len(args) < len(want) || !reflect.DeepEqual(args[:len(want)], want) {
				done <- fmt.Errorf("command %d: %v", i, args)
				return
			}
			if i == 3 {
				var s state
				if len(args) != 3 || json.Unmarshal([]byte(args[2]), &s) != nil || s.Armed || s.LastTrigger == 0 {
					done <- fmt.Errorf("bad saved state %v", args)
					return
				}
			}
			if i < 3 {
				fmt.Fprintf(server, "$%d\r\n%s\r\n", len(replies[i]), replies[i])
			} else if i == 3 {
				fmt.Fprint(server, "+OK\r\n")
			} else {
				fmt.Fprint(server, ":1\r\n")
			}
		}
		done <- nil
	}()
	if err := handle(speedEvent("44", "51", now*1000), &r, "test:state", "test:music", 240); err != nil {
		t.Fatal(err)
	}
	if err := <-done; err != nil {
		t.Fatal(err)
	}
}
