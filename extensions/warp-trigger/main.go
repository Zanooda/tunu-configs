package main

import (
	"bufio"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"math"
	"net"
	"os"
	"strconv"
	"strings"
	"time"
)

const stateKey = "tunu:warp-sound:trigger-state"

type event struct {
	From any            `json:"from"`
	To   any            `json:"to"`
	TS   any            `json:"ts"`
	Data map[string]any `json:"data"`
}

type state struct {
	Armed         bool     `json:"armed"`
	LastTrigger   float64  `json:"last_trigger"`
	BaselineSpeed *float64 `json:"baseline_speed"`
	BaselineTS    *float64 `json:"baseline_ts_ms"`
}

func number(v any) (float64, bool) {
	var n float64
	switch v := v.(type) {
	case float64:
		n = v
	case string:
		var err error
		n, err = strconv.ParseFloat(strings.TrimSpace(v), 64)
		if err != nil {
			return 0, false
		}
	default:
		return 0, false
	}
	return n, !math.IsNaN(n) && !math.IsInf(n, 0)
}

func decide(e event, currentSpeed, vehicleState string, s state, now float64) (state, bool) {
	if e.Data["hash"] != "engine-ecu" || e.Data["field"] != "speed" {
		return s, false
	}
	invalidate := func() (state, bool) { s.Armed = false; s.BaselineSpeed = nil; s.BaselineTS = nil; return s, false }
	timestamp, validTS := number(e.TS)
	before, validBefore := number(e.From)
	after, validAfter := number(e.To)
	current, validCurrent := number(currentSpeed)
	if !validTS || !validBefore || !validAfter || !validCurrent {
		return invalidate()
	}
	age := now - timestamp/1000
	if age < -.25 || age > 1 || math.Abs(current-after) > .01 {
		return invalidate()
	}
	if s.BaselineSpeed == nil || s.BaselineTS == nil {
		s.Armed = after <= 45
		s.BaselineSpeed = &after
		s.BaselineTS = &timestamp
		return s, false
	}
	if timestamp <= *s.BaselineTS || timestamp-*s.BaselineTS > 1000 || math.Abs(before-*s.BaselineSpeed) > .01 {
		return invalidate()
	}
	play := false
	if after <= 45 {
		s.Armed = true
	} else if after > 50 {
		play = before <= 50 && s.Armed && vehicleState == "ready-to-drive" && now-s.LastTrigger >= 45
		s.Armed = false
		if play {
			s.LastTrigger = now
		}
	}
	s.BaselineSpeed = &after
	s.BaselineTS = &timestamp
	return s, play
}

type redisConn struct {
	conn   net.Conn
	reader *bufio.Reader
}

func connect(addr string) (*redisConn, error) {
	conn, err := net.DialTimeout("tcp", addr, 200*time.Millisecond)
	if err != nil {
		return nil, err
	}
	// One deadline covers the whole action, not one full budget per command.
	if err := conn.SetDeadline(time.Now().Add(400 * time.Millisecond)); err != nil {
		conn.Close()
		return nil, err
	}
	return &redisConn{conn: conn, reader: bufio.NewReader(conn)}, nil
}

func (r *redisConn) command(args ...string) (any, error) {
	writer := bufio.NewWriter(r.conn)
	fmt.Fprintf(writer, "*%d\r\n", len(args))
	for _, arg := range args {
		fmt.Fprintf(writer, "$%d\r\n%s\r\n", len(arg), arg)
	}
	if err := writer.Flush(); err != nil {
		return nil, err
	}
	return readReply(r.reader)
}

func readReply(reader *bufio.Reader) (any, error) {
	line, err := reader.ReadString('\n')
	if err != nil {
		return nil, err
	}
	if len(line) < 3 || line[len(line)-2:] != "\r\n" {
		return nil, errors.New("invalid Redis reply")
	}
	body := line[1 : len(line)-2]
	switch line[0] {
	case '+':
		return body, nil
	case '-':
		return nil, fmt.Errorf("Redis: %s", body)
	case ':':
		return strconv.ParseInt(body, 10, 64)
	case '$':
		length, err := strconv.Atoi(body)
		if err != nil || length < -1 || length > 65536 {
			return nil, errors.New("invalid Redis string length")
		}
		if length == -1 {
			return nil, nil
		}
		data := make([]byte, length+2)
		if _, err := io.ReadFull(reader, data); err != nil {
			return nil, err
		}
		if string(data[length:]) != "\r\n" {
			return nil, errors.New("invalid Redis string terminator")
		}
		return string(data[:length]), nil
	default:
		return nil, errors.New("unsupported Redis reply")
	}
}

func (r *redisConn) get(args ...string) (string, error) {
	v, err := r.command(args...)
	if err != nil {
		return "", err
	}
	if v == nil {
		return "", nil
	}
	s, ok := v.(string)
	if !ok {
		return "", errors.New("expected Redis string")
	}
	return s, nil
}

func handle(e event, r *redisConn, key, channel string, song int) error {
	if e.Data["hash"] != "engine-ecu" || e.Data["field"] != "speed" {
		return nil
	}
	speed, err := r.get("HGET", "engine-ecu", "speed")
	if err != nil {
		return err
	}
	vehicle, err := r.get("HGET", "vehicle", "state")
	if err != nil {
		return err
	}
	raw, err := r.get("GET", key)
	if err != nil {
		return err
	}
	var s state
	if raw != "" {
		if err := json.Unmarshal([]byte(raw), &s); err != nil {
			return fmt.Errorf("saved trigger state: %w", err)
		}
	}
	updated, play := decide(e, speed, vehicle, s, float64(time.Now().UnixMilli())/1000)
	// Persist before publishing so a retry cannot replay the same crossing.
	data, err := json.Marshal(updated)
	if err != nil {
		return err
	}
	if string(data) != raw {
		if _, err := r.command("SET", key, string(data)); err != nil {
			return err
		}
	}
	if play {
		if _, err := r.command("PUBLISH", channel, "play:"+strconv.Itoa(song)); err != nil {
			return err
		}
	}
	return nil
}

func run() error {
	addr := flag.String("redis", "127.0.0.1:6379", "Redis address")
	key := flag.String("state-key", stateKey, "trigger state key")
	channel := flag.String("music-channel", "music", "music command channel")
	song := flag.Int("song-number", 240, "resident sound number (240-255)")
	flag.Parse()
	if *song < 240 || *song > 255 {
		return errors.New("resident sound number must be 240-255")
	}
	var e event
	if err := json.NewDecoder(io.LimitReader(os.Stdin, 16384)).Decode(&e); err != nil {
		return err
	}
	if e.Data["hash"] != "engine-ecu" || e.Data["field"] != "speed" {
		return nil
	}
	r, err := connect(*addr)
	if err != nil {
		return err
	}
	defer r.conn.Close()
	return handle(e, r, *key, *channel, *song)
}

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, "warp-trigger:", err)
		os.Exit(1)
	}
}
