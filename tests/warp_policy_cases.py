"""Generate deterministic cases from the Python policy for native parity tests."""
import importlib.util
import json
from pathlib import Path
import random

spec = importlib.util.spec_from_file_location(
    "warp_policy", Path(__file__).resolve().parents[1] / "extensions/warp-sound.py")
policy = importlib.util.module_from_spec(spec)
spec.loader.exec_module(policy)


def cases():
    rng = random.Random(240)
    result = []
    for sequence in range(100):
        saved = {"armed": False, "last_trigger": 0.0, "baseline_speed": None, "baseline_ts_ms": None}
        speed = 44.0
        timestamp = 100000.0 + sequence * 100000.0
        for _ in range(40):
            after = rng.choice([40.0, 44.0, 45.0, 46.0, 49.9, 50.0, 50.1, 51.0, 55.0])
            event = {"from": str(speed), "to": str(after), "ts": timestamp,
                     "data": {"hash": "engine-ecu", "field": "speed"}}
            current = str(after)
            now = timestamp / 1000
            vehicle = rng.choice(["ready-to-drive"] * 9 + ["parked", "stand-by"])
            fault = rng.randrange(20)
            if fault == 0:
                event["ts"] = None
            elif fault == 1:
                event["from"] = "NaN"
            elif fault == 2:
                now += 2
            elif fault == 3:
                current = "NaN"
            elif fault == 4:
                current = " " + current + " "
            elif fault == 5:
                event["from"] = "0"
            elif fault == 6:
                event["ts"] = True
            want, play = policy.decide(event, current, vehicle, saved, now)
            result.append({"event": event, "current": current, "vehicle": vehicle, "state": saved,
                           "now": now, "want": want, "play": play})
            saved = want
            speed = after
            timestamp += rng.choice([100.0, 200.0, 300.0, 500.0, 1500.0])
    return result


if __name__ == "__main__":
    print(json.dumps(cases()))
