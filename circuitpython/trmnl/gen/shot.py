"""Screenshot a page at 296x128 once it sets <html data-ready>.

Drives headless Chrome over --remote-debugging-pipe (Chrome DevTools
Protocol on fds 3/4), so there are no dependencies beyond the stdlib.

usage: shot.py <browser> <url> <out.png> [timeout_seconds]
"""

import base64
import json
import os
import subprocess
import sys
import tempfile
import time

W, H = 296, 128


class CDP:
    def __init__(self, browser, profile):
        # Chrome reads commands from fd 3 and writes responses to fd 4.
        to_chrome_r, self._w = os.pipe()
        self._r, from_chrome_w = os.pipe()
        self.proc = subprocess.Popen(
            [
                browser,
                "--headless=new",
                "--remote-debugging-pipe",
                "--disable-gpu",
                "--hide-scrollbars",
                "--use-mock-keychain",
                "--no-first-run",
                "--no-default-browser-check",
                "--disable-extensions",
                "--disable-breakpad",
                "--force-device-scale-factor=1",
                f"--window-size={W},{H}",
                f"--user-data-dir={profile}",
                *(["--no-sandbox"] if os.geteuid() == 0 else []),  # Chrome refuses root otherwise
                "about:blank",
            ],
            pass_fds=(3, 4),
            preexec_fn=lambda: (os.dup2(to_chrome_r, 3), os.dup2(from_chrome_w, 4)),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
        )
        os.close(to_chrome_r)
        os.close(from_chrome_w)
        self._buf = b""
        self._id = 0
        self.events = []

    def send(self, method, params=None, session=None):
        self._id += 1
        msg = {"id": self._id, "method": method, "params": params or {}}
        if session:
            msg["sessionId"] = session
        os.write(self._w, json.dumps(msg).encode() + b"\0")
        return self._wait(self._id)

    def _wait(self, want):
        while True:
            while b"\0" not in self._buf:
                chunk = os.read(self._r, 65536)
                if not chunk:
                    err = self.proc.stderr.read().decode(errors="replace")
                    raise RuntimeError("browser exited:\n" + err)
                self._buf += chunk
            raw, self._buf = self._buf.split(b"\0", 1)
            msg = json.loads(raw)
            if msg.get("id") == want:
                if "error" in msg:
                    raise RuntimeError(msg["error"])
                return msg.get("result", {})
            if "method" in msg:
                self.events.append(msg)                # kept for diagnostics

    def close(self):
        try:
            self.send("Browser.close")
        except Exception:
            pass
        try:
            self.proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self.proc.kill()


def diagnose(cdp, s):
    """Explain why a page never became ready: errors, failed requests, state."""
    lines = []
    try:
        r = cdp.send("Runtime.evaluate", {"returnByValue": True, "expression":
                     "JSON.stringify({url: location.href, title: document.title,"
                     " readyState: document.readyState,"
                     " text: (document.body ? document.body.innerText : '').slice(0, 300)})"}, s)
        st = json.loads(r["result"]["value"])
        lines.append(f"  page: {st['url']}  (readyState={st['readyState']}, title={st['title']!r})")
        if st["text"].strip():
            lines.append("  visible text: " + " | ".join(st["text"].split("\n")[:6]))
    except Exception as e:
        lines.append(f"  (couldn't read page state: {e})")

    urls = {}
    for ev in cdp.events:
        m, p = ev["method"], ev.get("params", {})
        if m == "Network.requestWillBeSent":
            urls[p["requestId"]] = p["request"]["url"]
        elif m == "Network.responseReceived" and p["response"]["status"] >= 400:
            lines.append(f"  HTTP {p['response']['status']}: {p['response']['url']}")
        elif m == "Network.loadingFailed" and not p.get("canceled"):
            lines.append(f"  request failed ({p['errorText']}): {urls.get(p['requestId'], '?')}")
        elif m == "Runtime.exceptionThrown":
            d = p["exceptionDetails"]
            msg = d.get("exception", {}).get("description") or d.get("text", "")
            lines.append("  JS exception: " + msg.split("\n")[0])
        elif m == "Runtime.consoleAPICalled" and p["type"] in ("error", "warning", "assert"):
            args = " ".join(str(a.get("value", a.get("description", ""))) for a in p["args"])
            lines.append(f"  console.{p['type']}: {args}")
        elif m == "Log.entryAdded" and p["entry"]["level"] in ("error", "warning"):
            e = p["entry"]
            lines.append(f"  {e['source']} {e['level']}: {e['text']}" + (f" ({e['url']})" if e.get("url") else ""))
    if len(lines) == 1:
        lines.append("  no errors or failed requests were reported")
    return "\n".join(lines)


def main():
    if len(sys.argv) < 4:
        sys.exit("usage: shot.py <browser> <url> <out.png> [timeout_seconds]")
    browser, url, out = sys.argv[1:4]
    timeout = float(sys.argv[4]) if len(sys.argv) > 4 else 30.0

    with tempfile.TemporaryDirectory() as profile:
        cdp = CDP(browser, profile)
        try:
            target = cdp.send("Target.createTarget", {"url": "about:blank"})
            s = cdp.send("Target.attachToTarget",
                         {"targetId": target["targetId"], "flatten": True})["sessionId"]

            cdp.send("Emulation.setDeviceMetricsOverride",
                     {"width": W, "height": H, "deviceScaleFactor": 1, "mobile": False}, s)
            for domain in ("Page", "Runtime", "Network", "Log"):
                cdp.send(domain + ".enable", session=s)
            cdp.send("Page.navigate", {"url": url}, s)

            deadline = time.monotonic() + timeout
            check = "document.documentElement.hasAttribute('data-ready')"
            while True:
                r = cdp.send("Runtime.evaluate", {"expression": check, "returnByValue": True}, s)
                if r.get("result", {}).get("value") is True:
                    break
                if time.monotonic() > deadline:
                    sys.exit(f"shot.py: page never set <html data-ready> within {timeout:g}s\n"
                             + diagnose(cdp, s))
                time.sleep(0.1)

            # Report what was actually rendered when it isn't what was asked for
            # (a redirect that drops the query string renders the wrong card).
            final = cdp.send("Runtime.evaluate", {"expression": "location.href",
                                                  "returnByValue": True}, s)
            final = final.get("result", {}).get("value", "")
            if final != url or os.environ.get("SHOT_DEBUG"):
                print(f"shot.py: requested {url}\nshot.py: rendered  {final}", file=sys.stderr)
            if os.environ.get("SHOT_DEBUG"):
                print(diagnose(cdp, s), file=sys.stderr)

            shot = cdp.send("Page.captureScreenshot",
                            {"format": "png",
                             "clip": {"x": 0, "y": 0, "width": W, "height": H, "scale": 1}}, s)
            with open(out, "wb") as f:
                f.write(base64.b64decode(shot["data"]))
        finally:
            cdp.close()


if __name__ == "__main__":
    main()
