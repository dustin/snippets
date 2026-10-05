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
            # events and unrelated replies are ignored

    def close(self):
        try:
            self.send("Browser.close")
        except Exception:
            pass
        try:
            self.proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self.proc.kill()


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
            cdp.send("Page.enable", session=s)
            cdp.send("Page.navigate", {"url": url}, s)

            deadline = time.monotonic() + timeout
            check = "document.documentElement.hasAttribute('data-ready')"
            while True:
                r = cdp.send("Runtime.evaluate", {"expression": check, "returnByValue": True}, s)
                if r.get("result", {}).get("value") is True:
                    break
                if time.monotonic() > deadline:
                    sys.exit(f"shot.py: page never set <html data-ready> within {timeout:g}s")
                time.sleep(0.1)

            shot = cdp.send("Page.captureScreenshot",
                            {"format": "png",
                             "clip": {"x": 0, "y": 0, "width": W, "height": H, "scale": 1}}, s)
            with open(out, "wb") as f:
                f.write(base64.b64decode(shot["data"]))
        finally:
            cdp.close()


if __name__ == "__main__":
    main()
