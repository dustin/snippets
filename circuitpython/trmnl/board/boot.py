# Runs before code.py on every reset and deep-sleep wake.
#
# Normally: make CIRCUITPY writable by code.py (so it can cache the shown
# image for the stale badge). The drive is then READ-ONLY over USB.
#
# To edit files over USB: hold button A while pressing reset.
# Waking from sleep by pressing A does not count; only a real reset does.
import board, digitalio, storage, microcontroller

usb_mode = False
if microcontroller.cpu.reset_reason != microcontroller.ResetReason.DEEP_SLEEP_ALARM:
    btn = digitalio.DigitalInOut(board.BUTTON_A)
    btn.switch_to_input(pull=digitalio.Pull.UP)
    usb_mode = not btn.value             # pressed (buttons pull low)
    btn.deinit()

if not usb_mode:
    storage.remount("/", readonly=False)
