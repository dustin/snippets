#include "longtime.h"

bool LongTimer::ready() {
    return armed && millis() > next;
}

long LongTimer::remaining() const {
    long remain = armed ? next - millis() : 0;
    return remain;
}

void LongTimer::set(long ms) {
    armed = ms != 0;
    if (armed)
        next = millis() + ms - 1;
}
