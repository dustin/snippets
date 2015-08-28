#ifndef LONGTIME_H
#define LONGTIME_H 1

#include <Arduino.h>
#include <stdint.h>

class LongTimer {
    long next;
    byte armed;
public:
    LongTimer () : armed (0) {}

    /// return true if the timer is ready
    bool ready();
    /// Return the number of milliseconds before the timer will fire
    long remaining() const;
    /// Returns true if the timer is not armed
    byte idle() const { return !armed; }
    /// set the one-shot timeout value
    /// @param ms Timeout value. Timer stops once the timer has fired.
    void set(long ms);
};

#endif /* LONGTIME_H */
