#include <ch.h>
#include <hal.h>

/*
 * Working area for the LED flashing thread.
 */
static THD_WORKING_AREA(onThreadWorkingArea, 128);
static THD_WORKING_AREA(offThreadWorkingArea, 128);
static THD_WORKING_AREA(signalerTheadWorkingArea, 128);

static MUTEX_DECL(mu);
static CONDVAR_DECL(cond);

#define LED PAL_LINE(GPIOB, 1)

/*
 * LED flashing thread.
 */
static THD_FUNCTION(onThread, arg) {
    while (true) {
        chMtxLock(&mu);
        chCondWait(&cond);
        palSetLine(LED);
        chMtxUnlock(&mu);
    }
}

/*
 * LED unflashing thread.
 */
static THD_FUNCTION(offThread, arg) {
    while (true) {
        chMtxLock(&mu);
        chCondWait(&cond);
        palClearLine(LED);
        chMtxUnlock(&mu);
    }
}

static THD_FUNCTION(signalerThread, arg) {
    while (true) {
        chMtxLock(&mu);
        chThdSleepMilliseconds(500);
        chCondSignal(&cond);
        chMtxUnlock(&mu);
    }
}

int main(void) {

    halInit();
    chSysInit();

    palSetLineMode(LED, PAL_MODE_OUTPUT_PUSHPULL);
    palClearLine(LED);

    /* Starting the flashing LEDs thread.*/
    (void)chThdCreateStatic(onThreadWorkingArea, sizeof(onThreadWorkingArea),
                            NORMALPRIO, onThread, NULL);
    (void)chThdCreateStatic(offThreadWorkingArea, sizeof(offThreadWorkingArea),
                            NORMALPRIO, offThread, NULL);
    (void)chThdCreateStatic(signalerTheadWorkingArea, sizeof(signalerTheadWorkingArea),
                            NORMALPRIO, signalerThread, NULL);
    for (;;) {
        chThdSleepMilliseconds(10000);
    }
}
