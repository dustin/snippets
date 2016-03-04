#include <ch.h>
#include <hal.h>

/*
 * Working area for the LED flashing thread.
 */
static THD_WORKING_AREA(onThreadWorkingArea, 128);
static THD_WORKING_AREA(offThreadWorkingArea, 128);

static mutex_t mu;

#define LED PAL_LINE(GPIOB, 1)

/*
 * LED flashing thread.
 */
static THD_FUNCTION(onThread, arg) {
    while (true) {
        chMtxLock(&mu);
        palSetLine(LED);
        chThdSleepMilliseconds(500);
        chMtxUnlock(&mu);
    }
}

/*
 * LED unflashing thread.
 */
static THD_FUNCTION(offThread, arg) {
    while (true) {
        chMtxLock(&mu);
        palClearLine(LED);
        chThdSleepMilliseconds(500);
        chMtxUnlock(&mu);
    }
}

int main(void) {

    halInit();
    chSysInit();

    palSetLineMode(LED, PAL_MODE_OUTPUT_PUSHPULL);
    palClearLine(LED);

    chMtxObjectInit(&mu);

    chThdSleepMilliseconds(2000);
    /* Starting the flashing LEDs thread.*/
    (void)chThdCreateStatic(onThreadWorkingArea, sizeof(onThreadWorkingArea),
                            NORMALPRIO, onThread, NULL);
    (void)chThdCreateStatic(offThreadWorkingArea, sizeof(offThreadWorkingArea),
                            NORMALPRIO, offThread, NULL);

    for (;;) {
        chThdSleepMilliseconds(10000);
    }
}
