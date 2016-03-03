#include <ch.h>
#include <hal.h>

/*
 * Working area for the LED flashing thread.
 */
static THD_WORKING_AREA(myThreadWorkingArea, 128);

#define LED PAL_LINE(GPIOB, 1)

/*
 * LED flashing thread.
 */
static THD_FUNCTION(myThread, arg) {
    while (true) {
        palClearLine(LED);
        chThdSleepMilliseconds(500);
        palSetLine(LED);
        chThdSleepMilliseconds(500);
    }
}

int main(void) {

    halInit();
    chSysInit();

    palSetLineMode(LED, PAL_MODE_OUTPUT_PUSHPULL);
    palClearLine(LED);

    chThdSleepMilliseconds(2000);
    /* Starting the flashing LEDs thread.*/
    (void)chThdCreateStatic(myThreadWorkingArea, sizeof(myThreadWorkingArea),
                            NORMALPRIO, myThread, NULL);

    for (;;) {
        chThdSleepMilliseconds(10000);
    }
}
