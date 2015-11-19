#include <stdbool.h>
#include "stm32f0xx.h"
#include "led.h"
#include "systick.h"

bool isPrime(unsigned int n) {
    unsigned int i;
    for (i = 3; i * i <= n; i++) {
        if (n % i == 0) {
            return false;
        }
    }
    return true;
}

void blink(unsigned int n) {
    unsigned int i;
    for (i = 0; i < n; i++) {
        systick_delayms(200);
		led_on(LED1);
		systick_delayms(100);
		led_off(LED1);
    }
}

int main(void)
{
	/* initialize the hardware */
	led_init();
	systick_init();

    blink(2);
    systick_delayms(500);

	/* Loop forever */
    unsigned int i;
	for (i = 3; ; i+=2) {
        if (isPrime(i)) {
            blink(i);
            systick_delayms(500);
        }
	}
}
