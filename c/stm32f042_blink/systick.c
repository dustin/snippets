/*
 * systick.c - systick routines
 */

#include "systick.h"

static __IO uint32_t TimingDelay;

/*
 * init the system tick infrastructure
 */
void systick_init(void)
{
	/* start the 1ms system tick */
	if (SysTick_Config(SystemCoreClock / 1000))
	{ 
		/* Capture error */ 
		while (1);
	}
}

/*
 * delay for number of milliseconds
 */
void systick_delayms(uint32_t ms)
{
	TimingDelay = ms;

	while(TimingDelay != 0);
}

/*
 * IRQ handler
 */
void SysTick_Handler(void)
{
	/* update ms delay timer */
	if (TimingDelay != 0x00)
	{ 
		TimingDelay--;
	}
}
