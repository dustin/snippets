#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

#include "msp.h"

static MSP *msp;

static void genCb(uint8_t cmdId, uint8_t bufLen, uint8_t *buf) {
    /*
    switch (cmdId) {
    case MSP_IDENT:
    case MSP_RAW_GPS:
    case MSP_COMP_GPS:
    case MSP_ATTITUDE:
    case MSP_ALTITUDE:
        return;
    }
    */

    printf("  Read %d bytes for cmd %d:\t", bufLen, cmdId);
    for (int i = 0; i < bufLen; i++) {
        printf("%02x ", buf[i]);
    }
    printf("\n");
}

static void stCb(MSPStatus *status) {
    printf("cycle_time: %d, i2c errors: %d, sensors (%02x)%s%s%s%s, setting=%d, flags: %x  %s\n",
           status->cycleTime, status->i2cErrors,
           status->sensors,
           status->sensors&1?" ACC":"", status->sensors&2?" BARO":"", status->sensors&4?" MAG":"", status->sensors&8?" GPS":"",
           status->setting,
           status->flags, msp_is_armed(msp) ? "ARMED" : ""
           );
}

static void rcCb(uint16_t *rc_chans) {
    printf("roll=%d, pitch=%d, yaw=%d, throttle=%d, aux1=%d, aux2=%d, aux3=%d, aux4=%d\n",
           rc_chans[CHAN_ROLL],
           rc_chans[CHAN_PITCH],
           rc_chans[CHAN_YAW],
           rc_chans[CHAN_THROTTLE],
           rc_chans[CHAN_AUX1],
           rc_chans[CHAN_AUX2],
           rc_chans[CHAN_AUX3],
           rc_chans[CHAN_AUX4]);
}

static void csCb(uint8_t cmdId, uint8_t cmdSize, uint8_t *buf, uint8_t cs) {
    uint8_t exp = cmdSize;
    exp ^= cmdId;
    for (int i = 0; i < cmdSize; i++) {
        exp ^= buf[i];
    }
    printf("CHECKSUM FAILED on %d command (got %x, expected %x)\n", cmdId, cs, exp);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Source file is required\n");
        exit(1);
    }
    int f = open(argv[1], O_RDONLY);
    if (f < 0) {
        perror(argv[1]);
        exit(1);
    }

    printf("MSP size is %d\n", (int)sizeof(MSP));

    msp = new_msp();
    msp->generic_cb = genCb;
    msp->rc_cb = rcCb;
    msp->status_cb = stCb;
    msp->checksum_failed_cb = csCb;

    /*
    msp_clear_interesting(msp);
    msp_set_interesting(msp, MSP_BOXIDS);
    msp_set_interesting(msp, MSP_RC);
    msp_set_interesting(msp, MSP_STATUS);
    */

    for (;;) {
        uint8_t x;
        int r = read(f, &x, 1);
        if (r < 1) {
            break;
        }

        msp_feed(msp, x);
    }
}
