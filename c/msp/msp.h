#ifndef MSP_H
#define MSP_H 1

#include <stdint.h>
#include <stdbool.h>

// Magic numbers copied from mwosd
#define  MSP_IDENT      100 // multitype + multiwii version + protocol version + capability variable
#define  MSP_STATUS     101 // cycletime & errors_count & sensor present & box activation & current setting number
#define  MSP_RAW_IMU    102 // 9 DOF
#define  MSP_SERVO      103 // 8 servos
#define  MSP_MOTOR      104 // 8 motors
#define  MSP_RC         105 // 8 rc chan and more
#define  MSP_RAW_GPS    106 // fix, numsat, lat, lon, alt, speed, ground course
#define  MSP_COMP_GPS   107 // distance home, direction home
#define  MSP_ATTITUDE   108 // 2 angles 1 heading
#define  MSP_ALTITUDE   109 // altitude, variometer
#define  MSP_ANALOG     110 // vbat, powermetersum, rssi if available on RX
#define  MSP_RC_TUNING  111 // rc rate, rc expo, rollpitch rate, yaw rate, dyn throttle PID
#define  MSP_PID        112 // P I D coeff (9 are used currently)
#define  MSP_BOX        113 // BOX setup (number is dependant of your setup)
#define  MSP_MISC       114 // powermeter trig
#define  MSP_MOTOR_PINS 115 // which pins are in use for motors & servos, for GUI
#define  MSP_BOXNAMES   116 // the aux switch names
#define  MSP_PIDNAMES   117 // the PID names
#define  MSP_BOXIDS     119 // get the permanent IDs associated to BOXes
#define  MSP_NAV_STATUS 121 // Returns navigation status
#define  MSP_CELLS      130 // FrSky SPort Telemtry

typedef enum {
    MSP_IDLE,
    MSP_HEADER_START,
    MSP_HEADER_M,
    MSP_HEADER_SIZE,
    MSP_HEADER_CMD,
    MSP_FILLBUF,
    MSP_CHECKSUM,
    MSP_DISCARD,
} _msp_state;

typedef struct {
    uint16_t cycleTime;
    uint16_t i2cErrors;
    uint16_t sensors;
    uint32_t flags;
    uint8_t setting;
} __attribute__((packed)) MSPStatus;

typedef struct {
    struct {
        uint8_t armed;
        uint8_t stable;
        uint8_t horizon;
        uint8_t baro;
        uint8_t mag;
        uint16_t camstab;
        uint16_t gpshome;
        uint16_t gpshold;
        uint16_t passthru;
        uint32_t osd_switch;
        uint32_t llights;
        uint32_t gpsmission;
        uint32_t gpsland;
    } boxes;

    union {
        uint8_t buf[0];

        MSPStatus status;

        // AERT1234
        uint16_t rc_chans[8];
    } _cmd_data;

    void (*generic_cb)(uint8_t cmdId, uint8_t bufLen, uint8_t *buf);
    void (*rc_cb)(uint16_t *rc_chans);
    void (*status_cb)(MSPStatus *status);
    void (*unexpected_byte_cb)(_msp_state st, uint8_t b);
    void (*checksum_failed_cb)(uint8_t cmdId, uint8_t bufLen, uint8_t *buf, uint8_t checksum);

    _msp_state _state;
    uint32_t   _interesting;

    uint8_t  _cmdDir;
    uint8_t  _cmdSize;
    uint8_t  _cmdId;
    uint8_t  _cmdI;
    uint8_t  _checksum;

    bool armed;
} MSP;

MSP *new_msp();
void destroy_msp(MSP*);

void msp_feed(MSP*, uint8_t);
bool msp_is_armed(MSP *);
bool msp_cmd_interesting(MSP *, uint8_t);
void msp_set_not_interesting(MSP *m, uint8_t c);
void msp_clear_interesting(MSP *m);
void msp_set_interesting(MSP *m, uint8_t c);

#define CHAN_ROLL 0
#define CHAN_PITCH 1
#define CHAN_YAW 2
#define CHAN_THROTTLE 3
#define CHAN_AUX1 4
#define CHAN_AUX2 5
#define CHAN_AUX3 6
#define CHAN_AUX4 7

#endif /* MSP_H */
