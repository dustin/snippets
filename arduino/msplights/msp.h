#include <stdint.h>

#ifndef MAX_MSP_CMD_LEN
#define MAX_MSP_CMD_LEN 16
#endif

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

enum _msp_state {
    MSP_IDLE,
    MSP_HEADER_START,
    MSP_HEADER_M,
    MSP_HEADER_SIZE,
    MSP_HEADER_CMD,
    MSP_FILLBUF,
    MSP_CHECKSUM,
    MSP_DISCARD,
};

typedef struct {
    uint16_t cycleTime;
    uint16_t i2cErrors;
    uint16_t sensors;
    uint32_t flags;
    uint8_t setting;
} __attribute__((packed)) MSPStatus;

class MSP {
public:
 MSP() : genericCallback(NULL), rcCallback(NULL), statusCallback(NULL),
        unexpectedByteCallback(NULL), checksumFailedCallback(NULL), state(MSP_IDLE),
        interesting(0xffffffff) { }
    ~MSP() { }

    void feed(uint8_t b);

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

   MSPStatus status;

    // AERT1234
    uint16_t rc_chans[8];

    bool isArmed() { return status.flags & boxes.armed; }

    void (*genericCallback)(uint8_t cmdId, uint8_t bufLen, uint8_t *buf);
    void (*rcCallback)(uint16_t *rc_chans);
    void (*statusCallback)(MSPStatus *status);
    void (*unexpectedByteCallback)(_msp_state st, uint8_t b);
    void (*checksumFailedCallback)(uint8_t cmdId, uint8_t bufLen, uint8_t *buf, uint8_t checksum);

    void notInteresting(uint8_t c) {
        interesting &= ~cmdmask(c);
    }

    void clearInteresting() {
        interesting = 0;
    }

    void setInteresting(uint8_t c) {
        interesting |= cmdmask(c);
    }

 private:

    inline _msp_state stateIdle(uint8_t b);
    inline _msp_state stateHeaderStart(uint8_t b);
    inline _msp_state stateM(uint8_t b);
    inline _msp_state stateSize(uint8_t b);
    inline _msp_state stateCmd(uint8_t b);
    inline _msp_state stateFillBuf(uint8_t b);
    inline _msp_state stateChecksum(uint8_t b);
    inline _msp_state stateDiscard(uint8_t b);

    uint32_t cmdmask(uint8_t c) {
        uint32_t c32 = 1;
        return c32 << (c - 99);
    }

    bool commandInteresting(uint8_t c) {
        return (cmdmask(c) & interesting) != 0;
    }

    void setupBoxIDs();
    void readStatus();
    void readRC();

    _msp_state state;
    uint32_t interesting;

    uint8_t cmdSize;
    uint8_t cmdId;
    uint8_t cmdI;
    uint8_t checksum;
    uint8_t *bufptr;
    uint8_t buf[MAX_MSP_CMD_LEN];
};

#define CHAN_ROLL 0
#define CHAN_PITCH 1
#define CHAN_YAW 2
#define CHAN_THROTTLE 3
#define CHAN_AUX1 4
#define CHAN_AUX2 5
#define CHAN_AUX3 6
#define CHAN_AUX4 7
