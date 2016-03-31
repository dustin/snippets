#define NCHAN 8

#define SUMD_VALID 0x01
#define SUMD_FAILSAFE 0x81
#define CRC_POLYNOME 0x1021

volatile uint16_t channels[NCHAN];

union chandata {
    uint16_t val;
    byte data[2];
};

struct {
    byte magic;
    byte hdr1;
    byte hdr2;
    byte nchan;
    union chandata channels[NCHAN*2];
    byte crc[2];
} packet;

void setup() {
    Serial.begin(115200);

    packet.magic = 0xa8;
    packet.nchan = NCHAN;

    for (int i = 0; i < NCHAN; i++) {
        channels[i] = 1500;
    }
}

uint16_t CRC16(uint16_t crc, uint8_t value) {
    crc = crc ^ (int16_t)value<<8;
    for(uint8_t i=0; i<8; i++) {
        if (crc & 0x8000) {
            crc = (crc << 1) ^ CRC_POLYNOME;
        } else {
            crc = (crc << 1);
        }
    }
    return crc;
}

void populateChans() {
    packet.hdr1 = SUMD_VALID;

    for (int i = 0; i < NCHAN; i++) {
        packet.channels[i].val = 8*channels[i];
    }
    uint16_t crc = 0;
    byte *b = (byte*)&packet;
    for (int i = 0; i < sizeof(packet)-2; i++) {
        crc = CRC16(crc, b[i]);
    }
    packet.crc[0] = crc >> 8;
    packet.crc[1] = crc & 0xff;
}

void loop() {
    for (int i = 0; i < NCHAN; i++) {
        channels[i] += (1 << i);
        if (channels[i] > 2000) {
            channels[i] = 1000;
        }
    }
    delay(1000);
}
