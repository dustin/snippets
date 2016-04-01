#define NCHAN 8

#define SUMD_VALID 0x01
#define SUMD_FAILSAFE 0x81
#define CRC_POLYNOME 0x1021

volatile uint16_t channels[NCHAN];

static uint16_t headercrc = 0;

struct {
    byte magic;
    byte hdr;
    byte nchan;
    byte channels[NCHAN*2];
    byte crc[2];
} packet __attribute__((packed));

void setup() {
    Serial.begin(115200);

    packet.magic = 0xa8;
    packet.nchan = NCHAN;

    for (int i = 0; i < NCHAN; i++) {
        channels[i] = 1500;
    }

    setHeader(SUMD_VALID);
}

void setHeader(byte b) {
    packet.hdr = b;

    headercrc = CRC16(0, packet.magic);
    headercrc = CRC16(headercrc, packet.hdr);
    headercrc = CRC16(headercrc, packet.nchan);
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
    uint16_t crc = headercrc;

    int j = 0;
    for (int i = 0; i < NCHAN; i++) {
        uint16_t val = 8*channels[i];

        packet.channels[j] = val >> 8;
        crc = CRC16(crc, packet.channels[j]);
        j++;

        packet.channels[j] = val&0xff;
        crc = CRC16(crc, packet.channels[j]);
        j++;
    }
    packet.crc[0] = crc >> 8;
    packet.crc[1] = crc & 0xff;
}

void transmit() {
    Serial.write((byte*)&packet, sizeof(packet));
}

void loop() {
    for (int i = 0; i < NCHAN; i++) {
        channels[i] += (i*2);
        if (channels[i] > 2000) {
            channels[i] = 1000;
        }
    }
    populateChans();
    transmit();
    delay(1000);
}
