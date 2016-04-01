#define SUMD_VALID 0x01
#define SUMD_FAILSAFE 0x81
#define CRC_POLYNOME 0x1021

class SUMD {
public:

    SUMD(const uint8_t nchans) {
        // [magic], [hdr], [nchan], [2 bytes * channels], [2 byte crc]
        data = new byte[(nchans*2)+5];
        data[0] = 0xa8;
        data[2] = nchans;

        for (int i = 0; i < nchans; i++) {
            setChannel(i, 1500);
        }

        setHeader(SUMD_VALID);
    }

    void setChannel(const int ch, const int val) {
        uint16_t cval = 8*val;
        uint8_t off = chanOffset(ch);
        data[off] = cval >> 8;
        data[off+1] = cval & 0xff;
    }

    uint16_t channel(const int ch) {
        uint8_t off = chanOffset(ch);
        return ((data[off] << 8) | data[off+1]) / 8;
    }

    void setHeader(const byte b) {
        data[1] = b;

        headercrc = CRC16(0, data[0]);
        headercrc = CRC16(headercrc, data[1]);
        headercrc = CRC16(headercrc, data[2]);
    }

    const uint8_t nchan() {
        return data[2];
    }

    const uint8_t size() {
        return (nchan() * 2) + 5;
    }

    const byte* bytes() {
        computeCRC();
        return data;
    }

private:
    byte *data;

    uint16_t headercrc;

    uint8_t chanOffset(const int ch) {
        return 3 + (ch*2);
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

    void computeCRC() {
        uint8_t off = chanOffset(0);
        uint16_t crc = headercrc;
        for (int i = 0; i < nchan(); i++) {
            crc = CRC16(crc, data[off]);
            off++;

            crc = CRC16(crc, data[off]);
            off++;
        }

        data[off++] = crc >> 8;
        data[off] = crc & 0xff;
    }
};
