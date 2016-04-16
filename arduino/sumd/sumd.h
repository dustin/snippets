/**
 * SUMD implementation as specified in
 * http://deviationtx.com/media/kunena/attachments/98/HoTT-SUMD-Spec-REV01-12062012-pdf.pdf
 */
#define SUMD_VALID 0x01
#define SUMD_FAILSAFE 0x81

#include <stdlib.h>
#include <util/crc16.h>

#define CRC16 _crc_xmodem_update

class SUMD {
public:

    // Construct a SUMD object managing the given number of channels.
    SUMD(const uint8_t nchans) {
        // [magic], [hdr], [nchan], [2 bytes * channels], [2 byte crc]
        data = (uint8_t*)malloc((nchans*2)+5);
        data[0] = 0xa8;
        data[2] = nchans;

        for (int i = 0; i < nchans; i++) {
            setChannel(i, 1500);
        }

        setHeader(SUMD_VALID);
    }

    // Set the value of the specified channel.
    void setChannel(const int ch, const int val) {
        uint16_t cval = 8*val;
        uint8_t off = chanOffset(ch);
        data[off] = cval >> 8;
        data[off+1] = cval & 0xff;
    }

    // Get the value of a specified channel.
    uint16_t channel(const int ch) {
        uint8_t off = chanOffset(ch);
        return ((data[off] << 8) | data[off+1]) / 8;
    }

    // Set the header to either SUMD_VALID or SUMD_FAILSAFE.
    void setHeader(const uint8_t b) {
        data[1] = b;

        headercrc = CRC16(0, data[0]);
        headercrc = CRC16(headercrc, data[1]);
        headercrc = CRC16(headercrc, data[2]);
    }

    // The number of channels being managed.
    const uint8_t nchan() {
        return data[2];
    }

    // The address of the buffer to transmit as a SUMD packet.
    const uint8_t* bytes() {
        computeCRC16();
        return data;
    }

    // The size of the buffer to transmit.
    const uint8_t size() {
        return (nchan() * 2) + 5;
    }

private:
    uint8_t *data;

    uint16_t headercrc;

    uint8_t chanOffset(const int ch) {
        return 3 + (ch*2);
    }

    void computeCRC16() {
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
