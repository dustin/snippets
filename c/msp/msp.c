#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "msp.h"

#if __BYTE_ORDER__ != __ORDER_LITTLE_ENDIAN__
#error "This code only works on little endians"
#endif

MSP *new_msp() {
    MSP *rv = calloc(1, sizeof(MSP));
    rv->_state = MSP_IDLE;
    rv->_interesting = 0xffffffff;
    return rv;
}

void destroy_msp(MSP *m) {
    free(m);
}

#define BADBYTE if (m->unexpectedByteCallback) { m->unexpectedByteCallback(m->_state, b); }

void readStatus(MSP *m) {
    if (m->statusCallback) {
        m->statusCallback(&m->status);
    }
}

void readRC(MSP *m) {
    if (m->rcCallback) {
        m->rcCallback(m->rc_chans);
    }
}

uint32_t cmdmask(uint8_t c) {
    uint32_t c32 = 1;
    return c32 << (c - 99);
}

bool commandInteresting(MSP *m, uint8_t c) {
    return (cmdmask(c) & m->_interesting) != 0;
}

void notInteresting(MSP *m, uint8_t c) {
    m->_interesting &= ~cmdmask(c);
}

void clearInteresting(MSP *m) {
    m->_interesting = 0;
}

void setInteresting(MSP *m, uint8_t c) {
    m->_interesting |= cmdmask(c);
}

_msp_state stateIdle(MSP *m, uint8_t b) {
    if (b == '$') {
        return MSP_HEADER_START;
    }
    BADBYTE
    return MSP_IDLE;
}

_msp_state stateHeaderStart(MSP *m, uint8_t b) {
    if (b == 'M') {
        return MSP_HEADER_M;
    }
    BADBYTE
    return MSP_IDLE;
}

_msp_state stateM(MSP *m, uint8_t b) {
    if (b == '>') {
        return MSP_HEADER_SIZE;
    }
    BADBYTE
    return MSP_IDLE;
}

_msp_state stateSize(MSP *m, uint8_t b) {
    if (b > MAX_MSP_CMD_LEN) {
        // Too large a body, just go into idle and resync
        return MSP_IDLE;
    }
    m->_cmdSize = b;
    m->_checksum = b;
    return MSP_HEADER_CMD;
}

_msp_state stateCmd(MSP *m, uint8_t b) {
    m->_cmdI = 0;
    m->_cmdId = b;
    m->_checksum ^= m->_cmdId;
    switch (m->_cmdId) {
    case MSP_RC:
        m->_bufptr = (uint8_t*)m->rc_chans;
        break;
    case MSP_STATUS:
        m->_bufptr = (uint8_t*)&m->status;
        break;
    default:
        m->_bufptr = m->_buf;
    }
    return commandInteresting(m, m->_cmdId) ? MSP_FILLBUF : MSP_DISCARD;
}

_msp_state stateFillBuf(MSP *m, uint8_t b) {
    *m->_bufptr = b;
    m->_bufptr++;
    m->_cmdI++;
    m->_checksum ^= b;
    return m->_cmdI == m->_cmdSize ? MSP_CHECKSUM : MSP_FILLBUF;
}

// This bit was copied from mwosd because of all the magic numbers and
// stuff.
void setupBoxIDs(MSP *m) {
    memset(&m->boxes, sizeof(m->boxes), 0);

    uint32_t bit = 1;
    for (int i = 0; i < m->_cmdSize; i++) {
      switch(m->_buf[i]) {
      case 0:
        m->boxes.armed |= bit;
        break;
      case 1:
        m->boxes.stable |= bit;
        break;
      case 2:
        m->boxes.horizon |= bit;
        break;
      case 3:
        m->boxes.baro |= bit;
        break;
      case 5:
        m->boxes.mag |= bit;
        break;
      case 8:
        m->boxes.camstab |= bit;
        break;
      case 10:
        m->boxes.gpshome |= bit;
        break;
      case 11:
        m->boxes.gpshold |= bit;
        break;
      case 12:
        m->boxes.passthru |= bit;
        break;
      case 16:
        m->boxes.llights |= bit;
        break;
      case 19:
        m->boxes.osd_switch |= bit;
        break;
      case 20:
        m->boxes.gpsmission |= bit;
        break;
      case 21:
        m->boxes.gpsland |= bit;
        break;
      }
      bit <<= 1;
    }
}

_msp_state stateChecksum(MSP *m, uint8_t b) {
    if ((m->_checksum ^ b) != 0) {
        // Checksum failed.  Drop it
        if (m->checksumFailedCallback) {
            m->checksumFailedCallback(m->_cmdId, m->_cmdSize, m->_buf, b);
        }
        return MSP_IDLE;
    }

    switch (m->_cmdId) {
    case MSP_STATUS:
        readStatus(m);
        break;
    case MSP_BOXIDS:
        setupBoxIDs(m);
        break;
    case MSP_RC:
        readRC(m);
        break;
     default:
         if (m->genericCallback) {
             m->genericCallback(m->_cmdId, m->_cmdSize, m->_buf);
         }
    }
    return MSP_IDLE;
}

_msp_state stateDiscard(MSP *m, uint8_t b) {
    return m->_cmdI++ >= m->_cmdSize ? MSP_IDLE : MSP_DISCARD;
}

/*
<preamble>,<direction>,<size>,<command>,,<crc>

Where:

preamble = the ASCII characters '$M'

direction = the ASCII character '<' if to the MWC or '>' if from the MWC

size = number of data bytes, binary. Can be zero as in the case of a data request to the MWC

command = message_id as per the table below

data = as per the table below. UINT16 values are LSB first.

crc = XOR of <size>, <command> and each data byte into a zero'ed sum
*/

void msp_feed(MSP *m, uint8_t b) {
    switch (m->_state) {
    case MSP_IDLE:
        m->_state = stateIdle(m, b);
        break;
    case MSP_HEADER_START:
        m->_state = stateHeaderStart(m, b);
        break;
    case MSP_HEADER_M:
        m->_state = stateM(m, b);
        break;
    case MSP_HEADER_SIZE:
        m->_state = stateSize(m, b);
        break;
    case MSP_HEADER_CMD:
        m->_state = stateCmd(m, b);
        break;
    case MSP_FILLBUF:
        m->_state = stateFillBuf(m, b);
        break;
    case MSP_CHECKSUM:
        m->_state = stateChecksum(m, b);
        break;
    case MSP_DISCARD:
        m->_state = stateDiscard(m, b);
        break;
    }
}
