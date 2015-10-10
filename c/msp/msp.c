#include <string.h>
#include <stdlib.h>

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

bool msp_is_armed(MSP *m) {
    return m->status.flags & m->boxes.armed;
}

#define BADBYTE if (m->unexpected_byte_cb) { m->unexpected_byte_cb(m->_state, b); }

static void _msp_read_status(MSP *m) {
    if (m->status_cb) {
        m->status_cb(&m->status);
    }
}

static void _msp_read_rc(MSP *m) {
    if (m->rc_cb) {
        m->rc_cb(m->rc_chans);
    }
}

static uint32_t _msp_cmdmask(uint8_t c) {
    uint32_t c32 = 1;
    return c32 << (c - 99);
}

bool msp_cmd_interesting(MSP *m, uint8_t c) {
    return (_msp_cmdmask(c) & m->_interesting) != 0;
}

void msp_set_not_interesting(MSP *m, uint8_t c) {
    m->_interesting &= ~_msp_cmdmask(c);
}

void msp_clear_interesting(MSP *m) {
    m->_interesting = 0;
}

void msp_set_interesting(MSP *m, uint8_t c) {
    m->_interesting |= _msp_cmdmask(c);
}

static _msp_state _msp_state_idle(MSP *m, uint8_t b) {
    if (b == '$') {
        return MSP_HEADER_START;
    }
    BADBYTE
    return MSP_IDLE;
}

static _msp_state _msp_state_hdr_start(MSP *m, uint8_t b) {
    if (b == 'M') {
        return MSP_HEADER_M;
    }
    BADBYTE
    return MSP_IDLE;
}

static _msp_state _msp_state_m(MSP *m, uint8_t b) {
    if (b == '>') {
        return MSP_HEADER_SIZE;
    }
    BADBYTE
    return MSP_IDLE;
}

static _msp_state _msp_state_size(MSP *m, uint8_t b) {
    if (b > MAX_MSP_CMD_LEN) {
        // Too large a body, just go into idle and resync
        return MSP_IDLE;
    }
    m->_cmdSize = b;
    m->_checksum = b;
    return MSP_HEADER_CMD;
}

static _msp_state _msp_state_cmd(MSP *m, uint8_t b) {
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
    return msp_cmd_interesting(m, m->_cmdId) ? MSP_FILLBUF : MSP_DISCARD;
}

static _msp_state _msp_state_fill_buf(MSP *m, uint8_t b) {
    *m->_bufptr = b;
    m->_bufptr++;
    m->_cmdI++;
    m->_checksum ^= b;
    return m->_cmdI == m->_cmdSize ? MSP_CHECKSUM : MSP_FILLBUF;
}

// This bit was copied from mwosd because of all the magic numbers and
// stuff.
static void msp_setup_boxes(MSP *m) {
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

static _msp_state _msp_state_checksum(MSP *m, uint8_t b) {
    if ((m->_checksum ^ b) != 0) {
        // Checksum failed.  Drop it
        if (m->checksum_failed_cb) {
            m->checksum_failed_cb(m->_cmdId, m->_cmdSize, m->_buf, b);
        }
        return MSP_IDLE;
    }

    switch (m->_cmdId) {
    case MSP_STATUS:
        _msp_read_status(m);
        break;
    case MSP_BOXIDS:
        msp_setup_boxes(m);
        break;
    case MSP_RC:
        _msp_read_rc(m);
        break;
     default:
         if (m->generic_cb) {
             m->generic_cb(m->_cmdId, m->_cmdSize, m->_buf);
         }
    }
    return MSP_IDLE;
}

static _msp_state _msp_state_discard(MSP *m, uint8_t b) {
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
        m->_state = _msp_state_idle(m, b);
        break;
    case MSP_HEADER_START:
        m->_state = _msp_state_hdr_start(m, b);
        break;
    case MSP_HEADER_M:
        m->_state = _msp_state_m(m, b);
        break;
    case MSP_HEADER_SIZE:
        m->_state = _msp_state_size(m, b);
        break;
    case MSP_HEADER_CMD:
        m->_state = _msp_state_cmd(m, b);
        break;
    case MSP_FILLBUF:
        m->_state = _msp_state_fill_buf(m, b);
        break;
    case MSP_CHECKSUM:
        m->_state = _msp_state_checksum(m, b);
        break;
    case MSP_DISCARD:
        m->_state = _msp_state_discard(m, b);
        break;
    }
}
