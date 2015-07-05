#include <string.h>
#include "msp.h"

#if __BYTE_ORDER__ != __ORDER_LITTLE_ENDIAN__
#error "This code only works on little endians"
#endif

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

void MSP::feed(uint8_t b) {
    switch (state) {
    case MSP_IDLE:
        state = stateIdle(b);
        break;
    case MSP_HEADER_START:
        state = stateHeaderStart(b);
        break;
    case MSP_HEADER_M:
        state = stateM(b);
        break;
    case MSP_HEADER_SIZE:
        state = stateSize(b);
        break;
    case MSP_HEADER_CMD:
        state = stateCmd(b);
        break;
    case MSP_FILLBUF:
        state = stateFillBuf(b);
        break;
    case MSP_CHECKSUM:
        state = stateChecksum(b);
        break;
    case MSP_DISCARD:
        state = stateDiscard(b);
        break;
    }
}

#define BADBYTE if (unexpectedByteCallback) { unexpectedByteCallback(state, b); }

_msp_state MSP::stateIdle(uint8_t b) {
    if (b == '$') {
        return MSP_HEADER_START;
    }
    BADBYTE
    return MSP_IDLE;
}

_msp_state MSP::stateHeaderStart(uint8_t b) {
    if (b == 'M') {
        return MSP_HEADER_M;
    }
    BADBYTE
    return MSP_IDLE;
}

_msp_state MSP::stateM(uint8_t b) {
    if (b == '>') {
        return MSP_HEADER_SIZE;
    }
    BADBYTE
    return MSP_IDLE;
}

_msp_state MSP::stateSize(uint8_t b) {
    if (b > MAX_MSP_CMD_LEN) {
        // Too large a body, just go into idle and resync
        return MSP_IDLE;
    }
    cmdSize = b;
    checksum = b;
    return MSP_HEADER_CMD;
}

_msp_state MSP::stateCmd(uint8_t b) {
    cmdI = 0;
    cmdId = b;
    checksum ^= cmdId;
    switch (cmdId) {
    case MSP_RC:
        bufptr = (uint8_t*)rc_chans;
        break;
    case MSP_STATUS:
        bufptr = (uint8_t*)&status;
        break;
    default:
        bufptr = buf;
    }
    return commandInteresting(cmdId) ? MSP_FILLBUF : MSP_DISCARD;
}

_msp_state MSP::stateFillBuf(uint8_t b) {
    *bufptr = b;
    bufptr++;
    cmdI++;
    checksum ^= b;
    return cmdI == cmdSize ? MSP_CHECKSUM : MSP_FILLBUF;
}

_msp_state MSP::stateChecksum(uint8_t b) {
    if ((checksum ^ b) != 0) {
        // Checksum failed.  Drop it
        if (checksumFailedCallback) {
            checksumFailedCallback(cmdId, cmdSize, buf, b);
        }
        return MSP_IDLE;
    }

    switch (cmdId) {
    case MSP_STATUS:
        readStatus();
        break;
    case MSP_BOXIDS:
        setupBoxIDs();
        break;
    case MSP_RC:
        readRC();
        break;
     default:
         if (genericCallback) {
             genericCallback(cmdId, cmdSize, buf);
         }
    }
    return MSP_IDLE;
}

_msp_state MSP::stateDiscard(uint8_t b) {
    return cmdI++ >= cmdSize ? MSP_IDLE : MSP_DISCARD;
}

void MSP::setupBoxIDs() {
    memset(&boxes, sizeof(boxes), 0);

    uint32_t bit = 1;
    for (int i = 0; i < cmdSize; i++) {
      switch(buf[i]) {
      case 0:
        boxes.armed |= bit;
        break;
      case 1:
        boxes.stable |= bit;
        break;
      case 2:
        boxes.horizon |= bit;
        break;
      case 3:
        boxes.baro |= bit;
        break;
      case 5:
        boxes.mag |= bit;
        break;
      case 8:
        boxes.camstab |= bit;
        break;
      case 10:
        boxes.gpshome |= bit;
        break;
      case 11:
        boxes.gpshold |= bit;
        break;
      case 12:
        boxes.passthru  |= bit;
        break;
      case 16:
        boxes.llights |= bit;
        break;
      case 19:
        boxes.osd_switch |= bit;
        break;
      case 20:
        boxes.gpsmission |= bit;
        break;
      case 21:
        boxes.gpsland |= bit;
        break;
      }
      bit <<= 1;
    }
}

void MSP::readStatus() {
    if (statusCallback) {
        statusCallback(&status);
    }
}

void MSP::readRC() {
    if (rcCallback) {
        rcCallback(rc_chans);
    }
}
