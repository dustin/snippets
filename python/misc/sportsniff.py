#!/usr/bin/env python

import sys
import struct

types = {
    0x0100: 'ALT_FIRST_ID',
    0x010f: 'ALT_LAST_ID',
    0x0110: 'VARIO_FIRST_ID',
    0x011f: 'VARIO_LAST_ID',
    0x0200: 'CURR_FIRST_ID',
    0x020f: 'CURR_LAST_ID',
    0x0210: 'VFAS_FIRST_ID',
    0x021f: 'VFAS_LAST_ID',
    0x0300: 'CELLS_FIRST_ID',
    0x030f: 'CELLS_LAST_ID',
    0x0400: 'T1_FIRST_ID',
    0x040f: 'T1_LAST_ID',
    0x0410: 'T2_FIRST_ID',
    0x041f: 'T2_LAST_ID',
    0x0500: 'RPM_FIRST_ID',
    0x050f: 'RPM_LAST_ID',
    0x0600: 'FUEL_FIRST_ID',
    0x060f: 'FUEL_LAST_ID',
    0x0700: 'ACCX_FIRST_ID',
    0x070f: 'ACCX_LAST_ID',
    0x0710: 'ACCY_FIRST_ID',
    0x071f: 'ACCY_LAST_ID',
    0x0720: 'ACCZ_FIRST_ID',
    0x072f: 'ACCZ_LAST_ID',
    0x0800: 'GPS_LONG_LATI_FIRST_ID',
    0x080f: 'GPS_LONG_LATI_LAST_ID',
    0x0820: 'GPS_ALT_FIRST_ID',
    0x082f: 'GPS_ALT_LAST_ID',
    0x0830: 'GPS_SPEED_FIRST_ID',
    0x083f: 'GPS_SPEED_LAST_ID',
    0x0840: 'GPS_COURS_FIRST_ID',
    0x084f: 'GPS_COURS_LAST_ID',
    0x0850: 'GPS_TIME_DATE_FIRST_ID',
    0x085f: 'GPS_TIME_DATE_LAST_ID',
    0x0900: 'A3_FIRST_ID',
    0x090f: 'A3_LAST_ID',
    0x0910: 'A4_FIRST_ID',
    0x091f: 'A4_LAST_ID',
    0x0a00: 'AIR_SPEED_FIRST_ID',
    0x0a0f: 'AIR_SPEED_LAST_ID',
    0x0b00: 'POWERBOX_BATT1_FIRST_ID',
    0x0b0f: 'POWERBOX_BATT1_LAST_ID',
    0x0b10: 'POWERBOX_BATT2_FIRST_ID',
    0x0b1f: 'POWERBOX_BATT2_LAST_ID',
    0x0b20: 'POWERBOX_STATE_FIRST_ID',
    0x0b2f: 'POWERBOX_STATE_LAST_ID',
    0x0b30: 'POWERBOX_CNSP_FIRST_ID',
    0x0b3f: 'POWERBOX_CNSP_LAST_ID',
    0xf101: 'RSSI_ID',
    0xf102: 'ADC1_ID',
    0xf103: 'ADC2_ID',
    0xfd00: 'SP2UART_A_ID',
    0xfd01: 'SP2UART_B_ID',
    0xf104: 'BATT_ID',
    0xf105: 'SWR_ID',
    0xf106: 'XJT_VERSION_ID',
    0x0a10: 'FUEL_QTY_FIRST_ID',
    0x0a1f: 'FUEL_QTY_LAST_ID',
}

def parseLine():
    try:
        l = sys.stdin.readline()
        a = l.split(',')
        return float(a[0]), int(a[1], 16)
    except:
        sys.exit(0)

def cksum(a):
    val = 0
    for b in a[:9]:
        val += b
        val += (val >> 8)
        val &= 0xff
    return val

def dump(a):
    ' '.join("0x%02x" % x for x in a)

def decodeValue(a):
    # 0x10 (8bit data frame header)
    # value_type (16 bit, e.g. voltage / speed)
    # value (32 bit, may be signed or unsigned depending on value type)
    # checksum (8 bit)

    if a[0] != 0x10:
        return "unknown msg: %s" % dump(a)

    s = ''.join(chr(x) for x in a)
    vtype, val = struct.unpack("<hl", s[1:7])
    vtname = types.get(vtype, '%04x' % vtype)

    return "sensor %s -> %d" % (vtname, val)

wantReq = 1
wantID = 2
data = 3

sys.stdin.readline()
prev = 0
timeout = .009
state = wantReq
datas = []
while True:
    chans = []
    t, n = parseLine()
    if t - prev > timeout:
        state = wantReq
        if datas:
            if cksum(datas) != 0xff:
                print "Checksum failure: %s" % dump(datas)
            else:
                print "> %s" % decodeValue(datas)
        datas = []

    prev = t

    if state == wantReq:
        if n == 0x7e:
            state = wantID
    elif state == wantID:
        print "< Have 0x%02x?" % n
        state = data
    elif state == data:
        datas.append(n)
