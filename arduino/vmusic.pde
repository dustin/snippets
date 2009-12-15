#include <ctype.h>

#include <avr/interrupt.h>
#include <SoftwareSerial.h>

#define VMUSIC_CTS 5  //to VMUSIC CTS pin 6 (or ground!)
#define VMUSIC_RX 2  //to VMUSIC TXD pin 5
#define VMUSIC_TX 3  //to VMUSIC RXD pin 4
#define ledPin 13

// set up a new serial port
SoftwareSerial mySerial =  SoftwareSerial(VMUSIC_RX, VMUSIC_TX);  //(RXpin, TXpin)
int mostRecent = 0;

void setup() {
    Serial.begin(9600);      // opens serial port, sets data rate to 9600 bps

    // define pin modes for tx, rx, led pins:
    pinMode(VMUSIC_CTS, OUTPUT);
    pinMode(VMUSIC_RX, INPUT);
    pinMode(VMUSIC_TX, OUTPUT);
    pinMode(ledPin, OUTPUT);  // declare LED as output
    digitalWrite(ledPin, LOW);

    // set the data rate for the SoftwareSerial port:
    mySerial.begin(9600);

    //This will allow softwareSerial to process incoming automatically:
    attachInterrupt(0, readSoftwareSerial, LOW);

    //Tells VMUSIC that hardware is connected:
    digitalWrite(VMUSIC_CTS, LOW);

    delay(1000);

    mySerial.print(0x10, BYTE);
    mySerial.print(0x0d, BYTE);

    synchronize();
    mySerial.print(0x10, BYTE);
    mySerial.print(0x0d, BYTE);
    setVolume(0xE0);
    synchronize();
}


void loop() {
    bool serialReceived = false;
    if(Serial.available() > 0){
        noInterrupts();
        digitalWrite(VMUSIC_CTS, LOW);
    }
    while(Serial.available() > 0){
        char incoming = Serial.read();
        mySerial.print(incoming);
        serialReceived = true;
    }
    if (serialReceived) {
        Serial.println("");
        mySerial.print(13,BYTE);  //carriage-return for VMUSIC2
        serialReceived = false;
    }
    interrupts();  //re-enable softwareSerial
}

void synchronize() {
    digitalWrite(ledPin, LOW);
    noInterrupts();
    mostRecent = -1;
    Serial.println("Synchronizing with e");
    mySerial.print(0x65, BYTE);
    mySerial.print(0x0d, BYTE);
    while (mostRecent == -1 && mostRecent != 'e') {
        mostRecent = mySerial.read();
    }

    Serial.println("Synchronizing with E");
    mySerial.print(0x45, BYTE);
    mySerial.print(0x0d, BYTE);
    while (mostRecent == -1 && mostRecent != 'E') {
        mostRecent = mySerial.read();
    }
    interrupts();
    digitalWrite(ledPin, HIGH);
}

void setVolume(byte level) {
    mySerial.print(0x88, BYTE);
    mySerial.print(0x20, BYTE);
    mySerial.print(level, BYTE);
    mySerial.print(0x0d, BYTE);
}

void readSoftwareSerial(){
    noInterrupts();
    // digitalWrite(ledPin, HIGH);
    mostRecent = mySerial.read();
    if (isprint(mostRecent)) {
        Serial.print(mostRecent, BYTE);
    } else if (mostRecent == 0x0d) {
        // Ignore
    } else {
        Serial.print("\\0x");
        Serial.print(mostRecent, HEX);
    }

    // digitalWrite(ledPin, LOW);
    interrupts();
}
