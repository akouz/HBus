# HBus USB Bridge


![USB Bridge](https://github.com/akouz/HBus/blob/master/Devices/02_USB_Bridge/USB_Bridge.jpg)

  * Either FTDI FT230X or SiLabs CP2102N chip
  * Galvanic isolation 5300 Vrms between USB and HBus
  * CAN driver MSP2551 with slope control for low EMI
  * Optional HBus Arbiter U4 for CSMA/CA mode
  * HBus terminator selected by jumpers JMP3, JMP4
  * Can be used as a generic USB-UART bridge via P3 connector when jumper JMP1 is off
