
![HBus](https://github.com/akouz/HBus/blob/master/HBus_50.png)

  * [HBus mode (config and control messages)](https://github.com/akouz/HBus/blob/master/Protocol/README.md#hbus-mode-config-and-control-messages)
  * [MQTT-SN mode (broadcast messages)](https://github.com/akouz/HBus/blob/master/Protocol/README.md#mqtt-sn-mode-broadcast-messages)
  * [NodeTest](https://github.com/akouz/HBus/blob/master/NodeTest/)
  * [Devices](https://github.com/akouz/HBus/tree/master/Devices)
    
## Introduction

HBus is a wired home automation interface. Nodes made as simple and as cheap as possible. Minimum requirement - Arduino Pro Mini modules (Atmega 328P) with 78L05 regulator and a CAN transceiver. Alternatevily, dedicated [HBbus modules](https://github.com/akouz/HBnode/tree/main) (AVR64DD32) can be used, [HBnode Pro Mini](https://github.com/akouz/HBnode/tree/main/AVR64DD32/Hardware/Pro_mini) or [HBnode Nano](https://github.com/akouz/HBnode/tree/main/AVR64DD32/Hardware/Nano). HBnode modules have more memory, they offer 12-bit ADC and they can update their firmware via HBus. 

Once configured, HBus system is self-contained, eg it can operate without any host or server, like KNX, C-Bus or Velbus. 

## Basics

  * Baud rate 19.2 kbps.
  * Communication by ordinary UARTs with CAN transceivers, dominant/recessive states. CAN protocol not used, CAN controllers not required.
  * CSMA/CD (collision detection) allowed for cost sensitive implementations. CSMA/CA (collision avoidance) recommended, but is not mandatory. Common Arduino modules require an additional low-cost microcontroller for CSMA/CA. [HBnode](https://github.com/akouz/HBnode) modules implement CSMA/CA without additional microcontrollers.  
  * Flow control by byte-stuffing.
  * Two modes of operation:
    * [Configuration:](https://github.com/akouz/HBus#hbus-mode-config-and-control-messages) point-to-point messages, master/slave communication model - "HBus mode".
    * [Operation:](https://github.com/akouz/HBus#mqtt-sn-mode-broadcast-messages) broadcast messages, producer/consumer communication model - "MQTT-SN mode".
  * All messages covered by CRC-16-CCITT.  
  * Encryption is optional.

## Wiring

Wiring made by Cat5/Cat6 cables. Twisted pairs used as follows:
   * (orange + orange/white) = HBus signal:
     * orange - HBH (HBus High)
     * orange/white - HBL (HBus Low)
   * (blue + blue/white) = common
   * (brown + brown/white) = +Vsupply (12V...24V)
   * (green + green/white) = not used.

Free topology is allowed. Total length of cables should not exceed 500 m. A single terminating resistor 100 Ohm should be placed somwhere in a middle of the network.

Cat5/Cat6 cables have limited current carrying capability. To prevent fire in case of short circuit, power supply +12V connected to the bus should have current limit or a slow-blow [fuse](https://github.com/akouz/HBus/tree/master/Fuse). Cat5/Cat6 single core cable has wires 0.5 mm diameter or 24 AWG (0.511 mm diameter). Single [AWG24 wire can carry 2...3.5 A](https://en.wikipedia.org/wiki/American_wire_gauge), depending on allowed temerature rise. For a short time AWG24 can carry 5A or more, but cable insulation material will be degraded rapidly. 

Thus, when 2 wires in parallel are used for power distribution, Cat5/Cat6 can carry up to 4..7 A of current. However, it would be a bit risky, because it is hard to quarantee that both cores are always connected. Safe and reliable applications should not distribute more than 3 A via Cat5/Cat6 cable, even when 2 or 3 cores connected in parallel.

More expensive EIB/KNX cable can be used too. With 0.8 mm wires it can safely distribute [up to 10A of current.](https://en.wikipedia.org/wiki/American_wire_gauge)

## CAN driver restrictions

Many CAN drivers has embedded mechanism to prevent long dominant state. If dominant state lasts longer than a pre-defined time-out, driver automatically goes into recessive state. Bit interval is 52.08 us at baud rate of 19.2 kbps. Start bit and a data byte of 0x00 produce dominant state for 9 bit-intervals, or for 0.47 ms. The following CAN transceivers are suitable for HBus: 

  * MCP2542 -	0.8 ms min
  * ATA6560, ATA6561  -	0.8 ms min
  * TJA1057 -	0.8 ms min
  * NCV7342 - 1.3 ms min
  * NCV7351 - 1.5 ms min
  * TLE7250V - 4.5 ms min
  * MCP2551 - 1.25 ms min, slope control
  * ADM3054 - no limit, isolated
  * IFX1050GVIO - no limit
  * SN65HVD230 - no limit, slope control
  * L9615 - no limit, slope control
  * PCA82C250 - no limit, slope control

Transceivers with slope control produce less noise on the bus, ie have better EMC.

## HBus protocol

[HBus protocol description](https://github.com/akouz/HBus/blob/master/Protocol/README.md)
