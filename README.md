# HBus

## Introduction

HBus is a wired home automation interface. Nodes made as simple and as cheap as possible. Minimum requirement - Arduino Pro Mini (Atmega 328P) with 78L05 regulator and a CAN transciever. Wiring made by Cat5/Cat6 cables. One twisted pair used for HBus communication, two twisted pairs distribute 12V power to HBus nodes, one pair left unused. 

## Basic principles

  * UART with CAN transceivers, dominant/recessive states.
  * Baud rate 19.2 kbps
  * CSMA/CD or CSMA/CA, see description.
  * CRC-16-CCITT.
  * Flow control by byte-stuffing.
  * Two modes of operation:
    * Configuration, point-to-point messages, master/slave communication model - HBus mode.
    * Operation, broadcast messages, producer/consumer communication model - MQTT mode.

## CAN driver restrictions

Many CAN drivers has embedded mechanism to prevent long dominant state. If dominant state lasts longer than a pre-defined time-out, driver automatically goes into recessive state. Bit interval is 52.08 us at baud rate of 19.2 kbps. Start bit and a data byte of 0x00 produce dominant state for 9 bit-intervals, or for 0.47 ms. The following CAN transceivers are suitable for HBus: 

  * MCP2542 -	dominant state 0.8 ms min
  * TJA1057 -	dominant state 0.8 ms min
  * NCV7342 - dominant state 1.3 ms min
  * NCV7351 - dominant state 1.5 ms min
  * TLE7250V - dominant state 4.5 ms min
  * MCP2551 - dominant state 1.25 ms min, slope control
  * ADM3054 - no limit, isolated
  * IFX1050GVIO - no limit
  * SN65HVD230 - no limit, slope control
  * L9615 - no limit, slope control
  * PCA82C250 - no limit, slope control

Transcevers with slope control produce less noise on the bus, ie offer metter EMI.

## Byte-stuffing

Code 0x1B (eg ESC symbol) marks the beginning of a 2-byte sequence. The following byte pairs are defined: 

  * 0x1B-0x02 	- start of HBus frame (SOF)
  * 0x1B-0x03 	- start of MQTT-SN frame (SOF)
  * 0x1B-0x07 	- end of frame (EOF)
  * 0x1B-0x08 	- insert 0x1B into data flow
  * 0x1B-0x09 	- insert 0x1B, 0x1B into data flow

## Frame structure

<table>
<thead>
<tr>
<th>Prefix</th>
<th>SOF</th>
<th>Message</th>
<th>CRC</th>
<th>EOF</th>
</tr>
</thead>
<tbody>
<tr>
<td>1 byte</td>
<td>2 bytes</td>
<td>8...136 bytes</td>
<td>2 bytes</td>
<td>2 bytes</td>
</tr>
</tbody>
<tbody>
<tr>
<td>priority</td>
<td>0x1B-0x02</td>
<td>message content</td>
<td>CRC</td>
<td>0x1B-0x07</td>
</tr></tbody></table>

Prefix used to wake-up CAN receivers from standby mode. Also it is used for early collision detection. If echo does not match the sent byte then sender must switch off its transmitter and wait until bus is free. 

  * Sender can start transmission if for 2 ms or more there were no traffic on bus.
  * Sender starts transmission with Sync byte. Its value depends on message priority:
    * 0xFF -	Low priority message
    * 0xFC -	Medium priority message 
    * 0xF0 -	High priority message
