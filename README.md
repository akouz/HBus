# HBus

  * [HBus mode (config and control messages)](https://github.com/akouz/HBus#hbus-mode-config-and-control-messages)
  * [MQTT-SN mode (broadcast messages)](https://github.com/akouz/HBus#mqtt-sn-mode-broadcast-messages)
 
## Introduction

HBus is a wired home automation interface. Nodes made as simple and as cheap as possible. Minimum requirement - Arduino Pro Mini (Atmega 328P) with 78L05 regulator and a CAN transciever. 

## Wiring

Wiring made by Cat5/Cat6 cables. Twisted pair used as follows:
   * orange - HBus_H
   * orange/white - HBus_L
   * blue + blue/white - common
   * brown + brown/white - supply +12V
   * green + green/white - not used.

Free topology is allowed. Total length of cables should not exceed 500 m. A single terminating resistor 100 Ohm should be placed somwhere in a middle of the network.

Cat5/Cat6 cables have limited current carrying capability. To prevent fire in case of short circuit, power supply +12V connected to the bus should have current limit at no more than 2A. In simplest case use a 1A...2A fuse. 

## Basic principles

  * UART with CAN transceivers, dominant/recessive states.
  * Baud rate 19.2 kbps.
  * CSMA/CD allowed for low-cost implementations. CSMA/CA recommended, but is not mandatory. CSMA/CA employs an additional low-cost microcontroller. 
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

Transcevers with slope control produce less noise on the bus, ie have better EMC.

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
<tr>
<td>priority</td>
<td>0x1B-0x02</td>
<td>HBus message content</td>
<td>CRC</td>
<td>0x1B-0x07</td>
<tr>
<td>priority</td>
<td>0x1B-0x03</td>
<td>MQTT message content</td>
<td>CRC</td>
<td>0x1B-0x07</td>
</tr></tbody></table>

Prefix used to wake-up CAN receivers from standby mode. Also it is used for early collision detection. If echo does not match the sent byte then sender must switch off its transmitter and wait until bus is free. 

  * Sender can start transmission if for 2 ms or more there were no traffic on bus.
  * Sender starts transmission with Prefix byte. Its value depends on message priority:
    * 0xFF -	Low priority message
    * 0xFC -	Medium priority message 
    * 0xF0 -	High priority message

CRC covers message content. Prefix and SOF exluded. Sender calculates CRC before byte-stuffing added to the message. Receiver calculates CRC after byte-stuffing removed from the received message. 

## Fields

Big endian used, eg MSB byte sent first, LSB byte sent last.

<table>
<thead>
<tr>
<th>Name</th>
<th>Bits</th>
<th>Description</th>
</tr>
</thead>
<tbody> 
<tr>
<td>NodeA_ID</td> 	
<td>16</td> 	
<td>ID of Node A, it must be unique in current network </td> 	
</tr>
<tr>
<td>NodeB_ID</td> 	
<td>16</td> 	
<td>ID of Node B, it must be unique in current network </td> 	
</tr>
<tr>
<td>MsgID</td> 	
<td>16</td> 	
<td>Message ID; it is incremented with every request; reply repeats MsgID of the request. Valid range 0x0001…0xFFFE</td> 	
</tr>
<tr>
<td>OkErr</td> 	
<td>8</td> 	
<td>OK = 0, any other value is an error code </td> 	
</tr>
</tbody></table>

# HBus mode (config and control messages)

## List of HBus commands

<table>
<thead>
<tr>
<th>Command</th>
<th>Name</th>
<th>Target</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td>1</td> 	
<td>REV</td> 	
<td>Node</td> 	
<td>Read node description, hardware and software revisions </td> 	
</tr>
<tr>
<td>2</td> 	
<td>STATUS</td> 	
<td>Node</td> 	
<td>Read node status</td> 	
</tr>
<tr>
<td>3</td> 	
<td>COLLECT</td> 	
<td>Group</td> 	
<td>A group of nodes must reply within specified time window, each node selects a random time slot </td> 	
</tr>
<td>4</td> 	
<td>PING</td> 	
<td>Node</td> 	
<td>Request node acknowledge</td> 	
</tr>
<tr>
<td>5</td> 	
<td>SET_ID</td> 	
<td>Node</td> 	
<td>Set permanent node ID</td> 	
</tr>
<tr>
<td>6</td> 	
<td>BOOT</td> 	
<td>All nodes</td> 	
<td>Reset selected node and put it into boot mode, other nodes go into standby mode</td> 	
</tr>
<tr>
<td>7</td> 	
<td>BEEP</td> 	
<td>Node</td> 	
<td>Beep and LED flash to identify the node</td> 	
</tr>
<tr>
<td>8</td> 	
<td>RD_DESCR</td> 	
<td>Node</td> 	
<td>Read node description </td> 	
</tr>
<tr>
<td>9</td> 	
<td>WR_DESCR</td> 	
<td>Node</td> 	
<td>Write node description</td> 	
</tr>
</tbody></table>

## [1] REV

Request from Node A to Node B

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x01</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>0</th>
</tbody></table>
 
 Reply from Node B to Node A

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<th>[8]</th>
<th>[9]</th>
<th>[10]</th>
<th>[11]</th>
<th>[12]</th>
<th>[13]</th>
<th>[14]</th>
<th>[15]</th>
</tr>
</thead>
<tbody>
<th>0x81</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>OkErr</th>
<th>DevType</th>
<th>DevModel</th>
<th>HwRevMaj</th>
<th>HwRevMin</th>
<th>BootRevMaj</th>
<th>BootRevMin</th>
<th>SwRevMaj</th>
<th>SwRevMin</th>
</tbody></table>

## [2] STATUS

Request from Node A to Node B

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x02</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>0</th>
</tbody></table>
 
 Reply from Node B to Node A

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<th>[8:N]</th>
</tr>
</thead>
<tbody>
<th>0x82</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>OkErr</th>
<th>Content depends on DevType and DevModel</th>
</tbody></table>

## [3] COLLECT

Request from node A to a Group

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3]</th>
<th>[4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x03</th>
<th>NodeA_ID</th>
<th>Group</th>
<th>Slots</th>
<th>MsgID</th>
<th>0</th>
</tbody></table>
 
  * Group - defines a group of nodes. The following groups defined so far:
    * 1 - all nodes
    * 2 - nodes with temporary ID
    * 3 - nodes with permanent ID
  * Slots - is number of 10ms time slots. Device should output its reply in a randomly selected time slot within specified number of slots. Number of slots must be in the range [4..63]

 Reply from a Group member Node B to Node A

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x83</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>0</th>
</tbody></table>

## [4] PING

Request from Node A to Node B

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x04</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>Param</th>
</tbody></table>
 
 Param - time interval, seconds. During that interval target node should not respond to the COLLECT command. 
 
 Reply from Node B to Node A

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x84</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>OkErr</th>
</tbody></table>

## [5] SET_ID

Node ID is used for point-to-point node addressing. Node ID is a 16-bit number.

If a node does not have permanent ID, it assigns itself a random temporary ID of 0xFyyy, where y - a hex digit. During configuration stage permanent IDs should be assigned to all nodes. Permanent IDs start from any hex digit other than 0xF. 

Request from Node A to Node B

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<th>[8:9]</th>
</tr>
</thead>
<tbody>
<th>0x05</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>0</th>
<th>New_ID</th>
</tbody></table>
 
 New_ID - a new ID for node B.
 
 Reply from Node B to Node A

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x85</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>OkErr</th>
</tbody></table>

If OkErr = 0 then NodeB_ID is the New_ID, otherwise NodeB_ID is the old node ID value.

## [6] BOOT

Request from Node A to Node B

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x06</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>Pause</th>
</tbody></table>
 
Reply from Node B to Node A

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x86</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>OkErr</th>
</tbody></table>

After sending reply the addressed node resets and starts its bootloader.

After receiving reply 0x86 other nodes must go into standby mode for Pause seconds. During that interval they should ignore all bus traffic and should not talk to the bus. 

## [7] BEEP

Request beep and LED flash to physically identify the node. 

Request from Node A to Node B

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x07</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>Duration</th>
</tbody></table>
 
Duration specifies duration of the beep, sec.
 
Reply from Node B to Node A

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x87</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>OkErr</th>
</tbody></table>

## [8] RD_DESCR

Read text description of the target node, such as name, location, etc.

Request from Node A to Node B

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x08</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>0</th>
</tbody></table>
  
Reply from Node B to Node A

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<th>[8]</th>
<th>[9:(9+N)]</th>
</tr>
</thead>
<tbody>
<th>0x88</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>0</th>
<th>N</th>
<th>Text</th>
</tbody></table>

  * N - length of text, typically up to 63 bytes 
  * Text - node description, UTF-8

## [9] WR_DESCR

Write text description to the target node.

Request from Node A to Node B

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<th>[8]</th>
<th>[9:(9+N)]</th>
</tr>
</thead>
<tbody>
<th>0x09</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>0</th>
<th>N</th>
<th>Text</th>
</tbody></table>

  * N - length of text, typically up to 63 bytes 
  * Text - node description, UTF-8

Reply from Node B to Node A

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr>
</thead>
<tbody>
<th>0x89</th>
<th>NodeA_ID</th>
<th>NodeB_ID</th>
<th>MsgID</th>
<th>OkErr</th>
</tbody></table>

# MQTT-SN mode (broadcast messages)

In that mode messages are made compatible to [MQTT for Sensor Networks – MQTT-SN](http://mqtt.org/documentation). However, HBus does not require MQTT broker. In a network segment all local messages are available for all nodes. It is up to node to select messages of interest from the stream.  But compatibility with MQTT-SN simplifies gateways software and eases integration with IoT.

## Message format

<table>
<thead>
<tr>
<th>Len</th>
<th>MsgType</th>
<th>Variable part</th>
</tr>
</thead>
<tbody>
<th>1 byte</th>
<th>1 byte</th>
<th>Len-2 bytes</th>
</tbody></table>

Message length Len should be in the range [8..136]. 

## MsgType

In simplest case only PUBLISH command is required.

<table>
<thead>
<tr>
<th>MsgType</th>
<th>Name</th>
<th>MsgType</th>
<th>Name</th>
</tr>
</thead>
<tbody>
<th>0x00</th>
<th>ADVERTISE</th>
<th>0x01</th>
<th>SEARCHGW</th>
</tbody>
<tbody>
<th>0x02</th>
<th>GWINFO</th>
<th>0x03</th>
<th>reserved</th>
</tbody>
<tbody>
<th>0x04</th>
<th>CONNECT</th>
<th>0x05</th>
<th>CONNACK</th>
</tbody>
<tbody>
<th>0x06</th>
<th>WILLTOPICREQ</th>
<th>0x07</th>
<th>WILLTOPIC</th>
</tbody>
<tbody>
<th>0x08</th>
<th>WILLMSGREQ</th>
<th>0x09</th>
<th>WILLMSG</th>
</tbody>
<tbody>
<th>0x0A</th>
<th>REGISTER</th>
<th>0x0B</th>
<th>REGACK</th>
</tbody>
<tbody>
<th>0x0C</th>
<th>PUBLISH</th>
<th>0x0D</th>
<th>PUBACK</th>
</tbody>
<tbody>
<th>0x0E</th>
<th>PUBCOMP</th>
<th>0x0F</th>
<th>PUBREC</th>
</tbody>
<tbody>
<th>0x10</th>
<th>PUBREL</th>
<th>0x11</th>
<th>reserved</th>
</tbody>
<tbody>
<th>0x12</th>
<th>SUBSCRIBE</th>
<th>0x13</th>
<th>SUBACK</th>
</tbody>
<tbody>
<th>0x14</th>
<th>UNSUBSCRIBE</th>
<th>0x15</th>
<th>UNSUBACK</th>
</tbody>
<tbody>
<th>0x16</th>
<th>PINGREQ</th>
<th>0x17</th>
<th>PINGRESP</th>
</tbody>
<tbody>
<th>0x18</th>
<th>DISCONNECT</th>
<th>0x19</th>
<th>reserved</th>
</tbody>
<tbody>
<th>0x1A</th>
<th>WILLTOPICUPD</th>
<th>0x1B</th>
<th>WILLTOPICRESP</th>
</tbody>
<tbody>
<th>0x1C</th>
<th>WILLMSGUPD</th>
<th>0x1D</th>
<th>WILLMSGRESP</th>
</tbody>
</table>

MsgType range [0x1E:0xFF] reserved.

## [0x0C] PUBLISH

<table>
<thead>
<tr>
<th>[0]</th>
<th>[1]</th>
<th>[2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<th>[8:(LEN-1)]</th>
</tr>
</thead>
<tbody>
<th>Len</th>
<th>0x0C</th>
<th>Flags</th>
<th>TopicID</th>
<th>MsgID</th>
<th>DF</th>
<th>Data</th>
</tbody></table>

DF specifies data format: 1 = Json, 2 = MessagePack, other - TBD.
