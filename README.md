# HBus

  * [HBus mode (config and control messages)](https://github.com/akouz/HBus#hbus-mode-config-and-control-messages)
  * [MQTT mode (broadcast messages)](https://github.com/akouz/HBus#mqtt-sn-mode-broadcast-messages)
  * [NodeTest](https://github.com/akouz/HBus#nodetest)
 
## Introduction

HBus is a wired home automation interface. Nodes made as simple and as cheap as possible. Minimum requirement - Arduino Pro Mini (Atmega 328P) with 78L05 regulator and a CAN transceiver. Once configured, system is self-contained, eg it can operate without any host or server. 

## Basics

  * Baud rate 19.2 kbps.
  * Communication by ordinary UARTs with CAN transceivers, dominant/recessive states. CAN protocol not used, CAN controllers not required.
  * CSMA/CD (collision detection) allowed for low-cost implementations. CSMA/CA (collision avoidance) recommended, but is not mandatory. CSMA/CA employs an additional low-cost microcontroller. 
  * Flow control by byte-stuffing.
  * Two modes of operation:
    * Configuration, point-to-point messages, master/slave communication model - HBus mode.
    * Operation, broadcast messages, producer/consumer communication model - MQTT mode. 
  * Messages covered by CRC-16-CCITT.  

## Wiring

Wiring made by Cat5/Cat6 cables. Twisted pairs used as follows:
   * (orange + orange/white) = HBus signal:
     * orange - HBH (HBus High)
     * orange/white - HBL (HBus Low)
   * (blue + blue/white) = common
   * (brown + brown/white) = supply +12V
   * (green + green/white) = not used.

Free topology is allowed. Total length of cables should not exceed 500 m. A single terminating resistor 100 Ohm should be placed somwhere in a middle of the network.

Cat5/Cat6 cables have limited current carrying capability. To prevent fire in case of short circuit, power supply +12V connected to the bus should have current limit at no more than 2A. In simplest case use a 1A...2A fuse. 

More expensive EIB/KNX cable can be used too.

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

# HBus protocol

Current firmware revision 0.6.

## Byte-stuffing

Code 0x1B (eg ESC symbol) marks the beginning of a 2-byte sequence. The following byte pairs are defined: 

  * 0x1B-0x02 	- start of HBus frame (SOF)
  * 0x1B-0x03 	- start of MQTT-SN frame (SOF)
  * 0x1B-0x07 	- end of frame (EOF)
  * 0x1B-0x08 	- insert 0x1B into data flow
  * 0x1B-0x09 	- insert 0x1B, 0x1B into data flow

## Frame structure

<table>
<thead><tr>
<th>Prefix</th>
<th>SOF</th>
<th>Message</th>
<th>CRC</th>
<th>EOF</th>
</tr></thead>
<tbody><tr>
<td>1 byte</td>
<td>2 bytes</td>
<td>8...136 bytes</td>
<td>2 bytes</td>
<td>2 bytes</td>
</tr><tr>
<td>priority</td>
<td>0x1B-0x02</td>
<td>HBus message content</td>
<td>CRC</td>
<td>0x1B-0x07</td>
</tr><tr>
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

CRC covers message content, Prefix and SOF exluded. Sender calculates CRC before byte-stuffing added to the message. Receiver calculates CRC after byte-stuffing removed from the received message. 

While debugging nodes, it is possible to transfer text messages duirng the pause from EOF to Prefix. In normal operation all debugging traffic should be disabled.

## Fields

Big endian used, eg MSB byte sent first, LSB byte sent last.

<table>
<thead><tr>
<th>Name</th>
<th>Bits</th>
<th>Description</th>
</tr></thead>
<tbody><tr>
<td>NodeA_ID</td> 	
<td>16</td> 	
<td>ID of Node A, it must be unique in current network </td> 	
</tr><tr>
<td>NodeB_ID</td> 	
<td>16</td> 	
<td>ID of Node B, it must be unique in current network </td> 	
</tr><tr>
<td>MsgID</td> 	
<td>16</td> 	
<td>Message ID; it is incremented with every request; reply repeats MsgID of the request. Valid range 0x0001…0xFFFE</td> 	
</tr><tr>
<td>OkErr</td> 	
<td>8</td> 	
<td>OK = 0; error codes must be in the range [0x80...0xFE], other codes reserved </td> 	
</tr></tbody></table>

# HBus mode (config and control messages)

## List of HBus commands

<table>
<thead><tr>
<th>Command</th>
<th>Name</th>
<th>Target</th>
<th>Description</th>
</tr></thead>
<tbody><tr>
<td>1</td> 	
<td>REV</td> 	
<td>Node</td> 	
<td>Read node description, hardware and software revisions </td> 	
</tr><tr>
<td>2</td> 	
<td>STATUS</td> 	
<td>Node</td> 	
<td>Read node status</td> 	
</tr><tr>
<td>3</td> 	
<td>COLLECT</td> 	
<td>Group</td> 	
<td>A group of nodes must reply within specified time window, each node selects a random time slot </td> 	
</tr><tr>
<td>4</td> 	
<td>PING</td> 	
<td>Node</td> 	
<td>Request node acknowledge</td> 	
</tr><tr>
<td>5</td> 	
<td>SET_ID</td> 	
<td>Node</td> 	
<td>Set permanent node ID</td> 	
</tr><tr>
<td>6</td> 	
<td>BOOT</td> 	
<td>All nodes</td> 	
<td>Reset selected node and put it into boot mode, other nodes go into standby mode</td> 	
</tr><tr>
<td>7</td> 	
<td>BEEP</td> 	
<td>Node</td> 	
<td>Beep and LED flash to identify the node</td> 	
</tr><tr>
<td>8</td> 	
<td>RD_DESCR</td> 	
<td>Node</td> 	
<td>Read node description </td> 	
</tr><tr>
<td>9</td> 	
<td>WR_DESCR</td> 	
<td>Node</td> 	
<td>Write node description</td> 	
</tr><tr>
<td>10</td> 	
<td>C_CMD</td> 	
<td>Node</td> 	
<td>Custom command</td> 	
</tr></tbody></table>

## [1] REV

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr>
<td>0x01</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>0</td>
</tr></tbody></table>
 
 Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<td>[8]</td>
<td>[9]</td>
<td>[10]</td>
<td>[11]</td>
<td>[12]</td>
<td>[13]</td>
<td>[14]</td>
<td>[15]</td>
<td>[16]</td>
<td>[17]</td>
</tr></thead>
<tbody><tr>
<td>0x81</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>OkErr</td>
<td>DevType</td>
<td>DevModel</td>
<td>HwRevMaj</td>
<td>HwRevMin</td>
<td>BootRevMaj</td>
<td>BootRevMin</td>
<td>SwRevMaj</td>
<td>SwRevMin</td>
<td>HbusRevMaj</td>
<td>HbusRevMin</td>
</tr></tbody></table>

## [2] STATUS

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr> 
<td>0x02</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>0</td>
</tr></tbody></table>
 
 Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<td>[8:N]</td>
</tr></thead>
<tbody><tr> 
<td>0x82</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>DF</td>
<td>Data, content depends on DevType and DevModel</td>
</tr></tbody></table>

 DF is data format: 
  * 0 = binary 
  * 1 = [JSON](https://www.json.org/) 
  * 2 = [MessagePack](https://github.com/msgpack/msgpack/blob/master/spec.md) 
  * other - TBD.

## [3] COLLECT

Request from node A to a Group

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3]</th>
<th>[4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr> 
<td>0x03</td>
<td>NodeA_ID</td>
<td>Group</td>
<td>Slots</td>
<td>MsgID</td>
<td>0</td>
</tr></tbody></table>
 
  * Group - defines a group of nodes. The following groups defined so far:
    * 1 - all nodes
    * 2 - nodes with temporary ID
    * 3 - nodes with permanent ID
  * Slots - is number of 10ms time slots. Device should output its reply in a randomly selected time slot within specified number of slots. Number of slots must be in the range [4..63]

 Reply from a Group member Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr>
<td>0x83</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>0</td>
</tr></tbody></table>

## [4] PING

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr>
<td>0x04</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>Param</td>
</tr></tbody></table>
 
 Param - time interval, seconds. During that interval target node should not respond to the COLLECT command. 
 
 Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr>
 <td>0x84</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>OkErr</td>
</tr></tbody></table>

## [5] SET_ID

Node ID is used for point-to-point node addressing. Node ID is a 16-bit number.

If a node does not have permanent ID, it assigns itself a random temporary ID of 0xFyyy, where y - a hex digit. During configuration stage permanent IDs should be assigned to all nodes. Permanent IDs start from any hex digit other than 0xF. 

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<td>[8:9]</td>
</tr></thead>
<tbody><tr>
<td>0x05</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>0</td>
<td>New_ID</td>
</tr></tbody></table>
 
 New_ID - a new ID for node B.
 
 Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr>
<td>0x85</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>OkErr</td>
</tr></tbody></table>

If OkErr = 0 then NodeB_ID is the New_ID, otherwise NodeB_ID is the old node ID value.

## [6] BOOT

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr>
<td>0x06</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>Pause</td>
</tr></tbody></table>
 
Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr>
<td>0x86</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>OkErr</td>
</tr></tbody></table>

After sending reply the addressed node resets and starts its bootloader.

After receiving reply 0x86 other nodes must go into standby mode for Pause seconds. During that interval they should ignore all bus traffic and should not talk to the bus. 

## [7] BEEP

Request beep and LED flash to physically identify the node. 

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr>
<td>0x07</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>Duration</td>
</tr></tbody></table>
 
Duration specifies duration of the beep, sec.
 
Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr>
<td>0x87</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>OkErr</td>
</tr></tbody></table>

## [8] RD_DESCR

Read text description of the target node, such as name, location, etc.

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr>
<td>0x08</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>0</td>
</tr></tbody></table>
  
Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<th>[8]</th>
<th>[9:(9+N)]</th>
</tr></thead>
<tbody><tr>
<td>0x88</th>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>0</td>
<td>N</td>
<td>Text</td>
</tr></tbody></table>

  * N - length of text, typically up to 63 bytes 
  * Text - node description, UTF-8

## [9] WR_DESCR

Write text description to the target node.

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<th>[8]</th>
<th>[9:(9+N)]</th>
</tr></thead>
<tbody><tr>
<td>0x09</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>0</td>
<td>N</td>
<td>Text</td>
</tr></tbody></table>

  * N - length of text, typically up to 63 bytes 
  * Text - node description, UTF-8

Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
</tr></thead>
<tbody><tr>
<td>0x89</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>OkErr</td>
</tr></tbody></table>

## [10] C_CMD

Optional custom command to HBus node. Content depends on device type and model, software revision, etc. For example, it can be a calibration command, etc.

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<th>[8:(8+N)]</th>
</tr></thead>
<tbody><tr>
<td>0x0A</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>DF</td>
<td>Cmd</td>
<td></td>
</tr></tbody></table>

  * DF is data format:
    * 0 = binary
    * 1 = JSON
    * 2 = MessagePack
    * other - TBD.
  * Cmd - custom command itself

Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<th>[8:(8+X)]</th>
</tr></thead>
<tbody><tr>
<td>0x89</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgID</td>
<td>OkErr</td>
<td>Rply</td>
</tr></tbody></table>

Rply  is an optional reply.

# MQTT mode (broadcast messages)

In that mode messages are made similar to [MQTT for Sensor Networks – MQTT-SN](http://mqtt.org/documentation). However, HBus does not require MQTT broker. In a network segment all local messages are available for all nodes. It is up to node to select messages of interest from the stream.  

Defined message functionally is similar to MQTT-SN PUBLISH message, see MQTT-SN clause 5.4.12. Message structure is as follows:

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5:6]</th>
<th>[7]</th>
<th>[8:N]</th>
</tr></thead>
<tbody><tr>
<td>Nonce</td>
<td>NodeID</td>
<td>TopicID</td>
<td>MessageID</td>
<td>DF</td>
<td>Data</td>
</tr></tbody></table>

  * Nonce - a random number
  * NodeID - ID of the broadcasting node; used for debug and monitoring.
  * TopicID - MQTT topic ID.
  * MsgID - message ID, all MQTT messages should use common ID incremented with every broadcasted message. MsgID used for network diagnostics. 
  * DF is data format: 
    * 0 = binary 
    * 1 = [JSON](https://www.json.org/) 
    * 2 = [MessagePack](https://github.com/msgpack/msgpack/blob/master/spec.md) 
    * other - TBD.
    
# NodeTest

NodeTest.exe is a Windows application to test and to configure nodes. NodeTest uses a USB-UART bridge with CAN transceiver. It could be a dedicated gaslvanically isolated [HBus USB bridge](https://github.com/akouz/HBus/tree/master/HBus_USB_Bridge), or a cheap [generic USB-UART bridge](https://www.ebay.com.au/sch/i.html?_from=R40&_trksid=m570.l1313&_nkw=usb+to+uart+bridge+board&_sacat=0&LH_TitleDesc=0&ul_noapp=true&_odkw=usb+to+uart+bridge) with a [CAN transceiver board](https://www.amazon.com/SN65HVD230-CAN-Board-Communication-Development/dp/B00KM6XMXO). 

![Pic1](https://github.com/akouz/HBus/blob/master/Doc/pic1.png)

  * After reset node issued a splash screen with ASCII text. For debug purposes, NodeTest prints all data appeared between HBus messages. If data bytes can be represented as visible ASCII chars, nodeTest prints ASCII chars, otherwise it prints hex value in square brackets. NodeTest marks these prints with " - dbg " prefix.
  * At splash screen the node reports its ID, it is 0xF058. First hex digit "F" indicates that node ID is temporary. After every reset node changes its temporary ID randomly.
  * NodeTest issues COLLECT command, its code is 0x03. NodeTest own ID os 0x2468. Replies should be within 32 time slots 10 ms each.
  * So far only one reply arrived. Reply code is 0x83. It was issued by node 0xF058 to node 0x2468.  
  
![Pic2](https://github.com/akouz/HBus/blob/master/Doc/pic2.png)

  * NodeTest issued SET_ID command (code 0x05) to node 0xF058. New ID is 0x1234.
  * Node replied OK, reply code 0x85. Its ID now is set to 0x1234. It is a permanent ID.
  
![Pic3](https://github.com/akouz/HBus/blob/master/Doc/pic3.png)

  * Double click to NodeTest list box clears it.
  * NodeTest issues REV command to node 0x1234, code 0x01. Node replies DevType 2, DevModel 1, h/w revision 0.1, bootloader revision 0.1, s/w revision 0.3.
  * NodeTest issues STATUS command, code 0x02. Node replies list of topics and list of topic values.
  * NodeTest issues BEEP command, code 0x07, with parameter 0x02. Node blinks its LED for 2 seconds.
  * NodeTest issies RD_DESCR command, code 0x08. Node replies zero length string.
  * NodeTest issies WR_DESCR command, code 0x09, with text string "Demo node", string length 9 bytes. Node replied OK.
  * NodeTest issies RD_DESCR command, code 0x08. Node replies with string "Demo node".
  
