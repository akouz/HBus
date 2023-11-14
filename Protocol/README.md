
# HBus protocol

Matching code:
  * [NodeTest rev 1.12](https://github.com/akouz/HBus/tree/master/NodeTest)
  * WiFi Gateway sketch [WiFiGw.ino rev 1.10](https://github.com/akouz/HBus/tree/master/Devices/03_WiFi_Gateway/WiFiGw)
  * [Arduino firmware core rev 1.1](https://github.com/akouz/HBus/tree/master/HBnodeMiniPro)

## Byte-stuffing

Code 0x1B (eg ESC symbol) marks the beginning of a 2-byte sequence. The following byte pairs are defined: 

  * 0x1B-0x02 	- start of HBus frame (SOF)
  * 0x1B-0x03 	- start of MQTT-SN frame (SOF)
  * 0x1B-0x04 	- start of encrypted HBus frame (SOF)
  * 0x1B-0x05 	- start of encrypted MQTT-SN frame (SOF)
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
<td>12...136 bytes</td>
<td>2 bytes</td>
<td>2 bytes</td>
</tr><tr>
<td>priority</td>
<td>(0x1B-0x02) or (0x1B-0x04)</td>
<td>HBus message content</td>
<td>CRC</td>
<td>0x1B-0x07</td>
</tr><tr>
<td>priority</td>
<td>(0x1B-0x03) or (0x1B-0x05)</td>
<td>MQTT message content</td>
<td>CRC</td>
<td>0x1B-0x07</td>
</tr></tbody></table>

Prefix used to wake-up CAN receivers from standby mode. Also it is used for early collision detection. If echo does not match the sent byte then sender must switch off its transmitter and wait until bus is free. 

  * Sender can start transmission if there were no traffic on bus for 2 ms or more.
  * Sender starts transmission with Prefix byte. Its value depends on message priority:
    * 0xFF -	Low priority message
    * 0xFC -	Medium priority message 
    * 0xF0 -	High priority message

CRC covers message content, Prefix and SOF exluded. Sender calculates CRC before byte-stuffing added to the message. Receiver calculates CRC after byte-stuffing removed from the received message. 

While debugging nodes, it is possible to transfer text messages duirng the pause from EOF to Prefix. In normal operation all debugging traffic should be disabled.

## Encryption

Cipher is a combination of XTEA block cipher and LFSR-32 stream cipher. 

First 8 bytes of message are encrypted by XTEA cipher. While encrypting the block, an intermediate value is used to initialise 32-bit LFSR. The rest of the message is encrypted by LFSR stream cipher. Cipher encrypts both message content and its CRC.

Decryption is made similarly. First 8 bytes of the message decrypted by XTEA block cipher, an intermediate value is used to initialise 32-bit LFSR. The rest of the message, including CRC, is decrypted by 32-bit LFSR stream cypher.

XTEA uses a 128-bit key. The key is combined from two parts: EEPROM key and flash key. Flash key defined at compile time, it should be unique for the user. EEPROM key defined during node configuration, it should be unique for the particular project.

At power-up node reads EEPROM key and encrypts it using its flash key. The result is used as XTEA key to encrypt HBus and MQTT-SN messages.

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
<td>MsgId</td> 	
<td>8</td> 	
<td>Message ID; it is incremented with every request; reply repeats MsgId of the request. Valid range 0x01…0xFE</td> 	
</tr><tr>
<td>Nonce</td> 	
<td>8</td> 	
<td>random number</td> 	
</tr><tr>
<td>RdWr</td> 	
<td>8</td> 	
<td>Read/write operation: 0 = read, 1 = write</td> 	
</tr><tr>
<td>OkErr</td> 	
<td>8</td> 	
<td>OK = 0; error codes must be in the range [0x80...0xFE], other codes reserved </td> 	
</tr><tr>
<td>TS</td> 	
<td>32</td> 	
<td>Time stamp, seconds since 00:00:00 of 01/01/2001 UTC, big endian</td> 	
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
<td>Node</td> 	
<td>Send a chunk of code to selected node</td> 	
</tr><tr>
<td>7</td> 	
<td>BEEP</td> 	
<td>Node</td> 	
<td>Beep and LED flash to identify the node</td> 	
</tr><tr>
<td>8</td> 	
<td>DESCR</td> 	
<td>Node</td> 	
<td>Read/write node description </td> 	
</tr><tr>
</tr><tr>
<td>9</td> 	
<td>SECURITY</td> 	
<td>Node</td> 	
<td>Set/read security settings</td> 	
</tr><tr>
<td>10</td> 	
<td>C_CMD</td> 	
<td>Node</td> 	
<td>Custom command</td> 	
</tr><tr>
<td>11</td> 	
<td>TOPIC</td> 	
<td>Node</td> 	
<td>Read TopicId and TopicName</td> 	
</tr></tbody></table>

## [1] REV

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr>
<td>0x01</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>0</td>
<td>TS</td> 
</tr></tbody></table>
 
A sample of request from node 0x0404 to node 0x0010:
``` 
01 04 04 00 10 D2 8F 00  23 2C DC 9E 
```
Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<td>[12]</td>
<td>[13]</td>
<td>[14]</td>
<td>[15]</td>
<td>[16]</td>
<td>[17]</td>
<td>[18]</td>
<td>[19]</td>
<td>[20]</td>
<td>[21]</td>
</tr></thead>
<tbody><tr>
<td>0x81</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>OkErr</td>
<td>TS</td>
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

A sample of reply from node 0x0010 to node 0x0404
```
81 04 04 00 10 D2 69 00  23 2C DC AE 02 01 01 00  00 01 01 0A 01 00 
```
Please note a 16 sec mismatch between TS of node 0x0404 (0x232CDC9E from NodeTest on PC) and TS of node 0x0010 (0x232CDCAE from HBus Gateway). Unfortunatelly PC time is not accurate. 

## [2] STATUS

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr> 
<td>0x02</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>0</td>
<td>TS</td>
</tr></tbody></table>

A sample of request from node 0x0404 to node 0x0010:
```
02 04 04 00 10 2C E3 00  23 2C EB AE 
```
 
 Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<td>[12:N]</td>
</tr></thead>
<tbody><tr> 
<td>0x82</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>DF</td>
<td>TS</td>
<td>Data, content depends on DevType and DevModel</td>
</tr></tbody></table>

 DF is data format: 
  * 0 = binary 
  * 1 = [JSON](https://www.json.org/) 
  * 2 = [MessagePack](https://github.com/msgpack/msgpack/blob/master/spec.md) 
  * other - TBD.

A sample of reply from node 0x0010 to node 0x0404
```
82 04 04 00 10 2C C9 01  23 2C EB BF {tid:[512,513,514,515,516,517], val:[0,14.01,23.02,1020.15,52.53,845.82]}
```

## [3] COLLECT

Request from node A to a Group

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3]</th>
<th>[4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr> 
<td>0x03</td>
<td>NodeA_ID</td>
<td>Group</td>
<td>Slots</td>
<td>MsgId</td>
<td>Nonce</td>
<td>0</td>
<th>TS</th>
</tr></tbody></table>
 
  * Group - defines a group of nodes. The following groups defined so far:
    * 1 - all nodes
    * 2 - nodes with temporary ID
    * 3 - nodes with permanent ID
  * Slots - is number of 10ms time slots. Device should output its reply in a randomly selected time slot within specified number of slots. Number of slots must be in the range [4..63]

A sample of request from node 0x0404 to group 1 (all nodes), 128 slots:
```
03 04 04 01 80 2F 4C 00  23 2C FC 6B 
```
 Reply from a Group member Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr>
<td>0x83</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>0</td>
<th>TS</th>
</tr></tbody></table>

A sample of reply from node 0x0010:
```
83 04 04 00 10 2F 76 00  23 2C FC 7B 
```

## [4] PING

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr>
<td>0x04</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>Param</td>
<th>TS</th>
</tr></tbody></table>
 
 Param - time interval, seconds. During that interval target node should not respond to the COLLECT command. 
 
A sample of request from node 0x0404 to node 0x0010:
```
04 04 04 00 10 30 0B 0A  23 2C FD B9 
```
 Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr>
 <td>0x84</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>OkErr</td>
<th>TS</th>
</tr></tbody></table>

A sample of reply from node 0x0010 to node 0x0404
```
84 04 04 00 10 30 08 00  23 2C FD C9 
```

## [5] SET_ID

Node ID is used for point-to-point node addressing. Node ID is a 16-bit number.

If a node does not have permanent ID, it assigns itself a random temporary ID of 0xFyyy, where y - a hex digit. During configuration stage permanent IDs should be assigned to all nodes. Permanent IDs start from any hex digit other than 0xF. 

It is recommended to limit permanent ID by the range [0x0001..0x07FF] (eg by the range [1..2047]).

Permanent ID can be assigned only once. Node will refuse to re-write permanent Node ID.

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<td>[12:13]</td>
</tr></thead>
<tbody><tr>
<td>0x05</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>1</td>
<th>TS</th>
<td>New_ID</td>
</tr></tbody></table>
 
 New_ID - a new ID for node B.
 
 Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr>
<td>0x85</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>OkErr</td>
<th>TS</th>
</tr></tbody></table>

If OkErr = 0 then NodeB_ID is the New_ID, otherwise NodeB_ID is the old node ID value.

## [6] BOOT

Request from Node A to Node B. Node B should write the received chunk of code into its EEPROM at specified address. Field Code holds binary data in the same format as [Intel HEX](https://en.wikipedia.org/wiki/Intel_HEX) without colon char.  

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<th>[12:N]</th>
</tr></thead>
<tbody><tr>
<td>0x06</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>0</td>
<th>TS</th>
<th>Code</th>
</tr></tbody></table>
 
Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr>
<td>0x86</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>OkErr</td>
<th>TS</th>
</tr></tbody></table>

## [7] BEEP

Request beep and LED flash to physically identify the node. 

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr>
<td>0x07</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>Duration</td>
<th>TS</th>
</tr></tbody></table>
 
Duration specifies duration of the beep, sec.
 
Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr>
<td>0x87</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>OkErr</td>
<th>TS</th>
</tr></tbody></table>

## [8] DESCR

Read/write target node text description, such as name, location, etc.

Read description, request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr>
<td>0x08</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>0</td>
<th>TS</th>
</tr></tbody></table>
  
Reply from Node B to Node A

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<th>[12]</th>
<th>[13:(13+N)]</th>
</tr></thead>
<tbody><tr>
<td>0x88</th>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>OkErr</td>
<th>TS</th>
<td>N</td>
<td>Text</td>
</tr></tbody></table>

  * N - length of text, typically up to 63 bytes 
  * Text - node description, UTF-8

Write description, request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<th>[12]</th>
<th>[13:(13+N)]</th>
</tr></thead>
<tbody><tr>
<td>0x08</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>1</td>
<th>TS</th>
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
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr>
<td>0x88</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>OkErr</td>
<th>TS</th>
</tr></tbody></table>

## [9] SECURITY

Assign/query node security settings

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<th>[12:13]</th>
<th>[14:21]</th>
</tr></thead>
<tbody><tr>
<td>0x09</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>RdWr</td>
<th>TS</th>
<td>Security</td>
<td>EEPROM Key</td>
</tr></tbody></table>

Reply from Node B to Node A:

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<th>[12:13]</th>
</tr></thead>
<tbody><tr>
<td>0x89</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>OkErr</td>
<th>TS</th>
<td>Security</td>
</tr></tbody></table>

8-byte long EEPROM Key field is optional. EEPROM Key can be written only once when EEPROM is blank. Once assigned, it cannot be rewritten, field value is ignored.

## [10] C_CMD

Optional custom command to HBus node. Content depends on device type and model, software revision, etc. For example, it can be a calibration command, etc.

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<th>[12:(13+N)]</th>
</tr></thead>
<tbody><tr>
<td>0x0A</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>DF</td>
<th>TS</th>
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
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<th>[12:(12+X)]</th>
</tr></thead>
<tbody><tr>
<td>0x8A</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>OkErr</td>
<th>TS</th>
<td>Rply</td>
</tr></tbody></table>

Rply  is an optional reply.

## [11] TOPIC

Read one of TopicId and TopicName stored by node. 

Request from Node A to Node B

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr>
<td>0x0B</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>ti</td>
<th>TS</th>
</tr></tbody></table>

  * ti - topic index.

Reply from Node B to Node A if topic exists:

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<th>[12:13]</th>
<th>[14:(14+N)]</th>
</tr></thead>
<tbody><tr>
<td>0x8B</th>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>0</td>
<th>TS</th>
<td>TopicId</td>
<td>TopicName</td>
</tr></tbody></table>

  * TopicName is UTF-8 string less than 64 bytes long

Reply from Node B to Node A if topic index is out of list:

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
</tr></thead>
<tbody><tr>
<td>0x8B</td>
<td>NodeA_ID</td>
<td>NodeB_ID</td>
<td>MsgId</td>
<td>Nonce</td>
<td>0xEE</td>
<th>TS</th>
</tr></tbody></table>

# MQTT-SN mode (broadcast messages)

## Message structure
In that mode messages are made similar to [MQTT for Sensor Networks – MQTT-SN](https://www.oasis-open.org/committees/document.php?document_id=66091&wg_abbrev=mqtt). However, HBus does not require MQTT broker. In a network segment all local messages are available for all nodes. It is up to node to select messages of interest from the stream.  

Message structure is as follows:

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<th>[12:N]</th>
</tr></thead>
<tbody><tr>
<td>MsgType</td>
<td>NodeID</td>
<td>TopicId</td>
<td>MsgId</td>
<td>Nonce</td>
<td>DF</td>
<th>TS</th>
<td>Data</td>
</tr></tbody></table>

  * MsgType - four msb bits are random; four lsb bits of that byte is message type as per [MQTT-SN](https://www.oasis-open.org/committees/document.php?document_id=66091&wg_abbrev=mqtt):
    * 0x0A - REGISTER
    * 0x0C - PUBLISH
  * NodeId - ID of the broadcasting node; used for debug and monitoring.
  * TopicId - MQTT-SN topic ID, 16-bit unsigned integer.
  * MsgId - message ID, all MQTT messages should use common MsgId incremented with every broadcasted message.
  * DF is data format: 
    * 0 = binary 
    * 1 = [JSON](https://www.json.org/) 
    * 2 = [MessagePack](https://github.com/msgpack/msgpack/blob/master/spec.md) 
    * other - TBD.
    
## REGISTER

Binds TopicId and TopicName.
  * If (TopicId == 0) then it is a request. Any node having correct binding of TopicId with TopicName should reply with another REGISTER message where  TopicId is not 0.
  * If (TopicId > 0) and (TopicId < 0xFFFF) then all nodes with matching TopicName should bind the specified TopicId with it. 
  * If (TopicId >= 0xFFFF) then related TopicId is cleared.

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<th>[12:N]</th>
</tr></thead>
<tbody><tr>
<td>0x0A</td>
<td>NodeID</td>
<td>TopicId</td>
<td>MsgId</td>
<td>Nonce</td>
<td>0</td>
<th>TS</th>
<td>TopicName</td>
</tr></tbody></table>

  * TopicName is UTF-8 string less than 64 bytes long

Typically nodes are pre-programmed with TopicNames, but all TopicIds set to 0. After power-up nodes request TopicId for every TopicName they have. If there is no answer, nodes propose a TopicId derived from their NodeId:

  * TopicId = (NodeId << 5) | TopicIndex
  
Thus, every node can assign up to 32 unique TopicId.

TopicId in the range [0x0000...0x001F] cannot be assigned by nodes because NodeId=0 is illegal. In that range TopicName and TopicId are pre-defined. Only few pairs defined so far, other values reserved for future use:
  * TopicName="time", TopicId = 0x0001, UTC time (seconds since 00:00:00 of 01/01/2001), time of the day (seconds since 00:00:00, hour, minute)
  * TopicName="timezone", TopicId = 0x0002, local time zone, offset from UTC in minutes
  * TopicName="debug", TopicId = 0x000D, debug messages
  * TopicName="error", TopicId = 0x000E, software errors
  * TopicName="failure", TopicId = 0x000F, hardware failures

## PUBLISH

Broadcast Payload to specified TopicId.

<table>
<thead><tr>
<th>[0]</th>
<th>[1:2]</th>
<th>[3:4]</th>
<th>[5]</th>
<th>[6]</th>
<th>[7]</th>
<th>[8:11]</th>
<th>[12:N]</th>
</tr></thead>
<tbody><tr>
<td>0x0C</td>
<td>NodeID</td>
<td>TopicId</td>
<td>MsgId</td>
<td>Nonce</td>
<td>DF</td>
<th>TS</th>
<td>Payload</td>
</tr></tbody></table>

A sample of pre-defined topic "time" message (header shown  in hex, payload in ASCII):
```
4C 00 10 00 01 02 FA 01  23 2C A3 69 {atime:590127977, tz:600, daysec:51977, hr:14, min:26}
```

  * 4C - high nibble 0x4 is a random value; low nibble 0xC is PUBLISH code
  * 00 10 - message issued by NodeID = 0x0010
  * 00 01 - TopicID=1, it is pre-defined topic "time"
  * 02 - MsgId = 0x02
  * FA - Nonce = 0xFA, random value
  * 01 - DF=1, JSON payload
  * 23 2C A3 69 - timestamp, hex number of seconds since 00:00:00 01/01/2001 UTC
  * atime - absolute time, decimal number of seconds since 00:00:00 01/01/2001 UTC
  * tz - time zone, offset 600 min from UTC, it is 10hr 00 min, eg Australia East Coast time zone (Sydney/Melbourne/Brisbane)
  * daysec - number of seconds since midnight, local time zone
  * hr - day hour,  local time zone
  * min - minute of the hour,  local time zone
