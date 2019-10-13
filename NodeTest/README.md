# NodeTest
#### Current rev 1.12

#### Notes:
  * Compiled by Lazarus 2.0.4 with installed Cportlaz_v1.0.0 serial port component

## Scope

NodeTest.exe is a Windows application to test and to configure HBus nodes. NodeTest uses a USB-UART bridge with CAN transceiver. It could be a dedicated galvanically isolated [HBus USB bridge](https://github.com/akouz/HBus/tree/master/Devices/02_USB_Bridge), or a cheap [generic USB-UART bridge](https://www.ebay.com.au/sch/i.html?_from=R40&_trksid=m570.l1313&_nkw=usb+to+uart+bridge+board&_sacat=0&LH_TitleDesc=0&ul_noapp=true&_odkw=usb+to+uart+bridge) with a [CAN transceiver board](https://www.amazon.com/SN65HVD230-CAN-Board-Communication-Development/dp/B00KM6XMXO). 

## 1. Initial settings
  * Assign NodeTest Own ID. In the sample shown NodeTest ID is set to 0x0404. Value should be in the range 0x0001...0x07FF. 
  * Select COM port connected to HBus. 
![Pic1_1](https://github.com/akouz/HBus/blob/master/NodeTest/Doc/pic1_1.png)
  * When NodeTest closed, it stores its settings in .ini files. 

## 2. Discover HBus nodes and assign permanent ID
  * Connect HBus node to network. After reset the sample node issued a splash screen with ASCII text. For debug purposes, NodeTest prints all data appeared between HBus messages. If data bytes can be represented as visible ASCII chars, nodeTest prints ASCII chars, otherwise it prints hex value in square brackets. NodeTest marks these prints with " - dbg " prefix.
![Pic2_1](https://github.com/akouz/HBus/blob/master/NodeTest/Doc/pic2_1.png)
  * At splash screen the node reports its ID, it is 0xFBD9. First hex digit "F" indicates that node ID is temporary. After every reset node changes its temporary ID randomly.
  * NodeTest issues COLLECT command, its code is 0x03, source node ID 0x0404. Replies should be within 128 time slots 10 ms each.
  * So far only one reply arrived. Reply code is 0x83. It was issued by node 0xFBD9 to node 0x0404.  
![Pic2_2](https://github.com/akouz/HBus/blob/master/NodeTest/Doc/pic2_2.png)
  * NodeTest issued SET_ID command (code 0x05) to node 0xFBD9. New ID is 0x0021.
  * Node replied OK, reply code 0x85. Its ID now is set to 0x0021. It is a permanent ID.
![Pic2_3](https://github.com/akouz/HBus/blob/master/NodeTest/Doc/pic2_3.png)

If device has topics, it tries to retrieve topic IDs from the net. For every topic it broadcast REGISTER command with TopicID=0 and waits a reply. If nobody replies, node asssigns TopicID basing on its NodeID and then broadcasts the pair [TopicID, TopicName] by REGISTER command/

In the following sample node 0x021 requests TopicIDs for topics "test1", "test2", "test3", "test4". Nobody repies, therefore the node assignes TopicIDs as follows: "test1"=1056 (0x0420), "test2"=1057 (0x0421), "test3"=1058 (0x0422), "test3"=1059 (0x0423):

FF MQTT 1A 00 21 00 00 02 AC 01  00 00 10 C2 test1 -- <TopicId=?>
FF MQTT DA 00 21 04 20 03 93 01  00 00 10 C3 test1 -- <TopicId=1056>
FF MQTT AA 00 21 00 00 04 F6 01  00 00 10 C3 test2 -- <TopicId=?>
FF MQTT 0A 00 21 04 21 05 91 01  00 00 10 C4 test2 -- <TopicId=1057>
FF MQTT 3A 00 21 00 00 06 29 01  00 00 10 C4 test3 -- <TopicId=?>
FF MQTT 3A 00 21 04 22 07 FC 01  00 00 10 C5 test3 -- <TopicId=1058>
FF MQTT AA 00 21 00 00 08 59 01  00 00 10 C5 test4 -- <TopicId=?>
FF MQTT 2A 00 21 04 23 09 F3 01  00 00 10 C6 test4 -- <TopicId=1059>


## 3. Explore HBus node and set description
  * Double click to NodeTest list box to clear it.
  * NodeTest issues REV command to node 0x1234, code 0x01. Node replies DevType 2, DevModel 1, h/w revision 0.1, bootloader revision 0.1, s/w revision 0.1, HBus library revision 0.6
  * NodeTest issues STATUS command, code 0x02. Node replies list of topics and list of topic values. 
  * NodeTest issues BEEP command, code 0x07, with parameter 0x02. Node blinks its LED for 2 seconds.
  * NodeTest issies RD_DESCR command, code 0x08. Node replies zero length string.
  * NodeTest issies WR_DESCR command, code 0x09, with text string "Demo node", string length 9 bytes. Node replied OK.
  * NodeTest issies RD_DESCR command, code 0x08. Node replies with string "Demo node".
![Pic3_1](https://github.com/akouz/HBus/blob/master/NodeTest/Doc/pic3_1.png)
  
## 4. MQTT mode
  * Double click to NodeTest list box to clear it.
  * Select MQTT tab and issue MQTT message. Issuing node ID 0xB055, eg it is NodeTest. Topic 101=0x65, value 12.3.
  * Select HBus tab and issue STATUS command to node 0x1234.  In the reply node 0x1234 reports that its topic 101 value is 12.30.
  * Topic 101 value becomes valid. Node sketch start broadcasting its valid topic values, issuing node 0x1234, topic 101, value 12.30. 
  
![Pic4_1](https://github.com/akouz/HBus/blob/master/NodeTest/Doc/pic4_1.png)

## 5. Notes
  * You can select text in ListBox. Using Ctrl+C you can copy selected text lines to Clipboard.

  
