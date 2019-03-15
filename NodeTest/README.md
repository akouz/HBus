# NodeTest

Current revision 1.05
Note: NodeTest compiled by Lazarus 1.4.2; Cportlaz_v1.0.0 must be installed. 

## Scope

NodeTest.exe is a Windows application to test and to configure nodes. NodeTest uses a USB-UART bridge with CAN transceiver. It could be a dedicated gaslvanically isolated [HBus USB bridge](https://github.com/akouz/HBus/tree/master/HBus_USB_Bridge), or a cheap [generic USB-UART bridge](https://www.ebay.com.au/sch/i.html?_from=R40&_trksid=m570.l1313&_nkw=usb+to+uart+bridge+board&_sacat=0&LH_TitleDesc=0&ul_noapp=true&_odkw=usb+to+uart+bridge) with a [CAN transceiver board](https://www.amazon.com/SN65HVD230-CAN-Board-Communication-Development/dp/B00KM6XMXO). 

## Initial connection

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
  
