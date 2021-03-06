# HBus Power Meter

Power meter uses two current transformers (CT). One measures total household current, another measures solar station current. 

![Pic1](https://github.com/akouz/HBus/blob/master/Devices/07_Power_Meter/Doc/Pic1.png)

Mains voltage is not measured, it assumed to be constant. However, Power Meter measures mains voltage zero-crossing (eg mains phase); it is required to calculate power balance. 

1m long 12V RGB LED strip used to indicate household power balance in the range from -2kW to +2kW.  

LED strip can be either close to the HBus Power Meter or it can be placed quite far away if LED Strip Driver used. A CAT5/CAT6 cable should be used to connect LED Strip Driver to the HBus Power Meter. Cable length can be up to 100 m. 

If LED Strip Driver not used then solder R7 and remove R6; connect LED strip to TB4 pin 1.

HBus Power Meter can work autonomously, without any other HBus device. In that case only 12Vdc 1A power supply is required. Measured power is indicated on LED strip:
- power consumed from grid indicated in red
- power exported to grid indicated in blue
- power produced and consumed locally indicated in green

Every 3 LEDs in full brightness indicate 200W of power, half brightness indicate 100W of power.

![Pic2](https://github.com/akouz/HBus/blob/master/Devices/07_Power_Meter/Doc/Pic2.jpg)

![Pic3](https://github.com/akouz/HBus/blob/master/Devices/07_Power_Meter/Doc/Pic3.jpg)

If device connected to HBus network, it also regularly broadcasts measured power in MQTT mode. With CT 2000:1  rated for 50A or 100A Power Meter can measure power in the range from -10kW to +10kW. 

HBus Power Meter employs double sided PCB, dimensions 50x70 mm. Assembled board rev 1.0 shown:

![Pic4](https://github.com/akouz/HBus/blob/master/Devices/07_Power_Meter/Doc/Power_Meter.jpg)

LED Strip Driver board 25x50 mm:

![Pic5](https://github.com/akouz/HBus/blob/master/Devices/07_Power_Meter/Doc/Strip_Driver.jpg)

Sketch rev 0.1 with NodeTest, no CT connected:

![Pic6](https://github.com/akouz/HBus/blob/master/Devices/07_Power_Meter/Doc/NodeTest.png)

Node promptly reports power changes and mains connection/disconnection. Also it regularly broadcasts power and mains presence. Please see the sketch for details. 
