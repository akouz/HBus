# HBus Power Meter

Power meter uses two current transformers (CT). One measures total household current, another measures solar station current. Mains voltage is not measured, it assumed to be stable. However, device measures mains voltage phase; it is required to calculate power balance. 

1m long 12V RGB LED strip used to indicate household power balance in the range from -2kW to +2kW.  

LED strip can be either close to the HBus Power Meter or it can be placed quite far away if LED Strip Driver used. A CAT5/CAT6 cable should be used to connect LED Strip Driver to the HBus Power Meter. Cable length can be up to 100 m. 

HBus Power Meter can work autonomously, without any other HBus device. In that case only 12Vdc 1A power supply is required. Measured power is indicated on LED strip:
- power consumed from grid indicated in red
- power exported to grid indicated in blue
- power produced and consumed locally indicated in green

Every 3 LEDs in full brightness indicate 200W of power, half brightness indicate 100W of power.

![Pic1](https://github.com/akouz/HBus/blob/master/HBus_Power_Meter/PIC/1.jpg)

If device connected to HBus network, it also regularly broadcasts measured power in MQTT mode.
