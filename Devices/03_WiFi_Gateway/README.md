# HBus WiFi Gateway

  * Based on Wemos Lolin D1
  * Step-down converter, input voltage range 8...30 Vdc  
  * Optional HBus Arbiter U2 for CSMA/CA
  * Optional temperature+humidity+pressure sensor [BME280](https://www.ebay.com.au/sch/i.html?_osacat=0&_odkw=BMP280&_from=R40&_trksid=p2334524.m570.l1313.TR3.TRC2.A0.H0.XBME280.TRS0&_nkw=BME280&_sacat=0)
  * Double sided PCB, 50x80 mm
  
  ![Pic1](https://github.com/akouz/HBus/blob/master/Devices/03_WiFi_Gateway/WiFi_Gateway.jpg)

Assembled Gateway without Arbiter.

Warning: for correct UART operation, [Wemos D1](https://wiki.wemos.cc/products:d1:d1_mini) must be reworked:
  * unsolder and lift U3 pin 2 from PCB (eg CH340C pin Tx)
  * add a resistor 3.3 kOhm in series with U3 pin 2
  
Reworked Wemos D1 still can be programmed via USB; just take it off the sokets and put it back when programmed.
  
![Pic2](https://github.com/akouz/HBus/blob/master/Devices/03_WiFi_Gateway/rework.jpg)

Resistor 3.3 kOhm, size 0603, soldered between pin 2 and PCB
