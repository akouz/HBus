 # Sketch White LED strip
 
Sketch combines functions of light sensor, movement detector and outside light with timer:
  * In the evening it set LED strip on for 4 hours at half brightness. PWM frequency 125 Hz.
  * If a movement detected while it is dark then LED strip is set to full brightness for 1 min; it is retriggered by every detected movement
  
Apart from "visual" functions, sketch sends the following report to HBus:
  * topic 1200 - relative light level, less than 0.9 is dark, greater than 1.0 is bright, value greater than 1.1 reloads the 4-hour timer
  * topic 1300 - movement detector, value reflects number of seconds before timer will be set off
  * topic 2000 - LED strip light level,  value in the range 0...1.0 presents PWM output to the LED strip
  * topic 500 - integrity check: set to 1.0 if there is no MOSFET voltage detected, or to 0 if MOSFET voltage is OK

Topics reported promply on change, as well as merely updated every 10 min. 

By setting topic 2000 value, it is possdible to control LED strip remotely.
