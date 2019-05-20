# Sketch WiFi_Gateway.ino

Current rev 1.0

## Time service

Every hour sketch requests time from NTP server and broadcast HBus messate to default topic "time" (TopicId=1) with the following ayload

{atime:1558391178, tz:570, daysec:28578, hr:7, min:56}

where 
  * atime - UTC time, seconds since 00:00:00 of 01/01/2001
  * tz - time zone, offset in minutes from UTC
  * daysec - second since 00:00:00 of current day
  * hr - hour of the day
  * min - miute of the hour

