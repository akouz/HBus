# Sketch WiFi_Gateway.ino

Current rev 1.0

## WiFi connection

In WiFi_Gateway.ino enter your WiFi network credentials:

> const char* ssid     = "your_ssid";

> const char* password = "your_password";

In case of lost WiFi connection, sketch tries to restore it, see **coos_task_reconnect()**.

## MQTT broker

In WiFi_Gateway.ino enter MQTT broker credentials:

> const char* mqtt_url = "your_mqtt_broker";

> const char* mqtt_password = "mqtt_password";

In case of lost connection, sketch will try to re-connect to MQTT broker, see **coos_task_reconnect()**.

Sketch keeps MQTT connection alive by sending a short message to broker every 5 sec, see **coos_task_mqtt_ping()**.

## MQTT-SN to MQTT conversion

Gateway reads HBus MQTT-SN messages from the bus, converts them into MQTT messages and sends to the MQTT broker, see **hbus_msg_to_mqtt()** in **HBus.cpp**. Duiring conversion Gateway replaces **TopicId** by a topic name. 

The topic name of the outgoing message consists of **topic_base** and **TopicName**. List of registered **TopicId**s stored in EEPROM, list of registered **TopicName**s stored in SPIFFS. 

For example,  lets assume **topic_base**="HBus",  **TopicName**="topic0" is registered as **TopicId**=100 (or 0x0064). If a node broadcasts  HBus message **PUBLISH**:

> 0C 00 11 00 64 0E 7B 01  {val:3} 

then Gateway converts it to MQTT message with payload **{val:3}** and topic name "**HBus/topic0**" and sends it to the broker.

## MQTT to MQTT-SN conversion

Gateway is subscribed to topics **topic_base**/#. Messages from MQTT broker to that topic will be processed as follows:
  * **topic_base** and "/" removed
  * if the remining **TopicName** is registered by Gateway then corresponding **TopicId** is used to make MQTT-SN message
  * resulting MQTT-SN message broadcasted to HBus

HBus nodes must register their **TopicName**s using **REGISTER** messages, see [HBus protocol](https://github.com/akouz/HBus#mqtt-sn-mode-broadcast-messages). Gateway detects **REGISTER** messages and automaticaly updates its list of registered **NodeName - NodeId** pairs.

## Time service

Every hour sketch requests time from a NTP server and broadcasts HBus MQTT-SN message to TopicId=1 (eg to default TopicName="time") with the following payload (sample):

> {atime:1558391178, tz:570, daysec:28578, hr:7, min:56}

where 
  * atime - UTC time, seconds since 00:00:00 of 01/01/2001
  * tz - time zone, offset in minutes from UTC
  * daysec - second since 00:00:00 of current day
  * hr - hour of the day
  * min - minute of the hour

