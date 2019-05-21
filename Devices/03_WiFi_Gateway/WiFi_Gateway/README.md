# Sketch WiFi_Gateway.ino

Current rev 1.0

## WiFi connection

In WiFi_Gateway.ino enter your WiFi network credentials:

> const char* ssid     = "your_ssid";

> const char* password = "your_password";

In case of lost WiFi connection, sketch tries to restore it, see **void coos_task_reconnect(void)**.

## MQTT broker

In WiFi_Gateway.ino enter MQTT broker credentials:

> const char* mqtt_url = "your_mqtt_broker";

> const char* mqtt_password = "mqtt_password";

In case of lost connection, sketch will try to re-connect to MQTT broker, see **void coos_task_reconnect(void)**.

Sketch keeps MQTT connection alive by sending a short message to broker every 10 sec, see **void coos_task_broadcast(void)**.

Gateway reads HBus MQTT-SN messages from the bus, converts them into MQTT messages and sends to the MQTT broker. see **void hbus_msg_to_mqtt(hb_msg_t* msg)** in **HBus.cpp**. Duiring conversion Gateway replaces **TopicId** by a topic name. 

The topic name of the outgoing message consists of **topic_base** and **TopicName**. List of registered TopicId stored in EEPROM, list of registered TopicName stored in SPIFFS. For example,  in case when **topic_base**="HBus" and **TopicName**="topic0" is registered as **TopicId**=100 (or 0x0064), then HBus message:

> 0C 00 11 00 64 0E 7B 01  {val:3} 

will be converted to MQTT message with payload {val:3} and topic name "**HBus/topic0**".

Gateway is subscribed to topics **topic_base**/#. Messages from MQTT broker to that topic will be processed as follows:
  * **topic_base** and "/" removed
  * if the remining **TopicName** is registered by Gateway then corresponding **TopicId** is used to nake MQTT-SN message
  * resulting MQTT-SN message broadcasted to HBus

HBus nodes register their **TopicName**s using REGISTER command, see [HBus protocol](https://github.com/akouz/HBus#mqtt-sn-mode-broadcast-messages)

## Time service

Every hour sketch requests time from a NTP server and broadcasts HBus MQTT-SN message to TopicId=1 (eg to default TopicName="time") with the following payload (sample):

> {atime:1558391178, tz:570, daysec:28578, hr:7, min:56}

where 
  * atime - UTC time, seconds since 00:00:00 of 01/01/2001
  * tz - time zone, offset in minutes from UTC
  * daysec - second since 00:00:00 of current day
  * hr - hour of the day
  * min - miute of the hour

