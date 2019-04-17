# Sketch PIR Sensor

Sketch detects movement using any of two PIR/RF movement detectors and sends MQTT message to the HBus. If light level is 0.9 or less, it also switches on output MOSFET at channel 1. After 1 min channel switched to 50% PWM (half brigtness), after 2 min MOSFET is off.

Every 10 min sketch broadcasts light level or movement sensor output.
