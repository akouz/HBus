#pragma once

#include "Arduino.h"

#include <Udp.h>

#define     SEVENZYYEARS            2208988800UL
#define     NTP_PACKET_SIZE         48
#define     NTP_DEFAULT_LOCAL_PORT  1337
#define     MAX_SERVER_NAME         64

#define     UNIX_EPOCH_OFFS         978307200    // offset between 00:00:00 1/1/1970 and 00:00:00 1/1/2001, sec

class myNTPClient {
  private:
    UDP*          _udp;
    bool          _udpSetup       = false;

    unsigned long sent_ms;
    bool          waiting;
    
    const char*   _poolServerName = "pool.ntp.org"; 
    char          _ServerName[MAX_SERVER_NAME];     
    
    int           _port           = NTP_DEFAULT_LOCAL_PORT;

    unsigned long _currentEpoc    = 0;      // sec
    unsigned long _lastUpdate     = 0;      // ms
  
    byte          _packetBuffer[NTP_PACKET_SIZE];


  public:
    myNTPClient(UDP& udp);

    /**
     * Starts the underlying UDP client with the default local port
     */
    void begin();

    /**
     * Starts the underlying UDP client with the specified local port
     */
    void begin(int port);

    bool setServerName(char* sn);

    void sendNTPPacket();
    unsigned long since_sent;   // ms

    bool checkReply();
    unsigned long tmout = 2000; // ms

    int getDay();
    int getHours();
    int getMinutes();
    int getSeconds();

    String getFormattedTime();

    /**
     * @return time in seconds since Jan. 1, 1970
     */
    unsigned long getEpochTime();
    /**
     * @return time in seconds since Jan. 1, 2001
     */
    unsigned long getEpochTime2001();

    /**
     * Stops the underlying UDP client
     */
    void end();
};
