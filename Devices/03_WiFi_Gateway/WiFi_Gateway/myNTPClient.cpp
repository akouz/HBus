/**
 * The MIT License (MIT)
 * Copyright (c) 2015 by Fabrice Weinberg
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * 
 * Modified by A.Kouznetsov. Remove blocking behaviour, compensate round-trip,
 * clear UDP Rx buffer before sending NTP request
 */

#include "myNTPClient.h"

// ===================================================
// Create
// ===================================================
myNTPClient::myNTPClient(UDP& udp) 
{
  this->_udp      = &udp;
  this->waiting   = false;
  this->_ServerName[0] = 0;
}

// ===================================================
// Begin
// ===================================================
void myNTPClient::begin() 
{
  this->begin(NTP_DEFAULT_LOCAL_PORT);
  this->_udpSetup = true;
}

void myNTPClient::begin(int port) {
  this->_port = port;
  this->_udp->begin(this->_port);
  this->_udpSetup = true;
}

// ===================================================
// Assign server name
// ===================================================
bool myNTPClient::setServerName(char* sn)
{
  for (int i=0; i<MAX_SERVER_NAME; i++)
  {
    this->_ServerName[i] = sn[i];
    if (sn[i] == 0)
    {
      if (i > 10)
      {
        return true;    // new server name assigned
      }
      else  // string is too short
      {
        this->_ServerName[0] = 0;
        return false;
      }
    }
  }
  return false;  // string is too long
}

// ===================================================
// Send request to NTP server
// ===================================================
void myNTPClient::sendNTPPacket() 
{
  // if there is a packet in rx buffer - read and dispose it
  int len = this->_udp->parsePacket();
  if (len > 0)
  {
      this->_udp->read(this->_packetBuffer, len);
  }  
  // set all bytes in the buffer to 0
  memset(this->_packetBuffer, 0, NTP_PACKET_SIZE);
  // Initialize values needed to form NTP request
  // (see URL above for details on the packets)
  this->_packetBuffer[0] = 0b11100011;   // LI, Version, Mode
  this->_packetBuffer[1] = 0;     // Stratum, or type of clock
  this->_packetBuffer[2] = 6;     // Polling Interval
  this->_packetBuffer[3] = 0xEC;  // Peer Clock Precision
  // 8 bytes of zero for Root Delay & Root Dispersion
  this->_packetBuffer[12]  = 49;
  this->_packetBuffer[13]  = 0x4E;
  this->_packetBuffer[14]  = 49;
  this->_packetBuffer[15]  = 52;

  // all NTP fields have been given values, now
  // you can send a packet requesting a timestamp:
  this->sent_ms = millis();                 // store time
  if (this->_ServerName[0]) 
    this->_udp->beginPacket(this->_ServerName, 123);        // user-defined name
  else
    this->_udp->beginPacket(this->_poolServerName, 123);   //default name, NTP requests are to port 123
  this->_udp->write(this->_packetBuffer, NTP_PACKET_SIZE);
  this->_udp->endPacket();
  this->since_sent = 0;
  this->waiting = true;
}
// ===================================================
// Check for reply from the NTP server
// ===================================================
bool myNTPClient::checkReply() 
{  
  this->since_sent = (unsigned long)(millis() - this->sent_ms);    
  if (this->waiting)
  {
    int len = this->_udp->parsePacket();
    if (len > 0)
    {
      this->waiting = false;
      this->_lastUpdate = (unsigned long)(this->sent_ms + (since_sent / 2));   // assuming that server sent time in the middle of round trip         
      this->_udp->read(this->_packetBuffer, NTP_PACKET_SIZE);
      unsigned long highWord = word(this->_packetBuffer[40], this->_packetBuffer[41]);
      unsigned long lowWord = word(this->_packetBuffer[42], this->_packetBuffer[43]);
      // combine the four bytes (two words) into a long integer
      // this is NTP time (seconds since Jan 1 1900):
      unsigned long secsSince1900 = (highWord << 16) | lowWord;

      this->_currentEpoc = (unsigned long)(secsSince1900 - SEVENZYYEARS);
      return true;    
    }
    if (this->since_sent >= this->tmout)
    {
       this->waiting = false;
    }
  }
  return false;
}

// ===================================================
// Time conversions
// ===================================================
unsigned long myNTPClient::getEpochTime() 
{
  return this->_currentEpoc + 
      ((unsigned long)((millis() - this->_lastUpdate)) / 1000);   // Time since last update
}

unsigned long myNTPClient::getEpochTime2001() 
{
    return   getEpochTime() - UNIX_EPOCH_OFFS;
}

int myNTPClient::getDay() 
{
  return (((this->getEpochTime()  / 86400L) + 4 ) % 7); //0 is Sunday
}
int myNTPClient::getHours() 
{
  return ((this->getEpochTime()  % 86400L) / 3600);
}
int myNTPClient::getMinutes() 
{
  return ((this->getEpochTime() % 3600) / 60);
}
int myNTPClient::getSeconds() 
{
  return (this->getEpochTime() % 60);
}


String myNTPClient::getFormattedTime() 
{
  unsigned long rawTime = this->getEpochTime();
  unsigned long hours = (rawTime % 86400L) / 3600;
  String hoursStr = hours < 10 ? "0" + String(hours) : String(hours);

  unsigned long minutes = (rawTime % 3600) / 60;
  String minuteStr = minutes < 10 ? "0" + String(minutes) : String(minutes);

  unsigned long seconds = rawTime % 60;
  String secondStr = seconds < 10 ? "0" + String(seconds) : String(seconds);

  return hoursStr + ":" + minuteStr + ":" + secondStr;
}

// ===================================================
// Stop UDP server
// ===================================================
void myNTPClient::end() 
{
  this->_udp->stop();
  this->_udpSetup = false;
}

/* EOF */