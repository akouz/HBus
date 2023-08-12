/*
 * File     HBrxtx.cpp
 * Rev      1.0 dated 20/12/2018
 
 * (c) 2019 Alex Kouznetsov,  https://github.com/akouz/hbus
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

//##############################################################################
// Inc
//##############################################################################

#include "HBrxtx.h"
#include "HBcipher.h"

//##############################################################################
// Var
//##############################################################################

Hb_rxtx HBrxtx;

//##############################################################################
// Func
//##############################################################################

// =============================================
// Constructor
// =============================================
Hb_rxtx::Hb_rxtx(void)
{
    flag.all = 0;
    txbuf = NULL;
}

// =============================================
// Add a symbol to Rx buffer
// =============================================
uchar Hb_rxtx::add_rx_uchar(uchar c, hb_msg_t* dest)
{
    uchar res = NOT_READY;
    if ((dest) && (dest->len < MAX_BUF))  // if rx buffer exists and not full
    {
        if (dest->esc)  // previous char was ESC
        {
            // --------------------
            // char after ESC
            // --------------------
            dest->esc = 0;
            switch (c)
            {
                // -------------- beginning of a frame
                case _ESC_START_HB:
                case _ESC_START_MQ:
                case _ESC_START_HBE:
                case _ESC_START_MQE:
                    if (dest->gate)
                    {
                        this->err_cnt++;      // START without STOP
                    }
                    dest->gate = 1;
                    dest->len = 0;
                    dest->hb = 0;
                    if ((c == _ESC_START_HB) || (c == _ESC_START_HBE))
                    {
                        dest->hb = 1;   // HBus message
                    }
                    dest->encrypt = 0;
                    if ((c == _ESC_START_HBE) || (c == _ESC_START_MQE))
                    {
                        dest->encrypt = 1;  // encrypted message
                    }
                    break;
                // -------------- end of a frame
                case _ESC_END:
                    if (dest->gate == 0)
                    {
                        err_cnt++;      // STOP without START
                    }
                    else
                    {
                        res = READY;    // message completed, rx buffer ready
                        dest->gate = 0;
                    }
                    break;
                // -------------- insert ESC
                case _ESC_ESC:
                    if (dest->gate == 0)
                    {
                        err_cnt++;
                    }
                    else
                    {
                        dest->buf[dest->len++] = _ESC;
                    }
                    break;
                // -------------- insert two ESC
                case _ESC_2ESC:
                    if (dest->gate == 0)
                    {
                        err_cnt++;
                    }
                    else
                    {
                        dest->buf[dest->len++] = _ESC;
                        if (dest->len < MAX_BUF)
                        {
                            dest->buf[dest->len++] = _ESC;
                        }
                    }
                    break;
                // --------------
                default:
                    err_cnt++;  // any other char after ESC is an error
                    break;
            }
        } // if was ESC
        else
        {
            // --------------------
            // regular input
            // --------------------
            if (c == _ESC)
            {
                dest->esc = 1;  // consider next char
            }
            else
            {
                if (dest->gate == 0)
                {
                    err_cnt++;
                }
                else
                {
                    dest->buf[dest->len++] = c;
                }
            }
        }
    }
    return res;
}

// =============================================
// Check crc
// =============================================
uchar Hb_rxtx::check_crc(hb_msg_t* msg)
{
    msgcrc = 0;
    calccrc = 0xF1F0;
    if ((msg) && (msg->len >= 2))
    {
        msg->len -= 2;
        msgcrc = 0x100*(uint)msg->buf[msg->len] + msg->buf[msg->len+1]; // supplied CRC
        calccrc = calc_crc(msg->buf, msg->len);
        if (msgcrc == calccrc)
        {
            return OK;
        }
    }
    return ERR;
}

// =============================================
// Receive symbol (while in receive mode)
// =============================================
hb_msg_t* Hb_rxtx::rx(uchar c)
{
    if (rxmsg.busy == 0)
    {
        if (READY == add_rx_uchar(c, &rxmsg))        
        {
            if (this->rxmsg.encrypt)                  // if message encrypted
            {
                HBcipher.decrypt(this->rxmsg.buf, this->rxmsg.len);  // then decrypt it
            }
            if ((flag.no_crc) || (OK == check_crc(&rxmsg)))  // if crc matches
            {
                // blink(5);   // blink 50 ms
                rxmsg.busy = 1;
                return &rxmsg;
            }
            else // crc mismatch
            {
                rxmsg.len = 0;     // reset output buffer
                rxmsg.all = 0;
#ifdef DEBUG                
                Serial.print(F(" Err: msgcrc="));
                Serial.print(msgcrc, HEX);
                Serial.print(F(", calccrc="));
                Serial.println(calccrc, HEX);
#endif                
            }
        }
    }
    return NULL;
}

// =============================================
// Start transmission
// =============================================
uchar Hb_rxtx::start_tx(hb_tx_msg_t* buf)
{
    if ((buf == NULL) || (buf->len < 8))
    {
        return ERR_PARAM;
    }
    txbuf = buf;
    Serial.write(priority);
    echobuf[0] = priority;
    Serial.write(_ESC);
    echobuf[1] = _ESC;
    buf->esc = 1;
    echolen = 2;
    txcnt = 2;
    txpos = 0;
    buf->gate = 0;
    if (buf->encrypt)
    {
        start = (buf->hb) ? _ESC_START_HBE : _ESC_START_MQE; // encrypted HBus or encrypted MQTT
    }
    else
    {
        start = (buf->hb) ? _ESC_START_HB : _ESC_START_MQ; // unencrypted HBus or unencrypted MQTT
    }
    return OK;
}

// =============================================
// Transmit, insert byte-stuffing
// =============================================
uchar Hb_rxtx::tx(uchar* pause_cnt)
{
    uchar val;
    // ------------------------------
    // receive echo
    // ------------------------------
    while (Serial.available())
    {
        *pause_cnt = 0;
        uchar rxchar = (uchar)Serial.read();
        if ((echolen) && (rxchar == echobuf[0])) // if echo matches
        {
           echobuf[0] = echobuf[1];
           echobuf[1] = echobuf[2];
           echolen = (echolen)? echolen-1 : 0;
        }
        else
        {
            return ERR_ECHO;
        }
        if ((txpos >= txbuf->len) && (echolen == 0)) // txbuf transmitted, echo received
        {
            Serial.flush();
            return READY;
        }
    }
    // ------------------------------
    // transmit next char(s)
    // ------------------------------
    while (echolen < 3)
    {
        if (txpos < txbuf->len)
        {
            if (txbuf->gate)  // if gate opened
            {
                if (txbuf->esc)
                {
                    txbuf->esc = 0;
                    if (txbuf->buf[txpos] == _ESC) // second ESC
                    {
                        txpos++;
                        val = _ESC_2ESC;
                    }
                    else    // it was a single ESC
                    {
                        val = _ESC_ESC;
                    }
                }
                else
                {
                    val = txbuf->buf[txpos++];
                    txbuf->esc = (val == _ESC) ? 1 : 0;
                }
                Serial.write(val);
                echobuf[echolen++] = val;
            }
            else // open the gate - start transmission
            {
                Serial.write(start);
                echobuf[echolen++] = start;
                txbuf->esc = 0;
                txbuf->gate = 1;
            }
        }
        else  // buffer finished
        {
            if  (txbuf->esc)    // last char was ESC
            {
                txbuf->esc = 0;
                val = _ESC_ESC;
            }
            else
            {
                switch(txpos - txbuf->len)
                {
                case 0: val = _ESC;         break;
                case 1: val = _ESC_END;     break;
                default:
                    txbuf->gate = 0;
                    txbuf->busy = 0;
                    return READY;
                    break;
                }
                txpos++;
            }
            Serial.write(val);
            echobuf[echolen++] = val;
        } // buffer finished
        txcnt++;
    }
    return NOT_READY;
}

/* EOF */
