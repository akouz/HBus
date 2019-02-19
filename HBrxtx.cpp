/*
 * Library   HBus rx/tx
 * Author    A.Kouznetsov
 * Rev       1.0 dated 20/12/2018
 * Target    Arduino

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

#include  "HBrxtx.h"

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
                    if (dest->gate)
                    {
                        err_cnt++;      // START without STOP
                    }  
                    dest->gate = 1;
                    dest->len = 0;
                    dest->hb = (c == _ESC_START_HB)? 1 : 0;
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
// Add crc
// =============================================
void Hb_rxtx::add_crc(hb_msg_t* msg)
{
    uint crc = calc_crc(msg->buf, msg->len);
    msg->buf[msg->len] = (uchar)(crc >> 8);
    msg->buf[msg->len+1] = (uchar)crc;
    msg->len += 2;
}
// =============================================
// Add crc and byte-stuffing
// =============================================
uchar Hb_rxtx::tx_encode(hb_msg_t* src, hb_msg_t* dest)
{
    uchar c;
    dest->len = 0;
    if ((src) && (dest))
    {
        src->all = 0;
        dest->all = 0;
        if (src->len < MAX_BUF-2)
        {
            add_crc(src);
            for (uchar i=0; i < src->len; i++)
            {
                c = src->buf[i];
                // -----------------------------
                if (src->esc == 0) 
                {
                    src->esc = (c == _ESC)? 1 : 0;
                    dest->buf[dest->len++] = c;
                }  
                // -----------------------------
                else
                {
                    src->esc = 0;
                    if (c == _ESC)  // second ESC
                    {
                        dest->buf[dest->len++] = _ESC_2ESC;
                    }
                    else  // single ESC
                    {  
                        dest->buf[dest->len++] = _ESC_ESC;
                        dest->buf[dest->len++] = c;
                    }
                }  
            } // for
            if (src->esc) // last char was ESC
            {
                dest->buf[dest->len++] = _ESC_ESC;
            }
            dest->valid = 1;
            return OK;
        }
    }  
    return ERR;
}
// =============================================
// Check crc
// =============================================
uchar Hb_rxtx::check_crc(hb_msg_t* msg)
{
    uint crc;
    if ((msg) && (msg->len >= 2))
    {
        msg->len -= 2;
        crc = 0x100*(uint)msg->buf[msg->len] + msg->buf[msg->len+1]; // supplied CRC
        if (crc == calc_crc(msg->buf, msg->len))
        {
            return OK;
        }
    }
    return ERR;
}
// =============================================
// Decode received data, if message completed then check crc
// =============================================
uchar Hb_rxtx::rx_decode(uchar* src, uchar* src_len, hb_msg_t* dest)
{
    uchar c, i;
    uchar res = NOT_READY;
    if ((src) && (dest)) // if both buffers exists
    {
        for (i=0; i < *src_len; i++)
        {
            c = src[i];            
            if (add_rx_uchar(c, dest) == READY)        // if message completed
            {
                shift_buf(src, i, *src_len - i);    // remove used part of input buffer
                *src_len -= (i+1);                  // keep remaining part of the buffer
                if ((flag.no_crc) || (OK == check_crc(dest)))  // if crc matches
                {
                    dest->valid = 1;
                    res = READY;
                }
                else // crc mismatch
                {
                    dest->len = 0;     // reset output buffer
                    dest->all = 0;          
                }
                return res;
            }
        }
        *src_len = 0; // input buffer is empty   
    }
    return res;
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
            if ((flag.no_crc) || (OK == check_crc(&rxmsg)))  // if crc matches
            {                    
                rxmsg.busy = 1;
                return &rxmsg;
            }
            else // crc mismatch
            {
                rxmsg.len = 0;     // reset output buffer
                rxmsg.all = 0;          
            }
        }
    }
    return NULL;
}
// =============================================
// Start transmission
// =============================================
uchar Hb_rxtx::start_tx(hb_msg_t* buf)
{
    if ((buf == NULL) || (buf->len < 8))
    {
        return ERR_PARAM;
    }
    txbuf = buf;
    Serial.write(priority);
    echobuf[0] = priority;
    Serial.write(buf->buf[0]);
    echobuf[1] = buf->buf[0];
    echolen = 2;
    txcnt = 2;
    txpos = 1;
    return OK;  
}
// =============================================
// Transmit
// =============================================
uchar Hb_rxtx::tx(uchar* pause_cnt)
{
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
            uchar val = txbuf->buf[txpos++];
            Serial.write(val);
            txcnt++;
            echobuf[echolen++] = val; 
        }
        else
        {
            break;
        }
    }
    return NOT_READY;
}

/* EOF */
