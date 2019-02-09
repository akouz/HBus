/*
 * Library   HBus rx/tx
 * Author    A.Kouznetsov
 * Rev       1.0 dated 20/12/2018
 * Target    Arduino

Redistribution and use in source and binary forms, with or without modification, are permitted.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
                        //Serial.print(" <end>, len=");
                        //Serial.println(dest->len);
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
                //Serial.print(" <esc> ");
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
                    //Serial.print(" <crc failed> ");
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
            //Serial.print(" rx_msg_ready ");
            if ((flag.no_crc) || (OK == check_crc(&rxmsg)))  // if crc matches
            {                    
                //Serial.print(" crc_OK ");
                rxmsg.busy = 1;
                return &rxmsg;
            }
            else // crc mismatch
            {
                //Serial.print(" crc_mismatch ");
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
        /*
        Serial.print(" Hb_rxtx::start_tx bad params: buf=");
        Serial.print((uint)buf);
        if (buf)
        {
            Serial.print(", buf->len=");
            Serial.print(buf->len);
        }
        Serial.println("");
        */        
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
