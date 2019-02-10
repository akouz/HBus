/*
 * Author    A.Kouznetsov
 * Rev       1.0 dated 8/1/2019
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

#include  "common.h"


//##############################################################################
// Var
//##############################################################################


//##############################################################################
// Func
//##############################################################################

// =============================================
// Copy buffer
// =============================================
void copy_buf(uchar* src, uchar* dst, uchar len)
{    
    if (dst)
    {
        for (uchar i=0; i<len; i++)
        {
            dst[i] = (src)? src[i] : 0; // if src does not exist then fill dst by 0                     
        }
    }
}
// =============================================
// Shift buffer down
// =============================================
void shift_buf(uchar* buf, uchar pos, uchar len)
{
    if ((buf) && (pos) && (len))
    {
        for (uchar i=0; i<len; i++)
        {
            buf[i] = buf[i+pos];
        }
    }
}
// =============================================
// Add uchar value to crc
// =============================================
void crc_add_uchar(uchar b, uint* crc)
{
    *crc ^= (b << 8);
    for (uchar j=0; j<8; j++)
    {
        *crc = (*crc & 0x8000)? ((*crc << 1) ^ 0x1021) : (*crc << 1);
    }
}

// =============================================
 // Calculate CRC
// =============================================
/*
 * Name  : CRC-16 CCITT
 * Poly  : 0x1021    x^16 + x^12 + x^5 + 1
 * Init  : 0xFFFF
 * Revert: false
 * XorOut: 0x0000
 * Check : 0x3B0A ("123456789" hex)
 */
uint calc_crc(uchar* buf, uchar len)
{
    uint crc = 0xFFFF;
    if (buf)
    {
        for (uchar i=0; i<len; i++)
        {
            crc_add_uchar(buf[i], &crc);
        }
    }
    return crc;
}
// =============================================
// Reset Tx buffer and start a new message
// =============================================
uchar begin_txmsg(hb_msg_t* txmsg, uchar hb)
{
    if (txmsg->busy)
    {
        return NOT_READY;
    }
    else
    {
        txmsg->hb = (hb)? 1 : 0;  
        txmsg->buf[0] = _ESC;
        txmsg->buf[1] = (hb)? _ESC_START_HB : _ESC_START_MQ;   // HBus/MQTT
        txmsg->crc = 0xFFFF;    // init crc
        txmsg->len = 2;
        return READY;   
    }    
}
// =============================================
// Add char to Tx message
// =============================================
uchar add_txmsg_uchar(hb_msg_t* txmsg, uchar c)
{
    if ((txmsg->len >= MAX_BUF) || (txmsg->busy))
    {
        return ERR;
    }
    crc_add_uchar(c, &txmsg->crc);   // calculate crc
    if (c == _ESC)
    {
        if (txmsg->len >= 4) // it could be _ESC_ESC sequence
        {
            if  ((txmsg->buf[txmsg->len-2] == _ESC) && 
                 (txmsg->buf[txmsg->len-1] == _ESC_ESC))
            {
                txmsg->buf[txmsg->len-1] = _ESC_2ESC;  // replace 
                return OK;
            }
        }
        txmsg->buf[txmsg->len++] = _ESC;
        txmsg->buf[txmsg->len++] = _ESC_ESC;
    }
    else
    {
        txmsg->buf[txmsg->len++] = c;
    }
    return OK;
}
// =============================================
// Copy message header
// =============================================
void copy_msg_hdr(hb_msg_t* src, uchar first, uchar last, hb_msg_t* txmsg)
{
    for (uchar i=first; i<last; i++)
    {
        if (i == 0)
        {
            add_txmsg_uchar(txmsg, src->buf[0] | 0x80); // set "reply" flag  
        }
        else
        {
            add_txmsg_uchar(txmsg, src->buf[i]);
        }
    }
}

// =============================================
// Finish Tx message
// =============================================
uchar finish_txmsg(hb_msg_t* txmsg)
{
    uchar crcL = (uchar)txmsg->crc;          // crc lsb
    add_txmsg_uchar(txmsg, (uchar)(txmsg->crc >> 8));  // crc msb
    add_txmsg_uchar(txmsg, crcL);
    if (txmsg->len <= MAX_BUF-2)
    {
        txmsg->buf[txmsg->len++] = _ESC;
        txmsg->buf[txmsg->len++] = _ESC_END;
        txmsg->busy = 1;
        return OK;
    }
    return ERR;
}
                           
/* EOF */