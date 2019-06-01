/*
 * File     HBnodes.cpp 
 * Target   Arduino ESP8266

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

#include "HBnodes.h"
#include <algorithm>

//##############################################################################
// Var
//##############################################################################

HB_nodes HBnodes;

//##############################################################################
// Func
//##############################################################################

// =============================================
// Constructor
// =============================================
HB_nodes::HB_nodes(void)
{
    node_list_len = 0;
    flag.all = 0;
}

// =============================================
// Read node list from EEPROM
// =============================================
void HB_nodes::init_node_list(void)
{
    for (uint i=0; i<MAX_NODE_LIST; i++)
    {
        node_list[i] = 0x100*(uint)EEPROM.read(EE_NODE_LIST + 2*i) + 
                             EEPROM.read(EE_NODE_LIST + 2*i + 1);         
    }
    for (uint i=0; i<MAX_NODE_LIST; i++)
    {
        if ((node_list[i] == 0) || (node_list[i] > 0x07FF)) 
        {
            node_list[i] = 0xFFFF;
        }        
    }    
    flag.up_to_date = 1;
    if (sort(node_list, MAX_NODE_LIST))
    {
        store();
    }
    flag.sorted = 1;
    node_list_len = MAX_NODE_LIST;
    for (uint i=0; i<MAX_NODE_LIST; i++)
    {
        if (node_list[i] >= 0xFFFF)
        {
            node_list_len = i;
            break;
        }
    }
}

// =============================================
// Check if the NodeId registered
// =============================================
int HB_nodes::is_registered(uint node_id)
{
    if ((node_list_len == 0) || (node_id == 0) || (node_id > 0x07FF))
    {
        return -1;
    }
    else if (flag.sorted)
    {
        if ((node_id < node_list[0]) ||  (node_id > node_list[node_list_len-1]))
        {
            return -1;
        }
    }
    for (uint i=0; i<node_list_len; i++)
    {
        if (node_list[i] == node_id)
        {
            return (int)i;
        }
    }
    return -1;
}

// =============================================
// Store node list in EEPROM
// =============================================
void HB_nodes::store(void)
{
    for (uint i=0; i<node_list_len; i++)
    {
        EEPROM.write(EE_NODE_LIST + 2*i, (uchar)(node_list[i] >> 8));
        EEPROM.write(EE_NODE_LIST + 2*i + 1, (uchar)node_list[i]);
    }
    if (node_list_len < MAX_NODE_LIST)
    {
        EEPROM.write(EE_NODE_LIST + 2*node_list_len, 0xFF);
        EEPROM.write(EE_NODE_LIST + 2*node_list_len + 1, 0xFF);
    }
    EEPROM.commit(); 
    flag.changed = 0; 
}

// =============================================
// Add NodeId to the list of registered nodes
// =============================================
uchar HB_nodes::add(uint node_id)
{
    if (is_registered(node_id) < 0)
    {
        if (node_list_len < MAX_NODE_LIST)
        {
            node_list[node_list_len++] = node_id;            
            sort(node_list, MAX_NODE_LIST);
            flag.sorted = 1;
            flag.changed = 1;             
            store();                       
        }
        else
        {
            return ERR_OVERFLOW;
        } 
    }
    return OK;
}

// =============================================
// Remove NodeId from the list of registered nodes
// =============================================
uchar HB_nodes::rem(uint node_id)
{
    if (node_list_len)
    {
        int res = is_registered(node_id);
        if (res >= 0)
        {
            node_list[(uint)res] = 0xFFFF;                    
            sort(node_list, MAX_NODE_LIST); 
            node_list_len--;
            store();
            return OK;       
        }
    }
    return ERR;
}

// =============================================
// Debug: print collected nodes list
// =============================================
void  HB_nodes::print_nodes(void)
{
    Serial.print("HBnodes: node_list_len=");
    Serial.print(node_list_len);
    if (node_list_len)
    {
        Serial.print(", first=");
        Serial.print(node_list[0], HEX);
        if (node_list_len > 0)
        {
            Serial.print(", last=");
            Serial.print(node_list[node_list_len-1], HEX);
        }    
    }
    Serial.println();
    if (node_list_len)
    {
        for (uint i=0; i<node_list_len; i++)
        {
            Serial.print(node_list[i], HEX);
            Serial.print(" ");
            if ((i & 7) == 7)
              Serial.print(" ");
            if ((i & 0xF) == 0xF)
              Serial.println();
        }
    }
    Serial.println();
}

/* EOF */