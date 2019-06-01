/*
 * File     HBnodes.h
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
 
#ifndef __HB_NODES_H
#define __HB_NODES_H

//##############################################################################
// Inc                                              
//##############################################################################

#include  "HBcommon.h"

//##############################################################################
// Def
//##############################################################################

#define MAX_NODE_LIST   0x200

//##############################################################################
// Class                                              
//##############################################################################

class HB_nodes{
    public:
            HB_nodes(void);
    void    init_node_list(void);
    int     is_registered(uint node_id);
    uchar   add(uint node_id);
    uchar   rem(uint node_id);
    void    print_nodes(void);
    void    store(void);
    uint    node_list_len;

    private:
    uint    node_list[MAX_NODE_LIST];
    union{
        uint    all;
        struct{
            unsigned up_to_date : 1;
            unsigned sorted     : 1;
            unsigned changed    : 1;
        };
    }flag;
};

extern HB_nodes HBnodes;    


#endif /* __HB_NODES_H */