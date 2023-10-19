// =====================================================================
// HBus commands
// =====================================================================
{
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
}

unit HBcmdU;

{$mode objfpc}{$H+}

//##############################################################################
interface
//##############################################################################

uses
  Classes, SysUtils, IniFiles, HBrxtxU, HButilsU;

const
  CMD_REV            = 1;
  CMD_STATUS         = 2;
  CMD_COLLECT        = 3;
  CMD_PING           = 4;
  CMD_SET_ID         = 5;
  CMD_BOOT           = 6;
  CMD_BEEP           = 7;
  CMD_DESCR          = 8;
  CMD_SECURITY       = 9;
  CMD_CUSTOM         = 10;
  CMD_TOPIC          = 11;

  MT_PUBLISH         = $0C;
  MT_REGISTER        = $0A;

  // -------------------


  // -------------------
  DEV_TYPE           = 1; // bridge/gateway
  DEV_MODEL          = 1; // PC bridge
  HW_REV_MAJ         = 1;
  HW_REV_MIN         = 0;
  SW_REV_MAJ         = 0;
  SW_REV_MIN         = 8;
  BT_REV_MAJ         = 0;
  BT_REV_MIN         = 0;


type

  { THbCmd }

  THbCmd = class(TStringList)
  private
    FIgnoreCollectCnt : integer;
    FCmdStr : string;         // command to send
    function FClrMsg : THbMsg;
    function FMakeHdr(cmd : byte; dest : word; param : byte) : string;
    function FMakeCmd(cmd : byte; dest : word; param : byte) : boolean;
  public
    OwnID : word;             // own HBus ID
    MsgId : byte;             // message ID
    Description : string;     // own description
    ErrMsg : string;
    // send HBus commands
    function CmdRev(dest : word) : THbMsg;
    function CmdStatus(dest : word) : THbMsg;
    function CmdCollect(group : byte; slots : byte) : THbMsg;
    function CmdPing(dest : word; interval : byte) : THbMsg;
    function CmdSetID(dest : word; newID : word) : THbMsg;
    function CmdBoot(dest : word;  ss : string) : THbMsg;
    function CmdBeep(dest : word; dur : byte) : THbMsg;
    function CmdDescr(dest : word; descr : string; cmdwr : byte) : THbMsg;
    function CmdCustom(dest : word; json : string) : THbMsg;
    function CmdRdTopic(dest : word; ti : byte) : THbMsg;
    function CmdSecurity(dest : word; security : string; wr : boolean) : THbMsg;
    function Publish(topicId : word; Msg_ID : word; val : string) : THbMsg;
    function Register(topicId : word; Msg_ID : word; val : string) : THbMsg;
    procedure Tick10ms;      // process it every 10 ms
    procedure Flush;
    constructor Create;
    destructor Destroy; override;
  end;

var
  HBcmd  : THbCmd;

//##############################################################################
implementation
//##############################################################################

{ THbCmd }

// =====================================
// Clear message
// =====================================
function THbCmd.FClrMsg : THbMsg;
begin
  result.encrypted := false;
  result.err := false;
  result.mqtt := false;
  result.valid := false;
  result.pri := 0;
  result.postpone := 0;
end;

// =====================================  
// Make command header
// =====================================  
function THbCmd.FMakeHdr(cmd : byte; dest : word; param : byte) : string;
var s : string;
    ts : longword;
begin
  // command
  s := char(cmd) + char(byte(OwnID >> 8)) + char(byte(OwnID and $FF));
  s := s + char(byte(dest >> 8)) + char(byte(dest and $FF));
  s := s + char(byte(MsgId)) + char(byte(Random($100)));
  s := s + char(param);
  ts := GetUTCtime;
  s := s + char(byte(ts >> 24));
  s := s + char(byte(ts >> 16));
  s := s + char(byte(ts >> 8));
  result := s + char(byte(ts));
  // increment MsgID
  inc(MsgId);
  if MsgId >= $FE then
    MsgId := 1;
end;

// =====================================  
// Single command, no params
// =====================================  
function THbCmd.FMakeCmd(cmd : byte; dest : word; param : byte) : boolean;
begin
  result := false;
  if cmd in [CMD_REV..CMD_TOPIC] then begin
    FCmdStr := FMakeHdr(cmd, dest, param);
    result := true;
  end;
end;


// =====================================  
// REV command
// =====================================  
function THbCmd.CmdRev(dest : word) : THbMsg;
begin
  result := FClrMsg;
  if  FMakeCmd(CMD_REV, dest, 0) then begin
    result.s := FCmdStr;
    result.valid := true;
  end;
end;

// =====================================  
// STATUS command
// =====================================  
function THbCmd.CmdStatus(dest : word) : THbMsg;
begin
  result := FClrMsg;
  if  FMakeCmd(CMD_STATUS, dest, 0) then begin
    result.s := FCmdStr;
    result.valid := true;
  end;
end;

// =====================================  
// COLLECT command
// =====================================  
function THbCmd.CmdCollect(group : byte; slots : byte) : THbMsg;
var grsl : word;
begin
  result := FClrMsg;
  grsl := $100*group + slots;
  FCmdStr := FMakeHdr(CMD_COLLECT, grsl, 0);
  result.s := FCmdStr;
  result.valid := true;
end;

// =====================================  
// PING command
// =====================================  
function THbCmd.CmdPing(dest : word; interval : byte) : THbMsg;
begin
  result := FClrMsg;
  if FMakeCmd(CMD_PING, dest, interval) then begin
    result.s := FCmdStr;
    result.valid := true;
  end;
end;

// =====================================  
// SET_ID command
// =====================================  
function THbCmd.CmdSetID(dest : word; newID : word) : THbMsg;
var c : char;
begin
  result := FClrMsg;
  if FMakeCmd(CMD_SET_ID, dest, 1) then begin
     c := char(byte(newID shr 8));
     FCmdStr := FCmdStr + c;
     c := char(byte(newID and $FF));
     FCmdStr := FCmdStr + c;
     result.s := FCmdStr;
     result.valid := true;
   end;
end;

// =====================================  
// BOOT command
// =====================================  
function THbCmd.CmdBoot(dest : word; ss : string) : THbMsg;
begin
  result := FClrMsg;
  if FMakeCmd(CMD_BOOT, dest, 0) then begin
    result.s := FCmdStr + ss;
    result.valid := true;
  end;
end;

// =====================================  
// BEEP command
// =====================================  
function THbCmd.CmdBeep(dest : word; dur : byte) : THbMsg;
begin
  result := FClrMsg;
  if FMakeCmd(CMD_BEEP, dest, dur) then begin
    result.s := FCmdStr;
    result.valid := true;
  end;
end;

// =====================================  
// DESCR command
// =====================================  
function THbCmd.CmdDescr(dest : word; descr : string; cmdwr : byte) : THbMsg;
var b : byte;
begin
  result := FClrMsg;
  if (cmdwr and 1) = 1 then begin  // write
    if FMakeCmd(CMD_DESCR, dest, cmdwr) and (Length(descr) < 64) then begin
      b := Length(descr);
      FCmdStr := FCmdStr + char(b) + descr;
      result.s := FCmdStr;
      result.valid := true;
    end;
  end else begin  // read
    if FMakeCmd(CMD_DESCR, dest, cmdwr) then begin
      result.s := FCmdStr;
      result.mqtt := false;
      result.valid := true;
    end;
  end
end;

// =====================================
// Custom JSON command
// =====================================
function THbCmd.CmdCustom(dest : word; json : string) : THbMsg;
begin
  result := FClrMsg;
  if FMakeCmd(CMD_CUSTOM, dest, 1) and (Length(json) < 64) then begin
    FCmdStr := FCmdStr + json;
    result.s := FCmdStr;
    result.valid := true;
  end;
end;

// =====================================
// Read TopicId and TopicName
// =====================================
function THbCmd.CmdRdTopic(dest : word; ti : byte) : THbMsg;
begin
  result := FClrMsg;
  if FMakeCmd(CMD_TOPIC, dest, ti) then begin
    result.s := FCmdStr;
    result.valid := true;
  end;
end;

// =====================================
// Read/write security
// =====================================
function THbCmd.CmdSecurity(dest : word; security : string; wr : boolean) : THbMsg;
begin
  result := FClrMsg;
  if wr then begin  // write
    if FMakeCmd(CMD_SECURITY, dest, 1) then begin
      FCmdStr := FCmdStr + security;
      result.s := FCmdStr;
      result.valid := true;
    end;
  end else begin  // read
    if FMakeCmd(CMD_SECURITY, dest, 0) then begin
      result.s := FCmdStr;
      result.valid := true;
    end;
  end
end;

// =====================================
// Make MQTT-SN message PUBLISH
// =====================================
function THbCmd.Publish(topicId : word; Msg_ID : word; val : string) : THbMsg;
var  s : string;
     b : byte;
     ts : longword;
begin
  result := FClrMsg;
  b := random($100) and $F0;
  result.s := '' + char(b or MT_PUBLISH) + char(byte(OwnID shr 8)) + char(byte(OwnID and $FF));
  result.s := result.s + char(byte(topicId shr 8)) + char(byte(topicId and $FF));
  result.s := result.s + char(byte(Msg_ID shr 8)) + char(byte(Msg_ID and $FF));
  result.s := result.s + char(1);
  ts := GetUTCtime;
  s := '' + char(byte(ts >> 24));
  s := s + char(byte(ts >> 16));
  s := s + char(byte(ts >> 8));
  s := s + char(byte(ts));
  s := s + '{val:'+val+'}';
  result.s := result.s + s;
  result.mqtt := true;
  result.valid := true;
end;

// =====================================
// Make MQTT-SN message REGISTER
// =====================================
function THbCmd.Register(topicId : word; Msg_ID : word; val : string) : THbMsg;
var b : byte;
begin
  result := FClrMsg;
  b := random($100) and $F0;
  result.s := '' + char(b or MT_REGISTER) + char(byte(OwnID shr 8)) + char(byte(OwnID and $FF));
  result.s := result.s + char(byte(topicId shr 8)) + char(byte(topicId and $FF));
  result.s := result.s + char(byte(Msg_ID shr 8)) + char(byte(Msg_ID and $FF));
  result.s := result.s + char(1) + val;
  result.mqtt := true;
  result.valid := true;
end;

// =====================================
// 10 ms - count time-out
// =====================================  
procedure THbCmd.Tick10ms;
begin
  if FIgnoreCollectCnt > 0 then
    dec(FIgnoreCollectCnt);
end;

// =====================================  
// Reset
// =====================================  
procedure THbCmd.Flush;
begin
  ErrMsg := '';
end;

// =====================================  
// Create
// =====================================  
constructor THbCmd.Create;
var ini : TIniFile;
    w : word;
begin
  Randomize;
  ini := TIniFile.Create('Gateway.ini');
  w := $F000 or Random($1000);
  OwnID := ini.ReadInteger('HBus','OwnID', w);
  Description := ini.ReadString('HBus','Descr', '');
  MsgId := ini.ReadInteger('HBus','MsgID',1);
  ini.Free;
  Flush;
end;

// =====================================  
// Destroy
// =====================================  
destructor THbCmd.Destroy;
var ini : TIniFile;
begin
  ini := TIniFile.Create('Gateway.ini');
  ini.WriteInteger('HBus','OwnID',OwnID);
  ini.WriteString('HBus','Descr',Description);
  ini.WriteInteger('HBus','MsgID',MsgId);
  ini.Free;
  inherited Destroy;
end;

end.

