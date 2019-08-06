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
  Classes, SysUtils, IniFiles, HBrxtxU;

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
    FExpRplyHdr : string;     // expected reply header
    FRplyTmout : integer;
    FIgnoreCollectCnt : integer;
    FIgnoreTrafficCnt : integer;
    FBeepCnt : integer;
    FCmdStr : string;         // command to send
    function FMakeHdr(cmd : byte; dest : word; param : byte) : string;
    function FMakeCmd(cmd : byte; dest : word; param : byte) : boolean;
    function FMakeRplyHdr(msg : THbMsg; param : byte) : string;
    function FRplyRev(hdr : string) : THbMsg;
    function FRplyStatus(hdr : string) : THbMsg;
    function FRplyCollect(msg : THbMsg) : THbMsg;
    function FRplyPing(hdr : string; param : byte) : THbMsg;
    function FRplySetID(msg : THbMsg) : THbMsg;
    function FRplyBoot(hdr : string; param : byte) : THbMsg;
    procedure FAlienBoot(param : byte);
    function FRplyBeep(hdr : string; param : byte) : THbMsg;
    function FRplyDescr(hdr : string; msg : THbMsg) : THbMsg;
    function FRplySecurity(hdr : string; msg : THbMsg) : THbMsg;
  public
    OwnID : word;             // own HBus ID
    MsgId : byte;             // message ID
    Description : string;     // own description
    ErrMsg : string;
    // HBus replies
    function isExpectedReply(msg : THbMsg) : boolean;
    // send HBus commands
    function CmdRev(dest : word) : THbMsg;
    function CmdStatus(dest : word) : THbMsg;
    function CmdCollect(group : byte; slots : byte) : THbMsg;
    function CmdPing(dest : word; interval : byte) : THbMsg;
    function CmdSetID(dest : word; newID : word) : THbMsg;
    function CmdBoot(dest : word; pause : byte) : THbMsg;
    function CmdBeep(dest : word; dur : byte) : THbMsg;
    function CmdDescr(dest : word; descr : string; wr : boolean) : THbMsg;
    function CmdCustom(dest : word; json : string) : THbMsg;
    function CmdRdTopic(dest : word; ti : byte) : THbMsg;
    function CmdSecurity(dest : word; security : string; wr : boolean) : THbMsg;
    function Publish(topicId : word; Msg_ID : word; val : string) : THbMsg;
    function Register(topicId : word; Msg_ID : word; val : string) : THbMsg;
    // receive HBus commands
    function RxCmd(rx :THbMsg) : THbMsg;
    procedure Tick10ms;      // process it every 10 ms
    procedure Flush;
    constructor Create;
    destructor Destroy; override;
end;

//##############################################################################
implementation
//##############################################################################

{ THbCmd }

// =====================================  
// Make command header
// =====================================  
function THbCmd.FMakeHdr(cmd : byte; dest : word; param : byte) : string;
var s : string;
begin
  // command
  s := char(cmd) + char(byte(OwnID >> 8)) + char(byte(OwnID and $FF));
  s := s + char(byte(dest >> 8)) + char(byte(dest and $FF));
  s := s + char(byte(MsgId)) + char(byte(Random($100)));
  result := s + char(param);
  // expected reply
  FExpRplyHdr := char(cmd or $80) + copy(result,2,6); // omit param
  FRplyTmout := 100;
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
  if FExpRplyHdr = '' then begin
    if cmd in [CMD_REV..CMD_TOPIC] then begin
      FCmdStr := FMakeHdr(cmd, dest, param);
      result := true;
    end;
  end;
end;

// =====================================  
// Make a reply header
// =====================================  
function THbCmd.FMakeRplyHdr(msg : THbMsg; param : byte) : string;
var s : string;
begin
  s :=  char(ord(msg.s[1]) or $80) + copy(msg.s,2,5); // copy up to MsgID
  s := s + char(byte(Random($100)));
  result := s + char(param);
end;

// =====================================  
// Check is it a reply
// =====================================  
function THbCmd.isExpectedReply(msg : THbMsg) : boolean;
var s : string;
    cmd : byte;
    dest : word;
begin
  result := false;
  if (msg.mqtt) and (Length(msg.s) >= 8) then begin
    if (ord(msg.s[1]) and $80) <> 0 then begin   // it is a reply
      dest := $100*ord(msg.s[2]) + ord(msg.s[3]);
      if dest = OwnID then begin                 // it is reply to me
        cmd := ord(msg.s[1]) and $7F;
        if cmd = CMD_COLLECT then begin
          result := true;
          FExpRplyHdr := '';
          FRplyTmout := 0;
          // whatewer else required
        end else begin
            s := copy(msg.s, 1, length(FExpRplyHdr));
            if s = FExpRplyHdr then begin        // it is a reply to the last command sent
               result := true;
               FExpRplyHdr := '';
               FRplyTmout := 0;
               // whatewer else required
            end;
        end;
      end;
    end;
  end;
end;

// =====================================  
// REV command
// =====================================  
function THbCmd.CmdRev(dest : word) : THbMsg;
begin
  if  FMakeCmd(CMD_REV, dest, 0) then begin
    result.s := FCmdStr;
    result.mqtt := false;
    result.valid := true;
  end else
    result.valid := false;
end;

// =====================================  
// STATUS command
// =====================================  
function THbCmd.CmdStatus(dest : word) : THbMsg;
begin
  if  FMakeCmd(CMD_STATUS, dest, 0) then begin
    result.s := FCmdStr;
    result.mqtt := false;
    result.valid := true;
  end else
    result.valid := false;
end;

// =====================================  
// COLLECT command
// =====================================  
function THbCmd.CmdCollect(group : byte; slots : byte) : THbMsg;
var grsl : word;
begin
  if FExpRplyHdr = '' then begin
    grsl := $100*group + slots;
    FCmdStr := FMakeHdr(CMD_COLLECT, grsl, 0);
    result.s := FCmdStr;
    result.mqtt := false;
    result.valid := true;
  end else
    result.valid := false;
end;

// =====================================  
// PING command
// =====================================  
function THbCmd.CmdPing(dest : word; interval : byte) : THbMsg;
begin
  if FMakeCmd(CMD_PING, dest, interval) then begin
    result.s := FCmdStr;
    result.mqtt := false;
    result.valid := true;
  end else
    result.valid := false;
end;

// =====================================  
// SET_ID command
// =====================================  
function THbCmd.CmdSetID(dest : word; newID : word) : THbMsg;
var s : string;
    c : char;
begin
  if FMakeCmd(CMD_SET_ID, dest, 1) then begin
     c := char(byte(newID shr 8));
     FCmdStr := FCmdStr + c;
     c := char(byte(newID and $FF));
     FCmdStr := FCmdStr + c;
     // expected reply
     FExpRplyHdr := Copy(FExpRplyHdr, 1, 3);
     FRplyTmout := 100;
     result.s := FCmdStr;
     result.mqtt := false;
     result.valid := true;
   end else
     result.valid := false;
end;

// =====================================  
// BOOT command
// =====================================  
function THbCmd.CmdBoot(dest : word; pause : byte) : THbMsg;
begin
  if FMakeCmd(CMD_BOOT, dest, pause) then begin
    result.s := FCmdStr;
    result.mqtt := false;
    result.valid := true;
  end else
    result.valid := false;
end;

// =====================================  
// BEEP command
// =====================================  
function THbCmd.CmdBeep(dest : word; dur : byte) : THbMsg;
begin
  if FMakeCmd(CMD_BEEP, dest, dur) then begin
    result.s := FCmdStr;
    result.mqtt := false;
    result.valid := true;
  end else
    result.valid := false;
end;

// =====================================  
// DESCR command
// =====================================  
function THbCmd.CmdDescr(dest : word; descr : string; wr : boolean) : THbMsg;
var b : byte;
begin
  if wr then begin  // write
    if FMakeCmd(CMD_DESCR, dest, 1) and (Length(descr) < 64) then begin
      b := Length(descr);
      FCmdStr := FCmdStr + char(b) + descr;
      result.s := FCmdStr;
      result.mqtt := false;
      result.valid := true;
    end else
      result.valid := false;
  end else begin  // read
    if FMakeCmd(CMD_DESCR, dest, 0) then begin
      result.s := FCmdStr;
      result.mqtt := false;
      result.valid := true;
    end else
      result.valid := false;
  end
end;

// =====================================
// Custom JSON command
// =====================================
function THbCmd.CmdCustom(dest : word; json : string) : THbMsg;
begin
  if FMakeCmd(CMD_CUSTOM, dest, 1) and (Length(json) < 64) then begin
    FCmdStr := FCmdStr + json;
    result.s := FCmdStr;
    result.mqtt := false;
    result.postpone := 0;
    result.valid := true;
  end else
    result.valid := false;
end;

// =====================================
// Read TopicId and TopicName
// =====================================
function THbCmd.CmdRdTopic(dest : word; ti : byte) : THbMsg;
begin
  if FMakeCmd(CMD_TOPIC, dest, ti) then begin
    result.s := FCmdStr;
    result.mqtt := false;
    result.valid := true;
  end else
    result.valid := false;
end;

// =====================================
// Read/write security
// =====================================
function THbCmd.CmdSecurity(dest : word; security : string; wr : boolean) : THbMsg;
begin
  if wr then begin  // write
    if FMakeCmd(CMD_SECURITY, dest, 1) then begin
      FCmdStr := FCmdStr + security;
      result.s := FCmdStr;
      result.mqtt := false;
      result.valid := true;
    end else
      result.valid := false;
  end else begin  // read
    if FMakeCmd(CMD_SECURITY, dest, 0) then begin
      result.s := FCmdStr;
      result.mqtt := false;
      result.valid := true;
    end else
      result.valid := false;
  end
end;

// =====================================
// Make MQTT-SN message PUBLISH
// =====================================
function THbCmd.Publish(topicId : word; Msg_ID : word; val : string) : THbMsg;
var  s : string;
     b : byte;
begin
  b := random($100) and $F0;
  result.s := '' + char(b or MT_PUBLISH) + char(byte(OwnID shr 8)) + char(byte(OwnID and $FF));
  result.s := result.s + char(byte(topicId shr 8)) + char(byte(topicId and $FF));
  result.s := result.s + char(byte(Msg_ID shr 8)) + char(byte(Msg_ID and $FF));
  result.s := result.s + char(1);
  s := '{val:'+val+'}';
  result.s := result.s + s;
  result.mqtt := true;
  result.postpone := 0;
  result.valid := true;
end;

// =====================================
// Make MQTT-SN message REGISTER
// =====================================
function THbCmd.Register(topicId : word; Msg_ID : word; val : string) : THbMsg;
var b : byte;
begin
  b := random($100) and $F0;
  result.s := '' + char(b or MT_REGISTER) + char(byte(OwnID shr 8)) + char(byte(OwnID and $FF));
  result.s := result.s + char(byte(topicId shr 8)) + char(byte(topicId and $FF));
  result.s := result.s + char(byte(Msg_ID shr 8)) + char(byte(Msg_ID and $FF));
  result.s := result.s + char(1) + val;
  result.mqtt := true;
  result.postpone := 0;
  result.valid := true;
end;

// =====================================  
// Process input commands, form a reply if required
// =====================================  
function THbCmd.RxCmd(rx : THbMsg) : THbMsg;
var dest       : word;
    cmd, param : byte;
    hdr        : string;
begin
  result.valid := false;
  if (not rx.mqtt) and (Length(rx.s) >= 8) and (FIgnoreTrafficCnt = 0) then begin
    cmd := ord(rx.s[1]);
    if (cmd = CMD_COLLECT) and (FIgnoreCollectCnt = 0) then begin
      result := FRplyCollect(rx);
    end else begin
      dest := $100*ord(rx.s[4]) + ord(rx.s[5]);
      param := ord(rx.s[8]);
      if dest = ownID then begin
        hdr := FMakeRplyHdr(rx, 0);
        case cmd of
          CMD_REV:       result := FRplyRev(hdr);
          CMD_STATUS:    result := FRplyStatus(hdr);
          CMD_PING:      result := FRplyPing(hdr, param);
          CMD_SET_ID:    result := FRplySetID(rx);
          CMD_BOOT:      result := FRplyBoot(hdr, param);
          CMD_BEEP:      result := FRplyBeep(hdr, param);
          CMD_DESCR:     result := FRplyDescr(hdr, rx);
          CMD_SECURITY:  result := FRplySecurity(hdr, rx);
        end;
      end else begin
        case cmd of
          CMD_BOOT:      FAlienBoot(param);  // BOOT to another node
        end;
      end;
    end;
  end;
end;

// =====================================  
// Reply REV
// =====================================  
function THbCmd.FRplyRev(hdr : string) : THbMsg;
begin
  result.s := hdr + char(DEV_TYPE) + char(DEV_MODEL);
  result.s := result.s + char(HW_REV_MAJ) + char(HW_REV_MIN);
  result.s := result.s + char(BT_REV_MAJ) + char(BT_REV_MIN);
  result.s := result.s + char(SW_REV_MAJ) + char(SW_REV_MIN);
  result.mqtt := false;
  result.postpone := 0;
  result.valid := true;
end;

// =====================================  
// Reply STATUS
// =====================================  
function THbCmd.FRplyStatus(hdr : string) : THbMsg;
begin
  result.s := hdr;
  result.mqtt := false;
  result.postpone := 0;
  result.valid := true;
end;

// =====================================  
// Reply COLLECT
// =====================================  
function THbCmd.FRplyCollect(msg : THbMsg) : THbMsg;
var r : double;
    grp : byte;
    slots : byte;
    s : string;
begin
  result.valid := false;
  if Length(msg.s) = 8 then begin
    grp := ord(msg.s[4]);
    case grp of
      1 : result.valid := true; // all nodes
      2 : if (OwnID and $F000) = $F000 then
            result.valid := true; // tmp ID nodes
      3 : if (OwnID and $F000) <> $F000 then
            result.valid := true; // permanent ID nodes
    end;
    if result.valid then begin
      slots := ord(msg.s[5]);
      r := Random(slots);
      result.postpone := byte(Trunc(r));
      s :=  char(CMD_COLLECT or $80) + copy(msg.s,2,2);
      s := s + char(byte(OwnID shr 8)) + char(byte(OwnID and $FF));
      result.s := s + copy(msg.s,2,5) + char(byte(Random($100))) + char(0); // copy MsgID
      result.mqtt := false;
      result.valid := true;;
    end;
  end;
end;

// =====================================  
// Reply PING
// =====================================  
function THbCmd.FRplyPing(hdr : string; param : byte) : THbMsg;
begin
  FIgnoreCollectCnt := param * 100; // param=sec, counter in 10 ms ticks
  result.s := hdr;
  result.mqtt := false;
  result.postpone := 0;
  result.valid := true;
end;

// =====================================  
// Reply SET_ID
// =====================================  
function THbCmd.FRplySetID(msg : THbMsg) : THbMsg;
begin
  if length(msg.s) >= 10 then begin
    ownID := $100*ord(msg.s[9]) + ord(msg.s[10]);
    result.s := char(CMD_SET_ID or $80) + copy(msg.s,2,2);
    result.s := result.s + char(byte(ownID shr 8)) + char(byte(ownID and $FF));
    result.s := result.s +  copy(msg.s,2,5) + char(byte(Random($100))) + char(0);  // MsgID and OK
  end else begin
    result.s := char(CMD_SET_ID or $80) + copy(msg.s,2,6) + char(2); // Err
  end;
  result.mqtt := false;
  result.postpone := 0;
  result.valid := true;
end;

// =====================================  
// Reply BOOT
// =====================================  
function THbCmd.FRplyBoot(hdr : string; param : byte) : THbMsg;
begin
  result.s := hdr;
  result.mqtt := false;
  result.postpone := 0;
  result.valid := true;
end;

// =====================================  
// Alien BOOT
// =====================================  
procedure THbCmd.FAlienBoot(param : byte);
begin
  FIgnoreTrafficCnt := param * 100;
end;

// =====================================  
// Reply BEEP
// =====================================  
function THbCmd.FRplyBeep(hdr : string; param : byte) : THbMsg;
begin
  SysUtils.Beep;
  FBeepCnt := param * 100;
  result.s := hdr;
  result.mqtt := false;
  result.postpone := 0;
  result.valid := true;
end;

// =====================================  
// Reply DESCR
// =====================================  
function THbCmd.FRplyDescr(hdr : string; msg : THbMsg) : THbMsg;
var i, len : byte;
begin
  if (ord(msg.s[8]) = 0) then begin // read
    if Length(Description) > 100 then
      Description := Copy(Description, 1, 100);
    result.s := hdr + char(byte(length(Description)));
    for i:=1 to length(Description) do
      result.s := result.s + Description[i];
  end else begin // write
    result.s := hdr;
    result.s[8] := char(0); // OK
    result.postpone := 0;
    if length(msg.s) > 8 then begin
      len := ord(msg.s[9]);
      Description := copy(msg.s,10, len);
    end;
  end;
  result.mqtt := false;
  result.postpone := 0;
  result.valid := true;
end;

// =====================================
// Reply SECURITY
// =====================================
function THbCmd.FRplySecurity(hdr : string; msg : THbMsg) : THbMsg;
var i, len : byte;
begin
  if (ord(msg.s[8]) = 0) then begin // read
  end else begin // write
  end;
  result.mqtt := false;
  result.postpone := 0;
  result.valid := true;
end;

// =====================================
// 10 ms - count time-out
// =====================================  
procedure THbCmd.Tick10ms;
begin
  if (FRplyTmout > 0) and (FExpRplyHdr <> '') then begin
    dec(FRplyTmout);
    if FRplyTmout = 0 then begin
       ErrMsg := 'time-out cmd='+IntToHex(ord(FExpRplyHdr[1]) and $7F,2);
       FExpRplyHdr := '';
    end;
  end;
  if FIgnoreCollectCnt > 0 then
    dec(FIgnoreCollectCnt);
  if FIgnoreTrafficCnt > 0 then
    dec(FIgnoreTrafficCnt);
end;

// =====================================  
// Reset
// =====================================  
procedure THbCmd.Flush;
begin
  FExpRplyHdr := ''; // no replies expected any more
  FRplyTmout := 0;
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

