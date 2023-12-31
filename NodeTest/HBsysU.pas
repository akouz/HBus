unit HBsysU;

{$mode objfpc}{$H+}

//##############################################################################
interface
//##############################################################################

uses
  Classes, SysUtils, Dialogs, HBcmdU, HBrxtxU;

const
  // node strings
  MAX_NODE_STR = 10;
  NAME_STR      = 0;
  LOCATION_STR  = 1;
  DESCR_STR     = 2;
  PROJ_STR      = 3;
  SKETCH_STR    = 4;
  MODULE_STR    = 5;
  NODE_STR_TXT : array[0..9] of string = (
    'Name','location','description','project','sketch','module','tbd','tbd','tbd','tbd');
  // revision fields
  REV_DEV_TYPE  = 1;
  REV_DEV_MODEL = 2;
type

  // =====================================
  { THBnode }
  // =====================================

  THBnode = class
  private
  public
    NodeID : word;
    str : array [0..MAX_NODE_STR] of string;
    rev : string;

    constructor Create;
    destructor Destroy; override;
  end;

  // =====================================
  { THbsys }
  // =====================================

  THbsys = class(TStringList)
  private
    FlastRxCmd : byte;
    FlastOkErr : byte;
    FexploreSM : integer;  // YBus explore batch state machine
    Fcnt : integer;
    FpingList : array [0..100] of word;
    FsendPingToCollected : boolean;
    FreadStr : integer;
    FreadRev : integer;
    function GetStr(rx : THbMsg) : string;
    function GetParam(rx : THbMsg) : string; // string as byte array
    function SendPing : boolean;
    function ReadStr(StrNo : byte) : boolean;
    function ReadRev : boolean;
    function FindNode(NodeID : word) : integer; // find a node amongst collected
  public
    busy : boolean;       // while processing batch
    SL : TStringList;
    function ReportNode(NodeID : word) : boolean; // report written to SL
    function StartHbusExplore : boolean;
    function Process(rx : THbMsg) : boolean;
    procedure Tick10ms;
    constructor Create;
    destructor Destroy; override;
  end;


var
  HBsys : THbsys;

//##############################################################################
implementation
//##############################################################################

//##############################################################################
{ THBnode }
//##############################################################################

// =====================================
// Create node
// =====================================
constructor THBnode.Create;
begin

end;

// =====================================
// Destroy node
// =====================================
destructor THBnode.Destroy;
begin
  inherited Destroy;
end;


//##############################################################################
{ THbsys }
//##############################################################################

// =====================================
// Find node amongst collected
// =====================================
function THbsys.FindNode(NodeID: word): integer;
var i : integer;
begin
  result := -1;
  if self.Count > 0 then begin
    for i := 0 to self.Count-1 do begin
      if NodeID = THBnode(self.Objects[i]).NodeID then begin
        result := i;
        break;
      end;
    end;
  end;
end;

// =====================================
// Old modules
// =====================================
function old_modules(dt : byte; dm : byte) : string;
begin
  result := 'unknown';
  if dt = 2 then begin
    if dm = 1 then
      result := 'Arduino Pro Mini'
    else if dm = 2 then
      result := 'Arduino Nano';
  end;
end;

// =====================================
// Report node
// =====================================
function THbsys.ReportNode(NodeID: word): boolean;
var i : integer;
    node : THBnode;
    s, ss : string;
    dt, dm : byte;
begin
  result := false;
  self.SL.Clear;
  i := self.FindNode(NodeID);
  if i >= 0 then begin
    result := true;
    // ------------------
    node := THBnode(self.Objects[i]);
    s := 'Project "' + node.str[PROJ_STR] + '", sketch "' + node.str[SKETCH_STR] + '"';
    s := s + ', rev '+ IntToStr(ord(node.rev[7])) + '.' + IntToStr(ord(node.rev[8]));
    if length(node.rev) > 12 then begin // if signature exists
      s := s + ', signature ';
      for i := 0 to 3 do begin
        s := s + IntToHex(ord(node.rev[11 + i]), 2);
        if i = 1 then
          s := s + '.';
      end;
    end;
    s := s + ', bootloader rev '+ IntToStr(ord(node.rev[3])) + '.' + IntToStr(ord(node.rev[4]));
    self.SL.Add(s);
    // ------------------
    dt := ord(node.rev[1]);
    dm := ord(node.rev[2]);
    s := 'Device type ' + IntToStr(ord(dt)) + ', model ' + IntToStr(ord(dm));
    ss := node.str[MODULE_STR];
    if ss = '' then
      ss := old_modules(dt, dm);
    s := s + ', employs module "' + ss + '", h/w rev ';
    s := s + IntToStr(ord(node.rev[5])) + '.' + IntToStr(ord(node.rev[6]));
    self.SL.Add(s);
    // ------------------
    s := 'Name "' + node.str[NAME_STR] + '"';
    if node.str[LOCATION_STR] <> '' then
       s := s + ', location "' + node.str[LOCATION_STR] + '"';
    if node.str[DESCR_STR] <> '' then
       s := s + ', description "' + node.str[DESCR_STR] + '"';
    self.SL.Add(s);
  end else
    self.SL.Add('Node 0x' + IntToHex(NodeID,4) + ' was not found');
end;

// =====================================
// Extract string from recieved THbMsg
// =====================================
function THbsys.GetStr(rx: THbMsg): string;
var len, i : byte;
begin
  result := '';
  len := ord(rx.s[13]);
  if len > 0 then begin
     for i:=1 to len do begin
       result := result + rx.s[13+i];
     end;
     result := Trim(result);
  end;
end;

// =====================================
// Extract data from recived THbMsg
// =====================================
function THbsys.GetParam(rx: THbMsg): string;
var len : byte;
begin
  result := '';
  len := length(rx.s);
  if len > 12 then
     result := copy(rx.s, 13, len-12);
end;

// =====================================
// Send PING to collected node
// =====================================
function THbsys.SendPing: boolean;
var s : string;
    NodeID : word;
begin
  result := false;
  if not HBrxtx.TxBusy then begin
    dec(Fcnt);
    NodeID := FpingList[Fcnt];
    s := HBrxtx.Tx(HBcmd.CmdPing(NodeID, 2));   // interval 2 sec
    if s<>'' then
      ShowMessage(s)
    else
      result := true;
  end;
end;

// =====================================
// Read text string from the node
// =====================================
function THbsys.ReadStr(StrNo: byte): boolean;
var s : string;
    NodeID : word;
    param : byte;
begin
  result := false;
  if not HBrxtx.TxBusy then begin
    dec(Fcnt);
    NodeID := THBnode(self.Objects[Fcnt]).NodeID;
    param :=  StrNo shl 1; // read is even
    s :=  HBrxtx.Tx(HBcmd.CmdDescr(NodeID, '', param));
    if s<>'' then
      ShowMessage(s)
    else
      result := true;
  end;
end;

// =====================================
// Read revesion
// =====================================
function THbsys.ReadRev: boolean;
var s : string;
    NodeID : word;
begin
  result := false;
  if not HBrxtx.TxBusy then begin
    dec(Fcnt);
    NodeID := THBnode(self.Objects[Fcnt]).NodeID;
    s :=  HBrxtx.Tx(HBcmd.CmdRev(NodeID));
    if s<>'' then
      ShowMessage(s)
    else
      result := true;
  end;
end;

// =====================================
// Start HBus explore batch
// =====================================
function THbsys.StartHbusExplore : boolean;
var s : string;
begin
  result := false;
  if not HBrxtx.TxBusy then begin
    self.busy := true;
    FexploreSM := 1;
    Fcnt := 0;
    FsendPingToCollected := false;
    FreadStr := -1;
    FreadRev := -1;
    HBcmd.Flush;
    s := HBrxtx.Tx(HBcmd.CmdCollect(3, 32));   // group 3, 32 slots
    if s<>'' then
      ShowMessage(s)
    else
      result := true;
  end;
end;

// =====================================
// Process recieved message
// =====================================
function THbsys.Process(rx: THbMsg): boolean;
var  NodeID : word;
     i : integer;
     s : string;
     b : byte;
     node : THBnode;
begin
  result := false;
  NodeID := 0;
  // --------------------
  // MQTT messages
  // --------------------
  if rx.mqtt then begin

  // --------------------
  // HBus messages
  // --------------------
  end else begin
    FlastRxCmd := ord(rx.s[1]);
    FlastOkErr := ord(rx.s[8]);
    NodeID := ord(rx.s[5]) + $100*ord(rx.s[4]);
    i := self.FindNode(NodeID);
    case FlastRxCmd of
    $81: begin // reply to REV command
           if (i >= 0) and (FlastOkErr < $80) then begin
             s := self.GetParam(rx);
             if s <> '' then begin
               if (THBnode(self.Objects[i]).rev = '') then
                  THBnode(self.Objects[i]).rev := s;
             end;
           end;
         end;
    $83: begin // reply to COLLECT command
           if i < 0 then begin // node not in the list
             FpingList[Fcnt] := NodeID;
             inc(Fcnt);    // a new node collected
             s := 'NodeID 0x'+IntToHex(NodeID,4);
             node := THBnode.Create;
             node.NodeID := NodeID;
             self.AddObject(s, TObject(node));
           end;
         end;
    $88: begin // reply to DESCR command
           if (i >= 0) and ((FlastOkErr and 1) = 0) and (FlastOkErr < $80) then begin
             b := FlastOkErr shr 1; // string index
             if (b < MAX_NODE_STR) then begin
               s := self.GetStr(rx);
               if s <> '' then begin
                 if (THBnode(self.Objects[i]).str[b] = '') then begin
                   self.Strings[i] := self.Strings[i] + ', ' + NODE_STR_TXT[b] + ' "' + s + '"';
                 end;
                 THBnode(self.Objects[i]).str[b] := s;
               end;
             end;
           end;
         end;
      end;
    end;
end;

// =====================================
// Every 10 ms
// =====================================
procedure THbsys.Tick10ms;
begin
  // -------------------------
  // send PING to recently collected nodes
  // -------------------------
  if FsendPingToCollected then begin
    if Fcnt > 0 then begin
      self.SendPing;
    end else begin
      if self.StartHbusExplore then  // collect again
        FsendPingToCollected := false;
    end;
  // -------------------------
  // read strings from collected nodes
  // -------------------------
  end else if FreadStr >= 0 then begin
    if FreadStr <= 5 then begin
      if Fcnt = 0 then begin
        FCnt := self.Count;
        inc(FreadStr);
      end else begin
        self.ReadStr(byte(FreadStr));
      end;
    end else begin
      FreadStr := -1; // finished
    end;
  // -------------------------
  // read revisions from collected nodes
  // -------------------------
  end else if FreadRev >= 0 then begin
    if FCnt > 0 then
      self.ReadRev
    else
      FreadRev := -1;
  end else begin
    // -------------------------
    // batch to explore all nodes in the net
    // -------------------------
    if FexploreSM > 0 then begin
      inc(FexploreSM);
      case FexploreSM of
      35: begin    // when COLLECT command finished
            if Fcnt > 0 then begin
              FsendPingToCollected := true;
            end;
          end;
      36: begin // when collect phase finished - read all strings
            self.Sort;
            FreadStr := 0;
            FCnt := self.Count;
          end;
      37: begin  // then read revisions
            FreadRev := 0;
            FCnt := self.Count;
          end;
      99: self.busy := false;
      end;
    end;
  end;
end;

// =====================================
// Create
// =====================================
constructor THbsys.Create;
begin
  self.Clear;
  self.SL := TStringList.Create;
  self.SL.Clear;
end;

// =====================================
// Destroy
// =====================================
destructor THbsys.Destroy;
var i : integer;
begin
  if self.Count > 0 then begin
    for i:=0 to self.Count-1 do begin
       THBnode(self.Objects[i]).Destroy;
    end;
  end;
  self.SL.Destroy;
  inherited Destroy;
end;

end.

