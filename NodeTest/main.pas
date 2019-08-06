// =============================================================================
// HBus node test
// =============================================================================
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
unit main;

{$mode objfpc}{$H+}

//##############################################################################
interface
//##############################################################################

uses
  Classes, SysUtils, FileUtil, CPort, Forms, Controls, Graphics, Dialogs, md5, sha1,
  ExtCtrls, StdCtrls, ComCtrls, Registry, IniFiles, Clipbrd, HBrxtxU, HBcmdU;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnLoadEECip : TButton;
    BtnPublish : TButton;
    BtnRev : TButton;
    BtnSaveEECip : TButton;
    BtnStatus : TButton;
    BtnCollect : TButton;
    BtnPing : TButton;
    BtnNewID : TButton;
    BtnBoot : TButton;
    BtnBeep : TButton;
    BtnRdDescr : TButton;
    BtnWrDescr : TButton;
    BtnCustomCmd : TButton;
    BtnRdTopic : TButton;
    BtnRegister : TButton;
    BtnSaveFlashCip : TButton;
    BtnLoadFlashCip : TButton;
    BtnMakeCip : TButton;
    BtnRdSecurity : TButton;
    BtnWrSecurity : TButton;
    BtnSaveSecurity : TButton;
    BtnCreateKey : TButton;
    BtnEECreate : TButton;
    CbPorts : TComboBox;
    CbDamp : TCheckBox;
    CbStatusRpt : TCheckBox;
    CBcipherM : TCheckBox;
    CBcipherH : TCheckBox;
    CBrev : TCheckBox;
    CBstatus : TCheckBox;
    CBcollect : TCheckBox;
    CBping : TCheckBox;
    CBsetID : TCheckBox;
    CBboot : TCheckBox;
    CBrddescr : TCheckBox;
    CBwrdescr : TCheckBox;
    CBcustomcmd : TCheckBox;
    CBtopic : TCheckBox;
    CBrdSecurity : TCheckBox;
    CBwrSecurity : TCheckBox;
    CBpublish : TCheckBox;
    CBregister : TCheckBox;
    CBbroadcast : TCheckBox;
    CbEEcipher : TCheckBox;
    CbTs : TCheckBox;
    EdEENotes : TEdit;
    EdGroup : TEdit;
    EdBootPause : TEdit;
    EdDuration : TEdit;
    EdDescr : TEdit;
    EdCustomCmd : TEdit;
    EdCipNotes : TEdit;
    EdKey5 : TEdit;
    EdKey6 : TEdit;
    EdKey7 : TEdit;
    EdKey8 : TEdit;
    EdRounds : TEdit;
    EdKey1 : TEdit;
    EdKey2 : TEdit;
    EdKey3 : TEdit;
    EdKey4 : TEdit;
    EdLFSR1 : TEdit;
    EdLFSR2 : TEdit;
    EdLFSR16 : TEdit;
    EdTopic : TEdit;
    EdTopic1 : TEdit;
    EdTopicI : TEdit;
    EdMsgId : TEdit;
    EdOwnId : TEdit;
    EdNewID : TEdit;
    EdPause : TEdit;
    EdSlots : TEdit;
    EdNode : TEdit;
    EdTopicName : TEdit;
    EdTopicVal : TEdit;
    GbEEkey : TGroupBox;
    GbFlashKey : TGroupBox;
    GBSecurity : TGroupBox;
    GBDescription : TGroupBox;
    GBunencrypted : TGroupBox;
    Label1 : TLabel;
    Label10 : TLabel;
    Label11 : TLabel;
    LblKey1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    Label6 : TLabel;
    Label7 : TLabel;
    Label8 : TLabel;
    Label9 : TLabel;
    LB : TListBox;
    LblKey2 : TLabel;
    LblKey3 : TLabel;
    LblKey4 : TLabel;
    LblKey5 : TLabel;
    LblKey6 : TLabel;
    LblKey7 : TLabel;
    LblKey8 : TLabel;
    LblLFSR1 : TLabel;
    LblLFSR2 : TLabel;
    LblLFSR3 : TLabel;
    OpenCipher : TOpenDialog;
    PageControl1 : TPageControl;
    Panel1 : TPanel;
    Panel2 : TPanel;
    Panel3 : TPanel;
    SaveCipher : TSaveDialog;
    TabSheet1 : TTabSheet;
    Timer1ms : TTimer;
    Timer1sec : TTimer;
    TsHBus : TTabSheet;
    TabSheet2 : TTabSheet;
    Timer10ms : TTimer;
    procedure BtnBeepClick(Sender : TObject);
    procedure BtnBootClick(Sender : TObject);
    procedure BtnCollectClick(Sender : TObject);
    procedure BtnCreateKeyClick(Sender : TObject);
    procedure BtnCustomCmdClick(Sender : TObject);
    procedure BtnEECreateClick(Sender : TObject);
    procedure BtnLoadEECipClick(Sender : TObject);
    procedure BtnLoadFlashCipClick(Sender : TObject);
    procedure BtnMakeCipClick(Sender : TObject);
    procedure BtnPublishClick(Sender : TObject);
    procedure BtnNewIDClick(Sender : TObject);
    procedure BtnPingClick(Sender : TObject);
    procedure BtnRdDescrClick(Sender : TObject);
    procedure BtnRdSecurityClick(Sender : TObject);
    procedure BtnRdTopicClick(Sender : TObject);
    procedure BtnRegisterClick(Sender : TObject);
    procedure BtnRevClick(Sender : TObject);
    procedure BtnSaveEECipClick(Sender : TObject);
    procedure BtnSaveFlashCipClick(Sender : TObject);
    procedure BtnSaveSecurityClick(Sender : TObject);
    procedure BtnStatusClick(Sender : TObject);
    procedure BtnWrDescrClick(Sender : TObject);
    procedure BtnWrSecurityClick(Sender : TObject);
    procedure CBcipherHChange(Sender : TObject);
    procedure CBcipherMChange(Sender : TObject);
    procedure CbPortsChange(Sender : TObject);
    procedure EdCipNotesDblClick(Sender : TObject);
    procedure EdEENotesDblClick(Sender : TObject);
    procedure EdKey5DblClick(Sender : TObject);
    procedure EdMsgIdDblClick(Sender : TObject);
    procedure EdNewIDDblClick(Sender : TObject);
    procedure EdNodeDblClick(Sender : TObject);
    procedure EdOwnIdDblClick(Sender : TObject);
    procedure EdRoundsDblClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure Label2DblClick(Sender : TObject);
    procedure LBDblClick(Sender : TObject);
    procedure LBKeyPress(Sender : TObject; var Key : char);
    procedure PageControl1Exit(Sender : TObject);
    procedure Panel1DblClick(Sender : TObject);
    procedure Timer10msTimer(Sender : TObject);
    procedure Timer1msTimer(Sender : TObject);
    procedure Timer1secTimer(Sender : TObject);
  private
    { private declarations }
    ComPortStr : string;
    NodeID : word;
    NewID : word;
    millis : longword;
    WasConnected : boolean;
    function CB_to_security : word;
    procedure security_to_CB(val : word);
  public
    { public declarations }
    function StrToHex(s : string; txt_i : byte) : string;
    procedure PrintHbMsg(msg : THbMsg);
    function num_str_c2pas(s : string) : string;
  end;

var
  Ready : boolean = false;
  Form1 : TForm1;
  HB : THbRxtx;
  HBcmd : THbCmd;
  TxMsg, RxMsg : THbMsg;

//##############################################################################
implementation
//##############################################################################

{ TForm1 }

{$R *.lfm}

// =====================================================
// Find all existing at the moment COM ports
// =====================================================
procedure TForm1.Label2DblClick(Sender : TObject);
var
  i, cnt: integer;
  reg: TRegistry;
  ts: TStringList;
  s: string;
begin
  CbPorts.Clear;
  reg := TRegistry.Create(KEY_READ);
  reg.RootKey := HKEY_LOCAL_MACHINE;
  reg.OpenKey('hardware\devicemap\serialcomm', False);
  ts := TStringList.Create;
  // ------------------------------
  // read all COM ports from registry
  // ------------------------------
  reg.GetValueNames(ts);
  // ------------------------------
  // out of all serial ports, select USB virtual COM ports
  // ------------------------------
  cnt := 0;
  for i := 0 to ts.Count - 1 do  begin
    s := reg.ReadString(ts.Strings[i]);
    if (s <> 'COM1') then begin
      CbPorts.AddItem(s, nil);
      if CbPorts.ItemIndex < 0 then
        CbPorts.ItemIndex := 0;
      if s = ComPortStr then
        CbPorts.ItemIndex := cnt;
      inc(cnt);
    end;
  end;
  ts.Free;
  reg.Free;
end;

// =====================================================
// Clear
// =====================================================
procedure TForm1.LBDblClick(Sender : TObject);
begin
  LB.Clear;
end;

// =====================================================
// Ctrl+C to copy selected text
// =====================================================
procedure TForm1.LBKeyPress(Sender : TObject; var Key : char);
var s : string;
    i : integer;
begin
  if Ord(Key) = 3 then begin
    if LB.SelCount > 0 then begin
      s := '';
      for i:=0 to LB.Items.Count-1 do begin
        if LB.Selected[i] then
        s := s + LB.Items.Strings[i] + char(13);
      end;
      Clipboard.AsText := s;
    end;
  end;
end;

// =====================================================
// 10 ms
// =====================================================
procedure TForm1.Timer10msTimer(Sender : TObject);
begin
  HBcmd.Tick10ms;
  HB.Tick10ms;
end;

// =====================================================
// 1 ms
// =====================================================
procedure TForm1.Timer1msTimer(Sender : TObject);
var i, cnt : integer;
    rx : THbMsg;
    msg_id : word;
    s : string;
begin
  inc(millis);
  if HB.ComPort.Connected then begin
     try
        rx := HB.Rx;
        // debug messages
        cnt := HB.DbgList.Count;
        if (cnt > 0) then begin
          s := '';
          if CbTs.Checked then
            s := ' -- <'+IntToStr(millis)+' ms>';
          for i:=0 to cnt-1 do begin
            LB.Items.Add(' - dbg: '+HB.DbgList.Strings[i]+s);
            s := '';
          end;
          for i:=0 to cnt-1 do
            HB.DbgList.Delete(cnt-1-i);
        end;
        // dump
        cnt := HB.DampList.Count;
        if (cnt > 0) then begin
          s := '';
          if CbTs.Checked then
            s := ' -- <'+IntToStr(millis)+' ms>';
          if CbDamp.Checked then begin
            for i:=0 to cnt-1 do begin
              LB.Items.Add(' --- bus: '+HB.DampList.Strings[i]+s);
              s := '';
            end;
          end;
          for i:=0 to cnt-1 do
            HB.DampList.Delete(cnt-1-i);
        end;
        // received
        if rx.valid then begin
          if not rx.err then begin
            if (rx.mqtt) then begin
              msg_id := HB.MsgID;   // received
              if (msg_id >= HBcmd.MsgId) or ((HBcmd.MsgId > $F0) and (msg_id < $10)) then begin
                 HBcmd.MsgID := msg_id +1;
                 if (HBcmd.MsgID > $FE) or (HBcmd.MsgID = 0) then
                   HBcmd.MsgID := 1;
                 EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
              end;
            end;
          end;
          PrintHbMsg(rx);
          rx.valid := false;
        end;
        if HB.TxStatus = 2 then
           HB.TxStatus := 0;
    except
      if HB.ComPort.Connected then
        HB.ComPort.Connected := false;
      HB.PortOk := false;
    end;
  end;
end;

// =====================================================
// 1 sec
// =====================================================
procedure TForm1.Timer1secTimer(Sender : TObject);
begin
  if (not HB.ComPort.Connected) and (not WasConnected) then begin
    Label2DblClick(Sender);
    CbPortsChange(Sender);
  end;
  WasConnected := HB.ComPort.Connected;
  if CbStatusRpt.Checked then
     BtnStatusClick(Sender);
end;

// =====================================================
// Convert security checkboxes to word
// =====================================================
function TForm1.CB_to_security : word;
var res : word;
begin
  res := 0;
  if  CBrev.Checked then         res := res or 1;
  if  CBstatus.Checked then      res := res or 2;
  if  CBcollect.Checked then     res := res or 4;
  if  CBping.Checked then        res := res or 8;
  if  CBsetID.Checked then       res := res or $10;
  if  CBboot.Checked then        res := res or $20;
  if  CBrddescr.Checked then     res := res or $40;
  if  CBwrdescr.Checked then     res := res or $80;
  if  CBcustomcmd.Checked then   res := res or $100;
  if  CBtopic.Checked then       res := res or $200;
  if  CBrdsecurity.Checked then  res := res or $400;
  if  CBwrsecurity.Checked then  res := res or $800;
  if  CBtopic.Checked then       res := res or $1000;
  if  CBpublish.Checked then     res := res or $2000;
  if  CBregister.Checked then    res := res or $4000;
  if  CBbroadcast.Checked then   res := res or $8000;
  result := res;
end;

// =====================================================
// Convert word to security checkboxes
// =====================================================
procedure TForm1.security_to_CB(val : word);
begin
  CBrev.Checked          := (val and 1) <> 0;
  CBstatus.Checked       := (val and 2) <> 0;
  CBcollect.Checked      := (val and 4) <> 0;
  CBping.Checked         := (val and 8) <> 0;
  CBsetID.Checked        := (val and $10) <> 0;
  CBboot.Checked         := (val and $20) <> 0;
  CBrddescr.Checked      := (val and $40) <> 0;
  CBwrdescr.Checked      := (val and $80) <> 0;
  CBcustomcmd.Checked    := (val and $100) <> 0;
  CBtopic.Checked        := (val and $200) <> 0;
  CBrdsecurity.Checked   := (val and $400) <> 0;
  CBwrsecurity.Checked   := (val and $800) <> 0;
  CBtopic.Checked        := (val and $1000) <> 0;
  CBpublish.Checked      := (val and $2000) <> 0;
  CBregister.Checked     := (val and $4000) <> 0;
  CBbroadcast.Checked    := (val and $8000) <> 0;
end;

// =====================================================
// Convert binary message into hex str
// =====================================================
function TForm1.StrToHex(s : string; txt_i : byte) : string;
var i, hl : integer;
    c : char;
begin
  result := '';
  if (txt_i = 0) then
    hl := length(s)
  else
    hl := txt_i; //
  for i:=1 to hl do begin
    c := s[i];
    result := result + IntToHex(ord(c),2) + ' ';
    if (i and 7)=0 then
      result := result + ' ';
  end;
  if (txt_i > 0) then begin
    result := result + copy(s, hl+1, length(s)- hl);
  end;
end;

// =====================================================
// Print message
// =====================================================
procedure TForm1.PrintHbMsg(msg : THbMsg);
var s : string;
    cmd : byte;
    OkErr : byte;
    TopicId, NewTopicId : word;
begin
  if (length(msg.s) >= 8) then begin
    s := IntToHex(msg.pri, 2); // show priority byte (prefix)
    cmd := ord(msg.s[1]);
    OkErr := ord(msg.s[8]);
    // ---------------------------
    // error
    // ---------------------------
    if msg.err then begin
      if (msg.mqtt) then
        s := ' Error--> MQTT '
      else
        s := ' Error--> HBus ';
      s := s + StrToHex(msg.s, 0); // binary
      s := s + ', crc=' + IntToHex(HB.LastCrc[0],4);
      s := s + '/' + IntToHex(HB.LastCrc[1],4);
    end else begin
      // ---------------------------
      // HBus message
      // ---------------------------
      if (not msg.mqtt) then begin
        s := s + ' HBus ';
        if (cmd = $82) and (OkErr = 1) then    // STATUS reply
          s := s + StrToHex(msg.s, 8)          // JSON
        else if (cmd = $88) then
          s := s + StrToHex(msg.s, 9)          // description
        else if (cmd = $A) or (cmd = $8A) then // custom cmd and reply
          s := s + StrToHex(msg.s, 8)
        else if (cmd = $8B) then begin         // topic
          if (OkErr = 0) then begin
            if (length(msg.s) >= 10) then begin
              NewTopicId := $100*ord(msg.s[9]) + ord(msg.s[10]);
              s := s + StrToHex(msg.s, 10);
              s := s + ' <'+IntToStr(NewTopicId)+'>';
            end;
          end else begin
            s := s + StrToHex(msg.s, 0);
            EdTopicI.Text:='0';
          end;
        end else
          s := s + StrToHex(msg.s, 0); // binary
      // ---------------------------
      // MQTT message
      // ---------------------------
      end  else begin
        s := s + ' MQTT ';
        TopicId := $100*ord(msg.s[4]) + ord(msg.s[5]);
        if (OkErr = 1) then begin
          s := s + StrToHex(msg.s, 8) // JSON
        end else begin
          s := s + StrToHex(msg.s, 0); // binary
        end;
        s := s + ' -- <TopicId=';
        if (TopicId > 0) then
          s := s + IntToStr(TopicId) + '>'
        else
          s := s + '?>';
      end;
    end; // not err
    if CbTs.Checked then
      s := s + ' -- <'+IntToStr(millis)+' ms>';
    LB.Items.Add(s);
  end;
end;

// =====================================================
// C number string into Pascal number string
// =====================================================
function TForm1.num_str_c2pas(s : string) : string;
var ss : string;
begin
  s := AnsiLowerCase(Trim(s));
  result := s;
  if length(s)>2 then begin
    ss := copy(s, 1, 2);
    if ss = '0x' then begin
      result := '$'+copy(s,3,length(s)-2);
    end;
  end;
end;

// =====================================================
// Create
// =====================================================
procedure TForm1.FormCreate(Sender : TObject);
var ini : TIniFile;
    val : word;
begin
  ini := TIniFile.Create('NodeTest.ini');
  ComPortStr := AnsiUpperCase(ini.ReadString('Config','Port','COM1'));
  NodeID := ini.ReadInteger('Config','ID',$FFFF);
  val := ini.ReadInteger('Config','Security',$FFFF);
  security_to_CB(val);
  EdTopic.Text := ini.ReadString('MQTT','topic','101');
  EdTopicVal.Text := ini.ReadString('MQTT','val','12.3');
  CBcipherM.Checked := ini.ReadInteger('MQTT','cipher', 0) <> 0;
  CBcipherH.Checked := ini.ReadInteger('HBus','cipher', 0) <> 0;
  ini.Free;
  EdNode.Text := '0x' + IntToHex(NodeID,3);
  EdNewID.Text := EdNode.Text;
  Label2DblClick(Sender); // select COM port
  HB := THbRxtx.Create(ComPortStr);
  if HB.ComPort.Connected then
    Label3.Caption:='Connected'
  else
    Label3.Caption:='Disonnected';
  HBcmd := THbCmd.Create;
  EdCipNotesDblClick(Sender);
  EdEENotesDblClick(Sender);

  EdOwnId.Text := '0x'+IntToHex(HBcmd.OwnID,3);
  EdOwnIdDblClick(Sender);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);

  Ready := true;
  CBcipherHChange(Sender);
  CBcipherMChange(Sender);
end;

// =====================================================
// Destroy
// =====================================================
procedure TForm1.FormDestroy(Sender : TObject);
var ini : TIniFile;
    val : word;
begin
  EdRoundsDblClick(Sender); // save cipher changes
  ini := TIniFile.Create('NodeTest.ini');
  if HB.ComPort.Connected then
    ini.WriteString('Config','Port',AnsiUpperCase(ComPortStr));
  ini.WriteInteger('Config','ID',NodeID);
  ini.WriteString('MQTT','topic',EdTopic.Text);
  ini.WriteString('MQTT','val',EdTopicVal.Text);
  if CBcipherM.Checked then val := 1 else val := 0;
  ini.WriteInteger('MQTT','cipher', val);
  if CBcipherH.Checked then val := 1 else val := 0;
  ini.WriteInteger('HBus','cipher', val);
  ini.Free;
  HB.Free;
  HBcmd.Free;
end;

// =====================================================
// Change COM port
// =====================================================
procedure TForm1.CbPortsChange(Sender : TObject);
begin
  ComPortStr := CbPorts.Text;
  if HB.ComPort.Connected then begin
    HB.FlushTx;
    HB.ComPort.Close;
  end;
  Label3.Caption:='Disonnected';
  if Trim(ComPortStr) <> '' then begin
    HB.ComPort.Port := ComPortStr;
    try
      HB.PortOk := true;
      HB.ComPort.Open;
      if HB.ComPort.Connected then
        Label3.Caption:='Connected';
    except
      if HB.ComPort.Connected then
        HB.ComPort.Connected := false;
      HB.PortOk := false;
    end;
  end;
  HB.FlushRx;
  HB.FlushTx;
end;

// =====================================================
// Cipher changed
// =====================================================
procedure TForm1.EdRoundsDblClick(Sender : TObject);
var s : string;
begin
  HB.cipher.Notes := EdCipNotes.Text;
  s := num_str_c2pas(EdKey1.Text);
  HB.Cipher.FlashKey[0] := StrToIntDef(s, $60F3C66D);
  s := num_str_c2pas(EdKey2.Text);
  HB.Cipher.FlashKey[1] := StrToIntDef(s, $5DF53900);
  s := num_str_c2pas(EdKey3.Text);
  HB.Cipher.FlashKey[2] := StrToIntDef(s, $4F533EB6);
  s := num_str_c2pas(EdKey4.Text);
  HB.Cipher.FlashKey[3] := StrToIntDef(s, $E42B2A61);
  s := num_str_c2pas(EdLFSR1.Text);
  HB.Cipher.LFSR1 := StrToIntDef(s, $1EDC6F41);
  s := num_str_c2pas(EdLFSR2.Text);
  HB.Cipher.LFSR2 := StrToIntDef(s, $04C11DB7);
  s := num_str_c2pas(EdLFSR16.Text);
  HB.Cipher.LFSR16 := StrToIntDef(s, $755B);
  HB.Cipher.Rounds := StrToIntDef(EdRounds.Text, 6);
end;

// =====================================================
// Exit from tab
// =====================================================
procedure TForm1.PageControl1Exit(Sender : TObject);
begin
  EdRoundsDblClick(Sender); // save changes
end;

// =====================================================
// Debug cipher
// =====================================================
procedure TForm1.Panel1DblClick(Sender : TObject);
var i : integer;
    s, ss : string;
begin
{
  s := '';
  for i:=0 to 8 do
    s := s + char($30 + i);
  for i:=8 to 31 do
    s := s + '1';
  LB.Items.Add(s);
  s := HB.Cipher.encrypt(s);
  ss := '';
  for i:=1 to length(s)-1 do begin
    ss := ss + IntToHex(ord(s[i]),2) + ' ';
    if (i and 7) = 0 then
      ss := ss + ' ';
  end;
  LB.Items.Add(ss);
  ss := HB.Cipher.decrypt(s);
  LB.Items.Add(ss);
}
end;

// =====================================================
// Reload cipher values
// =====================================================
procedure TForm1.EdCipNotesDblClick(Sender : TObject);
begin
  EdCipNotes.Text := HB.Cipher.Notes;
  EdKey1.Text:='0x'+IntToHex(HB.Cipher.FlashKey[0],8);
  EdKey2.Text:='0x'+IntToHex(HB.Cipher.FlashKey[1],8);
  EdKey3.Text:='0x'+IntToHex(HB.Cipher.FlashKey[2],8);
  EdKey4.Text:='0x'+IntToHex(HB.Cipher.FlashKey[3],8);
  EdRounds.Text:=IntToStr(HB.Cipher.Rounds);
  EdLFSR1.Text:='0x'+IntToHex(HB.Cipher.LFSR1,8);
  EdLFSR2.Text:='0x'+IntToHex(HB.Cipher.LFSR2,8);
  EdLFSR16.Text:='0x'+IntToHex(HB.Cipher.LFSR16,4);
end;

// =====================================================
// EEPROM key to form
// =====================================================
procedure TForm1.EdEENotesDblClick(Sender : TObject);
begin
  EdEENotes.Text := HB.Cipher.EENotes;
  EdKey5.Text:='0x'+IntToHex(HB.Cipher.EEKey[0],8);
  EdKey6.Text:='0x'+IntToHex(HB.Cipher.EEKey[1],8);
  EdKey7.Text:='0x'+IntToHex(HB.Cipher.EEKey[2],8);
  EdKey8.Text:='0x'+IntToHex(HB.Cipher.EEKey[3],8);
end;

// =====================================================
// EE cipher from form to HBcipher
// =====================================================
procedure TForm1.EdKey5DblClick(Sender : TObject);
var s : string;
begin
  HB.Cipher.EENotes := EdEENotes.Text;
  s := num_str_c2pas(EdKey5.Text);
  HB.Cipher.EEKey[0] := StrToIntDef(s, $4c25dc00);
  s := num_str_c2pas(EdKey6.Text);
  HB.Cipher.EEKey[1] := StrToIntDef(s, $bcb2e7dc);
  s := num_str_c2pas(EdKey7.Text);
  HB.Cipher.EEKey[2] := StrToIntDef(s, $89eb06ab);
  s := num_str_c2pas(EdKey8.Text);
  HB.Cipher.EEKey[3] := StrToIntDef(s, $15227cb7);
end;

// =====================================================
// Read node REV
// =====================================================
procedure TForm1.BtnRevClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  TxMsg := HBcmd.CmdRev(NodeID);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Save EEPROM cipher to file
// =====================================================
procedure TForm1.BtnSaveEECipClick(Sender : TObject);
begin
  EdKey5DblClick(Sender);
  SaveCipher.Filter := 'EEPROM cipher files|*.ecip|All files|*.*';
  SaveCipher.InitialDir := ExtractFilePath(Application.ExeName);
  if SaveCipher.Execute then begin
    if FileExists(SaveCipher.FileName) then begin
      if FileExists(SaveCipher.FileName+'~') then begin
        if FileExists(SaveCipher.FileName+'~~') then
           DeleteFile(SaveCipher.FileName+'~~');
        RenameFile(SaveCipher.FileName+'~',SaveCipher.FileName+'~~');
      end;
      RenameFile(SaveCipher.FileName, SaveCipher.FileName+'~');
    end;
    HB.Cipher.SaveEECip(SaveCipher.FileName);
    EdEENotesDblClick(Sender);
  end;
end;

// =====================================================
// Collect nodes
// =====================================================
procedure TForm1.BtnCollectClick(Sender : TObject);
var s : string;
    grp, slots : integer;
begin
  HBcmd.Flush;
  grp := StrToIntDef(EdGroup.Text,1);
  EdGroup.Text := IntToStr(grp);
  slots := StrToIntDef(EdSlots.Text,32);
  if (slots < 8) then
    slots := 8;
  if (slots > 255) then
    slots := 255;
  EdSlots.Text := IntToStr(slots);
  TxMsg := HBcmd.CmdCollect(grp, slots);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Create key
// =====================================================
procedure TForm1.BtnCreateKeyClick(Sender : TObject);
var s1, s2, ss : string;
    i : integer;
begin
  if InputQuery('Create KEY from a passphrase', 'Enter a passphrase', TRUE, s1) then begin
    if InputQuery('Create KEY from a passphrase', 'Enter the same passphrase again', TRUE, s2) then begin
       if s1 = s2 then begin
          s1 := s1 + 'HBsalt1_<nTw[q3z';
          ss := AnsiUpperCase(SHA1Print(SHA1String(s1)));
          s1 := copy(ss, 1, 8);
          EdKey1.Text := '0x'+s1;
          s1 := copy(ss, 9, 8);
          EdKey2.Text := '0x'+s1;
          s1 := copy(ss, 17, 8);
          EdKey3.Text := '0x'+s1;
          s1 := copy(ss, 25, 8);
          EdKey4.Text := '0x'+s1;
          ShowMessage('New flash keys generated');
       end else
         ShowMessage('Entered passphrases do not match, try again');
    end;
  end;
end;

// =====================================================
// Send costom command
// =====================================================
procedure TForm1.BtnCustomCmdClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  TxMsg := HBcmd.CmdCustom(NodeID, EdCustomCmd.Text);
  if TxMsg.valid then begin
    s := HB.Tx(TxMsg);
    inc(HBcmd.MsgId);
  end;
end;

// =====================================================
// Load EEPROM cipher from file
// =====================================================
procedure TForm1.BtnLoadEECipClick(Sender : TObject);
begin
  OpenCipher.Filter := 'EEPROM cipher files|*.ecip|All files|*.*';
  OpenCipher.InitialDir := ExtractFilePath(Application.ExeName);
  if OpenCipher.Execute then begin
    HB.Cipher.ReadEECip(OpenCipher.FileName);
    EdEENotesDblClick(Sender);
    HB.Cipher.Calc_Key;
  end;
end;

// =====================================================
// Load flash cipher from file
// =====================================================
procedure TForm1.BtnLoadFlashCipClick(Sender : TObject);
begin
  OpenCipher.Filter := 'Flash cipher files|*.fcip|All files|*.*';
  OpenCipher.InitialDir := ExtractFilePath(Application.ExeName);
  if OpenCipher.Execute then begin
    HB.Cipher.ReadFlashCip(OpenCipher.FileName);
    EdCipNotesDblClick(Sender);
    HB.Cipher.Calc_Key;
  end;
end;

// =====================================================
// Create EEPROM key
// =====================================================
procedure TForm1.BtnEECreateClick(Sender : TObject);
var s1, s2, ss : string;
    i : integer;
begin
  if InputQuery('Create KEY from a passphrase', 'Enter a passphrase', TRUE, s1) then begin
    if InputQuery('Create KEY from a passphrase', 'Enter the same passphrase again', TRUE, s2) then begin
       if s1 = s2 then begin
          s1 := s1 + 'HBsalt2_<nTw[q3z';
          ss := AnsiUpperCase(SHA1Print(SHA1String(s1)));
          s1 := copy(ss, 1, 8);
          EdKey5.Text := '0x'+s1;
          s1 := copy(ss, 9, 8);
          EdKey6.Text := '0x'+s1;
          s1 := copy(ss, 17, 8);
          EdKey7.Text := '0x'+s1;
          s1 := copy(ss, 25, 8);
          EdKey8.Text := '0x'+s1;
          ShowMessage('New EEPROM keys generated');
       end else
         ShowMessage('Entered passphrases do not match, try again');
    end;
  end;
end;

// =====================================================
// Generate file "cipher.h"
// =====================================================
procedure TForm1.BtnMakeCipClick(Sender : TObject);
var fn : string;
    sl : TStringList;
begin
  fn := ExtractFilePath(Application.ExeName) + 'cipher.h';
  if FileExists(fn) then begin
    if FileExists(fn+'~') then begin
      if FileExists(fn+'~~') then
        DeleteFile(fn+'~~');
      RenameFile(fn+'~',fn+'~~');
    end;
    RenameFile(fn,fn+'~');
  end;
  EdCipNotesDblClick(Sender); // reload cipher values
  sl := TStringList.Create;
  sl.Add('/* Cipher codes for HBus, https://github.com/akouz/HBus');
  sl.Add(' * Generated on '+DateToStr(now)+' at '+TimeToStr(now) +
    ' by NodeTest');
  sl.Add(' * '+EdCipNotes.Text + ' */');
  sl.Add('enum{');
  sl.Add('  KEY1   = ' + EdKey1.Text + ',');
  sl.Add('  KEY2   = ' + EdKey2.Text + ',');
  sl.Add('  KEY3   = ' + EdKey3.Text + ',');
  sl.Add('  KEY4   = ' + EdKey4.Text + ',');
  sl.Add('  ROUNDS = ' + EdRounds.Text + ',');
  sl.Add('  LFSR1  = ' + EdLFSR1.Text + ',');
  sl.Add('  LFSR2  = ' + EdLFSR2.Text + ',');
  sl.Add('  LFSR16 = ' + EdLFSR16.Text);
  sl.Add('};');
  sl.Add('/* EOF */');
  sl.SaveToFile(fn);
  sl.Free;
  ShowMessage('File "cipher.h" generated');
end;

// =====================================================
// Save flash cipher to file
// =====================================================
procedure TForm1.BtnSaveFlashCipClick(Sender : TObject);
begin
  EdRoundsDblClick(Sender);
  SaveCipher.Filter := 'Flash cipher files|*.fcip|All files|*.*';
  SaveCipher.InitialDir := ExtractFilePath(Application.ExeName);
  if SaveCipher.Execute then begin
    if FileExists(SaveCipher.FileName) then begin
      if FileExists(SaveCipher.FileName+'~') then begin
        if FileExists(SaveCipher.FileName+'~~') then
           DeleteFile(SaveCipher.FileName+'~~');
        RenameFile(SaveCipher.FileName+'~',SaveCipher.FileName+'~~');
      end;
      RenameFile(SaveCipher.FileName, SaveCipher.FileName+'~');
    end;
    HB.Cipher.SaveFlashCip(SaveCipher.FileName);
    EdCipNotesDblClick(Sender);
  end;
end;

// =====================================================
// Save security settings
// =====================================================
procedure TForm1.BtnSaveSecurityClick(Sender : TObject);
var ini : TIniFile;
    val : word;
begin
  ini := TIniFile.Create('NodeTest.ini');
  val := CB_to_security;
  ini.WriteInteger('Config','Security',val);
  ini.Free;
end;

// =====================================================
// Send MQTT message PUBLISH
// =====================================================
procedure TForm1.BtnPublishClick(Sender : TObject);
var topicId, msg_id : word;
    s : string;
begin
  HBcmd.Flush;
  topicId := StrToIntDef(EdTopic.text,100);
  EdTopic.text := IntToStr(topicId);
  TxMsg := HBcmd.Publish(topicId, HBcmd.MsgID, EdTopicVal.Text);
  s := HB.Tx(TxMsg);
  inc(HBcmd.MsgId);
  if HBcmd.MsgId >= $FFFE then
    HBcmd.MsgId := 1;
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Send MQTT message REGISTER
// =====================================================
procedure TForm1.BtnRegisterClick(Sender : TObject);
var topicId, msg_id : word;
    s : string;
begin
  HBcmd.Flush;
  topicId := StrToIntDef(EdTopic1.text,100);
  TxMsg := HBcmd.Register(topicId, HBcmd.MsgID, EdTopicName.Text);
  s := HB.Tx(TxMsg);
  inc(HBcmd.MsgId);
  if HBcmd.MsgId >= $FFFE then
    HBcmd.MsgId := 1;
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;


// =====================================================
// Boot
// =====================================================
procedure TForm1.BtnBootClick(Sender : TObject);
var pause : integer;
    s : string;
begin
  HBcmd.Flush;
  pause := StrToIntDef(EdBootPause.Text, 10); // sec
  EdBootPause.Text := IntToStr(pause);
  TxMsg := HBcmd.CmdBoot(NodeID, pause);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Beep
// =====================================================
procedure TForm1.BtnBeepClick(Sender : TObject);
var dur : integer;
    s : string;
begin
  HBcmd.Flush;
  dur := StrToIntDef(EdDuration.Text, 2); // sec
  EdDuration.Text := IntToStr(dur);
  TxMsg := HBcmd.CmdBeep(NodeID, dur);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
//  Set New ID
// =====================================================
procedure TForm1.BtnNewIDClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  EdNewIDDblClick(Sender);
  TxMsg := HBcmd.CmdSetID(NodeID, NewID);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// PING
// =====================================================
procedure TForm1.BtnPingClick(Sender : TObject);
var pause : integer;
    s : string;
begin
  HBcmd.Flush;
  pause := StrToIntDef(EdPause.Text, 0); // sec
  EdPause.Text := IntToStr(pause);
  TxMsg := HBcmd.CmdPing(NodeID, pause);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Read Description
// =====================================================
procedure TForm1.BtnRdDescrClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  TxMsg := HBcmd.CmdDescr(NodeID,'', false);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Write Description
// =====================================================
procedure TForm1.BtnWrDescrClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  s := Trim(EdDescr.Text);
  if length(s) > 63 then
    s := copy(s,1,64);
  EdDescr.Text := s;
  TxMsg := HBcmd.CmdDescr(NodeID, s, true);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Read security
// =====================================================
procedure TForm1.BtnRdSecurityClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  TxMsg := HBcmd.CmdSecurity(NodeID, '', false);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Write security
// =====================================================
procedure TForm1.BtnWrSecurityClick(Sender : TObject);
var s : string;
    val, i : word;
begin
  val := CB_to_security;
  s := '' + char(byte(val >> 8)) + char(byte(val)); // security settings
  if CbEEcipher.Checked then begin
    for i:=0 to 3 do begin
      s := s + char(byte(HB.Cipher.EEKey[i] >> 24));
      s := s + char(byte(HB.Cipher.EEKey[i] >> 16));
      s := s + char(byte(HB.Cipher.EEKey[i] >> 8));
      s := s + char(byte(HB.Cipher.EEKey[i]));
    end;
  end;
  TxMsg := HBcmd.CmdSecurity(NodeID, s, true);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;


// =====================================================
// Use cipher for HBus
// =====================================================
procedure TForm1.CBcipherHChange(Sender : TObject);
begin
  if Ready then
    HB.EncryptHB := CBcipherH.Checked;
end;

// =====================================================
// Use cipher for MQTT
// =====================================================
procedure TForm1.CBcipherMChange(Sender : TObject);
begin
  if Ready then
    HB.EncryptMQ := CBcipherM.Checked;
end;

// =====================================================
// Read TopicId and TopicName
// =====================================================
procedure TForm1.BtnRdTopicClick(Sender : TObject);
var ti : byte;
    s : string;
begin
  ti := StrToIntDef(EdTopicI.Text,0);
  HBcmd.Flush;
  TxMsg := HBcmd.CmdRdTopic(NodeID, ti);
  s := HB.Tx(TxMsg);
  EdTopicI.Text := IntToStr(ti+1);
end;

// =====================================================
// Read node STATUS
// =====================================================
procedure TForm1.BtnStatusClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  TxMsg := HBcmd.CmdStatus(NodeID);
  s := HB.Tx(TxMsg);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Node ID
// =====================================================
procedure TForm1.EdNodeDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdNode.Text);
  NodeID := StrToIntDef(s, $07FF);
  EdNode.Text := '0x'+IntToHex(NodeID,3);
end;

// =====================================================
// Own ID
// =====================================================
procedure TForm1.EdOwnIdDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdOwnId.Text);
  HBcmd.OwnID := StrToIntDef(s, $07FF);
  if (HBcmd.OwnID < 1) then
     HBcmd.OwnID := 1;
  if (HBcmd.OwnID > $07FF) then
     HBcmd.OwnID := $7FF;
  EdOwnId.Text := '0x'+IntToHex(HBcmd.OwnID,3)
end;

// =====================================================
// MsgID
// =====================================================
procedure TForm1.EdMsgIdDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdMsgId.Text);
  HBcmd.MsgID := StrToIntDef(s, 1);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2)
end;

// =====================================================
// New ID
// =====================================================
procedure TForm1.EdNewIDDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdNewId.Text);
  NewID := StrToIntDef(s, $07FF);
  if (NewID = 0) then
     NewID := 1;
  if (NewID > $07FF) then
     NewID := $07FF;
  EdNewID.Text := '0x'+IntToHex(NewID,4)
end;

end.

