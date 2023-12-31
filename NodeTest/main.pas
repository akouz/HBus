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
  Classes, SysUtils, FileUtil, CPort, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Registry, IniFiles, Clipbrd, Sha1,
  HBrxtxU, HBcmdU, HButilsU, HBbootU, HBsysU;

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
    BtnExploreNet: TButton;
    BtnList: TButton;
    BtnReportNode: TButton;
    CBping1: TCheckBox;
    CbPorts : TComboBox;
    CbDamp : TCheckBox;
    CBcipherM : TCheckBox;
    CBcipherH : TCheckBox;
    CBrev : TCheckBox;
    CBstatus : TCheckBox;
    CBcollect : TCheckBox;
    CBping : TCheckBox;
    CBboot : TCheckBox;
    CBrddescr : TCheckBox;
    CBwrdescr : TCheckBox;
    CBcustomcmd : TCheckBox;
    CBtopic : TCheckBox;
    CBrdSecurity : TCheckBox;
    CBignoreTS : TCheckBox;
    CBpublish : TCheckBox;
    CBregister : TCheckBox;
    CBbroadcast : TCheckBox;
    CbEEcipher : TCheckBox;
    CbTs : TCheckBox;
    CbSetSecurity: TCheckBox;
    CbCipherH2: TCheckBox;
    CbNode: TComboBox;
    EdEENotes : TEdit;
    EdGroup : TEdit;
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
    GbNetwork: TGroupBox;
    GbNode: TGroupBox;
    Label1 : TLabel;
    Label10 : TLabel;
    Label11 : TLabel;
    Label12: TLabel;
    LblCrc: TLabel;
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
    LB2: TListBox;
    OpenCipher : TOpenDialog;
    OpenSketch: TOpenDialog;
    PgCtrl : TPageControl;
    Panel1 : TPanel;
    Panel2 : TPanel;
    Panel3 : TPanel;
    PnlSecurityM: TPanel;
    PnlSecurityH: TPanel;
    RbModule: TRadioButton;
    RbSketch: TRadioButton;
    RbProject: TRadioButton;
    RbName: TRadioButton;
    RbLocation: TRadioButton;
    RbDescr: TRadioButton;
    SaveCipher : TSaveDialog;
    TsSys: TTabSheet;
    TsCipher : TTabSheet;
    Timer1ms : TTimer;
    Timer1sec : TTimer;
    TsHBus : TTabSheet;
    TsMQTT : TTabSheet;
    Timer10ms : TTimer;
    procedure BtnBeepClick(Sender : TObject);
    procedure BtnBootClick(Sender : TObject);
    procedure BtnExploreNetClick(Sender: TObject);
    procedure BtnCollectClick(Sender : TObject);
    procedure BtnCreateKeyClick(Sender : TObject);
    procedure BtnCustomCmdClick(Sender : TObject);
    procedure BtnEECreateClick(Sender : TObject);
    procedure BtnListClick(Sender: TObject);
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
    procedure BtnReportNodeClick(Sender: TObject);
    procedure BtnRevClick(Sender : TObject);
    procedure BtnSaveEECipClick(Sender : TObject);
    procedure BtnSaveFlashCipClick(Sender : TObject);
    procedure BtnSaveSecurityClick(Sender : TObject);
    procedure BtnStatusClick(Sender : TObject);
    procedure BtnWrDescrClick(Sender : TObject);
    procedure BtnWrSecurityClick(Sender : TObject);
    procedure CBcipherHChange(Sender : TObject);
    procedure CBcipherMChange(Sender : TObject);
    procedure CbCipherH2Change(Sender: TObject);
    procedure CbNodeChange(Sender: TObject);
    procedure CBping1Click(Sender: TObject);
    procedure CBpingChange(Sender: TObject);
    procedure CbPortsChange(Sender : TObject);
    procedure EdCipNotesDblClick(Sender : TObject);
    procedure EdEENotesDblClick(Sender : TObject);
    procedure EdKey5DblClick(Sender : TObject);
    procedure EdMsgIdDblClick(Sender : TObject);
    procedure EdNewIDDblClick(Sender : TObject);
    procedure EdNodeChange(Sender: TObject);
    procedure EdNodeDblClick(Sender : TObject);
    procedure EdOwnIdDblClick(Sender : TObject);
    procedure EdRoundsDblClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure Label2DblClick(Sender : TObject);
    procedure LB2DblClick(Sender: TObject);
    procedure LBDblClick(Sender : TObject);
    procedure LBKeyPress(Sender : TObject; var Key : char);
    procedure PgCtrlChange(Sender: TObject);
    procedure PgCtrlExit(Sender : TObject);
    procedure Panel1DblClick(Sender : TObject);
    procedure RbSketchChange(Sender: TObject);
    procedure Timer10msTimer(Sender : TObject);
    procedure Timer1msTimer(Sender : TObject);
    procedure Timer1secTimer(Sender : TObject);
  private
    { private declarations }
    ComPortStr : string;
    BootFn : string;
    NodeID : word;
    NewID : word;
    millis : longword;
    WasConnected : boolean;
    function FCbToSecurity : word;
    function FRbChecked : byte;
    procedure FSecurityToCb(val : word);
  public
    { public declarations }
    function StrToHex(s : string; txt_i : byte) : string;
    procedure PrintHbMsg(msg : THbMsg);
    function num_str_c2pas(s : string) : string;
  end;

var
  Ready  : boolean = false;
  Form1  : TForm1;

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
procedure TForm1.LB2DblClick(Sender: TObject);
begin
  LB2.Clear;
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
// Page change
// =====================================================
procedure TForm1.PgCtrlChange(Sender: TObject);
begin
  If PgCtrl.ActivePage.Caption = 'Sys' then begin
    LB.Visible := false;
    LB2.Visible := true;
  end else begin
      LB.Visible := true;
      LB2.Visible := false;
  end;
end;

// =====================================================
// 10 ms
// =====================================================
procedure TForm1.Timer10msTimer(Sender : TObject);
var i, j : integer;
    s : string;
begin
  HBcmd.Tick10ms;
  HBrxtx.Tick10ms;
  HBboot.Tick10ms;
  HBsys.Tick10ms;
  if (not BtnExploreNet.Enabled) and (not HBsys.busy) then begin // explore batch finished
     BtnListClick(Sender);
     CbNode.Clear;
     CbNode.ItemIndex := -1;
     if HBsys.Count > 0 then begin
       GbNode.Enabled := true;
       j := 0;
       for i:=0 to HBsys.Count-1 do begin
         s := '0x'+IntToHex(THBnode(HBsys.Objects[i]).NodeID, 4);
         CbNode.Items.Add(s);
         if s = EdNode.Text then
           j := i;
       end;
       CbNode.ItemIndex := j;
     end;
     CbNodeChange(Sender);
  end;
  BtnExploreNet.Enabled := not HBsys.busy;
  if not BtnBoot.Enabled then begin
    if HBboot.State = 0 then begin
       BtnBoot.Enabled := true;
       if HBboot.ErrStr <> '' then
          ShowMessage(HBboot.ErrStr);
    end;
  end;
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
  if HBrxtx.ComPort.Connected then begin
     try
        rx := HBrxtx.Rx;
        // debug messages
        cnt := HBrxtx.DbgList.Count;
        if (cnt > 0) then begin
          s := '';
          if CbTs.Checked then
            s := ' -- <'+IntToStr(millis)+' ms>';
          for i:=0 to cnt-1 do begin
            LB.Items.Add(' - dbg: '+HBrxtx.DbgList.Strings[i]+s);
            s := '';
          end;
          for i:=0 to cnt-1 do
            HBrxtx.DbgList.Delete(cnt-1-i);
        end;
        // dump
        cnt := HBrxtx.DampList.Count;
        if (cnt > 0) then begin
          s := '';
          if CbTs.Checked then
            s := ' -- <'+IntToStr(millis)+' ms>';
          if CbDamp.Checked then begin
            for i:=0 to cnt-1 do begin
              LB.Items.Add(' --- bus: '+HBrxtx.DampList.Strings[i]+s);
              s := '';
            end;
          end;
          for i:=0 to cnt-1 do
            HBrxtx.DampList.Delete(cnt-1-i);
        end;
        // received
        if rx.valid then begin
          if not rx.err then begin
            HBsys.Process(rx);          // process recived message, extract useful info
            if (rx.mqtt) then begin
              msg_id := HBrxtx.MsgID;   // received
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
        if HBrxtx.TxStatus = 2 then
           HBrxtx.TxStatus := 0;
    except
      if HBrxtx.ComPort.Connected then
        HBrxtx.ComPort.Connected := false;
      HBrxtx.PortOk := false;
    end;
  end;
end;

// =====================================================
// 1 sec
// =====================================================
procedure TForm1.Timer1secTimer(Sender : TObject);
begin
  if (not HBrxtx.ComPort.Connected) and (not WasConnected) then begin
    Label2DblClick(Sender);
    CbPortsChange(Sender);
  end;
  WasConnected := HBrxtx.ComPort.Connected;
end;

// =====================================================
// Convert security checkboxes to word
// =====================================================
function TForm1.FCbToSecurity : word;
var res : word;
begin
  res := 0;
  if  CBrev.Checked then         res := res or 1;
  if  CBstatus.Checked then      res := res or 2;
  if  CBcollect.Checked then     res := res or 4;
  if  (CBping.Checked) or (CBping1.Checked) then        res := res or 8;
  if  CBboot.Checked then        res := res or $10;
  if  CBrddescr.Checked then     res := res or $20;
  if  CBwrdescr.Checked then     res := res or $40;
  if  CBcustomcmd.Checked then   res := res or $80;
  if  CBtopic.Checked then       res := res or $100;
  if  CBrdsecurity.Checked then  res := res or $200;
  if  CBignoreTS.Checked then    res := res or $400;
  if  CBpublish.Checked then     res := res or $2000;
  if  CBregister.Checked then    res := res or $4000;
  if  CBbroadcast.Checked then   res := res or $8000;
  result := res;
end;

// =====================================================
// Convert radiobuttons to param
// =====================================================
function TForm1.FRbChecked: byte;
begin
  result := 0;
  if RbLocation.Checked then
    result := 2
  else if RbDescr.Checked then
    result := 4
  else if RbProject.Checked then
    result := 6
  else if RbSketch.Checked then
    result := 8
  else if RbModule.Checked then
    result := 10;
end;

// =====================================================
// Convert word to security checkboxes
// =====================================================
procedure TForm1.FSecurityToCb(val : word);
begin
  CBrev.Checked          := (val and 1) <> 0;
  CBstatus.Checked       := (val and 2) <> 0;
  CBcollect.Checked      := (val and 4) <> 0;
  CBping.Checked         := (val and 8) <> 0;
  CBping1.Checked        := CBping.Checked;
  CBboot.Checked         := (val and $10) <> 0;
  CBrddescr.Checked      := (val and $20) <> 0;
  CBwrdescr.Checked      := (val and $40) <> 0;
  CBcustomcmd.Checked    := (val and $80) <> 0;
  CBtopic.Checked        := (val and $100) <> 0;
  CBrdsecurity.Checked   := (val and $200) <> 0;
  CBignoreTS.Checked     := (val and $400) <> 0;
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
    TopicId, NewTopicId, SecSet : word;
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
      s := s + ', crc=' + IntToHex(HBrxtx.LastCrc[0],4);
      s := s + '/' + IntToHex(HBrxtx.LastCrc[1],4);
    end else begin
      // ---------------------------
      // HBus message
      // ---------------------------
      if (not msg.mqtt) then begin
        s := s + ' HBus ';
        if (cmd = $82) and (OkErr = 1) then    // STATUS reply
          s := s + StrToHex(msg.s, 12)          // JSON
        else if (cmd = $88) then
          s := s + StrToHex(msg.s, 13)          // description
        else if (cmd = $A) or (cmd = $8A) then // custom cmd and reply
          s := s + StrToHex(msg.s, 12)
        else if (cmd = $8B) then begin         // topic
          if (OkErr < $80) then begin
            if (length(msg.s) >= 14) then begin
              NewTopicId := $100*ord(msg.s[13]) + ord(msg.s[14]);
              s := s + StrToHex(msg.s, 14);
              s := s + ' <'+IntToStr(NewTopicId)+'>';
            end;
          end else begin
            s := s + StrToHex(msg.s, 0);
            EdTopicI.Text:='0';
          end;
        end else begin
          s := s + StrToHex(msg.s, 0); // binary
          if (cmd = $89) and (OkErr < $80) then begin // read secirity
            SecSet := $100*ord(msg.s[13]) + ord(msg.s[14]);
            FSecurityToCb(SecSet); // tick checkboxes
            if (OkErr = 1) then // OK1, EEPROM cipher is valid
              CbEEcipher.Checked:=false; // then do not send cipher, it will be ignored anyway
          end;
        end;
      // ---------------------------
      // MQTT message
      // ---------------------------
      end  else begin
        s := s + ' MQTT ';
        TopicId := $100*ord(msg.s[4]) + ord(msg.s[5]);
        if (OkErr = 1) then begin
          s := s + StrToHex(msg.s, 12) // JSON
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
  result := AnsiLowerCase(Trim(s));
  if length(result)>2 then begin
    ss := copy(result, 1, 2);
    if ss = '0x' then begin
      result := '$'+copy(result, 3, length(result)-2);
    end;
  end;
end;

// =====================================================
// Create
// =====================================================
procedure TForm1.FormCreate(Sender : TObject);
var ini : TIniFile;
    val : word;
    s : string;
begin
  ini := TIniFile.Create('NodeTest.ini');
  ComPortStr := AnsiUpperCase(ini.ReadString('Config','Port','COM1'));
  s := ini.ReadString('Config', 'NodeID', '0x0001');
  self.NodeID := 1;
  if length(s) > 3 then begin
    s := '$' + copy(s, 3, length(s) - 2);
    self.NodeID := StrToIntDef(s, 1);
  end;
  s := '0x' + IntToHex(self.NodeID, 4);
  EdNode.Text := s;
  EdNewID.Text := EdNode.Text;
  val := ini.ReadInteger('Config','Security',$FFFF);
  FSecurityToCb(val);
  EdTopic.Text := ini.ReadString('MQTT','topic','101');
  EdTopicVal.Text := ini.ReadString('MQTT','val','12.3');
  CBcipherM.Checked := ini.ReadInteger('MQTT','cipher', 0) <> 0;
  CBcipherH.Checked := ini.ReadInteger('HBus','cipher', 0) <> 0;
  BootFn := ini.ReadString('Sketch','FileName','');
  ini.Free;
  Label2DblClick(Sender); // select COM port
  HBrxtx := THbRxtx.Create(ComPortStr);
  if HBrxtx.ComPort.Connected then
    Label3.Caption:='Connected'
  else
    Label3.Caption:='Disonnected';
  HBcmd := THbCmd.Create;
  EdCipNotesDblClick(Sender);
  EdEENotesDblClick(Sender);

  HBboot := THbBoot.Create;
  HBsys := THBsys.Create;

  EdOwnId.Text := '0x'+IntToHex(HBcmd.OwnID,3);
  EdOwnIdDblClick(Sender);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);

  Ready := true;
  CBcipherHChange(Sender);
  CBcipherMChange(Sender);
  PgCtrlChange(Sender);
end;

// =====================================================
// Destroy
// =====================================================
procedure TForm1.FormDestroy(Sender : TObject);
var ini : TIniFile;
    val : word;
    s : string;
begin
  EdRoundsDblClick(Sender); // save cipher changes
  ini := TIniFile.Create('NodeTest.ini');
  if HBrxtx.ComPort.Connected then
    ini.WriteString('Config','Port',AnsiUpperCase(ComPortStr));
  s := '0x'+IntToHex(self.NodeID, 4);
  ini.WriteString('Config','NodeID', s);
  ini.WriteString('MQTT','topic',EdTopic.Text);
  ini.WriteString('MQTT','val',EdTopicVal.Text);
  if CBcipherM.Checked then val := 1 else val := 0;
  ini.WriteInteger('MQTT','cipher', val);
  if CBcipherH.Checked then val := 1 else val := 0;
  ini.WriteInteger('HBus','cipher', val);
  ini.WriteString('Sketch','FileName',BootFn);
  ini.Free;
  HBrxtx.Free;
  HBcmd.Free;
  HBboot.Free;
  HBsys.free;
end;

// =====================================================
// Change COM port
// =====================================================
procedure TForm1.CbPortsChange(Sender : TObject);
begin
  ComPortStr := CbPorts.Text;
  if HBrxtx.ComPort.Connected then begin
    HBrxtx.FlushTx;
    HBrxtx.ComPort.Close;
  end;
  Label3.Caption:='Disonnected';
  if Trim(ComPortStr) <> '' then begin
    HBrxtx.ComPort.Port := ComPortStr;
    try
      HBrxtx.PortOk := true;
      HBrxtx.ComPort.Open;
      if HBrxtx.ComPort.Connected then
        Label3.Caption:='Connected';
    except
      if HBrxtx.ComPort.Connected then
        HBrxtx.ComPort.Connected := false;
      HBrxtx.PortOk := false;
    end;
  end;
  HBrxtx.FlushRx;
  HBrxtx.FlushTx;
end;

// =====================================================
// Cipher changed
// =====================================================
procedure TForm1.EdRoundsDblClick(Sender : TObject);
var s : string;
begin
  HBrxtx.cipher.Notes := EdCipNotes.Text;
  s := num_str_c2pas(EdKey1.Text);
  HBrxtx.Cipher.FlashKey[0] := StrToIntDefLW(s, $60F3C66D);
  s := num_str_c2pas(EdKey2.Text);
  HBrxtx.Cipher.FlashKey[1] := StrToIntDefLW(s, $5DF53900);
  s := num_str_c2pas(EdKey3.Text);
  HBrxtx.Cipher.FlashKey[2] := StrToIntDefLW(s, $4F533EB6);
  s := num_str_c2pas(EdKey4.Text);
  HBrxtx.Cipher.FlashKey[3] := StrToIntDefLW(s, $E42B2A61);
  s := num_str_c2pas(EdLFSR1.Text);
  HBrxtx.Cipher.LFSR1 := StrToIntDefLW(s, $1EDC6F41);
  s := num_str_c2pas(EdLFSR2.Text);
  HBrxtx.Cipher.LFSR2 := StrToIntDefLW(s, $04C11DB7);
  s := num_str_c2pas(EdLFSR16.Text);
  HBrxtx.Cipher.LFSR16 := StrToIntDefLW(s, $755B);
  HBrxtx.Cipher.Rounds := StrToIntDefLW(EdRounds.Text, 6);
end;

// =====================================================
// Exit from tab
// =====================================================
procedure TForm1.PgCtrlExit(Sender : TObject);
begin
  EdRoundsDblClick(Sender); // save changes
end;

// =====================================================
// Debug cipher
// =====================================================
procedure TForm1.Panel1DblClick(Sender : TObject);
//var i : integer;
//    s, ss : string;
begin
{
  s := '';
  for i:=0 to 8 do
    s := s + char($30 + i);
  for i:=8 to 31 do
    s := s + '1';
  LB.Items.Add(s);
  s := HBrxtx.Cipher.encrypt(s);
  ss := '';
  for i:=1 to length(s)-1 do begin
    ss := ss + IntToHex(ord(s[i]),2) + ' ';
    if (i and 7) = 0 then
      ss := ss + ' ';
  end;
  LB.Items.Add(ss);
  ss := HBrxtx.Cipher.decrypt(s);
  LB.Items.Add(ss);
}
end;

// =====================================================
// Radio button
// =====================================================
procedure TForm1.RbSketchChange(Sender: TObject);
begin
  if RbSketch.Checked or RbProject.Checked or RbModule.Checked then
    BtnWrDescr.Enabled := false
  else
    BtnWrDescr.Enabled := true;
end;

// =====================================================
// Reload cipher values
// =====================================================
procedure TForm1.EdCipNotesDblClick(Sender : TObject);
begin
  EdCipNotes.Text := HBrxtx.Cipher.Notes;
  EdKey1.Text:='0x'+IntToHex(HBrxtx.Cipher.FlashKey[0],8);
  EdKey2.Text:='0x'+IntToHex(HBrxtx.Cipher.FlashKey[1],8);
  EdKey3.Text:='0x'+IntToHex(HBrxtx.Cipher.FlashKey[2],8);
  EdKey4.Text:='0x'+IntToHex(HBrxtx.Cipher.FlashKey[3],8);
  EdRounds.Text:=IntToStr(HBrxtx.Cipher.Rounds);
  EdLFSR1.Text:='0x'+IntToHex(HBrxtx.Cipher.LFSR1,8);
  EdLFSR2.Text:='0x'+IntToHex(HBrxtx.Cipher.LFSR2,8);
  EdLFSR16.Text:='0x'+IntToHex(HBrxtx.Cipher.LFSR16,4);
end;

// =====================================================
// EEPROM key to form
// =====================================================
procedure TForm1.EdEENotesDblClick(Sender : TObject);
begin
  EdEENotes.Text := HBrxtx.Cipher.EENotes;
  EdKey5.Text:='0x'+IntToHex(HBrxtx.Cipher.EEKey[0],8);
  EdKey6.Text:='0x'+IntToHex(HBrxtx.Cipher.EEKey[1],8);
  EdKey7.Text:='0x'+IntToHex(HBrxtx.Cipher.EEKey[2],8);
  EdKey8.Text:='0x'+IntToHex(HBrxtx.Cipher.EEKey[3],8);
end;

// =====================================================
// EE cipher from form to HBcipher
// =====================================================
procedure TForm1.EdKey5DblClick(Sender : TObject);
var s : string;
begin
  HBrxtx.Cipher.EENotes := EdEENotes.Text;
  s := num_str_c2pas(EdKey5.Text);
  HBrxtx.Cipher.EEKey[0] := StrToIntDefLW(s, $4c25dc00);
  s := num_str_c2pas(EdKey6.Text);
  HBrxtx.Cipher.EEKey[1] := StrToIntDefLW(s, $bcb2e7dc);
  s := num_str_c2pas(EdKey7.Text);
  HBrxtx.Cipher.EEKey[2] := StrToIntDefLW(s, $89eb06ab);
  s := num_str_c2pas(EdKey8.Text);
  HBrxtx.Cipher.EEKey[3] := StrToIntDefLW(s, $15227cb7);
end;

// =====================================================
// Read node REV
// =====================================================
procedure TForm1.BtnRevClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  TxMsg := HBcmd.CmdRev(NodeID);
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
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
    HBrxtx.Cipher.SaveEECip(SaveCipher.FileName);
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
  grp := StrToIntDefLW(EdGroup.Text,1);
  EdGroup.Text := IntToStr(grp);
  slots := StrToIntDefLW(EdSlots.Text,32);
  if (slots < 8) then
    slots := 8;
  if (slots > 255) then
    slots := 255;
  EdSlots.Text := IntToStr(slots);
  TxMsg := HBcmd.CmdCollect(grp, slots);
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Start batch exploring HBus networks
// =====================================================
procedure TForm1.BtnExploreNetClick(Sender: TObject);
begin
  HBsys.Clear;
  BtnExploreNet.Enabled := not HBsys.StartHbusExplore; // disable while batch in progress
  GbNode.Enabled := BtnExploreNet.Enabled;
end;

// =====================================================
// Create key
// =====================================================
procedure TForm1.BtnCreateKeyClick(Sender : TObject);
var s1, s2, ss : string;
begin
  s1 := ''; s2 := '';
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
    s := HBrxtx.Tx(TxMsg);
    if s<>'' then
       ShowMessage(s);
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
    HBrxtx.Cipher.ReadEECip(OpenCipher.FileName);
    EdEENotesDblClick(Sender);
    HBrxtx.Cipher.Calc_Key;
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
    HBrxtx.Cipher.ReadFlashCip(OpenCipher.FileName);
    EdCipNotesDblClick(Sender);
    HBrxtx.Cipher.Calc_Key;
  end;
end;

// =====================================================
// Create EEPROM key
// =====================================================
procedure TForm1.BtnEECreateClick(Sender : TObject);
var s1, s2, ss : string;
begin
  s1 := ''; s2 := '';
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
// List collected nodes
// =====================================================
procedure TForm1.BtnListClick(Sender: TObject);
var i : integer;
begin
  if HBsys.Count > 0 then begin
    if LB2.Items.Count > 0 then
      LB2.Items.Add('');
    LB2.Items.Add('Collected nodes:');
    for i:= 0 to HBsys.Count-1 do
      LB2.Items.Add(HBsys.Strings[i]);
  end else
    LB2.Items.Add('No collected nodes');
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
    HBrxtx.Cipher.SaveFlashCip(SaveCipher.FileName);
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
  val := FCbToSecurity;
  ini.WriteInteger('Config','Security',val);
  ini.Free;
end;

// =====================================================
// Send MQTT message PUBLISH
// =====================================================
procedure TForm1.BtnPublishClick(Sender : TObject);
var topicId: word;
    s : string;
begin
  HBcmd.Flush;
  topicId := StrToIntDefLW(EdTopic.text,100);
  EdTopic.text := IntToStr(topicId);
  TxMsg := HBcmd.Publish(topicId, HBcmd.MsgID, EdTopicVal.Text);
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
  inc(HBcmd.MsgId);
  if HBcmd.MsgId >= $FE then
    HBcmd.MsgId := 1;
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Send MQTT message REGISTER
// =====================================================
procedure TForm1.BtnRegisterClick(Sender : TObject);
var topicId : word;
    s : string;
begin
  HBcmd.Flush;
  topicId := StrToIntDefLW(EdTopic1.text,100);
  TxMsg := HBcmd.Register(topicId, HBcmd.MsgID, EdTopicName.Text);
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
  inc(HBcmd.MsgId);
  if HBcmd.MsgId >= $FE then
    HBcmd.MsgId := 1;
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Report node
// =====================================================
procedure TForm1.BtnReportNodeClick(Sender: TObject);
var NID : word;
    s : string;
    i : integer;
begin
  if length(CbNode.Text) > 3 then begin
    s := '$' + copy(CbNode.Text, 3, length(CbNode.Text) - 2);
    NID := StrToIntDef(s, 0);
    HBsys.ReportNode(NID);
    if LB2.Items.Count > 0 then
      LB2.Items.Add('');
    LB2.Items.Add('NodeID 0x' + IntToHex(NID,4) + ':');
    for i:= 0 to HBsys.SL.Count-1 do
      LB2.Items.Add(HBsys.SL.Strings[i]);
  end else
    LB2.Items.Add('Invalid NodeID');
end;


// =====================================================
// Boot
// =====================================================
procedure TForm1.BtnBootClick(Sender : TObject);
var s : string;
    len : word;
begin
  if BootFn <> '' then
     OpenSketch.InitialDir:=ExtractFileDir(BootFn);
  OpenSketch.FileName := ExtractFileName(BootFn);
  if OpenSketch.Execute then begin
    BootFn := OpenSketch.FileName;
    s := HbBoot.ReadSketch(BootFn);
    LblCrc.Visible:=true;
    len := HbBoot.AddrMax - $3FF;
    LblCrc.Caption:='Code length '+IntToHex(len,4) + ', crc '+IntToHex(HbBoot.BufCrc,4);
    if s = '' then begin
      // LB.Items.Add(' - dbg: Last chunk '+HexStr(HbBoot.LastChunk));
      s := HbBoot.StartBatch(NodeID);
      if s = '' then begin
         LB.Items.Add(' - dbg: Loading sketch '+ ExtractFileName(BootFn));
         BtnBoot.Enabled := false;
      end;
    end else
      ShowMessage('ERROR: '+ s + ', operation aborted');
  end;
end;

// =====================================================
// Beep
// =====================================================
procedure TForm1.BtnBeepClick(Sender : TObject);
var dur : integer;
    s : string;
begin
  HBcmd.Flush;
  dur := StrToIntDefLW(EdDuration.Text, 2); // sec
  EdDuration.Text := IntToStr(dur);
  TxMsg := HBcmd.CmdBeep(NodeID, dur);
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
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
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
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
  pause := StrToIntDefLW(EdPause.Text, 1); // sec
  EdPause.Text := IntToStr(pause);
  TxMsg := HBcmd.CmdPing(NodeID, pause);
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Read Description
// =====================================================
procedure TForm1.BtnRdDescrClick(Sender : TObject);
var s : string;
begin
  HBcmd.Flush;
  TxMsg := HBcmd.CmdDescr(NodeID,'', FRbChecked);
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
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
  TxMsg := HBcmd.CmdDescr(NodeID, s, FRbChecked or 1);
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
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
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Write security
// =====================================================
procedure TForm1.BtnWrSecurityClick(Sender : TObject);
var s : string;
    val, i : word;
begin
  val := FCbToSecurity;
  s := '' + char(byte(val >> 8)) + char(byte(val)); // security settings
  if CbEEcipher.Checked then begin
    for i:=0 to 3 do begin
      s := s + char(byte(HBrxtx.Cipher.EEKey[i] >> 24));
      s := s + char(byte(HBrxtx.Cipher.EEKey[i] >> 16));
      s := s + char(byte(HBrxtx.Cipher.EEKey[i] >> 8));
      s := s + char(byte(HBrxtx.Cipher.EEKey[i]));
    end;
  end;
  TxMsg := HBcmd.CmdSecurity(NodeID, s, true);
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Use cipher for HBus
// =====================================================
procedure TForm1.CBcipherHChange(Sender : TObject);
begin
  if Ready then begin
    CBcipherM.Checked := CBcipherH.Checked;
    HBrxtx.EncryptHB := CBcipherH.Checked;
    HBrxtx.EncryptMQ := CBcipherH.Checked;
    if CBcipherH.Checked then begin
      PnlSecurityH.Color := clSkyBlue;
      PnlSecurityM.Color := clSkyBlue;
    end  else begin
      PnlSecurityH.Color := clDefault;
      PnlSecurityM.Color := clDefault;
    end;
    CBcipherH2.Checked := CBcipherH.Checked;
  end;
end;

procedure TForm1.CbCipherH2Change(Sender: TObject);
begin
  if Ready then begin
    CBcipherH.Checked := CBcipherH2.Checked;
    CBcipherHChange(Sender);
  end;
end;

// =====================================================
// Select another node
// =====================================================
procedure TForm1.CbNodeChange(Sender: TObject);
begin
  if length(CbNode.Text) > 3 then begin
     EdNode.Text := CbNode.text;
     EdNodeDblClick(Sender);
  end;
end;

// =====================================================
// Use cipher for MQTT
// =====================================================
procedure TForm1.CBcipherMChange(Sender : TObject);
begin
  if Ready then begin
    HBrxtx.EncryptHB := CBcipherH.Checked;
    HBrxtx.EncryptMQ := CBcipherM.Checked;
    CBcipherH.Checked := CBcipherM.Checked;
    if CBcipherM.Checked then begin
      PnlSecurityH.Color := clSkyBlue;
      PnlSecurityM.Color := clSkyBlue;
    end  else begin
      PnlSecurityH.Color := clDefault;
      PnlSecurityM.Color := clDefault;
    end;
  end;
end;

// =====================================================
procedure TForm1.CBping1Click(Sender: TObject);
begin
  CBping.Checked := CBping1.Checked;
end;

// =====================================================
procedure TForm1.CBpingChange(Sender: TObject);
begin
  CBping1.Checked := CBping.Checked;
end;

// =====================================================
// Read TopicId and TopicName
// =====================================================
procedure TForm1.BtnRdTopicClick(Sender : TObject);
var ti : byte;
    s : string;
begin
  ti := StrToIntDefLW(EdTopicI.Text,0);
  HBcmd.Flush;
  TxMsg := HBcmd.CmdRdTopic(NodeID, ti);
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
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
  s := HBrxtx.Tx(TxMsg);
  if s<>'' then
    ShowMessage(s);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2);
end;

// =====================================================
// Node ID
// =====================================================
procedure TForm1.EdNodeDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdNode.Text);
  NodeID := StrToIntDefLW(s, $07FF);
  s := '0x'+IntToHex(NodeID,4);
  if EdNode.Text <> s then begin
    EdNode.Text := s;
    LblCrc.Visible:=false; // another node have another signature
  end;
end;

// =====================================================
// Own ID
// =====================================================
procedure TForm1.EdOwnIdDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdOwnId.Text);
  HBcmd.OwnID := StrToIntDefLW(s, $07FF);
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
  HBcmd.MsgID := StrToIntDefLW(s, 1);
  EdMsgId.Text := '0x'+IntToHex(HBcmd.MsgID,2)
end;

// =====================================================
// New ID
// =====================================================
procedure TForm1.EdNewIDDblClick(Sender : TObject);
var s : string;
begin
  s := num_str_c2pas(EdNewId.Text);
  NewID := StrToIntDefLW(s, $07FF);
  if (NewID = 0) then
     NewID := 1;
  if (NewID > $07FF) then
     NewID := $07FF;
  EdNewID.Text := '0x'+IntToHex(NewID,4)
end;

procedure TForm1.EdNodeChange(Sender: TObject);
begin

end;

end.

