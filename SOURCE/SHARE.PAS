unit share;
{$DEFINE RealBug}
{ $C Fixed Preload Permanent}

{$N+,E-}

interface

uses OWindows;

var DaysUsed: word;

procedure checkregistered;
procedure checktime;
procedure sharedlg(Aparent:pwindowsobject;atime:shortint;what_list:integer;help_id:integer);

implementation

uses Strings, WinTypes, WinProcs, Win31, WinDOS, ShellAPI, WinUnit, ModC, RegDlg, Version, Language;

type psharedlg=^tsharedlg;
     tsharedlg=object(twindow)
       sharelines,timerid:word;
       shwarten:pbmpfont;
       bshok,bshhelp,bsregister:pBMPCaptBtn;
       shs:string;
       cntdwn:longint;
       fontdy:integer;
       sharehandle:thandle;
       oldfocus:hwnd;
       id_lines,help_topic:integer;
       shtimeleft:Pbmplcd;
       constructor init(AParent:Pwindowsobject;what_list:integer;help_id:integer);
       destructor done;                                   virtual;
       procedure setupwindow;                             virtual;
       procedure paint(dc:hdc;var ps:tpaintstruct);       virtual;
       procedure wmQueryNewPalette(var msg:tmessage);     virtual wm_first+wm_QueryNewPalette;
       procedure wmqueryendsession(var msg:tmessage);     virtual wm_first+wm_queryendsession;
       procedure handleshok(var msg:tmessage);            virtual id_first+779;
       procedure handleshhelp(var msg:tmessage);          virtual id_first+780;
       procedure handleregister(var msg:tmessage);        virtual id_first+781;
       procedure wmkeydown(var msg:tmessage);             virtual wm_first+wm_keydown;
       procedure wmsyschar(var msg:tmessage);             virtual wm_first+wm_syschar;
       procedure wmsetfocus(var msg:tmessage);            virtual wm_first+wm_setfocus;
       procedure wmIHaveTheFocus(var msg:tmessage);       virtual wm_first+wm_Ihavethefocus;
       procedure wmclose(var msg:tmessage);               virtual wm_first+wm_close;
     end;

type pfreesharedlg=^tfreesharedlg;
     tfreesharedlg=object(tsharedlg)
       constructor init(AParent:Pwindowsobject;what_list:integer;help_id:integer);
       procedure paint(dc:hdc;var ps:tpaintstruct);virtual;
     end;

type ptimedsharedlg=^ttimedsharedlg;
     ttimedsharedlg=object(tsharedlg)
       constructor init(AParent:Pwindowsobject;atime:byte;what_list:integer;help_id:integer);
       procedure paint(dc:hdc;var ps:tpaintstruct);virtual;
       procedure wmtimer(var msg:tmessage);virtual wm_first+wm_timer;
     end;

PROCEDURE WndProc(hWnd,message,wParam:word;lParam:longint);FAR;
BEGIN
  CASE message OF
    WM_DESTROY  :;
    ELSE         DefWindowProc( hWnd, message, wParam, lParam );
  END;
END;  {}

procedure sharedlg;
var ppp:psharedlg;
    msg:tmsg;
begin
  if atime >= 0
    then ppp:=new(ptimedsharedlg,init(AParent,atime,what_list,help_id))
    else ppp:=new(pfreesharedlg, init(AParent,      what_list,help_id));
  if application^.makewindow(ppp)<>nil then
  WHILE True DO
  IF peekmessage(msg,0,0,$FFFF,PM_Remove) THEN
  BEGIN
    if(msg.hwnd=ppp^.hWindow)and(msg.message=$1234)and(msg.wparam=$4321)AND(msg.lParam=$12344321)then
    begin ppp^.done; exit; END;
    translatemessage(msg);
    dispatchmessage(msg);
  END ELSE Application^.IdleAction;
end;
{***************SHAREDIALOG-OBJEKT*********************************}
constructor tsharedlg.init;
var i:integer;
begin
  inherited init(aparent,'');
  id_lines:=what_list;
  val(getresstring(id_lines),sharelines,i);
  help_topic:=help_id;
  if aparent<>nil then enablewindow(aparent^.hwindow,false);
  with attr do
  begin
    style:=ws_popup or ws_visible or ws_border;
    x:=0;
    y:=0;
    w:=0;
    h:=0;
  end;
  shs:=getresstring(succ(id_lines+sharelines));
  bshhelp:=new(pBMPCaptBtn,Init(@self, 780,407,attr.h-35,'bshelp',false));
  bshok:=new(pBMPCaptBtn,Init(@self, 779,447,attr.h-35,'bsok',false));
  bsregister:=new(pBMPCaptBtn,Init(@self, 781,347,attr.h-35,'bregister',false));
end;

PROCEDURE tsharedlg.WmQueryNewPalette(VAR MSG:tMessage);
BEGIN
  pMyWindow(Application^.mainwindow)^.WmQueryNewPalette(MSG);
END;

procedure tsharedlg.setupwindow;
var
    dc:hdc;
    fx:ttextmetric;
    oldfont:thandle;
    sharefont:tlogfont;
begin
  inherited setupwindow;
  with sharefont do
  begin
    lfwidth:=0;
    lfescapement:=0;
    lforientation:=0;
    lfweight:=400;
    lfitalic:=0;
    lfunderline:=0;
    lfstrikeout:=0;
    IF CurrLanguage<>_GRK THEN
    BEGIN
      lfheight:=20;
      lfcharset:=ANSI_charset;
      lfoutprecision:=out_character_precis;
      lfclipprecision:=clip_default_precis;
      lfquality:=proof_quality;
      lfpitchandfamily:=tmpf_vector or ff_roman;
      strpcopy(@lffacename,'Tms Rmn');
    END ELSE
    BEGIN
      lfheight:=-14;
      lfcharset:=0;
      lfoutprecision:=3;
      lfclipprecision:=2;
      lfquality:=1;
      lfpitchandfamily:=34;
      strpcopy(@lffacename,'Arial Greek');
    END;
    sharehandle:=createfontindirect(sharefont);
  end;
  dc:=getdc(hwindow);
  oldfont:=selectobject(dc,sharehandle);
  gettextmetrics(dc,fx);
  fontdy:=fx.tmheight;
  selectobject(dc,oldfont);
  releasedc(hwindow,dc);
  with attr do
  begin
    w:=500;
    h:=fontdy*(sharelines+1)+49;
    x:=(getsystemmetrics(sm_cxscreen) div 2)-(w div 2);
    y:=(getsystemmetrics(sm_cyscreen) div 2)-(h div 2);
    movewindow(hwindow,x,y,w,h,true);
  end;
  Timerid:=settimer(hwindow,1997,100,nil);
  oldfocus:=bshhelp^.hwindow;
  movewindow(bshhelp^.hwindow,407,attr.h-34,bshhelp^.attr.w,bshhelp^.attr.h,true);
  bshhelp^.enable;
  movewindow(bshok^.hwindow,447,attr.h-34,bshok^.attr.w,bshok^.attr.h,true);
  bshok^.enable;
  movewindow(bsregister^.hwindow,338,attr.h-34,bsregister^.attr.w,bsregister^.attr.h,true);
  if typeof (self) = typeof (tfreesharedlg) then bsregister^.enable;
  movewindow(shwarten^.hwindow,10,attr.h-34,shwarten^.attr.w,shwarten^.attr.h,true);
  shwarten^.setbmptext(shs);
  shwarten^.enable;
  if shtimeleft<>nil then
  begin
    movewindow(shtimeleft^.hwindow,372,attr.h-34,shtimeleft^.attr.w,shtimeleft^.attr.h,true);
    shtimeleft^.enable;
  end;
end;

destructor tsharedlg.done;
begin
  killtimer(hwindow,Timerid);
  deleteobject(sharehandle);
  if parent<>nil then enablewindow(parent^.hwindow,true);
  postappmessage(getcurrenttask,wm_parentnotify,wm_destroy,0);
  inherited done;
end;

procedure tsharedlg.paint;
var oldfont:thandle;
    region:hrgn;
    nc:array [0..91] of char;
    i,mitte:integer;
begin
  {neuen Font waehlen und alten merken}
  oldfont:=selectobject(dc,sharehandle);
  {Text-und Farbattribute setzen}
  settextalign(dc,ta_center or ta_baseline);
  settextcolor(dc,rgb(255,255,0));
  setbkcolor(dc,rgb(0,0,0));
  {region fuer Textausgabe setzen}
  region:=createrectrgn(10,10,attr.w-15,((sharelines+1)*fontdy));
  selectcliprgn(dc,region);
  {Text ausgeben}
  for i:=1 to sharelines do
  begin
    strpcopy(nc,getresstring(id_lines+i));
    textout(dc,(attr.w div 2),10+(i*fontdy),nc,length(getresstring(id_lines+i)));
  end;
  deleteobject(region);
  selectobject(dc,oldfont);
end;

procedure tsharedlg.handleshok(var msg:tmessage);
begin
  if cntdwn=0 then Postmessage(hWindow,$1234,$4321,$12344321)
end;
procedure tsharedlg.handleshhelp(var msg:tmessage);
begin
  helpme(@self,help_topic);
end;
procedure tsharedlg.handleregister(var msg:tmessage);
begin
  Application^.MakeWindow(New(PRegWin,Init(@self,false)));
end;

procedure tsharedlg.wmkeydown(var msg:tmessage);
begin
  msg.result:=0;
  case msg.wparam of
    vk_f1,vk_help,72 {'H'}: handleshhelp(msg);
    else DefWndProc(MSG);
  end;
end;

procedure tsharedlg.wmsyschar;
begin
  if upcase (char(lo(msg.wparam))) = 'R' then handleregister (msg)
  else begin {inherited wmsyschar(msg);} exit; end;
  msg.result:=-1;
end;

procedure tsharedlg.wmclose(var msg:tmessage);
begin
  if cntdwn=0 then Postmessage(hWindow,$1234,$4321,$12344321)
end;
procedure tsharedlg.wmqueryendsession(var msg:tmessage);
begin
  if cntdwn=0 then msg.result:=1 else msg.result:=0;
end;
procedure tsharedlg.wmsetfocus(var msg:tmessage);
begin
  setfocus(oldfocus);
  msg.result:=0;
end;
procedure tsharedlg.wmIHaveTheFocus(var msg:tmessage);
begin
  oldfocus:=msg.wparam;
  msg.result:=0;
end;
{***************SHARE ohne COUNTDOWN**************************}
constructor tfreesharedlg.init;
begin
  inherited init(AParent,what_list,help_id);
  cntdwn:=0;
  shtimeleft:=nil;
  shwarten:=new(pbmpfont, init(@self, 778,10,attr.h-34,20,2,'font_16x20',64));
end;
procedure tfreesharedlg.paint;
begin
  {Rahmen zeichnen}
  Grayframefilled3d(dc,0,0,attr.w-2,attr.h-2,3,rgb(192,192,192),up);
  Grayframefilled3d(dc,7,  attr.h-38,326,29,2,rgb(0,0,0),down);
  Grayframefilled3d(dc,336,attr.h-36,66,25,2,rgb(0,0,0),down);
  Grayframefilled3d(dc,405,attr.h-36,86,25,2,rgb(0,0,0),down);
  Grayframefilled3d(dc,7,7,attr.w-16,((sharelines+1)*fontdy),2,rgb(0,0,0),down);
  inherited paint(dc, ps);
end;

{***************SHARE mit  COUNTDOWN**************************}
constructor ttimedsharedlg.init;
begin
  inherited init(AParent,what_list,help_id);
  cntdwn:=10*atime;
  shtimeleft:=new (pbmplcd, init(@self,777,372,attr.h-34,2,0,'lcd_13x20'));
  shwarten:=new(pbmpfont, init(@self, 778,10,attr.h-34,22,2,'font_16x20',64));
end;

procedure ttimedsharedlg.paint;
begin
  {Rahmen zeichnen}
  Grayframefilled3d(dc,0,0,attr.w-2,attr.h-2,3,rgb(192,192,192),up);
  Grayframefilled3d(dc,7,  attr.h-38,358,29,2,rgb(0,0,0),down);
  Grayframefilled3d(dc,369,attr.h-38,32,29,2,rgb(0,0,0),down);
  Grayframefilled3d(dc,405,attr.h-36,86,25,2,rgb(0,0,0),down);
  Grayframefilled3d(dc,7,7,attr.w-16,((sharelines+1)*fontdy),2,rgb(0,0,0),down);
  inherited paint(dc, ps);
end;
procedure ttimedsharedlg.wmtimer;
begin
  if getactivewindow=hwindow then if cntdwn>0 then dec(cntdwn);
  shtimeleft^.setbmpzahl((cntdwn+9) div 10);
end;
{*************************ENDE SHARE-OBJEKT*******************}

{************************** SHAREWARE-CHECK auf 30 TAGE by Jensi ******************************}
procedure checktime;
  const
    AppName: array [0..13] of char =
     (char (ord ('C') xor 203),
      char (ord ('o') xor 203),
      char (ord ('m') xor 203),
      char (ord ('p') xor 203),
      char (ord ('a') xor 203),
      char (ord ('t') xor 203),
      char (ord ('i') xor 203),
      char (ord ('b') xor 203),
      char (ord ('i') xor 203),
      char (ord ('l') xor 203),
      char (ord ('i') xor 203),
      char (ord ('t') xor 203),
      char (ord ('y') xor 203),
      char (ord (#00) xor 203));
    KeyName: array [0..8] of char =
     (char (ord ('E') xor 203),
      char (ord ('U') xor 203),
      char (ord ('P') xor 203),
      char (ord ('H') xor 203),
      char (ord ('O') xor 203),
      char (ord ('R') xor 203),
      char (ord ('I') xor 203),
      char (ord ('A') xor 203),
      char (ord (#00) xor 203));
    SubKey: array [0..25] of char =
     (char (ord ('M') xor 194),
      char (ord ('o') xor 194),
      char (ord ('d') xor 194),
      char (ord ('u') xor 194),
      char (ord ('l') xor 194),
      char (ord ('e') xor 194),
      char (ord ('P') xor 194),
      char (ord ('l') xor 194),
      char (ord ('a') xor 194),
      char (ord ('y') xor 194),
      char (ord ('e') xor 194),
      char (ord ('r') xor 194),
      char (ord ('\') xor 194),
      char (ord ('d') xor 194),
      char (ord ('a') xor 194),
      char (ord ('t') xor 194),
      char (ord ('a') xor 194),
      char (ord ('\') xor 194),
      char (ord ('p') xor 194),
      char (ord ('r') xor 194),
      char (ord ('i') xor 194),
      char (ord ('v') xor 194),
      char (ord ('a') xor 194),
      char (ord ('t') xor 194),
      char (ord ('e') xor 194),
      char (ord (#00) xor 194));
    Months: array [1..12] of word = (0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334);
    flipped_1: boolean = false;
    flipped_2: boolean = false;

  var
    FileName: array [0..20] of char;
    ArgList: array [1..2] of word;

  function hexconvert (ch: char): byte; near; assembler;
  asm
    mov  al, &ch
    cmp  al, '9'
    jbe  @1
    sub  al, 'a'-'0'-10
  @1:
    sub  al, '0'
  end;

  function ValidateEntry (const ValueString: PChar; var VersionInstalled, DateInstalled: word; Crypt: word): boolean;
  var
    i: byte;
    IsValid: boolean;
  begin
    if StrComp (ValueString, '') = 0 then begin
      { no entry present }
      ValidateEntry := false;
      exit;
    end;
    { entry is present }
    IsValid := false;
    if (ValueString [10] = #0) then
    if (ValueString [0] = '0') then
    if (ValueString [1] = 'x') then begin
      IsValid := true;
      for i := 2 to 9 do IsValid := IsValid and (ValueString [i] in ['0'..'9','a'..'f']);
    end;
    if not IsValid then begin
      ValidateEntry := false;
      exit;
    end;
    { entry is valid }
    DateInstalled :=
     (hexconvert (ValueString [2]) shl 12 +
      hexconvert (ValueString [3]) shl  8 +
      hexconvert (ValueString [4]) shl  4 +
      hexconvert (ValueString [5])) xor Crypt;
    VersionInstalled :=
     (hexconvert (ValueString [6]) shl 12 +
      hexconvert (ValueString [7]) shl  8 +
      hexconvert (ValueString [8]) shl  4 +
      hexconvert (ValueString [9])) xor Crypt;
  end;

  function GetValidWinIniEntry (var Version_1, Date_1: word): boolean;
  var i: byte;
  begin { retrieve existing version/date info from win.ini and validate }
    if not flipped_1 then begin
      for i := 0 to high (AppName) do AppName [i] := char (ord (AppName [i]) xor 203);
      for i := 0 to high (KeyName) do KeyName [i] := char (ord (KeyName [i]) xor 203);
      flipped_1 := true;
    end;
    if GetProfileString (AppName, KeyName, '', FileName, SizeOf (FileName)) = 0 then begin
      GetValidWinIniEntry := false;
      exit;
    end;
    GetValidWinIniEntry := ValidateEntry (FileName, Version_1, Date_1, $3425);
  end;

  procedure WriteWinIniEntry (const Version, Date: word);
  begin { encrypt version/date and write in win.ini file }
    ArgList [1] := Version xor $3425;
    ArgList [2] := Date    xor $3425;
    wvsprintf (FileName, '%#.8lx', ArgList);
    if not WriteProfileString (AppName, KeyName, FileName) then begin
      { user has win.ini write protected }
    end;
  end;

  function GetValidRegEntry (var Version_2, Date_2: word): boolean;
  var
    i: byte;
    cb: longint;
  begin { retrieve existing version/date info from reg.dat and validate }
    if not flipped_2 then begin
      for i := 0 to high (SubKey) do SubKey [i] := char (ord (SubKey [i]) xor 194);
      flipped_2 := true;
    end;
    cb := sizeof (FileName);
    if RegQueryValue (hKey_Classes_Root, SubKey, FileName, cb) <> 0 then begin
      GetValidRegEntry := false;
      exit;
    end;
    GetValidRegEntry := ValidateEntry (FileName, Version_2, Date_2, $6153);
  end;

  procedure WriteRegEntry (const Version, Date: word);
  begin { encrypt version/date and write in reg.dat file }
    ArgList [1] := Version xor $6153;
    ArgList [2] := Date    xor $6153;
    wvsprintf (FileName, '%#.8lx', ArgList);
    if RegSetValue (hKey_Classes_Root, SubKey, reg_sz, FileName, StrLen (FileName)) <> 0 then begin
      { user has reg.dat write protected }
    end;
  end;

  var
    Version, Version_1, Version_2: word;
    DT: TDateTime;
    dummy, DateToday, Date_1, Date_2: word;
    GotWinIni, GotRegDat: boolean;

begin
  DaysUsed := 0;
  { get version info }
  Version := ProductVersion;
  { get date info }
  with DT do begin
    GetDate (Year, Month, Day, dummy);
    DateToday := (Year - 1980) * 365 + Months [Month] + Day;
  end;
  { get encrypted info }
  GotWinIni := GetValidWinIniEntry (Version_1, Date_1);
  GotRegDat := GetValidRegEntry    (Version_2, Date_2);
  { update if necessary }
  if not GotWinIni and not GotRegDat then
  begin
    WriteWinIniEntry (Version, DateToday);
    WriteRegEntry (Version, DateToday);
    Version_1 := Version;
    Date_1    := DateToday;
  end else
  if not GotWinIni and     GotRegDat then
  begin
    WriteWinIniEntry (Version_2, Date_2);
    Version_1 := Version_2;
    Date_1    := Date_2;
  end else
  if     GotWinIni and not GotRegDat then
  begin
    WriteRegEntry (Version_1, Date_1);
  end else
  begin { got both }
    if Version_1 < Version_2 then Version_1 := Version_2;
    if Date_1    < Date_2    then Date_1    := Date_2;
  end;
  { check if we can still run it }
  if Version <= Version_1
    then begin { old or current version }
      DaysUsed := DateToday - Date_1;
    end
    else begin { new version }
      { rewrite entry }
      WriteWinIniEntry (Version, DateToday);
      WriteRegEntry    (Version, DateToday);
    end;
end;

procedure checkregistered;
var
  cb: longint; Key: HKey; i: byte; ts: string [50];
  UserName, CompName: array[0..50] of char;
  RegInfo, RegCode: array[0..50] of char;
  accu: extended;
begin
  if RegOpenKey (hKey_Classes_Root, 'ModulePlayer\data\registration', Key) = error_success then
  begin
    cb := SizeOf (UserName); if RegQueryValue (Key, 'user',    UserName, cb) <> error_success then UserName[0] := #0;
    cb := SizeOf (CompName); if RegQueryValue (Key, 'company', CompName, cb) <> error_success then CompName[0] := #0;
    cb := SizeOf (RegInfo);  if RegQueryValue (Key, 'info',    RegInfo, cb)  <> error_success then RegInfo[0] := #0;
    cb := SizeOf (RegCode);  if RegQueryValue (Key, 'code',    RegCode, cb)  <> error_success then RegCode[0] := #0;
    RegCloseKey (Key);
  end;
  IF ProductVersion>=230
  THEN RandSeed := (ProductVersion div 10-1) * $74E691BA
  ELSE RandSeed := ProductVersion div 10 * $74E691BA;
  accu := random;
  ts := StrPas (UserName); IF Length(ts)<>0 THEN for i := 1 to length (ts) do accu := accu * ord (ts[i])/256 * random;
  ts := StrPas (CompName); IF Length(ts)<>0 THEN for i := 1 to length (ts) do accu := accu * ord (ts[i])/256 * random;
  ts := StrPas (RegInfo);  IF Length(ts)<>0 THEN for i := 1 to length (ts) do accu := accu * ord (ts[i])/256 * random;
  str (accu:28, ts);
  dec (ts[0], 6);
  while not (ts[1] in ['0'..'9']) do delete (ts, 1, 1);
  delete (ts, 2, 1);
  {$IFDEF RealBug}
  delete (ts, Length(ts)-2, 3);
  {$ENDIF}
  StrPCopy (RegInfo, ts);
  ts := StrPas (RegCode);
  {$IFDEF RealBug}
  delete (ts, Length(ts)-2, 3);
  {$ENDIF}
  for i := 1 to length (ts) do
    while (i <= length (ts)) and not (ts[i] in ['0'..'9']) do delete (ts, i, 1);
  StrPCopy (RegCode, ts);
  Registered := StrComp (RegInfo, RegCode) = 0;
  if Registered then begin
    modc.username := StrPas (UserName);
    modc.compname := StrPas (CompName);
  end else modc.username := 'unregistered demo';
  ts:=shortver+'['+modc.username+']';
  StrPCopy (longver, ts);
end;

end.