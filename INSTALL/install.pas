PROGRAM modinstall;
{ $C Fixed Preload Permanent}
{$S 60000}
{$M 16384,8192}
{$D --- MOD4WIN Installation program (c)1993-96 SWE Bruns&Zänker and JSInc. ---}

{$IFDEF HyperWare} {$R hyp_inst.res} {$ELSE} {$R install.res} {$ENDIF}

USES winprocs,wintypes,strings,owindows,windos,lzexpand,winunit,win31,modc,InstLang,ShellAPI
     {$ifndef share}
     ,Register
     {$endif};

CONST  OptMem=1200000;
       ForcedMem=1400000;
       ReqMem:LongInt=OptMem+ForcedMem;
       IDLang:Byte=0;

VAR CAPTION:STRING[100];
    ForcedFiles: word;

CONST id_GrpSw=100;
      id_AssSw=101;
      lr_sw='LRSWITCH';

TYPE tlogorec=RECORD
       bmp:hbitmap;
       w,h:integer;
     END;

CONST  xLen=16384;
VAR    x:ARRAY[0..16384] OF char;

TYPE
  ShortStr = STRING[60];
  FileStr  = STRING[fsPathName];
  PathStr  = STRING[fsPathName];

TYPE tinstrec =RECORD
       chkfile1  :STRING[50];
       inifile   :ARRAY[0..100] OF char;
       defdir    :ARRAY[0..100] OF char;
       grpname   :STRING[20];
       appname   :ARRAY[0..50] OF char;
       optnames  :ARRAY[1..10,0..30] OF char;
       instopt   :ARRAY[1..10]OF boolean;
       numreqfiles:integer;
       numoptions:integer;
       aktoption:integer;
       aktfileinoption:word;
       installedfiles:word;
     END;


TYPE pcopy=^tcopy;
     tcopy=object(tmywindow)
      Count:word;
      atom:tatom;
      topic:thandle;
      woher,wohin,titelher,titelhin:pmystatic;
      prozent:word;
      pcancel:pmsgbutton;
      processing:boolean;
      CONSTRUCTOR Init(Aparent:Pwindowsobject);
      PROCEDURE   SetupWindow;                         virtual;
      DESTRUCTOR  Done;                                virtual;
      PROCEDURE   paintprcnt(adc:hdc);                 virtual;
      PROCEDURE   wmEraseBkGnd(VAR msg:tmessage);      virtual wm_first+wm_erasebkgnd;
      PROCEDURE   Break(VAR msg:tmessage);             virtual id_first+2;
      FUNCTION    timerproc(VAR msg:tmessage):BOOLEAN; virtual wm_first+wm_timer;
      PROCEDURE   dde(VAR msg:tmessage);               virtual wm_first+wm_dde_ack;
      PROCEDURE   WMClose(VAR msg:tmessage);           virtual wm_first+wm_close;
      FUNCTION    getentrie(rubrik:ShortStr; optnum,num:integer; VAR src:FileStr; VAR dest:PathStr; VAR newcnt:word):boolean;
      {$ifndef share}
      PROCEDURE   runcheck;
      {$endif}
    END;


TYPE pinstall=^tinstall;
     tinstall=object(Tmywindow)
       GrpSw, AssSw  :plrswitch;
       optsw         :ARRAY[1..10] OF
                      RECORD
                        sw:plrswitch;
                        swstatic:pmystatic;
                        optstatic:pmystatic;
                      END;
       GrpStatic,GrpSel,
       AssStatic,AssSel,
       pfad          :pmystatic;
       bicancel,biok :pbmpbutton;
       diredit       :pmyedit;
       logorec       :tlogorec;
        {$ifndef share}
        regbox       :pregisterwindow;
        {$ENDIF}
       CONSTRUCTOR Init(aparent:pwindowsobject);
       DESTRUCTOR Done; virtual;
       PROCEDURE SetupWindow; virtual;
       PROCEDURE Paint(dc:hdc; VAR ps:tpaintstruct); virtual;
       PROCEDURE wmerasebkgnd(VAR msg:tmessage); virtual wm_first+wm_erasebkgnd;
       PROCEDURE Ok; virtual;
       PROCEDURE No; virtual;
       PROCEDURE GrpSwitch; virtual;
       PROCEDURE AssSwitch; virtual;
       PROCEDURE OptSwitch(num:word); virtual;
       PROCEDURE wmctlcolor(VAR msg:tmessage); virtual wm_first + wm_ctlcolor;
       PROCEDURE wmkeydown(VAR msg:tmessage); virtual wm_first+wm_keydown;
       PROCEDURE WMCommand(VAR msg:tmessage); virtual wm_first+wm_command;
    END;
VAR Size,checksum:longint;


{**************TINSTALL-OBJEKT***********************************}
VAR windir,currdir:ARRAY[0..fsPathName] OF char;
    destdir:ARRAY[0..fsDirectory] OF char;
    makegroup, makeass: boolean;
    {$ifndef share}
      Company,user:STRING;
    {$endif}
    instrec:tinstrec;
    srec:tsearchrec;
FUNCTION getresstring1(ID:Word):STRING;
VAR S:STRING;
BEGIN
  S:=getresstring(ID);
  WHILE S[Length(S)] IN [':',' '] DO Dec(Byte(S[0]));
  getresstring1:=s;
END;

CONSTRUCTOR tinstall.Init(aparent:pwindowsobject);
VAR B: TBitMap;
    s:STRING[40];
    namestr:STRING[6];
    i,j:Longint;
BEGIN
 LoadLanguage;
 caption:=getresstring(300)+#0;
 WITH instrec DO
 BEGIN
   FillChar(optnames,SizeOf(optnames),0);
   FillChar(instrec,SizeOf(instrec),0);
   strcopy(inifile,inidir); strcat(inifile,'install.inf');
   if not FilePresent (inifile) then begin
     MessageBox(0,'No information file (INSTALL.INF) found.','Fatal Error',mb_ok OR mb_iconstop);
     Halt(0);
   end;
   I:=SizeOf(defdir)-1;
   IF (RegQueryValue (hKey_Classes_Root, 'ModulePlayer\shell\open\command', @defDir , i)<>ERROR_SUCCESS)OR(I<3)
   THEN GetPrivateProfileString('INSTALL','DEF_DIR','',@defdir,100,INIFILE)
   ELSE BEGIN
     IF StrRScan(defDir,'\')<>NIL THEN StrRScan(defDir,'\')^:=#0;
   END;
   GetPrivateProfileString('INSTALL','APPNAME','',@appname,50,INIFILE);
   grpname[0]:=char(byte(GetPrivateProfileString('INSTALL','GRPNAME','',@grpname[1],20,INIFILE)));
   numreqfiles:=0; i:=1;
   WHILE i>0 DO
   BEGIN
     Str(numreqfiles+1,s); s:=s+#0;
     i:=GetPrivateProfileString('REQFILES',@s[1],'',@x,30,INIFILE);
     IF i>0 THEN
     BEGIN
       inc(numreqfiles);
       IF numreqfiles=1 THEN
       BEGIN
         chkfile1:=strpas(x);
         chkfile1:=Copy(chkfile1,Pos(',',chkfile1)+1,Length(chkfile1)-Pos(',',chkfile1));
       END;
     END;
   END;
   numoptions:=0; i:=1;
   namestr := 'Name' + chr (ord ('1') + ord (WinLanguage)) + #0;
   IDLang := ord (WinLanguage);
   WHILE i>0 DO
   BEGIN
     Str(numoptions+1,s);
     s:='OPTFILES'+s+#0;
     i:=GetPrivateProfileString(@s[1],@namestr[1],'',@optnames[numoptions+1],30,INIFILE);
     IF i>0 THEN
     BEGIN
       S:=StrPas(optnames[numoptions+1]);
       WHILE (Length(S)>0)AND(S[Length(S)] IN [':',' ']) DO Dec(Byte(S[0]));
       IF Length(S)>0 THEN
       BEGIN
         inc(numoptions);
         StrPCopy(optnames[numoptions],S);
       END;
     END;
   END;
 END;
 INHERITED Init(aparent,@caption[1],ws_minimizebox OR ws_sysmenu,notopmost);
 WITH logorec DO
 BEGIN
   bmp:=LoadBitmap(HInstance,'logo');
   GetObject(BMP,SizeOf(B), @B);
   W := B.bmWidth;
   H := B.bmHeight;
 END;
 WITH Attr DO
 BEGIN
   style:=ws_popup OR ws_visible OR ws_border;
   w:=400;
   h:=logorec.h+85+3*(fh+10)+capdy+(2*GetSystemMetrics(sm_cyborder))+(instrec.numoptions*(fh+10));
   IF instrec.numoptions>0 THEN h:=h+fh+8;
   x:=(GetSystemMetrics(sm_cxscreen) DIV 2)-(w DIV 2);
   y:=(GetSystemMetrics(sm_cyscreen) DIV 2)-(h DIV 2);
 END;
 biok:=New(pmsgbutton,    Init(@self, 1,40        ,Attr.h-capdy-18,'mb_ok',false));
 bicancel:=New(pmsgbutton,Init(@self, 2,Attr.w-116,Attr.h-capdy-18,'mb_cancel',false));
 j:=0;
 IF Length(getresstring1(301))>j THEN j:=Length(getresstring1(301));
 IF Length(getresstring1(114))>j THEN j:=Length(getresstring1(114));
 IF Length(getresstring1(117))>j THEN j:=Length(getresstring1(117));
 IF INSTREC.numoptions>0 THEN WITH instrec DO
 FOR i:=1 TO numoptions DO
 BEGIN
   IF StrLen(optnames[i])>j THEN j:=StrLen(optnames[i]);
 END;
 s:=getresstring1(301)+#0+#0;
 pfad:=New(pmystatic,Init(@self,300,@s[1],20,capdy+126,j*fw,fh,j));
 diredit:=New(Pmyedit,Init(@self,100,'',24+(j+1)*fw,pfad^.Attr.y,Attr.w-44-(j+1)*fw,fh,fsDirectory,true));
 {ProgrammGruppe}
 strpcopy(@x,getresstring1(114));
 GrpStatic:=New(pmystatic,Init(@self,0,@x,20,pfad^.Attr.y+20,pfad^.Attr.w,fh,strlen(x)));
 GrpSw    :=New(plrswitch,Init(@self,id_GrpSw,diredit^.Attr.x+diredit^.Attr.w-24,diredit^.Attr.y+19,2,1,lr_sw));
 GrpSel   :=New(pmystatic,Init(@self,0,'',diredit^.Attr.x,diredit^.Attr.y+20,
                diredit^.Attr.w-GrpSw^.Attr.w-8,fh,40));
 { Associations by Jensi }
 strpcopy(@x,getresstring1(117));
 AssStatic:=New(pmystatic,Init(@self,0,@x,20,pfad^.Attr.y+40,pfad^.Attr.w,fh,strlen(x)));
 AssSw    :=New(plrswitch,Init(@self,id_AssSw,diredit^.Attr.x+diredit^.Attr.w-24,diredit^.Attr.y+39,2,1,lr_sw));
 AssSel   :=New(pmystatic,Init(@self,0,'',diredit^.Attr.x,diredit^.Attr.y+40,
                diredit^.Attr.w-AssSw^.Attr.w-8,fh,40));
 { Option Switches }
 IF INSTREC.numoptions>0 THEN WITH instrec DO
 FOR i:=1 TO numoptions DO
 BEGIN
   optsw[i].optstatic:=New(pmystatic,Init(@self,0,@optnames[i],20,fh+8+AssStatic^.Attr.y+(i*20),GrpStatic^.Attr.w,fh,30));
   optsw[i].sw       :=New(plrswitch,Init(@self,id_AssSw+i,diredit^.Attr.x+diredit^.Attr.w-24,
                           fh+8+AssSw^.Attr.y+(20*i),2,1,lr_sw));
   optsw[i].swstatic :=New(pmystatic,Init(@self,0,'',GrpSel^.Attr.x,fh+8+AssSel^.Attr.y+(i*20),
                           GrpSel^.Attr.w,fh,40));
 END;
END;

PROCEDURE tinstall.SetupWindow;
VAR i:integer;
BEGIN
  INHERITED SetupWindow;
  diredit^.SetText(instrec.defdir);
  pfad^.setassocwindow(diredit^.HWindow,0);
  GrpSw^.setpos(1); GrpSw^.enable;
  GrpStatic^.setassocwindow(GrpSw^.HWindow,0);
  AssSw^.setpos(2); AssSw^.enable;
  AssStatic^.setassocwindow(AssSw^.HWindow,0);
  biok^.enable; bicancel^.enable;
  IF instrec.numoptions>0 THEN
  FOR i:=1 TO instrec.numoptions DO WITH optsw[i] DO
  BEGIN
    optstatic^.setassocwindow(sw^.HWindow,0);
    sw^.enable;
    sw^.setpos(2);
    sw^.setpos(1);
  END;
  {$ifndef share}
    ourmessagebox(@self,getresstring(101),getresstring(100),mb_ok OR mb_iconasterisk);
  {$endif}
  oldfocus:=diredit^.HWindow;
END;

DESTRUCTOR tinstall.Done;
BEGIN
 DeleteObject(logorec.bmp);
 INHERITED Done;
 pBuzApplication(Application)^.MainWindow:=NIL;
END;

PROCEDURE tinstall.Paint;
BEGIN
  ps.ferase:=false;
END;

PROCEDURE tinstall.wmerasebkgnd;
VAR memdc:hdc;
    oldfont:thandle;
    i:integer;
    s:STRING[40];
BEGIN
  paintcaption(msg.wparam);
  {Rahmen zeichnen}
  {haupt}
  framefilled3d(msg.wparam,0,capdy,Attr.w-2,Attr.h-capdy-2,2,RGB(192,192,192),up);
  {um EINgaben-Feld}
  frame3d(msg.wparam,7,capdy+7,Attr.w-16,Attr.h-capdy-52,1,down);
  {um-Logo}
  frame3d(msg.wparam,(Attr.w DIV 2)-(logorec.w DIV 2)-3,capdy+12,logorec.w+6,logorec.h+12+fh,1,up);
  memdc:=CreateCompatibleDC(msg.wparam);
  SelectObject(memdc,logorec.bmp);
  BitBlt(msg.wparam,(Attr.w DIV 2)-(logorec.w DIV 2),capdy+15,logorec.w,logorec.h,memdc,0,0,srccopy);
  DeleteDC(memdc);
  oldfont:=SelectObject(msg.wparam,ansihandle);
  SetTextAlign(msg.wparam,ta_center OR ta_top OR ta_noupdatecp);
  SetTextColor(msg.wparam,rgb(0,0,0)); SetBkColor(msg.wparam,rgb(192,192,192));
  ExtTextOut(msg.wparam,Attr.w DIV 2,capdy+18+logorec.h,0,NIL,instrec.appname,strlen(instrec.appname),NIL);
  IF instrec.numoptions>0 THEN
  BEGIN
    s:=getresstring(102)+#0;
    ExtTextOut(msg.wparam,Attr.w DIV 2,AssSw^.Attr.y+22,0,NIL,@s[1],Length(s)-1,NIL);
  END;
  SelectObject(msg.wparam,oldfont);
  frame3d(msg.wparam,(Attr.w DIV 2)-((strlen(instrec.appname)*fw)DIV 2)-3,capdy+16+logorec.h,
          (strlen(instrec.appname)*fw)+6,fh+4,1,down);
  {um PATH-Static}
  WITH pfad^ DO frame3d(msg.wparam,Attr.x-2,Attr.y-2,Attr.w+4,Attr.h+4,1,down);
  {um PATH - EDIT}
  WITH diredit^ DO frame3d(msg.wparam,Attr.x-2,Attr.y-2,Attr.w+4,Attr.h+4,1,down);
  {um PRG-Gruppe-anlegen}
  WITH GrpStatic^ DO frame3d(msg.wparam,Attr.x-2,Attr.y-2,Attr.w+4,Attr.h+4,1,down);
  WITH GrpSel^    DO frame3d(msg.wparam,Attr.x-2,Attr.y-2,Attr.w+4,Attr.h+4,1,down);
  WITH GrpSw^     DO frame3d(msg.wparam,Attr.x-2,Attr.y-1,Attr.w+4,Attr.h+2,1,down);
  {um Assoziations}
  WITH AssStatic^ DO frame3d(msg.wparam,Attr.x-2,Attr.y-2,Attr.w+4,Attr.h+4,1,down);
  WITH AssSel^    DO frame3d(msg.wparam,Attr.x-2,Attr.y-2,Attr.w+4,Attr.h+4,1,down);
  WITH AssSw^     DO frame3d(msg.wparam,Attr.x-2,Attr.y-1,Attr.w+4,Attr.h+2,1,down);
  {um OPTIONS}
  IF INSTREC.numoptions>0 THEN
  FOR i:=1 TO instrec.numoptions DO WITH optsw[i] DO
  BEGIN
    WITH optstatic^ DO frame3d(msg.wparam,Attr.x-2,Attr.y-2,Attr.w+4,Attr.h+4,1,down);
    WITH sw^        DO frame3d(msg.wparam,Attr.x-2,Attr.y-1,Attr.w+4,Attr.h+2,1,down);
    WITH swstatic^  DO frame3d(msg.wparam,Attr.x-2,Attr.y-2,Attr.w+4,Attr.h+4,1,down);
 END;
 msg.result:=-1;
 INHERITED WmEraseBkgnd(MSG);
END;

PROCEDURE tinstall.GrpSwitch;
BEGIN
 IF GrpSw^.GetPos=1 THEN GrpSel^.setstringtext(getresstring(303))
                    ELSE GrpSel^.setstringtext(getresstring(302));
END;

PROCEDURE tinstall.AssSwitch;
BEGIN
 IF AssSw^.GetPos=1 THEN AssSel^.setstringtext(getresstring(303))
                    ELSE AssSel^.setstringtext(getresstring(302));
END;

PROCEDURE tinstall.OptSwitch;
BEGIN
 instrec.instopt[num]:=optsw[num].sw^.GetPos=2;
 IF optsw[num].sw^.GetPos=1 THEN optsw[num].swstatic^.setstringtext(getresstring(305))
                            ELSE optsw[num].swstatic^.setstringtext(getresstring(304));
 ReqMem:=LongInt(Byte(instrec.instopt[num]))*OptMem+ForcedMem;
END;

PROCEDURE tinstall.no; BEGIN PostMessage(HWindow,wm_close,0,0); END;

PROCEDURE tinstall.WMCommand(VAR msg:tmessage);
BEGIN
  CASE msg.wparam OF
    id_ok:Ok;
    id_cancel:no;
    id_GrpSw:GrpSwitch;
    id_AssSw:AssSwitch;
    id_AssSw+1..id_AssSw+10:IF instrec.numoptions>0 THEN optswitch(msg.wparam-id_AssSw);
    ELSE INHERITED WMCommand(msg);
  END
END;

PROCEDURE tinstall.wmkeydown;
BEGIN
  msg.result:=1;
  CASE msg.wparam OF
    vk_return,vk_execute:Ok;
    vk_space            :SendMessage(oldfocus,wm_char,vk_return,0);
    vk_escape           :no;
    ELSE BEGIN DefWndProc(msg); msg.result:=1; END;
   END;
END;

PROCEDURE tinstall.wmctlcolor(VAR msg:tmessage);
BEGIN
  IF (msg.lparamhi=CTLCOLOR_EDIT)THEN msg.lparamhi:=CTLCOLOR_STATIC;
  INHERITED wmctlcolor(msg);
END;

PROCEDURE tinstall.Ok;
VAR s,s2:STRING;
    pstart:pcopy;
    test:integer;

  FUNCTION isvaliddosname(Name:STRING):boolean;
  VAR i,lastslash,lastpoint,c:byte;
  BEGIN
    isvaliddosname:=false;
    lastslash:=3;
    lastpoint:=0;
    IF (Name[2]<>':') OR ((UpCase(Name[1])<'A')OR(UpCase(Name[1])>'Z')) OR
       ((Name[0]>#3)AND(Name[3]<>'\')) THEN Exit;
    FOR i:=4 TO Length(Name) DO
    BEGIN
     c:=Ord(Name[i]);
     IF (c<33)OR(c=34)OR(c=42)OR(c=43)OR(c=44)OR(c=47)OR((c<64)AND(c>58))
        OR(c=91)OR(c=93)OR(c=124) THEN Exit ELSE
     IF Name[i]='\' THEN
     BEGIN
       IF (i<3) OR
          ((lastslash=0)AND(i<>3))
          OR (i-lastslash>13)
          OR ((i-lastpoint>4) AND (lastpoint>lastslash))
          OR((i-lastslash>9)  AND(lastpoint<lastslash)) THEN Exit;
       lastslash:=i;
     END ELSE
     IF Name[i]=':' THEN
     BEGIN
       IF (i<>2)THEN Exit;
     END ELSE
     IF Name[i]='.' THEN
     BEGIN
       IF  (lastslash<lastpoint)
         OR(i-lastslash<2)
         OR(lastslash=0)
         OR(i-lastslash>9) THEN Exit;
       lastpoint:=i;
     END;
     IF ((i-lastslash>9)AND(lastpoint<lastslash))
         OR((lastpoint>lastslash)AND(i-lastpoint>3)) THEN Exit;
    END;
    isvaliddosname:=true;
  END;

BEGIN
  diredit^.GetText(@destdir,fsDirectory);
  strcopy(@destdir,AnsiUpper(@destdir));
  s:=strpas(@destdir);
  IF NOT isvaliddosname(s) THEN BEGIN
    ourmessagebox(@self,caption,getresstring(107),mb_ok OR mb_iconexclamation);
    SetFocus(diredit^.HWindow);
    Exit;
  END;
  IF GetDriveType(Ord(UpCase(s[1]))-65) = 0 THEN BEGIN
    ourmessagebox(@self,caption,getresstring(116),mb_ok OR mb_iconexclamation);
    SetFocus(diredit^.HWindow);
    Exit;
  END;
  IF (DiskFree(Ord(UpCase(s[1]))-64)<ReqMem) THEN BEGIN
    ourmessagebox(@self,caption,getresstring(103),mb_ok OR mb_iconexclamation);
    SetFocus(diredit^.HWindow);
    Exit;
  END;
  IF s[Length(s)]='\' THEN Delete(s,Length(s),1);
  {$i-}
  ChDir(s);
  IF IOResult<>0 THEN
  CASE ourmessagebox(@self,caption,getresstring(104),mb_yesnocancel OR mb_iconquestion) OF
    id_yes:
    BEGIN
    {$i-}
      s2:='';
      s:=s+'\';
      WHILE (Pos('\',s)<>0) AND (inoutres=0) DO
      BEGIN
        s2:=s2+Copy(s,1,Pos('\',s)-1);
        ChDir(s2);
        IF IOResult<>0 THEN MkDir(s2);
        Delete(s,1,Pos('\',s));
        s2:=s2+'\';
      END;
      IF IOResult<>0 THEN BEGIN
        ourmessagebox(@self,caption,getresstring(105),mb_ok OR mb_iconexclamation);
        SetFocus(diredit^.HWindow);
        Exit;
      END;
    END;
    id_no: Exit;
    id_cancel: no;
  END;
  MakeGroup := GrpSw^.GetPos=2;
  MakeAss   := AssSw^.GetPos=2;
  {registertest}
  {$ifndef share}
  IF user='' THEN
  BEGIN
    regbox:=New(pregisterwindow,Init(@self,getresstring(106),@user,@company));
    Application^.MakeWindow(regbox);
    Exit;
  END;
  {$endif};
  pstart:=New(pcopy,Init(@self));
  Application^.MakeWindow(pstart);
  EnableWindow(HWindow,false);
END;

{********************Entpackfenster*******************************}
CONSTRUCTOR tcopy.Init(Aparent:Pwindowsobject);
VAR s1   :ARRAY[0..fsPathName] OF Char;
    s2,s3:ARRAY[0..fsDirectory]OF Char;
    sstr,sstr2:^PathStr;
CONST ILang:ARRAY[0..8]OF char='Language'+#0;
BEGIN
  { add '\' to destination directory }
  strcopy(s2,destdir);
  IF s2[strlen(s2)-1]<>'\' THEN IF StrLen (s2) < fsDirectory THEN strcat(s2,'\') ELSE s2[fsDirectory-1] := '\';
  strcopy(destdir,s2);
  { create ini file }
  StrCopy (s1, s2);
  GetPrivateProfileString ('INSTALL', 'GRPNAME', '', s3, SizeOf (s3)-1, instrec.inifile);
  strcat (s1, s3);
  strcat (s1,'.ini');
  Str(IDLang,s3);
  WritePrivateProfileString(ILang,ILang,s3,s1);
  IF instrec.instopt[1] THEN
  BEGIN
    GetPrivateProfileString ('OPTFILES1', 'PATH', '', s3, SizeOf (s3)-1, instrec.INIFILE);
    StrLCat(s2,s3,SizeOf(s2)-1-1); { leave space for trailing backslash }
    WritePrivateProfileString ('State', 'LastDir', s2, s1);
  END;
  strpcopy(s3,getresstring(210));
  INHERITED Init(aparent,s3,ws_sysmenu,notopmost);
  WITH Attr DO
  BEGIN
    Attr.w:=22+(60*fw);
    Attr.h:=100+4*fh;
    Attr.x:=(GetSystemMetrics(sm_cxscreen) DIV 2)-(Attr.w DIV 2);
    Attr.y:=(GetSystemMetrics(sm_cyscreen) DIV 2)-(Attr.h DIV 2)+50;
    Attr.style:=ws_popup OR ws_visible OR ws_border ;
  END;
  GetMem(sstr,SizeOf (sstr^)); GetMem(sstr2,SizeOf (sstr2^));
  doserror:=18;
  forcedfiles:=0; Count:=1; instrec.aktfileinoption:=1;
  IF instrec.numoptions>0 THEN
  FOR instrec.aktoption:= 1 TO instrec.numoptions DO IF instrec.instopt[instrec.aktoption] THEN
  BEGIN
    WHILE getentrie('OPTFILES',instrec.aktoption,instrec.aktfileinoption,sstr^,sstr2^,
                    instrec.aktfileinoption) DO inc(forcedfiles);
    instrec.aktfileinoption:=1;
  END;
  FreeMem(sstr,SizeOf (sstr^)); FreeMem(sstr2,SizeOf (sstr2^));
  forcedfiles:=instrec.numreqfiles+forcedfiles;
  instrec.aktoption:=1;
  strpcopy(@x,getresstring(108));
  titelher:=New(pmystatic,Init(@self,0,@x,10,capdy+10,strlen(@x)*fw,fh,strlen(@x)));
  strpcopy(@x,getresstring(109));
  titelhin:=New(pmystatic,Init(@self,0,@x,10,capdy+30+2*fh,strlen(@x)*fw,fh,strlen(@x)));
  woher:=New(pmystatic,   Init(@self,0,''   ,10,capdy+20+fh,60*fw,fh,60));
  wohin:=New(pmystatic,   Init(@self,0,''   ,10,capdy+40+3*fh,60*fw,fh,60));
  pcancel:=New(pmsgbutton,Init(@self,2,wohin^.Attr.x+wohin^.Attr.w-59,Attr.h-capdy-15,'mb_cancel',false));
END;

PROCEDURE tcopy.SetupWindow;
BEGIN
  INHERITED SetupWindow;
  pcancel^.enable;
  prozent:=1;
  oldfocus:=pcancel^.HWindow;
  instrec.installedfiles:=0;
  processing:=false;
  SetTimer(HWindow,1993,200,NIL);
END;

DESTRUCTOR tcopy.Done;
BEGIN
  Parent^.enable;
  PostMessage(Parent^.HWindow,wm_command,2,0);
  INHERITED Done;
END;

VAR i:integer;
    s:STRING[fsPathName];
    dir:STRING[fsDirectory];
    sdir:ARRAY[0..fsPathName] OF char;
    names:STRING;
    olderrormode:word;

FUNCTION tcopy.getentrie(rubrik:ShortStr; optnum,num:integer; VAR src:FileStr; VAR dest:PathStr; VAR newcnt:word):boolean;
BEGIN
  getentrie:=false;
  olderrormode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  WITH instrec DO
  BEGIN
    { create section key in sdir }
    IF optnum>0 THEN
    BEGIN
      Str(optnum,s);
      s:=RUBRIK+s;
      strpcopy(sdir,s);
    END ELSE strpcopy(sdir,rubrik);
    Str(num,s); s:=s+#0;
    i:=GetPrivateProfileString(sdir,@s[1],'',@names[1],SizeOf(names)-1,INIFILE);
    IF i>0 THEN
    BEGIN
      names[0]:=char(byte(i));
      src:=Copy(names,1,Pos(',',names)-1);
      dest:=Copy(names,Pos(',',names)+1,Length(names)-Length(src)-1);
      IF optnum>0 THEN
      BEGIN
        { store subdirectory in sdir - create if necessary }
        IF GetPrivateProfileString(sdir,'PATH','',sdir,fsFileName+fsExtension+2,INIFILE)>0 THEN
        BEGIN
          s:=strpas(sdir); IF s[1]='\' THEN Delete(s,1,1); IF s[Length(s)]='\' THEN dec(s[0]);
          dir:=s;
          s:=strpas(destdir)+dir;
          IF Length (s) > fsDirectory-1 THEN
            BEGIN dec (dir[0], Length (s) - (fsDirectory-1)); s[0] := Chr(fsDirectory-1); END;
          {$i-}
          ChDir(s);
          IF IOResult<>0 THEN
          BEGIN
            s:=strpas(destdir); IF s[Length(s)]='\' THEN dec(s[0]);
            ChDir(s);
            MkDir(dir);
          END;
        END ELSE dir:='';
      END;
      IF optnum>0 THEN
      BEGIN
        { look for source files in sdir }
        IF DOSERROR<>0 THEN
        BEGIN
          s:=src+#0;
          strcopy(sdir,inidir); strcat(sdir,@s[1]);
          FindFirst(sdir,faReadOnly OR faHidden OR faSysFile OR faArchive,srec);
        END ELSE FindNext(srec);
        IF DOSERROR=0 THEN
        BEGIN
          src:=copy(src,1,Pos('\',src))+strpas(srec.Name);
          dest:=Copy(src,Pos('\',src)+1,Pos('.',src)-Pos('\',src)-1)+Copy(dest,Pos('.',dest),4);
          dest:=dir+'\'+dest;
          getentrie:=true;
        END ELSE
        BEGIN
          inc(instrec.aktfileinoption);
          getentrie:=getentrie('OPTFILES',optnum,instrec.aktfileinoption,src,dest,
                    instrec.aktfileinoption);
        END;
      END ELSE BEGIN inc(newcnt); getentrie:=true; END;
    END;
  END;
  SetErrorMode(olderrormode);
END;

PROCEDURE tcopy.paintprcnt(adc:hdc);
VAR oldfont:hFont;
    s:STRING[6];
    xb,yb,wb,hb,xp:integer;
BEGIN
  oldfont:=SelectObject(adc,oemHandle);
  xb:=8;
  yb:=wohin^.Attr.y+wohin^.Attr.h+10;
  wb:=wohin^.Attr.w-65;
  hb:=20;
  xp:=wb DIV 2 -((5*fw) DIV 2);
  framefilled3d(adc,xb,yb,wb,hb,1,rgb(192,192,192),down);
  framefilled3d(adc,xb+1,yb+1,xb+1+Trunc(1.0*(wb-2)*prozent/100)-xb-1,hb-2,2,rgb(192,192,192),up);
  Str(prozent:3,s); s:=s+' %'+#0;
  SetBkMode(adc,transparent);
  SetTextAlign(adc,ta_top OR ta_left);
  SetTextColor(adc,rgb(128,128,128));
  ExtTextOut(adc,xp+4,yb+8,0,NIL,pchar(@s[1]),5,NIL);
  SetTextColor(adc,rgb(0,0,0));
  ExtTextOut(adc,xp,yb+5,0,NIL,pchar(@s[1]),5,NIL);
  SelectObject(adc,oldfont);
END;

PROCEDURE tcopy.wmEraseBkGnd(VAR msg:tmessage);
VAR OLDFONT:thandle;
BEGIN
  paintcaption(msg.wParam);
  {Rahmen zeichnen}
  framefilled3d(msg.wParam,0,capdy,Attr.w-2,Attr.h-capdy-2,2,RGB(192,192,192),up);
  {CHILDS malen}
  WITH titelher^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH titelhin^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH woher^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH wohin^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  {
  frame3d(msg.wParam,titelher^.Attr.x-2,titelher^.Attr.y-2,
                     wohin^.Attr.w+4,wohin^.Attr.y+wohin^.Attr.h-titelher^.Attr.y+4,1,down);
  }
  paintprcnt(msg.wParam);
  INHERITED WmEraseBkgnd(MSG);
END;

VAR umsg:tmsg;
PROCEDURE tcopy.Break;
BEGIN
  WHILE processing DO
  BEGIN
    IF (PeekMessage(umsg,0,0,0,pm_remove))AND(msg.message<>wm_quit) THEN
    BEGIN
      TranslateMessage(umsg);
      DispatchMessage(umsg);
    END;
    IF msg.message=wm_quit THEN Halt(0);
  END;
  processing:=true;
  IF OURMessagebox(@self,modc.shortver,getresstring(308),mb_yesno OR mb_iconquestion)=id_yes
  THEN
  BEGIN
    processing:=false;
    PostMessage(HWindow,wm_close,0,0);
    Exit;
  END ELSE processing:=false;
END;

FUNCTION tcopy.timerproc;
VAR dst,src:integer;
    dststr,srcstr:tofstruct;
    s_src,s_dest:PathStr;
    s:STRING[100];
    p:pointer;
    dc:hdc;
    ende,IsValid:boolean;
    i: byte;
 BEGIN
   {Quelle}
   IF INHERITED timerproc(msg) THEN Exit;
   IF processing THEN Exit;
   processing:=true;
   KillTimer(HWindow,msg.wparam);
   ende:=false;
   IF msg.wparam=1994 THEN
   BEGIN
     {Timeout für Anlegen der Programm-Gruppe}
     ourmessagebox(@self,caption,getresstring(110),mb_ok OR mb_iconexclamation);
    {$ifndef share}
      runcheck;
    {$endif}
     {Install beendet}
     ourmessagebox(@self,caption,getresstring(111),mb_ok OR mb_iconinformation);
     processing:=false;
     PostMessage(HWindow,wm_close,0,0);
     Exit;
   END;

   {QUELL+ZIELDATEI aus INI HOLEN}
   strcopy(@currdir,@inidir);
   IF Count<=instrec.numreqfiles THEN
   BEGIN
     getentrie('REQFILES'+#0,0,Count,s_src,s_dest,Count);
     s_src:=s_src+#0;
     s_dest:=s_dest+#0;
     StrLCat(@currdir,@s_src[1],SizeOf(currdir)-1);
   END ELSE IF instrec.numoptions>0 THEN
   BEGIN
     WHILE (NOT instrec.instopt[instrec.aktoption])AND(instrec.aktoption<instrec.numoptions)
     DO inc(instrec.aktoption);
     IF instrec.instopt[instrec.aktoption] THEN
     BEGIN
       IF getentrie('OPTFILES',instrec.aktoption,instrec.aktfileinoption,s_src,s_dest,instrec.aktfileinoption) THEN
       BEGIN
         s_src:=s_src+#0;
         s_dest:=s_dest+#0;
         StrLCat(@currdir,@s_src[1],SizeOf(currdir)-1);
         inc(Count);
       END ELSE
       BEGIN
         inc(instrec.aktoption); instrec.aktfileinoption:=1;
         IF instrec.aktoption>instrec.numoptions THEN ENDE:=TRUE
         ELSE BEGIN processing:=false; SetTimer(HWindow,1993,200,NIL); Exit; END;
       END;
     END ELSE ENDE:=true;
   END ELSE ENDE:=true;
   IF NOT ende THEN
   BEGIN
     woher^.SetText(AnsiLower(@currdir));
     src:=LZOpenFile(@currdir,srcstr,of_read);
     IF (src=-1) THEN
     BEGIN
       ourmessagebox(@self,caption,getresstring(112),mb_ok OR mb_iconstop);
       processing:=false;
       PostMessage(HWindow,wm_close,0,0);
       Exit;
     END;
     {Ziel}
     strcopy(@windir,@destdir);
     StrLCat(@windir,@s_dest[1],SizeOf(windir)-1);
     wohin^.SetText(AnsiLower(@windir));
     dst:=LZOpenFile(@windir,dststr,of_write OR of_create OR of_cancel);
     LZCopy(src,dst);
     LZClose(dst);
     LZClose(src);
     prozent:=((Count-1)*100) DIV forcedfiles;
     dc:=GetDC(HWindow); paintprcnt(dc); ReleaseDC(HWindow,dc);
     inc(instrec.installedfiles);
   END;
   IF ENDE THEN
   BEGIN
     IF instrec.installedfiles>0 THEN
     BEGIN
       IF MakeAss THEN AssociateExtensions(strpas(destdir) + instrec.chkfile1);
       {BEGIN
         strpcopy(@x,getresstring(118));
         woher^.SetText(@x);
         wohin^.SetText('');
         titelhin^.SetText('');
         titelher^.SetText('');
         RegSetValue (hKey_Classes_Root, 'ModulePlayer', reg_sz, 'Module Player', StrLen ('Module Player'));
         RegSetValue (hKey_Classes_Root, 'ModulePlayer\shell\print', reg_sz, '', StrLen (''));
         s := strpas(destdir) + instrec.chkfile1 + ' %1' + #0;
         RegSetValue (hKey_Classes_Root, 'ModulePlayer\shell\open\command', reg_sz, @s[1], Length (s));
         FOR i := 1 TO 255 DO BEGIN
           Str (i, s);
           s := s + #0;
           s[0] := char (GetPrivateProfileString ('OPTFILES1', @s[1], '', @s[1], SizeOf (s), instrec.INIFILE));
           IF (Length (s) = 10) THEN s := s + #0;
           IF (Length (s) <> 11) THEN system.Break;
           Delete (s, 1, 7);
           IF NOT ((s[1] = '.') AND
                  (UpCase (s[2]) IN ['0'..'9', 'A'..'Z']) AND
                  (UpCase (s[3]) IN ['0'..'9', 'A'..'Z']) AND
                  (UpCase (s[4]) IN ['0'..'9', 'A'..'Z', #0])) THEN system.Break;
           s := s + #0;
           RegSetValue (hKey_Classes_Root, @s[1], reg_sz, 'ModulePlayer', StrLen ('ModulePlayer'));
         END;
       END;{}
       IF MakeGroup THEN BEGIN
         strpcopy(@x,getresstring(113));
         woher^.SetText(@x);
         wohin^.SetText('');
         titelhin^.SetText('');
         titelher^.SetText('');
         atom:=GlobalAddAtom('PROGMAN');
         topic:=GlobalAlloc(gmem_ddeshare,1024);
         SetTimer(HWindow,1994,10000,NIL);
         PostMessage($FFFF,wm_dde_initiate,HWindow,MakeLong(atom,atom));
        {SendMessage($FFFF,wm_dde_initiate,HWindow,MakeLong(atom,atom));}
       END ELSE BEGIN
         {$ifndef share}
           runcheck;
         {$endif}
         {Install beendet}
         ourmessagebox(@self,caption,getresstring(111),mb_ok OR mb_iconinformation);
         processing:=false;
         PostMessage(HWindow,wm_close,0,0);
         Exit;
       END;
     END ELSE
     BEGIN
       errmessagebox(@self,307);
       processing:=false;
       PostMessage(HWindow,wm_close,0,0);
       Exit;
     END;
   END ELSE SetTimer(HWindow,1993,200,NIL);
   processing:=false;
END;

PROCEDURE tcopy.dde(VAR msg:tmessage);
VAR s:STRING;
    p:pointer;
BEGIN
  IF (msg.lparamlo=atom) THEN
  BEGIN
    msg.lparamhi:=topic; msg.lparamlo:=0;
    p:=Ptr(topic,0);
    s:=strpas(@destdir); s:=s+instrec.chkfile1;
    strpcopy(p,'[Creategroup('+instrec.grpname+')] [showgroup('+instrec.grpname+',1)] [Additem('+s+')] [exitprogman(1)]');
    SendMessage(msg.wparam,wm_dde_execute,HWindow,msg.lparam);
   {PostMessage(msg.wparam,wm_dde_execute,HWindow,msg.lparam);}
  END ELSE
  BEGIN
    KillTimer(HWindow,1994);
    PostMessage(msg.wparam,wm_dde_terminate,HWindow,0);
    GlobalDeleteAtom(atom);
    GlobalFree(topic);
    {$ifndef share}
      runcheck;
    {$endif}
    {Install beendet}
    ourmessagebox(@self,caption,getresstring(111),mb_ok OR mb_iconinformation);
    Done;
    Exit;
  END;
END;

PROCEDURE tcopy.WMClose(VAR msg:tmessage);
BEGIN
  msg.result:=0;
  Done;
END;

{$ifndef share}
PROCEDURE tcopy.runcheck;
VAR f   :FILE;
    okay:boolean;
    i,test:word;
    s:STRING;

    PROCEDURE getchecksum(Name:STRING; VAR checks,siz:longint);
    VAR cf:FILE;
        j,numread:word;
    BEGIN
      Assign(cf,Name);
      {$I-}
      Reset(cf,1);
      IF IOResult<>0 THEN
      BEGIN
        Halt(1);
      END;
      {Checksummen berechnen}
      checks:=0;
      REPEAT
       BlockRead(cf,x,xLen,numread);
       IF numread>0 THEN FOR j:=0 TO numread-1
       DO checks:=checks+byte(x[j]);
      UNTIL numread=0;
      siz:=FileSize(cf);
      Close(cf);
    END;

BEGIN
  s:=strpas(@destdir);
  {$I-}
  Assign(f,s+instrec.chkfile1);
  filemode:=2;
  inoutres:=0;
  Reset(f,1);
  IF IOResult<>0 THEN BEGIN errmessagebox(@self,306); Exit; END;
  {Username suchen}
  okay:=false;
  REPEAT
   BlockRead(f,x,xLen,test);
   FOR i:=0 TO test-14 DO
   BEGIN
    IF ((x[i]='D')AND(x[i+1]='O')AND(x[i+2]='O')AND(x[i+3]='O')
    AND(x[i+4]='V')AND(x[i+5]='I')AND(x[i+6]='E'))
    AND ((x[i+7]='O')AND(x[i+8]='F')AND(x[i+9]='F')AND(x[i+10]='L')
    AND(x[i+11]='I')AND(x[i+12]='N')AND(x[i+13]='E')AND(x[i+14]=#0)) THEN
    BEGIN
      okay:=true;
      Seek(f,(FilePos(f)-test)+i);
    END;
   END;
   IF NOT okay THEN Seek(f,FilePos(f)-20);
  UNTIL okay OR (test<xLen);
  IF okay THEN
  BEGIN
    FOR i:=1 TO Length(user) DO x[i-1]:=Chr(NOT(byte(user[i])));
    FOR i:=Length(user)+1 TO 30 DO x[i-1]:=#0;
    BlockWrite(f,x,30);
  END;
  {COMPANY Suchen}
  okay:=false;
  Seek(f,FilePos(f)-1000);
  REPEAT
   BlockRead(f,x,xLen,test);
   FOR i:=0 TO test-14 DO
   BEGIN
    IF ((x[i]='D')AND(x[i+1]='O')AND(x[i+2]='O')AND(x[i+3]='O')
    AND(x[i+4]='V')AND(x[i+5]='I')AND(x[i+6]='E'))
    AND ((x[i+7]='O')AND(x[i+8]='F')AND(x[i+9]='F')AND(x[i+10]='L')
    AND(x[i+11]='I')AND(x[i+12]='N')AND(x[i+13]='E')AND(x[i+14]='2')AND(x[i+15]=#0)) THEN
    BEGIN
      okay:=true;
      Seek(f,(FilePos(f)-test)+i);
    END;
   END;
   IF NOT okay THEN Seek(f,FilePos(f)-20);
  UNTIL okay OR (test<xLen);
  IF okay THEN
  BEGIN
    IF Length(company)>0 THEN
    BEGIN
      FOR i:=1 TO Length(company) DO x[i-1]:=Chr(NOT(byte(company[i])));
      FOR i:=Length(company)+1 TO 30 DO x[i-1]:=#0;
    END ELSE FOR i:=1 TO 30 DO x[i]:=#0;
    BlockWrite(f,x,30);
  END;
  IF okay THEN
  BEGIN
    Close(f);
    {Checksummen berechnen}
    getchecksum(s+instrec.chkfile1,checksum,Size);
    checksum:=longint(checksum)-Ord('B')-Ord('L')-Ord('O')-Ord('E')
             -Ord('D')-Ord('I')-Ord('E');
    Reset(f,1);
    {Checkpos suchen}
    okay:=false;
    REPEAT
     BlockRead(f,x,xLen,test);
     FOR i:=0 TO test-8 DO
     BEGIN
      IF ((x[i]='B')AND(x[i+1]='L')AND(x[i+2]='O')AND(x[i+3]='E')
      AND(x[i+4]='D')AND(x[i+5]='I')AND(x[i+6]='E')) THEN
      BEGIN
        okay:=true;
        Seek(f,(FilePos(f)-test)+i);
      END;
     END;
     IF NOT okay THEN Seek(f,FilePos(f)-8);
    UNTIL okay OR (test<xLen);
    IF okay THEN BlockWrite(f,Size,8);
    Close(f);
  END;
END;
{$endif}


{***********************Hauptfenster******************************}
TYPE pinit=^tinit;
     tinit=object(TBUZapplication)
     PROCEDURE InitMainWindow; virtual;
     FUNCTION idleaction:boolean; virtual;
     END;

FUNCTION tinit.idleaction;
BEGIN
  idleaction:=false;
END;
PROCEDURE tinit.InitMainWindow;
VAR s:STRING[80];
BEGIN
  INHERITED InitMainWindow;
  {INI-Verzeichnis finden}
  s:=ParamStr(0);
  WHILE (s[Length(s)]<>'\')AND(Length(s)>0) DO dec(byte(s[0]));
  strpcopy(@inidir,s);
  MainWindow:=New(pinstall,Init(NIL));
END;

VAR thisrun:Tinit;
BEGIN
 TryToUsePalette:=False;
 thisrun.Init(NIL);
 thisrun.Run;
 thisrun.Done;
 releaselanguage;
END.