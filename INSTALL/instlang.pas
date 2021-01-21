UNIT InstLang;

INTERFACE

USES Win31,OWINDOWS,WINTYPES,WINPROCS,WINUNIT,MODC,Coroner,WinDOS;

PROCEDURE LoadLanguage;
PROCEDURE ReleaseLanguage;

IMPLEMENTATION

TYPE PLanguageSel=^LanguageSel;
     LanguageSel=OBJECT(Tmywindow)
       answer:plongint;
       bger,busa,bfra,bspa,bgrk,bita,bNL,bCZE:pmsgbutton;
       CONSTRUCTOR Init(aparent:pwindowsobject; result:plongint);
       DESTRUCTOR Done; VIRTUAL;
       PROCEDURE SetupWindow; VIRTUAL;
       PROCEDURE wmEraseBkGnd(VAR msg:tmessage); VIRTUAL  wm_first+wm_erasebkgnd;
       PROCEDURE usa(VAR msg:tmessage); VIRTUAL     id_first+101;
       PROCEDURE ger(VAR msg:tmessage); VIRTUAL     id_first+102;
       PROCEDURE fra(VAR msg:tmessage); VIRTUAL     id_first+103;
       PROCEDURE spa(VAR msg:tmessage); VIRTUAL     id_first+104;
       PROCEDURE grk(VAR msg:tmessage); VIRTUAL     id_first+105;
       PROCEDURE ita(VAR msg:tmessage); VIRTUAL     id_first+106;
       PROCEDURE nl (VAR msg:tmessage); VIRTUAL     id_first+107;
       PROCEDURE cze (VAR msg:tmessage);VIRTUAL     id_first+108;
       PROCEDURE wmkeydown(VAR msg:tmessage);    VIRTUAL wm_first+wm_keydown;
     END;

CONSTRUCTOR LanguageSel.Init;
VAR XOfs:Integer;
BEGIN
  answer:=result;
  INHERITED Init(aparent, ProjectName + ' Installation',ws_sysmenu,notopmost);
  XOfs:=20;
  answer^:=0;
  IF _USA IN LangPresent THEN
  BEGIN
    busa:=New(pmsgbutton,Init(@self,101,XOfs,120,'B_USA',false));
    Inc(XOfs,115);
    IF Answer^=0 THEN answer^:=1;
  END ELSE busa:=NIL;
  IF _GER IN LangPresent THEN
  BEGIN
    bger:=New(pmsgbutton,Init(@self,102,XOfs,120,'B_GER',false));
    Inc(XOfs,115);
    IF Answer^=0 THEN answer^:=2;
  END ELSE bger:=NIL;
  IF _FRE IN LangPresent THEN
  BEGIN
    bfra:=New(pmsgbutton,Init(@self,103,XOfs,120,'B_FRA',false));
    Inc(XOfs,115);
    IF Answer^=0 THEN answer^:=3;
  END ELSE bfra:=NIL;
  IF _SPA IN LangPresent THEN
  BEGIN
    bspa:=New(pmsgbutton,Init(@self,104,XOfs,120,'B_SPA',false));
    Inc(XOfs,115);
    IF Answer^=0 THEN answer^:=4;
  END ELSE bspa:=NIL;
  IF _GRK IN LangPresent THEN
  BEGIN
    bgrk:=New(pmsgbutton,Init(@self,105,XOfs,120,'B_GRK',false));
    Inc(XOfs,115);
    IF Answer^=0 THEN answer^:=5;
  END ELSE bgrk:=NIL;
  IF _ITA IN LangPresent THEN
  BEGIN
    bITA:=New(pmsgbutton,Init(@self,106,XOfs,120,'B_ITA',false));
    Inc(XOfs,115);
    IF Answer^=0 THEN answer^:=6;
  END ELSE bITA:=NIL;
  IF _NL IN LangPresent THEN
  BEGIN
    bNL:=New(pmsgbutton,Init(@self,107,XOfs,120,'B_NL',false));
    Inc(XOfs,115);
    IF Answer^=0 THEN answer^:=7;
  END ELSE bNL:=NIL;
  IF _CZE IN LangPresent THEN
  BEGIN
    bCZE:=New(pmsgbutton,Init(@self,108,XOfs,120,'B_CZE',false));
    Inc(XOfs,115);
    IF Answer^=0 THEN answer^:=7;
  END ELSE bCZE:=NIL;
  WITH Attr DO
  BEGIN
    style:=ws_popup OR ws_visible OR ws_border;
    h:=152;
    w:=XOfs-18;
    x :=(GetSystemMetrics(sm_cxscreen) - w) DIV 2;
    y :=(GetSystemMetrics (sm_cyscreen) - h) DIV 2;
  END;
END;

PROCEDURE LanguageSel.SetupWindow;
BEGIN
  INHERITED SetupWindow;
  IF Bcze<>NIL THEN BEGIN Bcze^.enable;oldfocus:=Bcze^.HWindow; END;
  IF Bnl <>NIL THEN BEGIN  Bnl^.enable;oldfocus:= Bnl^.HWindow; END;
  IF Bita<>NIL THEN BEGIN Bita^.enable;oldfocus:=Bita^.HWindow;END;
  IF Bgrk<>NIL THEN BEGIN Bgrk^.enable;oldfocus:=Bgrk^.HWindow;END;
  IF Bspa<>NIL THEN BEGIN Bspa^.enable;oldfocus:=Bspa^.HWindow;END;
  IF Bfra<>NIL THEN BEGIN Bfra^.enable;oldfocus:=Bfra^.HWindow;END;
  IF Bger<>NIL THEN BEGIN Bger^.enable;oldfocus:=Bger^.HWindow;END;
  IF Busa<>NIL THEN BEGIN Busa^.enable;oldfocus:=Busa^.HWindow;END;
END;

const ProjVersion = ProjectName + ' 2.40';

PROCEDURE LanguageSel.wmEraseBkGnd(VAR msg:tmessage);
VAR oldfont:hdc;
    XOfs:Integer;
BEGIN
  FrameFilled3D(msg.wParam,0,capdy,Attr.w-2,Attr.h-2-capdy,2,RGB(192,192,192),up);
  paintcaption(msg.wParam);
  SetBkMode(msg.wParam,transparent);
  SetTextAlign(msg.wParam,ta_center OR ta_top);
  SetTextColor(msg.wParam,0);
  oldfont:=SelectObject(msg.wParam,ansiHandle);
  XOfs:=57;
  IF _USA IN LangPresent THEN
  BEGIN
    TextOut(msg.wParam,XOfs,10+CapDY,'Welcome',7);
    TextOut(msg.wParam,XOfs,22+CapDY,'to',2);
    TextOut(msg.wParam,XOfs,34+CapDY,ProjVersion, length (ProjVersion));
    TextOut(msg.wParam,XOfs,60+CapDY,'Click the',9);
    TextOut(msg.wParam,XOfs,72+CapDY,'button with',11);
    TextOut(msg.wParam,XOfs,84+CapDY,'your language',13);
    Inc(XOfs,115);
  END;
  IF _GER IN LangPresent THEN
  BEGIN
    TextOut(msg.wParam,XOfs,10+CapDY,'Willkommen',10);
    TextOut(msg.wParam,XOfs,22+CapDY,'zu',2);
    TextOut(msg.wParam,XOfs,34+CapDY,ProjVersion, length (ProjVersion));
    TextOut(msg.wParam,XOfs,60+CapDY,'Bitte wählen',12);
    TextOut(msg.wParam,XOfs,72+CapDY,'Sie Ihre',8);
    TextOut(msg.wParam,XOfs,84+CapDY,'Sprache aus',11);
    Inc(XOfs,115);
  END;
  IF _FRE IN LangPresent THEN
  BEGIN
    TextOut(msg.wParam,XOfs,10+CapDY,'Bienvenue',10);
    TextOut(msg.wParam,XOfs,22+CapDY,'dans',4);
    TextOut(msg.wParam,XOfs,34+CapDY,ProjVersion, length (ProjVersion));
    TextOut(msg.wParam,XOfs,60+CapDY,'Choisissez',10);
    TextOut(msg.wParam,XOfs,72+CapDY,'le bouton de',12);
    TextOut(msg.wParam,XOfs,84+CapDY,'votre langue',12);
    Inc(XOfs,115);
  END;
  IF _SPA IN LangPresent THEN
  BEGIN
    TextOut(msg.wParam,XOfs,10+CapDY,'Bien venido',11);
    TextOut(msg.wParam,XOfs,22+CapDY,'a',1);
    TextOut(msg.wParam,XOfs,34+CapDY,ProjVersion, length (ProjVersion));
    TextOut(msg.wParam,XOfs,60+CapDY,'Seleccione',10);
    TextOut(msg.wParam,XOfs,72+CapDY,'su lengua',9);
    TextOut(msg.wParam,XOfs,84+CapDY,'por favor',9);
    Inc(XOfs,115);
  END;
  IF _GRK IN LangPresent THEN
  BEGIN
    TextOut(msg.wParam,XOfs,10+CapDY,'Kalwsil8ate',11);
    TextOut(msg.wParam,XOfs,22+CapDY,'sto',3);
    TextOut(msg.wParam,XOfs,34+CapDY,ProjVersion, length (ProjVersion));
    TextOut(msg.wParam,XOfs,60+CapDY,'Pieste to',9);
    TextOut(msg.wParam,XOfs,72+CapDY,'pliktro me',10);
    TextOut(msg.wParam,XOfs,84+CapDY,'th glwssa sas',13);
    Inc(XOfs,115);
  END;
  IF _ITA IN LangPresent THEN
  BEGIN
    TextOut(msg.wParam,XOfs,10+CapDY,'Benvenuti',9);
    TextOut(msg.wParam,XOfs,22+CapDY,'in',2);
    TextOut(msg.wParam,XOfs,34+CapDY,ProjVersion, length (ProjVersion));
    TextOut(msg.wParam,XOfs,60+CapDY,'Premete il',10);
    TextOut(msg.wParam,XOfs,72+CapDY,'pulsante con la',15);
    TextOut(msg.wParam,XOfs,84+CapDY,'vostra lingua',13);
    Inc(XOfs,115);
  END;
  IF _NL IN LangPresent THEN
  BEGIN
    TextOut(msg.wParam,XOfs,10+CapDY,'Welkom',6);
    TextOut(msg.wParam,XOfs,22+CapDY,'bij',3);
    TextOut(msg.wParam,XOfs,34+CapDY,ProjVersion, length (ProjVersion));
    TextOut(msg.wParam,XOfs,60+CapDY,'Klik de',7);
    TextOut(msg.wParam,XOfs,72+CapDY,'knop met',8);
    TextOut(msg.wParam,XOfs,84+CapDY,'jouw taal',9);
    Inc(XOfs,115);
  END;
  IF _CZE IN LangPresent THEN
  BEGIN
    TextOut(msg.wParam,XOfs,10+CapDY,'Vitejte',7);
    TextOut(msg.wParam,XOfs,22+CapDY,'v programu',10);
    TextOut(msg.wParam,XOfs,34+CapDY,ProjVersion, length (ProjVersion));
    TextOut(msg.wParam,XOfs,60+CapDY,'Kliknete',8);
    TextOut(msg.wParam,XOfs,72+CapDY,'na tlacitko s',13);
    TextOut(msg.wParam,XOfs,84+CapDY,'vasim jazykem',13);
    Inc(XOfs,115);
  END;
  SelectObject(msg.wParam,oldfont);
  INHERITED wmEraseBkGnd(msg);
END;

PROCEDURE LanguageSel.USA(VAR msg:tmessage); BEGIN answer^:=1; PostMessage(HWindow,wm_close,0,0); END;
PROCEDURE LanguageSel.GER(VAR msg:tmessage); BEGIN answer^:=2; PostMessage(HWindow,wm_close,0,0); END;
PROCEDURE LanguageSel.FRA(VAR msg:tmessage); BEGIN answer^:=3; PostMessage(HWindow,wm_close,0,0); END;
PROCEDURE LanguageSel.SPA(VAR msg:tmessage); BEGIN answer^:=4; PostMessage(HWindow,wm_close,0,0); END;
PROCEDURE LanguageSel.GRK(VAR msg:tmessage); BEGIN answer^:=5; PostMessage(HWindow,wm_close,0,0); END;
PROCEDURE LanguageSel.ITA(VAR msg:tmessage); BEGIN answer^:=6; PostMessage(HWindow,wm_close,0,0); END;
PROCEDURE LanguageSel.NL (VAR msg:tmessage); BEGIN answer^:=7; PostMessage(HWindow,wm_close,0,0); END;
PROCEDURE LanguageSel.CZE(VAR msg:tmessage); BEGIN answer^:=8; PostMessage(HWindow,wm_close,0,0); END;

DESTRUCTOR LanguageSel.Done;
BEGIN
  PostAppMessage(GetCurrentTask,19993,$1234,$12344321);
  TWindow.Done;
END;

PROCEDURE LanguageSel.wmkeydown(VAR msg:tmessage);
BEGIN
  CASE msg.wparam OF
    vk_execute,vk_return,vk_space:SendMessage(oldfocus,wm_char,vk_return,0);
    vk_escape           :PostMessage(HWindow,wm_close,0,0);
    ELSE DefWndProc(msg);
  END;
  msg.result:=0;
END;

PROCEDURE LoadLanguage;
VAR wmsg:tmsg;
    pw:PLanguageSel;
    stil:longint;
    ende:boolean;
    testlong:longint;
    olderrmode:word;
    s:STRING[fsPathName];
    Lang: TLang;

BEGIN
  hLanguage:=hInstance;
  ende:=false;
  Languages:=0;
  LangPresent:=[];
  s:=ParamStr(0); WHILE (s[Length(s)]<>'\' )AND(Length(s)>0)DO dec(byte(s[0]));
  for Lang := low (TLang) to high (TLang) do
    if FilePresent (s + INST_Pref + '_' + LangCodes[Lang] + '.DLL') then begin
      include (LangPresent, Lang);
      stil := succ (ord (Lang));
      inc (Languages);
    end;
  IF LangPresent=[] THEN
  BEGIN
    MessageBox(0,'No language files (INST_xxx.DLL) found.','Fatal Error',mb_ok OR mb_iconstop);
    Halt(0);
  END;
  IF Languages>1 THEN
  BEGIN
    pw:=New(PLanguageSel,Init(NIL,@stil));
    pw^.Create;
    WHILE  NOT ende DO
    BEGIN
      GetMessage(wmsg,0,0,0);
      IF ((wmsg.hwnd=0)
         AND(wmsg.lparam=$12344321)
         AND(wmsg.wparam=$1234)
         AND(wmsg.message=19993))
       THEN ende:=true
       ELSE
       BEGIN
         TranslateMessage(wmsg);
         DispatchMessage(wmsg);
       END;
    END;
  END;
  WinLanguage := TLang (pred (stil));
  RES := S+INST_Pref + '_' + LangCodes[WinLanguage] + '.DLL';
  s:=RES+#0;
  olderrmode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  hLanguage:=LoadLibrary(@s[1]);
  IF GetModuleUsage(hLanguage)>1 THEN
  BEGIN
    releaseLanguage;
    hLanguage:=LoadLibrary(@s[1]);
  END;
  SetErrorMode(olderrmode);
  {IST HANDLE VALID!}
  IF hLanguage<=32 THEN
  BEGIN
    s:=RES + ' not found!'+#0;
    MessageBeep(0);
    MessageBox(0,@s[1],'Fatal Error!',mb_ok OR mb_iconstop);
    Halt(0);
  END;
  pBUZApplication(Application)^.ReleaseFonts;
  pBUZApplication(Application)^.LoadFonts;
END;

PROCEDURE releaseLanguage;
BEGIN
  IF hLanguage=hInstance THEN Exit;
  WHILE GetModuleUsage(hLanguage)>1 DO FreeLibrary(hLanguage);
  FreeLibrary(hLanguage);
END;

VAR OldExit: pointer;
    OldCriticalExit: pointer;

{$IFOPT S+} {$DEFINE StackCheck} {$ENDIF} {$S-}
PROCEDURE CriticalExitProc; far;
BEGIN
  @CriticalExit:=OldCriticalExit;
  ExitProc := OldExit;
  releaseLanguage;
END;

PROCEDURE SafeExit; far;
BEGIN
  ExitProc := OldExit;
  releaseLanguage;
END;

{$IFDEF StackCheck} {$S+} {$ENDIF}

BEGIN
  IF HPrevInst = 0 THEN
  BEGIN
    OldExit := ExitProc;
    ExitProc := @SafeExit;
    OldCriticalExit:=@CriticalExit;
    CriticalExit:=CriticalExitProc;
  END;
END.
