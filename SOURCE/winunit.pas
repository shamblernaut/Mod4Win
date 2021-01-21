UNIT winunit;
{ $C Fixed Preload Permanent}
{$R winunit.res}

INTERFACE
USES winprocs,wintypes,odialogs,owindows,omemory,win31,strings,mmsystem,modC,User32,W32Defs,
     WOW32,objects,windos,validate,Coroner,GetOS,oFarProc;

CONST
  DefDrawFocus:Boolean=True;
  WinLanguage:TLang=_usa;
  SysBMPOfsX:Integer=2;
  SysBMPOfsY:Integer=2;
  SysBMPW:Integer=16;
  SysBMPH:Integer=14;
  {Kontrollobjekte}
    _on =1;
    off =0;
    up  =1;
    down=0;
  {Tmywindow}
    wm_IHaveTheFocus=11005;
    wm_idleaction   =11010;
    wm_selection    =11011;
    swebuz_maximize =11012;
    swebuz_restore  =11013;
    wm_internaldrop =11014;
    wm_dropcursorpos=11015;
    wm_candrag      =11016;
    wm_beginIntDrag =11017;
    wm_EndIntDrag   =11018;
    wm_getDragData  =11019;
    WM_SysTimer=$0118;
    MoveOnClick:boolean=true;
    FullMove:Boolean=True;
    CTLCOLOR_GRAYBOX=16;
    CTLCOLOR_INVSTATIC=32;
    {TOPMOST-CONST}
    id_topmost=11014;
    canbetopmost=1;
    notopmost=0;
    istopmost=2;

{Applikations-Farbpalette}
CONST
  TryToUsePalette:Boolean=True;
  UsePalette:Boolean=True;    {Soll die Winunit Ihre 256-Farben Features benutzen?}
  {=======================================================================}
  NumSysColors=23;
VAR
  cUp         :Longint;{ 1..3DRahmen up}
  cDown       :Longint;{ 2..3DRahmen down}
  cForeGnd    :Longint;{ 3..3DRahmen helle Füllung}
  cBackGnd    :Longint;{ 4..3DRahmen dunkle Füllung}
  cTextShadow :Longint;{ 5..Text Schatten (z.B.: Waitbox)}
  cVGEdit     :Longint;{ 6..Textfarbe normales Edit}
  cHGEdit     :Longint;{ 7..Hintegrundfarbe normales Edit}
  cVGStatic   :Longint;{ 8..Textfarbe Static}
  cHGStatic   :Longint;{09..Hintegrundfarbe Static}
  cVGInvStatic:Longint;{10..Textfarbe Inverses Static}
  cHGInvStatic:Longint;{11..Hintegrundfarbe Inverses Static}
  cVGLB       :Longint;{12..Textfarbe Listbox}
  cHGLB       :Longint;{13..Hintegrundfarbe Listbox}
  cVGGrayBox  :Longint;{14..Textfarbe Graue Listbox}
  cHGGrayBox  :Longint;{15..Hintegrundfarbe Graue Listbox}
  cBrEdit     :Longint;{16..Hintergrundpinsel-Farbe normales Edit}
  cBrStatic   :Longint;{17..Hintergrundpinsel-Farbe Static}
  cBrInvStatic:Longint;{18..Hintergrundpinsel-Farbe }
  cBrLB       :Longint;{19..Hintergrundpinsel-Farbe Listbox}
  cBrGrayBox  :Longint;{20..Hintergrundpinsel-Farbe Graue Listbox}
  cBrScrollbar:Longint;{21..Hintergrundpinsel-Farbe Schrollbar}
  cBlack      :Longint;{22..Schwarzer Hintergrundpinsel}
  cVGDisplay  :Longint;{23..Farbe im Display}
  BrEdit      :hBrush;
  BrStatic    :hBrush;
  BrInvStatic :hBrush;
  BrListBox   :hBrush;
  BrGrayBox   :hBrush;
  BrScrollbar :hBrush;
  BrBlack     :hBrush;

CONST Brightness:integer=100; {0=nix;100=normal;200=zu hell}
      Contrast  :integer=100;
      ColorDepth:integer=100;
      hPal:Word=0;               {Handle der ApplikationsPalette}
      hRefPal:Word=0;            {Handle der ApplikationsReferenzPalette}
      ColorsUsed:Word=0;         {Vom Programm benutzte FarbenAnzahl}
      {Farbtabelle}
CONST ColorTableVersion:Word=$300;
      NumCols:Word=0;
      ColorTableStart:STRING=' ';
      _ColorTableStart:STRING=' ';
      __ColorTableStart:STRING=' ';
      ___ColorTableStart:STRING=' ';
VAR Palette:tLogPalette ABSOLUTE ColorTableVersion;
VAR RealColorTable:ARRAY[0..255]OF tPaletteEntry ABSOLUTE ColorTableStart;     {FarbTabelle}
VAR RefColorTable:ARRAY[0..255]OF tPaletteEntry;

{Applicationweite Variablen}
CONST FontLoaded:Boolean=FALSE;
VAR oemHandle,ansiHandle:hfont;
    Fw,Fh:integer;

{*********************************Bitmapfunktionen***********************************************************}
{Funktion zum überprüfen, ob der Bilschirm eine Palette hat...}
FUNCTION  PaletteInstalled:BOOLEAN;
{Realisiert Helligkeits-,Kontrast- und Farbeinstellungen in die Palette}
PROCEDURE DoBrightNessContrastColor(DC:Word);
{Fügt eine Farbe der Farbtabelle hinzu...}
FUNCTION  IncludeColor(Color:tColorRef):Longint;
{Lädt ein Bitmap mit mehr, als nur Systemfarben}
FUNCTION  LoadMyBitmap(Instance:tHandle; BitMapName:pChar):hBitmap;
{Muss zum Löschen eines solchen Bitmaps aufgerufen werden...}
FUNCTION  DeleteMyBitMap(Hnd:tHandle):Bool;

{Gibt an der Stelle x,y im Fenster mit hwnd-Handle eine Bitmap der Größe dx,dy aus der Bitmapresource hBitmap
 mit dem Offset xl,yl (im Bitmap!) aus und liefert bei Erfolg true!}
FUNCTION  blit(hwindow:hwnd;x,y,dx,dy,x1,y1:integer;bmhandle:hbitmap):bool;
PROCEDURE frame3d(dc:hdc;x,y,dx,dy:integer;Width,up_down:byte);
PROCEDURE framefilled3d(dc:hdc;x,y,dx,dy:integer;Width:byte;color:longint;up_down:byte);
PROCEDURE Grayframefilled3d(dc:hdc;x,y,dx,dy:integer;Width:byte;color:longint;up_down:byte);
{Keystate - Funktion*****************************}
{*******CTRL ALT SHIFT STATE}
FUNCTION  keystate:byte;
{*******HOT KEY CHECK************ für wmkeyxxx}
FUNCTION iskey(wert:word):word;
{********KEY CHEcK ->TASTENKOMB. VALID?}
FUNCTION isvalidkey(wert:word):boolean;
{Wandelt beliebige Tasten+States in einen String}
FUNCTION Key2String(key:Akey):STRING;
{Messagebox-Interface************+}
PROCEDURE ErrMessageBox(pwindow:pwindowsobject;what:word);
FUNCTION ourmessagebox(aparent:pwindowsobject;atitle,Atext:STRING;flags:longint):longint;
PROCEDURE helpme(aparent:pwindowsobject;what:word);


{************************************Bitmap-Mutterobjekt***************************************************}
{Die Mutter aller BMP-Objekte...}
TYPE PBMPObject = ^BMPObject;
     BMPObject=OBJECT(tbutton)
       BMP: hBitmap;
       disabled:boolean;
       drawfocus:Boolean;
       CONSTRUCTOR Init(AParent: PWindowsObject; AnID:word;x0,y0: Integer;ABitMap: STRING);
       DESTRUCTOR Done;                             VIRTUAL;
       PROCEDURE BMPPaint;
       PROCEDURE FramePaint(DC:hDC);                VIRTUAL;
       PROCEDURE WMEraseBkGnd(VAR msg:tmessage);    VIRTUAL wm_first + wm_erasebkgnd;
       PROCEDURE WMLButtonDown(VAR Msg: TMessage);  VIRTUAL wm_First + wm_LButtonDown;
       PROCEDURE WMLButtonDblclk(VAR Msg: TMessage);VIRTUAL wm_First + wm_LButtonDblclk;
       PROCEDURE WMMousemove(VAR Msg: TMessage);    VIRTUAL wm_First + wm_mousemove;
       PROCEDURE WMSETFOCUS(VAR Msg: TMessage);     VIRTUAL wm_First + wm_setfocus;
       PROCEDURE WMKILLFOCUS(VAR Msg: TMessage);    VIRTUAL wm_First + wm_killfocus;
       PROCEDURE WMLButtonUp(VAR Msg: TMessage);    VIRTUAL wm_First + wm_LButtonUp;
       PROCEDURE WMSysKeyDown(VAR msg:tmessage);    VIRTUAL wm_first + wm_syskeydown;
       PROCEDURE WMKeyDown(VAR Msg: TMessage);      VIRTUAL wm_First + wm_keydown;
       PROCEDURE WMKeyUp(VAR Msg: TMessage);        VIRTUAL wm_First + wm_keyup;
       PROCEDURE disable;
       PROCEDURE enable;
     END;

{************************************Bitmap-Schalterobjekt***************************************************}
{An der Stelle x0,y0 wird im übergeordneten Fenster ein Schalter erzeugt, der sich selbst zeichnet.
 die zwei Zustände (clicked,normal) sind als Bitmaps in der Bitmapresource BMP übereinander}
TYPE PBMPButton = ^BMPButton;
     BMPButton=OBJECT(BMPObject)
       clicked:boolean;
       autorep:boolean;
       outside:boolean;
       KeyClick:Boolean;
       CONSTRUCTOR Init(AParent: PWindowsObject; AnID:word;x0,y0: Integer;ABitMap: STRING;Autorepeat:boolean);
       PROCEDURE wmerasebkgnd(VAR msg:tmessage);    VIRTUAL wm_first + wm_erasebkgnd;
       PROCEDURE WMLButtonDown(VAR Msg: TMessage);  VIRTUAL wm_First + wm_LButtonDown;
       PROCEDURE WMLButtonUp(VAR Msg: TMessage);    VIRTUAL wm_First + wm_LButtonUp;
       PROCEDURE WMkeydown(VAR Msg: TMessage);      VIRTUAL wm_First + wm_keydown;
       PROCEDURE WMKILLFOCUS(VAR Msg: TMessage);    VIRTUAL wm_First + wm_killfocus;
       PROCEDURE WMkeyup(VAR Msg: TMessage);        VIRTUAL wm_First + wm_keyup;
       PROCEDURE WMMousemove(VAR Msg: TMessage);    VIRTUAL wm_First + wm_mousemove;
       PROCEDURE wmtimer(VAR Msg: TMessage);        VIRTUAL wm_First + wm_timer;
     END;
TYPE PBMPButton1 = ^BMPButton1;
     BMPButton1=OBJECT(BMPButton)
       PROCEDURE FramePaint(DC:hDC);                VIRTUAL;
     END;

TYPE PBMPCaptBtn = ^BMPCaptBtn;
     BMPCaptBtn=OBJECT(BMPButton)
       CONSTRUCTOR Init(AParent: PWindowsObject; AnID:word;x0,y0: Integer;ABitMap: STRING;Autorepeat:boolean);
       DESTRUCTOR done;virtual;
       PROCEDURE FramePaint(DC:hDC);                VIRTUAL;
     END;

{************************************Bitmap-SystemSchalterobjekt*********************************************}
{An der Stelle x0,y0 wird im übergeordneten Fenster ein Schalter erzeugt, der sich selbst zeichnet
 und SystemCommands an den Parebt schickt.
 die zwei Zustände (clicked,normal) sind als Bitmaps in der Bitmapresource BMP übereinander}
TYPE PBMPSysButton = ^BMPSysButton;
     BMPSysButton=OBJECT(BMPButton)
       ofst:integer;
       FocusTo:Word;
       CONSTRUCTOR Init(AParent: PWindowsObject; AnID:word;x0,y0,w,h,xofs: Integer);
       PROCEDURE wmerasebkgnd(VAR msg:tmessage);    VIRTUAL wm_first + wm_erasebkgnd;
       PROCEDURE WMkeyup(VAR Msg: TMessage);        VIRTUAL wm_First + wm_keyup;
       PROCEDURE WMLButtonUp(VAR Msg: TMessage);    VIRTUAL wm_First + wm_LButtonUp;
       PROCEDURE WMSETFOCUS(VAR Msg: TMessage);     VIRTUAL wm_First + wm_setfocus;
     END;

{************************************Bitmap-Schalterobjekt***************************************************}
{An der Stelle x0,y0 wird im übergeordneten Fenster ein Schalter erzeugt, der sich selbst zeichnet.
 die drei Zustände (clicked,normal,active) sind als Bitmaps in der Bitmapresource BMP übereinander}
TYPE PBMP3Button = ^BMP3Button;
     BMP3Button=OBJECT(BMPButton)
       active:boolean;
       blinkIfActive:Boolean;
       WasActive    :Boolean;
       Event:Byte;
       CONSTRUCTOR Init(AParent:PWindowsObject;AnID:word;x0,y0:Integer;ABitMap:STRING;Autorepeat:boolean);
       PROCEDURE wmerasebkgnd(VAR msg:tmessage);    VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE setstyle(style:byte);{on,off}
       PROCEDURE SetBlink(OnOff:Byte);{on,Off}
       PROCEDURE TimerScroll(VAR msg:tmessage);     VIRTUAL wm_first+wm_idleaction;
     END;

TYPE PBMP3Button1 = ^BMP3Button1;
     BMP3Button1=OBJECT(BMP3Button)
       PROCEDURE FramePaint(DC:hDC);                VIRTUAL;
     END;

TYPE PBMP3CaptBtn = ^BMP3CaptBtn;
     BMP3CaptBtn=OBJECT(BMP3Button)
       CONSTRUCTOR Init(AParent: PWindowsObject; AnID:word;x0,y0: Integer;ABitMap: STRING;Autorepeat:boolean);
       DESTRUCTOR done;virtual;
     END;

{************************************Bitmap-Up-Down-Schalterobjekt*****************************************}
{An der Stelle x0,y0 wird im übergeordneten Fenster ein Schalter erzeugt, der sich selbst zeichnet.
 Der Schalter ist als up-down-Pfeil dargestellt und informiert das Parent-Fenster über seinen aktuellen
 Status, wenn der sich ändert. Der maximalwert (down) ist dwnmax und der Initialwert dwn.
 die drei Zustände (clicked,normal,active) sind als Bitmaps in der Bitmapresource BMP übereinander und
 die Pfeile nebeneinander}
TYPE PBMPUDButton = ^BMPUDButton;
     BMPUDButton=OBJECT(BMPObject)
       uclicked,dclicked:boolean;
       low_lim,up_lim,pos:integer;
       h,w:word;
       CONSTRUCTOR Init(AParent: PWindowsObject; AnID,x0,y0,ll,ul,dwn: Integer;ABitMap: STRING);
       PROCEDURE wmerasebkgnd(VAR msg:tmessage);    VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE wmtimer(VAR Msg: TMessage);        VIRTUAL wm_First + wm_timer;
       PROCEDURE WMLButtonDown(VAR Msg: TMessage);  VIRTUAL wm_First + wm_LButtonDown;
       PROCEDURE WMLButtonUp(VAR Msg: TMessage);    VIRTUAL wm_First + wm_LButtonUp;
       PROCEDURE WMkeydown(VAR Msg: TMessage);      VIRTUAL wm_First + wm_keydown;
       FUNCTION  getpos:integer;
       PROCEDURE setpos(n:integer);
       procedure SetLimits (ll, ul: integer);
     END;
{***************2State-Knopf mit runden Focus-Ecken**************************}
TYPE PMSGButton = ^MSGButton;
     MSGButton=OBJECT(BMPCaptBtn)
       CONSTRUCTOR Init(AParent: PWindowsObject; AnID:word;x0,y0: Integer;ABitMap: STRING;Autorepeat:boolean);
       PROCEDURE FramePaint(DC:hDC);VIRTUAL;
     END;

{************************************Bitmap-SchiebeSchalterobjekt*****************************************}
{An der Stelle x0,y0 wird im übergeordneten Fenster ein Schalter erzeugt, der sich selbst zeichnet.
 Der Schalter ist als Schiebeschalter dargestellt und informiert das Parent-Fenster über seinen aktuellen
 Status, wenn der sich ändert. Der maximalwert (down) ist dwnmax und der Initialwert dwn.
 die Zustände (lever,plain) sind als Bitmaps in der Bitmapresource BMP übereinander}
TYPE PSwitch = ^Switch;
     Switch=OBJECT(BMPObject)
       ID:Word;
       invalidpos:ARRAY[1..256] OF boolean;
       clicked:boolean;
       maxdown,pos:byte;
       h,w:word;
       CONSTRUCTOR Init(AParent: PWindowsObject; AnID,x0,y0:integer;maxdwn,dwn:Byte;ABitMap: STRING);
       PROCEDURE wmerasebkgnd(VAR msg:tmessage);    VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE WMLButtonDown(VAR Msg: TMessage);  VIRTUAL wm_First + wm_LButtonDown;
       PROCEDURE WMLButtonUp(VAR Msg: TMessage);    VIRTUAL wm_First + wm_LButtonUp;
       PROCEDURE WMmousemove(VAR Msg: TMessage);    VIRTUAL wm_First + wm_mousemove;
       PROCEDURE WMkeydown(VAR Msg: TMessage);      VIRTUAL wm_First + wm_keydown;
       PROCEDURE WMKILLFOCUS(VAR Msg: TMessage);    VIRTUAL wm_First + wm_killfocus;
       PROCEDURE invalidatepos(x:byte);
       PROCEDURE validatepos(x:byte);
       FUNCTION  getpos:integer;
       PROCEDURE setpos(x:integer);
       PROCEDURE drawpos(msg:tmessage);             VIRTUAL;
     END;
{************************************Bitmap-SchiebeSchalterobjekt*****************************************}
{An der Stelle x0,y0 wird im übergeordneten Fenster ein Schalter erzeugt, der sich selbst zeichnet.
 Der Schalter ist als Schiebeschalter dargestellt und informiert das Parent-Fenster über seinen aktuellen
 Status, wenn der sich ändert. Der maximalwert (down) ist dwnmax und der Initialwert dwn.
 die Zustände (lever,plain) sind als Bitmaps in der Bitmapresource BMP übereinander}
TYPE PLRSwitch = ^LRSwitch;
     LRSwitch=OBJECT(Switch)
       CONSTRUCTOR Init(AParent: PWindowsObject; AnID,x0,y0:integer;maxdwn,dwn:Byte;ABitMap: STRING);
       PROCEDURE wmerasebkgnd(VAR msg:tmessage);    VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE WMkeydown(VAR Msg: TMessage);      VIRTUAL wm_First + wm_keydown;
       PROCEDURE drawpos(msg:tmessage);             VIRTUAL;
     END;

{************************************Bitmap-Fontobjekt********************************************************}
{An der Stelle x0,y0 wird im übergeordneten Fenster ein STRING in einem Bitmapfont (ASCII 32..95)
 mit ni zeichen und der Ausrichtung lrc (left, right, center-[0,1,sonst) ausgegeben}
TYPE PBMPFont = ^BMPFont;
     BMPFont=OBJECT(tstatic)
       w,h,n,m:integer;
       Event:Byte;
       BMP: hBitmap;
       disabled:boolean;
       CharsInFont:Word;
       bmptext,index:ARRAY[0..256] OF byte;
       membmp:hBitmap;
       CONSTRUCTOR Init(AParent: PWindowsObject; AnID,x0,y0,ni,lrc: Integer;ABitMap: STRING;MaxChInFont:Byte);
       PROCEDURE  SetupWindow;                  VIRTUAL;
       DESTRUCTOR Done;                         VIRTUAL;
       PROCEDURE TimerScroll(VAR msg:tmessage);       VIRTUAL wm_first+wm_idleaction;
       PROCEDURE wmerasebkgnd(VAR msg:tmessage);VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE bmppaint;
       PROCEDURE setbmptext(text:STRING);
       FUNCTION  gettext:STRING;
       PROCEDURE disable;
       PROCEDURE enable;
     END;
{************************************Bitmap-LCD-Zahl**********************************************************}
{An der Stelle x0,y0 wird im übergeordneten Fenster eine natürliche Zahl in einem Bitmapfont
 mit ni Stellen (max. 10!) und führender Null lz (oder auch nicht) ausgegeben}
TYPE PBMPLCD=^BMPLCD;
     BMPLCD=OBJECT(tstatic)
       w,h,n,m:integer;
       disabled:boolean;
       BMP: hBitmap;
       zahl:STRING[10];
       CONSTRUCTOR Init(AParent: PWindowsObject; AnID,x0,y0,ni,lz: Integer;ABitMap: STRING);
       DESTRUCTOR Done; VIRTUAL;
       PROCEDURE wmerasebkgnd(VAR msg:tmessage);VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE bmppaint;
       PROCEDURE setbmpzahl(z:longint);
       FUNCTION  getbmpzahl:longint;
       PROCEDURE disable;
       PROCEDURE enable;
     END;

{Objekt zum einstellen von Helligkeit, Kontrast und Farbe der tBuZApplication}
TYPE pPictureSetup=^tPictureSetup;
     tPictureSetup=OBJECT(tWindow)
       xmbri,ymbri:integer;
       xmcon,ymcon:integer;
       xmcol,ymcol:integer;
       radius:Integer;
       hDrehKnopp:hBitMap;
       bReset:pBMPCaptBtn;
       bDefault:pBMPCaptBtn;
       bDisable:pBMP3CaptBtn;
       pMySelfInParent:Pointer;
       CONSTRUCTOR INIT(pMySelf:Pointer);
       DESTRUCTOR DONE;                                   VIRTUAL;
       FUNCTION  GetClassName:PChar;                      VIRTUAL;
       PROCEDURE GetWindowClass(VAR WndClass: TWndClass); VIRTUAL;
       PROCEDURE DrawKnopp(aDC:hDC;Value,xmiddle,ymiddle:integer);
       PROCEDURE wmQueryNewPalette(VAR msg:tmessage);     VIRTUAL wm_first+wm_QueryNewPalette;
       PROCEDURE WMSetFocus(VAR MSG:tMessage);            VIRTUAL wm_first+wm_SetFocus;
       PROCEDURE WMEraseBkGnd(VAR MSG:tMessage);          VIRTUAL wm_first+wm_EraseBkGnd;
       PROCEDURE WMSetCursor(VAR MSG:tMessage);           VIRTUAL wm_first+wm_SetCursor;
       PROCEDURE WMKeyDown(VAR MSG:tMessage);             VIRTUAL wm_first+wm_KeyDown;
       PROCEDURE WMKeyUp(VAR MSG:tMessage);               VIRTUAL wm_first+wm_KeyUp;
       PROCEDURE HandleReset(VAR Msg:tMessage);           VIRTUAL id_first+100;
       PROCEDURE HandleDefault(VAR Msg:tMessage);         VIRTUAL id_first+101;
       PROCEDURE HandleDisable(VAR Msg:tMessage);         VIRTUAL id_first+102;
     END;

{*********************************Fileload-Mauscursor********************************************************}
TYPE pplane=^plane;
     plane=ARRAY[0..31] OF longint;

TYPE  pwaitcursor=^waitcursor;
      waitcursor=OBJECT
       hwindow:hwnd;
       Cursor,Cursor1,old:hcursor;
       andplane,xorplane:pplane;
       percnt:longint;
       CONSTRUCTOR init(hpwindow:hwnd);
       PROCEDURE setpercnt(x:longint);
       DESTRUCTOR done;
     END;
{***********************************Fortschritts-Fenster*****************************************************}
TYPE pwaitbox=^waitbox;
     waitbox=OBJECT(twindow)
       oldcursor:hcursor;
       prcnt:byte;
       title:STRING[81];
       xt,yt:integer;
       xp,yp,xb,yb,wb,hb:integer;
       w_wait,h_wait:integer;
       CONSTRUCTOR init(aparent:pwindowsobject;comment:STRING);
       PROCEDURE   setupwindow;                         VIRTUAL;
       DESTRUCTOR  done;                                VIRTUAL;
       PROCEDURE   wmQueryNewPalette(VAR msg:tmessage); VIRTUAL wm_first+wm_QueryNewPalette;
       PROCEDURE   wmerasebkgnd(VAR msg:tmessage);      VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE   wmclose(VAR msg:tmessage);           VIRTUAL wm_first+wm_close;
       PROCEDURE   paintprcnt(adc:hdc);
       PROCEDURE   setpercnt(i:byte);
     END;
FUNCTION initwaitbox(aparent:pwindowsobject;title:STRING):pwaitbox;
PROCEDURE setwaitbox(p:pwaitbox;i:real);
PROCEDURE closewaitbox(p:pwaitbox);

{**********************Static mit eigenem Font**************************************}
TYPE pmystatic=^tmystatic;
     tmystatic=OBJECT(tstatic)
      AssocWindow:Hwnd;
      retcode:word;
      PROCEDURE setupwindow;VIRTUAL;
      PROCEDURE setassocwindow(Awindow:Hwnd;sendcode:word);
      PROCEDURE wmlbuttondown(VAR msg:tmessage);  VIRTUAL wm_first+wm_lbuttondown;
      PROCEDURE wmnchittest(VAR msg:tmessage);    VIRTUAL wm_first+wm_nchittest;
      PROCEDURE setstringtext(astring:STRING);    VIRTUAL;
      FUNCTION  getstringtext:STRING;             VIRTUAL;
     END;
{************************Zentriertes STATIC*****************************************}
 TYPE pmycenterstatic=^tmycenterstatic;
      tmycenterstatic=OBJECT(tmystatic)
      CONSTRUCTOR init(Aparent:Pwindowsobject;AnID:Integer;ATitle:Pchar;x,y,w,h:integer;Atextlen:word);
      END;

{Validator Für Zahlen}
type pnumval=^tnumval;
     tnumval=object(tvalidator)
       parent:pwindowsobject;
       constructor init(aparent:pwindowsobject);
       procedure   error;virtual;
       function    IsValid(const S: string): Boolean; virtual;
       function    IsValidInput(var S: string;SuppressFill: Boolean ): Boolean;virtual;
     END;

{***********************EDIT mit eigenem FONT**************************************}
TYPE pmyedit=^tmyedit;
     tmyedit=OBJECT(tedit)
     focusname :ARRAY[0..150] OF char;
     hasownfont:boolean;
     CONSTRUCTOR init(Aparent:Pwindowsobject;anid:integer;atitle:pchar;
                      x,y,w,h:integer;atextlen:word;ownfont:boolean);
     PROCEDURE setupwindow;                       VIRTUAL;
     PROCEDURE wmkeydown(VAR msg:tmessage);       VIRTUAL wm_first+wm_keydown;
     PROCEDURE wmsetfocus(VAR msg:tmessage);      VIRTUAL wm_first+wm_setfocus;
     PROCEDURE wmchar(VAR msg:tmessage);          VIRTUAL wm_first+wm_char;
     PROCEDURE wmsyskeydown(VAR msg:tmessage);    VIRTUAL wm_first+wm_syskeydown;
     PROCEDURE setstringtext(astring:STRING);     VIRTUAL;
     END;
{*************EDIT KILL FOcUS THEN QUIT*********************************************}
TYPE pmykfqedit=^tmykfqedit;
     tmykfqedit=OBJECT(tmyedit)
     PROCEDURE wmkillfocus(VAR msg:tmessage);     VIRTUAL wm_first+wm_killfocus;
     END;
{***************EDIT mit GROß+KLEINSCHREIBUNG***********************************}
type pmyudedit=^tmyudedit;
     tmyudedit=object(tmyedit)
     CONSTRUCTOR init(Aparent:Pwindowsobject;anid:integer;atitle:pchar;
                      x,y,w,h:integer;atextlen:word;ownfont:boolean);
     END;

{Datenbank-EDIT}
type pdbedit=^tdbedit;
     tdbedit=object(tmyedit)
       CONSTRUCTOR init(Aparent:Pwindowsobject;anid:integer;atitle:pchar;
                        x,y,w,h:integer;atextlen:word;ownfont:boolean);
       PROCEDURE wmkeydown(VAR msg:tmessage);       VIRTUAL wm_first+wm_keydown;
       PROCEDURE wmsyskeydown(VAR msg:tmessage);    VIRTUAL wm_first+wm_syskeydown;
       PROCEDURE wmsetfocus(VAR msg:tmessage);      VIRTUAL wm_first+wm_setfocus;
       PROCEDURE WMKillFocus(VAR msg:tmessage);     VIRTUAL wm_first+wm_killfocus;
     END;
{Nummern-Edit}
type pnredit=^tnredit;
     tnredit=object(tdbedit)
       CONSTRUCTOR init(Aparent:Pwindowsobject;anid:integer;atitle:pchar;
                        x,y,w,h:integer;atextlen:word;ownfont:boolean);
       PROCEDURE wmkeydown(VAR msg:tmessage);       VIRTUAL wm_first+wm_keydown;
     END;

{********************COMBOBOX**************************************************}
type pmycombobox=^tmycombobox;
     tmycombobox=object(tcombobox)
       static:pmystatic;
       constructor init(APArent:Pwindowsobject;anid:integer;x,y,w,h:word;style:longint;atextlen:word);
       FUNCTION    addstring(astring:STRING):integer;VIRTUAL;
       FUNCTION    getstring(VAR astring:STRING;index:integer):integer;VIRTUAL;
       FUNCTION    setselstring(astring:STRING):integer;
       PROCEDURE   wmkeydown(VAR msg:tmessage);VIRTUAL wm_first+wm_keydown;
       PROCEDURE   WMSETFOCUS(VAR Msg: TMessage);      VIRTUAL wm_First + wm_setfocus;
       PROCEDURE   wmsyskeydown(VAR msg:tmessage);     VIRTUAL wm_first+wm_syskeydown;
       PROCEDURE   WMCTLCOLOR(VAR msg:tmessage);     VIRTUAL wm_first+wm_CTLCOLOR;
     END;

{********************************LISTBOX*********************************************}
type pmylistbox=^tmylistbox;
     tmylistbox=object(tlistbox)
       static:pmystatic;
       redraw:boolean;
       lastselcount:integer;
       lastcount:integer;
       candrag:boolean;
       clicked:boolean;
       lastindex:integer;
       dropcursor:HCursor;
       constructor init(APArent:Pwindowsobject;anid:integer;x,y,w,h:word;style:longint;drag:boolean);
       destructor done;Virtual;
       PROCEDURE   setassocstatic(astatic:pmystatic);
       FUNCTION    addstring(astring:STRING):integer;VIRTUAL;
       FUNCTION    insertstring(astring:Pchar;index:integer):integer;VIRTUAL;
       FUNCTION    deletestring(index:integer):integer;VIRTUAL;
       PROCEDURE   clearlist;VIRTUAL;
       FUNCTION    getstring(VAR astring:STRING;index:integer):integer;VIRTUAL;
       FUNCTION    getselstring(VAR astring:STRING;maxchars:integer):integer;VIRTUAL;
       FUNCTION    Findstring(AString:STRING):integer;
       FUNCTION    getcaretindex:integer;
       FUNCTION    setcaretindex(aindex:integer):integer;
       FUNCTION    getsel(aindex:integer):integer;
       FUNCTION    setsel(aindex:integer;onoff:byte):integer;
       FUNCTION    getitemdata(Aindex:integer):longint;
       FUNCTION    setitemdata(aindex:integer;wert:longint):integer;
       PROCEDURE   wmkeydown(VAR msg:tmessage);         VIRTUAL wm_first+wm_keydown;
       PROCEDURE   WMSETFOCUS(VAR Msg: TMessage);       VIRTUAL wm_First+wm_setfocus;
       PROCEDURE   wmsyskeydown(VAR msg:tmessage);      VIRTUAL wm_first+wm_syskeydown;
       PROCEDURE   wmsetredraw(VAR msg:tmessage);       VIRTUAL wm_first+wm_setredraw;
       PROCEDURE   setstatictext;
       PROCEDURE   wmmousemove(VAR msg:tmessage);       VIRTUAL wm_first+wm_mousemove;
       PROCEDURE   wmlbuttondown(VAR msg:tmessage);     VIRTUAL wm_first+wm_lbuttondown;
       PROCEDURE   wmlbuttonup(VAR msg:tmessage);       VIRTUAL wm_first+wm_lbuttonup;
       PROCEDURE   wmrbuttondown(VAR msg:tmessage);     VIRTUAL wm_first+wm_rbuttondown;
       PROCEDURE   wmrbuttonup(VAR msg:tmessage);       VIRTUAL wm_first+wm_rbuttonup;
       PROCEDURE   wmCanDrag(VAR msg:tmessage);         VIRTUAL wm_first+wm_CanDrag;
       PROCEDURE   wmSysTimer(VAR msg:tmessage);        VIRTUAL wm_first+wm_SysTimer;
     END;

{WINDOW für SYSMENU************************}
TYPE
psyswindow=^tsyswindow;
tsyswindow=OBJECT(twindow)
  topmost:byte;
  hsysmenu:hmenu;
  style:longint;
  CONSTRUCTOR init(Aparent:pwindowsobject;w,h:integer;astyle:longint;_topmost:byte);
  PROCEDURE SetupWindow;VIRTUAL;
  PROCEDURE wmsyscommand(VAR msg:tmessage);          VIRTUAL wm_first+wm_syscommand;
  PROCEDURE wminitmenu(VAR msg:tmessage);            VIRTUAL wm_first+wm_initmenu;
  PROCEDURE wmncpaint(VAR msg:tmessage);             VIRTUAL wm_first+wm_ncpaint;
  PROCEDURE draw(Up:Boolean);
END;
{Hauptfenster mit eigenem SYSMENU+CAPTION}
TYPE pmywindow=^tmywindow;
     tmywindow=OBJECT(twindow)
       oldfocus:hwnd;
       syswindow:psyswindow;
       bClose,bMini,bMaxi:pbmpsysbutton;
       iswindowactive,MaxiMized:boolean;
       CapDY:integer;
       pcrect:trect;
       CONSTRUCTOR init(AParent:Pwindowsobject;atitle:Pchar;astyle:longint;topmost:byte);
       DESTRUCTOR DONE;VIRTUAL;
       FUNCTION  GetClassName:PChar;                      VIRTUAL;
       PROCEDURE GetWindowClass(VAR WndClass: TWndClass); VIRTUAL;
       PROCEDURE setupwindow;VIRTUAL;
       PROCEDURE wmQueryNewPalette(VAR msg:tmessage);     VIRTUAL wm_first+wm_QueryNewPalette;
       PROCEDURE wmActivateApp(VAR msg:tmessage);         VIRTUAL wm_first+wm_ActivateApp;
       PROCEDURE wmsetfocus(VAR msg:tmessage);            VIRTUAL wm_first+wm_setfocus;
       PROCEDURE wmsize(VAR msg:tmessage);                VIRTUAL wm_first+wm_size;
       PROCEDURE wmIHaveTheFocus(VAR msg:tmessage);       VIRTUAL wm_first+wm_Ihavethefocus;
       PROCEDURE wmactivate(VAR msg:tmessage);            VIRTUAL wm_first+wm_activate;
       PROCEDURE wmsyscommand(VAR msg:tmessage);          VIRTUAL wm_first+wm_syscommand;
       PROCEDURE wmsyschar(VAR msg:tmessage);             VIRTUAL wm_first+wm_syschar;
       PROCEDURE WMLButtonDown(VAR Msg: TMessage);        VIRTUAL wm_First+wm_LButtonDown;
       PROCEDURE wmmeasureitem(VAR msg:tmessage);         VIRTUAL wm_first+wm_measureitem;
       PROCEDURE wmctlcolor(VAR msg:tmessage);            VIRTUAL wm_first+wm_ctlcolor;
       PROCEDURE WMEraseBkGnd(VAR msg:tmessage);          VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE WMsettext(VAR Msg: TMessage);            VIRTUAL wm_First+wm_settext;
       PROCEDURE WMgettext(VAR Msg: TMessage);            VIRTUAL wm_First+wm_gettext;
       FUNCTION  idleaction:Boolean;                      VIRTUAL;
       PROCEDURE paintcaption(adc:hdc);                   VIRTUAL;
       PROCEDURE paintcaptiontext(adc:hdc;Arect:trect);   VIRTUAL;
       FUNCTION  TimerProc(VAR msg:tmessage):boolean;     VIRTUAL wm_first+wm_timer;
     END;
TYPE pmychildwindow=^tmychildwindow;
     tmychildwindow=OBJECT(tmywindow)
       FUNCTION  GetClassName:PChar;                      VIRTUAL;
       PROCEDURE GetWindowClass(VAR WndClass: TWndClass); VIRTUAL;
     END;

{************************Die SWE BUZ APPLICATION*****************************!!!!!!!!!}
CONST MaxEvents=32;
TYPE pbuzapplication=^tbuzapplication;
     tbuzapplication=OBJECT(Tapplication)
       Bright2Slide2 :integer;
       HSysKeyWnd    :hWnd;
       fontfilehandle:longint; {Handle der Font - DLL}
       fontfile      :FILE;    {PASCAL-FILE des FONTS}
       sysbmp        :hbitmap;
       Events        :ARRAY[1..MaxEvents]OF RECORD
                        Window:hWnd;
                        Msg:tMessage;
                        LastTime:Longint;
                      END;
       NumEvents     :Byte;
       MMTimer       :Integer;
       PROCEDURE  initmainwindow;VIRTUAL;
       DESTRUCTOR done;VIRTUAL;
       PROCEDURE  error(errorcode:integer);VIRTUAL;
       PROCEDURE  MessageLoop; VIRTUAL;
       {Verschickt nach MSG.lParam Millisekunden eine MSG.Message mit MSG.wParam and Window}
       FUNCTION   AddEvent(Window:hWnd;MSG:TMessage):Byte;
       PROCEDURE  RemoveEvent(What:Byte);
       PROCEDURE  LoadFonts;
       PROCEDURE  ReleaseFonts;
       FUNCTION   idleaction:boolean;VIRTUAL;
       PROCEDURE  _TimerProc(hWnd:hWnd;Msg,idTimer:Word;dwTime:Longint);VIRTUAL;
       PROCEDURE  _MMTimerproc(wTimerID, wMsg:word; dwUser, dw1, dw2:longint);VIRTUAL;
     END;

PROCEDURE KeepWindowsAlive;
FUNCTION SetWindowRegion(MyWindow:pMyWindow;BitmapName:pChar):Bool;

IMPLEMENTATION

PROCEDURE KeepWindowsAlive;
VAR  wmsg:tmsg;
BEGIN
  WHILE PeekMessage(wmsg,0,0,0,pm_remove) DO
  BEGIN
    CASE wMSG.Message OF
      wm_keydown..wm_deadchar,wm_command:;
      wm_mousefirst..wm_mouselast:;
      ELSE BEGIN
        TranslateMessage(wMsg);
        DispatchMessage(wMsg);
      END;
    END;
  END;
END;
{************HOTKEY-CHECK***************************************************}
FUNCTION iskey(wert:word):word;
VAR i,KS:Word;
BEGIN
  Ks:=KeyState;
  IsKey:=0;
  FOR i:=1 TO MAINHOTKEYS_BIS DO WITH MyKeys[i] DO IF(wert=key)AND(ks=state)THEN BEGIN IsKey:=i;Exit;END;
END;

FUNCTION isvalidkey(wert:word):boolean;
VAR i:Byte;
BEGIN
  IF Wert IN [$2a,$BA..$C0,$DB..$E4,$E6..$F5]THEN isvalidkey:=true ELSE
  IF Wert IN [$1..$7,$A..$C,$E,$F,$15..$1a,$1c..$1f,$29,$2B..$2c,$3a..$40,$5b..$5f,$80..$8F,$92..$FF] THEN isvalidkey:=false
  ELSE isvalidkey:=true;
END;

{************************Die SWE BUZ APPLICATION*****************************!!!!!!!!!}

PROCEDURE tbuzapplication._TimerProc(hWnd:hWnd;Msg,idTimer:Word;dwTime:Longint);
BEGIN
  IF MainWindow<>NIL THEN PostMessage(MainWindow^.hWindow,wm_Timer,0,0);
END;

PROCEDURE tbuzapplication._MMTimerproc(wTimerID, wMsg:word; dwUser, dw1, dw2:longint);
VAR Msg:tMSG;
BEGIN
  {IF NOT PeekMessage(Msg,dwUser,wm_Timer,wm_Timer,PM_NoYield OR PM_NoRemove)
  THEN{} PostMessage(dwUser,wm_Timer,0,0);
  MMTimer:=0;
END;

FUNCTION tbuzapplication.idleaction;
VAR IdleTime:Longint;
    i:Byte;
    DC:Word;
BEGIN
  IdleTime:=TimeGetTime;
  IF NumEvents>0 THEN
  FOR i:=1 TO MaxEvents DO WITH Events[i] DO
  IF(Window<>0)AND(IdleTime-LastTime>Msg.lParam)THEN
  BEGIN
    LastTime:=IdleTime;
    IF window<>1 THEN SendMessage(Window,msg.message,msg.wparam,msg.lparam) ELSE
    BEGIN
      {Fenster langsam aufblenden...}
      DC:=GetDC(GetActiveWindow);
      IF DC<>0 THEN
      BEGIN
        Inc(Brightness,8);
        IF Brightness>Bright2Slide2 THEN
        BEGIN
          Brightness:=Bright2Slide2;
          Bright2Slide2:=0;
          RemoveEvent(i);
        END;
        DoBrightNessContrastColor(DC);
        ReleaseDC(GetActiveWindow,DC);
      END;
    END;
  END;
  Idleaction:=(MainWindow<>NIL)AND(pMywindow(mainwindow)^.idleaction);
END;

FUNCTION TbuzApplication.AddEvent(Window:hWnd;MSG:TMessage):Byte;
VAR i:Byte;
BEGIN
  IF NumEvents<MaxEvents THEN
  BEGIN
    Inc(NumEvents);
    FOR i:=1 TO MaxEvents DO IF Events[i].Window=0 THEN
    BEGIN
      Events[i].Window:=Window;
      Events[i].Msg:=Msg;
      Events[i].LastTime:=TimeGetTime;
      AddEvent:=i;
      Exit;
    END;
  END ELSE AddEvent:=0;
END;

PROCEDURE TbuzApplication.RemoveEvent(What:Byte);
BEGIN
  IF What<MaxEvents THEN
  BEGIN
    Events[What].Window:=0;
    Dec(NumEvents);
  END;
END;

PROCEDURE TbuzApplication.MessageLoop;
VAR
  Message   : TMsg;
  IsDone    : Boolean;
  MainTimer : Word;
  MMTProc   : Pointer;
LABEL L1;
BEGIN
  IsDone     := False;
  MainTimer  := 0;
  MMTimer    := 0;
  HSysKeyWnd := mainwindow^.hwindow;
  MainTimer  := SetTimer(0,0,125,TFarProc(MakeObjProcInstance(@TbuzApplication._TimerProc,@Self)));
  MMTProc    := MakeObjProcInstance(@TbuzApplication._MMTimerProc,@Self);
  TimeBeginPeriod(1);
  REPEAT
    IF PeekMessage(Message,0,0,0,pm_Remove) THEN
    BEGIN
      IF Message.Message=wm_Quit THEN IsDone:=True ELSE
      IF(Message.message=wm_Syskeydown)THEN
      BEGIN
        IF sendmessage(HSysKeyWnd,wm_KeyDown,Message.wparam,Message.lparam)<>0
        THEN GOTO L1;
      END ELSE
      IF (Message.message=wm_Syskeyup)THEN
      BEGIN
        IF sendmessage(HSysKeyWnd,wm_Keyup,Message.wparam,Message.lparam)<>0
        THEN GOTO L1;
      END ELSE
      BEGIN
L1:     TranslateMessage(Message);
        DispatchMessage(Message);
        IF (message.message=wm_timer)
        OR (message.message=wm_enteridle)
        THEN IdleAction;
      END;
    END ELSE
    BEGIN
      IF NOT IdleAction THEN
      BEGIN
        IF MMTimer<>0 THEN TimeKillEvent(MMTimer);
        MMTimer:=0;
      END ELSE IF MMTimer=0 THEN MMTimer:=TimeSetEvent(20,20,TTimeCallBack(MMTProc),MainWindow^.hWindow,Time_OneShot);
      WaitMessage;
    END;
  UNTIL IsDone;
  Status := Message.WParam;
  IF (MainTimer<>0) THEN BEGIN KillTimer(0,MainTimer);MainTimer:=0; END;
  TimeEndPeriod(1);
  FreeObjProcInstances(@Self);
END;

PROCEDURE tbuzapplication.error;
BEGIN
 IF ourmessagebox(mainwindow,errorver,getresstring(5000),mb_iconstop or mb_yesno)=id_yes
 THEN BEGIN Done;halt(0);END;
END;


PROCEDURE tbuzapplication.LoadFonts;
VAR font        :tlogfont;
    fontfilename:ARRAY[0..150] OF char;
    fontfindhandle,fontloadhandle:thandle;
    fontpointer :pointer;
BEGIN
  IF FontLoaded THEN ReleaseFonts;
  IF GlobalFindAtom('SWEBUZisHere')=0 THEN
  BEGIN
    {FONT-DATEN werden aus der EXE geholt und dem SYSTEM hinzugefügt}
    fontfindhandle:=findresource(hinstance,makeintresource(30),'FONTDLL');
    fontloadhandle:=loadresource(hinstance,fontfindhandle);
    fontpointer:=lockresource(fontloadhandle);
    gettempfilename(gettempdrive('c'),'m4w',0,fontfilename);
    BYTE(fontfilename[strlen(fontfilename)-3]):=0;
    StrCat(fontfilename,'FON');
    {$I-}
    filemode:=2;
    inoutres:=0;
    assign(fontfile,fontfilename);
    rewrite(fontfile,1);
    blockwrite(fontfile,fontpointer^,sizeofresource(hinstance,fontfindhandle));
    close(fontfile);
    unlockresource(fontloadhandle);
    freeresource(fontloadhandle);
    setfattr(fontfile,fahidden+fasysfile+fareadonly+faarchive);
    IF addfontresource(fontfilename)<>0 THEN sendmessage($FFFF,wm_fontchange,0,0);
    fontfilehandle:=getmodulehandle(fontfilename);
    WriteProfileString('SWEBUZengine','FontName',@fontfilename);
  END;
  GlobalAddAtom('SWEBUZisHere');
  WITH font DO BEGIN
    lfheight:=12;
    lfwidth:=8;
    lfescapement:=0;
    lforientation:=0;
    lfweight:=400;
    lfitalic:=0;
    lfunderline:=0;
    lfstrikeout:=0;
    lfoutprecision :=0;
    lfclipprecision:=0;
    lfquality:=0;
    lfpitchandfamily:=3;
  END;
  IF WinLanguage=_GRK THEN
  BEGIN
    font.lfcharset:=161;
    font.lfpitchandfamily:=0;
    strpcopy(@font.lffacename,'SWEBUZGREEKOEM'+#0);  OEMhandle:=createfontindirect(font);
    font.lfcharset:=0;
    strpcopy(@font.lffacename,'SWEBUZGREEKANSI'+#0); AnsiHandle:=createfontindirect(font);
  END ELSE
  IF WinLanguage=_CZE THEN
  BEGIN
    font.lfcharset:=162;
    font.lfpitchandfamily:=0;
    strpcopy(@font.lffacename,'SWEBUZCZECHOEM'+#0);  OEMhandle:=createfontindirect(font);
    font.lfcharset:=0;
    strpcopy(@font.lffacename,'SWEBUZCZECHANSI'+#0); AnsiHandle:=createfontindirect(font);
  END ELSE
  BEGIN
    font.lfcharset:=255;
    strpcopy(@font.lffacename,'SWEBUZOEM'+#0);  OEMhandle:=createfontindirect(font);
    font.lfcharset:=0;
    strpcopy(@font.lffacename,'SWEBUZANSI'+#0); AnsiHandle:=createfontindirect(font);
  END;
  Fw:=8;Fh:=12;
  FontLoaded:=True;
END;

PROCEDURE tbuzapplication.ReleaseFonts;
VAR fontfilename:ARRAY[0..150] OF char;
BEGIN
  IF NOT FontLoaded THEN Exit;
  deleteobject(oemhandle);
  deleteobject(ansihandle);
  FontLoaded:=False;
  GlobalDeleteAtom(GlobalFindAtom('SWEBUZisHere'));
  IF GlobalFindAtom('SWEBUZisHere')=0 THEN
  BEGIN
    GetProfileString('SWEBUZengine','FontName','m4w',@fontfilename,150);
    WriteProfileString('SWEBUZengine',NIL,NIL);
    IF {NOT} removefontresource(@fontfilename)
    THEN {MessageBox(0,errorver,'Warning!'+#10+#10'Could not remove font.',0) ELSE}
    BEGIN
      sendmessage($FFFF,wm_fontchange,0,0);
      fontfilehandle:=getmodulehandle(fontfilename);
      if fontfilehandle <> 0 then
      BEGIN
        WHILE GetModuleUsage(fontfilehandle)>1 DO freelibrary(fontfilehandle);
        freelibrary(fontfilehandle);
      END;
      {$i-}
      assign(fontfile,fontfilename);
      setfattr(fontfile,faarchive);
      erase(fontfile);
    END;
  END;
END;

PROCEDURE tbuzapplication.initmainwindow;
VAR msg:tMessage;
    PC:ARRAY[0..15] OF char;
    i:Integer;
BEGIN
  MainWindow:=NIL;
  NumEvents:=0;
  FillChar(Events,SizeOf(Events),0);
  pBuzApplication(Application)^.addevent(0,msg);
  IF ExOs=Ex_NewWin THEN
  BEGIN
    sysbmp:=LoadBitmap(hinstance,'sysbuttons_w95');
    SysBMPOfsX:=2;
    SysBMPOfsY:=2;
    SysBMPW:=16;
    SysBMPH:=14;
  END ELSE
  BEGIN
    sysbmp:=LoadBitmap(hinstance,'sysbuttons');
    SysBMPOfsX:=0;
    SysBMPOfsY:=0;
    SysBMPW:=19;
    SysBMPH:=18;
  END;
  LoadFonts;
  {FullDrag-Feature}
  FullMove:=getprivateprofileint(s_state,'FullWindowMove',0,INIDIR)<>0;
  {FarbTabelle initialisieren}
  Brightness:=0;
  Bright2Slide2:=GetPrivateProfileInt(s_state,'Brightness',100,INIDIR);
  IF(Bright2Slide2<0)OR(Bright2Slide2>200)THEN Bright2Slide2:=100;
  Contrast:=GetPrivateProfileInt(s_state,'Contrast',100,INIDIR);
  IF(Contrast<0)OR(Contrast>200)THEN Contrast:=100;
  ColorDepth:=GetPrivateProfileInt(s_state,'ColorDepth',100,INIDIR);
  IF(ColorDepth<0)OR(ColorDepth>200)THEN ColorDepth:=100;
  UsePalette:=TryToUsePalette;
  IF PaletteInstalled THEN
  BEGIN
    ColorsUsed:=0;
    FOR i:=0 TO 255 DO WITH RealColorTable[i] DO
     BEGIN peRed:=i;peGreen:=i;peBlue:=i;peFlags:=pc_Reserved; END;
    hRefPal:=CreatePalette(Palette);
    hPal:=CreatePalette(Palette);
    UnrealizeObject(hPal);
    UnrealizeObject(hRefPal);
  END;
  IF TryToUsePalette THEN UsePalette:=getprivateprofileint(s_state,'UseFullPalette',1,INIDIR)<>0;
  IF PaletteInstalled THEN
  BEGIN
  {Fenster langsam aufblenden...}
    msg.wparam:=0;
    msg.lParam:=55;
    msg.message:=0;
    addevent(1,msg);
  END ELSE Brightness:=Bright2Slide2;
  FOR i:=0 TO NumSysColors-1 DO
  BEGIN
    LoadString(hInstance,30001+i,@PC,7);
    ASM
      {Convert String to Longint}
      mov cx,6
      xor si,si
      DB 66h; xor bx,bx
      mov bx,02h
      DB 66h; xor ax,ax
     @loop:
      DB 66h; shl bx,4
      mov al,PC.byte.[si]
      sub al,30h
      cmp al,0Fh
      jbe  @0
      sub al,07h
      cmp al,0Fh
      jbe  @0
      sub al,20h
     @0:
      DB 66h; add bx,ax
      inc si
      loop @loop
      {Include Color in Colortable}
      DB 66h; push bx
      call includecolor
      mov di,i
      shl di,2
      mov word ptr cUp.byte.[di],ax
      mov word ptr cUp.byte.[di+2],dx
    END;
  END;
  BrEdit      :=CreateSolidBrush(cBrEdit);
  BrStatic    :=CreateSolidBrush(cBrStatic);
  BrInvStatic :=CreateSolidBrush(cBrInvStatic);
  BrListBox   :=CreateSolidBrush(cBrLB);
  BrGrayBox   :=CreateSolidBrush(cBrGrayBox);
  BrScrollbar :=CreateSolidBrush(cBrScrollbar);
  BrBlack     :=CreateSolidBrush(cBlack);
END;

DESTRUCTOR tbuzapplication.done;
VAR s:STRING[150];
BEGIN
  IF mainwindow<>NIL THEN mainwindow^.done;
  DeleteMyBitmap(sysbmp);
  IF PaletteInstalled THEN
  BEGIN
    UnrealizeObject(hPal);
    UnrealizeObject(hRefPal);
    IF hPal<>0 THEN deleteobject(hPal);
    IF hRefPal<>0 THEN deleteobject(hRefPal);
  END;
  deleteobject(BrEdit);
  deleteobject(BrStatic);
  deleteobject(BrInvStatic);
  deleteobject(BrListBox);
  deleteobject(BrGrayBox);
  deleteobject(BrScrollbar);
  deleteobject(BrBlack);
  ReleaseFonts;
{ str(Byte(FullMove),s); writeinistring(s_state,'FullWindowMove'+#0,s); }
  IF TryToUsePalette THEN BEGIN str(Byte(UsePalette),s); writeinistring('State','UseFullPalette'+#0,s); END;
  IF PaletteInstalled THEN
  BEGIN
    IF Brightness<Bright2Slide2 THEN Brightness:=Bright2Slide2;
    str(Brightness,s); writeinistring('State','Brightness'+#0,s);
    str(Contrast,s); writeinistring('State','Contrast'+#0,s);
    str(ColorDepth,s); writeinistring('State','ColorDepth'+#0,s);
  END;
  INHERITED done;
END;

{*****************SYSMENU-CHILD ***************we are faking Windoofs***********}
CONSTRUCTOR tsyswindow.init;
BEGIN
  INHERITED init(Aparent,'');
  topmost:=_topmost;
  style:=astyle;
  attr.x:=SysBMPOfsX-1;
  attr.y:=SysBMPOfsX-1;
  attr.w:=w+2;
  attr.h:=h+2;
  attr.style:=ws_child OR ws_visible OR ws_caption OR ws_sysmenu;
  attr.exstyle:=ws_ex_noparentnotify;
END;

PROCEDURE tsyswindow.setupwindow;
VAR s:STRING[50];
BEGIN
  INHERITED setupwindow;
  hsysmenu:=getsystemmenu(hwindow,false);
  IF (topmost>0) THEN
  BEGIN
    Appendmenu(hsysmenu,mf_separator,3333,NIL);
    s:=getresstring(5001)+#0;
    appendmenu(hsysmenu,mf_string or mf_unchecked or mf_enabled,id_topmost,@s[1]);
    IF topmost=istopmost THEN
    with parent^ do setwindowpos(hwindow,hwnd_topmost,attr.x,attr.y,attr.w,attr.h,swp_showwindow);
  END;
END;

PROCEDURE  tsyswindow.wmncpaint;BEGIN  msg.result:=0;END;

PROCEDURE tsyswindow.draw(Up:Boolean);
VAR DC,MemDC:hDC;
    Brush:hBrush;
    rect:tRect;
BEGIN
  IF Parent^.hWindow=0 THEN Exit;
  DC:=GetDC(Parent^.hWindow);
  memdc:=createcompatibledc(dc);
  selectobject(memdc,pBUZApplication(Application)^.sysbmp);
  IF pMyWindow(parent)^.iswindowactive
  THEN brush:=createsolidbrush(getsyscolor(color_activecaption))
  ELSE brush:=createsolidbrush(getsyscolor(color_inactivecaption));
  WITH rect DO BEGIN left:=0;right:=20;top:=0;bottom:=18;END;
  fillrect(DC,rect,brush);
  deleteobject(brush);
  bitblt(dc,SysBMPOfsX,SysBMPOfsY,SysBMPW,SysBMPH,memdc,0,(1-Byte(Up))*SysBMPH,srccopy);
  deletedc(memdc);
  ReleaseDC(Parent^.hWindow,DC);
END;

PROCEDURE tsyswindow.wmsyscommand;
BEGIN
  CASE (msg.wparam AND $FFF0 )OF
  sc_keymenu,sc_mousemenu:
  BEGIN
    settimer(parent^.hwindow,-1,100,NIL);
    defwndproc(msg);
    KillTimer(parent^.hwindow,-1);
    Draw(True);
  END;
  ELSE IF msg.wparam=id_topmost THEN
  BEGIN
    IF (topmost AND istopmost)=istopmost
    THEN with parent^ do setwindowpos(hwindow,hwnd_notopmost,attr.x,attr.y,attr.w,attr.h,swp_showwindow)
    ELSE with parent^ do setwindowpos(hwindow,hwnd_topmost,attr.x,attr.y,attr.w,attr.h,swp_showwindow);
    topmost:=topmost xor istopmost;
  END ELSE postmessage(parent^.hwindow,wm_syscommand,msg.wparam,msg.lparam);
  msg.result:=0;
  END;
END;

{Initialisieren der SystemMenu-Einträge entsprechend des Fensterstils}
PROCEDURE tsyswindow.wminitmenu;
BEGIN
  sendmessage(parent^.hwindow,msg.message,msg.wparam,msg.lparam);
  IF(topmost AND CanBeTopmost)=CanBeTopmost
  THEN enablemenuitem(msg.wparam ,id_topmost,mf_enabled or mf_bycommand)
  ELSE enablemenuitem(msg.wparam ,id_topmost,mf_grayed or mf_bycommand);
  IF((TOPmost AND IsTopMost)=IsTopMost)AND((topmost AND CanBeTopmost)=CanBeTopmost)
  THEN checkmenuitem(msg.wparam ,id_topmost,mf_checked or mf_bycommand)
  ELSE checkmenuitem(msg.wparam ,id_topmost,mf_unchecked or mf_bycommand);
  enablemenuitem(msg.wparam,sc_close,mf_bycommand OR mf_enabled);
  enablemenuitem(msg.wparam,sc_restore,mf_bycommand OR mf_enabled);
  enablemenuitem(msg.wparam,sc_move,mf_bycommand OR mf_enabled);
  enablemenuitem(msg.wparam,sc_size ,mf_bycommand OR mf_grayed);
  IF (style AND ws_minimizebox)=ws_minimizebox
   THEN enablemenuitem(msg.wparam,sc_minimize ,mf_bycommand OR mf_enabled)
   ELSE enablemenuitem(msg.wparam,sc_minimize ,mf_bycommand OR mf_grayed);
  IF (style AND ws_maximizebox)=ws_maximizebox
   THEN enablemenuitem(msg.wparam,sc_maximize ,mf_bycommand OR mf_enabled)
   ELSE enablemenuitem(msg.wparam,sc_maximize ,mf_bycommand OR mf_grayed);
  IF isiconic(parent^.hwindow) THEN
  BEGIN
    enablemenuitem(msg.wparam,sc_maximize,mf_bycommand OR mf_grayed);
    enablemenuitem(msg.wparam,sc_minimize ,mf_bycommand OR mf_grayed);
    enablemenuitem(msg.wparam,sc_move,mf_bycommand OR mf_grayed);
  END ELSE
  IF pMyWindow(parent)^.Maximized THEN
  BEGIN
    enablemenuitem(msg.wparam,sc_maximize,mf_bycommand OR mf_grayed);
  END ELSE
  BEGIN
    IF (style AND ws_thickframe)=ws_thickframe
    THEN enablemenuitem(msg.wparam,sc_size ,mf_bycommand OR mf_enabled);
    enablemenuitem(msg.wparam,sc_restore,mf_bycommand OR mf_grayed)
  END;
  Draw(False);
END;

{******************FENSTER MIT EIG. CAPTION**************************************}
CONSTRUCTOR tmywindow.init;
BEGIN
   CapDY:=18;
   MaxiMized:=((astyle AND ws_maximize)=ws_maximize);
   INHERITED init(Aparent,atitle);
   IF (astyle AND ws_sysmenu)=ws_sysmenu THEN
   BEGIN
     attr.exstyle:=ws_ex_noparentnotify;
     syswindow:=new(psyswindow,init(@self,SysBMPW,SysBMPH,astyle,topmost));
     iswindowactive:=true;
     IF ExOs=Ex_NewWin
     THEN bclose:=New (PbmpSysButton, Init(@self,sc_close,0,0,SysBMPW,SysBMPH,4*SysBMPW))
     ELSE bClose:=NIL;
     IF (astyle AND ws_minimizebox)=ws_minimizebox THEN
     bmini:=New (PbmpSysButton, Init(@self,sc_minimize,0,0,SysBMPW,SysBMPH,SysBMPW)) ELSE bmini:=NIL;
     IF (astyle AND ws_maximizebox)=ws_maximizebox THEN
     bmaxi:=New (PbmpSysButton, Init(@self,sc_maximize,0,0,SysBMPW,SysBMPH,0)) ELSE bmaxi:=NIL;
   END ELSE syswindow:=NIL;
END;

DESTRUCTOR tMyWindow.Done;
BEGIN
  StrDispose(Attr.Title);Attr.title:=NIL;
  INHERITED DONE;
  IF Parent=NIL THEN Application^.MainWindow:=NIL;
END;

PROCEDURE tmywindow.setupwindow;
VAR OButton:Integer;
BEGIN
  tWindow.setupwindow;
  OButton:=attr.w-CapDY-3;
  WITH pcrect DO BEGIN top:=3; bottom:=CapDY; left:=22; right:=attr.w-5; END;
  IF bClose<>NIL THEN
  BEGIN
    movewindow(bClose^.hwindow,OButton,SysBMPOfsY,bClose^.attr.w,bClose^.attr.h,True);
    bClose^.enable;
    pcrect.right:=OButton-2;
    dec(OButton,bClose^.attr.w+2);
    bClose^.enable;
  END;
  IF bMaxi<>NIL THEN
  BEGIN
    movewindow(bMaxi^.hwindow,OButton,SysBMPOfsY,bMaxi^.attr.w,bMaxi^.attr.h,True);
    IF MaxiMized THEN bMaxi^.Ofst:=3*SysBMPW ELSE bMaxi^.Ofst:=2*SysBMPW;
    pcrect.right:=OButton-2;
    dec(OButton,bMaxi^.attr.w);
    bMaxi^.enable;
  END;
  IF bMini<>NIL THEN
  BEGIN
    movewindow(bMini^.hwindow,OButton,SysBMPOfsY,bMini^.attr.w,bMini^.attr.h,True);
    pcrect.right:=OButton-2;
    dec(OButton,bMini^.attr.w);
    bMini^.enable;
  END;
END;

FUNCTION  tmywindow.GetClassName:PChar;
BEGIN
  GetClassName:='SWEBUZWindow';
END;

PROCEDURE tmywindow.GetWindowClass(VAR WndClass: TWndClass);
BEGIN
  TWindow.GetWindowClass(WndClass);
  strcopy(WndClass.lpszclassname,GetClassName);
  WndClass.HIcon :=LoadIcon(hinstance, MakeIntResource(100));
  wndclass.style:=cs_bytealignclient OR cs_bytealignwindow OR cs_dblclks;
  wndclass.HBrBackGround:=0;
END;

PROCEDURE tmywindow.wmActivateApp;
BEGIN
  IF Bool(msg.wparam)AND(hPal<>0)THEN UnrealizeObject(hPal);
END;

PROCEDURE tmywindow.wmQueryNewPalette;
VAR DC:hDC;
BEGIN
  IF (PaletteInstalled)AND(hPal<>0) THEN
  BEGIN
    DC:=GetDC(GetActiveWindow);
    IF DC>0 THEN
    BEGIN
      msg.result:=1;
      SelectPalette(DC,hPal,false);
      RealizePalette(DC);
      ReleaseDC(GetActiveWindow,DC);
    END;
  END ELSE msg.result:=0;
END;

{Malt CaptionText}
PROCEDURE tmywindow.paintcaption;
VAR pdc,memdc:hdc;
    rect:trect;
BEGIN
  IF hWindow=0 THEN Exit;
  IF adc=0 THEN pdc:=getdc(hwindow) ELSE pdc:=adc;
  WITH rect DO BEGIN left:=0;right:=attr.w;top:=0;bottom:=18;END;
  paintcaptiontext(pdc,rect);
  IF adc=0 THEN ReleaseDC(hwindow,pdc);
  IF syswindow<>NIL THEN SysWindow^.Draw(True);
  IF bClose<>NIL THEN bClose^.Enable;
  IF bmini<>NIL THEN bmini^.enable;
  IF bmaxi<>NIL THEN bmaxi^.enable;
END;

PROCEDURE tmywindow.paintcaptiontext;
VAR  oldfont:thandle;
     brush:hbrush;
     Rect32:tRect32;
BEGIN
  IF OS=Windows_95 THEN
  BEGIN
    Rect32.Left  :=aRect.Left;
    Rect32.Right :=aRect.Right;
    Rect32.Top   :=aRect.Top;
    Rect32.Bottom:=aRect.Bottom;
    IF iswindowactive
    THEN DrawCaption(0,aDC,@Rect32,DC_ACTIVE OR DC_TEXT OR DC_GRADIENT)
    ELSE DrawCaption(0,aDC,@Rect32,DC_TEXT OR DC_GRADIENT);
  END ELSE
  BEGIN
    IF iswindowactive
    THEN brush:=createsolidbrush(getsyscolor(color_activecaption))
    ELSE brush:=createsolidbrush(getsyscolor(color_inactivecaption));
    fillrect(adc,Arect,brush);
    deleteobject(brush);
  END;
  oldfont:=selectobject(adc,ANSIhandle);
  setbkmode(adc,transparent);
  IF bClose<>NIL
  THEN settextalign(adc,ta_top OR ta_left)
  ELSE settextalign(adc,ta_top OR ta_center);
  IF iswindowactive
  THEN  settextcolor(adc,getsyscolor(color_captiontext))
  ELSE  settextcolor(adc,getsyscolor(color_inactivecaptiontext));
  IF (Attr.title<>NIL) THEN
  IF bClose<>NIL
  THEN exttextout(adc,pcrect.left,3,eto_clipped,@pcrect,attr.title,strlen(attr.title),NIL)
  ELSE exttextout(adc,(pcrect.right+pcrect.left) DIV 2,3,eto_clipped,@pcrect,attr.title,strlen(attr.title),NIL);
  selectobject(adc,oldfont);
END;

PROCEDURE tmywindow.WMsettext;
VAR dc:hdc;
    wasactive:boolean;
BEGIN
  WITH Msg DO Result:=DefWindowProc(Receiver,Message,wParam,lParam);
  IF isiconic(hwindow) THEN
  BEGIN
    wasactive:=getactivewindow=hwindow;
    show(sw_hide);
    show(sw_showminimized);
    IF wasactive THEN setactivewindow(hwindow);
  END ELSE
  BEGIN
    IF hWindow=0 THEN Exit;
    dc:=getdc(hwindow);
    paintcaptiontext(dc,pcrect);
    ReleaseDC(hwindow,dc);
  END;
END;
PROCEDURE tmywindow.WMGETtext;
BEGIN
  IF attr.title<>NIL THEN
  BEGIN
    move(attr.title^,pointer(msg.lparam)^,strlen(attr.title)+1);
    msg.result:=strlen(attr.title);
  END ELSE msg.result:=0;
END;

{Malt CaptionBitmap+ ~Text}
PROCEDURE tmywindow.wmactivate;
BEGIN
  IF msg.wparam=wa_inactive THEN iswindowactive:=false ELSE iswindowactive:=true;
  paintcaption(0);
  defwndproc(msg);
END;

PROCEDURE tmywindow.wmsyschar;
BEGIN
  IF (msg.wparam=vk_space)AND(keystate=4) THEN
  BEGIN
    IF not isiconic(hwindow) THEN msg.wparam:=0;
    postmessage(hwindow,wm_syscommand,sc_keymenu,msg.wparam)
  END
  ELSE defwndproc(msg);
END;

{Zur Weitergabe an das SysMenuWindow}
PROCEDURE tmywindow.wmsyscommand;
BEGIN
  msg.result:=0;
  CASE (msg.wparam AND $FFF0 )OF
   sc_keymenu:msg.result:=sendmessage(syswindow^.hwindow,wm_syscommand,msg.wparam,msg.lparam);
   sc_mousemenu:msg.result:=0;
   sc_minimize:BEGIN
                 DefWndProc(MSG);
                 MoveWindow(syswindow^.hWindow,0,0,0,GetSystemmetrics(SM_CYIcon),True);
               END;
   sc_maximize:IF bMaxi<>NIL THEN
     BEGIN
       IF (not isiconic(hwindow))AND(MaxiMized) THEN
       BEGIN
         IF Sendmessage(hWindow,wm_command,swebuz_restore,msg.lparam)=0 THEN
         BEGIN
           bMaxi^.Ofst:=2*SysBMPW;
           bmaxi^.enable;
           IF bmini<>NIL THEN bmini^.enable;
           IF bClose<>NIL THEN bClose^.Enable;
           Maximized:=NOT(Maximized);
         END;
       END ELSE
       IF not isiconic(hwindow)or (not maximized) THEN
       BEGIN
         IF Sendmessage(hWindow,wm_command,swebuz_maximize,msg.lparam)=0 THEN
         BEGIN
           bMaxi^.Ofst:=3*SysBMPW;
           bmaxi^.enable;
           IF bmini<>NIL THEN bmini^.enable;
           IF bClose<>NIL THEN bClose^.Enable;
           Maximized:=NOT(Maximized);
         END;
       END ELSE
       BEGIN
         msg.wparam:=sc_restore;
         defwndproc(msg);
       END;
       IF IsIconic(HWindow)THEN MoveWindow(syswindow^.hWindow,SysBMPOfsX,SysBMPOfsY,SysBMPW,SysBMPH,True);
       msg.result:=0;
     END ELSE msg.result:=0;
   sc_restore:
     BEGIN
       IF (not isiconic(hwindow))AND(MaxiMized)AND(bMaxi<>NIL) THEN
       BEGIN
         Sendmessage(hWindow,wm_command,swebuz_restore,msg.lparam);
         bMaxi^.Ofst:=2*SysBMPW;
         bmaxi^.enable;
         IF bmini<>NIL THEN bmini^.enable;
         IF bClose<>NIL THEN bClose^.Enable;
         Maximized:=false;
         msg.result:=0;
       END;
       IF IsIconic(HWindow)THEN
       BEGIN
         MoveWindow(syswindow^.hWindow,SysBMPOfsX,SysBMPOfsY,SysBMPW,SysBMPH,True);
         PostMessage(hWindow,wm_QueryNewPalette,0,0);
       END;
       defwndproc(msg);
     END;
   ELSE defwndproc(msg);
  END;
END;

FUNCTION tmywindow.idleaction;
BEGIN
  idleaction:=False;
END;

PROCEDURE tmywindow.wmlbuttondown;
VAR pt,pt2:tPoint;
BEGIN
  IF (MoveOnClick)OR(msg.lparamhi<=CapDY) THEN
  BEGIN
    IF FullMove THEN WITH Attr DO
    BEGIN
      {move window}
      Pt:=MakePoint(msg.lParam);
      ClientToScreen(hWindow,Pt);
      Dec(pt.x,x);
      Dec(pt.y,y);
      pt2.x:=pt.x;
      pt2.y:=pt.y;
      WHILE (GetAsyncKeyState(VK_lButton)AND$8000)>0 DO
      BEGIN
        GetCursorPos(tPoint((@attr.x)^));
        Dec(x,pt.x);
        Dec(Y,pt.y);
        IF (x<>pt2.x)OR(y<>pt2.y) THEN
        BEGIN
          pt2.x:=x; pt2.y:=y;
          movewindow(hwindow,x,y,w,h,True);
        END;
        Application^.IdleAction;
        Yield;
      END;
    END ELSE
    BEGIN
      msg.message:=wm_nclbuttondown;
      msg.wparam:=htcaption;
      defwndproc(msg);
    END;
  END ELSE defwndproc(msg);
END;

PROCEDURE tmywindow.wmsize;
VAR obutton:integer;
BEGIN
  IF (msg.wparam=size_maximized)or(msg.wparam=size_restored)THEN
  BEGIN
    WITH pcrect DO BEGIN top:=3; bottom:=CapDY; left:=22; right:=msg.lparamlo-3; END;
    OButton:=msg.lparamlo-CapDY-1;
    IF bClose<>NIL THEN
    BEGIN
      with bClose^ do movewindow(hwindow,obutton,attr.y,attr.w,attr.h,True);
      pcrect.right:=OButton-2;
      dec(OButton,bClose^.attr.w+2);
    END;
    IF bMaxi<>NIL THEN
    BEGIN
      with bmaxi^ do movewindow(hwindow,obutton,attr.y,attr.w,attr.h,True);
      pcrect.right:=OButton-2;
      dec(OButton,bMaxi^.attr.w);
    END;
    IF bMini<>NIL THEN
    BEGIN
      with bmini^ do movewindow(hwindow,obutton,attr.y,attr.w,attr.h,True);
      pcrect.right:=OButton-2;
      dec(OButton,bMini^.attr.w);
    END;
  END;
  IF syswindow<>NIL THEN
  BEGIN
    IF (msg.wparam=size_minimized) THEN syswindow^.show(sw_showminimized)
                                   ELSE syswindow^.show(sw_shownormal)
  END;
  INHERITED wmsize(msg);
END;

PROCEDURE tmywindow.wmmeasureitem;
BEGIN
  WITH pmeasureitemstruct(msg.lparam)^ DO
  BEGIN
    CASE ctltype OF
      odt_listbox,odt_combobox:
        BEGIN
          itemwidth:= Fw;
          itemheight:=Fh+1;
          IF ctltype=odt_listbox THEN itemdata:=lb_addstring
                                 ELSE itemdata:=cb_addstring;
        END;
      odt_menu:
        BEGIN
          itemwidth:= Fw*strlen(pchar(itemdata));
          itemheight:=Fh+6;
        END;
    END;
  END;
END;

PROCEDURE TMYWINDOW.wmctlcolor;
BEGIN
  IF PaletteInstalled THEN SelectPalette(msg.wparam,hPal,false);
  CASE msg.lparamhi OF
    ctlcolor_edit:
    BEGIN msg.result:=BrEdit;settextcolor(msg.wparam,cVGEdit);setbkcolor(msg.wparam,cHGEdit);END;
    ctlcolor_listbox:
    BEGIN msg.result:=BrListbox;settextcolor(msg.wparam,cVGLB);setbkcolor(msg.wparam,cHGLB);END;
    ctlcolor_static:
    BEGIN msg.result:=BrStatic;settextcolor(msg.wparam,cVGStatic);setbkcolor(msg.wparam,cHGStatic);END;
    CTLCOLOR_GRAYBOX:
    BEGIN msg.result:=BrGrayBox;settextcolor(msg.wparam,cVGGrayBox);setbkcolor(msg.wparam,cHGGrayBox);END;
    CTLCOLOR_INVSTATIC:
    BEGIN msg.result:=BrInvStatic;settextcolor(msg.wparam,cVGInvStatic);setbkcolor(msg.wparam,cHGInvStatic);END;
    ELSE defwndproc(msg);
  END;
END;

PROCEDURE tmywindow.wmerasebkgnd;
  PROCEDURE RePaint(What:pWindow);FAR;
  VAR DC:hDC;
      Rect:tRect;
  BEGIN
    WITH What^ DO
    IF (attr.style and ws_child)=ws_child THEN
    BEGIN
      GetUpdateRect(hWindow,Rect,False);
      DC:=GetDC(hWindow);
      SendMessage(hWindow,WM_EraseBkGnd,DC,0);
      SendMessage(hWindow,WM_Paint,DC,0);
      ReleaseDC(hWindow,DC);
      ValidateRect(hWindow,@Rect);
    END;
  END;

BEGIN
  ForEach(@RePaint);
  msg.result:=1;
END;

PROCEDURE tmywindow.wmsetfocus;      BEGIN setfocus(oldfocus); msg.result:=0;END;
PROCEDURE tmywindow.wmIHaveTheFocus; BEGIN oldfocus:=msg.wparam; msg.result:=0;END;

FUNCTION tmywindow.timerproc;
BEGIN
  IF msg.wparam=$FFFF THEN pbuzApplication(application)^.idleaction;
  timerproc:=msg.wparam=$FFFF;
END;

{Fenster mit Cursor}
FUNCTION  tmychildwindow.GetClassName:PChar;
BEGIN
  GetClassName:='M4W_scndSWEBUZWindow';
END;
PROCEDURE tmychildwindow.GetWindowClass(VAR WndClass: TWndClass);
BEGIN
  INHERITED GetWindowClass(WndClass);
  wndclass.HCursor:=0;
END;

{************************************Bitmap-Mutterobjekt***************************************************}
CONSTRUCTOR BMPObject.Init;
VAR B: TBitMap;
    nc:ARRAY[0..20] OF char;
BEGIN
  drawfocus:=DefDrawFocus;
  {Bitmap laden und Größe des Objektes an diese anpassen}
  strpcopy(nc,abitmap); BMP := LoadMyBitmap(hinstance,nc);
  ATTR.W := 0;
  ATTR.H := 0;
  IF BMP <> 0 THEN
  BEGIN
    GetObject(BMP,Sizeof(B), @B);
    ATTR.W := B.bmWidth;
    ATTR.H := B.bmHeight;
  END;
  INHERITED Init(AParent, AnID,'', x0, y0, Attr.W, Attr.H, False);
  Attr.Style := Attr.Style OR bs_OwnerDraw OR ws_tabstop;
  IF BMP = 0 THEN Attr.Style := Attr.Style AND (NOT ws_tabstop);
  ATTR.ExStyle:=ws_ex_noparentnotify;
  disabled:=true;
END;
DESTRUCTOR BMPObject.Done;
BEGIN
  INHERITED Done;
  IF BMP <> 0 THEN DeleteMyBitmap(BMP);
END;

PROCEDURE BMPObject.wmsyskeydown;
BEGIN
  {Senden an Parent wegen <ALT>+Fx}
  postmessage(parent^.hwindow,wm_syskeydown,msg.wparam,msg.lparam);
  msg.result:=0;
END;
PROCEDURE BMPObject.WMkeydown(VAR Msg: TMessage);
BEGIN
  msg.result:=$ffff;
  IF (not disabled)AND(msg.wparam=vk_return) THEN EXIT;
  {Focushandling}
  IF (disabled)OR(msg.wparam<>vk_tab)
  THEN msg.result:=sendmessage(parent^.hwindow,wm_keydown,msg.wparam,msg.lparam) ELSE
  BEGIN setfocus(getnextdlgtabitem(parent^.hwindow,hwindow,keystate=2));msg.result:=0; END;
END;
PROCEDURE BMPObject.WMkeyup(VAR Msg: TMessage);
BEGIN
  msg.result:=sendmessage(parent^.hwindow,wm_keyup,msg.wparam,msg.lparam);
END;
PROCEDURE BMPObject.FramePaint(DC:hDC);
VAR Rect:Trect;
BEGIN
  IF NOT drawfocus THEN Exit;
  IF getfocus=hwindow THEN
  BEGIN
    WITH rect DO
    BEGIN
      top:=1;
      bottom:=attr.h-1;
      left:=1;
      right:=attr.w-1;
    END;
    framerect(DC,rect,BrBlack);
  END;
END;
PROCEDURE BMPObject.wmerasebkgnd(VAR msg:tmessage);
BEGIN
  {Dummy...Nur für BMPPaint!!!}
END;
PROCEDURE BMPObject.bmppaint;
VAR msg:tmessage;
BEGIN
  IF NOT disabled THEN
  BEGIN
    IF hWindow=0 THEN Exit;
    msg.wparam:=getdc(hwindow);
    wmerasebkgnd(msg);
    ReleaseDC(hwindow,msg.wparam);
  END;
END;

PROCEDURE BMPObject.WMLButtonDown;
BEGIN
  IF disabled THEN
  BEGIN
    msg.lparamlo:=msg.lparamlo+attr.x;
    msg.lparamhi:=msg.lparamhi+attr.y;
    msg.result:=sendmessage(parent^.hwindow,msg.message,msg.wparam,msg.lparam);
  END ELSE defwndproc(msg);
END;

PROCEDURE BMPObject.WMLButtonUp;
BEGIN
  IF disabled THEN
  BEGIN
    msg.lparamlo:=msg.lparamlo+attr.x;
    msg.lparamhi:=msg.lparamhi+attr.y;
    msg.result:=sendmessage(parent^.hwindow,wm_lbuttonup,msg.wparam,msg.lparam);
  END;
  defwndproc(msg);
END;

PROCEDURE BMPObject.WMMouseMove;
BEGIN
  IF disabled THEN
  BEGIN
    msg.lparamlo:=msg.lparamlo+attr.x;
    msg.lparamhi:=msg.lparamhi+attr.y;
    msg.result:=sendmessage(parent^.hwindow,wm_mousemove,msg.wparam,msg.lparam);
  END ELSE defwndproc(msg);
END;

PROCEDURE BMPObject.WMLButtonDblclk(VAR Msg: TMessage);
BEGIN IF NOT DisAbled THEN msg.message:=wm_LbuttonDown; wmlbuttondown(msg); END;

PROCEDURE BMPObject.WMSETFOCUS(VAR Msg: TMessage);
BEGIN
  IF Disabled OR(BMP = 0) THEN setfocus(getnextdlgtabitem(parent^.hwindow,hwindow,keystate=2)) ELSE
  BEGIN
    bmppaint;
    postmessage(parent^.hwindow,wm_ihavethefocus,hwindow,0);
  END;
  msg.result:=0;
END;
PROCEDURE BMPObject.WMKILLFOCUS(VAR Msg: TMessage);
BEGIN
  bmppaint;
  msg.result:=0;
END;
PROCEDURE BMPObject.enable;
BEGIN
  disabled:=false;
  bmppaint;
END;
PROCEDURE BMPObject.disable;
BEGIN
  disabled:=true;
END;
{******ENDE**************************Bitmap-Mutterobjekt***************************************************}

{************************************Bitmap-Schalterobjekt***************************************************}
CONSTRUCTOR BMPCaptBtn.Init;
VAR B: TBitMap;
    nc:ARRAY[0..20] OF char;
BEGIN
  drawfocus:=DefDrawFocus;
  {Bitmap laden und Größe des Objektes an diese anpassen}
  strpcopy(nc,abitmap); BMP := LoadBitmap(hinstance,nc); IF BMP = 0 THEN Fail;
  GetObject(BMP,Sizeof(B), @B);
  ATTR.W := B.bmWidth;
  ATTR.H := B.bmHeight;
  tButton.Init(AParent, AnID,'', x0, y0, Attr.W, Attr.H, False);
  Attr.Style := Attr.Style OR bs_OwnerDraw OR ws_tabstop;
  ATTR.ExStyle:=ws_ex_noparentnotify;
  disabled:=true;
  attr.h:=attr.h DIV 2;
  clicked:=false;
  KeyClick:=false;
  autorep:=autorepeat;
END;

Destructor BMPCaptBtn.done;
BEGIN
  IF BMP<>0 THEN Deleteobject(BMP);
  tbutton.done;
END;

PROCEDURE BMPCaptBtn.FramePaint(DC:hDC);
BEGIN
  IF drawfocus AND (OutSide OR NOT(clicked OR KeyClick))AND(getfocus=hwindow) THEN
  BEGIN
    IF ExOs<>Ex_NewWin
    THEN frame3d(DC,2,1,Attr.W-5,Attr.H-4,1,up)
    ELSE frame3d(DC,1,1,Attr.W-3,Attr.H-3,1,up);
  END;
END;

PROCEDURE BMPButton1.FramePaint(DC:hDC);
VAR Rect:Trect;
BEGIN
  IF NOT drawfocus THEN Exit;
  IF getfocus=hwindow THEN
  BEGIN
    WITH rect DO
    BEGIN
      top:=1;
      bottom:=attr.h-1;
      left:=1;
      right:=attr.w;
    END;
    framerect(DC,rect,BrBlack);
  END;
END;

PROCEDURE BMP3Button1.FramePaint(DC:hDC);
VAR Rect:Trect;
BEGIN
  IF NOT drawfocus THEN Exit;
  IF getfocus=hwindow THEN
  BEGIN
    WITH rect DO
    BEGIN
      top:=1;
      bottom:=attr.h-1;
      left:=1;
      right:=attr.w;
    END;
    framerect(DC,rect,BrBlack);
  END;
END;

CONSTRUCTOR BMPButton.Init;
BEGIN
  INHERITED Init(AParent,AnID,x0,y0,ABitMap);
  attr.h:=attr.h DIV 2;
  clicked:=false;
  KeyClick:=false;
  autorep:=autorepeat;
  outside:=false;
END;

PROCEDURE BMPButton.wmerasebkgnd(VAR msg:tmessage);
VAR memdc:hdc;
BEGIN
  IF BMP = 0 THEN Exit;
  IF NOT disabled THEN
  BEGIN
    memdc:=createcompatibledc(msg.wparam);
    IF PaletteInstalled THEN
    BEGIN
      SelectPalette(msg.wparam,hPal,false);
      SelectPalette(memdc,hPal,false);
    END;
    selectobject(memdc,bmp);
    bitblt(msg.wparam,0,0,Attr.w,Attr.h,memdc,0,Byte((NOT OutSide)AND(clicked OR KeyClick))*Attr.h,srccopy);
    deletedc(memdc);
    FramePaint(msg.wparam);
    msg.result:=1;
  END;
END;

PROCEDURE bmpbutton.wmtimer;
BEGIN
  killtimer(hwindow,401);
  IF NOT(disabled)AND((NOT outside)OR(KeyClick)) THEN sendmessage(parent^.hwindow,wm_command,getid,hwindow);
  IF KeyClick OR Clicked THEN settimer(hwindow,401,55,NIL);
END;

PROCEDURE bmpbutton.WMkeydown(VAR Msg: TMessage);
BEGIN
  IF (NOT clicked)THEN
  BEGIN
    IF (NOT disabled)AND(msg.wparam=vk_return)AND(NOT KeyClick)THEN
    BEGIN
      KeyClick:=true;
      bmppaint;
      msg.result:=0;
      IF autorep THEN settimer(hwindow,401,150,NIL);
    END ELSE INHERITED WMKeyDown(MSG);
  END ELSE Msg.Result:=1;
END;

PROCEDURE bmpbutton.WMkeyup(VAR Msg: TMessage);
BEGIN
  IF NOT Clicked THEN
  BEGIN
    msg.result:=$FFFF;
    IF (disabled)OR(msg.wparam<>vk_return)
    THEN WITH msg do result:=sendmessage(parent^.hwindow,wm_keyup,wparam,lparam)
    ELSE IF KeyClick THEN
    BEGIN
      IF (msg.wparam=vk_return) THEN
      BEGIN
        msg.result:=0;
        KeyClick:=False;
        SendMessage(parent^.hwindow,wm_command,getid,hwindow);
      END;
    END;
    IF msg.wparam=vk_return THEN
    BEGIN
      KeyClick:=False;
      IF autorep THEN killtimer(hwindow,401);
      IF NOT DisAbled THEN BMPPaint;
    END;
  END ELSE Msg.Result:=1;
END;

PROCEDURE BMPButton.WMKILLFOCUS(VAR Msg: TMessage);
BEGIN
  IF (Clicked OR KeyClick)AND autorep THEN killtimer(hwindow,401);
  Clicked:=False;
  KeyClick:=False;
  bmpPaint;
  msg.result:=0;
END;

PROCEDURE bmpbutton.WMMousemove(VAR Msg: TMessage);
BEGIN
  IF clicked AND NOT(disabled)THEN
  BEGIN
    IF childwindowfrompoint(hwindow,makepoint(msg.lparam))<>hwindow
    THEN IF NOT outside THEN BEGIN outside:=true;  bmppaint; END ElSE
    ELSE IF     outside THEN BEGIN outside:=false; bmppaint; END;
  END;
  INHERITED wmmousemove(msg);
END;

PROCEDURE BMPButton.WMLButtonDown;
BEGIN
  IF NOT KeyClick THEN
  BEGIN
    IF NOT disabled THEN
    BEGIN
      outside:=false;
      clicked:=true;
      bmppaint;
      IF autorep THEN settimer(hwindow,401,150,NIL);
    END;
    INHERITED WMLButtonDown(MSG)
  END ELSE Msg.Result:=1;
END;

PROCEDURE BMPButton.WMLButtonUp;
BEGIN
  IF NOT KeyClick THEN
  BEGIN
    IF autorep THEN killtimer(hwindow,401);
    clicked:=false;
    IF NOT(outside OR disabled) THEN bmppaint;
    INHERITED WMLButtonUp(MSG);
  END ELSE Msg.Result:=1;
END;
{******ENDE**************************Bitmap-Schalterobjekt***************************************************}

{************************************Bitmap-Systemschalterobjekt*********************************************}
CONSTRUCTOR BMPSysButton.Init;
BEGIN
  drawfocus:=DefDrawFocus;
  {Bitmap laden und Größe des Objektes an diese anpassen}
  ATTR.W := w;
  ATTR.H := h;
  ofst:=xofs;
  bmp:=0;
  TButton.Init(AParent, AnID,NIL, x0, y0, Attr.W, Attr.H, False);
  Attr.Style := (Attr.Style OR bs_OwnerDraw)AND(NOT ws_tabstop);
  ATTR.ExStyle:=ws_ex_noparentnotify;
  Clicked:=false;
  outside:=false;
  autorep:=false;
  disabled:=true;
  KeyClick:=False;
  FocusTo:=0;
END;

PROCEDURE BMPSysButton.wmerasebkgnd(VAR msg:tmessage);
VAR memdc:hdc;
BEGIN
  IF NOT disabled THEN
  BEGIN
    memdc:=createcompatibledc(msg.wparam);
    selectobject(memdc,pBUZApplication(Application)^.sysbmp);
    bitblt(msg.wparam,0,0,Attr.w,Attr.h,memdc,ofst,Byte((NOT OutSide)AND(clicked OR KeyClick))*Attr.h,srccopy);
    deletedc(memdc);
    msg.result:=1;
  END;
END;

PROCEDURE BMPSysButton.WMSETFOCUS;
BEGIN
  IF Clicked THEN FocusTo:=msg.wparam ELSE setfocus(msg.wparam);
  msg.result:=0;
END;

PROCEDURE bmpSysbutton.WMkeyup(VAR Msg: TMessage);
BEGIN
  IF NOT Clicked THEN
  BEGIN
    msg.result:=$FFFF;
    IF (disabled)OR(msg.wparam<>vk_return)
    THEN WITH msg do result:=sendmessage(parent^.hwindow,wm_keyup,wparam,lparam)
    ELSE IF KeyClick THEN
    BEGIN
      IF (msg.wparam=vk_return) THEN
      BEGIN
        msg.result:=0;
        postmessage(parent^.hwindow,wm_syscommand,getid,hwindow);
      END;
    END;
    IF msg.wparam=vk_return THEN
    BEGIN
      KeyClick:=False;
      IF NOT DisAbled THEN BMPPaint;
    END;
  END ELSE Msg.Result:=1;
END;

PROCEDURE BMPSysButton.WMLButtonUp;
BEGIN
  IF NOT KeyClick THEN
  BEGIN
    clicked:=false;
    IF FocusTo<>0 THEN setfocus(FocusTo);
    FocusTo:=0;
    IF (NOT outside)AND(NOT disabled) THEN
    BEGIN
      bmppaint;
      postmessage(parent^.hwindow,wm_syscommand,getid,hwindow);
    END;
    outside:=false;
    BMPObject.WMLButtonUp(MSG);
  END ELSE Msg.Result:=1;
END;
{******ENDE**************************Bitmap-SystemSchalterobjekt*********************************************}

{************************************Bitmap-Schalterobjekt***************************************************}
CONSTRUCTOR BMP3CaptBtn.Init;
VAR B: TBitMap;
    nc:ARRAY[0..20] OF char;
BEGIN
  drawfocus:=DefDrawFocus;
  {Bitmap laden und Größe des Objektes an diese anpassen}
  strpcopy(nc,abitmap); BMP := LoadBitmap(hinstance,nc); IF BMP = 0 THEN Fail;
  GetObject(BMP,Sizeof(B), @B);
  ATTR.W := B.bmWidth;
  ATTR.H := B.bmHeight;
  tButton.Init(AParent, AnID,'', x0, y0, Attr.W, Attr.H, False);
  Attr.Style := Attr.Style OR bs_OwnerDraw OR ws_tabstop;
  ATTR.ExStyle:=ws_ex_noparentnotify;
  disabled:=true;
  attr.h:=attr.h DIV 3;
  clicked:=false;
  KeyClick:=false;
  autorep:=autorepeat;
  BlinkIfActive:=False;
END;

Destructor BMP3CaptBtn.done;
BEGIN
  IF BMP<>0 THEN Deleteobject(BMP);
  tbutton.done;
END;

CONSTRUCTOR BMP3Button.Init;
BEGIN
  BMPObject.Init(AParent,AnID,x0,y0,ABitMap);
  attr.h:=attr.h DIV 3;
  clicked:=false;
  active:=false;
  autorep:=autorepeat;
  KeyClick:=False;
  BlinkIfActive:=False;
  WasActive:=False;
  Event:=0;
END;

PROCEDURE BMP3Button.wmerasebkgnd(VAR msg:tmessage);
VAR memdc:hdc;
BEGIN
  IF BMP = 0 THEN Exit;
  IF NOT disabled THEN
  BEGIN
    memdc:=createcompatibledc(msg.wparam);
    IF PaletteInstalled THEN
    BEGIN
      SelectPalette(msg.wparam,hPal,false);
      SelectPalette(memdc,hPal,false);
    END;
    selectobject(memdc,bmp);
    IF (NOT OutSide)AND(clicked or keyclick)
    THEN   bitblt(msg.wparam,0,0,Attr.w,Attr.h,memdc,0,Attr.h,srccopy)
    ELSE IF active
      THEN bitblt(msg.wparam,0,0,Attr.w,Attr.h,memdc,0,Attr.h+Attr.h,srccopy)
      ELSE bitblt(msg.wparam,0,0,Attr.w,Attr.h,memdc,0,0,srccopy);
    deletedc(memdc);
    FramePaint(msg.wparam);
    msg.result:=1;
  END;
END;
PROCEDURE BMP3Button.setstyle;
BEGIN
  IF style=_on THEN
  BEGIN
    active:=true;WasActive:=True;
    IF BlinkIfActive THEN SetBlink(_on);
  END ELSE
  BEGIN
    active:=false;WasActive:=False;
    IF event<>0 THEN pBuZApplication(Application)^.RemoveEvent(Event);
    Event:=0;
  END;
  bmppaint;
END;

PROCEDURE BMP3BUTTON.SetBlink;
VAR msg:Tmessage;
BEGIN
  BlinkIfActive:=OnOff=_on;
  IF BlinkIfActive THEN
  BEGIN
    WasActive:=Active;
    msg.lparam:=500;
    msg.message:=wm_idleaction;
    IF Event=0 THEN Event:=pBuZApplication(Application)^.AddEvent(hWindow,Msg);
  END ELSE IF Event<>0 THEN
  BEGIN
    Active:=WasActive;
    bmppaint;
    pBuZApplication(Application)^.RemoveEvent(Event);
    Event:=0;
  END;
END;

PROCEDURE bmp3button.TimerScroll;
BEGIN
  Active:=NOT Active;
  bmppaint;
END;

{******ENDE**************************Bitmap-Schalterobjekt***************************************************}

{*****************2State-Button mit runden Ecken**********************************}
CONSTRUCTOR MSGButton.Init(AParent: PWindowsObject; AnID:word;x0,y0: Integer;ABitMap: STRING;Autorepeat:boolean);
VAR nc:ARRAY[0..50]OF Char;
    B:tBitmap;
BEGIN
  drawfocus:=DefDrawFocus;
  {Bitmap laden und Größe des Objektes an diese anpassen}
  strpcopy(nc,abitmap); BMP := LoadBitmap(hLanguage,nc); IF BMP = 0 THEN Fail;
  GetObject(BMP,Sizeof(B), @B);
  ATTR.W := B.bmWidth;
  ATTR.H := B.bmHeight DIV 2;
  tButton.Init(AParent, AnID,'', x0, y0, Attr.W, Attr.H, False);
  Attr.Style := Attr.Style OR bs_OwnerDraw OR ws_tabstop;
  ATTR.ExStyle:=ws_ex_noparentnotify;
  disabled:=true;
  clicked:=false;
  autorep:=autorepeat;
  KeyClick:=False;
END;

PROCEDURE MSGButton.FramePaint(DC:hDC);
VAR Rect:Trect;
BEGIN
  IF NOT drawfocus THEN Exit;
  IF getfocus=hwindow THEN
  BEGIN
    WITH rect DO
    BEGIN
      top:=2;
      bottom:=attr.h-2;
      left:=2;
      right:=attr.w-2;
    END;
    framerect(DC,rect,getstockObject(Black_brush));
  END;
END;

{************************************Bitmap-Up-Down-Schalterobjekt*******************************************}
CONSTRUCTOR bmpudbutton.Init(AParent: PWindowsObject; AnID,x0,y0,ll,ul,dwn:Integer;ABitMap: STRING);
BEGIN
  INHERITED Init(AParent,AnID,x0,y0,ABitMap);
  h:=attr.h DIV 3;
  attr.h:=2*h;
  attr.w:=attr.w DIV 2;
  w:=attr.w;
  uclicked:=false; dclicked:=false;
  pos:=dwn;
  SetLimits (ll, ul);
END;

PROCEDURE bmpudbutton.wmerasebkgnd(VAR msg:tmessage);
VAR  memdc:hdc;
BEGIN
  IF BMP = 0 THEN Exit;
  IF NOT disabled THEN
  BEGIN
    memdc:=createcompatibledc(msg.wparam);
    IF PaletteInstalled THEN
    BEGIN
      SelectPalette(msg.wparam,hPal,false);
      SelectPalette(memdc,hPal,false);
    END;
    selectobject(memdc,bmp);
    IF NOT disabled THEN
    BEGIN
      IF (pos>low_lim) THEN IF uclicked THEN bitblt(msg.wparam,0,0,w,h,memdc,0,h,srccopy)
                                        ELSE bitblt(msg.wparam,0,0,w,h,memdc,0,h+h,srccopy)
                                        ELSE bitblt(msg.wparam,0,0,w,h,memdc,0,0,srccopy);
      IF (pos<up_lim)  THEN IF dclicked THEN bitblt(msg.wparam,0,h,w,h,memdc,w,h,srccopy)
                                        ELSE bitblt(msg.wparam,0,h,w,h,memdc,w,h+h,srccopy)
                                        ELSE bitblt(msg.wparam,0,h,w,h,memdc,w,0,srccopy);
    END;
    deletedc(memdc);
    FramePaint(msg.wparam);
  END;
  msg.result:=1;
END;

PROCEDURE bmpudbutton.wmtimer;
VAR pos1:integer;
BEGIN
  IF NOT disabled THEN
  BEGIN
    Killtimer(hwindow,400);
    pos1:=pos;
    IF uclicked THEN BEGIN IF pos1>low_lim THEN dec (pos1) END
                ELSE BEGIN IF pos1<up_lim  THEN inc (pos1) END;
    IF pos1<>pos THEN
    BEGIN
      pos:=pos1;
      sendmessage(parent^.hwindow,wm_command,getid,hwindow);
      bmppaint;
    END;
    settimer(hwindow,400,150,NIL);
  END;
END;

PROCEDURE bmpudbutton.WMkeydown(VAR Msg: TMessage);
BEGIN
  IF disabled THEN INHERITED WMkeydown(MSG) ELSE
  BEGIN
    CASE msg.wparam OF
      vk_up  : IF Keystate=0 THEN
               BEGIN
                 msg.result:=sendmessage(hwindow,wm_lbuttondown,0,0);
                 postmessage(hwindow,wm_lbuttonup,0,0);
               END ELSE INHERITED WMkeydown(MSG);
      vk_down: IF Keystate=0 THEN
               BEGIN
                 msg.result:=sendmessage(hwindow,wm_lbuttondown,0,longint(attr.h-1)shl 16);
                 postmessage(hwindow,wm_lbuttonup,0,longint(attr.h-1)shl 16);
               END ELSE INHERITED WMkeydown(MSG);
      vk_return:msg.result:=SendMessage(Parent^.hWindow,wm_KeyDown,vk_return,msg.lparam);
      ELSE     INHERITED WMkeydown(MSG)
    END;
  END;
END;

PROCEDURE bmpudbutton.WMLButtonDown;
BEGIN
  IF NOT disabled THEN
  BEGIN
    IF msg.lparamhi<h THEN
    BEGIN  uclicked:=true; dclicked:=false; bmppaint; END ELSE
    BEGIN  dclicked:=true; uclicked:=false; bmppaint; END;
    settimer(hwindow,400,150,NIL);
  END;
  INHERITED wmlbuttondown(msg);
END;

PROCEDURE bmpudbutton.WMLButtonUp;
BEGIN
  killtimer(hwindow,400);
  IF NOT disabled THEN
  BEGIN
    IF dclicked and (pos<up_lim)  THEN inc(pos) ELSE
    IF uclicked and (pos>low_lim) THEN dec(pos);
    dclicked:=false;
    uclicked:=false;
    bmppaint;
    IF (msg.lparamlo>=w) OR (msg.lparamhi>=(h+h)) THEN sendmessage(parent^.hwindow,wm_command,getid,hwindow);
  END;
  INHERITED WMLButtonUp(MSG)
END;

FUNCTION  bmpudbutton.getpos;
BEGIN getpos:=pos; END;

PROCEDURE  bmpudbutton.setpos;
BEGIN IF (n<>pos)AND(n>=low_lim)AND(n<=up_lim) THEN BEGIN pos:=n; bmppaint;END; END;

procedure BmpUDButton.SetLimits (ll, ul: integer);
begin
  low_lim := ll; up_lim := ul;
  if pos < low_lim then pos := low_lim else
  if pos > up_lim  then pos := up_lim;
  BMPPaint;
end;

{******ENDE**************************Bitmap-Up-Down-Schalterobjekt*******************************************}

{************************************Bitmap-SchiebeSchalterobjekt*******************************************}
CONSTRUCTOR switch.Init(AParent: PWindowsObject; AnID,x0,y0:Integer;maxdwn,dwn:Byte;ABitMap: STRING);
BEGIN
  Id:=AnID;
  INHERITED Init(AParent,$ffff,x0,y0,ABitMap);
  fillchar(invalidpos,sizeof(invalidpos),0);
  clicked:=false;
  maxdown:=maxdwn;
  pos:=dwn;
  W := attr.w;
  H := attr.h DIV 2;
  attr.h:=maxdown*H;
END;

PROCEDURE switch.WMkeydown(VAR Msg: TMessage);
BEGIN
  IF disabled THEN INHERITED WMkeydown(MSG) ELSE
    CASE msg.wparam OF
     vk_up  : IF keystate=0 THEN
              BEGIN
                msg.lparamhi:=0+integer((pos-2)*(h+1));
                drawpos(msg);
                msg.result:=0;
              END ELSE INHERITED WMkeydown(MSG);
     vk_down: IF keystate=0 THEN
              BEGIN
                msg.lparamhi:=0+integer(pos*(h+1));
                drawpos(msg);
                msg.result:=0;
              END ELSE INHERITED WMkeydown(MSG);
     vk_return:msg.result:=SendMessage(Parent^.hWindow,wm_KeyDown,vk_return,msg.lparam);
    ELSE INHERITED WMkeydown(MSG);
  END;
END;

PROCEDURE switch.wmerasebkgnd(VAR msg:tmessage);
VAR i:integer;
    memdc:hdc;
    rect:trect;
BEGIN
  IF BMP = 0 THEN Exit;
  IF NOT disabled THEN
  BEGIN
    memdc:=createcompatibledc(msg.wparam);
    IF PaletteInstalled THEN
    BEGIN
      SelectPalette(msg.wparam,hPal,false);
      SelectPalette(memdc,hPal,false);
    END;
    selectobject(memdc,bmp);
    IF NOT disabled THEN  FOR i:=1 TO maxdown DO IF pos=i
      THEN bitblt(msg.wparam,0,(i-1)*h,w,h,memdc,0,0,srccopy)
      ELSE bitblt(msg.wparam,0,(i-1)*h,w,h,memdc,0,h,srccopy);
    deletedc(memdc);
    IF drawfocus AND(getfocus=hwindow) THEN
    BEGIN
      WITH rect DO
      BEGIN
        top:=0;
        bottom:=attr.h;
        left:=0;
        right:=attr.w;
      END;
      framerect(msg.wparam,rect,BrBlack);
    END;
  END;
  msg.result:=1;
END;


PROCEDURE switch.drawpos(msg:tmessage);
VAR pos1:integer;
BEGIN
  pos1:=integer(msg.lparamhi) DIV h +1;
  IF pos<>pos1 THEN
  BEGIN
    IF pos1<1 THEN  BEGIN  pos1:=1;  WHILE invalidpos[pos1] DO inc(pos1);  END ELSE
    IF pos1>maxdown THEN BEGIN pos1:=maxdown; WHILE invalidpos[pos1] DO dec(pos1);  END ELSE
    IF pos1<pos THEN WHILE invalidpos[pos1] DO inc(pos1) ELSE
    IF pos1>pos THEN WHILE invalidpos[pos1] DO dec(pos1);
    IF pos1<>pos THEN
    BEGIN
      pos:=pos1;
      bmppaint;
      postmessage(parent^.hwindow,wm_command,Id,hwindow);
    END;
  END;
END;

PROCEDURE switch.WMLButtonDown;
BEGIN
  IF NOT disabled THEN BEGIN clicked:=true; drawpos(msg);END;
  INHERITED WMLButtonDown(MSG)
END;

PROCEDURE switch.WMLButtonUp;
BEGIN
  clicked:=false;
  INHERITED WMLButtonUp(Msg);
END;

PROCEDURE switch.WMMousemove;
BEGIN
  IF (NOT disabled) AND clicked THEN drawpos(msg);
  INHERITED wmmousemove(msg);
END;

PROCEDURE switch.WMKillFocus;
BEGIN
  Clicked:=false;
  INHERITED wmKillfocus(msg);
END;


PROCEDURE switch.validatepos; BEGIN invalidpos[x]:=false;END;

PROCEDURE switch.invalidatepos;
BEGIN
  invalidpos[x]:=true;
  WHILE invalidpos[pos] DO BEGIN dec(pos); IF pos<1 THEN pos:=maxdown; END;
  IF X<>pos THEN bmppaint;
END;

FUNCTION  switch.getpos; BEGIN getpos:=pos; END;

PROCEDURE switch.setpos;
BEGIN
  IF pos<>x THEN
  BEGIN
    IF x<1 THEN  BEGIN  x:=1;  WHILE invalidpos[x] DO inc(x);  END ELSE
    IF x>maxdown THEN BEGIN x:=maxdown; WHILE invalidpos[x] DO dec(x);  END ELSE
    IF x<pos THEN WHILE invalidpos[x] DO inc(x) ELSE
    IF x>pos THEN WHILE invalidpos[x] DO dec(x);
    IF x<>pos THEN
    BEGIN
      pos:=x;
      postmessage(parent^.hwindow,wm_command,getid,hwindow);
      bmppaint;
    END;
  END;
END;
{******ENDE**************************Bitmap-SchiebeSchalterobjekt*******************************************}

{************************************Bitmap-SchiebeSchalterobjekt -LR*******************************************}


CONSTRUCTOR LRswitch.Init(AParent: PWindowsObject; AnID,x0,y0:Integer;maxdwn,dwn:Byte;ABitMap: STRING);
BEGIN
  Id:=AnID;
  bmpobject.Init(AParent,AnID,x0,y0,ABitMap);
  fillchar(invalidpos,sizeof(invalidpos),0);
  clicked:=false;
  maxdown:=maxdwn;
  pos:=dwn;
  W := attr.h;
  H := attr.w DIV 2;
  attr.w:=h*maxdown;
END;

PROCEDURE LRswitch.WMkeydown(VAR Msg: TMessage);
BEGIN
  IF disabled THEN BMPObject.WMkeydown(MSG) ELSE
    CASE msg.wparam OF
     vk_left: IF keystate=0 THEN
              BEGIN
                msg.lparamlo:=0+integer((pos-2)*(h+1));
                drawpos(msg);
                msg.result:=0;
              END ELSE BMPObject.WMkeydown(MSG);
     vk_right:IF keystate=0 THEN
              BEGIN
                msg.lparamlo:=0+integer(pos*(h));
                drawpos(msg);
                msg.result:=0;
              END ELSE BMPObject.WMkeydown(MSG);
     vk_return:msg.result:=SendMessage(Parent^.hWindow,wm_KeyDown,vk_return,msg.lparam);
    ELSE BMPObject.WMkeydown(MSG);
  END;
END;

PROCEDURE LRswitch.wmerasebkgnd(VAR msg:tmessage);
VAR i:integer;
    memdc:hdc;
    rect:trect;
BEGIN
  IF BMP = 0 THEN Exit;
  IF NOT disabled THEN
  BEGIN
    memdc:=createcompatibledc(msg.wparam);
    IF PaletteInstalled THEN
    BEGIN
      SelectPalette(msg.wparam,hPal,false);
      SelectPalette(memdc,hPal,false);
    END;
    selectobject(memdc,bmp);
    IF NOT disabled THEN  FOR i:=1 TO maxdown DO IF pos=i
      THEN bitblt(msg.wparam,(i-1)*h,0,h,w,memdc,0,0,srccopy)
      ELSE bitblt(msg.wparam,(i-1)*h,0,h,w,memdc,h,0,srccopy);
    deletedc(memdc);
    IF drawfocus AND(getfocus=hwindow) THEN
    BEGIN
      WITH rect DO
      BEGIN
        top:=0;
        bottom:=attr.h;
        left:=0;
        right:=attr.w;
      END;
      framerect(msg.wparam,rect,BrBlack);
    END;
  END;
  msg.result:=1;
END;


PROCEDURE LRswitch.drawpos(msg:tmessage);
VAR pos1:integer;
BEGIN
  msg.lparamhi:=msg.lparamlo;
  INHERITED drawpos(msg);
END;
{******ENDE**************************Bitmap-SchiebeSchalterobjekt*******************************************}
{************************************Bitmap-Fontobjekt*******************************************************}
CONSTRUCTOR BMPFont.Init(AParent: PWindowsObject; AnID,x0,y0,ni,lrc:Integer;ABitMap: STRING;MaxChInFont:Byte);
VAR B: TBitMap;
    i:integer;
    nc:ARRAY[0..20] OF char;
BEGIN
  CharsInFont:=MaxChInFont;
  strpcopy(nc,abitmap);
  BMP := LoadMyBitMap(hinstance,nc); IF BMP = 0 THEN Fail;
  GetObject(BMP,Sizeof(B), @B); W := B.bmWidth DIV CharsInFont; H := B.bmHeight;
  INHERITED Init(AParent, AnID,'', x0, y0, ni*W, H, 0);
  Attr.Style := Attr.Style OR bs_OwnerDraw;
  n:=ni;m:=lrc;FOR i:=0 TO n DO bmptext[i]:=0;
  FOR i:=1 TO n DO index[i]:=i;
  FOR i:=n+1 TO 255 DO index[i]:=0;
  disabled:=true;
  Event:=0;
END;

PROCEDURE BMPFont.SetupWindow;
VAR DC:hDC;
    msg:TMessage;
BEGIN
  INHERITED SetupWindow;
  DC:=getdc(hwindow);
  membmp:=createcompatiblebitmap(DC,attr.w,attr.h);
  releasedc(hwindow,DC);
END;


DESTRUCTOR BMPFont.Done;
BEGIN
  IF Event>0 THEN pBuZApplication(Application)^.RemoveEvent(Event);
  INHERITED Done;
  IF BMP <> 0 THEN DeleteMyBitmap(BMP);
  IF membmp<>0 THEN DeleteObject(MemBMP);
END;

PROCEDURE BMPFont.wmerasebkgnd;
VAR i:integer;
    memdc,memdc1:hdc;
BEGIN
  IF BMP = 0 THEN Exit;
  IF NOT disabled THEN
  BEGIN
    memdc:=createcompatibledc(msg.wparam);
    memdc1:=createcompatibledc(msg.wparam);
    IF PaletteInstalled THEN
    BEGIN
      SelectPalette(msg.wparam,hPal,false);
      SelectPalette(memdc,hPal,false);
      SelectPalette(memdc1,hPal,false);
    END;
    selectobject(memdc,bmp);
    selectobject(memdc1,membmp);
    FOR i:=1 TO n DO bitblt(memdc1,w*(i-1),0,w,h,memdc,word(w)*word(bmptext[index[i]]),0,srccopy);
    bitblt(msg.wparam,0,0,attr.w,attr.h,memdc1,0,0,srccopy);
    deletedc(memdc);
    deletedc(memdc1);
  END;
  msg.result:=1;
END;

PROCEDURE BMPFont.bmppaint;
VAR msg:tmessage;
    Rect:tRect;
BEGIN
  IF disabled THEN
  BEGIN
    WITH Attr DO SetRect(Rect,x,y,x+w,y+h);
    InvalidateRect(parent^.hWindow,@Rect,True)
  END ELSE
  BEGIN
    IF hWindow=0 THEN Exit;
    msg.wparam:=getdc(hwindow);
    wmerasebkgnd(msg);
    releasedc(hwindow,msg.wparam);
  END;
END;

FUNCTION bmpfont.gettext:STRING;
VAR i:integer;
    ch:Char;
BEGIN
  FOR i:=1 TO n DO
  BEGIN
    ch:=char(bmptext[i]+32);
    IF Byte(ch)>=128 THEN Inc(Byte(ch),64);
    gettext[i]:=ch;
  END;
  gettext[0]:=char(n);
END;

PROCEDURE bmpfont.TimerScroll;
VAR i:Word;
BEGIN
  IF NOT(Disabled)THEN
  BEGIN
    i:=n+index[0]+1;
    IF I>255 THEN i:=255;
    index[i]:=index[1];
    move(index[2],index[1],i-1);
    bmppaint;
  END;
  IF MSG.lParam>120 THEN
  BEGIN
    Msg.lParam:=120;
    pBuZApplication(Application)^.RemoveEvent(Event);
    Event:=pBuZApplication(Application)^.AddEvent(hWindow,Msg);
  END;
END;

PROCEDURE Replace(VAR S:STRING;S0,S1:STRING);
VAR CPos:Integer;
BEGIN
  CPos:=POS(S0,S);
  WHILE CPos<>0 DO
  BEGIN
    Delete(S,CPos,Length(S0));
    Insert(S1,S,CPos);
    CPos:=POS(S0,S);
  END;
END;

PROCEDURE BMPFont.setbmptext(text:STRING);
VAR i:integer;
    blitit:boolean;
    k:Byte;
    MSG:tMessage;
BEGIN
  IF CharsInFont<=64 THEN
  BEGIN
    Replace(text,'Ä','Ae');
    Replace(text,'ä','ae');
    Replace(text,'Ö','Oe');
    Replace(text,'ö','oe');
    Replace(text,'Ü','Ue');
    Replace(text,'ü','ue');
    Replace(text,'ß','ss');
    Replace(text,'~','-');
  END;
  IF length(text)<=n THEN
  BEGIN
    IF m=0 THEN WHILE length(text)<n DO text:=text+' '
    ELSE IF m=1 THEN WHILE length(text)<n DO insert(' ',text,1)
    ELSE WHILE length(text)<n DO
    BEGIN text:=text+' ';IF length(text)<n THEN insert(' ',text,1);END;
  END;
  blitit:=false;
  FOR i:=1 TO Length(text) DO
  BEGIN
    IF CharsInFont<=64 THEN k:=byte(upcase(Text[i]))-32 ELSE k:=byte(Text[i])-32;
    IF k>=160 THEN Dec(k,64);
    IF k>=CharsInFont THEN k:=0;
    IF NOT(k=BmpText[i]) THEN
    BEGIN
      Bmptext[i]:=k;
      IF i<=n THEN blitit:=true;
    END;
  END;
  IF blitit THEN
  BEGIN
    IF Event>0 THEN
    BEGIN
      pBuZApplication(Application)^.RemoveEvent(Event);
      Event:=0;
    END;
    IF length(text)>n THEN
    BEGIN
      Msg.lParam:=n*100;
      Msg.wParam:=0;
      Msg.Message:=WM_IdleAction;
      Event:=pBuZApplication(Application)^.AddEvent(hWindow,MSG);
    END;
    index[0]:=length(text);
    FOR i:=1 TO index[0] DO index[i]:=i;
    FOR i:=index[0]+1 TO 255 DO index[i]:=0;
    bmppaint;
  END;
END;

PROCEDURE bmpfont.enable;
VAR i:byte;
BEGIN
  FOR i:=1 TO index[0] DO index[i]:=i;
  FOR i:=index[0]+1 TO 255 DO index[i]:=0;
  disabled:=false;
  bmppaint;
END;

PROCEDURE bmpfont.disable; BEGIN disabled:=true; END;
{******ENDE**************************Bitmap-Fontobjekt*******************************************************}

{************************************Bitmap-LCD-Zahl**********************************************************}
CONSTRUCTOR BMPLCD.Init(AParent: PWindowsObject; AnID,x0,y0,ni,lz:Integer;ABitMap: STRING);
VAR B: TBitMap;
    i:integer;
    nc:ARRAY[0..20] OF char;
BEGIN
  IF ni>10 THEN fail;
  strpcopy(nc,abitmap);
  BMP := LoadMyBitmap(hinstance,nc); IF BMP = 0 THEN Fail;
  GetObject(BMP,Sizeof(B), @B); W := B.bmWidth DIV 11; H := B.bmHeight;
  INHERITED Init(AParent, AnID,'', x0, y0, ni*W, H, 0);
  Attr.Style := Attr.Style OR bs_OwnerDraw;
  ATTR.ExStyle:=ws_ex_noparentnotify;
  n:=ni;m:=lz;zahl:='';FOR i:=1 TO n DO zahl:=zahl+' ';
  disabled:=true;
END;

DESTRUCTOR BMPLCD.Done;
BEGIN
  INHERITED Done;
  IF BMP <> 0 THEN DeleteMyBitmap(BMP);
END;

PROCEDURE BMPLCD.wmerasebkgnd;
VAR i:integer;
    memdc:hdc;
BEGIN
  IF BMP = 0 THEN Exit;
  IF NOT disabled THEN
  BEGIN
    memdc:=createcompatibledc(msg.wparam);
    IF PaletteInstalled THEN
    BEGIN
      SelectPalette(msg.wparam,hPal,false);
      SelectPalette(memdc,hPal,false);
    END;
    bmp:=selectobject(memdc,bmp);
    FOR i:=1 TO n DO
    IF Byte(zahl[i])>=47
    THEN bitblt(msg.wparam,w*(i-1),0,w,h,memdc,w*(Byte(zahl[i])-47),0,srccopy)
    ELSE bitblt(msg.wparam,w*(i-1),0,w,h,memdc,0,0,srccopy);
    bmp:=selectobject(memdc,bmp);
    deletedc(memdc);
  END;
  msg.result:=1;
END;

PROCEDURE BMPLCD.bmppaint;
VAR msg:tmessage;
    Rect:tRect;
BEGIN
  IF BMP = 0 THEN Exit;
  IF disabled THEN
  BEGIN
    WITH Attr DO SetRect(Rect,x,y,x+w,y+h);
    InvalidateRect(parent^.hWindow,@Rect,True)
  END ELSE
  BEGIN
    IF hWindow=0 THEN Exit;
    msg.wparam:=getdc(hwindow);
    wmerasebkgnd(msg);
    ReleaseDC(hwindow,msg.wparam);
  END;
END;

PROCEDURE BMPLCD.setbmpzahl(z:longint);
VAR s:STRING[10];
BEGIN
 str(z,s);
 IF m=_on THEN WHILE length(s)<n DO insert('0',s,1) ELSE WHILE length(s)<n DO insert('/',s,1);
 IF s<>zahl THEN BEGIN zahl:=s; bmppaint; END;
END;

FUNCTION BMPLCD.getbmpzahl:longint;
VAR TMP:Longint;
    i:byte;
BEGIN
 TMP:=0;
 FOR i:=1 TO n DO IF Zahl[i]>'/' THEN TMP:=10*TMP+(Byte(zahl[i])-48);
 getbmpzahl:=TMP;
END;

PROCEDURE bmpLCD.enable;
BEGIN
  disabled:=false;
  bmppaint;
END;

PROCEDURE bmplcd.disable; BEGIN disabled:=true; END;
{******ENDE**************************Bitmap-LCD-Zahl**********************************************************}

{*********************************Fileload-Mauscursor********************************************************}
CONSTRUCTOR waitcursor.init;
VAR i:integer;
BEGIN
  hwindow:=hpwindow;
  andplane:=memalloc(sizeof(plane));xorplane:=memalloc(sizeof(plane));
  FOR i:=0 TO 4 DO BEGIN andplane^[i]:=-1;xorplane^[i]:=0; END;
  FOR i:=7 TO 24 DO andplane^[i]:=0;
  FOR i:=27 TO 31 DO BEGIN andplane^[i]:=-1; xorplane^[i]:=0; END;
  xorplane^[07]:=$ffffffff;  xorplane^[24]:=$ffffffff;  xorplane^[05]:=$f8ffff1f;
  xorplane^[26]:=$f8ffff1f;  xorplane^[06]:=$feffff7f;  xorplane^[25]:=$feffff7f;
  andplane^[05]:=$070000e0;  andplane^[26]:=$070000e0;  andplane^[06]:=$01000080;
  andplane^[25]:=$01000080;  xorplane^[08]:=$b335e6bc;  xorplane^[09]:=$add45abb;
  xorplane^[10]:=$2fd55abb;  xorplane^[11]:=$a9d542bb;  xorplane^[12]:=$add55abb;
  xorplane^[13]:=$add55abb;  xorplane^[14]:=$b335da8c;  xorplane^[15]:=$FFFFFFFF;
  xorplane^[16]:=$FFFFFFFF;  xorplane^[17]:=$01000080;  xorplane^[23]:=$01000080;
  percnt:=1; old:=getcursor;
  cursor:=0;
  setpercnt(0);
END;
PROCEDURE waitcursor.setpercnt(x:longint);
VAR a:longint;
BEGIN
  x:=trunc(0.28*x)+2;IF x>15 THEN x:=($ffff shr (x-16)) ELSE x:=($ffffffff shr x);
  x:=(x OR $80000001) AND $bffffffd;
  IF percnt<>x THEN
  BEGIN
    cursor1:=cursor;
    a:=lo(loword(x));
    a:=(a shl 8)+hi(loword(x));
    a:=(a shl 8)+lo(hiword(x));
    a:=(a shl 8)+hi(hiword(x));
    xorplane^[18]:=a;
    xorplane^[19]:=a;
    xorplane^[20]:=a;
    xorplane^[21]:=a;
    xorplane^[22]:=a;
    cursor:=createcursor(hinstance,15,15,32,32,andplane,xorplane);
    setcursor(cursor);  percnt:=x;
    IF cursor1<>0 THEN destroycursor(cursor1);
  END;
END;
DESTRUCTOR waitcursor.done;
BEGIN
  setcursor(old); destroycursor(cursor);
  freemem(andplane,sizeof(plane));freemem(xorplane,sizeof(plane));
END;

{*******************TSTATIC************** mit eigenem FONT************************}
PROCEDURE tmystatic.setAssocWindow;
BEGIN
  assocwindow:=Awindow;
  retcode:=sendcode;
END;
{Hat der STATIC ein mit ihm verbundenes HWINDOW dann SETFOCUS darauf}
PROCEDURE tmystatic.wmnchittest;
BEGIN
 IF assocwindow=0 THEN defwndproc(msg) ELSE msg.result:=htclient;
END;
PROCEDURE tmystatic.wmlbuttondown;
BEGIN
  IF assocwindow=$FFFF THEN postmessage(parent^.hwindow,wm_selection,attr.id,retcode)
  ELSE IF assocwindow>0 THEN setfocus(assocwindow);
  defwndproc(msg);
END;

PROCEDURE tmystatic.setupwindow;
BEGIN
  assocwindow:=0;
  INHERITED setupwindow;
  {setze Font für OpendialogPFAD - fhandle ist global und wird bei Programmstart bereitgestellt}
  sendmessage(hwindow,wm_setfont,ansihandle,0);
END;

PROCEDURE tmystatic.setstringtext;
VAR pch:pstring;
BEGIN
  strdispose(attr.title);
  attr.title:=memalloc(length(astring)+2);
  strpcopy(attr.title,astring);
  INHERITED settext(attr.title);
END;

FUNCTION tmystatic.getstringtext;
BEGIN
  GetStringText:=strpas(attr.title);
END;
{Zentriertes Static*************************************************************}
CONSTRUCTOR tmycenterstatic.init;
BEGIN
  INHERITED init(Aparent,Anid,Atitle,x,y,w,h,atextlen);
  attr.style:=attr.style AND (NOT ss_left) OR ss_center;
END;

{************************EDIT-Eingabe-Listname*******************}

CONSTRUCTOR tmyedit.init;
BEGIN
   INHERITED init(aparent,anid,atitle,x,y,w,h,atextlen,false);
   attr.style:= ws_child OR ws_visible OR
                ws_group OR ws_tabstop OR
                es_left OR es_uppercase OR es_oemconvert OR es_autohscroll {OR ss_useritem};
   hasownfont:=ownfont;
END;
PROCEDURE tmyedit.setupwindow;
BEGIN
  INHERITED setupwindow;
  {setze Font für OpendialogEDIT - fhandle ist global und wird bei
  Programmstart bereitgestellt}
  IF hasownfont THEN sendmessage(hwindow,wm_setfont,ansihandle,0);
END;

{reagieren auf verschiedene Tasten wenn edit-control den Focus hat}
PROCEDURE tmyedit.wmchar;
BEGIN
  CASE msg.wparam OF
    vk_tab,vk_return,vk_execute,vk_escape:msg.result:=0;
    ELSE INHERITED wmchar(msg);
  END;
END;
PROCEDURE tmyedit.wmkeydown;
BEGIN
  msg.result:=0;
  IF keystate=1 THEN
  BEGIN msg.result:=sendmessage(parent^.hwindow,wm_keydown,msg.wparam,msg.lparam);END ELSE
  CASE msg.wparam OF
    vk_tab: setfocus(getnextdlgtabitem(parent^.hwindow,getfocus,keystate=2));
    vk_escape,vk_f1,vk_f2,vk_help,vk_return,vk_execute:
            msg.result:=sendmessage(parent^.hwindow,wm_keydown,msg.wparam,attr.id);
    ELSE defwndproc(msg);
  END;
END;
PROCEDURE tmyedit.wmsyskeydown;
BEGIN
  postmessage(parent^.hwindow,wm_syskeydown,msg.wparam,msg.lparam);
  msg.result:=0;
END;

PROCEDURE tmyedit.wmsetfocus;
BEGIN
  gettext(focusname,textlen);
  postmessage(parent^.hwindow,wm_ihavethefocus,hwindow,0);
  SendMessage(hWindow,em_setsel,0,MakeLong(0,$FFFF));
  defwndproc(msg);
END;

PROCEDURE tmyedit.setstringtext;
VAR x:pchar;
BEGIN
  getmem(x,length(AString)+1);
  strpcopy(x,AString);
  settext(x);
  freemem(x,length(AString)+1);
END;

{**********Edit-Box die bei Kill-Focus eine message sendet*****************}
PROCEDURE tmykfqedit.wmkillfocus;
BEGIN
  postmessage(parent^.hwindow,wm_selection,attr.id,vk_escape);
  defwndproc(msg);
END;
{*********EDIT für GRO?+KLEIN*************************}
CONSTRUCTOR tmyudedit.init;
BEGIN
   INHERITED init(aparent,anid,atitle,x,y,w,h,atextlen,false);
   attr.style:= attr.style AND not es_uppercase;
   hasownfont:=ownfont;
END;
{Datenbank-Edit}
CONSTRUCTOR tdbedit.init;
BEGIN
   inherited init(aparent,anid,atitle,x,y,w,h,atextlen,false);
   attr.style:= (ws_child OR ws_visible OR ws_group OR ws_tabstop OR
                es_left OR es_oemconvert OR es_autohscroll OR ss_useritem)
                and not es_uppercase;
   hasownfont:=ownfont;
END;

{reagieren auf verschiedene Tasten wenn edit-control den Focus hat}
PROCEDURE tdbedit.wmkeydown;
BEGIN
  CASE msg.wparam OF
    vk_tab    :  setfocus(getnextdlgtabitem(parent^.hwindow,getfocus,keystate=2));
    vk_escape,vk_f1,vk_f2,vk_help,vk_return,vk_execute,vk_up,vk_down,vk_prior,vk_next
              :  postmessage(parent^.hwindow,wm_keydown,msg.wparam,attr.id);
    vk_insert,vk_delete
              :  IF keystate=3 THEN postmessage(parent^.hwindow,wm_keydown,msg.wparam,attr.id)
                               ELSE inherited defwndproc(msg);
    vk_add,vk_subtract:IF keystate=0 THEN inherited defwndproc(msg) ELSE
                        postmessage(parent^.hwindow,wm_keydown,msg.wparam,attr.id);
     ELSE defwndproc(msg);
   END;
   msg.result:=0;
END;

PROCEDURE tdbedit.wmsyskeydown;
BEGIN
  IF (keystate=4) AND (msg.wparam=vk_back) THEN defwndproc(msg) ELSE
  BEGIN
    postmessage(parent^.hwindow,wm_syskeydown,msg.wparam,msg.lparam);
    msg.result:=0;
  END;
END;

PROCEDURE tdbedit.wmsetfocus;
BEGIN
  gettext(focusname,textlen);
  postmessage(parent^.hwindow,wm_ihavethefocus,hwindow,0);
  SendMessage(hWindow,em_setsel,0,MakeLong(0,$FFFF));
  defwndproc(msg);
END;

PROCEDURE tdbedit.wmkillfocus;
BEGIN
  setselection(0,0);
  defwndproc(msg);
END;
{****************************NR-EDIT-FELD*****************************}
CONSTRUCTOR tnredit.init;
BEGIN
   INHERITED init(aparent,anid,atitle,x,y,w,h,atextlen,false);
   attr.style:= ws_child OR ws_visible OR
                ws_group OR ws_tabstop OR
                es_left;
   hasownfont:=ownfont;
END;
PROCEDURE tnredit.wmkeydown;
BEGIN
  IF msg.wparam=vk_return THEN postmessage(parent^.hwindow,wm_command,attr.id,$00001000)
  ELSE inherited wmkeydown(msg);
END;

{*************Waitbox-Interface*******************************************}
FUNCTION initwaitbox(aparent:pwindowsobject;title:STRING):pwaitbox;
VAR ww:pwaitbox;
BEGIN
  ww:=new(pwaitbox,init(aparent,title));
  application^.makewindow(ww);
  updatewindow(ww^.hwindow);
  ww^.setpercnt(0);
  initwaitbox:=ww;
END;
PROCEDURE setwaitbox(p:pwaitbox;i:real);BEGIN p^.setpercnt(trunc(i));END;
PROCEDURE closewaitbox(p:pwaitbox);BEGIN  dispose(p,done);END;
{**********Waitbox-Interface Ende!*****************************+}

FUNCTION Key2String(key:Akey):STRING;
VAR s1,s:STRING[50];
    i:Byte;
BEGIN
  CASE key.State OF
    1:s:='<CTRL><';
    2:s:='<SHIFT><';
    3:s:='<CTRL><SHIFT><';
    4:s:='<ALT><';
    5:s:='<ALT><SHIFT><';
    6:s:='<CTRL><ALT><';
    7:s:='<CTRL><ALT><SHIFT><';
    ELSE s:='<';
  END;
  CASE key.KEY of
    VK_f1..VK_F16:BEGIN str(lo((key.key)-$6f),s1);s:=s+'F'+s1;END;
    VK_ESCAPE  :s:=s+'ESCAPE';
    VK_Tab     :s:=s+'TAB';
    VK_Pause   :s:=s+'PAUSE';
    VK_Capital :s:=s+'CAPS LOCK';
    VK_NumLock :s:=s+'NUM LOCK';
    $91        :s:=s+'SROLL LOCK';
    VK_BACK    :s:=s+'BACKSPACE';
    VK_SPACE   :s:=s+'SPACE';
    VK_PRIOR   :s:=s+'PAGE UP';
    VK_NEXT    :s:=s+'PAGE DOWN';
    VK_END     :s:=s+'END';
    VK_HOME    :s:=s+'HOME';
    VK_LEFT    :s:=s+'CURSOR LEFT';
    VK_RIGHT   :s:=s+'CURSOR RIGHT';
    VK_UP      :s:=s+'CURSOR UP';
    VK_DOWN    :s:=s+'CURSOR DOWN';
    VK_INSERT  :s:=s+'INSERT';
    VK_DELETE  :s:=s+'DELETE';
    vk_ADD     :s:=s+'NUMPAD PLUS';
    vk_SUBTRACT:s:=s+'NUMPAD MINUS';
    vk_MULTIPLY:s:=s+'NUMPAD MULTIPLY';
    vk_DIVIDE  :s:=s+'NUMPAD DIVIDE';
    vk_DECIMAL :s:=s+'NUMPAD SEPERATOR';
    vk_return  :s:=s+'ENTER';
    vk_numpad0..vk_numpad9:BEGIN str(lo((key.key)-$60),s1);s:=s+'NUMPAD '+s1;END;
    $2a,$BA..$C0,$DB..$E4,$E6..$F5:
      FOR i:=0 TO 255 DO IF VkKeyScan(i)=key.key THEN BEGIN s:=s+Char(i);Break;END;
    ELSE s:=s+CHAR(key.KEY);
  END;
  Key2String:=s+'>';
END;
{Tasten-Check!}
FUNCTION keystate;
VAR x:byte;
BEGIN
{keystate:   0..none
             1..control
             2..shift
             3..shift&control
             4..alt
             5..ctrl+alt
             6..ctrl+shift
             7..ctrl+shift+alt }
  IF hi(getkeystate(vk_control))>0 THEN x:=1 ELSE x:=0;
  IF hi(getkeystate(vk_shift)  )>0 THEN x:=x or 2;
  IF hi(getkeystate(vk_menu)   )>0 THEN x:=x or 4;
  keystate:=x;
END;
{*********************************Bitmapfunktionen***************************************}
CONST PiAlreadyChecked:Boolean=False;
      PiAlreadyCheckedResult:Boolean=False;
FUNCTION PaletteInstalled;
VAR DC:Word;
BEGIN
  IF NOT UsePalette THEN BEGIN PaletteInstalled:=False; Exit; END;
  IF PiAlreadyChecked THEN BEGIN PaletteInstalled:=PiAlreadyCheckedResult; Exit; END;
  DC:=GetDC(0);
  IF NumCols=0 THEN NumCols:=GetDeviceCaps(DC,SizePalette)-GetDeviceCaps(DC,NumReserved);
  PiAlreadyCheckedResult:=(GetDeviceCaps(DC,RasterCaps)AND RC_PALETTE)>0;
  IF NumCols<21 THEN PiAlreadyCheckedResult:=False;
  ReleaseDC(0,DC);
  PiAlreadyChecked:=True;
  PaletteInstalled:=PiAlreadyCheckedResult;
END;

FUNCTION ChangeSingleColor(What:tPaletteEntry):Longint;
VAR Lum,j:Longint;
    Color:tPaletteEntry;
BEGIN
  WITH What DO
  BEGIN
    Lum:=((Longint(peRed)*30+peGreen*59+peBlue*11)DIV 100-128)*(100-ColorDepth);
    j:=((LongInt((peRed)-128)*ColorDepth+Lum)*Contrast DIV 100+12800)*Brightness DIV 10000;
    IF j<0 THEN j:=0 ELSE if j>255 THEN j:=255; Color.peRed  :=j;
    j:=((LongInt((peGreen)-128)*ColorDepth+Lum)*Contrast DIV 100+12800)*Brightness DIV 10000;
    IF j<0 THEN j:=0 ELSE if j>255 THEN j:=255; Color.peGreen:=j;
    j:=((LongInt((peBlue)-128)*ColorDepth+Lum)*Contrast DIV 100+12800)*Brightness DIV 10000;
    IF j<0 THEN j:=0 ELSE if j>255 THEN j:=255; Color.peBlue :=j;
  END;
  Color.peFlags:=pc_Reserved;
  ChangeSingleColor:=Longint(Color);
END;

PROCEDURE DoBrightNessContrastColor(DC:Word);
VAR i:byte;
BEGIN
  IF DC=0 THEN Exit;
  IF (ColorsUsed>0)THEN
  BEGIN
    FOR i:=0 TO ColorsUsed-1 DO Longint(RealColorTable[i]):=ChangeSingleColor(RefColorTable[i]);
    AnimatePalette(hPal,0,ColorsUsed,RealColorTable);
    i:=SelectPalette(DC,hPal,False);
    RealizePalette(DC);
  END;
END;

FUNCTION IncludeColor(Color:tColorRef):LongInt;
VAR i:Word;
BEGIN
  IF PaletteInstalled THEN
  BEGIN
    tPaletteEntry(Color).peFlags:=pc_Reserved;
    IF ColorsUsed>0 THEN
    FOR i:=0 TO ColorsUsed-1 DO IF Longint(Color)=Longint(RefColorTable[i])THEN
    BEGIN
      IncludeColor:=PaletteIndex(i);
      Exit;
    END;
    IF ColorsUsed>=NumCols THEN BEGIN IncludeColor:=Color;Exit;END;
    RefColorTable[ColorsUsed]:=tPaletteEntry(Color);
    IncludeColor:=PaletteIndex(ColorsUsed);
    AnimatePalette(hRefPal,0,ColorsUsed,RefColorTable);
    Inc(ColorsUsed);
  END ELSE
  IF UsePalette
  THEN IncludeColor:=$02000000+(ChangeSingleColor(tPaletteEntry(Color))AND $00ffffff)
  ELSE IncludeColor:=(Color)AND $00ffffff;
END;

{Das folgende dient zum laden von Color-Bitmaps...}
TYPE pBitMaps=^tBitMaps;
     tBitMapI=RECORD
                name:STRING[31]; {Resourcen-Name}
                handle:tHandle;  {handle des Bitmaps}
                LoadCount:Word;  {ist er 0, wird die Bitmap entladen}
              END;
     tBitMaps=ARRAY[1..1]OF tBitMapI;

CONST BitMapsLoaded:Word=0;
      hMem:Word=0;
      Bitmaps:pBitmaps=NIL;                         {Infos zu den Bitmaps um doppeltes laden zu verhindern}

PROCEDURE ReloadColors;
VAR i,j:Longint;
    BMHandle:tHandle;
    ColCount:Word;
    p:pByteArray;
    BiHeader:pBitMapInfoHeader ABSOLUTE p;
    BmInfo:pBitMapInfo ABSOLUTE p;
    DC:Word;
    RGBD:tPaletteEntry;
    NewHandle:hBitmap;
    BitMapName:pChar;
    BMP:tBitmap;
    BMPData:Pointer;
    FontFileName:ARRAY[0..8]OF Char;
    Rect:tRect ABSOLUTE FontFilename;

  PROCEDURE RePaint(What:pWindow);FAR;
  BEGIN
    WITH What^ DO IF (attr.style and ws_child)<>ws_child THEN
    BEGIN
      Invalidaterect(hWindow,NIL,true);
      UpdateWindow(hWindow);
      Validaterect(hWindow,NIL);
    END;
  END;

BEGIN
  ColorsUsed:=0;
  FOR i:=0 TO NumSysColors-1 DO
  BEGIN
    LoadString(hInstance,30001+i,@FontFileName,7);
    ASM
      {Convert String to Longint}
      mov cx,6
      xor si,si
      DB 66h; xor bx,bx
      mov bx,02h
      DB 66h; xor ax,ax
     @loop:
      DB 66h; shl bx,4
      mov al,fontfilename.byte.[si]
      sub al,30h
      cmp al,0Fh
      jbe  @0
      sub al,07h
      cmp al,0Fh
      jbe  @0
      sub al,20h
     @0:
      DB 66h; add bx,ax
      inc si
      loop @loop
      {Include Color in Colortable}
      DB 66h; push bx
      call includecolor
      mov di,word ptr i
      shl di,2
      mov word ptr cUp.byte.[di],ax
      mov word ptr cUp.byte.[di+2],dx
    END;
  END;
  deleteobject(BrEdit);
  deleteobject(BrStatic);
  deleteobject(BrInvStatic);
  deleteobject(BrListBox);
  deleteobject(BrGrayBox);
  deleteobject(BrScrollbar);
  deleteobject(BrBlack);
  BrEdit      :=CreateSolidBrush(cBrEdit);
  BrStatic    :=CreateSolidBrush(cBrStatic);
  BrInvStatic :=CreateSolidBrush(cBrInvStatic);
  BrListBox   :=CreateSolidBrush(cBrLB);
  BrGrayBox   :=CreateSolidBrush(cBrGrayBox);
  BrScrollbar :=CreateSolidBrush(cBrScrollbar);
  BrBlack     :=CreateSolidBrush(cBlack);
  DC:=GetDC(GetActiveWindow);
  FOR j:=1 TO BitmapsLoaded DO WITH Bitmaps^[j] DO
  BEGIN
    IF Length(Name)<>0 THEN BitMapName:=StrNew(@Name[1]) ELSE BitMapName:=MakeIntResource(Word((@Name[1])^));
    IF UsePalette THEN
    BEGIN
      BMHandle:=FindResource(hInstance,BitMapName,RT_BitMap);
      IF BMHandle=0 THEN
      BEGIN
        BMHandle:=FindResource(hLanguage,BitMapName,RT_BitMap);
        BMHandle:=LoadResource(hLanguage,BMHandle);
      END ELSE BMHandle:=LoadResource(hInstance,BMHandle);
      p:=LockResource(BMHandle);
      WITH BiHeader^,Bitmaps^[BitmapsLoaded],BmInfo^ DO
      BEGIN
        IF BiClrUsed=0 THEN ColCount:=1 SHL BiBitCount ELSE ColCount:=BiClrUsed;
        FOR i:=0 TO ColCount-1 DO WITH bmiColors[i] DO
        BEGIN
          RGBD.peRed  :=rgbRed;
          RGBD.peGreen:=rgbGreen;
          RGBD.peBlue :=rgbBlue;
          RGBD.peFlags:=0;
          IF PaletteInstalled THEN IncludeColor(tColorRef(RGBD)) ELSE
          BEGIN
            Longint(RGBD):=IncludeColor(tColorRef(RGBD));
            rgbRed  :=RGBD.peRed;
            rgbGreen:=RGBD.peGreen;
            rgbBlue :=RGBD.peBlue;
          END;
        END;
        IF PaletteInstalled THEN
        BEGIN
          SelectPalette(DC,hRefPal,false);
          RealizePalette(DC);
        END;
        NewHandle:=CreateDIBitmap(DC,BiHeader^,CBM_Init,@bmiColors[ColCount],BmInfo^,DIB_RGB_COLORS);
        WHILE UnLockResource(BMHandle) DO;
        FreeResource(BMHandle);
      END;
    END ELSE
    BEGIN
      NewHandle:=LoadBitmap(hInstance,BitMapName);
      IF NewHandle=0 THEN NewHandle:=LoadBitmap(hLanguage,BitMapName);
    END;
    IF Length(Name)<>0 THEN StrDispose(BitmapName);
    GetObject(handle,SizeOf(tBitmap),@BMP);
    IF BMP.BMPLanes<>1 THEN BMP.bmWidthBytes:=((Longint(BMP.BMPLanes)*BMP.bmWidth DIV 8)DIV 16+1)*16;
    BMPData:=GlobalallocPtr(gmem_moveable,Longint(BMP.bmWidthBytes)*BMP.bmHeight);
    i:=GetBitmapBits(NewHandle,Longint(BMP.bmWidthBytes)*BMP.bmHeight,BMPData);
    i:=SetBitmapBits(Handle,Longint(BMP.bmWidthBytes)*BMP.bmHeight,BMPData);
    GlobalFreePtr(BMPData);
    DeleteObject(NewHandle);
  END;
  IF PaletteInstalled THEN DoBrightnessContrastColor(DC);
  ReleaseDC(GetActiveWindow,DC);
  RePaint(pWindow(Application^.Mainwindow));
  Application^.Mainwindow^.ForEach(@RePaint);
END;

FUNCTION LoadMyBitmap(Instance:tHandle; BitMapName:pChar):hBitmap;
VAR i:integer;
    BMHandle:tHandle;
    ColCount:Word;
    p:pByteArray;
    BiHeader:pBitMapInfoHeader ABSOLUTE p;
    BmInfo:pBitMapInfo ABSOLUTE p;
    DC:Word;
    RGBD:tPaletteEntry;
BEGIN
  IF NOT TryToUsePalette THEN BEGIN LoadMyBitmap:=LoadBitmap(Instance,BitMapName); Exit; END;
  LoadMyBitmap:=0;
  DC:=GetDC(0); IF DC=0 THEN Exit;
  {Unterstützt Bildschirm Paletten?...Wenn nicht, einfach LoadBitmap aufrufen}
  IF HiWord(Longint(BitMapName))<>0 THEN Ansilowerbuff(BitMapName,Strlen(BitMapName));
  {Bitmap schon im Speicher?...Wenn ja, dann nur Handle zurückgeben.}
  IF BitMapsLoaded>0 THEN
  BEGIN
    IF HiWord(Longint(BitMapName))=0 THEN
    BEGIN
      FOR i:=1 TO BitMapsLoaded DO WITH Bitmaps^[i] DO
      IF Word((@Name[1])^)=LoWord(Longint(BitMapName)) THEN
      BEGIN
        Inc(LoadCount);
        LoadMyBitmap:=handle;
        ReleaseDC(0,DC);
        Exit;
      END;
    END ELSE
    FOR i:=1 TO BitMapsLoaded DO WITH Bitmaps^[i] DO IF Name=StrPas(BitMapName) THEN
    BEGIN
      Inc(LoadCount);
      LoadMyBitmap:=handle;
      ReleaseDC(0,DC);
      Exit;
    END;
  END;
  IF UsePalette THEN
  BEGIN
    {Resource finden, laden und im Speicher verankern}
    BMHandle:=FindResource(Instance,BitMapName,RT_BitMap);IF BMHandle<32 THEN
    BEGIN ReleaseDC(0,DC);Exit; END;
    BMHandle:=LoadResource(Instance,BMHandle); IF BMHandle<32  THEN
    BEGIN ReleaseDC(0,DC);Exit; END;
    p:=LockResource(BMHandle); IF p=NIL THEN BEGIN ReleaseDC(0,DC); FreeResource(BMHandle); Exit; END;
  END;
  {Speicherhandling für Bitmap-Info}
  IF BitMapsLoaded=0 THEN
  BEGIN
    hMem:=MyGlobalAlloc(gmem_nodiscard,8192);BitMaps:=GlobalLock(hMem);
    IF BitMaps=NIL THEN BEGIN ReleaseDC(0,DC);Exit; END;
  END ELSE
  IF GlobalSize(hMem)<(BitmapsLoaded+1)*SizeOf(tBitMapI) THEN
  BEGIN
    WHILE GlobalUnlock(hMem) DO;
    i:=MyGlobalReAlloc(hMem,GlobalSize(hMem)+16384,gmem_nodiscard);
    IF i=0 THEN BEGIN BitMaps:=GlobalLock(hMem); ReleaseDC(0,DC); Exit; END ELSE hMem:=i;
    BitMaps:=GlobalLock(hMem);
  END;
  Inc(BitMapsLoaded);
  WITH Bitmaps^[BitmapsLoaded] DO
  BEGIN
    IF HiWord(Longint(BitMapName))=0
    THEN BEGIN Word((@Name[1])^):=LoWord(Longint(BitMapName));Name[0]:=#0 END
    ELSE BEGIN Name:=StrPas(BitMapName); Name[Length(Name)+1]:=#0; END;
    LoadCount:=1;
    {Palettenhandling}
    IF UsePalette THEN WITH BiHeader^,BmInfo^ DO
    BEGIN
      {Neue Farben in Palette übernehmen...}
      IF BiClrUsed=0 THEN ColCount:=1 SHL BiBitCount ELSE ColCount:=BiClrUsed;
      FOR i:=0 TO ColCount-1 DO WITH bmiColors[i] DO
      BEGIN
        RGBD.peRed  :=rgbRed;
        RGBD.peGreen:=rgbGreen;
        RGBD.peBlue :=rgbBlue;
        RGBD.peFlags:=0;
        IF PaletteInstalled THEN IncludeColor(tColorRef(RGBD)) ELSE
        BEGIN
          Longint(RGBD):=IncludeColor(tColorRef(RGBD));
          rgbRed  :=RGBD.peRed;
          rgbGreen:=RGBD.peGreen;
          rgbBlue :=RGBD.peBlue;
        END;
      END;
      {Palette aktualisieren...}
      IF PaletteInstalled THEN
      BEGIN
        SelectPalette(DC,hRefPal,false);
        RealizePalette(DC);
      END;
      Handle:=CreateDIBitmap(DC,BiHeader^,CBM_Init,@bmiColors[ColCount],BmInfo^,DIB_RGB_COLORS);
      LoadMyBitmap:=Handle;
      WHILE UnLockResource(BMHandle) DO;
      FreeResource(BMHandle);
      IF PaletteInstalled THEN DoBrightNessContrastColor(DC);
    END ELSE
    BEGIN
      Handle:=LoadBitmap(Instance,BitMapName);
      LoadMyBitmap:=Handle;
    END;
  END;
  ReleaseDC(0,DC);
END;

FUNCTION DeleteMyBitMap(Hnd:tHandle):Bool;
VAR i:Word;
BEGIN
  DeleteMyBitMap:=false;
  IF BitMapsLoaded>0 THEN
  BEGIN
    FOR i:=1 TO BitMapsLoaded DO WITH Bitmaps^[i] DO IF Handle=Hnd THEN
    BEGIN
      Dec(LoadCount);
      IF LoadCount=0 THEN
      BEGIN
        DeleteObject(Handle);
        {Allockierten Speicher wieder freigeben...}
        IF i<BitMapsLoaded THEN move(Bitmaps^[i+1],Bitmaps^[i],(BitMapsLoaded-i)*Sizeof(tBitmapI));
        Dec(BitMapsLoaded);
        WHILE GlobalUnlock(hMem) DO;
        IF BitMapsLoaded=0 THEN GlobalFree(hMem) ELSE
        BEGIN
          i:=GlobalReAlloc(hMem,(BitmapsLoaded)*SizeOf(tBitMapI),gmem_nodiscard);
          IF i<>0 THEN hMem:=i;
          BitMaps:=GlobalLock(hMem);
        END;
      END ELSE DeleteMyBitMap:=True;
      Exit;
    END;
  END ELSE DeleteMyBitMap:=DeleteObject(Hnd);
END;

{Stellt Bitmapresource 'hbitmap' vom Offset x1,y1 (in der Bitmap) an der Stelle x,y (auf dem Bildschirm)
 mit der Größe dx,dy (alles in Pixeln!) auf dem Bildschirm dar.
 hbitmap muß zuvor mit Loadbitmap(..) geladen werden!}
FUNCTION blit(hwindow:hwnd;x,y,dx,dy,x1,y1:integer;bmhandle:hbitmap):bool;
VAR bmdc,memdc:hdc;
BEGIN
  IF hWindow=0 THEN Exit;
  bmdc:=getdc(hwindow);
  memdc:=createcompatibledc(bmdc);
  IF PaletteInstalled THEN
  BEGIN
    SelectPalette(bmdc,hPal,false);
    SelectPalette(memdc,hPal,false);
  END;
  selectobject(memdc,bmhandle);
  blit:=bitblt(bmdc,x,y,dx,dy,memdc,x1,y1,srccopy);
  deletedc(memdc);
  ReleaseDC(hwindow,bmdc);
END;

PROCEDURE frame3d(dc:hdc;x,y,dx,dy:integer;width,up_down:byte);
VAR i:integer;
    a,b:Longint;
    pa:hpen;
BEGIN
  IF PaletteInstalled THEN SelectPalette(dc,hPal,false);
  IF up_down=down THEN BEGIN a:=cDown; b:=cUp; END ELSE BEGIN a:=cUp; b:=cDown; END;
  pa:=selectobject(dc,createpen(ps_solid,1,a));
  FOR i:=0 TO width-1 DO
  BEGIN  moveto(dc,x+dx-1-i,y+i);lineto(dc,x+i,y+i);lineto(dc,x+i,y+dy-1-i); END;
  deleteobject(selectobject(dc,createpen(ps_solid,1,b)));
  FOR i:=0 TO width-1 DO
  BEGIN moveto(dc,x+i,y+dy-1-i);lineto(dc,x+dx-1-i,y+dy-1-i);lineto(dc,x+dx-1-i,y+i);END;
  deleteobject(selectobject(dc,createpen(ps_solid,1,cForeGnd)));
  moveto(dc,x+dx-1,y);lineto(dc,x+dx-1-width,y+width); moveto(dc,x,y+dy-1);lineto(dc,x+width,y+dy-1-width);
  deleteobject(selectobject(dc,pa));
END;

PROCEDURE framefilled3d(dc:hdc;x,y,dx,dy:integer;width:byte;color:longint;up_down:byte);
VAR i:integer;
    a,b:Longint;
    pa:hpen;
    rect:trect;
BEGIN
  IF PaletteInstalled THEN SelectPalette(dc,hPal,false);
  IF up_down=down THEN BEGIN a:=cDown; b:=cUp; END ELSE BEGIN a:=cUp; b:=cDown; END;
  pa:=selectobject(dc,createpen(ps_solid,1,a));
  FOR i:=0 TO width-1 DO BEGIN moveto(dc,x+dx-1-i,y+i);lineto(dc,x+i,y+i);lineto(dc,x+i,y+dy-1-i); END;
  deleteobject(selectobject(dc,createpen(ps_solid,1,b)));
  FOR i:=0 TO width-1 DO
  BEGIN
    moveto(dc,x+i,y+dy-1-i);lineto(dc,x+dx-1-i,y+dy-1-i);lineto(dc,x+dx-1-i,y+i);
  END;
  deleteobject(selectobject(dc,createpen(ps_solid,1,cForeGnd)));
  moveto(dc,x+dx-1,y); lineto(dc,x+dx-1-width,y+width); moveto(dc,x,y+dy-1); lineto(dc,x+width,y+dy-1-width);
  deleteobject(selectobject(dc,pa));
  pa:=createsolidbrush(color);
  rect.left:=x+width; rect.right:=x+dx-width; rect.bottom:=y+dy-width; rect.top:=y+width; fillrect(dc,rect,pa);
  deleteobject(pa);
END;

PROCEDURE Grayframefilled3d(dc:hdc;x,y,dx,dy:integer;width:byte;color:longint;up_down:byte);
VAR i:integer;
    a,b:Longint;
    pa:hpen;
    rect:trect;
BEGIN
  IF up_down=down THEN
  BEGIN a:=rgb(128,128,128); b:=rgb(255,255,255); END ELSE BEGIN a:=rgb(255,255,255); b:=rgb(128,128,128); END;
  pa:=selectobject(dc,createpen(ps_solid,1,a));
  FOR i:=0 TO width-1 DO BEGIN  moveto(dc,x+dx-1-i,y+i);lineto(dc,x+i,y+i);lineto(dc,x+i,y+dy-1-i); END;
  deleteobject(selectobject(dc,createpen(ps_solid,1,b)));
  FOR i:=0 TO width-1 DO BEGIN moveto(dc,x+i,y+dy-1-i);lineto(dc,x+dx-1-i,y+dy-1-i);lineto(dc,x+dx-1-i,y+i);END;
  deleteobject(selectobject(dc,createpen(ps_solid,1,rgb(192,192,192))));
  moveto(dc,x+dx-1,y); lineto(dc,x+dx-1-width,y+width); moveto(dc,x,y+dy-1); lineto(dc,x+width,y+dy-1-width);
  deleteobject(selectobject(dc,pa));
  pa:=createsolidbrush(color);
  rect.left:=x+width; rect.right:=x+dx-width; rect.bottom:=y+dy-width; rect.top:=y+width;fillrect(dc,rect,pa);
  deleteobject(pa);
END;
{*********************************Bitmapfunktionen****ENDE***************************************************}

{*************Messagebox-Window*********************************}
TYPE PModalWindow=^TModalwindow;
     TModalWindow=OBJECT(Tmywindow)
       Aboxtext:STRING;
       answer:plongint;
       defanswer:longint;
       iconbmp:hbitmap;
       bok,byes,bno,bcancel,bretry:pmsgbutton;
       lines:integer;
       textw,texth:integer;
       buttonw,buttonh,nButtons:integer;
       Iconw,Iconh:integer;
       mainw,mainh:integer;
       parentwasdisabled:boolean;
       CONSTRUCTOR init(aparent:pwindowsobject;atitle,atext:STRING;stil:plongint);
       PROCEDURE setupwindow;VIRTUAL;
       PROCEDURE WMEraseBkGnd(VAR msg:tmessage);  VIRTUAL wm_first+wm_EraseBkGnd;
       PROCEDURE ok(VAR msg:tmessage);VIRTUAL     id_first+id_ok;
       PROCEDURE cancel(VAR msg:tmessage);VIRTUAL id_first+id_cancel;
       PROCEDURE yes(VAR msg:tmessage);VIRTUAL    id_first+id_yes;
       PROCEDURE no(VAR msg:tmessage);VIRTUAL     id_first+id_no;
       PROCEDURE retry(VAR msg:tmessage);VIRTUAL  id_first+id_retry;
       DESTRUCTOR done; VIRTUAL;
       PROCEDURE wmkeydown(VAR msg:tmessage);     VIRTUAL wm_first+wm_keydown;
     END;

CONSTRUCTOR TModalWindow.init;
VAR nc:ARRAY[0..100] OF char;
    bmp:tbitmap;
BEGIN
  lines:=0;textw:=0;texth:=0;buttonw:=0;buttonh:=0;nButtons:=0;
  Iconw:=0;Iconh:=0;mainw:=0;mainh:=0;
  bok:=NIL;bcancel:=NIL;bno:=NIL;byes:=NIL;bretry:=NIL;
  aboxtext:=atext;
  answer:=stil;
  strpcopy(@nc,atitle);
  INHERITED init(aparent,@nc,ws_sysmenu,notopmost);
  WITH attr DO
  BEGIN
    style:=ws_popup OR ws_visible OR ws_border;
    exstyle:=ws_ex_dlgmodalframe;
    x:=0;
    y:=0;
    w:=50;
    h:=100;
  END;
  IF (lo(loword(stil^))AND$f0)<>0 THEN messagebeep((lo(loword(stil^))AND$f0));
  CASE (lo(loword(stil^))AND$f0) OF
    mb_iconstop       :iconbmp:=LoadBitmap(hinstance,'mb_iconstop');
    mb_iconquestion   :iconbmp:=LoadBitmap(hinstance,'mb_iconquestion');
    mb_iconexclamation:iconbmp:=LoadBitmap(hinstance,'mb_iconexclamation');
    mb_iconasterisk   :iconbmp:=LoadBitmap(hinstance,'mb_iconasterisk');
    ELSE iconbmp:=0;
  END;
  IF iconbmp<>0 THEN
  BEGIN
    getobject(iconbmp,sizeof(bmp),@bmp);
    iconw:=bmp.bmwidth;
    iconh:=bmp.bmheight;
  END;
  CASE (lo(loword(stil^))AND$0F) OF
    mb_okcancel:
    BEGIN
      nbuttons:=2;
      bok:=new(pmsgbutton,init(@self,id_ok,0,0,'MB_OK',false));
      buttonw:=bok^.attr.w;
      buttonh:=bok^.attr.h;
      bcancel:=new(pmsgbutton,init(@self,id_cancel,0,0,'MB_Cancel',false));
      defanswer:=id_Cancel;
    END;
    mb_yesnocancel:
    BEGIN
      nbuttons:=3;
      byes:=new(pmsgbutton,init(@self,id_yes,0,0,'MB_YES',false));
      buttonw:=byes^.attr.w;
      buttonh:=byes^.attr.h;
      bno:=new(pmsgbutton,init(@self,id_no,0,0,'MB_NO',false));
      bcancel:=new(pmsgbutton,init(@self,id_cancel,0,0,'MB_Cancel',false));
      defanswer:=id_Cancel;
    END;
    mb_yesno:
    BEGIN
      nbuttons:=2;
      byes:=new(pmsgbutton,init(@self,id_yes,0,0,'MB_YES',false));
      buttonw:=byes^.attr.w;
      buttonh:=byes^.attr.h;
      bno:=new(pmsgbutton,init(@self,id_no,0,0,'MB_NO',false));
      defanswer:=id_no;
    END;
    mb_retrycancel:
    BEGIN
      nbuttons:=2;
      bretry:=new(pmsgbutton,init(@self,id_retry,0,0,'MB_RETRY',false));
      buttonw:=bretry^.attr.w;
      buttonh:=bretry^.attr.h;
      bcancel:=new(pmsgbutton,init(@self,id_cancel,0,0,'MB_Cancel',false));
      defanswer:=id_Cancel;
    END;
    ELSE
    BEGIN
      nbuttons:=1;
      bok:=new(pmsgbutton,init(@self,id_ok,0,0,'MB_OK',false));
      buttonw:=bok^.attr.w;
      buttonh:=bok^.attr.h;
      defanswer:=id_ok;
    END;
  END;
END;

PROCEDURE TModalwindow.setupwindow;
VAR
    i,j,k,buttx,butty,buttdiff:integer;
    rest:STRING;
BEGIN
  INHERITED setupwindow;
  textw:=0;
  lines:=0;
  i:=1;
  rest:=aboxtext;
  WHILE i<>0 DO
  BEGIN
    inc(lines);
    i:=pos(#10,rest);
    IF textw<(i-1)*Fw THEN textw:=(i-1)*Fw;
    delete(rest,1,i);
  END;
  IF (length(rest)>0)AND(textw<length(rest)*Fw) THEN textw:=length(rest)*Fw;
  inc(textw,14);
  texth:=lines*fh;
  WITH attr DO
  BEGIN
    IF (iconw<>0)AND(textw<>0) THEN
    BEGIN
      IF (textw+15+iconw)>((5+buttonw)*nbuttons+5)
       THEN mainw:=textw+iconw+21 ELSE mainw:=(5+buttonw)*nbuttons+11;
    END ELSE IF iconw<>0 THEN
    BEGIN
      IF (10+iconw)>((5+buttonw)*nbuttons+5)
       THEN mainw:=iconw+16 ELSE mainw:=(5+buttonw)*nbuttons+11;
    END ELSE
    BEGIN
      IF (textw+10)>((5+buttonw)*nbuttons+5)
       THEN mainw:=textw+16 ELSE mainw:=(5+buttonw)*nbuttons+11;
    END;
    w:=mainw+2*getsystemmetrics(sm_cxdlgframe)+2*getsystemmetrics(sm_cxborder);
    IF texth>iconh THEN h:=texth ELSE h:=iconh;
    mainh:=h+27+buttonh;
    h:=mainh+capdy+(2*getsystemmetrics(sm_cydlgframe));
    x:=((GetSystemMetrics(sm_cxscreen)-w) DIV 2);
    y:=((getsystemmetrics(sm_cyscreen)-h) DIV 2);
    movewindow(hwindow,x,y,w,h,true);
    butty:=CapDY+mainh-buttonh-8;
    CASE nbuttons OF
     1:buttx:=(mainw-buttonw)DIV 2;
     2:BEGIN buttdiff:=(mainw-11)DIV 2;buttx:=(mainw-buttonw-buttdiff)DIV 2; END;
     3:BEGIN buttdiff:=(mainw-11)DIV 3;buttx:=(mainw-buttonw)DIV 2-buttdiff; END;
    END;
    IF bok<>NIL THEN WITH bok^ DO
    BEGIN
      attr.x:=buttx; attr.y:=butty; movewindow(hwindow,attr.x,attr.y,attr.w,attr.h,true);
      enable; buttx:=buttx+buttdiff;
      oldfocus:=hwindow;
    END;
    IF byes<>NIL THEN WITH byes^ DO
    BEGIN
      attr.x:=buttx; attr.y:=butty; movewindow(hwindow,attr.x,attr.y,attr.w,attr.h,true);
      enable; buttx:=buttx+buttdiff;
      oldfocus:=hwindow;
    END;
    IF bno<>NIL THEN WITH bno^ DO
    BEGIN
      attr.x:=buttx; attr.y:=butty; movewindow(hwindow,attr.x,attr.y,attr.w,attr.h,true);
      enable; buttx:=buttx+buttdiff;
    END;
    IF bretry<>NIL THEN WITH bretry^ DO
    BEGIN
      attr.x:=buttx; attr.y:=butty; movewindow(hwindow,attr.x,attr.y,attr.w,attr.h,true);
      enable; buttx:=buttx+buttdiff;
    END;
    IF bcancel<>NIL THEN WITH bcancel^ DO
    BEGIN
      attr.x:=buttx; attr.y:=butty; movewindow(hwindow,attr.x,attr.y,attr.w,attr.h,true);
      enable; buttx:=buttx+buttdiff;
    END;
  END;
  IF parent<>NIL THEN
  BEGIN
    IF iswindowenabled(pwindow(parent)^.hwindow) THEN  parentwasdisabled:=false
                                                 ELSE  parentwasdisabled:=true;
    IF NOT parentwasdisabled THEN enablewindow(parent^.hwindow,false);
  END;
END;

PROCEDURE TModalwindow.WMEraseBkGnd(VAR msg:tmessage);
VAR memdc:hdc;
    i:integer;
    s,s1:STRING;
    pc:pchar;
BEGIN
  paintcaption(msg.wParam);
  Grayframefilled3d(msg.wParam,0,CapDY,mainw,mainh-2,2,rgb(192,192,192),up);
  Grayframefilled3d(msg.wParam,6,CapDY+6,mainw-12,mainh-19-buttonh,1,rgb(192,192,192),down);
  IF iconbmp<>0 THEN
  BEGIN
    {Grayframefilled3d(msg.wParam,9,CapDY+(mainh-buttonh-7-iconh)DIV 2-1,iconw+2,iconh+2,1
                            ,rgb(192,192,192),up);{}
    memdc:=createcompatibledc(msg.wParam);
    selectobject(memdc,iconbmp);
    bitblt(msg.wParam,10,CapDY+(mainh-buttonh-7-iconh)DIV 2,iconw,iconh,memdc,0,0,srccopy);
    deletedc(memdc);
  END ELSE
  setbkmode(msg.wParam,transparent);
  settextalign(msg.wParam,ta_center OR ta_top);
  settextcolor(msg.wParam,0);
  memdc:=selectobject(msg.wParam,ansiHandle);
  s:=aboxtext;
  pc:=memalloc(256);
  FOR i:=0 TO lines-1 DO
  BEGIN
    IF pos(#10,s)<>0 THEN
    BEGIN
      s1:=copy(s,1,pos(#10,s)-1);
      delete(s,1,pos(#10,s));
    END ELSE s1:=s;
    strpcopy(pc,s1);
    IF iconw<>0 THEN
     textout(msg.wParam,(mainw+iconw+5) DIV 2,CapDY+(mainh-buttonh-7-texth)DIV 2+i*fh,pc,length(s1))
    ELSE
     textout(msg.wParam,mainw DIV 2,CapDY+(mainh-buttonh-7-texth)DIV 2+i*fh,pc,length(s1))
  END;
  selectobject(msg.wParam,memdc);
  freemem(pc,256);
  iNHERITED WmEraseBkGnd(MSG);
END;

PROCEDURE TModalWindow.ok(VAR msg:tmessage);
BEGIN answer^:=id_ok;    postmessage(hwindow,wm_close,0,0);END;
PROCEDURE TModalWindow.cancel(VAR msg:tmessage);
BEGIN answer^:=id_cancel;postmessage(hwindow,wm_close,0,0);END;
PROCEDURE TModalWindow.yes(VAR msg:tmessage);
BEGIN answer^:=id_yes;   postmessage(hwindow,wm_close,0,0);END;
PROCEDURE TModalWindow.no(VAR msg:tmessage);
BEGIN answer^:=id_no;    postmessage(hwindow,wm_close,0,0);END;
PROCEDURE TModalWindow.retry(VAR msg:tmessage);
BEGIN answer^:=id_retry; postmessage(hwindow,wm_close,0,0);END;

DESTRUCTOR TModalWindow.done;
BEGIN
  IF iconbmp<>0 THEN DeleteMyBitmap(iconbmp);
  IF answer^=0 THEN answer^:=DEFANSWER;
  postappmessage(getcurrenttask,19993,$1234,$12344321);
  IF (parent<>NIL) THEN enablewindow(parent^.hwindow,NOT parentwasdisabled);
  twindow.done;
END;
PROCEDURE tmodalwindow.wmkeydown(VAR msg:tmessage);
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    vk_execute,vk_return:msg.result:=0;
    vk_space:sendmessage(oldfocus,wm_char,vk_return,0);
    vk_escape:IF bno<>NIL THEN no(msg) ELSE cancel(msg);
    ELSE defwndproc(msg);
  END;
END;
{************************ENde MEssagebox*********************************}

{***********************************Waitbox-Fenster****************************************}
CONSTRUCTOR waitbox.init;
BEGIN
  oldcursor:=setcursor(loadcursor(0,idc_wait));
  INHERITED init(aparent,NIL);
  title:=comment;
  prcnt:=0;
  WITH attr DO
  BEGIN
    style:=ws_popup OR ws_visible;
    ExStyle:=ws_ex_noparentnotify OR ws_ex_dlgmodalframe;
  END;
END;
PROCEDURE waitbox.setupwindow;
BEGIN
  IF title[length(title)]<>#0 THEN title:=title+#0;
  w_wait:=strlen(@title[1])*fw+20;
  h_wait:=27+2*fh;
  WITH attr DO
  BEGIN
    w:=2+w_wait+2*getsystemmetrics(sm_cxdlgframe);
    h:=1+h_wait+2*getsystemmetrics(sm_cydlgframe);
    y:=(getsystemmetrics(sm_cyscreen)-h)DIV 2;
    x:=(GetSystemMetrics(sm_cxscreen)-w)DIV 2;
  END;
  xt:=(w_wait DIV 2);
  yt:=5;
  xb:=5;
  yb:=10+fh;
  wb:=w_wait-10;
  hb:=fh+12;
  xp:=attr.w DIV 2 -((5*fw) DIV 2)-getsystemmetrics(sm_cxdlgframe);
  yp:=yb+4;
  WITH attr DO movewindow(hwindow,x,y,w,h,true);
END;
DESTRUCTOR waitbox.done;
BEGIN
  setcursor(oldcursor);
  INHERITED done;
END;
PROCEDURE waitbox.WmQueryNewPalette(VAR MSG:tMessage);
BEGIN
  pMyWindow(Application^.mainwindow)^.WmQueryNewPalette(MSG);
END;
PROCEDURE waitbox.setpercnt(i:byte);
VAR adc:hdc;
BEGIN
  IF i<>prcnt THEN
  BEGIN
    prcnt:=i;
    IF hWindow=0 THEN Exit;
    adc:=getdc(hwindow);
    paintprcnt(adc);
    ReleaseDC(hwindow,adc);
  END;
END;
PROCEDURE waitbox.wmclose;BEGIN msg.result:=0;END;
PROCEDURE waitbox.paintprcnt(adc:hdc);
VAR oldfont:hFont;
    s:STRING[6];
BEGIN
  oldfont:=selectobject(adc,ansiHandle);
  Grayframefilled3d(adc,xb,yb,wb,hb,1,rgb(192,192,192),down);
  Grayframefilled3d(adc,xb+1,yb+1,xb+1+trunc(1.0*(wb-2)*prcnt/100)-xb-1,hb-2,2,rgb(192,192,192),up);
  str(prcnt:3,s);s:=s+' %'+#0;
  settextalign(adc,ta_top OR ta_left);
  settextcolor(adc,RGB(128,128,128));
  setbkcolor(adc,RGB(192,192,192));
  exttextout(adc,xp+4,yp+3,0,NIL,pchar(@s[1]),5,NIL);
  settextcolor(adc,RGB(0,0,0));
  setbkmode(adc,transparent);
  textout(adc,xp,yp,@s[1],5);
  selectobject(adc,oldfont);
END;
PROCEDURE waitbox.wmerasebkgnd;
VAR oldfont:hFont;
BEGIN
  oldfont:=selectobject(msg.wparam,ansiHandle);
  grayframefilled3d(msg.wparam,0,0,attr.w,attr.h,2,rgb(192,192,192),up);
  settextalign(msg.wparam,ta_top OR ta_center);
  settextcolor(msg.wparam,RGB(0,0,0));
  setbkcolor(msg.wparam,RGB(192,192,192));
  exttextout(msg.wparam,xt,yt,0,NIL,pchar(@title[1]),strlen(@title[1]),NIL);
  selectobject(msg.wparam,oldfont);
  paintprcnt(msg.wparam);
END;

{**********************************COLORSELECTOR-FENSTER*****************************************************}
CONSTRUCTOR tPictureSetup.INIT;
BEGIN
  INHERITED init(application^.mainwindow,'');
  pMySelfInParent:=pMySelf;
  Attr.w:=136;
  Attr.h:=75;
  Attr.x:=pMyWindow(Application^.MainWindow)^.attr.x;
  Attr.y:=pMyWindow(Application^.MainWindow)^.attr.y-attr.h+1;
  Attr.style:=ws_popup OR ws_visible OR ws_border;
  hDrehKnopp:=LoadBitMap(hInstance,'DrehKnopp');
  xmbri:=21;
  ymbri:=18;
  xmcon:=65;
  ymcon:=18;
  xmcol:=109;
  ymcol:=18;
  radius:=13;
  bReset  :=New(PbmpCaptBtn, Init(@self, 100, 0,58,'breset'  ,false));bReset^.Disabled  :=False;
  bDefault:=New(PbmpCaptBtn, Init(@self, 101,45,58,'bdefault',false));bDefault^.Disabled:=False;
  bDisable:=New(Pbmp3CaptBtn,Init(@self, 102,90,58,'benable' ,false));bDisable^.Disabled:=False;
  bDisAble^.Active:=NOT UsePalette;
END;

FUNCTION  tPictureSetup.GetClassName:PChar;
BEGIN
  GetClassName:='PictureSetupDlg';
END;
PROCEDURE tPictureSetup.GetWindowClass(VAR WndClass: TWndClass);
BEGIN
  INHERITED GetWindowClass(WndClass);
  wndclass.HCursor:=0;
END;

DESTRUCTOR tPictureSetup.DONE;
BEGIN
  DeleteObject(hDrehKnopp);
  INHERITED Done;
  Pointer(pMySelfInParent^):=NIL;
END;

PROCEDURE tPictureSetup.HandleReset(VAR Msg:tMessage);
BEGIN
  SetFocus(Application^.Mainwindow^.hWindow);
  IF UsePalette THEN
  BEGIN
    Brightness:=GetPrivateProfileInt(s_state,'Brightness',100,INIDIR);
    IF(Brightness<0)OR(Brightness>200)THEN Brightness:=100;
    Contrast:=GetPrivateProfileInt(s_state,'Contrast',100,INIDIR);
    IF(Contrast<0)OR(Contrast>200)THEN Contrast:=100;
    ColorDepth:=GetPrivateProfileInt(s_state,'ColorDepth',100,INIDIR);
    IF(ColorDepth<0)OR(ColorDepth>200)THEN ColorDepth:=100;
    IF PaletteInstalled THEN
    BEGIN
      msg.wparam:=GetDC(hWindow);
      WmEraseBkgnd(msg);
      DoBrightnessContrastColor(msg.wParam);
      ReleaseDC(hWindow,msg.wparam);
    END ELSE ReloadColors;
  END;
END;

PROCEDURE tPictureSetup.HandleDefault(VAR Msg:tMessage);
BEGIN
  SetFocus(Application^.Mainwindow^.hWindow);
  IF UsePalette THEN
  BEGIN
    Brightness:=100;
    Contrast:=100;
    ColorDepth:=100;
    IF PaletteInstalled THEN
    BEGIN
      msg.wparam:=GetDC(hWindow);
      WmEraseBkgnd(msg);
      DoBrightnessContrastColor(msg.wParam);
      ReleaseDC(hWindow,msg.wparam);
    END ELSE ReloadColors;
  END;
END;

PROCEDURE tPictureSetup.HandleDisable(VAR Msg:tMessage);
VAR Pal,dc:Word;
BEGIN
  bDisable^.SetStyle(Byte(UsePalette));
  UsePalette:=NOT(UsePalette);
  SetFocus(Application^.Mainwindow^.hWindow);
  ReloadColors;
END;

PROCEDURE tPictureSetup.DrawKnopp(aDC:hDC;Value,xmiddle,ymiddle:integer);
VAR DC:Word;
    y,x:Integer;
BEGIN
  IF hWindow=0 THEN Exit;
  IF aDC=0 THEN DC:=GetDC(hWindow) ELSE DC:=aDC;
  y:=ymiddle-round(radius*sin((5-Value*6/200)*Pi/4));
  x:=xmiddle+round(radius*cos((5-Value*6/200)*Pi/4));
  BitBlt(DC,x,y,3,3,0,0,0,blackness);
  IF aDC=0 THEN ReleaseDC(hWindow,DC);
END;

PROCEDURE tPictureSetup.WMEraseBkGnd(VAR MSG:tMessage);
VAR MemDC:Word;
BEGIN
  GrayFrameFilled3D(msg.wparam,0,0,attr.w-2,attr.h-17,1,rgb(192,192,192),up);
  memdc:=createcompatibledc(msg.wparam);
  selectobject(memdc,hDrehKnopp);
  bitblt(msg.wparam,  5, 3,34,34,memdc, 0, 0,srccopy);
  DrawKnopp(msg.wparam,Brightness,xmbri,ymbri);
  bitblt(msg.wparam, 49, 3,34,34,memdc, 0, 0,srccopy);
  DrawKnopp(msg.wparam,Contrast,xmcon,ymcon);
  bitblt(msg.wparam, 93, 3,34,34,memdc, 0, 0,srccopy);
  DrawKnopp(msg.wparam,ColorDepth,xmcol,ymcol);
  bitblt(msg.wparam, 14,39,15,15,memdc, 1,34,srccopy);
  bitblt(msg.wparam, 58,39,15,15,memdc,17,34,srccopy);
  bitblt(msg.wparam,102,39,15,15,memdc,33,34,srccopy);
  deletedc(memdc);
  bReset^.Enable;
  bDefault^.Enable;
  bDisable^.Enable;
  validaterect(hwindow,NIL);
  msg.result:=1;
END;

PROCEDURE tPictureSetup.WMSetCursor(VAR MSG:tMessage);
VAR InBri,InCon,InCol:Boolean;
    pt,pt1:tPoint;
    rect:tRect;
  FUNCTION IsInKnopp(xm,ym:integer):Boolean;
  BEGIN
    IsInKnopp:=Sqrt((pt.x-xm)*(pt.x-xm)+(pt.y-ym)*(pt.y-ym))<=(Radius+4);
  END;
  PROCEDURE SetKnopp(xm,ym,xofs,yofs:Integer;VAR Value:integer);
  VAR arc:Real;
      memDC,dc:Word;
  BEGIN
    WITH pt DO IF (xm<>x)AND(ym<>y)THEN
    BEGIN
      IF hWindow=0 THEN Exit;
      dc:=GetDC(hWindow);
      {Winkel berechnen}
      arc:=arctan((y-ym)/(xm-x));
      IF ((x-xm)<0)AND((y-ym)<0)THEN {3.Q} arc:=pi+arc ELSE
      IF ((x-xm)<0)AND((y-ym)>0)THEN {2.Q} arc:=arc+pi;
      Value:=Round((5/4-arc/Pi)*400/3);
      IF Value<0 THEN Value:=0 ELSE IF Value>200 THEN Value:=200;
      memdc:=createcompatibledc(dc);
      selectobject(memdc,hDrehKnopp);
      bitblt(dc,xofs,yofs,34,34,memdc,(Value MOD 2)*34,0,srccopy);
      deleteDC(memDC);
      DrawKnopp(dc,Value,xm,ym);
      ReleaseDC(hWindow,dc);
      IF PaletteInstalled THEN
      BEGIN
        IF Parent^.hWindow=0 THEN Exit;
        dc:=GetDC(parent^.hWindow);
        DoBrightnessContrastColor(DC);
        ReleaseDC(parent^.hWindow,dc);
      END ELSE ReloadColors;
    END
  END;

BEGIN
  InBri:=False;
  InCon:=False;
  InCol:=False;
  GetCursorPos(pt);Dec(pt.x,attr.x+2);Dec(pt.y,attr.y+2);
  InBri:=IsInKnopp(xmbri,ymbri);
  InCon:=IsInKnopp(xmcon,ymcon);
  InCol:=IsInKnopp(xmcol,ymcol);
  pt.x:=0;
  pt.y:=0;
  IF UsePalette AND(GetActiveWindow=Parent^.hWindow)AND(InBri OR InCon OR InCol)THEN
  BEGIN
    SetCursor(LoadCursor(0,IDC_Cross));
    IF (GetAsyncKeyState(vk_lButton)AND $8000)>0 THEN
    BEGIN
      WHILE (GetAsyncKeyState(vk_lButton)AND $8000)>0 DO
      BEGIN
        GetCursorPos(pt1);Dec(pt1.x,attr.x+2);Dec(pt1.y,attr.y+2);
        IF (pt1.y<>PT.y)OR(pt1.x<>PT.x) THEN
        BEGIN
          pt:=pt1;
          IF InBri THEN SetKnopp(xmbri,ymbri, 5,3,Brightness) ELSE
          IF InCon THEN SetKnopp(xmcon,ymcon,49,3,Contrast  ) ELSE SetKnopp(xmcol,ymcol,93,3,ColorDepth);
        END ELSE BEGIN Application^.IdleAction; Yield; END;
      END;
    END;
  END ELSE SetCursor(LoadCursor(0,IDC_Arrow));
  msg.result:=0;
END;

PROCEDURE tPictureSetup.WmQueryNewPalette(VAR MSG:tMessage);
BEGIN pMyWindow(Application^.mainwindow)^.WmQueryNewPalette(MSG);END;
PROCEDURE tPictureSetup.WMKeyDown(VAR MSG:tMessage);
BEGIN
  msg.result:=SendMessage(parent^.hwindow,msg.message,msg.wparam,msg.lparam);
END;
PROCEDURE tPictureSetup.WMKeyUp(VAR MSG:tMessage);
BEGIN
  msg.result:=SendMessage(parent^.hwindow,msg.message,msg.wparam,msg.lparam);
END;
PROCEDURE tPictureSetup.WMSetFocus(VAR MSG:tMessage);
VAR pt:tPoint;
    wnd:hWnd;
BEGIN
  IF IsWindowEnabled(Parent^.hWindow) THEN
  BEGIN
    GetCursorPos(pt);
    Wnd:=WindowFromPoint(pt);
    IF (Wnd=hWindow)OR((bReset^.hWindow<>Wnd)AND(bDefault^.hWindow<>Wnd)AND(bDisable^.hWindow<>Wnd))
    THEN SetFocus(Application^.Mainwindow^.hWindow);
  END ELSE SetFocus(msg.wParam);
  msg.result:=0;
END;
{***ENDE***************************COLORSELECTOR-FENSTER*****************************************************}

{*******Messagebox-Interface***************************************************}
FUNCTION ourmessagebox(aparent:pwindowsobject;atitle,Atext:STRING;flags:longint):longint;
VAR wmsg:tmsg;
    pw:Pmodalwindow;
    stil:longint;
    ende:boolean;
    testlong:longint;
    {NEUNEU!!!!!!!!!!!1}

CONST    disabled=$80;

    PROCEDURE enableall(p,firstchild:pwindowsobject;enbl:boolean);
    BEGIN
      IF p^.childlist<>NIL THEN enableall(p^.childlist,p^.childlist,enbl);
      IF ((p^.next<>firstchild)AND(p^.next<>NIL)) THEN enableall(p^.next,firstchild,enbl);
      {Wenn Fenster Disabled werden sollen}
      IF NOT enbl THEN
      BEGIN
        testlong:=getwindowlong(p^.hwindow,gwl_style);
        IF (((testlong AND ws_disabled)<>ws_disabled) AND
           ((testlong AND ws_child)<>ws_child)) THEN
        BEGIN
          {Im ATTR merken das es vor MSG-BOX enabled war}
          p^.setflags(disabled,true);
          {Fenster disablen}
          IF p<>aparent THEN enablewindow(p^.hwindow,false);
        END;
      END ELSE
      BEGIN
        {andernfalls wenn Ich den Stil geaendert habe ,wieder zurücksetzen}
        IF p^.isflagset(disabled) THEN
        BEGIN
          p^.setflags(disabled,false);
          IF p<>aparent THEN enablewindow(p^.hwindow,true);
        END;
      END;
    END;

BEGIN
  IF aparent<>NIL THEN enableall(application^.mainwindow,NIL,false);
  ende:=false;
  stil:=flags;
  pw:=new(Pmodalwindow,init(aparent,Atitle,atext,@stil));
  pw^.create;
  WHILE  NOT ende DO
  BEGIN
    IF PeekMessage(wmsg,0,0,0,pm_remove) THEN
    BEGIN
      IF ((wmsg.hwnd=0)
         AND(wmsg.lparam=$12344321)
         AND(wmsg.wparam=$1234)
         AND(wmsg.message=19993))
       THEN ende:=true ELSE
       BEGIN
         translatemessage(wmsg);
         dispatchmessage(wmsg);
       END;
     END ELSE IF aparent<>nil THEN pbuzapplication(application)^.idleaction;
  END;
  ourmessagebox:=stil;
  IF aparent<>NIL THEN enableall(application^.mainwindow,NIL,true);
END;

PROCEDURE ErrMessageBox;
BEGIN
  ourmessagebox(pwindow,errorver,getresstring(what),mb_ok OR mb_iconexclamation);
END;
PROCEDURE helpme(aparent:pwindowsobject;what:word);
VAR s:STRING[80];
    rec:tsearchrec;
BEGIN
  s:=m4wdir+hlp+#0;
  findfirst(@s[1],faanyfile,rec);
  IF doserror=0 THEN winhelp(aparent^.hwindow,@s[1],help_context,longint(what))
  ELSE errmessagebox(aparent,id_nohelpfound);
END;

{Prototyp für die Komoboboxen **************************************************************}
constructor tmycombobox.init;
BEGIN
  tcontrol.init(Aparent,anid,'',x,y,w,h);
   attr.style:=ws_child or ws_tabstop or ws_visible or  ws_group or ws_vscroll or
              cbs_disablenoscroll or cbs_dropdownlist or cbs_simple
              or cbs_nointegralheight or cbs_hasstrings or lbs_notify or style or
              cbs_ownerdrawfixed;
  textlen:=atextlen;
END;

FUNCTION tmycombobox.setselstring(astring:STRING):integer;
VAR nc:array[0..100] of char;
BEGIN
  strpcopy(@nc,astring);
  INHERITED setselstring(@nc,-1);
END;


FUNCTION tmycombobox.addstring(astring:STRING):integer;
VAR nc:array[0..100] of char;
BEGIN
  strpcopy(@nc,astring);
  INHERITED addstring(@nc);
END;

FUNCTION tmycombobox.getstring(VAR astring:STRING;index:integer):integer;
VAR nc:array[0..100] of char;
BEGIN
  tlistbox.getstring(@nc,index);
  astring:=strpas(@nc);
END;

PROCEDURE tmycombobox.wmkeydown;
BEGIN
  {WENN CONTROL PRESSED THEN ->PAPA}
  msg.result:=0;
  IF keystate=1 THEN msg.result:=sendmessage(parent^.hwindow,wm_keydown,msg.wparam,msg.lparam)
  ELSE case msg.wparam of
    vk_return,vk_execute:sendmessage(hwindow,wm_lbuttondblclk,0,msg.lparam);
     vk_tab: setfocus(getnextdlgtabitem(parent^.hwindow,getfocus,keystate=2));
     vk_f1,vk_help,vk_escape: msg.result:=sendmessage(parent^.hwindow,wm_keydown,msg.wparam,$FFFF);
     ELSE defwndproc(msg);
   END;
END;

PROCEDURE tmycombobox.wmsyskeydown;
BEGIN
  postmessage(parent^.hwindow,wm_syskeydown,msg.wparam,msg.lparam);
  msg.result:=0;
END;
PROCEDURE tmycombobox.WMSETFOCUS(VAR Msg: TMessage);
BEGIN
 postmessage(parent^.hwindow,wm_ihavethefocus,hwindow,0);
 defwndproc(msg);
END;
PROCEDURE tmycombobox.wmctlcolor;
BEGIN
  IF msg.lparamhi=ctlcolor_listbox THEN msg.result:=BrListBox ELSE defwndproc(msg);
END;


{**************************Prototyp für die Listboxen **********************************}
constructor tmylistbox.init;
BEGIN
  tcontrol.init(Aparent,anid,'',x,y,w,h);
  attr.style:=ws_child or  ws_tabstop or
              ws_visible or  ws_group or ws_vscroll or
              lbs_disablenoscroll or lbs_notify
              or lbs_nointegralheight or lbs_hasstrings or style
              or lbs_ownerdrawfixed ;
  static:=NIL;
  candrag:=drag;
  lastcount:=-1;
  lastselcount:=-1;
  redraw:=true;
  clicked:=false;
  lastindex:=-1;
  DropCursor:=loadcursor(hinstance,'dropcursor');
END;
Destructor tmylistbox.done;
BEGIN
  Destroycursor(Dropcursor);
  inherited done;
END;
PROCEDURE tmylistbox.setassocstatic;BEGIN static:=astatic;END;
FUNCTION tmylistbox.addstring(astring:STRING):integer;
BEGIN
  astring:=astring+#0;
  addstring:=integer(SendMessage(HWindow, lb_addstring,0, LongInt(@AString[1])));
  setstatictext;
END;
FUNCTION tmylistbox.insertstring;BEGIN INHERITED insertstring(astring,index);setstatictext;END;
PROCEDURE tmylistbox.clearlist;BEGIN INHERITED clearlist;setstatictext;END;
FUNCTION tmylistbox.deletestring;BEGIN INHERITED deletestring(index); setstatictext;END;

FUNCTION tmylistbox.getstring(VAR astring:STRING;index:integer):integer;
VAR x:integer;
BEGIN
  x:=SendMessage(HWindow, lb_gettext, Index, LongInt(@astring[1]));
  astring[0]:=char(lo(x));
  getstring:=x;
END;

FUNCTION tmylistbox.getselstring(VAR astring:STRING;maxchars:integer):integer;
BEGIN
  getselstring:=getstring(AString,getselindex);
END;

FUNCTION tmylistbox.Findstring;
BEGIN
  Findstring:=INTEGER(sendmessage(hwindow,lb_findstringexact,WORD(-1),longint(@Astring[1])));
END;
FUNCTION tmylistbox.getcaretindex;
BEGIN
  getcaretindex:=integer(sendmessage(hwindow,lb_getcaretindex,0,0));
END;
FUNCTION tmylistbox.Setcaretindex;
BEGIN
  Setcaretindex:=INTEGER(sendmessage(hwindow,lb_setcaretindex,Aindex,0));
END;
FUNCTION tmylistbox.getsel;
BEGIN
  IF (attr.style AND lbs_multiplesel)=lbs_multiplesel
  THEN Getsel:=INTEGER(sendmessage(hwindow,lb_Getsel,word(aindex),0))
  ELSE Getsel:=INTEGER(sendmessage(hwindow,lb_Getcursel,0,0));
END;
FUNCTION tmylistbox.Setsel;
BEGIN
  IF (attr.style AND lbs_multiplesel)=lbs_multiplesel
  THEN Setsel:=INTEGER(sendmessage(hwindow,lb_setsel,word(onoff),longint(Aindex)))
  ELSE Setsel:=INTEGER(sendmessage(hwindow,lb_setcursel,Aindex,0));
  setstatictext;
END;
FUNCTION  tmylistbox.getitemdata;
BEGIN
  getitemdata:=sendmessage(hwindow,lb_getitemdata,Aindex,0);
END;
FUNCTION  tmylistbox.setitemdata;
BEGIN
  setitemdata:=integer(sendmessage(hwindow,lb_setitemdata,aindex,wert));
END;

PROCEDURE tmylistbox.wmkeydown;
VAR r:integer;sel:boolean;such:STRING[10];
BEGIN
  {WENN CONTROL PRESSED THEN ->PAPA}
  msg.result:=0;
  IF keystate=1 THEN msg.result:=sendmessage(parent^.hwindow,wm_keydown,msg.wparam,msg.lparam)
  ELSE case msg.wparam of
    vk_escape:
      IF candrag AND clicked THEN
      BEGIN
        clicked:=false;
        sendmessage(hwindow,wm_lbuttonup,msg.wparam,0);
      END ELSE sendmessage(parent^.hwindow,wm_keydown,msg.wparam,attr.id);
    vk_return,vk_execute:
      IF getselindex>=0 THEN
      BEGIN
       msg.lparamhi:=lbn_dblclk;
       sendmessage(parent^.hwindow,wm_command,getdlgctrlid(hwindow),msg.lparam);
      END;
     vk_space:IF Boolean(getsel(getcaretindex)) THEN setsel(getcaretindex,off)
                                                ELSE setsel(getcaretindex,_on);
     vk_insert,vk_delete:
     IF (attr.style AND lbs_multiplesel)=lbs_multiplesel THEN
     BEGIN
       r:=getcaretindex;
       setsel(r,byte(msg.wparam=vk_insert));
       IF (r+1)<getcount THEN setcaretindex(r+1);
       setstatictext;
     END ELSE postmessage(parent^.hwindow,msg.message,msg.wparam,attr.id);
     vk_tab:setfocus(getnextdlgtabitem(parent^.hwindow,getfocus,keystate=2));
     vk_f5: sendmessage(parent^.hwindow,wm_keydown,msg.wparam,longint(attr.id));
     vk_f1,vk_f2,vk_help,vk_left,vk_right,vk_add,vk_subtract,vk_multiply:
            sendmessage(parent^.hwindow,wm_keydown,msg.wparam,$FFFF);
     vk_up,vk_down:
       BEGIN
         r:=getcaretindex;
         sel:=boolean(getsel(r));
         defwndproc(msg);
         IF keystate=2 THEN
         BEGIN
           setsel(r,byte(not sel));
           IF msg.wparam=vk_up THEN r:=r-1 ELSE r:=r+1;
           IF r<0 THEN r:=0;IF r>getcount-1 THEN r:=getcount-1;
           setcaretindex(r);
         END;
       END;
     byte('A')..byte('Z'),byte('0')..byte('9'):
       IF getcount>0 THEN sendmessage(parent^.hwindow,wm_keydown,msg.wparam,attr.id);
     ELSE defwndproc(msg);
   END;
END;

PROCEDURE tmylistbox.wmsyskeydown;
BEGIN
  postmessage(parent^.hwindow,wm_syskeydown,msg.wparam,msg.lparam);
  msg.result:=0;
END;
PROCEDURE tmylistbox.WMSETFOCUS(VAR Msg: TMessage);
BEGIN
 postmessage(parent^.hwindow,wm_ihavethefocus,hwindow,0);
 defwndproc(msg);
END;

PROCEDURE tmylistbox.wmsetredraw;
BEGIN
  redraw:=msg.wparam>0;
  defwndproc(msg);
  IF redraw THEN setstatictext;
END;

PROCEDURE tmylistbox.setstatictext;
VAR x,y:integer;
    s,of_s:STRING[50];
BEGIN
 IF (static<>NIL)AND(redraw) THEN
 BEGIN
   x:=getcount;
   y:=sendmessage(hwindow,lb_getselcount,0,0);
   IF (x<>lastcount)or(y<>lastselcount) THEN
   BEGIN
     lastcount:=x;
     lastselcount:=y;
     IF y<0 THEN y:=0;
     str(y:4,of_s);str(x:4,s);
     s:=of_s+' of '+s+' Files';
     static^.setstringtext(s);
   END;
 END;
END;
PROCEDURE tmylistbox.wmmousemove;
VAR r,i:integer;point:TPoint;wnd:Hwnd;
BEGIN
  IF (candrag)AND(clicked) THEN
  BEGIN
    IF lastindex>-1 THEN BEGIN setsel(lastindex,_on);lastindex:=-1;END;
    getcursorpos(point);
    wnd:=windowfrompoint(point);
    screentoclient(wnd,point);
    r:=sendmessage(wnd,wm_candrag,0,longint(point));
    IF r=1 THEN SETCURSOR(dropcursor)ELSE setcursor(loadcursor(0,idc_arrow));
    msg.result:=0;
  END ELSE
  IF ((attr.style AND lbs_multiplesel)=lbs_multiplesel)AND((msg.wparam AND mk_rbutton)=mk_rbutton)
  THEN
  BEGIN
    defwndproc(msg);
    r:=getcaretindex;
    IF (msg.wparam AND mk_shift)=mk_shift THEN
    BEGIN
      IF r<>lastindex THEN
      BEGIN
        IF r>lastindex THEN FOR i:=lastindex+1 to r
        DO setsel(i,byte(getsel(i)=0))
        ELSE FOR i:=r TO lastindex-1
        DO setsel(i,byte(getsel(i)=0));
        lastindex:=r;
      END;
    END ELSE lastindex:=r;
  END ELSE defwndproc(msg);
END;
PROCEDURE tmylistbox.wmlbuttondown;
BEGIN
  defwndproc(msg);
  IF (candrag)AND(getcount>0) THEN
  BEGIN
    setcapture(hwindow);
    CLICKED:=TRUE;
    lastindex:=getcaretindex;
    msg.result:=0;
  END;
END;
PROCEDURE tmylistbox.wmlbuttonup;
VAR wnd:word;
    point:tpoint;
    x:LongInt;
BEGIN
  IF GetCapture=hWindow THEN RELEASECAPTURE;
  defwndproc(msg);
  {WIN3.X-BUGFIX FÜR LB'S-DRAG nach links außerhalb des Fensters}
  IF (GetCaretIndex<0)THEN BEGIN SetCaretIndex(GetCount-1);SetSel(GetCount-1,_on);END;
  IF (candrag)AND(CLICKED) THEN
  BEGIN
    CLICKED:=false;
    point:=makepoint(msg.lparam);
    CLIENTTOSCREEN(hwindow,point);
    SCREENTOCLIENT(parent^.hwindow,point);
    wnd:=CHILDwindowfrompoint(parent^.hwindow,point);
    IF wnd<>hwindow THEN
    BEGIN
      sendmessage(parent^.hwindow,wm_dropcursorpos,wnd,longint(point));
      sendmessage(parent^.hwindow,wm_internaldrop,wnd,0+hwindow);
    END;
    SETCURSOR(loadcursor(0,idc_arrow));
  END;
  setstatictext;
END;

PROCEDURE tmylistbox.wmrbuttondown;
BEGIN
  IF (attr.style AND lbs_multiplesel)=lbs_multiplesel THEN
  BEGIN
    msg.message:=wm_lbuttondown;
    INHERITED wmlbuttondown(msg);
    lastindex:=getcaretindex;
  END ELSE DEFWNDPROC(MSG);
END;
PROCEDURE tmylistbox.wmrbuttonup;
BEGIN
  IF (attr.style AND lbs_multiplesel)=lbs_multiplesel THEN
  BEGIN
    IF GetCapture=hWindow THEN RELEASECAPTURE;
    msg.message:=wm_lbuttonup;
  END;
  defwndproc(msg);
END;

PROCEDURE TmyListbox.WMCandrag;BEGIN  msg.result:=Longint(Candrag);END;
PROCEDURE TmyListBox.WMSysTimer;
BEGIN
  IF (candrag)AND(clicked) THEN
  BEGIN
    WMMouseMove(msg);
  END;
  defwndproc(msg);
END;
{Validator für Zahlen}
constructor tnumval.init;
BEGIN
  inherited init;
  parent:=aparent;
END;
procedure tnumval.error;BEGIN messagebeep(0);END;
function tnumval.IsValid;BEGIN isvalid:=true;END;

function tnumval.IsValidInput(var S: string; SuppressFill: Boolean): Boolean;
var x:boolean;i:integer;
BEGIN
  x:=false;
  FOR i:=1 TO LENGTH(s) DO
  BEGIN
    IF (not x)AND(s[i]='0') THEN Delete(s,i,1) ELSE x:=true;
    IF NOT (s[i] IN ['0'..'9']) THEN DELETE(s,i,1);
  END;
  isvalidinput:=TRUE;
END;

{                                                                             }
{                                                                             }
{                    WindowRegion...                                          }
{                                                                             }
{                                                                             }
FUNCTION long2ptr(s:word;l:longint):pointer;ASSEMBLER;
ASM
  {Segment berechnen}
  mov ax,word ptr l+2
  mul selectorinc
  mov dx,s
  add dx,ax
  {Offset schreiben}
  mov ax,word ptr l
  {Ergebnis steht in DX:AX}
END;

FUNCTION LoadDIBitMap(aInst:Word;aBMP:PChar;VAR ResSize:LongInt):Word;
VAR ResFindHandle,
    ResLoadhandle :tHandle;
    ResPointer    :Pointer;
    RetHandle     :Word;
    RetPtr        :Pointer;
BEGIN
  LoadDIBitMap :=0; ResSize:=0;
  ResFindHandle:=FindResource(aInst,aBMP,RT_BITMAP); IF ResFindHandle=0 THEN EXIT{!};
  ResLoadHandle:=LoadResource(aInst,ResFindhandle);  IF ResLoadHandle=0 THEN EXIT{!};
  ResPointer   :=LockResource(ResLoadHandle);IF ResPointer=NIL THEN BEGIN FreeResource(ResLoadHandle);EXIT{!}; END;
  ResSize      :=SizeOfResource(aInst,ResFindHandle);
  RetHandle    :=GlobalAlloc(gmem_fixed,ResSize);
  RetPtr       :=GlobalLock(RetHandle);
  IF RetPtr<>NIL THEN BEGIN hMemCpy(RetPtr,Respointer,ResSize); GlobalUnLock(RetHandle); END;
  LoadDIBitMap :=RetHandle;
  UnlockResource(ResLoadHandle); FreeResource(ResLoadHandle);
END;

FUNCTION CreateRgnFromSWBMP(SegBits:Word;OfsBits:LongInt;BMPw,BMPh,xOfs,yOfs:Integer):hRgn;
VAR LineFill :LongInt;   Rgn      :Word ABSOLUTE LineFill;
    OrgBMPh  :Word;      Rgn2     :Word ABSOLUTE OrgBMPh;
    CurrBMPw :Word;      J        :Word ABSOLUTE CurrBMPw;
    HelpSel  :Word;
    I        :LongInt;
    PtFound  :Boolean;

CONST HelpBufSize = 262144;
BEGIN
  {Initialize}
  CreateRgnFromSWBMP:=0; IF (SegBits=0)OR(BMPw=0)OR(BMPh=0) THEN Exit{!};
  HelpSel:=GlobalAlloc(gMEM_NoDiscard OR gMEM_Fixed OR GMem_ZeroInit, HelpBufSize); IF HelpSel=0 THEN EXIT{!};
  IF GlobalLock(HelpSel)=NIL THEN BEGIN GlobalFree(HelpSel); EXIT{!}; END;
  IF (BMPw MOD 32)=0 THEN LineFill:=0 ELSE LineFill:=3-((BMPw-1) MOD 32)DIV 8;
  OrgBMPh:=BMPh;
  ASM
    push ds; push es                                                    {Save Segments       }
    MOV ES, HelpSel; DB $66; XOR DI, DI                                 {HelpBuffer in ES:EDI}
    MOV DS, SegBits; DB $66; MOV SI, word ptr OfsBits                   {Bilddaten in DS:ESI }
  @BmpLineLoop:
    MOV DX      , BMPw                                                  {Anzahl Spalten DX   }
    MOV PtFound , 0                                                     {Point found = FALSE }
    MOV AX      , xOfs
    MOV CurrBMPw, AX                                                    {Aktueller xPoint = 0}
    MOV AX, BMPh; DEC AX; ADD AX, yOfs
    DB $26,$67,$89,$07                         {MOV ES:[EDI],AX }       {Note Line No 0-based}
    DB $26,$67,$C7,$47,$02,$00,$00             {MOV ES:[EDI+2],0}       {Note found Lines    }
    DB $66; MOV BX, DI; DB $66; INC BX; DB $66; INC BX                  {ES:[EBX] = found lns}
  @OuterLoop:
    MOV CX, DX; CMP CX, 8; JBE @DoByte
    MOV CX, 8
  @DoByte:
    SUB DX, CX
    DB $67,$8A,$2E;                    {MOV CH,BYTE PTR DS:[ESI]}
  @InnerLoop:
    SHL CH, 1
    JC @PixelIsNotSet

    CMP PtFound, 0                           {neuer Startpoint ?}
    JNZ @GoOnByte
    {note StartPoint}
    DB  $26,$67,$83,$3B,$00                      {CMP ES:[EBX],0}
    ja  @NoInc
    DB  $66,$83,$C7,$04                               {ADD EDI,4}
  @NoInc:
    MOV AX, CurrBMPw
    DB  $26,$67,$89,$07                        {MOV ES:[EDI], AX}
    DB  $66,$83,$C7,$02                               {ADD EDI,2}
    DB  $26,$67,$FF,$03                            {INC ES:[EBX]}
    MOV PtFound, 1
    JMP @GoOnByte

  @PixelIsNotSet:
    CMP PtFound, 1                                   {Endpoint ?}
    JNZ @GoOnByte
    {note EndPoint}
    MOV AX, CurrBMPw
    DB  $26,$67,$89,$07                        {MOV ES:[EDI], AX}
    DB  $66,$83,$C7,$02                               {ADD EDI,2}
    MOV PtFound, 0

  @GoOnByte:
    INC CurrBMPw
    DEC CL; JNZ @InnerLoop

    DB $66; INC SI
    OR DX, DX
    JNZ @OuterLoop
    DB $66; ADD SI, word ptr LineFill

    CMP PtFound, 1                                   {Endpoint ?}
    JNZ @GoOnRow
    {note EndPoint}
    MOV AX, CurrBMPw
    DB  $26,$67,$89,$07                        {MOV ES:[EDI], AX}
    DB  $66,$83,$C7,$02                               {ADD EDI,2}

  @GoOnRow:
    DEC word ptr BMPh
    JNZ @BMPLineLoop
  @PopExit:
    pop es; pop ds
  @Exit:
  END;
  I:=0;
  IF Word(Long2Ptr(HelpSel,I+2)^)>0 THEN
  BEGIN
    Rgn:=CreateRectRgn(Integer(Long2Ptr(HelpSel,I+4)^), Integer(Long2Ptr(HelpSel,I)^),
                       Integer(Long2Ptr(HelpSel,I+6)^), Integer(Long2Ptr(HelpSel,I)^)+1);
    WHILE Word(Long2Ptr(HelpSel,I+2)^)>0 DO
    BEGIN
      FOR J:=1 TO Word(Long2Ptr(HelpSel,I+2)^) DO
      BEGIN
        Rgn2:=CreateRectRgn(Integer(Long2Ptr(HelpSel,I+(J*4))^)  , Integer(Long2Ptr(HelpSel,I)^),
                            Integer(Long2Ptr(HelpSel,I+(J*4)+2)^), Integer(Long2Ptr(HelpSel,I)^)+1);
        IF Rgn2<>ERROR THEN BEGIN CombineRgn(Rgn,Rgn,Rgn2,RGN_OR); DeleteObject(Rgn2);END;
      END;
      INC(I,LongInt(4)+(LongInt(J)*4));
    END;
    CreateRgnFromSWBMP:=Rgn;
  END;
  GlobalUnLock(HelpSel); GlobalFree(HelpSel);
END;

FUNCTION LoadRgnFromBMP(BitmapName:pChar;xOfs,yOfs:Integer):hRgn;
VAR ResSel :Word;
    ResSize:LongInt;
BEGIN
  LoadRgnFromBMP:=0;
  IF BitmapName=NIL THEN Exit;
  ResSel:=LoadDIBitMap(HInstance,BitmapName,ResSize);
  IF ResSel=0 THEN EXIT{!};
  IF GlobalLock(ResSel)=NIL THEN BEGIN GlobalFree(ResSel); EXIT{!}; END;
  WITH pBitmapInfoHeader(Ptr(ResSel,0))^ DO
  BEGIN
    IF biBitCount<>1 THEN BEGIN GlobalUnlock(ResSel); GlobalFree(ResSel);EXIT{!}; END;
    LoadRgnFromBMP:=CreateRgnFromSWBMP(ResSel,biSize+8,biWidth,biHeight,xOfs,yOfs);
  END;
  GlobalUnlock(ResSel); GlobalFree(ResSel);
END;

FUNCTION SetWindowRegion(MyWindow:pMyWindow;BitmapName:pChar):Bool;
VAR Rect  :tRect;  Point: tPoint ABSOLUTE Rect;
    ClientRgn:hRgn;
BEGIN
  SetWindowRegion:=False;
  IF OS=Windows_31 THEN EXIT{!};
  GetWindowRect(MyWindow^.hWindow,Rect);
  ClientRgn:=LoadRgnFromBMP(BitmapName,0,MyWindow^.CapDy);
  IF ClientRgn=0
  THEN WITH Rect DO ClientRgn:=CreateRectRgn(0,0,Right-Left,Bottom-Top)
  ELSE BEGIN
    ScreenToClient(MyWindow^.hWindow,Point);
    OffsetRgn(ClientRgn,-Point.X,-Point.Y);
  END;
  IF ClientRgn=0 THEN EXIT{!};
  IF SetWindowRgn(WOWHandle32(MyWindow^.hWindow,WOW_TYPE_HWND),WOWHandle32(ClientRgn,WOW_TYPE_HRGN),True) THEN
  BEGIN
    SetWindowRegion:=True;
    EXIT{!};
  END;
  DeleteObject(ClientRgn);
END;
{                                                                             }
{                                                                             }
{  End               WindowRegion...                                          }
{                                                                             }
{                                                                             }


VAR OldCriticalExit:PROCEDURE;
    fontfilename:ARRAY[0..150] OF char;
    s:STRING[150] ABSOLUTE fontfilename;
    fontfilehandle:tHandle;
    FontFile:File;
PROCEDURE FatalExit; FAR;
BEGIN
  CriticalExit:=OldCriticalExit;
  IF oemhandle<>0 THEN
  BEGIN
    deleteobject(oemhandle);
    IF ansihandle<>0 THEN deleteobject(ansihandle);
    GlobalDeleteAtom(GlobalFindAtom('SWEBUZisHere'));
    IF GlobalFindAtom('SWEBUZisHere')=0 THEN
    BEGIN
      GetProfileString('SWEBUZengine','FontName','m4w',@fontfilename,150);
      WriteProfileString('SWEBUZengine',NIL,NIL);
      IF NOT removefontresource(@fontfilename)
      THEN MessageBox(0,errorver,'Warning!'+#10+#10'Could not remove font.',0) ELSE
      BEGIN
        sendmessage($FFFF,wm_fontchange,0,0);
        fontfilehandle:=getmodulehandle(fontfilename);
        if fontfilehandle <> 0 then
        BEGIN
          WHILE GetModuleUsage(fontfilehandle)>1 DO freelibrary(fontfilehandle);
          freelibrary(fontfilehandle);
        END;
        {$i-}
        assign(fontfile,fontfilename);
        setfattr(fontfile,faarchive);
        erase(fontfile);
      END;
    END;
  END;
END;

BEGIN
  OldCriticalExit:=CriticalExit;
  CriticalExit:=FatalExit;
END.
