UNIT about;
{ $C Fixed Preload Permanent}
INTERFACE
USES owindows,wintypes,win31,strings,winprocs,mmsystem,modc,omemory,winunit;

{****************************Laufschrift-Objekt**************************************}
CONST wm_About=wm_User+$1234;

TYPE Plaufschrift=^laufschrift;
     laufschrift=object
       rect:tRect;
       memdc:hdc;
       HWindow:hwnd;
       membmp:hbitmap;
       lauffont:tlogfont;
       laufhandle:thandle;
       fontdy:integer;
       aboutlines,y,currline:word;
       ax,ay,aw,ah:integer;
       Timer:Integer;
       pc:ARRAY[0..100] OF char;
       CONSTRUCTOR Init(AParent: PWindowsObject; x0,y0,dx,dy: Integer);
       DESTRUCTOR Done;
       PROCEDURE Paint(paintdc:hdc);
       PROCEDURE wmAbout;
     END;

IMPLEMENTATION

CONSTRUCTOR laufschrift.Init(AParent: PWindowsObject; x0,y0,dx,dy: Integer);
VAR fx:ttextmetric;
    dc:hdc;
    b:tbitmap;
    oldfont:thandle;
    MSG:tMessage;
BEGIN
  ax:=x0;
  ay:=y0;
  aw:=dx;
  ah:=dy;
  HWindow:=aparent^.HWindow;
  WITH lauffont DO BEGIN
    lfheight:=-14;
    lfwidth:=0;
    lfescapement:=0;
    lforientation:=0;
    lfweight:=400;
    lfitalic:=0;
    lfunderline:=0;
    lfstrikeout:=0;
    lfcharset:=ANSI_charset;
    lfoutprecision:=out_character_precis;
    lfclipprecision:=clip_default_precis;
    lfquality:=proof_quality;
    lfpitchandfamily:=tmpf_vector OR ff_swiss;
    strpcopy(@lffacename,'Arial');
    laufhandle:=CreateFontIndirect(lauffont);
  END;
  dc:=GetDC(HWindow);
  oldfont:=SelectObject(dc,laufhandle);
  GetTextMetrics(dc,fx);
  fontdy:=fx.tmheight;
  SelectObject(dc,oldfont);
  memdc:=CreateCompatibleDC(dc);
  IF PaletteInstalled THEN SelectPalette(memdc,hPal,false);
  membmp:=CreateCompatibleBitmap(dc,aw,ah);
  ReleaseDC(HWindow,dc);
  SelectObject(memdc,membmp);
  SetTextAlign(memdc,ta_center OR ta_top);
  SetTextColor(memdc,palettergb(255,255,0));
  SetBkColor  (memdc,cBlack);
  SelectObject(memdc,laufhandle);
  SetBkMode(memdc,opaque);
  rect.left:=0;
  rect.right:=aw;
  rect.top:=0;
  rect.bottom:=ah;
  FillRect(memdc,rect,BrBlack);
  rect.left:=0;
  rect.right:=aw;
  rect.top:=ah-1;
  rect.bottom:=ah;
  Val(getresstring(id_aboutlines),aboutlines,y);
  y:=0;
  currline:=0;
  MSG.Message:=wm_About;
  msg.wParam:=1994;
  msg.lParam:=40;
  Timer:=pBuzApplication(Application)^.AddEvent(hWindow,MSG);
END;

DESTRUCTOR laufschrift.Done;
BEGIN
  pBuzApplication(Application)^.RemoveEvent(Timer);
  DeleteDC(memdc);
  DeleteObject(laufhandle);
  DeleteObject(membmp);
END;

PROCEDURE laufschrift.Paint;
BEGIN
  BitBlt(paintdc,ax,ay,aw,ah,memdc,0,0,srccopy);
END;

PROCEDURE laufschrift.wmAbout;
VAR dc:thandle;
    s:STRING[100];
BEGIN
  BitBlt(memdc,0,0,aw,ah,memdc,0,1,srccopy);
  IF currline<=aboutlines THEN
  BEGIN
    inc(y);
    IF ((y+ah) DIV fontdy)>currline THEN
    BEGIN
      currline:=(y+ah) DIV fontdy;
      LoadString(hLanguage,400+currline,pc,100);
      s:=strpas(pc);
      IF S='USERNAME' THEN s:=username
      ELSE IF s='COMPNAME' THEN s:=compname;
      s:=s+#0;
      strpcopy(pc,s);
    END;
    FillRect(memdc,rect,BrBlack);
    TextOut(memdc,aw DIV 2,fontdy*currline-y-1,pc,strlen(pc));
  END ELSE
  IF y<((aboutlines+4)*fontdy) THEN
  BEGIN
    inc(y);
    FillRect(memdc,rect,BrBlack);
  END ELSE
  BEGIN
    y:=0;
    currline:=0;
  END;
  dc:=GetDC(HWindow);
  BitBlt(dc,ax,ay,aw,ah,memdc,0,0,srccopy);
  ReleaseDC(HWindow,dc);
  PostMessage(HWindow,wm_box,0,0);
  SetTimer(HWindow,1994,65,NIL);
END;

BEGIN
END.