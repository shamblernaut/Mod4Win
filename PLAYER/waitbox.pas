UNIT WaitBox;
{$C Fixed Preload Permanent}
INTERFACE
USES WinTypes,WinProcs,Strings,{$IFNDEF MCI}MODc{$ELSE}FRAMES{$ENDIF};

PROCEDURE InitWaitBox(Parent:Word; Title: PChar);
PROCEDURE DoneWaitBox;
PROCEDURE SetPercent(How:Longint);

IMPLEMENTATION

CONST hWindow:hWnd=0;
      Percent:Longint=0;
{$IFDEF MCI}
    _Class='SWEBUZ_WaitBox'+#0;
VAR
  attr:RECORD x,y,w,h:integer; END;
  pt:tPoint;
  FH:Integer;
  MemDC:hDC;
  Brush:hBrush;
  Rect:tRect;
  MemBMP:hBitmap;
  Caption:PChar;

PROCEDURE DrawBar(DC:hDC);
VAR S:String[6];
BEGIN
  GrayFrameFilledDown(DC,6,15+FH,202,FH+4,1,GetSysColor(COLOR_BTNFACE));
  GrayFrameUp(DC,7,16+FH,200*Percent DIV 100,FH+2,1);
  SetTextAlign(DC,TA_Bottom OR TA_Center);
  SetTextColor(DC,GetSysColor(COLOR_BTNText));
  SetBkMode(DC,Transparent);
  Str(Percent,S);
  s:=S+'%'+#0;
  TextOut(DC,Attr.W DIV 2,19+2*FH,@S[1],Length(S)-1);
END;

PROCEDURE Draw(DC:hDC);
BEGIN
  fillrect(MemDC,rect,brush);
  DrawBar(MemDC);
  TextOut(MemDC,Attr.W DIV 2,10+FH,Caption,StrLen(Caption));
  GrayFrameUpDown(MemDC,1,1,attr.w-2,attr.h-2,1,1,1,GetSysColor(Color_BtnFace));
  framerect(MemDC,rect,getstockobject(Black_Brush));
  BitBlt(DC,0,0,attr.w,attr.h,MemDC,0,0,SrcCopy);
END;

FUNCTION WndProc(hWnd,message,wParam:word;lParam:longint):Longint;EXPORT;
VAR PS:tPaintStruct;
BEGIN
  WndProc:=0;
  CASE message OF
    WM_SetFocus:SetFocus(wparam);
    WM_EraseBkGnd:Draw(wParam);
    WM_Paint:
    BEGIN
      BeginPaint(hWnd,PS);
      Draw(PS.HDC);
      EndPaint(hWnd,PS);
    END;
    WM_MOVE:
    BEGIN
      ATTR.X:=LoWord(lParam);
      ATTR.Y:=HiWord(lParam);
      Invalidaterect(hWnd,NIL,True);
    END;
    ELSE WndProc:=DefWindowProc( hWnd, message, wParam, lParam );
  END;
END;

FUNCTION Init(hInst:THandle):Boolean;
VAR WCLASS:TWndClass;
BEGIN
  WITH WCLASS DO
  BEGIN
    hCursor      :=LoadCursor(0,IDC_ARROW);
    hIcon        :=LoadIcon(0, IDI_Application);
    lpszMenuName :=NIL;
    lpszClassName:=_Class;
    hbrBackground:=0;
    hInstance    :=hInst;
    style        :=cs_bytealignclient OR cs_bytealignwindow;
    lpfnWndProc  :=@WndProc;
    cbClsExtra   :=0 ;
    cbWndExtra   :=0 ;
  END;
  Init:=RegisterClass(WClass);
END;

PROCEDURE InitWaitBox;
VAR DC:hDC;
    FX:tTextMetric;
BEGIN
  IF (hWindow<>0)OR(NOT Init(hInstance)) THEN Exit;
  Caption := Title;
  IF Parent=1 THEN Parent:=0;
  hWindow:=CreateWindowEx($8,_Class,'',ws_popup OR ws_visible,0,0,0,0,Parent,0,hInstance,NIL);
  IF hWindow=0 THEN Exit;
  EnableWindow(hwindow,false);
  DC:=GetDC(hWindow);
  gettextmetrics(dc,fx);
  FH:=fx.tmAscent;
  Attr.W:=2   {Fensterrahmen}
         +200 {Balken}
         +2   {Rahmen um Balken}
         +2*5 {Abstand zur Seite};
  Attr.H:=2   {Fensterrahmen}
         +FH+2{Balken}
         +2   {Rahmen um Balken}
         +4*5 {Abstaende Balken und Font}
         +FH;
  Attr.X:=(GetSystemMetrics(sm_cxscreen)-Attr.W)DIV 2;
  Attr.Y:=(GetSystemMetrics(SM_CYScreen)-Attr.H)DIV 2;
  MemDC:=CreateCompatibleDC(DC);
  SetTextAlign(MemDC,TA_Bottom OR TA_Center);
  SetTextColor(MemDC,GetSysColor(COLOR_BTNText));
  SetBkColor(MemDC,GetSysColor(COLOR_BTNFace));
  MemBMP:=CreateCompatibleBitMap(DC,attr.w,attr.h);
  SelectObject(MemDC,MemBMP);
  Brush:=CreateSolidBrush(GetSysColor(Color_BtnFace));
  MoveWindow(hwindow,Attr.x,Attr.y,Attr.w,Attr.h,True);
  rect.left:=0;rect.Top:=0;rect.Bottom:=attr.h;rect.Right:=Attr.w;
  Draw(DC);
  ReleaseDC(hWindow,DC);
END;

PROCEDURE DoneWaitBox;
BEGIN
  IF hWindow=0 THEN Exit;
  DestroyWindow(hWindow);
  DeleteDC(MemDC);
  DeleteObject(MemBMP);
  DeleteObject(Brush);
  UnRegisterClass(_Class,hInstance);
  hWindow:=0;
END;

PROCEDURE SetPercent(How:Longint);
VAR DC:hDC;
BEGIN
  Yield;
  IF How>100 THEN How:=100 ELSE If How<0 THEN How:=0;
  IF Percent<>HOW THEN
  BEGIN
    Percent:=How;
    DC:=GetDC(hWindow);
    Draw(DC);
    ReleaseDC(hWindow,DC);
    Yield;
  END;
END;
{$ELSE MCI}

CONST aTitle:STRING[40]='';
PROCEDURE InitWaitBox(Parent:Word; Title: PChar);
BEGIN
  hWindow:=Parent;
  aTitle:=StrPas(Title);
END;

PROCEDURE DoneWaitBox;
BEGIN
END;

PROCEDURE SetPercent(How:Longint);
VAR   aText:STRING[40];
BEGIN
  Yield;
  IF How>100 THEN How:=100 ELSE If How<0 THEN How:=0;
  IF Percent<>HOW THEN
  BEGIN
    Percent:=How;
    Str(Percent,aText);
    aText:=aTitle+' ['+aText+'%]';
    SendMessage(hWindow,wm_unpacking,0,Longint(@aText));
    Yield;
  END;
END;
{$ENDIF MCI}

END.