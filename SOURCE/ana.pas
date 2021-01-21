UNIT ana;
{ $C Fixed Preload Permanent}
INTERFACE
USES owindows,winprocs,wintypes,strings,omemory,winunit,modc,playproc,win31;

VAR   effwnd:hwnd;

TYPE peff=^teff;
     teff=object(TWindow)
       PCursor,Norm_Cursor:HCursor;
       DisAbled     :ARRAY[1..32]OF BOOLEAN;
       DrawAll      :Boolean;
       pINFO        :PMODULINFO;
       LCDDC        :hDC;
       LCDBMP       :hBitmap;
       effbmp       :hbitmap;
       fontbmp      :hbitmap;
       commbmp      :hbitmap;
       LevelBMP     :hBitmap;
       LevelDC      :hDC;
       bmh          :integer;
       Effcts       :EffectState;
       lastinstr    :tbytestate;
       lastvols     :tbytestate;
       lastnotes    :tbytestate;
       oldtime      :longint;
       eff          :EffectSet;
       lastspd,
       lastbpm,
       lastLine     :byte;
       CONSTRUCTOR Init(aParent:pWindowsObject; Struktur:pmodulinfo);
       DESTRUCTOR Done;                                   VIRTUAL;
       PROCEDURE SetupWindow;                             VIRTUAL;
       PROCEDURE wmQueryNewPalette(VAR msg:tmessage);     VIRTUAL wm_first+wm_QueryNewPalette;
       PROCEDURE ProcessData(idletime:longint);           VIRTUAL;
       PROCEDURE wmerasebkgnd(VAR msg:tmessage);          VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE wmkeydown(VAR msg:tmessage);             VIRTUAL wm_first+wm_keydown;
       PROCEDURE wmrbuttondown(VAR msg:tmessage);         VIRTUAL wm_first+wm_rbuttondown;
       PROCEDURE WMLButtonDown(VAR msg:tmessage);         VIRTUAL wm_first+wm_lbuttondown;
       PROCEDURE wmlbuttondblclk(VAR msg:tmessage);       VIRTUAL wm_first+wm_lbuttondblclk;
       FUNCTION  GetClassName:PChar;                      VIRTUAL;
       PROCEDURE GetWindowClass(VAR WndClass: TWndClass); VIRTUAL;
       PROCEDURE WMMouseMove(VAR Msg: TMessage);          VIRTUAL wm_First+wm_MouseMove;
       PRIVATE
       DC:hdc;
       Wert,x_eff,bltmin,bltend,y_eff:Integer;
     END;

IMPLEMENTATION
{EFFEKTFENSTER***+ VOL****************************************************}
CONSTRUCTOR teff.Init;
VAR BMP:tBitmap;
BEGIN
  INHERITED Init (AParent, '');
  pinfo:=struktur;
  effbmp:=LoadMyBitMap  (hinstance,'effbmp');
  commbmp:=LoadMyBitMap (hinstance,'effcomm');
  lcdbmp:=LoadMyBitMap  (hinstance,'hkonoff');
  fontbmp:=LoadMyBitMap (hinstance,'Num_7x5');
  LevelBMP:=LoadMyBitMap(hinstance,'pegel');
  GetObject(effBMP,SizeOf(BMP), @BMP);
  WITH Attr DO
  BEGIN
    w:=0;
    h:=0;
    x:=GetPrivateProfileInt(s_state,'EFF_X',Pwindow(AParent)^.Attr.x,INIDIR);
    y:=GetPrivateProfileInt(s_state,'EFF_Y',Pwindow(Aparent)^.Attr.y+130,INIDIR);
    setcorrectwinpos(x,y,w,h);
    style:=ws_popup OR ws_visible OR ws_border;
  END;
  bmh:=BMP.bmHeight;
  FillChar(DisAbled,SizeOf(DisAbled),0);
  pCursor:=LoadCursor(hinstance,'PointCursor');
  Norm_Cursor:=LoadCursor(0,idc_arrow);
END;

PROCEDURE tEff.WmQueryNewPalette(VAR MSG:tMessage);
BEGIN
  pMyWindow(Application^.MainWindow)^.WmQueryNewPalette(MSG);
END;

PROCEDURE teff.SetupWindow;
VAR bmp:hbitmap;
BEGIN
  INHERITED SetupWindow;
  effwnd:=HWindow;
  dc:=GetDC(HWindow);
  LCDDC:=CreateCompatibleDC(dc);
  IF PaletteInstalled THEN SelectPalette(LCDDC,hPal,false);
  leveldc:=CreateCompatibleDC(dc);
  ReleaseDC(HWindow,dc);
  SelectObject(LCDDC,LCDBMP);
  SelectObject(leveldc,levelbmp);
  processdata(0);
END;

DESTRUCTOR teff.Done;
VAR s:STRING[10];
BEGIN
  IF GetCursor=pCursor THEN SetCursor(LoadCursor(0,idc_arrow));
  DestroyCursor(pCursor);
  Str(Attr.x,s); writeinistring(s_state,'EFF_X'+#0,s);
  Str(Attr.y,s); writeinistring(s_state,'EFF_Y'+#0,s);
  effwnd:=0;
  DeleteDC(LCDDC);
  DeleteDC(leveldc);
  DeleteMyBitMap(LCDBMP);
  DeleteMyBitMap(effBMP);
  DeleteMyBitMap(fontbmp);
  DeleteMyBitMap(levelbmp);
  DeleteMyBitMap(commbmp);
  INHERITED Done;
END;

PROCEDURE teff.wmerasebkgnd(VAR msg:tmessage);
VAR mdc:hdc;
    i,j:word;
    s:STRING[4];
BEGIN
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  mdc:=CreateCompatibleDC(msg.wParam);
  framefilled3d(msg.wParam,0,0,Attr.w-2,Attr.h-2,2,cForeGnd,up);
  {KanalZahlen+~Rahmen+InstrumenRahmen+DiodenRahmen}
  IF pinfo^.numtracks>0 THEN
  BEGIN
    frame3d(msg.wParam,  6,77, 19,(pinfo^.numtracks*9)+4,1,down);
    frame3d(msg.wParam, 27,77,Attr.w-120,(pinfo^.numtracks*9)+4,1,down);
    {Note}
    frame3d(msg.wParam,Attr.w-91,77, 19,(pinfo^.numtracks*9)+4,1,down);
    {Pan}
    frame3d(msg.wParam,Attr.w-70,77, 41,(pinfo^.numtracks*9)+4,1,down);
    {Instrument}
    frame3d(msg.wParam,Attr.w-27,77, 19,(pinfo^.numtracks*9)+4,1,down);
    SelectObject(mdc,fontbmp);
    {KanalNummern}
    FOR i:=1 TO pinfo^.numtracks DO
    BEGIN
      IF DisAbled[i] THEN s:=':::' ELSE Str(i,s);
      WHILE Length(s)<3 DO Insert('/',s,1);
      FOR j:=1 TO 3 DO BitBlt(msg.wParam,8+(5*(j-1)),80+((i-1)*9),5,7,mdc,5*(Byte(s[j])-47),0,srccopy);
    END;
  END;
  {Effekttext+~Rahmen}
  j:=0;
  SelectObject(mdc,effbmp);
  FOR i:=0 TO LastEffect DO IF (i<>14)AND(i IN pinfo^.EFFECTSUSEDSet)THEN
  BEGIN BitBlt (msg.wParam,29+j,8,14,65,mdc,14+i*14,0,srccopy); INC(j,14); END;
  frame3d(msg.wParam,27,6,Attr.w-120,69,1,down);
  {'effects'}
  BitBlt (msg.wParam,7,8,14,52,mdc,0,0,srccopy);
  SelectObject(mdc,commbmp);
  frame3d(msg.wParam,6  ,6,19,55,1,down);
  frame3d(msg.wParam,6  ,64,19,11,1,down);
  {Speed+Bpm-speed}
  BitBlt(msg.wParam,Attr.w-88,9,32,10,mdc,0 ,0,srccopy);
  BitBlt(msg.wParam,Attr.w-88,22,32,10,mdc,32,0,srccopy);
  BitBlt(msg.wParam,Attr.w-88,35,32,10,mdc,198,0,srccopy);
  BitBlt(msg.wParam,Attr.w-88,48,32,10,mdc,165,0,srccopy);
  frame3d(msg.wParam,Attr.w-91,6,83,55,1,down);
  frame3d(msg.wParam,Attr.w-91,64,83,11,1,down);
  {statuszeilentexte}
  BitBlt(msg.wParam,7 ,65,17,9,mdc,64,1,srccopy);
  BitBlt(msg.wParam,Attr.w-90,65,81,9,mdc,81,1,srccopy);
  DeleteDC(mdc);
  {Invalidaten...}
  DrawAll:=True;
  ValidateRect(HWindow,NIL);
END;

PROCEDURE teff.processdata;
VAR i:Longint;
    x:Longint;
    dey:integer;
    s:STRING[8];
    mdc:hdc;
    rect:prect;
BEGIN
  IF GetFocus=hWindow THEN SetFocus(pMyWindow(Parent)^.OldFocus);
  IF ((BMh+(pinfo^.numtracks*9)+24)<>Attr.h)OR(eff<>pinfo^.EFFECTSUSEDSet)
  THEN
  BEGIN
    dey:=0;
    FOR X:=0 TO LastEffect DO IF(X IN pinfo^.EFFECTSUSEDSet)AND(X<>14)THEN INC(dey);
    Attr.h:=BMh+(pinfo^.numtracks*9)+24;
    Attr.w:=124+14*dey;
    SetCorrectWinPos(Attr.x,Attr.y,Attr.w,Attr.h);
    MoveWindow(HWindow,Attr.x,Attr.y,Attr.w,Attr.h,True);
    InvalidateRect(HWindow,NIL,true);
    eff:=pinfo^.EFFECTSUSEDSet;
    Exit;
  END;
  dc:=GetDC(HWindow);
  IF PaletteInstalled THEN SelectPalette(DC,hPal,false);
  rect:=New(prect);
  i:=pinfo^.currspd;
  IF(i<>lastspd)OR(DRAWALL)THEN
  BEGIN
    {Speed}
    lastspd:=i;
    mdc:=SelectObject(dc,ANSIhandle); SetTextAlign(dc,ta_left OR ta_top OR ta_noupdatecp);
    SetTextColor(dc,cVGStatic); SetBkColor(dc,cHGStatic);
    Str(lastspd:3,s); s:=s+#0;
    WITH Rect^ DO
    BEGIN
      left:=Attr.w-36; right:=left+3*Fw; top:=8; bottom:=top+fh;
      ExtTextOut(dc,left,top,eto_opaque,rect,@s[1],3,NIL);
    END;
    SelectObject(dc,mdc);
  END;
  i:=pinfo^.currbpm;
  IF(i<>lastbpm)OR(DRAWALL)THEN
  BEGIN
    {BPM}
    lastbpm:=i;
    mdc:=SelectObject(dc,ANSIhandle); SetTextAlign(dc,ta_left OR ta_top OR ta_noupdatecp);
    SetTextColor(dc,cVGStatic); SetBkColor(dc,cHGStatic);
    Str(lastbpm:3,s); s:=s+#0;
    WITH Rect^ DO
    BEGIN
      left:=Attr.w-36; right:=left+3*Fw; top:=21; bottom:=top+fh;
      ExtTextOut(dc,left,top,eto_opaque,rect,@s[1],3,NIL);
    END;
    SelectObject(dc,mdc);
  END;
  i:=pinfo^.currLine;
  IF(i<>lastLine)OR(DRAWALL)THEN
  BEGIN
    {NOTE}
    lastline:=i;
    mdc:=SelectObject(dc,ANSIhandle); SetTextAlign(dc,ta_left OR ta_top OR ta_noupdatecp);
    SetTextColor(dc,cVGStatic); SetBkColor(dc,cHGStatic);
    Str(lastline:3,s); s:=s+#0;
    WITH Rect^ DO
    BEGIN
      left:=Attr.w-36; right:=left+3*Fw; top:=34; bottom:=top+fh;
      ExtTextOut(dc,left,top,eto_opaque,rect,@s[1],3,NIL);
    END;
    SelectObject(dc,mdc);
  END;
  i:=Longint(pinfo^.totaltime);
  IF(Longint(i)<>Longint(oldtime))OR(DRAWALL)THEN
  BEGIN
    {TotalTime}
    Longint(oldtime):=Longint(i);
    mdc:=SelectObject(dc,ANSIhandle); SetTextAlign(dc,ta_left OR ta_top OR ta_noupdatecp);
    SetTextColor(dc,cVGStatic); SetBkColor(dc,cHGStatic);
    IF Longint(oldtime+500)>Longint(99*60+59)*1000
    THEN x:=99*100+99
    ELSE x:=100*((Longint(oldtime)+500)DIV 1000 DIV 60)+((Longint(oldtime)+500)DIV 1000 MOD 60);
    Str(x,s); WHILE Length(S)<3 DO Insert('0',s,1); IF Length(S)<4 THEN Insert(' ',s,1);
    Insert(':',s,3); s:=s+#0;
    WITH Rect^ DO
    BEGIN
      left:=Attr.w-52; right:=left+5*Fw; top:=47; bottom:=top+fh;
      ExtTextOut(dc,left,top,eto_opaque,rect,@s[1],5,NIL);
    END;
    SelectObject(dc,mdc);
  END;
  Dispose(rect);
  mdc:=CreateCompatibleDC(dc);
  SelectObject(mdc,fontbmp);
  FOR i:=1 TO pinfo^.numtracks DO WITH pinfo^ DO
  BEGIN
    {KanalNummern (En/DisAbled)}
    IF DisAbled[i]=MOD_IsEnabled(i) THEN
    BEGIN
      DisAbled[i]:=NOT(DisAbled[i]);
      IF DisAbled[i] THEN s:=':::' ELSE Str(i,s);
      WHILE Length(s)<3 DO Insert('/',s,1);
      FOR x:=1 TO 3 DO BitBlt(dc,8+(5*(x-1)),80+((i-1)*9),5,7,mdc,5*(Byte(s[x])-47),0,srccopy);
    END;
    {Instrumenten-Nummern}
    X:=currinstr[i];
    IF (x<>lastinstr[i])OR(DRAWALL) THEN
    BEGIN
      LastInstr[i]:=X;
      IF X=0 THEN s:=':::' ELSE Str(X,s);
      WHILE Length(s)<3 DO Insert('/',s,1);
      FOR x:=1 TO 3 DO BitBlt(dc,Attr.w-25+(5*(x-1)),80+((i-1)*9),5,7,mdc,5*(Byte(s[x])-47),0,srccopy);
    END;
    {Channel-Vols}
    X:=currvols[i];
    IF (X<>lastvols[i])OR(DRAWALL) THEN
    BEGIN
      lastvols[i]:=X;
      BitBlt(dc,Attr.w-69,79+((i-1)*9),37,9,leveldc,0,9,srccopy);
      {aktiver Teil}
      BitBlt(dc,Attr.w-68+(16-(X AND $0F)SHL 1),79+((i-1)*9),(X SHR 4)SHL 1+3+(X AND $0F)SHL 1
               ,9,leveldc,1+(16-(X AND $0F)SHL 1),0,srccopy);
    END;
    {Curr. Notes}
    X:=0+currnotes[i];
    IF (X<>lastnotes[i])OR(DRAWALL) THEN
    BEGIN
      lastnotes[i]:=X;
      IF X=0 THEN
      BEGIN
        s:=':::';
        FOR x:=1 TO 3 DO BitBlt(dc,Attr.w-89+(5*(x-1)),80+((i-1)*9),5,7,mdc,5*(Byte(s[x])-47),0,srccopy);
      END ELSE
      BEGIN
        Dec(X);
        BitBlt(dc,Attr.w-89,80+((i-1)*9),10,7,mdc,60+((X MOD 12)*10),0,srccopy);
        BitBlt(dc,Attr.w-79,80+((i-1)*9),5 ,7,mdc, 5+((X DIV 12)*5 ),0,srccopy);
      END;
    END;
    {Effekte}
    dey:=0;
    FOR x:=0 TO LastEffect DO IF(X IN EffectsUsedSet)AND(x<>14)THEN
    BEGIN
      IF ((X IN CURREFFECTS[i])XOR(X IN Effcts[i]))THEN
      BEGIN
        IF NOT(X IN Effcts[i])
        THEN BEGIN BitBlt(dc,29+dey,79+(i-1)*9,14,9,lcddc,0 ,19,srccopy); Include(Effcts[i],X); END
        ELSE BEGIN BitBlt(dc,29+dey,79+(i-1)*9,14,9,lcddc,0 ,10,srccopy); Exclude(Effcts[i],X); END;
      END ELSE IF DRAWALL THEN
      BEGIN
        IF X IN Effcts[i]
        THEN BitBlt(dc,29+dey,79+(i-1)*9,14,9,lcddc,0 ,19,srccopy)
        ELSE BitBlt(dc,29+dey,79+(i-1)*9,14,9,lcddc,0 ,10,srccopy);
      END;
      inc(dey,14);
    END;
  END;
  DrawAll:=False;
  ReleaseDC(HWindow,dc);
  DeleteDC(mdc);
END;

PROCEDURE teff.WMMouseMove(VAR Msg: TMessage);
VAR pt:tPoint;
BEGIN
  Pt:=MakePoint(msg.lParam);
  {GetCursorPos(pt);
  Dec(pt.x,Attr.x);
  Dec(pt.y,Attr.y);{}
  IF (pt.x>=8)AND(pt.x<=23)AND
     ((pt.y>=80)AND(pt.y<=(80+(pinfo^.numtracks*9)))OR(pt.y<=75)AND(pt.y>=65))
  THEN SetCursor(PCursor) ELSE SetCursor(Norm_Cursor);
END;

PROCEDURE teff.WMLButtonDown;
VAR pt,pt2:tPoint;
BEGIN
  Pt:=MakePoint(msg.lParam);
  {ClientToScreen(hWindow,Pt);
  Dec(pt.x,Attr.x);
  Dec(pt.y,Attr.y);{}
  IF (pt.x>=8)AND(pt.x<=23)AND(pt.y>=80)AND(pt.y<=(80+(pinfo^.numtracks*9)))THEN
  BEGIN
    pt2.y:=(pt.y-80)DIV 9+1;
    MOD_EnableChn(pt2.y,NOT(Mod_IsEnabled(pt2.y)));
  END ELSE
  IF(pt.x>=8)AND(pt.x<=23)AND(pt.y<=75)AND(pt.y>=65)THEN MOD_EnableAllChn(True) ELSE
  IF MoveOnClick THEN
  BEGIN
    IF FullMove THEN
    BEGIN
      {move window}
      ClientToScreen(hWindow,Pt);
      WHILE (GetAsyncKeyState(VK_lButton)AND$8000)>0 DO
      BEGIN
        GetCursorPos(Pt2);
        IF (Pt.x<>pt2.x)OR(Pt.y<>pt2.y) THEN WITH Attr DO
        BEGIN
          X:=X+pt2.X-pt.X;
          Y:=Y+pt2.Y-pt.Y;
          Pt:=Pt2;
          MoveWindow(HWindow,x,y,w,h,True);
        END;
        Application^.IdleAction;
        Yield;
      END;
    END ELSE
    BEGIN
      msg.message:=wm_nclbuttondown;
      msg.wparam:=htcaption;
      DefWndProc(msg);
    END;
  END ELSE DefWndProc(msg);
END;

PROCEDURE teff.wmlbuttondblclk(VAR msg:tmessage); BEGIN WMLButtonDown(msg) END;

PROCEDURE teff.wmrbuttondown;
VAR pt,pt2:tPoint;
BEGIN
  Pt:=MakePoint(msg.lParam);
  {GetCursorPos(pt);
  Dec(pt.x,Attr.x);
  Dec(pt.y,Attr.y);{}
  IF (pt.x>=8)AND(pt.x<=23)THEN
  BEGIN
    IF(pt.y<=(80+(pinfo^.numtracks*9)))AND(pt.y>=80)THEN
    BEGIN
      MOD_EnableAllChn(False);
      pt2.y:=(pt.y-80)DIV 9+1;
      MOD_EnableChn(pt2.y,NOT(Mod_IsEnabled(pt2.y)));
    END ELSE
    IF(pt.y<=75)AND(pt.y>=65)
    THEN MOD_EnableAllChn(False)
    ELSE PostMessage(HWindow,wm_close,0,0);
  END ELSE PostMessage(HWindow,wm_close,0,0);
  msg.result:=0;
END;

PROCEDURE teff.wmkeydown;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    vk_f1:helpme(@self,id_effwnd);
    vk_return,vk_escape:PostMessage(HWindow,wm_close,0,0);
    ELSE msg.result:=SendMessage(Parent^.HWindow,wm_keydown,msg.wparam,msg.lparam);
  END;
END;
FUNCTION  teff.GetClassName:PChar; BEGIN  GetClassName:='SWEBUZEFFWND'; END;
PROCEDURE teff.GetWindowClass(VAR WndClass: TWndClass);
BEGIN
  TWindow.GetWindowClass(WndClass);
  strcopy(WndClass.lpszclassname,GetClassName);
  WndClass.HIcon :=LoadIcon(hinstance, MakeIntResource(100));
  wndclass.style:=cs_bytealignclient OR cs_bytealignwindow OR cs_dblclks;
  wndclass.HBrBackGround:=0;
  wndclass.HCursor:=0;
END;

BEGIN
  effwnd:=0;
END.