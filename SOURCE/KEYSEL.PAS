UNIT keysel;
{ $C Fixed Preload Permanent}
INTERFACE
USES owindows,winprocs,odialogs,wintypes,strings,mmsystem,winunit,modc,omemory;

CONST
       id_bhelp=500;
       id_bok=501;
       id_bcancel=502;
       id_bdefault=503;
       id_selbox=504;


TYPE pmykeysellistbox=^tmykeysellistbox;
     tmykeysellistbox=object(tmylistbox)
       PROCEDURE   wmkeydown(VAR msg:tmessage); virtual wm_first+wm_keydown;
     END;

TYPE pkeywin=^tkeywin;
     tkeywin=object(TWindow)
       OldSysKeyWnd:Word;
       onoffbmp:hbitmap;
       keyINDEX:word;
       keytext:pmycenterstatic;
       theoldkey:akey;
       CONSTRUCTOR Init(Aparent:pwindowsobject; kx,ky,kw,kh:integer);
       DESTRUCTOR Done; virtual;
       PROCEDURE SetupWindow; VIRTUAL;
       PROCEDURE Paint(pdc:hdc; VAR ps:TPaintStruct);    virtual;
       PROCEDURE wmQueryNewPalette(VAR msg:tmessage);   VIRTUAL wm_first+wm_QueryNewPalette;
       PROCEDURE WMKeyDown(VAR Msg: TMessage);          VIRTUAL wm_First + wm_keydown;
       PROCEDURE WMKeyUp(VAR Msg: TMessage);            VIRTUAL wm_First + wm_keyup;
       PROCEDURE WMSETFOCUS(VAR Msg: TMessage);         VIRTUAL wm_First + wm_setfocus;
       PROCEDURE WMKILLFOCUS(VAR Msg: TMessage);        VIRTUAL wm_First + wm_killfocus;
       PROCEDURE WMLButtonDown(VAR Msg: TMessage);      virtual wm_First+wm_LButtonDown;
       PROCEDURE wmctlcolor(VAR msg:tmessage);          VIRTUAL wm_first+wm_ctlcolor;
       PROCEDURE DRAWFCRECT(adc:hdc);
       PROCEDURE DRAWKEY(AKEY:word);
       PROCEDURE BLITSTATE(ADC:HDC);
       PROCEDURE UPDATEBOX(INDEX:WORD);
     END;

TYPE pkeysel=^tkeysel;
     tkeysel=object(tmywindow)
       Cancel:boolean;
       pok,phelp,pcancel,pdefault:pbmpbutton;
       pselbox:pmykeysellistbox;
       pkeys:pkeywin;
       oldkeys:ARRAY[1..MAINHOTKEYS_BIS] OF akey;
       CONSTRUCTOR Init(aparent:pwindowsobject);
       DESTRUCTOR Done; virtual;
       PROCEDURE SetupWindow; virtual;
       PROCEDURE wmkeydown(VAR msg:tmessage);          virtual WM_FIRST+WM_KEYDOWN;
       PROCEDURE wmsyschar(VAR msg:tmessage);          virtual wm_first+wm_syschar;
       PROCEDURE wmdrawitem(VAR msg:tmessage);         virtual wm_first+wm_drawitem;
       PROCEDURE WMCommand(VAR msg:tmessage);          virtual wm_first+wm_command;
       PROCEDURE wmErasebkgnd(VAR Msg:tMessage);       virtual wm_first+wm_erasebkgnd;
       PROCEDURE handleHelp(VAR Msg:tMessage);         virtual id_first + id_bhelp;
       PROCEDURE handleOk(VAR Msg:tMessage);           virtual id_first + id_bok;
       PROCEDURE handleCancel(VAR Msg:tMessage);       virtual id_first + id_bcancel;
       PROCEDURE handledefault(VAR Msg:tMessage);      virtual id_first + id_bdefault;
     END;



IMPLEMENTATION
{************************LISTBOX****************************************}
PROCEDURE tmykeysellistbox.wmkeydown;
BEGIN
  {WENN CONTROL PRESSED THEN ->PAPA}
  msg.result:=0;
  IF keystate=1 THEN msg.result:=SendMessage(Parent^.HWindow,wm_keydown,msg.wparam,msg.lparam)
  ELSE CASE msg.wparam OF
     vk_tab:
       SetFocus(GetNextDlgTabItem(Parent^.HWindow,GetFocus,keystate=2));
     vk_f1,vk_help,vk_escape,vk_return,vk_execute:
       msg.result:=SendMessage(Parent^.HWindow,wm_keydown,msg.wparam,$FFFF);
     ELSE DefWndProc(msg);
   END;
END;

{********KEYSELECTION-HAUPTFENSTER****************************************}

CONST y_tkeysel=230;
      x_tkeysel=336;


CONSTRUCTOR tkeysel.Init;
VAR pc:pchar;
BEGIN
  Cancel:=true;
  pc:=MemAlloc(200);
  strcopy(pc,modc.shortver);
  strcat(pc,strpcopy(@pc[100],GetResString(2028)));
  INHERITED Init (AParent, pc,ws_sysmenu,notopmost);
  FreeMem(pc,200);
  Attr.style:=ws_popup OR ws_visible OR ws_border;
  Attr.x :=pwindow(Parent)^.Attr.x+(pwindow(Parent)^.Attr.w DIV 2)-(x_tkeysel DIV 2);
  Attr.y :=pwindow(Parent)^.Attr.y+(pwindow(Parent)^.Attr.h DIV 2)-(y_tkeysel DIV 2);
  Attr.w:=X_tkeysel+GetSystemMetrics(sm_cxborder);
  Attr.h:=Y_tkeysel+capdy+(2*GetSystemMetrics(sm_cyborder));
  pOk:=     New(PbmpButton, Init(@self, id_bok,162,CapDY+200,'bsok',false));
  pCancel:= New(PbmpButton, Init(@self, id_bcancel,203,CapDY+200,'bscancel',false));
  pdefault:=New(PbmpButton, Init(@self, id_bdefault,244,CapDY+200,'b_default',false));
  phelp:=   New(PbmpButton, Init(@self, id_bhelp,285,CapDY+200,'bshelp',false));
  pselbox:=New(pmykeysellistbox,Init(@self,id_selbox,17,capdy+16,300,116,lbs_sort,false));
  pkeys:=New(pkeywin,Init(@self,14,capdy+142,306,42));
  Move(mykeys,oldkeys,SizeOf(mykeys));
END;

DESTRUCTOR tkeysel.Done;
BEGIN
  IF Cancel THEN Move(oldkeys,mykeys,SizeOf(mykeys));
  Parent^.enable;
  INHERITED Done;
END;

PROCEDURE tkeysel.SetupWindow;
VAR i,j:integer;
BEGIN
  INHERITED SetupWindow;
  Parent^.disable;
  phelp^.enable; pdefault^.enable; pok^.enable; pcancel^.enable;
  oldfocus:=pselbox^.HWindow;
  FOR i:=2100 TO (2100+MAINHOTKEYS_BIS-1) DO
  WITH pselbox^ DO
  BEGIN
    j:=AddString(getresstring(i));
    setitemdata(j,longint(i-2099));
  END;
  pselbox^.SetSelIndex(0);
  pkeys^.updatebox(word(pselbox^.getitemdata(pselbox^.GetSelIndex)))
END;

PROCEDURE tkeysel.wmErasebkgnd(VAR Msg:tMessage);
VAR i:integer;
    rect:trect;
BEGIN
  paintcaption(msg.wParam);
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  framefilled3d(msg.wParam,0,CapDY,x_tkeysel,y_tkeysel,2,cForeGnd,up); {Hauptrahmen}
  WITH rect DO BEGIN left:=7; top:=CapDY+7; right:=328; bottom:=top+185; END;
  FrameRect(msg.wParam,rect,brBlack);
  frame3d(msg.wParam,pok^.Attr.x-1,pok^.Attr.y-1,167,23,1,down);
  WITH pselbox^.Attr DO frame3d(msg.wParam,x-1,y-1,w+2,h+2,1,down);
  blit(HWindow,35,pok^.Attr.y,94,20,0,0,m_label);
  INHERITED wmErasebkgnd(Msg);
END;

PROCEDURE tkeysel.handleOk;
BEGIN
  Cancel:=false;
  PostMessage(HWindow,wm_close,0,0);
END;
PROCEDURE tkeysel.handleHelp; BEGIN helpme(@self,id_keyseldlg); END;
PROCEDURE tkeysel.handlecancel; BEGIN PostMessage(HWindow,wm_close,0,0); END;

PROCEDURE tkeysel.handledefault;
BEGIN
  mykeys[pkeys^.keyindex].key:=defkeys[pkeys^.keyindex].key;
  mykeys[pkeys^.keyindex].state:=defkeys[pkeys^.keyindex].state;
  pkeys^.updatebox(pkeys^.keyindex);
  pkeys^.focus;
END;

PROCEDURE tkeysel.WMCommand;
BEGIN
  IF msg.wparam=id_selbox THEN
  BEGIN
    IF (msg.lparamhi=lbn_selchange)
    THEN pkeys^.updatebox(word(pselbox^.getitemdata(pselbox^.GetSelIndex))) ELSE
    IF (msg.lparamhi=lbn_dblclk) THEN pkeys^.focus;
  END ELSE INHERITED WMCommand(msg);
END;

PROCEDURE tkeysel.wmkeydown;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    vk_f1,vk_help       :handlehelp(Msg);
    vk_escape           :handlecancel(Msg);
    vk_return,vk_execute:IF oldfocus=pselbox^.HWindow THEN pkeys^.focus ELSE handleok(Msg);
    ELSE DefWndProc(msg);
  END;
END;

PROCEDURE tkeysel.wmsyschar;
BEGIN
 CASE char(Lo(msg.wparam)) OF
 #0:;
   ELSE BEGIN INHERITED wmsyschar(msg); Exit; END;
 END;
 msg.result:=-1;
END;

PROCEDURE tkeysel.wmdrawitem;
VAR oldfont:hfont;
    VG,HG:LONGINT;
    s:STRING[40];
    drawframe:boolean;
BEGIN
  WITH pdrawitemstruct(msg.lparam)^ DO
  IF (ctltype=odt_listbox)AND(ctlid=id_selbox) THEN
  BEGIN
    IF PaletteInstalled THEN SelectPalette(hDC,hpal,false);
    drawframe:=false;
    HG:=cHGLB; VG:=cVGLB;
    CASE itemaction OF
      oda_focus     :
        CASE itemstate OF
          ods_disabled:VG:=cVGGrayBox;
          ods_focus
          +ods_selected:BEGIN drawframe:=true; VG:=cHGLB; HG:=cVGLB; END;
          ods_focus    :drawframe:=true;
          1           : BEGIN HG:=cVGLB; VG:=cHGLB; END;
        END;
      oda_drawentire:IF itemstate =ods_selected THEN BEGIN HG:=cVGLB; VG:=cHGLB; END;
    END;
    oldfont:=SelectObject(hdc,ANSIhandle);
    SetBkMode(hdc,transparent);
    SetTextAlign(hdc,ta_left OR ta_top OR ta_noupdatecp);
    SetBkColor(hDC,hg); SetTextColor(hdc,vg);
    IF pselbox^.GetString(s,itemid)>0 THEN
    BEGIN
      s:=s+#0;
      ExtTextOut(hdc,rcitem.left,rcitem.top,eto_opaque,@rcitem,@s[1],Length(s)-1,NIL);
    END;
    SelectObject(hdc,oldfont);
    IF drawframe THEN DrawFocusRect(hdc,rcitem);
  END;
END;

{****************HOTKEY-CHILD**************************************************+}
CONSTRUCTOR tkeywin.Init;
BEGIN
  INHERITED Init(Aparent,'');
  WITH Attr DO
  BEGIN
    style:=ws_visible OR ws_child  OR ws_tabstop;
    x:=kx; y:=ky; w:=kw; h:=kh;
  END;
  onoffbmp:=LOADMyBITMAP(hinstance,'HKONOFF');
  keytext:=New(pmycenterstatic,Init(@self,0,'',20,8,Attr.w-40,fh,20));
  keyindex:=1;
END;

PROCEDURE tkeywin.SetupWindow;
BEGIN
  OldSysKeyWnd:=pBUZApplication(Application)^.HSysKeyWnd;
  pBUZApplication(Application)^.HSysKeyWnd:=HWindow;
  INHERITED SetupWindow;
END;

DESTRUCTOR tkeywin.Done;
BEGIN
  DeleteMyBitMap(onoffbmp);
  INHERITED Done;
  pBUZApplication(Application)^.HSysKeyWnd:=OldSysKeyWnd;
END;

PROCEDURE tkeywin.WmQueryNewPalette(VAR MSG:tMessage);
BEGIN
  pMyWindow(Application^.MainWindow)^.WmQueryNewPalette(MSG);
END;

PROCEDURE tkeywin.Paint;
VAR oldfont:thandle;
BEGIN
  IF PaletteInstalled THEN SelectPalette(ps.hdc,hpal,false);
  framefilled3d(pdc,0,0,Attr.w,Attr.h,1,cForeGnd,down);
  WITH keytext^.Attr DO frame3d(pdc,x-1,y-1,w+2,h+2,1,down);
  DRAWFCRECT(pdc);
  oldfont:=SelectObject(pdc,ANSIhandle);
  SetTextAlign(pdc,ta_left OR ta_top OR ta_noupdatecp);
  SetTextColor(pdc,cVGStatic); SetBkColor(pdc,cHGStatic);
  TextOut(pdc,50,25 ,'Control',7);
  TextOut(pdc,150,25,'Shift',5);
  TextOut(pdc,250,25,'Alt',3);
  updatebox(keyindex);
  SelectObject(pdc,oldfont);
END;

PROCEDURE tkeywin.DRAWFCRECT(adc:hdc);
VAR Rect:TRECT; PDc:HDC;
BEGIN
  GetClientRect(HWindow,Rect);
  WITH Rect DO BEGIN dec(right,2); dec(bottom,2); inc(left,2); inc(top,2); END;
  IF ADc=0 THEN PDC:=GetDC(HWindow) ELSE pdc:=adc;
  IF PaletteInstalled THEN SelectPalette(pdc,hpal,false);
  IF GetFocus=HWindow THEN FrameRect(pdc,rect,brBlack) ELSE FrameRect(pdc,rect,BrStatic);
  IF adc=0 THEN ReleaseDC(HWindow,pdc);
END;

PROCEDURE tkeywin.UPDATEBOX(INDEX:WORD);
BEGIN
  keyindex:=index;
  blitstate(0);
  drawkey(mykeys[keyindex].key);
END;

PROCEDURE tkeywin.BLITSTATE;
VAR pdc,memdc:hdc;
BEGIN
  IF ADc=0 THEN PDC:=GetDC(HWindow) ELSE pdc:=adc;
  IF PaletteInstalled THEN SelectPalette(pdc,hpal,false);
  memdc:=CreateCompatibleDC(pdc);
  SelectObject(memdc,onoffbmp);
  {CTRL}
  IF (mykeys[keyindex].state AND 1) =1 THEN BitBlt(pdc,36,28,10,5,memdc,10,0,srccopy)
                                       ELSE BitBlt(pdc,36,28,10,5,memdc,0,0,srccopy);
  {SHIFT}
  IF (mykeys[keyindex].state AND 2) =2 THEN BitBlt(pdc,136,28,10,5,memdc,10,0,srccopy)
                                       ELSE BitBlt(pdc,136,28,10,5,memdc,0,0,srccopy);
  {ALT}
  IF (mykeys[keyindex].state AND 4) =4 THEN BitBlt(pdc,236,28,10,5,memdc,10,0,srccopy)
                                       ELSE BitBlt(pdc,236,28,10,5,memdc,0,0,srccopy);
  DeleteDC(memdc);
  IF adc=0 THEN ReleaseDC(HWindow,pdc);
END;

PROCEDURE tkeywin.DRAWKEY;
VAR s:STRING[20];
    i:Byte;
BEGIN
  s:='';
  CASE AKEY OF
    VK_MENU,vk_SHIFT,vk_CONTROL,0:s:='';
    VK_f1..VK_F16:BEGIN Str(Lo((akey)-$6f),s); s:='F'+s; END;
    VK_ESCAPE  :s:='ESCAPE';
    VK_Tab     :s:='TAB';
    VK_Pause   :s:='PAUSE';
    VK_Capital :s:='CAPS LOCK';
    VK_NumLock :s:='NUM LOCK';
    $91        :s:='SROLL LOCK';
    VK_BACK    :s:='BACKSPACE';
    VK_SPACE   :s:='SPACE';
    VK_PRIOR   :s:='PAGE UP';
    VK_NEXT    :s:='PAGE DOWN';
    VK_END     :s:='END';
    VK_HOME    :s:='HOME';
    VK_LEFT    :s:='CURSOR LEFT';
    VK_RIGHT   :s:='CURSOR RIGHT';
    VK_UP      :s:='CURSOR UP';
    VK_DOWN    :s:='CURSOR DOWN';
    VK_INSERT  :s:='INSERT';
    VK_DELETE  :s:='DELETE';
    vk_ADD     :s:='NUMPAD PLUS';
    vk_SUBTRACT:s:='NUMPAD MINUS';
    vk_MULTIPLY:s:='NUMPAD MULTIPLY';
    vk_DIVIDE  :s:='NUMPAD DIVIDE';
    vk_DECIMAL :s:='NUMPAD SEPERATOR';
    vk_return  :s:='ENTER';
    vk_numpad0..vk_numpad9:BEGIN Str(Lo((akey)-$60),s); s:='NUMPAD '+s; END;
    $2a,$BA..$C0,$DB..$E4,$E6..$F5:
      FOR i:=0 TO 255 DO IF VkKeyScan(i)=Akey THEN BEGIN s:=Char(i); Break; END;
    ELSE s:=CHAR(AKEY);
  END;
  keytext^.setstringtext(s);
  mykeys[keyindex].key:=akey;
END;

PROCEDURE tkeywin.wmsetfocus;
BEGIN
  DRAWFCRECT(0);
  theoldkey:=mykeys[keyindex];
  PostMessage(Parent^.HWindow,wm_ihavethefocus,HWindow,0);
  DefWndProc(msg);
END;
PROCEDURE tkeywin.wmkillfocus;
BEGIN
  IF mykeys[keyindex].key=0 THEN
  BEGIN
    DRAWKEY(theoldkey.Key);
    mykeys[keyindex].State:=theoldkey.state;
  END;
  DRAWFCRECT(0);
  blitstate(0);
  DefWndProc(msg);
END;
PROCEDURE tkeywin.WMLButtonDown; BEGIN focus; DefWndProc(msg); END;

PROCEDURE tkeywin.wmkeydown;
BEGIN
  IF GetFocus=HWindow THEN
  BEGIN
    msg.result:=0;
    IF (KeyState=4)AND(msg.wparam=vk_F4)THEN
    BEGIN SetFocus(GetNextDlgTabItem(Parent^.HWindow,GetFocus,true)); Exit; END;
    IF isvalidkey(msg.wparam) THEN
    BEGIN
      mykeys[keyindex].state:=keystate;
      IF msg.wparam IN[vk_menu,vk_shift,vk_control] THEN DRAWKEY(0) ELSE DRAWKEY(msg.wparam);
    END ELSE
    BEGIN
      mykeys[keyindex].state:=0;
      drawkey(0);
      msg.result:=1;
    END;
    BLITSTATE(0);
  END ELSE msg.result:=1;
END;

PROCEDURE tkeywin.wmkeyup;
BEGIN
  IF GetFocus=HWindow THEN
  BEGIN
    IF isvalidkey(msg.wparam) THEN
    BEGIN
      IF mykeys[keyindex].key=0 THEN
      BEGIN
        mykeys[keyindex].state:=keystate;
        IF mykeys[keyindex].state=0 THEN
        drawkey(0); {LEER-STRING MALEN}
      END ELSE theoldkey:=mykeys[keyindex];
      msg.result:=0;
    END ELSE msg.result:=1;
    BLITSTATE(0);
  END ELSE msg.result:=1;
END;
PROCEDURE tkeywin.wmctlcolor;
BEGIN
  IF PaletteInstalled THEN SelectPalette(msg.wparam,hPal,false);
  CASE msg.lparamhi OF
    ctlcolor_edit:
    BEGIN msg.result:=BrEdit; SetTextColor(msg.wparam,cVGEdit); SetBkColor(msg.wparam,cHGEdit); END;
    ctlcolor_listbox:
    BEGIN msg.result:=BrListbox; SetTextColor(msg.wparam,cVGLB); SetBkColor(msg.wparam,cHGLB); END;
    ctlcolor_static:
    BEGIN msg.result:=BrStatic; SetTextColor(msg.wparam,cVGStatic); SetBkColor(msg.wparam,cHGStatic); END;
    CTLCOLOR_GRAYBOX:
    BEGIN msg.result:=BrGrayBox; SetTextColor(msg.wparam,cVGGrayBox); SetBkColor(msg.wparam,cHGGrayBox); END;
    CTLCOLOR_INVSTATIC:
    BEGIN msg.result:=BrInvStatic; SetTextColor(msg.wparam,cVGInvStatic); SetBkColor(msg.wparam,cHGInvStatic); END;
    ELSE DefWndProc(msg);
  END;
END;


END.