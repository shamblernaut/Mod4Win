UNIT mulbox;
{ $C Fixed Preload Permanent}
INTERFACE

USES WinTypes,Win31,OWindows,ODialogs,winunit,newopen;

TYPE pml=^tml;
     tml=object(tmylistbox)
       PROCEDURE wmkeydown(VAR msg:tmessage);        virtual WM_FIRST+WM_KEYDOWN;
       PROCEDURE WMRButtonDown(VAR Msg: TMessage);   VIRTUAL wm_First+wm_RButtonDown;
     END;

TYPE  pEnableWnd=^tEnableWnd;
      tEnableWnd=RECORD
                   Window:hWnd;
                   hTask:tHandle;
                   NumWnd:Byte;
                   Items:ARRAY[0..255]OF
                   RECORD
                     Wnd:hWnd;
                     Enabled:Boolean;
                   END;
                 END;

TYPE pviewwin=^tviewwin;
     tviewwin=object(TWindow)
       pdir:Pmydirs;
       listbox:Pml;
       ordermode:Byte;
       border,bname,bext:Pbmpbutton;
       aktsel:integer;
       capdy:byte;
       oldfocus:hwnd;
       myscroll:pscrollbar;
       maxlines,maxcols:Integer;
       CONSTRUCTOR Init(Aparent:PwindowsObject; atitle:pchar; pdirs:pmydirs; akt_pos:integer);
       DESTRUCTOR Done; virtual;
       PROCEDURE SetupWindow; virtual;
       PROCEDURE wmQueryNewPalette(VAR msg:tmessage); VIRTUAL wm_first+wm_QueryNewPalette;
       PROCEDURE wmdrawitem(VAR msg:tmessage);       VIRTUAL wm_first+wm_drawitem;
       PROCEDURE WMCommand(VAR msg:tmessage);        VIRTUAL wm_first+wm_command;
       PROCEDURE wmsyschar(VAR msg:tmessage);        virtual wm_first+wm_syschar;
       PROCEDURE wmkeydown(VAR msg:tmessage);        virtual WM_FIRST+WM_KEYDOWN;
       PROCEDURE WMCTLCOLOR(VAR msg:tmessage);       VIRTUAL wm_first+wm_CTLCOLOR;
       PROCEDURE WMEraseBkGnd(VAR msg:tmessage);     VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE wmsetfocus(VAR msg:tmessage);       VIRTUAL wm_first+wm_setfocus;
       PROCEDURE wmIHaveTheFocus(VAR msg:tmessage);  VIRTUAL wm_first+wm_Ihavethefocus;
       PROCEDURE WMLButtonDown(VAR Msg: TMessage);   VIRTUAL wm_First+wm_LButtonDown;
       PROCEDURE WMRButtonDown(VAR Msg: TMessage);   VIRTUAL wm_First+wm_RButtonDown;
       PROCEDURE wmmeasureitem(VAR msg:tmessage);    VIRTUAL wm_first+wm_measureitem;
       PROCEDURE WMHScroll(VAR msg:tmessage);        VIRTUAL wm_first+wm_hscroll;
       PROCEDURE putin(How:Byte);
       PROCEDURE Ok;
     END;

TYPE PBoolean=^Boolean;
     PString =^STRING;
TYPE PaskWin=^TAskWin;
     TAskWin=OBJECT(TMyWindow)
         OldActiveWindow:hWnd;
         ResBuf:PChar;
         WavName:PString;
         IsModal:Boolean;
         PCancel:PBoolean;
         Epath,EName  :PMyEdit;
         PHDSel,Bok,Bquit,BHelp :PBmpButton;
         Comments:ARRAY[1..3] OF PMyStatic;
       CONSTRUCTOR Init(AParent:PWindowsObject; APChar:PChar; AFileName:PString; PCn:PBoolean);
       DESTRUCTOR Done;                              VIRTUAL;
       PROCEDURE MakeModal;
       {...}
       PROCEDURE MakeDlg(VAR EnabledWnd:pEnableWnd);
       PROCEDURE EndDlg(VAR EnabledWnd:pEnableWnd);
       PROCEDURE SetupWindow;                        VIRTUAL;
       PROCEDURE WMEraseBkGnd(VAR msg:tmessage);     VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE WMCommand(VAR msg:tmessage);        VIRTUAL wm_first+wm_command;
       PROCEDURE wmkeydown(VAR msg:tmessage);        virtual WM_FIRST+WM_KEYDOWN;
       PROCEDURE wmsyschar(VAR msg:tmessage);        virtual wm_first+wm_syschar;
     END;

CONST  byname =1;
       byext  =2;
       byorder=3;

       wm_songselected=wm_user+1334;

       id_order=120;
       id_name=121;
       id_ext=122;
       id_scroll=123;

IMPLEMENTATION

USES Strings, WinProcs, WinDOS, ModC, Files;

PROCEDURE tml.wmkeydown;
BEGIN
  CASE msg.wparam OF
  vk_left :SendMessage(Parent^.HWindow,wm_hscroll,sb_pageup,0);
  vk_right:SendMessage(Parent^.HWindow,wm_hscroll,sb_pagedown,0);
    ELSE INHERITED wmkeydown(msg);
  END;
  msg.result:=0;
END;

PROCEDURE tml.WMRButtonDown;
BEGIN
  PostMessage(Parent^.HWindow,wm_keydown,vk_escape,0);
END;


{***********************BOX-FENSTER**********************************************************}
PROCEDURE tViewWin.WmQueryNewPalette(VAR MSG:tMessage);
BEGIN
  pMyWindow(Application^.MainWindow)^.WmQueryNewPalette(MSG);
END;

CONST lbx:Integer=610;
      lby:Integer=230;
      wmin=164;
      hmin=54;
CONSTRUCTOR tviewwin.Init;
VAR  test:longInt;
     CurPos:tPoint;
BEGIN
  maxcols:=(2*GetSystemMetrics(sm_CXScreen)DIV 3 DIV 19 DIV Fw);
  maxlines:=(2*GetSystemMetrics(sm_CYScreen)DIV 3 DIV(FH+1));
  capdy:=0;
  pdir:=pdirs;
  Test:=0;
  aktsel:=akt_pos;
  INHERITED Init (AParent, ATitle);
  WITH Attr DO
  BEGIN
    style:=ws_popup OR ws_visible OR ws_border;
    exStyle:=$8;
    GetCursorPos(CurPos); x:=CurPos.X; y:=CurPos.Y;
    w    :=wmin+(2*GetSystemMetrics(sm_cxborder));
    h    :=hmin+capdy+(2*GetSystemMetrics(sm_cyborder));
    IF pdir^.songcount>1 THEN
    BEGIN
      IF (pdir^.songcount-1)>maxLines THEN
      BEGIN
        TEST:=((pDir^.Songcount-1) DIV maxlines)+1;
        IF Test>maxcols THEN
        BEGIN
          w:=w+(maxcols-1)*19*FW;
          test:=1;
        END ELSE
        BEGIN
          w:=w+(Test-1)*(19*fw);
          test:=0;
        END;
        inc(h,(maxlines-1)*(FH+1));
      END ELSE INC(h,(pdir^.songcount-1)*(fh+1));
    END;
    setcorrectwinpos(x,y,w,h);
 END;
 listbox:=New(pml,Init(@self,200,7,capdy+7,Attr.w-16,Attr.h-41-capdy,lbs_multicolumn OR lbs_sort,false));
 listbox^.Attr.style:=listbox^.Attr.style AND(NOT ws_vscroll)AND(NOT ws_vscroll);
 border:=New  (PbmpButton, Init(@self, id_order,Attr.w-149,Attr.h-28,'b_order',false));
 bname :=New  (PbmpButton, Init(@self, id_name ,Attr.w-102,Attr.h-28,'b_name',false));
 bext  :=New  (PbmpButton, Init(@self, id_ext  ,Attr.w-55,Attr.h-28,'b_ext',false));
 IF test=1 THEN
 BEGIN
   myscroll:=New(pscrollbar,Init(@self,id_scroll,6,listbox^.Attr.y+listbox^.Attr.h+7,listbox^.Attr.w-145,
                                     border^.Attr.h-2,true));
   myscroll^.Attr.style:=myscroll^.Attr.style AND NOT ws_tabstop;
 END ELSE myscroll:=NIL;
END;

DESTRUCTOR tviewwin.Done;
VAR s:STRING[40];
BEGIN
  Str(ordermode,s); writeinistring(s_state,'LOOKLB_MODE'+#0,s);
  Parent^.enable;
  INHERITED Done;
END;

PROCEDURE tviewwin.SetupWindow;
BEGIN
  INHERITED SetupWindow;
  border^.enable; bname^.enable; bext^.enable;
  oldfocus:=listbox^.HWindow;
  SendMessage(listbox^.HWindow,lb_setcolumnwidth,fw*19,0);
  Parent^.disable;
  IF myscroll<>NIL THEN WITH myscroll^ DO
  BEGIN
    SetRange(1,pdir^.songcount);
    PageMagnitude:=maxlines;
  END;
  ordermode:=byte(GetPrivateProfileInt(s_state,'LOOKLB_MODE',byorder,INIDIR));
  IF ordermode>byext THEN ordermode:=byorder;
  putin(ordermode);
END;

PROCEDURE tviewwin.WmEraseBkGnd;
  PROCEDURE RePaint(What:pWindow); FAR;
  BEGIN
    IF (what^.Attr.style AND ws_child)=ws_child THEN UpdateWindow(what^.HWindow);
  END;
BEGIN
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  framefilled3d(msg.wParam,0,CapDY,Attr.w-2,Attr.h-2,2,cForeGnd,up);   {Hauptrahmen}
  WITH listbox^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,2,down);
  IF myscroll<>NIL THEN WITH myscroll^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,2,down);
  WITH border^.Attr DO frame3d(msg.wParam,x-1,y-1,144,h+2,1,down);
  ForEach(@RePaint);
  ValidateRect(HWindow,NIL);
  msg.result:=1;
END;

PROCEDURE tviewwin.putin(How:Byte);
VAR i:Integer; s,s2:STRING[20]; j:longint;
BEGIN
  ordermode:=How;
  SendMessage(listbox^.HWindow,wm_setredraw,0,0);
  listbox^.ClearList;
  CASE How OF
  byname:
  BEGIN
    FOR i:=0 TO pdir^.songcount-1 DO
    BEGIN
      s:=pdir^.getsongstring(i);
      s2:=Copy(s,6,8); s2:=s2+'.'+filetypesasstring[byte(s[1])];
      s2:=s2+#0; j:=listbox^.AddString(s2);
      listbox^.setitemdata(j,longint(i+1));
    END;
  END;
  byext:
    BEGIN
      FOR i:=0 TO pdir^.songcount-1 DO
      BEGIN
        s:=pdir^.getsongstring(i);
        s2:=filetypesasstring[byte(s[1])]+Copy(s,6,8); s2:=s2+'.';
        IF Length(s2) = 11 THEN Insert(' ', s2, 3);  { temp fix for .xm }
        s2:=s2+#0; j:=listbox^.AddString(s2);
        listbox^.setitemdata(j,longint(i+1));
      END;
    END;
  byorder:
    BEGIN
      FOR i:=pdir^.songcount-1 DOWNTO 0 DO
      BEGIN
        s:=pdir^.getsongstring(i);
        s2:=Copy(s,6,8); s2:=s2+'.'+filetypesasstring[byte(s[1])];
        s2:=s2+#0; listbox^.InsertString(@s2[1],0);
        listbox^.setitemdata(0,longint(i+1));
      END;
    END;
  END;
  FOR i:=0 TO pdir^.songcount-1 DO IF (listbox^.getitemdata(i)-1)=aktsel THEN Break;
  listbox^.SetSelIndex(i);
  IF myscroll=NIL THEN SendMessage(listbox^.HWindow,lb_settopindex,0,0)
                  ELSE SendMessage(HWindow,wm_hscroll,sb_thumbposition,listbox^.GetSelIndex+1);
  SendMessage(listbox^.HWindow,wm_setredraw,1,0);
END;


PROCEDURE Tviewwin.wmdrawitem;
VAR oldfont:hfont;
    a,b    :longint;
    c      :integer;
    s      :STRING[30];
    s2     :STRING[6];
    x:byte; y:longint;
BEGIN
  WITH pdrawitemstruct(msg.lparam)^ DO
  BEGIN
   IF (ctltype=odt_listbox) THEN
   BEGIN
     IF PaletteInstalled THEN SelectPalette(hdc,hpal,false);
     b:=cVGLB;
     a:=cHGLB;
     CASE itemaction OF
       oda_focus,oda_select
                      :CASE itemstate OF
                       ods_disabled :BEGIN a:=cHGLB; b:=cVGGrayBox; END;
                       ods_focus
                       +ods_selected:BEGIN b:=cHGLB; a:=cVGLB; END;
                       1            :BEGIN a:=cVGGrayBox; b:=cHGLB; END;
                      END;
       oda_drawentire:IF itemstate=1 THEN IF NOT(GetActiveWindow=HWindow)
                                          THEN BEGIN a:=cVGGrayBox; b:=cHGLB; END
                                          ELSE BEGIN b:=cHGLB; a:=cVGLB; END;
     END;
     oldfont:=SelectObject(hdc,ansihandle);
     SetBkMode(hdc,transparent); SetBkColor(hdc,a); SetTextColor(hdc,b);
     SetTextAlign(hdc,ta_left OR ta_top OR ta_noupdatecp);
     c:=listbox^.GetString(s,itemid);
     IF c>=0 THEN
     BEGIN
       dec(rcitem.right,fw); inc(rcitem.left,fw); inc(rcitem.top,2); inc(rcitem.bottom,2);
       IF OrderMode<>ByOrder THEN s[1]:=UpCase(s[1]);
       IF Ordermode=byext THEN s:=Copy(s,4,9)+Copy(s,1,3);
       y:=listbox^.getitemdata(itemid);
       Str(y:4,s2); s2:=s2+':'; s2[Length(s2)+1]:=#0;
       c:=rcitem.right; rcitem.right:=rcitem.left+5*fw;
       IF b=cVGLB THEN SetTextColor(hdc,cVGLB);
       ExtTextOut(hdc,rcitem.left,rcitem.top,eto_opaque,@rcitem,pchar(@s2[1]),byte(s2[0]),NIL);
       s[Length(s)+1]:=#0;
       rcitem.right:=c; inc(rcitem.left,5*fw);
       SetTextColor(hdc,b);
       ExtTextOut(hdc,rcitem.left,rcitem.top,eto_opaque,@rcitem,pchar(@s[1]),byte(s[0]),NIL);
     END;
   END;
 END;
END;

PROCEDURE tviewwin.Ok;
BEGIN
  PostMessage(HWindow,wm_close,0,0);
  PostMessage(Parent^.HWindow,wm_songselected,0,listbox^.getitemdata(listbox^.GetSelIndex)-1);
END;

PROCEDURE tviewwin.WMCommand;
BEGIN
  CASE msg.wparam OF
    id_order:BEGIN IF ordermode<>id_order THEN putin(byorder); listbox^.focus; END;
    id_name :BEGIN IF ordermode<>id_name THEN putin(byname); listbox^.focus; END;
    id_ext  :BEGIN IF ordermode<>id_ext  THEN putin(byext); listbox^.focus; END;
    200     :IF msg.lparamhi=lbn_selchange THEN
             BEGIN
               aktsel:=listbox^.getitemdata(listbox^.GetSelIndex)-1;
               IF myscroll<>NIL THEN myscroll^.SetPosition(listbox^.GetSelIndex+1);

             END ELSE IF msg.lparamhi=lbn_dblclk THEN Ok;
    ELSE INHERITED WMCommand(msg);
  END;
END;

PROCEDURE tviewwin.wmsyschar;
VAR ch: char;
BEGIN
  ch := UpCase (char(Lo(msg.wparam)));
  CASE ch OF
    'N':SendMessage(HWindow,wm_command,id_order,0);
    'A':SendMessage(HWindow,wm_command,id_name,0);
    'E':SendMessage(HWindow,wm_command,id_ext,0);
    ELSE BEGIN DefWndProc(msg); Exit; END;
  END;
  msg.result:=-1;
END;

PROCEDURE tviewwin.wmkeydown;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    vk_return,vk_execute:Ok;
    vk_f1,vk_help       :helpme (@self, id_selbox);
    vk_escape           :PostMessage(HWindow,wm_close,0,0);
    ELSE DefWndProc(msg);
  END;
END;

PROCEDURE Tviewwin.wmctlcolor;
BEGIN
  CASE msg.lparamhi OF
    ctlcolor_edit,ctlcolor_listbox:
      BEGIN
        IF PaletteInstalled THEN SelectPalette(msg.wparam,hpal,false);
        msg.result:=BrBlack;
        SetTextColor(msg.wparam,cVGLB);
        SetBkColor(msg.wparam,cHGLB);
      END;
    ELSE DefWndProc(msg);
  END;
END;

PROCEDURE Tviewwin.wmsetfocus;      BEGIN SetFocus(oldfocus); msg.result:=0; END;
PROCEDURE Tviewwin.wmIHaveTheFocus; BEGIN oldfocus:=msg.wparam; msg.result:=0; END;

PROCEDURE tviewwin.WMLButtonDown;
VAR pt,pt2:tPoint;

  PROCEDURE RePaint(What:pWindow); FAR;
  BEGIN
    IF (what^.Attr.style AND ws_child)=ws_child THEN UpdateWindow(what^.HWindow);
  END;

BEGIN
  IF FullMove THEN WITH Attr DO
  BEGIN
    {move window}
    Pt:=MakePoint(msg.lParam);
    {GetCursorPos(pt);
    Dec(pt.x,x);
    Dec(pt.y,y);{}
    pt2.x:=pt.x;
    pt2.y:=pt.y;
    WHILE (GetAsyncKeyState(VK_lButton)AND$8000)>0 DO
    BEGIN
      GetCursorPos(tPoint((@Attr.x)^));
      Dec(x,pt.x);
      Dec(Y,pt.y);
      IF (x<>pt2.x)OR(y<>pt2.y) THEN
      BEGIN
        pt2.x:=x; pt2.y:=y;
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
END;

PROCEDURE tviewwin.wmmeasureitem;
BEGIN
  WITH pmeasureitemstruct(msg.lparam)^ DO
  BEGIN
    itemwidth:= Fw*17;
    itemheight:=Fh+1;
    itemdata:=lb_addstring;
  END;
END;

PROCEDURE tviewwin.WMHScroll;
VAR l:longint;
BEGIN
  l:=-1;
  CASE msg.wparam OF
    sb_linedown:WITH listbox^ DO IF GetSelIndex<(GetCount-1) THEN l:=GetSelIndex+1;
    sb_lineup  :WITH listbox^ DO IF GetSelIndex>0            THEN l:=GetSelIndex-1;
    sb_bottom  :WITH listbox^ DO l:=GetCount-1;
    sb_top     :WITH listbox^ DO l:=0;
    sb_pagedown:WITH listbox^ DO
                BEGIN
                   l:=GetSelIndex; inc(l,maxlines); IF l>GetCount THEN l:=GetCount-1;
                END;
    sb_pageup  :WITH listbox^ DO
                BEGIN
                   l:=GetSelIndex; dec(l,maxlines); IF l<0 THEN l:=0;
                END;
    sb_thumbposition,sb_thumbtrack:WITH listbox^ DO l:=msg.lparamlo-1;
  END;
  IF l>-1 THEN listbox^.SetSelIndex(l);
  IF myscroll<>NIL THEN SetScrollPos(myscroll^.HWindow,sb_ctl,listbox^.GetSelIndex+1,true);
  msg.result:=0;
END;

PROCEDURE tviewwin.wmrbuttondown;
BEGIN
  PostMessage(HWindow,wm_close,0,0);
END;
{*************************ENDE SELBOX**********************************************************}
CONST W_Sel=361 {(31+9) * Fw + 2*10 + 21};
      H_Sel=54;
      id_path=100;
      id_EName=101;
      id_HDSel=102;
      id_ok=103;
      id_Quit=104;
      id_Help=105;
CONSTRUCTOR TAskWin.Init;
VAR s: STRING[50];
BEGIN
  OldActiveWindow:=GetActiveWindow;
  IsModal:=False;
  IF APChar=NIL THEN FAIL;
  ResBuf:=APChar;
  WavName:=AFileName;
  PCancel:=PCn;
  s:=modc.shortver+getresstring(29)+#0;
  INHERITED Init(AParent,@s[1],ws_sysmenu,notopmost);
  WITH Attr DO
  BEGIN
    style:=ws_popup OR ws_visible OR ws_border;
    w :=W_Sel+(2*GetSystemMetrics(sm_cxborder));
    h :=H_Sel+(2*GetSystemMetrics(sm_cyborder))+capdy;
    x :=(GetSystemMetrics (sm_cxscreen) - w) DIV 2;
    y :=(GetSystemMetrics (sm_cyscreen) - h) DIV 2;
    setcorrectwinpos(x,y,w,h);
  END;
  EPath:=New(pmyedit,Init(@self, id_path,ResBuf,9*fw+10,CapDY+9,W_Sel-(9*fw+20)-21,Fh,fsDirectory+1,true));
  comments[1]:=New(pmystatic,Init(@self,0,NIL,EPath^.Attr.X-9*fw-4,EPath^.Attr.Y    ,5*Fw,Fh,5));
  pHDSel     :=New(PbmpButton, Init(@self,id_HDSel,EPath^.Attr.X+EPath^.Attr.W+5,EPath^.Attr.Y-4,'b_browse',false));
  EName:=New(pmyedit,Init(@self, id_Ename,NIL,9*fw+10,Epath^.Attr.Y+Epath^.Attr.H+10,8*Fw,Fh,fsFileName+1,true));
  comments[2]:=New(pmystatic,Init(@self,0,NIL,EName^.Attr.X-9*fw-4,EName^.Attr.Y    ,9*Fw,Fh,9));
  comments[3]:=New(pmystatic,Init(@self,0,NIL,EName^.Attr.X+EName^.Attr.W+2,EName^.Attr.Y,4*Fw ,Fh,5));
  bOk    :=New(PbmpButton, Init(@self, id_ok    ,Epath^.Attr.X+ EPAth^.Attr.W-123 ,
               EPath^.Attr.Y+EPath^.Attr.H+6,'bsok',false));
  bQuit  :=New(PbmpButton, Init(@self, id_quit,Epath^.Attr.X+ EPAth^.Attr.W-82 ,
               EPath^.Attr.Y+EPath^.Attr.H+6,'bscancel',false));
  bhelp  :=New(PbmpButton, Init(@self, id_help  ,Epath^.Attr.X+ EPAth^.Attr.W-41 ,
               EPath^.Attr.Y+EPath^.Attr.H+6,'bshelp',false));
END;

DESTRUCTOR TAskWin.Done;
BEGIN
  INHERITED Done;
  IsModal:=False;
END;

CONST WndEnable:pEnableWnd=NIL;
FUNCTION DisableWnd(hWind:hWnd;lp:Longint):BOOL;EXPORT;
BEGIN
  DisableWnd:=True;
  IF (GetWindowLong(hWind,GWL_STYLE) AND ws_Child=0)AND(LoWord(lp)<>hWind)THEN
  WITH WndEnable^,Items[NumWnd] DO
  BEGIN
    Enabled:=NOT(EnableWindow(hWind,False));
    Wnd:=hWind;
    Inc(NumWnd);
  END;
END;

FUNCTION EnableWnd(hWind:hWnd;lp:Longint):Bool;EXPORT;
VAR i:Word;
BEGIN
  EnableWnd:=True;
  IF (GetWindowLong(hWind,GWL_STYLE) AND ws_Child=0)AND(LoWord(lp)<>hWind)THEN
  WITH WndEnable^ DO FOR i:=0 TO NumWnd DO WITH Items[i] DO
  BEGIN
    IF Wnd=hWind THEN BEGIN EnableWindow(hWind,Enabled); Exit; END;
  END;
  EnableWindow(hWind,True);
END;

PROCEDURE TAskWin.MakeDlg(VAR EnabledWnd:pEnableWnd);
VAR Proc:tFarProc;
BEGIN
  New(EnabledWnd);
  EnabledWnd^.Window:=hWindow;
  EnabledWnd^.hTask:=GetWindowTask(hWindow);
  EnabledWnd^.NumWnd:=0;
  Proc:=MakeProcInstance(@DisableWnd,hInstance);
  WndEnable:=EnabledWnd;
  EnumTaskWindows(EnabledWnd^.hTask,Proc,EnabledWnd^.Window);
  WndEnable:=NIL;
  FreeProcInstance(Proc);
END;

PROCEDURE TAskWin.EndDlg(VAR EnabledWnd:pEnableWnd);
VAR Proc:tFarProc;
BEGIN
  Proc:=MakeProcInstance(@EnableWnd,hInstance);
  WndEnable:=EnabledWnd;
  EnumTaskWindows(EnabledWnd^.hTask,Proc,EnabledWnd^.Window);
  WndEnable:=NIL;
  FreeProcInstance(Proc);
  Dispose(EnabledWnd);
  IF IsWindow(OldActiveWindow)THEN SetActiveWindow(OldActiveWindow);
END;

PROCEDURE TAskWin.MakeModal;
VAR Message:TMsg;
    EnabledWnd:pEnableWnd;
BEGIN
  IsModal:=True;
  MakeDlg(EnabledWnd);
  WHILE IsModal DO
  BEGIN
    IF PeekMessage(Message,0,0,0,pm_Remove) THEN
    BEGIN
      TranslateMessage(Message);
      DispatchMessage(Message);
    END;
  END;
  EndDlg(EnabledWnd);
END;

PROCEDURE TAskWin.SetupWindow;
BEGIN
  INHERITED SetupWindow;
  comments[1]^.SetText('&Path:');       comments[1]^.setassocwindow(EPath^.HWindow,0);
  comments[2]^.SetText('&Filename:');   comments[2]^.setassocwindow(EName^.HWindow,0);
  comments[3]^.SetText('.WAV');
  pHDSel^.Enable; bok^.Enable; bquit^.Enable; bhelp^.Enable;
  EName^.SetStringText(WavName^);
  OldFocus:=Bok^.HWindow;
  PCancel^:=True;
END;

PROCEDURE tAskWin.WmEraseBkGnd;
BEGIN
  PaintCaption(msg.wparam);
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  framefilled3d(msg.wParam,0,CapDY,W_Sel,H_Sel,2,cForeGnd,up);   {Hauptrahmen}
  WITH Epath^.Attr DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  WITH EName^.Attr DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  WITH pHDSel^.Attr     DO Frame3D(msg.wParam,x-1,y-1,w+2,h+2,1,down);
  WITH Bok^.Attr     DO Frame3D(msg.wParam,x-1,y-1,3*w,h+2,1,down);
  INHERITED WmEraseBkGnd(msg);
END;

PROCEDURE TAskWin.WMCommand;
VAR P:PChar;
BEGIN
  Msg.result:=0;
  CASE Msg.wparam OF
    id_HDSel:Application^.MakeWindow(New(p_Dirbrowse,Init(@self,EPath)));
    id_Quit :PostMessage(HWindow,WM_Close,0,0);
    id_OK   :BEGIN
               GetMem(P,200);
               EPath^.GetText(P,200);
               IF NOT(IsValidDosName(StrPas(P)))THEN
               BEGIN
                 EPath^.Focus;
                 MessageBeep(0);
                 FreeMem(P,200);
                 Exit;
               END;
               PCancel^:=False;
               Ename^.GetText(P,9);
               WavName^:=StrPas(P);
               EPath^.GetText(ResBuf,200);
               FreeMem(P,200);
               PostMessage(HWindow,WM_Close,0,0);
             END;
    id_Help: helpme(@self,id_recnamedlg);
    ELSE INHERITED WMCommand(Msg);
  END;
END;

PROCEDURE tAskWin.wmkeydown;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    vk_return,vk_execute:PostMessage(HWindow,WM_Command,id_ok,0);
    vk_f1,vk_help       :helpme(@self,id_recnamedlg);
    vk_escape           :PostMessage(HWindow,wm_close,0,0);
    ELSE DefWndProc(msg);
  END;
END;

PROCEDURE TAskWin.wmsyschar;
BEGIN
  CASE UpCase (char(Lo(msg.wparam))) OF
    'P': EPath^.Focus;
    'F': EName^.Focus;
    ELSE BEGIN INHERITED wmsyschar(msg); Exit; END;
  END;
  msg.result:=-1;
END;

END.