UNIT Register;
INTERFACE
USES winprocs,wintypes,owindows,strings,winunit,objects,modc,ShellApi;

TYPE PregisterWindow=^Tregisterwindow;
     TregisterWindow=object(Tmywindow)
       npref,cpref:STRING[30];
       user,company:PString;
       byes,bno:pmsgbutton;
       pedituser,peditcompany:pmyudedit;
       lines:integer;
       mainw,mainh:integer;
       CONSTRUCTOR Init(aparent:pwindowsobject; atitle:STRING; puser,pcompany:PString);
       DESTRUCTOR  Done; virtual;
       PROCEDURE   SetupWindow; virtual;
       PROCEDURE   wmEraseBkGnd(VAR msg:tmessage); virtual wm_first+wm_erasebkgnd;
       PROCEDURE   yes(VAR msg:tmessage); virtual    id_first+id_yes;
       PROCEDURE   Cancel(VAR msg:tmessage); virtual     id_first+id_cancel;
       PROCEDURE   wmkeydown(VAR msg:tmessage);  virtual wm_first+wm_keydown;
       PROCEDURE   wmctlcolor(VAR msg:tmessage); virtual wm_first + wm_ctlcolor;
     END;

IMPLEMENTATION
{*********REGWND************}
CONSTRUCTOR TregisterWindow.Init;
VAR s:STRING[100];
    stil:longint;
    i,j:integer;
BEGIN
  user:=puser; company:=pcompany; user^:=''; company^:='';
  s:=atitle+#0;
  INHERITED Init(aparent,@s[1],ws_sysmenu,notopmost);
  npref:=getresstring(205); i:=Length(npref); npref:=npref+#0;
  cpref:=getresstring(206); IF i<Length(cpref) THEN i:=Length(cpref); cpref:=cpref+#0;
  WITH Attr DO
  BEGIN
    style:=ws_popup OR ws_visible OR ws_border;
    w:=(30+i)*fw+40;
    h:=4*fh +80;
    x:=GetSystemMetrics(sm_cxscreen)DIV 2 -w DIV 2;
    y:=GetSystemMetrics(sm_cyscreen)DIV 2 -h DIV 2;
  END;
  byes:=New(pmsgbutton,Init(@self,id_yes,40,Attr.h-capdy-18,'MB_OK',false));
  bno:=New(pmsgbutton,Init(@self,id_cancel,Attr.w-116,Attr.h-capdy-18,'MB_cancel',false));
  pedituser   :=New(pmyudedit,Init(@self,100,'',20+(fw*i),capdy+18,30*fw,fh, 30,true));
  peditcompany:=New(pmyudedit,Init(@self,100,'',20+(fw*i),capdy+18+(2*fh),30*fw,fh, 30,true));
END;

PROCEDURE Tregisterwindow.SetupWindow;
VAR cb : Longint;
    Key: hKey;
    PC : ARRAY[0..30]OF Char;
BEGIN
  INHERITED SetupWindow;
  bno^.enable;
  byes^.enable;
  oldfocus:=pedituser^.HWindow;
  Parent^.disable;
  if RegOpenKey (hKey_Classes_Root, 'ModulePlayer\data\registration', Key) = error_success then
  begin
    cb := SizeOf (PC);
    if RegQueryValue (Key, 'user',    PC, cb)=error_success then pedituser^.SetText(PC);
    cb := SizeOf (PC);
    if RegQueryValue (Key, 'company', PC, cb)=error_success then peditcompany^.SetText(PC);
    RegCloseKey (Key);
  end;
END;

PROCEDURE Tregisterwindow.wmEraseBkGnd(VAR msg:tmessage);
VAR oldfont:thandle;
BEGIN
  paintcaption(msg.wParam);
  framefilled3d(msg.wParam,0,capdy,Attr.w-2,Attr.h-capdy-2,2,RGB(192,192,192),up);
  frame3d(msg.wParam,10,capdy+10,Attr.w-20,peditcompany^.Attr.h+pedituser^.Attr.h+30,1,down);
  oldfont:=SelectObject(msg.wParam,ansihandle);
  SetTextAlign(msg.wParam,ta_left OR ta_top);
  SetTextColor(msg.wParam,rgb(0,0,0));
  WITH pedituser^.Attr    DO
  BEGIN
    ExtTextOut(msg.wParam,14,y,0,NIL,@npref[1],Length(npref)-1,NIL);
    frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  END;
  WITH peditcompany^.Attr DO
  BEGIN
    ExtTextOut(msg.wParam,14,y,0,NIL,@cpref[1],Length(cpref)-1,NIL);
    frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  END;
  SelectObject(msg.wParam,oldfont);
  INHERITED wmEraseBkGnd(msg);
END;

PROCEDURE tregisterwindow.wmctlcolor(VAR msg:tmessage);
BEGIN
  IF (msg.lparamhi=CTLCOLOR_EDIT)THEN msg.lparamhi:=CTLCOLOR_STATIC;
  INHERITED wmctlcolor(msg);
END;

PROCEDURE TregisterWindow.yes(VAR msg:tmessage);
VAR u,c:STRING[30];
    nc:ARRAY  [0..30] OF char;
    Key: HKey;
BEGIN
 pedituser^.GetText(@nc,30);
 u:=strpas(@nc); WHILE (u[1]=' ')AND(Length(u)>0) DO Delete(u,1,1);
 peditcompany^.GetText(@nc,30);
 c:=strpas(@nc);
 IF (Length(u)=0) THEN
 BEGIN
   SetFocus(pedituser^.HWindow);
   MessageBeep(0);
 END ELSE
 IF ourmessagebox(@self,'Installation',getresstring(201)+#10+' '+#10+u+#10+c+#10+' '+#10+getresstring(202)+#10+
    getresstring(203),mb_iconasterisk OR mb_yesno)=idyes THEN
 BEGIN
   user^:=u;
   company^:=c;
   u[Length(u)+1]:=#0;
   c[Length(c)+1]:=#0;
   IF RegCreateKey (hKey_Classes_Root, 'ModulePlayer\data\registration', Key) = error_success THEN
   BEGIN
     RegSetValue (Key, 'user'   , reg_sz, @u[1], Length(U));
     RegSetValue (Key, 'company', reg_sz, @c[1], Length(C));
   END;
   PostMessage(HWindow,wm_close,0,0);
 END;
END;
PROCEDURE TregisterWindow.Cancel; BEGIN PostMessage(HWindow,wm_close,0,0); END;
DESTRUCTOR TregisterWindow.Done;
BEGIN
  Parent^.enable;
  IF Length(user^)>0 THEN PostMessage(Parent^.HWindow,wm_command,1,0);
  INHERITED Done;
END;

PROCEDURE tregisterwindow.wmkeydown(VAR msg:tmessage);
BEGIN
  CASE msg.wparam OF
    vk_return,vk_execute:yes(msg);
    vk_space            :SendMessage(oldfocus,wm_char,vk_return,0);
    vk_escape           :Cancel(msg);
    ELSE DefWndProc(msg);
   END;
END;

{********Ende Registrierbox - Object************************************************}

END.