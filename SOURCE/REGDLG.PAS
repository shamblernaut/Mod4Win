UNIT RegDlg;
{ $C Fixed Preload Permanent}

INTERFACE

USES WinTypes, OWindows, winunit, Strings;

TYPE PRegWin=^TRegWin;
     TRegWin=OBJECT(TMyWindow)
         UserName, CompName: ARRAY[0..30] OF char;
         RegInfo, RegCode: ARRAY[0..50] OF char;
         EUser, EComp, EInfo, ECode: PMyEdit;
         BOk, BQuit,BHelp: PBmpButton;
         Comments: ARRAY[1..4] OF PMyStatic;
         SysChar: ARRAY[1..4] OF char;
         AskRest: Boolean;
       CONSTRUCTOR Init(AParent:PWindowsObject; Askrestart:Boolean);
       DESTRUCTOR Done;                              VIRTUAL;
       PROCEDURE SetupWindow;                        VIRTUAL;
       PROCEDURE WMEraseBkGnd(VAR msg:tmessage);     VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE WMCommand(VAR msg:tmessage);        VIRTUAL wm_first+wm_command;
       PROCEDURE wmkeydown(VAR msg:tmessage);        virtual WM_FIRST+WM_KEYDOWN;
       PROCEDURE wmsyschar(VAR msg:tmessage);        virtual wm_first+wm_syschar;
     END;

IMPLEMENTATION

USES WinProcs, ShellAPI, modc;

CONST WasShown: boolean = false;

CONST W_Sel=347; { (30+10) * Fw + 2*11 + 5 }
      H_Sel=128; { 4*Fh + 3*10 + 2*11 + 22 + 6 }
      id_user=100;
      id_comp=101;
      id_info=102;
      id_code=103;
      id_ok  =104;
      id_Quit=105;
      id_Help=106;
CONSTRUCTOR TRegWin.Init;
VAR s: STRING[50]; i: byte;
BEGIN
  s:=modc.shortver+GetResString (id_regdlg_caption)+#0;
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
  EUser := New (PMyUDEdit, Init (@self, id_user, NIL,10*fw + 16, CapDY +11,             30 * fw, fh, SizeOf (UserName), 
  true));
  EComp := New (PMyUDEdit, Init (@self, id_comp, NIL,10*fw + 16, CapDY +11 + 1*(fh+10), 30 * fw, fh, SizeOf (CompName), 
  true));
  EInfo := New (PMyUDEdit, Init (@self, id_info, NIL,10*fw + 16, CapDY +11 + 2*(fh+10), 30 * fw, fh, SizeOf (RegInfo),  
  true));
  ECode := New (PMyUDEdit, Init (@self, id_code, NIL,10*fw + 16, CapDY +11 + 3*(fh+10), 30 * fw, fh, SizeOf (RegCode),  
  true));
  WITH ECode^.Attr DO Style := Style OR es_Password;
  comments[1]:=New(pmystatic,Init(@self,0,NIL,11,EUser^.Attr.Y,10*Fw,Fh,10));
  comments[2]:=New(pmystatic,Init(@self,0,NIL,11,EComp^.Attr.Y,10*Fw,Fh,10));
  comments[3]:=New(pmystatic,Init(@self,0,NIL,11,EInfo^.Attr.Y,10*Fw,Fh,10));
  comments[4]:=New(pmystatic,Init(@self,0,NIL,11,ECode^.Attr.Y,10*Fw,Fh,10));
  WITH ECode^.Attr DO BEGIN
    bOk    :=New(PbmpButton, Init(@self,id_ok,  X+W-123,Y+H+11,'bsok',    false));
    bQuit  :=New(PbmpButton, Init(@self,id_quit,X+W-82 ,Y+H+11,'bscancel',false));
    bhelp  :=New(PbmpButton, Init(@self,id_help,X+W-41 ,Y+H+11,'bshelp',  false));
  END;
  FOR i := 1 TO 4 DO BEGIN s := getresstring(id_regdlg_caption + i); syschar[i] := UpCase (s[Pos ('&', s) + 1]); END;
  Askrest:=Askrestart;
END;

DESTRUCTOR TRegWin.Done;
BEGIN
  Parent^.enable;
  INHERITED Done;
END;

PROCEDURE TRegWin.SetupWindow;
VAR cb: longint; Key: HKey;
BEGIN
  INHERITED SetupWindow;
  comments[1]^.setstringtext(GetResString (id_regdlg_user)); comments[1]^.setassocwindow(EUser^.HWindow,0);
  comments[2]^.setstringtext(GetResString (id_regdlg_comp)); comments[2]^.setassocwindow(EComp^.HWindow,0);
  comments[3]^.setstringtext(GetResString (id_regdlg_info)); comments[3]^.setassocwindow(EInfo^.HWindow,0);
  comments[4]^.setstringtext(GetResString (id_regdlg_code)); comments[4]^.setassocwindow(ECode^.HWindow,0);
  bok^.Enable; bquit^.Enable; bhelp^.Enable;
  IF RegOpenKey (hKey_Classes_Root, 'ModulePlayer\data\registration', Key) = error_success THEN BEGIN
    cb := SizeOf (UserName); IF RegQueryValue (Key, 'user',    UserName, cb) = error_success THEN EUser^.SetText (UserName);
    cb := SizeOf (CompName); IF RegQueryValue (Key, 'company', CompName, cb) = error_success THEN EComp^.SetText (CompName);
    cb := SizeOf (RegInfo);  IF RegQueryValue (Key, 'info',    RegInfo, cb)  = error_success THEN EInfo^.SetText (RegInfo);
    cb := SizeOf (RegCode);  IF RegQueryValue (Key, 'code',    RegCode, cb)  = error_success THEN ECode^.SetText (RegCode);
    RegCloseKey (Key);
  END;
  { disable password style for fresh entries }
  IF NOT Registered THEN BEGIN
    IF NOT WasShown THEN ECode^.SetText ('');
    SendMessage (ECode^.HWindow, em_setpasswordchar, 0, 0);
  END;
  Parent^.disable;
  OldFocus:=Bok^.HWindow;
END;

PROCEDURE TRegWin.WmEraseBkGnd;
VAR i: byte; memdc:hdc;
BEGIN
  PaintCaption(msg.wparam);
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  framefilled3d(msg.wParam,0,CapDY,W_Sel,H_Sel,2,cForeGnd,up);   {Hauptrahmen}
  WITH EUser^.Attr DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  WITH EComp^.Attr DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  WITH EInfo^.Attr DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  WITH ECode^.Attr DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  FOR i := 1 TO 4 DO WITH comments[i]^.Attr DO Frame3D (msg.wparam,x-2,y-2,w+4,h+4,1,down);
  WITH Bok^.Attr   DO Frame3D(msg.wParam,x-1,y-1,3*w,h+2,1,down);
  WITH Attr DO        Frame3D(msg.wParam,5,CapDY+5,W_Sel-10,4*(fh+10)+2,1,up);
  memdc:=CreateCompatibleDC(msg.wParam);
  SelectObject(memdc,m_label);
  BitBlt(msg.wParam,(bok^.Attr.x-96) DIV 2,bok^.Attr.y,96,20,memdc,0,0,srccopy);
  DeleteDC(memdc);
  INHERITED WmEraseBkGnd(msg);
END;

PROCEDURE TRegWin.WMCommand;
VAR Key: HKey;
BEGIN
  Msg.result:=0;
  CASE Msg.wparam OF
    id_Quit :PostMessage(HWindow,WM_Close,0,0);
    id_OK   :BEGIN
      IF RegCreateKey (hKey_Classes_Root, 'ModulePlayer\data\registration', Key) = error_success THEN
      BEGIN
        EUser^.GetText (UserName, SizeOf (UserName)); RegSetValue (Key, 'user'   , reg_sz, UserName, SizeOf (UserName));
        EComp^.GetText (CompName, SizeOf (CompName)); RegSetValue (Key, 'company', reg_sz, CompName, SizeOf (CompName));
        EInfo^.GetText (RegInfo,  SizeOf (RegInfo));  RegSetValue (Key, 'info'   , reg_sz, RegInfo,  SizeOf (RegInfo));
        ECode^.GetText (RegCode,  SizeOf (RegCode));  RegSetValue (Key, 'code'   , reg_sz, RegCode,  SizeOf (RegCode));
        RegCloseKey (Key);
      END ELSE BEGIN
        { can't create key }
      END;
      IF (Askrest)AND(OurMessageBox(@Self,ShortVer,GetResstring(id_restart_code),MB_IconQuestion OR MB_YESNO)=ID_YES) THEN
      BEGIN
        Restart:=true;
        PostMessage(Application^.MainWindow^.HWindow,wm_close,0,0);
        Exit;
      END ELSE PostMessage(HWindow,WM_Close,0,0);
      WasShown := true;
    END;
    id_Help: helpme(@self,id_regdlg);
    ELSE INHERITED WMCommand(Msg);
  END;
END;

PROCEDURE TRegWin.wmkeydown;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    vk_return,vk_execute:PostMessage(HWindow,WM_Command,id_ok,0);
    vk_f1,vk_help       :helpme(@self,id_regdlg);
    vk_escape           :PostMessage(HWindow,wm_close,0,0);
    ELSE DefWndProc(msg);
  END;
END;

PROCEDURE TRegWin.wmsyschar;
VAR ch: char;
BEGIN
  ch := UpCase (char(Lo(msg.wparam)));
  IF ch = syschar [1] THEN EUser^.Focus ELSE
  IF ch = syschar [2] THEN EComp^.Focus ELSE
  IF ch = syschar [3] THEN EInfo^.Focus ELSE
  IF ch = syschar [4] THEN ECode^.Focus ELSE
    BEGIN INHERITED wmsyschar(msg); Exit; END;
  msg.result:=-1;
END;

END.