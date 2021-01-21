PROGRAM Register;

USES WinTypes, OWindows, winunit, Strings, WinProcs, Win31, MakeCode, ODialogs, CommDlg, WinDOS, File32;

TYPE PRegWin=^TRegWin;
     TRegWin=OBJECT(TMyWindow)
         UserName, CompName: ARRAY[0..30] OF char;
         RegInfo, RegCode: ARRAY[0..50] OF char;
         EVersion,EUser, EComp, EInfo:PMyEdit;
         ECode:PMyEdit;
         Comments: ARRAY[1..5] OF PMyStatic;
       CONSTRUCTOR Init;
       PROCEDURE SetupWindow;                        VIRTUAL;
       PROCEDURE WMEraseBkGnd(VAR msg:tmessage);     VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE wmkeydown(VAR msg:tmessage);        virtual WM_FIRST+WM_KEYDOWN;
       PROCEDURE Save(VAR msg:tmessage);             virtual ID_FIRST+101;
       PROCEDURE eMail(VAR msg:tmessage);            virtual ID_FIRST+102;
       PROCEDURE Text(VAR msg:tmessage);             virtual ID_FIRST+103;
       PROCEDURE GetClipData(VAR msg:tmessage);      virtual ID_FIRST+104;
       FUNCTION  IDLEACTION:BOOLEAN; VIRTUAL;
     END;


CONST W_Sel=347; { (30+10) * Fw + 2*11 + 5 }
      H_Sel=5*12 + 4*10 + 2*11 +12+6+6;

CONSTRUCTOR TRegWin.Init;
VAR B:pButton;
BEGIN
  INHERITED Init(NIL,'Registration Key Generator',ws_minimizebox OR ws_sysmenu,notopmost);
  WITH Attr DO
  BEGIN
    style:=ws_popup OR ws_visible OR ws_border;
    w :=W_Sel+(2*GetSystemMetrics(sm_cxborder));
    h :=H_Sel+(2*GetSystemMetrics(sm_cyborder))+capdy;
    x :=(GetSystemMetrics(sm_cxscreen) - w) DIV 2;
    y :=(GetSystemMetrics (sm_cyscreen) - h) DIV 2;
  END;
  EVersion:=New(PMyUDEdit,Init(@self,-1,'2.31',10*fw+16,CapDY+11,       30*fw,fh,50,true));
  EUser:=New(PMyUDEdit,Init(@self,-1,NIL,10*fw+16,CapDY+11+1*(fh+10),30*fw,fh,50,true));
  EComp:=New(PMyUDEdit,Init(@self,-1,NIL,10*fw+16,CapDY+11+2*(fh+10),30*fw,fh,50,true));
  EInfo:=New(PMyUDEdit,Init(@self,-1,NIL,10*fw+16,CapDY+11+3*(fh+10),30*fw,fh,50,True));
  ECode:=New(PMyUDEdit,Init(@self,-1,NIL,10*fw+16,CapDY+11+4*(fh+10),30*fw,fh,50,True));
  comments[1]:=New(pmystatic,Init(@self,0,NIL,11,EVersion^.Attr.Y,10*Fw,Fh,10));
  comments[2]:=New(pmystatic,Init(@self,0,NIL,11,EUser^.Attr.Y,10*Fw,Fh,10));
  comments[3]:=New(pmystatic,Init(@self,0,NIL,11,EComp^.Attr.Y,10*Fw,Fh,10));
  comments[4]:=New(pmystatic,Init(@self,0,NIL,11,EInfo^.Attr.Y,10*Fw,Fh,10));
  comments[5]:=New(pmystatic,Init(@self,0,NIL,11,ECode^.Attr.Y,10*Fw,Fh,10));
  B:=New(pButton,Init(@self,101,'REG file' ,11+0*83,CapDY+11+5*(fh+10),72,FH+6,False));
  B:=New(pButton,Init(@self,102,'full Copy',11+1*83,CapDY+11+5*(fh+10),72,FH+6,False));
  B:=New(pButton,Init(@self,103,'prt. Copy',11+2*83,CapDY+11+5*(fh+10),72,FH+6,False));
  B:=New(pButton,Init(@self,104,'Paste'    ,11+3*83,CapDY+11+5*(fh+10),72,FH+6,False));
  EnableKbHandler;
END;

PROCEDURE TRegWin.SetupWindow;
BEGIN
  INHERITED SetupWindow;
  comments[1]^.setstringtext('Version  :'); comments[1]^.setassocwindow(EVersion^.HWindow,0);
  comments[2]^.setstringtext('Name     :'); comments[2]^.setassocwindow(EUser^.HWindow,0);
  comments[3]^.setstringtext('Company  :'); comments[3]^.setassocwindow(EComp^.HWindow,0);
  comments[4]^.setstringtext('Payment  :'); comments[4]^.setassocwindow(EInfo^.HWindow,0);
  comments[5]^.setstringtext('Reg. Code:'); comments[5]^.setassocwindow(ECode^.HWindow,0);
  SendMessage(ECode^.hWindow,EM_SetReadOnly,1,0);
  OldFocus:=EVersion^.hWindow;
END;

PROCEDURE TRegWin.WmEraseBkGnd;
VAR i: byte;
BEGIN
  PaintCaption(msg.wparam);
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  framefilled3d(msg.wParam,0,CapDY,W_Sel,H_Sel,2,cForeGnd,up);   {Hauptrahmen}
  WITH EUser^.Attr DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  WITH EComp^.Attr DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  WITH EInfo^.Attr DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  WITH ECode^.Attr DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  WITH EVersion^.Attr DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  FOR i := 1 TO 4 DO WITH comments[i]^.Attr DO Frame3D (msg.wparam,x-2,y-2,w+4,h+4,1,down);
  WITH Attr DO        Frame3D(msg.wParam,5,CapDY+5,W_Sel-10,4*(fh+10)+2,1,up);
  INHERITED WmEraseBkGnd(msg);
END;

FUNCTION TRegWin.IDLEACTION;
VAR Version,Result:ARRAY[0..50]OF Char;
BEGIN
  FillChar(Version,51,0);
  FillChar(UserName,51,0);
  FillChar(CompName,51,0);
  FillChar(RegInfo,51,0);
  FillChar(RegCode,51,0);
  EVersion^.GetText(Version,50);
  EUser^.GetText(UserName,50);
  EComp^.GetText(CompName,50);
  EInfo^.GetText(RegInfo,50);
  ECode^.GetText(RegCode,50);
  GenerateCode(Version,UserName,CompName,RegInfo,Result);
  IF StrComp(Result,RegCode)<>0 THEN ECode^.SetText(Result);
  IF GetFocus<>EVersion^.hWindow THEN SendMessage(EVersion^.hWindow,EM_SetSel,0,MakeLong(0,50));
  IF GetFocus<>EUser^.hWindow THEN SendMessage(EUser^.hWindow,EM_SetSel,0,MakeLong(0,50));
  IF GetFocus<>EComp^.hWindow THEN SendMessage(EComp^.hWindow,EM_SetSel,0,MakeLong(0,50));
  IF GetFocus<>EInfo^.hWindow THEN SendMessage(EInfo^.hWindow,EM_SetSel,0,MakeLong(0,50));
  IF GetFocus<>ECode^.hWindow THEN SendMessage(ECode^.hWindow,EM_SetSel,0,MakeLong(0,50));
  IDLEACTION:=TRUE;
END;

PROCEDURE TRegWin.wmkeydown;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    vk_escape:PostMessage(HWindow,wm_close,0,0);
    ELSE DefWndProc(Msg);
  END;
END;

PROCEDURE TRegWin.Save(VAR msg:tmessage);
VAR Ofn          : TOpenFileName;
    szFile       : ARRAY[0..260] OF Char;
    szCurrentDir : ARRAY[0..260] OF Char;
    szFilter     : pChar;
    szCaption    : pChar;
BEGIN
   FillChar(Ofn, SizeOf(TOpenFileName), #0);
   GetCurDir(szCurrentDir,0);
   GetCurrentDirectory(260,szCurrentDir,0,False);
   szFilter:='Registry files (*.REG)'#0'*.REG'#0'all files (*.*)'#0'*.*'#0#0;
   FillChar(szFile,SizeOf(szFile),0);
   szCaption:='Save current data as Registry file'+#0;
   StrCopy(szFile,'MOD4WIN.REG');
   Ofn.lStructSize    := SizeOf(TOpenFileName);
   Ofn.lpstrInitialDir:= szCurrentDir;
   Ofn.lpstrFilter    := szFilter;
   Ofn.lpstrTitle     := szCaption;
   Ofn.lpstrFile      := szFile;
   Ofn.nMaxFile       := SizeOf(szFile);
   Ofn.nFilterIndex   := 1;
   Ofn.Flags          := ofn_HideReadOnly;
   IF GetSaveFileName(Ofn) THEN MakeRegFile(szFile,UserName,CompName,RegInfo,RegCode);
END;

PROCEDURE TRegWin.eMail(VAR msg:tmessage);
VAR Result:PChar;
BEGIN
  Result:=GlobalAllocPtr(gmem_share,4096);
  OpenClipBoard(GetActiveWindow);
  EmptyClipBoard;
  MakeEMailRegText(Result,UserName,CompName,RegInfo,RegCode);
  SetClipBoardData(CF_TEXT,SelectorOf(Result));
  CloseClipBoard;
END;

PROCEDURE TRegWin.Text(VAR msg:tmessage);
VAR Result:PChar;
BEGIN
  Result:=GlobalAllocPtr(gmem_share,4096);
  OpenClipBoard(GetActiveWindow);
  EmptyClipBoard;
  MakeRegText(Result,UserName,CompName,RegInfo,RegCode);
  SetClipBoardData(CF_TEXT,SelectorOf(Result));
  CloseClipBoard;
END;

PROCEDURE TRegWin.GetClipData(VAR msg:tmessage);
VAR ClipData:ARRAY[0..1024]OF Char;
    hClipBoard:Word;
    Version:ARRAY[0..50]OF Char;
BEGIN
  OpenClipBoard(GetActiveWindow);
  hClipBoard:=GetClipBoardData(CF_TEXT);
  CloseClipBoard;
  IF hClipBoard=0 THEN Exit;
  StrCopy(ClipData,Ptr(hClipBoard,0));
  EVersion^.GetText(Version,50);
  DecodeRegBuffer(ClipData,Version,UserName,CompName,RegInfo,RegCode);
  EVersion^.SetText(Version);
  EUser^.SetText(UserName);
  EComp^.SetText(CompName);
  EInfo^.SetText(RegInfo);
  ECode^.SetText(RegCode);
END;


TYPE pApp=^tApp;
     tApp=object(Tbuzapplication)
     PROCEDURE InitMainWindow; VIRTUAL;
     END;

PROCEDURE tApp.InitMainWindow;
BEGIN
  INHERITED InitMainWindow;
  MainWindow:=New(PRegWin,Init);
END;

VAR thisrun:TApp;
BEGIN
 thisrun.Init('MakeCode');
 thisrun.Run;
 thisrun.Done;
END.
