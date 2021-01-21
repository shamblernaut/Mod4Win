PROGRAM Interface16;
USES PlayProc,WinTypes,WinProcs,WinAPI,Win31,W32BASE,Strings;

CONST Class='Player32InterfaceClass';
      Caption='Player32Interface';
      wm_PutMem=WM_User+200;
      wm_GetMem=WM_User+201;
      wm_PutInf=WM_User+202;
      wm_GetInf=WM_User+203;
      wm_Seek  =WM_User+204;

TYPE tLongARRAY=ARRAY[0..16000]OF Longint;
     pLongARRAY=^tLongARRAY;

VAR  SharedMem:RECORD
       CASE Word OF
         1:(Bytes:ARRAY[0..2047]OF Byte;);
         2:(Filename:ARRAY[0..260] OF CHAR;);
         3:(SharedSettings:tSettings;);
         4:(SharedEnChn:RECORD
              Chn:Byte;
              How:Boolean;
            END;);
         5:(SharedCanPlay:RECORD
              Settings: TSettings;
              OvrRateChng: boolean
            END;);
         6:(SharedBool:Boolean;);
         7:(SharedLong:Longint;);
         8:(SharedByte:Byte;);
         9:(Longs:ARRAY[0..511]OF Longint;);
         1001:(SharedDS_Play:RECORD
                 FileName:ARRAY [0..260]OF Char;
                 Settings:tDSSettings;
                 Error:Longint;
               END;);
     END;

CONST DSInfo:pDS_Info=NIL;
VAR   Settings:tSettings;

PROCEDURE GetInfo;
VAR F:File;
BEGIN
  InOutRes:=0;
  Assign(F,'C:\MODINFO.TMP');
  ReWrite(F,1);
  IF IOResult=0 THEN
  BEGIN
    BLOCKWrite(F,MODInfo^,SizeOf(tModulInfo));
    Close(F);
  END;
END;

FUNCTION WndProc(hWnd,message,wParam:word;lParam:longint):Longint;FAR;
BEGIN
  WndProc:=0;
  WITH SharedMem DO
  CASE message OF
    wm_PutMem:SharedMem.Longs[wParam]:=lParam;
    wm_GetMem:WndProc:=SharedMem.Longs[wParam];
    wm_PutInf:pLongArray(ModInfo)^[wParam]:=lParam;
    wm_GetInf:GetInfo;
    WM_Seek:BEGIN
              ModInfo^.CurrTime:=lParam;
              MOD_EndPause(False);
            END;
    WM_Command  :
    CASE wParam OF
      1 :mod_Stop;
      2 :BEGIN
           WndProc:=mod_Play(hWnd,FileName,NIL{DiskName},False{OEM},@Settings,False,False);
         END;
      3 :mod_Pause(SharedBool);
      4 :mod_EndPause(SharedBool);
      5 :WndProc:=mod_GetTimePattern(SharedLong);
      6 :BEGIN
           Settings:=SharedSettings;
           Settings.Name:=StrPas(PChar(@SharedSettings.Name));
           MOD_SettingsChanged(Settings,wParam);
           SharedSettings:=Settings;
           StrPCopy(PChar(@SharedSettings.Name),Settings.Name);
           WndProc:=wParam;
         END;
      7 :mod_Repeat(SharedBool);
      8 :;
      9 :WITH SharedEnChn DO mod_EnableChn(Chn,How);
      10:WndProc:=Byte(mod_IsEnabled(SharedEnChn.Chn));
      11:BEGIN
           SharedCanPlay.Settings.Name:=StrPas(PChar(@SharedCanPlay.Settings.Name));
           WndProc:=Byte(mod_CanPlay(SharedCanPlay.Settings,SharedCanPlay.OvrRateChng));
           StrPCopy(PChar(@SharedCanPlay.Settings.Name),SharedCanPlay.Settings.Name);
         END;
      14:WndProc:=MOD_GetVersion;
      15:mod_EnableAllChn(SharedBool);
      16:WndProc:=mod_Play($FFFF,FileName,NIL{DiskName},False{OEM},@Settings,False,False);
      100:WndProc:=ModInfo^.CurrTime;
      101:WndProc:=ModInfo^.TotalTime;
      102:WndProc:=Byte(NOT ModInfo^.Playing);
      103:WndProc:=ModInfo^.CPUUsage;
      1000:DS_STOP;
      1001:IF lParam<>0 THEN
           BEGIN
             DSInfo:=DS_Play(@SharedDS_Play.FileName,SharedDS_Play.Settings,SharedDS_Play.Error);
             WndProc:=GetVDMPointer32W(DSInfo,1);
           END ELSE WndProc:=Longint(DS_Play(@SharedDS_Play.FileName,SharedDS_Play.Settings,SharedDS_Play.Error));
      1002:DS_Seek(SharedLong);
      1003:WndProc:=DS_KeepAlive;
      1004:BEGIN
             WndProc:=DS_GetBuffer;
             IF lParam<>0 THEN DSInfo^.BuffPtr:=Pointer(GetVDMPointer32W(DSInfo^.BuffPtr,1));
           END;
      1005:WndProc:=DS_DoneBuffer(SharedLong);
    END;
    WM_EndSession:Halt(0);
    WM_DESTROY  :PostQuitMessage(0);
    WM_SetFocus :SetFocus(wparam);
    ELSE WndProc:=DefWindowProc( hWnd, message, wParam, lParam );
  END;
END;

FUNCTION Init(hInst:THandle):Boolean;
VAR WCLASS:TWndClass;
BEGIN
  WITH WCLASS DO
  BEGIN
    hCursor      :=LoadCursor(0,IDC_ARROW);
    hIcon        :=LoadIcon(hinst, MakeIntResource(100));
    lpszMenuName :=NIL;
    lpszClassName:=Class;
    hbrBackground:=0;
    hInstance    :=hInst;
    style        :=cs_bytealignclient OR cs_bytealignwindow;
    lpfnWndProc  :=@WndProc;
    cbClsExtra   :=0 ;
    cbWndExtra   :=0 ;
  END;
  Init:=RegisterClass(WClass);
END;

VAR hWindow:hWnd;
    msg:tMSG;
BEGIN
  Settings:=DefDACSetting;
  IF (hPrevInst<>0)OR NOT(Init(hInstance)) THEN Halt(0);
  hWindow:=CreateWindow(Class,Caption,ws_popup,0,0,0,0,0,0,hInstance,NIL);
  EnableWindow(hwindow,false);
  ShowWindow(hWindow,SW_HIDE);
  WHILE GetMessage(msg,0,0,0) DO
  BEGIN
    TranslateMessage(msg);
    DispatchMessage(msg);
  END;
  DestroyWindow(hWindow);
  UnRegisterClass(Class,hInstance);
END.