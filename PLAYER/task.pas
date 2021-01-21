UNIT Task;
{$C Fixed Preload Permanent}
{$R TASK.RES}
{$DEFINE WMTIMER}
INTERFACE

USES WinProcs,WinTypes,Win31,WinDOS,MMSystem;

{ Unit zur Implementation einer unabhängigen Task in einer Anwendung
Benutzung: USES MMTask;
Wenn Idle<>NIL dann wird IDLE immer, wenn Zeit ist, aufgerufen}
CONST hMMTask:Word=0;
      TaskHandle:Word=0;

PROCEDURE StopTask;FAR;
PROCEDURE SetTaskProc(Proc:Pointer);FAR;
PROCEDURE StartTask;

IMPLEMENTATION

TYPE pTsk=^tTsk;
     tTsk=RECORD
       Dummy1:ARRAY[1..$210]OF Byte;
       Main:Pointer;
     END;

CONST TaskIsRunning:Boolean=False;
      TaskLoaded:Boolean=False;
      EndTask:Boolean=False;
      TaskFileName:ARRAY[0..255]OF Char='';
      Tsk:Procedure=NIL;
      {$IFDEF WMTIMER}
      Timer:Procedure=NIL;
      MainTimer:Word=0;
      {$ENDIF WMTIMER}
      Idle:PROCEDURE=NIL;
VAR   TaskFile:File;

{$IFDEF WMTIMER}
PROCEDURE Task_Timerproc(hwind:hwnd; msg:word; idTimer:word; Time:Longint); EXPORT;
VAR Message:tMSG;
BEGIN
  IF NOT PeekMessage(Message,0,wm_User,wm_User,PM_NoYield OR PM_NoRemove)
  THEN PostAppMessage(TaskHandle,wm_User,0,0);
END;
{$ENDIF WMTIMER}

PROCEDURE Task_MMTimerproc(wTimerID, wMsg:word; dwUser, dw1, dw2:longint); EXPORT;
BEGIN
  PostAppMessage(dwUser,wm_User,0,0);
END;

PROCEDURE SetTaskProc(Proc:Pointer);
BEGIN
  {$IFDEF WMTIMER}
  IF MainTimer<>0 THEN KillTimer(0,MainTimer);
  MainTimer:=0;
  {$ENDIF WMTIMER}
  @Idle:=Proc;
  IF @Idle<>NIL THEN
  BEGIN
    timeSetEvent(1,1,Task_MMTimerProc,TaskHandle,Time_OneShot);
    {$IFDEF WMTIMER}
    MainTimer:=SetTimer(0,1,1,@Timer);
    {$ENDIF WMTIMER}
  END;
END;

PROCEDURE Action;EXPORT;
VAR MSG:tMSG;
BEGIN
  TaskHandle:=GetCurrentTask;
  TaskIsRunning:=True;
  {$IFDEF WMTIMER}
  @Timer:=MakeProcInstance(@Task_TimerProc,hInstance);
  {$ENDIF WMTIMER}
  WHILE NOT EndTask DO
  BEGIN
    WHILE PeekMessage(MSG,0,0,0,PM_NoYield OR PM_Remove) DO
    BEGIN
      TranslateMessage(MSG);
      DispatchMessage(MSG);
    END;
    WaitMessage;
    IF @Idle<>NIL THEN Idle;
    IF @Idle<>NIL THEN timeSetEvent(30,5,Task_MMTimerProc,TaskHandle,Time_OneShot);
  END;
  SetTaskProc(NIL);
  WHILE PeekMessage(MSG,0,0,0,PM_NoYield OR PM_Remove) DO;
  {$IFDEF WMTIMER}
  IF @Timer<>NIL THEN FreeProcInstance(@Timer);
  {$ENDIF WMTIMER}
  @Tsk:=NIL;
  TaskIsRunning:=False;
END;

PROCEDURE StartTask;
VAR FindHandle:tHandle;
    LoadHandle:tHandle;
    MMTsk:pTsk;
BEGIN
  IF TaskLoaded THEN Exit;
  FindHandle:=FindResource(hInstance,'TASKEXE','TASKEXE');
  LoadHandle:=LoadResource(hInstance,FindHandle);
  MMTsk:=lockresource(LoadHandle);
  @Tsk:=MakeProcInstance(@Action,hInstance);
  MMTsk^.Main:=@Tsk;
  GetTempFileName(GetTempDrive('c'),'tsk',0,TaskFilename);
  {$i-}
  Assign(TaskFile,TaskFilename);
  setfattr(TaskFile,FaArchive);
  Erase(TaskFile);
  FileMode:=2;
  Rewrite(TaskFile,1);
  BlockWrite(TaskFile,MMTsk^,SizeOfResource(hInstance,FindHandle));
  Close(TaskFile);
  setfattr(TaskFile,FaArchive OR FaReadOnly);
  UnLockResource(LoadHandle);
  FreeResource(LoadHandle);
  hMMTask:=WinExec(TaskFileName,SW_Hide);
  IF hMMTask>32 THEN TaskLoaded:=True ELSE
  BEGIN
    IF @Tsk<>NIL THEN FreeProcInstance(@Tsk);   @Tsk:=NIL;
    setfattr(TaskFile,FaArchive);
    Erase(TaskFile);
  END;
END;

PROCEDURE StopTask;
VAR MSG:tMSG;
BEGIN
  IF TaskLoaded THEN
  BEGIN
    EndTask:=True;
    IF TaskIsRunning THEN
    BEGIN
      EndTask:=True;
      PostAppMessage(TaskHandle,wm_User,0,0);
      WHILE TaskIsRunning DO DirectedYield(TaskHandle);
    END;
    IF @Tsk<>NIL    THEN FreeProcInstance(@Tsk);   @Tsk:=NIL;
    setfattr(TaskFile,FaArchive);
    Erase(TaskFile);
    TaskLoaded:=False;
  END;
END;

CONST OldExit:Pointer=NIL;
PROCEDURE ExitTask;FAR;
BEGIN
  ExitProc:=OldExit;
  StopTask;
END;

BEGIN
  OldExit:=ExitProc;
  ExitProc:=@ExitTask;
  StartTask;
END.