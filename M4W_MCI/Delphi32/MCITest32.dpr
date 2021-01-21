PROGRAM MCITest32;
USES Windows,
     MMSYSTEM;

FUNCTION WhereY:SmallInt;
VAR SCREEN_INFO:tConsoleScreenBufferInfo ;
BEGIN
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE),SCREEN_INFO);
  WhereY:=SCREEN_INFO.dwCursorPosition.Y+1;
END;

FUNCTION WhereX:SmallInt;
VAR SCREEN_INFO:tConsoleScreenBufferInfo ;
BEGIN
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE),SCREEN_INFO);
  WhereX:=SCREEN_INFO.dwCursorPosition.X+1;
END;

PROCEDURE GotoXY(X,Y:SmallInt);
VAR Coord:tCoord;
BEGIN
  Coord.X:=X-1;
  Coord.Y:=Y-1;
  SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE),Coord);
  SetConsoleCursorPosition(GetStdHandle(STD_INPUT_HANDLE),Coord);
  SetConsoleCursorPosition(GetStdHandle(STD_ERROR_HANDLE),Coord);
END;

PROCEDURE ClrEOL;
VAR SCREEN_INFO:tConsoleScreenBufferInfo ;
BEGIN
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE),SCREEN_INFO);
  Write(' ':SCREEN_INFO.dwSize.X-SCREEN_INFO.dwCursorPosition.X);
  GotoXY(SCREEN_INFO.dwCursorPosition.X+1,SCREEN_INFO.dwCursorPosition.Y+1);
END;

FUNCTION KeyPressed:Boolean;
VAR InBuffer:ARRAY[1..128]OF tInputRecord;
    NumRead:DWord;
    i:DWord;
BEGIN
  PeekConsoleInput(GetStdHandle(STD_INPUT_HANDLE),InBuffer[1],128,NumRead);
  IF NumRead>0 THEN FOR i:=1 TO NumRead DO
  IF (InBuffer[i].EventType=KEY_Event)
  AND(NOT InBuffer[i].KEYEVENT.bKeyDown)
  AND(InBuffer[i].KEYEVENT.AsciiChar<>#0) THEN
  BEGIN
    KeyPressed:=True;
    Exit;
  END;
  KeyPressed:=False;
END;

FUNCTION ReadKey:Char;
VAR InBuffer:ARRAY[1..128]OF tInputRecord;
    NumRead:DWord;
    NumWritten:DWord;
    i:DWord;
BEGIN
  REPEAT
    ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE),InBuffer[1],128,NumRead);
    IF NumRead>0 THEN FOR i:=1 TO NumRead DO
    IF (InBuffer[i].EventType=KEY_Event)
    AND(NOT InBuffer[i].KEYEVENT.bKeyDown)
    AND(InBuffer[i].KEYEVENT.AsciiChar<>#0) THEN
    BEGIN
      ReadKey:=InBuffer[i].KeyEvent.AsciiChar;
      IF i>1       THEN WriteConsoleInput(GetStdHandle(STD_INPUT_HANDLE),InBuffer[1],i-1,NumWritten);
      IF i<NumRead THEN WriteConsoleInput(GetStdHandle(STD_INPUT_HANDLE),InBuffer[i+1],NumRead-i,NumWritten);
      Exit;
    END;
    Sleep(10);
  UNTIL False;
END;


FUNCTION Dispatch(StrCommand:pChar):Boolean;
VAR PC:ARRAY[0..80]OF Char;
BEGIN
  Dispatch:=False;
  Write(StrCommand,' ... ');
  IF MCISendString(StrCommand,@PC,SizeOf(PC),0)<>0 THEN Writeln(' Error: ',PC) ELSE
  BEGIN
    Writeln(' ok.');
    Dispatch:=True;
  END;
END;

FUNCTION WaitEndPlay:Boolean;
VAR PC:ARRAY[0..80]OF Char;
    T:Longint;
    Code:Integer;
    MM,SS,HH:STRING[5];
BEGIN
  WaitEndPlay:=True;
  WHILE (MCISendString('status MOD4WIN_MCI mode',PC,SizeOf(PC),0)=0)
  AND(lStrCmpi(PC,'playing')=0)
  AND(NOT KeyPressed) DO
  BEGIN
    GotoXY(1,WhereY);
    MCISendString('status MOD4WIN_MCI position',PC,SizeOf(PC),0);
    Val(PC,T,Code);
    Str(100+(T DIV    10) MOD  100,HH); Delete(HH,1,1);
    Str(100+(T DIV  1000) MOD   60,SS); Delete(SS,1,1);
    Str(100+(T DIV 60000) MOD  100,MM); Delete(MM,1,1);
    Write('Playing: ',MM,':',SS,'.',HH,' ...press a Key to stop.');
    ClrEol;
  END;
  GotoXY(1,WhereY);
  ClrEol;
END;

BEGIN
  IF  Dispatch('open no-sn.xm type m4w_mci alias MOD4WIN_MCI'#0)
  AND Dispatch('set MOD4WIN_MCI time format ms'#0)
  AND Dispatch('play MOD4WIN_MCI'#0)
  AND WaitEndPlay
  AND Dispatch('close MOD4WIN_MCI'#0)
  THEN Writeln('Done!');
  WHILE KeyPressed DO ReadKey;
  Writeln('Press a key to exit');
  REPEAT Until KeyPressed;
END.
