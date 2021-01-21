PROGRAM MCITest;
USES WinProcs,WinCrt,MMSystem;

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
    Code:Word;
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
  AND Dispatch('play MOD4WIN_MCI'#0)
  AND WaitEndPlay
  AND Dispatch('close MOD4WIN_MCI'#0)
  THEN Writeln('Done!');
END.