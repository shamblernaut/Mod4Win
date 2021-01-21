{*****************************************************************************}
{*                                                                           *}
{*  Program to test the IFACE32 Unit to MOD4WIN's playback routines.         *}
{*                                                                           *}
{*****************************************************************************}

PROGRAM PlayTest;
USES WinCrt,IFace32;

VAR Settings:tSettings;
    I:Word;
CONST MODFile:PChar='no-sn.xm';
BEGIN
  Settings:=DefDACSetting;
  IF MOD_GetVersion/100<>2.40
  THEN Writeln('Player32.DLL wrong version! Required: 2.40, You have:',MOD_GetVersion/100:5:2)
  ELSE IF (NOT MOD_CanPlay(Settings,True)) OR (MOD_ChangeSettings(Settings)<>0)
  THEN Writeln('ERROR! Could not Setup Player Engine')
  ELSE IF NOT IsMOD(MODFile)THEN Writeln('ERROR! ',MODFile,' is no valid MOD file')
  ELSE BEGIN
    i:=MOD_Play(MODFile);
    IF I=0 THEN
    BEGIN
      Writeln('--- Playing...hit a key or Wait for (mm:ss.hh)',
              MOD_TotalTime DIV 60000:3,':',
              MOD_TotalTime DIV 1000 MOD 60:2,'.',
              MOD_TotalTime DIV 10 MOD 100:2,
              ' to STOP ---');{}
      REPEAT
        GotoXY(1,2);
        {Keep the player alive!}
        Writeln('Current CPU Load =',MOD_CPUUsage DIV 1000:4,'%');
        Writeln('Current PlayTime =',MOD_CurrTime DIV 60000:3,':',MOD_CurrTime DIV 1000 MOD 60:2);
      UNTIL KeyPressed;
      MOD_Stop;
    END ELSE Writeln('ERROR! MOD_Play Failed:',i);
  END;
  ReadKey;
  DoneWinCrt;
END.