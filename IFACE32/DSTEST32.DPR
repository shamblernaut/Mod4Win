PROGRAM DSTest32;
USES Iface32,Windows,MMSYSTEM;

PROCEDURE GotoXY(X,Y:SmallInt);
VAR Coord:tCoord;
BEGIN
  Coord.X:=X-1;
  Coord.Y:=Y-1;
  SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE),Coord);
  SetConsoleCursorPosition(GetStdHandle(STD_INPUT_HANDLE),Coord);
  SetConsoleCursorPosition(GetStdHandle(STD_ERROR_HANDLE),Coord);
END;

FUNCTION KeyPressed:Boolean;
VAR InBuffer:ARRAY[1..128]OF tInputRecord;
    NumRead:DWord;
    i:DWord;
BEGIN
  PeekConsoleInput(GetStdHandle(STD_INPUT_HANDLE),InBuffer[1],128,NumRead);
  IF NumRead>0 THEN FOR i:=1 TO NumRead DO IF InBuffer[i].EventType=KEY_Event THEN
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
    IF NumRead>0 THEN FOR i:=1 TO NumRead DO IF InBuffer[i].EventType=KEY_Event THEN
    BEGIN
      ReadKey:=InBuffer[i].KeyEvent.AsciiChar;
      IF i>1       THEN WriteConsoleInput(GetStdHandle(STD_INPUT_HANDLE),InBuffer[1],i-1,NumWritten);
      IF i<NumRead THEN WriteConsoleInput(GetStdHandle(STD_INPUT_HANDLE),InBuffer[i+1],NumRead-i,NumWritten);
      Exit;
    END;
    Yield;
  UNTIL False;
END;

CONST CurrBuffer:Longint=0;
      LastBuffer:Longint=0;
      nBuffers=3;
VAR Settings:tDSSettings;
    Error:Longint;
    DSI:pDS_Info;
    WaveHandle:Longint;
    WaveFormat:tWaveFormatEx;
    WAVEHDR:ARRAY[0..nBuffers-1]OF tWaveHDR;

BEGIN
  {Setup Settings}
  WITH Settings DO
  BEGIN
    Device         :=0;        {0...DS; 1...GUS; 2...OPL4; 3...Top_Secret}
    SampleRate     :=44100;    {11025...48000 Hz}
    BitsPerSample  :=16;       {8/16}
    nChannels      :=2;        {1/2}
    Clipping       :=1;        {0/1}
    Panning        :=1;        {1=use panning; 0=snap to left middle or right}
    Surround       :=1;        {0=Surround off;1=Surround on}
    OverSampling   :=1;        {0: Don't use IDO; 1: Use IDO}
    Mix_dB         :=+00;      {-20...+20 dB digital Mixer Volume}
    SmallBufferSize:=4096;     {Buffersize to be filled everytime DS_KeepAlive is called}
    nSmallBuffers  :=16;       {Buffersize to be returned everytime DS_GetBuffer is called}
    nBigBuffers    :=nBuffers; {How many big sample buffers to be used [3..99]}
  END;
  {Start the Player32}
  DSI:=DS_Play('no-sn.xm',Settings,Error);
  {If something was wrong...EXIT!}
  IF Error<>0 THEN Exit;
  IF Settings.Device=0 THEN
  BEGIN
    {Prepare the WaveHeaders for Playback and open the WaveOut Driver}
    FillChar(WAVEHDR,SizeOf(WAVEHDR),0);
    FOR Error:=0 TO nBuffers-1 DO WaveHdr[Error].dwFlags:=WHDR_DONE;
    WITH WaveFormat DO
    BEGIN
      wFormatTag:=1;
      nChannels:=Settings.nChannels;
      nSamplesPerSec:=Settings.SampleRate;
      wBitsPerSample:=Settings.BitsPerSample;
      NavgBytesPerSec:=Longint(Settings.SampleRate)*Settings.nChannels*(Settings.BitsPerSample DIV 8);
      nBlockAlign:=Settings.nChannels*(Settings.BitsPerSample DIV 8);
      cbSize:=0;
    END;
    IF WaveOutOpen(@WaveHandle,0,@WaveFormat,0,0,0)<>0 THEN Exit;
  END;
  {Start the playback loop...}
  Writeln('--- Playing...hit a key or Wait for (mm:ss.hh)',
          DSI^.TotalTime DIV 60000:3,':',
          DSI^.TotalTime DIV 1000 MOD 60:2,'.',
          DSI^.TotalTime DIV 10 MOD 100:2,
          ' to STOP ---');{}
  REPEAT
    GotoXY(1,2);
    {Keep the player alive!}
    IF DS_KeepAlive<>0 THEN Writeln('Filling Buffer...') ELSE Writeln('Buffers are FULL!');
    Writeln('Current CPU Load =',DSI^.CPUUsage DIV 1000:4,'%');
    Writeln('Current PlayTime =',DSI^.CurrTime DIV 60000:3,':',DSI^.CurrTime DIV 1000 MOD 60:2);
    {Wave Driver Polling for getting the last played buffer done}
    IF Settings.Device=0 THEN
    IF WaveHdr[LastBuffer].dwFlags AND(WHDR_DONE OR WHDR_PREPARED)=(WHDR_DONE OR WHDR_PREPARED)THEN
    BEGIN
      DS_DoneBuffer(LastBuffer+1);
      {Unprepare old written headers...}
      WaveOutUnprepareHeader(WaveHandle,Addr(WaveHdr[LastBuffer]),SizeOf(TWaveHdr));
      Inc(LastBuffer);
      IF LastBuffer>=nBuffers THEN LastBuffer:=0;
    END;
    IF Settings.Device=0 THEN
    BEGIN
      {Get the next buffer if available}
      IF CurrBuffer=0 THEN CurrBuffer:=DS_GetBuffer;
      {If a new buffer is to be written out, check if there's a free Wave Buffer to put it in}
      IF CurrBuffer<>0 THEN
      WITH WaveHdr[CurrBuffer-1] DO IF dwFlags AND WHDR_DONE<>0 THEN
      BEGIN
        Writeln('Writing Buffer',CurrBuffer:3,' to WaveOut.');
        {Prepare the new Header...}
        dwbufferlength:=DSI^.BuffLen;
        lpdata:=DSI^.BuffPtr;
        WaveOutPrepareHeader(WaveHandle,Addr(WaveHdr[CurrBuffer-1]),SizeOf(TWaveHdr));
        {...and write it out!}
        WaveOutWrite(WaveHandle,Addr(WaveHdr[CurrBuffer-1]),SizeOf(TWaveHdr));
        {free CurrBuffer to be able to get a new ID via GetNextBuffer}
        CurrBuffer:=0;
      END;
    END ELSE CurrBuffer:=0;
    IF KeyPressed THEN CASE UpCase(ReadKey) OF
      'F':{Fast Forward}
          BEGIN
            {WaveOut Stuff}
            WaveOutReset(WaveHandle);
            FOR Error:=0 TO nBuffers-1 DO WaveHdr[Error].dwFlags:=whdr_done;
            CurrBuffer:=0;
            LastBuffer:=0;
            {Player32 Stuff}
            DS_Seek(DSI^.CurrTime+10000);
          END;
      'R':{Fast Rewind}
          BEGIN
            {WaveOut Stuff}
            WaveOutReset(WaveHandle);
            FOR Error:=0 TO nBuffers-1 DO WaveHdr[Error].dwFlags:=whdr_done;
            CurrBuffer:=0;
            LastBuffer:=0;
            {Player32 Stuff}
            DS_Seek(DSI^.CurrTime-10000);
          END;
      ELSE DSI^.Repeating:=1;
    END;
    {Exit on KeyPress or if MOD is repeating}
  UNTIL (DSI^.Repeating<>0);
  {Clean up the Keyboard buffer}
  Writeln;
  Writeln('Shutting down ...');
  {Close WaveOut Device...!!!! THIS MUST BE DONE BEFORE DS_STOP !!!!}
  IF Settings.Device=0 THEN REPEAT Yield; UNTIL WaveOutClose(WaveHandle)=0;
  {Free the MOD Memory}
  DS_STOP;
  {...All done :)}
END.

