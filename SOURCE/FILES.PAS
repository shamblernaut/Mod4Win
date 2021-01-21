UNIT files;
{ $C Fixed Preload Permanent}
INTERFACE

TYPE  TDirCallback=PROCEDURE(S:STRING);
      TRecurseDirs=FUNCTION (sSrc,sDst:STRING):Boolean;

CONST  IgnoreReadOnly:Boolean=False; { Ignore if File is Readonly }
       IgnoreSysFile :Boolean=True;  { Ignore if File is SYSTEM }
       IgnoreHidden  :Boolean=True;  { Ignore if File is HIDDEN }
       OverWrite     :Boolean=False; { Ignore if File already extists }
       CreatePath    :Boolean=True;  { Create Dst Path if it not exist }
       DstSetFlags   :Byte=0;
       DstKillFlags  :Byte=0;

FUNCTION IsValidDosName(Name:STRING):boolean;

FUNCTION IsDiskInDrive(Drive:Char):Boolean;

FUNCTION fCopy(sSrc,sDst:STRING):Boolean; {In SRC und DST sind WildCards, wie * und ? erlaubt}
FUNCTION fMove(sSrc,sDst:STRING):Boolean; {In SRC und DST sind WildCards, wie * und ? erlaubt}
FUNCTION xCopy(sSrc,sDst:STRING):Boolean; {In SRC und DST sind WildCards, wie * und ? erlaubt}
FUNCTION xMove(sSrc,sDst:STRING):Boolean; {In SRC und DST sind WildCards, wie * und ? erlaubt}

FUNCTION  ListZipFilesTo(Func:TDirCallBack; Name:STRING):BOOLEAN;
FUNCTION  ListArjFilesTo(Func:TDirCallBack; Name:STRING):BOOLEAN;
FUNCTION  ListLhaFilesTo(Func:TDirCallBack; Name:STRING):BOOLEAN;
PROCEDURE ListDosFilesTo(Func:TDirCallBack; Name:STRING);

IMPLEMENTATION

USES winapi,win31,winprocs,windos,strings,modc,owindows,omemory,objects,winunit;

TYPE psearchrec=^tsearchrec;

{Checkt, ob eine Disk im Laufwerk "DRIVE" ist...}
CONST IDIDSTR:ARRAY[0..15]OF Char ='C:\~~##!!.#~!'+#0#0#0;
FUNCTION IsDiskInDrive(Drive:Char):Boolean; ASSEMBLER;
ASM
  push ax
  mov bx,ds
  mov dx,Seg ididstr
  mov ds,dx
  mov dx,offset ididstr
  mov al,Drive
  mov byte Ptr ididstr,al
  mov ax,4300h
  Int 21h
  mov ax,0
  adc ax,0
  mov ds,bx
END;

{Überprüft, ob angegebener String ein valider DOS-Name ist (muss nicht existieren!)}
FUNCTION isvaliddosname(Name:STRING):boolean;
VAR i,lastslash,lastpoint,c:byte;
BEGIN
  isvaliddosname:=false;
  lastslash:=3;
  lastpoint:=0;
  IF (Name[2]<>':')OR((UpCase(Name[1])<'A')OR(UpCase(Name[1])>'Z'))OR((Name[0]>#3)AND(Name[3]<>'\'))THEN Exit;
  FOR i:=4 TO Length(Name) DO
  BEGIN
   c:=Ord(Name[i]);
   IF (c<33)OR(c=34)OR(c=42)OR(c=43)OR(c=44)OR(c=47)OR((c<64)AND(c>58))OR(c=91)OR(c=93)OR(c=124)THEN Exit
   ELSE IF Name[i]='\' THEN
   BEGIN
     IF (i<3) OR
        ((lastslash=0)AND(i<>3))
        OR(i-lastslash>13)
        OR((i-lastpoint>4)AND(lastpoint>lastslash))
        OR((i-lastslash>9)AND(lastpoint<lastslash))THEN Exit;
     lastslash:=i;
   END ELSE IF Name[i]=':' THEN
   BEGIN
     IF (i<>2)THEN Exit;
   END ELSE
   IF Name[i]='.' THEN
   BEGIN
     IF (lastslash<lastpoint)OR(i-lastslash<2)OR(lastslash=0)OR(i-lastslash>9)THEN Exit;
     lastpoint:=i;
   END;
   IF (i-lastslash>9)AND(lastpoint<lastslash)AND(lastpoint>lastslash)AND(i-lastpoint>3)THEN Exit;
  END;
  isvaliddosname:=true;
END;

{Nachbildung des DOS-Funktion SetFattr mit Filename...}
PROCEDURE SetFileAttr(p:pChar; Attr: Word); ASSEMBLER;
ASM
        PUSH    DS
        LDS     DX,p
        MOV     CX,Attr
        MOV     AX,4301H
        Int     21H
        POP     DS
        JC      @@1
        XOR     AX,AX
@@1:    MOV     DosError,AX
END;

{Nachbildung des DOS-Funktion SetFTime mit Filenhandle...}
PROCEDURE SetFileTime(hFile:Word; Time: Longint); ASSEMBLER;
ASM
        MOV     BX,hFile
        MOV     CX,WORD Ptr Time[0]
        MOV     DX,WORD Ptr Time[2]
        MOV     AX,5701H
        Int     21H
        JC      @@1
        XOR     AX,AX
@@1:    MOV     DosError,AX
END;

{Die ominösen "*??*" auflösen...
 in Src steht das Muster und in Dst die Sternchen}
PROCEDURE ResolveWildCards(Src,Dst:pChar);
BEGIN
  IF (StrScan(Dst,'*')=NIL)AND(StrScan(Dst,'?')=NIL)THEN Exit;
  WHILE Dst^<>#0 DO
  BEGIN
    IF Dst^='?' THEN Dst^:=Src^ ELSE IF Dst^='*' THEN BEGIN StrCopy(Dst,Src); Exit; END;
    IF Src^<>#0 THEN Inc(Src);
    Inc(Dst);
  END;
END;

{Teilt Path in Name und Dir}
PROCEDURE FSplit(Path:STRING; VAR Dir,Name:STRING);
VAR i:Byte;
BEGIN
  i:=Byte(Path[0]);
  WHILE (Path[i]<>'\') DO Dec(i);
  Dir:=Copy(Path,1,i);
  Name:=Copy(Path,i+1,Byte(Path[0])-i);
END;

{Dient als gemeinsamer Nenner für Move und Copy...}
FUNCTION MoveOrCopy(sSrc,sDst:STRING; MoveIt:Boolean):Boolean; {In SRC und DST sind WildCards, wie * und ? erlaubt}
VAR OldMode:Word;
    SR,Sr1:tSearchRec;
    Src,Dst:pChar;
    SrcDir,SrcName,SrcExt:pChar;
    DstDir,DstName,DstExt:pChar;
    p:pChar;
    hSrc,hDst:Integer;
    F:FILE;
LABEL _Exit,_FatalExit;
BEGIN
  OldMode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  {Anhängen von "\*.*" an unvollständige Pfade}
  IF (Pos('*',sDST)=0)AND(Pos('?',sDST)=0)AND((Pos('*',sSrc)<>0)OR(Pos('?',sSrc)<>0))THEN
  BEGIN
    IF sDst[Length(sDst)]<>'\' THEN sDst:=sDst+'\';
    sDst:=sDst+'*.*';
  END;
  {FileHandles auf Closed setzten}
  hSrc:=-1;
  hDst:=-1;
  MoveOrCopy:=False;
  {String-Speicher allockieren und Initialisieren}
  GetMem(Src    ,fsPathName+1 ); GetMem(SrcDir ,fsDirectory+1);
  GetMem(SrcName,fsFileName+1 ); GetMem(SrcExt ,fsExtension+1);
  GetMem(Dst    ,fsPathName+1 ); GetMem(DstDir ,fsDirectory+1);
  GetMem(DstName,fsFileName+1 ); GetMem(DstExt ,fsExtension+1);
  StrPCopy(Src,sSrc); FileExpand(Src,Src); FileSplit(Src,SrcDir,SrcName,SrcExt);
  StrPCopy(Dst,sDst); FileExpand(Dst,Dst); FileSplit(Dst,DstDir,DstName,DstExt);
  {Zielpfad erzeugen}
  p:=DstDir;
  Inc(p,3); {p^ zeigt hinter "C:\"}
  IF CreatePath THEN
  REPEAT
    p:=StrScan(p,'\');
    IF p<> NIL THEN p^:=#0 ELSE Break;
    FindFirst(DstDir,faDirectory,SR);
    IF DosError<>0 THEN
    BEGIN
      CreateDir(DstDir);
      IF DosError<>0 THEN GOTO _FatalExit;
    END;
    p^:='\';
    Inc(p);
  UNTIL False ELSE IF p^<>#0 THEN
  BEGIN
    p:=StrRScan(DstDir,'\');
    p^:=#0;
    FindFirst(DstDir,faDirectory,SR);
    IF DosError<>0 THEN GOTO _FatalExit;
    p^:='\';
  END;
  {Los geht's...}
  FindFirst(Src,$27,SR); IF DosError=18 THEN GOTO _Exit;
  {QuellPfad testen}
  IF (DosError<>0)OR(NOT IsDiskInDrive(Src^))THEN GOTO _FatalExit;
  WHILE DOSError=0 DO
  BEGIN
    {Quell- und Zielfilenamen erzeugen}
    StrCopy(Src,SrcDir); StrCat(Src,SR.Name); FileSplit(Src,SrcDir,SrcName,SrcExt);
    StrPCopy(Dst,sDst);  FileExpand(Dst,Dst); FileSplit(Dst,DstDir,DstName,DstExt);
    ResolveWildCards(SrcName,DstName); ResolveWildCards(SrcExt,DstExt);
    StrCopy(Dst,DstDir); StrCat(Dst,DstName); StrCat(Dst,DstExt);
    {Ziel=Quelle?}
    IF StrComp(Src,Dst)=0 THEN BEGIN DosError:=11; GOTO _FatalExit; END;
    {Flags testen}
    IF(NOT(IgnoreSysFile )AND((SR.Attr AND faSysFile )>0))
    OR(NOT(IgnoreHidden  )AND((SR.Attr AND faHidden  )>0))THEN BEGIN DosError:=2; GOTO _FatalExit END;
    {QuellAttribute setzen}
    SetFileAttr(Src,0);
    IF NOT(MoveIt AND(Src^=Dst^)) THEN
    BEGIN
      {Freien Speicher Prüfen}
      IF (DiskFree(Ord(UpCase(Dst^))-64)<SR.Size+8192) THEN
      BEGIN
        DosError:=8;
        GOTO _FatalExit;
      END;
    END;
    {Ist Ziel bereits vorhanden?}
    FindFirst(Dst,faAnyFile,SR1);
    IF DosError=0 THEN
    BEGIN
      {Darf Ziel überschrieben werden?}
      IF NOT(OverWrite)
      OR(((SR1.Attr AND faDirectory)>0)OR((SR1.Attr AND faVolumeID)>0))
      OR(NOT(IgnoreSysFile )AND((SR1.Attr AND faSysFile )>0))
      OR(NOT(IgnoreReadOnly)AND((SR1.Attr AND faReadOnly)>0))
      OR(NOT(IgnoreHidden  )AND((SR1.Attr AND faHidden  )>0))THEN BEGIN DosError:=5; GOTO _FatalExit END;
      {ZielAttribute setzen}
      SetFileAttr(Dst,0); IF DosError<>0 THEN GOTO _FatalExit;
    END;
    Assign(F,Src);
    IF MoveIt AND(Src^=Dst^) THEN
    BEGIN
      Rename(F,Dst);
    END ELSE
    BEGIN
      {Files öffnen}
      hSrc:=_lopen (Src,0); hDst:=_lcreat(Dst,0);
      IF (hSrc=-1)OR(hDst=-1) THEN BEGIN DosError:=5; GOTO _FatalExit; END;
      {Kopieren}
      p:=GlobalAllocPtr(gmem_fixed,32768); IF p=NIL THEN GOTO _FatalExit;
      WHILE _lwrite(hDst,p,_lread(hSrc,p,Integer(32768)))<>0 DO;
      GlobalFreePtr(p);
      {Zeit setzen}
      SetFileTime(hDst,SR.Time);
      {Files schliessen}
      _lclose(hSrc); hSrc:=-1;
      _lclose(hDst); hDst:=-1;
      {ZielFile löschen}
      IF MoveIt THEN Erase(F) ELSE SetFileAttr(Src,SR.Attr);
    END;
    SetFileAttr(Dst,DstSetFlags OR(SR.Attr AND(NOT DstKillFlags)));
    KeepWindowsAlive;
    IF (Application<>NIL)THEN Application^.IdleAction;
    FindNext(SR);
  END;
_Exit:
  MoveOrCopy:=True;
_FatalExit:
  {Clean Up}
  IF hSrc<>-1 THEN _lclose(hSrc);
  IF hDst<>-1 THEN _lclose(hDst);
  FreeMem(Src    ,fsPathName+1 ); FreeMem(SrcDir ,fsDirectory+1);
  FreeMem(SrcName,fsFileName+1 ); FreeMem(SrcExt ,fsExtension+1);
  FreeMem(Dst    ,fsPathName+1 ); FreeMem(DstDir ,fsDirectory+1);
  FreeMem(DstName,fsFileName+1 ); FreeMem(DstExt ,fsExtension+1);
  SetErrorMode(OldMode);
END;

{********************************** FILE COPY ****************************************************************}
FUNCTION fCopy(sSrc,sDst:STRING):Boolean; {In SRC und DST sind WildCards, wie * und ? erlaubt}
BEGIN
  fCopy:=MoveOrCopy(sSrc,sDst,False);
END;

FUNCTION fMove(sSrc,sDst:STRING):Boolean; {In SRC und DST sind WildCards, wie * und ? erlaubt}
BEGIN
  fMove:=MoveOrCopy(sSrc,sDst,True);
END;

{********************************** FILE COPY mit SubDirs ****************************************************}

{Ruft fCopy mit jedem SubDir unter SrcRoot auf...}
FUNCTION RecurseSubDirs(SrcRoot,SrcName,DstRoot,DstName:PString; Func:TRecurseDirs):Boolean;
VAR prec:psearchrec;
    pc:pChar;
    src,dst:PString;
LABEL _CleanUp;
BEGIN
  RecurseSubDirs:=False;
  GetMem(prec,SizeOf(tsearchrec));
  GetMem(Src,256);
  GetMem(Dst,256);
  GetMem(pc,256);
  StrPCopy(pc,SrcRoot^+'*.*');
  FindFirst(pc,faDirectory,prec^);
  FreeMem(pc,256);
  WHILE DOSError=0 DO
  BEGIN
    IF(prec^.Name[0]<>'.')AND((prec^.Attr AND faDirectory)>0)THEN
    BEGIN
      src^:=SrcRoot^+strpas(prec^.Name)+'\';
      dst^:=dstRoot^+strpas(prec^.Name)+'\';
      IF NOT RecurseSubDirs(Src,SrcName,Dst,DstName,fCopy) THEN GOTO _CleanUp;
    END;
    FindNext(prec^);
  END;
  RecurseSubDirs:=Func(SrcRoot^+SrcName^,DstRoot^+DstName^);
_CleanUp:
  FreeMem(prec,SizeOf(prec^));
  FreeMem(Src,256);
  FreeMem(Dst,256);
END;

{Dient als gemeinsamer Nenner für xMove und xCopy...}
FUNCTION XFiles(sSrc,sDst:STRING; Func:TRecurseDirs):Boolean; {In SRC und DST sind WildCards, wie * und ? erlaubt}
VAR OldMode:Word;
    SrcRoot,DstRoot:STRING[fsPathName+1];
    SrcName,DstName:STRING[fsFileName+fsExtension+2];
    CP:Boolean;
    SR:tSearchRec;
BEGIN
  xFiles:=False;
  OldMode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  {Anhängen von "\*.*" an unvollständige Pfade}
  IF (Pos('*',sDST)=0)AND(Pos('?',sDST)=0)AND((Pos('*',sSrc)<>0)OR(Pos('?',sSrc)<>0))THEN
  BEGIN
    IF sDst[Length(sDst)]<>'\' THEN sDst:=sDst+'\';
    sDst:=sDst+'*.*';
  END;
  {Pfad und Filenamen trennen...}
  sSrc[Length(sSrc)+1]:=#0; sDst[Length(sDst)+1]:=#0;
  FileExpand(@sSrc[1],@sSrc[1]); sSrc[0]:=Char(Strlen(@sSrc[1])); FSplit(sSrc,SrcRoot,SrcName);
  FileExpand(@sDst[1],@sDst[1]); sDst[0]:=Char(Strlen(@sDst[1])); FSplit(sDst,DstRoot,DstName);
  {Zielpfad da?}
  IF (NOT CreatePath)AND(Length(DstRoot)>3) THEN
  BEGIN
    DstRoot[Length(DstRoot)]:=#0;
    FindFirst(@DstRoot[1],faDirectory,SR);
    IF DosError<>0 THEN Exit;
    DstRoot[Length(DstRoot)]:='\';
  END;
  {Verzeichnisse durchwühlen}
  CP:=CreatePath;
  CreatePath:=True;
  xFiles:=RecurseSubDirs(@SrcRoot,@SrcName,@DstRoot,@DstName,Func);
  CreatePath:=CP;
  SetErrorMode(OldMode);
END;

FUNCTION xCopy(sSrc,sDst:STRING):Boolean; {In SRC und DST sind WildCards, wie * und ? erlaubt}
VAR Func:TRecurseDirs;
BEGIN
  @Func:=@fCopy;
  xCopy:=XFiles(sSrc,sDst,Func);
END;

FUNCTION xMove(sSrc,sDst:STRING):Boolean; {In SRC und DST sind WildCards, wie * und ? erlaubt}
VAR Func:TRecurseDirs;
BEGIN
  @Func:=@fMove;
  xMove:=XFiles(sSrc,sDst,Func);
END;

{**********************************************PKZip-Typen*****************************************************************}
TYPE  pBuffer=^bBuffer;
      bBuffer=ARRAY[0..8191] OF BYTE;

VAR   F    :FILE;
      Buf  :pBuffer;
      NumR :Word;
      i    :Word;
      lSeek:Longint;
      pStr :STRING;

CONST ZipLocalFileHeaderSignature   = $04034B50;
      ZipCentralFileHeaderSignature = $02014B50;
      ZipEndOfCentralDirSignature   = $06054B50;

TYPE  TZipLocalFileHeader = RECORD
                               Signature            :  LongInt;
                               Extract_Version_Reqd :  Word;
                               Bit_Flag             :  Word;
                               Compress_Method      :  Word;
                               Last_Mod_Time        :  Word;
                               Last_Mod_Date        :  Word;
                               Crc32                :  LongInt;
                               Compressed_Size      :  LongInt;
                               Uncompressed_Size    :  LongInt;
                               Filename_Length      :  Word;
                               Extra_Field_Length   :  Word;
                            END;
      TZipCentralFileHeader = RECORD
                                 Signature            :  LongInt;
                                 MadeBy_Version       :  Word;
                                 Extract_Version_Reqd :  Word;
                                 Bit_Flag             :  Word;
                                 Compress_Method      :  Word;
                                 Last_Mod_Time        :  Word;
                                 Last_Mod_Date        :  Word;
                                 Crc32                :  LongInt;
                                 Compressed_Size      :  LongInt;
                                 Uncompressed_Size    :  LongInt;
                                 Filename_Length      :  Word;
                                 Extra_Field_Length   :  Word;
                                 File_Comment_Length  :  Word;
                                 Starting_Disk_Num    :  Word;
                                 Internal_Attributes  :  Word;
                                 External_Attributes  :  LongInt;
                                 Local_Header_Offset  :  LongInt;
                              END;

        TZipEndOfCentralDir = RECORD
                                 Signature              :  LongInt;
                                 Disk_Number            :  Word;
                                 Central_Dir_Start_Disk :  Word;
                                 Entries_This_Disk      :  Word;
                                 Total_Entries          :  Word;
                                 Central_Dir_Size       :  LongInt;
                                 Start_Disk_Offset      :  LongInt;
                                 ZipFile_Comment_Length :  Word;
                              END;

FUNCTION ListZipFilesTo(Func:TDirCallBack; Name:STRING):BOOLEAN;
VAR DirEnd:Longint;
    ListedFiles:Longint;
LABEL VonVorne,Defekt;
BEGIN
  ListZipFilesTo:=FALSE;
  IF (@Func=NIL)OR(Name='') THEN Exit;
  Assign(F,Name);
  Filemode:=0;
  InOutres:=0;
  {$I-}Reset(F,1);
  IF (IOResult<>0)OR(FileSize(f)<SizeOf(TZipEndOfCentralDir))THEN Exit;
  Buf:=New(pBuffer);
  lSeek:=FileSize(f)-SizeOf(Buf^); IF lSeek<0 THEN lSeek:=0;
  {Suchen des Hauptverzeichnisses}
  REPEAT
    Seek(f,lSeek);
    BlockRead(f,Buf^,SizeOf(Buf^),NumR);
    dec(lSeek,SizeOf(Buf^)-SizeOf(TZipEndOfCentralDir)); IF lSeek<0 THEN lSeek:=0;
    FOR i:=numr-5 DOWNTO 0 DO
    IF Longint(Addr(Buf^[i])^)=ZipEndOfCentralDirSignature THEN
    BEGIN
      DirEnd:=FilePos(F)-numr+i;
      IF DirEnd+SizeOf(TZipEndOfCentralDir)>FileSize(F) THEN lSeek:=0
      ELSE lSeek:=TZipEndOfCentralDir(Addr(Buf^[i])^).Start_Disk_Offset;
      IF (lSeek<0)OR(lSeek>FileSize(F)) THEN lSeek:=0;
      Seek(F,lSeek);
      lSeek:=0;
      Break;
    END ELSE
    IF (Longint(Addr(Buf^[i])^)=ZipLocalFileHeaderSignature)
    OR (Longint(Addr(Buf^[i])^)=ZipCentralFileHeaderSignature) THEN
    BEGIN
      Seek(F,0);
      lSeek:=0;
      Break;
    END;
  UNTIL lSeek=0;
  NumR:=0;
  ListedFiles:=0;
  {Durchsuchen des Hauptverzeichnisses}
  IF FilePos(F)=0 THEN
  BEGIN
VonVorne:
    ListedFiles:=-1;
    REPEAT
      {Von vorne suchen(bei defektem END-Header)}
      BlockRead(F,Buf^,SizeOf(Buf^),NumR); IF NumR=0 THEN BEGIN Close(F); Dispose(Buf); Exit; END;
      FOR i:=0 TO NumR-5 DO
      IF (Longint(Addr(Buf^[i])^)=ZipLocalFileHeaderSignature)THEN
      BEGIN
        Seek(F,FilePos(F)-NumR+i);
        lSeek:=-1;
        Break;
      END ELSE
      IF (Longint(Addr(Buf^[i])^)=ZipEndOfCentralDirSignature)
      OR (Longint(Addr(Buf^[i])^)=ZipCentralFileHeaderSignature) THEN
      BEGIN
        Close(F);
        Dispose(Buf);
        Exit;
      END;
    UNTIL lSeek=-1;
    lSeek:=FilePos(F);
    WHILE lSeek<FileSize(F) DO
    BEGIN
      Seek(F,lSeek);
      BlockRead(F,Buf^,SizeOf(TZipLocalFileHeader)+80,NumR);
      WITH TZipLocalFileHeader(Addr(Buf^)^) DO
      BEGIN
        IF (Signature=ZipCentralFileHeaderSignature)
         OR(Signature=ZipEndOfCentralDirSignature)
         OR(Filename_Length+SizeOf(TZipLocalFileHeader)>Numr)
         OR(Filename_Length>80)
         OR(Filename_Length<1)
        THEN Break;
        StrMove(@pStr[1],Addr(Buf^[SizeOf(TZipLocalFileHeader)]),Filename_Length);
        pStr[0]:=char(Filename_Length);
        AnsiLowerBuff(@pStr[1],Filename_Length);
        IF pstr[Length(pstr)]<>'/' THEN Func(pStr);
        inc(lSeek,Filename_Length+Extra_Field_Length+SizeOf(TZipLocalFileHeader)+Compressed_Size);
      END;
    END;
  END ELSE
  REPEAT
    {Von Hinten suchen}
    Seek(F,FilePos(f)-NumR+lSeek);
    BlockRead(f,Buf^,SizeOf(Buf^),NumR); IF NumR<SizeOf(TZipCentralFileHeader) THEN Break;
    lSeek:=SizeOf(TZipCentralFileHeader);
    i:=0;
    WHILE (i+TZipCentralFileHeader(Addr(Buf^[i])^).Filename_Length+SizeOf(TZipCentralFileHeader)<=NumR)
       AND(Longint(Addr(Buf^[i])^)<>ZipEndOfCentralDirSignature)DO
    BEGIN
      WITH TZipCentralFileHeader(Addr(Buf^[i])^) DO
      IF (FileName_Length<81)AND(FileName_Length>0)THEN
      BEGIN
        StrMove(@pStr[1],Addr(Buf^[i+SizeOf(TZipCentralFileHeader)]),Filename_Length);
        pStr[0]:=char(Filename_Length);
        AnsiLowerBuff(@pStr[1],Filename_Length);
        IF pstr[Length(pstr)]<>'/' THEN BEGIN Func(pStr); Inc(ListedFiles); END;
        lSeek:=i;
        inc(i,Filename_Length+File_Comment_Length+extra_field_length+SizeOf(TZipCentralFileHeader));
        IF i+SizeOf(TZipCentralFileHeader)>NumR THEN Break;
      END ELSE GOTO Defekt;
    END;
  UNTIL FilePos(F)>Dirend;
DEFEKT:
  IF(ListedFiles=0)THEN BEGIN Seek(F,0); GOTO VonVorne; END;
  Dispose(Buf);
  ListZipFilesTo:=True;
  Close(F);
END;

{***************************************ARJ-Typen**************************************************************************}
CONST FirstHdrSize  = 30;
      FirstHdrSizeV = 34;
      CommentMax    = 2048;
      FNameMax      = 512;
      MaxSfX        = 1024;
      MaxHeaderSize = FirstHdrSize + 10 + FNameMax + CommentMax;

CONST
      Reserved     = $02;
      Volume_Flag  = $04;
      ExtFile_Flag = $08;
      PathSym_Flag = $10;
      Backup_Flag  = $20;

TYPE  TArjLocalHeader = RECORD
                         HeaderID           : Word;
                         BasicHeaderSize    : Word;
                         FirstHdrSize       : Byte;
                         ArchivVersion      : Byte;
                         MinExtractVersion  : Byte;
                         HostOS             : Byte;
                         ArjFlags           : Byte;
                         Method             : Byte;
                         FileType           : Byte;
                         Reserved1          : Byte;
                         DateTimeModified   : Longint;
                         CompressedSize     : Longint;
                         OriginalSize       : Longint;
                         OriginalFileCRC    : Longint;
                         FileSpecPos        : Word;
                         FileAccessMode     : Word;
                         HostData           : Word;
                         ExtraData          : Longint;
                         FileName           : ARRAY[0..512] OF Char;
                         ArchiveComment     : ARRAY[0..2048] OF Char;
                         BasicHeaderCRC     : Longint;
                         ExtHeaderSize      : Word;
                      END;

FUNCTION ListArjFilesTo(Func:TDirCallBack; Name:STRING):BOOLEAN;

  FUNCTION ReadHeader(bMain:BOOLEAN):BOOLEAN;
  VAR  pCh      : PChar;
  BEGIN
    ReadHeader:=FALSE;
    BlockRead(F, Buf^, SizeOf(TArjLocalHeader),NumR);
    IF NumR=0 THEN Exit;
    WITH TArjLocalHeader(Addr(Buf^)^) DO
    BEGIN
      IF (HeaderID <> $EA60) OR (BasicHeaderSize = 0) OR (BasicHeaderSize > MaxHeaderSize)THEN Exit;
      {pCh auf FileName setzen}
      pCh:=Addr(buf^[Ofs(ExtraData)]);
      IF ArjFlags AND ExtFile_Flag = ExtFile_Flag THEN INC(pCh,4);
      pStr:=StrPas(pCh);
      {pCh auf ArchivComment setzen}
      inc(pCh,strlen(pCh)+1);
      {pCh auf ExtHeaderSize setzen}
      inc(pCh,strlen(pCh)+5);
      IF (word(Addr(pCh^)^) <> 0) THEN inc(pCh,word(Addr(pCh^)^)+4);
      IF bMAIN THEN Seek(F, FilePos(F)-SizeOf(TArjLocalHeader)+Ofs(pch^)+2)
      ELSE Seek(F, FilePos(F)-SizeOf(TArjLocalHeader)+Ofs(pch^)+CompressedSize+2);
    END;
    ReadHeader:=True;
  END;

BEGIN
  ListArjFilesTo:= False;
  IF (@Func=NIL)OR(Name='') THEN Exit;
  Assign(F,Name);
  Filemode:=0;
  InOutres:=0;
  {$I-}Reset(F,1);
  IF (IOResult<>0)OR(FileSize(f)<SizeOf(TArjLocalHeader))THEN Exit;
  Buf:=New(pBuffer);
  {MainHeader finden}
  BlockRead(F, Buf^, 1024,NumR);
  FOR i:=0 TO NumR-1 DO IF word(Addr(buf^[i])^) = $EA60 THEN Break;
  IF (i=NumR-1)
  OR (word(Addr(buf^[i+2])^)= 0)
  OR (word(Addr(buf^[i+2])^)> MaxHeaderSize)THEN
  BEGIN
    Dispose(Buf);
    Close(F);
    Exit;
  END;
  Seek(F,i);
  {Durchsuchen des Files}
  ReadHeader(True);
  WHILE ReadHeader(False) DO
  BEGIN
    AnsiLowerBuff(@pStr[1],Length(pStr));
    IF pstr[Length(pstr)]<>'/' THEN Func(pStr);
    i:=0;
  END;
  ListArjFilesTo:= True;
  Close(F);
END;

{******************************************LHA-Typen***********************************************************************}
TYPE  TLHALocalFileHeader = RECORD
                              Headersize       :byte;
                              Headercheck      :byte;
                              method           :ARRAY[0..4] OF char;
                              Compressed_size  :longint;
                              Uncompressed_Size:longint;
                              time             :longint;
                              Attr             :word;
                              Filename_length  :byte;
                              Filename         :ARRAY[0..255] OF char;
                            END;

FUNCTION ListLhaFilesTo(Func:TDirCallBack; Name:STRING):BOOLEAN;
BEGIN
  ListLhaFilesTo:= False;
  IF (@Func=NIL)OR(Name='') THEN Exit;
  Assign(F,Name);
  Filemode:=0;
  InOutres:=0;
  {$I-}Reset(F,1);
  IF (IOResult<>0)OR(FileSize(f)<SizeOf(TLHALOCALFILEHEADER))THEN Exit;
  Buf:=New(pBuffer);
  {MainHeader finden}
  WHILE NOT(Eof(f)) DO
  BEGIN
    BlockRead(f,Buf^,SizeOf(TLHALocalFileHeader),NumR);
    Buf^[NumR-1]:=0;
    IF NumR=SizeOf(TLHALocalFileHeader) THEN
    WITH TLHALocalFileHeader(Addr(buf^)^) DO
    BEGIN
      IF (headersize<>(Ofs(Filename)+Filename_Length+3))
      AND(headersize<>(Ofs(Filename)+Filename_Length)) THEN
      BEGIN
        ListLhaFilesTo:=True;
        Close(f);
        Dispose(Buf);
        Exit;
      END;
      IF (Buf^[Headersize+1]=0)AND(Buf^[Headersize+2]=2)AND(Buf^[Headersize+3]>31) THEN
      BEGIN
        {Verzeichnis mit eingepackt}
        pStr:=strpas(pchar(@Buf^[Headersize+3]));
        i:=Length(pStr); IF (i>80)OR(i<3) THEN pStr:='' ELSE
        BEGIN
          dec(byte(pStr[0]),2);
          i:=Pos(#255,pStr); IF i=0 THEN pStr:='' ELSE
          WHILE i>0 DO
          BEGIN
            pStr[i]:='\';
            i:=Pos(#255,pStr);
          END;
        END;
      END ELSE pStr:='';
      filename[filename_length]:=#0;
      pStr:=pStr+'\'+strpas(@filename);
      WHILE (pstr[1]='\')AND(pstr[2]='\')DO Delete(pstr,1,1);
      AnsiLowerBuff(@pStr[1],Length(pStr));
      IF pstr[Length(pstr)]<>'\' THEN Func(pStr);
      Seek(f,FilePos(F)-SizeOf(TLHALocalFileHeader)+headersize+compressed_size+2);
    END;
  END;
  Close(f);
  ListLhaFilesTo:= True;
END;

{***********************************************normales DOS-Listing*******************************************************}
VAR s:STRING[100];

PROCEDURE ListDosFilesTo(Func:TDirCallBack; Name:STRING);
VAR prec:psearchrec;
BEGIN
  prec:=MemAlloc(SizeOf(tsearchrec));
  S:=Name+'*.*'+#0;
  FindFirst(@S[1],$37,prec^);
  WHILE IsDiskInDrive(Name[1])AND(DOSError=0) DO
  BEGIN
    s:=Name+strpas(prec^.Name);
    IF (prec^.Attr AND faDirectory)>0 THEN
    BEGIN
      s:=s+'\';
      IF (prec^.Name[0]<>'.')THEN
      BEGIN
        ListDosFilesTo(Func,S);
        IF DOSERROR=0 THEN Exit;
      END
    END ELSE
    BEGIN
      pStr:=S;
      AnsiLowerBuff(@pStr[1],Length(pStr));
      Func(pStr);
    END;
    FindNext(prec^);
  END;
  FreeMem(prec,SizeOf(prec^));
END;

END.