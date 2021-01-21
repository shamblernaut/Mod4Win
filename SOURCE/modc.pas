UNIT modc;
{$C Fixed Preload Permanent}
INTERFACE
{$IFDEF WINDOWS}
USES wintypes,winprocs,mmsystem,strings,win31,windos,ShellAPI;

{$IFDEF DEMO}
CONST
  SID_DEMO_VERSION = 350;
{$ENDIF}


CONST ProjectName = {$IFDEF HyperWare} 'HyperMOD' {$ELSE} 'MOD4WIN' {$ENDIF};
      RES_Pref    = {$IFDEF HyperWare} 'Hyp'      {$ELSE} 'RES'     {$ENDIF};
      HLP_Pref    = {$IFDEF HyperWare} 'Hyp'      {$ELSE} 'M4W'     {$ENDIF};
      INST_Pref   = {$IFDEF HyperWare} 'HINS'     {$ELSE} 'INST'    {$ENDIF};
      SongsPlayed:Longint=0;
      ini  = ProjectName + '.INI';
      EXE  = ProjectName + '.EXE';
      DLL  = 'PLAYER32.DLL';
      longver: ARRAY[0..42] OF char=ProjectName;
      shortver=ProjectName + ' ';
      errorver=shortver + 'Error';
      isplay=#175;
      willplay=' ';
      wasplay=#1;
      OvrRateChng:boolean=false;
      forcefilenamedisplay:boolean=true;
      PauseSetup:boolean=true;
      PlayOnLoad:Boolean=false;
      AskHD     :Boolean=False;
      load_state:boolean=true;
      hLanguage:word=0;
      {INISTRINGS}
      s_state   ='State';
      s_device  ='Playing Device';
      s_sampling='Sampling';
      s_packer  ='Packer';
      s_NoBPM   ='NonBPMSpeed';
      Restart:Boolean=False; {Für Sprachänderung...}
      Registered: boolean = {$IFDEF share} false {$ELSE} true {$ENDIF};

VAR   inidir        :ARRAY[0..fsPathName] OF char;  { complete path for ini file }
      m4wdir        :STRING[fsDirectory];           { mod4win directory }
      currentdir    :STRING[fsDirectory];
      username      :STRING[30];
      compname      :STRING[30];
      mixerpath     :ARRAY[0..fsPathName]  OF char; { complete path to mixer }
      HDPath        :ARRAY[0..fsDirectory] OF char; { DTD recording directory }
      RES, HLP      :string[fsPathName+fsFileName + fsExtension];
      detptr        :pchar;
      m_label       :hbitmap;

{ language stuff }

TYPE  TLang = (_usa,_ger,_fre,_spa,_grk,_ita,_nl,_cze);
const LangCodes: array[TLang] of string[3] = ('USA','GER','FRE','SPA','GRK','ITA','NL','CZE');
      LangNames: array[TLang] of string[8] = ('English','Deutsch','Français','Español','Greek','Italiano','Hollands','Czech');
VAR   LangPresent: SET OF TLang;
      Languages: byte;

{$ENDIF}

{*******************************************PLAYER.DLL***************************************************}
TYPE  TModType=(NoiseTr,StarTr,ProTr,Ftrk,WOW,MTM,OKTALYZ,f669,STM,fFAR,ULT,S3M,UNIS,DMF,FT2);
TYPE
     TInstrument=RECORD
                   Name      :STRING[41];
                   Offset    :Longint;
                   IEnd      :Longint;
                   LoopStart :Longint;
                   Loopend   :Longint;
                   Volume    :Byte;
                   FineTune  :Byte;
                   Bits      :Byte; {Bit 5..Stereo; Bit6..Gepackt; Bit7..S3M-Adlib-Instrument;}
                   Tuning    :Word;
                 END;

CONST
   LastEffect  = $44;
TYPE
   EffectSet   = SET OF 0..LastEffect;
   EffectState = ARRAY[1..32]OF EffectSet;
   tByteState  = ARRAY[1..32]OF byte;

   pCommentBuf = ^tCommentBuf;
   tCommentBuf = ARRAY[0..65519]OF Char;

TYPE PModulInfo=^TModulInfo;
     TModulInfo=RECORD
       FileType      :TModType;              {FileFormat}
       Dummy         :Byte;
       FileSiz       :WORD;                  {Dateigröße in kByte}
       ModName       :STRING;
       NumTracks     :Word;                  {Anzahl der Kanäle pro Notenzeile}
       DiffTracks    :Word;                  {Anzahl der voneinander verschiedenen gespielten Tracks}
       NumInstruments:Word;
       Instruments   :ARRAY[0..255]OF tInstrument;
       DummyPatterns :Word;                  {Anzahl der gespeicherten aber nicht gespielten Patterns}
       ExtraBytes    :Longint;               {Überlänge des Files}
       MissingBytes  :Longint;               {fehlende Bytezahl}
       EffectsUsedSet:EffectSet;
       PanningOff    :WordBool;              {Panning wird nicht benutzt}
       SongMem       :Longint;               {Speichergröße für Pattern und Samples}
       LoopFrom      :Integer;               {Pattern, von dem aus geloopt wird}
       LoopTo        :Integer;               {Pattern, zu welchem geloopt wird}
       Playing       :WordBool;              {Modul spielt}
       CPUUsage      :Longint;               {aktuelle CPU-Auslastung}
       TotalTime     :Longint;               {GesamtSpielzeit [ms]}
       CurrTime      :Longint;               {Aktuell zu hörende Zeit [ms]}
       BufTimeLen    :Longint;               {SamplePufferLänge [ms]}
       CurrPttrn     :Word;                  {Aktuell zu hörendes Pattern}
       NumPatterns   :Word;                  {Größe des Patternarrangements}
       DiffPatterns  :Word;                  {Anzahl der voneinander verschiedenen gespielten Pattern}
       CurrLine      :word;
       CurrEffects   :EffectState;           {zu hörende Effekte}
       CurrInstr     :tByteState;
       CurrVols      :tByteState;
       CurrNotes     :tByteState;
       CurrSpd       :Byte;
       CurrBPM       :Byte;
       Commentlen    :Word;
       Comment       :pCommentBuf;
       Dummy2        :Longint;
       Dummy3        :Longint;
     END;

CONST
  dev_DAC  = wave_Mapper; { -1 }
  dev_HDD  = -2;

TYPE pSettings=^tSettings;
     tSettings=RECORD
       Deviceid  :integer;
       Name      :STRING[40];
       samplerate:Longint;
       stereo    :byte;
       bits      :byte;
       nBuffers  :byte;
       Surround  :Boolean;
       NoBPM     :Boolean;
       TMode     :Byte;
       PreAmp    :Byte;
       Oversamp  :Byte;
       Panning   :Boolean;
       MemSize   :longint;
     END;

CONST
  defDACsetting:tsettings=(deviceid:dev_DAC; Name:'Windows Wave Mapper'; samplerate:48000; stereo:2;
                           bits:16; nBuffers:18; surround:true; NoBPM:false; tmode:2;
                           PreAmp:148; OverSamp:3;Panning:true; MemSize:0);
  defHDDsetting:tsettings=(deviceid:dev_HDD; Name:'Direct To Disk Recording'; samplerate:44100; stereo:2;
                           bits:16; nBuffers:16; surround:false; NoBPM:false; tmode:0;
                           PreAmp:148; OverSamp:3; Panning:true; MemSize:0);
{$IFDEF WINDOWS}
CONST
   {WMessages*************}
   wm_getresult    =2000;
   wm_newfiletoplay=2001;
   wm_getloadings  =2006;
   wm_box          =2007;
   wm_unpacking    =2008;

TYPE  Akey=RECORD key:word; state:byte; END;

CONST MAINHOTKEYS_BIS=43;
VAR   mykeys:ARRAY[1..MAINHOTKEYS_BIS] OF akey;
CONST defkeys:ARRAY[1..MAINHOTKEYS_BIS] OF akey=(
{12}   (key:word('F');  state:0),{File}
{ 3}   (key:word('P');  state:0),{Pause}
{ 2}   (key:vk_space;   state:0),{Play}
{43}   (key:word('A');  state:0),{About}
{10}   (key:word('O');  state:0),{Open}
{19}   (key:word('S');  state:0),{Setup}
{ 4}   (key:vk_up;      state:0),{Prev Song}
{ 7}   (key:vk_down;    state:0),{Next Song}
{ 6}   (key:vk_right;   state:0),{FF}
{ 5}   (key:vk_left;    state:0),{FR}
{ 8}   (key:vk_home;    state:0),{First Song}
{ 9}   (key:vk_end;     state:0),{Last Song}
{13}   (key:word('I');  state:0),{Info-Dialog}
{ 1}   (key:vk_escape;  state:0),{STOP}
{21}   (key:vk_f2;      state:0),{Shuffle}
{22}   (key:vk_f3;      state:0),{Repeat}
{23}   (key:vk_f4;      state:0),{Introscan}
{24}   (key:vk_f5;      state:0),{Copy}
{25}   (key:vk_f6;      state:0),{Move/Rename}
{27}   (key:vk_f8;      state:0),{Delete}
{15}   (key:word('T');  state:0),{Time-Mode}
{18}   (key:word('M');  state:0),{Mixer}
{14}   (key:word('E');  state:0),{Effekt-Panel}
{35}   (key:vk_add;     state:0),{Next Instrument in Detail-Dialog}
{36}   (key:vk_subtract;state:0),{Prev Instrument in Detail-Dialog}
{16}   (key:vk_prior;   state:0),{Show Filename}
{17}   (key:vk_next;    state:0),{Show Songname}
{20}   (key:vk_f1;      state:0),{HELP}
{28}   (key:vk_f9;      state:0),{Toggle Quality_Mode}
{29}   (key:vk_f10;     state:0),{Toggle BPM}
{30}   (key:vk_f11;     state:0),{Toggle Surround}
{31}   (key:vk_f12;     state:0),{Toggle IDO}
{34}   (key:vk_Multiply;state:0),{PreAmp up}
{33}   (key:vk_Divide;  state:0),{PreAmp down}
{32}   (key:vk_INSERT;  state:0),{Toggle PAN}
{37}   (key:vk_up;      state:2),{Detail Scrollbox up}
{38}   (key:vk_down;    state:2),{Detail Scrollbox down}
{39}   (key:vk_left;    state:2),{Detail Scrollbox left}
{40}   (key:vk_right;   state:2),{Detail Scrollbox right}
{26}   (key:vk_f7;      state:0),{Lösche Song aus Playliste}
{11}   (key:word('L');  state:0),{Quick select box}
{41}   (key:vk_add;     state:1),{Next Instrument in Instr-Dialog}
{42}   (key:vk_subtract;state:1) {Prev Instrument in Instr-Dialog}
       );
{$ENDIF}
CONST
   {Stringtable ID's}
    id_NoMem         =  1;
    id_FileNotFound  =  2;
    id_NoDevice      =  3;
    id_SyncDev       =  4;
    id_NoModFile     =  5;
    id_FileCorrupted =  6;
    id_NoFilesInList =  7;
    id_ExistFile     =  8;
    id_NixFile       =  9;
    id_InvModFile    = 10;
    id_FileUnsupport = 11;
    id_NoWriteFile   = 12;
    id_InvListFile   = 13;
    id_wrongini      = 14;
    id_driveerror    = 15;
    id_packed        = 16;
    id_unpacking     = 17;
    id_nofilesinarchiv=18;
    id_nomods        = 19;
    id_noarc         = 20;
    id_nomix         = 21;
    id_noenhanced    = 22;
    id_wronginput    = 23;
    id_No16Bit       = 24;
    id_nohelpfound   = 25;
    id_nosharesave   = 26;
    id_remove_text   = 27;
    id_remove_capt   = 28;
    id_recdlg_capt   = 29;

{ newopen dialog }

    id_newopen_first = 40;
    id_newopen_last  = 95;

{ progress indicator }

    id_loadingfiles  =100;
    id_loadcheckfiles=101;
    id_createplaylist=102;
    id_readingfiles  =103;
    id_takingplaylist=104;
    id_loadinglist   =105;

{ integrity message box }

    id_integbox_text = 200;
    id_integbox_capt = 201;

{ share dialog }

    id_sharestartlines=300;
    id_shareendlines  =310;
    id_shareoverlines =320;

{ about dialog }

    id_aboutlines     =400;

{Filetype ID's}

    id_filetypes      = 600; { in accordance to TModType }

{ restart on new language message box }

    id_restart_lang   = 700;
    id_restart_code   = 701;

{ various unsorted id's }

    id_filemenu       = 1999;
    id_copydlg_capt   = 2003;
    id_movedlg_capt   = 2004;
    id_filedlg_first  = 2005;
    id_pauseunpack    = 2013;
    id_filebrowse_capt= 2014;
    id_arcopts_capt   = 2022;
    id_setup_tmode    = 2023;
    id_setup_onoff    = 2026;
    id_hotkey_capt    = 2028;
    id_dirbrowse_capt = 2029;

{ setup dialog }

    id_setup_comments = 2050;
    id_setup_winmove  = 2056;
    id_arcopts_coms   = 2060;
    id_morearc_coms   = 2070;

{ registration dialog }

    id_regdlg_caption = 2080;
    id_regdlg_user    = 2081;
    id_regdlg_comp    = 2082;
    id_regdlg_info    = 2083;
    id_regdlg_code    = 2084;


{ hotkey editor dialog }

    id_hotkeys_first  = 2100;

{ info dialog }

    id_info_first     = 3000;
    id_insinfo_first  = 3045;

{ winunit }

    id_winunit_error  = 5000;
    id_winunit_ontop  = 5001;

{ sysmenu }

    id_menu_modinfo   = 5100;
    id_menu_effects   = 5101;

{ wave driver errors }

    id_waveerror_first= 10001;

{Helpfile ID's}

    id_maindlg       =0;
    id_setupdlg      =1;
    id_infodlg       =2;
    id_opendlg       =3;
    id_sharenotes    =4;
    id_registration  =5;
    id_instrwin      =6;
    id_effwnd        =7;
    id_keyseldlg     =8;
    id_arcoptdlg     =9;
    id_browsedlg     =10;
    id_arcparmdlg    =11;
    id_copydlg       =12;
    id_movedlg       =12;
    id_selbox        =13;
    id_minidlg       =15;
    id_recnamedlg    =16;
    id_regdlg        =17;
{$IFDEF WINDOWS}
FUNCTION getresstring(what:word):STRING;
PROCEDURE writeinistring(section,subsec,value:STRING);
PROCEDURE outdebug(What:integer; VAR Args);
FUNCTION formatstring(Whatres:integer; VAR args):STRING;
FUNCTION findexe(VAR Name:STRING):boolean;
PROCEDURE getthecorrectpath(VAR Name:STRING);
FUNCTION  shortpath(s:STRING; nmax:byte):STRING;
PROCEDURE savekeys(rubrik:STRING; startindex,endindex:word);
PROCEDURE setcorrectwinpos(VAR x,y,w,h:integer);
FUNCTION LoadMyLibrary(aName:pChar):tHandle;
FUNCTION MyGlobalAlloc(Flags: Word; Bytes: LongInt): THandle;
FUNCTION MyGlobalReAlloc(Mem: tHandle; Bytes: LongInt; Flags: Word): THandle;
FUNCTION MyGlobalAllocPtr(Flags: Word; Bytes: LongInt): Pointer;
FUNCTION FilePresent(fil:STRING):Boolean;

FUNCTION ExtensionsAssociated:Boolean;
PROCEDURE AssociateExtensions(CONST ExeFile:STRING);
{$ENDIF}
FUNCTION prevdir(instring:STRING):STRING;

TYPE tGPFType=(MemoryRead,MemoryWrite,DivByZero,InvalidOpCode);
PROCEDURE DoGPF(How:tGPFType);

IMPLEMENTATION

PROCEDURE DoGPF(How:tGPFType);
VAR a:WORD;
BEGIN
  CASE How OF
    MemoryRead   :a:=pWord(ptr(0,0))^;{Create an Exception...GPF}
    MemoryWrite  :pWord(ptr(0,0))^:=0;{Create an Exception...GPF}
    DivByZero    :ASM xor ax,ax; div ax END;{Create an Exception...divide by zero}
    InvalidOpCode:ASM DB 0DBh,00Eh,00h,00h,00h END;{Create an Exception...invalid opcode}
  END;
END;

{gibt string mit letztem text bis \ zurück}
FUNCTION prevdir(instring:STRING):STRING;
VAR i:byte;
BEGIN
 i:=byte(instring[0]);
 WHILE (i>0)AND(instring[i]<>'\') DO dec(i);
 prevdir:=Copy(instring,i+1,byte(instring[0])-i);
END;

{$IFDEF WINDOWS}
TYPE pPointer=^Pointer;
FUNCTION LoadMyLibrary(aName:pChar):tHandle;
VAR first,Current:pPointer;
    FirstSeg:Longint ABSOLUTE First;
    Lib:tHandle;
BEGIN
  {allen freien DOS-Speicher allockieren...}
  First:=Ptr(LoWord(GlobalDosAlloc(4096)),0);
  Current:=First;
  WHILE Current<>NIL DO BEGIN Current^:=Ptr(LoWord(GlobalDosAlloc(4096)),0); Current:=Current^; END;
  Lib:=LoadLibrary(aName);
  Current:=First;
  WHILE Current<>NIL DO BEGIN Current:=Current^; GlobalDosFree(HiWord(FirstSeg)); First:=Current; END;
  IF Lib>32 THEN LoadMyLibrary:=Lib ELSE LoadMyLibrary:=LoadLibrary(aName);
END;

FUNCTION MyGlobalAlloc(Flags: Word; Bytes: LongInt): THandle;
VAR first,Current:pPointer;
    FirstSeg:Longint ABSOLUTE First;
    GA:tHandle;
BEGIN
  {allen freien DOS-Speicher allockieren...}
  First:=Ptr(LoWord(GlobalDosAlloc(4096)),0);
  Current:=First;
  WHILE Current<>NIL DO BEGIN Current^:=Ptr(LoWord(GlobalDosAlloc(4096)),0); Current:=Current^; END;
  GA:=GlobalAlloc(Flags,Bytes);
  Current:=First;
  WHILE Current<>NIL DO BEGIN  Current:=Current^; GlobalDosFree(HiWord(FirstSeg)); First:=Current; END;
  IF GA>0 THEN MyGlobalAlloc:=GA ELSE MyGlobalAlloc:=GlobalAlloc(Flags,Bytes);
END;

FUNCTION MyGlobalReAlloc(Mem: tHandle; Bytes: LongInt; Flags: Word): THandle;
VAR first,Current:pPointer;
    FirstSeg:Longint ABSOLUTE First;
    GARe:tHandle;
BEGIN
  {allen freien DOS-Speicher allockieren...}
  First:=Ptr(LoWord(GlobalDosAlloc(4096)),0);
  Current:=First;
  WHILE Current<>NIL DO BEGIN Current^:=Ptr(LoWord(GlobalDosAlloc(4096)),0); Current:=Current^; END;
  GARe:=GlobalReAlloc(Mem,Bytes,Flags);
  Current:=First;
  WHILE Current<>NIL DO BEGIN  Current:=Current^; GlobalDosFree(HiWord(FirstSeg)); First:=Current; END;
  IF GARe>0 THEN MyGlobalReAlloc:=GARe ELSE MyGlobalReAlloc:=GlobalReAlloc(Mem,Bytes,Flags);
END;

FUNCTION MyGlobalAllocPtr(Flags: Word; Bytes: LongInt): Pointer;
VAR first,Current:pPointer;
    FirstSeg:Longint ABSOLUTE First;
    GAPtr:Pointer;
BEGIN
  {allen freien DOS-Speicher allockieren...}
  First:=Ptr(LoWord(GlobalDosAlloc(4096)),0);
  Current:=First;
  WHILE Current<>NIL DO BEGIN Current^:=Ptr(LoWord(GlobalDosAlloc(4096)),0); Current:=Current^; END;
  GAPtr:=GlobalAllocPtr(Flags,Bytes);
  Current:=First;
  WHILE Current<>NIL DO BEGIN  Current:=Current^; GlobalDosFree(HiWord(FirstSeg)); First:=Current; END;
  IF GAPtr<>NIL THEN MyGlobalAllocPtr:=GAPtr ELSE MyGlobalAllocPtr:=GlobalAllocPtr(Flags,Bytes);
END;

PROCEDURE setcorrectwinpos(VAR x,y,w,h:integer);
{VAR DC:hDC;
    wd,hd:Integer;}
BEGIN
  {DC:=GetDC(0);
  wd:=GetDeviceCaps(DC,horzres);
  hd:=GetDeviceCaps(DC,vertres);
  ReleaseDC(0,DC);
  IF (y+h)>hd THEN y:=hd-h;
  IF (x+w)>wd THEN x:=wd-w;}
  IF x<0 THEN x:=0;
  IF y<0 THEN y:=0;
END;

VAR outdebugin,outdebugtext:ARRAY[0..255] OF char;
PROCEDURE outdebug(What:integer; VAR Args);
BEGIN
  LoadString(hLanguage,what,outdebugin,256);
  wvsprintf(outdebugText,outdebugin,args);
  OutputDebugString(outdebugtext);
END;
FUNCTION formatstring(Whatres:integer; VAR args):STRING;
BEGIN
  LoadString(hLanguage,whatres,outdebugin,256);
  wvsprintf(outdebugText,outdebugin,args);
  formatstring:=strpas(outdebugtext);
END;

FUNCTION getresstring(what:word):STRING;
VAR pc:pchar;
BEGIN
  GetMem(pc,256);
  LoadString(hLanguage,what,pc,256);
  getresstring:=strpas(pc);
  FreeMem(pc,256);
END;

PROCEDURE savekeys(rubrik:STRING; startindex,endindex:word);
VAR i:word; s:STRING; s2:STRING[10];
BEGIN
   s:='';
   FOR i:=startindex TO endindex DO
   BEGIN
     Str(mykeys[i].key,s2);
     s:=s+s2+'|';
     Str(mykeys[i].STATE,s2);
     s:=s+s2+'|';
   END;
   WRITEINISTRING(S_State,rubrik,s);
END;

{kürzt Pfad-String auf nmax Zeichen zusammen - Für Display-Zwecke!}
FUNCTION shortpath;
VAR s2:STRING;
    k,j:integer;
BEGIN
  shortpath:=s;
  IF byte(s[0])>nmax THEN
  BEGIN
    s2:=s;
    k:=Pos('\',s2);
    WHILE byte(s2[0])>(nmax-4) DO
    BEGIN
      j:=Pos('\',s2);
      Delete(s2,Pos('\',s2),1);
      Delete(s2,j,Pos('\',s2)-j);
    END;
    Insert('\...',s2,k);
  END ELSE Exit;
  shortpath:=s2;
END;


FUNCTION findexe(VAR Name:STRING):boolean;
VAR dosenv           :pchar;
    Rec              :TSearchRec;
    cmds             :STRING;
    Path             :STRING[80];
    Fl               :FILE;
    i                :Word;
  PROCEDURE Searchit;
  BEGIN
    doserror:=0;
    FindFirst(@Path[1],$27,Rec);
    IF doserror=0 THEN BEGIN Assign(fl,path); GetFAttr(fl,i); END;
  END;
BEGIN
  cmds:=prevdir(Name);
  IF Pos('.',cmds)=0 THEN Name:=Name+'.exe';
  IF Pos('\',Name)=0 THEN
  BEGIN
    Path:=ParamStr(0);
    WHILE (Length(Path)>0)AND(Path[Length(Path)]<>'\') DO DEC(Byte(Path[0]));
    Path:=Path+Name+#0;
    Searchit;
    IF DosError<>0 THEN
    BEGIN
      dosenv:=GetDOSEnvironment;
      cmds:='';
      WHILE (dosenv^<>#0)AND(cmds='') DO
      BEGIN
        cmds:=strpas(dosenv);
        IF Pos('PATH',cmds)<>1 THEN
        BEGIN inc(dosenv,strlen(dosenv)+1); cmds:=''; END;
      END;
      Cmds:=Cmds+#0;
      Delete(cmds,1,5);
      WHILE (Length(cmds)>0)DO
      BEGIN
        Path:=Copy(cmds,1,Pos(';',cmds)-1); IF Path='' THEN Path:=cmds;
        IF Path[Length(Path)]<>'\' THEN Path:=Path+'\';
        Path:=Path+Name+#0;
        Searchit;
        IF (DosError=0)OR(Pos(';',cmds)=0)THEN CMDS[0]:=#0 ELSE Delete(cmds,1,Pos(';',cmds));
      END;
    END;
    Dec(Byte(Path[0]));
    Name:=Path;
  END ELSE BEGIN path:=Name+#0; Searchit; END;
  Findexe:=(DosError=0);
END;

PROCEDURE getthecorrectpath(VAR Name:STRING);
VAR olderrormode,fattr:word;
    pc :ARRAY[0..150]OF char;
    fl :FILE;
    rec:tsearchrec;
BEGIN
  olderrormode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  IF (Name='')OR(Pos(':',Name)=0) THEN Name:=m4wdir[1]+':\'
  ELSE IF Name[Length(Name)]<>'\' THEN Name:=Name+'\';
  strpcopy(pc,Name); strcat(pc,'*.*');
  {$i-}
  FindFirst(pc,$37,rec);
  IF doserror=0 THEN
  BEGIN
    Assign(fl,Name+rec.Name);
    GetFAttr(fl,fattr);
    IF fattr=0 THEN doserror:=1;
  END;
  IF NOT (doserror IN[0,18]) THEN Name:=m4wdir;
  IF Name[Length(Name)]='\' THEN dec(byte(Name[0]));
  SetErrorMode(olderrormode);
END;

PROCEDURE writeinistring(section,subsec,value:STRING);
BEGIN
  filemode:=1;
  section:=section+#0#0; subsec:=subsec+#0#0; value:=value+#0#0;
  WritePrivateProfileString(@section[1],@subsec[1],@value[1],inidir);
  filemode:=0;
END;

FUNCTION FilePresent(fil:STRING):Boolean;
VAR sr:tsearchrec;
BEGIN
  fil[Length(fil)+1]:=#0;
  FindFirst(@fil[1],faAnyFile,sr);
  FilePresent:=DosError=0;
END;

PROCEDURE AccociateExtension(CONST ExeFile:STRING;Ext:PChar;IcoIndex:Integer);
VAR XT  : ARRAY[0..80]OF Char;
    SUB : ARRAY[0..40]OF Char;
BEGIN
  {WIN.INI}
  IF EXT<>NIL THEN
  BEGIN
    StrPCopy(XT,ExeFile);
    StrCat(XT,' ^.');
    StrCat(XT,Ext);
    WriteProfileString('extensions',EXT,XT);      {win.ini -> [extensions] -> 669=c:\mod4win.exe ^.669}
  END;
  {Registry}
  IF EXT<>NIL THEN
  BEGIN
    StrCopy(XT,'.'#0);
    StrCat(XT,Ext);                               {XT :='.669'}
    StrCopy(SUB,'M4W_'#0);
    StrCat(SUB,Ext);                              {SUB:=M4W_669}
    RegSetValue (hKey_Classes_Root, XT, reg_sz, SUB, StrLen(SUB));
  END ELSE StrCopy(SUB,'ModulePlayer');
  StrCat(SUB,'\shell\open\command');            {SUB:=M4W_669\shell\open\command}
  StrPCopy(XT,ExeFile);
  StrCat(XT,' %1');                             {XT :=C:\MOD4WIN.EXE %1}
  RegSetValue (hKey_Classes_Root, SUB , reg_sz, XT, StrLen(XT));

  StrPCopy(XT,ExeFile);
  StrCat(XT,',');
  Str(IcoIndex,SUB);
  StrCat(XT,SUB);                               {XT :=C:\MOD4WIN.EXE,1}
  IF EXT<>NIL THEN
  BEGIN
    StrCopy(SUB,'M4W_'#0);
    StrCat(SUB,Ext);                            {SUB:=M4W_669}
  END ELSE StrCopy(SUB,'ModulePlayer');
  StrCat(SUB,'\DefaultIcon');                   {SUB:=M4W_669\DefaultIcon}
  RegSetValue (hKey_Classes_Root, SUB , reg_sz, XT, StrLen(XT));
END;

FUNCTION ExtensionAssociated(Ext:PChar):Boolean;
VAR S0 : ARRAY[0..80]OF Char;
    S1 : ARRAY[0..80]OF Char;
    I  : Longint;
BEGIN
  ExtensionAssociated:=False;
  {WIN.INI}
  IF EXT<>NIL THEN
  BEGIN
    StrPCopy(S1,ParamStr(0));
    GetProfileString('extensions',EXT,'',S0,SizeOf(S0));
    IF StrLIComp(S1,S0,Strlen(S1))<>0 THEN Exit;
  END;
  {Registry}
  IF Ext<>NIL THEN
  BEGIN
    StrCopy(S0,'.'#0);
    StrCat(S0,Ext);                               {XT :='.669'}
    I:=SizeOf(S1);
    IF (RegQueryValue (hKey_Classes_Root, S0, S1 , i)<>ERROR_SUCCESS)OR(I<3) THEN Exit;
  END ELSE StrCopy(S1,'ModulePlayer');
  StrCat(S1,'\shell\open\command');
  I:=SizeOf(S0);
  IF (RegQueryValue (hKey_Classes_Root, S1, S0 , i)<>ERROR_SUCCESS)OR(I<3) THEN Exit;
  StrPCopy(S1,ParamStr(0));
  IF StrLIComp(S1,S0,Strlen(S1))<>0 THEN Exit;
  ExtensionAssociated:=True;
END;

FUNCTION ExtensionsAssociated:Boolean;
BEGIN
  ExtensionsAssociated:=
        ExtensionAssociated(NIL)
    AND ExtensionAssociated('MOL')
    AND ExtensionAssociated('669')
    AND ExtensionAssociated('FAR')
    {AND ExtensionAssociated('IT'){}
    AND ExtensionAssociated('MOD')
    AND ExtensionAssociated('MTM')
    AND ExtensionAssociated('NST')
    AND ExtensionAssociated('OKT')
    AND ExtensionAssociated('S3M')
    AND ExtensionAssociated('STM')
    {AND ExtensionAssociated('ULT')}
    AND ExtensionAssociated('WOW')
    AND ExtensionAssociated('XM');
END;

PROCEDURE AssociateExtensions(CONST ExeFile:STRING);
BEGIN
  AccociateExtension(ExeFile,NIL,0);
  AccociateExtension(ExeFile,'MOL',0);
  AccociateExtension(ExeFile,'669',1);
  AccociateExtension(ExeFile,'FAR',2);
  {AccociateExtension(ExeFile,'IT',3);{}
  AccociateExtension(ExeFile,'MOD',4);
  AccociateExtension(ExeFile,'MTM',5);
  AccociateExtension(ExeFile,'NST',6);
  AccociateExtension(ExeFile,'OKT',7);
  AccociateExtension(ExeFile,'S3M',8);
  AccociateExtension(ExeFile,'STM',9);
  {AccociateExtension(ExeFile,'ULT',10);}
  AccociateExtension(ExeFile,'WOW',11);
  AccociateExtension(ExeFile,'XM',12);
END;
{$ENDIF}

PROCEDURE FindIniDir;
VAR s: STRING[fsPathName];
    F:File;
BEGIN
  { if run from a fixed disk use mod4win directory, otherwise use Windows directory }
  s := ParamStr (0);
  WHILE (s[Length (s)] <> '\') AND (Length (s) > 0) DO dec (s[0]);
  m4wdir := s;
  IF GetDriveType (Ord (UpCase (s[1])) - Ord ('A')) <> drive_fixed THEN
  BEGIN
    IF s[Length (s)] = '\' THEN dec (s[0]);
    S:=S+'\M4W#~~#~.~~~';
    FileMode:=2;
    ASSIGN(F,S);
    WHILE (s[Length (s)] <> '\') AND (Length (s) > 0) DO dec (s[0]);
    Rewrite(F);
    IF IOResult<>0 THEN s[0] := Chr (GetWindowsDirectory (@s[1], SizeOf (s)-1));
    Close(F);
    Erase(F);
  END;
  IF s[Length (s)] = '\' THEN dec (s[0]);
  StrPCopy (inidir, s + '\' + ini);
END;

BEGIN
  FindIniDir;
END.