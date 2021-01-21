PROGRAM {$IFDEF HyperWare} HyperMOD {$ELSE} MOD4WIN {$ENDIF};
{ $C Fixed Preload Permanent}
{$S 60000}
{$M 22000,16000}
{$D --- High Quality Module Player (c) 1993-98 XSS ---}
{$R MOD4WIN.res}
{$R M4WICONS.res}

USES
  {$IFDEF Check} Check, {$ENDIF}
  Version, Language,
  {$IFDEF Share} Share, {$ELSE} UserName, {$ENDIF}
  Playproc,details,dsetup,winunit,about,modc,win31,winprocs,wintypes,Coroner,GetOS,
  strings,windos,owindows,omemory,mmsystem,shellapi,newopen,copywnd,objects,ana,mulbox;

CONST x_abscr=117;
      y_abscr=19;
      dx_abscr=308;
      dy_abscr=40;
      run_step=2;
      y_fun=75;
      x_fun1=114;
      x_fun2=388;
      x_box1=25;
      y_box=47;
      x_box2=464;
      w_box=50;
      h_box=46;


  {Mainframe}
    X_Main=     540;
    Y_Main=     115;
    X_Bar=      8;
    Y_Bar=      83;
    X_Btn=      367;
    Y_Btn=      88;
    Y_Btn1=     70;
    X_Name=     9;
    Y_Name=     10;
    X_MTime=    21;
    Y_MTime=    50;
    X_STime=    55;
    Y_STime=    50;
    X_Track=    94;
    Y_Track=    54;
    X_Tracks=   158;
    Y_Tracks=   54;
    X_Pattern=  225;
    Y_Pattern=  54;
    X_Patterns= 274;
    Y_Patterns= 54;


    x_sr=       374;
    y_sr=       10;
    X_Bits=     409;
    Y_bits=     10;
    x_m_st=     471;
    y_m_st=     12;
    X_PRCNT=    374;
    Y_Prcnt=    46;
    X_NumCh=    374;
    Y_NumCh=    28;
    X_PBar=     370;
    Y_PBar=     10;
    W_PBar=     4;
    H_PBar=     51;
    x_Btime=    409;
    y_Btime=    46;
    x_Btimelo=  429;
    y_Btimelo=  46;
    x_tmode=    447;
    y_tmode=    52;
    x_IDO=      472;
    y_IDO=       29;
    x_BPM=      413;
    y_BPM=       34;
    x_PAN=      442;
    y_PAN=       34;
    x_Qmix=     446;
    y_Qmix=      16;
    x_PreAmp=   514;
    y_PreAmp=    82;

    CONST nWMLRects=17;
    CONST  wmlrects:ARRAY[1..nWMLRects,0..3] OF integer=(
                                          (x_m_st,y_m_st,x_m_st+30,y_m_st+10),{Surrnd}
                                          (x_bpm ,y_bpm ,x_bpm +20,y_bpm +10),{BPM}
                                          (x_pan ,y_pan ,x_pan +20,y_pan +10),{pan}
                                          (x_ido ,y_ido ,x_ido +27,y_ido +18),{ido}
                                          (x_tmode,y_tmode,x_tmode+51,y_tmode+10),{Timer-Mode}
                                          (x_qmix ,y_qmix ,x_qmix +15,y_qmix +10),{Clip}
                                          (508    ,8      ,508+23    ,8+72),    {Preamp}
                                          (x_name ,y_name ,x_name+320,y_name+24),{Name}
                                          (315,43,315+32,43+8),{Shuffle}
                                          (315,54,315+39,54+8),{Repeat}
                                          (315,65,315+44,65+8),{Introscan}
                                          (10,50,83,73),{Time}
                                          (95,54,144,73),{Current Song}
                                          (159,54,205,73),{Total Songs}
                                          (226,54,262,73),{Current Pattern}
                                          (275,54,311,73),{Total Patterns}
                                          (375,10,437,22){SampleRate/Bits}
                                          );
    id_stop=    400;   id_play=    401;   id_pause=   402;   id_prev=     403;
    id_fr=      404;   id_ff=      405;   id_next=    406;   id_rep=      408;
    id_shuffle= 409;   id_scan=    410;   id_setup=   411;   id_open=     412;
    id_help=    413;   id_swname = 426;   id_btntime= 427;   id_datei  =  428;
    id_mixer  = 429;   id_minimize=430;   id_sysmenu= 431;   id_menuerase=450;
    id_menucopy=451;   id_menumove=452;   id_info=    453;   id_effects=  454;
    id_about   =455;   id_delfromlist=456; id_color=   457;
    id_LoadState=458;

TYPE  pmod=^tmod;
      tmod=object
       {playlist-Verwaltung}
       numberlist:ARRAY[0..MaxPlayListEntries] OF Word;
       LastMODFromOpen:Boolean;
       hmainframe :hbitmap;
       HWindow:HWND;
       Parent:Pwindowsobject;
       dirlist:Pmydirs;
       {Buttons}
       bplay  :pbmp3button1;
       bstop  :pbmp3button1;
       bpause :pbmp3button1;
       bff    :pbmpbutton1;
       bfr    :pbmpbutton1;
       bprev  :pbmpbutton1;
       bnext  :pbmpbutton;
       binfo  :pbmpbutton;
       beff   :pbmpbutton;
       bopen  :pbmpbutton;
       btntime:pbmpbutton;
       bdatei :pbmpbutton;
       bShuff :pbmpbutton;
       bRep   :pbmpbutton;
       bScan  :pbmpbutton;
       pswname:pswitch;
       {Textfelder}
       pmodname :pbmpfont;
       percnt   :pbmplcd;
       track    :pbmplcd;
       tracks   :pbmplcd;
       pattern  :pbmplcd;
       patterns :pbmplcd;
       mtime    :pbmplcd;
       stime    :pbmplcd;
       bits     :pbmplcd;
       sr       :pbmplcd;
       PAmp     :pbmplcd;
       NumCh    :pbmplcd;
       Btime    :pbmplcd;
       Btimelo  :pbmplcd;
       {bitmaps}
       h_sh_rep_scan :hbitmap;
       hm_st         :hbitmap;
       htMode        :hbitmap;
       about         :boolean;          {About aktiviert?}
       habout        :hbitmap;          {About-Bitmap}
       hfun          :HBitMap;
       hPreAmp       :hbitmap;
       minus_mode    :hbitmap;
       {Opendialog}
       opendlg:popen;
       {Setupdialog}
       setupdlg:psetup;
       {Aboutlaufschrift}
       aboutlauf:laufschrift;
       {Status}
       playing         :boolean;
       pause           :boolean;
       repeatmode      :byte;
       shuffle         :boolean;
       TryToPlay       :boolean;
       introScan       :boolean;
       minusmode       :boolean;
       songstate       :RECORD New:boolean; loadres:integer; END;
       spulff          :longint;
       spulfr          :longint;
       {Settings...}
       playlistcurrent :word;
       lastnameindex   :word;
       werte           :tsettings;
       mod_pc          :ARRAY[0..100] OF char;
       {About-Fenster}
       hboxes           :hbitmap;
       boxcount         :byte;
       cmdtimer         :word;
       aktrunpos        :word;
       mdy              :integer; {Yoffset im  Window}
       {INITERROR}
       Error            :word;
       {COPY-WINDOW}
       copywindow       :hwnd;
       peffect:peff;
       pcopy            :pcopywindow;
       {IDLEACTION}
       idletime:longint;
       IdleState:RECORD
         LastDriverOkTime :longint;
         Playtime        :longint;
         pattern         :word;
         patterns        :word;
         cpuusage        :longint;
         bufftime        :longint;
         NumChn          :Byte;
         Playing         :boolean;
         Pause           :boolean;
         repeatmode      :byte;
         shuffle         :boolean;
         introScan       :boolean;
         NextBlinkTime   :Longint;
       END;
       { time delays }
       IntroScanTime,
       PlayErrorDelay,
       BackTrackDelay,
       NextPrevDelay,
       BlinkTime:       word;
       {Initialisierungsmethoden}
       CONSTRUCTOR Init(AParent:Pwindowsobject);
       DESTRUCTOR Done; VIRTUAL;
       {Mainframe}
       PROCEDURE initmainframe;
       PROCEDURE closemainframe;
       PROCEDURE showmainframe(adc:hdc);
       PROCEDURE enableallbuttons(enableall:boolean);
       PROCEDURE setplaytime(sec:longint);
       PROCEDURE DrawPreAmp(ADC:HDC);
       PROCEDURE DrawPrcntBar(ADC:HDC);
       PROCEDURE setmodname(Name:STRING; conv2ansi: boolean);
       {Button-Handler}
       PROCEDURE ClickButton(What:pBMPButton;lparam:Longint);
       FUNCTION  idhandler(msg:tmessage):boolean;
       PROCEDURE switchQMode;
       PROCEDURE switchsurround;
       PROCEDURE switchBPM;
       PROCEDURE switchPAN;
       PROCEDURE switchIDO;
       PROCEDURE switchTMode;

       FUNCTION  kbdhandler(msg:tmessage):boolean;
       FUNCTION  wmkeyup(msg:tmessage):boolean;
       PROCEDURE handleswname;                VIRTUAL;
       PROCEDURE handlestopbutton;            VIRTUAL;
       PROCEDURE handlestartbutton(PauseIt:Boolean);    VIRTUAL;
       PROCEDURE handlepausebutton;           VIRTUAL;
       PROCEDURE handlepreviousbutton;        VIRTUAL;
       PROCEDURE handlefrbutton;              VIRTUAL;
       PROCEDURE handleffbutton;              VIRTUAL;
       PROCEDURE NextPattern;
       PROCEDURE PrevPattern;
       PROCEDURE handlenextbutton;            VIRTUAL;
       PROCEDURE handleshufflebutton;         VIRTUAL;
       PROCEDURE handlerepeatbutton;          VIRTUAL;
       PROCEDURE handleIntroscanbutton;       VIRTUAL;
       PROCEDURE handlehelpbutton;            VIRTUAL;
       PROCEDURE handleopenbutton;            VIRTUAL;
       PROCEDURE handlesetupbutton;           VIRTUAL;
       PROCEDURE handlehome;                  VIRTUAL;
       PROCEDURE handleend;                   VIRTUAL;
       PROCEDURE ModTimerProc(timerid:word);  VIRTUAL;
       PROCEDURE getnewmodname;               VIRTUAL;
       PROCEDURE handleSelSong;
       PROCEDURE handledelfilefromlist;
       PROCEDURE handleerasefile;
       PROCEDURE handlecopy;
       PROCEDURE handlemove;
       FUNCTION  IDLEACTION:Boolean;          VIRTUAL;
       PROCEDURE handleplayend;               VIRTUAL;
       PROCEDURE handleinfo;              {schaltet info-dlg an/aus}
       PROCEDURE handleeffects;
       {Diverses}
       FUNCTION  getcorrectfilename(VAR Name:STRING; setplay,DoPlay:boolean):boolean;
       PROCEDURE getsettings(haschanged,justdraw:boolean);
       PROCEDURE getloadings(bCancel:BOOLEAN);
       PROCEDURE HandleModAbout;
       FUNCTION  LOADEXTERN(alist:pmydirs; aname:STRING; stillunchanged:boolean):word;
       PROCEDURE wmdropfiles(msg:tmessage);
       FUNCTION  wmnewfiletoplay(msg:tmessage):boolean; {True wenn msg verarbeitet MOD!}
       PROCEDURE AnimateBox;
       PROCEDURE Show_unpacking(Name:STRING);
       PROCEDURE handledatei;
       PROCEDURE updatemodwindow;
       PROCEDURE RemoveCurrent;
       PROCEDURE SAVESTATE;
       PROCEDURE LoadSTATE;
       PRIVATE
       SetupPaused:Boolean;
     END;
TYPE psoundwindow=^tsoundwindow;
     tsoundwindow=object(tmychildwindow)
       bhelp  :pbmpCaptbtn;
       bsetup :pbmpCaptbtn;
       bmixer :pbmpCaptbtn;
       bcolor :pbmpCaptbtn;
       w_mod  :pmod;
       o_track:pbmplcd; m_track:pbmplcd;
       o_mtime:pbmplcd; m_mtime:pbmplcd;
       o_stime:pbmplcd; m_stime:pbmplcd;
       o_rect:trect;
       o_stop,o_play,o_pause,o_prev,o_next:tpoint;
       x_mini,y_mini:integer;
       ministate:boolean;
       miniontop:boolean;
       mixerhandle:word;
       colorDlg:pPictureSetup;
       {Initialisierungsmethoden}
       syschar: ARRAY[1999..2002] OF char;
       CaptRect, MiniDisplRect: TRect;
       PCursor,Norm_Cursor:HCursor;
       PreAmpClicked: boolean;
       CONSTRUCTOR    Init(AParent:Pwindowsobject; atitle:Pchar);
       DESTRUCTOR     Done; VIRTUAL;
       PROCEDURE SetupWindow; VIRTUAL;
     {Mainframe}
       PROCEDURE showmainframe(adc:hdc);
       PROCEDURE initministate;
       PROCEDURE closeministate;
       PROCEDURE paintministate(adc:hdc);
       PROCEDURE enableallbuttons(enableall:boolean);
       FUNCTION  GetClassName:PChar;                      VIRTUAL;
       PROCEDURE wmsize(VAR msg:tmessage);                VIRTUAL wm_first+wm_size;
       PROCEDURE WMMove(VAR msg: tmessage);               virtual wm_first+wm_Move;
       PROCEDURE wmsyscommand(VAR msg:tmessage);          VIRTUAL wm_first+wm_syscommand;
       PROCEDURE wmmenuchar(VAR msg:tmessage);            VIRTUAL wm_first+wm_menuchar;
       PROCEDURE wminitmenu(VAR msg:tmessage);            VIRTUAL wm_first+wm_initmenu;
       PROCEDURE wmupdate(VAR msg:tmessage);              VIRTUAL wm_first+wm_update;
       FUNCTION  TimerProc(VAR msg:tmessage):boolean;     VIRTUAL wm_first+wm_timer;
       PROCEDURE WMCommand(VAR msg:tmessage);             VIRTUAL wm_first+wm_command;
       PROCEDURE getsettings(VAR msg:tmessage);           VIRTUAL wm_first+wm_getresult;
       PROCEDURE getloadings(VAR msg:tmessage);           VIRTUAL wm_first+wm_getloadings;
       PROCEDURE WMLButtonDblclk(VAR msg:tmessage);       VIRTUAL wm_First+wm_LButtonDblclk;
       PROCEDURE WMLButtonDown(VAR msg: tmessage);        virtual wm_first+wm_LButtonDown;
       PROCEDURE WMLButtonUp(VAR msg: tmessage);          virtual wm_first+wm_LButtonUp;
       PROCEDURE WMMouseMove(VAR msg: tmessage);          virtual wm_first+wm_MouseMove;
       PROCEDURE WMRButtonDown(VAR msg: tmessage);        virtual wm_first+wm_RButtonDown;
       PROCEDURE wmdropfiles(VAR msg:tmessage);           VIRTUAL wm_first+wm_dropfiles;
       PROCEDURE wmnewfiletoplay(VAR msg:tmessage);       VIRTUAL wm_first+wm_newfiletoplay;
       PROCEDURE wmkeydown(VAR msg:tmessage);             VIRTUAL wm_first+wm_keydown;
       PROCEDURE wmkeyUp(VAR msg:tmessage);               VIRTUAL wm_first+wm_keyUp;
       PROCEDURE wmbox(VAR msg:tmessage);                 VIRTUAL wm_first+wm_box;
       PROCEDURE wmerasebkgnd(VAR msg:tmessage);          VIRTUAL wm_first+wm_erasebkgnd;
       PROCEDURE wmendsession(VAR msg:tmessage);          VIRTUAL wm_first+wm_endsession;
       PROCEDURE wmunpacking(VAR msg:tmessage);           VIRTUAL wm_first+wm_unpacking;
       PROCEDURE wmdrawitem(VAR msg:tmessage);            VIRTUAL wm_first+wm_drawitem;
       PROCEDURE wmsongselected(VAR msg:tmessage);        VIRTUAL wm_first+wm_songselected;
       PROCEDURE wmactivateapp(VAR msg:tmessage);         VIRTUAL wm_first+wm_activateapp;
       PROCEDURE wmAbout(VAR Msg:tMessage); VIRTUAL wm_First+wm_About;
       FUNCTION  CanClose:boolean; VIRTUAL;
       PROCEDURE paintcaption(adc:hdc); VIRTUAL;
       FUNCTION  IDLEACTION:BOOLEAN; VIRTUAL;
     END;

PROCEDURE LogError(CONST aFile:String;What:Word);
VAR F:Text;
    Y,M,D,Dow:Word;
    H,MM,S,S100:Word;
BEGIN
  IF GetPrivateProfileInt(s_state,'LOG_ERRORS',0,INIDIR)=0 THEN Exit;
  Assign(F,M4WDIR+'\M4W_ERR.LOG');
  InOutRes:=0;
  Append(F);
  IF IOResult<>0 THEN Rewrite(F);
  GetDate(Y,M,D,Dow);
  GetTime(H,MM,S,S100);
  Writeln(F,Y:4,'/',M:2,'/',D:2,'; ',H:2,':',MM:2,':',S:2,' - ',aFile,' ...',getresstring(what));
  Close(F);
END;

PROCEDURE LOG_ErrMessageBox(CONST aFile:String;pwindow:pwindowsobject;what:word);
BEGIN
  LogError(aFile,What);
  ErrMessageBox(pwindow,what);
END;

{*************************************MAINFRAME**************************************************************}
CONSTRUCTOR tmod.Init;
VAR i:word; code,Check:integer;
    s:STRING;
    s2:STRING[10];
BEGIN
   LastMODFromOpen:=True;
   opendlg        :=NIL;
   Parent         :=aparent;
   FillChar(numberlist,SizeOf(numberlist),0);
   dirlist        :=New(pmydirs,Init);
   mdy            :=pmywindow(Parent)^.CapDY;
   Error          :=0;
   playlistcurrent:=0;
   lastnameindex  :=$FFFF;
   cmdtimer       :=0;
   aktarc         :='';
   about          :=false;
   playlistcurrent:=0;
   repeatmode     :=0;
   shuffle        :=false;
   introScan      :=false;
   playing        :=false;
   pause          :=false;
   songstate.New  :=true;
   songstate.loadres:=-1000;
   spulff         :=0;
   spulfr         :=0;
   TryToPlay      :=false;
   {Für Setup...<>0 --> Samplerate in 1kHz Schritten, egal, ob unterstützt}
   OvrRateChng:=GetPrivateProfileInt(s_state,'OverrideRateChange',1,INIDIR)<>0;
   {Pausiert während SETUP}
   PauseSetup:=GetPrivateProfileInt(s_state,'PauseWhileSetup',0,INIDIR)<>0;
   PlayOnLoad:=GetPrivateProfileInt(s_state,'PlayOnLoad',0,INIDIR)<>0;
   MoveOnClick:=GetPrivateProfileInt(s_state,'MoveOnClick',1,INIDIR)<>0;
   AskHD:=GetPrivateProfileInt(s_state,'AskHD',1,INIDIR)<>0;
   forcefilenamedisplay:=GetPrivateProfileInt(s_state,'ForceFileNameDisplay',1,INIDIR)<>0;
   {MODHOTKEYS}
   s[0]:=char(GetPrivateProfileString(s_state,'ModMainHotkeys','',@s[1],255,INIDIR));
   { timer delays }
   IntroScanTime  := GetPrivateProfileInt ('Timers', 'IntroScanTime',  15000, INIDIR);
   PlayErrorDelay := GetPrivateProfileInt ('Timers', 'PlayErrorDelay',  2000, INIDIR);
   BackTrackDelay := GetPrivateProfileInt ('Timers', 'BackTrackDelay',  1500, INIDIR);
   NextPrevDelay  := GetPrivateProfileInt ('Timers', 'NextPrevDelay',    300, INIDIR);
   BlinkTime      := GetPrivateProfileInt ('Timers', 'BlinkTime',        300, INIDIR);
   IF Length(s)>0 THEN
   BEGIN
     i:=1;
     WHILE (Length(s)>0)AND(i<=MainHotKeys_bis) DO
     BEGIN
       s2:=Copy(s,1,Pos('|',s)-1);
       Val(s2,code,Check);
       IF Check=0 THEN mykeys[i].key:=code ELSE mykeys[i].key:=defkeys[i].key;
       Delete(s,1,Pos('|',s));
       s2:=Copy(s,1,Pos('|',s)-1);
       Val(s2,code,Check);
       IF Check=0 THEN mykeys[i].state:=code ELSE mykeys[i].state:=defkeys[i].state;
       Delete(s,1,Pos('|',s));
       INC(i);
     END;
     IF i<=MAINHOTKEYS_BIS THEN Move(defkeys[i],mykeys[i],SizeOf(akey)*(MAINHOTKEYS_BIS-i+1));
   END ELSE Move(defkeys,mykeys,SizeOf(mykeys));
   savekeys('ModMainHotkeys',1,MAINHOTKEYS_BIS);
    { Soundsettings initialisieren und überprüfen }
   WITH werte DO
   BEGIN
     deviceid:=integer(GetPrivateProfileInt(s_device,'DeviceID',99,INIDIR));
     GetPrivateProfileString(s_device,'DeviceName','Wave Mapper',mod_pc,30,INIDIR);
     Name:=strpas(mod_pc);
     CASE DeviceID OF
       dev_HDD :
       BEGIN
         werte := defHDDSetting;
         nBuffers:=GetPrivateProfileInt(s_sampling,'HDDRecBuffers',0,INIDIR);
         IF(nbuffers>99)OR(nbuffers<1)THEN nBuffers:=4;
       END;
       ELSE BEGIN
         s[0]:=char(GetPrivateProfileString(s_sampling,'SampleRate','48000',@s[1],255,INIDIR)); Val(S,SampleRate,code);
         IF(samplerate>96000)OR(samplerate<1000)THEN samplerate:=0;
         bits:=GetPrivateProfileInt(s_sampling,'BitsPerSample',16,INIDIR);
         stereo:=GetPrivateProfileInt(s_sampling,'Channels',2,INIDIR);
         nBuffers:=GetPrivateProfileInt(s_sampling,'OutputBuffers',18,INIDIR);
         IF(nbuffers>99)OR(nbuffers<2)THEN nBuffers:=24;
         {0...MMTask; 1...WM_Timer; 2...MMTimer}
         tmode:=GetPrivateProfileInt(s_sampling,'TimerMode',2,INIDIR);
         PreAmp:=GetPrivateProfileInt(s_sampling,'Clipping',1,INIDIR) SHL 7;
         surround:=GetPrivateProfileInt(s_sampling,'Surround',1,INIDIR)<>0;
         OverSamp:=GetPrivateProfileInt(s_sampling,'OverSampling',3,INIDIR);
         IF NOT(OverSamp IN [0..3]) THEN OverSamp:=1;
         Panning:=GetPrivateProfileInt(s_sampling,'EnablePanning',1,INIDIR)<>0;
       END;
     END;
     code:=GetPrivateProfileInt(s_sampling,'PreAmplification',20,INIDIR);
     IF (code > 40) OR (code < 0) THEN code := 20;
     PreAmp := PreAmp AND $80 OR code AND $7F;
     NoBPM:=False;
     {============================================NEU============================================}
   END;
   IF NOT mod_CanPlay (werte, OvrRateChng) THEN
   BEGIN
     werte := DefDACSetting; IF NOT mod_CanPlay (werte, OvrRateChng) THEN
     BEGIN
       werte := DefHDDSetting;
     END;
     IF ourmessagebox(NIL,longver,getresstring(id_wrongini),mb_yesno OR mb_iconquestion) = id_yes THEN Error := 1;
   END;
   initmainframe;
   copywindow:=0;
   aktrunpos:=0;
END;

DESTRUCTOR tmod.Done;
VAR s:STRING[10];
BEGIN
  IF IsWindow(copywindow) THEN SendMessage(copywindow,wm_close,0,0);
  forcefilenamedisplay:=pswname^.GetPos=1;
  Str(byte(forcefilenamedisplay),s); writeinistring(s_state,'ForceFileNameDisplay'+#0,s);
  Str(werte.OverSamp,s);writeinistring(s_sampling,'OverSampling'#0,S);
  IF detailWND<>0 THEN SendMessage(detailwnd,wm_close,0,0);
  IF playing THEN mod_stop;
  IF about THEN aboutlauf.Done;
  dirlist^.grillunpackedsong;
  closemainframe;
  Dispose(dirlist,Done);
END;

PROCEDURE tmod.initmainframe;
BEGIN
  bstop  :=New(Pbmp3Button1, Init(Parent, id_stop,X_Bar,mdy+Y_Bar,'bstop',false));
  bplay  :=New(Pbmp3Button1, Init(Parent, id_play,X_Bar+50,mdy+Y_Bar,'bplay',false));
  bpause :=New(Pbmp3Button1, Init(Parent, id_pause,X_Bar+100,mdy+Y_Bar,'bpause',false));
  bprev  :=New(PbmpButton1, Init(Parent, id_prev,X_Bar+150,mdy+Y_Bar,'bprev',true));
  bfr    :=New(PbmpButton1, Init(Parent, id_fr,X_Bar+200,mdy+Y_Bar,'bfr',true));
  bff    :=New(PbmpButton1, Init(Parent, id_ff,X_Bar+250,mdy+Y_Bar,'bff',true));
  bnext  :=New(PbmpButton, Init(Parent, id_next,X_Bar+300,mdy+Y_Bar,'bnext',true));

  bopen  :=New(PbmpButton, Init(Parent, id_open,   X_Btn   ,mdy+Y_Btn,'bopen',false));
  bdatei :=New(PbmpButton, Init(Parent, id_datei,  X_Btn+33,mdy+Y_Btn,'bdatei',false));
  binfo  :=New(PbmpButton, Init(Parent, id_info,   X_Btn+66,mdy+Y_Btn,'binfo',false));
  beff   :=New(PbmpButton, Init(Parent, id_effects,X_Btn+99,mdy+Y_Btn,'beffects',false));
  bShuff :=New(PbmpButton, Init(Parent, id_shuffle,X_Btn   ,mdy+Y_Btn1,'bShuffle',false));
  bRep   :=New(PbmpButton, Init(Parent, id_rep,    X_Btn+33,mdy+Y_Btn1,'bRepeat',false));
  bScan  :=New(PbmpButton, Init(Parent, id_scan,   X_Btn+66,mdy+Y_Btn1,'bIntroScan',false));
  btntime:=New(PbmpButton, Init(Parent, id_btntime,X_Btn+99,mdy+Y_Btn1,'btime',false));

  pmodname:=New(pbmpfont  , Init(Parent, -1,X_name,mdy+Y_name,20,2,'font_16x20',64));
  pSWName :=New(pswitch   , Init(Parent, id_swname,11+x_name+pmodname^.Attr.w,mdy+y_name-1,2,1,'switch'));

  percnt:=New  (pbmplcd   , Init(Parent, -1,X_Prcnt   ,mdy+Y_Prcnt   ,3,0,'LCD_08x13'));
  pattern:=New (pbmplcd   , Init(Parent, -1,x_pattern ,mdy+y_pattern ,3,0,'lcd_13x20'));
  patterns:=New(pbmplcd   , Init(Parent, -1,x_patterns,mdy+y_patterns,3,0,'lcd_13x20'));
  track:=New   (pbmplcd   , Init(Parent, -1,X_Track   ,mdy+y_track   ,4,0,'lcd_13x20'));
  tracks:=New  (pbmplcd   , Init(Parent, -1,x_tracks  ,mdy+y_tracks  ,4,0,'lcd_13x20'));
  mtime:=New   (pbmplcd   , Init(Parent, -1,x_mtime   ,mdy+y_mtime   ,2,0,'lcd_15x24'));
  stime:=New   (pbmplcd   , Init(Parent, -1,x_stime   ,mdy+y_stime   ,2,1,'lcd_15x24'));
  bits:=New    (pbmplcd   , Init(Parent, -1,X_bits    ,mdy+Y_bits    ,2,0,'LCD_08x13'));
  sr:=New      (pbmplcd   , Init(Parent, -1,X_sr      ,mdy+Y_sr      ,2,0,'LCD_08x13'));
  PAmp:=New    (pbmplcd   , Init(Parent, -1,X_PreAmp  ,mdy+Y_PreAmp  ,2,0,'LCD_08x13'));
  NumCh:=New   (pbmplcd   , Init(Parent, -1,X_NumCh   ,mdy+Y_NumCH   ,2,0,'LCD_08x13'));
  Btime:=New   (pbmplcd   , Init(Parent, -1,X_btime   ,mdy+Y_btime   ,2,0,'LCD_08x13'));
  Btimelo:=New (pbmplcd   , Init(Parent, -1,X_btimelo ,mdy+Y_btimelo ,1,0,'LCD_08x13'));
  h_sh_rep_scan:= LoadMyBitMap(hinstance,'sh_rep_scan');
  hm_st        := LoadMyBitMap(hinstance,'m_st');
  htMode       := LoadMyBitMap(hinstance,'tMode');
  habout       := LoadMyBitMap(hinstance,'about');
  hPreAmp      := LoadMyBitMap(hinstance,'preAmp');
  hboxes       := LoadMyBitMap(hinstance,'boxes');
  hmainframe   := LoadMyBitMap(hinstance,'modmainframe');
  minus_mode   := LoadMyBitMap(hinstance,'minus_mode');
  hfun         := LoadMyBitMap(hinstance,'funbmp');
  sr^      .setbmpzahl((werte.samplerate + 500) DIV 1000);
  bits^    .setbmpzahl(longint(werte.bits));
  minusmode:=false;
  Track^.SetBMPZahl(0);
  Tracks^.SetBMPZahl(0);
  FillChar(IdleState,SizeOf(IdleState),$ff);
END;

PROCEDURE tmod.closemainframe;
BEGIN
  DeleteMyBitMap(h_sh_rep_scan);
  DeleteMyBitMap(hm_st);
  DeleteMyBitMap(htMode);
  DeleteMyBitMap(habout);
  DeleteMyBitMap(hPreAmp);
  DeleteMyBitMap(hboxes);
  DeleteMyBitMap(hmainframe);
  DeleteMyBitMap(minus_mode);
  DeleteMyBitMap(hfun);
END;

PROCEDURE tmod.showmainframe(ADc:HDc);
VAR dc,pdc,memdc:hdc;
    Mode: byte;
    membmp:hBitmap;
    _mdy:Integer;
BEGIN
  {ADC=0 bedeutet : keine WM_paint mit rgn sondern völlig neu zeichnen}
  IF hWindow=0 THEN Exit;
  IF adc=0 THEN dc:=GetDC(HWindow) ELSE dc:=adc;
  memdc:=CreateCompatibleDC(dc);
  IF PaletteInstalled THEN
  BEGIN
    SelectPalette(dc,hPal,false);
    SelectPalette(memdc,hPal,false);
  END;
  IF about THEN
  BEGIN
    SelectObject(memdc,habout);
    BitBlt(dc,0,mdy,x_main,y_main,memdc,0,0,srccopy);
  END ELSE
  BEGIN
    _mdy:=mdy;
    mdy:=0;
    pdc:=CreateCompatibleDC(dc);
    IF PaletteInstalled THEN SelectPalette(pdc,hPal,false);
    membmp:=CreateCompatibleBitmap(dc,x_main,y_main);
    SelectObject(pdc,membmp);
    SelectObject(memdc,hmainframe);
    BitBlt(pdc,0,mdy,x_main,y_main,memdc,0,0,srccopy);
    SelectObject(memdc,minus_mode);
    IF minusmode THEN BitBlt(pdc,11,mdy+61,8,2,memdc,0,0,srccopy)
                 ELSE BitBlt(pdc,11,mdy+61,8,2,memdc,0,2,srccopy);
    SelectObject(memdc,hm_st);
    BitBlt(pdc,x_m_st,mdy+y_m_st,27,11,memdc,0,11*(werte.stereo+Byte(Werte.Surround)-1),srccopy);
    SelectObject(memdc,htMode);
    CASE werte.DeviceID OF
      dev_HDD:  Mode := 5;
      ELSE      Mode := werte.TMode;
    END;
    BitBlt(pdc,x_tMode,mdy+y_tMode,23,9,memdc,0,9*Mode,srccopy);
    SelectObject(memdc,h_sh_rep_scan);
    BitBlt(pdc,x_IDO,mdy+y_IDO,26,17,memdc,26*Werte.OverSamp,66,srccopy);
    BitBlt(pdc,x_BPM,mdy+y_BPM,26, 9,memdc,0 ,48+9*(1-Byte(Werte.NoBPM)AND 1),srccopy);
    BitBlt(pdc,x_PAN,mdy+y_PAN,23, 9,memdc,26,48+9*(Byte(Werte.Panning)AND 1),srccopy);
    BitBlt(pdc,x_Qmix,mdy+y_Qmix,18, 9,memdc,26*(1-Byte(Werte.PreAmp)SHR 7),83,srccopy);
    IdleState.introScan:=NOT(Introscan);
    IdleState.repeatmode:=4;
    IdleState.Shuffle:=NOT(Shuffle);
    IF adc=0 THEN enableallbuttons(true);
    DrawPrcntBar(pdc);
    DrawPreAmp(pdc);
    mdy:=_mdy;
    BitBlt(dc,0,mdy,x_main,y_main,pdc,0,0,srccopy);
    DeleteDC(pdc);
    DeleteObject(MemBMP);
  END;
  DeleteDC(memdc);
  IF adc=0 THEN ReleaseDC(HWindow,dc);
END;

PROCEDURE tmod.enableallbuttons;
BEGIN
  IF enableall THEN
  BEGIN
    bplay^.enable;  bstop^.enable;  bpause^.enable;  bff^.enable;  bfr^.enable;  bnext^.enable;  bprev^.enable;
    binfo^.enable; beff^.enable; bShufF^.enable; BRep^.enable; bScan^.enable;
    bopen^.enable; bdatei^.enable; btime^.enable;
    percnt^.enable; mtime^.enable; stime^.enable; track^.enable; tracks^.enable;
    pattern^.enable;  patterns^.enable; bits^.enable; sr^.enable; Pamp^.enable;
    btimelo^.enable; pswname^.enable; btntime^.enable; NumCh^.enable;
    pmodname^.enable;
  END ELSE
  BEGIN
    bplay^.disable; bstop^.disable; bpause^.disable; bff^.disable; bfr^.disable; bnext^.disable;
    bprev^.disable; binfo^.disable; beff^.disable; bopen^.disable;
    pmodname^.disable; percnt^.disable; mtime^.disable; stime^.disable; track^.disable;
    tracks^.disable; pattern^.disable; patterns^.disable; sr^.disable; Pamp^.disable; bits^.disable;
    btimelo^.disable; btime^.disable; pswname^.disable; NumCh^.disable;
    btntime^.disable; bdatei^.disable; bShufF^.disable; BRep^.disable; bScan^.disable;
  END;
END;

PROCEDURE tmod.setplaytime(sec:longint);
VAR time:Longint;
BEGIN
  IF sec>=0 THEN
  BEGIN
    IF minusmode THEN time:=((Longint(MODINFO^.TotalTime)+500) DIV 1000)-Longint(sec) ELSE time:=sec;
    IF Time>99*60+59 THEN
    BEGIN
      mtime^.setbmpzahl(99);
      stime^.setbmpzahl(99);
    END ELSE
    BEGIN
      mtime^.setbmpzahl(longint(time) DIV 60);
      stime^.setbmpzahl(longint(time) MOD 60);
    END;
  END;
END;

PROCEDURE tmod.DrawPrcntBar;
VAR BMDC,MemDC:hDC;
    hw:integer;
    rect:tRect;
BEGIN
  IF ABOUT THEN Exit;
  IF hWindow=0 THEN Exit;
  IF ADC=0 THEN bmdc:=GetDC(HWindow) ELSE bmdc:=ADC;
  memdc:=CreateCompatibleDC(bmdc);
  IF PaletteInstalled THEN
  BEGIN
    SelectPalette(bmdc,hPal,false);
    SelectPalette(memdc,hPal,false);
  END;
  SelectObject(memdc,hMainFrame);
  IF (IdleState.CPUUsage>0)THEN
  BEGIN
    IF IdleState.CPUUsage<100 THEN hw:=3*(h_PBAR*IdleState.CPUUsage DIV 300) ELSE hw:=h_PBAR;
  END ELSE hw:=0;
  rect.left:=x_PBAR;
  rect.right:=rect.Left+w_PBAR;
  rect.top:=y_PBAR+MDy;
  rect.bottom:=rect.Top+h_PBAR-hw;
  FillRect(bmdc,rect,BrBlack);
  BitBlt(bmdc,x_PBAR ,y_PBAR+MDy+h_Pbar-hw ,w_PBAR,hw,memdc ,x_pBAR,y_Pbar+h_pbar-hw,srccopy);
  DeleteDC(memdc);
  IF adc=0 THEN ReleaseDC(HWindow,bmdc);
END;

PROCEDURE tmod.DrawPreAmp;
CONST PreH=72;    {QuellBitmapHöhe}
      PreW=23;    {QuellBitmapBreite}
      Border=36;  {Nullpunkt im Bitmap}
      X_Pre=508;  {X-Offset im ZielBitmap}
      Y_pre=8;    {Y-Offset im ZielBitmap}
      Steps=1;    {Schrittweite für eine Einheit zB: LED}
      MedInput=20; {Mittelwert von PreAmp}
VAR BMDC,MemDC:hDC;
    hw:integer;
BEGIN
  IF ABOUT THEN Exit;
  IF hWindow=0 THEN Exit;
  IF ADC=0 THEN bmdc:=GetDC(HWindow) ELSE bmdc:=ADC;
  memdc:=CreateCompatibleDC(bmdc);
  IF PaletteInstalled THEN
  BEGIN
    SelectPalette(bmdc,hPal,false);
    SelectPalette(memdc,hPal,false);
  END;
  SelectObject(memdc,hPreAmp);
  hw:=Border+Steps*((PreH DIV 2-4)*(MedInput-(Werte.PreAmp AND $7f)) DIV MedInput DIV Steps); {0..85}
  IF hw<Border THEN
  BEGIN{oberer Zweig}
    BitBlt(bmdc,X_Pre,Y_pre+MDy       ,PreW,hw    ,memdc,PreW,     0,srccopy); {Oben  .. Aktiv}
    BitBlt(bmdc,X_Pre,Y_pre+MDy+Border,PreW,Border,memdc,PreW,Border,srccopy); {mitte .. unten}
    BitBlt(bmdc,X_Pre,Y_pre+MDy+hw,PreW,Border-hw,memdc,0,hw,srccopy); {Aktiv .. mitte}
  END ELSE
  BEGIN{unterer Zweig}
    BitBlt(bmdc,X_Pre,Y_pre+MDy   ,PreW,Border ,memdc,PreW, 0,srccopy); {Oben  .. Mitte}
    BitBlt(bmdc,X_Pre,Y_pre+MDy+hw,PreW,PreH-hw,memdc,PreW,hw,srccopy); {Aktiv .. unten}
    BitBlt(bmdc,X_Pre,Y_pre+MDy+Border,PreW,hw-Border,memdc,0,Border,srccopy); {Mitte .. Aktiv}
  END;
  {Regler}
  BitBlt(bmdc,X_Pre,Y_pre+MDy+hw-3,PreW,7,memdc,0,PreH,srccopy);
  SelectObject(memdc,minus_mode);
  IF (Werte.PreAmp AND $7f)>=MedInput
  THEN BitBlt(bmdc,509,mdy+88,4,2,memdc,0,2,srccopy)
  ELSE BitBlt(bmdc,509,mdy+88,4,2,memdc,0,0,srccopy);
  pAmp^.SetBMPZahl(Abs(Werte.PreAmp AND $7F-20));
  DeleteDC(memdc);
  IF adc=0 THEN ReleaseDC(HWindow,bmdc);
END;

PROCEDURE tmod.setmodname(Name:STRING; conv2ansi: boolean);
VAR s:STRING;
BEGIN
  pmodname^.setbmptext(Name);
  IF (psoundwindow(Parent)^.ministate)OR(IsIconic(HWindow)) THEN
  BEGIN
    IF dirlist^.songcount=0 THEN s:=longver ELSE s:=Name;
    s[Length(s)+1]:=#0;
    IF conv2ansi THEN OemToAnsi(@s[1], @s[1]);
    pWindow(Parent)^.SetCaption(@s[1]);
  END;
END;
{WMCOMMAND-HANDLER für MOD-FRAME   RETURN=FALSE wenn nicht verarbeitet}
FUNCTION tmod.idhandler;
BEGIN
  idhandler:=true;
  IF NOT dirlist^.entpacke THEN
  CASE msg.wparam OF
    id_about  :handlemodabout;
    id_swname :handleswname;
    id_stop   :handlestopbutton;
    id_play   :handlestartbutton(False);
    id_pause  :handlepausebutton;
    id_prev   :handlepreviousbutton;
    id_fr     :handlefrbutton;
    id_ff     :handleffbutton;
    id_next   :handlenextbutton;
    id_shuffle:handleshufflebutton;
    id_rep    :handlerepeatbutton;
    id_scan   :handleIntroscanbutton;
    id_open   :handleopenbutton;
    id_datei  :handledatei;
    id_info   :handleinfo;
    id_effects:handleeffects;
    id_btntime: IF minusmode THEN
                BEGIN
                  IF NOT ABOUT THEN
                  IF psoundwindow(Parent)^.ministate
                  THEN blit(HWindow,6,psoundwindow(Parent)^.capdy+25,4,2,0,2,minus_mode)
                  ELSE blit(HWindow,11,79,8,2,0,2,minus_mode);
                  minusmode:=false;
                  setplaytime(modinfo^.currtime DIV 1000);
                END ELSE
                IF dirlist^.songcount>0 THEN
                BEGIN
                  IF NOT ABOUT THEN
                  IF psoundwindow(Parent)^.ministate
                  THEN blit(HWindow,6,psoundwindow(Parent)^.capdy+25,4,2,0,0,minus_mode)
                  ELSE blit(HWindow,11,79,8,2,0,0,minus_mode);
                  minusmode:=true;
                  setplaytime(Longint(modinfo^.currtime) DIV 1000);
                END;
    id_delfromlist:handledelfilefromlist;
    id_menucopy:handlecopy;
    id_menuerase:handleerasefile;
    id_menumove:handlemove;
    ELSE idhandler:=false;
  END ELSE idhandler:=false;
END;

PROCEDURE tmod.switchQMode;
BEGIN
  IF werte.DeviceID < dev_HDD THEN Exit;
  IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
  WITH Werte DO
  BEGIN
    PreAmp := PreAmp XOR $80;
    MOD_SETTINGSCHANGED(werte,Error);
    IF NOT ABOUT THEN blit(HWindow,x_Qmix,mdy+y_Qmix,18, 9,26*(1-Byte(PreAmp)SHR 7),83,h_sh_rep_scan);
  END;
END;
PROCEDURE tmod.switchsurround;
BEGIN
  IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
  WITH Werte DO IF stereo=2 THEN
  BEGIN
    Surround:=NOT(Surround);
    IF NOT ABOUT THEN blit(HWindow,x_m_st,mdy+y_m_st,27,11,0,11*(stereo+Byte(Surround)-1),hm_st);
    MOD_SETTINGSCHANGED(werte,Error);
  END
END;
PROCEDURE tmod.switchBPM;
BEGIN
  IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
  WITH Werte DO
  BEGIN
    NoBPM:=NOT(NoBPM);
    MOD_SETTINGSCHANGED(werte,Error);
    IF NOT ABOUT THEN blit(HWindow,x_BPM,mdy+y_BPM,26,9,0,48+9*(1-Byte(NoBPM)AND 1),h_sh_rep_scan);
  END;
END;
PROCEDURE tmod.switchPAN;
BEGIN
  IF werte.DeviceID < dev_HDD THEN Exit;
  IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
  WITH Werte DO
  BEGIN
    Panning:=NOT(Panning);
    MOD_SETTINGSCHANGED(werte,Error);
    IF NOT ABOUT THEN blit(HWindow,x_PAN,mdy+y_PAN,23,9,26,48+9*(Byte(Panning)AND 1),h_sh_rep_scan);
  END;
END;
PROCEDURE tmod.switchIDO;
BEGIN
  IF werte.DeviceID < dev_HDD THEN Exit;
  IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
  WITH Werte DO
  BEGIN
    Oversamp:=(Oversamp+1)MOD 4;
    MOD_SETTINGSCHANGED(werte,Error);
    IF NOT ABOUT THEN blit(HWindow,x_IDO,mdy+y_IDO,26,17,26*OverSamp,66,h_sh_rep_scan);
  END;
END;
PROCEDURE tmod.switchTMode;
BEGIN
  IF werte.DeviceID <= dev_HDD THEN Exit;
  WITH Werte DO
  BEGIN
    tmode:=(tmode+1)MOD 3;
    MOD_SETTINGSCHANGED(werte,Error);
    IF NOT ABOUT THEN blit(HWindow,x_tmode,mdy+y_tmode,23,9,0,9*(tmode),htmode);
  END;
END;

{WMKEYDOWN-HANDLER}
PROCEDURE tMod.ClickButton(What:pBMPButton;lparam:Longint);
BEGIN
  IF((Hi(HiWord(lparam))AND 64)<>64)AND(NOT What^.Disabled)AND(What^.BMP<>0) THEN What^.focus;
  IF LoWord(lParam)=1 THEN
  BEGIN
    IF (What^.Disabled AND psoundwindow(Parent)^.ministate) OR IsIconic (HWindow) OR (What^.BMP=0)
      THEN PostMessage(HWindow,Wm_Command,what^.GetID,what^.HWindow)
      ELSE SendMessage(What^.HWindow,wm_keydown,vk_return,0);
  END;
END;

FUNCTION tmod.kbdhandler;
BEGIN
  kbdhandler:=False;
  IF NOT(about)THEN
  BEGIN
    CASE iskey(msg.wparam) OF
      0:IF(msg.wparam IN[$30..$39,$60..$69])AND(EffWnd<>0)THEN
        BEGIN
          msg.wparam:=(msg.wparam-$30); IF msg.wparam>10 THEN msg.wparam:=(msg.wparam-$30);
          IF msg.wparam=0 THEN msg.wparam:=10;
          CASE KeyState OF
            0:;
            2:Inc(msg.wparam,10);
            1:Inc(msg.wparam,20);
            3:Inc(msg.wparam,30);
            ELSE msg.wparam:=99;
          END;
          IF msg.wparam<=modinfo^.numtracks THEN MOD_EnableChn(msg.wparam,NOT(Mod_IsEnabled(msg.wparam)))
          ELSE Exit;
        END ELSE Exit;
      1:ClickButton(bDatei,msg.lParam);
      2:ClickButton(bPause,msg.lParam);
      3:ClickButton(bPlay,msg.lParam);
      {4:About}
      5:ClickButton(bOpen,msg.lParam);
      6:ClickButton(pSoundWindow(Parent)^.bSetup,msg.lParam);
      7:ClickButton(bprev,msg.lParam);
      8:ClickButton(bnext,msg.lParam);
      9:ClickButton(bff,msg.lParam);
     10:ClickButton(bfr,msg.lParam);
     11:IF(Hi(msg.lparamhi)AND 64)<>64 THEN bprev^.focus;
     12:IF(Hi(msg.lparamhi)AND 64)<>64 THEN bnext^.focus;
     13:ClickButton(binfo,msg.lParam);
     14:ClickButton(bstop,msg.lParam);
     15:ClickButton(bshuff,msg.lParam);
     16:ClickButton(brep,msg.lParam);
     17:ClickButton(bscan,msg.lParam);
     18:IF NOT dirlist^.entpacke THEN handlecopy;
     19:IF NOT dirlist^.entpacke THEN handlemove;
     20:IF NOT dirlist^.entpacke THEN handleerasefile;
     21:ClickButton(btntime,msg.lParam);
     22:ClickButton(pSoundWindow(Parent)^.bmixer,msg.lParam);
     23:ClickButton(beff,msg.lParam);
     24..25,36..39,42..43:IF detailwnd<>0 THEN SendMessage(detailwnd,msg.message,msg.wparam,msg.lparam);
     26:IF pswname^.GetPos <> 1 THEN BEGIN pswname^.setpos(1); handleswname; END;
     27:IF pswname^.GetPos <> 2 THEN BEGIN pswname^.setpos(2); handleswname; END;
     28:ClickButton(pSoundWindow(Parent)^.bhelp,msg.lParam);
     29:switchqmode;
     30:switchbpm;
     31:WITH Werte DO IF stereo=2 THEN switchsurround ELSE Exit;
     32:switchido;
     33:WITH Werte DO
        BEGIN
          IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
          IF PreAmp AND $7F<40 THEN Inc(PreAmp);
          MOD_SETTINGSCHANGED(werte,Error);
          DrawPreAmp(0);
        END;
     34:WITH Werte DO
        BEGIN
          IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
          IF PreAmp AND $7F>0 THEN Dec(PreAmp);
          MOD_SETTINGSCHANGED(werte,Error);
          DrawPreAmp(0);
        END;
     35:switchpan;
     40:IF NOT dirlist^.entpacke THEN handledelfilefromlist;
     41:IF NOT dirlist^.entpacke THEN handleselsong;
    END;
    kbdhandler:=true;
  END;
END;

FUNCTION tmod.wmkeyup;
 PROCEDURE HandleButton(What:pBMPButton);
 BEGIN
   IF NOT What^.DisAbled THEN SendMessage(What^.HWindow,wm_keyup,vk_return,0);
 END;
BEGIN
  wmkeyup:=false;
  IF about THEN handlemodabout ELSE
  BEGIN
    CASE iskey(msg.wparam) OF
      0:Exit;
      1:HandleButton(bDatei);
      2:HandleButton(bPause);
      3:HandleButton(bPlay);
      4:IF NOT psoundwindow(Parent)^.ministate THEN handlemodabout ELSE Exit;
      5:HandleButton(bOpen);
      6:HandleButton(pSoundWindow(Parent)^.bSetup);
      7:HandleButton(bprev);
      8:HandleButton(bnext);
      9:HandleButton(bff);
     10:HandleButton(bfr);
     11:IF NOT dirlist^.entpacke THEN handlehome;
     12:IF NOT dirlist^.entpacke THEN handleend;
     13:HandleButton(binfo);
     14:HandleButton(bstop);
     15:HandleButton(bshuff);
     16:HandleButton(brep);
     17:HandleButton(bscan);
     21:HandleButton(btntime);
     22:HandleButton(pSoundWindow(Parent)^.bmixer);
     23:HandleButton(beff);
     {24,25 Detail-Dialog...}
     {26,27: NameSwitch}
     28:HandleButton(pSoundWindow(Parent)^.bhelp);
    END;
    wmkeyup:=true;
  END;
END;

PROCEDURE tmod.handledatei;
VAR  menu:hmenu;
     sm1,sm2,sm3,sm4:pchar;
BEGIN
  menu:=CreatePopupMenu;
  gl_str:=getresstring(2000)+'|'+Key2String(mykeys[18])+#0; sm1:=strnew(@gl_str[1]);
  gl_str:=getresstring(2001)+'|'+Key2String(mykeys[19])+#0; sm2:=strnew(@gl_str[1]);
  gl_str:=getresstring(1999)+'|'+Key2String(mykeys[40])+#0; sm3:=strnew(@gl_str[1]);
  gl_str:=getresstring(2002)+'|'+Key2String(mykeys[20])+#0; sm4:=strnew(@gl_str[1]);
  AppendMenu(menu,mf_enabled OR mf_ownerdraw,id_menucopy,sm1);
  AppendMenu(menu,mf_enabled OR mf_ownerdraw,id_menumove,sm2);
  AppendMenu(menu,mf_enabled OR mf_ownerdraw,id_delfromlist,sm3);
  AppendMenu(menu,mf_enabled OR mf_ownerdraw,id_menuerase,sm4);
  SetTimer(HWindow,-1,100,NIL);
  TrackPopupMenu(menu,tpm_leftalign OR tpm_leftbutton,pwindow(Parent)^.Attr.x+bdatei^.Attr.x,
                 pwindow(Parent)^.Attr.y+bdatei^.Attr.y+bdatei^.Attr.h,0,HWindow,NIL);
  KillTimer(HWindow,-1);
  DestroyMenu(menu);
  strdispose(sm1);
  strdispose(sm2);
  strdispose(sm3);
  strdispose(sm4);
END;

PROCEDURE tmod.handleSelSong;
BEGIN
  IF dirlist^.songcount>0 THEN
   Application^.MakeWindow(New(pviewwin,Init(Parent,NIL,dirlist,numberlist[playlistcurrent])));
END;

PROCEDURE tMOD.RemoveCurrent;
VAR I:Integer;
BEGIN
  FOR i:=1 TO dirlist^.songcount+1 DO IF (numberlist[i]>numberlist[playlistcurrent])THEN Dec(numberlist[i]);
  move(NumberList[PlayListCurrent+1],NumberList[PlayListCurrent],2*(MaxPlayListEntries-PlayListCurrent-1));
  updatemodwindow;
END;

PROCEDURE tmod.handledelfilefromlist;
VAR I:Integer;
BEGIN
  IF dirlist^.songcount>0 THEN
  IF Ourmessagebox(Parent,modc.shortver+getresstring(28),getresstring(27),mb_yesno OR mb_iconquestion)=id_yes THEN
  BEGIN
    dirlist^.deletesongstring(numberlist[playlistcurrent]);
    RemoveCurrent;
  END;
END;

PROCEDURE tmod.handleerasefile;
VAR I:Integer;
BEGIN
  IF dirlist^.songcount>0 THEN
  IF dirlist^.erasefile(Parent,numberlist[playlistcurrent],true) THEN RemoveCurrent;
END;

PROCEDURE tmod.handlecopy;
BEGIN
  IF IsWindow(copywindow) THEN SetFocus(copywindow) ELSE
  IF dirlist^.songcount>0 THEN
  BEGIN
    pcopy:=New(pcopywindow,Init(Parent,dirlist,numberlist[playlistcurrent],fm_copy,@copywindow));
    Application^.MakeWindow(pcopy);
    copywindow:=pcopy^.HWindow;
  END;
END;

PROCEDURE tmod.handlemove;
BEGIN
  IF IsWindow(copywindow) THEN SetFocus(copywindow) ELSE
  IF dirlist^.songcount>0 THEN
  BEGIN
    pcopy:=New(pcopywindow,Init(Parent,dirlist,numberlist[playlistcurrent],fm_move,@copywindow));
    Application^.MakeWindow(pcopy);
    copywindow:=pcopy^.HWindow;
  END;
END;


FUNCTION tmod.getcorrectfilename;
VAR info_error:word;
BEGIN
  modinfo^.totaltime:=0; modinfo^.currtime:=0; setplaytime(0);
  IF songstate.New THEN patterns^.setbmpzahl(0);
  getcorrectfilename:=false;
  Name:=dirlist^.getfullname(Parent,numberlist[playlistcurrent],setplay,true);
  Name[Length(Name)+1]:=#0;
  IF (GetActiveWindow=hWindow)AND(NOT IsIconic(hWindow)) THEN SetCursor(LoadCursor(0,idc_arrow));
  IF DoPlay THEN
  BEGIN
    getcorrectfilename:=true;
    songstate.New     :=false;
    songstate.loadres :=0;
  END ELSE
  BEGIN
    IF Name<>getresstring(id_archivernotfound) THEN
    BEGIN
      songstate.New :=false;
      info_error    :=mod_Play(ord (EffWnd<>0) * $FFFF, @Name[1], NIL, False ,@Werte,true,(repeatmode=2));
      songstate.loadres:=info_error;
      CASE info_error OF
        0              : getcorrectfilename:=true;
        id_FileNotFound: BEGIN LogError(Name,id_NixFile);       setmodname(getresstring(id_NixFile),false); END;
        id_NoModFile   : BEGIN LogError(Name,id_FileCorrupted); setmodname(getresstring(id_FileCorrupted),false); END;
        id_InvModFile  : BEGIN LogError(Name,id_FileUnsupport); setmodname(getresstring(id_FileUnsupport),false); END;
      END;
      IF info_error<>0 THEN
      BEGIN
        modinfo^.numpatterns:=0;
        SetTimer(HWindow,2000,PlayErrorDelay,NIL);
      END else SetPlayTime (0);  { in case we're in minus mode and now we know the total time }
    END ELSE setmodname(dirlist^.getname(numberlist[playlistcurrent]),true);
  END;
END;

PROCEDURE tmod.updatemodwindow;
BEGIN
  IF dirlist^.songcount>0 THEN
  BEGIN
    IF playlistcurrent<>1 THEN
    IF dirlist^.songcount<=playlistcurrent THEN playlistcurrent:=dirlist^.songcount;
    dirlist^.newlist:=true;
    track^.setbmpzahl(longint(numberlist[playlistcurrent]+1));
    IF PlayOnLoad OR playing THEN
    BEGIN
      handlestopbutton;
      handlestartbutton(False);
    END ELSE
    BEGIN
      lastnameindex:=$FFFF;
      getnewmodname;
    END;
  END ELSE
  BEGIN
    handlestopbutton;
    playlistcurrent:=0;
    track^.setbmpzahl(0);
    patterns^.setbmpzahl(0);
    setmodname('',false);
    modinfo^.totaltime:=0; modinfo^.currtime:=0;
    FillChar(ModInfo^,SizeOf(ModInfo^),0);
    IF detailWND<>0 THEN PostMessage(detailwnd,wm_newfiletoplay,0,0);
  END;
  tracks^.setbmpzahl(longint(dirlist^.songcount));
END;

PROCEDURE tmod.handleswname;
VAR s:STRING;
BEGIN
 IF dirlist^.songcount>0 THEN
 BEGIN
   track^.setbmpzahl(longint(numberlist[playlistcurrent]+1));
   IF (songstate.New)AND(cmdtimer=0)AND((pswname^.GetPos<>1)OR(playing)OR(detailwnd<>0)or(effwnd<>0))
      THEN getcorrectfilename(s,NOT marksong,false);
   IF detailwnd<>0 THEN
   BEGIN
     IF (cmdtimer=0)AND(numberlist[playlistcurrent]<>lastnameindex)
     THEN PostMessage(detailwnd,wm_newfiletoplay,0,0);
   END;
   IF (pswname^.GetPos=1)OR(cmdtimer<>0)
     THEN setmodname(dirlist^.getname(numberlist[playlistcurrent]),true)
     ELSE IF songstate.loadres=0 THEN setmodname(MODInfo^.modname,true);
   IF cmdtimer=0 THEN lastnameindex:=numberlist[playlistcurrent];
 END ELSE SetModName ('', false);
END;

PROCEDURE tmod.handlestopbutton;
BEGIN
  Introscan:=false;
  TryToPlay:=False;
  IF playing THEN
  BEGIN
    MOD_STOP;
    playing:=false;
    pause:=false;
  END;
END;

PROCEDURE tmod.handlestartbutton (PauseIt: boolean);
VAR result:word;
    s,RecordFilename:STRING[150];
    msg:tmessage;
    P:PwindowsObject;
    Canceled:boolean;
LABEL NoTimer;
BEGIN
  IF cmdtimer<>0 THEN
  BEGIN
    KillTimer(HWindow,cmdtimer);
    cmdtimer:=0;
    HandleStopButton;
  END;
  IF (dirlist^.songcount>0) THEN
   IF NOT(playing) THEN
   BEGIN
     TryToPlay:=true;
     MODINFO^.TotalTime:=0;
     pattern^.setbmpzahl(0);
     IF getcorrectfilename(s,marksong,true) THEN
     BEGIN
       IF Werte.DeviceID = dev_HDD THEN
       BEGIN
         RecordFileName:=PrevDir(S); {Filename aus Pfad nehmen}
         Result:=Pos('.',RecordFilename);
         IF Result<>0 THEN Delete(RecordFilename,Result,Length(RecordFileName)-Result+1);
         IF AskHD THEN{nach Save as-Filenamen fragen}
         BEGIN
           P:=Application^.MakeWindow(New(PaskWin,Init(Parent,@HDPath,@RecordFileName,@Canceled)));
           PAskWin(P)^.MakeModal;
           IF Canceled THEN BEGIN HandleStopButton; Exit; END;
           {writeprivateprofilestring(s_state,'HDPath',@HDpath,INIDIR);}
         END;
         IF HDPath[StrLen(@HDPath)-1]<>'\' THEN StrCat(@HdPath,'\');
         RecordFileName:=StrPas(HDPath)+RecordFileName+'.WAV';
         RecordFileName[Length(RecordFileName)+1]:=#0;
       END;
       S[Length(S)+1]:=#0;
       result:=mod_Play(HWindow,@s[1],@RecordFilename[1], False, @werte, PauseIt, (repeatmode=2));
     END ELSE
     BEGIN
       IF detailwnd<>0 THEN SendMessage(detailwnd,wm_newfiletoplay,0,0);
       Exit;
     END;
     IF detailwnd<>0 THEN SendMessage(detailwnd,wm_newfiletoplay,0,0);
     IF result = 0 THEN
     BEGIN
       GetSettings(True,True);
       TryToPlay:=false;
       playing:=true;
       pause:=PauseIt;
       idletime:=0;
       FillChar(IdleState,SizeOf(IdleState),$ff);
       handleswname;
       {$IFDEF Share}
       IF (NOT Registered)AND(DaysUsed>30)AND(SongsPlayed>0) THEN BEGIN
         HandleStopButton;
         PostMessage(HWindow,wm_close,0,0);
       END;
       Inc(SongsPlayed);
       {$ENDIF}
       IF Load_state THEN savestate;
     END ELSE
     BEGIN
       track^.setbmpzahl(longint(numberlist[playlistcurrent]+1));
       CASE result OF
         id_FileNotFound: BEGIN LogError(s,id_NixFile);       setmodname(getresstring(id_NixFile),false); END;
         id_NoModFile   : BEGIN LogError(s,id_FileCorrupted); setmodname(getresstring(id_FileCorrupted),false); END;
         id_InvModFile  : BEGIN LogError(s,id_FileUnsupport); setmodname(getresstring(id_FileUnsupport),false); END;
         ELSE
         BEGIN
           handleswname;
           TryToPlay:=false;
           IF IntroScan THEN HandleIntroScanButton;
           LOG_ErrMessageBox(s,Parent,result);
           GOTO NoTimer;
         END;
       END;
       SetTimer(HWindow,2000,PlayErrorDelay,NIL);
Notimer:
     END;
   END
   ELSE IF pause THEN handlepausebutton
   ELSE IF IntroScan THEN handleIntroScanbutton;
END;

PROCEDURE tmod.handlepausebutton;
BEGIN
  IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
  IF playing THEN
  BEGIN
    IF pause THEN BEGIN pause:=false; MOD_endpause(True); END ELSE
    BEGIN
       pause:=true;
       MOD_Pause(True);
       ModInfo^.CurrPttrn:=mod_GetTimePattern(ModInfo^.currtime);
       Pattern^.SetBMPZahl(ModInfo^.CurrPttrn);
    END;
  END ELSE Handlestartbutton(true);
END;

PROCEDURE tmod.handlehome;
BEGIN
  IF dirlist^.songcount>1 THEN
  BEGIN
    IF cmdtimer<>0 THEN KillTimer(HWindow,CmdTimer);
    playlistcurrent:=0;
    handlenextbutton;
  END;
END;

PROCEDURE tmod.handleend;
BEGIN
  IF dirlist^.songcount>1 THEN
  BEGIN
    IF cmdtimer<>0 THEN KillTimer(HWindow,CmdTimer);
    playlistcurrent:=dirlist^.songcount-1;
    handlenextbutton;
  END;
END;

PROCEDURE tmod.handlepreviousbutton;
BEGIN
  IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
  { if there's no prev song go away }
  if (PlayListCurrent = 1)
  and(RepeatMode <> 1)
  and(ModInfo^.CurrTime < BackTrackDelay)
  and(CmdTimer = 0) then
  BEGIN
    exit;{ }
  END;
  IF cmdtimer<>0 THEN KillTimer(HWindow,CmdTimer);
  if (ModInfo^.CurrTime > BackTrackDelay) and (CmdTimer = 0) then
  begin
    CmdTimer := SetTimer (HWindow, 1995, NextPrevDelay, nil);
    exit;
  end;  { go to start of song }
  if PlayListCurrent > 1 then dec (PlayListCurrent) else                        { go to previous song }
  if RepeatMode = 1 then begin
    if Shuffle then begin HandleShuffleButton; HandleShuffleButton; end;
    PlayListCurrent := DirList^.SongCount;
  end;
  cmdtimer:=SetTimer(HWindow,1993,NextPrevDelay,NIL);
  lastnameindex:=$FFFF;  { force getnewmodname }
  getnewmodname;
END;

PROCEDURE tmod.handlenextbutton;
BEGIN
  IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
  { if there's no next song go away }
  if (PlayListCurrent = DirList^.SongCount) and (RepeatMode <> 1) and (CmdTimer = 0) then exit;
  IF cmdtimer<>0 THEN KillTimer(HWindow,CmdTimer);
  if PlayListCurrent < DirList^.SongCount then inc (PlayListCurrent) else
  if RepeatMode = 1 then begin
    PlayListCurrent := 1;
    if Shuffle then begin HandleShuffleButton; HandleShuffleButton; end;
  end;
  cmdtimer:=SetTimer(HWindow,1993,NextPrevDelay,NIL);
  lastnameindex:=$FFFF;  { force getnewmodname }
  getnewmodname;
END;

PROCEDURE tmod.handleshufflebutton;
VAR i: integer;
  PROCEDURE FastWordSwap (VAR a, b: word); assembler;
  asm
    les  si, a; mov di,word Ptr b;
    mov ax,word Ptr es:[si]; mov bx,word Ptr es:[di]
    mov word Ptr es:[si],bx; mov word Ptr es:[di],ax
  END;
BEGIN
  Shuffle:=NOT(Shuffle);
  IF (dirlist^.songcount>2)AND shuffle THEN
  BEGIN
    Randomize;
    FastWordSwap (numberlist[1], numberlist[playlistcurrent]);
    playlistcurrent:=1;
    FOR i:=2 TO dirlist^.songcount-1 DO
        FastWordSwap(numberlist[i],numberlist[i+Random(dirlist^.songcount-i+1)]);
  END ELSE
  IF dirlist^.songcount>0 THEN
  BEGIN
    playlistcurrent:=numberlist[playlistcurrent]+1;
    FOR i:=1 TO dirlist^.songcount DO numberlist[i]:=i-1;
    track^.setbmpzahl(longint(numberlist[playlistcurrent+1]));
  END;
END;

PROCEDURE tmod.handlerepeatbutton;
BEGIN
  IF repeatmode=2 THEN MOD_repeat(false);
  repeatmode:=(repeatmode+1)MOD 3;
  IF repeatmode=2 THEN MOD_repeat(true);
END;

PROCEDURE tmod.handleIntroscanbutton;
BEGIN
  IF dirlist^.songcount<2 THEN Exit;
  IF IntroScan AND Playing THEN bPlay^.SetStyle(_on) ELSE IdleState.NextBlinkTime:=TimeGetTime+BlinkTime;
  introscan:=NOT(introscan);
  IF introscan AND NOT(Playing)THEN handlestartbutton(False);
END;

PROCEDURE tmod.modtimerproc;
BEGIN
  KillTimer(HWindow,timerid);
  IF timerid=2000 THEN
  BEGIN
    IF TryToPlay THEN
    BEGIN
      IF playlistcurrent<dirlist^.songcount THEN
      BEGIN
        IF cmdtimer<>0 THEN KillTimer(HWindow,CmdTimer);
        inc(playlistcurrent);
        cmdtimer:=SetTimer(HWindow,1993,NextPrevDelay,NIL);
      END ELSE IF repeatmode=1 THEN
      BEGIN
        IF cmdtimer<>0 THEN KillTimer(HWindow,CmdTimer);
        playlistcurrent:=1;
        IF shuffle THEN BEGIN handleshufflebutton; handleshufflebutton; END;
        cmdtimer:=SetTimer(HWindow,1993,NextPrevDelay,NIL);
      END;
      track^.setbmpzahl(longint(numberlist[playlistcurrent]+1));
    END;
    setmodname(dirlist^.getname(numberlist[playlistcurrent]),true);
    Exit;
  END;
  IF TimerID<>0 THEN cmdtimer:=0;
  case TimerID of
    1993: begin
      lastnameindex:=$FFFF;
      modinfo^.numpatterns:=0;
      IF playing OR TryToPlay THEN
      BEGIN
        mod_stop;
        playing:=false;
        IF pause THEN
        BEGIN
          pause:=false; handlepausebutton;
        END ELSE handlestartbutton(False);
      END ELSE
      BEGIN
        modinfo^.totaltime:=0;
        modinfo^.currtime:=0;
        idlestate.playtime:=-1;
        getnewmodname;
      END;
    END;
    1995: begin
      MOD_Pause(False);
      ModInfo^.currtime:=0;
      IF NOT Pause THEN MOD_ENDpause(False) ELSE MOD_Pause(True);
    end;
  end;
END;

PROCEDURE tmod.getnewmodname;
BEGIN
  IF lastnameindex<>numberlist[playlistcurrent] THEN
  BEGIN
    songstate.New:=true; songstate.loadres:=-1000;
    patterns^.setbmpzahl(0);
    handleswname;
  END;
END;

PROCEDURE tmod.handlefrbutton;
BEGIN
  IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
  IF NOT(playing)THEN HandlePauseButton;
  IF playing AND (spulff=0) THEN
  BEGIN
    IF (spulfr=0) THEN
    BEGIN
      Spulfr:=TimeGetTime;
      MOD_Pause(False);
    END;
    IF NOT IntroScan THEN
    BEGIN
      CASE TimeGetTime-SpulFr OF
           0..1000:DEC(ModInfo^.currtime,1000);
        1001..2000:DEC(ModInfo^.currtime,1500);
        2001..3000:DEC(ModInfo^.currtime,2000);
        3001..4000:DEC(ModInfo^.currtime,3000);
        4001..5000:DEC(ModInfo^.currtime,4000);
        ELSE DEC(ModInfo^.currtime,5000);
      END;
    END ELSE DEC(ModInfo^.currtime,1000);
    IF (ModINFO^.CurrTime<=0)THEN
    BEGIN
      IF (PLAYLISTCurrent>1)OR(REPEATMODE=1) THEN
      BEGIN
        IF PLAYLISTCurrent>1 THEN DEC(playlistcurrent) ELSE Playlistcurrent:=Dirlist^.songcount;
        modtimerproc(1993);
        mod_pause(false);
        IF Introscan THEN modinfo^.currtime:=IntroScanTime ELSE modinfo^.currtime:=MODinfo^.TotalTime;
        dec (modinfo^.currtime, 1000);
      END ELSE ModINFO^.CurrTime:=0;
    END;
    setplaytime(ModInfo^.currtime DIV 1000);
    ModInfo^.CurrPttrn:=mod_GetTimePattern(ModInfo^.currtime);
    Pattern^.SetBMPZahl(ModInfo^.CurrPttrn);
  END;
END;

PROCEDURE tmod.NextPattern;
BEGIN
  PatternSearch(ModInfo^.CurrPttrn+1);
  setplaytime(ModInfo^.currtime DIV 1000);
  Pattern^.SetBMPZahl(ModInfo^.CurrPttrn);
END;

PROCEDURE tmod.PrevPattern;
BEGIN
  IF ModInfo^.CurrPttrn>1 THEN
  BEGIN
    PatternSearch(ModInfo^.CurrPttrn-1);
    setplaytime(ModInfo^.currtime DIV 1000);
    Pattern^.SetBMPZahl(ModInfo^.CurrPttrn);
  END;
END;

PROCEDURE tmod.handleffbutton;
BEGIN
  IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
  IF NOT(playing)THEN HandlePauseButton;
  IF playing AND (spulfr=0) THEN
  BEGIN
    IF (spulff=0) THEN
    BEGIN
      Spulff:=TimeGetTime;
      MOD_Pause(False);
    END;
    IF NOT IntroScan THEN
    BEGIN
      CASE TimeGetTime-SpulFf OF
           0..1000:Inc(ModInfo^.currtime,1000);
        1001..2000:Inc(ModInfo^.currtime,1500);
        2001..3000:Inc(ModInfo^.currtime,2000);
        3001..4000:Inc(ModInfo^.currtime,3000);
        4001..5000:Inc(ModInfo^.currtime,4000);
        ELSE Inc(ModInfo^.currtime,5000);
      END;
    END ELSE Inc(ModInfo^.currtime,1000);
    IF (ModINFO^.CurrTime>=MODINFO^.TotalTime)OR(IntroScan AND(ModINFO^.CurrTime>IntroScanTime))THEN
    BEGIN
      IF(PLAYLISTCurrent<dirlist^.songcount)OR(REPEATMODE=1)THEN
      BEGIN
        IF PLAYLISTCurrent<dirlist^.songcount THEN INC(playlistcurrent)ELSE Playlistcurrent:=1;
        modtimerproc(1993);
        MOD_Pause(False);
      END ELSE ModINFO^.CurrTime:=MODINFO^.TotalTime;
    END;
    setplaytime(ModInfo^.currtime DIV 1000);
    ModInfo^.CurrPttrn:=mod_GetTimePattern(ModInfo^.currtime);
    Pattern^.SetBMPZahl(ModInfo^.CurrPttrn);
  END;
END;

FUNCTION tmod.idleaction;
BEGIN
  IF Introscan AND(IdleTime>IdleState.NextBlinkTime)THEN
  BEGIN
    IdleState.NextBlinkTime:=TimeGetTime+BlinkTime;
    bPlay^.SetStyle(Byte(NOT(bPlay^.active)));
  END;
  IF Playing
   AND( (IntroScan AND(ModInfo^.CurrTime > IntroScanTime)) OR (NOT MODINFO^.playing) )
   AND(SpulFF+SpulFr=0) and (cmdtimer = 0)
  THEN handleplayend;
  IF NOT ABOUT THEN
  BEGIN
    {$IFDEF debugging}
    with bPause^ do if cmdtimer <> 0 then SetStyle (_on) else SetStyle (off);
    {$ENDIF}
    {Play-Stop-Pause-Btns...}
    IF Idlestate.Playing<>Playing THEN
    BEGIN
      IdleState.Playing:=Playing;
      IF Playing
      THEN BEGIN bPlay^.SetStyle(_on); bStop^.Setstyle(OFF); END
      ELSE BEGIN bPLay^.SetStyle(OFF); bStop^.Setstyle(_on); END;
    END;
    IF IdleState.Pause<>Pause THEN
    BEGIN
      IdleState.Pause:=Pause;
      IF Pause THEN bPause^.SetStyle(_on) ELSE bPause^.SetStyle(OFF);
    END;
    {Shuffle-Repeat-Introscan-zeigen...}
    IF Idlestate.Introscan<>Introscan THEN
    BEGIN
      Idlestate.Introscan:=Introscan;
      IF Introscan
      THEN blit(HWindow,315,mdy+65,44,8,0,40,h_sh_rep_scan) {Introscan}
      ELSE blit(HWindow,315,mdy+65,44,8,0,16,h_sh_rep_scan); {remove Introscan}
    END;
    IF IdleState.RepeatMode<>RepeatMode THEN
    BEGIN
      IdleState.RepeatMode:=RepeatMode;
      CASE RepeatMode OF
       0:blit(HWindow,315,mdy+54,39,8,0,8,h_sh_rep_scan);
       1:BEGIN
           blit(HWindow,315,mdy+54,32,8, 0,32,h_sh_rep_scan); {Set Repeat all - repeat bold}
           blit(HWindow,348,mdy+54, 8,8,33,8,h_sh_rep_scan); {Set Repeat all - 1 normal}
         END;
       2:blit(HWindow,315,mdy+54,39,8,0,32,h_sh_rep_scan);
       END;
    END;
    IF IdleState.Shuffle<>Shuffle THEN
    BEGIN
      IdleState.Shuffle:=Shuffle;
      IF Shuffle
      THEN blit(HWindow,315,mdy+43,32,8,0,24,h_sh_rep_scan) {Shuffle}
      ELSE blit(HWindow,315,mdy+43,32,8,0,0,h_sh_rep_scan); {remove Shuffle}
    END;
    {Min/Sec anzeigen}
    IF (IdleState.playtime<>modinfo^.currtime DIV 1000) THEN
    BEGIN
      IdleState.playtime:=modinfo^.currtime DIV 1000;
      setplaytime(IdleState.playtime);
      IdleState.LastDriverOkTime:=IdleTime;
    END;
    {Pattern}
    IF IdleState.pattern<>modinfo^.currpttrn THEN
    BEGIN
      IdleState.pattern:=modinfo^.currpttrn;
      pattern^.setbmpzahl(IdleState.pattern);
    END;
    {TotalPatterns}
    IF IdleState.patterns<>modinfo^.numpatterns THEN
    BEGIN
      IdleState.patterns:=modinfo^.numpatterns;
      patterns^.setbmpzahl(IdleState.patterns);
    END;
    {Channels}
    IF IdleState.NumChn<>modinfo^.NumTracks THEN
    BEGIN
      IdleState.NumChn:=modinfo^.NumTracks;
      NumCh^.setbmpzahl(IdleState.NumChn);
    END;
    {CPU-Usage}
    IF IdleState.cpuusage<>(modinfo^.cpuusage)DIV 1000 THEN
    BEGIN
      IF 3*(modinfo^.cpuusage DIV 3000)<>IdleState.cpuusage THEN
      BEGIN
        IdleState.cpuusage:=modinfo^.cpuusage DIV 1000;
        DrawPrcntBar(0);
      END ELSE IdleState.cpuusage:=modinfo^.cpuusage DIV 1000;
      percnt^.setbmpzahl(IdleState.cpuusage);
    END;
    {BufferZeit}
    IF IdleState.bufftime<>modinfo^.buftimelen THEN
    BEGIN
      IdleState.bufftime:=modinfo^.buftimelen;
      btimelo^.setbmpzahl((IdleState.bufftime DIV 100) MOD 10);
      btime^.setbmpzahl(IdleState.bufftime DIV 1000);
    END;
    {IF NOT ABOUT}
  END;
  IF ( ((Spulff>0)AND(NOT (bFF^.Clicked OR bFF^.KeyClick))) OR
       ((Spulfr>0)AND(NOT (bFr^.Clicked OR bFR^.KeyClick))) )AND(playing) THEN
  BEGIN
    Spulff:=0; Spulfr:=0;
    IF PAUSE THEN
    BEGIN
      MOD_ENDpause(True);
      MOD_Pause(True);
    END ELSE MOD_ENDpause(False);
  END;
  IF (effwnd<>0) THEN peffect^.processdata(idletime);
  IF (Werte.tMode=0) THEN mod_Timer;
  IdleAction:=(ModInfo^.Playing)AND(effwnd<>0)OR About;
  idletime:=TIMEGETTIME;
END;

PROCEDURE tmod.handleplayend;
LABEL Normal;
BEGIN
  IF opendlg<>NIL THEN handlestopbutton ELSE IF playlistcurrent<dirlist^.songcount THEN
  BEGIN
     inc(playlistcurrent);
Normal:
     mod_Stop;
     playing:=false;
     patterns^.setbmpzahl(0);
     FillChar(ModInfo^,SizeOf(ModInfo^),0);
     IF detailWND<>0 THEN SendMessage(detailwnd,wm_newfiletoplay,0,0);
     HandleSwName;
     IF pause THEN  BEGIN pause:=false; handlepausebutton; END ELSE handlestartbutton(False);
  END ELSE IF repeatmode=1 THEN
  BEGIN
     playlistcurrent:=1;
     IF shuffle THEN BEGIN handleshufflebutton; handleshufflebutton; END;
     GOTO Normal
  END ELSE handlestopbutton;
END;

PROCEDURE tmod.handleinfo;
{Öffnet Detaildialog für MODS}
VAR s:STRING;
    pdet:pdetail;
BEGIN
  IF detailwnd=0 THEN
  BEGIN
    IF (NOT dirlist^.entpacke) THEN
    IF (songstate.New) THEN getcorrectfilename(s,NOT marksong,false);
    pdet:=New(pdetail,Init(Parent,MODinfo));
    Application^.MakeWindow(pdet);
    detailwnd:=pdet^.HWindow;
    handleswname;
  END ELSE SendMessage(detailwnd,wm_close,0,0);
END;

PROCEDURE tmod.handleeffects;
VAR s:STRING;
BEGIN
  IF EFFWND=0 THEN
  BEGIN
    BEGIN
      peffect:=New(peff,Init(Parent,modinfo));
      Application^.MakeWindow(peffect);
    END;
    IF (NOT dirlist^.entpacke) THEN
    IF ((songstate.New)or(ModInfo^.TotalTime=0))AND(DirList^.SongCount>0) THEN getcorrectfilename(s,NOT MarkSong,False);
    IF  songstate.loadres=0 THEN HandleSwName;
  END ELSE SendMessage(effwnd,wm_close,0,0);
END;

PROCEDURE tmod.handlesetupbutton;
BEGIN
  IF Playing AND (Werte.DeviceID = dev_HDD) THEN Exit;
  IF PauseSetup AND Playing AND NOT(Pause)
  THEN BEGIN Handlepausebutton; SetupPaused:=true END
  ELSE SetupPaused:=False;
  setupdlg:=New(psetup,Init(Parent,werte)); Application^.MakeWindow(setupdlg);
END;

PROCEDURE tmod.getsettings;
{Wird beim Schließen des SETUP-Dialogs aufgerufen}
VAR result:word;
    OldDevID: integer;
    StoppedIt: boolean;
    Mode: byte;
    StartTime:Longint;
    oPause:Boolean;
BEGIN
  IF (NOT JustDraw)AND setuppaused THEN Handlepausebutton;
  IF haschanged THEN
  BEGIN
    IF NOT JustDraw THEN
    BEGIN
      OldDevID := werte.DeviceID;
      setupdlg^.getresult(werte);
      StoppedIt := Playing
               AND (werte.DeviceID <> OldDevID)
               AND ((OldDevID < dev_DAC) OR (werte.DeviceID < dev_DAC));
      IF StoppedIt THEN
      BEGIN
        starttime:=modinfo^.currtime;
        HandleStopButton;            { if device changed }
        Result:=0;
      END ELSE
      BEGIN
        MOD_SETTINGSCHANGED(werte,result);
        IF NOT ModInfo^.Playing THEN HandleStopButton; { if the player stopped }
        IF result<>0 THEN
        BEGIN
          handlestopbutton;
          LOG_ErrMessageBox('',Parent,result);
          StoppedIt := false;
          PostMessage(HWindow,wm_Command,id_setup,0)
        END;
      END;
    END;
    bits^.setbmpzahl(werte.bits);
    sr^.setbmpzahl((werte.samplerate + 500) DIV 1000);
    DrawPreAmp(0);
    IF NOT ABOUT THEN
    BEGIN
      blit(HWindow,x_m_st,mdy+y_m_st,27,11,0,11*(werte.stereo+Byte(Werte.Surround)-1),hm_st);
      blit(HWindow,x_IDO,mdy+y_IDO,26,17,26*Werte.OverSamp,66,h_sh_rep_scan);
      blit(HWindow,x_BPM,mdy+y_BPM,26,9,0 ,48+9*(1-Byte(Werte.NoBPM)AND 1),h_sh_rep_scan);
      blit(HWindow,x_PAN,mdy+y_PAN,23,9,26,48+9*(Byte(Werte.Panning)AND 1),h_sh_rep_scan);
      blit(HWindow,x_Qmix,mdy+y_Qmix,18, 9,26*(1-Byte(Werte.PreAmp)SHR 7),83,h_sh_rep_scan);
    END;
    CASE werte.DeviceID OF
      dev_HDD:  Mode := 5;
      ELSE      Mode := werte.TMode;
    END;
    IF NOT ABOUT THEN blit(HWindow,x_tmode,mdy+y_tmode,23,9,0,9*Mode,htmode);
    IF (NOT JustDraw) AND StoppedIt THEN
    BEGIN
      playing:=false;
      oPause:=Pause;
      IF Werte.DeviceID=DEV_HDD THEN
      BEGIN
        Pause:=False;
        handlestartbutton(False);
      END ELSE
      BEGIN
        handlestartbutton(true);
        Pause:=oPause;
        IF modinfo^.playing THEN
        BEGIN
          IF Pause THEN mod_Pause(False);
          modinfo^.currtime:=starttime;
          IF NOT pause THEN mod_endpause(False) ELSE mod_Pause(True);
          ModInfo^.CurrPttrn:=mod_GetTimePattern(ModInfo^.currtime);
          Pattern^.SetBMPZahl(ModInfo^.CurrPttrn);
        END;
      END;
    END;
  END;
END;

PROCEDURE tmod.getloadings(bCancel:BOOLEAN);
{Wird beim Schließen des Open-Dialogs aufgerufen}
VAR i:word;
BEGIN
  IF opendlg<>NIL THEN opendlg:=NIL;
  if (bCancel=TRUE) THEN Exit;
  IF dirlist^.songcount>0 THEN
  BEGIN
    LastMODFromOpen:=True;
    playlistcurrent:=0;
    FOR i:=0 TO (dirlist^.songcount-1) DO IF dirlist^.getattribut(i)=isplay THEN playlistcurrent:=i+1;
    FOR i:=1 TO dirlist^.songcount DO numberlist[i]:=i-1;
    IF playlistcurrent=0 THEN
    BEGIN
      playlistcurrent:=1;
      dirlist^.grillunpackedsong;
      dirlist^.newlist:=true;
      IdleState.patterns:=0;
      lastnameindex:=$FFFF;
      IF PlayOnLoad OR playing THEN
      BEGIN
        IF playing THEN handlestopbutton;
        handlestartbutton(False);
      END ELSE BEGIN playlistcurrent:=0; handlenextbutton; END;
    END;
    IF shuffle THEN BEGIN handleshufflebutton; handleshufflebutton; END;
    tracks^.setbmpzahl(longint(dirlist^.songcount));
    track^.setbmpzahl(longint(numberlist[playlistcurrent]+1));
  END ELSE
  BEGIN
    playlistcurrent:=0;
    handlestopbutton;
    WITH modinfo^ DO BEGIN totaltime:=0; currtime:=0; numtracks:=0; END;
    idlestate.playtime:=$FFFF;
    tracks^.setbmpzahl(0); track^.setbmpzahl(0); patterns^.setbmpzahl(0);
    setmodname('',false);
    dirlist^.grillunpackedsong;
    FillChar(ModInfo^,SizeOf(ModInfo^),0);
    IF detailwnd<>0 THEN PostMessage(detailwnd,wm_newfiletoplay,0,0);
  END;
END;

PROCEDURE tmod.wmdropfiles;
VAR pname         :pchar;
    maxfiles      :word;
    playitems,i   :word;
    olderrormode  :word;
    z             :pmydirs;
    box           :pwaitbox;
BEGIN
  IF IsWindowEnabled(HWindow)AND (NOT dirlist^.entpacke) THEN
  BEGIN
    IF About THEN HandleMODAbout;
    pname    :=MemAlloc(fsPathName);
    maxfiles :=DragQueryFile(msg.wparam,$ffff,pname,fsPathName);
    IF maxfiles>0 THEN
    BEGIN
      playitems:=0;
      z:=New(pmydirs,Init);
      box:=initwaitbox(Application^.MainWindow,getresstring(id_loadingfiles));
      olderrormode:=SetErrorMode(sem_failcriticalerrors OR sem_noopenfileerrorbox);
      FOR i:=1 TO maxfiles DO
      BEGIN
        setwaitbox(box,((100.0*(i+maxfiles))/(2*maxfiles)));
        DragQueryFile(msg.wparam,i-1,pname,fsPathName);
        PLAYITEMS:=LOADEXTERN(z,strpas(pname),Playitems=0);
      END;
      SetErrorMode(olderrormode);
      closewaitbox(box);
      Dispose(z,Done);
      IF playitems>0 THEN
      BEGIN
        playlistcurrent:=1;
        currentdir:=dirlist^.getdirstring(playlistcurrent);
        FOR i:=1 TO playitems DO numberlist[i]:=i-1;
        tracks^.setbmpzahl(playitems); track^.setbmpzahl(playlistcurrent);
        IF shuffle THEN BEGIN handleshufflebutton; handleshufflebutton; END;
        lastnameindex:=$FFFF;
        getnewmodname;
        IF playing OR PLAYONLOAD THEN
        BEGIN
          IF PLAYING  THEN handlestopbutton;
          handlestartbutton(False);
        END;
      END;
    END;
    FreeMem(pname,fsPathName);
  END;
  DragFinish(msg.wparam);
  msg.result:=0;
END;

FUNCTION tmod.LOADEXTERN(alist:pmydirs; aname:STRING; stillunchanged:boolean):WORD;
VAR ts            :tsearchrec;
    dir           :ARRAY [0..80] OF char;
    Name          :ARRAY[0..12] OF char;
    ext           :ARRAY[0..4] OF char;
    s             :STRING;
    filetypeindex :byte;
    dirindex      :integer;
    unch:boolean;
BEGIN
  IF About THEN HandleMODAbout;
  LastMODFromOpen:=False;
  unch:=stillunchanged;
  s:=aname+#0;
  IF Length(aname)<=3 THEN DOSERROR:=GetDriveType(byte(UpCase(aname[1]))-66)+30
                      ELSE FindFirst(@s[1],$37,ts);
  IF (DOSERROR>31) OR dirlist^.IsValid(isitdirectory,ts,filetypeindex) THEN
  BEGIN
    s:=aname; IF s[Length(s)]<>'\' THEN s:=s+'\';
    IF alist^.saugdrive(s) >= 0 THEN
    BEGIN
      IF unch THEN BEGIN dirlist^.ClearList; alist^.TransferData(dirlist); END
              ELSE alist^.from2to1(dirlist, NIL);
      unch:=false;
      alist^.ClearList;
    END;
  END ELSE
  BEGIN
    filesplit(@s[1],dir,Name,ext);
    IF dir[strlen(dir)-1]='\' THEN dir[strlen(dir)-1]:=#0;
    strcopy(dir,AnsiLower(dir));
    s:=strpas(Name)+strpas(ext); strpcopy(ts.Name,s);
    ts.Attr:=faArchive;
    IF dirlist^.isvalidarchive(ts,filetypeindex) THEN
    BEGIN
      IF alist^.saugarc(strpas(dir),strpas(ts.Name),false) THEN
      BEGIN
        IF unch THEN BEGIN dirlist^.ClearList; alist^.TransferData(dirlist); END
                ELSE alist^.from2to1(dirlist, NIL);
        unch:=false;
        alist^.ClearList;
      END;
    END ELSE
    IF dirlist^.IsValid(NOT isitdirectory,ts,filetypeindex) THEN
    BEGIN
      IF filetypeindex=listfiletype THEN
      BEGIN
        s:=strpas(dir)+'\'+s;
        IF alist^.sauglist(Parent,s) THEN alist^.checklist(Parent,strpas(ts.Name),id_nofilesinlist);
        IF alist^.songcount<>0 THEN
        IF unch THEN BEGIN dirlist^.ClearList; alist^.TransferData(dirlist); END
                ELSE alist^.from2to1(dirlist, NIL);
        alist^.ClearList;
        unch:=false;
      END ELSE
      BEGIN
        IF unch THEN dirlist^.ClearList;
        dirindex:=dirlist^.pathindirlist(strpas(dir));
        IF dirindex<0 THEN dirindex:=dirlist^.adddirstring(strpas(dir));
        IF Pos('.',Name)>0 THEN Name[Pos('.',Name)]:=#0;
        s:=strpas(AnsiLower(Name));
        WHILE Length(s)<8 DO system.Insert(' ',s,Length(s)+1);
        s:=char(filetypeindex)+willplay+char(dirindex)+noarcconst+#1+s;
        IF (dirlist^.isinloadlist(s,0)<0) THEN dirlist^.addsongstring(s);
        unch:=false;
      END;
    END;
  END;
  IF unch THEN loadextern:=0 ELSE loadextern:=dirlist^.songcount;
END;

FUNCTION tmod.wmnewfiletoplay;
VAR playitems,i   :word;
    olderrormode  :word;
    z             :pmydirs;
    box           :pwaitbox;
BEGIN
  wmnewfiletoplay:=false;
  IF msg.wparam>0 THEN
  BEGIN
    IF About THEN HandleMODAbout;
    playitems:=0;
    z:=New(pmydirs,Init);
    box:=initwaitbox(Application^.MainWindow,getresstring(id_loadingfiles));
    olderrormode:=SetErrorMode(sem_failcriticalerrors OR sem_noopenfileerrorbox);
    FOR i:=1 TO msg.wparam DO
    BEGIN
      setwaitbox(box,((100.0*(i+msg.wparam))/(2*msg.wparam)));
      PLAYITEMS:=LOADEXTERN(z,Copy(PString(msg.lparam)^,1,Pos(#0,PString(msg.lparam)^)-1),Playitems=0);
      Delete(PString(msg.lparam)^,1,Pos(#0,PString(msg.lparam)^));
    END;
    SetErrorMode(olderrormode);
    closewaitbox(box);
    Dispose(z,Done);
    IF playitems>0 THEN
    BEGIN
      wmnewfiletoplay:=true;
      playlistcurrent:=1;
      currentdir:=dirlist^.getdirstring(playlistcurrent);
      FOR i:=1 TO playitems DO numberlist[i]:=i-1;
      tracks^.setbmpzahl(playitems); track^.setbmpzahl(playlistcurrent);
      IF shuffle THEN BEGIN handleshufflebutton; handleshufflebutton; END;
      lastnameindex:=$FFFF;
      getnewmodname;
      IF playing OR PLAYONLOAD THEN
      BEGIN
        IF PLAYING  THEN handlestopbutton;
        handlestartbutton(False);
      END;
    END;
  END;
END;

PROCEDURE tmod.handlemodabout;
BEGIN
  IF psoundwindow(Parent)^.ministate AND (NOT ABOUT) THEN Exit;
  About:=NOT(About);
  IF about THEN
  BEGIN
    EnableAllbuttons(false);
    aboutlauf.Init(Parent,x_abscr,mdy+y_abscr,dx_abscr,dy_abscr);
  END ELSE
  BEGIN
    EnableAllbuttons(true);
    aboutlauf.Done;
    InvalidateRect(hWindow,NIL,True);
  END;
  showmainframe(0); {Zeichnen + Buttons enablen!}
END;

PROCEDURE tmod.animatebox;
BEGIN
  IF playing AND NOT pause THEN
  BEGIN
    boxcount:=(boxcount+1)MOD 4;
    IF boxcount=3 THEN
    BEGIN
      blit(HWindow,x_box1,mdy+y_box,w_box,h_box,0,h_box,hboxes);
      blit(HWindow,x_box2,mdy+y_box,w_box,h_box,0,h_box,hboxes)
    END ELSE
    BEGIN
      blit(HWindow,x_box1,mdy+y_box,w_box,h_box,0,(2-boxcount)*h_box,hboxes);
      blit(HWindow,x_box2,mdy+y_box,w_box,h_box,0,boxcount*h_box,hboxes);
    END;
  END;
  IF aktrunpos>0 THEN blit(HWindow,x_fun1,mdy+y_fun+((aktrunpos-1)*run_step),39,3,0,0,hfun)
                 ELSE blit(HWindow,x_fun1,mdy+y_fun+24,39,3,0,0,hfun);

  blit(HWindow,x_fun1,mdy+y_fun+(aktrunpos*run_step),39,3,0,3+Random(6)*3,hfun);

  IF aktrunpos>0 THEN blit(HWindow,x_fun2,mdy+y_fun+((aktrunpos-1)*run_step),39,3,0,0,hfun)
                 ELSE blit(HWindow,x_fun2,mdy+y_fun+24,39,3,0,0,hfun);
  blit(HWindow,x_fun2,mdy+y_fun+(aktrunpos*run_step),39,3,0,3+Random(6)*3,hfun);
  inc(aktrunpos);
  IF ((aktrunpos)*run_step)>25 THEN aktrunpos:=0;
END;

PROCEDURE tmod.show_unpacking;
BEGIN
  setmodname(Name,true);
END;

PROCEDURE tmod.handlehelpbutton;
BEGIN
  IF psoundwindow(Parent)^.ministate
  THEN helpme(Parent,id_minidlg)
  ELSE helpme(Parent,id_maindlg);
END;

PROCEDURE tmod.handleopenbutton;
BEGIN
  opendlg:= New(popen,Init(Parent,dirlist)); Application^.MakeWindow(opendlg);
END;

PROCEDURE tmod.savestate;
VAR f:FILE;
    s:STRING;
  {$IFDEF DEMO}
    I:Word;
  {$ENDIF}
BEGIN
  {$I-}
  s:=inidir; WHILE s[Length(s)]<>'\' DO DEC(byte(s[0]));
  s:=s+ProjectName+'.sta';
  Assign(F,S);
  SetFAttr(f,faArchive);
  inoutres:=0;
  filemode:=2;
  Reset(f,1);
  IF (IOResult<>0)OR(FileSize(f)<>83509) THEN Rewrite(F,1) ELSE Seek(F,0);
  IF IOResult<>0 THEN Exit;
  {$IFDEF DEMO}
    BlockWrite(f,numberlist,word(SizeOf(numberlist)));
    FOR I:=Maxplaylistentries+1 TO OldMaxEntries DO BlockWrite(F, I, SizeOf(WORD));
    BlockWrite(f,playlistcurrent,2);
    BlockWrite(f,playing,1);
    BlockWrite(f,pause,1);
    BlockWrite(f,repeatmode,1);
    BlockWrite(f,shuffle,1);
    BlockWrite(f,introscan,1);
    BlockWrite(f,minusmode,1);
    BlockWrite(f,songstate,3);
    BlockWrite(f,modinfo^.currtime,4);
  {$ELSE}
  IF LastModFromOpen OR(DirList^.SongCount > 1)OR Eof(F) THEN
  BEGIN
    BlockWrite(f,numberlist,word(SizeOf(numberlist)));
    BlockWrite(f,playlistcurrent,2);
    BlockWrite(f,playing,1);
    BlockWrite(f,pause,1);
    BlockWrite(f,repeatmode,1);
    BlockWrite(f,shuffle,1);
    BlockWrite(f,introscan,1);
    BlockWrite(f,minusmode,1);
    BlockWrite(f,songstate,3);
    BlockWrite(f,modinfo^.currtime,4);
  END ELSE Seek(f,FilePos(f)+word(SizeOf(numberlist))+2+1+1+1+1+1+1+3+4);
  {$ENDIF}
  BlockWrite(f,effwnd,2);
  BlockWrite(f,detailwnd,2);
  {$IFDEF DEMO}
  BlockWrite(f,dirlist^.songcount,2);
  BlockWrite(f,dirlist^.arcdircount,2);
  BlockWrite(f,dirlist^.arccount,2);
  BlockWrite(f,dirlist^.dircount,2);
  BlockWrite(f,dirlist^.dirs,SizeOf(dirlist^.dirs));
  BlockWrite(f,dirlist^.arcdirs,SizeOf(dirlist^.arcdirs));
  BlockWrite(f,dirlist^.arcs,SizeOf(dirlist^.arcs));
  BlockWrite(f,dirlist^.psonglist^,SizeOf(dirlist^.psonglist^));
  S:=''; FOR I:=maxplaylistentries+1 TO OldMaxEntries DO BlockWrite(F, S, (songlistentriesize+1));
  {$ELSE}
  IF LastModFromOpen OR (DirList^.SongCount > 1)OR Eof(F) THEN
  BEGIN
    BlockWrite(f,dirlist^.songcount,2);
    BlockWrite(f,dirlist^.arcdircount,2);
    BlockWrite(f,dirlist^.arccount,2);
    BlockWrite(f,dirlist^.dircount,2);
    BlockWrite(f,dirlist^.dirs,SizeOf(dirlist^.dirs));
    BlockWrite(f,dirlist^.arcdirs,SizeOf(dirlist^.arcdirs));
    BlockWrite(f,dirlist^.arcs,SizeOf(dirlist^.arcs));
    BlockWrite(f,dirlist^.psonglist^,SizeOf(dirlist^.psonglist^));
  END ELSE Seek(f,FilePos(f)+2+2+2+2+SizeOf(dirlist^.dirs)+
                 SizeOf(dirlist^.arcdirs)+SizeOf(dirlist^.arcs)+SizeOf(dirlist^.psonglist^));
  {$ENDIF}
  BlockWrite(f,Werte.PreAmp,SizeOf(Werte.PreAmp));
  Close(f);
END;

PROCEDURE tmod.Loadstate;
VAR f:FILE;
    s:STRING;
    starttime:longint;
    oldsongcount:word;
    eff_wnd,info_wnd:hwnd;
    oPause:Boolean;
BEGIN
  {$I-}
  handlestopbutton;
  s:=inidir; WHILE s[Length(s)]<>'\' DO DEC(byte(s[0]));
  s:=s+ProjectName+'.sta';
  Assign(f,s);
  inoutres:=0; filemode:=0;
  Reset(f,1);
  IF IOResult<>0 THEN Exit;
  { disregard old status files }
  IF FileSize (f) <> 83509 THEN BEGIN Close (f); Exit; END;
  BlockRead(f,numberlist,word(SizeOf(numberlist)));
  {$IFDEF DEMO}
    Seek(F, FilePos(F)+((OldMaxEntries-maxplaylistentries)*SizeOf(WORD)));
  {$ENDIF}
  BlockRead(f,playlistcurrent,2);
  BlockRead(f,playing,1);
  BlockRead(f,pause,1);
  BlockRead(f,repeatmode,1);
  BlockRead(f,shuffle,1);
  BlockRead(f,introscan,1);
  BlockRead(f,minusmode,1);
  BlockRead(f,songstate,3);
  BlockRead(f,starttime,4);
  BlockRead(f,eff_wnd,2);
  BlockRead(f,info_wnd,2);
  BlockRead(f,dirlist^.songcount,2);
  {$IFDEF DEMO}
    IF DirList^.SongCount>MaxPlayListEntries THEN DirList^.SongCount:=MaxPlayListEntries;
    IF PlaylistCurrent>DirList^.SongCount THEN PlaylistCurrent:=1;
  {$ENDIF}
  BlockRead(f,dirlist^.arcdircount,2);
  BlockRead(f,dirlist^.arccount,2);
  BlockRead(f,dirlist^.dircount,2);
  BlockRead(f,dirlist^.dirs,SizeOf(dirlist^.dirs));
  BlockRead(f,dirlist^.arcdirs,SizeOf(dirlist^.arcdirs));
  BlockRead(f,dirlist^.arcs,SizeOf(dirlist^.arcs));
  BlockRead(f,dirlist^.psonglist^,SizeOf(dirlist^.psonglist^));
  {$IFDEF DEMO}
  Seek(F, FilePos(F)+((OldMaxEntries-Maxplaylistentries)*(songlistentriesize+1)));
  {$ENDIF}
  BlockRead(F,Werte.PreAmp,SizeOf(Werte.PreAmp));
  Close(F);
  IF eff_wnd<>0  THEN SendMessage(HWindow,wm_command,id_effects,0);
  IF info_wnd<>0 THEN SendMessage(HWindow,wm_command,id_info,0);
  oldsongcount:=dirlist^.songcount;
  IF dirlist^.songcount>0 THEN dirlist^.checklist(Parent,'',0);
  IF ((numberlist[playlistcurrent]>dirlist^.songcount)OR(oldsongcount<>dirlist^.songcount))
   AND(Dirlist^.songcount>0) THEN
  BEGIN
    playlistcurrent:=dirlist^.songcount;
    IF shuffle THEN
    BEGIN
      FOR oldsongcount:=1 TO dirlist^.songcount DO numberlist[oldsongcount]:=oldsongcount-1;
      handleshufflebutton; handleshufflebutton;
    END;
  END;
  IF (ParamCount=0)AND(dirlist^.songcount>0) THEN
  BEGIN
    LASTNAMEINDEX:=$FFFF;
    tracks^.setbmpzahl(longint(dirlist^.songcount));
    track^.setbmpzahl(longint(numberlist[playlistcurrent]+1));
    IF (PlayOnLoad OR playing)AND(Error=0) THEN
    BEGIN
      playing:=false;
      oPause:=Pause;
      handlestartbutton(true);
      Pause:=oPause;
      IF (modinfo^.playing) THEN
      BEGIN
        IF Pause THEN mod_Pause(False);
        modinfo^.currtime:=starttime;
        IF NOT pause THEN mod_endpause(False) ELSE mod_Pause(True);
        ModInfo^.CurrPttrn:=mod_GetTimePattern(ModInfo^.currtime);
        Pattern^.SetBMPZahl(ModInfo^.CurrPttrn);
      END;
    END
    ELSE getnewmodname;
  END ELSE
  BEGIN
    playlistcurrent:=0;
    tracks^.setbmpzahl(0); track^.setbmpzahl(0); patterns^.setbmpzahl(0);
    setmodname('',false);
  END;
  MinusMode:=NOT MinusMode;
  SendMessage(HWindow,wm_command,id_btntime,0);
  FillChar(IdleState,SizeOf(IdleState),$ff);
  InvalidateRect(hWindow,NIL,True);
  {showmainframe(0);{ }
END;

{****ENDE***************************************MOD-PLAYER************************}


{Hauptobject******************************************************************}
PROCEDURE tsoundwindow.wmdrawitem;
VAR oldfont:hfont;
    s:STRING[80];
BEGIN
  WITH pdrawitemstruct(msg.lparam)^,rcitem DO
  IF ctltype=odt_menu THEN
  BEGIN
    frame3d(hdc,1,1,right-left,GetMenuItemCount(hwnditem)*(bottom-top+1)-3,1,up);
    IF (itemstate=ods_selected)
    THEN frameFilled3d(hdc,left+1,top+1,right-left-2,bottom-top-2,2,cForeGnd,down)
    ELSE frameFilled3d(hdc,left,top,right-left,bottom-top,0,cForeGnd,down);
    oldfont:=SelectObject(hdc,ANSIhandle);
    SetBkMode(hdc,transparent);
    SetTextAlign(hdc,ta_left OR ta_top OR ta_noupdatecp);
    SetBkColor(hDC,cForeGnd);
    IF itemstate<>ods_disabled THEN SetTextColor(hdc,cVGStatic) ELSE SetTextColor(hdc,cTextShadow);
    inc(rcitem.left,10); inc(rcitem.top,2);
    s:=strpas(pchar(itemdata)); s:=s+#32#0;
    DrawText(hdc,pchar(itemdata),Pos('|',s)-1,rcitem,dt_left);
    Delete(s,1,Pos('|',s));
    DrawText(hdc,@s[1],-1,rcitem,dt_right);
    SelectObject(hdc,oldfont);
  END;
END;

{Malt CaptionText}
PROCEDURE tsoundwindow.paintcaption;
BEGIN
  INHERITED paintcaption(adc);
  IF NOT ministate THEN
  BEGIN
    bsetup^.enable;
    IF bColor<>NIL THEN bcolor^.enable;
    bmixer^.enable;
    bhelp^.enable;
  END;
END;

{ButtonHandling}
PROCEDURE tsoundwindow.WMCommand;
VAR s:STRING;
BEGIN
  IF NOT w_mod^.idhandler(msg) THEN
  CASE msg.wparam OF
    id_help      :W_mod^.handlehelpbutton;
    id_setup     :W_mod^.handlesetupbutton;
    id_mixer     :BEGIN
                    s:=strpas(mixerpath);
                    IF findexe(s) THEN
                    BEGIN
                      s:=s+#0;
                      IF WinExec(@s[1],sw_shownormal)>32
                      THEN
                      BEGIN
                        mixerhandle:=GetActiveWindow;
                        WHILE  GetParent(mixerhandle)>0 DO mixerhandle:=GetParent(mixerhandle);
                      END
                      ELSE mixerhandle:=0;
                    END ELSE LOG_ErrMessageBox('',@self,id_nomix);
                  END;
    id_color     :IF ColorDlg=NIL THEN
                  BEGIN
                    colordlg:=New(pPictureSetup,Init(@colordlg));
                    Application^.MakeWindow(colordlg);
                  END ELSE SendMessage(ColorDlg^.HWindow,wm_close,0,0);
    swebuz_maximize:
    BEGIN
      syswindow^.topmost:=syswindow^.topmost XOR canbetopmost;
      IF (syswindow^.topmost AND istopmost)=istopmost
      THEN
      BEGIN
        SetWindowPos(HWindow,hwnd_notopmost,Attr.x,Attr.y,Attr.w,Attr.h,swp_showwindow);
        miniontop:=true;
      END ELSE miniontop:=false;
      closeministate;
      msg.result:=0;
    END;
    swebuz_restore:
    BEGIN
      syswindow^.topmost:=syswindow^.topmost OR canbetopmost;
      IF (syswindow^.topmost AND istopmost)=istopmost
      THEN
      BEGIN
        miniontop:=true;
        SetWindowPos(HWindow,hwnd_topmost,Attr.x,Attr.y,Attr.w,Attr.h,swp_showwindow);
      END ELSE miniontop:=false;
      msg.result:=0;
      initministate;
      w_mod^.bplay^.focus;
    END;
    id_LoadState:w_mod^.loadstate;
    ELSE INHERITED WMCommand(msg);
  END ELSE INHERITED WMCommand(msg);
END;

PROCEDURE tsoundwindow.wminitmenu;
BEGIN
  IF effwnd=0 THEN CheckMenuItem(syswindow^.hsysmenu,id_effects,mf_unchecked OR mf_bycommand)
              ELSE CheckMenuItem(syswindow^.hsysmenu,id_effects,mf_checked OR mf_bycommand);
  IF detailwnd=0 THEN CheckMenuItem(syswindow^.hsysmenu,id_info,mf_unchecked OR mf_bycommand)
                 ELSE CheckMenuItem(syswindow^.hsysmenu,id_info,mf_checked OR mf_bycommand);
END;

{Fängt Message von OWNERDRAW-MENU ab}
PROCEDURE tsoundwindow.wmmenuchar;
VAR ch: char;
BEGIN
  IF msg.lparamlo=mf_popup THEN BEGIN
    ch := UpCase (char(Lo(msg.wparam)));
    IF ch = syschar [2000] THEN msg.result:=longint(2 SHL 16)+0 ELSE
    IF ch = syschar [2001] THEN msg.result:=longint(2 SHL 16)+1 ELSE
    IF ch = syschar [1999] THEN msg.result:=longint(2 SHL 16)+2 ELSE
    IF ch = syschar [2002] THEN msg.result:=longint(2 SHL 16)+3 ELSE
                                msg.result:=$FFFFFFFF;
  END;
END;

PROCEDURE tsoundwindow.wmsyscommand;
VAR s:STRING;
BEGIN
  CASE msg.wparam OF
    id_info,id_effects: SendMessage(HWindow,wm_command,msg.wparam,0);
  ELSE CASE msg.wparam AND $FFF0 OF
    sc_minimize       :
    BEGIN
      INHERITED wmsyscommand(msg);
      w_mod^.HandleSwName;
    END;
    sc_restore        :
    BEGIN
      INHERITED wmsyscommand(msg);
      IF NOT MiniState THEN SetCaption(longver);
    END;
    ELSE INHERITED wmsyscommand(msg);
  END;
  END;
END;

{Korrektes Done beim WIndows-Ende}
PROCEDURE tsoundwindow.wmendsession;
VAR S:STRING;
BEGIN
  msg.result:=0;
  IF msg.wparam>0 THEN
  BEGIN
    {
    Byte(s[0]):=getprofilestring('Windows','Load','',@s[1],sizeof(s)-1);
    if Length(S)<>0 THEN S:=S+' ';
    s:=S+ParamStr(0);
    writeprofilestring('Windows','Load',@s[1]);
    { }
    pbuzapplication(Application)^.Done;
    Halt(0);
  END;
END;

{Weiterreichen an Objekte}
CONST PR:BOOLEAN=FALSE;
PROCEDURE tsoundwindow.wmkeydown;
VAR Dev,Mode:WORD;
BEGIN
  msg.result:=0;
  IF NOT w_mod^.kbdhandler(msg) THEN
  IF (KeyState=1)AND(msg.wParam=VK_RIGHT)  THEN w_mod^.NextPattern   ELSE
  IF (KeyState=1)AND(msg.wParam=VK_LEFT)   THEN w_mod^.PrevPattern   ELSE
  IF (KeyState=1)AND(msg.wParam=VK_F5)     THEN BEGIN w_mod^.werte.Oversamp:=3; w_mod^.SwitchIDO; END ELSE
  IF (KeyState=1)AND(msg.wParam=VK_F6)     THEN BEGIN w_mod^.werte.Oversamp:=0; w_mod^.SwitchIDO; END ELSE
  IF (KeyState=1)AND(msg.wParam=VK_F7)     THEN BEGIN w_mod^.werte.Oversamp:=1; w_mod^.SwitchIDO; END ELSE
  IF (KeyState=1)AND(msg.wParam=VK_F8)     THEN BEGIN w_mod^.werte.Oversamp:=2; w_mod^.SwitchIDO; END ELSE
  IF (KeyState=0)AND(msg.wParam=VK_DELETE) THEN BEGIN IF NOT w_mod^.dirlist^.entpacke THEN w_mod^.handleerasefile; END ELSE
  IF (KeyState=0)AND(msg.wParam=Ord('N'))  THEN PostMessage(w_mod^.hWindow,WM_LButtonDblclk,0,0);
  IF (KeyState=0)AND((msg.wParam=Ord('X'))OR(msg.wParam=Ord('C'))) THEN
  BEGIN
    {w_mod^.HandleStopButton;}
    PostMessage(w_mod^.hWindow,wm_close,0,0);
  END ELSE
  IF (KeyState=0)AND(msg.wParam=Ord('D')) THEN
  BEGIN
    w_mod^.HandleStopButton;
    Dev:=w_mod^.Werte.DeviceID;
    Mode:=w_mod^.Werte.TMode;
    w_mod^.Werte.DeviceID:=dev_HDD;
    w_mod^.Werte.TMode:=0;
    w_mod^.handlestartbutton(False);
    while MODINFO^.playing do 
    begin
      KeepWindowsAlive;
      IdleAction;
    end;
    w_mod^.HandleStopButton;
    w_mod^.Werte.DeviceID:=Dev;
    w_mod^.Werte.TMode:=Mode;
    w_mod^.HandleNextButton;
    w_mod^.handlestartbutton(False);
  END ELSE
  IF (KeyState=0)AND(msg.wParam=VK_INSERT) THEN
  BEGIN
    SendMessage(w_mod^.bOpen^.HWindow,wm_keydown,vk_return,0);
    PostMessage(w_mod^.bOpen^.HWindow,wm_keyup,vk_return,0);
  END ELSE
  IF msg.wParam=VK_PAUSE  THEN
  BEGIN
    PR:=NOT PR;
    RepeatPattern(PR);
  END ELSE msg.result:=1;
END;

PROCEDURE tsoundwindow.wmkeyUp(VAR msg:tmessage);
BEGIN
  IF w_mod^.wmkeyup(msg) THEN msg.result:=0 ELSE msg.result:=1;
END;

{Stellt einzelne Objekte dar}
PROCEDURE tsoundwindow.showmainframe(ADc:HDc);
BEGIN
  IF ministate THEN paintministate(adc) ELSE
  BEGIN
    w_mod^.showmainframe(adc);
    paintcaption(adc);
  END
END;

PROCEDURE tsoundwindow.paintministate(adc:hdc);
VAR memdc,pdc:hdc;
BEGIN
  IF hWindow=0 THEN Exit;
  IF adc=0 THEN pdc:=GetDC(HWindow) ELSE pdc:=adc;
  IF PaletteInstalled THEN SelectPalette(pdc,hPal,false);
  framefilled3d(pdc,0,capdy,Attr.w-2,Attr.h-2-capdy,2,cForeGnd,up);
  framefilled3d(pdc,4,capdy+3,42,32,1,cBackGnd,down);
  frame3d(pdc,48,capdy+4,254,30,2,down);
  memdc:=CreateCompatibleDC(pdc);
  IF PaletteInstalled THEN SelectPalette(memdc,hPal,false);
  SelectObject(memdc,w_mod^.minus_mode);
  BitBlt(pdc,27,capdy+25,2,4,memdc,9,0,srccopy);
  IF w_mod^.minusmode THEN BitBlt(pdc,6,capdy+25,4,2,memdc,0,0,srccopy)
                      ELSE BitBlt(pdc,6,capdy+25,4,2,memdc,0,2,srccopy);
  DeleteDC(memdc);
  paintcaption(pdc);
  IF adc=0 THEN ReleaseDC(HWindow,pdc);
END;

PROCEDURE tsoundwindow.initministate;
BEGIN
  IF w_mod^.about THEN w_mod^.handlemodabout;
  WITH Attr,o_rect DO BEGIN  left:=x; right:=x+w; top:=y; bottom:=y+h; END;
  MoveWindow(HWindow,Attr.x,Attr.y,0,0,True);
  ministate:=true;
  enableallbuttons(false); IF bColor<>NIL THEN bcolor^.disable; bmixer^.disable; bsetup^.disable; bhelp^.disable;
  o_mtime:=w_mod^.mtime; o_stime:=w_mod^.stime; o_track:=w_mod^.track;
  m_track^.zahl:=w_mod^.track^.zahl;
  m_mtime^.zahl:=w_mod^.mtime^.zahl;
  m_stime^.zahl:=w_mod^.stime^.zahl;
  w_mod^.track:=m_track; w_mod^.mtime:=m_mtime; w_mod^.stime:=m_stime;
  WITH w_mod^ DO
  BEGIN
    o_stop.x:=bstop^.Attr.x; o_stop.y:=bstop^.Attr.y;
    WITH bstop^ DO MoveWindow(HWindow,50,capdy+6,Attr.w,Attr.h,true); bstop^.enable;
    o_play.x:=bplay^.Attr.x; o_play.y:=bplay^.Attr.y;
    WITH bplay^ DO MoveWindow(HWindow,100,capdy+6,Attr.w,Attr.h,true); bplay^.enable;
    o_pause.x:=bpause^.Attr.x; o_pause.y:=bpause^.Attr.y;
    WITH bpause^ DO MoveWindow(HWindow,150,capdy+6,Attr.w,Attr.h,true); bpause^.enable;
    o_prev.x:=bprev^.Attr.x; o_prev.y:=bprev^.Attr.y;
    WITH bprev^ DO MoveWindow(HWindow,200,capdy+6,Attr.w,Attr.h,true); bprev^.enable;
    o_next.x:=bnext^.Attr.x; o_next.y:=bnext^.Attr.y;
    WITH bnext^ DO MoveWindow(HWindow,250,capdy+6,Attr.w,Attr.h,true); bnext^.enable;
  END;
  w_mod^.HandleSwName;
  m_track^.enable; m_mtime^.enable; m_stime^.enable;
  Attr.x:=x_mini; Attr.y:=y_mini; Attr.w:=2*GetSystemMetrics(sm_cyborder)+307;
  Attr.h:=capdy+38+2*GetSystemMetrics(sm_cyborder);
  MoveWindow(HWindow,Attr.x,Attr.y,Attr.w,Attr.h,true);
  pcrect.right:=bMini^.Attr.X-2;
  SetWindowRegion(@self,'MODMINIMASK');
END;

PROCEDURE tsoundwindow.closeministate;
BEGIN
  ministate:=false;
  MoveWindow(HWindow,Attr.x,Attr.y,0,0,True);
  IF bColor<>NIL THEN bcolor^.enable; bmixer^.enable; bsetup^.enable; bhelp^.enable;
  w_mod^.mtime:=o_mtime; w_mod^.mtime^.zahl:=m_mtime^.zahl;
  w_mod^.stime:=o_stime; w_mod^.stime^.zahl:=m_stime^.zahl;
  w_mod^.track:=o_track; w_mod^.track^.zahl:=m_track^.zahl;
  m_track^.disable; m_mtime^.disable; m_stime^.disable;
  WITH w_mod^ DO
  BEGIN
    bstop^.Attr.x:=o_stop.x; bstop^.Attr.y:=o_stop.y;
    WITH bstop^DO MoveWindow(HWindow,Attr.x,Attr.y,Attr.w,Attr.h,true);
    bplay^.Attr.x:=o_play.x; bplay^.Attr.y:=o_play.y;
    WITH bplay^ DO MoveWindow(HWindow,Attr.x,Attr.y,Attr.w,Attr.h,true);
    bpause^.Attr.x:=o_pause.x; bpause^.Attr.y:=o_pause.y;
    WITH bpause^ DO MoveWindow(HWindow,Attr.x,Attr.y,Attr.w,Attr.h,true);
    bprev^.Attr.x:=o_prev.x; bprev^.Attr.y:=o_prev.y;
    WITH bprev^ DO MoveWindow(HWindow,Attr.x,Attr.y,Attr.w,Attr.h,true);
    bnext^.Attr.x:=o_next.x; bnext^.Attr.y:=o_next.y;
    WITH bnext^ DO MoveWindow(HWindow,Attr.x,Attr.y,Attr.w,Attr.h,true);
  END;
  x_mini:=Attr.x; y_mini:=Attr.y;
  SetCaption(longver);
  enableallbuttons(true);
  WITH o_rect DO
  BEGIN
    Attr.x:=left; Attr.y:=top; Attr.w:=right-left; Attr.h:=bottom-top;
    IF bColor<>NIL THEN pcrect.left:=bcolor^.Attr.x+bcolor^.Attr.w+2 ELSE pcrect.left:=22;
    MoveWindow(HWindow,left,top,right-left,bottom-top,true);
    pcrect.right:=bmixer^.Attr.x-2;
  END;
  SetWindowRegion(@self,'MODMAINMASK');
  InvalidateRect(hWindow,NIL,True);
  {showmainframe(0);{ }
END;

{Käse}
PROCEDURE tsoundwindow.wmunpacking;
VAR s:^STRING;
BEGIN
  s:=pointer(msg.lparam);
  w_mod^.show_unpacking(s^);
END;

PROCEDURE tsoundwindow.wmsongselected;
VAR i:word;
BEGIN
  WITH w_mod^DO
  BEGIN
    FOR i:=1 TO dirlist^.songcount DO IF numberlist[i]=msg.lparam THEN
    BEGIN
      playlistcurrent:=i;
      updatemodwindow;
      Break;
    END;
  END;
END;

FUNCTION  tsoundwindow.IDLEACTION;
BEGIN
  IDLEACTION:=w_mod^.IDLEACTION;
END;
{Weiterreichen an Objekte}
PROCEDURE tsoundwindow.enableallbuttons; BEGIN w_mod^.enableallbuttons(enableall); END;
PROCEDURE tsoundwindow.getsettings; BEGIN w_mod^.getsettings(boolean(msg.wparam),false); END;
PROCEDURE tsoundwindow.getloadings; BEGIN w_mod^.getloadings(Msg.wParam<>0); END;
PROCEDURE tsoundwindow.wmdropfiles; BEGIN w_mod^.wmdropfiles(msg); END;
PROCEDURE tsoundwindow.wmnewfiletoplay; BEGIN w_mod^.wmnewfiletoplay(msg); END;
FUNCTION tsoundwindow.timerproc;
BEGIN
  IF INHERITED timerproc(msg) THEN Exit;
  w_mod^.modtimerproc(msg.wparam);
END;

CONSTRUCTOR tsoundwindow.Init(Aparent:Pwindowsobject; Atitle:Pchar);
VAR i:word;
    ts: STRING[50];
BEGIN
  miniontop:=GetPrivateProfileInt(s_state,'MiniOnTop',1,INIDIR)<>0;
  ministate:=GetPrivateProfileInt(s_state,'MiniState',0,INIDIR)<>0;
  Load_state:=GetPrivateProfileInt(s_state,'StateSaver',1,INIDIR)<>0;
  INHERITED Init(aparent,atitle,ws_minimizebox OR ws_sysmenu OR ws_maximizebox OR ws_maximize,canbetopmost);
  {TEMP}
  IF NOT ExtensionsAssociated THEN
  BEGIN
    IF GetPrivateProfileInt(s_state,'AskExtensions',1,INIDIR)<>0 THEN
    BEGIN
      i:=Byte(ourmessagebox(NIL,longver,'Some Module file name extensions'#10+
                                        'are not associated with MOD4WIN.'#10#10+
                                        'Associate them right NOW?',mb_yesno OR mb_iconquestion)=ID_YES);
      IF ourmessagebox(NIL,longver,'Ask the "ASSOCIATIONS" Question again?',mb_yesno OR mb_iconquestion) <> id_yes
      THEN WritePrivateProfileString(s_state,'AskExtensions','0',INIDIR);
      IF i=1
      THEN WritePrivateProfileString(s_state,'ForceExtensions','1',INIDIR)
      ELSE WritePrivateProfileString(s_state,'ForceExtensions','0',INIDIR);
    END ELSE i:=GetPrivateProfileInt(s_state,'ForceExtensions',1,INIDIR);
    IF i=1 THEN AssociateExtensions(ParamStr(0));
  END;
  {END TEMP}
  WITH Attr DO
  BEGIN
    IF (ExOs=Ex_NewWin)
    THEN style:=WS_popup OR ws_border OR ws_visible OR ws_sysmenu
    ELSE style:=ws_popup OR ws_border OR ws_visible;
    exstyle:=ws_ex_acceptfiles OR WS_EX_NOPARENTNOTIFY;
    x:=GetPrivateProfileInt(s_state,'X_MAIN',90,INIDIR);
    y:=GetPrivateProfileInt(s_state,'Y_MAIN',100,INIDIR);
    w:=X_Main+GetSystemMetrics(sm_cxborder)+1;
    h:=Y_Main+18+2*GetSystemMetrics(sm_cyborder);
    setcorrectwinpos(x,y,w,h);
    {Wenn MINISTATE dann kein FLACKERN ->siehe SETUPWINDOW}
    w:=0;
    h:=0;
  END;
  x_mini:=GetPrivateProfileInt(s_state,'X_Mini',Attr.x,INIDIR);
  y_mini:=GetPrivateProfileInt(s_state,'Y_Mini',Attr.y,INIDIR);
  
  {IF x_mini+309>GetSystemMetrics(sm_cxscreen) THEN x_mini:=GetSystemMetrics(sm_cxscreen)-309;
  IF y_mini+capdy+38>GetSystemMetrics(sm_cyscreen) THEN y_mini:=GetSystemMetrics(sm_cyscreen)-38-capdy; }
  
  GetPrivateProfileString(s_state,'MixerPath','',@mixerpath,      SizeOf (MixerPath),     INIDIR);
  GetPrivateProfileString(s_state,'HDPath','C:\',@HDpath,         SizeOf (HDPath),        INIDIR);
  GetPrivateProfileString(s_state,'LastDir','',  @currentdir[1],  SizeOf (CurrentDir)-1,  INIDIR);
  currentdir[0]:=char(byte(strlen(@currentdir[1])));
  GetPrivateProfileString(s_state,'LastFMDir','',@currentfmdir[1],SizeOf (CurrentFMDir)-1,INIDIR);
  currentfmdir[0]:=char(byte(strlen(@currentfmdir[1])));

  GetPrivateProfileString(s_packer,'ARJ_P','arj.exe',@arcpackexecs[arjfiletype][1],99,INIDIR);
  arcpackexecs[arjfiletype][0]:=char(byte(strlen(@arcpackexecs[arjfiletype][1])));
  GetPrivateProfileString(s_packer,'ZIP_P','pkzip.exe',@arcpackexecs[zipfiletype][1],99,INIDIR);
  arcpackexecs[zipfiletype][0]:=char(byte(strlen(@arcpackexecs[zipfiletype][1])));
  GetPrivateProfileString(s_packer,'LHA_P','lha.exe',@arcpackexecs[lharcfiletype][1],99,INIDIR);
  arcpackexecs[lharcfiletype][0]:=char(byte(strlen(@arcpackexecs[lharcfiletype][1])));
  {PACKERPFADE}
  GetPrivateProfileString(s_packer,'ARJ_UP','arj.exe',@arcunpackexecs[arjfiletype][1],99,INIDIR);
  arcunpackexecs[arjfiletype][0]:=char(byte(strlen(@arcunpackexecs[arjfiletype][1])));
  GetPrivateProfileString(s_packer,'ZIP_UP','pkunzip.exe',@arcunpackexecs[zipfiletype][1],99,INIDIR);
  arcunpackexecs[zipfiletype][0]:=char(byte(strlen(@arcunpackexecs[zipfiletype][1])));
  GetPrivateProfileString(s_packer,'LHA_UP','lha.exe',@arcunpackexecs[lharcfiletype][1],99,INIDIR);
  arcunpackexecs[lharcfiletype][0]:=char(byte(strlen(@arcunpackexecs[lharcfiletype][1])));
  {ARJPACKKOMMANDOS}
  GetPrivateProfileString('ARJPACKERCMDS','UNPACK','',@Arcextractstring[arjfiletype][1],99,INIDIR);
  Arcextractstring[arjfiletype][0]:=char(byte(strlen(@Arcextractstring[arjfiletype][1])));
  IF Arcextractstring[arjfiletype]=''
  THEN Arcextractstring[arjfiletype]:=defextractstring[arjfiletype];
  GetPrivateProfileString('ARJPACKERCMDS','PACK','',@ArcCopystring[arjfiletype][1],99,INIDIR);
  ArcCopystring[arjfiletype][0]:=char(byte(strlen(@ArcCopystring[arjfiletype][1])));
  IF ArcCopystring[arjfiletype]=''
  THEN ArcCopystring[arjfiletype]:=defArcCopystring[arjfiletype];
  GetPrivateProfileString('ARJPACKERCMDS','DELETE','',@Arcdelstring[arjfiletype][1],99,INIDIR);
  Arcdelstring[arjfiletype][0]:=char(byte(strlen(@Arcdelstring[arjfiletype][1])));
  IF Arcdelstring[arjfiletype]=''
  THEN Arcdelstring[arjfiletype]:=defArcdelstring[arjfiletype];
  {zipPACKKOMMANDOS}
  GetPrivateProfileString('ZIPPACKERCMDS','UNPACK','',@Arcextractstring[zipfiletype][1],99,INIDIR);
  Arcextractstring[zipfiletype][0]:=char(byte(strlen(@Arcextractstring[zipfiletype][1])));
  IF Arcextractstring[zipfiletype]=''
  THEN Arcextractstring[zipfiletype]:=defextractstring[zipfiletype];
  GetPrivateProfileString('ZIPPACKERCMDS','PACK','',@ArcCopystring[zipfiletype][1],99,INIDIR);
  ArcCopystring[zipfiletype][0]:=char(byte(strlen(@ArcCopystring[zipfiletype][1])));
  IF ArcCopystring[zipfiletype]=''
  THEN ArcCopystring[zipfiletype]:=defArcCopystring[zipfiletype];
  GetPrivateProfileString('ZIPPACKERCMDS','DELETE','',@Arcdelstring[zipfiletype][1],99,INIDIR);
  Arcdelstring[zipfiletype][0]:=char(byte(strlen(@Arcdelstring[zipfiletype][1])));
  IF Arcdelstring[zipfiletype]=''
  THEN Arcdelstring[zipfiletype]:=defArcdelstring[zipfiletype];
  {LHAPACKKOMMANDOS}
  GetPrivateProfileString('LHAPACKERCMDS','UNPACK','',@Arcextractstring[lharcfiletype][1],99,INIDIR);
  Arcextractstring[lharcfiletype][0]:=char(byte(strlen(@Arcextractstring[lharcfiletype][1])));
  IF Arcextractstring[lharcfiletype]=''
  THEN Arcextractstring[lharcfiletype]:=defextractstring[lharcfiletype];
  GetPrivateProfileString('LHAPACKERCMDS','PACK','',@ArcCopystring[lharcfiletype][1],99,INIDIR);
  ArcCopystring[lharcfiletype][0]:=char(byte(strlen(@ArcCopystring[lharcfiletype][1])));
  IF ArcCopystring[lharcfiletype]=''
  THEN ArcCopystring[lharcfiletype]:=defArcCopystring[lharcfiletype];
  GetPrivateProfileString('LHAPACKERCMDS','DELETE','',@Arcdelstring[lharcfiletype][1],99,INIDIR);
  Arcdelstring[lharcfiletype][0]:=char(byte(strlen(@Arcdelstring[lharcfiletype][1])));
  IF Arcdelstring[lharcfiletype]=''
  THEN Arcdelstring[lharcfiletype]:=defArcdelstring[lharcfiletype];
  ShowPack:=GetPrivateProfileInt('PACKER','SHOWWINDOW',0,INIDIR)<>0;
  m_label:=LoadMyBitMap(hinstance,'m_label');
  {MOD-Interface init!}
  w_mod:=New(pmod,Init(@self));
  IF w_mod=NIL THEN Halt(0);
  {Caption-Buttons}
  IF ExOs=Ex_NewWin THEN
  BEGIN
    IF GetPrivateProfileInt (s_state, 'EnablePalette', 0, inidir) = 1
    THEN bcolor:=New(PbmpCaptBtn, Init(@self, id_color,20,2,'bcolor95',false))ELSE bcolor:=NIL;
    bmixer:=New  (PbmpCaptBtn, Init(@self, id_mixer,379,2,'bmixer95',false));
    bsetup:=New  (PbmpCaptBtn, Init(@self, id_setup,414,2,'bsetup95',false));
    bhelp:=New   (PbmpCaptBtn, Init(@self, id_help ,449 ,2,'bhelp95',false));
  END ELSE
  BEGIN
    IF GetPrivateProfileInt (s_state, 'EnablePalette', 0, inidir) = 1
    THEN bcolor:=New(PbmpCaptBtn, Init(@self, id_color,18,0,'bcolor',false))ELSE bcolor:=NIL;
    bmixer:=New  (PbmpCaptBtn, Init(@self, id_mixer,403,0,'bmixer',false));
    bsetup:=New  (PbmpCaptBtn, Init(@self, id_setup,436,0,'bsetup',false));
    bhelp:=New   (PbmpCaptBtn, Init(@self, id_help ,469 ,0,'bhelp',false));
  END;
  {ministate-Display}
  m_track:=New   (pbmplcd   , Init(@self,-1,10,capdy+5 ,4,0,'LCD_08x13'));
  m_mtime:=New   (pbmplcd   , Init(@self,-1,10,capdy+20,2,0,'LCD_08x13'));
  m_stime:=New   (pbmplcd   , Init(@self,-1,28,capdy+20,2,1,'LCD_08x13'));
  mixerhandle:=0;
  ColorDlg:=NIL;
  FOR i := 1999 TO 2002 DO BEGIN ts := getresstring(i); syschar[i] := UpCase (ts[Pos ('&', ts) + 1]); END;
  pCursor:=LoadCursor(hinstance,'PointCursor');
  Norm_Cursor:=LoadCursor(0,idc_arrow);
  PreAmpClicked := false;
END;

DESTRUCTOR tsoundwindow.Done;
VAR s:STRING[40];
    j:Byte;
BEGIN
  {$IFDEF share}
  IF (NOT Registered) AND (NOT Restart) THEN
    IF DaysUsed <= 30
      THEN sharedlg(@self,DaysUsed DIV 3,id_shareendlines,  id_registration)
      ELSE sharedlg(@self,10,            id_shareoverlines, id_registration);
  {$ENDIF}
  {$I-}
  ChDir(m4wdir[1]+':');
  IF ministate THEN miniontop:=(syswindow^.topmost AND istopmost)=istopmost;
  Str(byte(miniontop),s); writeinistring(s_state,'MiniOnTop'+#0,s);
  Str(byte(ministate),s); writeinistring(s_state,'MiniState'+#0,s);
  IF ministate THEN WITH o_rect DO
  BEGIN
    x_mini:=Attr.x; y_mini:=Attr.y;
    Attr.x:=left; Attr.y:=top;
  END;
  IF Attr.x<0 THEN Attr.x:=0; IF Attr.y<0 THEN Attr.y:=0;
  Str(Attr.x,s); writeinistring(s_state,'X_MAIN'+#0,s);
  Str(Attr.y,s); writeinistring(s_state,'Y_MAIN'+#0,s);
                writeinistring(s_state,'LastDir'+#0,currentdir);
                writeinistring(s_state,'LastFMDir'+#0,currentfmdir);
  Str(x_mini,s); writeinistring(s_state,'X_MINI'+#0,s);
  Str(y_mini,s); writeinistring(s_state,'Y_MINI'+#0,s);
  IF Load_state THEN w_mod^.savestate;
  {Falls Hilfe or Mixer noch offen, beenden}
  s:=m4wdir+hlp+#0;
  WinHelp(HWindow,@s[1],help_quit,0);
  IF IsWindow(mixerhandle) THEN PostMessage(mixerhandle,wm_quit,0,0);
  Dispose(w_mod,Done);
  DeleteMyBitMap(m_label);
  IF GetCursor=pCursor THEN SetCursor(LoadCursor(0,idc_arrow));
  DestroyCursor(pCursor);
  FOR j:=1 TO nWMLRects DO BEGIN dec(wmlrects[j][1],capdy); dec(wmlrects[j][3],capdy); END;
  INHERITED Done;
END;

FUNCTION  tsoundwindow.GetClassName:PChar;
BEGIN
  GetClassName:='M4W_MainWindow';
END;

PROCEDURE tsoundwindow.SetupWindow;
VAR s:STRING;
    j:Integer;
BEGIN
  IF miniontop THEN syswindow^.topmost:=syswindow^.topmost OR istopmost;
  INHERITED SetupWindow;
  w_mod^.HWindow:=HWindow;
  {$IFDEF share}
  checkregistered;
  IF NOT Registered THEN
  BEGIN
    w_mod^.about := true;  { to prevent invalid HWindow and invalid HDC in idle action }
    checktime;             { this displays a shareware dialog }
    sharedlg(NIL,-1,id_sharestartlines,id_sharenotes);
    checkregistered;       { see if the registration data have changed }
    w_mod^.about := false; { return - preliminary solution }
  END;
  SetCaption(longver);
  {$ENDIF}
  {$IFDEF DEMO}
  {OurMessageBox(NIL, LongVer, GetResString(SID_DEMO_VERSION), MB_OK OR MB_ICONINFORMATION);{ }
  {$ENDIF}
  AppendMenu(syswindow^.hsysmenu,mf_separator,3333,NIL);
  s:=getresstring(id_menu_modinfo)+#0;
  AppendMenu(syswindow^.hsysmenu,mf_string OR mf_unchecked OR mf_enabled,id_Info,@s[1]);
  s:=getresstring(id_menu_effects)+#0;
  AppendMenu(syswindow^.hsysmenu,mf_string OR mf_unchecked OR mf_enabled,id_effects,@s[1]);

  DragAcceptFiles(HWindow,true); { Schalted das Drag&Drop Feature ein }
  enableallbuttons(true);
  WITH w_mod^ DO IF forcefilenamedisplay THEN pswname^.setpos(1)
                                         ELSE pswname^.setpos(2);
  bhelp^.enable; bsetup^.enable; bmixer^.enable; IF bColor<>NIL THEN bcolor^.enable;
  { define caption-text-rect}
  WITH pcrect DO
  BEGIN
    top:=3;
    bottom:=CapDY;
    IF bColor<>NIL THEN left:=bcolor^.Attr.x+bcolor^.Attr.w+2 ELSE Left:=22;
    right:=bmixer^.Attr.x-2;
  END;
  WITH CaptRect DO BEGIN top := 0; bottom := CapDY; left := 0; right := x_main; END;
  WITH MiniDisplRect DO BEGIN top := CapDY + 3; bottom := top + 32; left := 4; right := left + 42; END;
  Attr.w:=X_Main+GetSystemMetrics(sm_cxborder)+1;
  Attr.h:=Y_Main+18+2*GetSystemMetrics(sm_cyborder);
  FOR j:=1 TO nWMLRects DO BEGIN inc(wmlrects[j][1],capdy); inc(wmlrects[j][3],capdy); END;
  { Focus auf 1. Button}
  oldfocus:=w_mod^.bstop^.HWindow;
  SetFocus(oldfocus);
  {LoadState}
  IF (Load_state) THEN {PostMessage(HWindow,wm_Command,id_LoadState,0);}w_mod^.loadstate;
  IF ministate THEN  SendMessage(HWindow,wm_syscommand,sc_restore,0) ELSE
  BEGIN
    syswindow^.topmost:=syswindow^.topmost XOR canbetopmost;
    WITH Attr DO MoveWindow(HWindow,x,y,w,h,true);
    SetWindowRegion(@self,'MODMAINMASK');
  END;
  IF (w_mod^.Error AND 1)=1 THEN PostMessage(HWindow,wm_Command,bSetup^.GetID,bSetup^.HWindow) ELSE
  IF ParamCount>0 THEN
  BEGIN
    gl_str:='';
    FOR j:=1 TO ParamCount DO gl_str:=gl_str+ParamStr(j)+#0;
    PostMessage(HWindow,wm_newfiletoplay,ParamCount,longint(@gl_str));
  END;
END;

FUNCTION tsoundwindow.CanClose;
BEGIN
  CanClose:=(NOT(w_mod^.dirlist^.entpacke) AND TWindow.CanClose);
END;

PROCEDURE tsoundwindow.WMLButtonDown(VAR msg: tmessage);
VAR i:Integer;
    Found:Boolean;
    rect:trect;
BEGIN
  IF w_mod^.About THEN ELSE
  IF ministate THEN
  BEGIN
    IF PtInRect (MiniDisplRect, TPoint (msg.lparam)) THEN w_mod^.handleinfo ELSE INHERITED WMLButtonDown (msg);
  END ELSE
  BEGIN
    Found:=False;
    FOR i:=1 TO nWMLRects DO IF PtInRect (trect(wmlrects[i]),TPoint(msg.lparam)) THEN BEGIN found:=true; Break; END;
    IF found THEN
    CASE i OF
    1:w_mod^.switchsurround;
    2:w_mod^.switchbpm;
    3:w_mod^.switchpan;
    4:w_mod^.switchido;
    5:w_mod^.switchtmode;
    6:w_mod^.switchqmode;
    8:IF NOT w_mod^.dirlist^.entpacke THEN w_mod^.handleselsong;
    9:PostMessage(HWindow,wm_command,id_shuffle,0);
    10:PostMessage(HWindow,wm_command,id_rep,0);
    11:PostMessage(HWindow,wm_command,id_scan,0);
    12:PostMessage(HWindow,wm_command,id_btntime,0);
    13:w_mod^.handleSelSong;
    14:PostMessage(HWindow,wm_command,id_open,0);
    15:PostMessage(HWindow,wm_command,id_effects,0);
    16:PostMessage(HWindow,wm_command,id_info,0);
    17:PostMessage(HWindow,wm_command,id_setup,0);
     7:BEGIN
        IF w_mod^.Playing AND (w_mod^.Werte.DeviceID = dev_HDD) THEN Exit;
        w_mod^.werte.preamp:=(w_mod^.werte.PreAmp AND $80)+(((wmlrects[7][3]-tpoint(msg.lparam).y)*40) DIV 72)AND $7F;
        w_mod^.drawpreamp(0);
        GetWindowRect(HWindow,rect);
        WITH Rect DO
        BEGIN
          right:=Left+wmlrects[7][2]+1;
          Bottom:=Top+wmlrects[7][3]+1;
          inc(left,wmlrects[7][0]+1);
          inc(Top ,wmlrects[7][1]+1);
        END;
        ClipCursor(@rect);
        SetCursor(0);
        PreAmpClicked := true;
      END;
    END ELSE INHERITED WMLButtonDown (msg);
  END;
  msg.result:=0;
END;

PROCEDURE tsoundwindow.WMLButtonUp(VAR msg: tmessage);
BEGIN
  IF NOT w_mod^.about AND PtInRect (trect(wmlrects[7]),TPoint(msg.lparam)) AND PreAmpClicked THEN
  BEGIN
    w_mod^.werte.preamp:=(w_mod^.werte.PreAmp AND $80)+(((wmlrects[7][3]-tpoint(msg.lparam).y)*40) DIV 72)AND $7F;
    w_mod^.drawpreamp(0);
    mod_settingschanged(w_mod^.werte,msg.wparam);
    ClipCursor(NIL);
    SetCursor(PCursor);
    PreAmpClicked := false;
  END ELSE DefWndProc(msg);
  msg.result:=0;
END;

PROCEDURE tsoundwindow.WMMouseMove;
VAR i:Integer; Found:Boolean;
BEGIN
  Found:=False;
  IF NOT w_mod^.about AND NOT ministate THEN
  FOR i:=1 TO nWMLRects DO IF PtInRect (trect(wmlrects[i]),TPoint(msg.lparam)) THEN BEGIN found:=true; Break; END;
  IF found THEN
  BEGIN
    IF (i=7) AND PreAmpClicked THEN
    BEGIN
       w_mod^.werte.preamp:=(w_mod^.werte.PreAmp AND $80)+(((wmlrects[7][3]-tpoint(msg.lparam).y)*40) DIV 72)AND $7F;
       w_mod^.drawpreamp(0);
       IF w_mod^.Werte.DeviceID<DEV_HDD THEN mod_settingschanged(w_mod^.werte,msg.wparam);
       SetCursor(0);
    END ELSE SetCursor(PCursor);
  END ELSE SetCursor(norm_cursor);
  msg.result:=0;
END;

PROCEDURE tsoundwindow.WMRButtonDown(VAR msg: tmessage);
BEGIN
  IF ministate AND PtInRect (MiniDisplRect, TPoint (msg.lparam))  THEN w_mod^.handleeffects;
  msg.result:=0;
END;

PROCEDURE tsoundwindow.WMLButtonDblclk(VAR msg:tmessage);
VAR i:integer;
BEGIN
  IF NOT PtInRect (CaptRect, TPoint (msg.lparam)) THEN
  IF w_mod^.about THEN w_mod^.handlemodabout ELSE
  FOR i:=1 TO nWMLRects DO IF i<>8 THEN
  IF PtInRect (trect(wmlrects[i]),TPoint(msg.lparam)) THEN
  BEGIN
    WMLButtonDown(msg);
    Exit;
  END;
  IF ministate THEN PostMessage(HWindow,wm_syscommand,sc_maximize,0)
               ELSE PostMessage(HWindow,wm_syscommand,sc_restore,0);
  msg.result:=0;
END;

PROCEDURE tsoundwindow.wmsize;
BEGIN
  INHERITED wmSize(MSG);
  IF MiniState THEN pcrect.right:=bMini^.Attr.X-2 ELSE pcrect.right:=bMixer^.Attr.X-2;
  IF bColor<>NIL THEN pcrect.left:=bcolor^.Attr.x+bcolor^.Attr.w+2 ELSE pcrect.left:=22;
  PaintCaption(0);
END;

PROCEDURE tsoundwindow.WMMove(VAR msg: tmessage);
BEGIN
  IF ColorDlg<>NIL THEN WITH ColorDlg^,Attr DO MoveWindow(HWindow,msg.lparamlo-1,msg.lparamhi-h,w,h,true);
  INHERITED WMMove(Msg);
END;

PROCEDURE tsoundwindow.wmerasebkgnd;
BEGIN
  showmainframe(msg.wparam);
  INHERITED wmerasebkgnd(msg);
END;
PROCEDURE tsoundwindow.wmupdate;    BEGIN w_mod^.RemoveCurrent; END;
PROCEDURE tsoundwindow.wmbox;       BEGIN w_mod^.animatebox; END;
PROCEDURE tsoundwindow.wmactivateApp(VAR msg:tmessage);
BEGIN
{  IF Bool(msg.wparam) THEN mod_SampleForeGnd ELSE mod_SampleBackGnd;{ }
  INHERITED WmActivateApp(Msg);
END;

PROCEDURE tsoundwindow.wmAbout(VAR Msg:tMessage);
BEGIN
  w_mod^.aboutlauf.wmAbout;
END;
{***************************************Hauptprogramm!!!!**********************************}
TYPE pmodplay=^tmodplay;
     tmodplay=object(Tbuzapplication)
     PROCEDURE InitMainWindow; VIRTUAL;
     END;

PROCEDURE tmodplay.InitMainWindow;
BEGIN
  INHERITED InitMainWindow;
  MainWindow:=New(Psoundwindow,Init(NIL,longver));
END;

{$IFDEF DATELIMIT}
VAR Year, Month, Day, DOW: word;
{$ENDIF}

VAR thisrun:Tmodplay;
    pc:ARRAY[0..255] OF char;
    ps:STRING ABSOLUTE pc;
    i,j:word;
LABEL Restrt;
BEGIN
 {
 ExOs:=Ex_OldWin;
 OS:=Windows_31;
 { }
 TryToUsePalette := True;
 LoadString(hInstance,32000,pc,256);
 DefDrawFocus:=StrComp(pc,'DO_NOT_DRAW_THE_FOCUS')<>0;
 IF (GetWinFlags AND wf_enhanced)<>wf_enhanced THEN
 BEGIN
   LoadString(hLanguage,id_noenhanced,pc,256);
   MessageBox(0,pc,errorver,MB_OK OR MB_Iconhand);
   Halt(0);
 END;
 {$IFDEF CDROM}
 ps := ParamStr(0);
 {ps[1] := 'd';}
 IF GetDriveType (Ord (UpCase (ps[1])) - Ord ('A')) <> drive_remote THEN BEGIN
   MessageBox (0, 'Sorry!'#$0d#$0a#$0d#$0a'This version of ' + ProjectName + ' must'#$0d#$0a +
     'be run from a CD-ROM drive.',modc.ver, mb_ok OR mb_iconstop);
   Halt(0);
 END;
 {$ENDIF}
 {$IFDEF ATPRO}
 Port[$390] := $15;
 IF Port[$391] <> $71 THEN BEGIN
   MessageBox (0, 'Sorry!'#$0d#$0a#$0d#$0a'This version of ' + ProjectName + #$0d#$0a +
     'is for AudioTrix Pro only.',modc.ver, mb_ok OR mb_iconstop);
   Halt(0);
 END;
 {$ENDIF}
 {$IFDEF DATELIMIT}
 GetDate (Year, Month, Day, DOW);
 IF (Year > 1995) OR (Year = 1995) AND (Month > 6) THEN BEGIN
   MessageBox (0, 'Sorry!'#$0d#$0a#$0d#$0a'This preview version of ' + ProjectName + ' has expired.'#$0d#$0a +
     'Please contact JSInc. for an updated version.',ErrorVer, mb_ok OR mb_iconstop);
   Halt(0);
 END;
 {$ENDIF}
 { Noch eine Instanz von Mod4Win aktiv? }
 IF (hprevinst<>0) THEN
 BEGIN
   i:=FindWindow('M4W_MainWindow',NIL);
   IF NOT IsIconic(i) THEN SetWindowPos(i,Hwnd_Top,0,0,0,0,SWP_NoSize OR SWP_ShowWindow);
   IF (i<>0)AND(ParamCount>0) THEN
   BEGIN
     { Ja! ==> Parameter übergeben und Quit! }
     gl_str:='';
     FOR j:=1 TO ParamCount DO gl_str:=gl_str+ParamStr(j)+#0;
     strpcopy(pc,gl_str);
     SendMessage(i,wm_newfiletoplay,ParamCount,longint(@gl_str));
   END;
   Halt(0);
 END;
 IF GetFreeSpace(0)<500000 THEN
 BEGIN
   LoadString(hLanguage,id_nomem,pc,256);
   MessageBox(0,pc,errorver,MB_OK OR MB_Iconhand);
 END;
ReStrt:
 CASE CurrLanguage OF
 _ITA:StrCopy(CriticalErrorName, ProjectName + ' ha compromesso l'' integrità del sistema ed è stato chiuso!'+#10+
                                 'E'' stato tentato tutto il possibile per una terminazione sicura ma probabilmente alcune'#10+
                                 'risorse rimangono statiche in memoria e il sistema potrebbe diventare instabile!');

 _SPA:
   StrCopy(CriticalErrorName, '¡' + ProjectName + ' intentó hacer alguna maldad a su sistema y fue cerrado!'+#10+
                    'Hemos hecho lo posible para que la salida del programa sea segura, pero algunos recursos pueden'+#10+
                    'haberse quedado en su memoria y su sistema puede volverse inestable');
 _GRK:
   StrCopy(CriticalErrorName,'Ôï ' + ProjectName + ' ðñïóðÜèçóå íá êÜíåé êÜôé Üó÷çìï óôï óýóôçìÜ óáò êáé ôåñmáôßóôçêå!'+#10+
                             'ÊÜíáìå üôé ìðïñïýóáìå ãéá áóöáëÞ ôåñìáôéóìü áëëÜ ìåñéêïß ðüñïé ðéèáíüí'+#10+
                             'íá ðáñáìåßíïõí óôáôéêïß óôç ìíÞìç êáé ôï óýóôçìá íá ãßíåé áóôáèÝò!');
 _GER:
   StrCopy(CriticalErrorName, ProjectName + ' war ein sehr böses Programm und musste deshalb geschlossen werden!'+#10+
                             'Wir haben versucht, das Programm sauber zu beenden. Einige Ressourcen bleiben'+#10+
                             'jedoch in Ihrem Speicher und Ihr System könnte jetzt unstabil werden.');
 _FRE:
   StrCopy(CriticalErrorName, ProjectName + ' a été en conflit avec  votre système et a été fermé.'+#10+
                             'Toutefois, des ressources peuvent encore résider en'+#10+
                             'mémoire et votre système peut devenir instable.');
 _NL:
   StrCopy(CriticalErrorName, ProjectName + ' probeerde iets raars met uw systeem te doen en is afgesloten!'+#10+
                             'We hebben ons best gedaan om alles veilig af te sluiten, maar sommige'+#10+
                             'resources kunnen achteblijven in het geheugen en ue systeem instabiel'+#10+
                             'maken. U doet er goed aan uw systeem opnieuw te starten!');
 _CZE:
   StrCopy(CriticalErrorName, ProjectName + ' se pokusil provest neco hrozneho se systemem a byl ukoncen!'+#10+
                              'Udelali jsme vse pro hladky konec, ale nektere prostredky asi zustanou'+#10+
                              'v pameti a system se muze stat nestabilnim!');
 ELSE
   StrCopy(CriticalErrorName, ProjectName + ' tried to do something awful with your system and was closed!'+#10+
                             'We did our best for a safe termination but some resources will probably'+#10+
                             'remain static in your memory and your system could get unstable!');
 END;
 thisrun.Init(longver);
 thisrun.Run;
 thisrun.Done;
 IF Restart THEN BEGIN ReleaseLanguage; LoadLanguage; Restart:=False; GOTO Restrt END;
END.
