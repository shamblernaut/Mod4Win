UNIT newopen;
{ $C Fixed Preload Permanent}
{$R newopen.res}

INTERFACE
USES
     owindows,odialogs,winprocs,strings,wintypes,windos,win31,objects,
     winunit,modc,omemory,files;

CONST faFiles=$37;

TYPE PPIFREC=^TPIFREC;
     TPIFREC=RECORD
    {0..1}    fill1  :ARRAY[0..1] OF byte;
    {2..32}   PIFNAME:ARRAY[0..29] OF char;   {LEN 30 -wenn kuerzer + #0}
    {33..37}  fill2  :ARRAY[0..3] OF byte;
    {38..100} EXECNAME:ARRAY[0..61] OF char;  {LEN 61 - wenn kuerzer+ #0}
    {101..164}startdir:ARRAY[0..62] OF char;
              fill3   :ARRAY[0..292] OF byte;
    {453..515}prgparm :ARRAY[0..62] OF char;
     END;

  CONST itemlength        =15;
        {$IFDEF DEMO}
        maxplaylistentries=   5;
        OldMaxEntries     =2999;
        {$ELSE}
        maxplaylistentries=2999;
        {$ENDIF}
        MaxArcListItems   =2999;

        songlistentriesize=13; {1:Filetype 2:Playstatus 3:dirindex 4:arcindex 5 arcdirindex 6..13:name}
        maxdirlistentries =200;
        maxarcdirentries  =200;
        maxarclistentries =200;
        showpack:boolean  =false;
        dldirtype         =#31;
        dlfloppytype      =#50;
        dlhdtype          =#51;
        dlremotetype      =#52;
        dlarcoffset       =32;
        allfiletypes      =20;

        allarctypes       =3;
        arjfiletype       =1;
        zipfiletype       =2;
        lharcfiletype     =3;
        listfiletype      =4;
        modfiletype       =5;
        nstfiletype       =6;
        oktfiletype       =7;
        _669filetype      =8;
        pt8filetype       =9;
        stmfiletype       =10;
        mtmfiletype       =11;
        farfiletype       =12;
        ultfiletype       =13;
        s3mfiletype       =14;
        isitdirectory     =true;
        marksong          =false;
        noarcconst        =#255;
        indexoffset       =31;
        filetypesasstring: ARRAY[1..allfiletypes] OF STRING[3]=
('arj','zip','lzh','mol','mod','nst','okt','669','wow','stm','mtm','far','ult','s3m','dmf','xm','wav','mid','ams','it');
        filetypeenabled  : ARRAY[1..allfiletypes] OF boolean=
(true, true, true, true, true, true, true, true, true, true, true, true, False, true, False,true,false,false,False,False);

        defextractstring : ARRAY[1..allarctypes] OF STRING[20]=
                          ('e -e -va -y -p -p1','-o -j','e -p -m -a -n');
        defArcdelstring  : ARRAY[1..allarctypes] OF STRING[20]=
                          ('d -va -y -p','-d -whs','d -p -m');
        defArcCopystring : ARRAY[1..allarctypes] OF STRING[20]=
                          ('u -y','-a -P -whs','a -p -m -i -l -x');


 {opendialog-ID'S}
        id_OAdd   =600;
        id_ODEL   =602;
        id_OOK       =604;
        id_OCancel   =605;
        id_Ohelp     =606;
        id_OSave     =607;
        id_Pathname  =609;
        id_filelist  =610;
        id_dirlist   =611;
        id_playlist    =612;
        id_filenameedit=613;
        id_leftstatic  =614;
        id_rightstatic =615;
        id_infostatic=616;
        id_olplus=617;
        id_olminus=618;
        id_olmal=619;
        id_orplus=620;
        id_orminus=621;
        id_ormal=622;
        id_leftsel=623;
        id_rightsel=624;

{stringids}
       enter_id=50; arj_id=52; zip_id=51; lha_id=53; mol_id=54; leavearc_id=55;
       leavearcdir_id=56; leavemol_id=57; leavedir_id=58; dir_id=59;
       arcdir_id=60; floppy_id=61; hd_id=62; remote_id=63; nofileinplaylist_id=64;
       novalidfiles_id=65; allfilesinplaylist_id=66; add_id=67; del_id=68;
       ok_id=69; quit_id=70; help_id=71; save_id=72; handlesave_id=73;
       filesel_id=74; filedesel_id=75; fileinvsel_id=76; playsel_id=77;
       playdesel_id=78; playinvsel_id=79; selmask_id=80; id_askcreatedir=81;
       fileopenerror_id=82; filecopy_id=83; askoverwrite_id=84; writeerroropen_id=85;
       nomem_id=86; readerror_id=87; writeerror_id=88; fileinlist_id=89;
       askerase1_id=90; askerase2_id=91; fromarc_id=92; erasefile_id=93; eraseerror_id=94;
       listsaved_id=95; f5_id=49; id_archivernotfound=48; id_badinput=47;

VAR     aktarc:STRING[20];
        jumps:longint;
        arcpackexecs,arcunpackexecs:ARRAY[1..allarctypes] OF STRING[100];
        Arcextractstring,arcdelstring,arccopystring: ARRAY[1..allarctypes] OF STRING[30];
        gl_str:STRING;
TYPE  tliste=ARRAY[0..maxplaylistentries] OF STRING[songlistentriesize];
      pfound=^tfound;
      tfound=ARRAY[0..MaxArcListItems{maxplaylistentries}] OF boolean;
      pbuffer=^tbuffer;
      tbuffer=ARRAY[0..MaxArcListItems{maxplaylistentries}] OF integer;
      pDynFound=^tDynFound;
      tDynFound=ARRAY[0..0] OF Boolean;

TYPE pseledit=^tseledit;
     tseledit=object(tmykfqedit)
       Alist:pmylistbox;
       tofs :byte;
       lastfocus:hwnd;
       selchar:char;
       CONSTRUCTOR Init(Aparent:Pwindowsobject; anid:integer; atitle:pchar; x,y,w,h:integer;
                   atextlen:word; ownfont:boolean; alistbox:pmylistbox; searchoffset:byte; firstchar:char);
       PROCEDURE SetupWindow; VIRTUAL;
       FUNCTION dosel(zeichen:char):boolean;
       PROCEDURE wmchar(VAR msg:tmessage); VIRTUAL wm_first+wm_char;
       PROCEDURE wmkeydown(VAR msg:tmessage); VIRTUAL wm_first+wm_keydown;
       PROCEDURE wmsyskeydown(VAR msg:tmessage);       VIRTUAL wm_first+wm_syskeydown;
       PROCEDURE wmsetfocus(VAR msg:tmessage); VIRTUAL wm_first+wm_setfocus;
       PROCEDURE wmkillfocus(VAR msg:tmessage); VIRTUAL wm_first+wm_killfocus;
     END;

TYPE PFEdit=^TFEdit;
     TFEdit=object(tmyedit)
       WindowPtr:PWord;
       CONSTRUCTOR Init(Aparent:Pwindowsobject; anid:integer; x,y,w,h:integer;
                   atextlen:word; AWindowPtr:PWord);
       DESTRUCTOR Done; Virtual;
       PROCEDURE SetupWindow; VIRTUAL;
       PROCEDURE wmkeydown(VAR msg:tmessage); VIRTUAL wm_first+wm_keydown;
     END;

TYPE pdirs=^Tdirs;
     TDirs=ARRAY[0..maxdirlistentries] OF STRING[80];

TYPE pmydirs=^tmydirs;
     tmydirs=object
     lastunpackedsong:STRING[100];
     lastupindex  :word;
     Limit        :Word;
     newlist      :boolean;
     dircount     :integer;
     songcount    :integer;
     arccount     :integer;
     arcdircount  :integer;
     psonglist    :^tliste;
     entpacke     :boolean;
     {$IFDEF DEMO}
     SongArr      : tListe;
     {$ENDIF}
     dirs         :TDirs;
     arcs         :ARRAY[1..maxarclistentries] OF STRING[14]; {1 dirindex 2: arctype 3..14 Name}
     arcdirs      :ARRAY[1..maxarcdirentries] OF STRING[80];
     CONSTRUCTOR Init;
     DESTRUCTOR  Done; VIRTUAL;
     PROCEDURE   ClearList;
     PROCEDURE   deletedirstring(At:integer);
     FUNCTION    getdirstring(At:integer):STRING;
     FUNCTION    Adddirstring(Name:STRING):integer;
     FUNCTION    setdirstring(At:integer; Name:STRING):integer;
     PROCEDURE   CompactList;
     FUNCTION    pathindirlist(path:STRING):integer;
     FUNCTION    isdirindexinsonglist(wo:integer):boolean;
     FUNCTION    getsongstring(index:word):STRING;
     FUNCTION    addsongstring(codename:STRING):WordBool;
     PROCEDURE   deletesongstring(index:word);
     FUNCTION    isinloadlist(Name:STRING; startindex:integer):integer;
     FUNCTION    getfullname(aparent:pwindowsobject; index:word; donothing,parentmessage:boolean):STRING;
     FUNCTION    getname(index:word):STRING;
     PROCEDURE   setattribut(index:word; what:char);
     FUNCTION    getattribut(index:word):char;
     FUNCTION    IsValid(dir:boolean; t:tsearchrec; VAR filetype:byte):boolean;
     FUNCTION    isvalidarchive(t:tsearchrec; VAR filetype:byte):boolean;
     PROCEDURE   deletearcstring(At:integer);
     FUNCTION    getarcstring(At:integer):STRING;
     FUNCTION    Addarcstring(Name:STRING):integer;
     FUNCTION    isarcindexinsonglist(wo:integer):boolean;
     FUNCTION    archiveinarclist(AnArchive:STRING; mitdir:boolean):integer;
     PROCEDURE   deletearcdirstring(At:integer);
     FUNCTION    getarcdirstring(At:integer):STRING;
     FUNCTION    Addarcdirstring(Name:STRING):integer;
     FUNCTION    isarcdirindexinsonglist(wo:integer):boolean;
     FUNCTION    arcdirinarcdirlist(AnArcDir:STRING):integer;
     PROCEDURE   TransferData(destlist:pmydirs);
     FUNCTION    openlistfile(Parent:pwindowsobject; VAR f:text; Name:STRING):boolean;
     FUNCTION    sauglist(Parent:pwindowsobject; Name:STRING):boolean;
     PROCEDURE   from2to1(destlist:pmydirs; pFoundArr:pDynFound);
     FUNCTION    from2to1_entrie(destlist:pmydirs; VAR Name:STRING):boolean;
     FUNCTION    checkfrom2to1_entrie(destlist:pmydirs; Name:STRING):integer;
     PROCEDURE   checklist(Parent:pwindowsobject; Name:STRING; errid:integer);
     FUNCTION    saugarc(dir,Name:STRING; alldirs:boolean):boolean;
     FUNCTION    saugdrive(Name:STRING):Integer;
     PROCEDURE   grillunpackedsong;
     FUNCTION    unpack(execstring,temppath:STRING):boolean;
     FUNCTION    copyfile(box:pwaitbox; listindex:word;
                 sourcename,fullname,scarcdir,dsarcdir:STRING; scarctype,dsarctype:byte):boolean;
     FUNCTION    erasefile(aparent:pwindowsobject; index:word; ask:boolean):boolean;
     FUNCTION    gettemppath:STRING;
     FUNCTION    savelist(aparent:pwindowsobject; Name:STRING; ask:boolean):boolean;
    END;

TYPE pmydirsARC=^tmydirsARC;
     tmydirsARC=object(tMYDIRS)
       CONSTRUCTOR Init(aLimit:Word);
       DESTRUCTOR  Done;                                                VIRTUAL;
     END;


TYPE p_browse=^tbrowse;
     tbrowse=object(tmywindow)
            {Fensterpositionen der Childs}
       x_browse,y_browse,
       X_bOok,Y_bOok,
       X_bOhelp,Y_bOhelp,
       X_bOquit,Y_bOquit,
       X_pathname,Y_pathname,dx_pathname,
       X_filelist,Y_filelist,dx_filelist,dy_filelist,
       X_dirlist,Y_dirlist,dx_dirlist,dy_dirlist:integer;
       {aktueller Pfad}
       path:STRING[150];
       exe_name:STRING[20];
       {Pointer auf die Listboxen}
       pfilelist,pdirlist:pmylistbox;
       {Pointer auf Editbox}
       ppathedit:pmyedit;
       {pointer auf Statics zur Betitelung der childs}
       comments:ARRAY[1..3] OF pmystatic;
       INFOstatic:pmystatic;
       {buttons r}
       bOHelp,book,bOquit:pBmpButton;
       {bitmaps}
       p_edit:pmyedit;
       drivebmphandle:hbitmap;
       CONSTRUCTOR Init(aparent:pwindowsobject; anedit:pmyedit);
       DESTRUCTOR Done; VIRTUAL;
       PROCEDURE SetupWindow; VIRTUAL;
       PROCEDURE wmEraseBkGnd(VAR msg:tmessage);     VIRTUAL WM_FIRST+WM_EraseBkGnd;
       PROCEDURE wmdrawitem(VAR msg:tmessage);       VIRTUAL wm_first+wm_drawitem;
       PROCEDURE wmkeydown(VAR msg:tmessage);        VIRTUAL wm_first+wm_keydown;
       PROCEDURE wmsyschar(VAR msg:tmessage);        VIRTUAL wm_first+wm_syschar;
       PROCEDURE wmsyskeydown(VAR msg:tmessage);     VIRTUAL wm_first+wm_syskeydown;
       PROCEDURE WMCommand(VAR msg:tmessage);        VIRTUAL wm_first+wm_command;
       PROCEDURE wmIHaveTheFocus(VAR msg:tmessage);  VIRTUAL wm_first+wm_Ihavethefocus;
       PROCEDURE handleOK(VAR msg:tmessage);         VIRTUAL id_first + id_OOK;
       PROCEDURE handleHilfe(VAR msg:tmessage);      VIRTUAL id_first + id_Ohelp;
       PROCEDURE handleCancel(VAR msg:tmessage);     VIRTUAL id_first + id_OCancel;
       PROCEDURE setinfo(box_id:hwnd);
       PROCEDURE updatedirlist;
     END;

TYPE p_DirBrowse=^tDirBrowse;
     tDirBrowse=OBJECT(tmywindow)
       {aktueller Pfad}
       path:STRING[150];
       {Pointer auf die Listboxen}
       pdirlist:pmylistbox;
       {Pointer auf Editbox}
       ppathedit:pmyedit;
       {pointer auf Statics zur Betitelung der childs}
       comments:ARRAY[1..3] OF pmystatic;
       INFOstatic:pmystatic;
       {buttons r}
       bOHelp,book,bOquit:pBmpButton;
       {bitmaps}
       p_edit:pmyedit;
       drivebmphandle:hbitmap;
       CONSTRUCTOR Init(aparent:pwindowsobject; anedit:pmyedit);
       DESTRUCTOR Done; VIRTUAL;
       PROCEDURE SetupWindow; VIRTUAL;
       PROCEDURE wmEraseBkGnd(VAR msg:tmessage);     VIRTUAL WM_FIRST+WM_EraseBkGnd;
       PROCEDURE wmdrawitem(VAR msg:tmessage);       VIRTUAL wm_first+wm_drawitem;
       PROCEDURE wmkeydown(VAR msg:tmessage);        VIRTUAL wm_first+wm_keydown;
       PROCEDURE wmsyschar(VAR msg:tmessage);        VIRTUAL wm_first+wm_syschar;
       PROCEDURE wmsyskeydown(VAR msg:tmessage);     VIRTUAL wm_first+wm_syskeydown;
       PROCEDURE WMCommand(VAR msg:tmessage);        VIRTUAL wm_first+wm_command;
       PROCEDURE wmIHaveTheFocus(VAR msg:tmessage);  VIRTUAL wm_first+wm_Ihavethefocus;
       PROCEDURE handleOK(VAR msg:tmessage);         VIRTUAL id_first + id_OOK;
       PROCEDURE handleHilfe(VAR msg:tmessage);      VIRTUAL id_first + id_Ohelp;
       PROCEDURE handleCancel(VAR msg:tmessage);     VIRTUAL id_first + id_OCancel;
       PROCEDURE setinfo(box_id:hwnd);
       PROCEDURE updatedirlist;
     END;

TYPE popen=^open;
     open=object(tmywindow)
       SaveListWindow:HWnd;
       {Fensterpositionen der Childs}
       x_open,y_open,X_pathname,Y_pathname,X_bOhelp,Y_bOhelp,
       X_bOSave,Y_bOSave,X_bOOk,Y_bOOk,X_bOCancel,Y_bOCancel,
       x_boaddone,y_boaddone,x_bodeleteone,y_bodeleteone,X_filelist,
       Y_filelist,dx_filelist,dy_filelist,X_dirlist,Y_dirlist,
       dx_dirlist,dy_dirlist,X_playlist,Y_playlist,dx_playlist,
       dy_playlist,dx_pathname:integer;
       {aktueller Pfad}
       path:STRING[80];
       {Puffer für alte Liste}
       NewList:pmydirs;
       inputlist:pmydirs;
       {Pointer auf die Listboxen}
       pfilelist,pdirlist,pplaylist:pmylistbox;
       {Pointer auf Editbox}
       ptempedit:pmykfqedit;
       {pointer auf Pfadnamenanzeige}
       pathname:pmystatic;
       {pointer auf Statics zur Betitelung der childs}
       comments:ARRAY[1..4] OF pmystatic;
       leftstatic,rightstatic,INFOstatic:pmystatic;
       {buttons im Openfenster}
       bOOK,bOHelp,bOSave,bOCancel,
       bOADD,bODel,bolplus,bolminus,bolmal,borplus,borminus,bormal :pBmpButton;
       {Focus vor alt-f1,f2..}
       previousfocus:hwnd;
       {bitmaps}
       bmphandles:ARRAY[1..allfiletypes] OF hbitmap;
       arcbmphandle:hbitmap;
       drivebmphandle:hbitmap;
       {MOL+ARC-Listendaten}
       aktliste:STRING;
       sauglistobj:pmydirs;
       {FILE-COUNT}
       realfilecount:word;
       {selection}
       selectionid:integer;
       aktarcliste:STRING; {kompletter Name}
       aktarcdir:STRING;
       lastfocus:hwnd;
       CONSTRUCTOR Init(aparent:pwindowsobject; Alist:pmydirs);
       DESTRUCTOR Done;                             VIRTUAL;
       PROCEDURE SetupWindow;                       VIRTUAL;
       PROCEDURE wmEraseBkGnd(VAR msg:tmessage);    VIRTUAL WM_FIRST+WM_EraseBkGnd;
       PROCEDURE wmctlcolor(VAR msg:tmessage);      VIRTUAL wm_first + wm_ctlcolor;
       PROCEDURE wmdrawitem(VAR msg:tmessage);      VIRTUAL wm_first + wm_drawitem;
       PROCEDURE handleAddOne;                      VIRTUAL;
       PROCEDURE handleAddAll;                      VIRTUAL;
       PROCEDURE handleDeleteOne(VAR msg:tmessage); VIRTUAL id_first + id_ODel;
       PROCEDURE handleDeleteAll;
       PROCEDURE handleOK(VAR msg:tmessage);        VIRTUAL id_first + id_OOK;
       PROCEDURE handleHilfe(VAR msg:tmessage);     VIRTUAL id_first + id_Ohelp;
       PROCEDURE handleCancel(VAR msg:tmessage);    VIRTUAL id_first + id_OCancel;

       PROCEDURE handleListSave(VAR msg:tmessage);  VIRTUAL id_first + id_Osave;
       PROCEDURE DestroySaver(WFOC:Boolean);
       PROCEDURE CreateSaver;
       PROCEDURE handlelPLUS(VAR msg:tmessage);     VIRTUAL id_first + id_Olplus;
       PROCEDURE handlerPLUS(VAR msg:tmessage);     VIRTUAL id_first + id_Orplus;
       PROCEDURE handlelMINUS(VAR msg:tmessage);    VIRTUAL id_first + id_Olminus;
       PROCEDURE handlerMINUS(VAR msg:tmessage);    VIRTUAL id_first + id_Orminus;
       PROCEDURE handlelMAL(VAR msg:tmessage);      VIRTUAL id_first + id_Olmal;
       PROCEDURE handlerMAL(VAR msg:tmessage);      VIRTUAL id_first + id_Ormal;
       PROCEDURE doinvselection(Alistbox:PMYlistbox);
       PROCEDURE saveliste(Name:STRING);
       PROCEDURE handleselection(ctrlid:word; tastcode:word);
       PROCEDURE wmselection(VAR msg:tmessage); VIRTUAL wm_first+wm_selection;
       PROCEDURE setinfo(box_id:hwnd);
       PROCEDURE WMCommand(VAR msg:tmessage); VIRTUAL wm_first+wm_command;
       PROCEDURE updatedirlist;
       PROCEDURE splitplaylistitem(VAR nc:STRING; VAR dirwert,arcindex,arcdirwert:byte);
       PROCEDURE uniteplaylistitem(aparent:pmydirs; VAR nc:STRING; modifypath:boolean);
       FUNCTION  isvalidarcdir(VAR Name:STRING):boolean;
       PROCEDURE loadlist(Name:STRING);
       PROCEDURE loadarc(Name:STRING);
       PROCEDURE loaddrive(Name:STRING);
       PROCEDURE wmkeydown(VAR msg:tmessage);        VIRTUAL wm_first+wm_keydown;
       PROCEDURE wmIHaveTheFocus(VAR msg:tmessage);  VIRTUAL wm_first+wm_Ihavethefocus;
       PROCEDURE wmsyschar(VAR msg:tmessage);       VIRTUAL wm_first+wm_syschar;
       PROCEDURE wmsyskeydown(VAR msg:tmessage);     VIRTUAL wm_first+wm_syskeydown;
       PROCEDURE wmdropfiles(VAR msg:tmessage);     VIRTUAL wm_first+wm_internaldrop;
       PROCEDURE setlbredraw(how:byte);
     END;



IMPLEMENTATION

CONSTRUCTOR TFEdit.Init;
BEGIN
  INHERITED Init(AParent,anid,NIL,x,y,w,h,ATextLen,True);
  WindowPtr:=AWindowPtr;
END;

DESTRUCTOR TFEdit.Done;
BEGIN
  Windowptr^:=0;
  INHERITED Done;
END;

PROCEDURE TFEdit.SetupWindow;
BEGIN
  INHERITED SetupWindow;
  WindowPtr^:=HWindow;
  Focus;
END;

PROCEDURE tFedit.wmkeydown;
BEGIN
  msg.result:=0;
  IF keystate<>0 THEN DefWndProc(msg) ELSE
  CASE msg.wparam OF
    vk_f1,vk_help,vk_escape,vk_return,vk_execute:
      PostMessage(Parent^.HWindow,wm_keydown,msg.wparam,(0 SHL 16)+Attr.id);
    ELSE DefWndProc(msg);
  END;
END;

{***********************************************OPEN********************************************************}
PROCEDURE open.setlbredraw(how:byte);
BEGIN
  SendMessage(pfilelist^.HWindow,wm_setredraw,how,0);
  SendMessage(pdirlist^.HWindow,wm_setredraw,how,0);
  SendMessage(pplaylist^.HWindow,wm_setredraw,how,0);
END;

PROCEDURE open.wmdropfiles;
BEGIN
  CASE GetDlgCtrlID(msg.wparam) OF
    id_playlist:
      BEGIN
        CASE GetDlgCtrlID(msg.lparamlo) OF
          id_dirlist:
            BEGIN
              lastfocus:=pdirlist^.HWindow;
              PostMessage(HWindow,wm_keydown,vk_f5,id_dirlist);
            END;
          id_filelist:handleaddone;
        END;
      END;
    id_filelist:IF GetDlgCtrlID(msg.lparamlo)=id_playlist
                THEN handledeleteone(Msg)
                ELSE IF GetDlgCtrlID(msg.lparamlo)=id_dirlist
                THEN PostMessage(pdirlist^.HWindow,wm_keydown,vk_return,0);
  END;
  msg.result:=0;
END;

PROCEDURE open.wmkeydown;
VAR s:STRING[20];
    i,j:integer;
BEGIN
  msg.result:=0;
  IF (msg.wparam=Lo(VkKeyScan(word('\'))))AND(keystate=1)THEN
  BEGIN
    s:=dldirtype+'<..>'+#0; pdirlist^.SetSelString(@s[1],-1);
    IF aktarcliste<>'' THEN
    BEGIN aktarcdir:=''; updatedirlist; END ELSE
    IF aktliste<>'' THEN BEGIN updatedirlist; END ELSE
    BEGIN
      IF path[0]>#3 THEN BEGIN path[0]:=#3; updatedirlist; END;
      pdirlist^.SetSelIndex(0);
    END;
  END ELSE
  CASE msg.wparam OF
    vk_return,vk_execute:IF (msg.lparamlo=id_leftstatic)OR(msg.lparamlo=id_rightstatic)
                         THEN  handleselection(msg.lparamlo,vk_return) ELSE
                         IF (msg.lparamlo=id_filenameedit) THEN handlelistsave(Msg)
                         ELSE handleok(Msg);
    vk_f1,vk_help       :handlehilfe(Msg);
    vk_escape           :IF (msg.lparamlo=id_leftstatic)OR(msg.lparamlo=id_rightstatic)
                         THEN handleselection(msg.lparamlo,vk_escape) ELSE
                         IF (msg.lparamlo=id_filenameedit)THEN DestroySaver(true)
                         ELSE handleCancel(Msg);
    vk_prior            :IF keystate=1 THEN
                         BEGIN
                           s:=dldirtype+'<..>'#0;
                           IF pdirlist^.SetSelString(pchar(@s[1]),-1)>=0 THEN updatedirlist;
                           IF pdirlist^.GetCount>0 THEN pdirlist^.focus;
                         END;
    vk_next             :IF keystate=1 THEN
                         BEGIN
                           IF (s[2]<>'.')AND(s[3]<>'.')AND(s[1]<>'[') THEN
                           IF pdirlist^.GetSelString(s,0)>0 THEN updatedirlist;
                         END;
    vk_right            :IF keystate=0 THEN handleaddone ELSE handleaddall;
    vk_left             :IF keystate=0 THEN handledeleteone(Msg) ELSE handledeleteall;
    vk_f5               :IF (msg.lparamlo=id_filelist) THEN handleaddone ELSE
                         IF (msg.lparamlo=id_playlist) THEN handledeleteone(Msg) ELSE
                         BEGIN
                           pdirlist^.GetSelString(s,20);
                           IF (s[1]=dldirtype)OR(s[1]IN[dlfloppytype..dlremotetype])
                           THEN BEGIN IF Copy(s,2,4)<>'<..>' THEN loaddrive(s); END
                           ELSE IF s[1]=char(listfiletype+dlarcoffset)
                           THEN loadlist(path+'\'+Copy(s,3,byte(s[0])))
                           ELSE IF byte(s[1])IN[arjfiletype+dlarcoffset..lharcfiletype+dlarcoffset]
                           THEN loadarc(Copy(s,3,byte(s[0])))
                         END;
    vk_add              :IF pfilelist^.HWindow=GetFocus
                         THEN SendMessage(HWindow,wm_selection,id_olplus,longint(msg.wparam))
                         ELSE IF pplaylist^.HWindow=GetFocus
                         THEN SendMessage(HWindow,wm_selection,id_orplus,longint(msg.wparam));
    vk_subtract         :IF pfilelist^.HWindow=GetFocus
                         THEN SendMessage(HWindow,wm_selection,id_olminus,longint(msg.wparam))
                         ELSE IF pplaylist^.HWindow=GetFocus
                         THEN SendMessage(HWindow,wm_selection,id_orminus,longint(msg.wparam));
    vk_multiply         :IF pfilelist^.HWindow=GetFocus THEN doinvselection(pfilelist)
                           ELSE IF pplaylist^.HWindow=GetFocus THEN doinvselection(pPlaylist);
     ELSE CASE msg.wparam OF
         word(#32)..word(#255):
                         IF (msg.lparamlo=id_filelist)OR(msg.lparamlo=id_playlist) THEN
                         BEGIN
                           IF msg.lparamlo=id_filelist
                           THEN
                           BEGIN
                             IF (aktliste='')THEN i:=1 ELSE i:=4;
                             ptempedit:=New(pseledit,Init(@self,id_leftsel,'',x_filelist-3+3*Fh,
                                        CapDY+Y_filelist+dy_filelist+10,dx_filelist-4*Fw,Fh,13,true,
                                        pfilelist,i,char(msg.wparam)));
                             Application^.MakeWindow(ptempedit);
                             leftstatic^.SetText('Sel?');
                           END
                           ELSE IF msg.lparamlo=id_playlist
                           THEN
                           BEGIN
                             ptempedit:=New(pseledit,Init(@self,id_rightsel,'',x_playlist-3+3*Fh,
                                        CapDY+Y_playlist+dy_playlist+10,dx_playlist-4*Fw,Fh,13,true,
                                        pplaylist,4,char(msg.wparam)));
                             Application^.MakeWindow(ptempedit);
                             rightstatic^.SetText('Sel?');
                           END;
                           ptempedit^.focus;
                         END ELSE
                           IF msg.lparam=id_dirlist THEN
                           FOR i:=0 TO pdirlist^.GetCount-1 DO
                           BEGIN
                             pdirlist^.GetString(s,i);
                             IF (s[2]='[')AND(UpCase(s[4])=char(msg.wparam)) THEN
                             BEGIN
                               IF pdirlist^.setsel(i,_on)<>LB_ERR
                               THEN updatedirlist;
                               msg.result:=0;
                               Exit;
                             END;
                           END ELSE IF msg.wparam=$53 THEN handlelistsave(Msg);
         ELSE DefWndProc(msg);
     END;
   END;
END;

{Teilt einen PLAYLISTEINTRAG in NAmen und DIR-INDEX auf-benutzt bei PLAYLIST->FILELIST}
PROCEDURE open.splitplaylistitem;
BEGIN
  dirwert:=byte(nc[3]); arcindex:=byte(nc[4]); arcdirwert:=byte(nc[5]);
  nc:=nc[1]+Copy(nc,6,8);
  WHILE Pos(' ',nc)<>0 DO system.Delete(nc,Pos(' ',nc),1);
END;
{formatiert einen String fuer die Playliste - String kommt aus FILELISTBOX}
PROCEDURE open.uniteplaylistitem(aparent:pmydirs; VAR nc:STRING; modifypath:boolean);
VAR s:STRING[20];
    pathindex,arcindex,arcdirindex:integer;
BEGIN
  pathindex:=aparent^.pathindirlist(path);
  IF modifypath THEN  IF pathindex<0 THEN pathindex:=aparent^.adddirstring(path);
  IF aktarcliste='' THEN
  BEGIN arcindex:=byte(noarcconst); arcdirindex:=1; END ELSE
  BEGIN
    s:=sauglistobj^.getarcstring(1);
    s[1]:=char(byte(pathindex));
    arcindex:=NewList^.archiveinarclist(s,true);
    IF arcindex<0 THEN arcindex:=NewList^.addarcstring(s);
    arcdirindex:=NewList^.arcdirinarcdirlist(aktarcdir);
    IF arcdirindex<0 THEN arcdirindex:=NewList^.addarcdirstring(aktarcdir);
  END;
  FillChar(s,SizeOf(s),#32);
  s:=nc[1]+willplay+char(byte(pathindex))+char(byte(arcindex))+
     char(byte(arcdirindex))+Copy(nc,2,byte(nc[0])-1);
  s[0]:=char(songlistentriesize);
  nc:=s;
END;
{schaut nach ob Archiv-Directory in aktueller Ebene angezeigt werden kann}
FUNCTION open.isvalidarcdir;
VAR i,ebene,peb:integer; apath:STRING;
  FUNCTION getebene(astring:STRING):integer;
  VAR j,eb:integer;
  BEGIN
    eb:=0; IF astring<>'' THEN
    BEGIN FOR j:=1 TO Length(astring) DO IF astring[j]='\' THEN inc(eb); INC(eb); END;
    getebene:=eb;
  END;
BEGIN
  isvalidarcdir:=false;
  apath:=Name; IF apath[1]='\' THEN Delete(APath,1,1);
  ebene:=getebene(aktarcdir); peb:=getebene(apath);
  WHILE (peb>EBene) DO
  BEGIN
    IF peb-1=EBENE THEN Name:=apath;
    WHILE (apath[byte(apath[0])]<>'\')AND (byte(apath[0])>0) DO dec(byte(apath[0]));
    IF Length(apath)>0 THEN dec(byte(apath[0]));
    peb:=getebene(apath);
  END;
  IF ebene=0 THEN BEGIN isvalidarcdir:=true; Exit; END;
  isvalidarcdir:=aktarcdir=apath;
END;

{Zeigt neue Dirliste + Fileliste an -wechselt in Listen und Unterverz. + Archive}
PROCEDURE open.updatedirlist;
VAR i,j,k,olderrormode           :word;
    f                            :tsearchrec;
    fl:FILE;
    pc                           :ARRAY[0..255] OF char;
    s,s1,spath,probe             :STRING[100];
    box                          :pwaitbox;
    playlistcount,arccount,arcindex   :integer;
    maxfiles,aktfiles            :word;
    templist                     :pmydirs;
    filetypeindex,dummybyte      :byte;
    dirindex:integer;
    prevseldir                   :STRING[15];
    dfree:longint;
BEGIN
 olderrormode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
 spath:=path;
 prevseldir:='';
 realfilecount:=0;
 BEGIN
 IF pdirlist^.GetSelString(s,SizeOf(s))>0 THEN
   CASE byte(s[1]) OF
   byte(dlfloppytype)..byte(dlremotetype):
    BEGIN
      GetDir(byte(s[4])-96,path);
      path:=path+#0;
      path:=strpas(AnsiLower(pchar(@path[1])));
      IF path[Length(path)]='\' THEN dec(path[0]);
    END;
   byte(dldirtype):
     BEGIN
       Delete(s,1,1);
       IF aktarcliste<>'' THEN
       BEGIN
         IF s='<..>' THEN
         BEGIN
           IF aktarcdir='' THEN
           BEGIN
             Dispose(sauglistobj,Done); sauglistobj:=NIL; aktarcliste:='';
             prevseldir:=aktarc; aktarc:='';
             comments[3]^.SetText('&Directories:');
           END ELSE
           BEGIN
             prevseldir:=dldirtype+'<'+prevdir(aktarcdir)+'>';
             WHILE (byte(aktarcdir[0])>0)AND
             (aktarcdir[byte(aktarcdir[0])]<>'\') DO dec(byte(aktarcdir[0]));
             IF Length(aktarcdir)>0 THEN dec(byte(aktarcdir[0]));
           END;
         END ELSE
         IF aktarcdir='' THEN aktarcdir:=Copy(s,2,Length(s)-2)
                         ELSE aktarcdir:=aktarcdir+'\'+Copy(s,2,Length(s)-2);
       END ELSE
       BEGIN
         IF s='<..>' THEN
         BEGIN
           IF (aktliste<>'')THEN
           BEGIN
             Dispose(sauglistobj,Done); sauglistobj:=NIL; aktliste:='';
             prevseldir:=aktarc; aktarc:='';
           END ELSE
           BEGIN
             prevseldir:=dldirtype+'<'+prevdir(path)+'>';
             s1:=''; s:=path;
             WHILE Pos('\',s)<>0 DO
             BEGIN
               s1:=s1+Copy(s,1,Pos('\',s));
               Delete(s,1,Pos('\',s));
             END;
             Delete(s1,Length(s1),1);
             path:=s1;
           END;
         END ELSE
         BEGIN
           path:=path+'\'+Copy(s,2,Length(s)-2);
         END;
       END;
     END;
   arjfiletype+dlarcoffset..lharcfiletype+dlarcoffset:
     BEGIN
       sauglistobj:=New(pmydirsARC,Init(MaxArcListItems));
       IF sauglistobj^.saugarc(path,Copy(s,3,byte(s[0])),true) THEN
       BEGIN
         IF (sauglistobj^.songcount>0)OR(jumps>0) THEN
         BEGIN
           aktarcdir:='';
           aktarcliste:=path+'\'+Copy(s,3,byte(s[0]));
           aktarc:=s;
           strpcopy(pc,shortpath(path+'\'+Copy(s,3,byte(s[0])),byte(dx_pathname DIV Fw)));
           pathname^.SetText(AnsiUpper(@pc));
           s:='&Dirs: '+Copy(s,3,byte(s[0]))+#0;
           comments[3]^.SetText(pchar(@s[1]));
         END ELSE
         BEGIN
           Dispose(sauglistobj,Done);
           sauglistobj:=NIL;
           errmessagebox(@self,id_noarc);
         END
       END ELSE
       BEGIN
         Dispose(sauglistobj,Done);
         sauglistobj:=NIL;
         errmessagebox(@self,id_noarc);
       END;
       IF sauglistobj=NIL THEN BEGIN SetErrorMode(olderrormode); Exit; END;
     END;
   listfiletype+dlarcoffset:
    BEGIN
      sauglistobj:=New(pmydirsARC,Init(MaxArcListItems));
      IF sauglistobj^.sauglist(@self,path+'\'+Copy(s,3,byte(s[0]))) THEN
      BEGIN
         sauglistobj^.checklist(@self,path+'\'+Copy(s,3,byte(s[0])),id_nofilesinlist);
         IF sauglistobj^.songcount>0 THEN
         BEGIN
           aktarc:=s;
           aktliste:=path+'\'+Copy(s,3,byte(s[0]));
           pdirlist^.ClearList; pfilelist^.ClearList;
           pdirlist^.AddString(dldirtype+'<..>'); pdirlist^.SetSelIndex(0);
           strpcopy(pc,shortpath(path+'\'+Copy(s,3,byte(s[0])),byte(dx_pathname DIV Fw)));
           pathname^.SetText(AnsiUpper(@pc));
           realfilecount:=sauglistobj^.songcount;
           setlbredraw(off);
           FOR i:=0 TO sauglistobj^.songcount-1 DO
           BEGIN
             s:=sauglistobj^.getsongstring(i);
             IF  sauglistobj^.checkfrom2to1_entrie(NewList,s)<0 THEN
             BEGIN
              s:=s+#0; pfilelist^.InsertString(pchar(@s[1]),pfilelist^.GetCount);
             END ;
           END;
           pfilelist^.setcaretindex(0);
           setlbredraw(_on);
         END ELSE
         BEGIN Dispose(sauglistobj,Done); sauglistobj:=NIL; SetErrorMode(olderrormode); Exit; END;
      END ELSE
      BEGIN Dispose(sauglistobj,Done); sauglistobj:=NIL; SetErrorMode(olderrormode); Exit; END;
    END;
  END;
  {Pfadnamen ausgeben und evtl kürzen}
  {WENN in einem ARCHIV}
  IF aktarcliste<>'' THEN
  BEGIN
    setlbredraw(off);
    pdirlist^.ClearList; pfilelist^.ClearList;
    pdirlist^.AddString(dldirtype+'<..>');
    FOR i:=2 TO sauglistobj^.arcdircount+1 DO
    BEGIN
      s:=sauglistobj^.getarcdirstring(i);
      IF isvalidarcdir(s)THEN
      BEGIN
        IF aktarcdir<>'' THEN s:=Copy(s,byte(aktarcdir[0])+2,(byte(s[0])-byte(aktarcdir[0])));
        IF s<>'' THEN
        BEGIN
          s:=dldirtype+'<'+s+'>'+#0;
          IF pdirlist^.findstring(s)=lb_err THEN
          BEGIN
            dec(byte(s[0]));
            pdirlist^.AddString(s);
          END;
        END
      END;
    END;
    IF sauglistobj^.songcount>0 THEN
    BEGIN
      dirindex:=sauglistobj^.arcdirinarcdirlist(aktarcdir);
      FOR i:=0 TO (sauglistobj^.songcount-1) DO
      BEGIN
        s:=sauglistobj^.getsongstring(i);
        IF  sauglistobj^.checkfrom2to1_entrie(NewList,s)<0 THEN
        BEGIN
          splitplaylistitem(s,dummybyte,dummybyte,filetypeindex);
          IF filetypeindex=byte(dirindex) THEN
          BEGIN
            pfilelist^.AddString(s);
            inc(realfilecount);
          END;
        END ELSE IF byte(s[5])=dirindex THEN inc(realfilecount);
      END;
      pfilelist^.setcaretindex(0);
    END;
    setlbredraw(_on);
  END ELSE IF aktliste='' THEN
  BEGIN
    strpcopy(@pc,path+'\*.*');
    {Ist dieser PFAD +LAUFWERK VALID?}
    FindFirst(@pc,fafiles,f);
    IF (doserror=0)AND(GetDriveType(word(UpCase(path[1]))-Ord('A'))=DRIVE_REMOTE) THEN
    BEGIN
      Assign(fl,path+'\'+f.Name);
      GetFAttr(fl,i);
      IF i=0 THEN doserror:=1;
    END;
    IF (doserror>0)AND(doserror<>18) THEN
    BEGIN
      doserror:=0; Path:=Path[1]+':'; strpcopy(@pc,path+'\*.*');
      FindFirst(@pc,fafiles,f);
      IF (doserror=0)AND(GetDriveType(word(UpCase(path[1]))-Ord('A'))=DRIVE_REMOTE) THEN
      BEGIN Assign(fl,path+'\'+f.Name); GetFAttr(fl,i); IF i=0 THEN doserror:=1; END;
    END;
    IF (doserror>0)AND(doserror<>18) THEN
    BEGIN
      ourmessagebox(@self,errorver,getresstring(id_driveerror),mb_ok OR mb_iconstop);
      path:=spath; SetErrorMode(olderrormode); Exit;
    END ELSE
    BEGIN
      strpcopy(pc,shortpath(path,byte(dx_pathname DIV Fw))+'\');
      pathname^.SetText(AnsiUpper(@pc));
      templist:=New(pmydirs,Init);
      {Erstelle temporäre Playliste im Speicher}
      box:=initwaitbox(@self,getresstring(id_loadcheckfiles));
      NewList^.TransferData(templist);
      playlistcount:=pplaylist^.GetCount;
      IF playlistcount>0 THEN
      BEGIN
        FOR i:=0 TO playlistcount-1 DO
        BEGIN
          pplaylist^.GetString(s,i);
          templist^.addsongstring(s);
        END;
      END;
      {Lese Anzahl Files}
      maxfiles:=0;
      aktfiles:=0;
      WHILE doserror=0 DO
      BEGIN
        inc(maxfiles);
        FindNext(f);
      END;
      {Löschen der File und Dir Listbox}
      setlbredraw(off);
      pdirlist^.ClearList;
      pfilelist^.ClearList;
      strpcopy(@pc,path+'\*.*');
      doserror:=0;
      FillChar(f,SizeOf(f),#0);
      {Suche nach allem ,was im Verz. steht ;sortieren nach Datei und Verzeichnis}
      FindFirst(@pc,fafiles,f);
      WHILE doserror=0  DO
      BEGIN
        strcopy(f.Name,AnsiLower(f.Name));
        IF NewList^.IsValid(isitdirectory,f,filetypeindex)THEN
        BEGIN
          s:=dldirtype+'<'+strpas(f.Name)+'>';
          IF(Length(path)>2)OR(s<>(dldirtype+'<..>'))THEN
          BEGIN
            pdirlist^.setitemdata(pdirlist^.AddString(s),f.Size);
          END;
        END
        ELSE IF NewList^.isvalidarchive(f,filetypeindex) THEN
        BEGIN
           s:=char(filetypeindex+dlarcoffset)+' '+strpas(AnsiLower(f.Name));
           pdirlist^.setitemdata(pdirlist^.AddString(s),f.Size);
        END ELSE
        IF NewList^.IsValid(NOT isitdirectory,f,filetypeindex) THEN
        BEGIN
          s:=strpas(f.Name); s:=Copy(s,1,(Pos('.',s))-1);
          IF filetypeindex=listfiletype THEN
          BEGIN
             s:=strpas(f.Name);
             s:=char(listfiletype+dlarcoffset)+' '+strpas(AnsiLower(f.Name));
             pdirlist^.setitemdata(pdirlist^.AddString(s),f.Size);
          END ELSE
          BEGIN
            inc(realfilecount);
            s:=char(filetypeindex)+s;
            IF playlistcount>0 THEN
            BEGIN
              probe:=s;
              uniteplaylistitem(NewList,probe,false);
              IF (templist^.isinloadlist(probe,0)<0) THEN pfilelist^.AddString(s);
            END ELSE pfilelist^.AddString(s);
          END;
        END;
        inc(aktfiles);
        setwaitbox(box,((100.0*aktfiles)/maxfiles));
        FindNext(f);
      END;
    END;
    FOR i:=0 TO 26 DO
    BEGIN
      j:=GetDriveType(i);
      IF (j<>0) THEN
      BEGIN
        s:='[-'+char(97+i)+'-]';
        IF j=drive_removable THEN Insert(dlfloppytype,s,1) ELSE
        IF j=drive_fixed THEN Insert(dlhdtype,s,1) ELSE Insert(dlremotetype,s,1);
        IF j=drive_fixed THEN
        BEGIN
          dfree:=DiskFree(i+1); IF dfree<0 THEN dfree:=0;
        END ELSE dfree:=0;
        pdirlist^.setitemdata(pdirlist^.AddString(s),dfree);
      END;
    END;
    setlbredraw(_on);
    closewaitbox(box);
    Dispose(templist,Done);
  END;
  IF (pdirlist^.GetCount>0) THEN
  BEGIN
   IF prevseldir[0]=#0 THEN pdirlist^.GetString(s,0) ELSE s:=prevseldir;
   j:=pdirlist^.findstring(s+#0);
   IF j>-1 THEN pdirlist^.SetSelIndex(j);
  END;
 END;
 SetErrorMode(olderrormode);
 pfilelist^.setcaretindex(0);
 setinfo(pdirlist^.HWindow);
END;

PROCEDURE open.WMCommand(VAR msg:tmessage);
BEGIN

  IF msg.wparam=id_oadd THEN
  BEGIN
    IF lastfocus=pdirlist^.HWindow
    THEN SendMessage(HWindow,wm_keydown,vk_f5,id_dirlist)
    ELSE handleaddone;
  END ELSE
  IF msg.lparamhi=lbn_dblclk THEN
  CASE msg.wparam OF
    id_filelist:handleaddone;
    id_dirlist:updatedirlist;
    id_playlist:handledeleteone(Msg);
  END ELSE
  IF msg.lparamhi=lbn_selchange THEN
  CASE msg.wparam OF
    id_filelist:BEGIN pfilelist^.setstatictext; setinfo(pfilelist^.HWindow); END;
    id_playlist:BEGIN pplaylist^.setstatictext; setinfo(pplaylist^.HWindow); END;
    id_dirlist: setinfo(pdirlist^.HWindow);
  END ELSE
  IF (msg.lparamhi=en_killfocus)THEN
  BEGIN
    IF msg.wparam=id_leftsel THEN
    BEGIN ptempedit:=NIL; pfilelist^.lastcount:=-1; pfilelist^.setstatictext; END
    ELSE IF msg.wparam=id_rightsel THEN
    BEGIN ptempedit:=NIL; pplaylist^.lastcount:=-1; pplaylist^.setstatictext; END
  END;
  INHERITED WMCommand(msg);
END;

CONSTRUCTOR open.Init;
VAR i:integer;
BEGIN
   inputlist:=alist;
   INHERITED Init (AParent, '',ws_sysmenu,NoTopmost);
   {Koordinatenberechnung}
   X_pathname  :=10+5*Fw+2;
   X_filelist  :=12;
   Y_pathname  :=10;
   Y_filelist  := y_pathname+2*Fh+10;
   Y_dirlist   := y_filelist;
   Y_playlist  := y_filelist;
   dy_filelist:=20*Fh;
   dx_dirlist :=14*Fw+10;
   dy_dirlist :=13*Fh;
   y_open:=4*Fh+dy_filelist+50;
   IF dx_dirlist<169 THEN dx_dirlist:=169;
   IF dy_dirlist<(y_open-189) THEN dy_dirlist:=y_open-189;
   dx_playlist:=(songlistentriesize+7)*Fw+10;
   dx_filelist:=dx_playlist;
   dy_playlist:=20*Fh;
   x_open:=48+dx_dirlist+dx_filelist+dx_playlist;
   dx_pathname:=x_open-(5*Fw)-25;
   X_dirlist  :=(x_open DIV 2)-(dx_dirlist DIV 2);
   X_playlist:=x_open-13-dx_playlist;
   x_boaddone  := (x_open DIV 2);
   y_boaddone  := y_dirlist+dy_dirlist+11;
   x_bodeleteone:=x_boaddone-49;
   y_bodeleteone:=y_boaddone;
   X_bOhelp    := x_boaddone+41;
   Y_bOhelp    := y_filelist+dy_filelist-20;
   X_bOSave    := x_boaddone-41;
   Y_bOSave    := y_bohelp;
   X_bOOk      := x_boaddone-82;
   Y_bOOk      := y_bohelp;
   X_bOCancel  := x_boaddone;
   Y_bOCancel  := y_bohelp;
   {Attributinitialisierung}
   Attr.style:=ws_popup OR ws_visible OR ws_border;
   Attr.x :=(GetSystemMetrics(sm_cxscreen)-x_open)DIV 2;
   Attr.y :=(GetSystemMetrics(sm_cyscreen)-y_open)DIV 2;
   Attr.w:=X_open+GetSystemMetrics(sm_cxborder);
   Attr.h:=Y_open+capdy+(2*GetSystemMetrics(sm_cyborder));
   {CHILD-INITIALISIERUNG}
   pathname:=New(pmystatic,Init(@self, id_pathname,'',X_pathname,CapDY+Y_pathname,dx_pathname,Fh,81));
   comments[1]:=New(pmystatic,Init(@self,0,'',(x_pathname-5*Fw)-2,CapDY+Y_pathname,5*Fw,Fh,5));
   comments[2]:=New(pmystatic,Init(@self,0,'',x_filelist-2,CapDY+Y_filelist-Fh-4,9*Fw,Fh,9));
   comments[3]:=New(pmystatic,Init(@self,0,'',x_dirlist,CapDY+Y_dirlist-Fh-4,18*Fw,Fh,18));
   comments[4]:=New(pmystatic,Init(@self,0,'',x_playlist,CapDY+Y_dirlist-Fh-4,9*Fw,Fh,9));
   bOOk:=New(pbmpbutton,Init(@self, id_Ook,X_bOok,CapDY+Y_bOok,'bsok',false));
   bOSave:=New(pbmpbutton,Init(@self, id_Osave,X_bOsave,CapDY+Y_bOsave,'bssave',false));
   bOCancel:=New(pbmpbutton,Init(@self, id_Ocancel,X_bOcancel,CapDY+Y_bOcancel,'bscancel',false));
   bOhelp:=New(pbmpbutton,Init(@self, id_Ohelp,X_bOhelp,CapDY+Y_bOhelp,'bshelp',false));
   bolplus :=New(pbmpbutton,Init(@self,id_Olplus ,x_bodeleteone-25,CapDY+y_bodeleteone,'boplus',false));
   bolminus:=New(pbmpbutton,Init(@self,id_Olminus,x_bodeleteone-25,CapDY+y_bodeleteone+14,'bominus',false));
   bolmal  :=New(pbmpbutton,Init(@self,id_Olmal,x_bodeleteone-25,CapDY+y_bodeleteone+28,'bomal',false));
   bodel   :=New(pbmpbutton,Init(@self,id_Odel,x_bodeleteone,CapDY+y_bodeleteone,'bodel',false));
   boadd   :=New(pbmpbutton,Init(@self,id_Oadd,x_boaddone,CapDY+y_boaddone,'boadd',false));
   borplus :=New(pbmpbutton,Init(@self,id_Orplus ,x_boaddone+61,CapDY+y_boaddone,'boplus',false));
   borminus:=New(pbmpbutton,Init(@self,id_Orminus,x_boaddone+61,CapDY+y_boaddone+14,'bominus',false));
   bormal  :=New(pbmpbutton,Init(@self,id_Ormal,x_boaddone+61,CapDY+y_boaddone+28,'bomal',false));
   pfilelist:=New(pmylistbox,Init(@self,id_filelist,x_filelist,CapDY+y_filelist,dx_filelist,dy_filelist,
   lbs_sort OR lbs_multiplesel ,true));
   leftstatic :=New(pmystatic,Init(@self,id_leftstatic ,'',x_filelist,CapDY+Y_filelist+dy_filelist+10,
                dx_filelist,Fh,songlistentriesize+6));
   rightstatic:=New(pmystatic,Init(@self,id_rightstatic,'',x_playlist,CapDY+Y_filelist+dy_filelist+10,
                dx_playlist,Fh,songlistentriesize+6));
   INFOstatic :=New(pmycenterstatic,Init(@self,id_infostatic,'',x_filelist,CapDY+Y_filelist+dy_filelist+30,
                x_open-25,Fh,80));
   pdirlist:=New(pmylistbox,Init(@self,id_dirlist,x_dirlist,CapDY+y_dirlist,dx_dirlist,dy_dirlist,
   lbs_sort ,true));
   pplaylist:=New(pmylistbox,Init(@self,id_playlist,x_playlist,CapDY+y_playlist,dx_playlist,dy_playlist,
   lbs_multiplesel,true ));
   {Bitmaps laden-Filetypes,Arc,drives}
   FOR i:=listfiletype+1 TO allfiletypes DO bmphandles[i-3]:=loadMybitmap(hinstance,makeintresource(i-3));
   arcbmphandle:=loadMybitmap(hinstance,'ARC');
   drivebmphandle:=loadMybitmap(hinstance,'BMP_drives');
   {Temporäres PMYDIRS für Archive+Listen auf nil setzen(Vergleichskriterium!)}
   sauglistobj:=NIL;
   {Temporäre Editfenster auf NIL setzen (für selection + Listenname)}
   ptempedit:=NIL;
   {aktuelle Liste auf leer setzen}
   aktliste:='';
   SaveListWindow:=0;
   {LAstfocus=0 für ADD auf LISTE}
   lastfocus:=0;
 END;

PROCEDURE open.SetupWindow;
VAR i,selind:word;
    pc      :pchar;
    s       :STRING[songlistentriesize];
    box     :pwaitbox;
BEGIN
  INHERITED SetupWindow;
  {Caption setzen}
  pc:=MemAlloc(255); strcopy(pc,longver); strcat(pc,' Open Files'); SetCaption(pc); FreeMem(pc,255);
  {aktuelles Directory checken und dorthin wechseln}
  getthecorrectpath(currentdir);
  path:=currentdir;
  InputList^.checklist(@self,'',0);
  Newlist:=New(pmydirs,Init);
  {Wenn alte Liste Einträge enthält dann übernehmen und in PLAYLISTBOX anzeigen}
  IF (InputList^.psonglist<>NIL) AND (InputList^.songcount>0) THEN
  BEGIN
   setlbredraw(off);
   box:=initwaitbox(@self,getresstring(id_createplaylist));
   InputList^.TransferData(Newlist);
   Newlist^.lastunpackedsong:=Inputlist^.lastunpackedsong;
   FOR i:=0 TO (NewList^.songcount-1) DO
   BEGIN
     s:=NewList^.getsongstring(i);
     pplaylist^.AddString(s);
     IF s[2]=isplay THEN selind:=i;
     setwaitbox(box,(100.0*(i+1)/NewList^.songcount));
   END;
   {Caret in PLAYLISTBOX auf gerade spielenden Song stellen}
   pplaylist^.setcaretindex(selind);
   selind:=selind-((dy_playlist DIV Fh)DIV 2); IF selind<0 THEN selind:=0;
   SendMessage(pplaylist^.HWindow,lb_settopindex,selind,0);
   setlbredraw(_on);
   closewaitbox(box);
  END;
  {Statictexte setzen und ASSOCWINDOWS(bei MOUSECLICK FOCUS auf dieses WINDOW) setzen}
  comments[1]^.SetText('Path:');
  comments[2]^.SetText('&Filelist:');   comments[2]^.setassocwindow(Pfilelist^.HWindow,0);
  comments[3]^.SetText('&Directories:'); comments[3]^.setassocwindow(PDirlist^.HWindow,0);
  comments[4]^.SetText('&Playlist:');   comments[4]^.setassocwindow(PPlaylist^.HWindow,0);
  leftstatic^.setassocwindow($FFFF,vk_f1); rightstatic^.setassocwindow($FFFF,vk_f3);
  pfilelist^.setassocstatic(leftstatic); pplaylist^.setassocstatic(rightstatic);
  pathname^.enable; book^.enable; bosave^.enable; bocancel^.enable; boHelp^.enable;
  boadd^.enable; bodel^.enable; bolplus^.enable;  borplus^.enable;
  bolminus^.enable; borminus^.enable; bolmal^.enable; bormal^.enable;
  Parent^.disable;
  {Directory+Filelist updaten}
  aktarcliste:='';
  updatedirlist;
  {SELINDEX in DIRLIST auf letztes Archiv setzen-wenn vorhanden}
  IF aktarc<>'' THEN
  BEGIN
    i:=pdirlist^.findstring(aktarc+#0);
   IF i<32768 THEN pdirlist^.SetSelIndex(i);
  END;
  {den Focus bekommt die DIRLISTBOX}
  oldfocus:=pdirlist^.HWindow;
END;

DESTRUCTOR open.Done;
VAR i:word;
BEGIN
  {Temporäre Listen destroyen}
  IF sauglistobj<>NIL THEN Dispose(sauglistobj,Done);
  IF Newlist<>NIL THEN Dispose(Newlist,Done);
  {Bitmaps deleten}
  FOR i:=listfiletype+1 TO allfiletypes DO deleteMyBitmap(bmphandles[i-3]);
  deleteMyBitmap(drivebmphandle);
  deleteMyBitmap(arcbmphandle);
  Parent^.enable;
  INHERITED Done;
END;

PROCEDURE open.wmEraseBkGnd(VAR msg:tmessage);
BEGIN
  paintcaption(msg.wParam);
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  framefilled3d(msg.wParam,0,CapDY,x_open-1,y_open,3,cForeGnd,up);
  frame3d(msg.wParam,X_pathname-2,CapDY+Y_pathname-2,dx_pathname+4,Fh+4,2,down);
  frame3d(msg.wParam,X_filelist-3,CapDY+Y_filelist-3,dx_filelist+6,dy_filelist+6,2,down);
  frame3d(msg.wParam,X_dirlist-3,CapDY+Y_dirlist-3,dx_dirlist+6,dy_dirlist+6,2,down);
  frame3d(msg.wParam,X_playlist-3,CapDY+Y_playlist-3,dx_playlist+6,dy_playlist+6,2,down);
  frame3d(msg.wParam,X_book-2,CapDY+Y_book-2,169,25,2,down);
  frame3d(msg.wParam,X_bodeleteone-2,CapDY+Y_bodeleteone-2,103,46,2,down);
  {PLUSMALMINUS -LINKS}
  frame3d(msg.wParam,X_bodeleteone-27,CapDY+Y_bodeleteone-2,18,46,2,down);
  {PLUSMALMINUS -RECHTS}
  frame3d(msg.wParam,X_boaddone+59,CapDY+Y_boaddone-2,18,46,2,down);
  frame3d(msg.wParam,X_filelist-3,CapDY+Y_filelist+dy_filelist+8,dx_filelist+6,Fh+4,2,up);
  frame3d(msg.wParam,X_playlist-3,CapDY+Y_filelist+dy_filelist+8,dx_playlist+6,Fh+4,2,up);
  frame3d(msg.wParam,X_filelist-3,CapDY+Y_filelist+dy_filelist+28,infostatic^.Attr.w+6,Fh+4,2,down);
  IF SaveListWindow<>0 THEN frame3d(msg.wparam,x_boaddone-(4*Fw)-2,CapDY+y_book+28,(8*Fw)+4,Fh+4,2,up);
  INHERITED wmEraseBkGnd(msg);
END;

{Infostatic-Text in Abhängigkeit vom Focus setzen}
PROCEDURE open.setinfo;
VAR index    :integer;
    s        :STRING[50];
    s2       :STRING[100];
    arcindex,arcdirindex,dirindex:byte;

    FUNCTION GetFreeSpace(wert:longint):STRING;
    VAR tempstr:STRING[20];
    BEGIN
      Str((wert DIV 102),tempstr); IF Length(tempstr)=1 THEN Insert('0',tempstr,1);
      Insert(GetResString(40),tempstr,Length(tempstr));
      IF Length(tempstr)>5 THEN Insert(GetResString(41),tempstr,Length(tempstr)-4);
      IF Length(tempstr)>9 THEN Insert(GetResString(41),tempstr,Length(tempstr)-8);
      GetFreeSpace:=tempstr;
    END;

BEGIN
  box_id:=GetDlgCtrlID(box_id);
  s2:='';
  CASE box_id OF
   id_dirlist:
     BEGIN
       index:=pdirlist^.getcaretindex;
       pdirlist^.GetString(s,index);
       CASE byte(s[1]) OF
        zipfiletype+dlarcoffset:
          BEGIN
            s:=GetFreeSpace(pdirlist^.getitemdata(index));
            s2:=getresstring(zip_id)+' ('+s+' KBytes)'+getresstring(enter_id)+getresstring(f5_id);
          END;
        arjfiletype+dlarcoffset:
          BEGIN
            s:=GetFreeSpace(pdirlist^.getitemdata(index));
            s2:=getresstring(arj_id)+' ('+s+' KBytes)'+getresstring(enter_id)+getresstring(f5_id);
          END;
        LHARCfiletype+dlarcoffset:
          BEGIN
            s:=GetFreeSpace(pdirlist^.getitemdata(index));
            s2:=getresstring(lha_id)+' ('+s+' KBytes)'+getresstring(enter_id)+getresstring(f5_id);
          END;
        listfiletype+dlarcoffset :
          BEGIN
            s:=GetFreeSpace(pdirlist^.getitemdata(index));
            s2:=getresstring(mol_id)+' ('+s+' KBytes)'+getresstring(enter_id)+getresstring(f5_id);
          END;
        byte(dldirtype):
          IF (s[3]='.')AND(s[4]='.') THEN
          BEGIN
            IF (aktarcliste<>'')
            THEN IF aktarcdir=''
              THEN s2:=getresstring(leavearc_id)
              ELSE s2:=getresstring(leavearcdir_id)
            ELSE IF aktliste<>''
              THEN s2:=getresstring(leavemol_id)
              ELSE s2:=getresstring(leavedir_id);
          END ELSE IF aktarcliste=''
                   THEN s2:=getresstring(dir_id)+getresstring(f5_id)
                   ELSE s2:=getresstring(arcdir_id);
        byte(dlfloppytype)          :
          s2:=getresstring(floppy_id)+getresstring(f5_id);
        byte(dlhdtype)
                  :
          BEGIN
            s:=GetFreeSpace(pdirlist^.getitemdata(index));
            s2:=getresstring(hd_id)+' ('+s+' KBytes free)'+getresstring(f5_id);
          END;
        byte(dlremotetype)          :
            s2:=getresstring(remote_id)+getresstring(f5_id);
       END;
     END;
   id_playlist:
     IF pplaylist^.GetCount>0 THEN
     BEGIN
       index:=pplaylist^.getcaretindex;
       IF index>=0 THEN
       BEGIN
         pplaylist^.GetString(s,index);
         splitplaylistitem(s,dirindex,arcindex,arcdirindex);
         s2:=NewList^.getdirstring(dirindex)+'\';
         IF arcindex<>byte(noarcconst)
         THEN s2:=s2+Copy(NewList^.getarcstring(arcindex),3,12)+
                  '  '+NewList^.getarcdirstring(arcdirindex)+'\';
         s:=Copy(s,2,8)+'.'+filetypesasstring[byte(s[1])];
         WHILE Pos(' ',s)<>0 DO system.Delete(s,Pos(' ',s),1);
         s2:=s2+s;
       END;
     END ELSE s2:=getresstring(nofileinplaylist_id);
   id_filelist:
     IF pfilelist^.GetCount>0 THEN
     BEGIN
       index:=pfilelist^.getcaretindex;
       IF index>=0 THEN
       BEGIN
         pfilelist^.GetString(s,index);
         IF aktliste<>'' THEN
         BEGIN
           splitplaylistitem(s,dirindex,arcindex,arcdirindex);
           s2:=sauglistobj^.getdirstring(dirindex)+'\';
           IF arcindex<>byte(noarcconst)THEN s2:=s2+Copy(sauglistobj^.getarcstring(arcindex),3,12)+'  ';
         END ELSE IF aktarcliste<>'' THEN
         BEGIN
           s2:=getresstring(2009)+aktarcdir+'\';
         END ELSE s2:=path+'\';
         s:=Copy(s,2,8)+'.'+filetypesasstring[byte(s[1])];
         WHILE Pos(' ',s)<>0 DO system.Delete(s,Pos(' ',s),1);
         s2:=s2+s;
       END;
     END ELSE
     BEGIN
       IF Realfilecount=0
       THEN s2:=getresstring(novalidfiles_id)
       ELSE s2:=getresstring(allfilesinplaylist_id);
     END;
   id_OAdd:    s2:=getresstring(add_id);
   id_ODEL:    s2:=getresstring(del_id);
   id_OOK :    s2:=getresstring(ok_id);
   id_OCancel: s2:=getresstring(quit_id);
   id_Ohelp:   s2:=getresstring(help_id);
   id_OSave:   s2:=getresstring(save_id);
   id_filenameedit: s2:=getresstring(handlesave_id);
   id_olplus:  s2:=getresstring(filesel_id);
   id_olminus: s2:=getresstring(filedesel_id);
   id_olmal:   s2:=getresstring(fileinvsel_id);
   id_orplus:  s2:=getresstring(playsel_id);
   id_orminus: s2:=getresstring(playdesel_id);
   id_ormal:   s2:=getresstring(playinvsel_id);
   id_leftstatic,id_rightstatic: s2:=getresstring(selmask_id);
  END;
  s2[Length(s2)+1]:=#0;
  infostatic^.SetText(@s2[1]);
END;

{Kommt von TEMPEDITS(*.*,Listname) wenn sie den Focus verlieren}
PROCEDURE open.WMselection(VAR msg:tmessage);
VAR DC:HDC; rect:Trect;
BEGIN
  IF msg.wparam=id_filenameedit THEN
  BEGIN
    { handlelistsave;}
     {IF plistnameedit<>NIL THEN
     BEGIN
       dispose(plistnameedit,done);
       plistnameedit:=nil;
       DC:=getdc(hwindow);
       with rect DO
       BEGIN left:=x_boaddone-(4*Fw)-2;top:=CapDY+y_book+28;right:=left+(8*Fw)+4;bottom:=top+Fh+4;END;
       fillrect(dc,rect,brstatic);
       Releasedc(hwindow,dc);
     END;}
  END
  ELSE handleselection(msg.wparam,word(msg.lparam));
  msg.result:=0;
END;

{Handled die *.* Felder }
PROCEDURE open.handleselection;
VAR s:STRING[20];
    filetypeindex:byte;

  {Macht aus *.??? ... einen vergleichbaren String}
  FUNCTION expandname(nc:STRING):STRING;
  VAR k,j:integer;
  BEGIN
    nc[byte(nc[0])+1]:=' ';
    nc[0]:=char(12);
    k:=Pos('.',nc);
    IF k<=0 THEN nc[9]:='.'
    ELSE WHILE nc[9]<>'.' DO Insert(' ',nc,k);
    nc[0]:=char(12);
    FOR k:=1 TO 12 DO
    BEGIN
      j:=k;
      IF nc[k]='*' THEN
      WHILE (j<13)AND(nc[j]<>'.')AND(NOT (nc[j] IN['a'..'z','-','_'])) DO
      BEGIN
       nc[j]:='?';
       inc(j);
      END;
    END;
    expandname:=nc;
  END;

  PROCEDURE dolistselections(Alist:pmylistbox; do_id:word);
  VAR i,j          :integer;
      eintrag      :STRING[20];
      okay         :boolean;
  BEGIN
    IF alist^.GetCount>0 THEN
    BEGIN
      s:=strpas(AnsiLower(pchar(@s[1])));
      s:=expandname(s);
      alist^.lastcount:=-1;
      SendMessage(alist^.HWindow,wm_setredraw,off,0);
      FOR i:=alist^.GetCount-1 DOWNTO 0 DO
      BEGIN
        alist^.GetString(eintrag,i);
        j:=byte(eintrag[1]);
        {wenn Liste in Filelistbox dann ist Struktur dort anders}
        IF (alist^.Attr.id=id_filelist)AND(aktliste='')
        THEN eintrag:=Copy(eintrag,2,8)
        ELSE eintrag:=Copy(eintrag,6,8);
        eintrag:=eintrag+'.'+filetypesasstring[j];
        {Da fängd drr bakvix ann........}
        IF Length(FileTypesAsString[j])<1 THEN Eintrag:=Eintrag+'   ' ELSE
        IF Length(FileTypesAsString[j])<2 THEN Eintrag:=Eintrag+'  ' ELSE
        IF Length(FileTypesAsString[j])<3 THEN Eintrag:=Eintrag+' ';
        {unn da hehrdr off..............}
        {Nich sehr schön aber geht...oder was.....?}
        okay:=true;
        WHILE Length(eintrag)<12 DO
        BEGIN
          j:=Pos('.',eintrag);
          Insert(' ',eintrag,j);
        END;
        FOR j:=1 TO 12 DO IF (eintrag[j]=' ')AND(s[j]='?') THEN eintrag[j]:='?';
        FOR j:=1 TO 12 DO IF (eintrag[j]<>s[j])AND(s[j]<>'?')THEN okay:=false;
        IF okay THEN
        IF (do_id=id_olplus)OR(do_id=id_orplus)
        THEN alist^.setsel(i,_on)
        ELSE
        IF (do_id=id_olminus)OR(do_id=id_orminus)
        THEN alist^.setsel(i,off);
      END;
      SendMessage(alist^.HWindow,wm_setredraw,_on,0);
      alist^.focus;
    END ELSE SetFocus(previousfocus);
    alist^.lastcount:=-1; alist^.setstatictext;
  END;

BEGIN
  {Button-Ids auf Funtionstasten umleiten}
  IF tastcode=vk_add THEN IF ctrlid=id_olplus THEN tastcode:=vk_f1
                                              ELSE tastcode:=vk_f3;
  IF tastcode=vk_subtract THEN IF ctrlid=id_olminus THEN tastcode:=vk_f1
                                                    ELSE tastcode:=vk_f3;
  CASE tastcode OF
  {Wurden ein Edit verlassen, dann destroyen und Focus auf vorheriges ELEMENT setzen}
  vk_escape:IF (ptempedit<>NIL)AND(ptempedit^.Attr.id=ctrlid) THEN
            BEGIN
              SetFocus(previousfocus);
              Dispose(ptempedit,Done);
              IF ctrlid=id_leftstatic THEN
              BEGIN pfilelist^.lastcount:=-1; pfilelist^.setstatictext; END
              ELSE BEGIN pplaylist^.lastcount:=-1; pplaylist^.setstatictext; END;
              ptempedit:=NIL;
            END;
  {Man wünscht eine Selektierung!!!!!!!!!}
  vk_return:IF ptempedit<>NIL THEN
            BEGIN
              FillChar(s,SizeOf(s),32);
              s[0]:=char(Lo(ptempedit^.GetText(pchar(@s[1]),12)));
              SetFocus(previousfocus);
              Dispose(ptempedit,Done); ptempedit:=NIL;
              {War die Eingabe leer dann destroyen und die Listboxanzeigen (12 OF xx Files)
               updaten}
              IF s[0]=#0 THEN
              BEGIN
                IF ctrlid=id_leftstatic THEN
                BEGIN pfilelist^.lastcount:=-1; pfilelist^.setstatictext; END ELSE
                IF ctrlid=id_rightstatic THEN
                BEGIN pplaylist^.lastcount:=-1; pplaylist^.setstatictext; END ELSE
                Exit;
              END;
              {Wurde etwas gewählt dann vergleichen und selektieren-je nach FILE-oder PLAYLIST}
              IF ctrlid=id_leftstatic THEN dolistselections(pfilelist,selectionid)
                                      ELSE dolistselections(pplaylist,selectionid);
            END;
  vk_f1    :IF ptempedit=NIL THEN
            BEGIN
              previousfocus:=GetFocus;
              IF ctrlid=id_leftstatic THEN  ctrlid:=id_olplus;
              selectionid:=ctrlid;
              ptempedit:=New(pmykfqedit,Init(@self,id_leftstatic,'*.*',x_filelist-3+3*Fh,
                                CapDY+Y_filelist+dy_filelist+10,dx_filelist-4*Fw,Fh,13,true));
              Application^.MakeWindow(ptempedit);
              leftstatic^.SetText('Sel?');
              ptempedit^.focus;
            END ELSE
            BEGIN
              handleselection(ptempedit^.Attr.id,vk_escape);
              handleselection(id_leftstatic,vk_f1);
            END;
  vk_f3    :IF ptempedit=NIL THEN
            BEGIN
              previousfocus:=GetFocus;
              IF ctrlid=id_rightstatic THEN ctrlid:=id_orplus;
              selectionid:=ctrlid;
              ptempedit:=New(pmykfqedit,Init(@self,id_rightstatic,'*.*',x_playlist-3+3*Fh,
                                CapDY+Y_filelist+dy_filelist+10,dx_playlist-4*Fw,Fh,13,true));
              Application^.MakeWindow(ptempedit);
              rightstatic^.SetText('Sel?');
              ptempedit^.focus;
            END ELSE
            BEGIN
              handleselection(ptempedit^.Attr.id,vk_escape);
              handleselection(id_rightstatic,vk_f3);
            END;
  END;
END;

PROCEDURE open.handlelPLUS;
BEGIN SendMessage(HWindow,wm_selection,id_olplus,longint(vk_add)); END;
PROCEDURE open.handlerPLUS;
BEGIN SendMessage(HWindow,wm_selection,id_orplus,longint(vk_add)); END;
PROCEDURE open.handlelMINUS;
BEGIN SendMessage(HWindow,wm_selection,id_olminus,longint(vk_subtract)); END;
PROCEDURE open.handlerMINUS;
BEGIN SendMessage(HWindow,wm_selection,id_orminus,longint(vk_subtract)); END;
PROCEDURE open.handlelMAL; BEGIN doinvselection(PFileList); END;
PROCEDURE open.handlerMAL; BEGIN doinvselection(PPlayList); END;

{Invertiert die Auswahl in der gegebenen Listbox}
PROCEDURE open.doinvselection;
VAR i,j,caretindex:integer;
    lb_select:boolean;
BEGIN
  i:=alistbox^.GetCount;
  IF i>0 THEN
  BEGIN
    SendMessage(AListbox^.HWindow,wm_setredraw,off,0);
    caretindex:=Alistbox^.getcaretindex;
    FOR j:=0 TO (i-1) DO
    BEGIN
      lb_select:=boolean(Alistbox^.getsel(j));
      Alistbox^.setsel(j,byte(NOT lb_select));
    END;
    Alistbox^.setcaretindex(caretindex);
    SendMessage(AListbox^.HWindow,wm_setredraw,_on,0);
  END;
END;

{Fügt alle Einträge der Fileliste der Playliste hinzu (box+Newlist)}
PROCEDURE open.handleAddAll;
VAR i,j,oldsongcount:integer;
    afocus          :word;
    s,test          :STRING[50];
    box             :pwaitbox;
    pdf             :pDynFound;
    pdfSize         :Word;
BEGIN
  setlbredraw(off);
  i:=pfilelist^.GetCount;
  IF i>0 THEN
  BEGIN
    IF aktliste<>'' THEN
    BEGIN
      pdfSize     :=sauglistobj^.SongCount; GetMem(pdf, pdfSize);
      oldsongcount:=NewList^.songcount;
      sauglistobj^.from2to1(NewList, pdf);
      FOR j:=oldsongcount TO (NewList^.songcount-1) DO
        IF pDF^[J-OldSongCount] THEN pplaylist^.AddString(NewList^.psonglist^[j]);
      FOR J:=pdfsize-1 DOWNTO 0 DO IF pDF^[J] THEN pFileList^.DeleteString(J);
      FreeMem(pDF, pdfSize);
    END ELSE
    BEGIN
      box:=initwaitbox(@self,getresstring(id_readingfiles));

      WHILE pfilelist^.GetCount>0 DO
      BEGIN
        SetWaitBox(box,(100.0*(i-pfilelist^.GetCount))/i);
        pfilelist^.GetString(s,0);
        uniteplaylistitem(NewList,s,true);
        IF NewList^.AddSongString(S) THEN
        BEGIN
          pplaylist^.AddString(s);
          pfilelist^.DeleteString(0);
        END ELSE BREAK;
      END;

      setlbredraw(_on); closewaitbox(box); setlbredraw(off);
    END;
  END;
  pfilelist^.setsel(-1,OFF);
  setlbredraw(_on);
  j:=pplaylist^.getcaretindex;
  IF j<0 THEN pplaylist^.setcaretindex(0);
  afocus:=GetFocus;
  IF (afocus=pfilelist^.HWindow)OR(afocus=pplaylist^.HWindow) THEN setinfo(afocus);
END;



{Fügt alle selektierten Einträge der Fileliste der Playliste hinzu (box+Newlist)}
PROCEDURE open.handleAddOne;
VAR i,x,l:integer;
    afocus:word;
    puffer:pbuffer;
    oldcursor:hcursor;
    s:STRING[50];
BEGIN
  IF NewList^.SongCount>=NewList^.Limit THEN EXIT;
  setlbredraw(off);
  puffer:=New(pbuffer);
  I     :=SendDlgItemMessage(HWindow,id_filelist,lb_getselcount,0,0);
  SendDlgItemMessage(HWindow,id_filelist,lb_getselitems,maxplaylistentries,longint(puffer));
  IF i>0 THEN
  BEGIN
    oldcursor:=SetCursor (LoadCursor(0,idc_wait));
    IF aktliste<>'' THEN
    BEGIN
      FOR x:=0 TO (i-1) DO
      BEGIN
        pfilelist^.GetString(s,puffer^[x]);
        IF (NOT sauglistobj^.from2to1_entrie(NewList,s))AND(NewList^.addsongstring(S))
        THEN pplaylist^.AddString(s) ELSE puffer^[x]:=-1;
       END;
      END ELSE
    BEGIN
      FOR x:=0 TO (i-1) DO
      BEGIN
        pfilelist^.GetString(s,puffer^[x]);
        uniteplaylistitem(NewList,s,true);
        IF (NewList^.isinloadlist(s,0)<0)AND(NewList^.addsongstring(s))
        THEN pPlayList^.AddString(S) ELSE puffer^[x]:=-1;
      END;
    END;
    FOR x:=(i-1) DOWNTO 0 DO IF puffer^[x]<>-1 THEN pfilelist^.DeleteString(puffer^[x]);
    SetCursor (oldcursor);
  END;
  Dispose(puffer);
  pfilelist^.setsel(-1,0);
  setlbredraw(_on);
  x:=pplaylist^.getcaretindex;
  IF x<0 THEN pplaylist^.setcaretindex(0);
  afocus:=GetFocus;
  IF (afocus=pfilelist^.HWindow)OR(afocus=pplaylist^.HWindow) THEN setinfo(afocus);
END;


{löscht alle Einträge der Playliste und der NEWLIST}
PROCEDURE open.handleDeleteAll;
VAR i,j,k,l:integer;
    wert,arcdirindex,arcindex:byte;
    nc,s,x:STRING[100];
    afocus:word;
    test:STRING[songlistentriesize];
    oldcursor:hcursor;
BEGIN
 setlbredraw(off);
 i:=pplaylist^.GetCount;
 IF i>0 THEN BEGIN
  oldcursor:=SetCursor (LoadCursor(0,idc_wait));
  FOR j:=0 TO (i-1)DO
  BEGIN
    pplaylist^.GetString(nc,j);
    s:=nc;
    splitplaylistitem(nc,wert,arcindex,arcdirindex);
    IF (aktarcliste<>'')AND(arcindex<>byte(noarcconst)) THEN
    BEGIN
      x:=NewList^.getarcstring(arcindex);
      IF sauglistobj^.getdirstring(1)=NewList^.getdirstring(byte(x[1]))
      THEN
      BEGIN {selbes Archiv}
        x:=NewList^.getarcdirstring(arcdirindex);
        k:=sauglistobj^.arcdirinarcdirlist(x);
        IF (k>0)AND(x=aktarcdir) THEN pfilelist^.AddString(nc);
      END;
    END ELSE
    IF aktliste<>'' THEN
    BEGIN
      l:=NewList^.checkfrom2to1_entrie(sauglistobj,s);
      IF l>-1 THEN
      BEGIN
        s:=sauglistobj^.getsongstring(l)+#0;
        pfilelist^.InsertString(pchar(@s[1]),l);
      END;
    END ELSE IF arcindex=byte(noarcconst) THEN
     IF  NewList^.pathindirlist(path)=wert THEN pfilelist^.AddString(nc)
  END;
  pplaylist^.ClearList;
  NewList^.ClearList;
  SetCursor (oldcursor);
 END;
 setlbredraw(_on);
 afocus:=GetFocus;
 IF (afocus=pfilelist^.HWindow)OR(afocus=pplaylist^.HWindow)
 THEN setinfo(afocus);
END;

{löscht alle selektierten Einträge der Playliste und der NEWLIST}
PROCEDURE open.handleDeleteOne;
VAR i,x,l   :integer;
    {puffer für Listbox - Indizes}
    puffer:pbuffer;
    nc,s  :STRING[50];
    olds:STRING[songlistentriesize+1];
    wert,arcindex,arcdirindex  :byte;
    afocus:word;
    oldcursor:hcursor;
    pf:pfound;
BEGIN
  i:=SendDlgItemMessage(HWindow,id_playlist,lb_getselcount,0,0);
  IF i=0 THEN Exit;
  setlbredraw(off);
  puffer:=New(pbuffer);
  SendDlgItemMessage(HWindow,id_playlist,lb_getselitems,maxplaylistentries,longint(puffer));
  IF i>0 THEN
  BEGIN
    pplaylist^.GetString(olds,pplaylist^.getcaretindex);
    oldcursor:=SetCursor (LoadCursor(0,idc_wait));
    pf:=New(pfound); FillChar(pf^,SizeOf(pf^),#0);
    FOR x:=(i-1) DOWNTO 0 DO
    BEGIN
      pplaylist^.GetString(nc,puffer^[x]);
      s:=nc;
      splitplaylistitem(nc,wert,arcindex,arcdirindex);
      IF aktarcliste<>'' THEN
      BEGIN
         l:=NewList^.checkfrom2to1_entrie(sauglistobj,s);
         IF l>-1 THEN pfilelist^.AddString(nc);
      END ELSE
      IF aktliste<>'' THEN
      BEGIN
         l:=NewList^.checkfrom2to1_entrie(sauglistobj,s);
         IF l>-1 THEN pf^[l]:=True;
      END
      ELSE
      IF (arcindex=byte(noarcconst))AND(NewList^.pathindirlist(path)=wert)
      THEN pfilelist^.AddString(nc);

      pplaylist^.DeleteString(puffer^[x]); NewList^.deletesongstring(puffer^[x]);
      {EVENTUELL überflüssige index-strings entfernen}
      IF NOT(NewList^.isdirindexinsonglist(wert)) THEN NewList^.deletedirstring(wert);
      IF (arcindex<>byte(noarcconst))AND(NOT NewList^.isarcindexinsonglist(arcindex))
        THEN NewList^.deletearcstring(arcindex);
      IF (arcdirindex>1)AND(NOT NewList^.isarcdirindexinsonglist(arcdirindex))
        THEN NewList^.deletearcdirstring(arcdirindex);
    END;
    IF aktliste<>'' THEN
    BEGIN
      FOR x:=0 TO sauglistobj^.songcount-1 DO IF pf^[x] THEN
      BEGIN s:=sauglistobj^.getsongstring(x)+#0; pfilelist^.InsertString(pchar(@s[1]),x); END;
    END;
    Dispose(pf);
    SetCursor (oldcursor);
  END;
  Dispose(puffer);
  newlist^.compactlist;
  pplaylist^.ClearList;
  IF newlist^.songcount>0 THEN
  FOR i:=0 TO newlist^.songcount-1 DO pplaylist^.AddString(newlist^.getsongstring(i));
  olds[Length(olds)+1]:=#0;
  x:=SendMessage(pplaylist^.HWindow,lb_findstring,$FFFF,longint(@olds[1]));
  IF x<0 THEN x:=0;
  setlbredraw(_on);
  afocus:=GetFocus;
  pplaylist^.setcaretindex(x);
  IF (afocus=pfilelist^.HWindow)OR(afocus=pplaylist^.HWindow)
  THEN setinfo(afocus);
END;

{Hilfe-Aufruf}
PROCEDURE open.handleHilfe;
BEGIN
  helpme(@self,id_opendlg);
END;
{Es wurde OK gedrückt - Inputliste updaten und Tschüß}

PROCEDURE open.handleOk;
BEGIN
  IF SaveListWindow<>0 THEN handlelistsave(Msg);
  Inputlist^.ClearList;
  Newlist^.TransferData(Inputlist);
  Inputlist^.lastunpackedsong:=newlist^.lastunpackedsong;
  {DEM Papi die neue Liste melden}
  PostMessage(Parent^.HWindow,wm_getloadings,0,0);
  {Pfad speichern für Neuöffnen von OPEN}
  currentdir:=path;
  PostMessage(HWindow,wm_close,0,0);
END;
{und tschüß}
PROCEDURE open.handleCancel;
BEGIN
  {DEM Papi die neue Open-Closed melden!}
  PostMessage(Parent^.HWindow,wm_getloadings,1,0);
  PostMessage(HWindow,wm_close,0,0);
END;



{Lädt Liste und fügt die validen Einträge der Playliste hinzu}
PROCEDURE open.loadlist;
VAR i,j                 :integer;
    dirindex,aktdirindex:byte;   {aktdirindex ->akt dir("PATH") in PMYDIRS}
    s                   :STRING;
    dummybyte:byte;
BEGIN
  sauglistobj:=New(pmydirs,Init);
  IF sauglistobj^.sauglist(@self,Name)THEN
  BEGIN
    sauglistobj^.checklist(@self,Name,id_nofilesinlist);
    IF sauglistobj^.songcount=0 THEN BEGIN Dispose(Sauglistobj,Done); Sauglistobj:=NIL; Exit; END;
    IF pplaylist^.GetCount=0 THEN sauglistobj^.TransferData(NewList)
                             ELSE sauglistobj^.from2to1(NewList, NIL);
    aktdirindex:=NewList^.pathindirlist(path);
    setlbredraw(off);
    pplaylist^.ClearList;
    FOR i:=0 TO (NewList^.songcount-1) DO
    BEGIN
      s:=NewList^.getsongstring(i);
      pplaylist^.AddString(s);
      splitplaylistitem(s,dirindex,dummybyte,dummybyte); s:=s+#0;
      IF dirindex=aktdirindex THEN
      BEGIN
        j:=pfilelist^.findstring(s);
        IF j>-1 THEN pfilelist^.DeleteString(j);
      END;
    END;
    setlbredraw(_on);
  END;
  Dispose(sauglistobj,Done); sauglistobj:=NIL;
END;


{Lädt Archiv und fügt die validen Einträge der Playliste hinzu}
PROCEDURE open.loadarc;
VAR i,j                 :integer;
    dirindex,aktdirindex:byte;   {aktdirindex ->akt dir("PATH") in PMYDIRS}
    s                   :STRING;
    dummybyte:byte;
BEGIN
  sauglistobj:=New(pmydirs,Init);
  IF sauglistobj^.saugarc(path,Name,false)THEN
  BEGIN
    sauglistobj^.checklist(@self,Name,id_nofilesinarchiv);
    IF sauglistobj^.songcount=0 THEN BEGIN Dispose(sauglistobj,Done); sauglistobj:=NIL; Exit; END;
    IF pplaylist^.GetCount=0 THEN sauglistobj^.TransferData(NewList)
                             ELSE sauglistobj^.from2to1(NewList, NIL);
    aktdirindex:=NewList^.pathindirlist(path);
    setlbredraw(off);
    pplaylist^.ClearList;
    FOR i:=0 TO (NewList^.songcount-1) DO
    BEGIN
      s:=NewList^.getsongstring(i);
      pplaylist^.AddString(s);
      splitplaylistitem(s,dirindex,dummybyte,dummybyte);
      IF dirindex=aktdirindex THEN
      BEGIN
        s:=s+#0;
        j:=pfilelist^.findstring(s);
        IF j>-1 THEN pfilelist^.DeleteString(j);
      END;
    END;
    setlbredraw(_on);
  END ELSE errmessagebox(@self,id_filenotfound);
  Dispose(sauglistobj,Done); sauglistobj:=NIL;
END;

PROCEDURE open.loaddrive;
VAR i,j                 :integer;
    dirindex,aktdirindex:byte;   {aktdirindex ->akt dir("PATH") in PMYDIRS}
    s                   :STRING;
    entrie,test         :STRING[20];
    dummybyte:byte;
    unreached:Integer;
    Args:ARRAY[0..0] OF Integer;
BEGIN
  IF aktarcliste<>'' THEN
  BEGIN
    IF sauglistobj^.songcount=0 THEN Exit;
    s:=Copy(Name,3,byte(Name[0])); s[0]:=char(Pos('>',s)-1);
    setlbredraw(off);
    IF aktarcdir<>'' THEN s:=aktarcdir+'\'+s;
    FOR i:=0 TO sauglistobj^.songcount-1 DO
    BEGIN
      entrie:=sauglistobj^.getsongstring(i); test:=entrie;
      splitplaylistitem(test,dummybyte,dummybyte,dirindex);
      IF Copy(sauglistobj^.getarcdirstring(dirindex),1,Length(s))=s THEN
      BEGIN
        IF NOT sauglistobj^.from2to1_entrie(NewList,entrie) THEN
        BEGIN
          pplaylist^.AddString(entrie); NewList^.addsongstring(entrie);
        END;
      END;
    END;
    setlbredraw(_on);
  END ELSE
  BEGIN
    sauglistobj:=New(pmydirs,Init);
    IF Name[2]='[' THEN s:=Name[4]+':'
    ELSE BEGIN s:=path+'\'+Copy(Name,3,byte(Name[0])); s[0]:=char(Pos('>',s)-1); END;
    s:=s+'\';
    unreached:=sauglistobj^.saugdrive(s);
    IF unreached>=0 THEN
    BEGIN
      sauglistobj^.checklist(@self,s,id_nofilesinarchiv);
      IF sauglistobj^.songcount=0 THEN BEGIN Dispose(sauglistobj,Done); sauglistobj:=NIL; Exit; END;
      IF pplaylist^.GetCount=0 THEN sauglistobj^.TransferData(NewList)
                               ELSE sauglistobj^.from2to1(NewList, NIL);
      IF unreached>0 THEN
      BEGIN
        args[0]:=unreached;
        s:=FormatString(7000,args);
        ourmessagebox(Parent,modc.shortver,s,mb_iconstop OR mb_ok);
      END;
      aktdirindex:=NewList^.pathindirlist(path);
      setlbredraw(off);
      pplaylist^.ClearList;
      FOR i:=0 TO (NewList^.songcount-1) DO
      BEGIN
        s:=NewList^.getsongstring(i);
        pplaylist^.AddString(s);
        splitplaylistitem(s,dirindex,dummybyte,dummybyte);
        IF dirindex=aktdirindex THEN
        BEGIN
          s:=s+#0;
          j:=pfilelist^.findstring(s);
          IF j>-1 THEN pfilelist^.DeleteString(j);
        END;
      END;
      setlbredraw(_on);
    END ELSE errmessagebox(@self,id_nomods);
    Dispose(sauglistobj,Done); sauglistobj:=NIL;
  END;
END;


PROCEDURE OPEN.DestroySaver;
VAR  dc  :HDC;
     rect:TRect;
BEGIN
  IF SaveListWindow<>0 THEN
  BEGIN
    IF WFOC THEN BEGIN OldFocus:=PreviousFocus; SetFocus(PreviousFocus); END;
    PostMessage(SaveListWindow,wm_close,0,0);
    WITH rect DO BEGIN left:=x_boaddone-(4*Fw)-2; top:=CapDY+y_book+28;
                       right:=left+(8*Fw)+4; bottom:=top+Fh+4; END;
    dc:=GetDC(HWindow);
    FillRect(dc,rect,brstatic);
    ReleaseDC(HWindow,dc);
  END;
END;

PROCEDURE OPEN.CreateSaver;
VAR  dc  :hdc;
BEGIN
  IF SaveListWindow=0 THEN
  BEGIN
    dc:=GetDC(HWindow);
    IF PaletteInstalled THEN SelectPalette(dc,hpal,false);
    frame3d(dc,x_boaddone-(4*Fw)-2,CapDY+y_book+28,(8*Fw)+4,Fh+4,2,up);
    ReleaseDC(HWindow,dc);
    PreviousFocus:=GetFocus;
    Application^.MakeWindow(New(pFedit,Init(@self,id_filenameedit,x_boaddone-(4*Fw),
                            CapDY+y_book+30,8*Fw,Fh,9,@SaveListWindow)));
  END;
END;

PROCEDURE open.handlelistsave;
VAR SaveListName  :ARRAY[0..10] OF CHAR;
BEGIN
 IF pplaylist^.GetCount=0 THEN errmessagebox(@self,id_nofilesinlist) ELSE
 BEGIN
   IF SaveListWindow=0 THEN CreateSaver ELSE
   BEGIN
     SendMessage(SaveListWindow,wm_gettext,10,longint(@SaveListName));
     saveliste(strpas(SaveListName));
     DestroySaver(true);
   END;
 END;
END;
{Speichert die aktuelle Playliste auf Platte}
PROCEDURE open.Saveliste;
VAR s :STRING[100];
BEGIN
  IF Registered THEN BEGIN
    IF Length(Name)=0 THEN Exit;
    IF pplaylist^.GetCount=0 THEN errmessagebox(@self,id_nofilesinlist) ELSE
    BEGIN
      s:=Name;
      WHILE (Length(s)>0) AND (Pos(' ',s)>0) DO Delete(s,Pos(' ',s),1);
      IF s<>'' THEN
      BEGIN
        IF Pos('.',s)<>0 THEN s:=Copy(s,1,Pos('.',s)-1);
        s:=path+'\'+s+'.'+filetypesasstring[listfiletype];
        {Speichere Liste und lasse nachfragen wenn Liste schon da}
        newlist^.savelist(@self,s,true);
      END ELSE errmessagebox(@self,id_wronginput);
    END;
  END ELSE errmessagebox (@self,id_nosharesave);
END;
{malt Listboxen}
PROCEDURE open.wmdrawitem(VAR msg:tmessage);
VAR oldfont:hfont;
    nc     :ARRAY[0..itemlength] OF char;
    a,b    :longint;
    c      :integer;
    drawframe,isarc:boolean;
    s      :STRING[50];
    endung :STRING[4];
    memdc  :hdc;
BEGIN
  WITH pdrawitemstruct(msg.lparam)^ DO
  BEGIN
    IF NOT(itemid=$ffff)AND((ctlid=id_filelist)OR(ctlid=id_dirlist)OR(ctlid=id_playlist))THEN
    BEGIN
      IF PaletteInstalled THEN SelectPalette(hdc,hpal,false);
      drawframe:=false;
      {Farben setzen}
      a:=chglb; b:=cVGLB;
      CASE itemaction OF
        oda_focus     :
          CASE itemstate OF
            ods_disabled:
              BEGIN a:=chglb; b:=CVGGrayBox; END;
            ods_focus+ods_selected:
              BEGIN drawframe:=true; b:=cHGLB; a:=cVGLB; END;
            ods_focus    :drawframe:=true;
            1            :BEGIN a:=cVGLB; b:=cHGLB; END;
          END;
        oda_drawentire:IF itemstate=1 THEN BEGIN a:=cVGLB; b:=cHGLB; END;
      END;
      IF (ptempedit<>NIL)AND((ptempedit^.Attr.id=id_leftsel)OR(ptempedit^.Attr.id=id_rightsel)) THEN
      CASE ctlid OF
        id_filelist:
         IF ptempedit^.Attr.id=id_leftsel THEN
         IF word(SendMessage(hwnditem,lb_getcaretindex,0,0))=itemid THEN drawframe:=true;
        id_playlist:
         IF ptempedit^.Attr.id=id_rightsel THEN
         IF word(SendMessage(hwnditem,lb_getcaretindex,0,0))=itemid THEN drawframe:=true;
      END;
      {Textattribute setzen}
      oldfont:=SelectObject(hdc,OEMhandle);
      SetBkMode(hdc,transparent);
      SetBkColor(hdc,a); SetTextColor(hdc,b);
      SetTextAlign(hdc,ta_left OR ta_top OR ta_noupdatecp);
      {Text holen}
      c:=-1;
      CASE ctlid OF
       id_filelist:c:=pfilelist^.GetString(s,itemid);
       id_dirlist :c:=pdirlist^ .GetString(s,itemid);
       id_playlist:c:=pplaylist^.GetString(s,itemid);
      END;
      {wenn Text vorhanden dann malen}
      IF c>=0 THEN
      BEGIN
        s[Length (s) + 1] := #0;
        IF (ctlid=id_playlist)OR((aktliste<>'')AND(ctlid=id_filelist))
          THEN AnsiToOem (@s[6], @s[6])
          ELSE AnsiToOem (@s[2], @s[2]);
        memdc:=CreateCompatibleDC(hdc);
        isarc:=false;
        IF ctlid<>id_dirlist THEN
        BEGIN
          endung:='.'+filetypesasstring[byte(s[1])];
          SelectObject(memdc,bmphandles[byte(s[1])-3]);
          IF (ctlid=id_playlist)OR((aktliste<>'')AND(ctlid=id_filelist)) THEN
          BEGIN
            IF s[4]<>noarcconst THEN isarc:=true;
            s:='   '+s[2]+' '+Copy(s,6,byte(s[0]));
          END ELSE IF (ctlid=id_filelist) THEN
          BEGIN
            WHILE Length(s)<9 DO Insert(' ',s,Length(s)+1);
            s:='    '+Copy(s,2,byte(s[0]));
            IF aktarcliste<>'' THEN isarc:=true;
          END;
          s:=s+endung+#0;
          ExtTextOut(hdc,rcitem.left+17,rcitem.top,eto_opaque,@rcitem,pchar(@s[1]),byte(s[0])-1,NIL);
          BitBlt(hdc,rcitem.left+1,rcitem.top,rcitem.right-rcitem.left,
               rcitem.bottom-rcitem.top,memdc,0,0,srccopy);
          IF isarc THEN
          BEGIN
            SelectObject(memdc,arcbmphandle);
            BitBlt(hdc,rcitem.left+30,rcitem.top,rcitem.right-rcitem.left,
                   rcitem.bottom-rcitem.top,memdc,0,0,srccopy);
          END;
        END ELSE
        BEGIN
        {Dirliste Text ausgeben und Bitmap malen}
          ExtTextOut(hdc,rcitem.left+38,rcitem.top,eto_opaque,@rcitem,pchar(@s[2]),byte(s[0])-1,NIL);
          SelectObject(memdc,drivebmphandle);
          IF (s[1]>=dlfloppytype)AND(s[1]<=dlremotetype)
            THEN  c:=(byte(s[1])-byte(dlfloppytype)+3)*28
            ELSE IF s[1]=dldirtype THEN c:=0 ELSE
          IF (byte(s[1])-dlarcoffset)=listfiletype THEN c:=28 ELSE c:=56;
          BitBlt(hdc,rcitem.left+1,rcitem.top,28,
              rcitem.bottom-rcitem.top,memdc,c,0,srccopy);
        END;
        IF drawframe THEN DrawFocusRect(hdc,rcitem);
        DeleteDC(memdc);
      END;
    END;
  END;
END;
{Farben+HG-BRUSH für diverse Statics,edits,LISTBOXEN setzen}
PROCEDURE open.wmctlcolor(VAR msg:tmessage);
BEGIN
  IF msg.lparamlo=pathname^.HWindow THEN msg.lparamhi:=ctlcolor_edit;
  INHERITED wmctlcolor(msg);
END;
{Last-Focus für Maus - Klicks auf BOADD speichern}
PROCEDURE open.wmIHaveTheFocus(VAR msg:tmessage);
BEGIN
  lastfocus:=oldfocus;
  setinfo(msg.wparam);
  INHERITED wmihavethefocus(msg);
  IF (msg.wparam<>book^.HWindow)AND(msg.wparam<>bosave^.HWindow)AND(msg.wparam<>SaveListWindow) THEN
  IF (SaveListWindow<>0) THEN DestroySaver(false);
END;
{ALT-L,F,D,P abfangen (Anfangsbuchstaben für List-Boxen)}
PROCEDURE open.wmsyschar;
BEGIN
 CASE Lo(msg.wparam) OF
  70,102:pfilelist^.focus;      {klein f+F}
  68,100:pdirlist^.focus;       {klein d+D}
  80,112:pplaylist^.focus;      {klein p+P}
   ELSE BEGIN INHERITED wmsyschar(msg); Exit; END;
 END;
 msg.result:=-1;
END;
{ALT-Funktionstasten abfangen}
PROCEDURE open.wmsyskeydown;
VAR s:STRING[10];
    j:word;
BEGIN
  IF (keystate<>4) THEN DefWndProc(msg)
  ELSE CASE msg.wparam OF
    vk_f1: handleselection(id_olplus,vk_f1);
    vk_f3: handleselection(id_orplus,vk_f3);
    vk_f2: BEGIN
             s:='[-'+path[1]+'-]'+#0;
             j:=GetDriveType(word(path[1])-97);
             IF j=drive_removable THEN Insert(dlfloppytype,s,1) ELSE
             IF j=drive_fixed THEN Insert(dlhdtype,s,1) ELSE Insert(dlremotetype,s,1);
             pdirlist^.SetSelString(pchar(@s[1]),-1);
             pdirlist^.focus;
             setinfo(pdirlist^.HWindow);
             msg.result:=0;
           END;
    ELSE DefWndProc(msg);
  END ;
END;
{****ENDE***************************************Setup********************************************************}

CONSTRUCTOR tmydirsARC.INIT(aLimit:Word);
BEGIN
  dircount   :=0;
  songcount  :=0;
  arccount   :=0;
  arcdircount:=0;
  entpacke   :=false;
  newlist    :=true;
  lastunpackedsong:='';
  lastupindex:=$FFFF;
  Limit      :=aLimit;
  IF (LongInt(songlistentriesize+1)*Limit)>$10000 THEN Limit:=$10000 DIV LongInt(songlistentriesize+1);
  psonglist  :=MyGlobalAllocPtr(gptr OR gmem_share,longint((songlistentriesize+1)*Limit));
  FillChar(dirs,SizeOf(dirs),#0);
  FillChar(arcs,SizeOf(arcs),#0);
  FillChar(arcdirs,SizeOf(arcdirs),#0);
END;


DESTRUCTOR tmydirsARC.Done;
BEGIN
  IF psonglist<>NIL THEN globalfreeptr(psonglist);
END;


{****************Directory und Songlist-Verwaltung************}
CONSTRUCTOR tmydirs.Init;
BEGIN
  dircount   :=0;
  songcount  :=0;
  arccount   :=0;
  arcdircount:=0;
  entpacke   :=false;
  newlist    :=true;
  lastunpackedsong:='';
  lastupindex:=$FFFF;
  Limit      :=MaxPlayListEntries;
  IF (LongInt(songlistentriesize+1)*Limit)>$10000 THEN Limit:=$10000 DIV LongInt(songlistentriesize+1);
  {$IFDEF DEMO}
  pSongList  :=@SongArr;
  {$ELSE}
  psonglist  :=MyGlobalAllocPtr(gptr OR gmem_share,longint((songlistentriesize+1)*Limit));
  {$ENDIF}
  FillChar(dirs,SizeOf(dirs),#0);
  FillChar(arcs,SizeOf(arcs),#0);
  FillChar(arcdirs,SizeOf(arcdirs),#0);
END;

DESTRUCTOR tmydirs.Done;
BEGIN
  {$IFNDEF DEMO}
  IF psonglist<>NIL THEN globalfreeptr(psonglist);
  {$ENDIF}
END;

PROCEDURE tmydirs.grillunpackedsong;
VAR f:FILE;
    olderrormode:word;
BEGIN
  olderrormode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  IF lastunpackedsong<>'' THEN
  BEGIN
   {$i-}
   Assign(f,lastunpackedsong);
   Erase(f);
  END;
  SetErrorMode(olderrormode);
END;

{PLAYLISTE und Dirliste zurücksetzen}
PROCEDURE tmydirs.ClearList;
VAR    i:integer;
BEGIN
  songcount  :=0;
  dircount   :=0;
  arccount:=0;
  arcdircount:=0;
  newlist:=true;
  FillChar(dirs,SizeOf(dirs),#0);
  FillChar(arcs,SizeOf(arcs),#0);
  FillChar(arcdirs,SizeOf(arcdirs),#0);
END;


PROCEDURE tmydirs.TransferData;
BEGIN
  destlist^.dircount   :=dircount;
  destlist^.arccount   :=arccount;
  destlist^.arcdircount:=arcdircount;
  destlist^.songcount  :=songcount;
  IF DestList^.SongCount>DestList^.Limit THEN DestList^.SongCount:=DestList^.Limit;
  destlist^.dirs       :=dirs;
  destlist^.arcs       :=arcs;
  destlist^.arcdirs    :=arcdirs;
{  destlist^.lastunpackedsong:=lastunpackedsong;}
  destlist^.lastupindex:=lastupindex;
  IF DestList^.LastUpIndex>DestList^.Limit THEN DestList^.LastUpIndex:=$FFFF;
  IF destlist^.psonglist=NIL THEN
     destlist^.psonglist:=Myglobalallocptr(gptr,((songlistentriesize+1)*DestList^.Limit));
  Move(psonglist^,destlist^.psonglist^,((songlistentriesize+1)*destlist^.SongCount));
END;

FUNCTION tmydirs.openlistfile;
VAR s:STRING;
BEGIN
{$i-}
  filemode:=0;
  Assign(f,Name);
  inoutres:=0;
  Reset (f);
  IF IOResult=0 THEN
  BEGIN
    ReadLn(f,s);
    IF s='[DIRECTORIES]' THEN openlistfile:=true
    ELSE errmessagebox(Parent,id_invlistFile)
  END ELSE
  BEGIN
    errmessagebox(Parent,id_Filenotfound);
    openlistfile:=false;
  END;
END;

FUNCTION tmydirs.sauglist;
VAR f   :text;
    s   :STRING;
    tstr:PString;
    rec :tsearchrec;
    eod :Boolean;
    drive:Byte;
    index:integer;
    oldmode:Word;
    oldcursor:HCursor;
    DriveMap:ARRAY[0..25] OF CHAR;
BEGIN
  oldmode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  oldcursor:=SetCursor(LoadCursor(0,idc_wait));
  sauglist:=false;
  IF openlistfile(Parent,f,Name) THEN
  BEGIN
    FillChar(DriveMap,SizeOf(DriveMap),0);
    ReadLn(f,s);
    GetMem(TStr,256);
    WHILE (s<>'[ARCHIVES]')AND(NOT Eof(f))DO
    BEGIN
      index:=byte(s[1])-indexoffset;
      Delete(s,1,1); WHILE (Pos(' ',s)>0)AND(Length(s)>0) DO Delete(s,Pos(' ',s),1);
      IF Length(s)>0 THEN
      BEGIN
        IF s[Length(s)]<>'\' THEN TStr^:=s+'\*.*' ELSE TStr^:=s+'*.*';
        TStr^[Length(TStr^)+1]:=#0;
        IF DriveMap[Byte(UpCase(s[1]))-65]<>#0 THEN
        BEGIN
          s[1]:=DriveMap[Byte(UpCase(s[1]))-65];
        END ELSE
        BEGIN
          Doserror:=0;
          FindFirst(@TStr^[1],faDirectory+faHidden+faReadOnly+faSysFile+faArchive,rec);
          IF doserror<>0 THEN
          BEGIN
            DosError:=255;
            drive:=2; eod:=false;
            WHILE (DOSERROR<>0)AND(NOT eod) DO
            BEGIN
              tstr^[1]:=char(drive+65);
              FindFirst(@TStr^[1],faDirectory+faHidden+faReadOnly+faSysFile+faArchive,rec);
              inc(drive);
              IF drive>26 THEN eod:=True;
            END;
            IF doserror=0 THEN
            BEGIN
              IF GetDriveType(Byte(UpCase(tstr^[1]))-65)>=Drive_remote
               THEN DriveMap[Byte(UpCase(s[1]))-65]:=tstr^[1];
              s[1]:=tstr^[1];
            END;
          END;
        END;
        IF dircount<maxdirlistentries THEN
        BEGIN
          setdirstring(index,s);
          inc(dircount);
        END;
      END;
      ReadLn(f,s);
    END;
    FreeMem(TStr,256);
    IF Eof(f) THEN BEGIN Close(f); errmessagebox(Parent,id_invlistFile); Exit; END;
    ReadLn(f,s);
    WHILE (s<>'[ARCHIVDIRECTORIES]')AND(NOT Eof(f))DO
    BEGIN
      s[1]:=char(byte(s[1])-indexoffset);
      s[2]:=char(byte(s[2])-indexoffset);
      addarcstring(s);
      ReadLn(f,s);
    END;
    IF Eof(f) THEN BEGIN Close(f); errmessagebox(Parent,id_invlistFile); Exit; END;
    ReadLn(f,s);
    WHILE (s<>'[FILES]')AND(NOT Eof(f))DO
    BEGIN
      Delete(s,1,1);
      addarcdirstring(s);
      ReadLn(f,s);
    END;
    IF Eof(f) THEN BEGIN Close(f); errmessagebox(Parent,id_invlistFile); Exit; END;
    WHILE NOT Eof(f) DO
    BEGIN
      ReadLn(f,s);
      s[1]:=char(byte(s[1])-indexoffset);
      s[2]:=char(byte(s[2])-indexoffset);
      s[3]:=char(byte(s[3])-indexoffset);
      s[4]:=char(byte(s[4])-indexoffset);
      Insert(willplay,s,2);
      IF byte(s[1])<=allfiletypes THEN addsongstring(s);
    END;
    Close(f);
    compactlist;
    sauglist:=true;
  END;
  SetErrorMode(oldmode);
  SetCursor(oldcursor);
END;

PROCEDURE tmydirs.checklist;
VAR rec:tsearchrec;
    i,j,z,counter:integer;
    s:STRING[140];
    found:pfound;
    arcfound:ARRAY[1..maxarclistentries]OF boolean;
    index:byte;
    box  :Pwaitbox;
    oldmode:word;
    drivemap:ARRAY[0..25] OF Boolean;
BEGIN
  FillChar(DriveMap,SizeOf(DriveMap),0);
  oldmode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  box:=initwaitbox(Parent,getresstring(id_loadinglist)+' '+Name);
  compactlist;
  found:=New(pfound);
  FillChar(found^,SizeOf(tfound),#0);
  FillChar(arcfound,SizeOf(arcfound),#0);
  z:=dircount;
  FOR i:=1 TO z DO
  BEGIN
    setwaitbox(box,(100.0*i)/z);
    s:=getdirstring(i)+'\*.*'+#0;
    IF GetDriveType(Byte(UpCase(s[1]))-65)>=Drive_remote THEN
    BEGIN
      IF DriveMap[Byte(UpCase(s[1]))-65] THEN DOSERROR:=0
      ELSE FindFirst(@s[1],faHidden+faReadOnly+faSysFile+faArchive,rec);
      IF DosError=0 THEN
      BEGIN
        DriveMap[Byte(UpCase(s[1]))-65]:=True;
        FOR j:=0 TO SongCount-1 DO IF psonglist^[j][3]=char(i) THEN found^[j]:=True;
        IF arccount>0 THEN FOR j:=1 TO ArcCount DO IF Arcs[j][1]=char(i) THEN arcfound[j]:=true;
      END ELSE deletedirstring(i);
    END ELSE
    BEGIN
      FindFirst(@s[1],faHidden+faReadOnly+faSysFile+faArchive,rec);
      counter:=0;
      WHILE doserror=0 DO
      BEGIN
        IF (IsValid(NOT isitdirectory,rec,index))AND(index>listfiletype) THEN
        BEGIN
          FillChar(s,20,32);
          s:=strpas(AnsiLower(rec.Name));
          s:=char(index)+willplay+char(i)+noarcconst+#1+Copy(s,1,Pos('.',s)-1);
          s[0]:=char(songlistentriesize);
          j:=isinloadlist(s,0);
          IF j>-1 THEN BEGIN inc(counter); found^[j]:=true; END;
        END ELSE IF isvalidarchive(rec,index) THEN
        BEGIN
          s:=char(byte(i))+char(index)+strpas(AnsiLower(rec.Name));
          j:=archiveinarclist(s,true);
          IF j>-1 THEN BEGIN inc(counter); arcfound[j]:=true; END;
        END;
        FindNext(rec);
      END;
      IF counter=0 THEN deletedirstring(i);
    END;
  END;
  FOR i:=songcount-1 DOWNTO 0 DO
    IF (NOT found^[i]) THEN
       IF (psonglist^[i][4]=noarcconst) THEN deletesongstring(i)
       ELSE IF NOT arcfound[byte(psonglist^[i][4])] THEN deletesongstring(i);
  IF arccount>0 THEN FOR i:=1 TO arccount DO IF NOT arcfound[i] THEN   deletearcstring(i);
  closewaitbox(box);
  Dispose(found);
  IF (songcount=0) AND (errid<>0) THEN errmessagebox(Parent,errid);
  SetErrorMode(oldmode);
END;

PROCEDURE tmydirs.from2to1(destlist:pmydirs; pFoundArr:pDynFound);
VAR newdirs    :ARRAY[0..maxdirlistentries] OF byte;
    newarcs    :ARRAY[0..maxarclistentries] OF byte;
    newarcdirs :ARRAY[0..maxarcdirentries] OF byte;
    i:integer;
    index:integer;
    s:STRING;
BEGIN
  FOR i:=1 TO dircount DO
  BEGIN
    s:=getdirstring(i);
    index:=destlist^.pathindirlist(s);               {ist Pfad schon in DIRS-Array enthalten}
    IF index<0 THEN index:=destlist^.adddirstring(s);
    newdirs[i]:=index;
  END;
  IF arccount>0 THEN
  BEGIN
    FOR i:=1 TO arccount DO
    BEGIN
      s:=getarcstring(i);
      s[1]:=char(newdirs[byte(s[1])]);
      index:=destlist^.archiveinarclist(s,true);               {ist ARC schon in ARC-Array enthalten}
      IF index<0 THEN index:=destlist^.addarcstring(s);
      newarcs[i]:=index;
    END;
    IF arcdircount>0 THEN
    FOR i:=1 TO (arcdircount+1) DO
    BEGIN
      s:=getarcdirstring(i);
      index:=destlist^.arcdirinarcdirlist(s);               {ist Pfad schon in DIRS-Array enthalten}
      IF index<0 THEN index:=destlist^.addarcdirstring(s);
      newarcdirs[i]:=index;
    END ELSE BEGIN newarcdirs[0]:=1; newarcdirs[1]:=1; newarcdirs[2]:=1; END;
  END;
  IF pFoundArr<>NIL THEN FillChar(pFoundArr^, SongCount, 0);
  FOR i:=0 TO SongCount-1 DO
  BEGIN
    s:=getsongstring(i);
    s[3]:=char(newdirs[byte(s[3])]); {neues Dir einfuegen!}
    IF s[4]<>noarcconst THEN
    BEGIN{ARCHIV-EINTRAG}
      s[4]:=char(newarcs[byte(s[4])]);
      s[5]:=char(newarcdirs[byte(s[5])]);
    END;
    IF destlist^.isinloadlist(s,0)<0 THEN IF destlist^.addsongstring(s)
    THEN IF pFoundArr<>NIL THEN pFoundArr^[I]:=True;
  END;
END;

FUNCTION tmydirs.checkfrom2to1_entrie;
VAR dir,Arc,arcdir,s:STRING[80];
    dirindex,arcindex,arcdirindex,index:integer;
BEGIN
  checkfrom2to1_entrie:=-1;
  dir:=getdirstring(byte(Name[3]));
  dirindex:=destlist^.pathindirlist(dir);
  IF dirindex<0 THEN Exit;
  Name[3]:=char(byte(dirindex));
  Arc:=getarcstring(byte(Name[4]));
  IF Arc='' THEN
  BEGIN
    arcindex:=byte(noarcconst);
    arcdirindex:=1;
  END ELSE
  BEGIN
    Arc[1]:=char(byte(dirindex));
    arcindex:=destlist^.archiveinarclist(Arc,true);
    IF arcindex<0 THEN Exit;
    arcdir:=getarcdirstring(byte(Name[5]));
    arcdirindex:=destlist^.arcdirinarcdirlist(arcdir);
    IF arcdirindex<0 THEN Exit;
  END;
  Name[4]:=char(byte(arcindex));
  Name[5]:=char(byte(arcdirindex));
  checkfrom2to1_entrie:=destlist^.isinloadlist(Name,0);
END;
FUNCTION tmydirs.from2to1_entrie;
VAR dir,Arc,arcdir,s:STRING[80];
    dirindex,arcindex,arcdirindex,index:integer;
BEGIN
  from2to1_entrie:=false;
  dir:=getdirstring(byte(Name[3]));
  dirindex:=destlist^.pathindirlist(dir);
  IF dirindex<0 THEN dirindex:=destlist^.adddirstring(dir);
  Name[3]:=char(byte(dirindex));
  Arc:=getarcstring(byte(Name[4]));
  IF Arc='' THEN
  BEGIN
    arcindex:=byte(noarcconst);
    arcdirindex:=1;
  END ELSE
  BEGIN
    Arc[1]:=char(byte(dirindex));
    arcindex:=destlist^.archiveinarclist(Arc,true);
    IF arcindex<0 THEN arcindex:=destlist^.addarcstring(Arc);
    arcdir:=getarcdirstring(byte(Name[5]));
    arcdirindex:=destlist^.arcdirinarcdirlist(arcdir);
    IF arcdirindex<0 THEN arcdirindex:=destlist^.addarcdirstring(arcdir);
  END;
  Name[4]:=char(byte(arcindex));
  Name[5]:=char(byte(arcdirindex));
  IF destlist^.isinloadlist(Name,0)>-1 THEN from2to1_entrie:=true;
END;

VAR aktdirs:pmydirs;
    aktdirindex,aktarcindex:integer;
    alledirs:boolean;
    dirscounted,dirsreached:Word;

PROCEDURE drivecallback(s:STRING); far;
VAR inp:STRING;
    i:integer;
    deststring,tempstr:STRING[songlistentriesize];
    rec:tsearchrec;
    filetypeindex:byte;
    dirindex:integer;
BEGIN
  inp:=s;
  IF inp[0]<>#0 THEN
  BEGIN
    deststring:='';
    {Dir+Name trennen}
    i:=Length(inp);
    WHILE inp[i]<>'\' DO dec(i);
    deststring:=Copy(inp,i+1,Length(inp)-i);
    IF i>0 THEN inp[0]:=char(i-1) ELSE inp[0]:=#0;
    {Ist dieser Eintrag valid}
    strpcopy(rec.Name,deststring); rec.Attr:=faArchive;
    IF aktdirs^.isvalidarchive(rec,filetypeindex) THEN
    BEGIN
      aktdirs^.saugarc(inp,deststring,True);
    END ELSE
    IF  aktdirs^.IsValid(NOT isitdirectory,rec,filetypeindex)THEN
    BEGIN
      IF filetypeindex=listfiletype THEN
      BEGIN
        {aktdirs^.sauglist(Application^.MainWindow,s);}
      END ELSE
      BEGIN
        {Archivdirectory checken}
        dirindex:=aktdirs^.pathindirlist(inp);
        IF dirindex<0 THEN
        BEGIN
          INC(dirscounted);
          dirindex:=aktdirs^.adddirstring(inp);
          IF dirindex>-1 THEN inc(dirsreached);
        END;
        IF dirindex>-1 THEN
        BEGIN
          tempstr:=Copy(deststring,1,(Pos('.',deststring))-1);
          FillChar(deststring,SizeOf(deststring),#32);
          deststring:=char(filetypeindex)+willplay+char(dirindex)+noarcconst+#1+tempstr;
          deststring[0]:=char(songlistentriesize);
          aktdirs^.addsongstring(deststring);
        END;
      END;
    END;
  END;
END;

FUNCTION tmydirs.saugdrive;
VAR cb:tdircallback;
    olderrormode:word;
    oldcursor:hcursor;
BEGIN
  aktdirs:=@self;
  doserror:=0;
  dirscounted:=0;
  dirsreached:=0;
  olderrormode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  oldcursor:=SetCursor(LoadCursor(0,idc_wait));
  @cb:=MakeProcInstance(@drivecallback,hinstance);
  listdosfilesto(cb,Name);
  FreeProcInstance(@cb);
  SetCursor(oldcursor);
  SetErrorMode(olderrormode);
  IF (songCount>0) THEN saugdrive:=dirscounted-dirsreached ELSE saugdrive:=-1;
END;

PROCEDURE listcallback(s:STRING); far;
VAR inp:STRING;
    i:integer;
    deststring,tempstr:STRING[songlistentriesize];
    rec:tsearchrec;
    arcdirindex:integer;
    filetypeindex:byte;
BEGIN
  inc(jumps);
  inp:=s;
  IF inp[0]<>#0 THEN
  BEGIN
    { Die / durch \ ersetzen}
    i:=Pos('/',inp);
    WHILE i>0 DO BEGIN inp[i]:='\'; i:=Pos('/',inp); END;
    deststring:='';
    {Dir+Name trennen}
    i:=Length(inp);
    WHILE inp[i]<>'\' DO dec(i);
    deststring:=Copy(inp,i+1,Length(inp)-i);
    IF i>0 THEN inp[0]:=char(i-1) ELSE inp[0]:=#0;
    IF inp[Length(inp)]='\' THEN Exit;
    IF ALLEDIRS THEN
    BEGIN
      arcdirindex:=aktdirs^.arcdirinarcdirlist(inp);
      IF arcdirindex<0 THEN arcdirindex:=aktdirs^.addarcdirstring(inp);
    END;
    {Ist dieser Eintrag valid}
    strpcopy(rec.Name,deststring); rec.Attr:=faArchive;
    IF aktdirs^.IsValid(NOT isitdirectory,rec,filetypeindex)AND(filetypeindex>listfiletype)THEN
    BEGIN
      {Archivdirectory checken}
      IF NOT alledirs THEN
      BEGIN
        arcdirindex:=aktdirs^.arcdirinarcdirlist(inp);
        IF arcdirindex<0 THEN arcdirindex:=aktdirs^.addarcdirstring(inp);
      END;
      tempstr      :=Copy(deststring,1,(Pos('.',deststring))-1);
      FillChar(deststring,SizeOf(deststring),#32);
      deststring   :=char(filetypeindex)+willplay+char(aktdirindex)+char(aktarcindex)+char(arcdirindex)+tempstr;
      deststring[0]:=char(songlistentriesize);
      aktdirs^.addsongstring(deststring);
    END;
  END;
END;

FUNCTION tmydirs.saugarc;
VAR cb:tdircallback;
    arcrec           :tsearchrec;
    arctype          :byte;
    _Result:boolean;
    olderrormode:word;
    oldcursor        :hcursor;
BEGIN
  alledirs:=alldirs;
  aktdirs:=@self;
  jumps:=0;
  oldcursor:=SetCursor(LoadCursor(0,idc_wait));
  saugarc:=false;
  strpcopy(arcrec.Name,Name);
  arcrec.Attr:=faArchive;
  IF isvalidarchive(arcrec,arctype) THEN
  BEGIN
    olderrormode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
    ASM
     mov word Ptr cb,offset listcallback
     mov word Ptr cb+2,cs
    END;
    aktdirindex:=adddirstring(dir);
    aktarcindex:=addarcstring(char(aktdirindex)+char(arctype)+Name);
    CASE arctype OF
      arjfiletype  :_Result:=listarjfilesto(cb,dir+'\'+Name+#0);
      zipfiletype  :_Result:=listzipfilesto(cb,dir+'\'+Name+#0);
      lharcfiletype:_Result:=listlhafilesto(cb,dir+'\'+Name+#0);
    END;
    SetErrorMode(olderrormode);
  END;
  SetCursor(oldcursor);
  IF _Result THEN saugarc:=true;
  SetCursor(oldcursor);
  alledirs:=false;
END;

{Ist path in Directory-Liste}
FUNCTION tmydirs.pathindirlist(path:STRING):integer;
VAR i:integer;
    gefunden:boolean;
    nc:ARRAY[0..80] OF char;
BEGIN
  i:=1;
  IF path[Length(path)]='\' THEN dec(path[0]);
  pathindirlist:=-1;
  gefunden:=false;
  strpcopy(@nc,path);
  path:=strpas(AnsiLower(@nc));
  IF dircount > 0 THEN
  BEGIN
   WHILE (i<maxdirlistentries) AND NOT(gefunden) DO
   BEGIN
     gefunden:= dirs[i]=path ;
     inc(i,1);
   END;
   IF gefunden THEN pathindirlist:=(i-1);
  END;
END;

{Pfad aus Directory-Liste entfernen}
PROCEDURE tmydirs.deletedirstring(At:integer);
BEGIN
  IF At<=maxdirlistentries THEN
  BEGIN
    dirs[At][0]:=#0;
    IF dircount>0 THEN dec(dircount);
  END;
END;
{Directory-String zur Dirliste hinzufuegen und Position zurückgeben}
FUNCTION tmydirs.adddirstring(Name:STRING):integer;
VAR i:integer;
    nc:ARRAY[0..80] OF char;
BEGIN
  IF Name[byte(Name[0])]='\' THEN dec(Name[0]);
  strpcopy(nc,Name);
  Name:=strpas(AnsiLower(nc));
  i:=1;
  WHILE dirs[i][0]<>#0 DO IF Dirs[i]=Name THEN
  BEGIN
    adddirstring:=i;
    Exit;
  END ELSE inc(i);
  adddirstring:=-1;
  IF dircount<maxdirlistentries THEN
  BEGIN
    dirs[i]:=Name;
    inc(dircount);
    adddirstring:=i;
  END;
END;
FUNCTION tmydirs.setdirstring;
VAR i:integer;
    nc:ARRAY[0..80] OF char;
BEGIN
  IF At<=maxdirlistentries THEN
  BEGIN
    IF Name[byte(Name[0])]='\' THEN dec(Name[0]);
    strpcopy(nc,Name);
    Name:=strpas(AnsiLower(nc));
    dirs[At]:=Name;
    setdirstring:=At;
  END ELSE setdirstring:=-1;
END;

{liefert PFADNAMEN für at INDEX zurück}
FUNCTION tmydirs.getdirstring(At:integer):STRING;
BEGIN
  IF (At>0)AND(At<=maxdirlistentries) THEN getdirstring:=dirs[At]
                                      ELSE getdirstring:='';
END;


{Schaut in der SONGLISTE nach, ob Directory-Index WO enthalten ist}
FUNCTION tmydirs.isdirindexinsonglist(wo:integer):boolean;
VAR i     :integer;
    s       :STRING[songlistentriesize];
    gefunden:boolean;
BEGIN
   i:=0;
   gefunden:=false;
   WHILE ((i<(songcount)) AND (NOT gefunden)) DO
   BEGIN
    s:=getsongstring(i);
    gefunden:=byte(s[3])=byte(wo);
    inc(i);
   END;
  isdirindexinsonglist:=gefunden;
END;


{Ist Archiv in Archiv-Liste}
FUNCTION tmydirs.archiveinarclist(Anarchive:STRING; mitdir:boolean):integer;
VAR i:integer;
    gefunden:boolean;
    nc:ARRAY[0..80] OF char;
BEGIN
  i:=1;
  archiveinarclist:=-1;
  gefunden:=false;
  IF arccount > 0 THEN
  BEGIN
   WHILE (i<=maxarclistentries) AND NOT(gefunden) DO
   BEGIN
     IF mitdir THEN gefunden:= arcs[i]=Anarchive
     ELSE gefunden:=Copy(arcs[i],2,byte(arcs[i][0])-1)=Copy(anarchive,2,byte(anarchive[0])-1);
     inc(i,1);
   END;
   IF gefunden THEN archiveinarclist:=(i-1);
  END;
END;

{Archiv aus Archiv-Liste entfernen}
PROCEDURE tmydirs.deletearcstring(At:integer);
BEGIN
  IF (At<maxarclistentries) THEN
  BEGIN
    arcs[At][0]:=#0;
    IF arccount>0 THEN dec(arccount);
  END;
END;
{Archiv zur Arcliste hinzufuegen und Position zurückgeben}
FUNCTION tmydirs.addarcstring(Name:STRING):integer;
VAR i:integer;
BEGIN
  i:=1;
  WHILE arcs[i][0]<>#0 DO IF arcs[i]=Name THEN
  BEGIN
    addarcstring:=i;
    Exit;
  END ELSE inc(i);
  IF arccount<maxarclistentries THEN
  BEGIN
    arcs[i]:=Name;
    inc(arccount);
    addarcstring:=i;
  END ELSE addarcstring:=-1;
END;

FUNCTION tmydirs.isarcindexinsonglist(wo:integer):boolean;
VAR i     :integer;
    s       :STRING[songlistentriesize];
    gefunden:boolean;
BEGIN
   i:=0;
   gefunden:=false;
   WHILE ((i<(songcount)) AND (NOT gefunden)) DO
   BEGIN
    s:=getsongstring(i);
    gefunden:=byte(s[4])=byte(wo);
    inc(i);
   END;
  isarcindexinsonglist:=gefunden;
END;

{liefert Archive-Namen für at INDEX zurück}
FUNCTION tmydirs.getarcstring(At:integer):STRING;
BEGIN
  IF (At>0)AND(At<=maxarclistentries)
   THEN getarcstring:=arcs[At]
   ELSE getarcstring:='';
END;

PROCEDURE tmydirs.deletearcdirstring(At:integer);
BEGIN
  IF (At>1)AND(At<=maxarcdirentries) THEN
  BEGIN
    arcdirs[At][0]:=#0;
    IF arcdircount>0 THEN dec(arcdircount);
  END;
END;

FUNCTION tmydirs.getarcdirstring(At:integer):STRING;
BEGIN
  IF (At>1)AND(At<=maxarcdirentries) THEN  getarcdirstring:=arcdirs[At]
                                     ELSE getarcdirstring:='';
END;

FUNCTION tmydirs.Addarcdirstring(Name:STRING):integer;
VAR i:integer;
BEGIN
  IF arcdircount<maxarcdirentries THEN
  BEGIN
  i:=2;
  WHILE (arcdirs[i][0]<>#0)AND(i<maxarcdirentries) DO inc(i);
  arcdirs[i]:=Name;
  inc(arcdircount);
  addarcdirstring:=i;
  END ELSE addarcdirstring:=-1;
END;
FUNCTION tmydirs.isarcdirindexinsonglist(wo:integer):boolean;
VAR i     :integer;
    s       :STRING[songlistentriesize];
    gefunden:boolean;
BEGIN
   i:=0;
   gefunden:=false;
   WHILE ((i<(songcount)) AND (NOT gefunden)) DO
   BEGIN
    s:=getsongstring(i);
    gefunden:=byte(s[5])=byte(wo);
    inc(i);
   END;
  isarcdirindexinsonglist:=gefunden;
END;

FUNCTION tmydirs.arcdirinarcdirlist(AnArcDir:STRING):integer;
VAR i:integer;
    gefunden:boolean;
BEGIN
  IF anarcdir[0]=#0 THEN arcdirinarcdirlist:=1 ELSE
  BEGIN
   i:=2;
   arcdirinarcdirlist:=-1;
   gefunden:=false;
   IF arcdircount > 0 THEN
   BEGIN
    WHILE (i<maxarcdirentries) AND NOT(gefunden) DO
    BEGIN
      gefunden:= arcdirs[i]=AnArcDir ;
      inc(i,1);
    END;
    IF gefunden THEN arcdirinarcdirlist:=(i-1);
   END;
  END;
END;



{SONGNAME mit Attributen an Position INDEX der SONGLIST zurückgeben}
FUNCTION tmydirs.getsongstring(index:word):STRING;
BEGIN
  IF (psonglist<>NIL)AND(SongCount>0) THEN
  BEGIN
   IF Index<Limit THEN getsongstring:=psonglist^[index]
                  ELSE getsongstring:='';
  END;
END;

{schauen ob name in der Playliste steht und gleiches Verzeichnis hat}
FUNCTION tmydirs.isinloadlist(Name:STRING; startindex:integer):integer;
VAR
    gefunden                 :boolean;
    str_ofs,psofs,psseg,Count,position:word;
    s                        :STRING[songlistentriesize];
BEGIN
  gefunden:=false;
  IF (psonglist<>NIL)AND(songcount>0)AND(startindex<=songcount) THEN
  BEGIN
    s:=Name;
    psseg:=Seg(psonglist^);
    psofs:=Ofs(psonglist^);
    str_ofs:=Ofs(s); Count:=songcount-startindex;
    psofs:=psofs+(startindex*(songlistentriesize+1));
    asm
          mov ax,psseg
          mov es,ax
          mov di,psofs
          mov si,str_ofs
          mov dx,Count
          mov bx,startindex

@start:   cmp gefunden,0
          jnz @hurra
          cmp dx,0
          jz @hurra

@Check:   mov cl,byte Ptr es:[di+1]
          cmp cl,byte Ptr ds:[si+1]
          jnz @weiter
          mov cx,word Ptr es:[di+3]
          cmp cx,word Ptr ds:[si+3]
          jnz @weiter
          mov cx,word Ptr es:[di+5]
          cmp cx,word Ptr ds:[si+5]
          jnz @weiter
          mov cx,word Ptr es:[di+7]
          cmp cx,word Ptr ds:[si+7]
          jnz @weiter
          mov cx,word Ptr es:[di+9]
          cmp cx,word Ptr ds:[si+9]
          jnz @weiter
          mov cx,word Ptr es:[di+11]
          cmp cx,word Ptr ds:[si+11]
          jnz @weiter
          mov cl,byte Ptr es:[di+13]
          cmp cl,byte Ptr ds:[si+13]
          jnz @weiter
          mov gefunden,1
          mov position,bx
@weiter:  dec dx
          add di,14
          inc bx
          jmp @start
@hurra:
    END;
  END;
  IF gefunden THEN isinloadlist:=position ELSE isinloadlist:=-1;
END;


{Spielattribut setzen}
PROCEDURE tmydirs.setattribut(index:word; what:char);
BEGIN
  IF (psonglist<>NIL) AND (index<Limit) THEN psonglist^[index][2]:=what;
END;

{NAchsehen ob noch nicht/schon gespielt,oder ob aktuelles Lied}
FUNCTION tmydirs.getattribut(index:word):char;
BEGIN
  IF (psonglist<>NIL)AND(SongCount>0)AND(index<Limit)
  THEN getattribut:=psonglist^[index][2]
  ELSE getattribut:='_';
END;

{String in SONGLISTE einfügen}
FUNCTION tmydirs.addsongstring(codename:STRING):WordBool;
VAR i     :byte;
    test  :integer;
BEGIN
 IF (songcount<Limit) THEN
 BEGIN
   psonglist^[songcount]:=codename;
   INC(SongCount,1);
   AddSongString:=True;
 END ELSE Addsongstring:=False;
END;

{String aus SONGLISTE entfernen}
PROCEDURE tmydirs.deletesongstring(index:word);
BEGIN
  IF songcount>0 THEN
  BEGIN
    Move(psonglist^[index+1],psonglist^[index],(songlistentriesize+1)*(Limit-Index));
    dec(songcount,1);
  END;
END;


FUNCTION tmydirs.getname(index:word):STRING;
VAR s:STRING;
BEGIN
  IF (Index>Limit)OR(SongCount=0) THEN BEGIN getname:=''; Exit; END;
  s:=Copy(psonglist^[index],6,8)+'.'+filetypesasstring[byte(psonglist^[index][1])];
  WHILE Pos(' ',s)<>0 DO system.Delete(s,Pos(' ',s),1);
  IF psonglist^[index][4]<>noarcconst THEN s:=getresstring(id_packed)+s;
  s[Length(s)+1]:=#0;
  getname:=s;
END;

FUNCTION tmydirs.gettemppath;
VAR deststring:STRING;
    f:FILE;
BEGIN
  GetTempFileName(#0,'',0,@deststring[1]);
  deststring[0]:=char(strlen(@deststring[1]));
  Assign(f,deststring);           {löschen des angelegten temp-files}
  Erase(f);
  WHILE deststring[Length(deststring)]<>'\' DO dec(byte(deststring[0]));
  gettemppath:=deststring;
END;

{gibt vollen Pfadnamen zurück}
{donothing heißt Datei soll nicht als gespielt abgehakt werden}
FUNCTION tmydirs.getfullname;
VAR s,realarcstr,filetofind          :STRING[100];
    temp                  :ARRAY[0..255] OF char;
    entpackstring         :STRING;
    deststring            :STRING;
    i                     :word;
    arcmethodindex        :byte;
    ts                    :tsearchrec;
    box                   :pwaitbox;
BEGIN
  getfullname:='';
  IF (psonglist<>NIL)AND(SongCount>0) THEN
  BEGIN
    {wenn gültiger PLAYLIST-INDEX dann}
    IF (Index<Limit) THEN
    BEGIN
      {Wenn PLAY dann Song markieren}
      IF NOT donothing THEN
      BEGIN
        FOR i:=0 TO songcount-1 DO IF getattribut(i)=isplay THEN setattribut(i,wasplay);
        setattribut(index,isplay);
      END;
      {Wenn gepacktes File}
      IF psonglist^[index][4]<>noarcconst THEN
      BEGIN
        filetofind:=Copy(psonglist^[index],6,8)+'.'+filetypesasstring[byte(psonglist^[index][1])];
        WHILE Pos(' ',filetofind)<>0 DO system.Delete(filetofind,Pos(' ',filetofind),1);
        deststring:=gettemppath;
        s:=deststring+filetofind;
        strpcopy(temp,s);
        IF (psonglist^[index][4]<>noarcconst)AND
           ((s<>lastunpackedsong)OR(index<>lastupindex))THEN newlist:=true;
        IF newlist THEN
        BEGIN
          grillunpackedsong;
          doserror:=18;
          newlist:=false;
        END ELSE FindFirst(temp,fafiles,ts);
        {Nachsehen ob File schon da ist}
        lastunpackedsong:=s;
        lastupindex:=index;
        IF doserror<>0 THEN
        BEGIN
           entpackstring:=getarcstring(byte(psonglist^[index][4]));
           arcmethodindex:=byte(entpackstring[2]);
           realarcstr:=arcunpackexecs[arcmethodindex];
           IF NOT findexe(realarcstr) THEN
           BEGIN
             errmessagebox(aparent,id_archivernotfound);
             getfullname:=getresstring(id_archivernotfound);
             Exit;
           END;
           {wenn gewünscht melde Entpacken AN!!!!}
           IF parentmessage THEN
           BEGIN
             s:=getresstring(id_unpacking)+filetofind;
             SendMessage(Application^.MainWindow^.HWindow,wm_unpacking,0,longint(@s));
           END;
           {formatiere Entpackstring}
           entpackstring:=getdirstring(byte(entpackstring[1]))+'\'+Copy(entpackstring,3,byte(entpackstring[0]));
           IF getarcdirstring(byte(psonglist^[index][5]))='' THEN s:=filetofind ELSE
           s:=getarcdirstring(byte(psonglist^[index][5]))+'\'+filetofind;
           {fertiger String für das Entpackprogramm}
           IF arcmethodindex=zipfiletype THEN
           BEGIN
             WHILE Pos('\',s)>0 DO s[Pos('\',s)]:='/';
             WHILE Pos('\',entpackstring)>0 DO entpackstring[Pos('\',entpackstring)]:='/';
           END;
           entpackstring:=realarcstr+' '+arcextractstring[arcmethodindex]+' '+entpackstring+' '+s;
           {Entpacken!}
           IF unpack(entpackstring,deststring) THEN s:=strpas(temp);
         END;
      END ELSE
      BEGIN
        s:=getdirstring(byte(psonglist^[index][3]));
        IF s='' THEN BEGIN getfullname:=getresstring(id_nixfile); Exit; END;
        s:=s+'\'+Copy(psonglist^[index],6,8)+'.'+filetypesasstring[byte(psonglist^[index][1])];
        WHILE Pos(' ',s)<>0 DO system.Delete(s,Pos(' ',s),1);
      END;
     strpcopy(temp,s);
     {schauen ob File noch vorhanden}
     FindFirst(temp,fafiles,ts);
     IF doserror<>0 THEN s:=getresstring(id_nixfile);
     getfullname:=s;
    END;
  END;
END;

{Test ob mit findfirst gefundener Name Directory oder Datei ist}
{wenn Datei dann Test auf gültige Endungen}
FUNCTION tmydirs.IsValid(dir:boolean; t:tsearchrec; VAR filetype:byte):boolean;
VAR s:STRING[20]; i:integer;
BEGIN
  IsValid:=false;
  {wenn auf gültiges Directory geprüft werden soll}
  IF dir THEN
  BEGIN
   IF strpas(t.Name)<>'.' THEN
   IF (t.Attr AND faDirectory)=faDirectory  THEN IsValid:=true;
  END ELSE
  {sonst Endung abschneiden und mit gültigen Endungen vergleichen}
  IF ((t.Attr AND faDirectory)<>faDirectory) THEN
  BEGIN
   s:=strpas(AnsiLower(t.Name));
   i:=Pos('.',s);
   IF i<>0 THEN
   BEGIN
    s:=Copy(s,i+1,3);
    FOR i:=3 TO allfiletypes DO IF s=filetypesasstring[i] THEN
    BEGIN
      IF filetypeenabled[i] THEN
      BEGIN
        IsValid:=true;
        filetype:=i;
      END;
    END;
   END;
  END;
END;
{Test ob mit findfirst gefundener Name gueltiges Archiv ist}
FUNCTION tmydirs.isvalidarchive(t:tsearchrec; VAR filetype:byte):boolean;
VAR s:STRING[20]; i:integer; ftype:byte;
BEGIN
  isvalidarchive:=false;
  ftype:=0;
  {Endung abschneiden und mit gültigen Endungen vergleichen}
  IF ((t.Attr AND faDirectory)<>faDirectory) THEN
  BEGIN
   s:=strpas(AnsiLower(t.Name));
   i:=Pos('.',s);
   IF i<>0 THEN
   BEGIN
     s:=Copy(s,i+1,3);
     FOR i:=1 TO allarctypes DO IF s=filetypesasstring[i] THEN ftype:=i;
     IF ftype=0 THEN
     BEGIN
       IF (s<>'669')AND(s[1]IN['a','0'..'9'])AND(s[2]IN['0'..'9'])AND(s[3]IN['0'..'9'])
       THEN ftype:=arjfiletype
       ELSE IF s='lha' THEN ftype:=lharcfiletype;
     END;
   END;
   IF (ftype>0)AND(NOT filetypeenabled[ftype]) THEN ftype:=0;
  END;
  IF ftype>0 THEN BEGIN filetype:=ftype; isvalidarchive:=true; END;
END;
{Der Folgende Kram startet eine Task, verbirgt sie, wartet, bis sie sich beendet
 und kehrt dann zurück                                                          }

CONST Nummsg=4;
VAR
   Unpackmsg      :TMsg;   {für PeekMessage...}
FUNCTION tmydirs.unpack(execstring,temppath:STRING):boolean;
VAR exechandle,fattr :word;
    f                :text;
    dosenv           :pchar;
    cmds             :STRING;
    oldcursor:hcursor;
    tl:longint;
   {PIF-VArs}
    filename:ARRAY[0..150] OF char;
    PIF_findhandle,PIF_loadhandle:thandle;
    PIFpointer :pointer;
    PIFFILE:FILE;
BEGIN
  IF entpacke THEN Exit ELSE
  BEGIN
    entpacke:=true;
    oldcursor:=SetCursor(LoadCursor(0,idc_wait));
    {Batch datei kreieren}
    inoutres:=0;
    IF (temppath[Length(temppath)]='\')AND(Length(temppath)>3) THEN dec(byte(temppath[0])); ChDir(temppath);
    IF temppath[Length(temppath)]<>'\' THEN temppath:=temppath+'\';
    Assign(f,temppath+'########.BAT');
    filemode:=1;
    Rewrite(f);
    IF IOResult<>0 THEN BEGIN entpacke:=false; Exit; END;
    WriteLn(f,execstring);
    IF showpack THEN  WriteLn(f,getresstring(2013));
    WriteLn(f,'@DEL '+temppath+'M4WUP.PIF ');
{    writeln(f,'@DEL '+temppath+'########.BAT ');}
    Close(f);
{    cmds:=cmds+' /c '+temppath+'########.BAT';}
    unpack:=false;
    {New with PIF}
      PIF_findhandle:=FindResource(hinstance,makeintresource(31),'PIF');
      PIF_loadhandle:=LoadResource(hinstance,PIF_findhandle);
      PIFpointer:=LockResource(PIF_loadhandle);
      WITH ppifrec(PIFpointer)^ DO strpcopy(execname,temppath+'########.BAT');
      strpcopy(filename,temppath+'M4WUP.PIF');
      {$I-}
      filemode:=2; inoutres:=0;
      Assign(PIFfile,filename);
      Rewrite(PIFfile,1);
      BlockWrite(PIFfile,PIFpointer^,545);
      Close(PIFfile);
      unlockresource(PIF_loadhandle);
      FreeResource(PIF_loadhandle);
    {ENDE PIF-MAKING}
    IF showpack THEN exechandle:=WinExec(filename,sw_show)
                ELSE exechandle:=WinExec(filename,0);
    IF exechandle>32 THEN
    BEGIN
      doserror:=0;
      WHILE doserror=0 DO
      BEGIN
        {ein wenig warten}
        IF PeekMessage(Unpackmsg,0,0,0,pm_Remove) THEN
        BEGIN
          TranslateMessage(Unpackmsg);
          DispatchMessage(Unpackmsg);
        END;
        {$i-}
        {IST PIF noch da?}
        GetFAttr(PIFFile,fattr);
      END;
      Erase(f);
      {XXXUP-Messages doch noch verschicken}
      unpack:=true;
    END;
    SetCursor(oldcursor);
    entpacke:=false;
  END;
END;

FUNCTION tmydirs.copyfile;
VAR f1,f2:FILE;
    olderrormode:word;
    s,temp,s3:STRING[100];
    s2:STRING;
    i:integer;
    plist:pmydirs;

  FUNCTION findormakesubdirs(askfor_path_create:boolean):boolean;
  VAR Create:boolean;
      subdir,tempstr:STRING[80];
  BEGIN
    findormakesubdirs:=true;
    IF fullname[3]<>'\' THEN Exit;
    tempstr:=fullname;
    WHILE tempstr[Length(tempstr)]<>'\' DO dec(byte(tempstr[0])); dec(byte(tempstr[0]));
    s3:=tempstr;
    Create:=false;
    GetDir(byte(UpCase(tempstr[1]))-64,subdir);
    ChDir(tempstr[1]+':\');
    WHILE tempstr<>'' DO
    BEGIN
      {$i-}
      i:=Pos('\',tempstr); IF i=0 THEN i:=Length(tempstr)+1;
      temp:=Copy(tempstr,1,i-1);
      ChDir(temp);
      IF IOResult<>0 THEN
      BEGIN
        IF NOT Create THEN
        BEGIN
          IF (NOT askfor_path_create)
          THEN Create:=true
          ELSE IF ourmessagebox(box,modc.shortver,getresstring(id_askcreatedir),mb_yesno OR
                                mb_iconinformation)=id_no
          THEN BEGIN findormakesubdirs:=false; Exit; END ELSE Create:=true;
        END;
        MkDir(temp);
        ChDir(temp);
      END;
      Delete(tempstr,1,i);
    END;
    ChDir(subdir);
  END;

  FUNCTION directcopy(askfor_path_create:boolean):boolean;
  VAR arr:pointer;
      fsiz, FTime:longint;
      test:word;
      cumf1:longint;
  BEGIN
     directcopy:=false;
     FOR test:=1 TO Length(sourcename) DO sourcename[test]:=UpCase(sourcename[test]);
     FOR test:=1 TO Length(fullname) DO fullname[test]:=UpCase(fullname[test]);
     IF fullname=sourcename THEN BEGIN directcopy:=true; Exit; END;
     Assign(f1,sourcename); Assign(f2,fullname);
     filemode:=0;
     {$I-}
     REPEAT
       inoutres:=0; Reset(f1,1);
     UNTIL (inoutres=0)OR
      (ourmessagebox(box,modc.shortver+getresstring(filecopy_id),getresstring(fileopenerror_id),mb_retrycancel)=id_cancel);
     IF IOResult<>0 THEN Exit;
     fsiz:=FileSize(f1);
     IF NOT findormakesubdirs(askfor_path_create) THEN BEGIN Close(f1);  Exit; END;
     Reset(f2,1); Close(f2);
     IF IOResult=0 THEN
     IF ourmessagebox(box,modc.shortver+getresstring(filecopy_id),getresstring(askoverwrite_id),
        mb_iconstop OR mb_yesno)<>id_yes THEN BEGIN Close(f1);  Exit; END;
     inoutres:=0; filemode:=2;
     Rewrite(f2,1);
     IF IOResult<>0 THEN
     BEGIN
       ourmessagebox(box,modc.shortver+getresstring(filecopy_id),getresstring(writeerroropen_id),mb_iconstop);
       Close(f1);
       Exit;
     END;
     IF DiskFree(Ord(UpCase(fullname[1]))-64)<fsiz THEN
     BEGIN
       ourmessagebox(box,modc.shortver+getresstring(filecopy_id),getresstring(nomem_id),mb_iconstop);
       Close(f1);
       Erase(f2);
       Exit;
     END;
     cumf1:=0;
     arr:=globalallocptr(gptr,8192);
     REPEAT
       WHILE PeekMessage(Unpackmsg,0,0,0,pm_remove) DO
       BEGIN
         TranslateMessage(Unpackmsg);
         DispatchMessage(Unpackmsg);
       END;
       REPEAT
         Seek(f1,cumf1);
         inoutres:=0;
         BlockRead(f1,arr^,8192,test);
       UNTIL (inoutres=0)OR
       (ourmessagebox(box,modc.shortver+getresstring(filecopy_id),getresstring(readerror_id),mb_retrycancel)=id_cancel);
       IF inoutres<>0 THEN
       BEGIN
         Close(f1);
         Close(f2);
         Erase(f2);
         globalfreeptr(arr);
         Exit;
       END ELSE cumf1:=cumf1+test;
       WHILE PeekMessage(Unpackmsg,0,0,0,pm_remove) DO
       BEGIN
         TranslateMessage(Unpackmsg);
         DispatchMessage(Unpackmsg);
       END;
       inoutres:=0;
       BlockWrite(f2,arr^,test);
       IF IOResult<>0 THEN
       IF (ourmessagebox(box,modc.shortver+getresstring(filecopy_id),getresstring(writeerror_id),
           mb_retrycancel)=id_cancel)  THEN
           BEGIN
             Close(f1);
             Close(f2);
             Erase(f2);
             globalfreeptr(arr);
             Exit;
           END ELSE BEGIN cumf1:=0; Seek(f2,0); END;
       IF fsiz>0 THEN setwaitbox(box,FileSize(f2) *100 / fsiz );
     UNTIL test<8192;
     GetFTime (f1, FTime); SetFTime (f2, FTime);
     Close(f1); Close(f2);
     globalfreeptr(arr);
     directcopy:=true;
  END;

BEGIN
  copyfile:=false;
  olderrormode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  IF (scarcdir='')AND(dsarcdir='') THEN copyfile:=directcopy(true)
  ELSE
  IF dsarctype<>listfiletype THEN
  BEGIN
    IF (scarcdir<>'') THEN sourcename:=getfullname(box,listindex,true,false);
    IF sourcename=getresstring(id_archivernotfound) THEN Exit;
    IF dsarcdir='' THEN copyfile:=directcopy(true)
    ELSE
    BEGIN
      {$I-}
      gl_str:=arcpackexecs[dsarctype];
      IF NOT findexe(gl_str) THEN errmessagebox(box,id_archivernotfound)
      ELSE
      BEGIN
        {$i-}
        {tempdir holen}
        s:=gettemppath; ChDir(s); dec(byte(s[0]));
        ChDir(s);
        {create subdirs}
        s2:=dsarcdir;
        WHILE s2[Length(s2)]<>'\' DO dec(byte(s2[0])); dec(byte(s2[0]));
        IF s2<>'' THEN Delete(s2,1,1); s3:=s2;
        s2:=fullname;
        fullname:=s+dsarcdir;
        {nicht nach PFAD - ERstellung fragen sondern einfach tun}
        IF directcopy(false) THEN
        BEGIN
          {Packe Temp-Datei in Archiv}
          fullname:=s2;
          IF s[Length(s)]<>'\' THEN s:=s+'\'; {TEMPORÄRER PFAD}
          {IF dsarctype=lharcfiletype THEN} Delete(dsarcdir,1,1);
          s2:=gl_str+' '+arccopystring[dsarctype]+' '+fullname+' '+dsarcdir;
          copyfile:=unpack(s2,s);
        END;
        {TEMP-Datei + Pfad löschen!}
        Assign(f1,s+dsarcdir); Erase(f1);
        ChDir(s3);
        WHILE s3<>'' DO
        BEGIN
          ChDir('..'); temp:='';
          WHILE (s3[Length(s3)]<>'\')AND(Length(s3)>0) DO
          BEGIN
           Insert(s3[Length(s3)],temp,1);
           dec(byte(s3[0]));
          END;
          IF (Length(s3)>0) THEN dec(byte(s3[0]));
          RmDir(temp);
        END;
      END;
    END;
  END ELSE
  BEGIN
    {COPY in Liste}
    plist:=New(pmydirs,Init);
    setwaitbox(box,2500 / 100 );
    IF plist^.sauglist(box,fullname) THEN
    BEGIN
      setwaitbox(box,5000 / 100 );
      s:=getsongstring(listindex);
      IF checkfrom2to1_entrie(plist,s)<0
      THEN
      BEGIN
         from2to1_entrie(plist,s);
         plist^.addsongstring(s);
         setwaitbox(box,90 / 100 );
         IF plist^.savelist(box,fullname,false)THEN copyfile:=true;
         setwaitbox(box,10000 / 100 );
      END ELSE ourmessagebox(box,modc.shortver,getresstring(fileinlist_id),mb_iconinformation);
    END;
    Dispose(plist,Done);
  END;
  SetErrorMode(olderrormode);
END;


{Löscht eine Datei physisch von Platte und löscht Eintrag in der Dirliste}
FUNCTION  tmydirs.erasefile;
VAR f:FILE;
    olderrormode:word;
    dir,Arc,arcdir:STRING[100];
    ss,Name:STRING[20];
    s:STRING;
    roger:boolean;
BEGIN
  olderrormode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  roger:=false;
  ss:=getsongstring(index);
  dir:=getdirstring(byte(ss[3]));
  Arc:=getarcstring(byte(ss[4]));
  arcdir:=getarcdirstring(byte(ss[5]));
  IF arcdir<>'' THEN arcdir:=arcdir+'\';
  Name:=Copy(ss,6,8)+'.'+filetypesasstring[byte(ss[1])];
  WHILE Pos(' ',Name)<>0 DO system.Delete(Name,Pos(' ',Name),1);
  IF ss[4]=noarcconst THEN
      s:=getresstring(askerase1_id)+dir+'\'+Name+getresstring(askerase2_id) ELSE
      s:=getresstring(askerase1_id)+arcdir+Name+getresstring(fromarc_id)+
         dir+'\'+Copy(Arc,3,Length(Arc))+getresstring(askerase2_id);
  IF (NOT ask) OR (ourmessagebox(aparent,modc.shortver+getresstring(erasefile_id),s,mb_yesno OR mb_iconquestion)=id_yes)
  THEN
  BEGIN
    IF ss[4]=noarcconst THEN
    BEGIN
      inoutres:=0;
      Assign(f,dir+'\'+Name); Erase(f);
      IF IOResult=0 THEN roger:=true;
    END ELSE
    BEGIN
      s:=arcpackexecs[byte(Arc[2])];
      IF NOT findexe(s) THEN errmessagebox(aparent,id_archivernotfound)
      ELSE
      IF unpack(s+' '+arcdelstring[byte(Arc[2])]+
                ' '+dir+'\'+Copy(Arc,3,Length(Arc))+' '+arcdir+Name,'')
      THEN BEGIN roger:=true; grillunpackedsong; END;
    END;
    IF NOT roger
    THEN ourmessagebox(aparent,modc.shortver+getresstring(erasefile_id),getresstring(eraseerror_id),mb_iconstop);
  END;
  SetErrorMode(olderrormode);
  IF roger THEN deletesongstring(index);
  erasefile:=roger;
END;

PROCEDURE  tmydirs.CompactList;
VAR i,cdst:integer;
    dst:PBuffer;
    newdirs:pdirs;
BEGIN
  IF (dircount=0)OR(songcount=0) THEN Exit;
  GetMem(dst,(maxdirlistentries+1)*2); IF dst=NIL THEN Exit;
  GetMem(newdirs,SizeOf(TDirs));
  IF newdirs=NIL THEN
  BEGIN
    FreeMem(dst,(maxdirlistentries+1)*2);
    Exit;
  END;
  FillChar(newdirs^,SizeOf(TDirs),0);

  {Mache Tabelle}
  cdst:=1;
  FOR i:=1 TO maxdirlistentries DO
  BEGIN
    IF dirs[i][0]<>#0 THEN
    BEGIN
      newdirs^[cdst]:=dirs[i];
      dst^[i]:=cdst;
      inc(cdst);
    END;
  END;

  Move(newdirs^,dirs,SizeOf(tdirs));
  FOR i:=0 TO songcount-1 DO
  BEGIN
    psonglist^[i][3]:=char(byte( dst^[byte(psonglist^[i][3])]));
  END;
  IF arccount>0 THEN
  FOR i:=1 TO maxarclistentries DO IF arcs[i][0]<>#0 THEN
  BEGIN
    arcs[i][1]:=char(byte( dst^[byte(arcs[i][1])]));
  END;
  FreeMem(newdirs,SizeOf(TDirs));
  FreeMem(dst,(maxdirlistentries+1)*2);
END;

FUNCTION tmydirs.savelist;
VAR olderrormode:word;
    f:text;
    s,s2 :STRING[81];
    i,j  :integer;
    _is  :boolean;
BEGIN
  savelist:=false;
  olderrormode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  Assign(f,Name); filemode:=2; inoutres:=0;
  {$i-}
  Reset(f);
  IF inoutres=0 THEN
  IF (ask=false)OR(ourmessagebox(Aparent,'FileSaveError',getresstring(id_existfile),
                   mb_iconexclamation OR mb_yesno)=idyes) THEN inoutres:=2;
  IF IOResult<>0 THEN
  BEGIN
    REPEAT
      inoutres:=0;
      Rewrite(f);
    UNTIL (inoutres=0)OR(ourmessagebox(aparent,modc.shortver+' Save List',
                         getresstring(id_NoWriteFile),mb_retrycancel)=id_cancel);
    IF IOResult=0 THEN
    BEGIN
     FOR j:=1 TO maxarclistentries DO
     BEGIN
      _is:=isarcindexinsonglist(j);
      IF NOT _is THEN
      BEGIN
        arcs[j]:=arcs[j+1];
        arcs[j+1][0]:=#0;
        FOR i:=0 TO songcount-1 DO
        BEGIN
          IF psonglist^[i][4]=char(j+1)THEN psonglist^[i][4]:=char(j);
        END;
      END;
     END;
     FOR j:=2 TO maxarcdirentries-1 DO
     BEGIN
      _is:=isarcdirindexinsonglist(j);
      IF NOT _is THEN
      BEGIN
        IF arcdirs[j]<>'' THEN IF arcdircount>0 THEN dec(arcdircount);
        arcdirs[j]:=arcdirs[j+1];
        arcdirs[j+1][0]:=#0;
        FOR i:=0 TO songcount-1 DO
        BEGIN
          IF psonglist^[i][5]=char(j+1)THEN psonglist^[i][5]:=char(j);
        END;
      END;
    END;
    WriteLn(f,'[DIRECTORIES]');
    FOR i:=1 TO dircount DO
    BEGIN
      s:=char(i+indexoffset)+getdirstring(i);
      WriteLn(f,s);
    END;
    WriteLn(f,'[ARCHIVES]');
    IF arccount>0 THEN FOR i:=1 TO arccount DO
    BEGIN
      s:=getarcstring(i);
      s[1]:=char(byte(s[1])+indexoffset); s[2]:=char(byte(s[2])+indexoffset);
      WriteLn(f,s);
    END;
    WriteLn(f,'[ARCHIVDIRECTORIES]');
    IF arcdircount>0 THEN FOR i:=2 TO arcdircount+1 DO
    BEGIN
      s:=char(i+indexoffset)+getarcdirstring(i);
      WriteLn(f,s);
    END;
    WriteLn(f,'[FILES]');
    j:=songcount-1;
    FOR i:=0 TO j DO
    BEGIN
      s:=getsongstring(i); Delete(s,2,1);
      s[1]:=char(byte(s[1])+indexoffset); s[2]:=char(byte(s[2])+indexoffset);
      s[3]:=char(byte(s[3])+indexoffset); s[4]:=char(byte(s[4])+indexoffset);
      WriteLn(f,s);
    END;
    Close(f);
    savelist:=true;
    ourmessagebox(Aparent,modc.shortver,getresstring(listsaved_id)+#10+'('+Name+')',mb_iconinformation);
  END;
 END;
 SetErrorMode(olderrormode);
END;

{**********SELEdit-Box die Listbox-Einträge Selektiert*****************}
CONSTRUCTOR tseledit.Init;
BEGIN
  INHERITED Init(aparent,anid,atitle,x,y,w,h,atextlen,ownfont);
  selchar:=firstchar;
  alist:=alistbox;
  tofs:=searchoffset;
END;
PROCEDURE tseledit.SetupWindow;
BEGIN
 INHERITED SetupWindow;
 PostMessage(HWindow,wm_char,word(selchar),0);
END;

FUNCTION tseledit.dosel;
VAR i,j,oldcaret:integer;
    Count:integer;
    s,s2:STRING[30];
    changed:boolean;
BEGIN
  Count:=alist^.GetCount;
  i:=0;
  oldcaret:=alist^.getcaretindex;
  changed:=false;
  s[0]:=char(byte(GetText(@s[1],30)));
  s:=s+zeichen;
  WHILE (i<Count)AND(NOT changed) DO
  BEGIN
    alist^.GetString(s2,i);
    { remove play status byte when in play list }
    IF tofs = 4 THEN Delete (s2, 2, 1);
    s2:=s2+'.'+filetypesasstring[byte(s2[1])];
    WHILE Pos(' ',s2)>0 DO Delete(s2,Pos(' ',s2),1);
    changed:=true;
    FOR j:=1 TO Length(s) DO IF UpCase(s[j])<>UpCase(s2[j+tofs]) THEN changed:=false;
    inc(i);
  END;
  IF changed AND(oldcaret<>(i-1)) THEN
  BEGIN
    SendMessage(alist^.HWindow,wm_setredraw,0,0);
    alist^.setcaretindex(i-1);
    i:=(i-1)-((alist^.Attr.h DIV Fh)DIV 2); IF i<0 THEN i:=0;
    SendMessage(alist^.HWindow,lb_settopindex,i,0);
    InvalidateRect(alist^.HWindow,NIL,true);
    SendMessage(alist^.HWindow,wm_setredraw,1,0);
  END;
  dosel:=changed;
END;
PROCEDURE tseledit.wmchar;
BEGIN
  CASE char(msg.wparam) OF
  #32..#255:IF dosel(char(msg.wparam)) THEN  INHERITED wmchar(msg);
  ELSE INHERITED wmchar(msg);
  END;
  msg.result:=0;
END;

PROCEDURE tseledit.wmkeydown;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    vk_tab    :  SetFocus(GetNextDlgTabItem(Parent^.HWindow,HWindow,keystate=2));
    vk_escape,vk_return,vk_execute: SetFocus(lastfocus);
    vk_f1,vk_help:IF keystate=0 THEN
     BEGIN
       msg.result:=SendMessage(Parent^.HWindow,wm_keydown,msg.wparam,Attr.id);
       SetFocus(lastfocus);
     END;
     ELSE DefWndProc(msg);
   END;
END;

PROCEDURE tseledit.wmsyskeydown;
BEGIN
  IF msg.wparam=vk_f4
  THEN PostMessage(Parent^.HWindow,wm_syskeydown,msg.wparam,msg.lparam)
  ELSE SetFocus(lastfocus);
  msg.result:=0;
END;

PROCEDURE tseledit.wmsetfocus;
BEGIN
  lastfocus:=msg.wparam;
  INHERITED wmsetfocus(msg);
END;
PROCEDURE tseledit.wmkillfocus;
BEGIN
  SetFocus(lastfocus);
  PostMessage(Parent^.HWindow,wm_command,Attr.id,longint(0+en_killfocus SHL 16));
  PostMessage(HWindow,wm_close,0,0);
  msg.result:=0;
END;

{***********************BROWSER***************************************************}
PROCEDURE tbrowse.updatedirlist;
VAR i,j,k,olderrormode           :word;
    f                            :tsearchrec;
    pc                           :ARRAY[0..255] OF char;
    s,s1,spath,probe             :STRING[100];
    box                          :pwaitbox;
    maxfiles,aktfiles            :word;
    fl:FILE;
    prevseldir                   :STRING[15];
    exe_found                    :boolean;
BEGIN
 olderrormode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
 exe_found:=false;
 spath:=path;
 prevseldir:='';
 BEGIN
 IF pdirlist^.GetSelString(s,SizeOf(s))>0 THEN
   CASE byte(s[1]) OF
   byte(dlfloppytype)..byte(dlremotetype):
    BEGIN
      GetDir(byte(s[4])-96,path);
      path:=path+#0;
      path:=strpas(AnsiLower(pchar(@path[1])));
      IF path[Length(path)]='\' THEN dec(path[0]);
    END;
   byte(dldirtype):
     BEGIN
       Delete(s,1,1);
       IF s='<..>' THEN
       BEGIN
         prevseldir:=dldirtype+'<'+prevdir(path)+'>';
         s1:=''; s:=path;
         WHILE Pos('\',s)<>0 DO
         BEGIN
           s1:=s1+Copy(s,1,Pos('\',s));
           Delete(s,1,Pos('\',s));
         END;
         Delete(s1,Length(s1),1);
         path:=s1;
       END ELSE path:=path+'\'+Copy(s,2,Length(s)-2);
     END;
  END;
  strpcopy(@pc,path+'\*.*');
  FindFirst(@pc,fafiles,f);
  IF (doserror=0)AND(GetDriveType(word(UpCase(path[1]))-Ord('A'))=DRIVE_REMOTE) THEN
  BEGIN
    Assign(fl,path+'\'+f.Name);
    GetFAttr(fl,i);
    IF i=0 THEN doserror:=1;
  END;
  IF (doserror>0)AND(doserror<>18) THEN
  BEGIN
    doserror:=0; Path:=Path[1]+':'; strpcopy(@pc,path+'\*.*');
    FindFirst(@pc,fafiles,f);
    IF (doserror=0)AND(GetDriveType(word(UpCase(path[1]))-Ord('A'))=DRIVE_REMOTE) THEN
    BEGIN Assign(fl,path+'\'+f.Name); GetFAttr(fl,i); IF i=0 THEN doserror:=1; END;
  END;
  IF (doserror>0)AND(doserror<>18) THEN
  BEGIN
    ourmessagebox(@self,errorver,getresstring(id_driveerror),mb_ok OR mb_iconstop);
    path:=spath; SetErrorMode(olderrormode); Exit;
  END ELSE
  BEGIN
    box:=initwaitbox(@self,getresstring(id_loadcheckfiles));
    {Lese Anzahl Files}
    maxfiles:=0;
    aktfiles:=0;
    WHILE doserror=0 DO
    BEGIN
      inc(maxfiles);
      FindNext(f);
    END;
    {Löschen der File und Dir Listbox}
    pdirlist^.ClearList;
    SendMessage(pfilelist^.HWindow,wm_setredraw,0,0);
    pfilelist^.ClearList;
    strpcopy(@pc,path+'\*.*');
    {Suche nach allem ,was im Verz. steht ;sortieren nach Datei und Verzeichnis}
    FindFirst(@pc,fafiles,f);
    WHILE doserror=0  DO
    BEGIN
      strcopy(f.Name,AnsiLower(f.Name));
      IF (f.Attr AND faDirectory)=faDirectory THEN
      BEGIN
        s:=strpas(f.Name);
        IF s<>'.' THEN
        BEGIN
          s:=dldirtype+'<'+s+'>';
          pdirlist^.AddString(s);
        END;
      END
      ELSE IF (f.Attr AND faVolumeID)<>faVolumeID THEN
      BEGIN
        s:=strpas(f.Name); s1:=Copy(s,(Pos('.',s))+1,3);
        IF (s1='exe')OR(s1='com')OR(s1='bat')OR(s1='pif') THEN
         BEGIN
           pfilelist^.AddString(s);
           IF (s=exe_name) THEN exe_found:=true;
         END;
      END;
      inc(aktfiles);
      setwaitbox(box,((100.0*aktfiles)/maxfiles));
      FindNext(f);
    END;
    IF exe_found THEN
    BEGIN
      s:=exe_name+#0;
      j:=pfilelist^.findstring(s);
      pfilelist^.setsel(j,_on);
    END ELSE exe_name:='';
    strpcopy(pc,path+'\'+exe_name);
    ppathedit^.SetText(AnsiUpper(@pc));
  END;
  FOR i:=0 TO 26 DO
  BEGIN
    j:=GetDriveType(i);
    IF (j<>0) THEN
    BEGIN
      s:='[-'+char(97+i)+'-]';
      IF j=drive_removable THEN Insert(dlfloppytype,s,1) ELSE
      IF j=drive_fixed THEN Insert(dlhdtype,s,1) ELSE Insert(dlremotetype,s,1);
      pdirlist^.AddString(s);
    END;
  END;
  SendMessage(pfilelist^.HWindow,wm_setredraw,1,0);
  closewaitbox(box);
 END;
 IF (pdirlist^.GetCount>0) THEN
 BEGIN
  IF prevseldir[0]=#0 THEN s:=dldirtype+'<..>'+#0
                      ELSE s:=prevseldir+#0;
  j:=pdirlist^.findstring(s);
  IF j>-1 THEN pdirlist^.SetSelIndex(j);
 END;
 SetErrorMode(olderrormode);
 pfilelist^.setcaretindex(0);
 setinfo(pdirlist^.HWindow);
END;


CONSTRUCTOR tbrowse.Init;
VAR i:integer;
BEGIN
   INHERITED Init (AParent, '',ws_sysmenu,notopmost);
   p_edit:=anedit;
   X_pathname  :=10+5*Fw+2;
   X_filelist  :=12;
   Y_pathname  :=10;
   Y_filelist  := y_pathname+3*Fh+5;
   Y_dirlist   := y_filelist;
   dy_filelist:=13*Fh;
   dx_dirlist :=18*Fw+10;
   dy_dirlist :=dy_filelist-30;
   y_browse:=3*Fh+dy_filelist+58;  {3* fuer path,filelist,und unter list static (+33 alt)}
   IF dy_dirlist<(y_browse-189) THEN dy_dirlist:=y_browse-189;
   dx_filelist:=14*Fw+10;

   x_browse:=40+dx_dirlist+dx_filelist;
   dx_pathname:=x_browse-(5*Fw)-25;
   X_dirlist  :=x_filelist+dx_filelist +14;
   X_bOquit    := x_dirlist+(dx_dirlist DIV 2)-21;
   Y_bOquit    := y_dirlist+dy_dirlist+10;
   X_bOok      := x_boquit-57;
   Y_bOok      := y_boquit;
   X_bOhelp    := x_boquit+57;
   Y_bOhelp    := y_book;
   Attr.style:=ws_popup OR ws_visible OR ws_border;
   Attr.x :=(GetSystemMetrics(sm_cxscreen)-x_browse)DIV 2;
   Attr.y :=(GetSystemMetrics(sm_cyscreen)-y_browse)DIV 2;
   Attr.w:=X_browse+GetSystemMetrics(sm_cxborder);
   Attr.h:=Y_browse+capdy+(2*GetSystemMetrics(sm_cyborder));

   ppathedit:=New(pmyedit,Init(@self, id_pathname,'',X_pathname,CapDY+Y_pathname,dx_pathname,Fh,150,true));
   pfilelist:=New(pmylistbox,Init(@self,id_filelist,x_filelist,CapDY+y_filelist,dx_filelist,dy_filelist,
   lbs_sort,false));
   pdirlist:=New(pmylistbox,Init(@self,id_dirlist,x_dirlist,CapDY+y_dirlist,dx_dirlist,dy_dirlist,
   lbs_sort ,false));
   comments[1]:=New(pmystatic,Init(@self,0,'',(x_pathname-5*Fw)-2,CapDY+Y_pathname,5*Fw,Fh,5));
   comments[2]:=New(pmystatic,Init(@self,0,'',x_filelist-2,CapDY+Y_filelist-Fh-4,9*Fw,Fh,9));
   comments[3]:=New(pmystatic,Init(@self,0,'',x_dirlist,CapDY+Y_dirlist-Fh-4,17*Fw,Fh,17));
   book  :=New(pbmpbutton,Init(@self, id_Ook,X_bOok,CapDY+Y_bOok,'bsok',false));
   bOquit:=New(pbmpbutton,Init(@self, id_Ocancel,X_bOquit,CapDY+Y_bOquit,'bscancel',false));
   bOhelp:=New(pbmpbutton,Init(@self, id_Ohelp,X_bOhelp,CapDY+Y_bOhelp,'bshelp',false));
   INFOstatic :=New(pmycenterstatic,Init(@self,id_infostatic,'',
                    x_filelist,CapDY+Y_filelist+dy_filelist+17,x_browse-25,Fh,80));
   drivebmphandle:=loadMybitmap(hinstance,'BMP_drives');
 END;


PROCEDURE tbrowse.SetupWindow;
VAR pc      :pchar; x:ARRAY[0..50] OF char;
    rec     :tsearchrec; fl:FILE;
    i,fattr:word;

BEGIN
  INHERITED SetupWindow;
  SendMessage(pfilelist^.HWindow,wm_setredraw,0,0);
  SendMessage(pdirlist^.HWindow,wm_setredraw,0,0);
  pc:=MemAlloc(255);
  strcopy(pc,modc.shortver); strpcopy(x,getresstring(2014)); strcat(pc,x); SetCaption(pc);
  IF p_edit^.GetText(pc,150)>0 THEN
  BEGIN
    path:=strpas(AnsiLower(pc));
    findexe(path);
    exe_name:=prevdir(path);
    path:=Copy(path,0,Length(path)-Length(exe_name));
  END ELSE BEGIN exe_name:=''; path:=''; END;
  IF path='' THEN path:='C:\';
  IF path[byte(path[0])]<>'\' THEN path:=path+'\';
  i:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  strpcopy(pc,path); strcat(pc,'*.*');
  {$i-}
  FindFirst(pc,fafiles,rec);
  IF (doserror=0)AND(GetDriveType(word(UpCase(path[1]))-Ord('A'))=DRIVE_REMOTE) THEN
  BEGIN
    Assign(fl,path+rec.Name);
    GetFAttr(fl,fattr);
    IF fattr=0 THEN doserror:=1;
  END;
  IF doserror IN[0,18] THEN ChDir(path) ELSE path:=m4wdir;
  IF path[byte(path[0])]='\' THEN dec(path[0]);
  SetErrorMode(i);
  FreeMem(pc,255);
  comments[1]^.SetText('&Path:');       comments[1]^.setassocwindow(Ppathedit^.HWindow,0);
  comments[2]^.SetText('&Filelist:');   comments[2]^.setassocwindow(Pfilelist^.HWindow,0);
  comments[3]^.SetText('&Directories:'); comments[3]^.setassocwindow(PDirlist^.HWindow,0);
  book^.enable; boquit^.enable; boHelp^.enable;
  updatedirlist;
  ppathedit^.SetSelection(0,ppathedit^.GetTextLen);
  oldfocus:=ppathedit^.HWindow;
  SendMessage(pfilelist^.HWindow,wm_setredraw,1,0);
  SendMessage(pdirlist^.HWindow,wm_setredraw,1,0);
  Parent^.disable;
END;

DESTRUCTOR tbrowse.Done;
BEGIN
  deleteMyBitmap(drivebmphandle);
  Parent^.enable;
  INHERITED Done;
END;

PROCEDURE tbrowse.wmEraseBkGnd(VAR msg:tmessage);
BEGIN
  paintcaption(msg.wParam);
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  framefilled3d(msg.wParam,0,CapDY,x_browse-1,y_browse,3,cForeGnd,up);
  frame3d(msg.wParam,X_pathname-2,CapDY+Y_pathname-2,dx_pathname+4,Fh+4,2,down);
  frame3d(msg.wParam,X_filelist-3,CapDY+Y_filelist-3,dx_filelist+6,dy_filelist+6,2,down);
  frame3d(msg.wParam,X_dirlist-3,CapDY+Y_dirlist-3,dx_dirlist+6,dy_dirlist+6,2,down);
  frame3d(msg.wParam,X_book-2,CapDY+Y_book-2,46,25,2,down);
  frame3d(msg.wParam,X_boquit-2,CapDY+Y_boquit-2,46,25,2,down);
  frame3d(msg.wParam,X_bohelp-2,CapDY+Y_bohelp-2,46,25,2,down);
  frame3d(msg.wParam,X_filelist-3,infostatic^.Attr.y-3,infostatic^.Attr.w+6,Fh+6,2,down);
  INHERITED wmEraseBkGnd(msg);
END;

PROCEDURE tbrowse.setinfo;
VAR s2:STRING;
BEGIN
  box_id:=GetDlgCtrlID(box_id);
  CASE box_id OF
   id_dirlist:
     BEGIN
       pdirlist^.GetString(s2,pdirlist^.getcaretindex);
       CASE byte(s2[1]) OF
        zipfiletype+dlarcoffset:  s2:=getresstring(zip_id)+getresstring(enter_id);
        arjfiletype+dlarcoffset:  s2:=getresstring(arj_id)+getresstring(enter_id);
        LHARCfiletype+dlarcoffset:s2:=getresstring(lha_id)+getresstring(enter_id);
        listfiletype+dlarcoffset :s2:=getresstring(mol_id)+getresstring(enter_id);
        byte(dldirtype)             :IF (s2[3]='.')AND(s2[4]='.')
                                     THEN s2:=getresstring(leavedir_id)
                                     ELSE s2:=getresstring(dir_id);
        byte(dlfloppytype)          :s2:=getresstring(floppy_id);
        byte(dlhdtype)              :s2:=getresstring(hd_id);
        byte(dlremotetype)          :s2:=getresstring(remote_id);
       END;
     END;
   id_filelist:
       IF pfilelist^.GetCount>0 THEN s2:=getresstring(2017)
       ELSE s2:=getresstring(novalidfiles_id);
   id_OOK,id_pathname : s2:=getresstring(2015);
   id_OCancel         : s2:=getresstring(2016);
   id_Ohelp           : s2:=getresstring(help_id);
  END;
  infostatic^.setstringtext(s2);
END;

PROCEDURE tbrowse.handleHilfe;
BEGIN
  helpme(@self,id_browsedlg);
END;

PROCEDURE tbrowse.handleOk;
VAR  s:STRING;
BEGIN
  path[0]:=char(byte(ppathedit^.GetText(@path[1],150)));
  s:=prevdir(path);
  IF Length(s)>12 THEN
  BEGIN
    ourmessagebox(@self,modc.shortver,getresstring(id_badinput),mb_iconstop);
    ppathedit^.focus;
    Exit;
  END ELSE
  BEGIN
    p_edit^.SetText(@path[1]);
    PostMessage(Parent^.HWindow,wm_getloadings,0,0);
    PostMessage(HWindow,wm_close,0,0);
  END;
END;

PROCEDURE tbrowse.handleCancel; BEGIN  PostMessage(HWindow,wm_close,0,0); END;

PROCEDURE tbrowse.wmdrawitem(VAR msg:tmessage);
VAR oldfont:hfont;
    a,b    :longint;
    c      :integer;
    drawframe:boolean;
    s      :STRING[50];
    memdc:hdc;
BEGIN
  WITH pdrawitemstruct(msg.lparam)^ DO
  BEGIN
   IF NOT(itemid=$ffff)AND((ctlid=id_filelist)OR(ctlid=id_dirlist))THEN
   BEGIN
     IF PaletteInstalled THEN SelectPalette(hdc,hpal,false);
     drawframe:=false;
     a:=cHGLB; b:=cVGLB;
     CASE itemaction OF
       oda_focus     :CASE itemstate OF
                       ods_disabled :BEGIN a:=cHGLB; b:=CVGGrayBox; END;
                       ods_focus
                       +ods_selected:BEGIN drawframe:=true; b:=cHGLB; a:=cVGLB; END;
                       ods_focus    :drawframe:=true;
                       1            :BEGIN a:=cVGLB; b:=cHGLB; END;
                      END;
       oda_drawentire:IF itemstate=1 THEN BEGIN a:=cVGLB; b:=cHGLB; END;
     END;
     oldfont:=SelectObject(hdc,ANSIhandle);
     SetBkMode(hdc,transparent);
     SetBkColor(hdc,a);
     SetTextColor(hdc,b);
     SetTextAlign(hdc,ta_left OR ta_top OR ta_noupdatecp);
     c:=-1;
     CASE ctlid OF
      id_filelist:c:=pfilelist^.GetString(s,itemid);
      id_dirlist:c:=pdirlist^.GetString(s,itemid);
     END;
     IF c>=0 THEN
     BEGIN
       IF ctlid<>id_dirlist THEN
       BEGIN
         s:=s+#0;
         ExtTextOut(hdc,rcitem.left,rcitem.top,eto_opaque,@rcitem,pchar(@s[1]),byte(s[0])-1,NIL);
       END ELSE
       BEGIN
         {Dirliste Text ausgeben und Bitmap malen}
         ExtTextOut(hdc,rcitem.left+33,rcitem.top,eto_opaque,@rcitem,pchar(@s[2]),byte(s[0])-1,NIL);
         memdc:=CreateCompatibleDC(hdc);
         SelectObject(memdc,drivebmphandle);
         IF (s[1]>=dlfloppytype)AND(s[1]<=dlremotetype)
         THEN  c:=(byte(s[1])-byte(dlfloppytype)+3)*28
         ELSE IF s[1]=dldirtype THEN c:=0 ELSE
              IF (byte(s[1])-dlarcoffset)=listfiletype THEN c:=28 ELSE c:=56;
         BitBlt(hdc,rcitem.left+1,rcitem.top,28,rcitem.bottom-rcitem.top,memdc,c,0,srccopy);
         DeleteDC(memdc);
       END;
       IF drawframe THEN DrawFocusRect(hdc,rcitem);
     END;
   END;
 END;
END;


PROCEDURE tbrowse.wmsyschar;
BEGIN
 CASE Lo(msg.wparam) OF
  70,102:pfilelist^.focus;      {klein f+F}
  68,100:pdirlist^.focus;       {klein d+D}
  byte('P'),byte('p'):ppathedit^.focus;
   ELSE BEGIN INHERITED wmsyschar(msg); Exit; END;
 END;
 msg.result:=-1;
END;

 PROCEDURE tbrowse.wmsyskeydown;
 VAR s:STRING[10];
 j:word;
 BEGIN
   IF (keystate<>4) THEN DefWndProc(msg)
   ELSE CASE msg.wparam OF
     vk_f2: BEGIN
              s:='[-'+path[1]+'-]'+#0;
              j:=GetDriveType(word(path[1])-97);
              IF j=drive_removable THEN Insert(dlfloppytype,s,1) ELSE
              IF j=drive_fixed THEN Insert(dlhdtype,s,1) ELSE Insert(dlremotetype,s,1);
              pdirlist^.SetSelString(pchar(@s[1]),-1);
              pdirlist^.focus;
              msg.result:=0;
            END;
     ELSE DefWndProc(msg);
   END ;
 END;

PROCEDURE tbrowse.wmkeydown;
VAR s:STRING[20]; i,j:integer;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    vk_back             :BEGIN
                           IF path[0]>#3 THEN
                           BEGIN
                             s:=dldirtype+'<..>'+#0;
                             pdirlist^.SetSelString(@s[1],-1);
                             path[0]:=#3; updatedirlist;
                           END;
                           pdirlist^.SetSelIndex(0);
                         END;
    vk_return,vk_execute:handleok(Msg);
    vk_f1,vk_help       :handlehilfe(Msg);
    vk_escape           :handleCancel(Msg);
    vk_prior            :IF keystate=1 THEN
                         BEGIN
                           s:=dldirtype+'<..>'#0;
                           IF pdirlist^.SetSelString(@s[1],-1)>=0 THEN updatedirlist;
                           IF pdirlist^.GetCount>0 THEN pdirlist^.focus;
                         END;
    vk_next             :IF keystate=1 THEN
                         BEGIN
                           IF (s[2]<>'.')AND(s[3]<>'.')AND(s[1]<>'[') THEN
                           IF pdirlist^.GetSelString(s,0)>0 THEN updatedirlist;
                         END;
    word('A')..WORD('Z'),word('0')..word('9'):BEGIN
                           IF msg.lparam=id_dirlist THEN
                           FOR i:=0 TO pdirlist^.GetCount-1 DO
                           BEGIN
                             pdirlist^.GetString(s,i);
                             IF (s[2]='[')AND(UpCase(s[4])=char(msg.wparam)) THEN
                             BEGIN
                               IF pdirlist^.setsel(i,_on)<>LB_ERR
                               THEN updatedirlist;
                               msg.result:=0;
                               Exit;
                             END;
                           END;
                         END;

        ELSE DefWndProc(msg);
   END;
END;

PROCEDURE tbrowse.WMCommand(VAR msg:tmessage);
VAR pc:ARRAY [0..150] OF char;
BEGIN
  IF msg.lparamhi=lbn_dblclk THEN
  BEGIN
   IF msg.wparam =id_dirlist  THEN updatedirlist ELSE
   IF msg.wparam =id_filelist THEN handleok(Msg);
  END ELSE
  IF msg.lparamhi=lbn_selchange THEN
  CASE msg.wparam OF
    id_filelist:
    BEGIN
      pfilelist^.GetSelString(exe_name,12);
      strpcopy(@pc,path+'\'+exe_name);
      ppathedit^.SetText(@pc);
    END;
    id_dirlist: setinfo(pdirlist^.HWindow);
  END ELSE INHERITED WMCommand(msg);
END;

PROCEDURE tbrowse.wmIHaveTheFocus; BEGIN setinfo(msg.wparam); INHERITED wmihavethefocus(msg); END;
{*****************************ENDE BROWSER*************************************************}

PROCEDURE tDirBrowse.updatedirlist;
VAR i,j,k,olderrormode           :word;
    f                            :tsearchrec;
    pc                           :ARRAY[0..255] OF char;
    s,s1,spath,probe             :STRING[100];
    box                          :pwaitbox;
    maxfiles,aktfiles            :word;
    fl:FILE;
    prevseldir                   :STRING[15];
    exe_found:boolean;
BEGIN
 olderrormode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
 exe_found:=false;
 spath:=path;
 prevseldir:='';
 BEGIN
 IF pdirlist^.GetSelString(s,SizeOf(s))>0 THEN
   CASE byte(s[1]) OF
   byte(dlfloppytype)..byte(dlremotetype):
    BEGIN
      GetDir(byte(s[4])-96,path);
      path:=path+#0;
      path:=strpas(AnsiLower(pchar(@path[1])));
      IF path[Length(path)]='\' THEN dec(path[0]);
    END;
   byte(dldirtype):
     BEGIN
       Delete(s,1,1);
       IF s='<..>' THEN
       BEGIN
         prevseldir:=dldirtype+'<'+prevdir(path)+'>';
         s1:=''; s:=path;
         WHILE Pos('\',s)<>0 DO
         BEGIN
           s1:=s1+Copy(s,1,Pos('\',s));
           Delete(s,1,Pos('\',s));
         END;
         Delete(s1,Length(s1),1);
         path:=s1;
       END ELSE path:=path+'\'+Copy(s,2,Length(s)-2);
     END;
  END;
  strpcopy(@pc,path+'\*.*');
  FindFirst(@pc,fafiles,f);
  IF (doserror=0)AND(GetDriveType(word(UpCase(path[1]))-Ord('A'))=DRIVE_REMOTE) THEN
  BEGIN
    Assign(fl,path+'\'+f.Name);
    GetFAttr(fl,i);
    IF i=0 THEN doserror:=1;
  END;
  IF (doserror>0)AND(doserror<>18) THEN
  BEGIN
    doserror:=0; Path:=Path[1]+':'; strpcopy(@pc,path+'\*.*');
    FindFirst(@pc,fafiles,f);
    IF (doserror=0)AND(GetDriveType(word(UpCase(path[1]))-Ord('A'))=DRIVE_REMOTE) THEN
    BEGIN Assign(fl,path+'\'+f.Name); GetFAttr(fl,i); IF i=0 THEN doserror:=1; END;
  END;
  IF (doserror>0)AND(doserror<>18) THEN
  BEGIN
    ourmessagebox(@self,errorver,getresstring(id_driveerror),mb_ok OR mb_iconstop);
    path:=spath; SetErrorMode(olderrormode); Exit;
  END ELSE
  BEGIN
    box:=initwaitbox(@self,getresstring(id_loadcheckfiles));
    {Lese Anzahl Files}
    maxfiles:=0;
    aktfiles:=0;
    WHILE doserror=0 DO
    BEGIN
      inc(maxfiles);
      FindNext(f);
    END;
    {Löschen der File und Dir Listbox}
    pdirlist^.ClearList;
    strpcopy(@pc,path+'\*.*');
    {Suche nach allem ,was im Verz. steht ;sortieren nach Datei und Verzeichnis}
    FindFirst(@pc,fafiles,f);
    WHILE doserror=0  DO
    BEGIN
      strcopy(f.Name,AnsiLower(f.Name));
      IF (f.Attr AND faDirectory)=faDirectory THEN
      BEGIN
        s:=strpas(f.Name);
        IF s<>'.' THEN
        BEGIN
          s:=dldirtype+'<'+s+'>';
          pdirlist^.AddString(s);
        END;
      END;
      inc(aktfiles);
      setwaitbox(box,((100.0*aktfiles)/maxfiles));
      FindNext(f);
    END;
    strpcopy(pc,path+'\');
    ppathedit^.SetText(AnsiUpper(@pc));
  END;
  FOR i:=0 TO 26 DO
  BEGIN
    j:=GetDriveType(i);
    IF (j<>0) THEN
    BEGIN
      s:='[-'+char(97+i)+'-]';
      IF j=drive_fixed THEN BEGIN Insert(dlhdtype,s,1); pdirlist^.AddString(s); END;
    END;
  END;
  closewaitbox(box);
 END;
 IF (pdirlist^.GetCount>0) THEN
 BEGIN
  IF prevseldir[0]=#0 THEN s:=dldirtype+'<..>'+#0
                      ELSE s:=prevseldir+#0;
  j:=pdirlist^.findstring(s);
  IF j>-1 THEN pdirlist^.SetSelIndex(j);
 END;
 SetErrorMode(olderrormode);
 setinfo(pdirlist^.HWindow);
END;

CONST X_Browse=300;
      Y_Browse=192;

CONSTRUCTOR tDirbrowse.Init;
VAR i:integer;
BEGIN
   INHERITED Init (AParent, NIL,ws_sysmenu,notopmost);
   p_edit:=anedit;
   WITH Attr DO
   BEGIN
     Style:=ws_popup OR ws_visible OR ws_border;
     X :=(GetSystemMetrics(sm_cxscreen)-x_browse)DIV 2;
     Y :=(GetSystemMetrics(sm_cyscreen)-y_browse)DIV 2;
     W :=X_browse+GetSystemMetrics(sm_cxborder);
     H :=Y_browse+capdy+(2*GetSystemMetrics(sm_cyborder));
   END;
   ppathedit:=New(pmyedit,Init(@self, id_pathname,NIL,6*fw+10,CapDY+8,X_Browse-(6*fw+20),Fh,150,true));
   pdirlist:=New(pmylistbox,Init(@self,id_dirlist,10 ,CapDY+39,29*fw,9*(fh+2),lbs_sort ,false));

   comments[1]:=New(pmystatic,Init(@self,0,NIL,PPathedit^.Attr.X-6*fw,PPathedit^.Attr.Y    ,5*Fw ,Fh,5));
   comments[2]:=New(pmystatic,Init(@self,0,NIL,PDirList^.Attr.X      ,PDirList^.Attr.Y-fh-4,17*Fw,Fh,17));

   book  :=New(pbmpbutton,Init(@self, id_Ook    ,PDirList^.Attr.X+PDirList^.Attr.W+7,PDirList^.Attr.Y-2,'bsok',false));
   bOquit:=New(pbmpbutton,Init(@self, id_Ocancel,BoOk^.Attr.X ,BOOk^.Attr.Y+30                       ,'bscancel',false));
   bOhelp:=New(pbmpbutton,Init(@self, id_Ohelp  ,BoOk^.Attr.X ,BOOk^.Attr.Y+60                       ,'bshelp',false));
   INFOstatic :=New(pmycenterstatic,Init(@self,id_infostatic,NIL,
                    PdirList^.Attr.X-2,PdirList^.Attr.Y+PdirList^.Attr.H+8,x_browse-18,Fh,80));
   drivebmphandle:=loadMybitmap(hinstance,'BMP_drives');
 END;


PROCEDURE tDirbrowse.SetupWindow;
VAR pc      :pchar; x:ARRAY[0..50] OF char;
    rec     :tsearchrec; fl:FILE;
    i,fattr:word;

BEGIN
  INHERITED SetupWindow;
  SendMessage(pdirlist^.HWindow,wm_setredraw,0,0);
  GetMem(pc,255);
  strcopy(pc,modc.shortver); strpcopy(x,getresstring(2029)); strcat(pc,x); SetCaption(pc);
  IF p_edit^.GetText(pc,150)>0 THEN
  BEGIN
    path:=strpas(AnsiLower(pc));
    findexe(path);
    path:=Copy(path,0,Length(path)-Length(prevdir(path)));
  END ELSE BEGIN path:=''; END;
  IF path='' THEN path:='C:\';
  IF path[byte(path[0])]<>'\' THEN path:=path+'\';
  i:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
  strpcopy(pc,path); strcat(pc,'*.*');
  {$i-}
  FindFirst(pc,fafiles,rec);
  IF (doserror=0)AND(GetDriveType(word(UpCase(path[1]))-Ord('A'))=DRIVE_REMOTE) THEN
  BEGIN
    Assign(fl,path+rec.Name);
    GetFAttr(fl,fattr);
    IF fattr=0 THEN doserror:=1;
  END;
  IF doserror IN[0,18] THEN ChDir(path) ELSE path:=m4wdir;
  IF path[byte(path[0])]='\' THEN dec(path[0]);
  SetErrorMode(i);
  FreeMem(pc,255);
  comments[1]^.SetText('&Path:');       comments[1]^.setassocwindow(Ppathedit^.HWindow,0);
  comments[2]^.SetText('&Directories:');   comments[2]^.setassocwindow(PDirlist^.HWindow,0);
  book^.enable; boquit^.enable; boHelp^.enable;
  updatedirlist;
  ppathedit^.SetSelection(0,ppathedit^.GetTextLen);
  oldfocus:=ppathedit^.HWindow;
  SendMessage(pdirlist^.HWindow,wm_setredraw,1,0);
  Parent^.disable;
END;

DESTRUCTOR tDirbrowse.Done;
BEGIN
  deleteMyBitmap(drivebmphandle);
  Parent^.enable;
  INHERITED Done;
END;

PROCEDURE tDirbrowse.wmEraseBkGnd(VAR msg:tmessage);
BEGIN
  paintcaption(msg.wParam);
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  framefilled3d(msg.wParam,0,CapDY,x_browse-1,y_browse,3,cForeGnd,up);
  WITH Ppathedit^.Attr DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  WITH PDirList^.Attr  DO frameFilled3d(msg.wParam,X-2,Y-2,W+4,H+4,1,CHGEdit,Down);
  WITH BOOk^.Attr DO Frame3D(Msg.Wparam,X-1,Y-1,W+2,H+2,1,Down);
  WITH BOQuit^.Attr DO Frame3D(Msg.Wparam,X-1,Y-1,W+2,H+2,1,Down);
  WITH BOHelp^.Attr DO Frame3D(Msg.Wparam,X-1,Y-1,W+2,H+2,1,Down);
  WITH InfoStatic^.Attr DO Frame3D(Msg.Wparam,X-1,Y-1,W+2,H+2,1,Down);
  INHERITED wmEraseBkGnd(msg);
END;

PROCEDURE tDirbrowse.setinfo;
VAR s2:STRING;
BEGIN
  box_id:=GetDlgCtrlID(box_id);
  CASE box_id OF
   id_dirlist:
     BEGIN
       pdirlist^.GetString(s2,pdirlist^.getcaretindex);
       CASE byte(s2[1]) OF
        zipfiletype+dlarcoffset:  s2:=getresstring(zip_id)+getresstring(enter_id);
        arjfiletype+dlarcoffset:  s2:=getresstring(arj_id)+getresstring(enter_id);
        LHARCfiletype+dlarcoffset:s2:=getresstring(lha_id)+getresstring(enter_id);
        listfiletype+dlarcoffset :s2:=getresstring(mol_id)+getresstring(enter_id);
        byte(dldirtype)             :IF (s2[3]='.')AND(s2[4]='.')
                                     THEN s2:=getresstring(leavedir_id)
                                     ELSE s2:=getresstring(dir_id);
        byte(dlfloppytype)          :s2:=getresstring(floppy_id);
        byte(dlhdtype)              :s2:=getresstring(hd_id);
        byte(dlremotetype)          :s2:=getresstring(remote_id);
       END;
     END;
   id_OOK,id_pathname : s2:=getresstring(2015);
   id_OCancel         : s2:=getresstring(2016);
   id_Ohelp           : s2:=getresstring(help_id);
  END;
  infostatic^.setstringtext(s2);
END;

PROCEDURE tDirbrowse.handleHilfe;
BEGIN
  helpme(@self,id_browsedlg);
END;

PROCEDURE tDirbrowse.handleOk;
VAR  s:STRING;
BEGIN
  path[0]:=char(byte(ppathedit^.GetText(@path[1],150)));
  s:=prevdir(path);
  IF Length(s)>12 THEN
  BEGIN
    ourmessagebox(@self,modc.shortver,getresstring(id_badinput),mb_iconstop);
    ppathedit^.focus;
    Exit;
  END ELSE
  BEGIN
    p_edit^.SetText(@path[1]);
    PostMessage(Parent^.HWindow,wm_getloadings,0,0);
    PostMessage(HWindow,wm_close,0,0);
  END;
END;

PROCEDURE tDirbrowse.handleCancel; BEGIN  PostMessage(HWindow,wm_close,0,0); END;

PROCEDURE tDirbrowse.wmdrawitem(VAR msg:tmessage);
VAR oldfont:hfont;
    a,b    :longint;
    c      :integer;
    drawframe:boolean;
    s      :STRING[50];
    memdc:hdc;
BEGIN
  WITH pdrawitemstruct(msg.lparam)^ DO
  BEGIN
   IF NOT(itemid=$ffff)AND(ctlid=id_dirlist)THEN
   BEGIN
     IF PaletteInstalled THEN SelectPalette(hdc,hpal,false);
     drawframe:=false;
     a:=cHGLB; b:=cVGLB;
     CASE itemaction OF
       oda_focus     :CASE itemstate OF
                       ods_disabled :BEGIN a:=cHGLB; b:=CVGGrayBox; END;
                       ods_focus
                       +ods_selected:BEGIN drawframe:=true; b:=cHGLB; a:=cVGLB; END;
                       ods_focus    :drawframe:=true;
                       1            :BEGIN a:=cVGLB; b:=cHGLB; END;
                      END;
       oda_drawentire:IF itemstate=1 THEN BEGIN a:=cVGLB; b:=cHGLB; END;
     END;
     oldfont:=SelectObject(hdc,ANSIhandle);
     SetBkMode(hdc,transparent);
     SetBkColor(hdc,a);
     SetTextColor(hdc,b);
     SetTextAlign(hdc,ta_left OR ta_top OR ta_noupdatecp);
     c:=-1;
     CASE ctlid OF
      id_dirlist:c:=pdirlist^.GetString(s,itemid);
     END;
     IF c>=0 THEN
     BEGIN
       IF ctlid<>id_dirlist THEN ELSE
       BEGIN
         {Dirliste Text ausgeben und Bitmap malen}
         ExtTextOut(hdc,rcitem.left+33,rcitem.top,eto_opaque,@rcitem,pchar(@s[2]),byte(s[0])-1,NIL);
         memdc:=CreateCompatibleDC(hdc);
         SelectObject(memdc,drivebmphandle);
         IF (s[1]>=dlfloppytype)AND(s[1]<=dlremotetype)
         THEN  c:=(byte(s[1])-byte(dlfloppytype)+3)*28
         ELSE IF s[1]=dldirtype THEN c:=0 ELSE
              IF (byte(s[1])-dlarcoffset)=listfiletype THEN c:=28 ELSE c:=56;
         BitBlt(hdc,rcitem.left+1,rcitem.top,28,rcitem.bottom-rcitem.top,memdc,c,0,srccopy);
         DeleteDC(memdc);
       END;
       IF drawframe THEN DrawFocusRect(hdc,rcitem);
     END;
   END;
 END;
END;


PROCEDURE tDirbrowse.wmsyschar;
BEGIN
 CASE Lo(msg.wparam) OF
  68,100:pdirlist^.focus;       {klein d+D}
  byte('P'),byte('p'):ppathedit^.focus;
   ELSE BEGIN INHERITED wmsyschar(msg); Exit; END;
 END;
 msg.result:=-1;
END;

 PROCEDURE tDirbrowse.wmsyskeydown;
 VAR s:STRING[10];
 j:word;
 BEGIN
   IF (keystate<>4) THEN DefWndProc(msg)
   ELSE CASE msg.wparam OF
     vk_f2: BEGIN
              s:='[-'+path[1]+'-]'+#0;
              j:=GetDriveType(word(path[1])-97);
              IF j=drive_removable THEN Insert(dlfloppytype,s,1) ELSE
              IF j=drive_fixed THEN Insert(dlhdtype,s,1) ELSE Insert(dlremotetype,s,1);
              pdirlist^.SetSelString(pchar(@s[1]),-1);
              pdirlist^.focus;
              msg.result:=0;
            END;
     ELSE DefWndProc(msg);
   END ;
 END;

PROCEDURE tDirbrowse.wmkeydown;
VAR s:STRING[20]; i,j:integer;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    vk_back             :BEGIN
                           IF path[0]>#3 THEN
                           BEGIN
                             s:=dldirtype+'<..>'+#0;
                             pdirlist^.SetSelString(@s[1],-1);
                             path[0]:=#3; updatedirlist;
                           END;
                           pdirlist^.SetSelIndex(0);
                         END;
    vk_return,vk_execute:handleok(Msg);
    vk_f1,vk_help       :handlehilfe(Msg);
    vk_escape           :handleCancel(Msg);
    vk_prior            :IF keystate=1 THEN
                         BEGIN
                           s:=dldirtype+'<..>'#0;
                           IF pdirlist^.SetSelString(@s[1],-1)>=0 THEN updatedirlist;
                           IF pdirlist^.GetCount>0 THEN pdirlist^.focus;
                         END;
    vk_next             :IF keystate=1 THEN
                         BEGIN
                           IF (s[2]<>'.')AND(s[3]<>'.')AND(s[1]<>'[') THEN
                           IF pdirlist^.GetSelString(s,0)>0 THEN updatedirlist;
                         END;
    word('A')..WORD('Z'),word('0')..word('9'):BEGIN
                           IF msg.lparam=id_dirlist THEN
                           FOR i:=0 TO pdirlist^.GetCount-1 DO
                           BEGIN
                             pdirlist^.GetString(s,i);
                             IF (s[2]='[')AND(UpCase(s[4])=char(msg.wparam)) THEN
                             BEGIN
                               IF pdirlist^.setsel(i,_on)<>LB_ERR
                               THEN updatedirlist;
                               msg.result:=0;
                               Exit;
                             END;
                           END;
                         END;

        ELSE DefWndProc(msg);
   END;
END;

PROCEDURE tDirbrowse.WMCommand(VAR msg:tmessage);
VAR pc:ARRAY [0..150] OF char;
BEGIN
  IF msg.lparamhi=lbn_dblclk THEN
  BEGIN
   IF msg.wparam =id_dirlist  THEN updatedirlist ELSE
   IF msg.wparam =id_filelist THEN handleok(Msg);
  END ELSE
  IF msg.lparamhi=lbn_selchange THEN
  CASE msg.wparam OF
    id_dirlist: setinfo(pdirlist^.HWindow);
  END ELSE INHERITED WMCommand(msg);
END;

PROCEDURE tDirbrowse.wmIHaveTheFocus; BEGIN setinfo(msg.wparam); INHERITED wmihavethefocus(msg); END;


BEGIN
END.
