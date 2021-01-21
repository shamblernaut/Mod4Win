UNIT copywnd;
{ $C Fixed Preload Permanent}
INTERFACE
USES
     newopen,owindows,odialogs,winprocs,strings,wintypes,windos,win31,
     winunit,modc,omemory,files;
CONST
     fm_copy:boolean=true;
     fm_move:boolean=false;
     wm_update=wm_user+101;

VAR  currentfmdir:STRING[fsDirectory];

TYPE pcopywindow=^tcopy;
     Tcopy=object(tmywindow)
       {Fensterpositionen der Childs}
       x_fman,y_fman, X_bOok,Y_bOok,  X_bOhelp,Y_bOhelp,
       X_bOquit,Y_bOquit,
       X_pathname,Y_pathname,dx_pathname,y_von,y_aus,y_in,
       X_filelist,Y_filelist,dx_filelist,dy_filelist,
       X_dirlist,Y_dirlist,dx_dirlist,dy_dirlist:integer;
       {aktueller Pfad}
       path:STRING[150];
       {Puffer für neue Liste}
       inputlist:pmydirs;
       inputindex:word;
       {Pointer auf die Listboxen}
       pfilelist,pdirlist:pmylistbox;
       {Pointer auf Editbox}
       ppathedit:pmyedit;
       {pointer auf Pfadnamenanzeige}
       pvon,paus,pin:pmystatic;
       {pointer auf Statics zur Betitelung der childs}
       comments:ARRAY[1..6] OF pmystatic;
       INFOstatic:pmystatic;
       {buttons im tcopyfenster}
       bOHelp,book,bOquit:pBmpButton;
       {bitmaps}
       bmphandles:ARRAY[1..allfiletypes] OF hbitmap;
       arcbmphandle:hbitmap;
       drivebmphandle:hbitmap;
       {MOL+ARC-Listendaten}
       aktliste:STRING;
       sauglistobj:pmydirs;
       {FILE-COUNT}
       aktarcliste:STRING; {kompletter Name}
       aktarcdir:STRING;
       songname,orgsongname:STRING[15];
       scarcindex:byte;
       c_or_m:boolean;
       pvontext,paustext,pintext:STRING[150];
       pParenHandle:pword;
       syschar: ARRAY[2005..2005] OF char;
       CONSTRUCTOR Init(aparent:pwindowsobject; Alist:pmydirs; aindex:word; copyormove:boolean; pWHandle:pWord);
       DESTRUCTOR Done; virtual;
       PROCEDURE SetupWindow; virtual;
       PROCEDURE wmErasebkgnd(VAR Msg:tMessage);     virtual WM_FIRST+WM_EraseBkGnd;
       PROCEDURE wmctlcolor(VAR msg:tmessage);       virtual wm_first+wm_ctlcolor;
       PROCEDURE wmdrawitem(VAR msg:tmessage);       virtual wm_first+wm_drawitem;
       PROCEDURE wmkeydown(VAR msg:tmessage);        virtual wm_first+wm_keydown;
       PROCEDURE wmsyschar(VAR msg:tmessage);        virtual wm_first+wm_syschar;
       PROCEDURE wmsyskeydown(VAR msg:tmessage);     virtual wm_first+wm_syskeydown;
       PROCEDURE WMCommand(VAR msg:tmessage);        virtual wm_first+wm_command;
       PROCEDURE wmIHaveTheFocus(VAR msg:tmessage);  virtual wm_first+wm_Ihavethefocus;
       PROCEDURE handleOK(VAR Msg:tMessage);         virtual id_first + id_OOK;
       PROCEDURE handleHilfe(VAR Msg:tMessage);      virtual id_first + id_Ohelp;
       PROCEDURE handleCancel(VAR Msg:tMessage);     virtual id_first + id_OCancel;
       PROCEDURE setinfo(box_id:hwnd);
       PROCEDURE updatedirlist;
       PROCEDURE splitplaylistitem(VAR nc:STRING; VAR dirwert,arcindex,arcdirwert:byte);
       FUNCTION  isvalidarcdir(VAR Name:STRING):boolean;
       PROCEDURE setlbredraw(how:byte);
       PROCEDURE wmdropfiles(VAR msg:tmessage);     virtual wm_first+wm_internaldrop;
     END;

IMPLEMENTATION
{***********************************************COPYWND********************************************}
PROCEDURE tcopy.wmdropfiles;
BEGIN
  CASE GetDlgCtrlID(msg.wparam) OF
    id_filelist:IF GetDlgCtrlID(msg.lparamlo)=id_dirlist
                THEN PostMessage(pdirlist^.HWindow,wm_keydown,vk_return,0);
  END;
  msg.result:=0;
END;

PROCEDURE tcopy.setlbredraw(how:byte);
BEGIN
  SendMessage(pfilelist^.HWindow,wm_setredraw,how,0);
  SendMessage(pdirlist^.HWindow,wm_setredraw,how,0);
END;

{Teilt einen PLAYLISTEINTRAG in NAmen und DIR-INDEX auf-benutzt bei PLAYLIST->FILELIST}
PROCEDURE tcopy.splitplaylistitem;
BEGIN
  dirwert:=byte(nc[3]); arcindex:=byte(nc[4]); arcdirwert:=byte(nc[5]);
  nc:=nc[1]+Copy(nc,6,8);
  WHILE Pos(' ',nc)<>0 DO system.Delete(nc,Pos(' ',nc),1);
END;

{schaut nach ob Archiv-Directory in aktueller Ebene angezeigt werden kann}
FUNCTION tcopy.isvalidarcdir;
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

PROCEDURE tcopy.updatedirlist;
VAR i,j,k,olderrormode           :word;
    f                            :tsearchrec;
    pc                           :ARRAY[0..255] OF char;
    s,s1,spath,probe             :STRING[100];
    box                          :pwaitbox;
    playlistcount,arccount,arcindex   :integer;
    maxfiles,aktfiles            :word;
    templist                     :pmydirs;
    filetypeindex,dummybyte      :byte;
    dirindex:integer;
    fl:FILE;
    prevseldir                   :STRING[15];
BEGIN
 olderrormode:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
 spath:=path;
 prevseldir:='';
 BEGIN
 IF pdirlist^.GetSelString(s,SizeOf(s))>0 THEN
   CASE byte(s[1]) OF
   byte(dlfloppytype)..byte(dlremotetype):
    BEGIN
      getcurdir(pc,byte(s[4])-96); path:=strpas(AnsiLower(pc));
      IF path[Length(path)]='\' THEN dec(byte(path[0]));
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
             pin^.SetText(''); pintext:='';
             ppathedit^.enable;
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
             pin^.SetText(''); pintext:='';
             ppathedit^.enable;
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
           strpcopy(pc,aktarcliste);
           pin^.SetText(AnsiUpper(@pc)); pintext:=aktarcliste;
           s:='&Dirs: '+Copy(s,3,byte(s[0]))+#0;
           comments[3]^.SetText(pchar(@s[1]));
           ppathedit^.disable;
         END ELSE
         BEGIN
           Dispose(sauglistobj,Done); sauglistobj:=NIL;
           errmessagebox(@self,id_noarc);
         END
       END ELSE
       BEGIN
         Dispose(sauglistobj,Done); sauglistobj:=NIL;
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
           pintext:=path+'\'+Copy(s,3,byte(s[0]));
           strpcopy(pc,shortpath(pintext,byte(dx_pathname DIV Fw)));
           pin^.SetText(AnsiUpper(@pc));
           strpcopy(@pc,pvontext);
           ppathedit^.SetText(@pc);
           ppathedit^.disable;
           setlbredraw(off);
           FOR i:=0 TO sauglistobj^.songcount-1 DO
           BEGIN
             s:=sauglistobj^.getsongstring(i)+#0;
             pfilelist^.InsertString(pchar(@s[1]),pfilelist^.GetCount);
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
    pdirlist^.ClearList;
    pfilelist^.ClearList;
    pdirlist^.AddString(dldirtype+'<..>');
    IF aktarcdir<>'' THEN s:='\' ELSE s:='';
    strpcopy(pc,s+aktarcdir+'\'+songname);
    ppathedit^.SetText(AnsiUpper(@pc));
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
        splitplaylistitem(s,dummybyte,dummybyte,filetypeindex);
        IF filetypeindex=byte(dirindex) THEN pfilelist^.AddString(s);
      END;
      pfilelist^.setcaretindex(0);
    END;
    setlbredraw(_on);
  END ELSE IF aktliste='' THEN
  BEGIN
    strpcopy(@pc,path+'\*.*');
    {Ist dieser PFAD +LAUFWERK VALID?}
    FindFirst(@pc,fafiles,f);
    IF doserror=0 THEN
    BEGIN
      Assign(fl,path+'\'+f.Name);
      GetFAttr(fl,i);
      IF i=0 THEN doserror:=1;
    END;
    IF (doserror>0)AND(doserror<>18) THEN
    BEGIN
      doserror:=0; Path:=Path[1]+':'; strpcopy(@pc,path+'\*.*');
      FindFirst(@pc,fafiles,f);
      IF doserror=0 THEN
      BEGIN Assign(fl,path+'\'+f.Name); GetFAttr(fl,i); IF i=0 THEN doserror:=1; END;
    END;
    IF (doserror>0)AND(doserror<>18) THEN
    BEGIN
      ourmessagebox(@self,errorver,getresstring(id_driveerror),mb_ok OR mb_iconstop);
      path:=spath; SetErrorMode(olderrormode); Exit;
    END ELSE
    BEGIN
      strpcopy(pc,path+'\'+songname);
      ppathedit^.SetText(AnsiUpper(@pc));
      templist:=New(pmydirsARC,Init(MaxArcListItems));
      {Erstelle temporäre Playliste im Speicher}
      box:=initwaitbox(@self,getresstring(id_loadcheckfiles));
      inputList^.TransferData(templist);
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
      {Suche nach allem ,was im Verz. steht ;sortieren nach Datei und Verzeichnis}
      FindFirst(@pc,fafiles,f);
      WHILE doserror=0  DO
      BEGIN
        strcopy(f.Name,AnsiLower(f.Name));
        IF inputList^.IsValid(isitdirectory,f,filetypeindex) THEN
        BEGIN
          s:=dldirtype+'<'+strpas(f.Name)+'>';
          pdirlist^.AddString(s);
        END
        ELSE IF inputList^.isvalidarchive(f,filetypeindex) THEN
        BEGIN
           s:=char(filetypeindex+dlarcoffset)+' '+strpas(AnsiLower(f.Name));
           pdirlist^.AddString(s);
        END ELSE
        IF inputList^.IsValid(NOT isitdirectory,f,filetypeindex) THEN
        BEGIN
          s:=strpas(f.Name); s:=Copy(s,1,(Pos('.',s))-1);
          IF filetypeindex=listfiletype THEN
          BEGIN
           IF (c_or_m=fm_copy) THEN
           BEGIN
             s:=strpas(f.Name);
             s:=char(listfiletype+dlarcoffset)+' '+strpas(AnsiLower(f.Name));
             pdirlist^.AddString(s);
           END;
          END ELSE
          BEGIN
            s:=char(filetypeindex)+s;
            pfilelist^.AddString(s);
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
        pdirlist^.AddString(s);
      END;
    END;
    setlbredraw(_on);
    closewaitbox(box);
    Dispose(templist,Done);
  END;
  IF (pdirlist^.GetCount>0) THEN
  BEGIN
   IF prevseldir[0]=#0 THEN pdirlist^.GetString(s,0)
                       ELSE s:=prevseldir;
   j:=pdirlist^.findstring(s+#0);
   IF j>-1 THEN pdirlist^.SetSelIndex(j);
  END;
 END;
 SetErrorMode(olderrormode);
 pfilelist^.setcaretindex(0);
 setinfo(pdirlist^.HWindow);
END;


CONSTRUCTOR tcopy.Init;
VAR i:integer;
    ts: STRING[50];
BEGIN
   inputlist:=alist; inputindex:=aindex; c_or_m:=copyormove;
   pParenHandle:=pWHandle;
   INHERITED Init (AParent, '',ws_sysmenu,notopmost);
     X_pathname  :=10+5*Fw+2;
     X_filelist  :=12;
     y_von       :=10;
     y_aus       :=y_von+Fh+6;
     Y_pathname  :=y_aus+Fh+6;
     y_in        :=y_pathname+Fh+6;
     Y_filelist  := y_in+2*Fh+10;
     Y_dirlist   := y_filelist;
     dy_filelist:=13*Fh;
     dx_dirlist :=18*Fw+10;
     dy_dirlist :=dy_filelist-30;
     y_fman:=6*Fh+dy_filelist+58;
     IF dy_dirlist<(y_fman-189) THEN dy_dirlist:=y_fman-189;
     dx_filelist:=(songlistentriesize+7)*Fw+10;
     x_fman:=40+dx_dirlist+dx_filelist;
     dx_pathname:=x_fman-(5*Fw)-25;
     X_dirlist  :=x_filelist+dx_filelist +14;
     X_bOquit    := x_dirlist+(dx_dirlist DIV 2)-21;
     Y_bOquit    := y_dirlist+dy_dirlist+10;
     X_bOok      := x_boquit-57;
     Y_bOok      := y_boquit;
     X_bOhelp    := x_boquit+57;
     Y_bOhelp    := y_book;
     WITH Attr DO
     BEGIN
       style:=ws_popup OR ws_visible OR ws_border;
       x :=(GetSystemMetrics(sm_cxscreen)-x_fman)DIV 2;
       y :=(GetSystemMetrics(sm_cyscreen)-y_fman)DIV 2;
       w :=X_fman+2*GetSystemMetrics(sm_cxborder);
       h :=Y_fman+capdy+(2*GetSystemMetrics(sm_cyborder));
    END;
  ppathedit:=New(pmyedit,Init(@self, id_pathname,'',X_pathname,CapDY+Y_pathname,dx_pathname,Fh,79,true));
  pdirlist:=New(pmylistbox,Init(@self,id_dirlist,x_dirlist,CapDY+y_dirlist,dx_dirlist,dy_dirlist,
  lbs_sort ,true));
  pvon:=New(pmystatic,Init(@self, 0,'',X_pathname,CapDY+Y_von,dx_pathname,Fh,79));
  paus:=New(pmystatic,Init(@self, 0,'',X_pathname,CapDY+Y_aus,dx_pathname,Fh,79));
  pin :=New(pmystatic,Init(@self, 0,'',X_pathname,CapDY+Y_in,dx_pathname,Fh,79));
  comments[1]:=New(pmystatic,Init(@self,0,'',(x_pathname-5*Fw)-2,CapDY+Y_pathname,5*Fw,Fh,5));
  comments[2]:=New(pmystatic,Init(@self,0,'',x_filelist-2,CapDY+Y_filelist-Fh-4,9*Fw,Fh,9));
  comments[3]:=New(pmystatic,Init(@self,0,'',x_dirlist,CapDY+Y_dirlist-Fh-4,18*Fw,Fh,18));
  comments[4]:=New(pmystatic,Init(@self,0,'',(x_pathname-5*Fw)-2,CapDY+Y_von,5*Fw,Fh,5));
  comments[5]:=New(pmystatic,Init(@self,0,'',(x_pathname-5*Fw)-2,CapDY+Y_aus,5*Fw,Fh,5));
  comments[6]:=New(pmystatic,Init(@self,0,'',(x_pathname-5*Fw)-2,CapDY+Y_in,5*Fw,Fh,5));
  book  :=New(pbmpbutton,Init(@self, id_Ook,X_bOok,CapDY+Y_bOok,'bsok',false));
  bOquit:=New(pbmpbutton,Init(@self, id_Ocancel,X_bOquit,CapDY+Y_bOquit,'bscancel',false));
  bOhelp:=New(pbmpbutton,Init(@self, id_Ohelp,X_bOhelp,CapDY+Y_bOhelp,'bshelp',false));
  pfilelist:=New(pmylistbox,Init(@self,id_filelist,x_filelist,CapDY+y_filelist,dx_filelist,dy_filelist,
  lbs_sort,false));
  INFOstatic :=New(pmycenterstatic,Init(@self,id_infostatic,'',
                   x_filelist,CapDY+Y_filelist+dy_filelist+12,x_fman-25,Fh,80));
  FOR i:=listfiletype TO allfiletypes
    DO bmphandles[i-3]:=loadmybitmap(hinstance,makeintresource(i-3));
  arcbmphandle:=loadmybitmap(hinstance,'ARC');
  drivebmphandle:=loadmybitmap(hinstance,'BMP_drives');
  sauglistobj:=NIL;
  aktliste:='';
  paustext:=''; pintext:=''; pvontext:='';
  FOR i := 2005 TO 2005 DO BEGIN ts := getresstring(i); syschar[i] := UpCase (ts[Pos ('&', ts) + 1]); END;
END;


PROCEDURE tcopy.SetupWindow;
VAR dir,arcdir:STRING[90];
    Arc:STRING[20];
    s:STRING;
    i:word;
BEGIN
  INHERITED SetupWindow;
  s:=modc.shortver;
  IF c_or_m=fm_copy THEN s:=s+getresstring(2003)
                    ELSE s:=s+getresstring(2004);
  s:=s+#0;
  SetCaption(@s[1]);
  getthecorrectpath(currentfmdir);
  path:=currentfmdir;
  s:=Inputlist^.getsongstring(inputindex);
  dir:=Inputlist^.getdirstring(byte(s[3]));
  Arc:=Inputlist^.getarcstring(byte(s[4]));
  scarcindex:=byte(Arc[2]);
  arcdir:=Inputlist^.getarcdirstring(byte(s[5]));
  IF arcdir[0]<>#0 THEN Insert('\',arcdir,1);
  songname:=Copy(s,6,8)+'.'+filetypesasstring[byte(s[1])];
  WHILE Pos(' ',songname)<>0 DO system.Delete(songname,Pos(' ',songname),1);
  orgsongname:=songname;
  IF s[4]=noarcconst THEN
  BEGIN
    pvontext:=dir+'\'+songname;
    s:=shortpath(pvontext,byte(dx_pathname DIV Fw))+#0;
    pvon^.SetText(AnsiUpper(@s[1]));
  END ELSE
  BEGIN
    pvontext:=arcdir+'\'+songname;
    s:=shortpath(pvontext,byte(dx_pathname DIV Fw))+#0;
    pvon^.SetText(AnsiUpper(@s[1]));
    paustext:=dir+'\'+Copy(Arc,3,Length(Arc));
    s:=shortpath(paustext,byte(dx_pathname DIV Fw))+#0;
    paus^.SetText(AnsiUpper(@s[1]));
  END;
  comments[1]^.setstringtext(getresstring(2005)); comments[1]^.setassocwindow(Ppathedit^.HWindow,0);
  comments[2]^.SetText('&Filelist:');   comments[2]^.setassocwindow(Pfilelist^.HWindow,0);
  comments[3]^.SetText('&Directories:'); comments[3]^.setassocwindow(PDirlist^ .HWindow,0);
  comments[4]^.setstringtext(getresstring(2006));
  comments[5]^.setstringtext(getresstring(2007));
  comments[6]^.setstringtext(getresstring(2008));
  book^.enable; boquit^.enable; boHelp^.enable;
  aktarcliste:='';
  updatedirlist;
  IF aktarc<>'' THEN
  BEGIN
    i:=pdirlist^.findstring(aktarc+#0);
   IF i<32768 THEN pdirlist^.SetSelIndex(i);
  END;
  ppathedit^.SetSelection(0,ppathedit^.GetTextLen);
  oldfocus:=ppathedit^.HWindow;
  Parent^.disable;
END;

DESTRUCTOR tcopy.Done;
VAR i:word;
BEGIN
  currentfmdir:=path;
  IF sauglistobj<>NIL THEN Dispose(sauglistobj,Done);
  FOR i:=listfiletype TO allfiletypes DO DeleteMyBitMap(bmphandles[i-3]);
  DeleteMyBitMap(drivebmphandle);
  DeleteMyBitMap(arcbmphandle);
  Parent^.enable;
  pParenHandle^:=0;
  INHERITED Done;
END;

PROCEDURE tcopy.wmErasebkgnd(VAR Msg:tMessage);
BEGIN
  paintcaption(msg.wParam);
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  framefilled3d(msg.wParam,0,CapDY,x_fman,y_fman,2,cForeGnd,up);
  frameFilled3d(msg.wParam,X_pathname-2,CapDY+Y_pathname-2,dx_pathname+4,Fh+4,1,down,cBlack);
  frameFilled3d(msg.wParam,X_pathname-2,CapDY+Y_von-2,dx_pathname+4,Fh+4,1,down,cBlack);
  frameFilled3d(msg.wParam,X_pathname-2,CapDY+Y_aus-2,dx_pathname+4,Fh+4,1,down,cBlack);
  frameFilled3d(msg.wParam,X_pathname-2,CapDY+Y_in-2,dx_pathname+4,Fh+4,1,down,cBlack);
  frame3d(msg.wParam,X_filelist-3,CapDY+Y_filelist-3,dx_filelist+6,dy_filelist+6,2,down);
  frame3d(msg.wParam,X_dirlist-3,CapDY+Y_dirlist-3,dx_dirlist+6,dy_dirlist+6,2,down);
  frame3d(msg.wParam,X_book-1,CapDY+Y_book-1,44,23,1,down);
  frame3d(msg.wParam,X_boquit-1,CapDY+Y_boquit-1,44,23,1,down);
  frame3d(msg.wParam,X_bohelp-1,CapDY+Y_bohelp-1,44,23,1,down);
  frame3d(msg.wParam,X_filelist-3,CapDY+Y_filelist+dy_filelist+10,infostatic^.Attr.w+6,Fh+4,2,down);
  INHERITED wmErasebkgnd(Msg);
END;

PROCEDURE tcopy.setinfo;
VAR index:integer;
    s:STRING[50];
    s2:STRING;
    arcindex,arcdirindex,dirindex:byte;
BEGIN
  box_id:=GetDlgCtrlID(box_id); s2:='';
  CASE box_id OF
   id_dirlist:
     BEGIN
       index:=pdirlist^.getcaretindex;
       pdirlist^.GetString(s,index);
       CASE byte(s[1]) OF
        zipfiletype+dlarcoffset:  s2:=getresstring(zip_id)+getresstring(enter_id);
        arjfiletype+dlarcoffset:  s2:=getresstring(arj_id)+getresstring(enter_id);
        LHARCfiletype+dlarcoffset:s2:=getresstring(lha_id)+getresstring(enter_id);
        listfiletype+dlarcoffset :s2:=getresstring(mol_id)+getresstring(enter_id);
        byte(dldirtype)             :IF (s[3]='.')AND(s[4]='.') THEN
                                    BEGIN
                                      IF (aktarcliste<>'') THEN
                                      IF aktarcdir=''
                                        THEN s2:=getresstring(leavearc_id)
                                        ELSE s2:=getresstring(leavearcdir_id)
                                      ELSE
                                      IF aktliste<>''
                                        THEN s2:=getresstring(leavemol_id)
                                        ELSE s2:=getresstring(leavedir_id);
                                    END ELSE
                                    IF aktarcliste=''
                                      THEN s2:=getresstring(dir_id)
                                      ELSE s2:=getresstring(arcdir_id);
        byte(dlfloppytype)          :s2:=getresstring(floppy_id);
        byte(dlhdtype)              :s2:=getresstring(hd_id);
        byte(dlremotetype)          :s2:=getresstring(remote_id);
       END;
     END;
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
     END ELSE s2:=getresstring(novalidfiles_id);
   id_OOK,id_pathname :
     IF c_or_m=fm_copy THEN s2:=getresstring(2010)
                       ELSE s2:=getresstring(2011);
   id_OCancel: s2:=getresstring(quit_id);
   id_Ohelp:   s2:=getresstring(help_id);
  END;
  infostatic^.setstringtext(s2);
END;

PROCEDURE tcopy.handleHilfe;
BEGIN
  IF c_or_m=fm_copy THEN helpme(@self,id_copydlg) ELSE helpme(@self,id_movedlg);
END;

PROCEDURE tcopy.handleOk;
VAR scarcdir,scdir,dsarcdir,dsdir,s:STRING[150];
    box:pwaitbox;
    result:boolean;
    i:word;
    x:pchar;
BEGIN
  scarcdir:=pvontext;
  scdir   :=paustext;
  dsarcdir[0]:=char(ppathedit^.GetText(@dsarcdir[1],150));
  dsdir   :=pintext;
  IF scdir='' THEN BEGIN scdir:=scarcdir; scarcdir:=''; END;
  IF dsdir='' THEN BEGIN dsdir:=dsarcdir; dsarcdir:=''; END;
  {check auf validen Namen}
  IF dsarcdir<>'' THEN
  BEGIN
    REPEAT
      i:=Pos('*',dsarcdir);
      IF i>0 THEN
      BEGIN
        IF dsarcdir[i-1]='.' THEN
        BEGIN
         Delete(dsarcdir,i,3);
         dsarcdir:=dsarcdir+Copy(orgsongname,Pos('.',orgsongname)+1,3);
        END ELSE
        BEGIN
          Delete(dsarcdir,i,1);
          Insert(Copy(orgsongname,1,Pos('.',orgsongname)-1),dsarcdir,i);
        END;
      END;
    UNTIL i=0;
    s:=prevdir(dsarcdir); IF Length(s)>12 THEN
    BEGIN
      ourmessagebox(@self,modc.shortver,getresstring(id_badinput),mb_iconstop);
      ppathedit^.focus;
      Exit;
    END;
  END ELSE
  BEGIN
    REPEAT
      i:=Pos('*',dsdir);
      IF i>0 THEN
      BEGIN
        IF dsdir[i-1]='.' THEN
        BEGIN
         Delete(dsdir,i,3);
         dsdir:=dsdir+Copy(orgsongname,Pos('.',orgsongname)+1,3);
        END ELSE
        BEGIN
          Delete(dsdir,i,1);
          Insert(Copy(orgsongname,1,Pos('.',orgsongname)-1),dsdir,i);
        END;
      END;
    UNTIL i=0;
    IF dsdir[2]<>':' THEN BEGIN Insert(path+'\',dsdir,1); END;
    IF dsdir[3]<>'\' THEN
    BEGIN
      i:=SetErrorMode(SEM_noopenfileerrorbox OR sem_failcriticalerrors);
      GetDir(byte(UpCase(dsdir[1]))-64,s);
      IF IOResult=0 THEN path:=s;
      SetErrorMode(i);
      IF s[Length(s)]<>'\' THEN s:=s+'\'; Delete(s,1,2);
      Insert(s,dsdir,3);
    END;
    IF isvaliddosname(dsdir) THEN
    BEGIN
      s:=prevdir(dsdir);
      IF (Length(dsdir)=2)OR(s='') THEN dsdir:=dsdir+orgsongname
      ELSE IF Pos('.',s)=0 THEN dsdir:=dsdir+Copy(orgsongname,Pos('.',orgsongname),4);
    END ELSE
    BEGIN
      ourmessagebox(@self,modc.shortver,getresstring(id_wronginput),mb_iconstop);
      ppathedit^.focus;
      Exit;
    END;
  END;
  {alle Buchstaben klein machen}
  x:=@dsdir[1]; x:=AnsiLower(@dsdir[1]);
  x:=@dsarcdir[1]; x:=AnsiLower(@dsarcdir[1]);
  x:=@scdir[1]; x:=AnsiLower(@scdir[1]);
  x:=@scarcdir[1]; x:=AnsiLower(@scarcdir[1]);
  {Check auf copy in sich selbst}
  IF (((scarcdir='')AND(dsarcdir=''))AND(scdir<>dsdir))OR
     ((scarcdir<>dsarcdir)OR(scdir<>dsdir)) THEN
  BEGIN
   SetClassWord(HWindow,gcw_hcursor,LoadCursor(0,idc_wait));
   IF scarcdir <> '' THEN s := ':' + scarcdir ELSE s := '';
   IF c_or_m=fm_copy THEN box:=initwaitbox(@self,getresstring(2003)+': '+scdir+s)
                     ELSE box:=initwaitbox(@self,getresstring(2004)+': '+scdir+s);
   disable;
   result:=inputlist^.copyfile(box,inputindex,scdir,dsdir,scarcdir,dsarcdir,
           scarcindex,(byte(aktarc[1])-dlarcoffset));
   IF c_or_m=fm_move THEN
   BEGIN
     IF result  THEN IF inputlist^.erasefile(box,inputindex,false) THEN
     SendMessage(Application^.MainWindow^.HWindow,wm_update,inputindex,0);
   END;
   enable;
   SetClassWord(HWindow,gcw_hcursor,LoadCursor(0,idc_arrow));
   Dispose(box,Done);
   IF result THEN PostMessage(HWindow,wm_close,0,0);
  END  ELSE
  BEGIN
    ourmessagebox(@self,modc.shortver,getresstring(2012),mb_iconstop);
    ppathedit^.focus;
  END;
END;

PROCEDURE tcopy.handleCancel;
BEGIN
  PostMessage(HWindow,wm_close,0,0);
END;

PROCEDURE tcopy.wmdrawitem(VAR msg:tmessage);
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
    IF PaletteInstalled THEN SelectPalette(hdc,hpal,false);
    IF NOT(itemid=$ffff)AND((ctlid=id_filelist)OR(ctlid=id_dirlist)OR(ctlid=id_playlist))THEN
    BEGIN
      drawframe:=false;
      a:=cHGLB;
      IF ctlid=id_dirlist THEN b:=cVGLB ELSE b:=cForeGnd;
      CASE itemaction OF
        oda_focus     :
          CASE itemstate OF
            ods_disabled:
              BEGIN a:=cHGLB; b:=cTextShadow; END;
            ods_focus
            +ods_selected:
              BEGIN
               drawframe:=true;
               IF ctlid=id_dirlist THEN BEGIN b:=cHGLB; a:=cVGLB; END;
              END;
            ods_focus    :
              drawframe:=true;
            1           :
              IF ctlid=id_dirlist THEN BEGIN a:=cVGLB; b:=cHGLB; END;
          END;
       oda_drawentire:
         IF itemstate =1 THEN
         BEGIN
           IF ctlid=id_dirlist THEN BEGIN a:=cVGLB; b:=cHGLB; END
                               ELSE BEGIN a:=cForeGnd; b:=cHGLB; END;
         END;
      END;
      oldfont:=SelectObject(hdc,ANSIhandle);
      SetBkMode(hdc,transparent);
      SetBkColor(hdc,a); SetTextColor(hdc,b);
      SetTextAlign(hdc,ta_left OR ta_top OR ta_noupdatecp);
     c:=-1;
     CASE ctlid OF
      id_filelist:c:=pfilelist^.GetString(s,itemid);
      id_dirlist :c:=pdirlist^.GetString(s,itemid);
     END;
     IF c>=0 THEN
      BEGIN
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
         ExtTextOut(hdc,rcitem.left+17,rcitem.top,eto_opaque,
                    @rcitem,pchar(@s[1]),byte(s[0])-1,NIL);
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
        ExtTextOut(hdc,rcitem.left+33,rcitem.top,eto_opaque,
                   @rcitem,pchar(@s[2]),byte(s[0])-1,NIL);
        SelectObject(memdc,drivebmphandle);
        IF (s[1]>=dlfloppytype)AND(s[1]<=dlremotetype) THEN  c:=(byte(s[1])-byte(dlfloppytype)+3)*28
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


PROCEDURE tcopy.wmctlcolor;
BEGIN
  IF msg.lparamlo=pFileList^.HWindow THEN msg.lparamhi:=CTLCOLOR_GRAYBOX ELSE
  IF ((msg.lparamlo=pvon^.HWindow)OR(msg.lparamlo=paus^.HWindow)
       OR(msg.lparamlo=pin^.HWindow)) THEN msg.lparamhi:=CTLCOLOR_INVSTATIC;
  INHERITED wmctlcolor(msg);
END;

PROCEDURE tcopy.wmsyschar;
VAR ch: char;
BEGIN
  ch := UpCase (char(Lo(msg.wparam)));
  CASE ch OF
    'F':pfilelist^.focus;
    'D':pdirlist^.focus;
    ELSE IF ch = syschar[2005] THEN ppathedit^.focus
    ELSE BEGIN INHERITED wmsyschar(msg); Exit; END;
  END;
  msg.result:=-1;
END;

PROCEDURE tcopy.wmsyskeydown;
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
             setinfo(pdirlist^.HWindow);
             msg.result:=0;
           END;
    ELSE DefWndProc(msg);
  END ;
END;

PROCEDURE tcopy.wmkeydown;
VAR s:STRING[20]; i,j:integer;
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
    vk_return,vk_execute:handleok(Msg);
    vk_f1,vk_help       :handlehilfe(Msg);
    vk_escape           :handleCancel(Msg);
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
                           END ELSE IF (msg.lparam=id_filelist) THEN
                           FOR i:=0 TO pfilelist^.GetCount-1 DO
                           BEGIN
                             pfilelist^.GetString(s,i);
                             IF (aktliste='')THEN j:=2 ELSE j:=6;
                             IF (UpCase(s[j])=char(msg.wparam)) THEN
                             BEGIN
                               pfilelist^.setsel(i,_on);
                               msg.result:=0;
                               Exit;
                             END;
                           END;
                         END;
        ELSE DefWndProc(msg);
   END;
END;

PROCEDURE tcopy.WMCommand(VAR msg:tmessage);
VAR pc:ARRAY[0..150] OF char;
    s:STRING;
BEGIN
  IF msg.lparamhi=lbn_dblclk THEN
  BEGIN
   IF  msg.wparam =id_dirlist THEN updatedirlist;
  END ELSE
  IF msg.lparamhi=lbn_selchange THEN
  CASE msg.wparam OF
  id_filelist:BEGIN setinfo(pfilelist^.HWindow); END;
  id_dirlist: setinfo(pdirlist^.HWindow);
  END ELSE
  IF msg.lparamhi=en_update THEN
  BEGIN
    IF msg.wparam=id_pathname THEN
    BEGIN
      ppathedit^.GetText(@pc,149);
      s:=strpas(@pc);
      s:=prevdir(s);
      IF s<>'' THEN songname:=s ELSE songname:=orgsongname;
    END;
  END;
  INHERITED WMCommand(msg);
END;
PROCEDURE tcopy.wmIHaveTheFocus(VAR msg:tmessage);
BEGIN
  setinfo(msg.wparam);
  INHERITED wmihavethefocus(msg);
END;


{****ENDE***************************************Setup********************************************************}
BEGIN
END.