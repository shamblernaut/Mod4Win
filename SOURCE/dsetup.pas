UNIT Dsetup;
{ $C Fixed Preload Permanent}
INTERFACE
USES WinTypes, WinDOS, OWindows, winunit, modc, newopen, keysel, language;

CONST maxCoords=22;
     coords:ARRAY[1..maxCoords,1..6] OF integer=({xs,ys,ws,hs,xd,yd}
     (0,0,46,11,32,39),   (44,0,36,11,210,39),   (78,0,62,11,273,39),   { Sampling, Buffers, min. Memory }
     (138,0,21,09,350,35),(183,11,13,11,371,47),(197,11,13,09,371,59),  { IDO, Off, Lin         }
     (211,11,18,09,371,71),(157,0,22,11,397,39),  (177,0,20,11,422,39), { CUB, PAN, Clip        }
     (195,0,55,11,456,39),(155,11,12,9,327,62),                         { Load State, KB        }
     (1,11,18,9,66,62),   (20,11,19,9,116,55),   (40,11,25,9,110,69),   { kHz, 8 bit, 16 bit    }
     (66,11,26,9,162,47), (93,11,30,9,162,59),   (124,11,30,11,162,71), { Mono, Stereo, Surrnd  }
     (0,22,53,11,30,85),  (104,22,107,11,30,195),(50,22,57,11,30,251),  { Mod4Win, Rec. Path, Mixer Path }
     (169,11,13,11,490,69),(183,11,13,11,490,55));                      { On, Off               }
     X_SETUP=     525;
     Y_SETUP=     318;
     X_devname=   10;
     Y_devname=   10;
     X_bName=     498;
     Y_bName=     8;
     X_sSR=       13;
     Y_sSR=       56;
     X_bSR=       45;
     Y_bSR=       53;
     X_bBits=     89;
     Y_bBits=     55;
     X_bMS=       141;
     Y_bMS=       47;
     x_nBlocks=   206;
     y_nBlocks=   56;
     x_bnBlocks=  238;
     y_bnBlocks=  53;
     x_Mem=       271;
     y_Mem=       56;
     X_bSSave=    433;
     Y_bSSave=    62;
     X_BSLang=    370;
     Y_BSLang=    62;
     X_bSOk=      392;
     Y_bSOk=      62;
     X_bSCancel=  474;
     Y_bSCancel=  62;
     X_bShelp=    474;
     Y_bShelp=    42;
{SETUP-IDS}
     id_devname=   1500; id_bName=     1501; id_sSR=       1502; id_bSR=       1503;
     id_bBits=     1504; id_mem=       1505; id_nBlocks=   1506; id_bnBlocks=  1507;
     id_bMS=       1508; id_bSOk=      1509; id_bSSave=    1510; id_bSCancel=  1511;
     id_bShelp=    1512; id_timermode= 1513; id_ratechange=1514; id_pausesetup=1515;
     id_playonload=1516; id_moveonclick=1517;  id_mixpath=1518; id_browse1=1519;
     id_browse2=1520;   id_browse3=1521;   id_browse4=1522;    id_arjpath=1523;
     id_zippath=1524;   id_lhapath=1525;   id_arjmore=1526;    id_zipmore=1527;
     id_lhamore=1528;   id_bdefault=1530;  id_arjsw=1540;      id_zipsw=1541;
     id_lhasw=1542;     id_showpack=1543;  id_bshotkey=1544;   id_bspacker=1545;
     id_bpm=1546;       id_loadstate=1547; id_bsLang=1548;     id_RecPath=1549;
     id_RecBrowse=1550; id_askHD=1551;     id_bsregist=1552;

CONST IsShare: boolean = {$IFDEF share} true {$ELSE} false {$ENDIF};

TYPE psetup=^setup;
     setup=object(tmywindow)
       DACID:Integer;
       pKeys:pKeysel;
       Cancel:boolean;
       inisettings,currsettings:tsettings;
       hsetupteile:hbitmap;
       numdevs    :word;
       bSLang,bShelp,bSSave,bSOk,bSCancel,bshotkey,bspacker,bsregist:pbmpbutton;
       bIDO,bPAN,bPAmp,bMS,bBits,bloadstate:pswitch;
       bSR,bName,bnblocks:pBmpUDButton;
       sr,mem,nBlocks:pbmplcd;
       devname:pbmpfont;
       comments,whatsel:ARRAY[1..6] OF pmystatic;
       sLang:pmystatic;
       ptimerswitch,pratechange,ppausesetup,pplayonload,pmoveonclick,PAskHD:plrswitch;
       MixPathEdit,RecPathEdit:PMyEdit;
       pMixBrowse,pRecBrowse:pbmpbutton;
       s_mixpath:ARRAY[0..fsPathName]  OF char;
       s_recpath:ARRAY[0..fsDirectory] OF char;
       s_ratechange,s_pausesetup,s_playonload,s_moveonclick,s_fullmove,s_AskHD:boolean;
       s_Language: TLang;
       syschar: ARRAY[2050..2055] OF char;
       CONSTRUCTOR Init(aparent:pwindowsobject; initsettings:tsettings);
       DESTRUCTOR Done; virtual;
       PROCEDURE SetupWindow; virtual;
       PROCEDURE wmEraseBkGnd(VAR msg:tmessage);        virtual WM_FIRST+WM_EraseBkGnd;
       PROCEDURE wmkeydown(VAR msg:tmessage);           virtual WM_FIRST+WM_KEYDOWN;
       PROCEDURE wmctlcolor(VAR msg:tmessage);          virtual wm_first+wm_ctlcolor;
       PROCEDURE wmsyschar(VAR msg:tmessage);           virtual wm_first+wm_syschar;
       PROCEDURE wmendbrowse(VAR msg:tmessage);         virtual wm_first+wm_getloadings;
       PROCEDURE setbuttons;
       PROCEDURE handleDevice(VAR Msg: TMessage);       virtual id_first + id_bname;
       PROCEDURE handleBlocks(VAR Msg: TMessage);       virtual id_first + id_bnBlocks;
       PROCEDURE handleSR(VAR Msg: TMessage);           virtual id_first + id_bsr;
       PROCEDURE handleBits(VAR Msg: TMessage);         virtual id_first + id_bbits;
       PROCEDURE handleMS(VAR Msg: TMessage);           virtual id_first + id_bms;
       PROCEDURE handleSHelp(VAR Msg: TMessage);        virtual id_first + id_bshelp;
       PROCEDURE handlehotkeys(VAR Msg: TMessage);      virtual id_first + id_bshotkey;
       PROCEDURE handlepacker(VAR Msg: TMessage);       virtual id_first + id_bspacker;
       PROCEDURE handleregister(VAR Msg: TMessage);     virtual id_first + id_bsregist;
       PROCEDURE handleSOk(VAR Msg: TMessage);          virtual id_first + id_bsok;
       PROCEDURE handleSCancel(VAR Msg: TMessage);      virtual id_first + id_bscancel;
       PROCEDURE handleSSave(VAR Msg: TMessage);        virtual id_first + id_bssave;
       PROCEDURE handleSLang(VAR Msg: TMessage);        virtual id_first + id_bsLang;
       PROCEDURE handleStimersw(VAR Msg: TMessage);     virtual id_first + id_timermode;
       PROCEDURE handleSratesw(VAR Msg: TMessage);      virtual id_first + id_ratechange;
       PROCEDURE handleSpausesw(VAR Msg: TMessage);     virtual id_first + id_pausesetup;
       PROCEDURE handleSloadsw(VAR Msg: TMessage);      virtual id_first + id_playonload;
       PROCEDURE handleAskHD(VAR Msg: TMessage);        virtual id_first + id_AskHD;
       PROCEDURE handleSmovesw(VAR Msg: TMessage);      virtual id_first + id_moveonclick;
       PROCEDURE handlemixbrowse(VAR Msg: TMessage);    virtual id_first + id_browse1;
       PROCEDURE handleAskSel (VAR Msg: TMessage);      virtual id_first + id_RecBrowse;
       PROCEDURE getedits;         virtual;
       PROCEDURE getresult(VAR result:tsettings);
     END;

TYPE parcopt=^tarcopt;
     tarcopt=object(tmywindow)
       Cancel:boolean;
       {Backup-Vars}
       s_arcpackexecs  :ARRAY[1..allarctypes] OF STRING[100];
       s_arcunpackexecs:ARRAY[1..allarctypes] OF STRING[100];
       s_showpack      :boolean;
       comments:ARRAY[1..4] OF pmystatic;
       whatsel :ARRAY[1..4] OF pmystatic;
       pmore   :ARRAY[1..3] OF pbmpbutton;
       pbrowse :ARRAY[1..3] OF pbmpbutton;
       pshowpack,parjsw,pzipsw,plhasw:plrswitch;
       baok,bacancel,bahelp:pbmpbutton;
       arjedit,zipedit,lhaedit:PMYEDIT;
       reskeys: ARRAY[2020..2021] OF char;
       syschar: ARRAY[2060..2060] OF char;
       CONSTRUCTOR Init(aparent:pwindowsobject);
       DESTRUCTOR Done; virtual;
       PROCEDURE SetupWindow; virtual;
       PROCEDURE wmEraseBkGnd(VAR msg:tmessage);      virtual WM_FIRST+WM_EraseBkGnd;
       PROCEDURE handleaHelp(VAR Msg: TMessage);      virtual id_first + id_bshelp;
       PROCEDURE handleaOk(VAR Msg: TMessage);        virtual id_first + id_bsok;
       PROCEDURE handleaCancel(VAR Msg: TMessage);    virtual id_first + id_bscancel;

       PROCEDURE handlearjsw(VAR Msg: TMessage);      virtual id_first + id_arjsw;
       PROCEDURE handlezipsw(VAR Msg: TMessage);      virtual id_first + id_zipsw;
       PROCEDURE handlelhasw(VAR Msg: TMessage);      virtual id_first + id_lhasw;
       PROCEDURE handleshowsw(VAR Msg: TMessage);     virtual id_first + id_showpack;
       PROCEDURE handlearjmore(VAR Msg: TMessage);    virtual id_first + id_arjmore;
       PROCEDURE handlezipmore(VAR Msg: TMessage);    virtual id_first + id_zipmore;
       PROCEDURE handlelhamore(VAR Msg: TMessage);    virtual id_first + id_lhamore;

       PROCEDURE handlearjbrowse(VAR Msg: TMessage);  virtual id_first + id_browse2;
       PROCEDURE handlezipbrowse(VAR Msg: TMessage);  virtual id_first + id_browse3;
       PROCEDURE handlelhabrowse(VAR Msg: TMessage);  virtual id_first + id_browse4;
       PROCEDURE getedits; virtual;
       PROCEDURE wmctlcolor(VAR msg:tmessage);        virtual wm_first + wm_ctlcolor;
       PROCEDURE wmsyschar(VAR msg:tmessage);         virtual wm_first+wm_syschar;
       PROCEDURE wmkeydown(VAR msg:tmessage);         virtual WM_FIRST+WM_KEYDOWN;
     END;

TYPE pmorearc=^tmorearc;
     tmorearc=object(tmywindow)
       Cancel:boolean;
       arctype:byte;
       commands:ARRAY[1..3] OF STRING[100];
       up_edit,p_edit,del_edit:PMYudEDIT;
       pok,phelp,pcancel,pdefault:pbmpbutton;
       comments:ARRAY[1..3] OF pmystatic;
       s_arccmds:ARRAY[1..allarctypes] OF STRING[30];
       syschar: ARRAY[2070..2072] OF char;
       CONSTRUCTOR Init(aparent:pwindowsobject; arcindex:byte);
       DESTRUCTOR Done; virtual;
       PROCEDURE SetupWindow; virtual;
       PROCEDURE wmkeydown(VAR msg:tmessage);           virtual WM_FIRST+WM_KEYDOWN;
       PROCEDURE wmsyschar(VAR msg:tmessage);           virtual wm_first+wm_syschar;
       PROCEDURE wmEraseBkGnd(VAR msg:tmessage);        virtual WM_FIRST+WM_EraseBkGnd;
       PROCEDURE handleHelp(VAR Msg: TMessage);         virtual id_first + id_bshelp;
       PROCEDURE handleOk(VAR Msg: TMessage);           virtual id_first + id_bsok;
       PROCEDURE handleCancel(VAR Msg: TMessage);       virtual id_first + id_bscancel;
       PROCEDURE handledefault(VAR Msg: TMessage);      virtual id_first + id_bdefault;
       PROCEDURE getedits; virtual;
     END;

IMPLEMENTATION

USES Strings, WinProcs, MMSystem, OMemory, PlayProc, RegDlg;

CONST lr_sw='LRSWITCH';
{********************************Archiv-Optionen*******************************************}

CONSTRUCTOR tarcopt.Init;
VAR i:word;
    pc:pchar;
    ts: STRING[50];
BEGIN
  Cancel:=true;
  INHERITED Init (AParent, '',ws_sysmenu,notopmost);
  WITH Attr DO
  BEGIN
    style:=ws_popup OR ws_visible OR ws_border;
    w :=(pwindow(Parent)^.Attr.w-2);
    h :=capdy+(2*GetSystemMetrics(sm_cyborder))+152;
    x :=pwindow(Parent)^.Attr.x+(x_setup DIV 2)-(w DIV 2);
    y :=pwindow(Parent)^.Attr.y+(y_setup DIV 2)-(h DIV 2);
    setcorrectwinpos(x,y,w,h);
  END;
  FOR i:=1 TO allarctypes DO
  BEGIN
    s_arcpackexecs[i]:=arcpackexecs[i];
    s_arcunpackexecs[i]:=arcunpackexecs[i];
  END;
  s_showpack:=showpack;
  baOk:=New(PbmpButton, Init(@self    , id_bsok    ,389,CapDY+122,'bsok',false));
  baCancel:=New(PbmpButton, Init(@self, id_bscancel,430,CapDY+122,'bscancel',false));
  bahelp:=New(PbmpButton, Init(@self  , id_bshelp  ,471,CapDY+122,'bshelp',false));

  comments[1]:=New(pmystatic,Init(@self,0,'&ARJ :',16,CapDY+18,5*Fw,Fh,5));
  comments[2]:=New(pmystatic,Init(@self,0,'&ZIP :',16,CapDY+43,5*Fw,Fh,5));
  comments[3]:=New(pmystatic,Init(@self,0,'&LHA :',16,CapDY+68,5*Fw,Fh,5));
  comments[4]:=New(pmystatic,Init(@self,0,'',16,CapDY+93,39*Fw,Fh,39));

  whatsel[1] :=New(pmystatic,Init(@self,1,'',54+5*fw,CapDY+18,9*Fw,Fh,9));
  whatsel[2] :=New(pmystatic,Init(@self,1,'',54+5*fw,CapDY+43,9*Fw,Fh,9));
  whatsel[3] :=New(pmystatic,Init(@self,1,'',54+5*fw,CapDY+68,9*Fw,Fh,9));
  whatsel[4] :=New(pmystatic,Init(@self,1,'',351,CapDY+93,13*Fw,Fh,13));
  GetMem(PC,100);
  parjsw:=New(plrswitch,Init(@self,id_arjsw,24+5*fw,CapDY+17,2,1,lr_sw));
  strpcopy(pc,arcunpackexecs[arjfiletype]);
  arjedit:=New(pmyedit,Init(@self,id_arjpath,pc,54+15*fw,CapDY+18,32*fw,Fh,254,true));
  pbrowse[1]:=New(PbmpButton, Init(@self, id_browse2,442,CapDY+14,'b_browse',false));
  pmore[1]  :=New(PbmpButton, Init(@self, id_arjmore,466,CapDY+14,'b_more',false));

  pzipsw:=New(plrswitch,Init(@self,id_zipsw,24+5*fw,CapDY+42,2,1,lr_sw));
  strpcopy(pc,arcunpackexecs[zipfiletype]);
  zipedit:=New(pmyedit,Init(@self,id_zippath,pc,54+15*fw,CapDY+43,32*fw,Fh,254,true));
  pbrowse[2]:=New(PbmpButton, Init(@self, id_browse3,442,CapDY+39,'b_browse',false));
  pmore[2]  :=New(PbmpButton, Init(@self, id_zipmore,466,CapDY+39,'b_more',false));

  plhasw:=New(plrswitch,Init(@self,id_lhasw,24+5*fw,CapDY+67,2,1,lr_sw));
  strpcopy(pc,arcunpackexecs[lharcfiletype]);
  lhaedit:=New(pmyedit,Init(@self,id_lhapath,pc,54+15*fw,CapDY+68,32*fw,Fh,254,true));
  pbrowse[3]:=New(PbmpButton, Init(@self, id_browse4,442,CapDY+64,'b_browse',false));
  pmore  [3]:=New(PbmpButton, Init(@self, id_lhamore,466,CapDY+64,'b_more',false));
  FreeMem(pc,100);
  pshowpack:=New(plrswitch,Init(@self,id_showpack,483,CapDY+92,2,1,lr_sw));
  FOR i := 2020 TO 2021 DO BEGIN ts := getresstring(i); reskeys[i] := ts[1]; END;
  FOR i := 2060 TO 2060 DO BEGIN ts := getresstring(i); syschar[i] := UpCase (ts[Pos ('&', ts) + 1]); END;
END;

DESTRUCTOR tarcopt.Done;
VAR i:byte;
BEGIN
  IF Cancel THEN
  BEGIN
    FOR i:=1 TO allarctypes DO
    BEGIN
      arcpackexecs[i]:=s_arcpackexecs[i];
      arcunpackexecs[i]:=s_arcunpackexecs[i];
      showpack:=s_showpack;
    END;
  END;
  Parent^.enable;
  INHERITED Done;
END;

PROCEDURE tarcopt.SetupWindow;
VAR i:word;
    s:STRING[80];
    Msg:TMessage;
BEGIN
  INHERITED SetupWindow;
  s:=modc.shortver+getresstring(2022)+#0;
  SetCaption(@s[1]);
  bahelp^.enable; baOk^.enable; baCancel^.enable;
  pshowpack^.enable;
  parjsw^.enable;
  pzipsw^.enable;
  plhasw^.enable;
  FOR i:=1 TO 3 DO
  BEGIN
    pbrowse[i]^.enable; pmore[i]^.enable;
    whatsel[i]^.setstringtext(getresstring(2020));
  END;
  comments[1]^.setassocwindow(arjedit^.HWindow,0);
  comments[2]^.setassocwindow(zipedit^.HWindow,0);
  comments[3]^.setassocwindow(lhaedit^.HWindow,0);
  IF showpack THEN i:=2 ELSE i:=1; pshowpack^.setpos(byte(i));
  comments[4]^.setstringtext(getresstring(2060)); comments[4]^.setassocwindow(pshowpack^.HWindow,0);
  handleshowsw(Msg);
  Parent^.disable;
  oldfocus:=baok^.HWindow;
END;

PROCEDURE tarcopt.wmEraseBkGnd(VAR msg:tmessage);
VAR
    memdc:hdc;
    i:integer;
    oldfont:word;
    rect:trect;
BEGIN
  paintcaption(msg.wParam);
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  framefilled3d(msg.wParam,0,CapDY,Attr.w-2,Attr.h-2-capdy,2,cForeGnd,up); {Hauptrahmen}
  WITH baok^.Attr DO frame3d(msg.wParam,x-1,y-1,(w*3),h+2,1,down);
  {FRAMES für SELCTION-GROUPS}
  frame3d(msg.wParam,7,CapDY+6,508,110,1,up);
  WITH parjsw^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH rect,parjsw^.Attr DO BEGIN left:=x-1; top:=y-1; right:=left+w+2; bottom:=top+h+2; END;
  FrameRect(msg.wParam,rect,BrBlack);
  WITH pzipsw^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH rect,pzipsw^.Attr DO BEGIN left:=x-1; top:=y-1; right:=left+w+2; bottom:=top+h+2; END;
  FrameRect(msg.wParam,rect,BrBlack);
  WITH plhasw^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH rect,plhasw^.Attr DO BEGIN left:=x-1; top:=y-1; right:=left+w+2; bottom:=top+h+2; END;
  FrameRect(msg.wParam,rect,BrBlack);
  WITH pshowpack^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH rect,pshowpack^.Attr DO BEGIN left:=x-1; top:=y-1; right:=left+w+2; bottom:=top+h+2; END;
  FrameRect(msg.wParam,rect,BrBlack);
  WITH arjedit^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,2,down);
  WITH zipedit^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,2,down);
  WITH lhaedit^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,2,down);
  FOR i:=1 TO 4 DO
  BEGIN
    WITH comments[i]^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
    WITH whatsel[i]^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,2,down);
  END;
  memdc:=CreateCompatibleDC(msg.wParam);
  SelectObject(memdc,m_label);
  BitBlt(msg.wParam,35,baok^.Attr.y,94,20,memdc,0,0,srccopy);
  DeleteDC(memdc);
  INHERITED wmEraseBkGnd(msg);
END;

PROCEDURE tarcopt.handleshowsw;
BEGIN
 showpack:=pshowpack^.GetPos=2;
 IF showpack THEN whatsel[4]^.setstringtext(getresstring(2026))
             ELSE whatsel[4]^.setstringtext(getresstring(2027));
END;

PROCEDURE tarcopt.handlearjsw;
VAR pc:pchar;
BEGIN
  pc:=MemAlloc(200);
  whatsel[1]^.GetText(PC,99);
  IF(parjsw^.GetPos=1)AND(PC^=reskeys[2021])THEN
  BEGIN
    arjedit^.GetText(PC,99); arcpackexecs[arjfiletype]:=StrPas(PC);
    whatsel[1]^.setstringtext(getresstring(2020));
    strpcopy(pc,arcunpackexecs[arjfiletype]); arjedit^.SetText(pc);
  END ELSE
  IF(parjsw^.GetPos=2)AND(PC^=reskeys[2020])THEN
  BEGIN
    arjedit^.GetText(PC,99); arcunpackexecs[arjfiletype]:=StrPas(PC);
    whatsel[1]^.setstringtext(getresstring(2021));
    strpcopy(pc,arcpackexecs[arjfiletype]); arjedit^.SetText(pc);
  END;
  FreeMem(pc,200);
END;
PROCEDURE tarcopt.handlezipsw;
VAR pc:pchar;
BEGIN
  pc:=MemAlloc(200);
  whatsel[2]^.GetText(PC,99);
  IF(pZIPsw^.GetPos=1)AND(PC^=reskeys[2021])THEN
  BEGIN
    ZIPedit^.GetText(PC,99); arcpackexecs[ZIPfiletype]:=StrPas(PC);
    whatsel[2]^.setstringtext(getresstring(2020));
    strpcopy(pc,arcunpackexecs[ZIPfiletype]); ZIPedit^.SetText(pc);
  END ELSE
  IF(pZIPsw^.GetPos=2)AND(PC^=reskeys[2020])THEN
  BEGIN
    ZIPedit^.GetText(PC,99); arcunpackexecs[ZIPfiletype]:=StrPas(PC);
    whatsel[2]^.setstringtext(getresstring(2021));
    strpcopy(pc,arcpackexecs[ZIPfiletype]); ZIPedit^.SetText(pc);
  END;
  FreeMem(pc,200);
END;
PROCEDURE tarcopt.handlelhasw;
VAR pc:pchar;
BEGIN
  pc:=MemAlloc(200);
  whatsel[3]^.GetText(PC,99);
  IF(pLHAsw^.GetPos=1)AND(PC^=reskeys[2021])THEN
  BEGIN
    LHAedit^.GetText(PC,99); arcpackexecs[LHARCfiletype]:=StrPas(PC);
    whatsel[3]^.setstringtext(getresstring(2020));
    strpcopy(pc,arcunpackexecs[LHARCfiletype]); LHAedit^.SetText(pc);
  END ELSE
  IF(pLHAsw^.GetPos=2)AND(PC^=reskeys[2020])THEN
  BEGIN
    LHAedit^.GetText(PC,99); arcunpackexecs[LHARCfiletype]:=StrPas(PC);
    whatsel[3]^.setstringtext(getresstring(2021));
    strpcopy(pc,arcpackexecs[LHARCfiletype]); LHAedit^.SetText(pc);
  END;
  FreeMem(pc,200);
END;

PROCEDURE tarcopt.handlearjmore; BEGIN Application^.MakeWindow(New(pmorearc,Init(@self,1))); END;
PROCEDURE tarcopt.handlezipmore; BEGIN Application^.MakeWindow(New(pmorearc,Init(@self,2))); END;
PROCEDURE tarcopt.handlelhamore; BEGIN Application^.MakeWindow(New(pmorearc,Init(@self,3))); END;
PROCEDURE tarcopt.handlearjbrowse; BEGIN Application^.MakeWindow(New(p_browse,Init(@self,arjedit))); END;
PROCEDURE tarcopt.handlezipbrowse; BEGIN Application^.MakeWindow(New(p_browse,Init(@self,zipedit))); END;
PROCEDURE tarcopt.handlelhabrowse; BEGIN Application^.MakeWindow(New(p_browse,Init(@self,lhaedit))); END;

PROCEDURE tarcopt.handleaHelp; BEGIN helpme(@self,id_arcoptdlg); END;
PROCEDURE tarcopt.handleaCancel; BEGIN PostMessage(HWindow,wm_close,0,0); END;

PROCEDURE tarcopt.getedits;
VAR pc:pchar;
BEGIN
  pc:=MemAlloc(200);
  arjedit^.GetText(pc,99);
  IF parjsw^.GetPos=1 THEN arcunpackexecs[arjfiletype]:=strpas(pc)
                      ELSE arcpackexecs[arjfiletype]:=strpas(pc);
  zipedit^.GetText(pc,99);
  IF pzipsw^.GetPos=1 THEN arcunpackexecs[zipfiletype]:=strpas(pc)
                      ELSE arcpackexecs[zipfiletype]:=strpas(pc);
  lhaedit^.GetText(pc,99);
  IF plhasw^.GetPos=1 THEN arcunpackexecs[lharcfiletype]:=strpas(pc)
                      ELSE arcpackexecs[lharcfiletype]:=strpas(pc);
  FreeMem(pc,200);
END;

PROCEDURE tarcopt.handleaOk;
BEGIN
  getedits;
  Cancel:=false;
  PostMessage(HWindow,wm_close,0,0);
END;

PROCEDURE tarcopt.wmkeydown;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    vk_return,vk_execute:handleaok(Msg);
    vk_f1,vk_help       :handleahelp(Msg);
    vk_escape           :handleacancel(Msg);
    ELSE DefWndProc(msg);
  END;
END;

PROCEDURE tarcopt.wmsyschar;
VAR ch: char;
BEGIN
  ch := UpCase (char(Lo(msg.wparam)));
  CASE ch OF
    'A':arjedit^.focus;
    'Z':zipedit^.focus;
    'L':lhaedit^.focus;
    ELSE IF ch = syschar[2060] THEN pshowpack^.focus
    ELSE BEGIN INHERITED wmsyschar(msg); Exit; END;
  END;
  msg.result:=-1;
END;

PROCEDURE tarcopt.wmctlcolor;
BEGIN
  IF GetDlgCtrlID(msg.lparamlo)=1 THEN msg.lparamhi:=ctlcolor_edit;
  INHERITED wmctlcolor(msg);
END;


{***********************************************Setup********************************************************}
CONSTRUCTOR setup.Init;
VAR i, j:integer;
    ts: STRING[50];
BEGIN
  Cancel:=true;
  INHERITED Init (AParent, '',ws_sysmenu,notopmost);
  Attr.style:=ws_popup OR ws_visible OR ws_border;
  Attr.x :=(GetSystemMetrics(sm_cxscreen)-x_setup)DIV 2;
  Attr.y :=(GetSystemMetrics(sm_cyscreen)-y_setup)DIV 2;
  Attr.w:=X_setup+2*GetSystemMetrics(sm_cxborder);
  Attr.h:=Y_setup+capdy+(2*GetSystemMetrics(sm_cyborder));
  numdevs:=waveoutgetnumdevs;
  WITH initsettings DO
  BEGIN
    IF (NOT (byte (DeviceID) IN [byte (dev_DAC),byte (dev_HDD)]))
    AND((NumDevs=0)OR(NOT (byte (DeviceID) IN [dev_DAC+1..NumDevs-1])))
    THEN DeviceId:=0;
    IF(samplerate>96000)OR(samplerate<1000)THEN samplerate:=44100;
    IF(bits<>16)AND(bits<>8)THEN bits:=8;
    IF DeviceID >= dev_DAC THEN IF NOT nBuffers IN [2..99] THEN nBuffers := 24;
    IF DeviceID  = dev_HDD THEN IF NOT nBuffers IN [1..99] THEN nBuffers :=  4;
    IF(stereo<1)OR(stereo>2)THEN BEGIN stereo:=1; Surround:=false; END;
  END;
  s_ratechange:=OvrRateChng;
  s_pausesetup:=pausesetup;
  s_playonload:=playonload;
  s_moveonclick:=MoveOnClick;
  s_fullmove:=fullmove;
  s_AskHD:=AskHD;
  s_Language:=CurrLanguage;
  strcopy(@s_mixpath,@mixerpath);
  strcopy(@s_recpath,@HDpath);
  inisettings:=initsettings;
  currsettings:=initsettings;

  bshotkey:=New(PbmpButton, Init(@self, id_bshotkey,9    ,CapDY+290,'bhotkeys',false));
  bspacker:=New(PbmpButton, Init(@self, id_bspacker,70   ,CapDY+290,'bpacker',false));
  IF IsShare THEN bsregist:=New(PbmpButton, Init(@self, id_bsregist,131  ,CapDY+290,'bregister',false));
  IF Languages>1 THEN BEGIN
    IF IsShare THEN i := 292 ELSE i := 261;
    bSLang:=New(PbmpButton, Init(@self, id_bsLang  ,i    ,CapDY+290,'b_Lang',false));
  END;
  bSOk    :=New(PbmpButton, Init(@self, id_bsok    ,350  ,CapDY+290,'bsok',false));
  bSSave  :=New(PbmpButton, Init(@self, id_bssave  ,391  ,CapDY+290,'bssave',false));
  bSCancel:=New(PbmpButton, Init(@self, id_bscancel,432  ,CapDY+290,'bscancel',false));
  bShelp  :=New(PbmpButton, Init(@self, id_bshelp  ,473  ,CapDY+290,'bshelp',false));

  IF Languages>1 THEN
    sLang:=New(pmystatic,Init(@self,1,@LangStr[Currlanguage,_name][1],i-9*Fw,CapDY+294,8*Fw,Fh,7));

  devname:=New(pbmpfont,Init(@self, id_devname,X_devname,CapDY+Y_devname,30,2,'font_16x20',64));
  DACID:=200;
  WITH currsettings DO
  BEGIN
    i:=DeviceID; IF i=dev_HDD THEN i:=NumDevs; j:=0;
    IF mod_CanPlay (defDACSetting, OvrRateChng) THEN BEGIN dec (j); DACID:=j; IF (DeviceID=dev_DAC) THEN i:=j END;
    bName:=New(pbmpudbutton,Init(@self,id_bname,x_bname,CapDY+y_bname,j,numdevs,i,'b_up_down_1'));
    i := 1;  
    j := 96;
    bSR:=  New(pbmpudbutton,Init(@self,id_bsr,x_bsr,CapDY+y_bsr,-j,-i,-currsettings.samplerate DIV 1000,'b_up_down_1'));
    bBits:=New(pswitch, Init(@self, id_bbits,x_bbits,CapDY+y_bbits,2,1,'switch'));
    bMS:=  New(pswitch, Init(@self,id_bms,x_bms,CapDY+y_bms,3,1,'switch'));
    i := 2; 
    j := 99; 
    bnblocks:=New(pbmpudbutton,Init(@self,id_bnblocks,x_bnblocks,CapDY+y_bnblocks,-j,-i,-nBuffers,'b_up_down_1'));
  END;
  bIDO      :=New(pswitch, Init(@self,0,351,CapDY+y_bms,3,1,'switch'));
  bPAN      :=New(pswitch, Init(@self,0,398,CapDY+55,2,1,'switch'));
  bPAmp     :=New(pswitch, Init(@self,0,423,CapDY+55,2,1,'switch'));
  bloadstate:=New(pswitch, Init(@self,0,468,CapDY+55,2,1,'switch'));

  sr:=New(pbmplcd,Init(@self,id_ssr,x_ssr,CapDY+y_ssr,2,0,'lcd_13x20'));
  mem:=New(pbmplcd,Init(@self,id_mem,x_mem,CapDY+y_mem,4,0,'lcd_13x20'));
  nblocks:=New(pbmplcd,Init(@self,id_nblocks,x_nblocks,CapDY+y_nblocks,2,0,'lcd_13x20'));
  FOR I:=1 TO 5 DO
  BEGIN
    comments[I]:=New(PMyStatic,Init(@self,0,NIL,16 ,CapDY+100+((I-1)*(fh+6)),39*Fw,Fh,40));
    whatsel[I] :=New(PMyStatic,Init(@self,1,NIL,351,CapDY+100+((I-1)*(fh+6)),13*Fw,Fh,13));
  END;

  ptimerswitch:=New(plrswitch,Init(@self,id_timermode  ,471,CapDY+99 ,3,1,lr_sw));
  pratechange :=New(plrswitch,Init(@self,id_ratechange ,483,CapDY+117,2,1,lr_sw));
  ppausesetup :=New(plrswitch,Init(@self,id_pausesetup ,483,CapDY+135,2,1,lr_sw));
  pplayonload :=New(plrswitch,Init(@self,id_playonload ,483,CapDY+153,2,1,lr_sw));
  pmoveonclick:=New(plrswitch,Init(@self,id_moveonclick,471,CapDY+171,3,1,lr_sw));

  RecPathEdit:=New(pmyedit,Init(@self,id_RecPath ,@HDPath,16,CapDY+208,59*fw-1,Fh,SizeOf(HDPath),true));
  pRecBrowse :=New(PbmpButton, Init(@self,id_RecBrowse,  492,CapDY+204,'b_browse',false));
  comments[6]:=New(PMyStatic,Init(@self,0,NIL,16 ,RecPathEdit^.Attr.Y+RecPathEdit^.Attr.H+10,39*Fw,Fh,40));
  whatsel[6] :=New(PMyStatic,Init(@self,1,NIL,351,RecPathEdit^.Attr.Y+RecPathEdit^.Attr.H+10,13*Fw,Fh,13));
  pAskHD     :=New(plrswitch,Init(@self,id_AskHD,488,RecPathEdit^.Attr.Y+RecPathEdit^.Attr.H+9,2,1,lr_sw));

  MixPathEdit:=New(pmyedit,Init(@self,id_mixpath ,@mixerpath,16 ,CapDY+264,59*fw-1,Fh,SizeOf(mixerpath),true));
  pMixBrowse :=New(PbmpButton, Init(@self, id_browse1        ,492,CapDY+260,'b_browse',false));

  sr^.setbmpzahl(currsettings.samplerate DIV 1000);
  hsetupteile:=loadmybitmap(hinstance,'sgraypieces');
  FOR i := 2050 TO 2055 DO BEGIN ts := getresstring(i); syschar[i] := UpCase (ts[Pos ('&', ts) + 1]); END;
END;

DESTRUCTOR setup.Done;
VAR i:byte;
BEGIN
  i:=1;
  IF Cancel THEN
  BEGIN
    currsettings:=inisettings;
    OvrRateChng:=s_ratechange;
    pausesetup:=s_pausesetup;
    playonload:=s_playonload;
    moveonclick:=s_moveonclick;
    fullmove:=s_fullmove;
    AskHD:=s_AskHD;
    CurrLanguage:=s_Language;
    strcopy(@mixerpath,@s_mixpath);
    strcopy(@HDpath,@s_recpath);
    i:=0;
  END;
  SendMessage(Parent^.HWindow,wm_getresult,word(i),0);
  deletemybitmap(hsetupteile);
  Parent^.enable;
  INHERITED Done;
END;

PROCEDURE setup.SetupWindow;
VAR i:word;
    pc:pchar;
    Msg:tMessage;
BEGIN
  INHERITED SetupWindow;
  pc:=MemAlloc(200); strcopy(pc,longver); strcat(pc,' Setup'); SetCaption(pc); FreeMem(pc,200);
  handledevice(Msg);
  devname^.enable; nBlocks^.enable; bnBlocks^.enable; mem^.enable; bSR^.enable; bMS^.enable; bBits^.enable;
  bName^.enable; bShelp^.enable; bSSave^.enable; bSOk^.enable; bSCancel^.enable; sr^.enable; ptimerswitch^.enable;
  pratechange^.enable;  ppausesetup^.enable;  pplayonload^.enable; pmoveonclick^.enable; bshotkey^.enable;
  bloadstate^.enable; bIDO^.enable; bPan^.enable; bPAmp^.enable; bspacker^.enable; pMixBrowse^.enable; pRecBrowse^.enable;
  pAskHD^.Enable;
  IF Languages>1 THEN bSLang^.enable;
  IF IsShare THEN bsRegist^.enable;

  ptimerswitch^.setpos(CurrSettings.tmode+1);
  FOR i:=1 TO 6 DO comments[i]^.setstringtext(getresstring(2050+(i-1)));
  FOR i:=1 TO 3 DO whatsel[i]^.setstringtext(getresstring(2020));
  comments[1]^.setassocwindow(ptimerswitch^.HWindow,0);
  IF OvrRateChng THEN i:=2 ELSE i:=1; pratechange^.setpos(byte(i));
  comments[2]^.setassocwindow(pratechange^.HWindow,0);
  IF pausesetup THEN i:=2 ELSE i:=1; ppausesetup^.setpos(byte(i));
  comments[3]^.setassocwindow(ppausesetup^.HWindow,0);
  IF playonload THEN i:=2 ELSE i:=1; pplayonload^.setpos(byte(i));
  comments[4]^.setassocwindow(pplayonload^.HWindow,0);
  IF fullmove THEN i:=3 ELSE IF moveonclick THEN i:=2 ELSE i:=1; pmoveonclick^.setpos(byte(i));
  comments[5]^.setassocwindow(pmoveonclick^.HWindow,0);
  IF AskHD THEN i:=2 ELSE i:=1; pAskHD^.setpos(byte(i));
  comments[6]^.setassocwindow(pAskHD^.HWindow,0);

  WITH CurrSettings DO
  BEGIN
    bPAmp^.SetPos (Succ (PreAmp SHR 7));
    bIDO^.SetPos  (Succ (byte (OverSamp)));
    bPAN^.SetPos  (Succ (byte (Panning)));
  END;

  IF Load_state THEN i:=2 ELSE i:=1; bloadstate^.setpos(byte(i));
  {handleStimersw;}
  handleSratesw(Msg); handleSpausesw(Msg); handleSloadsw(Msg); handlesmovesw(Msg); handleAskHD(Msg);
  Parent^.disable;
  handledevice(Msg);
  oldfocus:=bsok^.HWindow;
END;

PROCEDURE setup.wmEraseBkGnd(VAR msg:tmessage);
VAR
    memdc:hdc;
    i:integer;
    oldfont:word;
    rect:trect;
BEGIN
  paintcaption(msg.wParam);
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  memdc:=CreateCompatibleDC(msg.wParam);
  SelectObject(memdc,hsetupteile);
  framefilled3d(msg.wParam,0,CapDY,x_setup,y_setup,2,cForeGnd,up); {Hauptrahmen}
  WITH rect DO
  BEGIN
    left:=7; top:=CapDY+43; right:=left+189; bottom:=top+41; FrameRect(msg.wParam,rect,BrBlack); {Sampling}
    left:=200; top:=CapDY+43; right:=left+61; bottom:=top+41; FrameRect(msg.wParam,rect,BrBlack); {Buffers}
    left:=265; top:=CapDY+43; right:=left+77; bottom:=top+41; FrameRect(msg.wParam,rect,BrBlack); {minmem}
    left:=346; top:=CapDY+43; right:=447; bottom:=top+41;    FrameRect(msg.wParam,rect,BrBlack); {SOURROUND...}
    left:=451; top:=CapDY+43; right:=517; bottom:=top+41;    FrameRect(msg.wParam,rect,BrBlack); {LoadstATE}
  END;
  {OKSAVENOHELP}
  WITH bsok^.Attr DO frame3d(msg.wParam,x-1,y-1,(w*4)-1,h+2,1,down);
  {HOTKEY/PACKER}
  IF IsShare THEN i := 3 ELSE i := 2;
  WITH bshotkey^.Attr DO frame3d(msg.wParam,x-1,y-1,(w*i)+1,h+2,1,down);
  { BMP LCD's }
  framefilled3d(msg.wParam,x_devname-3,CapDY+y_devname-3,486,27,2,cBackGnd,down); {Name}
  framefilled3d(msg.wParam,x_ssr-3,    CapDY+y_ssr-3,     32,27,2,cBackGnd,down); {Samprate}
  framefilled3d(msg.wParam,x_mem-3,    CapDY+y_mem-3,     58,27,2,cBackGnd,down); {minmem}
  framefilled3d(msg.wParam,x_nblocks-3,CapDY+y_nblocks-3, 32,27,2,cBackGnd,down); {buffers}
  {SW}
  WITH bPAmp^.Attr      DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH bPAN^.Attr       DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH bIDO^.Attr       DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH bms^.Attr        DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH bbits^.Attr      DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH bloadstate^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  {ud}
  WITH bname^.Attr      DO frame3d(msg.wParam,x-1,y-1,w+2,h+2,1,down);
  WITH bsr^.Attr        DO frame3d(msg.wParam,x-1,y-1,w+2,h+2,1,down);
  WITH bnblocks^.Attr   DO frame3d(msg.wParam,x-1,y-1,w+2,h+2,1,down);
  {Browse}
  WITH pMixBrowse^.Attr DO Frame3D(msg.wParam,x-1,y-1,w+2,h+2,1,down);
  WITH pRecBrowse^.Attr DO Frame3D(msg.wParam,x-1,y-1,w+2,h+2,1,down);

  WITH rect DO
  BEGIN
    top:=CapDY+y_bMS-1; bottom:=top+35;
    left:=bms^.Attr.x-1       ; right:=left+20; FrameRect(msg.wParam,rect,BrBlack);
    left:=bIDO^.Attr.x-1      ; right:=left+20; FrameRect(msg.wParam,rect,BrBlack);
    top:=CapDY+54; bottom:=top+24;
    left:=bbits^.Attr.x-1     ; right:=left+20; FrameRect(msg.wParam,rect,BrBlack);
    left:=bloadstate^.Attr.x-1; right:=left+20; FrameRect(msg.wParam,rect,BrBlack);
    left:=bPAN^.Attr.x-1      ; right:=left+20; FrameRect(msg.wParam,rect,BrBlack);
    left:=bPAmp^.Attr.x-1     ; right:=left+20; FrameRect(msg.wParam,rect,BrBlack);
  END;
  {FRAMES für SELECTION-GROUPS}
  WITH Comments[1]^.Attr DO frame3d(msg.wParam,X-9,Y-10,510,102,1,up);
  WITH RecPathEdit^.Attr DO frame3d(msg.wParam,X-9,Y-8,510,48,1,up);
  WITH MixPathEdit^.Attr DO frame3d(msg.wParam,X-9,Y-8,510,29,1,up);

  WITH ptimerswitch^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH rect,ptimerswitch^.Attr DO BEGIN left:=x-1; top:=y-1; right:=left+w+2; bottom:=top+h+2; END;
  FrameRect(msg.wParam,rect,BrBlack);
  WITH pratechange^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH rect,pratechange^.Attr DO BEGIN left:=x-1; top:=y-1; right:=left+w+2; bottom:=top+h+2; END;
  FrameRect(msg.wParam,rect,BrBlack);
  WITH ppausesetup^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH rect,ppausesetup^.Attr DO BEGIN left:=x-1; top:=y-1; right:=left+w+2; bottom:=top+h+2; END;
  FrameRect(msg.wParam,rect,BrBlack);
  WITH pplayonload^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH rect,pplayonload^.Attr DO BEGIN left:=x-1; top:=y-1; right:=left+w+2; bottom:=top+h+2; END;
  FrameRect(msg.wParam,rect,BrBlack);
  WITH pmoveonclick^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH rect,pmoveonclick^.Attr DO BEGIN left:=x-1; top:=y-1; right:=left+w+2; bottom:=top+h+2; END;
  FrameRect(msg.wParam,rect,BrBlack);
  WITH pAskHD^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH rect,pAskHD^.Attr DO BEGIN left:=x-1; top:=y-1; right:=left+w+2; bottom:=top+h+2; END;
  FrameRect(msg.wParam,rect,BrBlack);
  WITH MixPathEdit^.Attr DO FrameFilled3d(msg.wParam,x-2,y-2,w+4,h+4,1,CHGEdit,down);
  WITH RecPathEdit^.Attr DO FrameFilled3d(msg.wParam,x-2,y-2,w+4,h+4,1,CHGEdit,down);
  FOR i:=1 TO 6  DO WITH comments[i]^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  FOR i:=1 TO 6  DO WITH whatsel[i]^.Attr  DO FrameFilled3d(msg.wParam,x-2,y-2,w+4,h+4,1,CHGEDit,down);
  FOR i:=1 TO maxCoords DO BitBlt(msg.wParam,coords[i][5],CapDY+coords[i][6],coords[i][3],coords[i][4],
                                  memdc,coords[i][1],coords[i][2],srccopy);
  {Sprache}
  IF Languages>1 THEN
  framefilled3d(msg.wParam,sLang^.Attr.x-2-FW,bsLang^.Attr.y-2,sLang^.Attr.w+bsLang^.Attr.w+4+2*FW,
                bsLang^.Attr.h+4,1,cBackGnd,down);
  DeleteDC(MemDC);
  INHERITED wmEraseBkGnd(msg);
END;

PROCEDURE setup.setbuttons;
VAR Msg:TMessage;
BEGIN
  WITH currsettings DO
  BEGIN
    bms^.setpos(stereo+Byte(Surround));
    bbits^.setpos(bits DIV 8);
    bSR^.setpos(-currsettings.samplerate DIV 1000);
    sr^.setbmpzahl(currsettings.samplerate DIV 1000);
    bnBlocks^.SetPos(-nBuffers);
    nBuffers := -bnBlocks^.GetPos;
    nBlocks^.setbmpzahl(nBuffers);
    IF DeviceID = dev_HDD
    THEN mem^.SetBMPZahl (Trunc(1.0*nBuffers*32768/1024))
    ELSE mem^.SetBMPZahl (Trunc(1.0*nBuffers*stereo*Bits/8*10*20*samplerate/1000/1024));
    bPAmp^.SetPos (Succ (PreAmp SHR 7));
    bIDO^.SetPos  (Succ (byte (OverSamp)));
    bPAN^.SetPos  (Succ (byte (Panning)));
    PTimerSwitch^.SetPos (Succ (TMode));
    HandleSTimerSw(Msg);
  END;
END;

PROCEDURE setup.handleDevice;
VAR hdcaps:twaveoutcaps;
    OldDev:Integer;
    OldPreAmp: byte;
    OldSetting:tSettings;
    S:STRING;
    Code:Integer;
BEGIN
  OldSetting:=CurrSettings;
  WITH currsettings DO
  BEGIN
    OldPreAmp := PreAmp;
    OldDev:=DeviceID;
    DeviceID:=bname^.GetPos;
    IF DeviceID=DACID THEN DeviceID:=dev_DAC;
    IF (NumDevs<>0) AND ((DeviceID IN [dev_DAC+1..NumDevs-1]) or (DeviceID = dev_DAC)) THEN
    BEGIN
      ptimerswitch^.validatepos(1);
      ptimerswitch^.validatepos(2);
      ptimerswitch^.validatepos(3);
      IF OldDev<>DeviceID THEN
      BEGIN
        bSR^.SetLimits (-96, -1);
        bnBlocks^.SetLimits (-99, -2);
        s[0]:=char(GetPrivateProfileString(s_sampling,'SampleRate','48000',@s[1],255,INIDIR)); Val(S,SampleRate,code);
        Stereo     := GetPrivateProfileInt(s_sampling,'Channels',           DefDACSetting.Stereo,    INIDIR);
        Surround   := GetPrivateProfileInt(s_sampling,'Surround',     byte (DefDACSetting.Surround), INIDIR) <> 0;
        Bits       := GetPrivateProfileInt(s_sampling,'BitsPerSample',      DefDACSetting.Bits,      INIDIR);
        nBuffers   := GetPrivateProfileInt(s_sampling,'OutputBuffers',      DefDACSetting.nBuffers,  INIDIR);
        PreAmp     := GetPrivateProfileInt(s_sampling,'Clipping',           DefDACSetting.PreAmp SHR 7,INIDIR) SHL 7;
        OverSamp   := GetPrivateProfileInt(s_sampling,'OverSampling', byte (DefDACSetting.OverSamp), INIDIR);
        Panning    := GetPrivateProfileInt(s_sampling,'EnablePanning',byte (DefDACSetting.Panning),  INIDIR) <> 0;
        TMode      := GetPrivateProfileInt(s_sampling,'TimerMode',          DefDACSetting.TMode,     INIDIR);
        IF NOT(OverSamp IN [0..3]) THEN OverSamp:=1;
      END;
      waveoutgetdevcaps(DeviceID,@hdcaps,SizeOf(hdcaps));
      Name := StrPas (hdcaps.szpname);
    END ELSE IF DeviceID = NumDevs THEN
    BEGIN
      DeviceID:=dev_HDD;
      HdCaps.dwFormats:=$0FFF;
      hdCaps.wCHannels:=2;
      ptimerswitch^.validatepos(1);
      ptimerswitch^.invalidatepos(2);
      ptimerswitch^.invalidatepos(3);
      IF OldDev<>DeviceID THEN
      BEGIN
        bSR^.SetLimits (-96, -1);
        bnBlocks^.SetLimits (-99, -1);
        CurrSettings := DefHDDSetting;
        nBuffers   := GetPrivateProfileInt(s_sampling,'HDDRecBuffers',      DefHDDSetting.nBuffers,  INIDIR);
      END;
    END;
    devname^.setbmptext(Name);
    PreAmp := PreAmp AND $80 OR OldPreAmp AND $7F;
  END;
  WITH bms^ DO IF currsettings.DeviceID >= dev_HDD THEN
  BEGIN
    validatepos(1);
    IF ((hdcaps.dwformats AND $0AAA)>0)AND(hdcaps.wChannels>1) THEN
    BEGIN
      validatepos(2);
      validatepos(3);
    END ELSE
    BEGIN
      invalidatepos(2);
      invalidatepos(3);
      Currsettings.stereo:=1;
      Currsettings.surround:=false;
    END
  END ELSE
  BEGIN
    ValidatePos(2);
    InvalidatePos (1);
    InvalidatePos (3);
  END;
  WITH bbits^ DO IF currsettings.DeviceID >= dev_HDD THEN
  BEGIN
    validatepos(1);
    IF (hdcaps.dwformats AND $0CCC)>0 THEN
    BEGIN
      validatepos(2);
    END ELSE
    BEGIN
      invalidatepos(2);
      CurrSettings.Bits:=8;
    END
  END ELSE
  BEGIN
    ValidatePos (2);
    InvalidatePos (1);
  END;
  WHILE NOT mod_CanPlay(currsettings, OvrRateChng) DO WITH currsettings DO
  BEGIN
    IF bits=16 THEN bits:=8 ELSE
    IF stereo=2 THEN BEGIN stereo:=1; Surround:=false; END
    ELSE samplerate:=1000*(samplerate DIV 1000-1);
    IF SampleRate<1000 THEN
    BEGIN
      Code:=bname^.GetPos;
      IF (OldSetting.DeviceID>CurrSettings.DeviceID)
      THEN BEGIN bname^.SetPos(pred(Code)); IF pred(Code)<>bname^.GetPos THEN bname^.SetPos(succ(Code)) END
      ELSE BEGIN bname^.SetPos(succ(Code)); IF succ(Code)<>bname^.GetPos THEN bname^.SetPos(pred(Code));END;
      CurrSettings:=OldSetting;
      HandleDevice(Msg);
      Exit;
    END;
  END;
  IF CurrSettings.DeviceID >= dev_Hdd THEN
  BEGIN
    bPAmp^.ValidatePos (2);
    bIDO^.ValidatePos (3);
    bIDO^.ValidatePos (2);
    bPAN^.ValidatePos (1);
  END ELSE
  BEGIN
    bPAmp^.InvalidatePos (2);
    bIDO^.InvalidatePos (3);
    bIDO^.InvalidatePos (2);
    bPAN^.InvalidatePos (1);
  END;
  SetButtons;
END;

PROCEDURE setup.handleBlocks;
BEGIN
  currsettings.nBuffers:=-bnBlocks^.GetPos;
  SetButtons;
END;

PROCEDURE setup.handleSR;
VAR OldSettings:tSettings;
BEGIN
  oldsettings:=currsettings;
  currsettings.samplerate:=1000 * Longint(-bsr^.GetPos);
  WHILE NOT mod_CanPlay(currsettings, OvrRateChng) DO WITH currsettings DO
  BEGIN
    IF oldsettings.samplerate>=samplerate THEN
    BEGIN
      samplerate:=1000*(samplerate DIV 1000-1);
      IF samplerate<1000 THEN
      BEGIN
        IF bits=16 THEN bits:=8 ELSE
        IF stereo=2 THEN BEGIN stereo:=1; Surround:=false; END ELSE
        BEGIN
          CurrSettings:=OldSettings;
          Break;
        END;
        Samplerate:=1000 * Longint(-bsr^.GetPos);
      END;
    END ELSE
    BEGIN
      samplerate:=1000*(samplerate DIV 1000+1);
      IF samplerate>96000 THEN
      BEGIN
        IF bits=16 THEN bits:=8 ELSE
        IF stereo=2 THEN BEGIN stereo:=1; Surround:=false; END ELSE
        BEGIN
          CurrSettings:=OldSettings;
          Break;
        END;
        Samplerate:=1000 * Longint(-bsr^.GetPos);
      END;
    END;
  END;
  setbuttons;
END;

PROCEDURE setup.handleBits;
BEGIN
  WITH CurrSettings DO
  BEGIN
    CASE bbits^.GetPos OF
     1:Bits:= 8;
     2:Bits:=16;
    END;
    WHILE NOT mod_CanPlay(currsettings, OvrRateChng) DO
      IF stereo=2 THEN BEGIN stereo:=1; Surround:=false; END
      ELSE samplerate:=1000*(samplerate DIV 1000-1);
    setbuttons;
  END;
END;

PROCEDURE setup.handleMS;
BEGIN
  WITH CurrSettings DO
  BEGIN
    stereo:=byte(bms^.GetPos);
    IF stereo>2 THEN BEGIN stereo:=2; surround:=true; END ELSE surround:=false;
    WHILE NOT mod_CanPlay(currsettings, OvrRateChng) DO
      IF bits=16 THEN bits:=8 ELSE samplerate:=1000*(samplerate DIV 1000-1);
    IF Stereo=1 THEN Surround:=false;
    setbuttons;
  END;
END;

PROCEDURE setup.handleStimersw;
BEGIN
  currsettings.tmode:=ptimerswitch^.GetPos-1;
  whatsel[1]^.setstringtext(getresstring(2023+currsettings.tmode));
END;
PROCEDURE setup.handleSratesw;
BEGIN
 OvrRateChng:=pratechange^.GetPos=2;
 IF OvrRateChng THEN whatsel[2]^.setstringtext(getresstring(2026))
                ELSE whatsel[2]^.setstringtext(getresstring(2027));
END;
PROCEDURE setup.handleSpausesw;
BEGIN
 pausesetup:=ppausesetup^.GetPos=2;
 IF pausesetup THEN whatsel[3]^.setstringtext(getresstring(2026))
               ELSE whatsel[3]^.setstringtext(getresstring(2027));
END;
PROCEDURE setup.handleSloadsw;
BEGIN
 playonload:=pplayonload^.GetPos=2;
 IF playonload THEN whatsel[4]^.setstringtext(getresstring(2026))
               ELSE whatsel[4]^.setstringtext(getresstring(2027));
END;
PROCEDURE setup.handleAskHD;
BEGIN
 AskHD:=pAskHD^.GetPos=2;
 IF AskHD THEN whatsel[6]^.setstringtext(getresstring(2026))
          ELSE whatsel[6]^.setstringtext(getresstring(2027));
END;

PROCEDURE setup.handleSmovesw;
VAR i: byte;
BEGIN
 i := pmoveonclick^.GetPos;
 CASE i OF
   1: BEGIN moveonclick := false; fullmove := false; END;
   2: BEGIN moveonclick := true;  fullmove := false; END;
   3: BEGIN moveonclick := true;  fullmove := true;  END;
 END;
 whatsel[5]^.setstringtext(getresstring(2055+i));
END;

PROCEDURE setup.handleslang;
BEGIN
  CurrLanguage:=GetNextLangID;
  sLang^.SetStringText(LangStr[Currlanguage,_name]);
END;

PROCEDURE setup.handlehotkeys; BEGIN Application^.MakeWindow(New(pkeysel,Init(@self))); END;
PROCEDURE setup.handlepacker; BEGIN  Application^.MakeWindow(New(parcopt,Init(@self))); END;
PROCEDURE setup.handleregister; BEGIN Application^.MakeWindow(New(PRegWin,Init(@self,true))); END;
PROCEDURE setup.handlemixbrowse; BEGIN Application^.MakeWindow(New(p_browse,Init(@self,MixPathEdit))); END;
PROCEDURE setup.handleAskSel; BEGIN Application^.MakeWindow(New(p_Dirbrowse,Init(@self,RecPathEdit))); END;

PROCEDURE setup.handleSHelp; BEGIN helpme(@self,id_setupdlg); END;
PROCEDURE setup.getresult; BEGIN result:=currsettings; END;
PROCEDURE setup.handleSCancel; BEGIN PostMessage(HWindow,wm_close,0,0); END;

PROCEDURE setup.getedits;
BEGIN
  MixPathEdit^.GetText(@mixerpath,SizeOf(mixerpath));
  RecPathEdit^.GetText(@HDPath,SizeOf(HDPath));
END;

PROCEDURE setup.handleSOk;
VAR PC:STRING[6];
    S:STRING;
    _SR:Longint;
    Code:Integer;
BEGIN
  IF CurrLanguage<>s_Language THEN
  BEGIN
    IF OurMessageBox(@Self,ShortVer,GetResstring(700),MB_IconQuestion OR MB_YESNO)=ID_YES THEN
    BEGIN
      Restart:=true;
      Cancel:=false;
      PostMessage(Application^.MainWindow^.HWindow,wm_close,0,0);
      Exit;
    END ELSE Exit;
  END;
  s[0]:=char(GetPrivateProfileString(s_sampling,'SampleRate','48000',@s[1],255,INIDIR)); Val(S,_SR,code);
  IF (
     (GetPrivateProfileInt(s_device,'DeviceID',32767,INIDIR)=32767) OR
     (_SR=0) OR
     (GetPrivateProfileInt(s_sampling,'BitsPerSample',128,INIDIR)=128) OR
     (GetPrivateProfileInt(s_sampling,'Channels',128,INIDIR)=128) OR
     (GetPrivateProfileInt(s_sampling,'OutputBuffers',100,INIDIR)=100)
     )  THEN handleSsave(Msg) ELSE
  BEGIN
    getedits;
    Load_state:=bloadstate^.GetPos=2;
    Currsettings.Panning:=bPAN^.GetPos=2;
    Currsettings.Oversamp:=bIDO^.GetPos-1;
    Currsettings.PreAmp:=(bPAmp^.GetPos-1)SHL 7+Currsettings.PreAmp AND $7f;
    Cancel:=false;
    PostMessage(HWindow,wm_close,0,0);
  END;
END;

PROCEDURE setup.handleSSave;
VAR hds:STRING[50];
BEGIN
  getedits;
  Load_state:=bloadstate^.GetPos=2;
  Currsettings.Panning:=bPAN^.GetPos=2;
  Currsettings.Oversamp:=bIDO^.GetPos-1;
  Currsettings.PreAmp:=(bPAmp^.GetPos-1)SHL 7+Currsettings.PreAmp AND $7f;
  WITH currsettings DO
  BEGIN
    Str(deviceid,hds);   writeinistring(s_device,'DeviceID',hds);
                         writeinistring(s_device,'DeviceName',Name);
    Str(PreAmp AND $7F,hds);     writeinistring(s_sampling,'PreAmplification',hds);
    IF DeviceID >= dev_DAC THEN BEGIN
      Str(Samplerate,hds);       writeinistring(s_sampling,'SampleRate',hds);
      Str(Bits,hds);             writeinistring(s_sampling,'BitsPerSample',hds);
      Str(stereo,hds);           writeinistring(s_sampling,'Channels',hds);
      Str(nBuffers,hds);         writeinistring(s_sampling,'OutputBuffers',hds);
      Str(byte(tmode),hds);      writeinistring(s_sampling,'TimerMode',hds);
      Str(PreAmp SHR 7,hds);     writeinistring(s_sampling,'Clipping',hds);
      Str(byte(OverSamp),hds);   writeinistring(s_sampling,'OverSampling',hds);
      Str(byte(Panning),hds);    writeinistring(s_sampling,'EnablePanning',hds);
    END ELSE IF DeviceID = dev_HDD THEN BEGIN
      Str(nBuffers,hds);         writeinistring(s_sampling,'HDDRecBuffers',hds);
    END;
    Str(byte(surround),hds);     writeinistring(s_sampling,'Surround',hds);
    Str(byte(OvrRateChng),hds);  writeinistring(s_state,'OverrideRateChange'+#0,hds);
    Str(byte(PauseSetup),hds);   writeinistring(s_state,'PauseWhileSetup'+#0,hds);
    Str(byte(PlayOnLoad),hds);   writeinistring(s_state,'PlayOnLoad'+#0,hds);
    Str(byte(MoveOnClick),hds);  writeinistring(s_state,'MoveOnClick'+#0,hds);
    Str(byte(FullMove),hds);     writeinistring(s_state,'FullWindowMove'+#0,hds);
    Str(byte(AskHD),hds);        writeinistring(s_state,'AskHD'+#0,hds);
    Str(byte(Load_state),hds);   writeinistring(s_state,'StateSaver'+#0,hds);
    Str(byte(showpack),hds);     writeinistring(s_packer,'ShowWindow'+#0,hds);
    Str(Byte(Currlanguage),hds); writeinistring('Language','Language'+#0,hds);

    savekeys('ModMainHotkeys',1,MAINHOTKEYS_BIS);
    writeinistring(s_state,'MixerPath'+#0,strpas(@mixerpath));
    writeinistring(s_state,'HDPath'+#0,strpas(@HDPath));
    writeinistring(s_packer,'ARJ_P'+#0,arcpackexecs[arjfiletype]);
    writeinistring(s_packer,'ARJ_UP'+#0,arcunpackexecs[arjfiletype]);
    writeinistring(s_packer,'ZIP_P'+#0,arcpackexecs[zipfiletype]);
    writeinistring(s_packer,'ZIP_UP'+#0,arcunpackexecs[zipfiletype]);
    writeinistring(s_packer,'LHA_P'+#0,arcpackexecs[lharcfiletype]);
    writeinistring(s_packer,'LHA_UP'+#0,arcunpackexecs[lharcfiletype]);
    {ARJPACKKOMMANDOS}
    writeinistring('ARJPACKERCMDS','UNPACK'+#0,Arcextractstring[arjfiletype]);
    writeinistring('ARJPACKERCMDS','PACK'+#0  ,ArcCOPYstring[arjfiletype]);
    writeinistring('ARJPACKERCMDS','DELETE'+#0,Arcdelstring[arjfiletype]);
    {zipPACKKOMMANDOS}
    writeinistring('ZIPPACKERCMDS','UNPACK'+#0,Arcextractstring[zipfiletype]);
    writeinistring('ZIPPACKERCMDS','PACK'+#0  ,ArcCOPYstring[zipfiletype]);
    writeinistring('ZIPPACKERCMDS','DELETE'+#0,Arcdelstring[zipfiletype]);
    {LHAPACKKOMMANDOS}
    writeinistring('LHAPACKERCMDS','UNPACK'+#0,Arcextractstring[lharcfiletype]);
    writeinistring('LHAPACKERCMDS','PACK'+#0  ,ArcCOPYstring[lharcfiletype]);
    writeinistring('LHAPACKERCMDS','DELETE'+#0,Arcdelstring[lharcfiletype]);
    IF CurrLanguage<>s_Language THEN
    BEGIN
      IF OurMessageBox(@Self,ShortVer,GetResstring(700),MB_IconQuestion OR MB_YESNO)=ID_YES THEN
      BEGIN
        Restart:=true;
        Cancel:=false;
        PostMessage(Application^.MainWindow^.HWindow,wm_close,0,0);
        Exit;
      END ELSE Exit;
    END;
  END;
  Cancel:=false;
  PostMessage(HWindow,wm_close,0,0);
END;

PROCEDURE Setup.wmkeydown;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    $53 {'S'ave}        :handlessave(Msg);
    vk_return,vk_execute:handlesok(Msg);
    vk_f1,vk_help       :handleshelp(Msg);
    vk_escape           :handlescancel(Msg);
    vk_left             :BEGIN bName^.setpos(bName^.GetPos-1); handleDevice(Msg); END;
    vk_right            :BEGIN bName^.setpos(bName^.GetPos+1); handleDevice(Msg); END;
    vk_up               :CASE keystate OF
                         0:BEGIN bSR^.setpos(bSR^.GetPos-1); handleSR(Msg); END;
                         1:BEGIN bbits^.setpos(bbits^.GetPos-1); handleBits(Msg); END;
                         2:BEGIN bMS^.setpos(bMS^.GetPos-1); handleMS(Msg); END;
                         3:BEGIN bnBlocks^.setpos(bnBlocks^.GetPos-1); handleBlocks(Msg); END;
                         END;
    vk_down             :CASE keystate OF
                         0:BEGIN bSR^.setpos(bSR^.GetPos+1); handleSR(Msg); END;
                         1:BEGIN bbits^.setpos(bbits^.GetPos+1); handleBits(Msg); END;
                         2:BEGIN bMS^.setpos(bMS^.GetPos+1); handleMS(Msg); END;
                         3:BEGIN bnBlocks^.setpos(bnBlocks^.GetPos+1); handleBlocks(Msg); END;
                         END;
    ELSE DefWndProc(msg);
  END;
END;

PROCEDURE setup.wmctlcolor;
BEGIN
  IF GetDlgCtrlID(msg.lparamlo)=1 THEN msg.lparamhi:=ctlcolor_edit;
  INHERITED wmctlcolor(msg);
END;

PROCEDURE setup.wmsyschar;
VAR ch: char;
BEGIN
  ch := UpCase (char(Lo(msg.wparam)));
  CASE ch OF
    'M':MixPathEdit^.focus;
    'D':RecPathEdit^.focus;
    'A':handlepacker(Msg);
    'H':handlehotkeys(Msg);
    'R':handleregister(Msg);
    'L':handleSLang(Msg);
  ELSE
    IF ch = syschar [2050] THEN ptimerswitch^.focus ELSE
    IF ch = syschar [2051] THEN pratechange^.focus  ELSE
    IF ch = syschar [2052] THEN ppausesetup^.focus  ELSE
    IF ch = syschar [2053] THEN pplayonload^.focus  ELSE
    IF ch = syschar [2054] THEN pmoveonclick^.focus ELSE
    IF ch = syschar [2055] THEN pAskHD^.focus ELSE
      BEGIN INHERITED wmsyschar(msg); Exit; END;
  END;
  msg.result:=-1;
END;

PROCEDURE setup.wmendbrowse;
BEGIN
  getedits;
  msg.result:=0;
END;

{****ENDE***************************************MAIN-Setup***********************************}
CONST y_tmore=130;
      x_tmore=336;


CONSTRUCTOR tmorearc.Init;
VAR i:word;
    pc:pchar;
    ts: STRING[50];
BEGIN
  arctype:=arcindex;
  Cancel:=true;
  pc:=MemAlloc(200);
  strcopy(pc,modc.shortver);
  IF arctype=arjfiletype THEN strcat(pc,'ARJ') ELSE
  IF arctype=zipfiletype THEN strcat(pc,'ZIP') ELSE  strcat(pc,'LHA');
  strcat(pc,' Parameter');
  INHERITED Init (AParent, pc,ws_sysmenu,notopmost);
  FreeMem(pc,200);
  s_arccmds[1]:=arcextractstring[arctype];
  s_arccmds[2]:=arccopystring[arctype];
  s_arccmds[3]:=arcdelstring[arctype];
  Attr.style:=ws_popup OR ws_visible OR ws_border;
  Attr.x :=pwindow(Parent)^.Attr.x+(x_setup DIV 2)-(x_tmore DIV 2);
  Attr.y :=pwindow(Parent)^.Attr.y+(y_setup DIV 2)-(y_tmore DIV 2);
  Attr.w:=X_tmore+GetSystemMetrics(sm_cxborder);
  Attr.h:=Y_tmore+capdy+(2*GetSystemMetrics(sm_cyborder));
  comments[1]:=New(pmystatic,Init(@self,0,'',16,CapDY+18,11*Fw,Fh,11));
  comments[2]:=New(pmystatic,Init(@self,0,'',16,CapDY+43,11*Fw,Fh,11));
  comments[3]:=New(pmystatic,Init(@self,0,'',16,CapDY+68,11*Fw,Fh,11));
  up_edit:=New(pmyudedit,Init(@self,id_arjpath,'',24+12*fw,CapDY+18,25*fw,Fh,30,true));
  p_edit:=New(pmyudedit,Init(@self,id_zippath,'',24+12*fw,CapDY+43,25*fw,Fh,30,true));
  del_edit:=New(pmyudedit,Init(@self,id_lhapath,'',24+12*fw,CapDY+68,25*fw,Fh,30,true));
  pOk:=     New(PbmpButton, Init(@self, id_bsok,162,CapDY+100,'bsok',false));
  pCancel:= New(PbmpButton, Init(@self, id_bscancel,203,CapDY+100,'bscancel',false));
  pdefault:=New(PbmpButton, Init(@self, id_bdefault,244,CapDY+100,'b_default',false));
  phelp:=   New(PbmpButton, Init(@self, id_bshelp,285,CapDY+100,'bshelp',false));
  FOR i := 2070 TO 2072 DO BEGIN ts := getresstring(i); syschar[i] := UpCase (ts[Pos ('&', ts) + 1]); END;
END;

DESTRUCTOR tmorearc.Done;
BEGIN
  IF Cancel THEN
  BEGIN
    arcextractstring[arctype]:=s_arccmds[1];
    arccopystring[arctype]:=s_arccmds[2];
    arcdelstring[arctype]:=s_arccmds[3];
  END;
  Parent^.enable;
  INHERITED Done;
END;

PROCEDURE tmorearc.SetupWindow;
VAR  pc:pchar;
BEGIN
  INHERITED SetupWindow;
  Parent^.disable;
  comments[1]^.setstringtext(getresstring(2070));
  comments[1]^.setassocwindow(up_edit^.HWindow,0);
  comments[2]^.setstringtext(getresstring(2071));
  comments[2]^.setassocwindow(p_edit^.HWindow,0);
  comments[3]^.setstringtext(getresstring(2072));
  comments[3]^.setassocwindow(del_edit^.HWindow,0);
  phelp^.enable; pdefault^.enable; pok^.enable; pcancel^.enable;
  pc:=MemAlloc(200);
  strpcopy(pc,arcextractstring[arctype]); up_edit^.SetText(pc);
  strpcopy(pc,arccopystring[arctype]); p_edit^.SetText(pc);
  strpcopy(pc,arcdelstring[arctype]); del_edit^.SetText(pc);
  FreeMem(pc,200);
  oldfocus:=pok^.HWindow;
END;

PROCEDURE tmorearc.wmEraseBkGnd(VAR msg:tmessage);
VAR i:integer;
    rect:trect;
BEGIN
  paintcaption(msg.wParam);
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  framefilled3d(msg.wParam,0,CapDY,x_tmore,y_tmore,2,cForeGnd,up); {Hauptrahmen}
  WITH rect DO BEGIN left:=7; top:=CapDY+7; right:=328; bottom:=top+85; END;
  FrameRect(msg.wParam,rect,BrBlack);
  frame3d(msg.wParam,pok^.Attr.x-1,pok^.Attr.y-1,167,23,1,down);
  WITH up_edit^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,2,down);
  WITH p_edit^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,2,down);
  WITH del_edit^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,2,down);
  FOR i:=1 TO 3 DO  WITH comments[i]^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  blit(HWindow,35,pok^.Attr.y,94,20,0,0,m_label);
  INHERITED wmEraseBkGnd(msg);
END;

PROCEDURE tmorearc.handleOk;
BEGIN
  Cancel:=false;
  getedits;
  PostMessage(HWindow,wm_close,0,0);
END;
PROCEDURE tmorearc.handleHelp; BEGIN helpme(@self,id_arcparmdlg); END;
PROCEDURE tmorearc.handlecancel; BEGIN PostMessage(HWindow,wm_close,0,0); END;

PROCEDURE tmorearc.handledefault;
VAR pc:pchar;
BEGIN
  pc:=MemAlloc(50);
  strpcopy(pc,defextractstring[arctype]); up_edit^.SetText(pc);
  strpcopy(pc,defarccopystring[arctype]); p_edit^.SetText(pc);
  strpcopy(pc,defarcdelstring[arctype]); del_edit^.SetText(pc);
  arcextractstring[arctype]:=defextractstring[arctype];
  arccopystring[arctype]:=defarccopystring[arctype];
  arcdelstring[arctype]:=defarcdelstring[arctype];
  FreeMem(pc,50);
END;
PROCEDURE tmorearc.getedits;
BEGIN
  arcextractstring[arctype][0]:=char(byte(up_edit^.GetText(@arcextractstring[arctype][1],30)));
  arccopystring[arctype][0]:=char(byte(p_edit^.GetText(@arccopystring[arctype][1],30)));
  arcdelstring[arctype][0]:=char(byte(del_edit^.GetText(@arcdelstring[arctype][1],30)));
END;

PROCEDURE tmorearc.wmkeydown;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    word('D'),word('d') :handledefault(Msg);
    vk_f1,vk_help       :handlehelp(Msg);
    vk_escape           :handlecancel(Msg);
    vk_return,vk_execute:handleok(Msg);
    ELSE DefWndProc(msg);
  END;
END;

PROCEDURE tmorearc.wmsyschar;
VAR ch: char;
BEGIN
  ch := UpCase (char(Lo(msg.wparam)));
  IF ch = syschar [2070] THEN up_edit^.focus  ELSE
  IF ch = syschar [2071] THEN p_edit^.focus   ELSE
  IF ch = syschar [2072] THEN del_edit^.focus ELSE
    BEGIN INHERITED wmsyschar(msg); Exit; END;
  msg.result:=-1;
END;

BEGIN
END.