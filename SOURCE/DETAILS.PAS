UNIT details;
{ $C Fixed Preload Permanent}
INTERFACE
USES owindows,winprocs,wintypes,strings,omemory,winunit,modc,win31;

VAR  detailwnd:hwnd;

CONST MaxInstChars:Byte=22; {Maximale Länge der Instrumenten-Strings...}
     y_starttext =39;
     y_modname   =7;
     id_Modname  =701;
     id_nInstr   =702;
     id_NPatt    =703;
     id_instr    =720;
     id_plus=750;
     id_minus=751;
     id_genbox=752;

TYPE psongbox=^tsongbox;
     tsongbox=object(tmylistbox)
       PROCEDURE wmkeydown(VAR msg:tmessage);     VIRTUAL wm_first+wm_keydown;
       PROCEDURE wmsetfocus(VAR msg:tmessage);    VIRTUAL wm_first+wm_setfocus;
     END;
TYPE pinsinfo=^tinsinfo;
     tinsinfo=object(tmywindow)
       index:byte;
       infstruct:pmodulinfo;
       _digits:pbmplcd;
       bplus,bminus:pbmpbutton;
       insname:pmycenterstatic;
       CONSTRUCTOR Init(Aparent:pwindowsobject; ix,iy:integer; ainfostruct:pmodulinfo; instrindex:byte);
       DESTRUCTOR Done; virtual;
       PROCEDURE SetupWindow;                       VIRTUAL;
       PROCEDURE wmerasebkgnd(VAR msg:tmessage);    VIRTUAL wm_first+wm_EraseBkGnd;
       PROCEDURE PAINTDATA(adc:hdc);                VIRTUAL;
       PROCEDURE wmkeydown(VAR msg:tmessage);       VIRTUAL  wm_first+wm_keydown;
       PROCEDURE handleplus(VAR msg:tmessage);      VIRTUAL  id_first+id_plus;
       PROCEDURE handleminus(VAR msg:tmessage);     VIRTUAL  id_first+id_minus;
     END;

TYPE pdetail=^detail;
     detail=object(tmychildwindow)
       CenterLines,AnsiLines:Word; {Anzahl der zentriert auszugebende Zeilen}
       CommentW,CommentH,CTop,Cleft:word;
       mname:pbmpfont;
       info:pmodulinfo;
       insinfo:pinsinfo;
       genbox:psongbox;
       bplus,bminus:pbmpbutton;
       index:byte;
       wmlrects:ARRAY[1..2] OF TRect;
       x_insinfo,y_insinfo:integer;
       x_lotext,x_hitext,x_detail,x_modname,y_detail,x_info,y_info:integer;
       PCursor,Norm_Cursor:HCursor;
       LB_RIGHTSTARTPOS:Integer;
       CONSTRUCTOR Init(aparent:pwindowsobject; struktur:pmodulinfo);
       DESTRUCTOR Done;                            VIRTUAL;
       PROCEDURE SetupWindow;                      VIRTUAL;
       PROCEDURE UpdateWindow(VAR msg:tmessage);   VIRTUAL wm_first+wm_newfiletoplay;
       PROCEDURE updategenbox;                     VIRTUAL;
       PROCEDURE handleplus(VAR msg:tmessage);     VIRTUAL id_first+id_plus;
       PROCEDURE handleminus(VAR msg:tmessage);    VIRTUAL id_first+id_minus;
       PROCEDURE wmerasebkgnd(VAR msg:tmessage);   VIRTUAL wm_first+wm_EraseBkGnd;
       PROCEDURE PAINTINSTR(adc:hdc; all:boolean); VIRTUAL;
       PROCEDURE wmkeydown(VAR msg:tmessage);      VIRTUAL wm_first+wm_keydown;
       PROCEDURE WMLButtonDown(VAR msg:tmessage);  VIRTUAL wm_first+wm_lbuttondown;
       PROCEDURE WMMouseMove(VAR Msg: TMessage);   VIRTUAL wm_First+wm_MouseMove;
       PROCEDURE wmdrawitem(VAR msg:tmessage);     virtual wm_first+wm_drawitem;
     END;

IMPLEMENTATION
VAR   LastInstrument:Byte;
{********************GENINFO-LB****************************}
PROCEDURE tsongbox.wmkeydown;
BEGIN
  msg.result:=SendMessage(Parent^.HWindow,wm_keydown,msg.wparam,msg.lparam);
END;
PROCEDURE tsongbox.wmsetfocus;
BEGIN
  SetFocus(GetNextDlgTabItem(Parent^.HWindow,HWindow,keystate=2));
  msg.result:=0;
END;

{**************************DETAIL**************************}
CONSTRUCTOR detail.Init;
VAR xname,nname:integer;
    i,numfh:integer;
BEGIN
  INHERITED Init (AParent, '',ws_sysmenu,notopmost);
  CommentW:=0;
  CommentH:=0;
  info:=struktur;
  numfh:=3;
  CenterLines:=3;
  x_detail :=Fw*2*(4+MaxInstChars)+34;
  y_detail :=78+(Fh+2)*15+(Fh+1)*(numfh);
  x_lotext :=7;
  x_hitext :=(x_detail DIV 2)+2;
  x_modname:=7;
  x_info   :=7;
  y_info:=y_detail-numfh*(Fh+1)-13;
  WITH Attr DO BEGIN
    style:=ws_popup OR ws_visible OR ws_border;
    w :=X_detail+GetSystemMetrics(sm_cxborder)+1;
    h :=Y_detail+capdy+(2*GetSystemMetrics(sm_cyborder));
    x :=GetPrivateProfileInt(s_state,'XDetail',200,INIDIR);
    y :=GetPrivateProfileInt(s_state,'YDetail',200,INIDIR);
    setcorrectwinpos(x,y,w,h);
  END;
  nname:=(x_detail-16)DIV 16;
  xname:=x_detail DIV 2-8*nname;
  mname:=New(pbmpfont,Init(@self,id_modname,xname,CapDY+y_modname+4,nname,2,'font_16x20',64));
  bminus:=New(pbmpbutton,Init(@self,id_minus,X_lotext+2,CapDY+Y_starttext+2,'bominus',true));
  bplus:=New(pbmpbutton,Init(@self,id_plus  ,(x_detail DIV 2)-18,CapDY+Y_starttext+2,'boplus',true));
  genbox :=New(psongbox,Init(@self,id_genbox,x_info+2,CapDY+y_info+2,x_detail-20,(fh+1)*numfh+1,ws_hscroll,false));
  genbox^.Attr.style:=genbox^.Attr.style AND NOT lbs_disablenoscroll;
  pCursor:=LoadCursor(hinstance,'PointCursor');
  Norm_Cursor:=LoadCursor(0,idc_arrow);
  insinfo:=NIL;
  index:=1;
  WITH wmlrects[1] DO
  BEGIN
    left:=x_lotext; Right:=left+(x_detail DIV 2)-9;
    Top:=CapDY+y_starttext+3+fh+2; Bottom:=Top+15*(Fh+2);
  END;
  WITH wmlrects[2] DO
  BEGIN
    left:=x_hitext; Right:=left+(x_detail DIV 2)-9;
    Top:=CapDY+y_starttext+3; Bottom:=Top+16*(Fh+2);
  END;
  x_insinfo:=GetPrivateProfileInt(s_state,'InsInfo_X',Attr.x+(Attr.w DIV 2)-((11*fw)+10),INIDIR);
  y_insinfo:=GetPrivateProfileInt(s_state,'InsInfo_Y',Attr.y+(Attr.h DIV 2)-65,INIDIR);
END;

PROCEDURE detail.SetupWindow;
VAR i:word;
    pc:pchar;
    msg:tmessage;
BEGIN
  Msg.wParam:=$31;
  i:=GetPrivateProfileInt(s_state,'InstrOn',0,INIDIR);
  IF i>0 THEN wmkeydown(MSg);
  INHERITED SetupWindow;
  detailwnd:=HWindow;
  pc:=MemAlloc(200);
  strcopy(pc,longver);
  strcat(pc,' Module Info');
  SetCaption(pc);
  FreeMem(pc,200);
  UpdateWindow(msg);
  mname^.enable;
  bplus^.enable;
  bminus^.enable;
  updategenbox;
  oldfocus:=bplus^.HWindow;
END;

PROCEDURE detail.wmerasebkgnd(VAR msg:tmessage);
VAR i:integer;
    rgn:hrgn;
    nname,xname:integer;
BEGIN
  IF PaletteInstalled THEN SelectPalette(msg.wParam,hpal,false);
  paintcaption(msg.wParam);
  SetTextAlign(msg.wParam,ta_left OR ta_top OR ta_noupdatecp);
  SelectObject(msg.wParam,OEMhandle);
  rgn:=CreateRectRgn(0,CapDY,x_detail,CapDY+y_detail+2);
  SelectClipRgn(msg.wParam,rgn);
  ExcludeClipRect(msg.wParam,x_lotext,CapDY+y_starttext,x_lotext+(x_detail DIV 2)-9,y_starttext+16*(Fh+2)+6);
  ExcludeClipRect(msg.wParam,x_hitext,CapDY+y_starttext,x_hitext+(x_detail DIV 2)-9,y_starttext+16*(Fh+2)+6);
  ExcludeClipRect(msg.wParam,x_info,CapDY+y_info,x_info+genbox^.Attr.w+4,y_info+genbox^.Attr.h+4);
  ExcludeClipRect(msg.wParam,x_modname,CapDY+y_modname,x_detail-7,CapDY+y_modname+28);
  framefilled3d(msg.wParam,0,CapDY,x_detail,y_detail,3,cForeGnd,up);
  DeleteObject(rgn);
  rgn:=CreateRectRgn(x_modname,CapDY+y_modname,x_detail-7,CapDY+y_detail-4);
  SelectClipRgn(msg.wParam,rgn);
  nname:=(x_detail-16)DIV 16;
  xname:=x_detail DIV 2-8*nname;
  ExcludeClipRect(msg.wParam,xname,CapDY+y_modname+4,xname+nname*16,CapDY+y_modname+24);
  framefilled3d(msg.wParam,x_modname,CapDY+y_modname,x_detail-14,28,2,cBackGnd,down);
  framefilled3d(msg.wParam,x_lotext,CapDY+y_starttext,(x_detail DIV 2)-9,16*(Fh+2)+6,2,cBackGnd,down);
  framefilled3d(msg.wParam,x_hitext,CapDY+y_starttext,(x_detail DIV 2)-9,16*(Fh+2)+6,2,cBackGnd,down);
  paintinstr(msg.wParam,true);
  DeleteObject(rgn);
  INHERITED WMEraseBkGnd(msg);
END;

PROCEDURE detail.PAINTINSTR(adc:hdc; all:boolean);
VAR oldfont:thandle;
    s:STRING[40];
    pdc:hdc;
    i:word;
    pa,oldpen:hpen;
    rect:trect;
BEGIN
  IF ADc=0 THEN PDC:=GetDC(HWindow) ELSE pdc:=adc;
  IF PaletteInstalled THEN SelectPalette(pdc,hpal,false);
  oldfont:=SelectObject(pdc,OEMhandle);
  SetTextColor(pdc,cVGLB);
  SetBkColor(pdc,cHGLB);
  SetBkMode(pdc,transparent);
  pa:=CreatePen(ps_solid,1,cDown);
  oldpen:=SelectObject(pdc,pa);
  SetTextAlign(pdc,ta_center OR ta_top OR ta_noupdatecp);
  TextOut(pdc,(x_lotext+x_hitext) DIV 2,CapDY+y_starttext+3,'Instruments',11);
  SetTextAlign(pdc,ta_left OR ta_top OR ta_noupdatecp);
  SetBkMode(pdc,opaque);
  rect.left:=x_lotext+4;
  rect.right:=rect.left+(4+MaxInstChars)*fw;
  FOR i:=0 TO 30 DO
  BEGIN
    IF (i+index)>LastInstrument
    THEN BEGIN Str(((i+index)-LastInstrument):3,s); s:=s+' '+info^.instruments[(i+index)-LastInstrument].Name; END
    ELSE BEGIN Str(  i+index      :3,s); s:=s+' '+info^.instruments[ i+index     ].Name; END;
    rect.top:=CapDY+y_starttext+3+((i+1) MOD 16)*(Fh+2);
    rect.bottom:=rect.top+fh+2;
    ExtTextOut(pdc,rect.left,rect.top,eto_opaque,@rect,@s[1],Length(s),NIL);
    MoveTo(pdc,rect.left-2,rect.top-1);
    LineTo(pdc,rect.right+2,rect.top-1);
    IF I=14 THEN
    BEGIN
      rect.left:=x_HiText+4;
      rect.right:=rect.left+(4+MaxInstChars)*fw;
    END;
  END;
  MoveTo(pdc,x_lotext+2+3*FW+FW DIV 2,CapDY+y_starttext+5+FH);
  LineTo(pdc,x_lotext+2+3*FW+FW DIV 2,CapDY+y_starttext+4+16*(Fh+2));
  MoveTo(pdc,x_hitext+2+3*FW+FW DIV 2,CapDY+y_starttext+3);
  LineTo(pdc,x_hitext+2+3*FW+FW DIV 2,CapDY+y_starttext+4+16*(Fh+2));
  IF all THEN frame3d(pdc,x_info,CapDY+y_info,genbox^.Attr.w+4,genbox^.Attr.h+4,2,down);
  SelectObject(pdc,oldpen);
  DeleteObject(pa);
  SelectObject(pdc,oldfont);
  IF ADC=0 THEN ReleaseDC(HWindow,PDC);
END;

PROCEDURE detail.UpdateWindow;
VAR OldMaxInstChars:Byte;
    DC:word;
    PS:tPaintStruct;
    Right:byte;
    Bottom,rows,hor,vrt:Word;
    OldCw,OldCh:Word;
    wstru:TWindowPlacement;
LABEL NoComment,OutHere;
FUNCTION StringEmpty (s: STRING): boolean;
VAR i: byte;
BEGIN
  StringEmpty := true;
  FOR i := 1 TO Length (s) DO IF s[i] <> ' ' THEN BEGIN StringEmpty := false; Break; END;
END;
BEGIN
  {**********************************COMMENT***************************************************}
  OldCw:=CommentW;
  OldCH:=CommentH;
  IF Info^.Commentlen<>0 THEN WITH info^ DO
  BEGIN
    CASE FileType OF
      fFAR     :rows:=132;
      f669,Unis:rows:=36;
      MTM,DMF  :rows:=40;
      ULT      :rows:=32;
      ELSE GOTO NoComment;
    END;
    CTop:=65535; Bottom:=0; CLeft:=255; Right:=0;
    FOR DC:=0 TO CommentLen-1 DO
    BEGIN
      IF(Comment^[DC]=#0)THEN
      BEGIN
        DC :=(DC DIV rows+1) * rows-1;
        IF DC >= CommentLen THEN Break;
      END ELSE
      IF (Comment^[DC]>#32)THEN
      BEGIN
        hor:=DC MOD rows+1;
        vrt:=DC DIV rows+1;
        IF Bottom<vrt THEN Bottom:=vrt;
        IF CTop  >vrt THEN CTop  :=vrt;
        IF Right <hor THEN Right :=hor;
        IF CLeft >hor THEN CLeft :=hor;
      END;
    END;
    IF Bottom<>0 THEN
    BEGIN
      CommentW:=Right-CLeft+1;
      CommentH:=Bottom-CTop+1;
      GOTO OutHere;
    END ELSE GOTO NoComment;
  END ELSE
  BEGIN
NoComment:
    CommentW:=0;
    CommentH:=0;
OutHere:
    IF Genbox^.Attr.w>(FW*(Commentw+2))THEN rows:=0 ELSE rows:=Commentw+2;
    IF(OldCw<>CommentW)THEN SendMessage(Genbox^.HWindow,lb_sethorizontalextent,Fw*rows,0);
  END;
  {************END*******************COMMENT***************************************************}
  OldMaxInstChars:=MaxInstChars;
  MaxInstChars := 0;
  LastInstrument:=0;
  Index:=1;
  FOR DC := 1 TO 255 DO WITH Info^.Instruments[DC] DO
  BEGIN
    {if Info^.FileType in [fFar, S3M, ULT, DMF,XM] THEN{}
    IF Length(Name) > MaxInstChars THEN MaxInstChars := Length (Name);
    IF NOT StringEmpty(Name)OR(Offset<>0)OR(IEnd<>0)OR(LoopStart<>0)OR(Loopend<>0)OR
      (Volume<>0)OR(FineTune<>0)OR(Bits<>0)THEN LastInstrument:=DC;
  END;
  IF LastInstrument<31 THEN LastInstrument:=31;
  IF LastInstrument=31 THEN
  BEGIN
    bplus^.Show(sw_Hide);
    bminus^.Show(sw_Hide);
  END ELSE
  BEGIN
    bplus^.Show(sw_Show);
    bminus^.Show(sw_Show);
  END;
  IF MaxInstChars < 22 THEN MaxInstChars := 22;
  hor:=CenterLines+OldCh; IF hor>12 THEN hor:=12;
  vrt:=CommentH+3+Byte(Info^.dummypatterns<>0)+Byte((Info^.LoopFrom>0)AND(info^.Songmem>0));
  IF Commenth<>0 THEN INC(vrt,4);
  IF vrt>12 THEN vrt:=12;
  IF (OldMaxInstChars<>MaxInstChars)OR(hor<>vrt)THEN
  BEGIN
    x_detail :=Fw*2*(4+MaxInstChars)+34;
    x_hitext :=(x_detail DIV 2)+2;
    Attr.w:=X_detail+GetSystemMetrics(sm_cxborder)+1;
    MName^.n:=(x_detail-16)DIV 16;
    rows:=vrt;
    y_detail :=78+(Fh+2)*15+(Fh+1)*(rows);
    y_info   :=y_detail-rows*(Fh+1)-13;
    WITH MName^  DO
    BEGIN
      MoveWindow(HWindow,x_detail DIV 2-8*n,Attr.y,16*n,Attr.h,True);
      IF membmp<>0 THEN DeleteObject(MemBMP);
      DC:=GetDC(HWindow);
      membmp:=CreateCompatibleBitmap(DC,Attr.w,Attr.h);
      ReleaseDC(HWindow,DC);
      bmppaint;
    END;
    WITH bplus^  DO MoveWindow(HWindow,(x_detail DIV 2)-18,Attr.y,Attr.w,Attr.h,True);
    WITH genbox^ DO MoveWindow(HWindow,x_info+2,CapDY+y_info+2,x_detail-18,(fh+1)*rows+1,True);
    WITH wmlrects[1] DO Right:=left+(x_detail DIV 2)-9;
    WITH wmlrects[2] DO BEGIN left:=x_hitext; Right:=left+(x_detail DIV 2)-9; END;
    WITH Attr DO
    BEGIN
      h:=Y_detail+capdy+(2*GetSystemMetrics(sm_cyborder));
      IF NOT IsIconic(HWindow) THEN
      BEGIN
        MoveWindow(HWindow,x,y,w,h,True);
        InvalidateRect(HWindow,NIL,true);
      END ELSE
      WITH wstru DO
      BEGIN
        Length:=SizeOf(wstru);
        GetWindowPlacement(HWindow,@wstru);
        ptmaxposition.y:=Attr.y;
        ptmaxposition.x:=Attr.x;
        rcnormalposition.right:=rcnormalposition.left+w;
        rcnormalposition.bottom:=rcnormalposition.top+h;
        showcmd:=sw_showNA;
        Flags:=0;
        SetWindowPlacement(HWindow,@wstru);
      END;
    END;
    DC:=GetDC(HWindow);
    msg.wParam:=DC;
    Paint(DC,PS);
    bplus^.enable;
    bminus^.enable;
    ReleaseDC(HWindow,DC);
  END;
  paintinstr(0,true);
  mname^.setbmptext(info^.MODName);
  IF insinfo<>NIL THEN insinfo^.paintdata(0);
  updategenbox;
END;

PROCEDURE detail.updategenbox;
VAR s:STRING;
    args:ARRAY[0..16]OF word;
    i,j:integer;
    rect:trect;

  PROCEDURE sconvert(VAR s:STRING; pc:ARRAY OF char; nmax:byte); ASSEMBLER;
  {Konvertiert einen PChar(ohne Abschliessende NULL) in einen STRING unter
   Ausschluß von Steuerzeichen}
  ASM
      mov dx,ds; cld
      {CX--->Anzahl der Zeichen}
      mov cl,nmax
      mov ah,cl   { save nmax }
      XOR ch,ch
      {DS:SI--->PC[0]}
      lds si,pc
      {ES:DI--->S[1]}
      les di,s
      mov bx,di   { save DI   }
      inc di
  @start:
      lodsb
      OR  al,al   { was it 0  }
      jz  @stop   { stop it   }
      cmp al,' '
      {PC[i]>=#32?}
      jae @1
      {nein--->S[i+1]:=32}
      mov al,' '
  @1: {Ja --->S[i+1]:=pc[i]}
      stosb
      loop @start
  @stop:
      {letzte Position eines Zeichens berechnen}
      sub ah,cl
      {Anzahl der Zeichen in S[0] schreiben}
      mov al,ah
      mov di,bx   { restore   }
      stosb
      mov ds,dx
  END;

BEGIN
  IF info<>NIL THEN
  WITH info^,genbox^ DO
  BEGIN
    SendMessage(HWindow,wm_setredraw,0,0);
    ClearList;
    args[0]:=numtracks;
    s:=getresstring(byte(Filetype)+600)+#0; args[1]:=Ofs(s)+1; args[2]:=Seg(s);
    args[3]:=numinstruments;
    AddString(formatstring(3002,args));
    args[0]:=filesiz; s:=formatstring(3000,args);
    IF extrabytes>0 THEN
    BEGIN
      args[0]:=loword(extrabytes); args[1]:=hiword(extrabytes);
      s:=s+formatstring(3004,args);
    END ELSE
    IF missingbytes>0 THEN
    BEGIN
      args[0]:=loword(missingbytes); args[1]:=hiword(missingbytes);
      s:=s+formatstring(3005,args);
    END;
    IF songmem>0 THEN
    BEGIN
      args[0]:=songmem DIV 1024; s:=s+formatstring(3008,args);
    END;
    AddString(s);
    args[0]:=diffpatterns; args[1]:=numpatterns; AddString(formatstring(3006,args));
    IF integer (dummypatterns) > 0 THEN BEGIN args[0]:=+dummypatterns; AddString(formatstring(3003,args)); END ELSE
    IF integer (dummypatterns) < 0 THEN BEGIN args[0]:=-dummypatterns; AddString(formatstring(3010,args)); END;
    IF songmem>0 THEN
    BEGIN
      IF LOOPFROM>0 THEN BEGIN args[0]:=loopfrom; args[1]:=loopto; AddString(formatstring(3009,args)); END;
    END;
    CenterLines:=GetCount;
    AnsiLines:=Centerlines;
    IF (commentw>0)THEN
    BEGIN
      AddString('');
      { draw frame }
      i := Length(getresstring(byte(Filetype)+600)) + 21;
      s[0] := Chr(i); s[1] := 'Ú'; FillChar (s[2], i - 2, 'Ä'); s[i] := '·';
      AddString (s);
      s := '³ ' + getresstring(byte(Filetype)+600) + ' song information º';
      AddString (s);
      s[0] := Chr(i); s[1] := 'Ô'; FillChar (s[2], i - 2, 'Í'); s[i] := '¼';
      AddString (s);
      { line width }
      CASE FileType OF
        fFAR     : j:=132;
        f669,Unis: j:=36;
        MTM,DMF  : j:=40;
        ULT      : j:=32;
        ELSE       j:=0;
      END;
      CenterLines:=GetCount;
      FOR i:=1 TO CommentH DO
      BEGIN
        sconvert(s,comment^[j*(i-2+CTop)+Cleft-1],Commentw);
        WHILE Length(s)<CommentW DO s:=s+' ';
        AddString(s);
      END;
    END;
    setsel(0,0);
    SendMessage(genbox^.HWindow,lb_getitemrect,0,longint(@rect));
    LB_RIGHTSTARTPOS:=rect.right;
    SendMessage(HWindow,wm_setredraw,1,0);
  END;
END;

DESTRUCTOR detail.Done;
VAR i:WORD; s:STRING[10];
    xdet,ydet:integer;
BEGIN
  xdet:=Attr.x; IF xdet<0 THEN xdet:=0;
  ydet:=Attr.y; IF ydet<0 THEN ydet:=0;
  IF insinfo<>NIL THEN
  BEGIN
    x_insinfo:=insinfo^.Attr.x;
    y_insinfo:=insinfo^.Attr.y;
  END;
  Str(x_insinfo,s); writeinistring(s_state,'InsInfo_X'+#0,s);
  Str(y_insinfo,s); writeinistring(s_state,'InsInfo_Y'+#0,s);
  IF insinfo<>NIL THEN i:=1 ELSE i:=0;
  Str(i,s); writeinistring(s_state,'InstrOn'+#0,s);
  Str(xdet,s);     writeinistring(s_state,'XDetail'+#0,s);
  Str(ydet,s);     writeinistring(s_state,'YDetail'+#0,s);
  detailwnd:=0;
  IF GetCursor=pCursor THEN SetCursor(LoadCursor(0,idc_arrow));
  DestroyCursor(pCursor);
  INHERITED Done;
END;

PROCEDURE detail.handleplus;
BEGIN
  IF index + 30 < LastInstrument THEN BEGIN
    inc(index);
    paintinstr(0,false);
  END;
END;

PROCEDURE detail.handleminus;
BEGIN
  IF index > 1 THEN BEGIN
    dec(index);
    paintinstr(0,false);
  END;
END;

PROCEDURE detail.wmmousemove;
VAR Point:Tpoint;
    handc:boolean;
BEGIN
  point.x:=msg.lparamlo; point.y:=msg.lparamhi;
  handc:=false;
  IF PtInRect(wmlrects[1],point) OR PtInRect(wmlrects[2],point) THEN handc:=true;
  IF handc THEN SetCursor(PCursor) ELSE SetCursor(Norm_Cursor);
  msg.result:=0;
END;

PROCEDURE detail.WMLButtonDown;
VAR no:integer;
    left,right:boolean;
    Point:Tpoint;
BEGIN
  LEFT:=FALSE; RIGHT:=FALSE;
  point.x:=msg.lparamlo; point.y:=msg.lparamhi;
  IF PtInRect(wmlrects[1],point) THEN Left :=TRUE ELSE
  IF PtInRect(wmlrects[2],point) THEN Right:=TRUE ELSE INHERITED WMLButtonDown(msg);
  IF left OR right THEN
  BEGIN
    IF left THEN no:=(15*(msg.lparamhi-(CapDY+y_starttext+2)))DIV(15*(Fh+2))+(index-1)
    ELSE IF right THEN no:=(15*(msg.lparamhi-(CapDY+y_starttext+2)))DIV(15*(Fh+2))+15+index;
    IF no>LastInstrument THEN no:=no-LastInstrument;
    IF insinfo<>NIL THEN
    BEGIN
      insinfo^.index:=no;
      insinfo^.Paintdata(0);
    END ELSE
    BEGIN
      insinfo:=New(pinsinfo,Init(@self,x_insinfo,y_insinfo,info,no));
      Application^.MakeWindow(insinfo);
    END;
  END;
END;

PROCEDURE detail.wmkeydown;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    $31..$39:
    BEGIN
      IF insinfo<>NIL THEN
      BEGIN
        insinfo^.index:=byte(msg.wparam-$30);
        insinfo^.Paintdata(0);
      END ELSE
      BEGIN
        insinfo:=New(pinsinfo,Init(@self,x_insinfo,y_insinfo,info,byte(msg.wparam-$30)));
        Application^.MakeWindow(insinfo);
      END;
    END;
    vk_f1   :helpme(@self,id_infodlg);
    vk_return,vk_escape:PostMessage(HWindow,wm_close,0,0);
    ELSE
    BEGIN
      CASE iskey(msg.wparam) OF
        24:handleplus(Msg);
        25:handleminus(Msg);
        36: WITH genbox^ DO SendMessage(HWindow,wm_vscroll,sb_lineup,(HWindow SHL 16)+getcaretindex);
        37: WITH genbox^ DO SendMessage(HWindow,wm_vscroll,sb_linedown,(HWindow SHL 16)+getcaretindex);
        38: WITH genbox^ DO SendMessage(HWindow,wm_hscroll,sb_lineup,(HWindow SHL 16)+getcaretindex);
        39: WITH genbox^ DO SendMessage(HWindow,wm_hscroll,sb_linedown,(HWindow SHL 16)+getcaretindex);
        42: IF InsInfo<>NIL THEN InsInfo^.handlePlus(Msg);
        43: IF InsInfo<>NIL THEN InsInfo^.handleMinus(Msg);
        ELSE msg.result:=SendMessage(Parent^.HWindow,wm_keydown,msg.wparam,msg.lparam);
      END;
    END
  END;
END;

PROCEDURE detail.wmdrawitem;
VAR oldfont:hfont;
    hg:word;
    p      :Pchar;
BEGIN
  WITH pdrawitemstruct(msg.lparam)^ DO
  IF (ctltype=odt_listbox)AND(ctlid=id_genbox) THEN
  BEGIN
    IF PaletteInstalled THEN SelectPalette(hdc,hpal,false);
    IF ItemId>=AnsiLines
    THEN oldfont:=SelectObject(hdc,OEMhandle)
    ELSE oldfont:=SelectObject(hdc,ANSIhandle);
    IF (ItemId>=CenterLines)AND((CommentW+2)*Fw>=(rcitem.right-rcitem.left))
    THEN SetTextAlign(hdc,ta_left OR ta_top OR ta_noupdatecp)
    ELSE SetTextAlign(hdc,ta_center OR ta_top OR ta_noupdatecp);
    SendMessage(genbox^.HWindow,lb_getitemrect,itemid,longint(@rcitem));
    SetBkColor(hDC,cHGLB); SetTextColor(hdc,cVGLB); SetBkMode(hdc,opaque);
    HG:=SendMessage(hwnditem,lb_gettextlen,itemid,0);
    IF HG>0 THEN WITH rcitem DO
    BEGIN
      GetMem(p,HG+1);
      SendMessage(hwnditem,lb_gettext,itemid,longint(p));
      IF (ItemId>=CenterLines)AND((CommentW+2)*Fw>=(rcitem.right-rcitem.left))
      THEN ExtTextOut(hdc,left+Fw,top,eto_opaque,@rcitem,p,HG,NIL) ELSE
      BEGIN
        inc(left,(right-lb_rightstartpos));
        ExtTextOut(hdc,(right-left) DIV 2,top,eto_opaque,@rcitem,p,HG,NIL);
      END;
      FreeMem(p,HG+1);
    END;
    SelectObject(hdc,oldfont);
  END;
END;

{DETAIL zu INSTR*******************************************************************}
CONSTRUCTOR tinsinfo.Init;
BEGIN
  index:=instrindex;
  infstruct:=ainfostruct;
  INHERITED Init(Aparent,'Instruments',ws_sysmenu,notopmost);
  WITH Attr DO
  BEGIN
    x:=ix; y:=iy; w:=23*fw+20; h:=capdy+164;
    setcorrectwinpos(x,y,w,h);
    style:=ws_border OR ws_visible OR ws_popup;
  END;
  _digits:=New(pbmplcd   ,Init(@self,0       ,Attr.w-55,capdy+120,3,0,'LCD_08x13'));
  bplus  :=New(pbmpbutton,Init(@self,id_plus ,Attr.w-24,capdy+119,'boplus',true));
  bminus :=New(pbmpbutton,Init(@self,id_minus,Attr.w-75,capdy+119,'bominus',true));
  insname:=New(pmycenterstatic,Init(@self,0,'',8,CapDY+142,Attr.w-18,Fh,22));
END;

DESTRUCTOR tinsinfo.Done;
BEGIN
  pdetail(Parent)^.x_insinfo:=Attr.x;
  pdetail(Parent)^.y_insinfo:=Attr.y;
  INHERITED Done;
  pdetail(Parent)^.insinfo:=NIL;
END;

PROCEDURE tinsinfo.SetupWindow;
BEGIN
  INHERITED SetupWindow;
  oldfocus:=bplus^.HWindow;
  _digits^.enable; bplus^.enable; bminus^.enable;
  SendMessage(insname^.HWindow,wm_setfont,OEMhandle,0);
END;

PROCEDURE tinsinfo.wmerasebkgnd(VAR msg:tmessage);
BEGIN
  paintcaption(msg.wParam);
  framefilled3d(msg.wParam,0,capdy,Attr.w-2,Attr.h-capdy-2,2,cForeGnd,up);
  frame3d(msg.wParam,6,capdy+6,Attr.w-14,(fh+2)*7+8,1,down);
  framefilled3d(msg.wParam,Attr.w-77,capdy+116,69,20,1,cBackGnd,down); {6,attr.w-14}
  WITH bminus^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH bplus^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  WITH insname^.Attr DO frame3d(msg.wParam,x-2,y-2,w+4,h+4,1,down);
  blit(HWindow,8,capdy+116,94,20,0,0,m_label);
  PAINTDATA(msg.wParam);
  INHERITED WMEraseBkGnd(msg);
END;

PROCEDURE tinsinfo.PAINTDATA(adc:hdc);
VAR oldfont:thandle; s:STRING[32]; pdc:hdc;
  FUNCTION ConvertFT (finetune: byte): shortint; assembler;
    asm
      mov al, finetune
      SHL ax, 4
      cbw
      SHR ax, 4
    END;
BEGIN
  IF ADc=0 THEN PDC:=GetDC(HWindow) ELSE pdc:=adc;
  IF PaletteInstalled THEN SelectPalette(pdc,hpal,false);
  oldfont:=SelectObject(pdc,ANSIhandle);
  SetTextAlign(pdc,ta_left OR ta_top OR ta_noupdatecp);
  SetTextColor(pdc,cVGStatic); SetBkColor(pdc,cHGStatic);
  WITH infstruct^.instruments[index] DO
  BEGIN
    s := Name;
    WHILE (Length (s) > 0) AND (s[1] = ' ') DO Delete (s, 1, 1);
    IF Length (s) > 22 THEN s[0] := Chr (22);
    insname^.setstringtext(s);

    IF Bits < 128
      THEN BEGIN Str({2*}((longint(iend)-offset{+1}){DIV 2}):9,s); s:=getresstring(3045)+s+#0; END
      ELSE BEGIN wvsprintf (@s[1], '%08lX', IEnd); s[0] := #9; s := 'Param D00-3:  ' + s; END;
    TextOut(pdc,14,capdy+10 ,@s[1],Length(s)-1);

    IF Bits < 128
      THEN BEGIN
        IF loopstart>0 THEN Str({2*}((longint(loopstart)-offset{+1}){DIV 2}):9,s) ELSE Str(0:9,s);
        s:=getresstring(3046)+s+#0; END
      ELSE BEGIN wvsprintf (@s[1], '%08lX', LoopStart); s[0] := #9; s := 'Param D04-7:  ' + s; END;
    TextOut(pdc,14,capdy+24 ,@s[1],Length(s)-1);

    IF Bits < 128
      THEN BEGIN
        IF LoopEnd>0  THEN Str({2*}((longint(loopend)-loopstart{+1}){DIV 2}):9,s) ELSE Str(0:9,s);
        s:=getresstring(3047)+s+#0; END
      ELSE BEGIN wvsprintf (@s[1], '%08lX', LoopEnd); s[0] := #9; s := 'Param D08-B:  ' + s; END;
    TextOut(pdc,14,capdy+38 ,@s[1],Length(s)-1);

    Str(ConvertFT (finetune):9,s); s:=getresstring(3048)+s+#0;
    TextOut(pdc,14,capdy+52 ,@s[1],Length(s)-1);

    Str(volume:9,s);   s:=getresstring(3049)+s+#0;
    TextOut(pdc,14,capdy+66 ,@s[1],Length(s)-1);

    IF Bits < 128
      THEN BEGIN Str(Bits AND $1F:5,s); s:=getresstring(3050)+s+' bit'+#0; END
      ELSE BEGIN Str((Bits AND $7f), s); s:='=> AdLib Instr. # '+s+' <='+#0; END;
    TextOut(pdc,14,capdy+80 ,@s[1],Length(s)-1);

    Str(Tuning:6,s);    s:=getresstring(3051)+s+' Hz'#0;
    TextOut(pdc,14,capdy+94 ,@s[1],Length(s)-1);

  END;
  SelectObject(pdc,oldfont);
  _digits^.setbmpzahl(index);
  IF ADC=0 THEN ReleaseDC(HWindow,PDC);
END;

PROCEDURE tinsinfo.wmkeydown;
BEGIN
  msg.result:=0;
  CASE msg.wparam OF
    vk_f1 :helpme(@self,id_instrwin);
    vk_return,vk_escape:PostMessage(HWindow,wm_close,0,0);
    ELSE msg.result:=SendMessage(Parent^.HWindow,wm_keydown,msg.wparam,msg.lparam);
  END;
END;
PROCEDURE tinsinfo.handleplus;      BEGIN inc(index); IF index>LastInstrument THEN index:=1; PAINTDATA(0); END;
PROCEDURE tinsinfo.handleminus;     BEGIN dec(index); IF index=0 THEN index:=LastInstrument; PAINTDATA(0); END;

BEGIN
  detailwnd:=0;
END.