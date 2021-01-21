UNIT Frames;
INTERFACE
USES WinTypes,WinProcs,Win31;

PROCEDURE GrayFrameUp(dc:hdc;x,y,dx,dy:integer;Width:Byte);
PROCEDURE GrayFrameDown(dc:hdc;x,y,dx,dy:integer;Width:Byte);
PROCEDURE GrayFrameFilledUp(dc:hdc;x,y,dx,dy:integer;Width:byte;color:longint);
PROCEDURE GrayFrameFilledDown(dc:hdc;x,y,dx,dy:integer;Width:byte;color:longint);
PROCEDURE GrayFrameUpDown(dc:hdc;x,y,dx,dy:integer;UW,MW,DW:Integer;color:longint);
PROCEDURE GrayFrameFilledUpDown(dc:hdc;x,y,dx,dy:integer;UW,MW,DW:byte;color,ColorFill:longint);
PROCEDURE GrayFrameDownUp(dc:hdc;x,y,dx,dy:integer;UW,MW,DW:byte;color:longint);

IMPLEMENTATION

PROCEDURE GrayFrameUp(dc:hdc;x,y,dx,dy:integer;Width:Byte);
VAR i:integer;
    pa:hpen;
BEGIN
  IF Width=0 THEN Exit;
  pa:=selectobject(dc,createpen(ps_solid,1,GetSysColor(COLOR_BtnHighLight)));
  FOR i:=0 TO width-1 DO BEGIN moveto(dc,x+dx-1-i,y+i);lineto(dc,x+i,y+i);lineto(dc,x+i,y+dy-1-i); END;
  deleteobject(selectobject(dc,createpen(ps_solid,1,GetSysColor(COLOR_BtnShadow))));
  FOR i:=0 TO width-1 DO BEGIN moveto(dc,x+i,y+dy-1-i);lineto(dc,x+dx-1-i,y+dy-1-i);lineto(dc,x+dx-1-i,y+i);END;
  deleteobject(selectobject(dc,createpen(ps_solid,1,GetSysColor(COLOR_BtnFace))));
  moveto(dc,x+dx-1,y);lineto(dc,x+dx-1-width,y+width); moveto(dc,x,y+dy-1);lineto(dc,x+width,y+dy-1-width);
  deleteobject(selectobject(dc,pa));
END;

PROCEDURE GrayFrameDown(dc:hdc;x,y,dx,dy:integer;Width:Byte);
VAR i:integer;
    pa:hpen;
BEGIN
  IF Width=0 THEN Exit;
  pa:=selectobject(dc,createpen(ps_solid,1,GetSysColor(COLOR_BtnShadow)));
  FOR i:=0 TO width-1 DO BEGIN moveto(dc,x+dx-1-i,y+i);lineto(dc,x+i,y+i);lineto(dc,x+i,y+dy-1-i); END;
  deleteobject(selectobject(dc,createpen(ps_solid,1,GetSysColor(COLOR_BtnHighLight))));
  FOR i:=0 TO width-1 DO BEGIN moveto(dc,x+i,y+dy-1-i);lineto(dc,x+dx-1-i,y+dy-1-i);lineto(dc,x+dx-1-i,y+i);END;
  deleteobject(selectobject(dc,createpen(ps_solid,1,GetSysColor(COLOR_BtnFace))));
  moveto(dc,x+dx-1,y);lineto(dc,x+dx-1-width,y+width); moveto(dc,x,y+dy-1);lineto(dc,x+width,y+dy-1-width);
  deleteobject(selectobject(dc,pa));
END;

PROCEDURE GrayFrameFilledUp(dc:hdc;x,y,dx,dy:integer;Width:byte;color:longint);
VAR pa:hBrush;
    rect:trect;
BEGIN
  GrayFrameUp(DC,x,y,dx,dy,Width);
  pa:=createsolidbrush(color);
  rect.left:=x+width; rect.right:=x+dx-width; rect.bottom:=y+dy-width; rect.top:=y+width; fillrect(dc,rect,pa);
  deleteobject(pa);
END;

PROCEDURE GrayFrameFilledDown(dc:hdc;x,y,dx,dy:integer;Width:byte;color:longint);
VAR pa:hBrush;
    rect:trect;
BEGIN
  GrayFrameDown(DC,x,y,dx,dy,Width);
  pa:=createsolidbrush(color);
  rect.left:=x+width; rect.right:=x+dx-width; rect.bottom:=y+dy-width; rect.top:=y+width; fillrect(dc,rect,pa);
  deleteobject(pa);
END;

PROCEDURE GrayFrameUpDown(dc:hdc;x,y,dx,dy:integer;UW,MW,DW:Integer;color:longint);
VAR pa:hBrush;
    rect:tRect;
BEGIN
  IF mw<0 THEN mw:=0;
  GrayFrameUp(DC,x,y,dx,dy,uw);
  pa:=createsolidbrush(color);
  rect.left:=x+uw; rect.right:=x+dx-uw; rect.bottom:=y+dy-uw; rect.top:=y+uw;
  WHILE Rect.Left<x+uw+mw DO
  BEGIN
    FrameRect(dc,rect,pa); Inc(Rect.left); Inc(Rect.Top); Dec(Rect.Right);Dec(Rect.Bottom);
  END;
  deleteobject(pa);
  GrayFrameDown(DC,x+uw+mw,y+uw+mw,dx-uw-mw-uw-mw,dy-uw-uw-mw-mw,dw);
END;

PROCEDURE GrayFrameFilledUpDown(dc:hdc;x,y,dx,dy:integer;UW,MW,DW:byte;color,ColorFill:longint);
VAR pa:hBrush;
    rect:tRect;
BEGIN
  GrayFrameUpDown(dc,x,y,dx,dy,UW,MW,DW,color);
  pa:=createsolidbrush(ColorFill);
  rect.left  :=x+UW+MW+DW;
  rect.right :=rect.left+dx-2*(UW+MW+DW);
  rect.top   :=y+UW+MW+DW;
  rect.bottom:=rect.top+dy-2*(UW+MW+DW);
  fillrect(dc,rect,pa);
  deleteobject(pa);
END;

PROCEDURE GrayFrameDownUp(dc:hdc;x,y,dx,dy:integer;UW,MW,DW:byte;color:longint);
VAR pa:hBrush;
    rect:tRect;
BEGIN
  GrayFrameDown(DC,x,y,dx,dy,uw);
  pa:=createsolidbrush(color);
  rect.left:=x+uw; rect.right:=x+dx-uw; rect.bottom:=y+dy-uw; rect.top:=y+uw;
  WHILE Rect.Left<x+uw+mw DO
  BEGIN
    FrameRect(dc,rect,pa); Inc(Rect.left); Inc(Rect.Top); Dec(Rect.Right);Dec(Rect.Bottom);
  END;
  deleteobject(pa);
  GrayFrameUp(DC,x+uw+mw,y+uw+mw,dx-uw-mw-uw-mw,dy-uw-uw-mw-mw,dw);
END;

END.