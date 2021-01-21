@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
if "%1"=="" GOTO CD
cd %1
GOTO GO
:CD
cd SE
:GO
..\helpcomp\hcp.exe modsehlp.hpj
echo.
echo.
copy modsehlp.hlp ..\m4w_se.hlp
del modsehlp.hlp
cd ..\..
