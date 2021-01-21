@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
if "%1"=="" GOTO CD
cd %1
GOTO GO
:CD
cd fre
:GO
..\helpcomp\hcp.exe modfrhlp.hpj
echo.
echo.
copy modfrhlp.hlp ..\m4w_fre.hlp
del modfrhlp.hlp
cd ..\..
