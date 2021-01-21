@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
if "%1"=="" GOTO CD
cd %1
GOTO GO
:CD
cd grk
:GO
..\helpcomp\hcp.exe modgkhlp.hpj
echo.
echo.
copy modgkhlp.hlp ..\m4w_grk.hlp
del modgkhlp.hlp
cd ..\..
