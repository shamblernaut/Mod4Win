@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
if "%1"=="" GOTO CD
cd %1
GOTO GO
:CD
cd usa
:GO
..\helpcomp\hcp.exe modushlp.hpj
echo.
echo.
copy modushlp.hlp ..\m4w_usa.hlp
del modushlp.hlp
cd ..\..
