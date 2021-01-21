@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
if "%1"=="" GOTO CD
cd %1
GOTO GO
:CD
cd ger
:GO
..\helpcomp\hcp.exe modhelp.hpj
echo.
echo.
copy modhelp.hlp ..\m4w_ger.hlp
del modhelp.hlp 
cd ..\..
