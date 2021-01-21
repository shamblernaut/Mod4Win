@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
if "%1"=="" GOTO CD
cd %1
GOTO GO
:CD
cd nor
:GO
..\helpcomp\hcp.exe modnohlp.hpj
echo.
echo.
copy modnohlp.hlp ..\m4w_nor.hlp
del modnohlp.hlp
cd ..\..
