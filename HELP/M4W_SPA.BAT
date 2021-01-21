@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
if "%1"=="" GOTO CD
cd %1
GOTO GO
:CD
cd spa
:GO
..\helpcomp\hcp.exe modsphlp.hpj
echo.
echo.
copy modsphlp.hlp ..\m4w_spa.hlp
del modsphlp.hlp
cd ..\..
