@echo off
set path=C:\tools;f:\exe;f:\compiler;e:\td;e:\bp;f:\winunit;f:\dosunint
set LIB=
set INCLUDE=
cd player
ren player32.pas MPLAY32.pas
call bpw MPLAY32.pas /DEWSPLAYER /DEWS /R..\intl
ren MPLAY32.pas player32.pas
copy MPLAY32.dll ..\_burger_
del MPLAY32.dll
CD ..
call delbak
