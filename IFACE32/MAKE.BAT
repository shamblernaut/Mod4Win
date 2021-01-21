@echo off
del iface16.exe
del iface16.bin
call bpw iface16
ren iface16.exe iface16.bin
call bpw playtest
call bpw dstest16
call bpw32 dstest32
call delbak