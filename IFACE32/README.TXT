Inteface libraries for MOD4WIN's Payer32.dll.

Files:

Player32.DLL       ... The actual MOD4WIN player engine
IFACE16.BIN        ... 16Bit Interface APP

IFACE32.PAS        ... 16/32 Bit Interface to Iface16.bin

DSTEST32.*         ... Delphi 2.0 demo App for the DS Interface
DSTEST16.PAS       ... Borland Pascal 7.0 demo App for the DS Interface

How does it work?:

In 16/32 Bit Delphi Apps just add 'IFACE32' to your uses section and
call the desired functions.

!!! NEVER !!! Mix DS_XXXX and MOD_XXXX playing Functions.

Try to have FUN!

PS: This interface is subject to heavy changes when the real 32Bit DLL
    will be released!
