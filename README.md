# Mod4Win

This is the Mod4Win 2.4 Beta source code.
The source code has been kindly provided by the authors, Kay Bruns,
Uwe Zaenker and Jens Puchert.

The intent of the maintainer is that this code be preserved and hopefully 
updated or re-written to work on modern computers.

The code is written in pascal and assembly. The compilers have been removed
from this repository as they were not freely distributable.


# Original Readme Extract:

Features
--------

Mod4Win is a player for digital music modules on IBM-PC compatible machines 
running Microsoft Windows.

- Mod4Win 2.30 supports:
  * NoiseTracker (*.NST), Pro-, Fast-, and TakeTracker (*.MOD), Grave Composer 
    (*.WOW), Oktalyzer (*.OKT), ScreamTracker 2.x (*.STM), ScreamTracker 3.x 
    (*.S3M), Composer 669 and UNIS 669 (*.669), Farandole Composer (*.FAR), 
    MultiTracker (*.MTM), and FastTracker II (*.XM) modules
  * Sample rates between 11 and 48 kHz
  * 8 and 16 bit sample depth
  * Mono, Stereo, and Stereo Surround Sound
  * Interpolated Dynamic Oversampling (IDO)
  * Direct Hardware Support
  * Direct To Disk Recording

- Full archive support
  * Supports ARJ (*.ARJ), LHARC (*.LHA, *.LZH), and PKZIP (*.ZIP)

- Jukebox function for up to 2999 MOD-Files in one session
  * Generates playlists (*.MOL) with files from up to 200 directories or 
    archives
  * Drag & Drop feature of one or more modules, archives, and playlists
  * Launching of a module, archive or playlist from a command line parameter

- Intuitive and easy to use interface with complex functionality
  * Hotkeys for all player functions, many of them user-definable
  * Mini Status uses minimal desktop space and stays optionally always on top
  * Effect Panel shows current effects, instruments, notes, volumes, and speed
  * Integrated File Manager to copy, move/rename, and delete modules from and 
    into drives, directories, archives, and list files
  * Quick Selection Box displays all modules in your current playlist and lets 
    you select one immediately
  * User Registration Dialog simplifies the registration process

- Mod4Win saves all settings, such as
  * window positions
  * last accessed directories
  * sound card and wave driver settings
  * optionally the complete current status, so the next session will start 
    exactly where you stopped the last one
_______________________________________________________________________________

System requirements
-------------------

Sound:
- A sound card with at least one DAC that can process sampling at 11 kHz or 
  better and an appropriate asynchronous wave driver, also known as an MPC-2 
  compatible wave device.
  - or -
- A sound card that is directly supported in hardware (OPL4 based card with
  onboard RAM or Gravis Ultrasound)
- Note: DAC emulators like speaker.drv cannot be supported.  In direct to disk
  recording mode there is no special sound hardware required.

Processor:
- AT 386 SX/16 as the absolute minimum to run the program.
- AT 386 DX/20 for playing at the highest sample rate.
- AT 486 DX/33 with 8 MB RAM to use Mod4Win as a background jukebox at 44 kHz 
  and 16 bit stereo sampling.
- Note: To play modules with more than 8 channels using IDO and panning, a 
  faster processor may be required.  In direct to disk recording mode and
  direct hardware support mode, any AT 386 or better is sufficient to run the
  program.

Video:
- A VGA video adapter with at least 16 colors simultaneously.
- Suggested video resolution 800 x 600 to show the Main Dialog, Info Dialog, 
  and Effect Panel without overlapping.
- Note: With a Hercules or Monochrome adapter many of the dialogs will 
  appear unreadable.

Operating System:
- Microsoft Windows 3.1 or higher running in 386 enhanced mode.
- Note: This version of Mod4Win will not run on 286-based machines anymore, 
  neither will it run under Windows in standard mode.  There is no special
  support for Windows 95 at this point.

For a more detailed discussion of System Requirements see also the sections
Known Problems and Frequently asked Questions (FAQ) in the Windows help file.
