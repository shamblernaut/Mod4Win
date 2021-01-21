                           *************************
                           * MOD4WIN  Version 2.30 *
                           *************************
_______________________________________________________________________________

Installation Instructions:
--------------------------

- start Windows (version 3.1 or higher required)

- insert disk in drive A or B

- in the Program Manager go to File | Run...

- on the Command Line type <drive>:install.exe, e.g. "A:install.exe"

- follow the instructions of the installation program
_______________________________________________________________________________

What's new in Mod4Win 2.30
--------------------------

>> 2.20 ==> 2.25 <<

+ Direct Hardware Support for GF1 based cards (Gravis Ultrasound)
+ loads 16 bit instruments in OPL4 Mode
* MOD4WIN now appears on the Windows 95 taskbar
+ you can now en-/disable all channels and play solo instruments from the 
  Effect Panel
+ MOD4WIN has its own GPF handler that automatically releases all libraries, 
  fonts, and temporary files that were in use
* bug fixes
  - Setup Dialog crashed when there was no Wave Driver installed
  - Shareware Dialog crashed on ALT-F4
  - Windows Timer and Interrupt Timer were sometimes locking up the system
  - state was not fully restored when started from a list icon
  - removed nasty GPF from Open Dialog

>> 2.25 ==> 2.30 <<

+ added support for the XM Format
+ Surround Sound option in GF1 Mode for modules with less than 8 channels
* Effect Panel display is more lively now in DAC mode
* bug fixes
  - some Effects where played wrong for the MOD Format
  - fixed GPF in Quick Selection Box
  - fixed sticky Fast Forward and Rewind buttons when extracting archives
_______________________________________________________________________________

**************************************************************************
* The following chapters are all included in the Mod4Win Help file and   *
* really look much better there.  Of course, if you absolutely love text *
* files, you can study it here. 					 *
**************************************************************************
_______________________________________________________________________________

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
_______________________________________________________________________________

Authors
-------

Mod4Win is a joint project between Kay Bruns, Uwe Zaenker, and Jens Puchert.

For a detailed rundown on who did what in Mod4Win please see the Authors
section in the Windows help file.

If you find any problems with the program or would like to comment on anything,
it's best to contact the person responsible for the particular section of the 
program you'd like to comment on.  For contact information please refer to the
Authors section in the Windows help file.

If you have general questions about Mod4Win, questions regarding registration 
and payment, or dealer inquiries, please contact Kay Bruns or Uwe Zänker (if 
you live in Europe), or Jens Puchert (if you live in the United States or 
anywhere else outside of Europe).

Note: Before you contact any one of us, please check the Frequently asked 
Questions (FAQ) section in the Windows help file.  Your question may already 
be answered there and you can save yourself and us some valuable time.
_______________________________________________________________________________

Trouble Shooting
----------------

For a detailed discussion on trouble shooting see the sections about

- Known Problems and
- Frequently Asked Questions (FAQ)

in the Windows help file.
_______________________________________________________________________________

Shareware Notes
---------------

* Differences between Shareware- and Full Version *

Principally shareware- and full version are equally powerful.  We figured that 
a limited shareware version makes no sense, because you can't test and evaluate
a feature that hasn't been implemented in your free demo.  However, experience 
shows that it is necessary to limit the free demo version in some way and 
instead of removing features, we let you enjoy a free 30 day trial period in 
which you can evaluate the program free of restrictions.  Exempt from that is 
the ability to save your own playlists which is only allowed if you license a 
full version.

If you find the program useful to you and would like to use it beyond your 30 
day free trial period you will have to obtain a full version through 
Registration.  The free demo version will remind you that you haven't 
registered yet every time you start and shut down Mod4Win.  Of course you will 
be spared all of this, if you purchase a copy of the full version of the 
program.  Your registered copy of Mod4Win will also show your name in the 
caption of the Main Dialog and in the credit scroll of the About Dialog.

* Documentation *

Since Mod4Win is so easy and intuitive to use, we don't find it necessary to 
issue printed documentation for it.  The hyperlinked Windows help system is 
never more than one keystroke away (just hit F1 from any dialog).  If you 
would like to have something on paper, feel free to spool this help text to 
your printer or to a file.

* Retail Sales of the Full Version *

Mod4Win is not available as a retail product at this point.  To obtain a 
registered full version please follow the instructions under Registration.

* Update Service *

Registered users always get a great discount on updated releases.  Depending 
on how big a step there is between your and the current version, prices range 
from free updates to about half the price of the full version.  For details 
please refer to the section Registration in the Windows help file.
_______________________________________________________________________________

License Regulations
-------------------

1. All attempts to defeat the shareware check and the 30 days trial period 
   limit are a violation of our copyright and strictly prohibited.  If such 
   attempts become known we reserve the legal right to prosecute the 
   offending party.

2. Modification of a file that belongs to the Mod4Win package is strictly 
   prohibited!

3. Disassembling, reverse engineering, patching, hacking, or cracking of this 
   program, as well as distribution of any material the encourages or assists 
   in any of these activities is strictly prohibited.  Any violation against 
   this rule constitutes copyright fraud and may be subject to prosecution 
   under copyright law.

4. Possession of the registered version is permitted only to the registered 
   user.

Note especially well: The sample modules included in the distribution package 
                      of Mod4Win are copyright of their respective owners.

Users of any version of Mod4Win implicitly agree to all license regulations 
when they install the software on their machine.

For detailed explanation of all licensing regulations please refer to this
section in the Windows help file.
_______________________________________________________________________________

Liability, Warranty and Trademark
---------------------------------

We, the Authors, make no warranty of any kind, expressed or implied, including 
but not limited to any warranties of fitness for a particular purpose.  In no 
event shall the authors be liable for any incidental or consequential damage 
arising from the use of, or inability to use, this program.  We hereby deny 
any liability to the maximum extent permitted by law.

You are fully responsible for everything you are doing with this program!

We reserve all rights for our program.

Mod4Win and IDO are trademarks of JSInc. and SWE Bruns & Zänker.

For a more detailed explanation of liability, warranty, and trademark please
refer to this section in the Windows help file.
_______________________________________________________________________________

Registration for Mod4Win 2.30
-----------------------------

The basic registration procedure is simple.  Just fill out the Registration 
Form, and send it with the proper Payment to your distributor.  Depending on 
where you live, you will be served by different distributors and slightly 
different registration procedures apply.  For details please refer to the
section on Registration in the Windows help file.

If you are subscribed to Compuserve Information Service, you can also register 
right from the convenience and privacy of your monitor.  Go to the SWREG forum,
ID #4138 and follow the instructions there.

For up to date info on credit cards, product updates, and more check our 
preliminary World Wide Web page at http://scuzzy.fmmo.ca/mediatrix/mod4win.htm
(courtesy of Mediatrix Peripherals, Inc.).
_______________________________________________________________________________

Price Chart for Mod4Win 2.30
----------------------------

new user registration fee: $30
update from 1.xx:          $15
update from 2.00:          $12
update from 2.1x:          $10
update from 2.2x:          FREE

If you already have a copy of the free demo version of Mod4Win 2.30 there will
be no additional charge for sending you the registration code.

If you do not have a copy of the free demo version of Mod4Win 2.30 and require 
a disk with the registered version to be sent out to you there will be an 
additional $5 shipping and handling charge per order.

You are no longer required to provide a self-addressed envelope.  Shipping is 
done via first class mail within the United States and Canada, and via air mail
everywhere else.  Disks are generally in 3.5" DS-HD format.  For maximal 
protection, all disks are sent in a bubble mailer.
_______________________________________________________________________________

Registration Form for Mod4Win 2.30
----------------------------------

Please print the registration form from the Windows help file (click on 
Contents and then on Registration Form).
_______________________________________________________________________________

Final Words
-----------

Have fun with Mod4Win!
