Getting QuArK to compile under Delphi 6 Personal Edition
========================================================

Obtaining and installing everything
-----------------------------------

Obtain Delphi 6 Personal Edition from www.borland.com (you need to register to complete the download, and also so that you can register Delphi after you have installed it) (approx. 140M).  There are some patches to Delphi 6, but I haven't installed them and things seemed to work ok.  If you do download patches, make sure they are the Personal Edition patches.

Get the latest QuArK source from SourceForge (something like 12M, but you get lots of little files instead of one big zip file), and (if you haven't already got it) the mini 
Python installer (minipy) from the QuArK downloads page (1M).

You will also need the build pack for whichever game you want to make maps for.  If you will be developing QuArK, it is a Good Idea (tm) to test any changes you make, so it is a good idea to grab a build pack or two before you start editing QuArK source code.

If you are an existing QuArK user, you should already have minipy (or the full Python) installed, and probably a build pack or two.

Install Delphi 6 PE (you should register it the first time you run it, you can do that across the Internet (if you have an Internet connection), via web/email, or via telephone).

Install minipy, and any build packs.

(I don't need to say that if you are testing levels then you need the respective game installed.)

Compiling QuArK
---------------

The QuArK source download contains a readme file regarding the QuArK source - follow it carefully as it applies to Delphi 6 too.  You will need to install vclar40.dpk - Delphi 6 will give you a warning that it is an older version package, just say yes and it will be converted for you.

What has changed for Delphi 6
-----------------------------

Files which I had to change to get QuArK to compile under D6PE:

	prog\SystemDetails.pas - included a {$ifdef VER140} to cover Delphi 6
	prog\Resizer_delphi6.asm - modified version of Resizer.asm
	prog\CCode.pas - {$I} for Resizer_delphi6.asm inside a {$ifdef VER140}

The biggest change was to Resizer.asm (the new version is called Resizer_delphi6.asm for easy reversal 'cause I don't know assembler, and just made some fairly obvious changes which might or might not be correct).  What changed were lines like:

	sal esi, dword +24

was changed to:

	sal esi, byte +24

Intel's Pentium CPU manual notes that SAL (shift arithmetic left) accepts a register (ESI in this case) and an argument.  It multiplies the contents of the register by 2 the number of times indicated by the second argument, which can be 1, CL or an 8 bit number.  I'm guessing that the Delphi 2..5 inline assembler automatically converted the "DWORD" typecast to a "BYTE" typecasty, but D6 is not as forgiving, and demands a "BYTE" typecast, which is all that it really is, just a typecast.  There are lots of lines which needed this change, but it is the same change throughout.

Testing
-------

After building QuArK, I copied the .exe into a runtime directory, ran it, configured the Q3 build tools (GtkRadiant 1.2.1), created a simple Q3 map with a few entities, a duplicator, several textures and shaders, and then built and ran it.  Everything seemed to work ok, including the OpenGL preview inside QuArK, except for the grid in the editing window disappearing at odd intervals.

There is a lot of QuArK which I don't use, but loading/saving, the texture-browser, common tasks in the map-editing window, OpenGL preview and building (including QuArK's console) all appear basically to work.

Questions
---------

Questions should probably be directed to the QuArK-Python forum which I monitor from time to time, not that I will necessarily be able to help, nor remember what I have done ;-)

Rowdy
1-Apr-2002
