

                 Patch4 to QuArK Snapshot of 2002ap20
                 
  * [tiglari] Mohaa map reading implemented, various map writing
       bugs fixed
       
  * [nurail] JKII light path entities corrected


                 Patch3 to QuArK Snapshot of 2002ap29

   
 * [nurail] error involving line 447 mapquakemenu hopefully fixed

 * [rtdtheprof] torque maps read correctly

 * [tiglari] Mohaa maps should now be written correctly (not read, yet). 


                Patch 2  to QuArK Snapshot of 2002ap29
                
 * [tiglari] mirror duplicators are supposed to work now.  This is
    really a fix to new texture positioning scheme, the underlying
    problem might have been having other repercussions.


                Pattch 1 to QuArK Snapshot of 2002ap20
                
 * [tiglari] fix errors on opening map editor (for some people,
      not me, for reasons which I don't understand at all)
      
 * [tiglari] basic Mohaa maps now compile and run.  Texture
      flags don't work yet but.  Another peculiar feature of
      Mohaa is that the maps are written into the main directory,
      since the engine seems to have trouble with tmpquark.  This
      may change.


                QuArK Snapshot of 2002ap29
                
 * [Decker, tiglari] improved RTCW support
 
 * [tiglari] fix bug in brush primitives writing
 
 * [Decker] start Mohaa support
 
 * [Andy] start JKII support
 
 * [Rowdy] make it compile in Delphi 6

 * [tiglari]
    On search menu, 'find bad tex scale' item that locates faces with
     axes almost parallel or too small (the settable parameters; I'm
     open to suggestion as to what the default values should actually
     be).

 * [tiglari]
   `Move containing' item on the vertex RMB that moves the parent item
     selected from the submenu so that the vertex becomes on-grid

 * [tiglari]
    Better F1 help for the 'texture wrap multiplier' menu item on the
     face RMB|Textures submenu
     
                QuArK Snapshot of 2002mar28
           
 * [tiglari]
    fix groups hidden/greyed icon change (report by off_by_two)

 * [tiglari]
    change map-writing system so that format is specified in game
    config rather than Map|Option flags.

  This snapshot is distributed also as a patch that can be overwritten
  on the 2002feb28 snapshot.
  

               QuArK Snapshot of 2002feb28
           
 * [Andy]
    .md3 skins can hopefully be loaded from paths with
    forward slashes

 * [Decker/tiglari]
    a memory leak involving the display of textures
    in the multi-pages panel has hopefully been fixed
    
 * [tiglari]
    memory-use tracker.  To use it, first check Developer mode in
    the options menu, then restart QuArk.  On the command menu will
    appear an item 'heapstatus', click it to get a display of how
    much memory has been allocated, and on a second click, how much
    this has changed.  The undo stack seems to consume about 1500 bytes
    for a simple brush move; I set this to 0 for leak hunting.

           QuArK Snapshot of 2002feb24

 * [Decker]
   Added some Medal of Honor:Allied Assault support. Not much,
   as the buildtools for maps (.BSP files and such) have not yet 
   been released, nor have entity-describtions.

 * [Decker]
   The buildtools-setup for Return to Castle Wolfenstein is now
   configured for GTKRadiant's Q3MAP.EXE program.

 * [Andy/Decker]
   Added .PNG images file support, though only 8-bits/paletted
   images shown in QuArK's texture-browser. This means that any
   24-bits .PNG images will not show correctly in QuArK.
   (Programmers note: Duplicated ZLib code. Should try to only
   use source\components\zip, and eliminate the need for 
   source\components\PNGZlib.PAS and source\components\*.OBJ)

 * [Andy/Decker]
   Added Tribes 2 .VL2 and .CS-script files support. Note that 
   .VL2 files are actually .ZIP files, just like .PK3 files are,
   so if you're not happy with the way QuArK handles .VL2 files,
   you can use WinZIP or any other ZIP-utility. And .CS files 
   are just plain text-files, so use your favorite "NotePad"-
   editor.


           QuArK Snapshot of 2002jan06

 * [Decker]
   Added Return to Castle Wolfenstein game-support, though
   the buildtools-setup for RTCW isn't configured properly
   yet.

 * [Decker]
   Maybe fixed the problem, of endlessly occuring dialog-
   boxes, which asked if QuArK should save modified files
   now. Usually this happened, when using the "Create new
   texture-links" in the Texture Browser, and just continued
   to work within QuArK. (See revision 1.29 of Qk1.PAS)


           Patch2 to O80601 Snapshot

 * 'hlradfilemaker' duplicator plugin.

 * 'symxyz' duplicator with toggleable axes.

 * 'Adjust Angles Automatically' Menu Option and
   'Auto-Adjust Normal' Map option renamed to
   `Quantize angles'

 * usercenters now transform with containing groups under
     linear mapping

 * Q3A CDROM config now works.

 * fix bugs in snap object (separation, when selected or
     tagged face is horizontal)

 * refurbish slide poly etc plugin; now works wrt tagged
     plane as well as tagged edge

 * Some Half-Life config fixes (see DataHL.qrk changelog)


           Patch1 to 080601 Snaphsot

 * reset texture cycle command, so that the texture
   cycle files can be edited, and results seen in the
   same editing session

 *'snap object to tagged' menu item: if a face is tagged,
   select a face, then a parental object from the menu,
   then 'snap object to tagged'.  Object will move and
   rotate that selected face is oriented parallel to
   tagged one, with specified separation.


           QuArK Snapshot of 080601

Changes from 070901:

 *Multiple selection list browser:
    when there is a multiple selection, menu-items and a hotkey
    are activated to list the selected items in a dialog, from
    which the members can be further selected.

 *Texture substitution cycle for standard duplicators
   (basic, linear): give the specific tex_sub the name
   of a file such as texcycle.txt.  The file should
   have a list of texture names on each line, whitespace-
   separated, e.g.:

    gothic_block/block10d gothic_block/blocks10

 *clear mark command.

 *threepoint plane can now glue to tagged plane

 *BSP Support:
    - all bsp's viewed except for Sin (patches omitted from Q3A/STVEF)

    - entity lumps now edit correctly (comments no longer mis-written
         into them)

    - bsp exploration facilities:

        due to less memory use & chance of accidental
        save.

        Command menu items for collecting nodes & planes;
        these appear in the treeview.  Large maps can't show
        all the planes in the treeview.

        When asked whether to save changes, don't.

        There are also some facilities for finding planes
        that lie near others: first you get ones that have
        close neighbors, then you can collect those neighbors,
        then browse the group (including opening the nodes to
        find the nodes split by a given plane).

 *Blue Shift pak's now open

 *Crystal Space Support:
    beziers & new build tool for Crystal Space support


 *6DX support:

     -Start room removed for 6DX

     - *.hmf file support added for 6dx, map checks disabled.
         6dx default map needs textures added to it
         (in data6dx.qrk)


             QuArK Snapshot 070901

Changes from 081801

 *Preliminary 6DX support
 *Cleanup of Crystal Space Support
 *arg replacer added to duplicator list (see infobase
    for usage)
 *some extruder bugs fixed
 *path dup texture bug fixed
 *'elbows' for path dup reinstated and tested
    (made elbow segs by putting path dup template
     copy into elbow template & glueing and pasting
     do dissociated path dup; didn't leak)
 *texture browser search bug fixed

             QuArK Snapshot 061801

Changes from 052101

 4 Users:
  *texture search button in texture browser
  *storeable 3d camera positions
    - add them on the 3d view eye handles & background
        RMB menus
    - they appear in the map and treeview like
        duplicators, can be dragged, deleted, etc
        like ordinary map objects
    - set and store view menu items on their RMB
    - if multiple views are open, these commands use
       the last 3d view clicked on (usually, doesn't
       quite always seem to work, dunno why)
    - cycle the ones in a group with prev/next +
        'C' depressed (should be made user-configurable)
    - find camera positions dialog on search menu
  *extruder 'revert to duplicator' now imports
    texture changes back to duplicator, so you can
    texture at least the first segment of the extruder
    by dissociating images, then change shape by revert
    to duplicator.  Hopefully useful when 1-segment
    extruder is put into path duplicator.
  *group selection movement commands (swap & align,
   suggested by Alan Donald and quantum_red)
  *due north path duplicator bug fixed
  *menu promotion/demotion added to the basic distribution
  *incrementable specifics added to basic, linear, symx/y/z
    and Copy/New Copy One duplicators.
     - If 'increment' specific is checked, then target,
         targetname and killtarget specifics ending in
         a string of digits have the digid string incremented
         in the sequence.
     - if 'incr. all targ. specific is checked, all specifics
         whose name contains 'target' get incremented.
     - any specifics listed in 'incrementable; are also
         incremented.
     - if 'final_target' has a value, this is used as the
         'target' value in the last image.  Likewise
         for 'final_killtarget' etc.
     - if 'increment by' has a value, then this is used
         as the increment, rather than 1.  This can be
         used to use increments with nested duplicators,
         if the outer one increments by 10 and the inner
         by 1, you can have an Nx9 grid,etc.
     - if 'incre_lip' has a value, then that value is
         used to increment the 'lip' speciric, likewise
         incre_<spec> for any other choice of <spec>.
     - for (New) Copy One, each duplictor needs a different
         'increment by' value (otherwise crosstalk).

     Limitation: Doesn't work for symxy and
       Radial duplicators (yet; if you want it to,
       holler).
  *corrections to the 'vertex movement' plugin (this is
    still basically experimental)
  *new 3 point plane stuff:
    a) introduce one from New Map Items|tools, drag
         the handles around and tag it, the glue, cut
         etc.
    b) use tag plane command after tagging an edge

 Under the hood:
   *entity conversion (fgd->qrk) tools pythonified
   *'Code Code' specific for toolbarbuttons (tbbtn) and
     the python buttons (Typ="P" only).

    Example for tbbtn:

     search1:tbbtn = {
       Typ = "P"
       Cap = "Search"
       Code = "import plugins.tex_search; plugins.tex_search.openbox()"
     }

     GetEntitiesBtn: =
     {
       Typ = "P"
       Cap = "Get entities for this game"
       Code = "import quarkx; quarkx.beep()"
     }

   will execute the python string in the Code specifics.


               63 Snapshot 052101

 Changes from 050701:

  *Bsp opening bug in 050701 fixed
  *Map writing altered for brush primitives, Valve mapversion
    220, and 'No TX comments' mode so that if integral or
    almost integral vertexes for a face are found, they are
    used as its threepoints.  This enables better export
    of maps to editors and tools that don't support fp
    coordinates.
  *'thin face' finder added (similar to microbrush h/k);
     detect & remove suspiciously thin faces.
  *when an 'out' duplicator is dissociated, all
    other 'out' dups in its immediate group are
    dissociated also
  *caulk of hidden faces added to brush curves & extruder
    (default tex used for games w/o a caulk texture)
  *wrong shape for non-inner non-inverse cap/bevels
    fixed (missing faces added)
  *'linear' (matrix) specific editor.
    try out with New Copy One duplicator, push button
    to call up editing dialog.
  *matrix2 support changed to linear in radial dup.
  *rotate/scale specifics removed from linearform
  *linear duplicators changed to so that by default,
    mappings apply around 'usercenter' of each item
    in their group, rather than duplicator location

     -- group
          |
          ---- linear duplicator
          |
          ----- group: if this group has a usercenter,
          |        and the dup has a linear specific,
          |        the mapping will apply with the
          |        usercenter as fixed point
          |
          ------group: ditto for this one (each around
                  its own center, seems wierd but
                  features of the code make it tricky
                  to do otherwise).

    old behavior can be restored by unchecking 'item center'.
  *support for the commercial counterstrike/hl
     expansion.
  *Hollowmaker/Wallmaker bug fixed
  *debug writing 'left' to console bug fixed


 Changes from  Snapshot 042801

  *Dragging on RMB blocked.  If anyone wants it back, an option
    could be provided.
  *instance duplicator removed from toolbox, pushing
    'instance' drops it into path dup, where it
    dumps things at the path points.  linear stuff
    removed from path point forms, prolly not useful
    New Copy One does that stuff better.
  *extruder functionality extended.  RMB menu has
    items for punching holes in things (e.g. putting
    a pipe thru a wall w/o overlap).  After 'dissociate
    images', RMB on the resulting group has an 'Extruder
    Stuff' submenu with the hole-punching, some texturing
    stuff that doesn't work right, and a 'revert' that
    restores the duplicator.
  *New Copy One duplicator, implementing ideas of Rolf
     Hulsbergen (better than Instance Dup, for many
     purposes): drop NCO into group like ordinary
     duplicator , drag image away with handle, use scale\
     & rotation fields to rotate around dup location.
  *rotation specific for linear & other dups changed
     to Typ E, so that rotation handles will work.
  *wrap texture from tagged works for parallel faces
  *wrap texture hotkey works.
  *forms creation added to forms editor
  *stvef maps now readable (no more reversion to Q3A mode,
    if in STVEF mode)


 Changes from Snapshot 042001:

  *RMB Menus for the Texture L handles: (tiglari)
    glue/align to tagged.

  *Reimplementation of addon builder: (Andy) still crashes on
    missionpack, this can be avoided by extracting the .bsp's &
    temporarily giving the .pk3 a different extension.

  *Selection menu: (tiglari, major Decker design input)
    change selection to Parent, Next, Previous in group.
      (no 'type' selections yet)

  *Reorganize/Navigate tree added to bezier RMB

  *having 'F' depressed with LMB will now select a face of current
     poly, rather than next poly.


 Changes from 6.2:

  *Customizable hot keys: (tiglari)
    Perhaps more hot  keys should be added.  Which ones?

  *Custom Centers for groups: (tiglari)
    various linear matrix operations (rotations etc)
    will use these if present

  *'scale' and 'rotation' specifics: (tiglari)
    for Linear & some other duplicators.  Does the
    work of 'linear' but more perspicuously.

  *Extuder: (tiglari)
    Makes solid & hollow pipes from 2d outline.  Put
    into path duplicator to extrude along path.

  *Instance Duplicator: (tiglari)
    For lots of copies in various places, idependent
    scale & rotation.

  *Entity Extractor: (Andy)
    I see some problem, not sure if I'm using it right.

  *Merge Polys in Group: (tiglari)
    Mergeable polys in a group are merged.  Not guaranteed
    optimal.

