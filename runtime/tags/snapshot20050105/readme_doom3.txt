====================================================================
readme_doom3.txt - some notes on Doom 3 map editing support in QuArK
====================================================================

Technical comments or suggestions to the QuArK-Python forum please.

General comments, suggestions or bug reports to the QuArK forum
please.

--------------------------------------------------------------------
What's there?
--------------------------------------------------------------------

Doom 3 is a *huge* game, including many thousands of textures,
shaders, sounds, models and entities.  Where possible, it was
decided to leave as much in as possible, rather than cutting out all
the rarely-used entities etc., consequently there might be a lot of
guff in there that really is not used during map-making.

--------------------------------------------------------------------
What's not there?
--------------------------------------------------------------------

Probably lots of stuff, the more we learn about Doom 3 mapping,
probably the more we will find is missing.  This is a work in
progress ;-)

Specifically:

  * support for version 2 .map files (QuArK reads and writes
    version 1 .map files, and cannot read version 2 .map files,
    fortunately Doom 3 seems to be able to compile version 1 .map
    files)

  * integration of map compiling into the QuArK menu (see below)

  * infobase documentation (or wiki documentation)

  * dynamic texture links - don't expect these to work if you add
    custom texture pak4's, unless you extract the textures from the
    .pk4 files before making links

  * display of models in 3D map editing views

--------------------------------------------------------------------
Editing maps
--------------------------------------------------------------------

As noted above, QuArK currently supports only version 1 .map files.
If anyone has information on the format of version 2 .map files, pls
post it to the QuArK-Python forum.

Otherwise use QuArK as you normally would.  Setup the paths to the
game, switch to Doom 3 mode, and create a new Doom 3 map.

As noted elsewhere, the texture-browser will show only materials as
nearly all textures seem to have an associated material.

The insert map item dialog will show a *lot* of stuff, cdunde has
done a lot of work in reducing and consolidating the list of
entities.  It is hoped to refine this list further.

--------------------------------------------------------------------
Compiling maps
--------------------------------------------------------------------

Aye, here's the rub.

To compile maps in Doom 3 you need to run the game (also requiring
the CD to be inserted), pull down the console, and type 'dmap
<mapname>', then watch many lines of text scroll up the Doom 3
console.  After that, if compilation was successful, you can bring
down the console again, and type 'devmap <mapname>' to run the map.

Before you can do that, you need to export your map (as a .map file)
from QuArK to the base/maps directory under the Doom 3 installation
directory.

It is hoped to better integrate map-compiling functionality in a
future release.

Hint: to save the comsole output from a map compile in Doom 3, drop
down the console and type 'conDump <filanme>'.  You can peruse the
generated file with a text editor of your choice.

So basically:

1. edit your map in QuArK

2. save it as a .qrk file in a directory of your choice (so as to
   retain groupings etc.)

3. when you are ready to compile it, select Doom 3 - Export .map
   file only from the editing window menu

4. run Doom 3

5. pull down the console with Ctrl/Alt/~

6. type (you can omit the .map extension):

   dmap <mapname>

7. watch the compile messages scroll by, to save them for review
   type:

   conDump <filename.txt>

8. if the compile was successful, pull down the Doom 3 console the
   type:

   devmap <mapname>

--------------------------------------------------------------------
Some interesting oddities
--------------------------------------------------------------------

Although Doom 3 includes textures (lots of textures), it appears
that you should use only materials (what used to be called shaders
in older games), consequently you won't find any textures in the
texture-browser, just materials.  One possible exception is special
textures, like caulk and clip, although my results using either the
textures or the invisible materials have been somewhat varied.

Every entity written to a .map file must have a unique name
specific.  You can specify your own name specific, this will not be
overwritten, but you should be careful to use unique names that will
not clash with the auto-generated names.  Auto-generated names are
formed by adding an underscore and a number to the end of the entity
name, for example if the 17th entity in a .map file was a
weapon_plasmagun it would be given a name of weapon_plasmagun_17.

When using target to point, say, a trigger_once to a door, you
should give the object you are targeting (e.g. a door) a name
specific, and point the trigger's target at that name.  There is no
longer a targetname specific.  For example, set the door's "name" to
"my_door" and set the trigger's "target" to "my_door".  Also, the
pale blue arrows will not show up in QuArK's editing windows showing
where targetted items are linked.

Doors and door frames (and probably lots of other things) are
standardised.  You can insert a func_static, then select one of the
door frame models, or insert a func_door and select the appropriate
model for the door.  Then you just need to add some walls around it,
and probably door would help.  Some of the door models are oriented
normally, some of them sideways.  If the door model appears sideways
in your map (which, unfortunately, you will not discover until you
compile/run the map) just add an "angle" specific with a value of,
say, "90" to rotate your door 90 degrees.

--------------------------------------------------------------------
Changes
--------------------------------------------------------------------

27-Dec-2004

  display bounding boxes in 2D map editing views
  
  added an export .map file option to the Doom 3 menu in the editing
    window (significantly simplifies preparation for compiling maps)

--------------------------------------------------------------------
Thanx
--------------------------------------------------------------------

id Software for an awesome game

Armin Rigo for creating QuakeMap, renaming it to QuArK, and
releasing it as open source

the ppl on the QuArK-Python forum, especially cdunde and Peter,
for offering suggestions and encouragement

cdunde particularly for his help with cleaning up the entities

--------------------------------------------------------------------

Rowdy
27-Dec-2004
