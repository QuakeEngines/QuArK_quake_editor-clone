
                       



                           QuArK 6.3.0
                          Jan 16, 2003
                       

 This is the final release of QuArK 6.3.0.  The .0 means that we envision
 some futher enhancement releases in the 6.3 series, adding and extending
 plugins, but not making major changes.
 
 Installation and Use:
 
   * You must have either the 'Minipy' installed, or Python 1.5.2.
      Python 2.x won't suffice, but can coexist happily with either
      of these.
   
   * DO NOT install this by unzipping over a previous version of QuArK;
      this does require some annoying reconfiguration, but the subtle
      bugs that result from mixing versions are FAR WORSE.

   * After clicking on quark.exe, select the game you want to edit from
      the 'games' menu, then 'configure' to set the paths to the game
      directory and the location of the build-tools.
      
   * For Q1, Hexen II, Q2 and Heretic 2, use the build-packs on the
      QuArK site (http://www.planetquake.com); for Half-Life, use
      Zoner's tools.
      
   * Q3-engine games (Q3A, Elite Force, RCTW, JKII) require that
      GtkRadiant or the 'official' tools be installed; for other games
      there are various options, as discussed on the QuArK site
      http://www.planetquake.com/quark.  The most recent stable release
      of GtkRadiant includes Ydnar's greatly improved version of
      the build tools; get them.  For these games you also want the
      mapmedia and clean shaders in the GtkRadiant distributions, so
      best get the whole thing.
      
   * See the QuArK infobase (on the QuArK site) for more details.
   
   * There have been changes from 6.2 in the internal representation
      of texture position information, so if you're trying it out an
      a project done with QuArK 6.2 or earlier, back up your original
      work, and experiment on a copy.
      
 
 Help and Interaction:
 
   http://www.planetquake.com/quark
   http://groups.yahoo.com/group/quark
 
 
Games Supported:

  Quake 1
  Hexen II
  Half Life (including all addons and major mods)
  Quake 2
  Heretic 2
  Soldier of Fortune
  KingPin
  Sin
  6DX (possibly somewhat outdated)
  Quake 3
  Star Trek Voyager Elite Force
  Crystal Space (somewhat outdated)
  Torque
  Return To Castle Wolfenstein (unpolished)
  Jedi Knight 2 (unpolished)
  Soldier of Fortune 2 (unpolished)
  Medal of Honor:Allied Assault (partial)
       
 Real and Possible Unresolved Issues (from QuArK forums, June-Dec 2002)

   * hole-finder on Search menu finds leaks when there aren't (real)
   
   * very likely still a significant memory leak - save your work often,
       especially when using patches
   
   * quark 6.3 Q1 mdl-viewing problems?
       can't replicate

   * copy/paste causing texture misalignment?
       can't replicate in current version
       
   * software 3d view becomes unresponsive after long use; sometimes
      opening OGL window revives it; sometimes not (prolly can't fix;
      is this associated with resource leak?
      
   * Kingpin animated textures with animation length > 2 don't work.
      since these don't appear in the original game, this is perhaps
      a 'lost extra feature' (from quark511qta) rather than a bug as
      such.

   * Linear duplicator doesn't interact properly with face-sharing;
      basic is fine.
      
   * extruder and radial duplicators still don't have applylinear methods.
   
   * console errors on leaving some windows in the explorer
   

License:

  This program is distributed under the GNU Public License, as detailed
  in copying.txt
  
Disclaimer:

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  
Updates:

  see updates.txt
  
Source Code:

  codebase:   http://www.sourceforge.net/projects/quark
  discussion: http://groups.yahoo.com/group/quark-python
