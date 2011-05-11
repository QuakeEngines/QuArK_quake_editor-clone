(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) QuArK Development Team

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

http://quark.sourceforge.net/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.4  2010/04/16 21:48:00  danielpharos
Added auto-Delphi-compiler version string selection, and moved compiler-string down a bit.

Revision 1.3  2010/04/16 21:18:45  danielpharos
Move some version-stuff about. quarkpy now also checks the minor version number.

Revision 1.2  2010/04/16 20:07:23  danielpharos
Move some version-stuff about. quarkpy now also checks the minor version number.

Revision 1.1  2010/04/16 18:44:59  danielpharos
Reduced missing init-logging entries to a single problematic line. Also, logging now uses const strings (faster).
}

unit QkConsts;

interface

{$I DelphiVer.inc}

uses SysUtils;

const
  QuArKVersion            = 'QuArK 6.6';
  QuArKMinorVersion       = 'Beta 4';
  QuArKCopyright          = 'Copyright (C) 1996-2011 Armin Rigo and others';
{$IFDEF CompiledWithDelphi1}
  QuArKUsedCompiler       = 'Delphi 1.0';
{$ELSE}
{$IFDEF CompiledWithDelphi2}
  QuArKUsedCompiler       = 'Delphi 2.0';
{$ELSE}
{$IFDEF CompiledWithDelphi3}
  QuArKUsedCompiler       = 'Delphi 3.0';
{$ELSE}
{$IFDEF CompiledWithDelphi4}
  QuArKUsedCompiler       = 'Delphi 4.0';
{$ELSE}
{$IFDEF CompiledWithDelphi5}
  QuArKUsedCompiler       = 'Delphi 5.0';
{$ELSE}
{$IFDEF CompiledWithDelphi6}
  QuArKUsedCompiler       = 'Delphi 6.0';
{$ELSE}
{$IFDEF CompiledWithDelphi7}
  QuArKUsedCompiler       = 'Delphi 7.0';
{$ELSE}
  QuArKUsedCompiler       = 'Delphi';
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
  QuArKCompileDate        = 40674;   //This is the compiled date
  { Amount of days that have passed after 30 Dec 1899 (Delphi 2+).
    You can use EncodeDate(Year, Month, Day) to compute it, but this value
    really needs to be a constant, so put the resulting value in here.
    The result can be checked in the About form. }
  QuArKDaysOld            = 270;     //About a 9 month difference...
  { This is the amount of days after which a certain build is considered
    old by the update-check. }
  QuArKWebsite            = 'http://quark.sourceforge.net/';
  QuArKRepository         = 'http://sourceforge.net/projects/quark/';
  QuArKForum              = 'http://quark.sourceforge.net/forums/';
  QuArKInfobase           = 'http://quark.sourceforge.net/infobase/';
  QuArKDefaultHelpPage    = 'index.html'; 
  QuArKUpdateSite         = 'quark.sourceforge.net';
  QuArKUpdateFile         = '/update/index.dat';

implementation

end.
