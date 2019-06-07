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

// Versions of this unit are floating freely in several Delphi User Group
// forums out there I don't know the origin.
// It was added because it is a misnomer that a compiler constant is defined for
// that compiler and all newer versions. - SilverPaladin
// DanielPharos: Here's the source: http://delphi.about.com/od/objectpascalide/a/compiler_ver.htm
// or, see: http://delphi.wikia.com/wiki/Borland_Compiler_Conditional_Defines

{$IFDEF VER80}
  {$DEFINE CompiledWithDelphi1}
  {$DEFINE Delphi1orNewerCompiler}
  {$DEFINE Delphi1-2}
  {$DEFINE Delphi1-3}
  {$DEFINE Delphi1-4}
  {$DEFINE Delphi1-5}
  {$DEFINE Delphi1-6}
  {$DEFINE Delphi1-7}
{$ENDIF}
{$IFDEF VER90}
  {$DEFINE CompiledWithDelphi2}
  {$DEFINE Delphi1orNewerCompiler}
  {$DEFINE Delphi2orNewerCompiler}
  {$DEFINE Delphi1-2}
  {$DEFINE Delphi1-3}
  {$DEFINE Delphi1-4}
  {$DEFINE Delphi1-5}
  {$DEFINE Delphi1-6}
  {$DEFINE Delphi1-7}
  {$DEFINE Delphi2-3}
  {$DEFINE Delphi2-4}
  {$DEFINE Delphi2-5}
  {$DEFINE Delphi2-6}
  {$DEFINE Delphi2-7}
{$ENDIF}
{$IFDEF VER100}
  {$DEFINE CompiledWithDelphi3}
  {$DEFINE Delphi1orNewerCompiler}
  {$DEFINE Delphi2orNewerCompiler}
  {$DEFINE Delphi3orNewerCompiler}
  {$DEFINE Delphi1-3}
  {$DEFINE Delphi1-4}
  {$DEFINE Delphi1-5}
  {$DEFINE Delphi1-6}
  {$DEFINE Delphi1-7}
  {$DEFINE Delphi2-3}
  {$DEFINE Delphi2-4}
  {$DEFINE Delphi2-5}
  {$DEFINE Delphi2-6}
  {$DEFINE Delphi2-7}
  {$DEFINE Delphi3-4}
  {$DEFINE Delphi3-5}
  {$DEFINE Delphi3-6}
  {$DEFINE Delphi3-7}
{$ENDIF}
{$IFDEF VER120}
  {$DEFINE CompiledWithDelphi4}
  {$DEFINE Delphi1orNewerCompiler}
  {$DEFINE Delphi2orNewerCompiler}
  {$DEFINE Delphi3orNewerCompiler}
  {$DEFINE Delphi4orNewerCompiler}
  {$DEFINE Delphi1-4}
  {$DEFINE Delphi1-5}
  {$DEFINE Delphi1-6}
  {$DEFINE Delphi1-7}
  {$DEFINE Delphi2-4}
  {$DEFINE Delphi2-5}
  {$DEFINE Delphi2-6}
  {$DEFINE Delphi2-7}
  {$DEFINE Delphi3-4}
  {$DEFINE Delphi3-5}
  {$DEFINE Delphi3-6}
  {$DEFINE Delphi3-7}
  {$DEFINE Delphi4-5}
  {$DEFINE Delphi4-6}
  {$DEFINE Delphi4-7}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE CompiledWithDelphi5}
  {$DEFINE Delphi1orNewerCompiler}
  {$DEFINE Delphi2orNewerCompiler}
  {$DEFINE Delphi3orNewerCompiler}
  {$DEFINE Delphi4orNewerCompiler}
  {$DEFINE Delphi5orNewerCompiler}
  {$DEFINE Delphi1-5}
  {$DEFINE Delphi1-6}
  {$DEFINE Delphi1-7}
  {$DEFINE Delphi2-5}
  {$DEFINE Delphi2-6}
  {$DEFINE Delphi2-7}
  {$DEFINE Delphi3-5}
  {$DEFINE Delphi3-6}
  {$DEFINE Delphi3-7}
  {$DEFINE Delphi4-5}
  {$DEFINE Delphi4-6}
  {$DEFINE Delphi4-7}
  {$DEFINE Delphi5-6}
  {$DEFINE Delphi5-7}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE CompiledWithDelphi6}
  {$DEFINE Delphi1orNewerCompiler}
  {$DEFINE Delphi2orNewerCompiler}
  {$DEFINE Delphi3orNewerCompiler}
  {$DEFINE Delphi4orNewerCompiler}
  {$DEFINE Delphi5orNewerCompiler}
  {$DEFINE Delphi6orNewerCompiler}
  {$DEFINE Delphi1-6}
  {$DEFINE Delphi1-7}
  {$DEFINE Delphi2-6}
  {$DEFINE Delphi2-7}
  {$DEFINE Delphi3-6}
  {$DEFINE Delphi3-7}
  {$DEFINE Delphi4-6}
  {$DEFINE Delphi4-7}
  {$DEFINE Delphi5-6}
  {$DEFINE Delphi5-7}
  {$DEFINE Delphi6-7}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE CompiledWithDelphi7}
  {$DEFINE Delphi1orNewerCompiler}
  {$DEFINE Delphi2orNewerCompiler}
  {$DEFINE Delphi3orNewerCompiler}
  {$DEFINE Delphi4orNewerCompiler}
  {$DEFINE Delphi5orNewerCompiler}
  {$DEFINE Delphi6orNewerCompiler}
  {$DEFINE Delphi7orNewerCompiler}
  {$DEFINE Delphi1-7}
  {$DEFINE Delphi2-7}
  {$DEFINE Delphi3-7}
  {$DEFINE Delphi4-7}
  {$DEFINE Delphi5-7}
  {$DEFINE Delphi6-7}
{$ENDIF}
