{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  File:       dxerr8.h                                                      *}
{*  Content:    DirectX Error Library Include File                            *}
{*                                                                            *}
{*  DirectX 8.x Delphi adaptation by Alexey Barkovoy                          *}
{*  E-Mail: directx@clootie.ru                                                *}
{*                                                                            *}
{*  Modified: 19-Jan-2004                                                     *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*    http://clootie.ru                                                       *}
{*    http://sourceforge.net/projects/delphi-dx9sdk                           *}
{*                                                                            *}
{******************************************************************************}
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

unit DXErr8;

interface

{$HPPEMIT '#include "dxerr8.h"'}

uses
  Windows;

(*==========================================================================;
 *
 *
 *  File:   dxerr8.h
 *  Content:    DirectX Error Library Include File
 *
 ****************************************************************************)

//
//  DXGetErrorString8
//
//  Desc:  Converts an DirectX HRESULT to a string
//
//  Args:  HRESULT hr   Can be any error code from
//                      DPLAY D3D8 D3DX8 DMUSIC DSOUND
//
//  Return: Converted string
//

const
  //////////// DLL export definitions ///////////////////////////////////////
  dxerr8dll = 'dxerr81ab.dll';
  {$EXTERNALSYM dxerr8dll}

var
  DXGetErrorString8A: function (hr: HRESULT): PAnsiChar; stdcall;
{$EXTERNALSYM DXGetErrorString8A}
var
  DXGetErrorString8W: function (hr: HRESULT): PWideChar; stdcall;
{$EXTERNALSYM DXGetErrorString8W}

var
  DXGetErrorString8: function (hr: HRESULT): PChar;  stdcall;
{$EXTERNALSYM DXGetErrorString8}

//
//  DXTrace
//
//  Desc:  Outputs a formatted error message to the debug stream
//
//  Args:  CHAR* strFile   The current file, typically passed in using the
//                         __FILE__ macro.
//         DWORD dwLine    The current line number, typically passed in using the
//                         __LINE__ macro.
//         HRESULT hr      An HRESULT that will be traced to the debug stream.
//         CHAR* strMsg    A string that will be traced to the debug stream (may be NULL)
//         BOOL bPopMsgBox If TRUE, then a message box will popup also containing the passed info.
//
//  Return: The hr that was passed in.
//

var
  DXTraceA: function (strFile: PAnsiChar; dwLine: DWORD; hr: HRESULT; strMsg: PAnsiChar; bPopMsgBox: BOOL): HRESULT; stdcall;
{$EXTERNALSYM DXTraceA}
var
  DXTraceW: function (strFile: PWideChar; dwLine: DWORD; hr: HRESULT; strMsg: PWideChar; bPopMsgBox: BOOL): HRESULT; stdcall;
{$EXTERNALSYM DXTraceW}

var
  DXTrace: function (strFile: PChar; dwLine: DWORD; hr: HRESULT; strMsg: PChar; bPopMsgBox: BOOL): HRESULT; stdcall;
{$EXTERNALSYM DXTrace}

//
// Helper macros
//
(*
#if defined(DEBUG) | defined(_DEBUG)
    #define DXTRACE_MSG(str)              DXTrace( __FILE__, (DWORD)__LINE__, 0, str, FALSE )
    #define DXTRACE_ERR(str,hr)           DXTrace( __FILE__, (DWORD)__LINE__, hr, str, TRUE )
    #define DXTRACE_ERR_NOMSGBOX(str,hr)  DXTrace( __FILE__, (DWORD)__LINE__, hr, str, FALSE )
#else
    #define DXTRACE_MSG(str)              (0L)
    #define DXTRACE_ERR(str,hr)           (hr)
    #define DXTRACE_ERR_NOMSGBOX(str,hr)  (hr)
#endif
*)

implementation



var
  HDXErr81ab  : HMODULE;

function LoadDXErr81ab: Boolean;
begin
  HDXErr81ab := LoadLibrary('dxerr81ab.dll');
  If HDXErr81ab = 0 Then
  begin
    Result := False;
    Exit;
  end;
  @DXGetErrorString8W := GetProcAddress(HDXErr81ab, 'DXGetErrorString8W');
  If @DXGetErrorString8W = nil Then
  begin
    Result := False;
    Exit;
  end;
  @DXGetErrorString8 := GetProcAddress(HDXErr81ab, 'DXGetErrorString8A');
  If @DXGetErrorString8 = nil Then
  begin
    Result := False;
    Exit;
  end;
  @DXTraceA := GetProcAddress(HDXErr81ab, 'DXTraceA');
  If @DXTraceA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @DXTraceW := GetProcAddress(HDXErr81ab, 'DXTraceW');
  If @DXTraceW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @DXTrace := GetProcAddress(HDXErr81ab, 'DXTraceA');
  If @DXTrace = nil Then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function UnloadDXErr81ab: Boolean;
begin
  If FreeLibrary(HDXErr81ab) = False Then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

end.

