(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) Armin Rigo

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

http://www.planetquake.com/quark - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.5  2001/06/05 18:43:29  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.4  2001/03/20 21:34:48  decker_dk
Updated copyright-header
}

unit PyMenus;

interface

uses Windows, SysUtils, Classes, Python, PyForms;

const
 wID_HelpMenu    = 32100;
 wID_ToolboxMenu = 32200;

type
 TOldMenuItemInfo = record
                     cbSize, fMask, fType, fState, wID: LongInt;
                     hSubMenu: HMenu;
                     hbmpChecked, hbmpUnchecked: HBitmap;
                     dwItemData: LongInt;
                     dwTypeData: PChar;
                     cch: Integer;
                    end;

function UpdateMenu(Menu: HMenu; List: PyObject; Callbacks: TList; FreeUnused: Boolean; sc, sci: PyObject; MenuBar, Recursive: Boolean) : Boolean;
function FindMenuItem(Menu, PopupMenu: HMenu; var Info: TOldMenuItemInfo) : Boolean;

implementation

uses Quarkx, Qk1, QkObjects, PyImages, Travail;

const
 state_Normal = 0;
 state_Checked = 2;
 state_RadioCheck = 3;
 state_Disabled = 4;
 state_Default = 8;
 state_CheckMask = $03;

function UpdateMenu(Menu: HMenu; List: PyObject; Callbacks: TList; FreeUnused: Boolean; sc, sci: PyObject; MenuBar, Recursive: Boolean) : Boolean;
const
 ExtraCount = 2;
var
 Tags: TBits;
 I, J, Count, MenuCount, TagCount: Integer;
 Info, nInfo: TOldMenuItemInfo;
 Z, nZ: array[0..255] of Char;
 ListItem, obj, textobj: PyObject;
 IsPopup, FillRec: Boolean;
 PopupHandle: HMenu;
 P: PChar;
 Hourglass: Boolean;
begin
 Result:=False;
 Count:=PyObject_Length(List);
 if Count<0 then Exit;
 {$IFDEF Debug}
 if not IsMenu(Menu) then
  Raise InternalE('UpdateMenu: not a menu handle');
 {$ENDIF}
 if MenuBar then
  begin
   if Count>0 then
    begin
     obj:=PyList_GetItem(List, Count-1);
     if obj=Nil then Exit;
    end
   else
    obj:=Nil;
   if obj<>HelpMenu then
    begin
     PyList_Append(List, HelpMenu);
     Inc(Count);
    end;
  end;
 PopupHandle:=0;
 Hourglass:=False;
 try
  MenuCount:=GetMenuItemCount(Menu);
  while MenuCount>Count do
   begin
    Dec(MenuCount);
    DeleteMenu(Menu, MenuCount, mf_ByPosition);
    Result:=True;
   end;
  FillChar(nInfo, SizeOf(nInfo), 0);
  nInfo.cbSize:=SizeOf(nInfo);

  Tags:=Nil; try
  if FreeUnused then
   begin
    TagCount:=Callbacks.Count;
    Tags:=TBits.Create;
    Tags.Size:=TagCount;
   end
  else
   TagCount:=0;

  for I:=0 to Count-1 do
   begin
    ListItem:=PySequence_GetItem(List, I);
    if ListItem=Nil then Exit;
    textobj:=Nil;
    try

     FillRec:=False;
     if ListItem = Py_None then
      begin
       nInfo.fMask:=MIIM_TYPE or MIIM_ID;
       nInfo.fType:=MFT_SEPARATOR;
       nInfo.wID:=-1;
       IsPopup:=False;
      end
     else
      begin
       nInfo.fMask:=MIIM_TYPE or MIIM_STATE or MIIM_ID or MIIM_SUBMENU or MIIM_CHECKMARKS;
       nInfo.fType:=MFT_STRING;
       nInfo.hSubMenu:=0;
       nInfo.hbmpChecked:=0;
       nInfo.hbmpUnchecked:=0;
       if ListItem = HelpMenu then
        begin
         StrPCopy(nZ, LoadStr1(4));
         nInfo.dwTypeData:=nZ;
         nInfo.fState:=0;
         nInfo.fType:=MFT_STRING {or MFT_RIGHTJUSTIFY};
         nInfo.wID:=wID_HelpMenu;
         {nInfo.hSubMenu:=g_Form1.HelpMenu.Handle;}
         IsPopup:=True;
        end
       else
        if ListItem = ToolboxMenu then
         begin
          StrPCopy(nZ, LoadStr1(5));
          nInfo.dwTypeData:=nZ;
          nInfo.fState:=0;
          nInfo.fType:=MFT_STRING;
          nInfo.wID:=wID_ToolboxMenu;
          {nInfo.hSubMenu:=g_Form1.WindowMenu.Handle;}
          IsPopup:=True;
         end
        else
         begin
          textobj:=PyObject_GetAttrString(ListItem, 'text');
          if textobj=Nil then Exit;
          nInfo.dwTypeData:=PyString_AsString(textobj);
          if nInfo.dwTypeData=Nil then Exit;
          if PySequence_In(sci, ListItem)>0 then
           begin
            obj:=PySequence_GetItem(sc, PySequence_Index(sci, ListItem));
            if obj=Nil then Exit;
            try
             P:=PyString_AsString(obj);
             if P=Nil then Exit;
             nInfo.dwTypeData:=StrECopy(nZ, nInfo.dwTypeData);
             nInfo.dwTypeData^:=Chr(vk_Tab);
             StrCopy(nInfo.dwTypeData+1, P);
             nInfo.dwTypeData:=nZ;
            finally
             Py_DECREF(obj);
            end;
           end;
          nInfo.fType:=MFT_STRING;

          IsPopup:=PyObject_HasAttrString(ListItem, 'items');
          FillRec:=IsPopup;

          obj:=PyObject_GetAttrString(ListItem, 'state');
          if obj=Nil then Exit;
          J:=PyInt_AsLong(obj);
          Py_DECREF(obj);
          if J and state_Disabled <> 0 then
           nInfo.fState:=MFS_GRAYED
          else
           nInfo.fState:=0;
          if J and state_Default <> 0 then
           nInfo.fState:=nInfo.fState or MFS_DEFAULT;
          if J and state_CheckMask <> 0 then
           nInfo.fState:=nInfo.fState or MFS_CHECKED;
          if J and state_CheckMask = state_RadioCheck then
           nInfo.fType:=nInfo.fType or MFT_RADIOCHECK;

          if PyObject_HasAttrString(ListItem, 'menuicon') then
           begin
            obj:=PyObject_GetAttrString(ListItem, 'menuicon');
            if obj=Nil then Exit;
            try
             if obj^.ob_type = @TyImage1_Type then
              begin
               nInfo.hbmpUnchecked:=PyImage1(obj)^.GetMenuBitmap;
               nInfo.hbmpChecked:=nInfo.hbmpUnchecked;
              end;
            finally Py_DECREF(obj); end;
           end;

          J:=Callbacks.IndexOf(ListItem);
          if J<0 then
           begin
            J:=Callbacks.IndexOf(Nil);
            if J<0 then
             J:=Callbacks.Add(ListItem)
            else
             Callbacks[J]:=ListItem;
            Py_INCREF(ListItem);
           end;
          if J<TagCount then
           Tags[J]:=True;
          nInfo.wID:=J+1;
         end;
      end;

     if I<MenuCount then
      begin
       FillChar(Info, SizeOf(Info), 0);
       Info.cbSize:=SizeOf(Info);
       Info.fMask:=MIIM_TYPE or MIIM_STATE or MIIM_ID or MIIM_SUBMENU or MIIM_CHECKMARKS;
       Info.dwTypeData:=Z;
       Info.cch:=SizeOf(Z);
       GetMenuItemInfo(Menu, I, True, PMenuItemInfo(@Info)^);
       if (nInfo.fType = Info.fType) and (nInfo.fState = Info.fState) and (nInfo.wID = Info.wID)
       and (StrComp(Z, nInfo.dwTypeData) = 0) and (IsPopup = (Info.hSubMenu<>0))
       and ((nInfo.hSubMenu=0) or (nInfo.hSubMenu=Info.hSubMenu))
       and (nInfo.hbmpUnchecked=Info.hbmpChecked) then
        Continue;
       if (nInfo.hSubMenu=0) and IsPopup then
        if Info.hSubMenu<>0 then
         nInfo.hSubMenu:=Info.hSubMenu
        else
         nInfo.hSubMenu:=CreatePopupMenu;
       PopupHandle:=nInfo.hSubMenu;
       SetMenuItemInfo(Menu, I, True, PMenuItemInfo(@nInfo)^);
       Result:=True;
      end
     else
      begin
       if (nInfo.hSubMenu=0) and IsPopup then
        nInfo.hSubMenu:=CreatePopupMenu;
       PopupHandle:=nInfo.hSubMenu;
       InsertMenuItem(Menu, I, True, PMenuItemInfo(@nInfo)^);
       Result:=True;
      end;

     if FillRec and Recursive then
      begin
       obj:=PyObject_GetAttrString(ListItem, 'items');
       if obj<>Nil then
        try
         if not Hourglass then
          begin
           ProgressIndicatorStart(0,0);
           Hourglass:=True;
          end;
         UpdateMenu(PopupHandle, obj, Callbacks, FreeUnused, sc, sci, MenuBar, True);
        finally
         Py_DECREF(obj);
        end;
      end;

    finally
     Py_XDECREF(textobj);
     Py_DECREF(ListItem);
    end;
   end;

  for J:=TagCount-1 downto 0 do
   if not Tags[J] then
    begin
     obj:=PyObject(Callbacks[J]);
     if obj<>Nil then
      begin
       Callbacks[J]:=Nil;
       Py_DECREF(obj);
      end;
    end;
  finally Tags.Free; end;
 finally
  if Hourglass then
   ProgressIndicatorStop;
 {Py_XDECREF(HelpMenu);}
 end;
end;

function FindMenuItem(Menu, PopupMenu: HMenu; var Info: TOldMenuItemInfo) : Boolean;
var
 I: Integer;
begin
 for I:=0 to GetMenuItemCount(Menu)-1 do
  begin
   FillChar(Info, SizeOf(Info), 0);
   Info.cbSize:=SizeOf(Info);
   Info.fMask:=MIIM_SUBMENU or MIIM_ID;
   GetMenuItemInfo(Menu, I, True, PMenuItemInfo(@Info)^);
   if (Info.hSubMenu = PopupMenu)
   or ((Info.hSubMenu<>0) and FindMenuItem(Info.hSubMenu, PopupMenu, Info)) then
    begin
     Result:=True;
     Exit;
    end;
  end;
 Result:=False;
end;

end.
