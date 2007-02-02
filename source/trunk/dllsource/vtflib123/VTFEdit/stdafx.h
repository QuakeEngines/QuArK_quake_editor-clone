/*
 * VTFEdit
 * Copyright (C) 2005 Neil Jedrzejewski & Ryan Gregg
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

#pragma once

#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <tchar.h>
#include <richedit.h>

#include "..\lib\VTFLib.h"
#pragma comment(lib, "..\\lib\\VTFLib.lib")

#include "..\lib\HLLib.h"
#pragma comment(lib, "..\\lib\\HLLib.lib")

#include "..\lib\il.h"
#pragma comment(lib, "..\\lib\\DevIL.lib")

#ifdef CreateDirectory
#undef CreateDirectory
#endif

#ifdef GetObject
#undef GetObject
#endif

#ifdef GetTempPath
#undef GetTempPath
#endif

#ifdef MessageBox
#undef MessageBox
#endif