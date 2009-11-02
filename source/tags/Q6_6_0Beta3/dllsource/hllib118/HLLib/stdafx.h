/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifdef HLLIB_EXPORTS
#define HLLIB_API __declspec(dllexport)
#else
#define HLLIB_API __declspec(dllimport)
#endif

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define LPCBYTE const LPBYTE