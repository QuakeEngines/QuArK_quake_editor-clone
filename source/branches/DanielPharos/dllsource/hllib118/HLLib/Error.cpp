/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 *
 * Modified 11/21/2004 By Ryan Gregg
 * - SetLastError() can now concat system error messages
 *   onto set error messages.
 */

#include "Error.h"

#include <stdio.h>

CError::CError()
{
	this->szLastError = 0;
}

CError::~CError()
{
	delete []this->szLastError;
}

//
// GetLastError()
// Returns the last error encountered by an object.  If a
// function fails this may tell you why.
//
LPCSTR CError::GetLastError() const
{
	return this->szLastError != 0 ? this->szLastError : "";
}

//
// SetLastError()
// Sets the last error encountered by an object.
//
void CError::SetLastError(LPCSTR szLastError, BOOL bSystemError)
{
	delete []this->szLastError;
	if(szLastError != 0)
	{
		if(bSystemError)
		{
			CHAR cBuffer[512]; 
			LPVOID lpMessage;
			DWORD dwLastError = ::GetLastError(); 

			if(FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM, NULL, dwLastError, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)&lpMessage, 0, NULL))
			{
				sprintf(cBuffer, "\n\nSystem Error: 0x%.8x:\n%s", dwLastError, lpMessage); 
 
				LocalFree(lpMessage);
			}
			else
			{
				sprintf(cBuffer, "\n\nSystem Error: 0x%.8x.", dwLastError); 
			}

			this->szLastError = new char[strlen(szLastError) + strlen(cBuffer) + 1];
			sprintf(this->szLastError, "%s%s", szLastError, cBuffer);
		}
		else
		{
			this->szLastError = new char[strlen(szLastError) + 1];
			strcpy(this->szLastError, szLastError);
		}
	}
	else
	{
		this->szLastError = 0;
	}
}