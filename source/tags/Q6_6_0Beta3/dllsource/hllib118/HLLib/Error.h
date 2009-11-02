/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef ERROR_H
#define ERROR_H

#include "stdafx.h"

class HLLIB_API CError
{
private:
	LPSTR szLastError;

public:
	CError();
	virtual ~CError();

public:
	LPCSTR GetLastError() const;

protected:
	void SetLastError(LPCSTR szLastError, BOOL bSystemError = FALSE);
};

#endif