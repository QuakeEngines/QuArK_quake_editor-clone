/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef PACKAGEFACTORY_H
#define PACKAGEFACTORY_H

#include "stdafx.h"
#include "Package.h"

class HLLIB_API CPackageFactory
{
public:
	static CPackage *Create(DWORD dwPackageType);
	static CPackage *Create(LPCSTR szFileName);
};

#endif