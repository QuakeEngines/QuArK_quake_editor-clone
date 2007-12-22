/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 *
 * Modified 3/3/2005 By Ryan Gregg
 * - Improved MapDataStructures() error checking.
 *
 * Modified 9/4/2004 By Ryan Gregg
 * - Added GetPackageExtension().
 * - Added GetPackageDescription().
 */

#include "PAKFile.h"

CPAKFile::CPAKFile()
{
	this->lpView = 0;
}

CPAKFile::~CPAKFile()
{
	this->Close();
}

DWORD CPAKFile::GetPackageType() const
{
	return HLLIB_PACKAGE_PAK;
}


LPCSTR CPAKFile::GetPackageExtension() const
{
	return ".pak";
}

LPCSTR CPAKFile::GetPackageDescription() const
{
	return "Half-Life Package File";
}

BOOL CPAKFile::MapDataStructures()
{
	if(!this->Mapping->MapView(this->lpView))
	{
		this->SetLastError(this->Mapping->GetLastError());
		return FALSE;
	}

	try
	{
		this->lpPAKBase = this->lpView;

		this->lpPAKHeader = (LPPAKHEADER)this->lpPAKBase;
		this->lpPAKData = (LPVOID)((LPBYTE)this->lpPAKHeader + sizeof(PAKHEADER));
		this->lpPAKDirectoryItems = (LPPAKDIRECTORYITEM)((LPBYTE)this->lpPAKBase + this->lpPAKHeader->DirectoryOffset);

		// Check that we have room to read the header.
		if(sizeof(PAKHEADER) > this->Mapping->GetMappingSize())
		{
			this->SetLastError("Corrupt file.\n\nThe file map is too small for it's header.");
			return FALSE;
		}

		// Check that the header has the right signature.
		if(memcmp(this->lpPAKHeader->Signature, "PACK", 4) != 0)
		{
			this->SetLastError("Invalid file.\n\nThe file's signature does not match.");
			return FALSE;
		}

		// Check that everything fits in the file after we have mapped it.
		if(this->lpPAKHeader->DirectoryOffset + (this->lpPAKHeader->DirectoryLength / sizeof(PAKDIRECTORYITEM)) * sizeof(PAKDIRECTORYITEM) > this->Mapping->GetMappingSize())
		{
			this->SetLastError("Corrupt file.\n\nThe file map is not within file bounds.");
			return FALSE;
		}
	}
	catch(...)
	{
		this->SetLastError("Corrupt file.\n\nThe file map is not within file bounds.");
		return FALSE;
	}

	return TRUE;
}

void CPAKFile::UnmapDataStructures()
{
	this->lpPAKBase = 0;

	this->lpPAKHeader = 0;
	this->lpPAKData = 0;
	this->lpPAKDirectoryItems = 0;

	this->Mapping->UnmapView(this->lpView);
}

CDirectoryFolder *CPAKFile::BuildRoot()
{
	CDirectoryFolder *Root = new CDirectoryFolder("root");

	DWORD dwItemCount = this->lpPAKHeader->DirectoryLength / sizeof(PAKDIRECTORYITEM);

	// Loop through each file in the PAK file.
	for(DWORD i = 0; i < dwItemCount; i++)
	{
		char cFileName[56];
		strcpy(cFileName, this->lpPAKDirectoryItems[i].ItemName);

		// Check if we have just a file, or if the file has directories we need to create.
		if(strchr(cFileName, '/') == 0)
		{
			Root->AddFile(cFileName, i);
		}
		else
		{
			// Tokenize the file path and create the directories.
			CDirectoryFolder *InsertFolder = Root;

			char cTemp[56] = "";
			char *cToken = strtok(cFileName, "/");
			while(cToken != 0)
			{
				strcpy(cTemp, cToken);

				cToken = strtok(0, "/");

				if(cToken != 0)
				{
					// Check if the directory exists.
					CDirectoryItem *TempItem = InsertFolder->GetItem(cTemp);
					if(TempItem == 0 || TempItem->GetType() == DirectoryItemFile)
					{
						// It doesn't, create it.
						InsertFolder = InsertFolder->AddFolder(cTemp);
					}
					else
					{
						// It does, use it.
						InsertFolder = static_cast<CDirectoryFolder *>(TempItem);
					}
				}
			}

			// The file name is the last token, add it.
			InsertFolder->AddFile(cTemp, i);
		}
	}

	return Root;
}

BOOL CPAKFile::GetFileSize(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	dwSize = this->lpPAKDirectoryItems[File->GetID()].ItemLength;

	return TRUE;
}

BOOL CPAKFile::GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	dwSize = this->lpPAKDirectoryItems[File->GetID()].ItemLength;

	return TRUE;
}

BOOL CPAKFile::GetFileData(CDirectoryFile *File, LPBYTE lpData)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	if(lpData == 0)
	{
		this->SetLastError("No data allocated to copy file to.");

		return FALSE;
	}

	// Get the length of the file data.
	DWORD dwLength = this->lpPAKDirectoryItems[File->GetID()].ItemLength;

	// Write the file data.
	memcpy(lpData, (LPBYTE)this->lpPAKBase + this->lpPAKDirectoryItems[File->GetID()].ItemOffset, dwLength);

	return TRUE;
}