/*
 * HLLib
 * Copyright (C) 2006 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#include "XZPFile.h"

CXZPFile::CXZPFile()
{
	this->lpView = 0;
}

CXZPFile::~CXZPFile()
{
	this->Close();
}

DWORD CXZPFile::GetPackageType() const
{
	return HLLIB_PACKAGE_XZP;
}


LPCSTR CXZPFile::GetPackageExtension() const
{
	return ".xzp";
}

LPCSTR CXZPFile::GetPackageDescription() const
{
	return "XBox Package File";
}

BOOL CXZPFile::MapDataStructures()
{
	if(!this->Mapping->MapView(this->lpView))
	{
		this->SetLastError(this->Mapping->GetLastError());
		return FALSE;
	}

	try
	{
		this->lpXZPBase = this->lpView;

		this->lpXZPHeader = (LPXZPHEADER)this->lpXZPBase;
		this->lpXZPItemData = (LPXZPITEMDATA)((LPBYTE)this->lpXZPHeader + sizeof(XZPHEADER));
		this->lpXZPItemInfo = (LPXZPITEMINFO)((LPBYTE)this->lpXZPBase + this->lpXZPHeader->InfoOffset);
		this->lpXZPFooter = (LPXZPFOOTER)((LPBYTE)this->lpXZPItemInfo + this->lpXZPHeader->InfoSize);

		// Check that we have room to read the header.
		if(sizeof(XZPHEADER) > this->Mapping->GetMappingSize())
		{
			this->SetLastError("Corrupt file.\n\nThe file map is too small for it's header.");
			return FALSE;
		}

		// Check that the header has the right signature.
		if(memcmp(this->lpXZPHeader->Signature, "piZx", 4) != 0)
		{
			this->SetLastError("Invalid file.\n\nThe file's header signature does not match.");
			return FALSE;
		}

		// Check that the header has the right size.
		if(this->lpXZPHeader->HeaderSize != sizeof(XZPHEADER))
		{
			this->SetLastError("Invalid file.\n\nThe file's header size does not match.");
			return FALSE;
		}

		// Check that everything fits in the file after we have mapped it.
		if(this->lpXZPFooter->FileSize > this->Mapping->GetMappingSize())
		{
			this->SetLastError("Corrupt file.\n\nThe file map is not within file bounds.");
			return FALSE;
		}

		// Check that the footer has the right signature.
		if(memcmp(this->lpXZPFooter->Signature, "tFzX", 4) != 0)
		{
			this->SetLastError("Invalid file.\n\nThe file's footer signature does not match.");
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

void CXZPFile::UnmapDataStructures()
{
	this->lpXZPBase = 0;

	this->lpXZPHeader = 0;
	this->lpXZPItemData = 0;
	this->lpXZPItemInfo = 0;
	this->lpXZPFooter = 0;

	this->Mapping->UnmapView(this->lpView);
}

CDirectoryFolder *CXZPFile::BuildRoot()
{
	CDirectoryFolder *Root = new CDirectoryFolder("root");

	// Loop through each file in the XZP file.
	for(DWORD i = 0; i < this->lpXZPHeader->DataCount1; i++)
	{
		// Find it's info (file name).
		for(DWORD j = 0; j < this->lpXZPHeader->InfoCount; j++)
		{
			if(this->lpXZPItemData[i].ID == this->lpXZPItemInfo[j].ID)
			{
				char cFileName[256];
				strcpy(cFileName, (LPCSTR)this->lpXZPBase + this->lpXZPItemInfo[j].NameOffset);

				// Check if we have just a file, or if the file has directories we need to create.
				if(strchr(cFileName, '/') == 0 && strchr(cFileName, '\\') == 0)
				{
					Root->AddFile(cFileName, i);
				}
				else
				{
					// Tokenize the file path and create the directories.
					CDirectoryFolder *InsertFolder = Root;

					char cTemp[256] = "";
					char *cToken = strtok(cFileName, "/\\");
					while(cToken != 0)
					{
						strcpy(cTemp, cToken);

						cToken = strtok(0, "/\\");

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
				break;
			}
		}
	}

	return Root;
}

BOOL CXZPFile::GetFileSize(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	dwSize = this->lpXZPItemData[File->GetID()].ItemSize;

	return TRUE;
}

BOOL CXZPFile::GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	dwSize = this->lpXZPItemData[File->GetID()].ItemSize;

	return TRUE;
}

BOOL CXZPFile::GetFileData(CDirectoryFile *File, LPBYTE lpData)
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

	if(this->lpXZPHeader->Flags != 6)
	{
		this->SetLastError("Compressed XZP content not supported.");

		return FALSE;
	}

	// Get the length of the file data.
	DWORD dwLength = this->lpXZPItemData[File->GetID()].ItemSize;

	// Write the file data.
	memcpy(lpData, (LPBYTE)this->lpXZPBase + this->lpXZPItemData[File->GetID()].ItemOffset, dwLength);

	return TRUE;
}