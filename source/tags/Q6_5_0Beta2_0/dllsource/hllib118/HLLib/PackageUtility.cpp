/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 * 
 * Modified 9/3/2004 By Ryan Gregg
 * - Added ~CPackageUtility().
 * - Added CallCallBack().
 * - Added GetLastError().
 * - Added SetLastError().
 * - Added callback functions to Extract() functions.
 */

#include "PackageUtility.h"

CPackageUtility::CPackageUtility(CPackage *Package)
{
	this->Package = Package;
}

CPackageUtility::~CPackageUtility()
{

}

//
// GetDirectoryItem()
// Searches the directory tree for a directory item.  szItemPath must
// be the full path to the item.  eg: root\images\wall.bmp
//
CDirectoryItem *CPackageUtility::GetDirectoryItem(LPCSTR szItemPath)
{
	CDirectoryFolder *Folder = this->Package->GetRoot();

	char *cItemPath = new char[strlen(szItemPath) + 1];
	strcpy(cItemPath, szItemPath);

	char *cToken = strtok(cItemPath, "\\/");
	if(cToken == 0 || stricmp(Folder->GetName(), cToken) != 0)
	{
		delete []cItemPath;
		return 0;
	}

	cToken = strtok(0, "\\/");
	while(cToken != 0)
	{
		if(*cToken != 0)
		{
			CDirectoryItem *Item = Folder->GetItem(cToken);

			if(Item == 0)
			{
				delete []cItemPath;
				return 0;
			}

			if(Item->GetType() == DirectoryItemFolder)
			{
				Folder = static_cast<CDirectoryFolder *>(Item);
			}
			else
			{
				delete []cItemPath;
				return Item;
			}
		}

		cToken = strtok(0, "\\/");
	}

	delete []cItemPath;
	return Folder;
}

//
// GetDirectoryItemPath()
// Get the full path of the directory item and store it in szItemPath.  Note
// szItemPath MUST be of sufficient size to hold the path and MUST be an empty string.
//
void CPackageUtility::GetDirectoryItemPath(CDirectoryItem *Item, LPSTR szItemPath)
{
	LPCSTR szItemName = Item->GetName();
	LPSTR szNewItemPath;
	
	if(*szItemPath == '\0')
	{
		szNewItemPath = new char[strlen(szItemName) + 1];
		sprintf(szNewItemPath, "%s", szItemName);
	}
	else
	{
		szNewItemPath = new char[strlen(szItemPath) + 1 + strlen(szItemName) + 1];
		sprintf(szNewItemPath, "%s\\%s", szItemName, szItemPath);
	}

	strcpy(szItemPath, szNewItemPath);
	delete []szNewItemPath;

	if(Item->GetParent() != 0)
	{
		this->GetDirectoryItemPath(Item->GetParent(), szItemPath);
	}
}

//
// GetDirectoryItemSize()
// Return the size of the directory item.  If the directory item is a folder
// the result includes files in subdirectories.
//
DWORD CPackageUtility::GetDirectoryItemSize(CDirectoryItem *Item)
{
	DWORD dwSize = 0;
	if(Item->GetType() == DirectoryItemFolder)
	{
		CDirectoryFolder *Folder = static_cast<CDirectoryFolder *>(Item);

		for(DWORD i = 0; i < Folder->ItemCount(); i++)
		{
			dwSize += this->GetDirectoryItemSize(Folder->GetItem(i));
		}

		return dwSize;
	}
	else
	{
		return this->Package->GetFileSize(static_cast<CDirectoryFile *>(Item), dwSize) ? dwSize : 0;
	}
}

//
// GetDirectoryItemSize()
// Return the size of the directory item on the disk.  If the directory item
// is a folder the result includes files in subdirectories.
//
DWORD CPackageUtility::GetDirectoryItemSizeOnDisk(CDirectoryItem *Item)
{
	DWORD dwSize = 0;
	if(Item->GetType() == DirectoryItemFolder)
	{
		CDirectoryFolder *Folder = static_cast<CDirectoryFolder *>(Item);

		for(DWORD i = 0; i < Folder->ItemCount(); i++)
		{
			dwSize += this->GetDirectoryItemSizeOnDisk(Folder->GetItem(i));
		}

		return dwSize;
	}
	else
	{
		return this->Package->GetFileSizeOnDisk(static_cast<CDirectoryFile *>(Item), dwSize) ? dwSize : 0;
	}
}

//
// GetFolderFileCount()
// Return the number of files in the folder including files in subdirectories.
//
DWORD CPackageUtility::GetFolderFileCount(CDirectoryFolder *Folder)
{
	DWORD dwCount = 0;

	for(DWORD i = 0; i < Folder->ItemCount(); i++)
	{
		CDirectoryItem *Item = Folder->GetItem(i);

		if(Item->GetType() == DirectoryItemFolder)
		{
			dwCount += this->GetFolderFileCount(static_cast<CDirectoryFolder *>(Item));
		}
		else
		{
			dwCount++;
		}
	}

	return dwCount;
}

//
// GetFolderFileCount()
// Return the number of folders in the folder including subdirectories.
//
DWORD CPackageUtility::GetFolderFolderCount(CDirectoryFolder *Folder)
{
	DWORD dwCount = 0;

	for(DWORD i = 0; i < Folder->ItemCount(); i++)
	{
		CDirectoryItem *Item = Folder->GetItem(i);

		if(Item->GetType() == DirectoryItemFolder)
		{
			dwCount += this->GetFolderFolderCount(static_cast<CDirectoryFolder *>(Item)) + 1;
		}
	}

	return dwCount;
}

//
// Extract()
// Extracts a directory item to szFolder.
//
BOOL CPackageUtility::Extract(CDirectoryItem *Item, LPCSTR szFolder, void (*CallBack)(BOOL, LPCSTR))
{
	if(Item->GetType() == DirectoryItemFolder)
	{
		return this->Extract(static_cast<CDirectoryFolder *>(Item), szFolder, CallBack);
	}
	else
	{
		return this->Extract(static_cast<CDirectoryFile *>(Item), szFolder, CallBack);
	}
}

//
// Extract()
// Extracts a file to szFolder.  The file's package name is used.
//
BOOL CPackageUtility::Extract(CDirectoryFile *File, LPCSTR szFolder, void (*CallBack)(BOOL, LPCSTR))
{
	DWORD dwSize;
	LPBYTE lpData;

	// Get the file's size.
	if(!this->Package->GetFileSize(File, dwSize))
	{
		this->SetLastError(this->Package->GetLastError());
		this->CallCallBack(FALSE, File, CallBack);

		return FALSE;
	}

	lpData = new BYTE[dwSize];

	// Get the file's data.
	if(!this->Package->GetFileData(File, lpData))
	{
		delete []lpData;
		this->SetLastError(this->Package->GetLastError());
		this->CallCallBack(FALSE, File, CallBack);

		return FALSE;
	}

	// Create a string holding the new files's name.
	char cName[MAX_PATH];
	char cFileName[MAX_PATH];

	strcpy(cName, File->GetName());
	this->RemoveIllegalCharacters(cName);

	if(szFolder != 0)
	{
		if(_snprintf(cFileName, MAX_PATH, "%s\\%s", szFolder, cName) < 0)
		{
			this->SetLastError("Path exceeds maximum path length.");
			this->CallCallBack(FALSE, File, CallBack);

			return FALSE;
		}
	}
	else
	{
		strcpy(cFileName, cName);
	}

	// Create the file.
	FILE *pFile;
	if((pFile = fopen(cFileName, "wb")) == 0)
	{
		delete []lpData;
		this->SetLastError("fopen() failed.", TRUE);
		this->CallCallBack(FALSE, File, CallBack);

		return FALSE;
	}

	// Write the file's data.
	if(dwSize != 0)
	{
		if(fwrite(lpData, dwSize, 1, pFile) != 1)
		{
			delete []lpData;
			fclose(pFile);
			this->SetLastError("fwrite() failed.", TRUE);
			this->CallCallBack(FALSE, File, CallBack);

			return FALSE;
		}
	}

	// Close the file.
	delete []lpData;
	fclose(pFile);

	this->CallCallBack(TRUE, File, CallBack);

	return TRUE;
}

//
// Extract()
// Extracts a folder to szFolder.  The folders's package name is used and
// the necessary directory structure is created.  The function will continue
// to try and extract the next item if it encounters a failure.
//
BOOL CPackageUtility::Extract(CDirectoryFolder *Folder, LPCSTR szFolder, void (*CallBack)(BOOL, LPCSTR))
{
	// Create a string holding the new folder's name.
	char cName[MAX_PATH];
	char cFolder[MAX_PATH];

	strcpy(cName, Folder->GetName());
	this->RemoveIllegalCharacters(cName);

	// Create path.
	if(szFolder != 0)
	{
		if(_snprintf(cFolder, MAX_PATH, "%s\\%s", szFolder, cName) < 0)
		{
			this->SetLastError("Path exceeds maximum path length.");
			this->CallCallBack(FALSE, Folder, CallBack);

			return FALSE;
		}
	}
	else
	{
		strcpy(cFolder, cName);
	}

	// Create the new folder if it doesn't exist.
	if(!this->DirectoryExists(cFolder))
	{
		if(!CreateDirectory(cFolder, 0))
		{
			this->SetLastError("CreateDirectory() failed.", TRUE);
			this->CallCallBack(FALSE, Folder, CallBack);

			return FALSE;
		}
	}

	// Extract each item in the folder.
	BOOL bResult = TRUE;

	for(DWORD i = 0; i < Folder->ItemCount(); i++)
	{
		bResult &= this->Extract(Folder->GetItem(i), cFolder, CallBack);
	}

	this->CallCallBack(TRUE, Folder, CallBack);

	return bResult;
}

//
// RemoveIllegalCharacters()
// Remove illegal characters from path name.
//
void CPackageUtility::RemoveIllegalCharacters(char *cName)
{
	char cIllegalCharacters[] = { '/', '?', '<', '>', '\\', ':', '*', '|', '"' };

	for(unsigned int i = 0; i < strlen(cName); i++)
	{
		for(unsigned int j = 0; j < 9; j++)
		{
			if(cName[i] == cIllegalCharacters[j])
			{
				// Remove the illegal character.
				for(unsigned int k = i; k < strlen(cName); k++)
				{
					cName[k] = cName[k + 1];
				}
				continue;
			}
		}
	}
}

//
// DirectoryExists()
// Returns whether or not the directory szFolder exists.
//
BOOL CPackageUtility::DirectoryExists(LPCSTR szFolder)
{
	WIN32_FIND_DATA FindData;
	HANDLE Handle = FindFirstFile(szFolder, &FindData);

	if(Handle == INVALID_HANDLE_VALUE)
	{
		return FALSE;
	}
	else
	{
		FindClose(Handle);

		return (FindData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
	}
}

//
// CallCallBack()
// Generates a message to call the callback function with.
//
void CPackageUtility::CallCallBack(BOOL bResult, CDirectoryItem *Item, void (*CallBack)(BOOL, LPCSTR))
{
	if(CallBack != 0)
	{
		char cMessage[512];
		if(bResult)
		{
			sprintf(cMessage, "%s extracted.", Item->GetName());
			CallBack(TRUE, cMessage);
		}
		else
		{
			sprintf(cMessage, "Failed to extract %s. %s", Item->GetName(), this->GetLastError());
			CallBack(FALSE, cMessage);
		}
	}
}