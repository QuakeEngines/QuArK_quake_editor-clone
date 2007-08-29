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
 * Modified 2/4/2005 By Ryan Gregg
 * - Added GetFileExists().
 * - Modified to work with empty files.
 *
 * Modified 9/9/2004 By Ryan Gregg
 * - Fixed GetFileSizeOnDisk().
 *
 * Modified 9/4/2004 By Ryan Gregg
 * - Added GetPackageExtension().
 * - Added GetPackageDescription().
 * - Added GetPackageBlockCount();
 * - Added GetPackageUsedBlockCount().
 *
 * Modified 9/3/2004 By Ryan Gregg
 * - Added GetItemsFlags().
 * - Added GetPackageVersion();
 */

#include "GCFFile.h"

CGCFFile::CGCFFile()
{
	this->lpHeaderView = 0;
	this->lpDataView = 0;
}

CGCFFile::~CGCFFile()
{
	this->Close();
}

DWORD CGCFFile::GetPackageType() const
{
	return HLLIB_PACKAGE_GCF;
}

LPCSTR CGCFFile::GetPackageExtension() const
{
	return ".gcf";
}

LPCSTR CGCFFile::GetPackageDescription() const
{
	return "Half-Life Game Cache File";
}

DWORD CGCFFile::GetPackageVersion()
{
	return this->lpGCFHeader->GCFVersion;
}

DWORD CGCFFile::GetPackageBlockCount()
{
	return this->lpGCFDataBlockHeader->BlockCount;
}

DWORD CGCFFile::GetPackageUsedBlockCount()
{
	return this->lpGCFDataBlockHeader->BlocksUsed;
}

BOOL CGCFFile::MapDataStructures()
{
	DWORD dwHeaderSize = this->GetPackageSize() < 16777216 ? this->GetPackageSize() : 16777216;

	// Calculating how large the header of GCF file is so we can map
	// it to memory is a pain in the ass so I use a fixed size of 16MB.
	// This should be sufficient for a 4GB file without causing file
	// mapping problems.  This is a hack.

	if(!this->Mapping->MapView(this->lpHeaderView, dwHeaderSize))
	{
		this->SetLastError(this->Mapping->GetLastError());
		return FALSE;
	}

	try
	{
		this->lpGCFBase = this->lpHeaderView;

		this->lpGCFHeader = (LPGCFHEADER)this->lpGCFBase;

		if(sizeof(GCFHEADER) > this->Mapping->GetMappingSize())
		{
			this->SetLastError("Corrupt file.\n\nThe file map is too small for it's header.");
			return FALSE;
		}

		// Make sure we have a supported version.
		if(this->lpGCFHeader->GCFVersion != 3 && this->lpGCFHeader->GCFVersion != 5 && this->lpGCFHeader->GCFVersion != 6)
		{
			char cError[512];
			sprintf(cError, "Invalid GCF version number (v%d).\n\nYou have a version of a GCF file that HLLib does not know how to read.  Check for product updates.", this->lpGCFHeader->GCFVersion);
			this->SetLastError(cError);

			return FALSE;
		}

		this->lpGCFBlockEntryHeader = (LPGCFBLOCKENTRYHEADER)((LPBYTE)this->lpGCFHeader + sizeof(GCFHEADER));
		this->lpGCFBlockEntries = (LPGCFBLOCKENTRY)((LPBYTE)this->lpGCFBlockEntryHeader + sizeof(GCFBLOCKENTRYHEADER));

		this->lpGCFFragMapHeader = (LPGCFFRAGMAPHEADER)((LPBYTE)this->lpGCFBlockEntries + sizeof(GCFBLOCKENTRY) * this->lpGCFBlockEntryHeader->BlockCount);
		this->lpGCFFragMap = (LPGCFFRAGMAP)((LPBYTE)this->lpGCFFragMapHeader + sizeof(GCFFRAGMAPHEADER));

		// The BlockEntryMap section was removed in the June 21, 2004 Steam update (v6).
		switch(this->lpGCFHeader->GCFVersion)
		{
		case 3:
		case 5:
			this->lpGCFBlockEntryMapHeader = (LPGCFBLOCKENTRYMAPHEADER)((LPBYTE)this->lpGCFFragMap + sizeof(GCFFRAGMAP) * this->lpGCFFragMapHeader->BlockCount);
			this->lpGCFBlockEntryMap = (LPGCFBLOCKENTRYMAP)((LPBYTE)this->lpGCFBlockEntryMapHeader + sizeof(GCFBLOCKENTRYMAPHEADER));

			this->lpGCFDirectoryHeader = (LPGCFDIRECTORYHEADER)((LPBYTE)this->lpGCFBlockEntryMap + sizeof(GCFBLOCKENTRYMAP) * this->lpGCFBlockEntryMapHeader->BlockCount);
			break;
		case 6:
			this->lpGCFBlockEntryMapHeader = 0;
			this->lpGCFBlockEntryMap = 0;

			this->lpGCFDirectoryHeader = (LPGCFDIRECTORYHEADER)((LPBYTE)this->lpGCFFragMap + sizeof(GCFFRAGMAP) * this->lpGCFFragMapHeader->BlockCount);
			break;
		}

		this->lpGCFDirectoryEntries = (LPGCFDIRECTORYENTRY)((LPBYTE)this->lpGCFDirectoryHeader + sizeof(GCFDIRECTORYHEADER));

		this->lpGCFDirectoryNames = (LPVOID)((LPBYTE)this->lpGCFDirectoryEntries + sizeof(GCFDIRECTORYENTRY) * this->lpGCFDirectoryHeader->ItemCount);

		this->lpGCFDirectoryInfo1Entries = (LPGCFDIRECTORYINFO1ENTRY)((LPBYTE)this->lpGCFDirectoryNames + this->lpGCFDirectoryHeader->NameSize);
		this->lpGCFDirectoryInfo2Entries = (LPGCFDIRECTORYINFO2ENTRY)((LPBYTE)this->lpGCFDirectoryInfo1Entries + sizeof(GCFDIRECTORYINFO1ENTRY) * this->lpGCFDirectoryHeader->Info1Count);

		this->lpGCFDirectoryCopyEntries = (LPGCFDIRECTORYCOPYENTRY)((LPBYTE)this->lpGCFDirectoryInfo2Entries + sizeof(GCFDIRECTORYINFO2ENTRY) * this->lpGCFDirectoryHeader->ItemCount);
		this->lpGCFDirectoryLocalEntries = (LPGCFDIRECTORYLOCALENTRY)((LPBYTE)this->lpGCFDirectoryCopyEntries + sizeof(GCFDIRECTORYCOPYENTRY) * this->lpGCFDirectoryHeader->CopyCount);

		switch(this->lpGCFHeader->GCFVersion)
		{
		case 3:
			this->lpGCFDirectoryMapHeader = 0;
			this->lpGCFDirectoryMapEntries = (LPGCFDIRECTORYMAPENTRY)((LPBYTE)this->lpGCFDirectoryHeader + this->lpGCFDirectoryHeader->DirectorySize);
			break;
		case 5:
		case 6:
			this->lpGCFDirectoryMapHeader = (LPGCFDIRECTORYMAPHEADER)((LPBYTE)this->lpGCFDirectoryHeader + this->lpGCFDirectoryHeader->DirectorySize);
			this->lpGCFDirectoryMapEntries = (LPGCFDIRECTORYMAPENTRY)((LPBYTE)this->lpGCFDirectoryMapHeader + sizeof(GCFDIRECTORYMAPHEADER));
			break;
		}

		this->lpGCFChecksumHeader = (LPGCFCHECKSUMHEADER)((LPBYTE)this->lpGCFDirectoryMapEntries + sizeof(GCFDIRECTORYMAPENTRY) * this->lpGCFDirectoryHeader->ItemCount);
		this->lpGCFChecksumMapHeader = (LPGCFCHECKSUMMAPHEADER)((LPBYTE)(this->lpGCFChecksumHeader) + sizeof(GCFCHECKSUMHEADER));

		this->lpGCFChecksumMapEntries = (LPGCFCHECKSUMMAPENTRY)((LPBYTE)(this->lpGCFChecksumMapHeader) + sizeof(GCFCHECKSUMMAPHEADER));
		this->lpGCFChecksumEntries = (LPGCFCHECKSUMENTRY)((LPBYTE)(this->lpGCFChecksumMapEntries) + sizeof(GCFCHECKSUMMAPENTRY) * this->lpGCFChecksumMapHeader->ItemCount);

		switch(this->lpGCFHeader->GCFVersion)
		{
		case 3:
			// In version 3 the GCFDATABLOCKHEADER is missing the LastVersionPlayed field.
			// The below hack makes the file map correctly.
			this->lpGCFDataBlockHeader = (LPGCFDATABLOCKHEADER)((LPBYTE)this->lpGCFChecksumMapHeader + this->lpGCFChecksumHeader->ChecksumSize - sizeof(DWORD));
			break;
		case 5:
		case 6:
			this->lpGCFDataBlockHeader = (LPGCFDATABLOCKHEADER)((LPBYTE)this->lpGCFChecksumMapHeader + this->lpGCFChecksumHeader->ChecksumSize);
			break;
		}

		LPVOID lpGCFDataBlocks = (LPVOID)((LPBYTE)this->lpGCFBase + this->lpGCFDataBlockHeader->FirstBlockOffset);

		// Make sure our file maps out correctly.
		if((LPBYTE)lpGCFDataBlocks + this->lpGCFDataBlockHeader->BlockCount * this->lpGCFDataBlockHeader->BlockSize > (LPBYTE)this->lpGCFBase + this->Mapping->GetMappingSize())
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

void CGCFFile::UnmapDataStructures()
{
	this->lpGCFBase = 0;

	this->lpGCFHeader = 0;

	this->lpGCFBlockEntryHeader = 0;
	this->lpGCFBlockEntries = 0;

	this->lpGCFFragMapHeader = 0;
	this->lpGCFFragMap = 0;

	this->lpGCFBlockEntryMapHeader = 0;
	this->lpGCFBlockEntryMap = 0;

	this->lpGCFDirectoryHeader = 0;
	this->lpGCFDirectoryEntries = 0;
	this->lpGCFDirectoryNames = 0;
	this->lpGCFDirectoryInfo1Entries = 0;
	this->lpGCFDirectoryInfo2Entries = 0;
	this->lpGCFDirectoryCopyEntries = 0;
	this->lpGCFDirectoryLocalEntries = 0;

	this->lpGCFDirectoryMapHeader = 0;
	this->lpGCFDirectoryMapEntries = 0;

	this->lpGCFChecksumHeader = 0;
	this->lpGCFChecksumMapHeader = 0;
	this->lpGCFChecksumMapEntries = 0;
	this->lpGCFChecksumEntries = 0;

	this->lpGCFDataBlockHeader = 0;

	this->Mapping->UnmapView(this->lpHeaderView);
	this->Mapping->UnmapView(this->lpDataView);
}

CDirectoryFolder *CGCFFile::BuildRoot()
{
	CDirectoryFolder *Root = new CDirectoryFolder(this->GetDirectoryItemName(0), 0);

	this->BuildFolder(Root);

	return Root;
}

//
// BuildFolder()
// Creates the directory tree for Folder.
//
void CGCFFile::BuildFolder(CDirectoryFolder *Folder)
{
	// Get the first directory item.
	DWORD dwNext = this->lpGCFDirectoryEntries[Folder->GetID()].FirstIndex;

	// Loop through directory items.
	while(dwNext)
	{
		// Check if the item is a folder.
		if(this->lpGCFDirectoryEntries[dwNext].DirectoryType == 0x00000000)
		{
			// Add the directory item to the current folder.
			CDirectoryFolder *NewFolder = Folder->AddFolder(this->GetDirectoryItemName(dwNext), dwNext);

			// Build the new folder.
			this->BuildFolder(NewFolder);
		}
		else
		{
			// Add the directory item to the current folder.
			Folder->AddFile(this->GetDirectoryItemName(dwNext), dwNext);
		}

		// Get the next directory item.
		dwNext = this->lpGCFDirectoryEntries[dwNext].NextIndex;
	}
}

BOOL CGCFFile::GetItemsFlags(CDirectoryItem *Item, DWORD &dwFlags)
{
	if(Item->GetID() == HLLIB_INVALID_ID)
	{
		dwFlags = 0;
		this->SetLastError("Invalid item ID.");

		return FALSE;
	}

	dwFlags = this->lpGCFDirectoryEntries[Item->GetID()].DirectoryType;

	return TRUE;
}

BOOL CGCFFile::GetFileExists(CDirectoryFile *File, BOOL &bExists)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		bExists = FALSE;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	DWORD dwID = File->GetID();

	bExists = this->lpGCFDirectoryEntries[dwID].ItemSize == 0 || this->lpGCFDirectoryMapEntries[dwID].FirstBlockIndex != this->lpGCFBlockEntryHeader->BlockCount ? TRUE : FALSE;

	return TRUE;
}

BOOL CGCFFile::GetFileSize(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	dwSize = this->lpGCFDirectoryEntries[File->GetID()].ItemSize;

	return TRUE;
}

BOOL CGCFFile::GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	DWORD dwID = File->GetID();

	if(this->lpGCFDirectoryMapEntries[dwID].FirstBlockIndex == this->lpGCFBlockEntryHeader->BlockCount)
	{
		dwSize = 0;
	}
	else
	{
		dwSize = this->lpGCFDirectoryEntries[dwID].ItemSize + (this->lpGCFDataBlockHeader->BlockSize - this->lpGCFDirectoryEntries[dwID].ItemSize % this->lpGCFDataBlockHeader->BlockSize);
	}

	return TRUE;
}

BOOL CGCFFile::GetFileData(CDirectoryFile *File, LPBYTE lpData)
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

	DWORD dwID = File->GetID();

	// File is empty.
	if(this->lpGCFDirectoryEntries[dwID].ItemSize == 0)
	{
		return TRUE;
	}

	// Get the first data block.
	DWORD dwBlockEntryIndex = this->lpGCFDirectoryMapEntries[dwID].FirstBlockIndex;

	// Check if the GCF file has any data for this file.
	if(dwBlockEntryIndex == this->lpGCFBlockEntryHeader->BlockCount)
	{
		this->SetLastError("File contains no physical data to write.");

		return FALSE;
	}

	DWORD dwCurrentDataSize = 0;
	DWORD dwTotalDataSize = this->lpGCFDirectoryEntries[dwID].ItemSize;

	// Loop through each data block.
	while(TRUE)
	{
		// Get the first data block fragment.
		DWORD dwDataBlockIndex = this->lpGCFBlockEntries[dwBlockEntryIndex].FirstDataBlockIndex;
		DWORD dwDataBlockSize = this->lpGCFBlockEntries[dwBlockEntryIndex].FileDataSize;

		// Loop through each data block fragment.
		while(dwDataBlockSize > 0)
		{
			// Map the fragment to memory.
			LPBYTE lpDataBlock = this->GetDataBlock(dwDataBlockIndex);

			// Check if the fragment mapped.
			if(lpDataBlock == 0)
			{
				return FALSE;
			}

			// Compute the size of the fragment.
			DWORD dwFragmentSize = dwDataBlockSize >= this->lpGCFDataBlockHeader->BlockSize ? this->lpGCFDataBlockHeader->BlockSize : dwDataBlockSize;

			// Write the fragment to the file.
			memcpy(lpData + dwCurrentDataSize, lpDataBlock, dwFragmentSize);

			dwCurrentDataSize += dwFragmentSize;

			// Get the next data block fragment.
			dwDataBlockIndex = this->lpGCFFragMap[dwDataBlockIndex].NextDataBlockIndex;
			dwDataBlockSize -= dwFragmentSize;
		}

		// Check to see if we have written all data blocks.
		if(this->lpGCFBlockEntries[dwBlockEntryIndex].NextBlockEntryIndex == this->lpGCFBlockEntryHeader->BlockCount)
		{
			break;
		}

		// Get the next data block.
		dwBlockEntryIndex = this->lpGCFBlockEntries[dwBlockEntryIndex].NextBlockEntryIndex;
	}

	// Check to see if we wrote the whole file (the algorithm map have failed).
	if(dwCurrentDataSize != dwTotalDataSize)
	{
		this->SetLastError("Failed to write all physical data.");

		return FALSE;
	}

	return TRUE;
}

//
// GetDirectoryItemName()
// Returns the name of the directory item dwDirectoryItemIndex.
//
LPCSTR CGCFFile::GetDirectoryItemName(DWORD dwDirectoryItemIndex) const
{
	// The root directory item doesn't have a name, give it one.
	if(dwDirectoryItemIndex == 0)
		return "root";
	else
		return (LPCSTR)((LPBYTE)this->lpGCFDirectoryNames + this->lpGCFDirectoryEntries[dwDirectoryItemIndex].NameOffset);
}

//
// GetDataBlock()
// Maps the data block dwDataBlockIndex to memory and
// returns it's data.
//
LPCBYTE CGCFFile::GetDataBlock(DWORD dwDataBlockIndex)
{
	LPVOID lpData;
	if(!this->Mapping->MapView(this->lpDataView, lpData, this->lpGCFDataBlockHeader->FirstBlockOffset + dwDataBlockIndex * this->lpGCFDataBlockHeader->BlockSize, this->lpGCFDataBlockHeader->BlockSize))
	{
		this->SetLastError(this->Mapping->GetLastError());
		return NULL;
	}

	return (LPCBYTE)lpData;
}