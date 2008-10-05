/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef GCFFILE_H
#define GCFFILE_H

#include "MappedPackage.h"

#define HLLIB_PACKAGE_GCF 2

#define HLLIB_GCF_FLAG_FILE						0x00004000	// The item is a file.
#define HLLIB_GCF_FLAG_ENCRYPTED				0x00000100	// The item is encrypted.
#define HLLIB_GCF_FLAG_BACKUP					0x00000040	// Backup the item before overwriting it.
#define HLLIB_GCF_FLAG_LOCAL_COPY				0x0000000a	// The item is to be copied to the disk.
#define HLLIB_GCF_FLAG_LOCAL_COPY_NO_OVERWRITE 	0x00000001	// Don't overwrite the item if copying it to the disk and the item already exists.

class HLLIB_API CGCFFile : public CMappedPackage
{
private:
// Pack structures to align with each DWORD.
#pragma pack(4)

	typedef struct tagGCFHEADER
	{
		DWORD Dummy0;		// Always 0x00000001
		DWORD Dummy1;		// Always 0x00000001
		DWORD GCFVersion;	// GCF version number.
		DWORD CacheID;
		DWORD LastVersionPlayed;
		DWORD Dummy3;
		DWORD Dummy4;
		DWORD FileSize;		// Total size of GCF file in bytes.
		DWORD BlockSize;	// Size of each data block in bytes.
		DWORD BlockCount;	// Number of data blocks.
		DWORD Dummy5;
	} GCFHEADER, *LPGCFHEADER;

	typedef struct tagGCFBLOCKENTRYHEADER
	{
		DWORD BlockCount;	// Number of data blocks.
		DWORD BlocksUsed;	// Number of data blocks that point to data.
		DWORD Dummy0;
		DWORD Dummy1;
		DWORD Dummy2;
		DWORD Dummy3;
		DWORD Dummy4;
		DWORD Checksum;		// Header checksum.
	} GCFBLOCKENTRYHEADER, *LPGCFBLOCKENTRYHEADER;

	typedef struct tagGCFBLOCKENTRY
	{
		DWORD EntryType;				// Flags for the block entry.  0x200F0000 == Not used.
		DWORD FileDataOffset;			// The offset for the data contained in this block entry in the file.
		DWORD FileDataSize;				// The length of the data in this block entry.
		DWORD FirstDataBlockIndex;		// The index to the first data block of this block entry's data.
		DWORD NextBlockEntryIndex;		// The next block entry in the series.  (N/A if == BlockCount.)
		DWORD PreviousBlockEntryIndex;	// The previous block entry in the series.  (N/A if == BlockCount.)
		DWORD DirectoryIndex;			// The index of the block entry in the directory.
	} GCFBLOCKENTRY, *LPGCFBLOCKENTRY;

	typedef struct tagGCFFRAGMAPHEADER
	{
		DWORD BlockCount;	// Number of data blocks.
		DWORD Dummy0;
		DWORD Dummy1;
		DWORD Checksum;		// Header checksum.
	} GCFFRAGMAPHEADER, *LPGCFFRAGMAPHEADER;

	typedef struct tagGCFFRAGMAP
	{
		DWORD NextDataBlockIndex;		// The index of the next data block.
	} GCFFRAGMAP, *LPGCFFRAGMAP;

	// The below section is part of version 5 but not version 6.

	typedef struct tagGCFBLOCKENTRYMAPHEADER
	{
		DWORD BlockCount;			// Number of data blocks.	
		DWORD FirstBlockEntryIndex;	// Index of the first block entry.
		DWORD LastBlockEntryIndex;	// Index of the last block entry.
		DWORD Dummy0;
		DWORD Checksum;				// Header checksum.
	} GCFBLOCKENTRYMAPHEADER, *LPGCFBLOCKENTRYMAPHEADER;

	typedef struct tagGCFBLOCKENTRYMAP
	{
		DWORD PreviousBlockEntryIndex;	// The previous block entry.  (N/A if == BlockCount.)
		DWORD NextBlockEntryIndex;		// The next block entry.  (N/A if == BlockCount.)
	} GCFBLOCKENTRYMAP, *LPGCFBLOCKENTRYMAP;

	// End section.

	typedef struct tagGCFDIRECTORYHEADER
	{
		DWORD Dummy0;				// Always 0x00000004
		DWORD CacheID;				// Cache ID.
		DWORD LastVersionPlayed;	// GCF file version.
		DWORD ItemCount;			// Number of items in the directory.	
		DWORD FileCount;			// Number of files in the directory.
		DWORD Dummy1;				// Always 0x00008000
		DWORD DirectorySize;		// Size of lpGCFDirectoryEntries & lpGCFDirectoryNames & lpGCFDirectoryInfo1Entries & lpGCFDirectoryInfo2Entries & lpGCFDirectoryCopyEntries & lpGCFDirectoryLocalEntries in bytes.
		DWORD NameSize;				// Size of the directory names in bytes.
		DWORD Info1Count;			// Number of Info1 entires.
		DWORD CopyCount;			// Number of files to copy.
		DWORD LocalCount;			// Number of files to keep local.
		DWORD Dummy2;
		DWORD Dummy3;
		DWORD Checksum;				// Header checksum.
	} GCFDIRECTORYHEADER, *LPGCFDIRECTORYHEADER;

	typedef struct tagGCFDIRECTORYENTRY
	{
		DWORD NameOffset;		// Offset to the directory item name from the end of the directory items.
		DWORD ItemSize;			// Size of the item.  (If file, file size.  If folder, num items.)
		DWORD ChecksumIndex;	// Checksome index. (0xFFFFFFFF == None).
		DWORD DirectoryType;	// Flags for the directory item.  (0x00000000 == Folder).
		DWORD ParentIndex;		// Index of the parent directory item.  (0xFFFFFFFF == None).
		DWORD NextIndex;		// Index of the next directory item.  (0x00000000 == None).
		DWORD FirstIndex;		// Index of the first directory item.  (0x00000000 == None).
	} GCFDIRECTORYENTRY, *LPGCFDIRECTORYENTRY;

	typedef struct tagGCFDIRECTORYINFO1ENTRY
	{
		DWORD Dummy0;
	} GCFDIRECTORYINFO1ENTRY, *LPGCFDIRECTORYINFO1ENTRY;

	typedef struct tagGCFDIRECTORYINFO2ENTRY
	{
		DWORD Dummy0;
	} GCFDIRECTORYINFO2ENTRY, *LPGCFDIRECTORYINFO2ENTRY;

	typedef struct tagGCFDIRECTORYCOPYENTRY
	{
		DWORD DirectoryIndex;	// Index of the directory item.
	} GCFDIRECTORYCOPYENTRY, *LPGCFDIRECTORYCOPYENTRY;

	typedef struct tagGCFDIRECTORYLOCALENTRY
	{
		DWORD DirectoryIndex;	// Index of the directory item.
	} GCFDIRECTORYLOCALENTRY, *LPGCFDIRECTORYLOCALENTRY;

	// The below header was added in version 4 or version 5.

	typedef struct tagGCFDIRECTORYMAPHEADER
	{
		DWORD Dummy0;			// Always 0x00000001
		DWORD Dummy1;			// Always 0x00000000
	} GCFDIRECTORYMAPHEADER, *LPGCFDIRECTORYMAPHEADER;

	typedef struct tagGCFDIRECTORYMAPENTRY
	{
		DWORD FirstBlockIndex;	// Index of the first data block. (N/A if == BlockCount.)
	} GCFDIRECTORYMAPENTRY, *LPGCFDIRECTORYMAPENTRY;

	typedef struct tagGCFCHECKSUMHEADER
	{
		DWORD Dummy0;			// Always 0x00000001
		DWORD ChecksumSize;		// Size of LPGCFCHECKSUMHEADER & LPGCFCHECKSUMMAPHEADER & in bytes.
	} GCFCHECKSUMHEADER, *LPGCFCHECKSUMHEADER;

	typedef struct tagGCFCHECKSUMMAPHEADER
	{
		DWORD Dummy0;			// Always 0x14893721
		DWORD Dummy1;			// Always 0x00000001
		DWORD ItemCount;		// Number of items.
		DWORD ChecksumCount;	// Number of checksums.
	} GCFCHECKSUMMAPHEADER, *LPGCFCHECKSUMMAPHEADER;

	typedef struct tagGCFCHECKSUMMAPENTRY
	{
		DWORD ChecksumCount;		// Number of checksums.
		DWORD FirstChecksumIndex;	// Index of first checksum.
	} GCFCHECKSUMMAPENTRY, *LPGCFCHECKSUMMAPENTRY;

	typedef struct tagGCFCHECKSUMENTRY
	{
		DWORD Checksum;				// Checksum.
	} GCFCHECKSUMENTRY, *LPGCFCHECKSUMENTRY;

	typedef struct tagGCFDATABLOCKHEADER
	{
		DWORD LastVersionPlayed;	// GCF file version.  This field is not part of all file versions.
		DWORD BlockCount;			// Number of data blocks.
		DWORD BlockSize;			// Size of each data block in bytes.
		DWORD FirstBlockOffset;		// Offset to first data block.
		DWORD BlocksUsed;			// Number of data blocks that contain data.
		DWORD Checksum;				// Header checksum.
	} GCFDATABLOCKHEADER, *LPGCFDATABLOCKHEADER;

#pragma pack()

private:
	LPVOID lpHeaderView;
	LPVOID lpDataView;

private:
	LPVOID lpGCFBase;

	LPGCFHEADER lpGCFHeader;

	LPGCFBLOCKENTRYHEADER lpGCFBlockEntryHeader;
	LPGCFBLOCKENTRY lpGCFBlockEntries;

	LPGCFFRAGMAPHEADER lpGCFFragMapHeader;
	LPGCFFRAGMAP lpGCFFragMap;

	// The below section is part of version 5 but not version 6.
	LPGCFBLOCKENTRYMAPHEADER lpGCFBlockEntryMapHeader;
	LPGCFBLOCKENTRYMAP lpGCFBlockEntryMap;

	LPGCFDIRECTORYHEADER lpGCFDirectoryHeader;
	LPGCFDIRECTORYENTRY lpGCFDirectoryEntries;
	LPVOID lpGCFDirectoryNames;
	LPGCFDIRECTORYINFO1ENTRY lpGCFDirectoryInfo1Entries;
	LPGCFDIRECTORYINFO2ENTRY lpGCFDirectoryInfo2Entries;
	LPGCFDIRECTORYLOCALENTRY lpGCFDirectoryLocalEntries;
	LPGCFDIRECTORYCOPYENTRY lpGCFDirectoryCopyEntries;

	LPGCFDIRECTORYMAPHEADER lpGCFDirectoryMapHeader;
	LPGCFDIRECTORYMAPENTRY lpGCFDirectoryMapEntries;

	LPGCFCHECKSUMHEADER lpGCFChecksumHeader;
	LPGCFCHECKSUMMAPHEADER lpGCFChecksumMapHeader;
	LPGCFCHECKSUMMAPENTRY lpGCFChecksumMapEntries;
	LPGCFCHECKSUMENTRY lpGCFChecksumEntries;

	LPGCFDATABLOCKHEADER lpGCFDataBlockHeader;

public:
	CGCFFile();
	virtual ~CGCFFile();

public:
	DWORD GetPackageVersion();

	DWORD GetPackageBlockCount();
	DWORD GetPackageUsedBlockCount();

protected:
	virtual BOOL MapDataStructures();
	virtual void UnmapDataStructures();

	virtual CDirectoryFolder *BuildRoot();

private:
	void BuildFolder(CDirectoryFolder *Folder);

public:
	BOOL GetItemsFlags(CDirectoryItem *Item, DWORD &dwFlags);

	BOOL GetFileExists(CDirectoryFile *File, BOOL &bExists);
	virtual BOOL GetFileSize(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileData(CDirectoryFile *File, LPBYTE lpData);

public:
	virtual DWORD GetPackageType() const;
	virtual LPCSTR GetPackageExtension() const;
	virtual LPCSTR GetPackageDescription() const;

private:
	LPCSTR GetDirectoryItemName(DWORD dwDirectoryItemIndex) const;
	LPCBYTE GetDataBlock(DWORD dwDataBlockIndex);
};

#endif