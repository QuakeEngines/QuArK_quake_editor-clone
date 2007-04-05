/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef __cplusplus
#error HLLib Requires C++
#endif

#ifndef HLLIB_H
#define HLLIB_H

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define LPCBYTE const LPBYTE

#ifdef HLLIB_EXPORTS
#define HLLIB_API __declspec(dllexport)
#else
#define HLLIB_API __declspec(dllimport)
#endif

#define HLLIB_VERSION 10108
#define HLLIB_VERSION_STRING "1.1.8"

HLLIB_API DWORD GetHLLibVersion();
HLLIB_API LPCSTR GetHLLibVersionString();

//
// CError
//

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

#define HLLIB_INVALID_ID 0xffffffff

//
// EDirectoryItemType
//
enum HLLIB_API EDirectoryItemType
{
	DirectoryItemFolder,
	DirectoryItemFile
};

//
// CDirectoryItem
//
class HLLIB_API CDirectoryFolder;

class HLLIB_API CDirectoryItem
{
private:
	LPSTR szName;
	DWORD dwID;

	CDirectoryFolder *Parent;

public:
	CDirectoryItem(LPCSTR szName, DWORD dwID, CDirectoryFolder *Parent);
	virtual ~CDirectoryItem();

	LPCSTR GetName() const;
	virtual EDirectoryItemType GetType() = 0;
	DWORD GetID();

public:
	CDirectoryFolder *GetParent();
};

//
// CDirectoryFile
//
class HLLIB_API CDirectoryFile : public CDirectoryItem
{
public:
	CDirectoryFile(LPCSTR szName, DWORD dwID = HLLIB_INVALID_ID, CDirectoryItem *Parent = 0);
	virtual ~CDirectoryFile();

	virtual EDirectoryItemType GetType();
};

//
// CDirectoryFolder
//
class HLLIB_API CDirectoryFolder : public CDirectoryItem
{
private:
	class CDirectoryItemVector;

private:
	CDirectoryItemVector *DirectoryItemVector;

public:
	CDirectoryFolder(LPCSTR szName, DWORD dwID = HLLIB_INVALID_ID, CDirectoryItem *Parent = 0);
	virtual ~CDirectoryFolder();

	virtual EDirectoryItemType GetType();

public:
	DWORD ItemCount();

	CDirectoryFile *AddFile(LPCSTR szName, DWORD dwID = HLLIB_INVALID_ID);
	CDirectoryFolder *AddFolder(LPCSTR szName, DWORD dwID = HLLIB_INVALID_ID);
	
	CDirectoryItem *GetItem(DWORD dwItemIndex);
	CDirectoryItem *GetItem(LPCSTR szItemName);
};

//
// CMapping
//
class HLLIB_API CMapping : public CError
{
public:
	CMapping();
	virtual ~CMapping();

public:
	virtual CMapping *Clone() = 0;

public:
	virtual DWORD GetMappingSize() = 0;

	virtual BOOL Mapped() = 0;
	virtual BOOL Map() = 0;
	virtual void Unmap() = 0;

	virtual BOOL MapView(LPVOID &lpView) = 0;
	virtual BOOL MapView(LPVOID &lpView, DWORD dwLength) = 0;
	virtual BOOL MapView(LPVOID &lpView, LPVOID &lpData, DWORD dwOffset, DWORD dwLength) = 0;
	virtual void UnmapView(LPVOID &lpView) = 0;
};


//
// CFileMapping
//
/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

class HLLIB_API CFileMapping : public CMapping
{
private:
	BOOL bMapped;

	LPSTR szFileName;
	BOOL bVolatileAccess;

	HANDLE hFile;
	HANDLE hFileMapping;
	DWORD dwFileSize;

private:
	DWORD dwAllocationGranularity;

public:
	CFileMapping(LPCSTR szFileName);
	CFileMapping(LPCSTR szFileName, BOOL bVolatileAccess);
	virtual ~CFileMapping();

public:
	virtual CMapping *Clone();

public:
	virtual DWORD GetMappingSize();

	virtual BOOL Mapped();
	virtual BOOL Map();
	virtual void Unmap();

	virtual BOOL MapView(LPVOID &lpView);
	virtual BOOL MapView(LPVOID &lpView, DWORD dwLength);
	virtual BOOL MapView(LPVOID &lpView, LPVOID &lpData, DWORD dwOffset, DWORD dwLength);
	virtual void UnmapView(LPVOID &lpView);
};

//
// CPrimitiveFileMapping
//
/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

class HLLIB_API CPrimitiveFileMapping : public CMapping
{
private:
	BOOL bMapped;

	LPSTR szFileName;
	BOOL bVolatileAccess;

	HANDLE hFile;
	DWORD dwFileSize;

public:
	CPrimitiveFileMapping(LPCSTR szFileName);
	CPrimitiveFileMapping(LPCSTR szFileName, BOOL bVolatileAccess);
	virtual ~CPrimitiveFileMapping();

public:
	virtual CMapping *Clone();

public:
	virtual DWORD GetMappingSize();

	virtual BOOL Mapped();
	virtual BOOL Map();
	virtual void Unmap();

	virtual BOOL MapView(LPVOID &lpView);
	virtual BOOL MapView(LPVOID &lpView, DWORD dwLength);
	virtual BOOL MapView(LPVOID &lpView, LPVOID &lpData, DWORD dwOffset, DWORD dwLength);
	virtual void UnmapView(LPVOID &lpView);
};

//
// CMemoryMapping
//
class HLLIB_API CMemoryMapping : public CMapping
{
private:
	BOOL bMapped;

	LPVOID lpMemoryBase;
	DWORD dwMemorySize;

public:
	CMemoryMapping(LPVOID lpMemoryBase, DWORD dwMemorySize);
	virtual ~CMemoryMapping();

public:
	virtual CMapping *Clone();

public:
	virtual DWORD GetMappingSize();

	virtual BOOL Mapped();
	virtual BOOL Map();
	virtual void Unmap();

	virtual BOOL MapView(LPVOID &lpView);
	virtual BOOL MapView(LPVOID &lpView, DWORD dwLength);
	virtual BOOL MapView(LPVOID &lpView, LPVOID &lpData, DWORD dwOffset, DWORD dwLength);
	virtual void UnmapView(LPVOID &lpView);
};

//
// CPackage
//
#define HLLIB_PACKAGE_UNKNOWN 0

class HLLIB_API CPackage : public CError
{
private:
	LPSTR szPackageName;

protected:
	CDirectoryFolder *Root;

public:
	CPackage();
	virtual ~CPackage();

	virtual BOOL Opened() = 0;

	virtual BOOL Open(LPCSTR szFileName) = 0;
	virtual BOOL Open(LPVOID lpMemoryBase, DWORD dwMemeotySize, LPCSTR szFileName = 0) = 0;
	virtual void Close() = 0;

	CDirectoryFolder *GetRoot();

protected:
	virtual BOOL MapDataStructures() = 0;
	virtual void UnmapDataStructures() = 0;

	virtual CDirectoryFolder *BuildRoot() = 0;

public:
	virtual BOOL GetFileSize(CDirectoryFile *File, DWORD &dwSize) = 0;
	virtual BOOL GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize) = 0;
	virtual BOOL GetFileData(CDirectoryFile *File, LPBYTE lpData) = 0;

public:
	LPCSTR GetPackageName() const;
	virtual DWORD GetPackageType() const = 0;
	virtual LPCSTR GetPackageExtension() const = 0;
	virtual LPCSTR GetPackageDescription() const = 0;
	virtual DWORD GetPackageSize() = 0;

protected:
	void SetPackageName(LPCSTR szPackageName);
};

//
// CMappedPackage
//
class HLLIB_API CMappedPackage : public CPackage
{
protected:
	CMapping *Mapping;

public:
	CMappedPackage();
	virtual ~CMappedPackage();

	virtual BOOL Opened();

	virtual BOOL Open(LPCSTR szFileName);
	virtual BOOL Open(LPCSTR szFileName, BOOL bFileMapping, BOOL bVolatileAccess);
	virtual BOOL Open(LPVOID lpMemoryBase, DWORD dwMemeotySize, LPCSTR szFileName = 0);
	virtual void Close();

private:
	BOOL OpenWithMapping(CMapping *Mapping);

protected:
	virtual BOOL MapDataStructures() = 0;
	virtual void UnmapDataStructures() = 0;

public:
	virtual DWORD GetPackageSize();
};

//
// CBSPFile
//
#define HLLIB_PACKAGE_BSP 1

#define LUMP_HL1_ENTITIES		0
#define LUMP_HL1_TEXTUREDATA	2
#define LUMP_HL1_COUNT			15

#define LUMP_HL2_ENTITIES		0
#define LUMP_HL2_PAKFILE		40
#define LUMP_HL2_COUNT			64

class HLLIB_API CBSPFile : public CMappedPackage
{
private:
	enum EBSPType
	{
		eUnknown,
		eHL1,
		eHL2
	};

	#pragma pack(4)
	typedef struct tagHL1BSPLUMP
	{
		INT Offset;
		INT Length;
	} HL1BSPLUMP, *LPHL1BSPLUMP;

	typedef struct tagHL1BSPHEADER
	{
		INT Signature;
		HL1BSPLUMP Lumps[LUMP_HL1_COUNT];
	} HL1BSPHEADER, *LPHL1BSPHEADER;

	typedef struct tagHL1BSPMIPTEXTUREHEADER
	{
		INT MipTextureCount;
		INT Offsets[1];
	} HL1BSPMIPTEXTUREHEADER, *LPHL1BSPMIPTEXTUREHEADER;

	typedef struct tagHL1BSPMIPTEXTURE
	{
		CHAR  Name[16];
		DWORD Width;
		DWORD Height;
		DWORD Offsets[4];
	} HL1BSPMIPTEXTURE, *LPHL1BSPMIPTEXTURE;
	#pragma pack()

	typedef struct tagHL2BSPLUMP
	{
		int			Offset;
		int			Length;
		int			Version;				// Default to zero.
		char		FourCC[4];				// Default to ( char )0, ( char )0, ( char )0, ( char )0
	} HL2BSPLUMP, *LPHL2BSPLUMP;

	typedef struct tagHL2BSPHEADER
	{
		char		Signature[4];			// BSP file signature.
		int			Version;				// BSP file version.
		HL2BSPLUMP	Lumps[LUMP_HL2_COUNT];	// Lumps.
		int			MapRevision;			// The map's revision (iteration, version) number.
	} HL2BSPHEADER, *LPHL2BSPHEADER;

private:
	LPVOID lpView;

private:
	LPVOID lpBSPBase;

	LPHL1BSPHEADER lpHL1BSPHeader;
	LPVOID lpHL1EntityData;
	LPVOID lpHL1TextureData;

	LPHL2BSPHEADER lpHL2BSPHeader;
	LPVOID lpHL2EntityData;
	LPVOID lpHL2PAKData;

public:
	CBSPFile();
	virtual ~CBSPFile();

protected:
	virtual BOOL MapDataStructures();
	virtual void UnmapDataStructures();

	virtual CDirectoryFolder *BuildRoot();

public:
	virtual BOOL GetFileSize(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileData(CDirectoryFile *File, LPBYTE lpData);

private:
	void GetLumpInfo(DWORD dwLumpIndex, DWORD &dwWidth, DWORD &dwHeight, DWORD &dwPaletteSize, LPBYTE &lpPalette, LPBYTE &lpPixels);

public:
	virtual DWORD GetPackageType() const;
	virtual LPCSTR GetPackageExtension() const;
	virtual LPCSTR GetPackageDescription() const;
};

//
// CGCFFile
//
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

//
// CPAKFile
//
#define HLLIB_PACKAGE_PAK 3

class HLLIB_API CPAKFile : public CMappedPackage
{
private:
	#pragma pack(4)

	typedef struct tagPAKHEADER
	{
		CHAR  Signature[4];
		DWORD DirectoryOffset;
		DWORD DirectoryLength;
	} PAKHEADER, *LPPAKHEADER;

	typedef struct tagPAKDIRECTORYITEM
	{
		CHAR  ItemName[56];
		DWORD ItemOffset;
		DWORD ItemLength;
	} PAKDIRECTORYITEM, *LPPAKDIRECTORYITEM;

	#pragma pack()

private:
	LPVOID lpView;

private:
	LPVOID lpPAKBase;

	LPPAKHEADER lpPAKHeader;
	LPVOID lpPAKData;
	LPPAKDIRECTORYITEM lpPAKDirectoryItems;

public:
	CPAKFile();
	virtual ~CPAKFile();

protected:
	virtual BOOL MapDataStructures();
	virtual void UnmapDataStructures();

	virtual CDirectoryFolder *BuildRoot();

public:
	virtual BOOL GetFileSize(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileData(CDirectoryFile *File, LPBYTE lpData);

public:
	virtual DWORD GetPackageType() const;
	virtual LPCSTR GetPackageExtension() const;
	virtual LPCSTR GetPackageDescription() const;
};

//
// CWADFile
//
#define HLLIB_PACKAGE_WAD 4

class HLLIB_API CWADFile : public CMappedPackage
{
private:
	#pragma pack(4)

	typedef struct tagWADHEADER
	{
		CHAR  Signature[4];
		DWORD LumpCount;
		DWORD LumpOffset;
	} WADHEADER, *LPWADHEADER;

	typedef struct tagWADLUMP
	{
		DWORD LumpOffset;
		DWORD LumpDiskLength;
		DWORD LumpLength;
		CHAR  LumpType;
		CHAR  LumpCompression;
		CHAR  Dummy0;
		CHAR  Dummy1;
		CHAR  LumpName[16];
	} WADLUMP, *LPWADLUMP;

	#pragma pack()

private:
	LPVOID lpView;

private:
	LPVOID lpWADBase;

	LPWADHEADER lpWADHeader;
	LPVOID lpWADData;
	LPWADLUMP lpWADLumps;

public:
	CWADFile();
	virtual ~CWADFile();

protected:
	virtual BOOL MapDataStructures();
	virtual void UnmapDataStructures();

	virtual CDirectoryFolder *BuildRoot();

public:
	virtual BOOL GetFileSize(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileData(CDirectoryFile *File, LPBYTE lpData);

	BOOL GetImageSize(CDirectoryFile *File, DWORD dwMipmap, DWORD &dwPaletteSize, DWORD &dwPixelSize);
	BOOL GetImageData(CDirectoryFile *File, DWORD dwMipmap, DWORD &dwWidth, DWORD &dwHeight, LPBYTE lpPaletteData, LPBYTE lpPixelData);

	BOOL GetImageSize(CDirectoryFile *File, DWORD dwMipmap, DWORD &dwSize);
	BOOL GetImageData(CDirectoryFile *File, DWORD dwMipmap, DWORD &dwWidth, DWORD &dwHeight, LPBYTE lpData);

private:
	BOOL GetLumpInfo(DWORD dwLumpIndex, DWORD dwMipmap, DWORD &dwWidth, DWORD &dwHeight, DWORD &dwPaletteSize, LPBYTE &lpPalette, LPBYTE &lpPixels);

public:
	virtual DWORD GetPackageType() const;
	virtual LPCSTR GetPackageExtension() const;
	virtual LPCSTR GetPackageDescription() const;
};

//
// CZIPFile
//
#define HLLIB_PACKAGE_ZIP 5

class HLLIB_API CZIPFile : public CMappedPackage
{
private:
	LPVOID lpView;

public:
	CZIPFile();
	virtual ~CZIPFile();

protected:
	virtual BOOL MapDataStructures();
	virtual void UnmapDataStructures();

	virtual CDirectoryFolder *BuildRoot();

public:
	virtual BOOL GetFileSize(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileData(CDirectoryFile *File, LPBYTE lpData);

public:
	virtual DWORD GetPackageType() const;
	virtual LPCSTR GetPackageExtension() const;
	virtual LPCSTR GetPackageDescription() const;
};

//
// CXZPFile
//
#define HLLIB_PACKAGE_XZP 6

class HLLIB_API CXZPFile : public CMappedPackage
{
private:
	#pragma pack(4)

	typedef struct tagXZPHEADER
	{
		CHAR  Signature[4];
		DWORD Flags;
		DWORD DataCount0;
		DWORD DataCount1;
		DWORD Dummy0;
		DWORD HeaderSize;
		DWORD InfoCount;
		DWORD InfoOffset;
		DWORD InfoSize;
	} XZPHEADER, *LPXZPHEADER;

	typedef struct tagXZPITEMDATA
	{
		DWORD ID;
		DWORD ItemSize;
		DWORD ItemOffset;
	} XZPITEMDATA, *LPXZPITEMDATA;

	typedef struct tagXZPITEMINFO
	{
		DWORD ID;
		DWORD NameOffset;
		DWORD TimeCreated;
	} XZPITEMINFO, *LPXZPITEMINFO;

	typedef struct tagXZPFOOTER
	{
		DWORD FileSize;
		CHAR  Signature[4];
	} XZPFOOTER, *LPXZPFOOTER;

	#pragma pack()

private:
	LPVOID lpView;

private:
	LPVOID lpXZPBase;

	LPXZPHEADER lpXZPHeader;
	LPXZPITEMDATA lpXZPItemData;
	LPXZPITEMINFO lpXZPItemInfo;
	LPXZPFOOTER lpXZPFooter;

public:
	CXZPFile();
	virtual ~CXZPFile();

protected:
	virtual BOOL MapDataStructures();
	virtual void UnmapDataStructures();

	virtual CDirectoryFolder *BuildRoot();

public:
	virtual BOOL GetFileSize(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileData(CDirectoryFile *File, LPBYTE lpData);

public:
	virtual DWORD GetPackageType() const;
	virtual LPCSTR GetPackageExtension() const;
	virtual LPCSTR GetPackageDescription() const;
};

//
// CPackageUtility
//
class HLLIB_API CPackageUtility : public CError
{
private:
	CPackage *Package;

public:
	CPackageUtility(CPackage *Package);
	~CPackageUtility();

	CDirectoryItem *GetDirectoryItem(LPCSTR szItemPath);

	void GetDirectoryItemPath(CDirectoryItem *Item, LPSTR szItemPath);

	DWORD GetDirectoryItemSize(CDirectoryItem *Item);
	DWORD GetDirectoryItemSizeOnDisk(CDirectoryItem *Item);

	DWORD GetFolderFileCount(CDirectoryFolder *Folder);
	DWORD GetFolderFolderCount(CDirectoryFolder *Folder);

	BOOL Extract(CDirectoryItem *Item, LPCSTR szFolder = 0, void (*CallBack)(BOOL, LPCSTR) = 0);
	BOOL Extract(CDirectoryFile *File, LPCSTR szFolder = 0, void (*CallBack)(BOOL, LPCSTR) = 0);
	BOOL Extract(CDirectoryFolder *Folder, LPCSTR szFolder = 0, void (*CallBack)(BOOL, LPCSTR) = 0);

private:
	void CallCallBack(BOOL bResult, CDirectoryItem *Item, void (*CallBack)(BOOL, LPCSTR));

	void RemoveIllegalCharacters(char *cName);
	BOOL DirectoryExists(LPCSTR szFolder);
};

//
// CPackageFactory
//
class HLLIB_API CPackageFactory
{
public:
	static CPackage *Create(DWORD dwPackageType);
	static CPackage *Create(LPCSTR szFileName);
};

#endif