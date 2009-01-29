/*
 * HLLib
 * Copyright (C) 2006 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#include "ZIPFile.h"

CZIPFile::CZIPFile()
{
	this->lpView = 0;
}

CZIPFile::~CZIPFile()
{
	this->Close();
}

DWORD CZIPFile::GetPackageType() const
{
	return HLLIB_PACKAGE_ZIP;
}

LPCSTR CZIPFile::GetPackageExtension() const
{
	return ".zip";
}

LPCSTR CZIPFile::GetPackageDescription() const
{
	return "ZIP File";
}

BOOL CZIPFile::MapDataStructures()
{
	if(!this->Mapping->MapView(this->lpView))
	{
		this->SetLastError(this->Mapping->GetLastError());
		return FALSE;
	}

	return TRUE;
}

void CZIPFile::UnmapDataStructures()
{
	this->Mapping->UnmapView(this->lpView);
}

// From zip_uncompressed.h in the Source SDK.
#pragma pack(1)
struct ZIP_EndOfCentralDirRecord
{
	unsigned int signature; // 4 bytes (0x06054b50)
	unsigned short numberOfThisDisk;  // 2 bytes
	unsigned short numberOfTheDiskWithStartOfCentralDirectory; // 2 bytes
	unsigned short nCentralDirectoryEntries_ThisDisk;	// 2 bytes
	unsigned short nCentralDirectoryEntries_Total;	// 2 bytes
	unsigned int centralDirectorySize; // 4 bytes
	unsigned int startOfCentralDirOffset; // 4 bytes
	unsigned short commentLength; // 2 bytes
	// zip file comment follows
};

struct ZIP_FileHeader
{
	unsigned int signature; //  4 bytes (0x02014b50) 
	unsigned short versionMadeBy; // version made by 2 bytes 
	unsigned short versionNeededToExtract; // version needed to extract 2 bytes 
	unsigned short flags; // general purpose bit flag 2 bytes 
	unsigned short compressionMethod; // compression method 2 bytes 
	unsigned short lastModifiedTime; // last mod file time 2 bytes 
	unsigned short lastModifiedDate; // last mod file date 2 bytes 
	unsigned int crc32; // crc-32 4 bytes 
	unsigned int compressedSize; // compressed size 4 bytes 
	unsigned int uncompressedSize; // uncompressed size 4 bytes 
	unsigned short fileNameLength; // file name length 2 bytes 
	unsigned short extraFieldLength; // extra field length 2 bytes 
	unsigned short fileCommentLength; // file comment length 2 bytes 
	unsigned short diskNumberStart; // disk number start 2 bytes 
	unsigned short internalFileAttribs; // internal file attributes 2 bytes 
	unsigned int externalFileAttribs; // external file attributes 4 bytes 
	unsigned int relativeOffsetOfLocalHeader; // relative offset of local header 4 bytes 
	// file name (variable size) 
	// extra field (variable size) 
	// file comment (variable size) 
};

struct ZIP_LocalFileHeader
{
	unsigned int signature; //local file header signature 4 bytes (0x04034b50) 
	unsigned short versionNeededToExtract; // version needed to extract 2 bytes 
	unsigned short flags; // general purpose bit flag 2 bytes 
	unsigned short compressionMethod; // compression method 2 bytes 
	unsigned short lastModifiedTime; // last mod file time 2 bytes 
	unsigned short lastModifiedDate; // last mod file date 2 bytes 
	unsigned int crc32; // crc-32 4 bytes 
	unsigned int compressedSize; // compressed size 4 bytes 
	unsigned int uncompressedSize; // uncompressed size 4 bytes 
	unsigned short fileNameLength; // file name length 2 bytes 
	unsigned short extraFieldLength; // extra field length 2 bytes 
	// file name (variable size) 
	// extra field (variable size) 
	// file data (variable size) 
};
#pragma pack()

CDirectoryFolder *CZIPFile::BuildRoot()
{
	CDirectoryFolder *Root = new CDirectoryFolder("root");

	char cFileName[256];

	DWORD i = 0;
	LPBYTE lpCurrent = (LPBYTE)this->lpView;
	LPBYTE lpEnd = (LPBYTE)this->lpView + this->Mapping->GetMappingSize();
	while(lpCurrent < lpEnd)
	{
		BYTE lpLocalFileHeaderSignature[] = { 0x50, 0x4b, 0x03, 0x04 };
		BYTE lpFileHeaderSignature[] = { 0x50, 0x4b, 0x01, 0x02 };
		BYTE lpEndOfCentralDirRecordSignature[] = { 0x50, 0x4b, 0x05, 0x06 };

		if(memcmp(lpCurrent, lpLocalFileHeaderSignature, sizeof(lpLocalFileHeaderSignature)) == 0)
		{
			ZIP_LocalFileHeader *lpZIPLocalFileHeader = (ZIP_LocalFileHeader *)lpCurrent;
			lpCurrent += sizeof(ZIP_LocalFileHeader);

			memcpy(cFileName, lpCurrent, lpZIPLocalFileHeader->fileNameLength);
			cFileName[lpZIPLocalFileHeader->fileNameLength] = 0;
			lpCurrent += lpZIPLocalFileHeader->fileNameLength;

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

			lpCurrent += lpZIPLocalFileHeader->extraFieldLength;
			
			lpCurrent += lpZIPLocalFileHeader->compressedSize;

			i++;
		}
		else if(memcmp(lpCurrent, lpFileHeaderSignature, sizeof(lpFileHeaderSignature)) == 0)
		{
			ZIP_FileHeader *lpZIPFileHeader = (ZIP_FileHeader *)lpCurrent;
			lpCurrent += sizeof(ZIP_FileHeader);

			lpCurrent += lpZIPFileHeader->fileNameLength;

			lpCurrent += lpZIPFileHeader->extraFieldLength;

			lpCurrent += lpZIPFileHeader->fileCommentLength;
		}
		else if(memcmp(lpCurrent, lpEndOfCentralDirRecordSignature, sizeof(lpEndOfCentralDirRecordSignature)) == 0)
		{
			ZIP_EndOfCentralDirRecord *lpZIPEndOfCentralDirRecord = (ZIP_EndOfCentralDirRecord *)lpCurrent;
			lpCurrent += sizeof(ZIP_EndOfCentralDirRecord);

			lpCurrent += lpZIPEndOfCentralDirRecord->commentLength;

			break;
		}
		else
		{
			break;
		}
	}

	return Root;
}

BOOL CZIPFile::GetFileSize(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	DWORD i = 0;
	LPBYTE lpCurrent = (LPBYTE)this->lpView;
	LPBYTE lpEnd = (LPBYTE)this->lpView + this->Mapping->GetMappingSize();
	while(lpCurrent < lpEnd)
	{
		BYTE lpLocalFileHeaderSignature[] = { 0x50, 0x4b, 0x03, 0x04 };
		BYTE lpFileHeaderSignature[] = { 0x50, 0x4b, 0x01, 0x02 };
		BYTE lpEndOfCentralDirRecordSignature[] = { 0x50, 0x4b, 0x05, 0x06 };

		if(memcmp(lpCurrent, lpLocalFileHeaderSignature, sizeof(lpLocalFileHeaderSignature)) == 0)
		{
			ZIP_LocalFileHeader *lpZIPLocalFileHeader = (ZIP_LocalFileHeader *)lpCurrent;
			lpCurrent += sizeof(ZIP_LocalFileHeader);

			lpCurrent += lpZIPLocalFileHeader->fileNameLength;

			lpCurrent += lpZIPLocalFileHeader->extraFieldLength;
			
			lpCurrent += lpZIPLocalFileHeader->compressedSize;

			if(i == File->GetID())
			{
				dwSize = lpZIPLocalFileHeader->uncompressedSize;
				return TRUE;
			}

			i++;
		}
		else if(memcmp(lpCurrent, lpFileHeaderSignature, sizeof(lpFileHeaderSignature)) == 0)
		{
			ZIP_FileHeader *lpZIPFileHeader = (ZIP_FileHeader *)lpCurrent;
			lpCurrent += sizeof(ZIP_FileHeader);

			lpCurrent += lpZIPFileHeader->fileNameLength;

			lpCurrent += lpZIPFileHeader->extraFieldLength;

			lpCurrent += lpZIPFileHeader->fileCommentLength;
		}
		else if(memcmp(lpCurrent, lpEndOfCentralDirRecordSignature, sizeof(lpEndOfCentralDirRecordSignature)) == 0)
		{
			ZIP_EndOfCentralDirRecord *lpZIPEndOfCentralDirRecord = (ZIP_EndOfCentralDirRecord *)lpCurrent;
			lpCurrent += sizeof(ZIP_EndOfCentralDirRecord);

			lpCurrent += lpZIPEndOfCentralDirRecord->commentLength;

			break;
		}
		else
		{
			break;
		}
	}

	dwSize = 0;
	this->SetLastError("Invalid file ID.");

	return FALSE;
}

BOOL CZIPFile::GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	DWORD i = 0;
	LPBYTE lpCurrent = (LPBYTE)this->lpView;
	LPBYTE lpEnd = (LPBYTE)this->lpView + this->Mapping->GetMappingSize();
	while(lpCurrent < lpEnd)
	{
		BYTE lpLocalFileHeaderSignature[] = { 0x50, 0x4b, 0x03, 0x04 };
		BYTE lpFileHeaderSignature[] = { 0x50, 0x4b, 0x01, 0x02 };
		BYTE lpEndOfCentralDirRecordSignature[] = { 0x50, 0x4b, 0x05, 0x06 };

		if(memcmp(lpCurrent, lpLocalFileHeaderSignature, sizeof(lpLocalFileHeaderSignature)) == 0)
		{
			ZIP_LocalFileHeader *lpZIPLocalFileHeader = (ZIP_LocalFileHeader *)lpCurrent;
			lpCurrent += sizeof(ZIP_LocalFileHeader);

			lpCurrent += lpZIPLocalFileHeader->fileNameLength;

			lpCurrent += lpZIPLocalFileHeader->extraFieldLength;
			
			lpCurrent += lpZIPLocalFileHeader->compressedSize;

			if(i == File->GetID())
			{
				dwSize = lpZIPLocalFileHeader->compressedSize;
				return TRUE;
			}

			i++;
		}
		else if(memcmp(lpCurrent, lpFileHeaderSignature, sizeof(lpFileHeaderSignature)) == 0)
		{
			ZIP_FileHeader *lpZIPFileHeader = (ZIP_FileHeader *)lpCurrent;
			lpCurrent += sizeof(ZIP_FileHeader);

			lpCurrent += lpZIPFileHeader->fileNameLength;

			lpCurrent += lpZIPFileHeader->extraFieldLength;

			lpCurrent += lpZIPFileHeader->fileCommentLength;
		}
		else if(memcmp(lpCurrent, lpEndOfCentralDirRecordSignature, sizeof(lpEndOfCentralDirRecordSignature)) == 0)
		{
			ZIP_EndOfCentralDirRecord *lpZIPEndOfCentralDirRecord = (ZIP_EndOfCentralDirRecord *)lpCurrent;
			lpCurrent += sizeof(ZIP_EndOfCentralDirRecord);

			lpCurrent += lpZIPEndOfCentralDirRecord->commentLength;

			break;
		}
		else
		{
			break;
		}
	}

	dwSize = 0;
	this->SetLastError("Invalid file ID.");

	return FALSE;
}

BOOL CZIPFile::GetFileData(CDirectoryFile *File, LPBYTE lpData)
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

	DWORD i = 0;
	LPBYTE lpCurrent = (LPBYTE)this->lpView;
	LPBYTE lpEnd = (LPBYTE)this->lpView + this->Mapping->GetMappingSize();
	while(lpCurrent < lpEnd)
	{
		BYTE lpLocalFileHeaderSignature[] = { 0x50, 0x4b, 0x03, 0x04 };
		BYTE lpFileHeaderSignature[] = { 0x50, 0x4b, 0x01, 0x02 };
		BYTE lpEndOfCentralDirRecordSignature[] = { 0x50, 0x4b, 0x05, 0x06 };

		if(memcmp(lpCurrent, lpLocalFileHeaderSignature, sizeof(lpLocalFileHeaderSignature)) == 0)
		{
			ZIP_LocalFileHeader *lpZIPLocalFileHeader = (ZIP_LocalFileHeader *)lpCurrent;
			lpCurrent += sizeof(ZIP_LocalFileHeader);

			lpCurrent += lpZIPLocalFileHeader->fileNameLength;

			lpCurrent += lpZIPLocalFileHeader->extraFieldLength;
			
			if(i == File->GetID())
			{
				if(lpZIPLocalFileHeader->compressionMethod != 0)
				{
					this->SetLastError("Compressed ZIP content not supported.");

					return FALSE;
				}

				memcpy(lpData, lpCurrent, lpZIPLocalFileHeader->uncompressedSize);
				return TRUE;
			}

			lpCurrent += lpZIPLocalFileHeader->compressedSize;

			i++;
		}
		else if(memcmp(lpCurrent, lpFileHeaderSignature, sizeof(lpFileHeaderSignature)) == 0)
		{
			ZIP_FileHeader *lpZIPFileHeader = (ZIP_FileHeader *)lpCurrent;
			lpCurrent += sizeof(ZIP_FileHeader);

			lpCurrent += lpZIPFileHeader->fileNameLength;

			lpCurrent += lpZIPFileHeader->extraFieldLength;

			lpCurrent += lpZIPFileHeader->fileCommentLength;
		}
		else if(memcmp(lpCurrent, lpEndOfCentralDirRecordSignature, sizeof(lpEndOfCentralDirRecordSignature)) == 0)
		{
			ZIP_EndOfCentralDirRecord *lpZIPEndOfCentralDirRecord = (ZIP_EndOfCentralDirRecord *)lpCurrent;
			lpCurrent += sizeof(ZIP_EndOfCentralDirRecord);

			lpCurrent += lpZIPEndOfCentralDirRecord->commentLength;

			break;
		}
		else
		{
			break;
		}
	}

	this->SetLastError("Invalid file ID.");

	return FALSE;
}