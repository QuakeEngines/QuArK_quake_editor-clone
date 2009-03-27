/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 *
 * Modified 1/8/2006 By Ryan Gregg
 * - Added Source BSP support.
 *
 * Modified 3/3/2005 By Ryan Gregg
 * - Added support for entity data extraction.
 * - Improved MapDataStructures() error checking.
 */

#include "BSPFile.h"

CBSPFile::CBSPFile()
{
	this->eBSPType = eUnknown;
	this->lpView = 0;
}

CBSPFile::~CBSPFile()
{
	this->Close();
}

DWORD CBSPFile::GetPackageType() const
{
	return HLLIB_PACKAGE_BSP;
}

LPCSTR CBSPFile::GetPackageExtension() const
{
	return ".bsp";
}

LPCSTR CBSPFile::GetPackageDescription() const
{
	return "Half-Life Level";
}

BOOL CBSPFile::MapDataStructures()
{
	if(!this->Mapping->MapView(this->lpView))
	{
		this->SetLastError(this->Mapping->GetLastError());
		return FALSE;
	}

	try
	{
		this->lpBSPBase = this->lpView;

		this->lpHL1BSPHeader = (LPHL1BSPHEADER)this->lpBSPBase;
		this->lpHL2BSPHeader = (LPHL2BSPHEADER)this->lpBSPBase;

		// Check that we have room to read the header.
		if(sizeof(HL1BSPHEADER) > this->Mapping->GetMappingSize() || sizeof(HL2BSPHEADER) > this->Mapping->GetMappingSize())
		{
			this->SetLastError("Corrupt file.\n\nThe file map is to small for it's header.");
			return FALSE;
		}

		// Check that the header has the right signature.
		if(this->lpHL1BSPHeader->Signature != 30 && memcmp(this->lpHL2BSPHeader->Signature, "VBSP", 4) != 0)
		{
			this->SetLastError("Invalid file version.\n\nThe file's signature does not match.");
			return FALSE;
		}

		if(memcmp(this->lpHL2BSPHeader->Signature, "VBSP", 4) == 0)
		{
			this->eBSPType = eHL2;
		}
		else if(this->lpHL1BSPHeader->Signature == 30)
		{
			this->eBSPType = eHL1;
		}

		switch(this->eBSPType)
		{
			case eHL1:
			{
				for(DWORD i = 0; i < LUMP_HL1_COUNT; i++)
				{
					if(this->lpHL1BSPHeader->Lumps[i].Length > 0)
					{
						if((DWORD)(this->lpHL1BSPHeader->Lumps[i].Offset + this->lpHL1BSPHeader->Lumps[i].Length) > this->Mapping->GetMappingSize())
						{
							this->SetLastError("Corrupt file.\n\nThe file map is not within file bounds.");
							return FALSE;
						}
					}
				}

				if(this->lpHL1BSPHeader->Lumps[LUMP_HL1_ENTITIES].Length != 0)
				{
					this->lpHL1EntityData = (LPVOID)((LPBYTE)this->lpBSPBase + this->lpHL1BSPHeader->Lumps[LUMP_HL1_ENTITIES].Offset);
				}
				else
				{
					this->lpHL1EntityData = 0;
				}
				if(this->lpHL1BSPHeader->Lumps[LUMP_HL1_TEXTUREDATA].Length != 0)
				{
					this->lpHL1TextureData = (LPVOID)((LPBYTE)this->lpBSPBase + this->lpHL1BSPHeader->Lumps[LUMP_HL1_TEXTUREDATA].Offset);
				}
				else
				{
					this->lpHL1TextureData = 0;
				}
				break;
			}
			case eHL2:
			{
				if(this->lpHL2BSPHeader->Lumps[LUMP_HL2_ENTITIES].Length != 0)
				{
					this->lpHL2EntityData = (LPVOID)((LPBYTE)this->lpBSPBase + this->lpHL2BSPHeader->Lumps[LUMP_HL2_ENTITIES].Offset);
				}
				else
				{
					this->lpHL2EntityData = 0;
				}
				if(this->lpHL2BSPHeader->Lumps[LUMP_HL2_PAKFILE].Length != 0)
				{
					this->lpHL2PAKData = (LPVOID)((LPBYTE)this->lpBSPBase + this->lpHL2BSPHeader->Lumps[LUMP_HL2_PAKFILE].Offset);
				}
				else
				{
					this->lpHL2PAKData = 0;
				}
				break;
			}
		}
	}
	catch(...)
	{
		this->SetLastError("Corrupt file.\n\nThe file map is not within file bounds.");
		return FALSE;
	}

	return TRUE;
}

void CBSPFile::UnmapDataStructures()
{
	this->eBSPType = eUnknown;

	this->lpBSPBase = 0;

	this->lpHL1BSPHeader = 0;
	this->lpHL1EntityData = 0;
	this->lpHL1TextureData = 0;

	this->lpHL2BSPHeader = 0;
	this->lpHL2EntityData = 0;
	this->lpHL2PAKData = 0;

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

CDirectoryFolder *CBSPFile::BuildRoot()
{
	CDirectoryFolder *Root = new CDirectoryFolder("root");

	switch(this->eBSPType)
	{
		case eHL1:
		{
			LPHL1BSPMIPTEXTUREHEADER lpMipTextureHeader = (LPHL1BSPMIPTEXTUREHEADER)(this->lpHL1TextureData);

			char cFileName[256];

			// Loop through each lump in the WAD file.
			for(INT i = 0; i < lpMipTextureHeader->MipTextureCount; i++)
			{
				if(lpMipTextureHeader->Offsets[i] == -1)
				{
					continue;
				}

				LPHL1BSPMIPTEXTURE lpMipTexture = (LPHL1BSPMIPTEXTURE)((LPBYTE)this->lpHL1TextureData + lpMipTextureHeader->Offsets[i]);

				if(lpMipTexture->Offsets[0] == 0)
				{
					continue;
				}

				sprintf(cFileName, "%s.bmp", lpMipTexture->Name);

				// Add the lump as a bitmap.
				Root->AddFile(cFileName, (DWORD)i);
			}

			// Add the entity lump.
			if(strlen(this->GetPackageName()) == 0)
			{
				strcpy(cFileName, "entity.ent");
			}
			else
			{
				const char *cStart = strrchr(this->GetPackageName(), '\\');

				if(cStart == 0)
				{
					cStart = this->GetPackageName();
				}
				else
				{
					cStart++;
				}

				const char *cEnd = strrchr(cStart, '.');

				if(cEnd == 0)
				{
					cEnd = cStart + strlen(cStart);
				}

				strncpy(cFileName, cStart, cEnd - cStart);
				cFileName[cEnd - cStart] = '\0';
				strcat(cFileName, ".ent");
			}

			// Add the lump as a bitmap.
			Root->AddFile(cFileName, HLLIB_INVALID_ID - 1);
			break;
		}
		case eHL2:
		{
			char cFileName[256];

			DWORD i = 0;
			LPBYTE lpCurrent = (LPBYTE)this->lpHL2PAKData;
			while(lpCurrent < (LPBYTE)this->lpHL2PAKData + this->lpHL2BSPHeader->Lumps[LUMP_HL2_PAKFILE].Length)
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

			// Add the entity lump.
			if(strlen(this->GetPackageName()) == 0)
			{
				strcpy(cFileName, "entity.ent");
			}
			else
			{
				const char *cStart = strrchr(this->GetPackageName(), '\\');

				if(cStart == 0)
				{
					cStart = this->GetPackageName();
				}
				else
				{
					cStart++;
				}

				const char *cEnd = strrchr(cStart, '.');

				if(cEnd == 0)
				{
					cEnd = cStart + strlen(cStart);
				}

				strncpy(cFileName, cStart, cEnd - cStart);
				cFileName[cEnd - cStart] = '\0';
				strcat(cFileName, ".ent");
			}

			// Add the lump as a bitmap.
			Root->AddFile(cFileName, HLLIB_INVALID_ID - 1);

			// Add the pak lump.
			if(strlen(this->GetPackageName()) == 0)
			{
				strcpy(cFileName, "pak.zip");
			}
			else
			{
				const char *cStart = strrchr(this->GetPackageName(), '\\');

				if(cStart == 0)
				{
					cStart = this->GetPackageName();
				}
				else
				{
					cStart++;
				}

				const char *cEnd = strrchr(cStart, '.');

				if(cEnd == 0)
				{
					cEnd = cStart + strlen(cStart);
				}

				strncpy(cFileName, cStart, cEnd - cStart);
				cFileName[cEnd - cStart] = '\0';
				strcat(cFileName, ".zip");
			}

			// Add the lump as a bitmap.
			Root->AddFile(cFileName, HLLIB_INVALID_ID - 2);
			break;
		}
	}

	return Root;
}

BOOL CBSPFile::GetFileSize(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	switch(this->eBSPType)
	{
		case eHL1:
		{
			if(File->GetID() == HLLIB_INVALID_ID - 1)
			{
				dwSize = (DWORD)this->lpHL1BSPHeader->Lumps[LUMP_HL1_ENTITIES].Length - 1;
				return TRUE;
			}

			//
			// Read lump.
			//

			DWORD dwWidth, dwHeight, dwPaletteSize;
			LPBYTE lpPalette, lpPixels;

			this->GetLumpInfo(File->GetID(), dwWidth, dwHeight, dwPaletteSize, lpPalette, lpPixels);

			// Compute the size of the texture as a bitmap.
			dwSize = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) + dwPaletteSize * 4 + dwWidth * dwHeight;
			break;
		}
		case eHL2:
		{
			if(File->GetID() == HLLIB_INVALID_ID - 1)
			{
				dwSize = (DWORD)this->lpHL2BSPHeader->Lumps[LUMP_HL2_ENTITIES].Length - 1;
				return TRUE;
			}

			if(File->GetID() == HLLIB_INVALID_ID - 2)
			{
				dwSize = (DWORD)this->lpHL2BSPHeader->Lumps[LUMP_HL2_PAKFILE].Length;
				return TRUE;
			}

			DWORD i = 0;
			LPBYTE lpCurrent = (LPBYTE)this->lpHL2PAKData;
			while(lpCurrent < (LPBYTE)this->lpHL2PAKData + this->lpHL2BSPHeader->Lumps[LUMP_HL2_PAKFILE].Length)
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
			break;
		}
	}

	return TRUE;
}

BOOL CBSPFile::GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	switch(this->eBSPType)
	{
		case eHL1:
		{
			if(File->GetID() == HLLIB_INVALID_ID - 1)
			{
				dwSize = (DWORD)this->lpHL1BSPHeader->Lumps[LUMP_HL1_ENTITIES].Length - 1;
				return TRUE;
			}

			DWORD dwWidth, dwHeight, dwPaletteSize;
			LPBYTE lpPalette, lpPixels;

			this->GetLumpInfo(File->GetID(), dwWidth, dwHeight, dwPaletteSize, lpPalette, lpPixels);

			DWORD dwPixelSize = dwWidth * dwHeight;
			DWORD dwPixelDataSize = (dwPixelSize) + (dwPixelSize / 4) + (dwPixelSize / 16) + (dwPixelSize / 64);

			dwSize = sizeof(HL1BSPMIPTEXTURE) + dwPixelDataSize + dwPaletteSize * 3;
		}
		case eHL2:
		{
			if(File->GetID() == HLLIB_INVALID_ID - 1)
			{
				dwSize = (DWORD)this->lpHL2BSPHeader->Lumps[LUMP_HL2_ENTITIES].Length - 1;
				return TRUE;
			}

			if(File->GetID() == HLLIB_INVALID_ID - 2)
			{
				dwSize = (DWORD)this->lpHL2BSPHeader->Lumps[LUMP_HL2_PAKFILE].Length;
				return TRUE;
			}

			DWORD i = 0;
			LPBYTE lpCurrent = (LPBYTE)this->lpHL2PAKData;
			while(lpCurrent < (LPBYTE)this->lpHL2PAKData + this->lpHL2BSPHeader->Lumps[LUMP_HL2_PAKFILE].Length)
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
			break;
		}
	}

	return TRUE;
}

BOOL CBSPFile::GetFileData(CDirectoryFile *File, LPBYTE lpData)
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

	switch(this->eBSPType)
	{
		case eHL1:
		{
			if(File->GetID() == HLLIB_INVALID_ID - 1)
			{
				memcpy(lpData, this->lpHL1EntityData, (DWORD)this->lpHL1BSPHeader->Lumps[LUMP_HL1_ENTITIES].Length - 1);
				return TRUE;
			}

			//
			// Read lump.
			//

			DWORD dwWidth, dwHeight, dwPaletteSize;
			LPBYTE lpPalette, lpPixels;

			this->GetLumpInfo(File->GetID(), dwWidth, dwHeight, dwPaletteSize, lpPalette, lpPixels);

			//
			// Allocate data.
			//

			BITMAPFILEHEADER *FileHeader = (BITMAPFILEHEADER *)lpData;
			BITMAPINFOHEADER *InfoHeader = (BITMAPINFOHEADER *)(lpData + sizeof(BITMAPFILEHEADER));
			LPBYTE lpPaletteData = (LPBYTE)((LPBYTE)InfoHeader + sizeof(BITMAPINFOHEADER));
			LPBYTE lpPixelData = lpPaletteData + dwPaletteSize * 4;

			ZeroMemory(FileHeader, sizeof(BITMAPFILEHEADER));
			ZeroMemory(InfoHeader, sizeof(BITMAPINFOHEADER));

			//
			// Fill in headers.
			//

			FileHeader->bfType = 19778;
			FileHeader->bfSize = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) + dwPaletteSize * 4 + dwWidth * dwHeight;
			FileHeader->bfOffBits = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) + dwPaletteSize * 4;

			InfoHeader->biSize = sizeof(BITMAPINFOHEADER);
			InfoHeader->biWidth = dwWidth;
			InfoHeader->biHeight = dwHeight;
			InfoHeader->biPlanes = 1;
			InfoHeader->biBitCount = 8;
			InfoHeader->biSizeImage = 0;
			InfoHeader->biClrUsed = dwPaletteSize;
			InfoHeader->biClrImportant = dwPaletteSize;

			//
			// Fill in Palette data.
			//

			for(DWORD i = 0; i < dwPaletteSize; i++)
			{
				lpPaletteData[i * 4 + 0] = lpPalette[i * 3 + 2];
				lpPaletteData[i * 4 + 1] = lpPalette[i * 3 + 1];
				lpPaletteData[i * 4 + 2] = lpPalette[i * 3 + 0];
				lpPaletteData[i * 4 + 3] = 0;
			}

			//
			// Fill in Index data.
			//

			for(DWORD i = 0; i < dwWidth; i++)
			{
				for(DWORD j = 0; j < dwHeight; j++)
				{
					lpPixelData[i + (dwHeight - 1 - j) * dwWidth] = lpPixels[i + j * dwWidth];
				}
			}
		}
		case eHL2:
		{
			if(File->GetID() == HLLIB_INVALID_ID - 1)
			{
				memcpy(lpData, this->lpHL2EntityData, (DWORD)this->lpHL2BSPHeader->Lumps[LUMP_HL2_ENTITIES].Length - 1);
				return TRUE;
			}

			if(File->GetID() == HLLIB_INVALID_ID - 2)
			{
				memcpy(lpData, this->lpHL2PAKData, (DWORD)this->lpHL2BSPHeader->Lumps[LUMP_HL2_PAKFILE].Length);
				return TRUE;
			}

			DWORD i = 0;
			LPBYTE lpCurrent = (LPBYTE)this->lpHL2PAKData;
			while(lpCurrent < (LPBYTE)this->lpHL2PAKData + this->lpHL2BSPHeader->Lumps[LUMP_HL2_PAKFILE].Length)
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
			break;
		}
	}

	return TRUE;
}

void CBSPFile::GetLumpInfo(DWORD dwLumpIndex, DWORD &dwWidth, DWORD &dwHeight, DWORD &dwPaletteSize, LPBYTE &lpPalette, LPBYTE &lpPixels)
{
	//
	// Read lump.
	//

	LPHL1BSPMIPTEXTUREHEADER lpMipTextureHeader = (LPHL1BSPMIPTEXTUREHEADER)(this->lpHL1TextureData);
	LPHL1BSPMIPTEXTURE lpMipTexture = (LPHL1BSPMIPTEXTURE)((LPBYTE)this->lpHL1TextureData + lpMipTextureHeader->Offsets[dwLumpIndex]);

	dwWidth = lpMipTexture->Width;
	dwHeight = lpMipTexture->Height;

	LPBYTE lpPointer = (LPBYTE)lpMipTexture + lpMipTexture->Offsets[0];
	lpPixels = lpPointer;

	DWORD dwPixelSize = dwWidth * dwHeight;

	lpPointer += (dwPixelSize) + (dwPixelSize / 4) + (dwPixelSize / 16) + (dwPixelSize / 64);
	dwPaletteSize = (DWORD)(*(LPWORD)lpPointer);

	lpPointer += 2;
	lpPalette = lpPointer;
}