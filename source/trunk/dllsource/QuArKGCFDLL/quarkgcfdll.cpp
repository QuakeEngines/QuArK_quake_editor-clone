#define HLLIB_EXPORTS

#include <iostream>

#include "..\hllib118\Lib/HLLib.h"

#include "../common/logger.h"

#include "quarkgcfdll.h"
using namespace std;

static Logger* sLog = NULL;
static char* sLogPath = NULL;

bool Inited = false;

DLL_EXPORT int Init(void)
{
	if (Inited)
	{
		return 0;
	}
	if (sLogPath == NULL)
	{
		return 1;
	}
	char* LogFileName = new char[strlen(sLogPath)+strlen(sLogFilename)+1];
	strcpy(LogFileName, sLogPath);
	strcat(LogFileName, sLogFilename);
	sLog = new Logger(LogFileName);
	delete [] LogFileName;
	Inited = true;
	return 0;
}

DLL_EXPORT void Unload(void)
{
	if (!Inited)
	{
		return;
	}
	delete sLog;
	sLog = NULL;
	delete [] sLogPath;
	sLogPath = NULL;
	Inited = false;
	return;
}

DLL_EXPORT int APIVersion(void)
{
	return QUARKGCFDLL_API_VERSION;
}

DLL_EXPORT void SetLogPath(char* LogPath)
{
	sLogPath = new char[strlen(LogPath)+1];
	strcpy(sLogPath, LogPath);
}

DLL_EXPORT p_packagehandle GCFOpen(const char* PackageName)
{
	sLog->scope("GCFOpen %s\r\n", PackageName);

	p_packagehandle pkg = new packagehandle;
	pkg->Package = CPackageFactory::Create(PackageName);
	if (pkg->Package == 0)
	{
		sLog->msg(10, "GCFOpen Error loading %s:\r\nUnsupported package type.\r\n", PackageName);
		delete pkg;
		return NULL;
	}
	pkg->PackageUtility = new CPackageUtility(pkg->Package);

	if (!pkg->Package->Open(PackageName))
	{
	    sLog->msg(10, "Error loading %s:\r\n%s\r\n", PackageName, pkg->Package->GetLastError());
	    delete pkg->Package;
	    delete pkg;
	    return NULL;
	}

	return pkg;
}

DLL_EXPORT void GCFClose(p_packagehandle pkg)
{
	sLog->scope("GCFClose(%p)\r\n", pkg);
	pkg->Package->Close();
	delete pkg->Package;
	delete pkg->PackageUtility;
	delete (pkg);
}

DLL_EXPORT p_packagefile GCFOpenElement(p_packagehandle pkg, char* FileName)
{
	sLog->scope("GCFOpenElement(%p,%s)\r\n", pkg,FileName);

	p_packagefile p = new packagefile;

	p->Item = pkg->PackageUtility->GetDirectoryItem(FileName);
	p->Package = pkg->Package;

	if (p->Item == 0)
	{
	    sLog->msg(10, "GCFOpenFile: %s not found in package.\r\n", FileName);
	    delete p;
	    return NULL;
	}
	return p;
}

DLL_EXPORT void GCFCloseElement(p_packagefile p)
{
	sLog->scope("GCFCloseElement(%p)\r\n",p);
	delete p;
}

DLL_EXPORT bool GCFElementIsFolder(p_packagefile p)
{
	sLog->scope("GCFElementIsFolder(%p)\r\n",p); 
	return (p->Item->GetType() == DirectoryItemFolder);
}

DLL_EXPORT DWORD GCFNumSubElements(p_packagefile p)
{
	sLog->scope("GCFNumSubElements(%p)\r\n",p);
	if (p->Item->GetType() == DirectoryItemFolder)
	    return static_cast<CDirectoryFolder *>(p->Item)->ItemCount();
	else
	    return 0;
}

DLL_EXPORT p_packagefile GCFGetSubElement(p_packagefile p, DWORD e)
{
	sLog->scope("GCFGetSubElement(%p,%lu)\r\n", p, e);
	if (p->Item->GetType() == DirectoryItemFolder)
	{
	    p_packagefile subp = new packagefile;
	    subp->Item = static_cast<CDirectoryFolder *>(p->Item)->GetItem(e);
	    subp->Package = p->Package;
	    return subp;
	}
	else
	    return NULL;
}

DLL_EXPORT const char* GCFSubElementName(p_packagefile p)
{
	sLog->scope("GCFSubElementName(%p)\r\n", p);
	return p->Item->GetName();
}


DLL_EXPORT bool GCFReadFile(p_packagefile p, LPBYTE lpData)
{
	sLog->scope("GCFReadFile(%p,%p)\r\n", p, lpData);
	if (p->Item->GetType() == DirectoryItemFile)
	{
	    // Get the file's data.
	    if (!p->Package->GetFileData(static_cast<CDirectoryFile *>(p->Item), lpData))
	    {
			sLog->msg(10,"Package->GetFileData:%\r\n",p->Package->GetLastError() );
			return false;
		}
		return true;
	}
	else
	{
	    return false;
	}
}

DLL_EXPORT DWORD GCFFileSize(p_packagefile p)
{
	sLog->scope("GCFFileSize(%p)\r\n", p);
	DWORD size;
	if (p->Item->GetType() == DirectoryItemFile)
	{
	    if (!p->Package->GetFileSize(static_cast<CDirectoryFile *>(p->Item), size))
	    {
			sLog->msg(10, "Package->GetFileSize:%s\r\n",p->Package->GetLastError());
			return 0;
		}
		else
			return size;
	}
	else
	{
	    return 0;
	}
}

DLL_EXPORT void GCFList(FILE *pFile, CPackageUtility &PackageUtility, CDirectoryItem *Item)
{
	sLog->scope("GCFList(%p,%p,%p)\r\n", pFile, PackageUtility, Item);
	DWORD itemcount;
	char cPath[MAX_PATH] = "";
  
	if (Item->GetType() == DirectoryItemFolder)
	{
	    //PackageUtility.GetDirectoryItemPath(Item, cPath);
	    //fprintf(pFile, "%s\r\n", cPath);
	    CDirectoryFolder *Folder = static_cast<CDirectoryFolder *>(Item);
	    itemcount = Folder->ItemCount();
	    for(DWORD i = 0; i < itemcount; i++)
	    {
			GCFList(pFile, PackageUtility, Folder->GetItem(i));
		}
	}
	else if(Item->GetType() == DirectoryItemFile)
	{
	    PackageUtility.GetDirectoryItemPath(Item, cPath);
	    fprintf(pFile, "%s\r\n", cPath);
	}
}

DLL_EXPORT bool GCFPrepList(char* packagefile, char* textfile)
{
	sLog->scope("GCFPrepList(%s,%s)\r\n", packagefile, textfile);
	BOOL bFileMapping = FALSE;
	BOOL bVolatileAccess = FALSE;
	// Create an appropriate package.
	// This function creates an appropriate package class based on the
	// filename extension alone.  This package must be destroyed at a
	// latter time by calling "delete Package;".
	CPackage *Package = CPackageFactory::Create(packagefile);
	// If CPackageFactory::Create() returns null the package extension
	// is unsupported.
	if(Package == NULL)
	{
	    sLog->msg(10, "Error loading %s:\r\nUnsupported package type.\r\n", packagefile);
	    return false;
	}
	// Create a package utility.
	// A package utility has nothing to do with loading a package
	// but provides several useful functions for generating statistical
	// information on packages, extracting package items, and performing
	// other miscellaneous tasks.
	CPackageUtility PackageUtility = CPackageUtility(Package);
	// Open the package.
	// I know, ugly hack to gain CMappedPackage member access...  I don't
	// want to require runtime type checking.  CPackageFactory::Create will
	// always return a CMappedPackage object but this could change in later
	// versions.  To be extra cautious you could make your own package factory.
	if(!static_cast<CMappedPackage *>(Package)->Open(packagefile, bFileMapping, bVolatileAccess))
	{
	    printf("Error loading %s:\r\n%s\r\n", packagefile, Package->GetLastError());
	    delete Package;
	    return false;
	}
	// List items in package.
/*  if(bList)
  {
    if(!bSilent)
      printf("Listing...\r\n");*/
	  FILE *pFile = stdout;
//    if(textfile != 0)
//    {
		  pFile = fopen(textfile, "wt");
		if(pFile == 0)
		{
	        printf("Error opening %s:\r\n%s\r\n", textfile, "fopen() failed.\r\n");
			delete Package;
			return false;
		}
//    }
		GCFList(pFile, PackageUtility, Package->GetRoot());
/*    if(textfile != 0)
    {*/
		fclose(pFile);
/*    }
    if(!bSilent)
      printf("Done.\r\n");
  }*/
	// Close the package.
	Package->Close();
	// Destroy the package.
	delete Package;
  
	return true;
}
