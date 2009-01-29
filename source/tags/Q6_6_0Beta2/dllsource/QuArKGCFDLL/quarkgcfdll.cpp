#define QUARKGCFDLL_API_VERSION 2
#define HLLIB_EXPORTS

#include <iostream>

#include "..\hllib118\Lib/HLLib.h"

#include "../common/logger.h"
using namespace std;

#define  DLL_EXPORT   extern "C" __declspec( dllexport ) 

static Logger* sLog;


extern "C"
{
  
struct packagehandle
{
  CPackage* Package;
  CPackageUtility* PackageUtility;
};

typedef struct packagehandle* p_packagehandle;

struct packagefile
{
  CPackage *Package;
  CDirectoryItem* Item;
};

typedef struct packagefile* p_packagefile;


DLL_EXPORT unsigned long APIVersion(void)
{
  return QUARKGCFDLL_API_VERSION;
}

DLL_EXPORT p_packagehandle GCFOpen(char* PackageName)
{
  sLog = new Logger("quarkgcf.log");
  sLog->scope("GCFOpen %s\n",PackageName);

  p_packagehandle pkg = new packagehandle;
  pkg->Package= CPackageFactory::Create(PackageName);
  if(pkg->Package == 0)
  {
    sLog->msg(10,"GCFOpen Error loading %s:\nUnsupported package type.\n", PackageName);
    delete pkg;
    return NULL;
  }
  pkg->PackageUtility = new CPackageUtility(pkg->Package);

  if(!pkg->Package->Open(PackageName))
  {
    sLog->msg(10,"Error loading %s:\n%s\n", PackageName, pkg->Package->GetLastError());
    delete pkg->Package;
    delete pkg;
    return NULL;
  }

  return pkg;
}

DLL_EXPORT void GCFClose(p_packagehandle pkg)
{
  sLog->scope("GCFClose(%p)\n",pkg);
  pkg->Package->Close();
  delete pkg->Package;
  delete pkg->PackageUtility;
  delete (pkg);
}

DLL_EXPORT p_packagefile GCFOpenElement(p_packagehandle pkg, char* FileName)
{
  sLog->scope("GCFOpenElement(%p,%s)\n", pkg,FileName);

  p_packagefile p = new  packagefile;

  p->Item = pkg->PackageUtility->GetDirectoryItem(FileName);
  p->Package = pkg->Package;

  if(p->Item == 0)
  {
    sLog->msg(10,"GCFOpenFile: %s not found in package.\n", FileName);
    delete p;
    return NULL;
  }
  return p;
}

DLL_EXPORT void GCFCloseElement(p_packagefile p )
{
  sLog->scope("GCFCloseElement(%p)\n",p);
  delete p;
}

DLL_EXPORT int GCFElementIsFolder(p_packagefile p )
{
  sLog->scope("GCFElementIsFolder(%p)\n",p); 
  return (p->Item->GetType() == DirectoryItemFolder);
}

DLL_EXPORT long GCFNumSubElements(p_packagefile p )
{
  sLog->scope("GCFNumSubElements(%p)\n",p);
  if (p->Item->GetType() == DirectoryItemFolder)
    return static_cast<CDirectoryFolder *>(p->Item)->ItemCount();
  else
    return 0;
}

DLL_EXPORT p_packagefile GCFGetSubElement(p_packagefile p , long e)
{
  sLog->scope("GCFGetSubElement(%p,%lu)\n",p,e);
  if (p->Item->GetType() == DirectoryItemFolder)
  {
    p_packagefile subp = new  packagefile;
    subp->Item = static_cast<CDirectoryFolder *>(p->Item)->GetItem(e);
    subp->Package = p->Package;
    return subp;
  }
  else
    return NULL;
}

DLL_EXPORT const char* GCFSubElementName(p_packagefile p )
{
  sLog->scope("GCFSubElementName(%p)\n",p);
  return p->Item->GetName();
}


DLL_EXPORT unsigned long GCFReadFile(p_packagefile p ,LPBYTE lpData)
{
  sLog->scope("GCFReadFile(%p,%p)\n",p ,lpData);
  if(p->Item->GetType() == DirectoryItemFile)
  {
    // Get the file's data.
    if(!p->Package->GetFileData(static_cast<CDirectoryFile *>(p->Item), lpData))
    {
      sLog->msg(10,"Package->GetFileData:%\n",p->Package->GetLastError() );
      return 0;
    }
    return 1;
  }
  else
  {
    return 0;
  }
}

DLL_EXPORT unsigned long GCFFileSize(p_packagefile p)
{
  DWORD size;
  sLog->scope("GCFFileSize(%p)\n", p);
  if(p->Item->GetType() == DirectoryItemFile)
  {
    if(!p->Package->GetFileSize(static_cast<CDirectoryFile *>(p->Item), size))
    {
      sLog->msg(10,"Package->GetFileSize:%s\n",p->Package->GetLastError());
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
  char cPath[512] = "";
   
  if(Item->GetType() == DirectoryItemFolder)
  {
    //PackageUtility.GetDirectoryItemPath(Item, cPath);
    //fprintf(pFile, "%s\n", cPath);
    
    CDirectoryFolder *Folder = static_cast<CDirectoryFolder *>(Item);
    for(DWORD i = 0; i < Folder->ItemCount(); i++)
    {
      GCFList(pFile, PackageUtility, Folder->GetItem(i));
    }
  }
  else if(Item->GetType() == DirectoryItemFile)
  {
    PackageUtility.GetDirectoryItemPath(Item, cPath);
    fprintf(pFile, "%s\n", cPath);
  }
}

DLL_EXPORT int GCFPrepList(char* arg0, char* arg1)
{
  char *cPackage = 0;
  char *cDestination = 0;
  char *cList = 0;
  BOOL bFileMapping = FALSE;
  BOOL bVolatileAccess = FALSE;
  cPackage = arg0;
  // Create an appropriate package.
  // This function creates an appropriate package class based on the
  // filename extension alone.  This package must be destroyed at a
  // latter time by calling "delete Package;".
  CPackage *Package = CPackageFactory::Create(cPackage);
  // If CPackageFactory::Create() returns null the package extension
  // is unsupported.
  if(Package == 0)
  {
    printf("Error loading %s:\nUnsupported package type.\n", cPackage);
    return 3;
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
  if(!static_cast<CMappedPackage *>(Package)->Open(cPackage, bFileMapping, bVolatileAccess))
  {
    printf("Error loading %s:\n%s\n", cPackage, Package->GetLastError());
    delete Package;
    return 3;
  }
  // List items in package.
/*  if(bList)
  {
    if(!bSilent)
      printf("Listing...\n");*/
    FILE *pFile = stdout;
//    if(cList != 0)
//    {
      cList = arg1;
      pFile = fopen(cList, "wt");
      if(pFile == 0)
      {
        printf("Error opening %s:\n%s\n", cList, "fopen() failed.");
        delete Package;
        return 3;
      }
//    }
    GCFList(pFile, PackageUtility, Package->GetRoot());
/*    if(cList != 0)
    {*/
      fclose(pFile);
/*    }
    if(!bSilent)
      printf("Done.\n");
  }*/
  // Close the package.
  Package->Close();
  // Destroy the package.
  delete Package;
  
  return 0;
}

}