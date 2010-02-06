#define QUARKGCFDLL_API_VERSION 3

const char* sLogFilename = "quarkgcf.log";

#define  DLL_EXPORT   extern "C" __declspec( dllexport ) 


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

DLL_EXPORT int Init(void);
DLL_EXPORT void Unload(void);
DLL_EXPORT int APIVersion(void);
DLL_EXPORT void SetLogPath(char* LogPath);
DLL_EXPORT p_packagehandle GCFOpen(const char* PackageName);
DLL_EXPORT void GCFClose(p_packagehandle pkg);
DLL_EXPORT p_packagefile GCFOpenElement(p_packagehandle pkg, char* FileName);
DLL_EXPORT void GCFCloseElement(p_packagefile p);
DLL_EXPORT bool GCFElementIsFolder(p_packagefile p);
DLL_EXPORT DWORD GCFNumSubElements(p_packagefile p);
DLL_EXPORT p_packagefile GCFGetSubElement(p_packagefile p, DWORD e);
DLL_EXPORT const char* GCFSubElementName(p_packagefile p);
DLL_EXPORT bool GCFReadFile(p_packagefile p, LPBYTE lpData);
DLL_EXPORT DWORD GCFFileSize(p_packagefile p);
DLL_EXPORT void GCFList(FILE *pFile, CPackageUtility &PackageUtility, CDirectoryItem *Item);
DLL_EXPORT bool GCFPrepList(char* packagefile, char* textfile);

}
