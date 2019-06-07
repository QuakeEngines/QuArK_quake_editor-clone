#define QUARKSTEAMFSDLL_API_VERSION 2

#include <iostream>
#include <stdlib.h>
#include "filesystem.h"
#include "dbg.h"


#include "../common/logger.h"

#define  DLL_EXPORT   extern "C" __declspec( dllexport ) 

static Logger* sLog;
static int sSpewErr;

extern "C"
{
  
struct fshandle
{
	IFileSystem*  m_pFileSystem;
	CSysModule*   m_pModule;
};

typedef struct fshandle* pfshandle;

struct steamfile
{
  FileHandle_t f;
	IFileSystem*  m_pFileSystem;
};

typedef struct steamfile* psteamfile;

struct steamfindfile
{
  FileFindHandle_t f;
	IFileSystem*  m_pFileSystem;
  const char* fname;
};

typedef struct steamfindfile* psteamfindfile;

DLL_EXPORT unsigned long APIVersion(void)
{
  return QUARKSTEAMFSDLL_API_VERSION;
}

char* msgtypes[SPEW_TYPE_COUNT] ={"SPEW_MESSAGE",
                  "SPEW_WARNING",
                  "SPEW_ASSERT",
	                "SPEW_ERROR", 
                  "SPEW_LOG"};
SpewRetval_t mySpewFunc( SpewType_t spewType, tchar const *pMsg )
{
  sLog->msg(20,"%s : %s\n",msgtypes[spewType],pMsg);
  sSpewErr++;
  return SPEW_CONTINUE;
}

void SetEnv(const char* name,const char* value)
{
  char* env;
  sLog->scope("SetEnv %s,%p\n",name,value);
  if (value)
  {
    env=new char[strlen(name)+strlen(value)+3];
    sprintf(env,"%s=%s",name,value);
    putenv(env);
    delete env;
  }  
}

void AddEnv(const char* name,const char* value,const char sep)
{
  char* oldenv;
  char* env;
  sLog->scope("AddEnv %s,%p,%c\n",name,value,sep);
  if (value)
  {
    oldenv=getenv(name);
    env=new char[strlen(name)+strlen(oldenv)+strlen(value)+10];
    sprintf(env,"%s=%s%c%s",name,oldenv,sep,value);
    sLog->msg(20,"new value: %s\n",env);
    putenv(env);
    delete env;
  }  
}


DLL_EXPORT pfshandle SteamFSInit( const char* m_pFileSystemDLLName,
                                  unsigned long contentid,
                                  const char* pSteamAppUser,
                                  const char* pSteamUserPassphrase,
                                  const char* pSteamAppId,
                                  const char* pSteamPath)
{
  sSpewErr=0;
  sLog = new Logger("quarksteamfs.log");
  sLog->scope("SteamFSInit %s,%lu\n",m_pFileSystemDLLName,contentid);

  SetEnv("SteamAppUser",pSteamAppUser);
  SetEnv("SteamUserPassphrase",pSteamUserPassphrase);
  SetEnv("SteamAppId",pSteamAppId);
  AddEnv("PATH",pSteamPath,';');

  CreateInterfaceFn	m_ConnectFactory;
  SpewOutputFunc( mySpewFunc );

  pfshandle pfs = new struct fshandle;

	m_ConnectFactory = Sys_GetFactoryThis();
    
  sLog->msg(30,"Sys_LoadInterface()..");
	if ( !Sys_LoadInterface(m_pFileSystemDLLName,FILESYSTEM_INTERFACE_VERSION,&pfs->m_pModule,(void**)&pfs->m_pFileSystem ) )
	{
	  sLog->msg(10,"cannot load FileSystem dll %s\n",m_pFileSystemDLLName);
    return NULL;
	}
  sLog->msg(30,"ok\n");

  sLog->msg(30,"m_pFileSystem->Connect()..");
	if ( !pfs->m_pFileSystem->Connect( m_ConnectFactory ) )
  {
	  sLog->msg(10,"IFileSystem::Connect failed\n");
    return NULL;
  }
  sLog->msg(30,"ok\n");

  sLog->msg(30,"m_pFileSystem->Init()...");
  if ( pfs->m_pFileSystem->Init() != INIT_OK )
  {
	  sLog->msg(10,"IFileSystem::Init failed\n" );
    return NULL;
  }
  sLog->msg(30,"ok\n");

  sLog->msg(30, "mount steam content %lu ...",contentid);
  FilesystemMountRetval_t mountresult1  = static_cast<IFileSystem*>(pfs->m_pFileSystem)->MountSteamContent( contentid );
  if(mountresult1 != FILESYSTEM_MOUNT_OK)
	{
		sLog->msg(10, "Could not mount content %lu \n",contentid);
    return NULL;
  }
  sLog->msg(30, "ok\n");


  pfs->m_pFileSystem->AddSearchPath( "", "MOD", PATH_ADD_TO_TAIL );

  if (sSpewErr != 0)
  {
    sLog->msg(10, "Steam reported %d errors, but did NOT set the proper return codes ! Init Failed !\n",sSpewErr);
    pfs->m_pFileSystem->Shutdown();
    Sys_UnloadModule( pfs->m_pModule );
    delete pfs;
    return NULL;
  }
  else
	  return pfs;
}

DLL_EXPORT void SteamFSTerm(pfshandle pfs)
{
  sLog->scope("SteamFSTerm %p\n",pfs);
  if (pfs)
  {
    pfs->m_pFileSystem->Shutdown();
    Sys_UnloadModule( pfs->m_pModule );
    delete pfs;
  }
}
DLL_EXPORT psteamfile SteamFSOpen(pfshandle pfs, const char* name,const char* mode)
{
  sLog->scope("SteamFSOpen %p,%s,%s\n",pfs,name,mode);
  if (pfs)
  {
    psteamfile pf = new struct steamfile;
    pf->m_pFileSystem=pfs->m_pFileSystem;
    pf->f = pfs->m_pFileSystem->Open( name, mode );
    return pf;
  }
  else
    return NULL;
}

DLL_EXPORT void SteamFSClose(psteamfile pf)
{
  sLog->scope("SteamFSClose %p\n",pf);
  if (pf)
  {
    pf->m_pFileSystem->Close( pf->f );
    delete pf;
  }
}

DLL_EXPORT unsigned long SteamFSSize(psteamfile pf)
{
  sLog->scope("SteamFSSize %p\n",pf);
  if (pf)
    return pf->m_pFileSystem->Size( pf->f );
  else
    return 0;
}

DLL_EXPORT unsigned long SteamFSRead(psteamfile pf, void* buffer, unsigned long size)
{
  sLog->scope("SteamFSRead %p %p %lu\n",pf,buffer,size);
  if (pf)
    return pf->m_pFileSystem->Read( buffer, size, pf->f );
  else
    return 0;
}

DLL_EXPORT psteamfindfile SteamFSFindFirst(pfshandle pfs, const char* pattern)
{
  sLog->scope("SteamFSFindFirst %p,%s\n",pfs,pattern);
  if (pfs)
  {
    psteamfindfile pff = new struct steamfindfile;
    pff->f=0;
    pff->m_pFileSystem=pfs->m_pFileSystem;
    pff->fname = static_cast<IFileSystem*>(pfs->m_pFileSystem)->FindFirst( pattern, &pff->f );
    return pff;
  }
  else
    return NULL;
}

DLL_EXPORT psteamfindfile SteamFSFindNext(psteamfindfile pff)
{
  sLog->scope("SteamFSFindNext %p\n",pff);
  if (pff)
  {
    pff->fname = static_cast<IFileSystem*>(pff->m_pFileSystem)->FindNext( pff->f );
    return pff;
  }
  else
    return NULL;
}

DLL_EXPORT unsigned long  SteamFSFindIsDir(psteamfindfile pff)
{
  sLog->scope("SteamFSFindIsDir %p\n",pff);
  if (pff)
    return  static_cast<IFileSystem*>(pff->m_pFileSystem)->FindIsDirectory( pff->f );
  else
    return 0;
}

DLL_EXPORT void SteamFSFindFinish(psteamfindfile pff)
{
  sLog->scope("SteamFSFindFinish %p\n",pff);
  if (pff)
  {
    static_cast<IFileSystem*>(pff->m_pFileSystem)->FindClose(pff->f);
    delete pff;
  }
}

DLL_EXPORT const char * SteamFSFindName(psteamfindfile pff)
{
  sLog->scope("SteamFSFindName %p\n",pff);
  if (pff)
    return pff->fname;
  else
    return NULL;
}


}