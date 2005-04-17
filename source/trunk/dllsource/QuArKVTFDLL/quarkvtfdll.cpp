#define QUARKVTFDLL_API_VERSION 5

#include <stdlib.h>

#include "UtlBuffer.h"
#include "imageloader.h"
#include "vtf.h"
#include "dbg.h"
#include <windows.h>

#include "../common/logger.h"

#define  DLL_EXPORT   extern "C" __declspec( dllexport ) 


static Logger* sLog;

BOOL WINAPI DllMain(
    HINSTANCE hinstDLL,  // handle to DLL module
    DWORD fdwReason,     // reason for calling function
    LPVOID lpReserved )  // reserved
{
    // Perform actions based on the reason for calling.
    switch( fdwReason ) 
    { 
        case DLL_PROCESS_ATTACH:
         // Initialize once for each new process.
         // Return FALSE to fail DLL load.
           sLog = new Logger("quarkvtf.log");
            break;

        case DLL_THREAD_ATTACH:
         // Do thread-specific initialization.
            break;

        case DLL_THREAD_DETACH:
         // Do thread-specific cleanup.
            break;

        case DLL_PROCESS_DETACH:
         // Perform any necessary cleanup.
          delete sLog;
            break;
    }
    return TRUE;  // Successful DLL_PROCESS_ATTACH.
}


DLL_EXPORT unsigned long APIVersion(void)
{
  return QUARKVTFDLL_API_VERSION;
}

DLL_EXPORT int vtf_to_mem(void* bufmem, long readlength,long iMipLevel, unsigned char *pDstImage, long usealpha)
{
  sLog->scope("vtf_to_mem %p,%lu,%lu,%p,%lu\n",bufmem,readlength,iMipLevel,pDstImage,usealpha);
  CUtlBuffer buf;
  buf.SetExternalBuffer( bufmem, readlength );

  IVTFTexture *pTex = CreateVTFTexture();
  if (!pTex->Unserialize( buf ))
  {
    sLog->msg("*** Error reading VTF\n ");
    return 0;
  }
	
  sLog->msg( "vtf width: %d\n", pTex->Width() );
  sLog->msg( "vtf height: %d\n", pTex->Height() );
  sLog->msg( "vtf numFrames: %d\n", pTex->FrameCount() );

  sLog->msg( "TEXTUREFLAGS_POINTSAMPLE=%s\n", ( pTex->Flags() & TEXTUREFLAGS_POINTSAMPLE ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_TRILINEAR=%s\n", ( pTex->Flags() & TEXTUREFLAGS_TRILINEAR ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_CLAMPS=%s\n", ( pTex->Flags() & TEXTUREFLAGS_CLAMPS ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_CLAMPT=%s\n", ( pTex->Flags() & TEXTUREFLAGS_CLAMPT ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_ANISOTROPIC=%s\n", ( pTex->Flags() & TEXTUREFLAGS_ANISOTROPIC ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_HINT_DXT5=%s\n", ( pTex->Flags() & TEXTUREFLAGS_HINT_DXT5 ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_NOCOMPRESS=%s\n", ( pTex->Flags() & TEXTUREFLAGS_NOCOMPRESS ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_NORMAL=%s\n", ( pTex->Flags() & TEXTUREFLAGS_NORMAL ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_NOMIP=%s\n", ( pTex->Flags() & TEXTUREFLAGS_NOMIP ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_NOLOD=%s\n", ( pTex->Flags() & TEXTUREFLAGS_NOLOD ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_MINMIP=%s\n", ( pTex->Flags() & TEXTUREFLAGS_MINMIP ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_PROCEDURAL=%s\n", ( pTex->Flags() & TEXTUREFLAGS_PROCEDURAL ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_ONEBITALPHA=%s\n", ( pTex->Flags() & TEXTUREFLAGS_ONEBITALPHA ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_EIGHTBITALPHA=%s\n", ( pTex->Flags() & TEXTUREFLAGS_EIGHTBITALPHA ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_ENVMAP=%s\n", ( pTex->Flags() & TEXTUREFLAGS_ENVMAP ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_RENDERTARGET=%s\n", ( pTex->Flags() & TEXTUREFLAGS_RENDERTARGET ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_DEPTHRENDERTARGET=%s\n", ( pTex->Flags() & TEXTUREFLAGS_DEPTHRENDERTARGET ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_NODEBUGOVERRIDE=%s\n", ( pTex->Flags() & TEXTUREFLAGS_NODEBUGOVERRIDE ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_SINGLECOPY=%s\n", ( pTex->Flags() & TEXTUREFLAGS_SINGLECOPY ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_ONEOVERMIPLEVELINALPHA=%s\n", ( pTex->Flags() & TEXTUREFLAGS_ONEOVERMIPLEVELINALPHA ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_PREMULTCOLORBYONEOVERMIPLEVEL=%s\n", ( pTex->Flags() & TEXTUREFLAGS_PREMULTCOLORBYONEOVERMIPLEVEL ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_NORMALTODUDV=%s\n", ( pTex->Flags() & TEXTUREFLAGS_NORMALTODUDV ) ? "true" : "false" );
  sLog->msg( "TEXTUREFLAGS_ALPHATESTMIPGENERATION=%s\n", ( pTex->Flags() & TEXTUREFLAGS_ALPHATESTMIPGENERATION ) ? "true" : "false" );
  ImageFormat srcFormat = pTex->Format();
  sLog->msg( "vtf format: %s\n", ImageLoader::GetName( srcFormat ) );
  sLog->msg( "mip levels: %d\n", pTex->MipCount() );
  sLog->msg( "face count: %d\n", pTex->FaceCount() );
  sLog->msg( "Cube Map: %d\n", pTex->IsCubeMap() );
  int iCubeFace = 0;
  unsigned char *pSrcImage = pTex->ImageData( 0, iCubeFace, iMipLevel );

  int iWidth, iHeight;
  pTex->ComputeMipLevelDimensions( iMipLevel, &iWidth, &iHeight );
  sLog->msg("Image: %ld x %ld\n",iWidth,iHeight );

  ImageFormat dstFormat = usealpha ? IMAGE_FORMAT_BGRA8888 : IMAGE_FORMAT_BGR888;
  if (pDstImage==0)
  {
    pDstImage = new unsigned char[ImageLoader::GetMemRequired( iWidth, iHeight, dstFormat, false )];
  }
  if( !ImageLoader::ConvertImageFormat( pSrcImage, srcFormat,pDstImage, dstFormat, iWidth, iHeight, 0, 0 ) )
  {
    sLog->msg( "Error converting from %s to %s\n",ImageLoader::GetName( srcFormat ), ImageLoader::GetName( dstFormat ) );
    return ( 0 );
  }
  return 1;
}


DLL_EXPORT int vtf_info(void* bufmem, long readlength, int* width, int* height, int* miplevels, int* hasalpha)
{
  sLog->scope("vtf_info %p,%lu,%p,%p,%p,%p\n",bufmem,readlength,width,height,miplevels,hasalpha);

  CUtlBuffer buf;
  buf.SetExternalBuffer( bufmem, readlength );

  IVTFTexture *pTex = CreateVTFTexture();
  if (!pTex->Unserialize( buf ,true))
  {
    fprintf(stderr,  "*** Error reading VTF file\n ");
    return 0;
  }
	
  *width= pTex->Width();
  *height= pTex->Height();
  *miplevels= pTex->MipCount();
  *hasalpha= (pTex->Flags() & TEXTUREFLAGS_EIGHTBITALPHA) ||
    (pTex->Flags() & TEXTUREFLAGS_ONEBITALPHA) ? 1 : 0;
  sLog->msg( "vtf width: %d\n", pTex->Width() );
  sLog->msg( "vtf height: %d\n", pTex->Height() );
  sLog->msg( "mip levels: %d\n", pTex->MipCount() );
  return 1;
}

DLL_EXPORT long filesize_of_vtf(long usealpha, int iWidth, int iHeight, int iOutformat)
{
 
  sLog->scope("filesize_of_vtf %lu,%d,%d,%s\n",usealpha,iWidth,iHeight,ImageLoader::GetName( ImageFormat(iOutformat) ));
  IVTFTexture *pTex = CreateVTFTexture();

  ImageFormat srcFormat = usealpha ? IMAGE_FORMAT_BGRA8888 : IMAGE_FORMAT_BGR888;

  if (!pTex->Init( iWidth,
                   iHeight,
                   /*srcFormat*/ImageFormat(iOutformat),
                   TEXTUREFLAGS_NOMIP||
                   TEXTUREFLAGS_NOLOD || 
                   usealpha ? TEXTUREFLAGS_EIGHTBITALPHA : 0,
                   1 ))
  {
    sLog->msg("*** Error init VTF\n ");
    return 0;
  }
  sLog->msg("texinit ok\n ");
  return pTex->FileSize();
}

DLL_EXPORT int mem_to_vtf(void* bufmem, unsigned long length, unsigned char *pSrcImage, long usealpha, int iWidth, int iHeight,int iOutformat)
{
  sLog->scope("mem_to_vtf %p,%lu,%p,%d,%d,%d,%s\n",bufmem,length,pSrcImage,usealpha,iWidth,iHeight,ImageLoader::GetName( ImageFormat(iOutformat) ));
  IVTFTexture *pTex = CreateVTFTexture();

  ImageFormat srcFormat = usealpha ? IMAGE_FORMAT_BGRA8888 : IMAGE_FORMAT_BGR888;

  if (!pTex->Init( iWidth,
                   iHeight,
                   srcFormat,
                   TEXTUREFLAGS_NOMIP ||
                   TEXTUREFLAGS_NOLOD ||
                   usealpha ? TEXTUREFLAGS_EIGHTBITALPHA : 0,
                   1 ))
  {
    sLog->msg("*** Error init VTF\n ");
    return 0;
  }
  sLog->msg("texinit ok\n ");

  unsigned char *pDstImage = pTex->ImageData();
  ImageFormat dstFormat=ImageFormat(iOutformat);
  sLog->msg("ImageDataPtr %p ok\n ",pDstImage);

  if( !ImageLoader::ConvertImageFormat( pSrcImage, srcFormat,pDstImage, dstFormat, iWidth, iHeight, 0, 0 ) )
  {
    sLog->msg( "Error converting from %s to %s\n",ImageLoader::GetName( srcFormat ), ImageLoader::GetName( dstFormat ) );
    return ( 0 );
  }
  sLog->msg("1stConvertImageFormat ok\n ");

  pTex->ConvertImageFormat( IMAGE_FORMAT_DEFAULT,false );
  sLog->msg("2ndConvertImageFormat ok\n ");
  pTex->ComputeReflectivity( );
  sLog->msg("ComputeReflectivity ok\n ");
  pTex->ComputeAlphaFlags();
  sLog->msg("ComputeAlphaFlags ok\n ");
  pTex->PostProcess(false);
  sLog->msg("PostProcess ok\n ");
  pTex->ConvertImageFormat( ImageFormat(iOutformat),false );
  sLog->msg("3rdConvertImageFormat ok\n ");


  CUtlBuffer buf;
  buf.SetExternalBuffer( bufmem, length );

  if (!pTex->Serialize( buf ))
  {
    sLog->msg("*** Error writing VTF\n ");
    return 0;
  }
  sLog->msg("Serialize ok\n ");
  return 1;

}


