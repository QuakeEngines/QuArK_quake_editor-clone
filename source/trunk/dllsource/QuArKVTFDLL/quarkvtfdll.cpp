#define QUARKVTFDLL_API_VERSION 3

#include <stdlib.h>

#include "UtlBuffer.h"
#include "imageloader.h"
#include "vtf.h"

#define  DLL_EXPORT   extern "C" __declspec( dllexport ) 

 inline void msg( char const *pMsg, ... )
 {
#ifdef DEBUG
   va_list marker;
  	va_start( marker, pMsg );
	  vfprintf(stderr, pMsg, marker );
    fflush(stderr);
	  va_end( marker );
#endif
 }



DLL_EXPORT unsigned long APIVersion(void)
{
  return QUARKVTFDLL_API_VERSION;
}

DLL_EXPORT int vtf_to_mem(void* bufmem, long readlength,long iMipLevel, unsigned char *pDstImage, long usealpha)
{
  CUtlBuffer buf;
  buf.SetExternalBuffer( bufmem, readlength );

  IVTFTexture *pTex = CreateVTFTexture();
  if (!pTex->Unserialize( buf ))
  {
    msg("*** Error reading VTF\n ");
    return 0;
  }
	
  msg( "vtf width: %d\n", pTex->Width() );
  msg( "vtf height: %d\n", pTex->Height() );
  msg( "vtf numFrames: %d\n", pTex->FrameCount() );

  msg( "TEXTUREFLAGS_POINTSAMPLE=%s\n", ( pTex->Flags() & TEXTUREFLAGS_POINTSAMPLE ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_TRILINEAR=%s\n", ( pTex->Flags() & TEXTUREFLAGS_TRILINEAR ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_CLAMPS=%s\n", ( pTex->Flags() & TEXTUREFLAGS_CLAMPS ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_CLAMPT=%s\n", ( pTex->Flags() & TEXTUREFLAGS_CLAMPT ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_ANISOTROPIC=%s\n", ( pTex->Flags() & TEXTUREFLAGS_ANISOTROPIC ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_HINT_DXT5=%s\n", ( pTex->Flags() & TEXTUREFLAGS_HINT_DXT5 ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_NOCOMPRESS=%s\n", ( pTex->Flags() & TEXTUREFLAGS_NOCOMPRESS ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_NORMAL=%s\n", ( pTex->Flags() & TEXTUREFLAGS_NORMAL ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_NOMIP=%s\n", ( pTex->Flags() & TEXTUREFLAGS_NOMIP ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_NOLOD=%s\n", ( pTex->Flags() & TEXTUREFLAGS_NOLOD ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_MINMIP=%s\n", ( pTex->Flags() & TEXTUREFLAGS_MINMIP ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_PROCEDURAL=%s\n", ( pTex->Flags() & TEXTUREFLAGS_PROCEDURAL ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_ONEBITALPHA=%s\n", ( pTex->Flags() & TEXTUREFLAGS_ONEBITALPHA ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_EIGHTBITALPHA=%s\n", ( pTex->Flags() & TEXTUREFLAGS_EIGHTBITALPHA ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_ENVMAP=%s\n", ( pTex->Flags() & TEXTUREFLAGS_ENVMAP ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_RENDERTARGET=%s\n", ( pTex->Flags() & TEXTUREFLAGS_RENDERTARGET ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_DEPTHRENDERTARGET=%s\n", ( pTex->Flags() & TEXTUREFLAGS_DEPTHRENDERTARGET ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_NODEBUGOVERRIDE=%s\n", ( pTex->Flags() & TEXTUREFLAGS_NODEBUGOVERRIDE ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_SINGLECOPY=%s\n", ( pTex->Flags() & TEXTUREFLAGS_SINGLECOPY ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_ONEOVERMIPLEVELINALPHA=%s\n", ( pTex->Flags() & TEXTUREFLAGS_ONEOVERMIPLEVELINALPHA ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_PREMULTCOLORBYONEOVERMIPLEVEL=%s\n", ( pTex->Flags() & TEXTUREFLAGS_PREMULTCOLORBYONEOVERMIPLEVEL ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_NORMALTODUDV=%s\n", ( pTex->Flags() & TEXTUREFLAGS_NORMALTODUDV ) ? "true" : "false" );
  msg( "TEXTUREFLAGS_ALPHATESTMIPGENERATION=%s\n", ( pTex->Flags() & TEXTUREFLAGS_ALPHATESTMIPGENERATION ) ? "true" : "false" );
  ImageFormat srcFormat = pTex->Format();
  msg( "vtf format: %s\n", ImageLoader::GetName( srcFormat ) );
  msg( "mip levels: %d\n", pTex->MipCount() );
  msg( "face count: %d\n", pTex->FaceCount() );
  msg( "Cube Map: %d\n", pTex->IsCubeMap() );
  int iCubeFace = 0;
  unsigned char *pSrcImage = pTex->ImageData( 0, iCubeFace, iMipLevel );

  int iWidth, iHeight;
  pTex->ComputeMipLevelDimensions( iMipLevel, &iWidth, &iHeight );
  msg("Image: %ld x %ld\n",iWidth,iHeight );

  ImageFormat dstFormat = usealpha ? IMAGE_FORMAT_BGRA8888 : IMAGE_FORMAT_BGR888;
  if (pDstImage==0)
  {
    pDstImage = new unsigned char[ImageLoader::GetMemRequired( iWidth, iHeight, dstFormat, false )];
  }
  if( !ImageLoader::ConvertImageFormat( pSrcImage, srcFormat,pDstImage, dstFormat, iWidth, iHeight, 0, 0 ) )
  {
    msg( "Error converting from %s to %s\n",ImageLoader::GetName( srcFormat ), ImageLoader::GetName( dstFormat ) );
    return ( 0 );
  }
  return 1;
}


DLL_EXPORT int vtf_info(void* bufmem, long readlength, int* width, int* height, int* miplevels, int* hasalpha)
{

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
  msg( "vtf width: %d\n", pTex->Width() );
  msg( "vtf height: %d\n", pTex->Height() );
  msg( "mip levels: %d\n", pTex->MipCount() );
  return 1;
}
