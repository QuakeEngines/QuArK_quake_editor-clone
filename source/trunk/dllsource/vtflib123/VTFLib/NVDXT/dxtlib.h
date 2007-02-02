/****************************************************************************************
	
    Copyright (C) NVIDIA Corporation 2003

    TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW, THIS SOFTWARE IS PROVIDED
    *AS IS* AND NVIDIA AND ITS SUPPLIERS DISCLAIM ALL WARRANTIES, EITHER EXPRESS
    OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, IMPLIED WARRANTIES OF MERCHANTABILITY
    AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL NVIDIA OR ITS SUPPLIERS
    BE LIABLE FOR ANY SPECIAL, INCIDENTAL, INDIRECT, OR CONSEQUENTIAL DAMAGES
    WHATSOEVER (INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF BUSINESS PROFITS,
    BUSINESS INTERRUPTION, LOSS OF BUSINESS INFORMATION, OR ANY OTHER PECUNIARY LOSS)
    ARISING OUT OF THE USE OF OR INABILITY TO USE THIS SOFTWARE, EVEN IF NVIDIA HAS
    BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

*****************************************************************************************/
#pragma once

#include <dds/nvdxt_options.h>
// for .dds file reading and writing
typedef HRESULT (*FileReadCallback)(size_t count, void *buffer, void * userData);
typedef HRESULT (*FileWriteCallback)(size_t count, void *buffer, void * userData);

// callback for MIP maps only
typedef HRESULT (*MIPcallback)(void * data, int miplevel, DWORD size, int width, int height, void * user_data);



// call back
// pointer to data
// mip level
// size of chunk

 


inline bool IsPower2(unsigned int x)
{              
    if ( x < 1 )
        return false;

    if (x == 1)
        return true;

    if ( x & (x-1) )        
        return false;

    return true;
}




inline bool IsMultiple4(unsigned int x)
{              
    if ( x == 0 )
        return false;

    if ((x % 4) == 0)
        return true;
    else
        return false;
}


    

class imgdes
{
public:

    unsigned char *imageData;              /* Image data, you must delete [] this */

    unsigned int bytepitch;        /* Image pitch in bytes */
    
    unsigned int width;            /* Image width */
    unsigned int height;           /* Image height */

    unsigned int bits_per_component;            // 8, 16 or 32
    unsigned int planes;           // 3 or 4
    
    unsigned int base_width;      // without MIP maps      
                    
    unsigned int format;                 //  D3DFORMAT ;
    int    nMipMapsInImage;       // 1 to n

};

/*
   Compresses an image with a user supplied callback with the data for each MIP level created
   Only supports input of RGB 24 or ARGB 32 bpp
*/

#ifdef NVDXTC
extern "C" {
#endif


HRESULT nvDXTcompressRGBA(unsigned char * image_data, 
                      unsigned long image_width,
                      unsigned long image_height, 
                      DWORD byte_pitch,
                      CompressionOptions * options,
                      DWORD planes,
                      MIPcallback MIPMapRoutine,
                      FileWriteCallback fileWriteRoutine,
                      RECT * rect = 0);

HRESULT nvDXTcompressBGRA(unsigned char * image_data, 
                      unsigned long image_width,
                      unsigned long image_height, 
                      DWORD byte_pitch,
                      CompressionOptions * options,
                      DWORD planes,
                      MIPcallback MIPMapRoutine,   \
                      FileWriteCallback fileWriteRoutine,
                      RECT * rect = 0);


HRESULT nvDXTcompressVolumeBGRA(unsigned char * raw_data, // pointer to data (24 or 32 bit)
                unsigned long w, // width in texels
                unsigned long h, // height in texels
                unsigned long depth, // depth of volume texture
                DWORD byte_pitch,
                CompressionOptions * options,
                DWORD planes, // 3 or 4
                MIPcallback MIPMapRoutine,  // callback for generated levels
                FileWriteCallback fileWriteRoutine,  // call to .dds write routine
                RECT * rect = NULL);   // subrect to operate on, NULL is whole image


HRESULT nvDXTcompressVolumeRGBA(unsigned char * raw_data, // pointer to data (24 or 32 bit)
                unsigned long w, // width in texels
                unsigned long h, // height in texels
                unsigned long depth, // depth of volume texture
                DWORD byte_pitch,
                CompressionOptions * options,
                DWORD planes, // 3 or 4
                MIPcallback MIPMapRoutine,  // callback for generated levels
                FileWriteCallback fileWriteRoutine,  // call to .dds write routine
                RECT * rect = NULL);   // subrect to operate on, NULL is whole image


// floating point input
HRESULT nvDXTcompress32F(fpImage & srcImage,
                         CompressionOptions * options,
                         MIPcallback MIPMapRoutine,
                         FileWriteCallback fileWriteRoutine,
                         RECT * rect = 0);

HRESULT nvDXTcompress(RGBAImage & image,
                      CompressionOptions * options,
                      MIPcallback MIPMapRoutine,
                      FileWriteCallback fileWriteRoutine,  // call to .dds write routine

                      RECT * rect = 0);



#ifdef  NVDXTC
}
#endif


#ifdef  NVDXTDLL

typedef void (*DXTDataTransfer)(DWORD count, void *buffer, void *);

#ifdef  NVDXTC
extern "C" {
#endif




#ifdef  NVDXTC
}
#endif

#else

#ifndef EXCLUDE_LIBS



#if _DEBUG

 #if _MSC_VER >=1300
  #ifdef _MT
   #ifdef _DLL
    #ifdef _STATIC_CPPLIB
     #pragma message("Note: including lib: nvDXTlibMTDLL_Sd.lib") 
     #pragma comment(lib, "nvDXTlibMTDLL_Sd.lib")
    #else
     #pragma message("Note: including lib: nvDXTlibMTDLLd.lib") 
     #pragma comment(lib, "nvDXTlibMTDLLd.lib")
    #endif
   #else // DLL
    #ifdef _STATIC_CPPLIB
     #pragma message("Note: including lib: nvDXTlibMT_Sd.lib") 
     #pragma comment(lib, "nvDXTlibMT_Sd.lib")
    #else
     #pragma message("Note: including lib: nvDXTlibMTd.lib") 
     #pragma comment(lib, "nvDXTlibMTd.lib")
    #endif
   #endif //_DLL
  #else // MT
    #ifdef _STATIC_CPPLIB
     #pragma message("Note: including lib: nvDXTlib_Sd.lib") 
     #pragma comment(lib, "nvDXTlib_Sd.lib")
    #else
     #pragma message("Note: including lib: nvDXTlibd.lib") 
     #pragma comment(lib, "nvDXTlibd.lib")
    #endif
  #endif // _MT
 #else // _MSC_VER

  #ifdef _MT
   #ifdef _DLL                         
    #pragma message("Note: including lib: nvDXTlibMTDLL6.lib") 
    #pragma comment(lib, "nvDXTlibMTDLL6.lib")
   #else // _DLL
    #pragma message("Note: including lib: nvDXTlibMT6.lib") 
    #pragma comment(lib, "nvDXTlibMT6.lib")
   #endif //_DLL
  #else // _MT
   #pragma message("Note: including lib: nvDXTlib6.lib") 
   #pragma comment(lib, "nvDXTlib6.lib")
  #endif // _MT
 
 #endif // _MSC_VER

#else // _DEBUG


 #if _MSC_VER >=1300
  #ifdef _MT
   #ifdef _DLL
    #ifdef _STATIC_CPPLIB
     #pragma message("Note: including lib: nvDXTlibMTDLL_S.lib") 
     #pragma comment(lib, "nvDXTlibMTDLL_S.lib")
    #else
     #pragma message("Note: including lib: nvDXTlibMTDLL.lib") 
     #pragma comment(lib, "nvDXTlibMTDLL.lib")
    #endif
   #else // DLL
    #ifdef _STATIC_CPPLIB
     #pragma message("Note: including lib: nvDXTlibMT_S.lib") 
     #pragma comment(lib, "nvDXTlibMT_S.lib")
    #else
     #pragma message("Note: including lib: nvDXTlibMT.lib") 
     #pragma comment(lib, "nvDXTlibMT.lib")
    #endif
   #endif //_DLL
  #else // MT
    #ifdef _STATIC_CPPLIB
     #pragma message("Note: including lib: nvDXTlib_S.lib") 
     #pragma comment(lib, "nvDXTlib_S.lib")
    #else
     #pragma message("Note: including lib: nvDXTlib.lib") 
     #pragma comment(lib, "nvDXTlib.lib")
    #endif
  #endif // _MT
 #else // _MSC_VER

  #ifdef _MT
   #ifdef _DLL                         
    #pragma message("Note: including lib: nvDXTlibMTDLL6.lib") 
    #pragma comment(lib, "nvDXTlibMTDLL6.lib")
   #else // _DLL
    #pragma message("Note: including lib: nvDXTlibMT6.lib") 
    #pragma comment(lib, "nvDXTlibMT6.lib")
   #endif //_DLL
  #else // _MT
   #pragma message("Note: including lib: nvDXTlib6.lib") 
   #pragma comment(lib, "nvDXTlib6.lib")
  #endif // _MT
 
 #endif // _MSC_VER
#endif // _DEBUG

#endif

#endif // NVDXTC




#define DXTERR_INPUT_POINTER_ZERO -1
#define DXTERR_DEPTH_IS_NOT_3_OR_4 -2
#define DXTERR_NON_POWER_2 -3
#define DXTERR_INCORRECT_NUMBER_OF_PLANES -4
#define DXTERR_NON_MUL4 -5


/* example

LPDIRECT3DTEXTURE8 pCurrentTexture = 0; 

HRESULT LoadAllMipSurfaces(void * data, int iLevel, void * user_data)
{
    HRESULT hr;
    LPDIRECT3DSURFACE8 psurf;
    D3DSURFACE_DESC sd;
    D3DLOCKED_RECT lr;
       
    hr = pCurrentTexture->GetSurfaceLevel(iLevel, &psurf);
    
    if (FAILED(hr))
        return hr;
    psurf->GetDesc(&sd);
    
    
    hr = pCurrentTexture->LockRect(iLevel, &lr, NULL, 0);
    if (FAILED(hr))
        return hr;
    
    memcpy(lr.pBits, data, sd.Size);
    
    hr = pCurrentTexture->UnlockRect(iLevel);
    
    ReleasePpo(&psurf);
    
    return 0;
}
       

    hr = D3DXCreateTexture(m_pd3dDevice, Width, Height, nMips,  0,   D3DFMT_DXT3,  D3DPOOL_MANAGED, &pCurrentTexture);
    nvDXTcompress(raw_data, Width, Height, DXT3, true, 4, LoadAllMipSurfaces, NULL);

*/



#ifdef NVDXTC
extern "C" {
#endif

/*
  
  readMIPMapCount, number of MIP maps to load. 0 is all


*/
void  nvDXTdecompress(
        imgdes & outputImage,
        int readMIPMapCount,
        FileReadCallback fileReadRoutine,
        void * userData);

// decompress to fpImage

void  nvDXTdecompress(
        fpImage & outputImage,
        int readMIPMapCount,
        FileReadCallback fileReadRoutine,
        void * userData);


#ifdef NVDXTC
}
#endif

#ifndef COLOR_FORMAT_ENUM
#define COLOR_FORMAT_ENUM

enum ColorFormat
{
	COLOR_RGB,
	COLOR_ARGB,
	COLOR_BGR,
	COLOR_BGRA,
	COLOR_RGBA,
	COLOR_ABGR,
};

#endif

