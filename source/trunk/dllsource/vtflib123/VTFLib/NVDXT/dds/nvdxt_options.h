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

#include <windows.h>
#include <dds/tPixel.h>
#include <dds/ddsTypes.h>

typedef HRESULT (*MIPcallback)(void * data, int miplevel, DWORD size, int width, int height, void * user_data);
                     

inline char * GetDXTCVersion() { return "Version 7.82";}

// max mip maps
#define MAX_MIP_MAPS 14



#include <dds/ddsTypes.h>





typedef unsigned char nvBoolean;  // for photoshop scripting




typedef struct nvNormalMap
{
public:
        
    nvNormalMap()
    {
        bEnableNormalMapConversion = false;

        minz = 0;
        scale = 1;
        filterKernel = kFilter3x3;
         
        heightConversionMethod = kAverageRGB;
        alphaResult = kAlphaNone;

        bWrap = false;
        bInvertX = false;
        bInvertY = false;
        bInvertZ = false;
        bAddHeightMap = false;
        bNormalMapSwapRGB = false;


    }
    nvBoolean bEnableNormalMapConversion; // do not convert to a normal map
    int minz;       // minimum value the z value can attain in the x y z  normal
                    // keeps normal point "upwards"
    float scale;    // height multiplier



    NormalMapFilters filterKernel;  // kernel used to create normal maps.  Done this way to be compatible with plugins
    HeightConversionMethods heightConversionMethod;  // method to convert color to height
    AlphaResult alphaResult;     // what to do with the alpha channel when done

    nvBoolean bWrap;
    nvBoolean bInvertX;       // flip the x direction
    nvBoolean bInvertY;       // flip the y direction
    nvBoolean bInvertZ;       // flip the z direction

    nvBoolean bAddHeightMap;
    nvBoolean bNormalMapSwapRGB; // swap color channels
} nvNormalMap;

typedef struct CompressionOptions
{
    CompressionOptions()
    {
        SetDefaultOptions();

    }

    void SetDefaultOptions()
    {
        quality = kQualityHighest;
        rescaleImageType = kRescaleNone; 
        rescaleImageFilter = kMipFilterCubic; 
        scaleX = 1;
        scaleY = 1;
        bClamp = false;
        clampX = 4096;
        clampY = 4096;

        bClampScale = false;
        clampScaleX = 4096;
        clampScaleY = 4096;

        mipMapGeneration = kGenerateMipMaps;         // dNoMipMaps, dUseExistingMipMaps, dGenerateMipMaps
        specifiedMipMaps = 0;   // if dSpecifyMipMaps or dUseExistingMipMaps is set (number of mipmaps to generate)

        mipFilterType = kMipFilterTriangle;      // for MIP maps
        
        bBinaryAlpha = false;       // zero or one alpha channel




        bRGBE = false;

        bAlphaBorder = false;       // make an alpha border
        bAlphaBorderLeft = false;    
        bAlphaBorderRight = false;   

        bAlphaBorderTop = false;     
        bAlphaBorderBottom = false;  

        


        bBorder = false;            // make a color border
        borderColor.u = 0;        // color of border


        bFadeColor = false;         // fade color over MIP maps
        bFadeAlpha = false;         // fade alpha over MIP maps

        fadeToColor.r = 0;        // color to fade to
        fadeToColor.g = 0;        // color to fade to
        fadeToColor.b = 0;        // color to fade to
        fadeToColor.a = 0;        // alpha to fade to

        fadeToDelay = 0;        // start fading after 'n' MIP maps

        fadeAmount = 15;         // percentage of color to fade in

        alphaThreshold = 128;  // When Binary Alpha is selected, below this value, alpha is zero


        bDitherColor = false;       // enable dithering during 16 bit conversion
        bDitherMip0 = false;// enable dithering during 16 bit conversion for each MIP level (after filtering)

        bForceDXT1FourColors = false;  // do not let DXT1 use 3 color representation


        sharpenFilterType = kSharpenFilterNone;
        bErrorDiffusion = false;

        weightType = kLuminanceWeighting;     
        bNormalizeTexels = false;

        weight[0] = 0.3086f; // luminance conversion values   
        weight[1] = 0.6094f;    
        weight[2] = 0.0820f;    

        // gamma value for all filters
        bEnableFilterGamma = false;
        filterGamma = 2.2f;

        // alpha value for 
        filterBlur = 1.0f;
        // width of filter
        filterWidth = 10.0f;
        bOverrideFilterWidth = false;

        textureType = kTextureType2D;        // regular decal, cube or volume  
        textureFormat = kDXT1;	             // 

        bSwapRGB = false;           // swap color positions R and G
        user_data = NULL;            // user supplied point passed down to write functions

        int i,j;

        float default_filter[5][5] = 
        { 
            0, 0, 0, 0, 0,
                0,0,-2,0,0,
                0,-2,11,-2,0,
                0, 0,-2, 0,0,
                0, 0, 0, 0, 0,  
        };


        for(i = 0; i<5; i++)
            for(j = 0; j<5; j++)
                custom_filter_data.filter[i][j] = default_filter[i][j];


        custom_filter_data.div = 3;   // div
        custom_filter_data.bias = 0;  // bias

        unsharp_data.radius = 5.0; // radius
        unsharp_data.amount = 0.5;  // amount
        unsharp_data.threshold = 0;     // threshold

        xsharp_data.strength  = 255; // xsharp strength
        xsharp_data.threshold  = 255; // xsharp threshold


        sharpening_passes_per_mip_level[0] = 0;

        for(i = 1; i<MAX_MIP_MAPS; i++)
            sharpening_passes_per_mip_level[i] = 1;

        bAlphaModulate = false;


        bUserSpecifiedFadingAmounts = false;
        userFadingSet = 0;


        for(i = 0; i<256; i++)
        {
            color_palette[i].r = 0;
            color_palette[i].g = 0;
            color_palette[i].b = 0;
            color_palette[i].a = 0;
        }

        paletteSize = 0;                // this will be set by the format read or write
        autoGeneratePalette = false;


        outputScale.r = 1.0;             // scale and bias when writing output values
        outputScale.g = 1.0;             // scale and bias when writing output values
        outputScale.b = 1.0;             // scale and bias when writing output values
        outputScale.a = 1.0;             // scale and bias when writing output values

        outputBias.r = 0.0;
        outputBias.g = 0.0;
        outputBias.b = 0.0;
        outputBias.a = 0.0;

        inputScale.r = 1.0;             // scale and bias after loading data
        inputScale.g = 1.0;             // scale and bias 
        inputScale.b = 1.0;             // scale and bias 
        inputScale.a = 1.0;             // scale and bias 

        inputBias.r = 0.0;
        inputBias.g = 0.0;
        inputBias.b = 0.0;
        inputBias.a = 0.0;

        bCalcLuminance = false;       // do not convert to luminance by default

        bOutputWrap = false;          // wrap the values when outputting to the desired format
                                      // 

    }


    QualitySetting      quality;

    
    CompressionWeighting weightType;   // weighting type for DXT compressop
    float               weight[3];                    // weights used for compress



    nvNormalMap         normalMap;               // filled when processing normal maps
    nvBoolean           bNormalizeTexels;


    nvBoolean           bClamp;             // Clamp to max size     
    float               clampX;             // clamping values
    float               clampY;

    nvBoolean           bClampScale;      // maximum value of h or w (retain scale)
    float               clampScaleX;             // clamping values
    float               clampScaleY;
                                        

    RescaleTypes        rescaleImageType;     // rescaling type before image before compression
    MipFilterTypes      rescaleImageFilter;   // rescaling filter

    float               scaleX;             // scale to this if we are prescaling images before compressing
    float               scaleY;

    MipMapGeneration    mipMapGeneration;         // changed MIPMaptype to an enum

    long                specifiedMipMaps;   // if dSpecifyMipMaps or dUseExistingMipMaps is set (number of mipmaps to generate)

    MipFilterTypes      mipFilterType;      // for MIP map, select from MIPFilterTypes

    nvBoolean           bBinaryAlpha;       // zero or one alpha channel

    int                 alphaThreshold;          // threshold for alpha transparency DXT1
                                            // or When Binary Alpha is selected, below this value, alpha is zero



    nvBoolean           bRGBE;                  // convert to RGBE
    nvBoolean           bAlphaBorder;           // make an alpha border
    nvBoolean           bAlphaBorderLeft;       // make an alpha border on just the left
    nvBoolean           bAlphaBorderRight;      // make an alpha border on justthe right

    nvBoolean           bAlphaBorderTop;        // make an alpha border on just the top
    nvBoolean           bAlphaBorderBottom;      // make an alpha 


    nvBoolean           bBorder;            // make a color border
    rgba_t              borderColor;        // color of border


    nvBoolean           bFadeColor;         // fade color over MIP maps
    nvBoolean           bFadeAlpha;         // fade alpha over MIP maps

    rgba_t              fadeToColor;        // color and alpha to fade to

    long                fadeToDelay;        // start fading after 'n' MIP maps

    long                fadeAmount;         // percentage of color to fade in


    // dithering is currently disabled
    nvBoolean           bDitherColor;       // enable dithering during 16 bit conversion
    nvBoolean           bDitherMip0;        // enable dithering during 16 bit conversion for each MIP level (after filtering)

    nvBoolean           bForceDXT1FourColors;  // do not let DXT1 use 3 color representation


    // sharpening after creating each MIP map level

    // used when custom sharping filter is used
    // 5x5 filter 
    struct 
    {
        float filter[5][5];
        float div;
        float bias;

    }  custom_filter_data; 



    // used when unsharpen sharping filter is used
    struct  
    {
        float radius; // radius
        float amount;  // amount
        float threshold;     // threshold

    } unsharp_data; 
    
    // used when xsharpen sharping filter is used
    struct 
    {
        // 0 - 255, stored as float
        float strength;
        float threshold;
    } xsharp_data;


    int                 sharpening_passes_per_mip_level[MAX_MIP_MAPS]; 

    SharpenFilterTypes  sharpenFilterType; // post filtering image sharpening

    nvBoolean           bErrorDiffusion;        // diffuse error, used for helping gradient images

    // convert to gamma space before filtering
    nvBoolean           bEnableFilterGamma;
    float               filterGamma;               // gamma value for filtering (MIP map generation)


    float               filterBlur;               // sharpness or blurriness of filtering
    nvBoolean           bOverrideFilterWidth; // use the specified width in FilterWidth,instead of the default
    float               filterWidth;     // override fiter width with this value

	TextureTypes 		textureType;        // what type of texture is this?
	TextureFormats 		textureFormat;	    // format to convert to

    nvBoolean           bSwapRGB;             // swap color positions R and G

    bool                bAlphaModulate;            // modulate color by alpha for filtering


    nvBoolean           bUserSpecifiedFadingAmounts;
    float               userFadingAmounts[MAX_MIP_MAPS];
    int                 userFadingSet;

    int                 paletteSize;
    rgba_t              color_palette[256];
    nvBoolean           autoGeneratePalette;     // generate palette for p8 and p4 formats

    fpPixel             outputScale;             // scale and bias when writing output values
    fpPixel             outputBias;

    fpPixel             inputScale;             // scale and bias when writing output values
    fpPixel             inputBias;

    nvBoolean           bOutputWrap;          // wrap the values (before clamping to the format range) 
                                    // when outputting to the desired format

    nvBoolean           bCalcLuminance; // convert color to luminance for 'L' formats

    void *              user_data;               // user supplied values passed down to write functions

} CompressionOptions;

