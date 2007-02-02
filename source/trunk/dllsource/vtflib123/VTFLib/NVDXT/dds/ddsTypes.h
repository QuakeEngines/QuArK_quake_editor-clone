#pragma once
typedef enum RescaleTypes
{
    kRescaleNone,               // no rescale
    kRescaleNearestPower2,     // rescale to nearest power of two
    kRescaleBiggestPower2,   // rescale to next bigger power of 2
    kRescaleSmallestPower2,  // rescale to smaller power of 2 
    kRescaleNextSmallestPower2,  // rescale to next smaller power of 2
    kRescalePreScale,           // rescale to this size
    kRescaleRelScale,           // relative rescale

} RescaleTypes;


typedef enum SharpenFilterTypes
{
    kSharpenFilterNone,
    kSharpenFilterNegative,
    kSharpenFilterLighter,
    kSharpenFilterDarker,
    kSharpenFilterContrastMore,
    kSharpenFilterContrastLess,
    kSharpenFilterSmoothen,
    kSharpenFilterSharpenSoft,
    kSharpenFilterSharpenMedium,
    kSharpenFilterSharpenStrong,
    kSharpenFilterFindEdges,
    kSharpenFilterContour,
    kSharpenFilterEdgeDetect,
    kSharpenFilterEdgeDetectSoft,
    kSharpenFilterEmboss,
    kSharpenFilterMeanRemoval,
    kSharpenFilterUnSharp,
    kSharpenFilterXSharpen,
    kSharpenFilterWarpSharp,
    kSharpenFilterCustom,
};


typedef enum MipMapGeneration
{
    kGenerateMipMaps = 30,
    kUseExistingMipMaps = 31,
    kNoMipMaps = 32,
};


typedef enum MipFilterTypes
{
    kMipFilterPoint ,    
    kMipFilterBox ,      
    kMipFilterTriangle , 
    kMipFilterQuadratic ,
    kMipFilterCubic ,    

    kMipFilterCatrom ,   
    kMipFilterMitchell , 

    kMipFilterGaussian , 
    kMipFilterSinc ,     
    kMipFilterBessel ,   

    kMipFilterHanning ,  
    kMipFilterHamming ,  
    kMipFilterBlackman , 
    kMipFilterKaiser,
};


enum TextureFormats
{
    kDXT1 ,
    kDXT1a ,  // DXT1 with one bit alpha
    kDXT3 ,   // explicit alpha
    kDXT5 ,   // interpolated alpha
    k4444 ,   // a4 r4 g4 b4
    k1555 ,   // a1 r5 g5 b5
    k565 ,    // a0 r5 g6 b5
    k8888 ,   // a8 r8 g8 b8
    k888 ,    // a0 r8 g8 b8
    k555 ,    // a0 r5 g5 b5
    k8c   ,   // paletted color only
    kV8U8 ,   // DuDv 
    kCxV8U8 ,   // normal map
    kA8 ,            // alpha only
    k4c  ,            // 16 bit color palette
    kQ8W8V8U8,
    kA8L8,
    kR32F,
    kA32B32G32R32F,
    kA16B16G16R16F,
    kL8,       // luminance

    k8a   ,   // paletted with alpha
    k4a  ,            // 16 bit color palette with alpha
    kR16F,            // single component fp16
    kDXT5_NM ,   // normal map compression.  G = Y, alpha = X
    k3Dc,              // ATI's normal map scheme


    
};


enum TextureTypes
{
    kTextureType2D,
    kTextureTypeCube,
    kTextureTypeVolume,  
};


enum CompressionWeighting
{
    kLuminanceWeighting,
    kGreyScaleWeighting,
    kTangentSpaceNormalMapWeighting,
    kObjectSpaceNormalMapWeighting,
    kUserDefinedWeighting, // used values stored in 'weight'
};
    
enum NormalMapFilters
{
    kFilter4x = 1040,
    kFilter3x3 = 1041,
    kFilter5x5 = 1042,
    kFilter7x7 = 1043,
    kFilter9x9 = 1044,
    kFilterDuDv = 1045,
};


enum HeightConversionMethods
{
    kAlphaChannel = 1009,
    kAverageRGB = 1010,
    kBiasedRGB = 1011,
    kRed = 1012,
    kGreen = 1013,
    kBlue = 1014,
    kMaxRGB = 1015,
    kColorSpace = 1016,
    kNormalize = 1017,
    kNormalMapToHeightMap = 1018,
};

enum AlphaResult
{
    kAlphaNone = 1033,
    kAlphaHeight = 1034,
    kAlphaClear = 1035,
    kAlphaWhite = 1036,
};

         
enum QualitySetting 
{
    kQualityFastest = 68,
    kQualityNormal = 69,
    kQualityHighest = 70,
};