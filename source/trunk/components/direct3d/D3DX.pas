unit D3DX;
///////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 1999 Microsoft Corporation.  All Rights Reserved.
//
//  Files:      d3dx.h, d3dxcore.h, d3dxmath.h, d3dxerr.h,
//              d3dxshapes.h, d3dxsprite.h
//
//  Content:    D3DX utility library
//
//  DirectX 7.0 Delphi adaptation by Arne Schäpers
//  NOTE: Requires D3DXAS[D].DLL
//
//  Modified:   14-JUN-2000
//
//  Download:   http://www.delphi-jedi.org/DelphiGraphics/
//
///////////////////////////////////////////////////////////////////////////

{$MINENUMSIZE 4}
{$ALIGN ON}

// TD3DXVector3 = TD3DVector, TD3DXMatrix = TD3DMatrix
//{QuArK - disabled} {$DEFINE USE_D3DTYPES_FOR_D3DX}

{ $DEFINE DEBUG}
interface
uses Windows, SysUtils, Direct3D, DirectDraw;

const D3DXDLL = {$IFDEF DEBUG} (*'d3dxasd.dll'*) 'dlls/d3dxas.dll'; {$ELSE} 'dlls/d3dxas.dll'; {$ENDIF} {QuArK - 'dlls' path}

type PFloat = ^Float;

function EqualGUID(const G1,G2: TGUID): Boolean;  // from D3DUtils

// ===================== D3DXErr ============================
//----------------------------------------------------------------------
//
//   d3dxerr.h --  0xC code definitions for the D3DX API
//
//   Copyright (c) 1991-1999, Microsoft Corp. All rights reserved.
//
//----------------------------------------------------------------------
//
//  Values are 32 bit values layed out as follows:
//
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//  +---+-+-+-----------------------+-------------------------------+
//  |Sev|C|R|     Facility          |               Code            |
//  +---+-+-+-----------------------+-------------------------------+
//
//  where
//
//      Sev - is the severity code
//
//          00 - Success
//          01 - Informational
//          10 - Warning
//          11 - Error
//
//      C - is the Customer code flag
//
//      R - is a reserved bit
//
//      Facility - is the facility code
//
//      Code - is the facility's status code
//
//
// Define the facility codes
//
const
  FACILITY_D3DX    = $877;

// MessageId: D3DXERR_NOMEMORY
// MessageText: Out of memory.
  D3DXERR_NOMEMORY    = HRESULT($C8770BB8);

// MessageId: D3DXERR_NULLPOINTER
// MessageText: A NULL pointer was passed as a parameter.
  D3DXERR_NULLPOINTER    = HRESULT($C8770BB9);

// MessageId: D3DXERR_INVALIDD3DXDEVICEINDEX
// MessageText: The Device Index passed in is invalid.
  D3DXERR_INVALIDD3DXDEVICEINDEX    = HRESULT($C8770BBA);

// MessageId: D3DXERR_NODIRECTDRAWAVAILABLE
// MessageText: DirectDraw has not been created.
  D3DXERR_NODIRECTDRAWAVAILABLE    = HRESULT($C8770BBB);

// MessageId: D3DXERR_NODIRECT3DAVAILABLE
// MessageText: Direct3D has not been created.
  D3DXERR_NODIRECT3DAVAILABLE    = HRESULT($C8770BBC);

// MessageId: D3DXERR_NODIRECT3DDEVICEAVAILABLE
// MessageText: Direct3D device has not been created.
  D3DXERR_NODIRECT3DDEVICEAVAILABLE    = HRESULT($C8770BBD);

// MessageId: D3DXERR_NOPRIMARYAVAILABLE
// MessageText: Primary surface has not been created.
  D3DXERR_NOPRIMARYAVAILABLE    = HRESULT($C8770BBE);

// MessageId: D3DXERR_NOZBUFFERAVAILABLE
// MessageText: Z buffer has not been created.
  D3DXERR_NOZBUFFERAVAILABLE    = HRESULT($C8770BBF);

// MessageId: D3DXERR_NOBACKBUFFERAVAILABLE
// MessageText: Backbuffer has not been created.
  D3DXERR_NOBACKBUFFERAVAILABLE    = HRESULT($C8770BC0);

// MessageId: D3DXERR_COULDNTUPDATECAPS
// MessageText: Failed to update caps database after changing display mode.
  D3DXERR_COULDNTUPDATECAPS    = HRESULT($C8770BC1);

// MessageId: D3DXERR_NOZBUFFER
// MessageText: Could not create Z buffer.
  D3DXERR_NOZBUFFER    = HRESULT($C8770BC2);

// MessageId: D3DXERR_INVALIDMODE
// MessageText: Display mode is not valid.
  D3DXERR_INVALIDMODE    = HRESULT($C8770BC3);

// MessageId: D3DXERR_INVALIDPARAMETER
// MessageText: One or more of the parameters passed is invalid.
  D3DXERR_INVALIDPARAMETER    = HRESULT($C8770BC4);

// MessageId: D3DXERR_INITFAILED
// MessageText: D3DX failed to initialize itself.
  D3DXERR_INITFAILED    = HRESULT($C8770BC5);

// MessageId: D3DXERR_STARTUPFAILED
// MessageText: D3DX failed to start up.
  D3DXERR_STARTUPFAILED    = HRESULT($C8770BC6);

// MessageId: D3DXERR_D3DXNOTSTARTEDYET
// MessageText: D3DXInitialize() must be called first.
  D3DXERR_D3DXNOTSTARTEDYET    = HRESULT($C8770BC7);

// MessageId: D3DXERR_NOTINITIALIZED
// MessageText: D3DX is not initialized yet.
  D3DXERR_NOTINITIALIZED    = HRESULT($C8770BC8);

// MessageId: D3DXERR_FAILEDDRAWTEXT
// MessageText: Failed to render text to the surface.
  D3DXERR_FAILEDDRAWTEXT    = HRESULT($C8770BC9);

// MessageId: D3DXERR_BADD3DXCONTEXT
// MessageText: Bad D3DX context.
  D3DXERR_BADD3DXCONTEXT    = HRESULT($C8770BCA);

// MessageId: D3DXERR_CAPSNOTSUPPORTED
// MessageText: The requested device capabilities are not supported.
  D3DXERR_CAPSNOTSUPPORTED    = HRESULT($C8770BCB);

// MessageId: D3DXERR_UNSUPPORTEDFILEFORMAT
// MessageText: The image file format is unrecognized.
  D3DXERR_UNSUPPORTEDFILEFORMAT    = HRESULT($C8770BCC);

// MessageId: D3DXERR_IFLERROR
// MessageText: The image file loading library error.
  D3DXERR_IFLERROR    = HRESULT($C8770BCD);

// MessageId: D3DXERR_FAILEDGETCAPS
// MessageText: Could not obtain device caps.
  D3DXERR_FAILEDGETCAPS    = HRESULT($C8770BCE);

// MessageId: D3DXERR_CANNOTRESIZEFULLSCREEN
// MessageText: Resize does not work for full-screen.
  D3DXERR_CANNOTRESIZEFULLSCREEN    = HRESULT($C8770BCF);

// MessageId: D3DXERR_CANNOTRESIZENONWINDOWED
// MessageText: Resize does not work for non-windowed contexts.
  D3DXERR_CANNOTRESIZENONWINDOWED    = HRESULT($C8770BD0);

// MessageId: D3DXERR_FRONTBUFFERALREADYEXISTS
// MessageText: Front buffer already exists.
  D3DXERR_FRONTBUFFERALREADYEXISTS    = HRESULT($C8770BD1);

// MessageId: D3DXERR_FULLSCREENPRIMARYEXISTS
// MessageText: The app is using the primary in full-screen mode.
  D3DXERR_FULLSCREENPRIMARYEXISTS    = HRESULT($C8770BD2);

// MessageId: D3DXERR_GETDCFAILED
// MessageText: Could not get device context.
  D3DXERR_GETDCFAILED    = HRESULT($C8770BD3);

// MessageId: D3DXERR_BITBLTFAILED
// MessageText: Could not bitBlt.
  D3DXERR_BITBLTFAILED    = HRESULT($C8770BD4);

// MessageId: D3DXERR_NOTEXTURE
// MessageText: There is no surface backing up this texture.
  D3DXERR_NOTEXTURE    = HRESULT($C8770BD5);

// MessageId: D3DXERR_MIPLEVELABSENT
// MessageText: There is no such miplevel for this surface.
  D3DXERR_MIPLEVELABSENT    = HRESULT($C8770BD6);

// MessageId: D3DXERR_SURFACENOTPALETTED
// MessageText: The surface is not paletted.
  D3DXERR_SURFACENOTPALETTED    = HRESULT($C8770BD7);

// MessageId: D3DXERR_ENUMFORMATSFAILED
// MessageText: An error occured while enumerating surface formats.
  D3DXERR_ENUMFORMATSFAILED    = HRESULT($C8770BD8);

// MessageId: D3DXERR_COLORDEPTHTOOLOW
// MessageText: D3DX only supports color depths of 16 bit or greater.
  D3DXERR_COLORDEPTHTOOLOW    = HRESULT($C8770BD9);

// MessageId: D3DXERR_INVALIDFILEFORMAT
// MessageText: The file format is invalid.
  D3DXERR_INVALIDFILEFORMAT    = HRESULT($C8770BDA);

// MessageId: D3DXERR_NOMATCHFOUND
// MessageText: No suitable match found.
  D3DXERR_NOMATCHFOUND    = HRESULT($C8770BDB);

function D3DXGetErrorMsg(Res: HResult): String;  // Delphi Helper

// ==================== D3DXCore ============================

const
  // {9B74ED7A-BBEF-11d2-9F8E-0000F8080835}
  IID_ID3DXContext: TGUID =
     (D1:$9b74ed7a;D2:$bbef;D3:$11d2;D4:($9f, $8e, $0, $0, $f8, $8, $8, $35));

///////////////////////////////////////////////////////////////////////////
// Interfaces:
///////////////////////////////////////////////////////////////////////////

//-------------------------------------------------------------------------
// ID3DXContext interface:
//
// This encapsulates all the stuff that the app might
// want to do at initialization time and any global control over d3d and
// ddraw.
//-------------------------------------------------------------------------
type
  ID3DXContext = interface (IUnknown)
    ['{9B74ED7A-BBEF-11d2-9F8E-0000F8080835}']
    (*** ID3DXContext methods ***)
    // Get the DDraw and Direct3D objects to call DirectDraw or
    // Direct3D Immediate Mode functions.
    // If the objects don't exist (because they have not
    // been created for some reason) NULL is returned.
    // All the objects returned in the following Get* functions
    // are addref'ed. It is the application's responsibility to
    // release them when no longer needed.

    // Note from AS: Delphi problems with COM pointers as function
    // results require these member functions to be declared with
    // a simple Pointer result type. Use D3DXDDFromContext instead
    // or employ a hard type cast:
    // MyDD := IDirectDraw7(MyD3DX.GetDD);
    function GetDD: Pointer; stdcall; // IDirectDraw7; stdcall;
    function GetD3D: Pointer; stdcall; // IDirect3D7; stdcall;
    function GetD3DDevice: Pointer; stdcall; // IDirect3DDevice7; stdcall;

    // Get the various buffers that get created at the init time
    // These are addref'ed as well. It is the application's responsibility
    // to release them before the app quits or when it needs a resize.
    function GetPrimary: Pointer; stdcall; // IDirectDrawSurface7; stdcall;
    function GetZBuffer: Pointer; stdcall; // IDirectDrawSurface7; stdcall;
    function GetBackBuffer(which: Cardinal): Pointer; stdcall; // IDirectDrawSurface7; stdcall;

    // Get the associated window handles
    function GetWindow: HWnd; stdcall;
    function GetFocusWindow: HWnd; stdcall;

    // Various Get methods, in case the user had specified default
    // parameters
    function GetDeviceIndex(var DeviceIndex, HWLevel: DWord): HResult; stdcall;
    function GetNumBackBuffers: DWord; stdcall;

    function GetNumBits(var pColorBits, DepthBits,
               AlphaBits, StencilBits: DWord): HResult; stdcall;

    function GetBuffferSize(var Width, Height: DWord): HResult; stdcall;

    // Get the flags that were used to create this context
    function GetCreationFlags: DWord; stdcall;
    function GetRefreshRate: DWord; stdcall;

    // Restoring surfaces in case stuff is lost
    function RestoreSurfaces: HResult; stdcall;

    // Resize all the buffers to the new width and height
    function Resize(Width, Height: DWord): HResult; stdcall;

    // Update the frame using a flip or a blit,
    // If the D3DX_UPDATE_NOVSYNC flag is set, blit is used if the
    // driver cannot flip without waiting for vsync in full-screen mode.
    function UpdateFrame(flags: DWord): Hresult; stdcall;

    // Render a string at the specified coordinates, with the specified
    // colour. This is only provided as a convenience for
    // debugging/information during development.
    // topLeftX and topLeftY represent the location of the top left corner
    // of the string, on the render target.
    // The coordinate and color parameters each have a range of 0.0-1.0
    function DrawDebugText(topLeftX, topLeftY: Float;
      Color: TD3DCOLOR; Str: PAnsiChar): HResult; stdcall;

    // Clears to the current viewport
    // The following are the valid flags:
    // D3DCLEAR_TARGET  (to clear the render target )
    // D3DCLEAR_ZBUFFER (to clear the depth-buffer )
    // D3DCLEAR_STENCIL (to clear the stencil-buffer )
    function Clear(ClearFlags: DWord): HResult; stdcall;

    function SetClearColor(Color: TD3DColor): HResult; stdcall;
    function SetClearDepth(z: float): HResult; stdcall;
    function SetClearStencil(stencil: DWord): HResult; stdcall;
  end;

  PID3DXContext = ID3DXContext;

function D3DXD3DFromContext(const Context: ID3DXContext; out D3D: IDirect3D7): Boolean;
function D3DXD3DDeviceFromContext(const Context: ID3DXContext; out D3DDev: IDirect3DDevice7): Boolean;
function D3DXDDFromContext(const Context: ID3DXContext; out DD: IDirectDraw7): Boolean;
function D3DXPrimaryFromContext(const Context: ID3DXContext; out Primary: IDirectDrawSurface7): Boolean;
function D3DXZBufferFromContext(const Context: ID3DXContext; out ZBuffer: IDirectDrawSurface7): Boolean;
function D3DXBackBufferFromContext(const Context: ID3DXContext; which: Cardinal; out BackBuffer: IDirectDrawSurface7): Boolean;

//-------------------------------------------------------------------------
// Flags for Update member function:
//

// Flag to indicate that blit should be used instead of a flip
// for full-screen rendering.
const D3DX_UPDATE_NOVSYNC = 1 shl 0;


///////////////////////////////////////////////////////////////////////////
// Defines and Enumerators used below:
///////////////////////////////////////////////////////////////////////////

//-------------------------------------------------------------------------
// D3DX_DEFAULT:
// ---------
// A predefined value that could be used for any parameter in D3DX APIs or
// member functions that is an enumerant or a handle.  The D3DX
// documentation indicates wherever D3DX_DEFAULT may be used,
// and how it will be interpreted in each situation.
//-------------------------------------------------------------------------
const D3DX_DEFAULT = Cardinal($ffffffff);  // ULONG_MAX

//-------------------------------------------------------------------------
// D3DX_DEFAULT_FLOAT:
// ------------------
// Similar to D3DX_DEFAULT, but used for floating point parameters.
// The D3DX documentation indicates wherever D3DX_DEFAULT_FLOAT may be used,
// and how it will be interpreted in each situation.
//-------------------------------------------------------------------------
const D3DX_DEFAULT_FLOAT = 3.402823466e+38; //  FLT_MAX

//-------------------------------------------------------------------------
// Hardware Acceleration Level:
// ---------------------------
// These constants represent pre-defined hardware acceleration levels,
// and may be used as a default anywhere a (DWORD) deviceIndex is required.
// Each pre-define indicates a different level of hardware acceleration.
// They are an alternative to using explicit deviceIndices retrieved by
// D3DXGetDeviceDescription().
//
// The only case these pre-defines should be used as device indices is if
// a particular level of acceleration is required, and given more than
// one capable device on the computer, it does not matter which one
// is used.
//
// The method of selection is as follows: If one of the D3DX devices on
// the primary DDraw device supports a requested hardware acceleration
// level, it will be used. Otherwise, the first matching device discovered
// by D3DX will be used.
//
// Of course, it is possible for no match to exist for any of the
// pre-defines on a particular computer.  Passing such a value into the
// D3DX apis will simply cause them to fail, reporting that no match
// is available.
//
// D3DX_HWLEVEL_NULL:      Null implementation (draws nothing)
// D3DX_HWLEVEL_REFERENCE: Reference implementation (slowest)
// D3DX_HWLEVEL_2D:        2D acceleration only (RGB rasterizer used)
// D3DX_HWLEVEL_RASTER:    Rasterization acceleration (likely most useful)
// D3DX_HWLEVEL_TL:        Transform and lighting acceleration
// D3DX_DEFAULT:           The highest level of acceleration available
//                         on the primary DDraw device.
//-------------------------------------------------------------------------
const
  D3DX_HWLEVEL_NULL       = (D3DX_DEFAULT - 1);
  D3DX_HWLEVEL_REFERENCE  = (D3DX_DEFAULT - 2);
  D3DX_HWLEVEL_2D         = (D3DX_DEFAULT - 3);
  D3DX_HWLEVEL_RASTER     = (D3DX_DEFAULT - 4);
  D3DX_HWLEVEL_TL         = (D3DX_DEFAULT - 5);

//-------------------------------------------------------------------------
// Surface Class:
// -------------
// These are the various types of 2D-surfaces classified according to their
// usage. Note that a number of them overlap. e.g. STENCILBUFFERS and
// DEPTHBUFFERS overlap (since in DX7 implementation the stencil and depth
// bits are part of the same pixel format).
//
// Mapping to the DX7 DDPIXELFORMAT concepts:
// -----------------------------------------
// D3DX_SC_DEPTHBUFFER:    All ddpfs which have the DDPF_ZPIXELS or the
//                           DDPF_ZBUFFER flags set.
// D3DX_SC_STENCILBUFFER:  All ddpfs which have the DDPF_STENCILBUFFER
//                          flag set.
// D3DX_SC_BUMPMAP:        All ddpfs which have the DDPF_BUMPLUMINANCE
//                           or the DDPF_BUMPDUDV flags set.
// D3DX_SC_LUMINANCEMAP:   All ddpfs which have the DDPF_BUMPLUMINANCE
//                           or the DDPF_LUMINANCE flags set.
// D3DX_SC_COLORTEXTURE:   All the surfaces that have color information in
//                           them and can be used for texturing.
// D3DX_SC_COLORRENDERTGT: All the surfaces that contain color
//                           information and can be used as render targets.
//-------------------------------------------------------------------------
const
   D3DX_SC_DEPTHBUFFER     = $01;
   D3DX_SC_STENCILBUFFER   = $02;
   D3DX_SC_COLORTEXTURE    = $04;
   D3DX_SC_BUMPMAP         = $08;
   D3DX_SC_LUMINANCEMAP    = $10;
   D3DX_SC_COLORRENDERTGT  = $20;

//-------------------------------------------------------------------------
// Surface Formats:
// ---------------
// These are the various types of surface formats that can be enumerated,
// there is no DDPIXELFORMAT structure in D3DX, the enums carry the meaning
// (like FOURCCs).
//
// All the surface classes are represented here.
//
//-------------------------------------------------------------------------
type TD3DX_SURFACEFORMAT = DWord;

const  // really enum D3DX_SURFACEFORMAT
  D3DX_SF_UNKNOWN    =  0;
  D3DX_SF_R8G8B8     =  1;
  D3DX_SF_A8R8G8B8   =  2;
  D3DX_SF_X8R8G8B8   =  3;
  D3DX_SF_R5G6B5     =  4;
  D3DX_SF_R5G5B5     =  5;
  D3DX_SF_PALETTE4   =  6;
  D3DX_SF_PALETTE8   =  7;
  D3DX_SF_A1R5G5B5   =  8;
  D3DX_SF_X4R4G4B4   =  9;
  D3DX_SF_A4R4G4B4   = 10;
  D3DX_SF_L8         = 11;      // 8 bit luminance-only
  D3DX_SF_A8L8       = 12;      // 16 bit alpha-luminance
  D3DX_SF_U8V8       = 13;      // 16 bit bump map format
  D3DX_SF_U5V5L6     = 14;      // 16 bit bump map format with luminance
  D3DX_SF_U8V8L8     = 15;      // 24 bit bump map format with luminance
  D3DX_SF_UYVY       = 16;      // UYVY format (PC98 compliance)
  D3DX_SF_YUY2       = 17;      // YUY2 format (PC98 compliance)
  D3DX_SF_DXT1       = 18;      // S3 texture compression technique 1
  D3DX_SF_DXT3       = 19;      // S3 texture compression technique 3
  D3DX_SF_DXT5       = 20;      // S3 texture compression technique 5
  D3DX_SF_R3G3B2     = 21;      // 8 bit RGB texture format
  D3DX_SF_A8         = 22;      // 8 bit alpha-only
  D3DX_SF_TEXTUREMAX = 23;      // Last texture format

  D3DX_SF_Z16S0      = 256;
  D3DX_SF_Z32S0      = 257;
  D3DX_SF_Z15S1      = 258;
  D3DX_SF_Z24S8      = 259;
  D3DX_SF_S1Z15      = 260;
  D3DX_SF_S8Z24      = 261;
  D3DX_SF_DEPTHMAX   = 262;     // Last depth format

//  D3DX_SF_FORCEMAX  = DWORD(-1)


//-------------------------------------------------------------------------
// Filtering types for Texture APIs
//
// -------------
// These are the various filter types for generation of mip-maps
//
// D3DX_FILTERTYPE
// -----------------------------------------
// D3DX_FT_POINT:   Point sampling only - no filtering
// D3DX_FT_LINEAR:  Bi-linear filtering
//
//-------------------------------------------------------------------------
type TD3DX_FILTERTYPE = DWord;

const  // really enum D3DX_FILTERTYPE
  D3DX_FT_POINT    = $01;
  D3DX_FT_LINEAR   = $02;
  D3DX_FT_DEFAULT  = D3DX_DEFAULT;


///////////////////////////////////////////////////////////////////////////
// Structures used below:
///////////////////////////////////////////////////////////////////////////

//-------------------------------------------------------------------------
// D3DX_VIDMODEDESC: Display mode description.
// ----------------
// width:       Screen Width
// height:      Screen Height
// bpp:         Bits per pixel
// refreshRate: Refresh rate
//-------------------------------------------------------------------------
type
  D3DX_VIDMODEDESC = record
    width, height: DWord;
    bpp: DWord;
    refreshRate: DWord;
  end;
  PD3DX_VIDMODEDESC = ^D3DX_VIDMODEDESC;

//-------------------------------------------------------------------------
// D3DX_DEVICEDESC: Description of a device that can do 3D
// ---------------
// deviceIndex:   Unique (DWORD) number for the device.
// hwLevel:       Level of acceleration afforded.  This is one of the
//                predefined Device Indices, and exists in this
//                structure for informational purposes only.  More than
//                one device on the system may have the same hwLevel.
//                To refer to a particular device with the D3DX apis,
//                use the value in the deviceIndex member instead.
// ddGuid:        The ddraw GUID
// d3dDeviceGuid: Direct3D Device GUID
// ddDeviceID:    DDraw's GetDeviceIdentifier GUID.  This GUID is unique to
//                a particular driver revision on a particular video card.
// driverDesc:    String describing the driver
// monitor:       Handle to the video monitor used by this device (multimon
//                specific).  Devices that use different monitors on a
//                multimon system report different values in this field.
//                Therefore, to test for a multimon system, an application
//                should look for more than one different monitor handle in
//                the list of D3DX devices.
// onPrimary:     Indicates if this device is on the primary monitor
//                (multimon specific).
//-------------------------------------------------------------------------
const D3DX_DRIVERDESC_LENGTH = 256;
type
  D3DX_DEVICEDESC = record
    deviceIndex: DWord;
    hwLevel: DWord;
    ddGuid: TGUID;
    d3dDeviceGuid: TGUID;
    ddDeviceID: TGUID;
    driverDesc: Array[0..D3DX_DRIVERDESC_LENGTH-1] of Char;
    monitor: HMONITOR;
    onPrimary: Boolean;
  end;

///////////////////////////////////////////////////////////////////////////
// APIs:
///////////////////////////////////////////////////////////////////////////

//-------------------------------------------------------------------------
// D3DXInitialize: The very first call a D3DX app must make.
//-------------------------------------------------------------------------
function D3DXInitialize: HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXUninitialize: The very last call a D3DX app must make.
//-------------------------------------------------------------------------
function D3DXUninitialize: HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXGetDeviceCount: Returns the maximum number of D3DXdevices
// ------------------  available.
//
// D3DXGetDeviceDescription: Lists the 2D and 3D capabilities of the devices.
// ------------------------  Also, the various guids needed by ddraw and d3d.
//
// Params:
//     [in] DWORD deviceIndex: Which device? Starts at 0.
//     [in] D3DX_DEVICEDESC* pd3dxDevice: Pointer to the D3DX_DEVICEDESC
//                                        structure to be filled in.
//-------------------------------------------------------------------------
function D3DXGetDeviceCount: DWord; stdcall;

function D3DXGetDeviceDescription(deviceIndex: DWord;
                             var pd3dxDeviceDesc: D3DX_DEVICEDESC): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXGetMaxNumVideoModes: Returns the maximum number of video-modes .
// -----------------------
//
// Params:
//     [in]  DWORD deviceIndex: The device being referred to.
//     [in]  DWORD flags: If D3DX_GVM_REFRESHRATE is set, then the refresh
//                        rates are not ignored.
//
// D3DXGetVideoMode:  Describes a particular video mode for this device
// ----------------
//
// Note:  These queries will simply give you a list of modes that the
//        display adapter tells DirectX that it supports.
//        There is no guarantee that D3DXCreateContext(Ex) will succeed
//        with all listed video modes.  This is a fundamental limitation
//        of the current DirectX architecture which D3DX cannot hide in
//        any clean way.
//
// Params:
//     [in]  DWORD deviceIndex: The device being referred to.
//     [in]  DWORD flags: If D3DX_GVM_REFRESHRATE is set, then the refresh
//                        rates are returned
//     [in]  DWORD which: Which VideoMode ? Starts at 0.
//     [out] D3DX_VIDMODEDESC* pModeList: Pointer to the D3DX_VIDMODEDESC
//                        structure that will be filled in.
//-------------------------------------------------------------------------
function D3DXGetMaxNumVideoModes(deviceIndex, flags: DWord): DWord; stdcall;

function D3DXGetVideoMode(deviceIndex, flags, modeIndex: DWord;
             var pModeDesc: D3DX_VIDMODEDESC): HResult; stdcall;

const D3DX_GVM_REFRESHRATE    =  $00000001;
//-------------------------------------------------------------------------
// D3DXGetMaxSurfaceFormats: Returns the maximum number of surface
// ------------------------  formats supported by the device at that
//                           video mode.
//
// D3DXGetSurfaceFormat: Describes one of the supported surface formats.
// ---------------------
//
// Params:
//     [in]  DWORD  deviceIndex: The device being referred to.
//     [in]  D3DX_VIDMODEDESC* pDesc: The display mode at which the supported
//                                    surface formats are requested. If it is
//                                    NULL, the current display mode is
//                                    assumed.
//     [in]  DWORD surfClassFlags: Required surface classes.  Only surface
//                                 formats which support all specified
//                                 surface classes will be returned.
//                                 (Multiple surface classes may be specified
//                                 using bitwise OR.)
//     [in]  DWORD which: Which surface formats to retrieve. Starts at 0.
//     [out] D3DX_SURFACEFORMAT* pFormat: The surface format
//-------------------------------------------------------------------------
function D3DXGetMaxSurfaceFormats
   (deviceIndex: DWord;
    var pDesc: D3DX_VIDMODEDESC;
    surfClassFlags: DWord): DWord; stdcall;

function D3DXGetSurfaceFormat(
    deviceIndex: DWord;
    pDesc: PD3DX_VIDMODEDESC;
    surfClassFlags: DWord;
    surfaceIndex: DWord;
    var pFormat: TD3DX_SURFACEFORMAT): HResult; stdcall;


//-------------------------------------------------------------------------
// D3DXGetCurrentVideoMode: Retrieves the current video mode for this device.
// -------------------
//
// Params:
//     [in]  DWORD deviceIndex: The device being referred to.
//     [out] D3DX_VIDMODEDESC* pVidMode: The current video mode
//-------------------------------------------------------------------------
function D3DXGetCurrentVideoMode(deviceIndex: DWord;
                            var pVidMode: D3DX_VIDMODEDESC): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXGetDeviceCaps: Lists all the capabilities of a device at a display
//                    mode.
// ----------------
//
// Params:
//     [in]  DWORD  deviceIndex: The device being referred to.
//     [in]  D3DX_VIDMODEDESC* pDesc:  If this is NULL, we will return the
//                                     caps at the current display mode of
//                                     the device.
//     [out] D3DDEVICEDESC7* pD3DDeviceDesc7: D3D Caps ( NULL to ignore
//                                              parameter)
//     [out] DDCAPS7* pDDHalCaps: DDraw HAL Caps (NULL to ignore parameter)
//     [out] DDCAPS7* pDDHelCaps: DDraw HEL Caps (NULL to ignore  paramter)
//-------------------------------------------------------------------------
function D3DXGetDeviceCaps(
   deviceIndex: DWord;
   pVidMode: PD3DX_VIDMODEDESC;
   pD3dCaps: PD3DDeviceDesc7;
   pDDHALCaps: PDDCaps;
   pDDHELCaps: PDDCaps): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXCreateContext: Initializes the chosen device. It is the simplest init
// -----------------  function available.  Parameters are treated the same
//                    as the matching subset of parameters in
//                    D3DXCreateContextEx, documented below.
//                    Remaining D3DXCreateContextEx parameters that are
//                    not present in D3DXCreateContext are treated as
//                    D3DX_DEFAULT.  Note that multimon is not supported
//                    with D3DXCreateContext.
//
// D3DXCreateContextEx: A more advanced function to initialize the device.
// -------------------  Also accepts D3DX_DEFAULT for most of the parameters
//                      and then will do what D3DXCreateContext did.
//
// Note: Do not expect D3DXCreateContext(Ex) to be fail-safe (as with any
//       API).  Supported device capablilites should be used as a guide
//       for choosing parameter values.  Keep in mind that there will
//       inevitably be some combinations of parameters that just do not work.
//
// Params:
//     [in]  DWORD deviceIndex: The device being referred to.
//     [in]  DWORD flags: The valid flags are D3DX_CONTEXT_FULLSCREEN, and
//                        D3DX_CONTEXT_OFFSCREEN.  These flags cannot both
//                        be specified.  If no flags are specified, the
//                        context defaults to windowed mode.
//
//     [in]  HWND  hwnd: Device window.  See note.
//     [in]  HWND  hwndFocus: Window which receives keyboard messages from
//                            the device window.  The device window should be
//                            a child of focus window.  Useful for multimon
//                            applications.  See note.
//     NOTE:
//         windowed:
//             hwnd must be a valid window.  hwndFocus must be NULL or
//             D3DX_DEFAULT.
//
//         fullscreen:
//             Either hwnd or hwndFocus must be a valid window.  (Both cannot
//             be NULL or D3DX_DEFAULT).  If hwnd is NULL or D3DX_DEFAULT,
//             a default device window will be created as a child of hwndFocus.
//
//         offscreen:
//             Both hwnd and hwndFocus must be NULL or D3DX_DEFAULT
//
//     [in]  DWORD numColorBits: If D3DX_DEFAULT is passed for windowed mode,
//                               the current desktop's color depth is chosen.
//                               For full screen mode, D3DX_DEFAULT causes 16
//                               bit color to be used.
//     [in]  DWORD numAlphaBits: If D3DX_DEFAULT is passed, 0 is chosen.
//     [in]  DWORD numDepthbits: If D3DX_DEFAULT is passed,
//                               the highest available number of depth bits
//                               is chosen.  See note.
//     [in]  DWORD numStencilBits: If D3DX_DEFAULT is passed, the highest
//                                 available number of stencil bits is chosen.
//                                 See note.
//
//     NOTE: If both numDepthBits and numStencilBits are D3DX_DEFAULT,
//           D3DX first picks the highest available number of stencil
//           bits.  Then, for the chosen number of stencil bits,
//           the highest available number of depth bits is chosen.
//           If only one of numStencilBits or numDepthBits
//           is D3DX_DEFAULT, the highest number of bits available
//           for this parameter is chosen out of only the formats
//           that support the number of bits requested for the
//           fixed parameter.
//
//     [in]  DWORD numBackBuffers: Number of back buffers, or D3DX_DEFAULT.
//                                 See note.
//
//     NOTE:
//        windowed: D3DX_DEFAULT means 1.  You must specify one back buffer.
//
//        fullscreen: D3DX_DEFAULT means 1.  Any number of back buffers can be
//            specified.
//
//        offscreen: D3DX_DEFAULT means 0.  You cannot specify additional back
//            buffers.
//
//     [in]  DWORD width: Width, in pixels, or D3DX_DEFAULT.  See note.
//     [in]  DWORD height: Height, in pixels, or D3DX_DEFAULT.  See note.
//
//     NOTE:
//        windowed: If either width or height is D3DX_DEFAULT, both values
//            default to the dimensions of the client area of hwnd.
//
//        fullscreen: If either width or height is D3DX_DEFAULT, width
//            defaults to 640, and height defaults to 480.
//
//        offscreen: An error is returned if either width or height is
//            D3DX_DEFAULT.
//
//     [in]  DWORD refreshRate: D3DX_DEFAULT means we let ddraw choose for
//                              us.  Ignored for windowed and offscreen modes.
//     [out] LPD3DXCONTEXT* ppCtx: This is the Context object that is used for
//                                 rendering on that device.
//
//-------------------------------------------------------------------------
function D3DXCreateContext(
   deviceIndex: DWord;
   flags: DWord;
   wnd: HWnd;
   width, height: DWord;
   out ppctx: PID3DXContext): HResult; stdcall;

function D3DXCreateContextEx(
   deviceIndex: DWord;
   flags: DWord;
   wnd: HWnd;
   WndFocus: HWnd;
   numColorBits, numAlphaBits, numDepthBits, numStencilBits: DWord;
   numBackBuffers: DWord;
   width, height: DWord;
   refreshrate: DWord;
   out ppctx: PID3DXContext): HResult; stdcall;

// The D3DXCreateContext(Ex) flags are:
const
  D3DX_CONTEXT_FULLSCREEN  = $00000001;
  D3DX_CONTEXT_OFFSCREEN   = $00000002;

//-------------------------------------------------------------------------
// D3DXGetErrorString: Prints out the error string given an hresult. Prints
// ------------------  Win32 as well as DX6 error messages besides the D3DX
//                     messages.
//
// Params:
//     [in]  HRESULT hr: The error code to be deciphered.
//     [in]  DWORD strLength: Length of the string passed in.
//     [out] LPSTR pStr:  The string output. This string of appropriate
//                       size needs to be passed in.
//-------------------------------------------------------------------------
procedure D3DXGetErrorString(hr: HResult; strLength: DWord;
                             pStr: PAnsiChar); stdcall;

//-------------------------------------------------------------------------
// D3DXMakeDDPixelFormat: Fills in a DDPIXELFORMAT structure based on the
// ---------------------   D3DX surface format requested.
//
// Params:
//     [in]  D3DX_SURFACEFORMAT d3dxFormat: Surface format.
//     [out] DDPIXELFORMAT*     pddpf:      Pixel format matching the given
//                                          surface format.
//-------------------------------------------------------------------------
function D3DXMakeDDPixelFormat(const d3dxFormat: TD3DX_SurfaceFormat;
                          var ddpf: TDDPixelFormat): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXMakeSurfaceFormat: Determines the surface format corresponding to
// ---------------------  a given DDPIXELFORMAT.
//
// Params:
//     [in]  DDPIXELFORMAT* pddpf: Pixel format.
// Return Value:
//     D3DX_SURFACEFORMAT: Surface format matching the given pixel format.
//                         D3DX_SF_UNKNOWN if the format is not supported
//-------------------------------------------------------------------------

function D3DXMakeSurfaceFormat(var pddpf: TDDPixelFormat): TD3DX_SurfaceFormat; stdcall;



///////////////////////////////////////////////////////////////////////////
// Texturing APIs:
///////////////////////////////////////////////////////////////////////////

//-------------------------------------------------------------------------
// D3DXCheckTextureRequirements: Return information about texture creation
// ----------------------------  (used by CreateTexture, CreateTextureFromFile
//                                and CreateCubeMapTexture)
//
// Parameters:
//
//  pd3dDevice
//      The D3D device with which the texture is going to be used.
//  pFlags
//      allows specification of D3DX_TEXTURE_NOMIPMAP
//      D3DX_TEXTURE_NOMIPMAP may be returned in the case where mipmap creation
//      is not supported.
//  pWidth
//      width in pixels or NULL
//      returns corrected width
//  pHeight
//      height in pixels or NULL
//      returns corrected height
//  pPixelFormat
//      surface format
//      returns best match to input format
//
//  Notes: 1. Unless the flags is set to specifically prevent creating
//            mipmaps, mipmaps are generated all the way till 1x1 surface.
//         2. width, height and pixelformat are altered based on available
//            hardware. For example:
//              a. Texture dimensions may be required to be powers of 2
//              b. We may require width == height for some devices
//              c. If PixelFormat is unavailable, a best fit is made
//-------------------------------------------------------------------------
function D3DXCheckTextureRequirements(
   const pd3dDevice: IDirect3DDevice7;
   pFlags, pWidth, pHeight: PDWord;
   var PixelFormat: TD3DX_SurfaceFormat): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXCreateTexture: Create an empty texture object
// -----------------
//
// Parameters:
//
//  pd3dDevice
//      The D3D device with which the texture is going to be used.
//  pFlags
//      allows specification of D3DX_TEXTURE_NOMIPMAP
//      D3DX_TEXTURE_NOMIPMAP may be returned in the case where mipmap creation
//      is not supported. Additionally, D3DX_TEXTURE_STAGE<n> can be specified
//      to indicate which texture stage the texture is for e.g.
//      D3D_TEXTURE_STAGE1 indicates that the texture is for use with texture
//      stage one. Stage Zero is the default if no TEXTURE_STAGE flags are
//      set.
//  pWidth
//      width in pixels; 0 or NULL is unacceptable
//      returns corrected width
//  pHeight
//      height in pixels; 0 or NULL is unacceptable
//      returns corrected height
//  pPixelFormat
//      surface format. D3DX_DEFAULT is unacceptable.
//      returns actual format that was used
//  pDDPal
//      DDraw palette that is set (if present) on paletted surfaces.
//      It is ignored even if it is set, for non-paletted surfaces.
//  ppDDSurf
//      The ddraw surface that will be created
//  pNumMipMaps
//      the number of mipmaps actually generated
//
//  Notes: See notes for D3DXCheckTextureRequirements.
//-------------------------------------------------------------------------
function D3DXCreateTexture(
  const pd3dDevice: IDirect3DDevice7;
  pFlags, pWidth, pHeight: PDWord;
  var PixelFormat: TD3DX_SurfaceFormat;
  DDPal: IDirectDrawPalette;
  out DDSurf: IDirectDrawSurface7;
  NumMipMaps: PDWord): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXCreateCubeMapTexture: Create blank cube-map texture
// ------------------------
//
// Parameters:
//
//  pd3dDevice
//      The D3D device with which the texture is going to be used.
//  pFlags
//      allows specification of D3DX_TEXTURE_NOMIPMAP
//      D3DX_TEXTURE_NOMIPMAP may be returned in the case where mipmap creation
//      is not supported. Additionally, D3DX_TEXTURE_STAGE<n> can be specified
//      to indicate which texture stage the texture is for e.g.
//      D3D_TEXTURE_STAGE1 indicates that the texture is for use with texture
//      stage one. Stage Zero is the default if no TEXTURE_STAGE flags are
//      set.
//  cubefaces
//      allows specification of which faces of the cube-map to generate.
//      D3DX_DEFAULT, 0, and DDSCAPS2_CUBEMAP_ALLFACES all mean
//      "create all 6 faces of the cubemap". Any combination of
//      DDSCAPS2_CUBEMAP_POSITIVEX, DDSCAPS2_CUBEMAP_NEGATIVEX,
//      DDSCAPS2_CUBEMAP_POSITIVEY, DDSCAPS2_CUBEMAP_NEGATIVEY,
//      DDSCAPS2_CUBEMAP_POSITIVEZ, or DDSCAPS2_CUBEMAP_NEGATIVEZ, is
//      valid.
//  colorEmptyFaces
//      allows specification of the color to use for the faces that were not
//      specified in the cubefaces parameter.
//  pWidth
//      width in pixels; 0 or NULL is unacceptable
//      returns corrected width
//  pHeight
//      height in pixels; 0 or NULL is unacceptable
//      returns corrected height
//  pPixelFormat
//      surface format. D3DX_DEFAULT is unacceptable.
//      returns actual format that was used
//  pDDPal
//      DDraw palette that is set (if present) on paletted surfaces.
//      It is ignored even if it is set, for non-paletted surfaces.
//  ppDDSurf
//      the ddraw surface that will be created
//  pNumMipMaps
//      the number of mipmaps generated for a particular face of the
//      cubemap.
//
//  Notes: See notes for D3DXCheckTextureRequirements.
//-------------------------------------------------------------------------
function D3DXCreateCubeMapTexture(
  const d3dDevice: IDirect3DDevice7;
  Flags: PDWord;
  const cubefaces: DWord;
  const colorEmptyFaces: TD3DColor;
  pWidth, pHeight: PDWord;
  var PixelFormat: TD3DX_SurfaceFormat;
  DDPal: IDirectDrawPalette;
  out DDSurf: IDirectDrawSurface7;
  NumMipMaps: PDWord): HResult; stdcall;


//-------------------------------------------------------------------------
// D3DXCreateTextureFromFile: Create a texture object from a file or from the
// -------------------------  resource. Only BMP and DIB are supported from the
//                            resource portion of the executable.
//
// Parameters:
//
//  pd3dDevice
//      The D3D device with which the texture is going to be used.
//  pFlags
//      allows specification of D3DX_TEXTURE_NOMIPMAP
//      D3DX_TEXTURE_NOMIPMAP may be returned in the case where mipmap creation
//      is not supported. Additionally, D3DX_TEXTURE_STAGE<n> can be specified
//      to indicate which texture stage the texture is for e.g.
//      D3D_TEXTURE_STAGE1 indicates that the texture is for use with texture
//      stage one. Stage Zero is the default if no TEXTURE_STAGE flags are
//      set.
//  pWidth
//      Width in pixels. If 0 or D3DX_DEFAULT, the width will be taken
//      from the file
//      returns corrected width
//  pHeight
//      Height in pixels. If 0 or D3DX_DEFAULT, the height will be taken
//      from the file
//      returns corrected height
//  pPixelFormat
//      If D3DX_SF_UNKNOWN is passed in, pixel format closest to the bitmap
//      will be chosen
//      returns actual format that was used
//  pDDPal
//      DDraw palette that is set (if present) on paletted surfaces.
//      It is ignored even if it is set, for non-paletted surfaces.
//  ppDDSurf
//      The ddraw surface that will be created.
//  pNumMipMaps
//      The number of mipmaps generated.
//  pSrcName
//      File name. BMP, DIB, DDS, are supported.
//
//      TGA is supported for the following cases: 16, 24, 32bpp direct color and 8bpp palettized.
//      Also, 8, 16bpp grayscale is supported. RLE versions of the above
//      TGA formats are also supported. ColorKey and Premultiplied Alpha
//      are not currently supported for TGA files.
//      returns created format
//
//  Notes: See notes for D3DXCheckTextureRequirements.
//-------------------------------------------------------------------------
function D3DXCreateTextureFromFile(
 const d3dDevice: IDirect3DDevice7;
 Flags, Width, Height: PDWord;
 var PixelFormat: TD3DX_SurfaceFormat;
  DDPal: IDirectDrawPalette;
  out DDSurf: IDirectDrawSurface7;
  NumMipMaps: PDWord;
  SrcName: PAnsiChar;
  filterType: TD3DX_FilterType): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXLoadTextureFromFile: Load from a file into a mipmap level. Doing the
// -----------------------  necessary color conversion and rescaling. File
//                          format support is identical to
//                          D3DXCreateTextureFromFile's.
//
//  pd3dDevice
//      The D3D device with which the texture is going to be used.
// pTexture
//      a pointer to a DD7Surface which was created with either
//      CreateTextureFromFile or CreateTexture.
// mipMapLevel
//      indicates mipmap level
//      Note:
//          1. Error if mipmap level doesn't exist
//          2. If D3DX_DEFAULT and equal number of mipmap levels exist
//             then all the source mip-levels are loaded
//          3. If the source has mipmaps and the dest doesn't, use the top one
//          4. If the dest has miplevels and source doesn't, we expand
//          5. If there are unequal numbers of miplevels, we expand
// pSrcName
//      File name. BMP, DIB, DDS, are supported.
//      For details on TGA support, refer to the comments for
//      D3DXCreateTextureFromFile
// pSrcRect
//      the source rectangle or null (whole surface)
// pDestRect
//      the destination rectangle or null (whole surface)
// filterType
//      filter used for mipmap generation
//-------------------------------------------------------------------------
function D3DXLoadTextureFromFile(
  const D3DDevice: IDirect3DDevice7;
  Texture: IDirectDrawSurface7;
  mipMapLevel: DWord;
  SrcName: PAnsiChar;
  pSrcRect, pDestRect: PRect;
  filterType: TD3DX_FilterType): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXLoadTextureFromSurface: Load from a DDraw Surface into a mipmap level.
// --------------------------  Doing the necessary color conversion.
//
//  pd3dDevice
//      The D3D device with which the texture is going to be used.
// pTexture
//      a pointer to a DD7Surface which was created with either
//      CreateTextureFromFile or CreateTexture.
// mipMapLevel
//      indicates mipmap level
//      Note:
//          1. Error if mipmap level doesn't exist
//          2. If D3DX_DEFAULT and equal number of mipmap levels exist
//             then all the source mip-levels are loaded
//          3. If the source has mipmaps and the dest doesn't, use the top one
//          4. If the dest has miplevels and source doesn't, we expand
//          5. If there are unequal numbers of miplevels, we expand
// pSurfaceSrc
//      the source surface
// pSrcRect
//      the source rectangle or null (whole surface)
// pDestRect
//      the destination rectangle or null (whole surface)
// filterType
//      filter used for mipmap generation
//-------------------------------------------------------------------------
function D3DXLoadTextureFromSurface(
  const D3DDevice: IDirect3DDevice7;
  Texture: IDirectDrawSurface7;
  mipMapLevel: DWord;
  SurfaceSrc: IDirectDrawSurface7;
  pSrcRect, pDestRect: PRect;
  filterType: TD3DX_FilterType): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXLoadTextureFromMemory: Load a mip level from memory. Doing the necessary
// -------------------------  color conversion.
//
//  pd3dDevice
//      The D3D device with which the texture is going to be used.
// pTexture
//      a pointer to a DD7Surface which was created with either
//      CreateTextureFromFile or CreateTexture.
// mipMapLevel
//      indicates mipmap level
//      Note:
//          1. Error if mipmap level doesn't exist
//          2. If D3DX_DEFAULT and equal number of mipmap levels exist
//             then all the source mip-levels are loaded
//          3. If the source has mipmaps and the dest doesn't, use the top one
//          4. If the dest has miplevels and source doesn't, we expand
//          5. If there are unequal numbers of miplevels, we expand
// pMemory
//      pointer to source memory from which the texture will be loaded
// pDDPal
//      DirectDraw Palette, that the app passes in optionally if the memory is
//      supposed to be paletteized.
// srcPixelFormat
//      PixelFormat of the source.
// srcPitch
//      The pitch of the memory or D3DX_DEFAULT (based on srcPixelFormat)
// pDestRect
//      The destination rectangle or null (whole surface)
// filterType
//      filter used for mipmap generation
//
// Assumptions: The source (memory) is loaded in full
//-------------------------------------------------------------------------
function D3DXLoadTextureFromMemory(
  const D3DDevice: IDirect3DDevice7;
  Texture: IDirectDrawSurface7;
  mipMapLevel: DWord;
  pMemory: Pointer;
  DDPal: IDirectDrawPalette;
  srcPixelFormat: TD3DX_SurfaceFormat;
  srcPitch: DWord;
  pDestRect: PRect;
  filterType: TD3DX_FilterType): HResult; stdcall;


//-------------------------------------------------------------------------
// Flags for texture create functions; applies to
// D3DXCreateTexture, D3DXCreateCubeMapTexture and D3DXCreateTextureFromFile.
//

// Flag to indicate that mipmap generation is not desired.
const
  D3DX_TEXTURE_NOMIPMAP = 1 shl 8;

// Flags to indicate which texture stage the texture is
// intended for use with. Specifying the stage is necessary at
// texture creation time for HW devices that expose the
// D3DDEVCAPS_SEPARATETEXTUREMEMORIES bit in their D3DDEVICEDESC
// structure.
const
   D3DX_TEXTURE_STAGE0    = 0;
   D3DX_TEXTURE_STAGE1    = 1;
   D3DX_TEXTURE_STAGE2    = 2;
   D3DX_TEXTURE_STAGE3    = 3;
   D3DX_TEXTURE_STAGE4    = 4;
   D3DX_TEXTURE_STAGE5    = 5;
   D3DX_TEXTURE_STAGE6    = 6;
   D3DX_TEXTURE_STAGE7    = 7;

// Mask to extract the texture stage value out of the flags to
// the texture create functions.
const
   D3DX_TEXTURE_STAGE_MASK = $07;

// =========================================================
// *************** D3DXMath.h ******************************
// =========================================================


//===========================================================================
//
// General purpose utilities
//
//===========================================================================
const
  D3DX_PI: Float = 3.141592654;
  D3DX_1BYPI: Float = 0.318309886;

function D3DXToRadian(Degree: Float): Float;
function D3DXToDegree(Radian: Float): Float;


//===========================================================================
//
// Vectors
//
//===========================================================================

//--------------------------
// 2D Vector
//--------------------------
type
  TD3DXVector2 = record
    x,y: Float;
  end;
  PD3DXVector2 = ^TD3DXVector2;


{$IFDEF __cplusplus}
public:
    D3DXVECTOR2() {};
    D3DXVECTOR2( const float * );
    D3DXVECTOR2( float x, float y );

    // casting
    operator float* ();
    operator const float* () const;

    // assignment operators
    D3DXVECTOR2& operator += ( const D3DXVECTOR2& );
    D3DXVECTOR2& operator -= ( const D3DXVECTOR2& );
    D3DXVECTOR2& operator *= ( float );
    D3DXVECTOR2& operator /= ( float );

    // unary operators
    D3DXVECTOR2 operator + () const;
    D3DXVECTOR2 operator - () const;

    // binary operators
    D3DXVECTOR2 operator + ( const D3DXVECTOR2& ) const;
    D3DXVECTOR2 operator - ( const D3DXVECTOR2& ) const;
    D3DXVECTOR2 operator * ( float ) const;
    D3DXVECTOR2 operator / ( float ) const;

    friend D3DXVECTOR2 operator * ( float, const D3DXVECTOR2& );

    BOOL operator == ( const D3DXVECTOR2& ) const;
    BOOL operator != ( const D3DXVECTOR2& ) const;
{$ENDIF}
function D3DXVector2Zero: TD3DXVector2;  // (0,0)
function D3DXVector2(const _x, _y: Float): TD3DXVector2;
function D3DXVec2Add(const v1,v2: TD3DXVector2): TD3DXVector2;
function D3DXVec2Subtract(const v1,v2: TD3DXVector2): TD3DXVector2;
function D3DXVector2Equal(const v1, v2: TD3DXVector2): Boolean;

//--------------------------
// 3D Vector
//--------------------------
type
{$IFDEF USE_D3DTYPES_FOR_D3DX}
  TD3DXVector3 = TD3DVector;
{$ELSE}
  TD3DXVector3 = record
    x,y,z: Float;
  end;
{$ENDIF}

  TD3DXVector = TD3DVector;
  PD3DXVector3 = ^TD3DXVector3;

{$IFDEF __cplusplus}
public:
    D3DXVECTOR3() {};
    D3DXVECTOR3( const float * );
    D3DXVECTOR3( const D3DVECTOR& );
    D3DXVECTOR3( float x, float y, float z );

    // casting
    operator float* ();
    operator const float* () const;

    operator D3DVECTOR* ();
    operator const D3DVECTOR* () const;

    operator D3DVECTOR& ();
    operator const D3DVECTOR& () const;

    // assignment operators
    D3DXVECTOR3& operator += ( const D3DXVECTOR3& );
    D3DXVECTOR3& operator -= ( const D3DXVECTOR3& );
    D3DXVECTOR3& operator *= ( float );
    D3DXVECTOR3& operator /= ( float );

    // unary operators
    D3DXVECTOR3 operator + () const;
    D3DXVECTOR3 operator - () const;

    // binary operators
    D3DXVECTOR3 operator + ( const D3DXVECTOR3& ) const;
    D3DXVECTOR3 operator - ( const D3DXVECTOR3& ) const;
    D3DXVECTOR3 operator * ( float ) const;
    D3DXVECTOR3 operator / ( float ) const;

    friend D3DXVECTOR3 operator * ( float, const struct D3DXVECTOR3& );

    BOOL operator == ( const D3DXVECTOR3& ) const;
    BOOL operator != ( const D3DXVECTOR3& ) const;
{$ENDIF}
function D3DXVector3Zero: TD3DXVector3; // (0,0,0)
function D3DXVector3(const _x, _y, _z: Float): TD3DXVector3;
function D3DXVec3Add(const v1,v2: TD3DXVector3): TD3DXVector3;
function D3DXVec3Subtract(const v1,v2: TD3DXVector3): TD3DXVector3;
function D3DXVector3Equal(const v1, v2: TD3DXVector3): Boolean;


//--------------------------
// 4D Vector
//--------------------------
type
  TD3DXVector4 = record
    x,y,z,w: Float;
  end;
  PD3DXVector4 = ^TD3DXVector4;
{$IFDEF __cplusplus}
public:
    D3DXVECTOR4() {};
    D3DXVECTOR4( const float* );
    D3DXVECTOR4( float x, float y, float z, float w );

    // casting
    operator float* ();
    operator const float* () const;

    // assignment operators
    D3DXVECTOR4& operator += ( const D3DXVECTOR4& );
    D3DXVECTOR4& operator -= ( const D3DXVECTOR4& );
    D3DXVECTOR4& operator *= ( float );
    D3DXVECTOR4& operator /= ( float );

    // unary operators
    D3DXVECTOR4 operator + () const;
    D3DXVECTOR4 operator - () const;

    // binary operators
    D3DXVECTOR4 operator + ( const D3DXVECTOR4& ) const;
    D3DXVECTOR4 operator - ( const D3DXVECTOR4& ) const;
    D3DXVECTOR4 operator * ( float ) const;
    D3DXVECTOR4 operator / ( float ) const;

    friend D3DXVECTOR4 operator * ( float, const D3DXVECTOR4& );

    BOOL operator == ( const D3DXVECTOR4& ) const;
    BOOL operator != ( const D3DXVECTOR4& ) const;
{$ENDIF}
function D3DXVector4Zero: TD3DXVector4; // (0,0,0,0)
function D3DXVector4(const _x, _y, _z, _w: Float): TD3DXVector4;
function D3DXVector4Add(const v1,v2: TD3DXVector4): TD3DXVector4;
function D3DXVector4Subtract(const v1,v2: TD3DXVector4): TD3DXVector4;
function D3DXVector4Equal(const v1, v2: TD3DXVector4): Boolean;


//===========================================================================
//
// Matrices
//
//===========================================================================
type
{$IFDEF USE_D3DTYPES_FOR_D3DX}
  TD3DXMatrix = TD3DMatrix;
{$ELSE}
  TD3DXMatrix = record
   case Integer of
    0: (m00, m01, m02, m03,
        m10, m11, m12, m13,
        m20, m21, m22, m23,
        m30, m31, m32, m33: Float);
    1: (m: Array[0..3,0..3] of Float);
  end;
{$ENDIF}
  PD3DXMatrix = ^TD3DXMatrix;

{$IFDEF __cplusplus}
public:
    D3DXMATRIX() {};
    D3DXMATRIX( const float * );
    D3DXMATRIX( const D3DMATRIX& );
    D3DXMATRIX( float m00, float m01, float m02, float m03,
                float m10, float m11, float m12, float m13,
                float m20, float m21, float m22, float m23,
                float m30, float m31, float m32, float m33 );


    // access grants
    float& operator () ( UINT iRow, UINT iCol );
    float  operator () ( UINT iRow, UINT iCol ) const;

    // casting operators
    operator float* ();
    operator const float* () const;

    operator D3DMATRIX* ();
    operator const D3DMATRIX* () const;

    operator D3DMATRIX& ();
    operator const D3DMATRIX& () const;

    // assignment operators
    D3DXMATRIX& operator *= ( const D3DXMATRIX& );
    D3DXMATRIX& operator += ( const D3DXMATRIX& );
    D3DXMATRIX& operator -= ( const D3DXMATRIX& );
    D3DXMATRIX& operator *= ( float );
    D3DXMATRIX& operator /= ( float );

    // unary operators
    D3DXMATRIX operator + () const;
    D3DXMATRIX operator - () const;

    // binary operators
    D3DXMATRIX operator * ( const D3DXMATRIX& ) const;
    D3DXMATRIX operator + ( const D3DXMATRIX& ) const;
    D3DXMATRIX operator - ( const D3DXMATRIX& ) const;
    D3DXMATRIX operator * ( float ) const;
    D3DXMATRIX operator / ( float ) const;

    friend D3DXMATRIX operator * ( float, const D3DXMATRIX& );

    BOOL operator == ( const D3DXMATRIX& ) const;
    BOOL operator != ( const D3DXMATRIX& ) const;
{$ENDIF}
function D3DXMatrix(const _m00, _m01, _m02, _m03,
                          _m10, _m11, _m12, _m13,
                          _m20, _m21, _m22, _m23,
                          _m30, _m31, _m32, _m33: Float): TD3DXMatrix;

function D3DXMatrixAdd(var mOut: TD3DXMatrix; const m1,m2: TD3DXMatrix): PD3DXMatrix;
function D3DXMatrixSubtract(var mOut: TD3DXMatrix; const m1,m2: TD3DXMatrix): PD3DXMatrix;
function D3DXMatrixMul(var mOut: TD3DXMatrix; const m: TD3DXMatrix; const MulBy: Float): PD3DXMatrix;
function D3DXMatrixEqual(const m1, m2: TD3DXMatrix): Boolean;


//===========================================================================
//
//    Quaternions
//
//===========================================================================
type
  TD3DXQuaternion = record
    x, y, z, w: Float;
  end;
  PD3DXQuaternion = ^TD3DXQuaternion;
{$IFDEF __cplusplus}
public:
    D3DXQUATERNION() {}
    D3DXQUATERNION( const float * );
    D3DXQUATERNION( float x, float y, float z, float w );

    // casting
    operator float* ();
    operator const float* () const;

    // assignment operators
    D3DXQUATERNION& operator += ( const D3DXQUATERNION& );
    D3DXQUATERNION& operator -= ( const D3DXQUATERNION& );
    D3DXQUATERNION& operator *= ( const D3DXQUATERNION& );
    D3DXQUATERNION& operator *= ( float );
    D3DXQUATERNION& operator /= ( float );

    // unary operators
    D3DXQUATERNION  operator + () const;
    D3DXQUATERNION  operator - () const;

    // binary operators
    D3DXQUATERNION operator + ( const D3DXQUATERNION& ) const;
    D3DXQUATERNION operator - ( const D3DXQUATERNION& ) const;
    D3DXQUATERNION operator * ( const D3DXQUATERNION& ) const;
    D3DXQUATERNION operator * ( float ) const;
    D3DXQUATERNION operator / ( float ) const;

    friend D3DXQUATERNION operator * (float, const D3DXQUATERNION& );

    BOOL operator == ( const D3DXQUATERNION& ) const;
    BOOL operator != ( const D3DXQUATERNION& ) const;
{$ENDIF}
function D3DXQuaternion(const _x, _y, _z, _w: Float): TD3DXQuaternion;
function D3DXQuaternionAdd(const q1,q2: TD3DXQuaternion): TD3DXQuaternion;
function D3DXQuaternionSubtract(const q1,q2: TD3DXQuaternion): TD3DXQuaternion;
function D3DXQuaternionEqual(const q1, q2: TD3DXQuaternion): Boolean;
function D3DXQuaternionScale(var qOut: TD3DXQuaternion; const q: TD3DXQuaternion; const s: Float): PD3DXQuaternion;


//===========================================================================
//
// Planes
//
//===========================================================================
type
  TD3DXPlane = record
    a,b,c,d: Float;
  end;
  PD3DXPlane = ^TD3DXPlane;
{$IFDEF __cplusplus}
public:
    D3DXPLANE() {}
    D3DXPLANE( const float* );
    D3DXPLANE( float a, float b, float c, float d );

    // casting
    operator float* ();
    operator const float* () const;

    // unary operators
    D3DXPLANE operator + () const;
    D3DXPLANE operator - () const;

    // binary operators
    BOOL operator == ( const D3DXPLANE& ) const;
    BOOL operator != ( const D3DXPLANE& ) const;
{$ENDIF}

function D3DXPlaneZero: TD3DXPlane; // (0,0,0,0)
function D3DXPlane(const _a, _b, _c, _d: Float): TD3DXPlane;

function D3DXPlaneEqual(const p1, p2: TD3DXPlane): Boolean;


//===========================================================================
//
// Colors
//
//===========================================================================
type
  TD3DXColor = record
    r, g, b, a: Float;
  end;
  PD3DXColor = ^TD3DXColor;

{$IFDEF __cplusplus}
public:
    D3DXCOLOR() {}
    D3DXCOLOR( DWORD argb );
    D3DXCOLOR( const float * );
    D3DXCOLOR( const D3DCOLORVALUE& );
    D3DXCOLOR( float r, float g, float b, float a );

    // casting
    operator DWORD () const;

    operator float* ();
    operator const float* () const;

    operator D3DCOLORVALUE* ();
    operator const D3DCOLORVALUE* () const;

    operator D3DCOLORVALUE& ();
    operator const D3DCOLORVALUE& () const;

    // assignment operators
    D3DXCOLOR& operator += ( const D3DXCOLOR& );
    D3DXCOLOR& operator -= ( const D3DXCOLOR& );
    D3DXCOLOR& operator *= ( float );
    D3DXCOLOR& operator /= ( float );

    // unary operators
    D3DXCOLOR operator + () const;
    D3DXCOLOR operator - () const;

    // binary operators
    D3DXCOLOR operator + ( const D3DXCOLOR& ) const;
    D3DXCOLOR operator - ( const D3DXCOLOR& ) const;
    D3DXCOLOR operator * ( float ) const;
    D3DXCOLOR operator / ( float ) const;

    friend D3DXCOLOR operator * (float, const D3DXCOLOR& );

    BOOL operator == ( const D3DXCOLOR& ) const;
    BOOL operator != ( const D3DXCOLOR& ) const;

{$ENDIF}

function D3DXColor(const _r, _g, _b, _a: Float): TD3DXColor;
function D3DXColorToDWord(c: TD3DXColor): DWord;
function D3DXColorFromDWord(c: DWord): TD3DXColor;
function D3DXColorEqual(const c1, c2: TD3DXColor): Boolean;

//===========================================================================
//
// D3DX math functions:
//
// NOTE:
//  * All these functions can take the same object as in and out parameters.
//
//  * Out parameters are typically also returned as return values, so that
//    the output of one function may be used as a parameter to another.
//
// NOTE from as: A bit odd for Delphi but some of these functions are implemented
// in D3DX.DLL so, for consistency, I had to stick to this scheme. Generally:
// Function result = @vOut
//
//===========================================================================

//--------------------------
// 2D Vector
//--------------------------

// "inline"
function D3DXVec2Length(const v: TD3DXVector2): Float;
function D3DXVec2LengthSq(const v: TD3DXVector2): Float;
function D3DXVec2Dot(const v1, v2: TD3DXVector2): Float;

// Z component of ((x1,y1,0) cross (x2,y2,0))
function D3DXVec2CCW (const v1, v2: TD3DXVector2): Float;

// Minimize each component.  x = min(x1, x2), y = min(y1, y2)
function D3DXVec2Minimize(var vOut: TD3DXVector2; const v1, v2: TD3DXVEctor2): PD3DXVector2;

// Maximize each component.  x = max(x1, x2), y = max(y1, y2)
function D3DXVec2Maximize(var vOut: TD3DXVector2; const v1, v2: TD3DXVector2): PD3DXVector2;

function D3DXVec2Scale(var vOut: TD3DXVector2; const v: TD3DXVector2; const s: Float): PD3DXVector2;

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec2Lerp(var vOut: TD3DXVector2;
  const v1, v2: TD3DXVector2; const s: Float): PD3DXVector2;

// non-inline
function D3DXVec2Normalize(var vOut: TD3DXVector2;
   const v: TD3DXVector2): PD3DXVector2; stdcall;

// Hermite interpolation between position V1, tangent T1 (when s == 0)
// and position V2, tangent T2 (when s == 1).
function D3DXVec2Hermite(var vOut: TD3DXVector2;
   const v1, t1, v2, t2: TD3DXVector2; const s: Float): PD3DXVector2; stdcall;

// Barycentric coordinates.  V1 + f(V2-V1) + g(V3-V1)
function D3DXVec2BaryCentric(vOut: TD3DXVector2;
   const v1, v2, v3: TD3DXVector2; const f, g: Float): PD3DXVector2; stdcall;

// Transform (x, y, 0, 1) by matrix.
function D3DXVec2Transform(var vOut: TD3DXVector4;
  const v: TD3DXVector2; const m: TD3DXMatrix): PD3DXVector4; stdcall;

// Transform (x, y, 0, 1) by matrix, project result back into w=1.
function D3DXVec2TransformCoord(var vOut: TD3DXVector2;
  const v: TD3DXVector2; const m: TD3DXMatrix): PD3DXVector2; stdcall;

// Transform (x, y, 0, 0) by matrix.
function D3DXVec2TransformNormal(var vOut: TD3DXVector2;
  const v: TD3DXVector2; const m: TD3DXMatrix): PD3DXVector2; stdcall;


//--------------------------
// 3D Vector
//--------------------------

// inline
function D3DXVec3Length(const v: TD3DXVector3): Float;
function D3DXVec3LengthSq(const v: TD3DXVector3): Float;
function D3DXVec3Dot(const v1, v2: TD3DXVector3): Float;

function D3DXVec3Cross(var vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;

// Minimize each component.  x = min(x1, x2), y = min(y1, y2)
function D3DXVec3Minimize(var vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;

// Maximize each component.  x = max(x1, x2), y = max(y1, y2)
function D3DXVec3Maximize(var vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;

function D3DXVec3Scale(var vOut: TD3DXVector3; const v: TD3DXVector3; const s: Float): PD3DXVector3;

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec3Lerp(var vOut: TD3DXVector3;
  const v1, v2: TD3DXVector3; const s: Float): PD3DXVector3;

// non-inline
function D3DXVec3Normalize(var vOut: TD3DXVector3;
   const v: TD3DXVector3): PD3DXVector3; stdcall;

// Hermite interpolation between position V1, tangent T1 (when s == 0)
// and position V2, tangent T2 (when s == 1).
function D3DXVec3Hermite(var vOut: TD3DXVector3;
   const v1, t1, v2, t2: TD3DXVector3; const s: Float): PD3DXVector3; stdcall;

// Barycentric coordinates.  V1 + f(V2-V1) + g(V3-V1)
function D3DXVec3BaryCentric(vOut: TD3DXVector3;
   const v1, v2, v3: TD3DXVector3; const f, g: Float): PD3DXVector3; stdcall;

// Transform (x, y, z, 1) by matrix.
function D3DXVec3Transform(var vOut: TD3DXVector4;
  const v: TD3DXVector3; const m: TD3DXMatrix): PD3DXVector4; stdcall;

// Transform (x, y, 0, 1) by matrix, project result back into w=1.
function D3DXVec3TransformCoord(var vOut: TD3DXVector3;
  const v: TD3DXVector3; const m: TD3DXMatrix): PD3DXVector3; stdcall;

// Transform (x, y, 0, 0) by matrix.
function D3DXVec3TransformNormal(var vOut: TD3DXVector3;
  const v: TD3DXVector3; const m: TD3DXMatrix): PD3DXVector3; stdcall;

//--------------------------
// 4D Vector
//--------------------------

// inline
function D3DXVec4Length(const v: TD3DXVector4): Float;
function D3DXVec4LengthSq(const v: TD3DXVector4): Float;
function D3DXVec4Dot(const v1, v2: TD3DXVector4): Float;

// Minimize each component.  x = min(x1, x2), y = min(y1, y2)
function D3DXVec4Minimize(var vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4;

// Maximize each component.  x = max(x1, x2), y = max(y1, y2)
function D3DXVec4Maximize(var vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4;

function D3DXVec4Scale(var vOut: TD3DXVector4; const v: TD3DXVector4; const s: Float): PD3DXVector4;

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec4Lerp(var vOut: TD3DXVector4;
  const v1, v2: TD3DXVector4; const s: Float): PD3DXVector4;


// non-inline

// Cross-product in 4 dimensions.
function D3DXVec4Cross(var vOut: TD3DXVector4;
  const v1, v2, v3: TD3DXVector4): PD3DXVector4; stdcall;

function D3DXVec4Normalize(var vOut: TD3DXVector4;
   const v: TD3DXVector4): PD3DXVector4; stdcall;

// Hermite interpolation between position V1, tangent T1 (when s == 0)
// and position V2, tangent T2 (when s == 1).
function D3DXVec4Hermite(var vOut: TD3DXVector4;
   const v1, t1, v2, t2: TD3DXVector4; const s: Float): PD3DXVector4; stdcall;

// Barycentric coordinates.  V1 + f(V2-V1) + g(V3-V1)
function D3DXVec4BaryCentric(vOut: TD3DXVector4;
   const v1, v2, v3: TD3DXVector4; const f, g: Float): PD3DXVector4; stdcall;

// Transform (x, y, z, 1) by matrix.
function D3DXVec4Transform(var vOut: TD3DXVector4;
  const v: TD3DXVector4; const m: TD3DXMatrix): PD3DXVector4; stdcall;


//--------------------------
// 4D Matrix
//--------------------------

// "inline"
function D3DXMatrixIdentity(var mOut: TD3DXMatrix): PD3DXMatrix;

function D3DXMatrixIsIdentity(const m: TD3DXMatrix): Boolean;

// non-inline
function D3DXMatrixfDeterminant(const m: TD3DXMatrix): Float; stdcall;

// Matrix multiplication.  The result represents the transformation M2
// followed by the transformation M1.  (Out = M1 * M2)
function D3DXMatrixMultiply(var mOut: TD3DXMatrix; const m1, m2: TD3DXMatrix): PD3DXMatrix; stdcall;

function D3DXMatrixTranspose(var mOut: TD3DXMatrix; const m: TD3DXMatrix): PD3DXMatrix; stdcall;

// Calculate inverse of matrix.  Inversion my fail, in which case NULL will
// be returned.  The determinant of pM is also returned it pfDeterminant
// is non-NULL.
function D3DXMatrixInverse(var mOut: TD3DXMatrix; pfDeterminant: PFloat;  // check!
    const m: TD3DXMatrix): PD3DXMatrix; stdcall;

// Build a matrix which scales by (sx, sy, sz)
function D3DXMatrixScaling(var mOut: TD3DXMatrix; const sx, sy, sz: Float): PD3DXMatrix; stdcall;

// Build a matrix which translates by (x, y, z)
function D3DXMatrixTranslation(var mOut: TD3DXMatrix; const x, y, z: Float): PD3DXMatrix; stdcall;

// Build a matrix which rotates around the X axis
function D3DXMatrixRotationX(var mOut: TD3DXMatrix; const angle: Float): PD3DXMatrix; stdcall;

// Build a matrix which rotates around the Y axis
function D3DXMatrixRotationY(var mOut: TD3DXMatrix; const angle: Float): PD3DXMatrix; stdcall;

// Build a matrix which rotates around the Z axis
function D3DXMatrixRotationZ(var mOut: TD3DXMatrix; const angle: Float): PD3DXMatrix; stdcall;

// Build a matrix which rotates around an arbitrary axis
function D3DXMatrixRotationAxis(var mOut: TD3DXMatrix; const v: TD3DXVector3;
    const angle: Float): PD3DXMatrix; stdcall;

// Build a matrix from a quaternion
function D3DXMatrixRotationQuaternion(var mOut: TD3DXMatrix; const Q: TD3DXQuaternion): PD3DXMatrix; stdcall;

// Yaw around the Y axis, a pitch around the X axis,
// and a roll around the Z axis.
function D3DXMatrixRotationYawPitchRoll(var mOut: TD3DXMatrix; const yaw, pitch, roll: Float): PD3DXMatrix; stdcall;

// Build transformation matrix.  NULL arguments are treated as identity.
// Mout = Msc-1 * Msr-1 * Ms * Msr * Msc * Mrc-1 * Mr * Mrc * Mt
function D3DXMatrixTransformation(var mOut: TD3DXMatrix;
   const pScalingCenter: PD3DXVector3;
   const pScalingRotation: PD3DXQuaternion; const pScaling, pRotationCenter: PD3DXVector3;
   const pRotation: PD3DXQuaternion; const pTranslation: PD3DXVector3): PD3DXMatrix; stdcall;

// Build affine transformation matrix.  NULL arguments are treated as identity.
// Mout = Ms * Mrc-1 * Mr * Mrc * Mt
function D3DXMatrixAffineTransformation(var mOut: TD3DXMatrix;
   Scaling: Float; const pRotationCenter: PD3DXVector3;
   const pRotation: PD3DXQuaternion; const pTranslation: PD3DXVector3): PD3DXMatrix; stdcall;

// Build a lookat matrix. (right-handed)
                                                   // *pEye.. *pUp?
function D3DXMatrixLookAt(var mOut: TD3DXMatrix; const Eye, At, Up: TD3DXVector3): PD3DXMatrix; stdcall;

// Build a lookat matrix. (left-handed)
function D3DXMatrixLookAtLH(var mOut: TD3DXMatrix; const Eye, At, Up: TD3DXVector3): PD3DXMatrix; stdcall;

// Build a perspective projection matrix. (right-handed)
function D3DXMatrixPerspective(var mOut: TD3DXMatrix; w,h,zn,zf: Float): PD3DXMatrix; stdcall;

// Build a perspective projection matrix. (left-handed)
function D3DXMatrixPerspectiveLH(var mOut: TD3DXMatrix; w,h,zn,zf: Float): PD3DXMatrix; stdcall;

// Build a perspective projection matrix. (right-handed)
function D3DXMatrixPerspectiveFov(var mOut: TD3DXMatrix; flovy, aspect, zn, zf: Float): PD3DXMatrix; stdcall;

// Build a perspective projection matrix. (left-handed)
function D3DXMatrixPerspectiveFovLH(var mOut: TD3DXMatrix; flovy, aspect, zn, zf: Float): PD3DXMatrix; stdcall;

// Build a perspective projection matrix. (right-handed)
function D3DXMatrixPerspectiveOffCenter(var mOut: TD3DXMatrix;
   l, r, b, t, zn, zf: Float): PD3DXMatrix; stdcall;

// Build a perspective projection matrix. (left-handed)
function D3DXMatrixPerspectiveOffCenterLH(var mOut: TD3DXMatrix;
   l, r, b, t, zn, zf: Float): PD3DXMatrix; stdcall;

// Build an ortho projection matrix. (right-handed)
function D3DXMatrixOrtho(var mOut: TD3DXMatrix; w, h, zn, zf: Float): PD3DXMatrix; stdcall;

// Build an ortho projection matrix. (left-handed)
function D3DXMatrixOrthoLH(var mOut: TD3DXMatrix; w, h, zn, zf: Float): PD3DXMatrix; stdcall;

// Build an ortho perspective projection matrix. (right-handed)
function D3DXMatrixOrthoOffCenter(var mOut: TD3DXMatrix;
  l, r, b, t, zn, zf: Float): PD3DXMatrix; stdcall;

// Build an ortho projection matrix. (left-handed)
function D3DXMatrixOrthoOffCenterLH(var mOut: TD3DXMatrix;
  l, r, b, t, zn, zf: Float): PD3DXMatrix; stdcall;

// Build a matrix which flattens geometry into a plane, as if casting
// a shadow from a light.
function D3DXMatrixShadow(var mOut: TD3DXMatrix;     // *pLight, *pPlane?
  const Light: TD3DXVector4; const Plane: TD3DXPlane): PD3DXMatrix; stdcall;

// Build a matrix which reflects the coordinate system about a plane
function D3DXMatrixReflect(var mOut: TD3DXMatrix;
   const Plane: TD3DXPlane): PD3DXMatrix; stdcall;  // *pPlane?


//--------------------------
// Quaternion
//--------------------------

// inline
function D3DXQuaternionLength(const q: TD3DXQuaternion): Float;

// Length squared, or "norm"
function D3DXQuaternionLengthSq(const q: TD3DXQuaternion): Float;
function D3DXQuaternionDot(const q1, q2: TD3DXQuaternion): Float;

// (0, 0, 0, 1)
function D3DXQuaternionIdentity(var qOut: TD3DXQuaternion): PD3DXQuaternion;

function D3DXQuaternionIsIdentity (const q: TD3DXQuaternion): Boolean;

// (-x, -y, -z, w)
function D3DXQuaternionConjugate(var qOut: TD3DXQuaternion;
  const q: TD3DXQuaternion): PD3DXQuaternion;

// Compute a quaternion's axis and angle of rotation. Expects unit quaternions.
procedure D3DXQuaternionToAxisAngle(const q: TD3DXQuaternion;
  var Axis: TD3DXVector3; var Angle: Float); stdcall;

// Build a quaternion from a rotation matrix.
function D3DXQuaternionRotationMatrix(var qOut: TD3DXQuaternion;
  const m: TD3DXMatrix): PD3DXQuaternion; stdcall;

// Rotation about arbitrary axis.
function D3DXQuaternionRotationAxis(var qOut: TD3DXQuaternion;
  const v: TD3DXVector3; Angle: Float): PD3DXQuaternion; stdcall;

// Yaw around the Y axis, a pitch around the X axis,
// and a roll around the Z axis.
function D3DXQuaternionRotationYawPitchRoll(var qOut: TD3DXQuaternion;
  yaw, pitch, roll: Float): PD3DXQuaternion; stdcall;

// Quaternion multiplication.  The result represents the rotation Q2
// followed by the rotation Q1.  (Out = Q2 * Q1)
function D3DXQuaternionMultiply(var qOut: TD3DXQuaternion;
   const q1, q2: TD3DXQuaternion): PD3DXQuaternion; stdcall;

function D3DXQuaternionNormalize(var qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion): PD3DXQuaternion; stdcall;

// Conjugate and re-norm
function D3DXQuaternionInverse(var qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion): PD3DXQuaternion; stdcall;

// Expects unit quaternions.
// if q = (cos(theta), sin(theta) * v); ln(q) = (0, theta * v)
function D3DXQuaternionLn(var qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion): PD3DXQuaternion; stdcall;

// Expects pure quaternions. (w == 0)  w is ignored in calculation.
// if q = (0, theta * v); exp(q) = (cos(theta), sin(theta) * v)
function D3DXQuaternionExp(var qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion): PD3DXQuaternion; stdcall;

// Spherical linear interpolation between Q1 (s == 0) and Q2 (s == 1).
// Expects unit quaternions.
function D3DXQuaternionSlerp(var qOut: TD3DXQuaternion;
   const q1, q2: TD3DXQuaternion; t: Float): PD3DXQuaternion; stdcall;

// Spherical quadrangle interpolation.
// Slerp(Slerp(Q1, Q4, t), Slerp(Q2, Q3, t), 2t(1-t))
function D3DXQuaternionSquad(var qOut: TD3DXQuaternion;
   const q1, q2, q3, q4: TD3DXQuaternion; t: Float): PD3DXQuaternion; stdcall;

// Slerp(Slerp(Q1, Q2, f+g), Slerp(Q1, Q3, f+g), g/(f+g))
function D3DXQuaternionBaryCentric(var qOut: TD3DXQuaternion;
   const q1, q2, q3: TD3DXQuaternion; f, g: Float): PD3DXQuaternion; stdcall;


//--------------------------
// Plane
//--------------------------

// inline

// ax + by + cz + dw
function D3DXPlaneDot(const p: TD3DXPlane; const v: TD3DXVector4): Float;

// ax + by + cz + d
function D3DXPlaneDotCoord(const p: TD3DXPlane; const v: TD3DXVector3): Float;

// ax + by + cz
function D3DXPlaneDotNormal(const p: TD3DXPlane; const v: TD3DXVector3): Float;

// non-inline

// Normalize plane (so that |a,b,c| == 1)
function D3DXPlaneNormalize(var pOut: TD3DXPlane;
   const p: TD3DXPlane): PD3DXPlane; stdcall;

// Find the intersection between a plane and a line.  If the line is
// parallel to the plane, NULL is returned.
function D3DXPlaneIntersectLine(var vOut: TD3DXVector3;
   const p: TD3DXPlane; const v1, v2: TD3DXVector3): PD3DXVector3; stdcall;

// Construct a plane from a point and a normal
function D3DXPlaneFromPointNormal(var pOut: TD3DXPlane;
   const vPoint, vNormal: TD3DXVector3): PD3DXPlane; stdcall;

// Construct a plane from 3 points
function D3DXPlaneFromPoints(var pOut: TD3DXPlane;
   const v1, v2, v3: TD3DXVector3): PD3DXPlane; stdcall;

// Transform a plane by a matrix.  The vector (a,b,c) must be normal.
// M must be an affine transform.
function D3DXPlaneTransform(var pOut: TD3DXPlane;
   const m: TD3DXMatrix): PD3DXPlane; stdcall;


//--------------------------
// Color
//--------------------------

// inline

// (1-r, 1-g, 1-b, a)
function D3DXColorNegative(var cOut: TD3DXColor; const c: TD3DXColor): PD3DXColor;

function D3DXColorAdd(var cOut: TD3DXColor; const c1,c2: TD3DXColor): PD3DXColor;
function D3DXColorSubtract(var cOut: TD3DXColor; const c1,c2: TD3DXColor): PD3DXColor;
function D3DXColorScale(var cOut: TD3DXColor; const c: TD3DXColor; s: Float): PD3DXColor;

// (r1*r2, g1*g2, b1*b2, a1*a2)
function D3DXColorModulate(var cOut: TD3DXColor; const c1,c2: TD3DXColor): PD3DXColor;

// Linear interpolation of r,g,b, and a. C1 + s(C2-C1)
function D3DXColorLerp(var cOut: TD3DXColor; const c1,c2: TD3DXColor; s: Float): PD3DXColor;

// non-inline

// Interpolate r,g,b between desaturated color and color.
// DesaturatedColor + s(Color - DesaturatedColor)
function D3DXColorAdjustSaturation(var cOut: TD3DXColor;
   const c: TD3DXColor; saturation: Float): PD3DXColor; stdcall;

// Interpolate r,g,b between 50% grey and color.  Grey + s(Color - Grey)
function D3DXColorAdjustContrast(var cOut: TD3DXColor;
   const c: TD3DXColor; contrast: Float): PD3DXColor; stdcall;


//===========================================================================
//
//    Matrix Stack
//
//===========================================================================
type
  ID3DXMatrixStack = interface(IUnknown)
    ['{E3357330-CC5E-11d2-A434-00A0C90629A8}']
    // ID3DXMatrixStack methods
    // Pops the top of the stack, returns the current top
    // *after* popping the top.
    function Pop: HResult; stdcall;

    // Pushes the stack by one, duplicating the current matrix.
    function Push: HResult; stdcall;

    // Loads identity in the current matrix.
    function LoadIdentity: HResult; stdcall;

    // Loads the given matrix into the current matrix
    function LoadMatrix(const M: TD3DXMATRIX): HResult; stdcall;

    // Right-Multiplies the given matrix to the current matrix.
    // (transformation is about the current world origin)
    function MultMatrix(const M: TD3DXMATRIX): HResult; stdcall;

    // Left-Multiplies the given matrix to the current matrix
    // (transformation is about the local origin of the object)
    function MultMatrixLocal(const M: TD3DXMATRIX): HResult; stdcall;

    // Right multiply the current matrix with the computed rotation
    // matrix, counterclockwise about the given axis with the given angle.
    // (rotation is about the current world origin)
    function RotateAxis(const V: TD3DXVECTOR3; Angle: Float): HResult; stdcall;

    // Left multiply the current matrix with the computed rotation
    // matrix, counterclockwise about the given axis with the given angle.
    // (rotation is about the local origin of the object)
    function RotateAxisLocal(const V: TD3DXVECTOR3; Angle: Float): HResult; stdcall;

    // Right multiply the current matrix with the computed rotation
    // matrix. All angles are counterclockwise. (rotation is about the
    // current world origin)

    // The rotation is composed of a yaw around the Y axis, a pitch around
    // the X axis, and a roll around the Z axis.
    function RotateYawPitchRoll(yaw, pitch, roll: Float): HResult; stdcall;

    // Left multiply the current matrix with the computed rotation
    // matrix. All angles are counterclockwise. (rotation is about the
    // local origin of the object)

    // The rotation is composed of a yaw around the Y axis, a pitch around
    // the X axis, and a roll around the Z axis.
    function RotateYawPitchRollLocal(yaw, pitch, roll: Float): HResult; stdcall;

    // Right multiply the current matrix with the computed scale
    // matrix. (transformation is about the current world origin)
    function Scale(x,y,z: Float): HResult; stdcall;

    // Left multiply the current matrix with the computed scale
    // matrix. (transformation is about the local origin of the object)
    function ScaleLocal(x,y,z: Float): HResult; stdcall;

    // Right multiply the current matrix with the computed translation
    // matrix. (transformation is about the current world origin)
    function Translate(x,y,z: Float): HResult; stdcall;

    // Left multiply the current matrix with the computed translation
    // matrix. (transformation is about the local origin of the object)
    function TranslateLocal(x,y,z: FLoat): HResult; stdcall;

    // Obtain the current matrix at the top of the stack
    function GetTop: TD3DXMatrix; stdcall; // STDMETHOD_(D3DXMATRIX*, GetTop)(THIS) PURE;
 end;


function D3DXCreateMatrixStack(Flags: DWord; out Stack: ID3DXMatrixStack): HResult; stdcall;


///////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 1999 Microsoft Corporation.  All Rights Reserved.
//
//  File:       d3dxshapes.h
//  Content:    D3DX simple shapes
//
///////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////
// Interfaces:
///////////////////////////////////////////////////////////////////////////

//-------------------------------------------------------------------------
// ID3DXSimpleShape interface:
//-------------------------------------------------------------------------

type
  ID3DXSimpleShape = interface (IUnknown)
    ['{CFCD4602-EB7B-11d2-A440-00A0C90629A8}']
    // ID3DXSimpleShape methods
    function GetVB: Pointer; stdcall;  // really IDirect3DVertexBuffer7
    function GetIndices(var pIndices: PWord): Cardinal; stdcall;
    function Draw: HResult; stdcall;
  end;

// #AS# same problem as with ID3DXContext.GetDD... Delphi can't return interfaces directly
function D3DXVBFromShape(const Shape: ID3DXSimpleShape; out VB: IDirect3DVertexBuffer7): Boolean;

///////////////////////////////////////////////////////////////////////////
// Functions:
///////////////////////////////////////////////////////////////////////////

//-------------------------------------------------------------------------
// D3DXCreatePolygon: Creates an 'n' sided polygon using the device
// ----------------  specified. It returns a vertex buffer that can be used
//                   for drawing or manipulation by the program later on.
//
// Params:
//     [in]  LPDIRECT3DDEVICE7 pDevice: The device to create off.
//     [in]  float sideSize: Length of a side.
//     [in]  DWORD numTexCoords:   The number of texture coordinates desired
//                                 in the vertex-buffer. (Default is 1)
//                                 D3DX_DEFAULT is a valid input.
//     [out] IDirect3DVertexBuffer7** ppVB: The output shape interface.
//-------------------------------------------------------------------------
function D3DXCreatePolygon(const Device: IDirect3DDevice7;
   sideSize: Float;
   numSides, numTexCoords: Cardinal;
   out Shape: ID3DXSimpleShape): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXCreateBox: Creates a box (cuboid) of given dimensions using the
// ------------  device. It returns a vertex buffer that can
//               be used for drawing or manipulation by the program later on.
//
// Params:
//     [in]  LPDIRECT3DDEVICE7 pDevice: The device to create off.
//     [in]  float width: Width of the box (along x-axis)
//     [in]  float height: Height of the box (along y-axis)
//     [in]  float depth: Depth of the box (along z-axis)
//     [in]  DWORD numTexCoords: The number of texture coordinates desired
//                               in the vertex-buffer. Default is 1.
//                               D3DX_DEFAULT is a valid input here.
//     [out] LPD3DXSIMPLESHAPE* ppShape: The output vertex-buffer.
//-------------------------------------------------------------------------
function D3DXCreateBox(const Device: IDirect3DDevice7;
   width, height, depth: Float;
   numTexCoords: Cardinal;
   out Shape: ID3DXSimpleShape): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXCreateCylinder: Creates a cylinder of given dimensions using the
// -----------------  device. It returns a vertex buffer that
//                    can be used for drawing or manipulation by the program
//                    later on.
//
// Params:
//     [in]  LPDIRECT3DDEVICE7 pDevice: The device to create off.
//     [in]  float baseRadius: Base-radius (default is 1.0f, shd be >= 0.0f)
//     [in]  float topRadius: Top-radius (default is 1.0f, shd be >= 0.0f)
//     [in]  float height: Height (default is 1.0f, shd be >= 0.0f)
//     [in]  DWORD numSlices: Number of slices about the main axis.
//                            (default is 8) D3DX_DEFAULT is a valid input.
//     [in]  DWORD numStacks: Number of stacks along the main axis.
//                            (default is 8) D3DX_DEFAULT is a valid input.
//     [in]  DWORD numTexCoords: The number of texture coordinates desired
//                               in the vertex-buffer. Default is 1.
//                               D3DX_DEFAULT is a valid input here.
//     [out] LPD3DXSIMPLESHAPE* ppShape: The output shape interface.
//-------------------------------------------------------------------------
function D3DXCreateCylinder(const Device: IDirect3DDevice7;
   baseRadius, topRadius, height: Float;
   numSlices, numStacks, numTexCoords: Cardinal;
   out Shape: ID3DXSimpleShape): HResult; stdcall;


//-------------------------------------------------------------------------
// D3DXCreateTorus: Creates a torus of given dimensions using the
// --------------  device specified. It returns a vertex buffer that can
//                 be used for drawing or manipulation by the program later
//                 on. It draws a doughnut, centered at (0, 0, 0) whose axis
//                 is aligned with the z-axis. With the innerRadius used
//                 as the radius of the cross-section (minor-Radius) and
//                 the outerRadius used as the radius of the central 'hole'.
//
// Params:
//     [in]  LPDIRECT3DDEVICE7 pDevice: The device to create off.
//     [in]  float innerRadius: inner radius (default is 1.0f, shd be >= 0.0f)
//     [in]  float outerRadius: outer radius (default is 2.0f, shd be >= 0.0f)
//     [in]  DWORD numSides: Number of sides in the cross-section
//                           (default is 8). D3DX_DEFAULT is a valid input.
//     [in]  DWORD numRings: Number of rings making up the torus
//                           (default is 8) D3DX_DEFAULT is a valid input.
//     [in]  DWORD numTexCoords: The number of texture coordinates desired
//                                 in the vertex-buffer. Default is 1.
//                                 D3DX_DEFAULT is a valid input here.
//     [out] LPD3DXSIMPLESHAPE* ppShape: The output shape interface.
//-------------------------------------------------------------------------
function D3DXCreateTorus(const Device: IDirect3DDevice7;
   innerRadius, outerRadius: Float;
   numSides, numRings, numTexCoords: Cardinal;
   out Shape: ID3DXSimpleShape): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXCreateTeapot: Creates a teapot using the device specified.
// ----------------  It returns a vertex buffer that can be used for
//                   drawing or manipulation by the program later on.
//
// Params:
//     [in]  LPDIRECT3DDEVICE7 pDevice: The device to create off.
//     [in]  DWORD numTexCoords: The number of texture coordinates desired
//                               in the vertex-buffer. Default is 1.
//                               D3DX_DEFAULT is a valid input here.
//     [out] LPD3DXSIMPLESHAPE* ppShape: The output shape interface.
//-------------------------------------------------------------------------
function D3DXCreateTeapot(const Device: IDirect3DDevice7;
   numTexCoords: Cardinal;
   out Shape: ID3DXSimpleShape): HResult; stdcall;

//-------------------------------------------------------------------------
// D3DXCreateSphere: Creates a cylinder of given dimensions using the
// ----------------  device specified.
//                   It returns a vertex buffer that can be used for
//                   drawing or manipulation by the program later on.
//
// Params:
//     [in]  LPDIRECT3DDEVICE7 pDevice: The device to create off.
//     [in]  float radius: radius (default is 1.0f, shd be >= 0.0f)
//     [in]  float height: Height (default is 1.0f, shd be >= 0.0f)
//     [in]  DWORD numSlices: Number of slices about the main axis
//                            (default is 8) D3DX_DEFAULT is a valid input.
//     [in]  DWORD numStacks: Number of stacks along the main axis
//                            (default is 8) D3DX_DEFAULT is a valid input.
//     [in]  DWORD numTexCoords: The number of texture coordinates desired
//                               in the vertex-buffer. Default is 1.
//                               D3DX_DEFAULT is a valid input here.
//     [out] LPD3DXSIMPLESHAPE* ppShape: The output shape interface.
//-------------------------------------------------------------------------
function D3DXCreateSphere(const Device: IDirect3DDevice7;
   radius: Float;
   numSlices, numStacks, numTexCoords: Cardinal;
   out Shape: ID3DXSimpleShape): HResult; stdcall;

///////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 1999 Microsoft Corporation.  All Rights Reserved.
//
//  File:       d3dxsprite.h
//  Content:    D3DX sprite helper functions
//
//      These functions allow you to use sprites with D3DX. A "sprite" is
//      loosely defined as a 2D image that you want to transfer to the
//      rendering target. The source image can be a texture created
//      with the help of the D3DX texture loader; though advanced users may
//      want to create their own. A helper function (PrepareDeviceForSprite)
//      is provided to make it easy to set up render states on a device.
//      (Again, advanced users can use their own created devices.)
//
//      There are two general techniques for sprites; the simpler one just
//      specifies a destination rectangle and a rotation anlge. A more
//      powerful technique supports rendering to non-rectangular quads.
//
//      Both techniques support clipping, alpha, and rotation. More
//      details are below.
//
///////////////////////////////////////////////////////////////////////////


//-------------------------------------------------------------------------
// D3DXPrepareDeviceForSprite:
//
// Call this function to set up all the render states necessary for
// BltSprite/WarpSprite to work correctly. (Advanced users may opt to
// not call this function first; in which case Blt/WarpSprite functions
// will use whatever render/texture states were set up on the device when
// they are called.)
//
// Warning: This function modifies render states and may impact performance
// negatively on some 3D hardware if it is called too often per frame.
//
// Warning: If the render state changes (other than through calls to
// BltSprite or WarpSprite), you will need to call this function again before
// calling BltSprite or WarpSprite.
//
// Details: This function modifies the the rendering first texture stage and
// it modifies some renderstates for the entire device. Here is the exact
// list:
//
//   SetTextureStageState(0, D3DTSS_COLORARG1,         D3DTA_TEXTURE);
//   SetTextureStageState(0, D3DTSS_COLOROP,           D3DTOP_SELECTARG1);
//   SetTextureStageState(0, D3DTSS_ALPHAARG1,         D3DTA_TEXTURE);
//   SetTextureStageState(0, D3DTSS_ALPHAARG2,         D3DTA_DIFFUSE);
//   SetTextureStageState(0, D3DTSS_ALPHAOP,           D3DTOP_MODULATE);
//   SetTextureStageState(0, D3DTSS_MINFILTER,         D3DTFN_LINEAR);
//   SetTextureStageState(0, D3DTSS_MAGFILTER,         D3DTFG_LINEAR);
//
//   SetRenderState(D3DRENDERSTATE_SRCBLEND,           D3DBLEND_SRCALPHA);
//   SetRenderState(D3DRENDERSTATE_DESTBLEND,          D3DBLEND_INVSRCALPHA);
//   SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE,   TRUE);
//
//   Depending on the value of ZEnable parameter, this function will
//   will either call
//   SetRenderState(D3DRENDERSTATE_ZENABLE,            FALSE);
//   - or -
//   SetRenderState(D3DRENDERSTATE_ZENABLE,            TRUE);
//
// Parameters:
//      pd3dDevice  - a pointer to the d3d device that you wish to prepare
//                    for use with D3DX Sprite Services
//      ZEnable     - a flag indicating whether you want the sprites to
//                    check and update the Z buffer as part of rendering.
//                    If ZEnable is FALSE, OR you are using
//                    alpha-blending, then it is necessary to render your
//                    sprites from back-to-front.
//
//-------------------------------------------------------------------------

function D3DXPrepareDeviceForSprite(const D3DDevice: IDirect3DDevice7;
          ZEnable: Boolean { = False}): HResult; stdcall;



//-------------------------------------------------------------------------
// The D3DXDrawBasicSprite() function performs blitting of source images onto
// a 3D rendering device. This function only calls SetTexture on the first
// renderstage with the parameter (pd3dTexture) if that parameter is non-null.
// This function assumes that D3DXPrepareDeviceForSprite has been called on
// the device or that caller has in some other way correctly prepared the
// renderstates.
//
// This function supports scaling, rotations, alpha-blending, and choosing
// a source sub-rect.
//
// Rotation angle is specified in radians. Both rotations and scales
// are applied around the center of the sprite; where the center of the
// sprite is half the width/height of the sprite, plus the offset parameter.
//
// Use the offset parameter if you want the sprite's center to be something
// other than the image center.
//
// The destination point indicates where you would like the center of
// the sprite to draw to.
//
// Parameters:
//      pd3dTexture - a pointer to the surface containing the texture
//      pd3dDevice  - a pointer to the d3d device to render to. It is
//                    assumed that render states are set up. (See
//                    D3DXPrepareDeviceForSprite)
//      ppointDest  - a pointer to the target point for the sprite. The
//                    components of the vector must be in screen
//                    space.
//      alpha       - alpha value to apply to sprite. 1.0 means totally
//                    opaque; and 0.0 means totally transparent.
//                    WARNING: If you are using alpha, then you should render
//                    from back to front in order to avoid rendering
//                    artifacts.
//      angleRad    - angle of rotation around the 'center' of the rect
//      scale       - a uniform scale that is applied to the source rect
//                    to specify the size of the image that is rendered
//      pOffset     - offset from the center of the source rect to use as the
//                    center of rotation
//      pSourceRect - a rect that indicates what portion of the source
//                    source texture to use. If NULL is passed, then the
//                    entire source is used. If the source texture was
//                    created via D3DX, then the rect should be specified
//                    in the coordinates of the original image (so that you
//                    don't have to worry about stretching/scaling that D3DX
//                    may have done to make the image work with your current
//                    3D Device.) Note that horizontal or vertical mirroring
//                    may be simply accomplished by swapping the left/right
//                    or top/bottom fields of this RECT.
//-------------------------------------------------------------------------

function D3DXDrawSpriteSimple(const D3DTexture: IDirectDrawSurface7;
      const D3DDevice: IDirect3DDevice7;
      ppointDesc: TD3DXVector3;
      Alpha {= 1.0}, Scale { = 1.0}, AngleRad {= 0.0}: Float;
      pOffset: TD3DXVector2;
      SourceRect: TRect): HResult; stdcall;

//-------------------------------------------------------------------------
// The D3DXDrawSprite() function transforms source images onto a 3D
// rendering device. It takes a general 4x4 matrix which is use to transform
// the points of a default rect: (left=-.5, top=-.5, right=+.5, bottom=+.5).
// (This default rect was chosen so that it was centered around the origin
// to ease setting up rotations. And it was chosen to have a width/height of one
// to ease setting up scales.)
//
// This function only calls SetTexture on the first
// renderstage with the parameter (pd3dTexture) if that parameter is non-null.
// This function assumes that D3DXPrepareDeviceForSprite has been called on
// the device or that caller has in some other way correctly prepared the
// renderstates.
//
// This function supports alpha-blending, and choosing
// a source sub-rect. (A value of NULL for source sub-rect means the entire
// texture is used.)
//
// Note that if the transformed points have a value for w (the homogenous
// coordinate) that is not 1, then this function will invert it and pass
// that value to D3D as the rhw field of a TLVERTEX. If the value for w is
// zero, then it use 1 as the rhw.
//
// Parameters:
//      pd3dTexture - a pointer to the surface containing the texture
//      pd3dDevice  - a pointer to the d3d device to render to. It is
//                    assumed that render states are set up. (See
//                    D3DXPrepareDeviceForSprite)
//      pMatrixTransform - 4x4 matrix that specifies the transformation
//                    that will be applied to the default -.5 to +.5
//                    rectangle.
//      alpha       - alpha value to apply to sprite. 1.0 means totally
//                    opaque; and 0.0 means totally transparent.
//                    WARNING: If you are using alpha, then you should render
//                    from back to front in order to avoid rendering
//                    artifacts.Furthermore, you should avoid scenarios where
//                    semi-transparent objects intersect.
//      pSourceRect - a rect that indicates what portion of the source
//                    source texture to use. If NULL is passed, then the
//                    entire source is used. If the source texture was
//                    created via D3DX, then the rect should be specified
//                    in the coordinates of the original image (so that you
//                    don't have to worry about stretching/scaling that D3DX
//                    may have done to make the image work with your current
//                    3D Device.) Note that mirroring may be simply accomplished
//                    by swapping the left/right or top/bottom fields of
//                    this RECT.
//
//-------------------------------------------------------------------------

function D3DXDrawSpriteTransform(const D3DTexture: IDirectDrawSurface7;
  const D3DDevice: IDirect3DDevice7;
  const MatrixTransform: TD3DXMatrix;
  alpha { = 1.0}: Float;
  pSourceRect: PRect {= nil}): HResult; stdcall;

//-------------------------------------------------------------------------
// The D3DXBuildSpriteTransform() function is a helper provided which
// creates a matrix corresponding to simple properties. This matrix is
// set up to pass directly to D3DXTransformSprite.
//
// Parameters:
//      pMatrix     - a pointer to the result matrix
//      prectDest   - a pointer to the target rectangle for the sprite
//      angleRad    - angle of rotation around the 'center' of the rect
//      pOffset     - offset from the center of the source rect to use as the
//                    center of rotation
//
//-------------------------------------------------------------------------

function D3DXBuildSpriteTransform(var pMatrix: TD3DXMATRIX;
  prectDest: TRect;
  AngleRad: Float {= 0.0};
  pOffset: PD3DXVector2 {= nil}): HResult; stdcall;

//-------------------------------------------------------------------------
// The D3DXDrawSprite3D() function renders a texture onto a 3D quad. The
// quad ABCD is broken into two triangles ABC and ACD which are rendered
// via DrawPrim.
//
// Parameters:
//      pd3dTexture - a pointer to the surface containing the texture
//      pd3dDevice  - a pointer to the d3d device to render to. It is
//                    assumed that render states are set up. (See
//                    D3DXPrepareDeviceForSprite)
//      quad        - array of 4 points in the following order:
//                    upper-left, upper-right, lower-right, lower-left.
//                    If these vectors contain a W, then this function
//                    will take the reciprocal of that value to pass as
//                    as the rhw (i.e. reciprocal homogenous w).
//      alpha       - alpha value to apply to sprite. 1.0 means totally
//                    opaque; and 0.0 means totally transparent.
//                    WARNING: If you are using alpha, then you should render
//                    from back to front in order to avoid rendering
//                    artifacts.Furthermore, you should avoid scenarios where
//                    semi-transparent objects intersect.
//      pSourceRect - a rect that indicates what portion of the source
//                    source texture to use. If NULL is passed, then the
//                    entire source is used. If the source texture was
//                    created via D3DX, then the rect should be specified
//                    in the coordinates of the original image (so that you
//                    don't have to worry about stretching/scaling that D3DX
//                    may have done to make the image work with your current
//                    3D Device.) Note that mirroring may be simply accomplished
//                    by swapping the left/right or top/bottom fields of
//                    this RECT.
//-------------------------------------------------------------------------

function D3DXDrawSprite3D(const D3DTexture: IDirectDrawSurface7;
  const D3DDevice: IDirect3DDevice7;
  const Quad: PD3DXVector4;  // four elements!
  Alpha: Float { = 1.0};
  const pSourceRect: PRect { = nil}): HResult; stdcall;

{$DEFINE STATIC}
implementation
// ----------- Macros and inlines as functions -----------------
function D3DXToRadian(Degree: Float): Float;
begin
  Result := Degree * (D3DX_PI / 180.0);
end;

function D3DXToDegree(Radian: Float): Float;
begin
  Result := Radian * (180.0 / D3DX_PI);
end;

function EqualGUID(const G1,G2: TGUID): Boolean;
begin
  Result := CompareMem(@G1,@G2,SizeOf(TGUID));
end;


//===========================================================================
//
// Inline functions
//
//===========================================================================


//--------------------------
// 2D Vector
//--------------------------
function D3DXVector2Zero: TD3DXVector2;  // (0,0)
begin
  Result.x := 0; Result.y := 0;
end;

function D3DXVector2(const _x, _y: Float): TD3DXVector2;
begin
  Result.x := _x; Result.y := _y;
end;

function D3DXVec2Add(const v1, v2: TD3DXVector2): TD3DXVector2;
begin
  Result.x := v1.x + v2.x; Result.y := v1.y + v2.y;
end;

function D3DXVec2Subtract(const v1, v2: TD3DXVector2): TD3DXVector2;
begin
  Result.x := v1.x - v2.x; Result.y := v1.y - v2.y;
end;

function D3DXVector2Equal(const v1, v2: TD3DXVector2): Boolean;
begin
  Result := (v1.x = v2.x) and (v1.y = v2.y);
end;

function D3DXVec2Length(const v: TD3DXVector2): Float;
begin
  with v do Result := Sqrt(Sqr(x)+Sqr(y));
end;

function D3DXVec2LengthSq(const v: TD3DXVector2): Float;
begin
  with v do Result := Sqr(x) + Sqr(y);
end;

function D3DXVec2Dot(const v1, v2: TD3DXVector2): Float;
begin
  Result := v1.x * v2.x + v1.y * v2.y;
end;

// Z component of ((x1,y1,0) cross (x2,y2,0))
function D3DXVec2CCW (const v1, v2: TD3DXVector2): Float;
begin
  Result := v1.x * v2.y - v1.y * v2.x;
end;

// Minimize each component.  x = min(x1, x2), y = min(y1, y2)
function D3DXVec2Minimize(var vOut: TD3DXVector2; const v1, v2: TD3DXVEctor2): PD3DXVector2;
begin
  if v1.x < v2.x then vOut.x := v1.x else vOut.y := v2.x;
  if v1.y < v2.y then vOut.y := v1.y else vOut.y := v2.y;
  Result := @vOut;
end;

// Maximize each component.  x = max(x1, x2), y = max(y1, y2)
function D3DXVec2Maximize(var vOut: TD3DXVector2; const v1, v2: TD3DXVector2): PD3DXVector2;
begin
  if v1.x > v2.x then vOut.x := v1.x else vOut.y := v2.x;
  if v1.y > v2.y then vOut.y := v1.y else vOut.y := v2.y;
  Result := @vOut;
end;

function D3DXVec2Scale(var vOut: TD3DXVector2; const v: TD3DXVector2; const s: Float): PD3DXVector2;
begin
  vOut.x := v.x*s; vOut.y := v.y*s;
  Result := @vOut;
end;

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec2Lerp(var vOut: TD3DXVector2; const v1, v2: TD3DXVector2; const s: Float): PD3DXVector2;
begin
  vOut.x := v1.x + s * (v2.x-v1.x);
  vOut.y := v1.y + s * (v2.y-v1.y);
  Result := @vOut;
end;

//--------------------------
// 3D Vector
//--------------------------
function D3DXVector3Zero: TD3DXVector3; // (0,0,0)
begin
  Result.x := 0; Result.y := 0; Result.z := 0;
end;

function D3DXVector3(const _x, _y, _z: Float): TD3DXVector3;
begin
  Result.x := _x; Result.y := _y; Result.z :=_z;
end;

function D3DXVec3Add(const v1,v2: TD3DXVector3): TD3DXVector3;
begin
  Result.x := v1.x+v2.x; Result.y := v1.y+v2.y; Result.z := v1.z+v2.z;
end;

function D3DXVec3Subtract(const v1,v2: TD3DXVector3): TD3DXVector3;
begin
  Result.x := v1.x-v2.x; Result.y := v1.y-v2.y; Result.z := v1.z-v2.z;
end;

function D3DXVector3Equal(const v1, v2: TD3DXVector3): Boolean;
begin
  Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
end;

function D3DXVec3Length(const v: TD3DXVector3): Float;
begin
  with v do Result := Sqrt(Sqr(x)+Sqr(y)+Sqr(z));
end;

function D3DXVec3LengthSq(const v: TD3DXVector3): Float;
begin
  with v do Result := Sqr(x)+Sqr(y)+Sqr(z);
end;

function D3DXVec3Dot(const v1, v2: TD3DXVector3): Float;
begin
  Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
end;

function D3DXVec3Cross(var vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
begin
  vOut.x := v1.y * v2.z - v1.z * v2.y;
  vOut.y := v1.z * v2.x - v1.x * v2.z;
  vOut.z := v1.x * v2.y - v1.y * v2.x;
  Result := @vOut;
end;

// Minimize each component.  x = min(x1, x2), y = min(y1, y2)
function D3DXVec3Minimize(var vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
begin
  if v1.x < v2.x then vOut.x := v1.x else vOut.x := v2.x;
  if v1.y < v2.y then vOut.y := v1.y else vOut.y := v2.y;
  if v1.z < v2.z then vOut.z := v1.z else vOut.z := v2.z;
  Result := @vOut;
end;

// Maximize each component.  x = max(x1, x2), y = max(y1, y2)
function D3DXVec3Maximize(var vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
begin
  if v1.x > v2.x then vOut.x := v1.x else vOut.x := v2.x;
  if v1.y > v2.y then vOut.y := v1.y else vOut.y := v2.y;
  if v1.z > v2.z then vOut.z := v1.z else vOut.z := v2.z;
  Result := @vOut;
end;


function D3DXVec3Scale(var vOut: TD3DXVector3; const v: TD3DXVector3; const s: Float): PD3DXVector3;
begin
  with vOut do
  begin
    x := v.x * s; y := v.y * s; z := v.z * s;
  end;
  Result := @vOut;
end;

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec3Lerp(var vOut: TD3DXVector3;
  const v1, v2: TD3DXVector3; const s: Float): PD3DXVector3;
begin
  vOut.x := v1.x + s * (v2.x-v1.x);
  vOut.y := v1.y + s * (v2.y-v1.y);
  vOut.z := v1.z + s * (v2.z-v1.z);
  Result := @vOut;
end;

//--------------------------
// 4D Vector
//--------------------------

function D3DXVector4Zero: TD3DXVector4; // (0,0,0,0)
begin
  with Result do
  begin
    x := 0; y := 0; z := 0; w := 0;
  end;
end;

function D3DXVector4(const _x, _y, _z, _w: Float): TD3DXVector4;
begin
  with Result do
  begin
    x := _x; y := _y; z := _z; w := _w;
  end;
end;

function D3DXVector4Add(const v1,v2: TD3DXVector4): TD3DXVector4;
begin
  with Result do
  begin
    x := v1.x + v2.x; y := v1.y + v2.y;
    z := v1.z + v2.z; w := v1.w + v2.w;
  end;
end;

function D3DXVector4Subtract(const v1,v2: TD3DXVector4): TD3DXVector4;
begin
  with Result do
  begin
    x := v1.x - v2.x; y := v1.y - v2.y;
    z := v1.z - v2.z; w := v1.w - v2.w;
  end;
end;

function D3DXVector4Equal(const v1, v2: TD3DXVector4): Boolean;
begin
  Result := (v1.x = v2.x) and (v1.y = v2.y) and
    (v1.z = v2.z) and (v1.w = v2.w);
end;

function D3DXVec4Length(const v: TD3DXVector4): Float;
begin
  with v do Result := Sqrt(Sqr(x)+Sqr(y)+Sqr(z)+Sqr(w));
end;

function D3DXVec4LengthSq(const v: TD3DXVector4): Float;
begin
  with v do Result := Sqr(x)+Sqr(y)+Sqr(z)+Sqr(w);
end;

function D3DXVec4Dot(const v1, v2: TD3DXVector4): Float;
begin
  Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w;
end;

// Minimize each component.  x = min(x1, x2), y = min(y1, y2)
function D3DXVec4Minimize(var vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4;
begin
  if v1.x < v2.x then vOut.x := v1.x else vOut.x := v2.x;
  if v1.y < v2.y then vOut.y := v1.y else vOut.y := v2.y;
  if v1.z < v2.z then vOut.z := v1.z else vOut.z := v2.z;
  if v1.w < v2.w then vOut.w := v1.w else vOut.w := v2.w;
  Result := @vOut;
end;

// Maximize each component.  x = max(x1, x2), y = max(y1, y2)
function D3DXVec4Maximize(var vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4;
begin
  if v1.x > v2.x then vOut.x := v1.x else vOut.x := v2.x;
  if v1.y > v2.y then vOut.y := v1.y else vOut.y := v2.y;
  if v1.z > v2.z then vOut.z := v1.z else vOut.z := v2.z;
  if v1.w > v2.w then vOut.w := v1.w else vOut.w := v2.w;
  Result := @vOut;
end;

function D3DXVec4Scale(var vOut: TD3DXVector4; const v: TD3DXVector4; const s: Float): PD3DXVector4;
begin
  with vOut do
  begin
    x := v.x * s; y := v.y * s; z := v.z * s; w := v.w * s;
  end;
  Result := @vOut;
end;

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec4Lerp(var vOut: TD3DXVector4;
  const v1, v2: TD3DXVector4; const s: Float): PD3DXVector4;
begin
  with vOut do
  begin
    x := v1.x + s * (v2.x - v1.x);
    y := v1.y + s * (v2.y - v1.y);
    z := v1.z + s * (v2.z - v1.z);
    w := v1.w + s * (v2.w - v1.w);
  end;
  Result := @vOut;
end;


//--------------------------
// 4D Matrix
//--------------------------
function D3DXMatrix(const _m00, _m01, _m02, _m03,
                          _m10, _m11, _m12, _m13,
                          _m20, _m21, _m22, _m23,
                          _m30, _m31, _m32, _m33: Float): TD3DXMatrix;
begin
  with Result do
  begin
    m00 := _m00; m01 := _m01; m02 := _m02; m03 := _m03;
    m10 := _m10; m11 := _m11; m12 := _m12; m13 := _m13;
    m20 := _m20; m21 := _m21; m22 := _m22; m23 := _m23;
    m30 := _m30; m31 := _m31; m32 := _m32; m33 := _m33;
  end;
end;

function D3DXMatrixAdd(var mOut: TD3DXMatrix; const m1,m2: TD3DXMatrix): PD3DXMatrix;
var pOut, p1, p2: PFloat; x: Integer;
begin
  pOut := @mOut.m00; p1 := @m1.m00; p2 := @m2.m00;
  for x := 0 to 15 do
  begin
    pOut^ := p1^+p2^;
    Inc(pOut); Inc(p1); Inc(p2);
  end;
  Result := @mOut;
end;

function D3DXMatrixSubtract(var mOut: TD3DXMatrix; const m1,m2: TD3DXMatrix): PD3DXMatrix;
var pOut, p1, p2: PFloat; x: Integer;
begin
  pOut := @mOut.m00; p1 := @m1.m00; p2 := @m2.m00;
  for x := 0 to 15 do
  begin
    pOut^ := p1^-p2^;
    Inc(pOut); Inc(p1); Inc(p2);
  end;
  Result := @mOut;
end;

function D3DXMatrixMul(var mOut: TD3DXMatrix; const m: TD3DXMatrix; const MulBy: Float): PD3DXMatrix;
var pOut, p: PFloat; x: Integer;
begin
  pOut := @mOut.m00; p := @m.m00;
  for x := 0 to 15 do
  begin
    pOut^ := p^* MulBy;
    Inc(pOut); Inc(p);
  end;
  Result := @mOut;
end;

function D3DXMatrixEqual(const m1, m2: TD3DXMatrix): Boolean;
begin
  Result := CompareMem(@m1,@m2,SizeOf(TD3DXMatrix));
end;

// "inline"
function D3DXMatrixIdentity(var mOut: TD3DXMatrix): PD3DXMatrix;
begin
  FillChar(mOut, SizeOf(mOut), 0);
  mOut.m00 := 1; mOut.m11 := 1; mOut.m22 := 1; mOut.m33 := 1;
  Result := @mOut;
end;

function D3DXMatrixIsIdentity(const m: TD3DXMatrix): Boolean;
begin
  with m do Result :=
    (m00 = 1) and (m11 = 1) and (m22 = 1) and (m33 = 1) and
    (m01 + m02 + m03 + m10 + m12 + m13 + m20 + m21 + m23 + m30 + m31 + m32 = 0);
end;

//--------------------------
// Quaternion
//--------------------------
function D3DXQuaternion(const _x, _y, _z, _w: Float): TD3DXQuaternion;
begin
  with Result do
  begin
    x := _x; y := _y; z := _z; w := _w;
  end;
end;

function D3DXQuaternionAdd(const q1,q2: TD3DXQuaternion): TD3DXQuaternion;
begin
  with Result do
  begin
    x := q1.x+q2.x; y := q1.y+q2.y; z := q1.z+q2.z; w := q1.w+q2.w;
  end;
end;

function D3DXQuaternionSubtract(const q1,q2: TD3DXQuaternion): TD3DXQuaternion;
begin
  with Result do
  begin
    x := q1.x-q2.x; y := q1.y-q2.y; z := q1.z-q2.z; w := q1.w-q2.w;
  end;
end;

function D3DXQuaternionEqual(const q1, q2: TD3DXQuaternion): Boolean;
begin
  Result := (q1.x = q2.x) and (q1.y = q2.y) and
    (q1.z = q2.z) and (q1.w = q2.w);
end;

function D3DXQuaternionScale(var qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion; const s: Float): PD3DXQuaternion;
begin
  with qOut do
  begin
    x := q.x*s; y := q.y*s; z := q.z*s; w := q.w*s;
  end;
  Result := @qOut;
end;

function D3DXQuaternionLength(const q: TD3DXQuaternion): Float;
begin
  with q do Result := Sqrt(Sqr(x)+Sqr(y)+Sqr(z)+Sqr(w));
end;

// Length squared, or "norm"
function D3DXQuaternionLengthSq(const q: TD3DXQuaternion): Float;
begin
  with q do Result := Sqr(x)+Sqr(y)+Sqr(z)+Sqr(w);
end;

function D3DXQuaternionDot(const q1, q2: TD3DXQuaternion): Float;
begin
  Result := q1.x * q2.x + q1.y * q2.y + q1.z * q2.z + q1.w * q2.w;
end;

function D3DXQuaternionIdentity(var qOut: TD3DXQuaternion): PD3DXQuaternion;
begin
  with qOut do
  begin
    x := 0; y := 0; z := 0; w := 1.0;
  end;
  Result := @qOut;
end;

function D3DXQuaternionIsIdentity(const q: TD3DXQuaternion): Boolean;
begin
  with q do Result := (x+y+z = 0) and (w = 1.0);
end;

// (-x, -y, -z, w)
function D3DXQuaternionConjugate(var qOut: TD3DXQuaternion;
  const q: TD3DXQuaternion): PD3DXQuaternion;
begin
  with qOut do
  begin
    x := -q.x; y := -q.y; z := -q.z; w := -q.w;
  end;
  Result := @qOut;
end;

//--------------------------
// Plane
//--------------------------

function D3DXPlaneZero: TD3DXPlane; // (0,0,0,0)
begin
  with Result do
  begin
    a := 0; b :=0; c := 0 ; d := 0;
  end;
end;

function D3DXPlane(const _a, _b, _c, _d: Float): TD3DXPlane;
begin
  with Result do
  begin
    a := _a; b := _b; c := _c; d := _d;
  end;
end;


function D3DXPlaneEqual(const p1, p2: TD3DXPlane): Boolean;
begin
  Result := (p1.a = p2.a) and (p1.b = p2.b) and
    (p1.c = p2.c) and (p1.d = p2.d);
end;

// ax + by + cz + dw
function D3DXPlaneDot(const p: TD3DXPlane; const v: TD3DXVector4): Float;
begin
  with p,v do Result := a*x + b*y + c*z + d*w;
end;

// ax + by + cz + d
function D3DXPlaneDotCoord(const p: TD3DXPlane; const v: TD3DXVector3): Float;
begin
  with p,v do Result := a*x + b*y + c*z + d;
end;

// ax + by + cz
function D3DXPlaneDotNormal(const p: TD3DXPlane; const v: TD3DXVector3): Float;
begin
  with p,v do Result := a*x + b*y + c*z;
end;

//--------------------------
// Color
//--------------------------

function D3DXColor(const _r, _g, _b, _a: Float): TD3DXColor;
begin
  with Result do
  begin
    r := _r; g := _g; b := _b; a := _a;
  end;
end;

function D3DXColorToDWord(c: TD3DXColor): DWord;

  function ColorLimit(const x: Float): DWord;
  begin
    if x > 1.0 then Result := 255
     else if x < 0 then Result := 0
      else Result := Trunc(x * 255.0 + 0.5);
  end;
begin
  Result := ColorLimit(c.a) shl 24 or ColorLimit(c.r) shl 16
    or ColorLimit(c.g) shl 8 or ColorLimit(c.b);
end;

function D3DXColorFromDWord(c: DWord): TD3DXColor;
begin
  with Result do
  begin
    r := ((c shr 24) and $0f) / 256;
    g := ((c shr 16) and $0f) / 256;
    b := ((c shr 8) and $0f) / 256;
    a := ((c shr 0) and $0f) / 256;
  end;
end;

function D3DXColorEqual(const c1, c2: TD3DXColor): Boolean;
begin
  Result := (c1.r = c2.r) and (c1.g = c2.g) and (c1.b = c2.b) and (c1.a = c2.a);
end;

function D3DXColorNegative(var cOut: TD3DXColor; const c: TD3DXColor): PD3DXColor;
begin
 with cOut do
 begin
   r := 1.0 - c.r; g := 1.0 - c.g; b := 1.0 - c.b;
   a := c.a;
 end;
 Result := @cOut;
end;

function D3DXColorAdd(var cOut: TD3DXColor; const c1,c2: TD3DXColor): PD3DXColor;
begin
  with cOut do
  begin
    r := c1.r+c2.r; g := c1.g + c2.g; b := c1.b + c2.b;
    a := c1.a + c2.a;
  end;
  Result := @cOut;
end;

function D3DXColorSubtract(var cOut: TD3DXColor; const c1,c2: TD3DXColor): PD3DXColor;
begin
  with cOut do
  begin
    r := c1.r-c2.r; g := c1.g - c2.g; b := c1.b - c2.b;
    a := c1.a - c2.a;
  end;
  Result := @cOut;
end;

function D3DXColorScale(var cOut: TD3DXColor; const c: TD3DXColor; s: Float): PD3DXColor;
begin
  with cOut do
  begin
    r := c.r * s; g := c.g * s;
    b := c.b * s; a := c.a * s;
  end;
  Result := @cOut;
end;

// (r1*r2, g1*g2, b1*b2, a1*a2)
function D3DXColorModulate(var cOut: TD3DXColor; const c1,c2: TD3DXColor): PD3DXColor;
begin
  with cOut do
  begin
    r := c1.r * c2.r; g := c1.g * c2.g;
    b := c1.b * c2.b; a := c1.a * c2.a;
  end;
  Result := @cOut;
end;

// Linear interpolation of r,g,b, and a. C1 + s(C2-C1)
function D3DXColorLerp(var cOut: TD3DXColor; const c1,c2: TD3DXColor; s: Float): PD3DXColor;
begin
  with cOut do
  begin
    r := c1.r + s * (c2.r - c1.r);
    g := c1.g + s * (c2.g - c1.g);
    b := c1.b + s * (c2.b - c1.b);
    a := c1.a + s * (c2.a - c1.a);
  end;
  Result := @cOut;
end;


// ----------- Delphi Helpers --------------------------------
function D3DXGetErrorMsg(Res: HResult): String;  // Delphi Helper
begin
  SetLength(Result,512);
  D3DXGetErrorString(Res, 512, PAnsiChar(Result));
  Result := PChar(Result);  // remove garbage at string end
end;


function D3DXDDFromContext(const Context: ID3DXContext; out DD: IDirectDraw7): Boolean;
begin
  Result := Assigned(Context);
  if Result then
  begin
    Pointer(DD) := Context.GetDD;
    Result := Assigned(DD);
    if Result then DD._AddRef;
  end;
end;

function D3DXD3DFromContext(const Context: ID3DXContext; out D3D: IDirect3D7): Boolean;
begin
  Result := Assigned(Context);
  if Result then
  begin
    Pointer(D3D) := Context.GetD3D;
    Result := Assigned(D3D);
    if Result then D3D._AddRef;
  end;
end;

function D3DXD3DDeviceFromContext(const Context: ID3DXContext; out D3DDev: IDirect3DDevice7): Boolean;
begin
  Result := Assigned(Context);
  if Result then
  begin
    Pointer(D3DDev) := Context.GetD3DDevice;
    Result := Assigned(D3DDev);
    if Result then D3DDev._AddRef;
  end;
end;

function D3DXPrimaryFromContext(const Context: ID3DXContext; out Primary: IDirectDrawSurface7): Boolean;
begin
  Result := Assigned(Context);
  if Result then
  begin
    Pointer(Primary) := Context.GetPrimary;
    Result := Assigned(Primary);
    if Result then Primary._AddRef;
  end;
end;

function D3DXZBufferFromContext(const Context: ID3DXContext; out ZBuffer: IDirectDrawSurface7): Boolean;
begin
  Result := Assigned(Context);
  if Result then
  begin
    Pointer(ZBuffer) := Context.GetZBuffer;
    Result := Assigned(ZBuffer);
    if Result then ZBuffer._AddRef;
  end;
end;

function D3DXBackBufferFromContext(const Context: ID3DXContext; which: Cardinal; out BackBuffer: IDirectDrawSurface7): Boolean;
begin
  Result := Assigned(Context);
  if Result then
  begin
    Pointer(BackBuffer) := Context.GetBackBuffer(which);
    Result := Assigned(BackBuffer);
    if Result then BackBuffer._AddRef;
  end;
end;

function D3DXVBFromShape(const Shape: ID3DXSimpleShape; out VB: IDirect3DVertexBuffer7): Boolean;
begin
  Result := Assigned(Shape);
  if Result then
  begin
    Pointer(VB) := Shape.GetVB;
    Result := Assigned(VB);
    if Result then VB._AddRef;
  end;
end;

// currently unused
type TD3DXLoadState = (D3DXLoaded, D3DXOldVersion, D3DXDLLNotFound);

{$IFDEF STATIC}
function D3DXInitialize; external d3dxdll name 'D3DXInitialize';
function D3DXUninitialize; external d3dxdll name 'D3DXUninitialize';
function D3DXGetDeviceCount; external d3dxdll name 'D3DXGetDeviceCount';
function D3DXGetDeviceDescription; external d3dxdll name 'D3DXGetDeviceDescription';
function D3DXGetMaxNumVideoModes; external d3dxdll name 'D3DXGetMaxNumVideoModes';
function D3DXGetVideoMode; external d3dxdll name 'D3DXGetVideoMode';
function D3DXGetMaxSurfaceFormats; external d3dxdll name 'D3DXGetMaxSurfaceFormats';
function D3DXGetSurfaceFormat; external d3dxdll name 'D3DXGetSurfaceFormat';
function D3DXGetCurrentVideoMode; external d3dxdll name 'D3DXGetCurrentVideoMode';
function D3DXGetDeviceCaps; external d3dxdll name 'D3DXGetDeviceCaps';
function D3DXCreateContext; external d3dxdll name 'D3DXCreateContext';
function D3DXCreateContextEx; external d3dxdll name 'D3DXCreateContextEx';
procedure D3DXGetErrorString; external d3dxdll name 'D3DXGetErrorString';
function D3DXMakeDDPixelFormat; external d3dxdll name 'D3DXMakeDDPixelFormat';
function D3DXMakeSurfaceFormat; external d3dxdll name 'D3DXMakeSurfaceFormat';
function D3DXCheckTextureRequirements; external d3dxdll name 'D3DXCheckTextureRequirements';
function D3DXCreateTexture; external d3dxdll name 'D3DXCreateTexture';
function D3DXCreateCubeMapTexture; external d3dxdll name 'D3DXCreateCubeMapTexture';
function D3DXCreateTextureFromFile; external d3dxdll name 'D3DXCreateTextureFromFile';
function D3DXLoadTextureFromFile; external d3dxdll name 'D3DXLoadTextureFromFile';
function D3DXLoadTextureFromSurface; external d3dxdll name 'D3DXLoadTextureFromSurface';
function D3DXLoadTextureFromMemory; external d3dxdll name 'D3DXLoadTextureFromMemory';
function D3DXVec2Normalize; external d3dxdll name 'D3DXVec2Normalize';
function D3DXVec2Hermite; external d3dxdll name 'D3DXVec2Hermite';
function D3DXVec2BaryCentric; external d3dxdll name 'D3DXVec2BaryCentric';
function D3DXVec2Transform; external d3dxdll name 'D3DXVec2Transform';
function D3DXVec2TransformCoord; external d3dxdll name 'D3DXVec2TransformCoord';
function D3DXVec2TransformNormal; external d3dxdll name 'D3DXVec2TransformNormal';
function D3DXVec3Normalize; external d3dxdll name 'D3DXVec3Normalize';
function D3DXVec3Hermite; external d3dxdll name 'D3DXVec3Hermite';
function D3DXVec3BaryCentric; external d3dxdll name 'D3DXVec3BaryCentric';
function D3DXVec3Transform; external d3dxdll name 'D3DXVec3Transform';
function D3DXVec3TransformCoord; external d3dxdll name 'D3DXVec3TransformCoord';
function D3DXVec3TransformNormal; external d3dxdll name 'D3DXVec3TransformNormal';
function D3DXVec4Cross; external d3dxdll name 'D3DXVec4Cross';
function D3DXVec4Normalize; external d3dxdll name 'D3DXVec4Normalize';
function D3DXVec4Hermite; external d3dxdll name 'D3DXVec4Hermite';
function D3DXVec4BaryCentric; external d3dxdll name 'D3DXVec4BaryCentric';
function D3DXVec4Transform; external d3dxdll name 'D3DXVec4Transform';
function D3DXMatrixfDeterminant; external d3dxdll name 'D3DXMatrixfDeterminant';
function D3DXMatrixMultiply; external d3dxdll name 'D3DXMatrixMultiply';
function D3DXMatrixTranspose; external d3dxdll name 'D3DXMatrixTranspose';
function D3DXMatrixInverse; external d3dxdll name 'D3DXMatrixInverse';
function D3DXMatrixScaling; external d3dxdll name 'D3DXMatrixScaling';
function D3DXMatrixTranslation; external d3dxdll name 'D3DXMatrixTranslation';
function D3DXMatrixRotationX; external d3dxdll name 'D3DXMatrixRotationX';
function D3DXMatrixRotationY; external d3dxdll name 'D3DXMatrixRotationY';
function D3DXMatrixRotationZ; external d3dxdll name 'D3DXMatrixRotationZ';
function D3DXMatrixRotationAxis; external d3dxdll name 'D3DXMatrixRotationAxis';
function D3DXMatrixRotationQuaternion; external d3dxdll name 'D3DXMatrixRotationQuaternion';
function D3DXMatrixRotationYawPitchRoll; external d3dxdll name 'D3DXMatrixRotationYawPitchRoll';
function D3DXMatrixTransformation; external d3dxdll name 'D3DXMatrixTransformation';
function D3DXMatrixAffineTransformation; external d3dxdll name 'D3DXMatrixAffineTransformation';
function D3DXMatrixLookAt; external d3dxdll name 'D3DXMatrixLookAt';
function D3DXMatrixLookAtLH; external d3dxdll name 'D3DXMatrixLookAtLH';
function D3DXMatrixPerspective; external d3dxdll name 'D3DXMatrixPerspective';
function D3DXMatrixPerspectiveLH; external d3dxdll name 'D3DXMatrixPerspectiveLH';
function D3DXMatrixPerspectiveFov; external d3dxdll name 'D3DXMatrixPerspectiveFov';
function D3DXMatrixPerspectiveFovLH; external d3dxdll name 'D3DXMatrixPerspectiveFovLH';
function D3DXMatrixPerspectiveOffCenter; external d3dxdll name 'D3DXMatrixPerspectiveOffCenter';
function D3DXMatrixPerspectiveOffCenterLH; external d3dxdll name 'D3DXMatrixPerspectiveOffCenterLH';
function D3DXMatrixOrtho; external d3dxdll name 'D3DXMatrixOrtho';
function D3DXMatrixOrthoLH; external d3dxdll name 'D3DXMatrixOrthoLH';
function D3DXMatrixOrthoOffCenter; external d3dxdll name 'D3DXMatrixOrthoOffCenter';
function D3DXMatrixOrthoOffCenterLH; external d3dxdll name 'D3DXMatrixOrthoOffCenterLH';
function D3DXMatrixShadow; external d3dxdll name 'D3DXMatrixShadow';
function D3DXMatrixReflect; external d3dxdll name 'D3DXMatrixReflect';
procedure D3DXQuaternionToAxisAngle; external d3dxdll name 'D3DXQuaternionToAxisAngle';
function D3DXQuaternionRotationMatrix; external d3dxdll name 'D3DXQuaternionRotationMatrix';
function D3DXQuaternionRotationAxis; external d3dxdll name 'D3DXQuaternionRotationAxis';
function D3DXQuaternionRotationYawPitchRoll; external d3dxdll name 'D3DXQuaternionRotationYawPitchRoll';
function D3DXQuaternionMultiply; external d3dxdll name 'D3DXQuaternionMultiply';
function D3DXQuaternionNormalize; external d3dxdll name 'D3DXQuaternionNormalize';
function D3DXQuaternionInverse; external d3dxdll name 'D3DXQuaternionInverse';
function D3DXQuaternionLn; external d3dxdll name 'D3DXQuaternionLn';
function D3DXQuaternionExp; external d3dxdll name 'D3DXQuaternionExp';
function D3DXQuaternionSlerp; external d3dxdll name 'D3DXQuaternionSlerp';
function D3DXQuaternionSquad; external d3dxdll name 'D3DXQuaternionSquad';
function D3DXQuaternionBaryCentric; external d3dxdll name 'D3DXQuaternionBaryCentric';
function D3DXPlaneNormalize; external d3dxdll name 'D3DXPlaneNormalize';
function D3DXPlaneIntersectLine; external d3dxdll name 'D3DXPlaneIntersectLine';
function D3DXPlaneFromPointNormal; external d3dxdll name 'D3DXPlaneFromPointNormal';
function D3DXPlaneFromPoints; external d3dxdll name 'D3DXPlaneFromPoints';
function D3DXPlaneTransform; external d3dxdll name 'D3DXPlaneTransform';
function D3DXColorAdjustSaturation; external d3dxdll name 'D3DXColorAdjustSaturation';
function D3DXColorAdjustContrast; external d3dxdll name 'D3DXColorAdjustContrast';
function D3DXCreateMatrixStack; external d3dxdll name 'D3DXCreateMatrixStack';
function D3DXCreatePolygon; external d3dxdll name 'D3DXCreatePolygon';
function D3DXCreateBox; external d3dxdll name 'D3DXCreateBox';
function D3DXCreateCylinder; external d3dxdll name 'D3DXCreateCylinder';
function D3DXCreateTorus; external d3dxdll name 'D3DXCreateTorus';
function D3DXCreateTeapot; external d3dxdll name 'D3DXCreateTeapot';
function D3DXCreateSphere; external d3dxdll name 'D3DXCreateSphere';
function D3DXPrepareDeviceForSprite; external d3dxdll name 'D3DXPrepareDeviceForSprite';
function D3DXDrawSpriteSimple; external d3dxdll name 'D3DXDrawSpriteSimple';
function D3DXDrawSpriteTransform; external d3dxdll name 'D3DXDrawSpriteTransform';
function D3DXBuildSpriteTransform; external d3dxdll name 'D3DXBuildSpriteTransform';
function D3DXDrawSprite3D; external d3dxdll name 'D3DXDrawSprite3D';
{$ELSE}
{$ENDIF}
end.

