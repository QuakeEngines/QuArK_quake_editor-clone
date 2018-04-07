(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) QuArK Development Team

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

http://quark.sourceforge.net/ - Contact information in AUTHORS.TXT
**************************************************************************)
unit SystemDetails;

interface

{$I DelphiVer.inc}

uses
  SysUtils, StrUtils, Windows, Classes, ExtraFunctionality;

function CheckWindowsNT: Boolean;
function ProcessExists(const exeFileName: String): Boolean;
function WindowExists(const WindowName: String): Boolean;
function RetrieveModuleFilename(ModuleHandle: HMODULE): String;
procedure WarnDriverBugs;
procedure SetDllSearchPath;

type
  {$IFDEF Delphi4orNewerCompiler}
     TLargInt = _LARGE_INTEGER;
  {$ELSE}
     TLargInt = TLargeInteger;
     Int64 = TLargeInteger;
     LongWord = DWORD;
  {$ENDIF}

  TCPU = class(TPersistent)
  private
    FVendorID,
    FVendor,
    FSubModel: string;
    FModel,
    FCount,
    FArchitecture,
    FLevel,
    FStepping,
    FFamily,
    FTyp,
    FVendorNo,
    FFreq :integer;
    FCPUID :boolean;
    function CPUIDExists :boolean;
    procedure GetCPUID;
    function GetCPUIDLevel :integer;
    function GetCPUType :integer;
    function GetCPUVendor :string;
    function GetCPUVendorID :string;
    function GetCPUFreqEx :extended;
    function GetSubModel :string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetInfo;
    procedure Report(var sl :TStringList);
  published
    property CPUID :Boolean read FCPUID write FCPUID stored false;
    property Architecture :integer read FArchitecture write FArchitecture stored false;
    property Level :integer read FLevel write FLevel stored false;
    property Count :integer read FCount write FCount stored false;
    property Vendor :string read FVendor write FVendor stored false;
    property VendorID :string read FVendorID write FVendorID stored false;
    property Freq :integer read FFreq write FFreq stored false;
    property Family :integer read FFamily write FFamily stored false;
    property Stepping :integer read FStepping write FStepping stored false;
    property Model :integer read FModel write FModel stored false;
    property Typ :integer read FTyp write FTyp stored false;
    property SubModel :string read FSubModel write FSubModel stored false;
  end;

  TMemory = class(TPersistent)
  private
    FMemoryLoad: DWORD;
    FPhysicalTotal: SIZE_T;
    FPhysicalFree: SIZE_T;
    FPageFileTotal: SIZE_T;
    FPageFileFree: SIZE_T;
    FVirtualTotal: SIZE_T;
    FVirtualFree: SIZE_T;

    FAllocGranularity: DWORD;
    FMinAppAddress: Integer;
    FMaxAppAddress: Integer;
    FPageSize: DWORD;
    FGDIRes: Byte;
    FUserRes: Byte;
    FSystemRes: Byte;
    function GetSystemRes: Byte;
    function GetGDIRes: Byte;
    function GetUSERRes: Byte;
  public
    procedure GetInfo;
    procedure Report(var sl :TStringList);
  published
    property MemoryLoad :DWORD read FMemoryLoad write FMemoryLoad stored false;
    property PhysicalTotal :SIZE_T read FPhysicalTotal write FPhysicalTotal stored false;
    property PhysicalFree :SIZE_T read FPhysicalFree write FPhysicalFree stored false;
    property PageFileTotal :SIZE_T read FPageFileTotal write FPageFileTotal stored false;
    property PageFileFree :SIZE_T read FPageFileFree write FPageFileFree stored false;
    property VirtualTotal :SIZE_T read FVirtualTotal write FVirtualTotal stored false;
    property VirtualFree :SIZE_T read FVirtualFree write FVirtualFree stored false;
    property AllocGranularity :DWORD read FAllocGranularity write FAllocGranularity stored false;
    property MaxAppAddress :Integer read FMaxAppAddress write FMaxAppAddress stored false;
    property MinAppAddress :Integer read FMinAppAddress write FMinAppAddress stored false;
    property PageSize :DWORD read FPageSize write FPageSize stored false;
    property Win9x_SystemRes :Byte read FSystemRes write FSystemRes stored false;
    property Win9x_GDIRes :Byte read FGDIRes write FGDIRes stored false;
    property Win9x_UserRes :Byte read FUserRes write FUserRes stored false;
  end;

  TOperatingSystem = class(TPersistent)
  private
    FExtended: Boolean;
    FBuildNumber: DWORD;
    FMajorVersion: DWORD;
    FMinorVersion: DWORD;
    FPlatform: string;
    FCSD: string;
    FServicePackMajor: integer;
    FServicePackMinor: integer;
    FSuiteMask: integer;
    FProductType: integer;
    FWow64: Boolean;
    FVersion: string;
    FRegUser: string;
    FSerialNumber: string;
    FRegOrg: string;
    FEnv: TStrings;
    FDirs: TStrings;
    procedure GetEnvironment;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetInfo;
    procedure Report(var sl :TStringList);
  published
    property Extended :boolean read FExtended write FExtended stored false;
    property MajorVersion :DWORD read FMajorVersion write FMajorVersion stored false;
    property MinorVersion :DWORD read FMinorVersion write FMinorVersion stored false;
    property BuildNumber :DWORD read FBuildNumber write FBuildNumber stored false;
    property Platform :string read FPlatform write FPlatform stored false;
    property Version :string read FVersion write FVersion stored false;
    property CSD :string read FCSD write FCSD stored false;
    property ServicePackMajor :integer read FServicePackMajor write FServicePackMajor stored false;
    property ServicePackMinor :integer read FServicePackMinor write FServicePackMinor stored false;
    property SuiteMask :integer read FSuiteMask write FSuiteMask stored false;
    property ProductType :integer read FProductType write FProductType stored false;
    property Wow64 :Boolean read FWow64 write FWow64 stored false;
    property SerialNumber :string read FSerialNumber write FSerialNumber stored false;
    property RegisteredUser :string read FRegUser write FRegUser stored false;
    property RegisteredOrg :string read FRegOrg write FRegOrg stored false;
    property Environment :TStrings read FEnv write FEnv stored false;
    property Directories :TStrings read FDirs write FDirs stored False;
  end;

  TWorkstation = class(TPersistent)
  private
    FName: string;
    FUser: string;
    FSystemUpTime: Extended;
    FBIOSExtendedInfo: string;
    FBIOSCopyright: string;
    FBIOSName: string;
    FBIOSDate: string;
    //FScrollLock: Boolean;
    //FNumLock: Boolean;
    //FCapsLock: Boolean;
    function GetSystemUpTime: Extended;
  public
    procedure GetInfo;
    procedure Report(var sl :TStringList);
  published
    property Name :string read FName write FName stored false;
    property User :string read FUser write FUser stored false;
    property SystemUpTime :Extended read FSystemUpTime write FSystemUpTime stored false;
    property BIOSCopyright :string read FBIOSCopyright write FBIOSCopyright stored false;
    property BIOSDate :string read FBIOSDate write FBIOSDate stored false;
    property BIOSExtendedInfo :string read FBIOSExtendedInfo write FBIOSExtendedInfo stored false;
    property BIOSName :string read FBIOSName write FBIOSName stored false;
    //property CapsLock :Boolean read FCapsLock write FCapsLock stored false;
    //property NumLock :Boolean read FNumLock write FNumLock stored false;
    //property ScrollLock :Boolean read FScrollLock write FScrollLock stored false;
  end;

  TCurveCap = (ccCircles,ccPieWedges,ccChords,ccEllipses,ccWideBorders,ccStyledBorders,
               ccWideStyledBorders,ccInteriors,ccRoundedRects);
  TLineCap = (lcPolylines,lcMarkers,lcMultipleMarkers,lcWideLines,lcStyledLines,
               lcWideStyledLines,lcInteriors);
  TPolygonCap = (pcAltFillPolygons,pcRectangles,pcWindingFillPolygons,pcSingleScanlines,
                 pcWideBorders,pcStyledBorders,pcWideStyledBorders,pcInteriors);
  TRasterCap = (rcRequiresBanding,rcTranserBitmaps,rcBitmaps64K,rcSetGetDIBits,
                rcSetDIBitsToDevice,rcFloodfills,rcWindows2xFeatures,rcPaletteBased,
                rcScaling,rcStretchBlt,rcStretchDIBits);
  TTextCap = (tcCharOutPrec,tcStrokeOutPrec,tcStrokeClipPrec,tcCharRotation90,
              tcCharRotationAny,tcScaleIndependent,tcDoubledCharScaling,tcIntMultiScaling,
              tcAnyMultiExactScaling,tcDoubleWeightChars,tcItalics,tcUnderlines,
              tcStrikeouts,tcRasterFonts,tcVectorFonts,tcNoScrollUsingBlts);

  TCurveCaps = set of TCurveCap;
  TLineCaps = set of TLineCap;
  TPolygonCaps = set of TPolygonCap;
  TRasterCaps = set of TRasterCap;
  TTextCaps = set of TTextCap;

  TDisplay = class(TPersistent)
  private
    FVertRes: integer;
    FColorDepth: integer;
    FHorzRes: integer;
    FPixelDiagonal: integer;
    FPixelHeight: integer;
    FVertSize: integer;
    FPixelWidth: integer;
    FHorzSize: integer;
    FTechnology: string;
    FCurveCaps: TCurveCaps;
    FLineCaps: TLineCaps;
    FPolygonCaps: TPolygonCaps;
    FRasterCaps: TRasterCaps;
    FTextCaps: TTextCaps;
    FMemory: TStrings;
    FChipset: TStrings;
    FDevices: TStrings;
    FAdapter: TStrings;
    FDAC: TStrings;
    FProvider: TStrings;
    FDriverDate: TStrings;
    FDriverVersion: TStrings;
    FAcc: TStrings;
    FModes: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetInfo;
    procedure Report(var sl :TStringList);
  published
    property Adapter :TStrings read FAdapter write FAdapter stored false;
    property Devices :TStrings read FDevices write FDevices stored false;
    property Accelerator :TStrings read FAcc write FAcc stored false;
    property DAC :TStrings read FDAC write FDAC stored false;
    property Chipset :TStrings read FChipset write FChipset stored false;
    property Memory :TStrings read FMemory write FMemory stored false;
    property Provider :TStrings read FProvider write FProvider stored false;
    property DriverDate :TStrings read FDriverDate write FDriverDate stored false;
    property DriverVersion :TStrings read FDriverVersion write FDriverVersion stored false;
    property HorzRes :integer read FHorzRes write FHorzRes stored false;
    property VertRes :integer read FVertRes write FVertRes stored false;
    property ColorDepth :integer read FColorDepth write FColorDepth stored false;
    property Technology :string read FTechnology write FTechnology stored false;
//    property HorzSize :integer read FHorzSize write FHorzSize stored false;
//    property VertSize :integer read FVertSize write FVertSize stored false;
    property PixelWidth :integer read FPixelWidth write FPixelWidth stored false;
    property PixelHeight :integer read FPixelHeight write FPixelHeight stored false;
    property PixelDiagonal :integer read FPixelDiagonal write FPixelDiagonal stored false;
    property RasterCaps :TRasterCaps read FRasterCaps write FRasterCaps stored false;
    property CurveCaps :TCurveCaps read FCurveCaps write FCurveCaps stored false;
    property LineCaps :TLineCaps read FLineCaps write FLineCaps stored false;
    property PolygonCaps :TPolygonCaps read FPolygonCaps write FPolygonCaps stored false;
    property TextCaps :TTextCaps read FTextCaps write FTextCaps stored false;
    property Modes :TStrings read FModes write FModes stored False;
  end;

  TDirectX = class(TPersistent)
  private
    FVersion: string;
    FDirect3D: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetInfo;
    procedure Report(var sl :TStringList);
  published
    property Version :string read FVersion write FVersion stored false;
    property Direct3D :TStrings read FDirect3D write FDirect3D stored false;
  end;

implementation

uses ShlObj, TlHelp32, Psapi, Registry, Logging, QkExceptions;

type
  TPlatformType = (osWin95Comp, osWinNTComp);
  TPlatform = (osWin95, osWin98, osWin98SE, osWinME, osWinNT4, osWin2000, osWinXP, osWin2003, osWinVista, osWin7, osWin8, osWin81, osWin2008, osWin2008R2, osWin2012, osWin2012R2); //Note: Not all are currently detected!

  TStrBuf = array[0..11] of char;

var
  VLevel, VFamily, VModel, VStepping, VTyp: Byte;
  VFeatures: LongInt;
  WindowsPlatformCompatibility: TPlatformType;
  WindowsPlatform: TPlatform;
  AMDDriverDescBug: boolean;

function FormatBytes(const Number: Cardinal) : String;
begin
  Result:=formatfloat('#,##',Number);
  if Length(Result)=0 then
    Result:='0';
end;

{ TCPU }

constructor TCPU.Create;
begin
  inherited;
end;

destructor TCPU.Destroy;
begin
  inherited;
end;

const
  ID_Bit = $200000;    // EFLAGS ID bit

  CPUVendorIDs :array[0..13] of string = ('GenuineIntel',
                                          'UMC UMC UMC',
                                          'AuthenticAMD',
                                          'CyrixInstead',
                                          'NexGenDriven',
                                          'CentaurHauls',
                                          'RiseRiseRise',
                                          'SiS SiS SiS',
                                          'GenuineTMx86',
                                          'Geode by NSC',
                                          'VIA VIA VIA ',
                                          'AMDisbetter!',
                                          'TransmetaCPU',
                                          'Vortex86 SoC');

  CPUVendors :array[0..13] of string = ('Intel',
                                        'UMC',
                                        'AMD',
                                        'Cyrix',
                                        'NexGen',
                                        'CentaurHauls',
                                        'Rise Technology',
                                        'SiS',
                                        'Transmeta',
                                        'National Semiconductor',
                                        'VIA',
                                        'AMD',
                                        'Transmeta',
                                        'Vortex');

function TCPU.CPUIDExists: boolean; register;
asm
	PUSHFD			        //direct access to flags no possible, only via stack
	POP     EAX		      //flags to EAX
	MOV     EDX,EAX		  //save current flags
	XOR     EAX,ID_BIT	//not ID bit
	PUSH    EAX		      //onto stack
	POPFD			          //from stack to flags, with not ID bit
	PUSHFD			        //back to stack
	POP     EAX		      //get back to EAX
	XOR     EAX,EDX		  //check if ID bit affected
	JZ      @exit		    //no, CPUID not availavle
	MOV     AL,True		  //Result=True
@exit:
end;

procedure TCPU.GetCPUID; assembler;
asm
	PUSH    EAX               //Save reg.
	PUSH    EBX
	PUSH    EDX
	PUSH    ECX
	MOV     EAX,1
	DW      $A20F             //CPUID Command Execute
	PUSH    eax
	MOV     VStepping,al      //Store Stepping
	AND     VStepping,0fh     //Stepping mask
	AND     al,0f0h           //Model mask
	SHR     al,4              //Model shift
	MOV     VModel,al         //Store Model
	AND     ax,0f00h          //Family mask
	SHR     ax,8              //Family shift
	MOV     VFamily,al        //Store Family
	AND     al,0f0h;          //Type mask
	SHR     al,4              //type shift
	MOV     VTyp,al           //Store Type
	POP     eax
	MOV     VFeatures,edx     //Store features
	POP     ECX
	POP     EDX
	POP     EBX
	POP     EAX               //Restore Reg.
end;

function TCPU.GetCPUIDLevel: integer;
begin
  VLevel:=0;
  asm
	MOV eax, 0      //  Get Level
	DB 0Fh,0a2h     //  CPUID opcode
	MOV VLevel,al
	//RET
  end;
  result:=VLevel;
End;

function TCPU.GetCPUType: integer; assembler
asm
	PUSH ebx
	PUSH ecx
	PUSH edx

	MOV ebx,esp
	AND esp,0FFFFFFFCh  //align down to nearest dword
	PUSHFD              //save original flags

// i386 CPU check
// The AC bit, bit #18, is a new bit introduced in the EFLAGS
// register on the i486 DX CPU to generate alignment faults.
// This bit can not be set on the i386 CPU.

	PUSHFD
	POP eax
	MOV ecx,eax
	XOR eax,40000h   	//toggle AC bit
	PUSH eax
	POPFD
	PUSHFD
	POP eax
	XOR eax,ecx
	MOV eax,3          //assume 80386
	JE @@end_CPUTyp     //it's a 386

// i486 DX CPU / i487 SX MCP and i486 SX CPU checking
// Checking for ability to set/clear ID flag (Bit 21) in EFLAGS
// which indicates the presence of a processor
// with the ability to use the CPUID instruction.

	PUSHFD
	POP eax
	MOV ecx,eax
	XOR eax,200000h   //toggle ID bit
	PUSH eax
	POPFD
	PUSHFD
	POP eax
	XOR eax, ecx
	MOV eax,4
	JE @@end_CPUTyp   //it's a 486 w/o support for CPUID

// Execute CPUID instruction to determine vendor, family,
// model and stepping.  The use of the CPUID instruction used
// in this program can be used for B0 and later steppings
// of the P5 processor.

	PUSH ebx        // CPUID modifies EBX  !!!
	MOV eax, 1     	// Level
	DB 0Fh,0a2h     // CPUID opcode
	MOV al,ah
	AND eax, 0FH
	POP ebx

@@end_CPUTyp:
	POPFD            // restore original flags
	MOV esp,ebx      // restore original ESP
	POP edx
	POP ecx
	POP ebx
end;

function TCPU.GetCPUVendor :string;
var
  i :integer;
  s :TStrBuf;

  function _GetCPUVendor :TStrBuf; assembler; register;
  asm
  	PUSH    ebx	    //Save regs
	PUSH    edi
	MOV     edi,eax		//@Result (TStrBuf)
	MOV     eax,0
	DW      $A20F		  //CPUID Command
	MOV     eax,ebx
	XCHG	ebx,ecx     //save ECX result
	MOV	ecx,4
  @1:
	STOSB            	//save first 4 byte
	SHR     eax,8
	LOOP    @1
	MOV     eax,edx
	MOV	ecx,4
  @2:
	STOSB            	//save middle 4 byte
	SHR     eax,8
	LOOP    @2
	MOV     eax,ebx
	MOV	ecx,4
  @3:            		//save last 4 byte
	STOSB
	SHR     eax,8
	LOOP    @3
	POP     edi		    //Restore regs
	POP     ebx
  end;

begin
  Log(LOG_VERBOSE, 'Getting CPU vendor information...');
  i:=0;
  result:='';
  s:=_GetCPUVendor;
  repeat
    result:=result+s[i];
    inc(i);
  until i>11;
  FVendorNo:=-1;
  for i:=low(CPUVendorIDs) to high(CPUVendorIDs) do
  begin
    if result=CPUVendorIDs[i] then
    begin
      result:=CPUVendors[i];
      FVendorNo:=i;
      break;
    end;
  end;
end;

function TCPU.GetCPUVendorID :string;
begin
  case Family of
     4: case FVendorNo Of
          0: case Model of
               0: result:='i80486DX-25/33';
               1: result:='i80486DX-50';
               2: result:='i80486SX';
               3: result:='i80486DX2';
               4: result:='i80486SL';
               5: result:='i80486SX2';
               7: result:='i80486DX2WB';
               8: result:='i80486DX4';
               9: result:='i80486DX4WB';
             end;
          1: case Model of
               1: result:='U5D(486DX)';
               2: result:='U5S(486SX)';
             end;
          2: case Model of
               3: result:='80486DX2WT';
               7: result:='80486DX2WB';
               8: result:='80486DX4';
               9: result:='80486DX4WB';
              14: result:='5x86';
              15: result:='5x86WB';
             end;
          3: case Model of
               4: result:='Cyrix Media GX';
               9: result:='Cyrix 5x86';
             end;
        end;
     5: case FVendorNo of
          0: case Model of
               0: result:='P5 A-step';
               1: result:='P5';
               2: result:='P54C';
               3: result:='P24T OverDrive';
               4: result:='P55C';
               5: result:='DX4 OverDrive?';
               6: result:='P5 OverDrive?';
               7: result:='P54C';
               8: result:='P55C(0,25µm)MMX';
             end;
          2: case Model of
               0: result:='SSA5';
               1: result:='5k86';
               2: result:='5k86';
               3: result:='5k86';
               6: result:='K6';
               7: result:='K6';
               8: result:='K6-3D';
               9: result:='K6PLUS-3D';
             end;
          3: case Model of
               0: result:='Pentium Cx6X86 GXm';
               2: result:='Std. Cx6x86';
               4: result:='Cx6x86 GXm';
             end;
          else
             if FVendorNo=4 then
               result:='Nx586';
             if FVendorNo=5 then
               result:='IDT C6 (WinChip)';
          end;
     6: case FVendorNo of
          0: case Model of
               0: result:='PentiumPro A-step';
               1: result:='Pentium Pro';
               3: result:='Pentium II';
               4: result:='P55CT (P54 overdrive)';
               5: result:='Pentium II 0,25µm';
               6: result:='Celeron, model 06';
               7: result:='Pentium III, model 07';
               8: result:='Pentium/Celeron III, model 08';
               9: result:='Pentium M/Celeron M, model 09';
               10: result:='Pentium Xeon III, model 0Ah';
               11: result:='Pentium III, model 0Bh';
               13: result:='Pentium M/Celeron M, model 0Dh';
               14: result:='Intel Core';
               15: result:='Intel Core 2';
             end;
          2: case Model of
               6: result:='K6';
               7: result:='K6';
               8: result:='K6-3D';
               9: result:='K6PLUS-3D';
             end;
          3: if Model=0 then
               result:='Cx6x86 MX/MII';
        end;
  end;
end;

function GetTimeStampHi: DWORD; assembler; register;
asm
  DW      $310F        //RDTSC Command
  MOV @Result, EDX;
end;

function GetTimeStampLo: DWORD; assembler; register;
asm
	DW      $310F       //RDTSC Command
	MOV @Result, EAX;
end;

function GetCPUIDFlags: DWORD; assembler; register;
asm
	PUSH    EBX	   	//Save registers
	PUSH    EDI
	MOV     EAX,1 	        //Set up for CPUID
	DW      $A20F 	        //CPUID OpCode
	MOV @Result,EDX	        //Put the flag array into a DWord
	POP     EDI	       	//Restore registers
	POP     EBX
end;

function GetTimeStamp :TLargInt;
begin
  result.QuadPart:=0;
  if (GetCPUIDFlags and 16) <> 16 then
    exit;
  result.HighPart:=DWORD(GetTimeStampHi);
  result.LowPart:=GetTimeStampLo;
end;

function GetTicksPerSecond(Iterations :Word) :Comp;
var
  Freq ,PerfCount,Target :int64;
  StartTime, EndTime, Elapsed :TLargInt;

  procedure StartTimer;
  begin
    StartTime:=GetTimeStamp;
    EndTime.QuadPart:=0;
    Elapsed.QuadPart:=0;
  end;

  procedure StopTimer;
  begin
    EndTime:=GetTimeStamp;
    Elapsed.QuadPart:=(EndTime.QuadPart-StartTime.QuadPart);
  end;

begin
  Result:=0;
  if not QueryPerformanceFrequency(Freq) then
    exit;
  QueryPerformanceCounter(PerfCount);
  {$IFDEF Delphi4orNewerCompiler}
  Target:=PerfCount+(Freq*Iterations);
  {$ELSE}
  Target.QuadPart:=PerfCount.QuadPart+(Freq.QuadPart*Iterations);
  {$ENDIF}
  StartTimer;
  repeat
    QueryPerformanceCounter(PerfCount);
  {$IFDEF Delphi4orNewerCompiler}
  until (PerfCount>=Target);
  {$ELSE}
  until (PerfCount.QuadPart>=Target.QuadPart);
  {$ENDIF}
  StopTimer;
  Result:=(Elapsed.QuadPart/Iterations);
end;

function TCPU.GetCPUFreqEx: extended;
var
  c :comp;
begin
  c:=GetTicksPerSecond(1);
  Result:=c/1E6;
end;

function TCPU.GetSubModel :string;
begin
  case VTyp of
    3: result:='Reserved';
    2: result:='Secondary';
    1: result:='OverDrive';
    0: result:='Primary';
    else
      result:='Not Detected!';
  end;
end;

procedure TCPU.GetInfo;
var
  SI :TSystemInfo;
begin
  Log(LOG_VERBOSE, 'Starting gathering CPU information...');
  ZeroMemory(@SI,SizeOf(SI));
  GetSystemInfo(SI);
  Count:=SI.dwNumberOfProcessors;
  Family:=SI.dwProcessorType;
//  Vendor:=
//  VendorID:=
  CPUID:=CPUIDExists;
  if CPUID then
  begin
    Level:=GetCPUIDLevel;
    Freq:=Round(GetCPUFreqEx);
    Typ:=GetCPUType;
    GetCPUID;
    Typ:=VTyp;
    Family:=VFamily;
    Model:=VModel;
    Stepping:=VStepping;
    Vendor:=GetCPUVendor;
    VendorID:=GetCPUVendorID;
    SubModel:=GetSubModel;
  end;
end;

procedure TCPU.Report(var sl: TStringList);
begin
  with sl do
  begin
    add(format('%d x %s %s - %d MHz',[self.Count,Vendor,VendorID,Freq]));
    add(format('Submodel: %s',[Submodel]));
    add(format('Model ID: Family %d  Model %d  Stepping %d  Level %d',[Family,Model,Stepping,Level]));
  end;
end;

{ TOperatingSystem }

constructor TOperatingSystem.Create;
begin
  inherited;
  FEnv:=TStringList.Create;
  FDirs:=TStringList.Create;
end;

destructor TOperatingSystem.Destroy;
begin
  FEnv.Free;
  FDirs.Free;
  inherited;
end;

procedure TOperatingSystem.GetEnvironment;
var
  b :pchar;
  s :pchar;
begin
  Log(LOG_VERBOSE, 'Gathering environment information...');
  FEnv.Clear;
  b:=GetEnvironmentStrings;
  try
    s:=b;
    while True do
    begin
      if StrLen(s)=0 then
        break;
      FEnv.Add(s);
      inc(s, StrLen(s) + 1);
    end;
  finally
    FreeEnvironmentStrings(b);
  end;
end;

function GetSpecialFolder(Handle: Hwnd; nFolder: Integer): string;
var
  PIDL: PItemIDList;
  Path: LPSTR;
begin
  Result:='';
  Path:=StrAlloc(MAX_PATH);
  try
    SHGetSpecialFolderLocation(Handle, nFolder, PIDL);

    if SHGetPathFromIDList(PIDL, Path) then
      Result:=StrPas(Path);

  finally
    StrDispose(Path);
  end;
end;

procedure TOperatingSystem.GetInfo;
var
  OS: TOSVersionInfoEx;
  bIsWow64: BOOL;
  Wow64Ptr: Pointer;
  p: pchar;
  n: DWORD;
  WinH: HWND;
  s: string;
  rkOSInfo: string;
  rvVersionName: string;
const
  rkOSInfo95 = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows\CurrentVersion';
  rkOSInfoNT = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
  rvVersionName95 = 'Version';
  rvVersionNameNT = 'CurrentType';
  rvRegOrg = 'RegisteredOrganization';
  rvRegOwn = 'RegisteredOwner';
  rvProductID = 'ProductID';

  cUserProfile = 'USERPROFILE';
  cUserProfileReg = {HKEY_CURRENT_USER\}'SOFTWARE\Microsoft\Windows\CurrentVersion\ProfileList';
  cUserProfileRec = {HKEY_CURRENT_USER\}'SOFTWARE\Microsoft\Windows\CurrentVersion\ProfileReconciliation';
  cProfileDir = 'ProfileDirectory';
begin
  Log(LOG_VERBOSE, 'Starting gathering OS information...');
  FDirs.Clear;
  Extended:=False;
  ZeroMemory(@OS,SizeOf(OS));
  OS.dwOSVersionInfoSize:=SizeOf(TOSVersionInfo);
  if not GetVersionEx(POSVersionInfo(@OS)^) then
    raise exception.create('Unable to retrieve system details. Call to GetVersionEx failed!');
  if (OS.dwPlatformId = VER_PLATFORM_WIN32_NT) and (OS.dwMajorVersion > 4) then
  begin
    // This platform supports TOSVersionInfoEx
    ZeroMemory(@OS,SizeOf(OS));
    OS.dwOSVersionInfoSize:=SizeOf(TOSVersionInfoEx);
    if not GetVersionEx(POSVersionInfo(@OS)^) then
      raise exception.create('Unable to retrieve system details. Call to GetVersionEx failed!');
    Extended:=True;
  end;
  MajorVersion:=OS.dwMajorVersion;
  MinorVersion:=OS.dwMinorVersion;
  BuildNumber:=OS.dwBuildNumber;
  if Extended then
  begin
    ServicePackMajor:=OS.wServicePackMajor;
    ServicePackMinor:=OS.wServicePackMinor;
    SuiteMask:=OS.wSuiteMask;
    ProductType:=OS.wProductType;
  end
  else
  begin
    ServicePackMajor:=0;
    ServicePackMinor:=0;
    SuiteMask:=0;
    ProductType:=0;
    //See: http://msdn.microsoft.com/en-us/library/ms724833.aspx
  end;
  Wow64Ptr := GetProcAddress(GetModuleHandle('kernel32'),'IsWow64Process');
  if Wow64Ptr <> nil then
  begin
    IsWow64Process := Wow64Ptr;
    bIsWow64 := FALSE;
    if IsWow64Process(GetCurrentProcess(), bIsWow64) = false then
      //FIXME: Even though we probably should raise an error, let's just play it safe...
      Wow64:=False
    else
      if bIsWow64 then
        Wow64:=True
      else
        Wow64:=False;
  end
  else
    Wow64:=False;
  case OS.dwPlatformId of
    VER_PLATFORM_WIN32s:
     begin
      Platform:='Windows 32s';
      WindowsPlatformCompatibility:=osWin95Comp;
      WindowsPlatform:=osWin95;
     end;
    VER_PLATFORM_WIN32_WINDOWS:
      case MajorVersion of
      4:
       begin
        case MinorVersion of
        0:
         begin
          Platform:='Windows 95';
          WindowsPlatform:=osWin95;
         end;
        10:
         begin
          Platform:='Windows 98';
          WindowsPlatform:=osWin98;
         end;
        90:
         begin
          Platform:='Windows ME';
          WindowsPlatform:=osWinME;
         end;
        else
         begin
          Platform:='Unknown (Probably OK)';
          WindowsPlatform:=osWin95;
         end;
        end;
        WindowsPlatformCompatibility:=osWin95Comp;
       end;
      else
       begin
        if MajorVersion>4 then
        begin
          Platform:='Unknown (Probably OK)';
          WindowsPlatform:=osWinME;
        end
        else
        begin
          Platform:='Unknown';
          WindowsPlatform:=osWin95;
        end;
       end;
       WindowsPlatformCompatibility:=osWin95Comp;
      end;
    VER_PLATFORM_WIN32_NT:
      case MajorVersion of
      (*3:
       begin
        Platform:='Windows NT 3.51';
        WindowsPlatform:=osWinNT351;
        WindowsPlatformCompatibility:=osWinNTComp;
       end;*)
      4:
       begin
        case MinorVersion of
        0:
         begin
          Platform:='Windows NT4';
          WindowsPlatform:=osWinNT4;
         end;
        else
         begin
          Platform:='Unknown (Probably OK)';
          WindowsPlatform:=osWinNT4;
         end;
        end;
        WindowsPlatformCompatibility:=osWinNTComp;
       end;
      5:
       begin
        case MinorVersion of
        0:
         begin
          Platform:='Windows 2000';
          WindowsPlatform:=osWin2000;
         end;
        1:
         begin
          Platform:='Windows XP';
          WindowsPlatform:=osWinXP;
         end;
        2:
         begin
          Platform:='Windows Server 2003 or Windows XP 64-bit';
          WindowsPlatform:=osWin2003;
         end;
        else
         begin
          Platform:='Unknown (Probably OK)';
          WindowsPlatform:=osWin2000;
         end;
        end;
        WindowsPlatformCompatibility:=osWinNTComp;
       end;
      6:
       begin
        case MinorVersion of
        0:
         begin
          Platform:='Windows Vista or Windows Server 2008';
          WindowsPlatform:=osWinVista;
         end;
        1:
         begin
          Platform:='Windows 7 or Windows Server 2008 R2';
          WindowsPlatform:=osWin7;
         end;
        2:
         begin
          Platform:='Windows 8 or Windows Server 2012';
          WindowsPlatform:=osWin8;
         end;
        3:
         begin
          Platform:='Windows 8.1 or Windows Server 2012 R2';
          WindowsPlatform:=osWin81;
         end;
        else
         begin
          Platform:='Unknown (Probably OK)';
          WindowsPlatform:=osWinVista;
         end;
        end;
        WindowsPlatformCompatibility:=osWinNTComp;
       end;
      else
       begin
        if MajorVersion>6 then
        begin
          Platform:='Unknown (Probably OK)';
          WindowsPlatform:=osWin81;
          WindowsPlatformCompatibility:=osWinNTComp;
        end
        else
        begin
          Platform:='Unknown';
          WindowsPlatform:=osWin95;
          WindowsPlatformCompatibility:=osWin95Comp;
        end;
       end;
      end;
    else
      raise exception.create('Unknown Windows platform detected!');
  end;
  case WindowsPlatformCompatibility of
  osWin95Comp:
   begin
    rkOSInfo:=rkOSInfo95;
    rvVersionName:=rvVersionName95;
   end;
  osWinNTComp:
   begin
    rkOSInfo:=rkOSInfoNT;
    rvVersionName:=rvVersionNameNT;
   end;
  end;
  CSD:=strpas(OS.szCSDVersion);
  Version:='';
  RegisteredUser:='';
  RegisteredOrg:='';
  SerialNumber:='';
  with TRegistry.create(KEY_READ) do
  begin
    rootkey:=HKEY_LOCAL_MACHINE;
    if OpenKey(rkOSInfo,false) then
    begin
      if ValueExists(rvVersionName) then
        Version:=ReadString(rvVersionName);
      if ValueExists(rvRegOrg) then
        RegisteredOrg:=ReadString(rvRegOrg);
      if ValueExists(rvRegOwn) then
        RegisteredUser:=ReadString(rvRegOwn);
      if ValueExists(rvProductID) then
        SerialNumber:=ReadString(rvProductID);
      FDirs.Add('CommonFiles='  +ReadString('CommonFilesDir'));
      FDirs.Add('ProgramFiles=' +ReadString('ProgramFilesDir'));
      FDirs.Add('Device='       +ReadString('DevicePath'));
      FDirs.Add('OtherDevice='  +ReadString('OtherDevicePath'));
      FDirs.Add('Media='        +ReadString('MediaPath'));
      FDirs.Add('Config='       +ReadString('ConfigPath'));
      FDirs.Add('Wallpaper='    +ReadString('WallPaperDir'));
      CloseKey;
    end;
    Free;
  end;

  n:=MAX_PATH;
  p:=StrAlloc(n);
  try
    GetWindowsDirectory(p,n);
    FDirs.Add('Windows='+StrPas(p));

    GetSystemDirectory(p,n);
    FDirs.Add('System='+StrPas(p));

    GetTempPath(n,p);
    FDirs.Add('Temp='+StrPas(p));
  finally
    StrDispose(p);
  end;

  WinH:=GetDesktopWindow;
  FDirs.Add('AppData='          +GetSpecialFolder(WinH,CSIDL_APPDATA));
  FDirs.Add('CommonDesktopDir=' +GetSpecialFolder(WinH,CSIDL_COMMON_DESKTOPDIRECTORY));
  FDirs.Add('CommonAltStartUp=' +GetSpecialFolder(WinH,CSIDL_COMMON_ALTSTARTUP));
  FDirs.Add('RecycleBin='       +GetSpecialFolder(WinH,CSIDL_BITBUCKET));
  FDirs.Add('CommonPrograms='   +GetSpecialFolder(WinH,CSIDL_COMMON_PROGRAMS));
  FDirs.Add('CommonStartMenu='  +GetSpecialFolder(WinH,CSIDL_COMMON_STARTMENU));
  FDirs.Add('CommonStartup='    +GetSpecialFolder(WinH,CSIDL_COMMON_STARTUP));
  FDirs.Add('CommonFavorites='  +GetSpecialFolder(WinH,CSIDL_COMMON_FAVORITES));
  FDirs.Add('Cookies='          +GetSpecialFolder(WinH,CSIDL_COOKIES));
  FDirs.Add('Controls='         +GetSpecialFolder(WinH,CSIDL_CONTROLS));
  FDirs.Add('Desktop='          +GetSpecialFolder(WinH,CSIDL_DESKTOP));
  FDirs.Add('DesktopDir='       +GetSpecialFolder(WinH,CSIDL_DESKTOPDIRECTORY));
  FDirs.Add('Favorites='        +GetSpecialFolder(WinH,CSIDL_FAVORITES));
  FDirs.Add('Drives='           +GetSpecialFolder(WinH,CSIDL_DRIVES));
  FDirs.Add('Fonts='            +GetSpecialFolder(WinH,CSIDL_FONTS));
  FDirs.Add('History='          +GetSpecialFolder(WinH,CSIDL_HISTORY));
  FDirs.Add('Internet='         +GetSpecialFolder(WinH,CSIDL_INTERNET));
  FDirs.Add('InternetCache='    +GetSpecialFolder(WinH,CSIDL_INTERNET_CACHE));
  FDirs.Add('NetWork='          +GetSpecialFolder(WinH,CSIDL_NETWORK));
  FDirs.Add('NetHood='          +GetSpecialFolder(WinH,CSIDL_NETHOOD));
  FDirs.Add('MyDocuments='      +GetSpecialFolder(WinH,CSIDL_PERSONAL));
  FDirs.Add('PrintHood='        +GetSpecialFolder(WinH,CSIDL_PRINTHOOD));
  FDirs.Add('Printers='         +GetSpecialFolder(WinH,CSIDL_PRINTERS));
  FDirs.Add('Programs='         +GetSpecialFolder(WinH,CSIDL_PROGRAMS));
  FDirs.Add('Recent='           +GetSpecialFolder(WinH,CSIDL_RECENT));
  FDirs.Add('SendTo='           +GetSpecialFolder(WinH,CSIDL_SENDTO));
  FDirs.Add('StartMenu='        +GetSpecialFolder(WinH,CSIDL_STARTMENU));
  FDirs.Add('StartUp='          +GetSpecialFolder(WinH,CSIDL_STARTUP));
  FDirs.Add('Templates='        +GetSpecialFolder(WinH,CSIDL_TEMPLATES));
  s:=ReverseString(FDirs.Values['Desktop']);
  s:=ReverseString(Copy(s,Pos('\',s)+1,255));
  FDirs.Add('Profile='+s);
  GetEnvironment;
end;

procedure TOperatingSystem.Report(var sl: TStringList);
begin
  with sl do
  begin
    add('Platform: '+Platform);
    if Length(Version)<>0 then
     add(format('Version: %s %d.%d.%d',[Version,MajorVersion,MinorVersion,BuildNumber]))
    else
     add(format('Version: %d.%d.%d',[MajorVersion,MinorVersion,BuildNumber]));
    if Extended then
    begin
      if ServicePackMinor<>0 then
        add(format('Service Pack: %d.%d',[ServicePackMajor,ServicePackMinor]))
      else
        add(format('Service Pack: %d',[ServicePackMajor]));
      //FIXME: Handle SuiteMask!
      case ProductType of
        0: ; //Nothing set
        VER_NT_WORKSTATION: add('Type: Workstation');
        VER_NT_DOMAIN_CONTROLLER: add('Type: Domain Controller');
        VER_NT_SERVER: add('Type: Server');
        else
          add('Type: Unknown');
      end;
    end;
    if Wow64 then
      add('Running under Wow64');
  end;
end;

function TMemory.GetGDIRes: Byte;
begin
  {$IFDEF ONLYWIN9X}
  Result:=GetFreeSysRes(cGDI)
  {$ELSE}
  Result:=0;
  {$ENDIF}
end;

function TMemory.GetSystemRes: Byte;
begin
  {$IFDEF ONLYWIN9X}
  Result:=GetFreeSysRes(cSystem)
  {$ELSE}
  Result:=0;
  {$ENDIF}
end;

function TMemory.GetUSERRes: Byte;
begin
  {$IFDEF ONLYWIN9X}
  Result:=GetFreeSysRes(cUser)
  {$ELSE}
  Result:=0;
  {$ENDIF}
end;

procedure TMemory.GetInfo;
var
  SI :TSystemInfo;
  MS :TMemoryStatus;
begin
  Log(LOG_VERBOSE, 'Starting gathering memory information...');
  ZeroMemory(@MS,SizeOf(MS));
  MS.dwLength:=SizeOf(MS);
  GlobalMemoryStatus(MS);
  MemoryLoad:=MS.dwMemoryLoad;
  PhysicalTotal:=MS.dwTotalPhys;
  PhysicalFree:=MS.dwAvailPhys;
  VirtualTotal:=MS.dwTotalVirtual;
  VirtualFree:=MS.dwAvailVirtual;
  PageFileTotal:=MS.dwTotalPageFile;
  PageFileFree:=MS.dwAvailPageFile;
  ZeroMemory(@SI,SizeOf(SI));
  GetSystemInfo(SI);
  AllocGranularity:=SI.dwAllocationGranularity;
  MaxAppAddress:=Integer(SI.lpMaximumApplicationAddress);
  MinAppAddress:=Integer(SI.lpMinimumApplicationAddress);
  PageSize:=SI.dwPageSize;
  FSystemRes:=GetSystemRes;
  FGDIRes:=GetGDIRes;
  FUserRes:=GetUserRes;
end;

procedure TMemory.Report(var sl: TStringList);
begin
  with sl do
  begin
    add('Physical Memory Total: '+FormatBytes(PhysicalTotal)+' Bytes');
    add('Physical Memory Free: '+FormatBytes(PhysicalFree)+' Bytes');
    add('Virtual Memory Free: '+FormatBytes(VirtualFree)+' Bytes');
  end;
end;

function TWorkstation.GetSystemUpTime: Extended;
begin
  try
    FSystemUpTime:=GetTickCount/1000;//GetTimeStamp.QuadPart/GetTicksPerSecond(1);
  except
    FSystemUpTime:=0;
  end;
  result:=FSystemUpTime;
end;

function GetMachine: string;
var
  n: dword;
  buf: string;
const
  rkMachine = {HKEY_LOCAL_MACHINE\}'SYSTEM\CurrentControlSet\Control\ComputerName\ComputerName';
  rvMachine = 'ComputerName';
begin
  Log(LOG_VERBOSE, 'Gathering machine information...');
  n:=254;
  SetLength(buf,n);
  GetComputerName(PChar(buf),n);
  SetLength(buf,n-1);
  result:=buf;
  with TRegistry.Create(KEY_READ) do
  begin
    rootkey:=HKEY_LOCAL_MACHINE;
    if OpenKey(rkMachine,false) then
    begin
      if ValueExists(rvMachine) then
        result:=ReadString(rvMachine);
      CloseKey;
    end;
    free;
  end;
end;

function GetUser: string;
var
  n: dword;
  buf: string;
begin
  Log(LOG_VERBOSE, 'Gathering user information...');
  n:=254;
  SetLength(buf,n);
  GetUserName(PChar(buf),n);
  SetLength(buf,n-1);
  result:=buf;
end;

procedure TWorkstation.GetInfo;
var
  bdata: pchar;
  KeyState: TKeyBoardState;
const
  bdatasize = 255;

  cBIOSName = $FE061;
  cBIOSDate = $FFFF5;
  cBIOSExtInfo = $FEC71;
  cBIOSCopyright = $FE091;

  rkBIOS = {HKEY_LOCAL_MACHINE\}'HARDWARE\DESCRIPTION\System';
  rvBiosDate = 'SystemBiosDate';
  rvBiosID = 'Identifier';
  rvBiosVersion = 'SystemBiosVersion';
  // Video BIOS info is available only under NT
  //rvVideoBiosDate = 'VideoBiosDate';
  //rvVideoBiosVersion = 'VideoBiosVersion';
begin
  Log(LOG_VERBOSE, 'Starting gathering workstation information...');
  FSystemUpTime:=GetSystemUpTime;
  FName:=GetMachine;
  FUser:=GetUser;
  if WindowsPlatformCompatibility=osWinNTComp then
  begin
    with TRegistry.Create(KEY_READ) do
    begin
      rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkBIOS,false) then
      begin
        if ValueExists(rvBIOSID) then
          FBiosName:=ReadString(rvBIOSID);
        if ValueExists(rvBIOSVersion) then
        begin
          bdata:=stralloc(bdatasize + 1); //Note: One larger for null-terminator
          try
            FillChar(bdata^,bdatasize+1,0);
            readbinarydata(rvBIOSVersion,bdata^,bdatasize);
            FBIOSCopyright:=strpas(pchar(bdata));
          except
          end;
          strdispose(bdata);
        end;
        if ValueExists(rvBIOSDate) then
          FBIOSDate:=ReadString(rvBIOSDate);
        CloseKey;
      end;
      free;
    end;
  end
  else
  begin
    FBIOSName:=string(pchar(ptr(cBIOSName)));
    FBIOSDate:=string(pchar(ptr(cBIOSDate)));
    FBIOSCopyright:=string(pchar(ptr(cBIOSCopyright)));
    FBIOSExtendedInfo:=string(pchar(ptr(cBIOSExtInfo)));
  end;
  GetKeyboardState(KeyState);
  //FCapsLock:=KeyState[VK_CAPITAL]=1;
  //FNumLock:=KeyState[VK_NUMLOCK]=1;
  //FScrollLock:=KeyState[VK_SCROLL]=1;
end;

procedure TWorkstation.Report(var sl: TStringList);
begin
  with sl do
  begin
    add('Name: '+Name);
    add('User: '+User);
//    add('System Up Time: '+formatseconds(SystemUpTime,true,false,false));
  end;
end;

function GetClassDevices(const AStartKey,AClassName,AValueName :string; var AResult :TStrings) :string; //@CONST!
var
  i,j :integer;
  sl :TStringList;
  s,rclass :string;
const
  rvClass = 'Class';
  rvLink = 'Link';
begin
  Result:='';
  AResult.Clear;
  with TRegistry.Create(KEY_READ) do
  begin
    RootKey:=HKEY_LOCAL_MACHINE;
    if OpenKey(AStartKey,false) then
    begin
      sl:=TStringList.Create;
      try
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do
        begin
          if OpenKey(AStartKey+'\'+sl[i],false) then
          begin
            if ValueExists(rvClass) then
            begin
              rclass:=UpperCase(ReadString(rvClass));
              if rclass=UpperCase(AClassName) then
              begin
                if WindowsPlatformCompatibility=osWin95Comp then
                begin
                  s:=UpperCase(ReadString(rvLink));
                  CloseKey;
                  if not OpenKey(AStartKey+'\'+s,False) then
                    Exit;
                end
                else
                  s:=sl[i];
                Result:=AStartKey+'\'+s;
                GetKeyNames(sl);
                CloseKey;
                for j:=0 to sl.count-1 do
                begin
                  if OpenKey(AStartKey+'\'+s+'\'+sl[j],false) then
                  begin
                    if ValueExists(AValueName) then
                      AResult.Add(ReadString(AValueName));
                    CloseKey;
                  end;
                end;
                Break;
              end;
            end;
            CloseKey;
          end;
        end;
      finally
        sl.free;
      end;
    end;
    free;
  end;
end;

function GetStrFromBuf(Buffer :pchar) :string;
var
  i,j :integer;
begin
  result:='';
  j:=0;
  i:=0;
  repeat
    if buffer[i]<>#0 then
    begin
      result:=result+buffer[i];
      j:=0;
    end
    else
      inc(j);
    inc(i);
  until j>1;
end;

procedure TDisplay.GetInfo;
var
  rk :string;
  bdata :pchar;
  idata :DWORD;
  sl :tstringlist;
  i :integer;
  j :DWORD;
  DevMode :TDevMode;
  MaxDev :DWORD;
  Found: Boolean;
  l_hdc: HDC;
  ClassKey: string;
const
  bdatasize = 255;

  rkVideoHardware = {HKEY_LOCAL_MACHINE\}'HARDWARE\DEVICEMAP\VIDEO';
  rvVideoKey1 = '\Device\Video';
  rvVideoKey2 = '\\Device\\Video';
  rvHardware = 'HardwareInformation';
  rvHWVideo = 'AdapterString';
  //rvHWBios = 'BiosString';
  rvHWChip = 'ChipType';
  rvHWDAC = 'DacType';
  rvHWMem = 'MemorySize';
  rvProvider = 'ProviderName';
  rvDriverDate = 'DriverDate';
  rvDriverVersion = 'DriverVersion';

  rvVideoClass = 'Display';

  rkClassInfo = 'INFO';
  rvCIVideo = 'DriverDesc';
  rvCIDAC = 'DACType';
  rvCIChip = 'ChipType';
  rvCIMem = 'VideoMemory';
  rvCIRev = 'Revision';

  rv3DClass = '3D Accelerators';

  DescValue = 'DriverDesc';
begin
  Log(LOG_VERBOSE, 'Starting gathering of DISPLAY system information...');
  l_hdc := GetDC(0);
  if l_hdc = 0 then
    raise exception.Create('Unable to get DC of entire screen');
  try

    FHorzRes:=GetDeviceCaps(l_hdc,windows.HORZRES);
    FVertRes:=GetDeviceCaps(l_hdc,windows.VERTRES);
    FColorDepth:=GetDeviceCaps(l_hdc,BITSPIXEL);
   
    case GetDeviceCaps(l_hdc,windows.TECHNOLOGY) of
      DT_PLOTTER:    FTechnology:='Vector Plotter';
      DT_RASDISPLAY: FTechnology:='Raster Display';
      DT_RASPRINTER: FTechnology:='Raster Printer';
      DT_RASCAMERA:  FTechnology:='Raster Camera';
      DT_CHARSTREAM: FTechnology:='Character Stream';
      DT_METAFILE:   FTechnology:='Metafile';
      DT_DISPFILE:   FTechnology:='Display File';
    end;
   
    FHorzSize:=GetDeviceCaps(l_hdc,HORZSIZE);
    FVertSize:=GetDeviceCaps(l_hdc,VERTSIZE);
    FPixelWidth:=GetDeviceCaps(l_hdc,ASPECTX);
    FPixelHeight:=GetDeviceCaps(l_hdc,ASPECTY);
    FPixelDiagonal:=GetDeviceCaps(l_hdc,ASPECTXY);
   
    FCurveCaps:=[];
    if GetDeviceCaps(l_hdc,windows.CURVECAPS)<>CC_NONE then
    begin
      if (GetDeviceCaps(l_hdc,windows.CURVECAPS) and CC_CIRCLES)=CC_CIRCLES then
        FCurveCaps:=FCurveCaps+[ccCircles];
      if (GetDeviceCaps(l_hdc,windows.CURVECAPS) and CC_PIE)=CC_PIE then
        FCurveCaps:=FCurveCaps+[ccPieWedges];
      if (GetDeviceCaps(l_hdc,windows.CURVECAPS) and CC_CHORD)=CC_CHORD then
        FCurveCaps:=FCurveCaps+[ccChords];
      if (GetDeviceCaps(l_hdc,windows.CURVECAPS) and CC_ELLIPSES)=CC_ELLIPSES then
        FCurveCaps:=FCurveCaps+[ccEllipses];
      if (GetDeviceCaps(l_hdc,windows.CURVECAPS) and CC_WIDE)=CC_WIDE then
        FCurveCaps:=FCurveCaps+[ccWideBorders];
      if (GetDeviceCaps(l_hdc,windows.CURVECAPS) and CC_STYLED)=CC_STYLED then
        FCurveCaps:=FCurveCaps+[ccStyledBorders];
      if (GetDeviceCaps(l_hdc,windows.CURVECAPS) and CC_WIDESTYLED)=CC_WIDESTYLED then
        FCurveCaps:=FCurveCaps+[ccWideStyledBorders];
      if (GetDeviceCaps(l_hdc,windows.CURVECAPS) and CC_INTERIORS)=CC_INTERIORS then
        FCurveCaps:=FCurveCaps+[ccInteriors];
      if (GetDeviceCaps(l_hdc,windows.CURVECAPS) and CC_ROUNDRECT)=CC_ROUNDRECT then
        FCurveCaps:=FCurveCaps+[ccRoundedRects];
    end;
   
    FLineCaps:=[];
    if GetDeviceCaps(l_hdc,windows.LINECAPS)<>LC_NONE then
    begin
      if (GetDeviceCaps(l_hdc,windows.LINECAPS) and LC_POLYLINE)=LC_POLYLINE then
        FLineCaps:=FLineCaps+[lcPolylines];
      if (GetDeviceCaps(l_hdc,windows.LINECAPS) and LC_MARKER)=LC_MARKER then
        FLineCaps:=FLineCaps+[lcMarkers];
      if (GetDeviceCaps(l_hdc,windows.LINECAPS) and LC_POLYMARKER)=LC_POLYMARKER then
        FLineCaps:=FLineCaps+[lcMultipleMarkers];
      if (GetDeviceCaps(l_hdc,windows.LINECAPS) and LC_WIDE)=LC_WIDE then
        FLineCaps:=FLineCaps+[lcWideLines];
      if (GetDeviceCaps(l_hdc,windows.LINECAPS) and LC_STYLED)=LC_STYLED then
        FLineCaps:=FLineCaps+[lcStyledLines];
      if (GetDeviceCaps(l_hdc,windows.LINECAPS) and LC_WIDESTYLED)=LC_WIDESTYLED then
        FLineCaps:=FLineCaps+[lcWideStyledLines];
      if (GetDeviceCaps(l_hdc,windows.LINECAPS) and LC_INTERIORS)=LC_INTERIORS then
        FLineCaps:=FLineCaps+[lcInteriors];
    end;
   
    FPolygonCaps:=[];
    if GetDeviceCaps(l_hdc,POLYGONALCAPS)<>PC_NONE then
    begin
      if (GetDeviceCaps(l_hdc,POLYGONALCAPS) and PC_POLYGON)=PC_POLYGON then
        FPolygonCaps:=FPolygonCaps+[pcAltFillPolygons];
      if (GetDeviceCaps(l_hdc,POLYGONALCAPS) and PC_RECTANGLE)=PC_RECTANGLE then
        FPolygonCaps:=FPolygonCaps+[pcRectangles];
      if (GetDeviceCaps(l_hdc,POLYGONALCAPS) and PC_WINDPOLYGON)=PC_WINDPOLYGON then
        FPolygonCaps:=FPolygonCaps+[pcWindingFillPolygons];
      if (GetDeviceCaps(l_hdc,POLYGONALCAPS) and PC_SCANLINE)=PC_SCANLINE then
        FPolygonCaps:=FPolygonCaps+[pcSingleScanlines];
      if (GetDeviceCaps(l_hdc,POLYGONALCAPS) and PC_WIDE)=PC_WIDE then
        FPolygonCaps:=FPolygonCaps+[pcWideBorders];
      if (GetDeviceCaps(l_hdc,POLYGONALCAPS) and PC_STYLED)=PC_STYLED then
        FPolygonCaps:=FPolygonCaps+[pcStyledBorders];
      if (GetDeviceCaps(l_hdc,POLYGONALCAPS) and PC_WIDESTYLED)=PC_WIDESTYLED then
        FPolygonCaps:=FPolygonCaps+[pcWideStyledBorders];
      if (GetDeviceCaps(l_hdc,POLYGONALCAPS) and PC_INTERIORS)=PC_INTERIORS then
        FPolygonCaps:=FPolygonCaps+[pcInteriors];
    end;
   
    FRasterCaps:=[];
    if (GetDeviceCaps(l_hdc,windows.RASTERCAPS) and RC_BANDING)=RC_BANDING then
      FRasterCaps:=FRasterCaps+[rcRequiresBanding];
    if (GetDeviceCaps(l_hdc,windows.RASTERCAPS) and RC_BITBLT)=RC_BITBLT then
      FRasterCaps:=FRasterCaps+[rcTranserBitmaps];
    if (GetDeviceCaps(l_hdc,windows.RASTERCAPS) and RC_BITMAP64)=RC_BITMAP64 then
      FRasterCaps:=FRasterCaps+[rcBitmaps64K];
    if (GetDeviceCaps(l_hdc,windows.RASTERCAPS) and RC_DI_BITMAP)=RC_DI_BITMAP then
      FRasterCaps:=FRasterCaps+[rcSetGetDIBits];
    if (GetDeviceCaps(l_hdc,windows.RASTERCAPS) and RC_DIBTODEV)=RC_DIBTODEV then
      FRasterCaps:=FRasterCaps+[rcSetDIBitsToDevice];
    if (GetDeviceCaps(l_hdc,windows.RASTERCAPS) and RC_FLOODFILL)=RC_FLOODFILL then
      FRasterCaps:=FRasterCaps+[rcFloodfills];
    if (GetDeviceCaps(l_hdc,windows.RASTERCAPS) and RC_GDI20_OUTPUT)=RC_GDI20_OUTPUT then
      FRasterCaps:=FRasterCaps+[rcWindows2xFeatures];
    if (GetDeviceCaps(l_hdc,windows.RASTERCAPS) and RC_PALETTE)=RC_PALETTE then
      FRasterCaps:=FRasterCaps+[rcPaletteBased];
    if (GetDeviceCaps(l_hdc,windows.RASTERCAPS) and RC_SCALING)=RC_SCALING then
      FRasterCaps:=FRasterCaps+[rcScaling];
    if (GetDeviceCaps(l_hdc,windows.RASTERCAPS) and RC_STRETCHBLT)=RC_STRETCHBLT then
      FRasterCaps:=FRasterCaps+[rcStretchBlt];
    if (GetDeviceCaps(l_hdc,windows.RASTERCAPS) and RC_STRETCHDIB)=RC_STRETCHDIB then
      FRasterCaps:=FRasterCaps+[rcStretchDIBits];
   
    FTextCaps:=[];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_OP_CHARACTER)=TC_OP_CHARACTER then
      FTextCaps:=FTextCaps+[tcCharOutPrec];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_OP_STROKE)=TC_OP_STROKE then
      FTextCaps:=FTextCaps+[tcStrokeOutPrec];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_CP_STROKE)=TC_CP_STROKE then
      FTextCaps:=FTextCaps+[tcStrokeClipPrec];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_CR_90)=TC_CR_90 then
      FTextCaps:=FTextCaps+[tcCharRotation90];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_CR_ANY)=TC_CR_ANY then
      FTextCaps:=FTextCaps+[tcCharRotationAny];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_SF_X_YINDEP)=TC_SF_X_YINDEP then
      FTextCaps:=FTextCaps+[tcScaleIndependent];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_SA_DOUBLE)=TC_SA_DOUBLE then
      FTextCaps:=FTextCaps+[tcDoubledCharScaling];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_SA_INTEGER)=TC_SA_INTEGER then
      FTextCaps:=FTextCaps+[tcIntMultiScaling];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_SA_CONTIN)=TC_SA_CONTIN then
      FTextCaps:=FTextCaps+[tcAnyMultiExactScaling];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_EA_DOUBLE)=TC_EA_DOUBLE then
      FTextCaps:=FTextCaps+[tcDoubleWeightChars];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_IA_ABLE)=TC_IA_ABLE then
      FTextCaps:=FTextCaps+[tcItalics];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_UA_ABLE)=TC_UA_ABLE then
      FTextCaps:=FTextCaps+[tcUnderlines];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and  TC_SO_ABLE)=TC_SO_ABLE then
      FTextCaps:=FTextCaps+[tcStrikeouts];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_RA_ABLE)=TC_RA_ABLE then
      FTextCaps:=FTextCaps+[tcRasterFonts];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_VA_ABLE)=TC_VA_ABLE then
      FTextCaps:=FTextCaps+[tcVectorFonts];
    if (GetDeviceCaps(l_hdc,windows.TEXTCAPS) and TC_SCROLLBLT)=TC_SCROLLBLT then
      FTextCaps:=FTextCaps+[tcNoScrollUsingBlts];

  finally
    ReleaseDC(0, l_hdc);
  end;

  if WindowsPlatformCompatibility=osWinNTComp then
    ClassKey:='SYSTEM\CurrentControlSet\Control\Class'
  else
    ClassKey:='SYSTEM\CurrentControlSet\Services\Class';

  sl:=tstringlist.create;
  try
    FAdapter.Clear;
    FDevices.Clear;
    FDAC.Clear;
    FChipset.Clear;
    FMemory.Clear;
    FProvider.Clear;
    FDriverDate.Clear;
    FDriverVersion.Clear;
    Log(LOG_VERBOSE, 'Gathering of display driver information...');
    try
      rk:=GetClassDevices(ClassKey,rvVideoClass,DescValue,FDevices);
    except
      on ERegistryException do
      begin
        AMDDriverDescBug:=True;
        rk:='';
      end;
    else
      //Not handled here
      raise;
    end;
    Found:=False;

    //Something went wrong!
    if rk='' then
      Exit;

    Log(LOG_VERBOSE, 'Enumerating of display driver information...');
    bdata:=stralloc(bdatasize+1); //Note: One larger for null-terminator
    with TRegistry.Create(KEY_READ) do
    begin
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rk,false) then
      begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.count-1 do
        begin
          if OpenKey(rk+'\'+sl[i]+'\'+rkClassInfo,false) then
          begin
            Found:=True;
   
            if ValueExists(rvCIDAC) then
              FDAC.Add(ReadString(rvCIDAC))
            else
              FDAC.Add('Unknown');

            if ValueExists(rvCIChip) then
            begin
              FChipset.Add(ReadString(rvCIChip));
              if ValueExists(rvCIRev) then
                FChipset[FChipset.Count-1]:=FChipset[FChipset.Count-1]+' Rev '+ReadString(rvCIRev);
            end
            else
              FChipset.Add('Unknown');
   
            if ValueExists(rvCIMem) then
              FMemory.Add(FormatBytes(readinteger(rvCIMem)))
            else
              FMemory.Add('Unknown');
   
            CloseKey;
          end;
        end;
      end;
   
      if not Found then
      begin
        MaxDev:=0;
        if OpenKey(rkVideoHardware,false) then
        begin
          try
            MaxDev:=readinteger('MaxObjectNumber');
          except
            MaxDev:=0;
          end;
          CloseKey;
        end;

        for i:=0 to MaxDev do
        begin
          if OpenKey(rkVideoHardware,false) then
          begin
            if ValueExists(rvVideoKey1+IntToStr(i)) then
              rk:=ReadString(rvVideoKey1+IntToStr(i))
            else
            begin
              if ValueExists(rvVideoKey2+IntToStr(i)) then
                rk:=ReadString(rvVideoKey2+IntToStr(i))
              else
                rk:='';
            end;
            CloseKey;
   
            if rk<>'' then
            begin
              rk:=copy(rk,pos('Machine\',rk)+8,255);
              if OpenKey(rk,false) then
              begin
                if ValueExists(rvHardware+'.'+rvHWVideo) then
                begin
                  FillChar(bdata^,bdatasize+1,0);
                  try
                    readbinarydata(rvHardware+'.'+rvHWVideo,bdata^,bdatasize);
                  except
                    Log(LOG_WARNING, 'Could not retrieve adapter name!');
                    bdata^:=#0;
                  end;
                  FAdapter.Add(getstrfrombuf(pchar(bdata)));
                end
                else
                  FAdapter.Add('Unknown');

                if ValueExists(rvHardware+'.'+rvHWDAC) then
                begin
                  FillChar(bdata^,bdatasize+1,0);
                  try
                    readbinarydata(rvHardware+'.'+rvHWDAC,bdata^,bdatasize);
                  except
                    Log(LOG_WARNING, 'Could not retrieve DAC name!');
                    bdata^:=#0;
                  end;
                  FDAC.Add(getstrfrombuf(pchar(bdata)));
                end
                else
                  FDAC.Add('Unknown');

                if ValueExists(rvHardware+'.'+rvHWChip) then
                begin
                  FillChar(bdata^,bdatasize+1,0);
                  try
                    readbinarydata(rvHardware+'.'+rvHWChip,bdata^,bdatasize);
                  except
                    Log(LOG_WARNING, 'Could not retrieve Chipset name!');
                    bdata^:=#0;
                  end;
                  FChipset.Add(getstrfrombuf(pchar(bdata)));
                end
                else
                  FChipset.Add('Unknown');

                if ValueExists(rvHardware+'.'+rvHWMem) then
                begin
                  try
                    try
                      //Modern systems use REG_DWORD
                      idata:=readinteger(rvHardware+'.'+rvHWMem);
                    except on ERegistryException do
                      //Older systems use REG_BINARY
                      readbinarydata(rvHardware+'.'+rvHWMem,idata,4);
                    end
                  except
                    Log(LOG_WARNING, 'Could not retrieve Video Hardware Memory size!');
                    idata:=0;
                  end;
                  FMemory.Add(FormatBytes(idata));
                end
                else
                  FMemory.Add('Unknown');

                if ValueExists(rvProvider) then
                  FProvider.Add(ReadString(rvProvider))
                else
                  FProvider.Add('Unknown');

                if ValueExists(rvDriverDate) then
                  FDriverDate.Add(ReadString(rvDriverDate))
                else
                  FDriverDate.Add('Unknown');

                if ValueExists(rvDriverVersion) then
                  FDriverVersion.Add(ReadString(rvDriverVersion))
                else
                  FDriverVersion.Add('Unknown');

                CloseKey;
              end;
            end;
          end;
        end;
      end;
      free;
    end;
    strdispose(bdata);
   
    Log(LOG_VERBOSE, 'Gathering of 3D accelerator information...');
    FAcc.Clear;
    try
      GetClassDevices(ClassKey,rv3DClass,DescValue,FAcc);
    except
      on ERegistryException do
      begin
        AMDDriverDescBug:=True;
        FAcc.Clear;
      end;
    else
      //Not handled here
      raise;
    end;
   
    Log(LOG_VERBOSE, 'Gathering of display modes information...');
    FModes.Clear;
    j:=0;
    ZeroMemory(@DevMode,SizeOf(DevMode));
    DevMode.dmSize:=sizeof(DevMode);
    while EnumDisplaySettings(nil,j,DevMode) do
    begin
      with Devmode do
      begin
        FModes.Add(Format('%d x %d - %d bit',[dmPelsWidth,dmPelsHeight,dmBitsPerPel]));
        Inc(j);
      end;
    end;
  finally
    sl.free;
  end;
end;

constructor TDisplay.Create;
begin
  inherited;
  AMDDriverDescBug:=False;
  FAdapter:=TStringList.Create;
  FDevices:=TStringList.Create;
  FModes:=TStringList.Create;
  FAcc:=TStringList.Create;
  FDAc:=TStringList.Create;
  FChipset:=TStringList.Create;
  FMemory:=TStringList.Create;
  FProvider:=TStringList.Create;
  FDriverDate:=TStringList.Create;
  FDriverVersion:=TStringList.Create;
end;

destructor TDisplay.Destroy;
begin
  FAdapter.Free;
  FDevices.Free;
  FModes.Free;
  FAcc.Free;
  FDAc.Free;
  FChipset.Free;
  FMemory.Free;
  FProvider.Free;
  FDriverDate.Free;
  FDriverVersion.Free;
  inherited;
end;

procedure TDisplay.Report(var sl: TStringList);
var
  i :integer;
begin
  with sl do
  begin
    for i:=0 to Devices.count-1 do
    begin
      add(format('[Device %d] %s', [i+1,Devices[i]]));
    end;
    for i:=0 to Adapter.count-1 do
    begin
      add(format('[Adapter %d] %s', [i+1,Adapter[i]]));
      add('    Chipset: '   +Chipset[i]);
      add('    DAC: '       +DAC[i]);
      add('    Memory: '    +Memory[i] + ' Bytes');
      add('    Provider: '       +Provider[i]);
      add('    Driver date: '    +DriverDate[i]);
      add('    Driver version: ' +DriverVersion[i]);
    end;
    (*for i:=0 to Modes.Count-1 do
    begin
      add(format('[Mode %d] %s', [i+1,Modes[i]]));
    end;*)
  end;
end;

constructor TDirectX.Create;
begin
  inherited;
  FDirect3D:=TStringlist.Create;
end;

destructor TDirectX.Destroy;
begin
  FDirect3D.Free;
  inherited;
end;

procedure TDirectX.GetInfo;
var
  bdata :pchar;
  sl :tstringlist;
  i :integer;
const
  bdatasize = 255;

  rkDirectX = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\DirectX';
  rvDXVersion = 'Version';
  rvDXInstalledVersion = 'InstalledVersion';
  rkDirect3D = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Direct3D\Drivers';
  rkDirectPlay = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\DirectPlay\Services';
  rkDirectMusic = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\DirectMusic\SoftwareSynths';
  rvDesc = 'Description';
begin
  Log(LOG_VERBOSE, 'Starting gathering of DirectX system information...');
  with TRegistry.Create(KEY_READ) do
  begin
    rootkey:=HKEY_LOCAL_MACHINE;
    if OpenKey(rkDirectX,false) then
    begin
      bdata:=stralloc(bdatasize+1); //Note: One larger for null-terminator
      FVersion:=ReadString(rvDXVersion);
      if FVersion='' then
      begin
        if ValueExists(rvDXInstalledVersion) then
          try
            FillChar(bdata^,bdatasize+1,0);
            readbinarydata(rvDXInstalledVersion,bdata^,4);
            FVersion:=inttostr(lo(integer(bdata^)))+'.'+inttostr(hi(integer(bdata^)));
          except
            {$IFDEF Delphi4orNewerCompiler}
            try
              FillChar(bdata^,bdatasize+1,0);
              readbinarydata(rvDXInstalledVersion,bdata^,8);
              FVersion:=inttostr(lo(integer(bdata^)))+'.'+inttostr(hi(integer(bdata^)));
            except
            end;
            {$ENDIF}
          end;
      end;
      CloseKey;
      strdispose(bdata);
    end;
    FDirect3D.Clear;
    sl:=tstringlist.create;
    try
      if OpenKey(rkDirect3D,false) then
      begin
        getkeynames(sl);
        CloseKey;
        for i:=0 to sl.count-1 do
          if OpenKey(rkDirect3D+'\'+sl[i],false) then
          begin
            if ValueExists(rvDesc) then
              FDirect3D.Add(ReadString(rvDesc));
            CloseKey;
          end;
      end;
    finally
      sl.free;
    end;
    free;
  end;
end;

procedure TDirectX.Report(var sl: TStringList);
begin
  with sl do
  begin
    if Version<>'' then
    begin
      add('Installed version: '+Version);
      addstrings(Direct3D);
    end
    else
      add('Not installed.');
  end;
end;

Procedure GetCPUDetails(var s: TStringlist);
var
  c: TCPU;
begin
  c:=TCPU.Create;
  try
    c.getInfo;
    c.report(s);
  finally
    c.free;
  end;
end;

Procedure GetDisplayDetails(var s: TStringlist);
var
  c: TDisplay;
begin
  c:=TDisplay.Create;
  try
    c.getInfo;
    c.report(s);
  finally
    c.free;
  end;
end;

Procedure GetDirectXDetails(var s: TStringlist);
var
  c: TDirectX;
begin
  c:=TDirectX.Create;
  try
    c.getInfo;
    c.report(s);
  finally
    c.free;
  end;
end;

Procedure GetMemoryDetails(var s: TStringlist);
var
  c: TMemory;
begin
  c:=TMemory.Create;
  try
    c.getInfo;
    c.report(s);
  finally
    c.free;
  end;
end;

Procedure GetWorkStationDetails(var s: TStringlist);
var
  c: TWorkStation;
begin
  c:=TWorkStation.Create;
  try
    c.getInfo;
    c.report(s);
  finally
    c.free;
  end;
end;

Procedure GetOperatingSystemDetails(var s: TStringlist);
var
  c: TOperatingSystem;
begin
  c:=TOperatingSystem.Create;
  try
    c.getInfo;
    c.report(s);
  finally
    c.free;
  end;
end;

procedure GetPythonDetails(var s: TStringlist);
{
  Peter: deprecated as of 18-08-2003.
  Logging of Python interpreter details now done in Python.pas.

  This routine looks in the wrong place anyway.  Versions of Python later than
  2.0 do not use the \Software\Python\PythonCore\CurrentVersion registry key.
}
var
  R: TRegistry;
  v: string;
  installed: boolean;
begin
  Log(LOG_VERBOSE, 'Starting gathering Python information...');
  R:=TRegistry.Create(KEY_READ);
  try
    R.RootKey:=HKEY_LOCAL_MACHINE;
    installed:=R.KeyExists('\SOFTWARE\Python\PythonCore\CurrentVersion');
    if installed then
    begin
      R.OpenKey('\SOFTWARE\Python\PythonCore\CurrentVersion', false);
      v:=R.ReadString('');
      s.Add('Version: '+v);
      R.OpenKey('\SOFTWARE\Python\PythonCore\'+v+'\Dll', false);
      s.Add('Dll path: '+R.ReadString(''));
    end
    else
    begin
      s.Add('Not Installed.');
    end;
  finally
    R.free;
  end;
end;

function ProcessExists(const exeFileName: String): Boolean;
var 
  ContinueLoop: BOOL; 
  FSnapshotHandle: THandle; 
  FProcessEntry32: TProcessEntry32;
  ProcessNumber: Integer;
  ProcessSize, BytesReturned: DWORD;
  ProcessList, ProcessList2: PDWORD;
  ProcessHandle: THandle;
  ProcessModule: HMODULE;
  SizeNeeded: DWORD;
  ProcessName: String;
  ProcessNameBuffer: PChar;
  ProcessNameBufferSize: Cardinal;
  RealProcessNameSize: Cardinal;
  I: Integer;
begin
  Result := False;
  if WindowsPlatformCompatibility=osWin95Comp then
  begin
    FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    try
      FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
      ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
      while ContinueLoop <> false do
      begin
        if (ExtractFileName(FProcessEntry32.szExeFile) = ExeFileName)
          or (FProcessEntry32.szExeFile = ExeFileName) then
        begin
          Result := True;
          break;
        end;
        ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
      end;
    finally
      CloseHandle(FSnapshotHandle);
    end;
  end
  else
  begin
    ProcessNumber := 16;
    GetMem(ProcessList, ProcessNumber * SizeOf(DWORD));
    try
      repeat
        ProcessNumber := ProcessNumber * 2;
        ReallocMem(ProcessList, ProcessNumber * SizeOf(DWORD));
        BytesReturned := 0;
        ProcessSize := ProcessNumber * SizeOf(DWORD);
        if EnumProcesses(ProcessList, ProcessSize, BytesReturned) = false then
          raise exception.Create('Unable to enumerate processes!');
      until BytesReturned < ProcessSize;
      ProcessNumber := BytesReturned div SizeOf(DWORD);
      ProcessList2 := ProcessList;
      for I:=0 to ProcessNumber-1 do
      begin
        ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessList2^);
        if ProcessHandle <> 0 then
        begin
          try
            if EnumProcessModules(ProcessHandle, @ProcessModule, SizeOf(ProcessModule), SizeNeeded) <> false then
            begin
              ProcessNameBufferSize := 128;
              GetMem(ProcessNameBuffer, ProcessNameBufferSize * SizeOf(Char));
              try
                repeat
                  ProcessNameBufferSize := ProcessNameBufferSize * 2;
                  ReallocMem(ProcessNameBuffer, ProcessNameBufferSize * SizeOf(Char));
                  RealProcessNameSize := GetModuleBaseName(ProcessHandle, ProcessModule, ProcessNameBuffer, ProcessNameBufferSize);
                until RealProcessNameSize < ProcessNameBufferSize;
                if RealProcessNameSize > 0 then
                begin
                  SetLength(ProcessName, RealProcessNameSize);
                  ProcessName := PChar(ProcessNameBuffer);
                  if CompareStr(ProcessName, exeFileName) = 0 then
                  begin
                    Result:=True;
                    break;
                  end;
                end;
              finally
                FreeMem(ProcessNameBuffer);
              end;
            end;
          finally
            CloseHandle(ProcessHandle);
          end;
        end;
        Inc(ProcessList2);
      end;
    finally
      FreeMem(ProcessList);
    end;
  end;
end;

function WindowExists(const WindowName: String): Boolean;
var
  FoundWindow: HWND;
begin
  FoundWindow:=FindWindow(nil, PChar(WindowName));
  if FoundWindow<>0 then
    Result := True
  else
    Result := False;
end;

function RetrieveModuleFilename(ModuleHandle: HMODULE) : String;
var
  Path: LPSTR;
begin
  Path:=StrAlloc(MAX_PATH);
  try
    if GetModuleFileName(ModuleHandle, Path, MAX_PATH) = 0 then
      raise exception.create('Unable to retrieve filename of a module!');
    Result := StrPas(Path);
  finally
    StrDispose(Path);
  end;
end;

function CheckWindowsNT: Boolean;
begin
  Result:=(WindowsPlatformCompatibility=osWinNTComp);
end;

procedure WarnDriverBugs;
var
  S: String;
begin
  if AMDDriverDescBug then
  begin
    //Note: Can't put in dictionary; no Python loaded yet!
    S:='Registry corruption detected! The most probable cause is a known bad AMD driver. Please contact your video card manufacturer for a corrected driver.';
    S:=S+#13#10#13#10'Technical details:'#13#10;
    S:=S+'This bad AMD Graphics driver gives the "DriverDesc" key the wrong data-type (REG_BINARY instead of REG_SZ).'#13#10;
    S:=S+'This can be manually corrected, but this is not recommended.';
    S:=S+'QuArK will continue to run, but some graphical options may be disabled.'#13#10;
    S:=S+'For more information, see: http://quark.sourceforge.net/forums/index.php?topic=1064';
    MessageBox(0, PChar(S), 'QuArK', MB_TASKMODAL or MB_ICONWARNING or MB_OK);
  end;
end;

procedure SetDllSearchPath;
var
  c: TOperatingSystem;
  SetDllDirectoryPtr: Pointer;
begin
  //This is only available on Windows XP SP1 and later (and Windows Server 2003 and later).
  c:=TOperatingSystem.Create; //FIXME: Don't do this twice...!
  try
    c.getInfo;
    if (c.FMajorVersion > 5)
   or ((c.FMajorVersion = 5) and (c.FMinorVersion > 1))
   or ((c.FMajorVersion = 5) and (c.FMinorVersion = 1) and (c.ServicePackMajor >= 1)) then
      SetDllDirectoryPtr := GetProcAddress(GetModuleHandle('kernel32'), 'SetDllDirectoryA'); //Note: Delphi7 always calls the ANSI version
  finally
    c.free;
  end;

  if SetDllDirectoryPtr <> nil then
  begin
    SetDllDirectory := SetDllDirectoryPtr;
    if SetDllDirectory('') = false then
    begin
      LogWindowsError(GetLastError(), 'SetDllDirectory("")');
      SetDllDirectoryPtr := nil;
    end;
  end;

  if SetDllDirectoryPtr = nil then
  begin
    Log(LOG_WARNING, 'Failed to change the DLL search path; QuArK will be vulnerable to DLL hijacking!');
  end;
end;

Procedure LogSystemDetails;
var
  s: TStringlist;
  i: integer;
begin
  Log(LOG_INFO, 'Now logging system details...');
  s:=TStringList.Create;
  try
    s.add('CPU:');
    GetCPUDetails(s);
    s.add('');
    s.add('MEMORY:');
    GetMemoryDetails(s);
    s.add('');
    s.add('OS:');
    GetOperatingSystemDetails(s);
(*Peter: removed as of 18-08-2003.
    Logging of Python interpreter details now done in Python.pas.

    s.add('');
    s.add('PYTHON:');
    GetPythonDetails(s);
    s.add('');
*)

(*DECKER 2001.03.17 - we're not interested in Machine-/Username. We're not Login-Crackers!
    s.add('MACHINE:');
    GetWorkStationDetails(s);
*)
    s.add('');
    s.add('VIDEO:');
    GetDisplayDetails(s);
    s.add('');
    s.add('DIRECTX:');
    GetDirectxDetails(s);
    for i:=0 to s.count-1 do
    begin
      Log(LOG_SYS, s.strings[i]);
    end;
  finally
    s.free;
  end;
end;

initialization
begin
  //We should never allow an exception here, because then we'll exit with a vague runtime error!
  //try
    LogSystemDetails;
  //except
    //on E : Exception do
      //Log(LOG_WARNING, 'Failed to get system information: '+E.Message); //Note: Do *NOT* put this through LoadStr; not loaded yet!
  //end;
end;

end.
