{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.2  2001/02/11 22:26:44  aiv
Added CVS Headers

}
unit SystemDetails;

interface

uses
  SysUtils, Windows, Classes, Registry;

{$IFDEF VER130}
  {$DEFINE D4PLUS}
{$ENDIF}
{$IFDEF VER120}
  {$DEFINE D4PLUS}
{$ENDIF}

Procedure LogSystemDetails;

type
  {$IFDEF D4PLUS}
     TLargInt = _LARGE_INTEGER;
  {$ELSE}
     TLargInt = TLargeInteger;
     Int64 = TLargeInteger;
     LongWord = DWORD;
  {$ENDIF}
  TPlatFormType = (os9x,osNT4,os2K);
  TStrBuf = array[0..11] of char;
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
    function CPUIDExists: boolean;
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
    FMaxAppAddress: integer;
    FVirtualTotal: integer;
    FPageFileFree: integer;
    FVirtualFree: integer;
    FPhysicalFree: integer;
    FAllocGranularity: integer;
    FMinAppAddress: integer;
    FMemoryLoad: integer;
    FPhysicalTotal: integer;
    FPageFileTotal: integer;
    FPageSize: integer;
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
    property PhysicalTotal :integer read FPhysicalTotal write FPhysicalTotal stored false;
    property PhysicalFree :integer read FPhysicalFree write FPhysicalFree stored false;
    property VirtualTotal :integer read FVirtualTotal write FVirtualTotal stored false;
    property VirtualFree :integer read FVirtualFree write FVirtualFree stored false;
    property PageFileTotal :integer read FPageFileTotal write FPageFileTotal stored false;
    property PageFileFree :integer read FPageFileFree write FPageFileFree stored false;
    property MemoryLoad :integer read FMemoryLoad write FMemoryLoad stored false;
    property AllocGranularity :integer read FAllocGranularity write FAllocGranularity stored false;
    property MaxAppAddress :integer read FMaxAppAddress write FMaxAppAddress stored false;
    property MinAppAddress :integer read FMinAppAddress write FMinAppAddress stored false;
    property PageSize :integer read FPageSize write FPageSize stored false;
    property Win9x_SystemRes: Byte read FSystemRes write FSystemRes stored false;
    property Win9x_GDIRes: Byte read FGDIRes write FGDIRes stored false;
    property Win9x_UserRes: Byte read FUserRes write FUserRes stored false;
  end;
  TOperatingSystem = class(TPersistent)
  private
    FBuildNumber: integer;
    FMajorVersion: integer;
    FMinorVersion: integer;
    FPlatform: string;
    FCSD: string;
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
    property MajorVersion :integer read FMajorVersion write FMajorVersion stored false;
    property MinorVersion :integer read FMinorVersion write FMinorVersion stored false;
    property BuildNumber :integer read FBuildNumber write FBuildNumber stored false;
    property Platform :string read FPlatform write FPlatform stored false;
    property Version :string read FVersion write FVersion stored false;
    property CSD :string read FCSD write FCSD stored false;
    property SerialNumber :string read FSerialNumber write FSerialNumber stored false;
    property RegisteredUser :string read FRegUser write FRegUser stored false;
    property RegisteredOrg :string read FRegOrg write FRegOrg stored false;
    property Environment :TStrings read FEnv write FEnv stored false;
    property Directories: TStrings read FDirs write FDirs stored False;
  end;
  TWorkstation = class(TPersistent)
  private
    FName: string;
    FLastBoot: TDatetime;
    FUser: string;
    FSystemUpTime: Extended;
    FBIOSExtendedInfo: string;
    FBIOSCopyright: string;
    FBIOSName: string;
    FBIOSDate: string;
    FScrollLock: Boolean;
    FNumLock: Boolean;
    FCapsLock: Boolean;
    function GetSystemUpTime: Extended;
  public
    procedure GetInfo;
    procedure Report(var sl :TStringList);
  published
    property Name :string read FName write FName stored false;
    property User :string read FUser write FUser stored false;
    property SystemUpTime :Extended read FSystemUpTime write FSystemUpTime stored false;
    property LastBoot :TDatetime read FLastBoot write FLastBoot stored false;
    property BIOSCopyright :string read FBIOSCopyright write FBIOSCopyright stored false;
    property BIOSDate :string read FBIOSDate write FBIOSDate stored false;
    property BIOSExtendedInfo :string read FBIOSExtendedInfo write FBIOSExtendedInfo stored false;
    property BIOSName :string read FBIOSName write FBIOSName stored false;
    property CapsLock: Boolean read FCapsLock write FCapsLock stored false;
    property NumLock: Boolean read FNumLock write FNumLock stored false;
    property ScrollLock: Boolean read FScrollLock write FScrollLock stored false;
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
    FBIOSDate: string;
    FBIOSVersion: string;
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
    FAdapter: TStrings;
    FDAC: TStrings;
    FAcc: TStrings;
    FModes: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetInfo;
    procedure Report(var sl :TStringList);
  published
    property Adapter :TStrings read FAdapter write FAdapter stored false;
    property Accelerator :TStrings read FAcc write FAcc stored false;
    property DAC :TStrings read FDAC write FDAC stored false;
    property Chipset :TStrings read FChipset write FChipset stored false;
    property Memory :TStrings read FMemory write FMemory stored false;
    property HorzRes :integer read FHorzRes write FHorzRes stored false;
    property VertRes :integer read FVertRes write FVertRes stored false;
    property ColorDepth :integer read FColorDepth write FColorDepth stored false;
    // BIOS info is available only under NT
    property BIOSVersion :string read FBIOSVersion write FBIOSVersion stored false;
    property BIOSDate :string read FBIOSDate write FBIOSDate stored false;
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

var
  IsNT,IS95,Is98,Is2000,IsOSR2: Boolean;
  WindowsUser, MachineName: string;
  Platform: TPlatformType;

implementation


uses ShlObj, Logging;

var
  VLevel, VFamily, VModel, VStepping, VTyp :Byte;
  VFeatures :LongInt;
  ClassKey: string;
const
  DescValue = 'DriverDesc';

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

  CPUVendorIDs :array[0..5] of string = ('GenuineIntel',
                                         'UMC UMC UMC',
                                         'AuthenticAMD',
                                         'CyrixInstead',
                                         'NexGenDriven',
                                         'CentaurHauls');

  CPUVendors :array[0..5] of string = ('Intel',
                                       'UMC',
                                       'AMD',
                                       'Cyrix',
                                       'NexGen',
                                       'CentaurHauls');

function TCPU.CPUIDExists: boolean; register;
asm
	PUSHFD			 //direct access to flags no possible, only via stack
	POP     EAX		 //flags to EAX
	MOV     EDX,EAX		 //save current flags
	XOR     EAX,ID_BIT	 //not ID bit
	PUSH    EAX		 //onto stack
	POPFD			 //from stack to flags, with not ID bit
	PUSHFD			 //back to stack
	POP     EAX		 //get back to EAX
	XOR     EAX,EDX		 //check if ID bit affected
	JZ      @exit		 //no, CPUID not availavle
	MOV     AL,True		 //Result=True
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
	MOV eax, 0	//  Get Level
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
	JE @@end_CPUTyp   //it's a 386

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
  	PUSH    ebx	       	//Save regs
	PUSH    edi
	MOV     edi,eax		//@Result (TStrBuf)
	MOV     eax,0
	DW      $A20F		//CPUID Command
	MOV     eax,ebx
	XCHG	ebx,ecx         //save ECX result
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
  @3:            			//save last 4 byte
	STOSB
	SHR     eax,8
	LOOP    @3
	POP     edi		//Restore regs
	POP     ebx
  end;

begin
  i:=0;
  result:='';
  s:=_GetCPUVendor;
  repeat
    result:=result+s[i];
    inc(i);
  until i>11;
  FVendorNo:=-1;
  for i:=0 to high(CPUVendorIDs) do
    if result=CPUVendorIDs[i] then begin
      result:=CPUVendors[i];
      FVendorNo:=i;
      break;
    end;
end;

function TCPU.GetCPUVendorID :string;
begin
  case Family of
     4 :case FVendorNo Of
          0 :case Model of
               0 :result:='i80486DX-25/33';
               1 :result:='i80486DX-50';
               2 :result:='i80486SX';
               3 :result:='i80486DX2';
               4 :result:='i80486SL';
               5 :result:='i80486SX2';
               7 :result:='i80486DX2WB';
               8 :result:='i80486DX4';
	              9 :result:='i80486DX4WB';
             end;
          1 :case Model of
               1 :result:='U5D(486DX)';
	              2 :result:='U5S(486SX)';
             end;
          2 :case Model of
               3 :result:='80486DX2WT';
               7 :result:='80486DX2WB';
               8 :result:='80486DX4';
               9 :result:='80486DX4WB';
              14 :result:='5x86';
              15 :result:='5x86WB';
             end;
          3 :case Model of
               4 :result:='Cyrix Media GX';
               9 :result:='Cyrix 5x86';
             end;
        end;
     5 :case FVendorNo of
          0 :case Model of
               0 :result:='P5 A-step';
               1 :result:='P5';
               2 :result:='P54C';
               3 :result:='P24T OverDrive';
               4 :result:='P55C';
               5 :result:='DX4 OverDrive?';
               6 :result:='P5 OverDrive?';
	              7 :result:='P54C';
               8 :result:='P55C(0,25µm)MMX';
             end;
          2 :case Model of
	              0 :result:='SSA5';
               1 :result:='5k86';
               2 :result:='5k86';
               3 :result:='5k86';
               6 :result:='K6';
               7 :result:='K6';
               8 :result:='K6-3D';
	              9 :result:='K6PLUS-3D';
             end;
          3 :case Model of
               0 :result:='Pentium Cx6X86 GXm';
               2 :result:='Std. Cx6x86';
               4 :result:='Cx6x86 GXm';
             end;
          else
             if FVendorNo=4 then
               result:='Nx586';
             if FVendorNo=5 then
               result:='IDT C6 (WinChip)';
	end;
     6 :case FVendorNo of
          0 :case Model of
               0 :result:='PentiumPro A-step';
               1 :result:='Pentium Pro';
               3 :result:='Pentium II';
               4 :result:='P55CT (P54 overdrive)';
               5 :result:='Pentium II 0,25µm';
             end;
	  2 :case Model of
               6 :result:='K6';
               7 :result:='K6';
               8 :result:='K6-3D';
       	       9 :result:='K6PLUS-3D';
             end;
          3 :if Model=0 then
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
  {$IFDEF D4PLUS}
  Target:=PerfCount+(Freq*Iterations);
  {$ELSE}
  Target.QuadPart:=PerfCount.QuadPart+(Freq.QuadPart*Iterations);
  {$ENDIF}
  StartTimer;
  repeat
    QueryPerformanceCounter(PerfCount);
  {$IFDEF D4PLUS}
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
    3 :result:='Reserved';
    2 :result:='Secondary';
    1 :result:='OverDrive';
    0 :result:='Primary';
    else
      result:='Not Detected!';
  end;
end;

procedure TCPU.GetInfo;
var
  SI :TSystemInfo;
begin
  ZeroMemory(@SI,SizeOf(SI));
  GetSystemInfo(SI);
  Count:=SI.dwNumberOfProcessors;
  Family:=SI.dwProcessorType;
//  Vendor:=
//  VendorID:=
  CPUID:=CPUIDExists;
  if CPUID then begin
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
  with sl do begin
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
  c,i :dword;
  b :pchar;
  s :string;
begin
  FEnv.Clear;
  c:=1024;
  b:=GetEnvironmentStrings;
  i:=0;
  s:='';
  while i<c do begin
    if b[i]<>#0 then
      s:=s+b[i]
    else begin
      if s='' then
        break;
      FEnv.Add(s);
      s:='';
    end;
    inc(i);
  end;
  FreeEnvironmentStrings(b);
end;

function GetSpecialFolder(Handle: Hwnd; nFolder: Integer): string;
var
  PIDL: PItemIDList;
  Path: LPSTR;
begin
  Result:='';
  Path:=StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(Handle, nFolder, PIDL);

  if SHGetPathFromIDList(PIDL, Path) then
    Result:=StrPas(Path);

  StrDispose(Path);
end;

function ReverseStr(S: string): string;
var
  l,i: integer;
begin
  l:=Length(s);
  Result:='';
  for i:=0 to l-1 do
    Result:=Result+s[l-i];
end;

procedure TOperatingSystem.GetInfo;
var
  OS :TOSVersionInfo;
  OK: Boolean;
  p: pchar;
  n: DWORD;
  WinH: HWND;
  s: string;
const
  rkOSInfo95 = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows\CurrentVersion';
  rkOSInfoNT = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
  rvVersionName95 = 'Version';
  rvVersionNameNT = 'CurrentType';
  rvRegOrg = 'RegisteredOrganization';
  rvRegOwn = 'RegisteredOwner';
  rvProductID = 'ProductID';

  cUserProfile = 'USERPROFILE';
  cUserProfileReg = {HKEY_CURRENT_USER\}'Software\Microsoft\Windows\CurrentVersion\ProfileList';
  cUserProfileRec = {HKEY_CURRENT_USER\}'SOFTWARE\Microsoft\Windows\CurrentVersion\ProfileReconciliation';
  cProfileDir = 'ProfileDirectory';
begin
  FDirs.Clear;
  ZeroMemory(@OS,SizeOf(OS));
  OS.dwOSVersionInfoSize:=SizeOf(OS);
  GetVersionEx(OS);
  MajorVersion:=OS.dwMajorVersion;
  MinorVersion:=OS.dwMinorVersion;
  BuildNumber:=word(OS.dwBuildNumber);
  case OS.dwPlatformId of
    VER_PLATFORM_WIN32s        :Platform:='Windows 3.1x';
    VER_PLATFORM_WIN32_WINDOWS :Platform:='Windows 95';
    VER_PLATFORM_WIN32_NT      :Platform:='Windows NT';
  end;
  if MajorVersion>4 then
    Platform:='Windows 2000';
  CSD:=strpas(OS.szCSDVersion);
  Version:='';
  RegisteredUser:='';
  RegisteredOrg:='';
  SerialNumber:='';
  with TRegistry.create do begin
    rootkey:=HKEY_LOCAL_MACHINE;
    if isnt then begin
      if OpenKey(rkOSInfoNT,false) then begin
        if ValueExists(rvVersionNameNT) then
          Version:=ReadString(rvVersionNameNT);
        if ValueExists(rvRegOrg) then
          RegisteredOrg:=ReadString(rvRegOrg);
        if ValueExists(rvRegOwn) then
          RegisteredUser:=ReadString(rvRegOwn);
        if ValueExists(rvProductID) then
          SerialNumber:=ReadString(rvProductID);
        closekey;
      end;
    end else begin
      if OpenKey(rkOSInfo95,false) then begin
        if ValueExists(rvVersionName95) then
          Version:=ReadString(rvVersionName95);
        if ValueExists(rvRegOrg) then
          RegisteredOrg:=ReadString(rvRegOrg);
        if ValueExists(rvRegOwn) then
          RegisteredUser:=ReadString(rvRegOwn);
        if ValueExists(rvProductID) then
          SerialNumber:=ReadString(rvProductID);
        closekey;
      end;
    end;
    rootkey:=HKEY_LOCAL_MACHINE;
    if IsNT then
      OK:=OpenKey(rkOSInfoNT,False)
    else
      OK:=OpenKey(rkOSInfo95,False);
    if OK then begin
      FDirs.Add('CommonFiles='+ReadString('CommonFilesDir'));
      FDirs.Add('ProgramFiles='+ReadString('ProgramFilesDir'));
      FDirs.Add('Device='+ReadString('DevicePath'));
      FDirs.Add('OtherDevice='+ReadString('OtherDevicePath'));
      FDirs.Add('Media='+ReadString('MediaPath'));
      FDirs.Add('Config='+ReadString('ConfigPath'));
      FDirs.Add('Wallpaper='+ReadString('WallPaperDir'));
      CloseKey;
    end;
    Free;
  end;

  n:=MAX_PATH;
  p:=StrAlloc(n);

  GetWindowsDirectory(p,n);
  FDirs.Add('Windows='+StrPas(p));

  GetSystemDirectory(p,n);
  FDirs.Add('System='+StrPas(p));

  GetTempPath(n,p);
  FDirs.Add('Temp='+StrPas(p));

  StrDispose(p);

  WinH:=GetDesktopWindow;
  FDirs.Add('AppData='+GetSpecialFolder(WinH,CSIDL_APPDATA));
  FDirs.Add('CommonDesktopDir='+GetSpecialFolder(WinH,CSIDL_COMMON_DESKTOPDIRECTORY));
  FDirs.Add('CommonAltStartUp='+GetSpecialFolder(WinH,CSIDL_COMMON_ALTSTARTUP));
  FDirs.Add('RecycleBin='+GetSpecialFolder(WinH,CSIDL_BITBUCKET));
  FDirs.Add('CommonPrograms='+GetSpecialFolder(WinH,CSIDL_COMMON_PROGRAMS));
  FDirs.Add('CommonStartMenu='+GetSpecialFolder(WinH,CSIDL_COMMON_STARTMENU));
  FDirs.Add('CommonStartup='+GetSpecialFolder(WinH,CSIDL_COMMON_STARTUP));
  FDirs.Add('CommonFavorites='+GetSpecialFolder(WinH,CSIDL_COMMON_FAVORITES));
  FDirs.Add('Cookies='+GetSpecialFolder(WinH,CSIDL_COOKIES));
  FDirs.Add('Controls='+GetSpecialFolder(WinH,CSIDL_CONTROLS));
  FDirs.Add('Desktop='+GetSpecialFolder(WinH,CSIDL_DESKTOP));
  FDirs.Add('DesktopDir='+GetSpecialFolder(WinH,CSIDL_DESKTOPDIRECTORY));
  FDirs.Add('Favorites='+GetSpecialFolder(WinH,CSIDL_FAVORITES));
  FDirs.Add('Drives='+GetSpecialFolder(WinH,CSIDL_DRIVES));
  FDirs.Add('Fonts='+GetSpecialFolder(WinH,CSIDL_FONTS));
  FDirs.Add('History='+GetSpecialFolder(WinH,CSIDL_HISTORY));
  FDirs.Add('Internet='+GetSpecialFolder(WinH,CSIDL_INTERNET));
  FDirs.Add('InternetCache='+GetSpecialFolder(WinH,CSIDL_INTERNET_CACHE));
  FDirs.Add('NetWork='+GetSpecialFolder(WinH,CSIDL_NETWORK));
  FDirs.Add('NetHood='+GetSpecialFolder(WinH,CSIDL_NETHOOD));
  FDirs.Add('MyDocuments='+GetSpecialFolder(WinH,CSIDL_PERSONAL));
  FDirs.Add('PrintHood='+GetSpecialFolder(WinH,CSIDL_PRINTHOOD));
  FDirs.Add('Printers='+GetSpecialFolder(WinH,CSIDL_PRINTERS));
  FDirs.Add('Programs='+GetSpecialFolder(WinH,CSIDL_PROGRAMS));
  FDirs.Add('Recent='+GetSpecialFolder(WinH,CSIDL_RECENT));
  FDirs.Add('SendTo='+GetSpecialFolder(WinH,CSIDL_SENDTO));
  FDirs.Add('StartMenu='+GetSpecialFolder(WinH,CSIDL_STARTMENU));
  FDirs.Add('StartUp='+GetSpecialFolder(WinH,CSIDL_STARTUP));
  FDirs.Add('Templates='+GetSpecialFolder(WinH,CSIDL_TEMPLATES));
  s:=ReverseStr(FDirs.Values['Desktop']);
  s:=ReverseStr(Copy(s,Pos('\',s)+1,255));
  FDirs.Add('Profile='+s);
  GetEnvironment;
end;

procedure TOperatingSystem.Report(var sl: TStringList);
begin
  with sl do begin
    add('Platform: '+Platform);
    add(format('Version: %s %d.%d.%d',[Version,MajorVersion,MinorVersion,BuildNumber]));
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
  MaxAppAddress:=DWORD(SI.lpMaximumApplicationAddress);
  MinAppAddress:=DWORD(SI.lpMinimumApplicationAddress);
  PageSize:=DWORD(SI.dwPageSize);
  FSystemRes:=GetSystemRes;
  FGDIRes:=GetGDIRes;
  FUserRes:=GetUserRes;
end;

procedure TMemory.Report(var sl: TStringList);
begin
  with sl do begin
    add(formatfloat('Physical Memory Free: #,## B',PhysicalFree));
    add(formatfloat('Virtual Memory Free: #,## B',VirtualFree));
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

function GetMachine :string;
var
  n :dword;
  buf :pchar;
const
  rkMachine = {HKEY_LOCAL_MACHINE\}'SYSTEM\CurrentControlSet\Control\ComputerName\ComputerName';
    rvMachine = 'ComputerName';
begin
  n:=255;
  buf:=stralloc(n);
  GetComputerName(buf,n);
  result:=strpas(buf);
  strdispose(buf);
  with TRegistry.Create do begin
    rootkey:=HKEY_LOCAL_MACHINE;
    if OpenKey(rkMachine,false) then begin
      if ValueExists(rvMachine) then
        result:=ReadString(rvMachine);
      closekey;
    end;
    free;
  end;
end;

function GetUser :string;
var
  n :dword;
  buf :pchar;
begin
  n:=255;
  buf:=stralloc(n);
  GetUserName(buf,n);
  result:=strpas(buf);
  strdispose(buf);
end;

procedure TWorkstation.GetInfo;
var
  bdata :pchar;
  KeyState : TKeyBoardState;
const
  cBIOSName = $FE061;
  cBIOSDate = $FFFF5;
  cBIOSExtInfo = $FEC71;
  cBIOSCopyright = $FE091;

  rkBIOS = {HKEY_LOCAL_MACHINE\}'HARDWARE\DESCRIPTION\System';
    rvBiosDate = 'SystemBiosDate';
    rvBiosID = 'Identifier';
    rvBiosVersion = 'SystemBiosVersion';

begin
  try
    FLastBoot:=Now-(GetTimeStamp.QuadPart/GetTicksPerSecond(1))/(24*3600);
  except
    FLastBoot:=0;
  end;
  FSystemUpTime:=GetSystemUpTime;
  FName:=GetMachine;
  FUser:=GetUser;
  if isNT then begin
    with TRegistry.Create do begin
      rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkBIOS,false) then begin
        if ValueExists(rvBIOSID) then
          FBiosName:=ReadString(rvBIOSID);
        if ValueExists(rvBIOSVersion) then begin
          bdata:=stralloc(255);
          try
            readbinarydata(rvBIOSVersion,bdata^,255);
            FBIOSCopyright:=strpas(pchar(bdata));
          except
          end;
        end;
        if ValueExists(rvBIOSDate) then
          FBIOSDate:=ReadString(rvBIOSDate);
        closekey;
      end;
      free;
    end;
  end else begin
    FBIOSName:=string(pchar(ptr(cBIOSName)));
    FBIOSDate:=string(pchar(ptr(cBIOSDate)));
    FBIOSCopyright:=string(pchar(ptr(cBIOSCopyright)));
    FBIOSExtendedInfo:=string(pchar(ptr(cBIOSExtInfo)));
  end;
  GetKeyboardState(KeyState);
  FCapsLock:=KeyState[VK_CAPITAL]=1;
  FNumLock:=KeyState[VK_NUMLOCK]=1;
  FScrollLock:=KeyState[VK_SCROLL]=1;
end;


procedure TWorkstation.Report(var sl: TStringList);
begin
  with sl do begin
    add('Name: '+Name);
    add('User: '+User);
//    add('System Up Time: '+formatseconds(SystemUpTime,true,false,false));
  end;
end;

function GetClassDevices(AStartKey,AClassName,AValueName :string; var AResult :TStrings) :string;
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
  with TRegistry.Create do begin
    RootKey:=HKEY_LOCAL_MACHINE;
    if OpenKey(AStartKey,false) then begin
      sl:=TStringList.Create;
      GetKeyNames(sl);
      CloseKey;
      for i:=0 to sl.Count-1 do
        if OpenKey(AStartKey+'\'+sl[i],false) then begin
          if ValueExists(rvClass) then begin
            rclass:=UpperCase(ReadString(rvClass));
            if rclass=UpperCase(AClassName) then begin
              if not IsNT then begin
                s:=UpperCase(ReadString(rvLink));
                CloseKey;
                if not OpenKey(AStartKey+'\'+s,False) then
                  Exit;
              end else
                s:=sl[i];
              Result:=AStartKey+'\'+s;
              GetKeyNames(sl);
              CloseKey;
              for j:=0 to sl.count-1 do
                if OpenKey(AStartKey+'\'+s+'\'+sl[j],false) then begin
                  if ValueExists(AValueName) then
                    AResult.Add(ReadString(AValueName));
                  CloseKey;
                end;
                Break;
            end;
          end;
          CloseKey;
        end;
      sl.free;
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
    if buffer[i]<>#0 then begin
      result:=result+buffer[i];
      j:=0;
    end else
      inc(j);
    inc(i);
  until j>1;
end;

procedure TDisplay.GetInfo;
var
  rk :string;
  idata,bdata :pchar;
  sl :tstringlist;
  i :integer;
  DevMode : TDevMode;
  Found: Boolean;
const
  rkVideoHardware = {HKEY_LOCAL_MACHINE\}'HARDWARE\DEVICEMAP\VIDEO';
  rvVideoKey1 = '\Device\Video0';
  rvVideoKey2 = '\\Device\\Video0';
   rvHardware = 'HardwareInformation';
    rvHWVideo = 'AdapterString';
    rvHWDAC = 'DacType';
    rvHWChip = 'ChipType';
    rvHWMem = 'MemorySize';

  rvVideoClass = 'Display';

    rkClassInfo = 'INFO';
    rvCIVideo = 'DriverDesc';
    rvCIDAC = 'DACType';
    rvCIChip = 'ChipType';
    rvCIMem = 'VideoMemory';
    rvCIRev = 'Revision';

  rv3DClass = '3D Accelerators';

  rkBIOS = {HKEY_LOCAL_MACHINE\}'HARDWARE\DESCRIPTION\System';
    rvVideoBiosDate = 'VideoBiosDate';
    rvVideoBiosVersion = 'VideoBiosVersion';
begin
  FHorzRes:=GetDeviceCaps(GetDC(0),windows.HORZRES);
  FVertRes:=GetDeviceCaps(GetDC(0),windows.VERTRES);
  FColorDepth:=GetDeviceCaps(GetDC(0),BITSPIXEL);
  case GetDeviceCaps(GetDC(0),windows.TECHNOLOGY) of
    DT_PLOTTER:    FTechnology:='Vector Plotter';
    DT_RASDISPLAY: FTechnology:='Raster Display';
    DT_RASPRINTER: FTechnology:='Raster Printer';
    DT_RASCAMERA:  FTechnology:='Raster Camera';
    DT_CHARSTREAM: FTechnology:='Character Stream';
    DT_METAFILE:   FTechnology:='Metafile';
    DT_DISPFILE:   FTechnology:='Display File';
  end;
  FHorzSize:=GetDeviceCaps(GetDC(0),HORZSIZE);
  FVertSize:=GetDeviceCaps(GetDC(0),VERTSIZE);
  FPixelWidth:=GetDeviceCaps(GetDC(0),ASPECTX);
  FPixelHeight:=GetDeviceCaps(GetDC(0),ASPECTY);
  FPixelDiagonal:=GetDeviceCaps(GetDC(0),ASPECTXY);
  FCurveCaps:=[];
  if GetDeviceCaps(GetDC(0),windows.CURVECAPS)<>CC_NONE then begin
    if (GetDeviceCaps(GetDC(0),windows.CURVECAPS) and CC_CIRCLES)=CC_CIRCLES then
      FCurveCaps:=FCurveCaps+[ccCircles];
    if (GetDeviceCaps(GetDC(0),windows.CURVECAPS) and CC_PIE)=CC_PIE then
      FCurveCaps:=FCurveCaps+[ccPieWedges];
    if (GetDeviceCaps(GetDC(0),windows.CURVECAPS) and CC_CHORD)=CC_CHORD then
      FCurveCaps:=FCurveCaps+[ccChords];
    if (GetDeviceCaps(GetDC(0),windows.CURVECAPS) and CC_ELLIPSES)=CC_ELLIPSES then
      FCurveCaps:=FCurveCaps+[ccEllipses];
    if (GetDeviceCaps(GetDC(0),windows.CURVECAPS) and CC_WIDE)=CC_WIDE then
      FCurveCaps:=FCurveCaps+[ccWideBorders];
    if (GetDeviceCaps(GetDC(0),windows.CURVECAPS) and CC_STYLED)=CC_STYLED then
      FCurveCaps:=FCurveCaps+[ccStyledBorders];
    if (GetDeviceCaps(GetDC(0),windows.CURVECAPS) and CC_WIDESTYLED)=CC_WIDESTYLED then
      FCurveCaps:=FCurveCaps+[ccWideStyledBorders];
    if (GetDeviceCaps(GetDC(0),windows.CURVECAPS) and CC_INTERIORS)=CC_INTERIORS then
      FCurveCaps:=FCurveCaps+[ccInteriors];
    if (GetDeviceCaps(GetDC(0),windows.CURVECAPS) and CC_ROUNDRECT)=CC_ROUNDRECT then
      FCurveCaps:=FCurveCaps+[ccRoundedRects];
  end;
  FLineCaps:=[];
  if GetDeviceCaps(GetDC(0),windows.LINECAPS)<>LC_NONE then begin
    if (GetDeviceCaps(GetDC(0),windows.LINECAPS) and LC_POLYLINE)=LC_POLYLINE then
      FLineCaps:=FLineCaps+[lcPolylines];
    if (GetDeviceCaps(GetDC(0),windows.LINECAPS) and LC_MARKER)=LC_MARKER then
      FLineCaps:=FLineCaps+[lcMarkers];
    if (GetDeviceCaps(GetDC(0),windows.LINECAPS) and LC_POLYMARKER)=LC_POLYMARKER then
      FLineCaps:=FLineCaps+[lcMultipleMarkers];
    if (GetDeviceCaps(GetDC(0),windows.LINECAPS) and LC_WIDE)=LC_WIDE then
      FLineCaps:=FLineCaps+[lcWideLines];
    if (GetDeviceCaps(GetDC(0),windows.LINECAPS) and LC_STYLED)=LC_STYLED then
      FLineCaps:=FLineCaps+[lcStyledLines];
    if (GetDeviceCaps(GetDC(0),windows.LINECAPS) and LC_WIDESTYLED)=LC_WIDESTYLED then
      FLineCaps:=FLineCaps+[lcWideStyledLines];
    if (GetDeviceCaps(GetDC(0),windows.LINECAPS) and LC_INTERIORS)=LC_INTERIORS then
      FLineCaps:=FLineCaps+[lcInteriors];
  end;
  FPolygonCaps:=[];
  if GetDeviceCaps(GetDC(0),POLYGONALCAPS)<>PC_NONE then begin
    if (GetDeviceCaps(GetDC(0),POLYGONALCAPS) and PC_POLYGON)=PC_POLYGON then
      FPolygonCaps:=FPolygonCaps+[pcAltFillPolygons];
    if (GetDeviceCaps(GetDC(0),POLYGONALCAPS) and PC_RECTANGLE)=PC_RECTANGLE then
      FPolygonCaps:=FPolygonCaps+[pcRectangles];
    if (GetDeviceCaps(GetDC(0),POLYGONALCAPS) and PC_WINDPOLYGON)=PC_WINDPOLYGON then
      FPolygonCaps:=FPolygonCaps+[pcWindingFillPolygons];
    if (GetDeviceCaps(GetDC(0),POLYGONALCAPS) and PC_SCANLINE)=PC_SCANLINE then
      FPolygonCaps:=FPolygonCaps+[pcSingleScanlines];
    if (GetDeviceCaps(GetDC(0),POLYGONALCAPS) and PC_WIDE)=PC_WIDE then
      FPolygonCaps:=FPolygonCaps+[pcWideBorders];
    if (GetDeviceCaps(GetDC(0),POLYGONALCAPS) and PC_STYLED)=PC_STYLED then
      FPolygonCaps:=FPolygonCaps+[pcStyledBorders];
    if (GetDeviceCaps(GetDC(0),POLYGONALCAPS) and PC_WIDESTYLED)=PC_WIDESTYLED then
      FPolygonCaps:=FPolygonCaps+[pcWideStyledBorders];
    if (GetDeviceCaps(GetDC(0),POLYGONALCAPS) and PC_INTERIORS)=PC_INTERIORS then
      FPolygonCaps:=FPolygonCaps+[pcInteriors];
  end;
  FRasterCaps:=[];
  if (GetDeviceCaps(GetDC(0),windows.RASTERCAPS) and RC_BANDING)=RC_BANDING then
    FRasterCaps:=FRasterCaps+[rcRequiresBanding];
  if (GetDeviceCaps(GetDC(0),windows.RASTERCAPS) and RC_BITBLT)=RC_BITBLT then
    FRasterCaps:=FRasterCaps+[rcTranserBitmaps];
  if (GetDeviceCaps(GetDC(0),windows.RASTERCAPS) and RC_BITMAP64)=RC_BITMAP64 then
    FRasterCaps:=FRasterCaps+[rcBitmaps64K];
  if (GetDeviceCaps(GetDC(0),windows.RASTERCAPS) and RC_DI_BITMAP)=RC_DI_BITMAP then
    FRasterCaps:=FRasterCaps+[rcSetGetDIBits];
  if (GetDeviceCaps(GetDC(0),windows.RASTERCAPS) and RC_DIBTODEV)=RC_DIBTODEV then
    FRasterCaps:=FRasterCaps+[rcSetDIBitsToDevice];
  if (GetDeviceCaps(GetDC(0),windows.RASTERCAPS) and RC_FLOODFILL)=RC_FLOODFILL then
    FRasterCaps:=FRasterCaps+[rcFloodfills];
  if (GetDeviceCaps(GetDC(0),windows.RASTERCAPS) and RC_GDI20_OUTPUT)=RC_GDI20_OUTPUT then
    FRasterCaps:=FRasterCaps+[rcWindows2xFeatures];
  if (GetDeviceCaps(GetDC(0),windows.RASTERCAPS) and RC_PALETTE)=RC_PALETTE then
    FRasterCaps:=FRasterCaps+[rcPaletteBased];
  if (GetDeviceCaps(GetDC(0),windows.RASTERCAPS) and RC_SCALING)=RC_SCALING then
    FRasterCaps:=FRasterCaps+[rcScaling];
  if (GetDeviceCaps(GetDC(0),windows.RASTERCAPS) and RC_STRETCHBLT)=RC_STRETCHBLT then
    FRasterCaps:=FRasterCaps+[rcStretchBlt];
  if (GetDeviceCaps(GetDC(0),windows.RASTERCAPS) and RC_STRETCHDIB)=RC_STRETCHDIB then
    FRasterCaps:=FRasterCaps+[rcStretchDIBits];
  FTextCaps:=[];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_OP_CHARACTER)=TC_OP_CHARACTER then
    FTextCaps:=FTextCaps+[tcCharOutPrec];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_OP_STROKE)=TC_OP_STROKE then
    FTextCaps:=FTextCaps+[tcStrokeOutPrec];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_CP_STROKE)=TC_CP_STROKE then
    FTextCaps:=FTextCaps+[tcStrokeClipPrec];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_CR_90)=TC_CR_90 then
    FTextCaps:=FTextCaps+[tcCharRotation90];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_CR_ANY)=TC_CR_ANY then
    FTextCaps:=FTextCaps+[tcCharRotationAny];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_SF_X_YINDEP)=TC_SF_X_YINDEP then
    FTextCaps:=FTextCaps+[tcScaleIndependent];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_SA_DOUBLE)=TC_SA_DOUBLE then
    FTextCaps:=FTextCaps+[tcDoubledCharScaling];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_SA_INTEGER)=TC_SA_INTEGER then
    FTextCaps:=FTextCaps+[tcIntMultiScaling];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_SA_CONTIN)=TC_SA_CONTIN then
    FTextCaps:=FTextCaps+[tcAnyMultiExactScaling];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_EA_DOUBLE)=TC_EA_DOUBLE then
    FTextCaps:=FTextCaps+[tcDoubleWeightChars];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_IA_ABLE)=TC_IA_ABLE then
    FTextCaps:=FTextCaps+[tcItalics];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_UA_ABLE)=TC_UA_ABLE then
    FTextCaps:=FTextCaps+[tcUnderlines];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and  TC_SO_ABLE)=TC_SO_ABLE then
    FTextCaps:=FTextCaps+[tcStrikeouts];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_RA_ABLE)=TC_RA_ABLE then
    FTextCaps:=FTextCaps+[tcRasterFonts];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_VA_ABLE)=TC_VA_ABLE then
    FTextCaps:=FTextCaps+[tcVectorFonts];
  if (GetDeviceCaps(GetDC(0),windows.TEXTCAPS) and TC_SCROLLBLT)=TC_SCROLLBLT then
    FTextCaps:=FTextCaps+[tcNoScrollUsingBlts];
  sl:=tstringlist.create;
  FAdapter.Clear;
  FDAC.Clear;
  FChipset.Clear;
  FMemory.Clear;
  bdata:=stralloc(255);
  rk:=GetClassDevices(ClassKey,rvVideoClass,DescValue,FAdapter);
  Found:=False;
  with TRegistry.Create do begin
    RootKey:=HKEY_LOCAL_MACHINE;
    if OpenKey(rk,false) then begin
      GetKeyNames(sl);
      CloseKey;
      for i:=0 to sl.count-1 do
        if OpenKey(rk+'\'+sl[i]+'\'+rkClassInfo,false) then begin
          Found:=True;
          if ValueExists(rvCIDAC) then
            FDAC.Add(ReadString(rvCIDAC));
          if ValueExists(rvCIChip) then
            FChipset.Add(ReadString(rvCIChip));
          if ValueExists(rvCIRev) then
            FChipset[FChipset.Count-1]:=FChipset[FChipset.Count-1]+' Rev '+ReadString(rvCIRev);
          if ValueExists(rvCIMem) then
            FMemory.Add(inttostr(readinteger(rvCIMem)));
          CloseKey;
        end;
    end;
    if not Found then begin
      if OpenKey(rkVideoHardware,false) then begin
        if ValueExists(rvVideoKey1) then
          rk:=ReadString(rvVideoKey1)
        else
          if ValueExists(rvVideoKey2) then
            rk:=ReadString(rvVideoKey2)
          else
            rk:='';

        closekey;
        if rk<>'' then begin
          rk:=copy(rk,pos('Machine\',rk)+8,255);
          if OpenKey(rk,false) then begin
            if ValueExists(rvHardware+'.'+rvHWVideo) then
              try
                readbinarydata(rvHardware+'.'+rvHWVideo,bdata^,255);
                FAdapter.Add(getstrfrombuf(pchar(bdata)));
              except
              end;
            if ValueExists(rvHardware+'.'+rvHWDAC) then
              try
                readbinarydata(rvHardware+'.'+rvHWDAC,bdata^,255);
                FDAC.Add(getstrfrombuf(pchar(bdata)));
              except
              end;
            if ValueExists(rvHardware+'.'+rvHWChip) then
              try
                readbinarydata(rvHardware+'.'+rvHWChip,bdata^,255);
                FChipset.Add(getstrfrombuf(pchar(bdata)));
              except
              end;
            if ValueExists(rvHardware+'.'+rvHWMem) then
              try
                idata:=stralloc(255);
                readbinarydata(rvHardware+'.'+rvHWMem,idata,4);
                FMemory.Add(inttostr(integer(idata)));
                strdispose(idata);
              except
              end;
            closekey;
          end;
        end;
      end;
    end;
    if OpenKey(rkBIOS,false) then begin
      if ValueExists(rvVideoBIOSVersion) then begin
        try
          readbinarydata(rvVideoBIOSVersion,bdata^,151);
          FBIOSVersion:=strpas(pchar(bdata));
        except
        end;
      end;
      if ValueExists(rvVideoBIOSDate) then
        FBIOSDate:=ReadString(rvVideoBIOSDate);
      closekey;
    end;
    free;
  end;

  FAcc.Clear;
  GetClassDevices(ClassKey,rv3DClass,DescValue,FAcc);
  FModes.Clear;
  i:=0;
  while EnumDisplaySettings(nil,i,Devmode) do
    with Devmode do begin
      FModes.Add(Format('%d x %d - %d bit',[dmPelsWidth,dmPelsHeight,dmBitsPerPel]));
      Inc(i);
    end;
  strdispose(bdata);
  sl.free;
end;

function Bool2YN(b :boolean) :string;
begin
  if b then
    result:='Yes'
  else
    result:='No';
end;

constructor TDisplay.Create;
begin
  inherited;
  FAdapter:=TStringList.Create;
  FModes:=TStringList.Create;
  FAcc:=TStringList.Create;
  FDAc:=TStringList.Create;
  FChipset:=TStringList.Create;
  FMemory:=TStringList.Create;
end;

destructor TDisplay.Destroy;
begin
  FAdapter.Free;
  FDAc.Free;
  FChipset.Free;
  FMemory.Free;
  FAcc.Free;
  FModes.Free;
  inherited;
end;

procedure TDisplay.Report(var sl: TStringList);
var
  i :integer;
begin
  with sl do begin
    for i:=0 to Adapter.count-1 do begin
       add(format('[%d] %s',[i+1,Adapter[i]]));
       if Chipset.count-1>=i then
         add('    Chipset: '+Chipset[i]);
        if DAC.count-1>=i then
          add('    DAC: '+DAC[i]);
        if Memory.count-1>=i then
          add('    Memory: '+Memory[i]+' B');
    end;
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
  rkDirectX = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\DirectX';
    rvDXVersionNT = 'InstalledVersion';
    rvDXVersion95 = 'Version';
  rkDirect3D = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Direct3D\Drivers';
  rkDirectPlay = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\DirectPlay\Services';
  rkDirectMusic = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\DirectMusic\SoftwareSynths';
    rvDesc = 'Description';
begin
  with TRegistry.Create do begin
    rootkey:=HKEY_LOCAL_MACHINE;
    if OpenKey(rkDirectX,false) then begin
      bdata:=stralloc(255);
      if ValueExists(rvDXVersion95) then
        FVersion:=ReadString(rvDXVersion95);
      if FVersion='' then
        if ValueExists(rvDXVersionNT) then
          try
            readbinarydata(rvDXVersionNT,bdata^,4);
            FVersion:=inttostr(lo(integer(bdata^)))+'.'+inttostr(hi(integer(bdata^)));
          except
            {$IFDEF D4PLUS}
            try
              readbinarydata(rvDXVersionNT,bdata^,8);
              FVersion:=inttostr(lo(integer(bdata^)))+'.'+inttostr(hi(integer(bdata^)));
            except
            end;
            {$ENDIF}
          end;
      closekey;
      strdispose(bdata);
    end;
    FDirect3D.Clear;
    sl:=tstringlist.create;
    if OpenKey(rkDirect3D,false) then begin
      getkeynames(sl);
      closekey;
      for i:=0 to sl.count-1 do
        if OpenKey(rkDirect3D+'\'+sl[i],false) then begin
          if ValueExists(rvDesc) then
            FDirect3D.Add(ReadString(rvDesc));
          closekey;
        end;
    end;
    sl.free;
    free;
  end;
end;


procedure TDirectX.Report(var sl: TStringList);
begin
  with sl do begin
    if Version<>'' then begin
      add('Installed version: '+Version);
      addstrings(Direct3D);
    end else
      add('Not installed.');
  end;   
end;

function IsOSNT :boolean;
var
  OS :TOSVersionInfo;
begin
  ZeroMemory(@OS,SizeOf(OS));
  OS.dwOSVersionInfoSize:=SizeOf(OS);
  GetVersionEx(OS);
  result:=OS.dwPlatformId=VER_PLATFORM_WIN32_NT;
end;

function IsOS95 :boolean;
var
  OS :TOSVersionInfo;
begin
  ZeroMemory(@OS,SizeOf(OS));
  OS.dwOSVersionInfoSize:=SizeOf(OS);
  GetVersionEx(OS);
  result:=(OS.dwMajorVersion>=4) and (OS.dwMinorVersion=0) and (OS.dwPlatformId=VER_PLATFORM_WIN32_WINDOWS);
end;

function IsOS98 :boolean;
var
  OS :TOSVersionInfo;
begin
  ZeroMemory(@OS,SizeOf(OS));
  OS.dwOSVersionInfoSize:=SizeOf(OS);
  GetVersionEx(OS);
  result:=(OS.dwMajorVersion>=4) and (OS.dwMinorVersion>0) and (OS.dwPlatformId=VER_PLATFORM_WIN32_WINDOWS);
end;

function IsOSOSR2 :boolean;
var
  OS :TOSVersionInfo;
begin
  ZeroMemory(@OS,SizeOf(OS));
  OS.dwOSVersionInfoSize:=SizeOf(OS);
  GetVersionEx(OS);
  result:=(OS.dwMajorVersion>=4) and (OS.dwMinorVersion=0) and (lo(OS.dwBuildNumber)>1000) and (OS.dwPlatformId=VER_PLATFORM_WIN32_WINDOWS);
end;

function IsOS2000 :boolean;
var
  OS :TOSVersionInfo;
begin
  ZeroMemory(@OS,SizeOf(OS));
  OS.dwOSVersionInfoSize:=SizeOf(OS);
  GetVersionEx(OS);
  result:=(OS.dwMajorVersion>=5) and (OS.dwPlatformId=VER_PLATFORM_WIN32_NT);
end;

function GetPlatform: TPlatformType;
begin
  Result:=os9x;
  if isNT then begin
    if is2000 then
      Result:=os2K
    else
      Result:=osNT4;
  end;
end;

Procedure GetCPUDetails(var s: TStringlist);
var
  c: TCPU;
begin
  c:=TCPU.Create;
  c.getInfo;
  c.report(s);
  c.free;
end;

Procedure GetDisplayDetails(var s: TStringlist);
var
  c: TDisplay;
begin
  c:=TDisplay.Create;
  c.getInfo;
  c.report(s);
  c.free;
end;

Procedure GetDirectXDetails(var s: TStringlist);
var
  c: TDirectX;
begin
  c:=TDirectX.Create;
  c.getInfo;
  c.report(s);
  c.free;
end;

Procedure GetMemoryDetails(var s: TStringlist);
var
  c: TMemory;
begin
  c:=TMemory.Create;
  c.getInfo;
  c.report(s);
  c.free;
end;

Procedure GetWorkStationDetails(var s: TStringlist);
var
  c: TWorkStation;
begin
  c:=TWorkStation.Create;
  c.getInfo;
  c.report(s);
  c.free;
end;

Procedure GetOperatingSystemDetails(var s: TStringlist);
var
  c: TOperatingSystem;
begin
  c:=TOperatingSystem.Create;
  c.getInfo;
  c.report(s);
  c.free;
end;

procedure GetPythonDetails(var S: TStringlist);
var
  R: TRegistry;
  v: string;
  installed: boolean;
begin
  R:=TRegistry.Create;
  R.RootKey:=HKEY_LOCAL_MACHINE;
  installed:=R.KeyExists('\Software\Python\PythonCore\CurrentVersion');
  if installed then begin
    R.OpenKey('\Software\Python\PythonCore\CurrentVersion', false);
    v:=R.ReadString('');
    S.Add('Version: '+v);
    R.OpenKey('\Software\Python\PythonCore\'+v+'\Dll', false);
    S.Add('Dll path: '+R.ReadString(''));
  end else begin
    S.Add('Not Installed!');
  end;
  R.free;
end;

Procedure LogSystemDetails;
var
  s: TStringlist;
  i: integer;
begin
  s:=TStringList.Create;
  s.add('CPU:');
  GetCPUDetails(s);
  s.add('');
  s.add('MEMORY:');
  GetMemoryDetails(s);
  s.add('');
  s.add('OS:');
  GetOperatingSystemDetails(s);
  s.add('');
  s.add('PYTHON:');
  GetPythonDetails(s);
  s.add('');
  s.add('MACHINE:');
  GetWorkStationDetails(s);
  s.add('');
  s.add('VIDEO:');
  GetDisplayDetails(s);
  s.add('');
  s.add('DIRECTX:');
  GetDirectxDetails(s);
  s.add('');
  for i:=0 to s.count-1 do begin
    aLog(-1,'syslog> '+s.strings[i]);
  end;
end;

initialization
  IsNT:=IsOSNT;
  IS95:=IsOS95;
  Is98:=IsOS98;
  Is2000:=IsOS2000;
  IsOSR2:=IsOSOSR2;
  WindowsUser:=GetUser;
  MachineName:=GetMachine;
  Platform:=GetPlatform;
  if IsNT then
    ClassKey:='SYSTEM\CurrentControlSet\Control\Class'
  else
    ClassKey:='SYSTEM\CurrentControlSet\Services\Class';
end.

