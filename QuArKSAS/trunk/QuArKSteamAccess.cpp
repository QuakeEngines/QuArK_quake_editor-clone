#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <sstream>
#include <windows.h>

#include "..\public\tier0\basetypes.h"
#include "..\common\filesystem_tools.h"
#include "..\vstdlib\ICommandLine.h"
#include "..\public\tier1\utlbuffer.h"
#include "..\public\tier0\dbg.h"

#include "logger.h"
using namespace std;

IFileSystem * g_pFullFileSystem = NULL;
static Logger* sLog = NULL;

inline std::string stringify(double x)
{
  stringstream o;
  if (!(o << x))
    throw;
  return o.str();
}

inline std::string stringify(int x)
{
  stringstream o;
  if (!(o << x))
    throw;
  return o.str();
}

//SpewType_t descriptions:
char* msgtypes[SPEW_TYPE_COUNT] ={"SPEW_MESSAGE",
                  "SPEW_WARNING",
                  "SPEW_ASSERT",
                  "SPEW_ERROR",
                  "SPEW_LOG"};

SpewRetval_t mySpewFunc( SpewType_t spewType, tchar const *pMsg )
{
  int MessageLevel;
  switch (spewType)
  {
    case SPEW_MESSAGE:
      MessageLevel = 40;
      break;
    case SPEW_WARNING:
      MessageLevel = 10;
      break;      
    case SPEW_ASSERT:
      MessageLevel = 20;
      break;
    case SPEW_ERROR:
      MessageLevel = 0;
      break;
    case SPEW_LOG: 
      MessageLevel = 20;
      break;
    default:
      MessageLevel = 0;
  }
  sLog->msg(MessageLevel, "%s : %s\n", msgtypes[spewType], pMsg);
  return SPEW_CONTINUE;
}

void FixPathDelimiter(string &str)
{
  unsigned int y;
  y = str.find("/");
  while (y != string::npos)
  {
    str.replace(y, 1, "\\");
    y = str.find("/");
  }
}

//Return codes:
// 0: succes
// 1: error
// 2: nothing to do
int main(int argc, const char* argv[])
{
  printf("\n");
  printf("QuArK SAS (Steam Access System)\n");
  printf("-------------------------------\n");
  printf("Version 1.0\n");
  printf("\n");

  sLog = new Logger("QuArKSAS.log");
  sLog->msg(20,"QuArK SAS (Steam Access System)\n");
  sLog->msg(20,"-------------------------------\n");
  sLog->msg(20,"Version 1.0\n");
  sLog->msg(20,"\n");

  int GameCode = 220; //Default to HL2
  string GameDirectory = "";
  string OutputDirectory = "";
  int OutputLevel = 40; //Debug set to 40, normal default 20
  int FileNR = 0;
  int curFile = 0;
  string* Files = NULL;

  string Argument;
  bool FilesFound = false;
  bool DisplayInformation = false;
  bool OverwriteFiles = false;
  int z;
  for (z = 1; z < argc; z++)
  {
    Argument = argv[z];
    if (FilesFound)
    {
      curFile++;
      FixPathDelimiter(Argument);
      Files[curFile] = Argument;
      printf(Files[curFile].c_str());
      printf("\n");
    }
    else
    {
      if (Argument == "-g")
      {
        if (argc == z + 1)
        {
          printf("Not enough parameters specified: Can't read -g.\n");
          printf("\n");
          return 1;
        }
        z++;
        GameCode = atol(argv[z]);
        printf("Gamecode: ");
        printf(stringify(GameCode).c_str());
        printf("\n");
      }
      else if (Argument == "-gamedir")
      {
        if (argc == z + 1)
        {
          printf("Not enough parameters specified: Can't read -gamedir.\n");
          printf("\n");
          return 1;
        }
        z++;
        Argument = argv[z];
        FixPathDelimiter(Argument);
        if (!(Argument.substr(Argument.length() - 1, 1) == "\\"))
        {
          Argument.append("\\");
        }
        GameDirectory = Argument;
        printf("Game directory: ");
        printf(GameDirectory.c_str());
        printf("\n");
      }
      //Done below (combined with "/?")
      /*else if (Argument == "-help")
      {
        DisplayInformation = true;
      }*/
      else if (Argument == "-o")
      {
        if (argc == z + 1)
        {
          printf("Not enough parameters specified: Can't read -o.\n");
          printf("\n");
          return 1;
        }
        z++;
        Argument = argv[z];
        FixPathDelimiter(Argument);
        if (!(Argument.substr(Argument.length() - 1, 1) == "\\"))
        {
          Argument.append("\\");
        }
        OutputDirectory = Argument;
        printf("Output directory: ");
        printf(OutputDirectory.c_str());
        printf("\n");
      }
      else if (Argument == "-overwrite")
      {
        OverwriteFiles = true;
      }
      else if (Argument == "-v")
      {
        if (argc == z + 1)
        {
          printf("Not enough parameters specified: Can't read -v.\n");
          printf("\n");
          return 1;
        }
        z++;
        OutputLevel = atol(argv[z]);
        sLog->setlevel(OutputLevel);
        printf("Output level: ");
        printf(stringify(OutputLevel).c_str());
        printf("\n");
      }
      else if ((Argument == "/?") || (Argument == "-help"))
      {
        DisplayInformation = true;
      }
      else
      {
        FilesFound = true;
        printf("\n");
        printf("Files found:\n");
        FileNR = argc - z + 1;
        Files = new string[FileNR - 1];
        FixPathDelimiter(Argument);
        Files[0] = Argument;
        printf(Files[0].c_str());
        printf("\n");
      }
    }
  }
  printf("\n");

  if ((argc == 1) || (DisplayInformation))
  {
    printf("Usage:\n");
    printf("QuArKSAS.exe [parameters] <filenames>\n");
    printf("<filenames>: All the filenames of the files to extract.\n");
    printf("\n");
    printf("Available parameters:\n");
    printf("-g <ID>: Set Steam game number (default: 220 (HL2)).\n");
    printf("-gamedir <directory>: The game directory containing the to extract files.\n");
    printf("-help: Displays this information.\n");
    printf("-o <directory>: Specifies the output directory.\n");
    printf("-overwrite: Overwrite any existing output files.\n");
    printf("-v <number>: Verbose log level. 0 is least output, 40 is max.\n");
    printf("/?: Displays this information.\n");
    printf("\n");
    printf("\n");
    printf("After all parameters have been read from left to right,\n");
    printf("the rest will be interpreted as <filenames>.\n");
    printf("\n");
    printf("Note: all parameters are case-sensitive.\n");
    printf("\n");
  }

  if (!FilesFound)
  {
    sLog->msg(10, "No files to extract: Shutting down.\n");
    delete sLog;
    return 2;
  }

  //If OutputDirectory is not set, use the current (working) directory
  if (OutputDirectory == "")
  {
    DWORD BufferLength = 0;
    char *Buffer;
    BufferLength = GetCurrentDirectory(BufferLength, NULL);
    if (BufferLength == 0)
    {
      sLog->msg(0, "Unable to set OutputDirectory: GetCurrentDirectory (1) failed!");
      delete sLog;
      return 1;
    }
    
    Buffer = new char[BufferLength];
    if (GetCurrentDirectory(BufferLength, Buffer) == 0)
    {
      sLog->msg(0, "Unable to set OutputDirectory: GetCurrentDirectory (2) failed!");
      delete [] Buffer;
      delete sLog;
      return 1;
    }
    OutputDirectory = Buffer;
    delete [] Buffer;
  }

  if (OutputDirectory.length() == 0)
  {
    sLog->msg(0, "Unable to continue: no OutputDirectory set!");
    delete sLog;
    return 1;
  }

  //Setting up Steam error message function
  sLog->msg(40,"Setting up SpewOutputFunc... ");
  SpewOutputFunc(mySpewFunc);
  sLog->msg(40,"Done!\n");

  //Creating a Steam command line, so it knows the game directory
  Argument = "-game ";
  Argument.append(GameDirectory);
  CommandLine()->CreateCmdLine(Argument.c_str());
  sLog->msg(40,"Commandline created.\n");

  //Initializing Steam filesystem
  sLog->msg(30,"Initializing filesystem... ");
  if (!FileSystem_Init( NULL, 0, FS_INIT_FULL))
  {
    sLog->msg(0, "Unable to access Steam: FileSystem_Init failed!");
    delete [] Files;
    delete sLog;
    return 1;
  }
  sLog->msg(30,"Done!\n");

  //Mounting Steam content, with the given GameCode (or the default)
  //This will make Steam see the content of the GCF files
  sLog->msg(30,"Mounting filesystem content... ");
  if (!g_pFullFileSystem->MountSteamContent(GameCode) == FILESYSTEM_MOUNT_OK)
  {
    sLog->msg(0, "Unable to access Steam: MountSteamContent failed!");
    sLog->msg(10, "  Please check if the provided gamecode is valid, and the game is installed correctly.");
    delete [] Files;
    delete sLog;
    return 1;
  }
  sLog->msg(30,"Done!\n");

  string FilePath;
  string FindFilePath;
  const char *RealFileName;
  string RealFullFileName;
  FileFindHandle_t SteamFileHandle;
  CUtlBuffer FileBuffer;
  char* MemBuffer;
  int BufferSize;
  string OutputFileName;
  HANDLE hFile;
  DWORD CreationDisposition;
  DWORD BytesWritten;

  if (OverwriteFiles)
  {
    CreationDisposition = CREATE_ALWAYS;
  }
  else
  {
    CreationDisposition = CREATE_NEW;
  }

  bool DirFound;
  HANDLE FindDirectory;
  WIN32_FIND_DATA FindDirectoryData;
  unsigned int y;
  for (z = 0; z < FileNR - 1; z++)
  {
    y = Files[z].find_last_of("\\");
    if (y == string::npos) 
    {
      FilePath = Files[z];
    }
    else
    {
      FilePath = Files[z].substr(0, y);
    }

    sLog->msg(30, "Requested file: ");
    sLog->msg(30, Files[z].c_str());
    sLog->msg(30, "\n");
    sLog->msg(40, "Requested file's directory: ");
    sLog->msg(40, FilePath.c_str());
    sLog->msg(40, "\n");

    RealFileName = g_pFullFileSystem->FindFirst(Files[z].c_str(), &SteamFileHandle);
    if (RealFileName == NULL)
    {
      Argument = "Unable to extract file: ";
      Argument.append(Files[z]);
      Argument.append(". FindFirst failed!\n");
      sLog->msg(0,Argument.c_str());
      continue;
    }

    //FindFirst deletes the path, so we put it back
    RealFullFileName = FilePath + "\\";
    RealFullFileName.append(RealFileName);

    if (OutputLevel == 40)
    {
      Argument = "Trying to read file ";
      Argument.append(stringify(z));
      Argument.append(": ");
      Argument.append(RealFullFileName.c_str());
      Argument.append(".\n");
      sLog->msg(40,Argument.c_str());
    }

    if (!g_pFullFileSystem->ReadFile(RealFullFileName.c_str(), NULL, FileBuffer))
    {
      Argument = "Unable to extract file: ";
      Argument.append(RealFullFileName.c_str());
      Argument.append(". ReadFile failed!\n");
      sLog->msg(0, Argument.c_str());
      continue;
    }
    FileBuffer.SeekGet(FileBuffer.SEEK_HEAD, 0);
    FileBuffer.SeekPut(FileBuffer.SEEK_HEAD, 0);
    BufferSize = FileBuffer.GetBytesRemaining();

    MemBuffer = new char[BufferSize];
    FileBuffer.Get(MemBuffer, BufferSize);
    FileBuffer.Purge();

    OutputFileName = OutputDirectory + Files[z];

    sLog->msg(20, "Output filename: ");
    sLog->msg(20, OutputFileName.c_str());
    sLog->msg(20, "\n");

    if (OutputLevel == 40)
    {
      Argument = "Trying to write file ";
      Argument.append(stringify(z));
      Argument.append(": ");
      Argument.append(OutputFileName);
      Argument.append(".\n");
      sLog->msg(40,Argument.c_str());
    }

    //Check if the FilePath exists, and if not, create it!
    FindFilePath = "";
    DirFound = false;
    y = 0;
    sLog->msg(30, "Checking path's existance... ");
    while (FindFilePath != FilePath)
    {
      if (FindFilePath == "")
      {
        y = FilePath.find("\\", y);
      }
      else
      {
        y = FilePath.find("\\", y + 1);
      }
      if (y == string::npos)
      {
        //Not needed...
        //y = FilePath.length();
        FindFilePath = FilePath;
      }
      else
      {
        FindFilePath = FilePath.substr(0, y);
      }
      FindDirectory = FindFirstFile((OutputDirectory + FindFilePath).c_str(), &FindDirectoryData);
      if (FindDirectory == INVALID_HANDLE_VALUE)
      {
        sLog->msg(20, "Can't find output directory-part ");
        sLog->msg(20, (OutputDirectory + FindFilePath).c_str());
        sLog->msg(20, ": Creating it... ");
        //Some error occured. Probably can't find the dir, so let's create it
        if (CreateDirectory((OutputDirectory + FindFilePath).c_str(), NULL) == 0)
        {
          Argument = "Unable to extract file: ";
          Argument.append(RealFullFileName.c_str());
          Argument.append(". CreateFile (Dir) failed!\n");
          sLog->msg(0, Argument.c_str());
          DirFound = true;
          break;
        }
        sLog->msg(20, "Done!\n");
      }
      else
      {
        if (FindClose(FindDirectory) == 0)
        {
          Argument = "Warning: FindClose (Dir) failed!\n";
          sLog->msg(10, Argument.c_str());
        }
      }
    }
    sLog->msg(30, "Done!\n");
    if (DirFound)
    {
      delete [] MemBuffer;
      continue;
    }

    hFile = CreateFile(OutputFileName.c_str(), GENERIC_WRITE, 0, NULL, CreationDisposition, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile == INVALID_HANDLE_VALUE)
    {
      Argument = "Unable to extract file: ";
      Argument.append(RealFullFileName.c_str());
      Argument.append(". CreateFile failed!\n");
      sLog->msg(0, Argument.c_str());
      delete [] MemBuffer;
      continue;
    }

    if (WriteFile(hFile, MemBuffer, BufferSize, &BytesWritten, NULL) == 0)
    {
      Argument = "Unable to extract file: ";
      Argument.append(RealFullFileName.c_str());
      Argument.append(". WriteFile failed!\n");
      sLog->msg(0, Argument.c_str());
      CloseHandle(hFile);
      delete [] MemBuffer;
      continue;
    }

    delete [] MemBuffer;

    if (FlushFileBuffers(hFile) == 0)
    {
      Argument = "Unable to extract file: ";
      Argument.append(RealFullFileName.c_str());
      Argument.append(". FlushFileBuffers failed!\n");
      sLog->msg(0, Argument.c_str());
      CloseHandle(hFile);
      continue;
    }

    if (CloseHandle(hFile) == 0)
    {
      Argument = "Unable to extract file: ";
      Argument.append(RealFullFileName.c_str());
      Argument.append(". CloseHandle failed!\n");
      sLog->msg(0, Argument.c_str());
      continue;
    }

    g_pFullFileSystem->FindClose(SteamFileHandle);
  }
  sLog->msg(0, "Done extracting files!\n");

  g_pFullFileSystem->Shutdown();
  sLog->msg(40,"Filesystem shutdown completed.\n");
  delete [] Files;

  sLog->msg(20, "Shutdown completed.\n");
  delete sLog;
  return 0;
}