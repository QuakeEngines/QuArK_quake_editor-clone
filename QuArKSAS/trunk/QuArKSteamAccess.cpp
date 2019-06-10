#include <cstdlib>
#include <cassert>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <sstream>
#include <vector>
using namespace std;

#define WIN32_LEAN_AND_MEAN
#include "windows.h"

#include "..\public\tier0\basetypes.h"
#include "..\common\filesystem_tools.h"
#include "..\public\tier0\icommandline.h"
#include "..\public\tier1\utlbuffer.h"
#include "..\public\tier0\dbg.h"

ofstream Logger;
int OutputLevel = 20;
#define Log(LogLevel, msg) if (OutputLevel >= LogLevel) Logger << msg << endl;
#define VersionString "1.04"

IFileSystem * g_pFullFileSystem = NULL;

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
	stringstream tmp;
	tmp << msgtypes[spewType] << " : " << pMsg;
	Log(MessageLevel, tmp.str());
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

void AddPathDelimiter(string &str)
{
	if (str.length() > 0)
	{
		if (str[str.length() - 1] != '\\')
		{
			str.append("\\");
		}
	}
}

void RemovePathDelimiter(string &str)
{
	if (str.length() > 0)
	{
		if (str[str.length() - 1] == '\\')
		{
			str.resize(str.length() - 1);
		}
	}
}

inline void ClearStringstream(stringstream& sstream)
{
	sstream.clear();
	sstream.str("");
}

//Return codes:
// 0: succes
// 1: error
// 2: nothing to do
int main(int argc, const char* argv[])
{
	cout << "QuArK SAS (Steam Access System)" << endl;
	cout << "-------------------------------" << endl;
	cout << "Version " << VersionString << endl;
	cout << endl;
	
	//Sanity check
	assert(argc > 1);
	
	int GameCode = 220; //Default to HL2
	string GameDirectory = "";
	string OutputDirectory = "";
	vector<string> Files;
	
	Logger.open("QuArKSAS.log");
	if (!Logger.is_open())
	{
		cout << "WARNING: Couldn't open log file!" << endl;
		//return 1;
	}
	Log(0, "QuArK SAS (Steam Access System)");
	Log(0, "-------------------------------");
	Log(0, "Version 1.04");
	Log(0, "");
	
	stringstream tmp;
	string Argument;
	bool FilesFound = false;
	bool DisplayInformation = false;
	bool OverwriteFiles = false;
	for (int argIndex = 1; argIndex < argc; ++argIndex)
	{
		Argument = argv[argIndex];
		if (FilesFound)
		{
			FixPathDelimiter(Argument);
			Files.push_back(Argument);
			Log(10, Argument);
			cout << Argument << endl;
		}
		else
		{
			if (Argument == "-g")
			{
				if (argc == argIndex + 1)
				{
					ClearStringstream(tmp);
					tmp << "Not enough parameters specified: Can't read -g.";
					Log(0, tmp.str());
					cout << tmp.str() << endl;
					return 1;
				}
				++argIndex;
				GameCode = atol(argv[argIndex]);
				ClearStringstream(tmp);
				tmp << "Gamecode: " << GameCode;
				Log(40, tmp.str());
				cout << tmp.str() << endl;
			}
			else if (Argument == "-gamedir")
			{
				if (argc == argIndex + 1)
				{
					ClearStringstream(tmp);
					tmp << "Not enough parameters specified: Can't read -gamedir.";
					Log(0, tmp.str());
					cout << tmp.str() << endl;
					return 1;
				}
				++argIndex;
				Argument = argv[argIndex];
				FixPathDelimiter(Argument);
				AddPathDelimiter(GameDirectory);
				GameDirectory = Argument;
				ClearStringstream(tmp);
				tmp << "Game directory: " << GameDirectory;
				Log(40, tmp.str());
				cout << tmp.str() << endl;
			}
			//Done below (combined with "/?")
			/*else if (Argument == "-help")
			{
				DisplayInformation = true;
			}*/
			else if (Argument == "-o")
			{
				if (argc == argIndex + 1)
				{
					ClearStringstream(tmp);
					tmp << "Not enough parameters specified: Can't read -o.";
					Log(0, tmp.str());
					cout << tmp.str() << endl;
					return 1;
				}
				++argIndex;
				Argument = argv[argIndex];
				FixPathDelimiter(Argument);
				AddPathDelimiter(Argument);
				OutputDirectory = Argument;
				ClearStringstream(tmp);
				tmp << "Output directory: " << OutputDirectory;
				Log(40, tmp.str());
				cout << tmp.str() << endl;
			}
			else if (Argument == "-overwrite")
			{
				OverwriteFiles = true;
			}
			else if (Argument == "-v")
			{
				if (argc == argIndex + 1)
				{
					ClearStringstream(tmp);
					tmp << "Not enough parameters specified: Can't read -v.";
					Log(0, tmp.str());
					cout << tmp.str() << endl;
					return 1;
				}
				++argIndex;
				OutputLevel = atol(argv[argIndex]);
				if (OutputLevel < 0)
				{
					OutputLevel = 0;
				}
				if (OutputLevel > 40)
				{
					OutputLevel = 40;
				}
				ClearStringstream(tmp);
				tmp << "Output level: " << OutputLevel;
				Log(40, tmp.str());
				cout << tmp.str() << endl;
			}
			else if ((Argument == "/?") || (Argument == "-help"))
			{
				DisplayInformation = true;
			}
			else
			{
				FilesFound = true;
				cout << endl;
				Files.reserve(size_t(argc - argIndex));
				FixPathDelimiter(Argument);
				Files.push_back(Argument);
				ClearStringstream(tmp);
				tmp << "Files found:" << endl << Files[0];
				Log(10, tmp.str());
				cout << tmp.str() << endl;
			}
		}
	}
	Log(10, "");
	cout << endl;
	
	if ((argc == 1) || (DisplayInformation))
	{
		cout << "Usage:" << endl;
		cout << "QuArKSAS.exe [parameters] <filenames>" << endl;
		cout << "<filenames>: All the filenames of the files to extract." << endl;
		cout << "" << endl;
		cout << "Available parameters:" << endl;
		cout << "-g <ID>: Set Steam game number (default: 220 (HL2))." << endl;
		cout << "-gamedir <directory>: The game directory containing the data-files from which to extract." << endl;
		cout << "-help: Displays this information." << endl;
		cout << "-o <directory>: Specifies the output directory." << endl;
		cout << "-overwrite: Overwrite any existing output files." << endl;
		cout << "-v <number>: Verbose log level. 0 is least output, 40 is max." << endl;
		cout << "/?: Displays this information." << endl;
		cout << endl;
		cout << endl;
		cout << "After all parameters have been read from left to right," << endl;
		cout << "the rest will be interpreted as <filenames>." << endl;
		cout << endl;
		cout << "Note: all parameters are case-sensitive." << endl;
		cout << endl;
	}
	
	if (!FilesFound)
	{
		Log(10, "No files to extract: Shutting down.");
		return 2;
	}
	
	//If OutputDirectory is not set, use the current (working) directory
	if (OutputDirectory == "")
	{
		DWORD BufferLength = 0;
		BufferLength = GetCurrentDirectory(BufferLength, NULL);
		if (BufferLength == 0)
		{
			Log(0, "Unable to set OutputDirectory: GetCurrentDirectory (1) failed!");
			return 1;
		}
		
		unique_ptr<char[]> Buffer(new char[BufferLength]);
		if (GetCurrentDirectory(BufferLength, Buffer.get()) == 0)
		{
			Log(0, "Unable to set OutputDirectory: GetCurrentDirectory (2) failed!");
			return 1;
		}
		OutputDirectory = Buffer.get();
		Buffer.release();
	}
	
	if (OutputDirectory.length() == 0)
	{
		Log(0, "Unable to continue: no OutputDirectory set!");
		return 1;
	}
	
	//Setting up Steam error message function
	Log(40, "Setting up SpewOutputFunc...");
	SpewOutputFunc(mySpewFunc);
	Log(40, "Setting up SpewOutputFunc...Done!");
	
	//Creating a Steam command line, so it knows the game directory
	ClearStringstream(tmp);
	RemovePathDelimiter(GameDirectory);
	tmp << argv[0] << " -NoVConfig -game \"" << GameDirectory << "\"";
	Log(40, "Creating Steam Commandline...");
	CommandLine()->CreateCmdLine(tmp.str().c_str());
	Log(40, "Creating Steam Commandline... Done!");
	
	//Initializing Steam filesystem
	Log(30, "Initializing filesystem...");
#ifdef FileSystem_UseVProjectBinDir
	FileSystem_UseVProjectBinDir(true); //Added with Source SDK 2007
#endif
	if (!FileSystem_Init(NULL, 0, FS_INIT_FULL))
	{
		Log(0, "Unable to access Steam: FileSystem_Init failed!");
		return 1;
	}
	Log(30, "Initializing filesystem... Done!");
	
	//Mounting Steam content, with the given GameCode (or the default)
	//This will make Steam see the content of the GCF files
	Log(30, "Mounting filesystem content...");
	if (g_pFullFileSystem->MountSteamContent(GameCode) != FILESYSTEM_MOUNT_OK)
	{
		Log(0, "Unable to access Steam: MountSteamContent failed!");
		Log(10, "Please check if the provided gamecode is valid, and the game is installed correctly.");
		return 1;
	}
	Log(30, "Mounting filesystem content... Done!");
	
	DWORD CreationDisposition;
	if (OverwriteFiles)
	{
		CreationDisposition = CREATE_ALWAYS;
	}
	else
	{
		CreationDisposition = CREATE_NEW;
	}
	
	for (size_t z = 0; z < Files.size(); ++z)
	{
		string FilePath;
		
		unsigned int y; //Gets recycled a lot
		y = Files[z].find_last_of("\\");
		if (y == string::npos) 
		{
			FilePath = Files[z];
		}
		else
		{
			FilePath = Files[z].substr(0, y);
		}
		
		ClearStringstream(tmp);
		tmp << "Requested file: " << Files[z].c_str();
		Log(30, tmp.str());
		ClearStringstream(tmp);
		tmp << "Requested file's directory: " << FilePath;
		Log(40, tmp.str());
		
		FileFindHandle_t SteamFileHandle;
		const char* RealFileName = g_pFullFileSystem->FindFirst(Files[z].c_str(), &SteamFileHandle);
		if (RealFileName == NULL)
		{
			ClearStringstream(tmp);
			tmp << "Unable to extract file: " << Files[z] << ". FindFirst failed!";
			Log(0, tmp.str());
			continue;
		}
		
		//FindFirst deletes the path, so we put it back
		string RealFullFileName = FilePath + "\\";
		RealFullFileName.append(RealFileName);
		
		if (OutputLevel == 40)
		{
			ClearStringstream(tmp);
			tmp << "Trying to read file " << z << ": " << RealFullFileName << ".";
			Log(40, tmp.str());
		}
		
		CUtlBuffer FileBuffer;
		if (!g_pFullFileSystem->ReadFile(RealFullFileName.c_str(), NULL, FileBuffer))
		{
			ClearStringstream(tmp);
			tmp << "Unable to extract file: " << RealFullFileName << ". ReadFile failed!";
			Log(0, tmp.str());
			continue;
		}
		FileBuffer.SeekGet(FileBuffer.SEEK_HEAD, 0);
		FileBuffer.SeekPut(FileBuffer.SEEK_HEAD, 0);
		int BufferSize = FileBuffer.GetBytesRemaining();
		assert(BufferSize >= 0);
		
		unique_ptr<char[]> MemBuffer(new char[size_t(BufferSize)]);
		FileBuffer.Get(MemBuffer.get(), BufferSize);
		FileBuffer.Purge();
		
		string OutputFileName = OutputDirectory + Files[z];
		
		ClearStringstream(tmp);
		tmp << "Output filename: " << OutputFileName;
		Log(20, tmp.str());
		
		if (OutputLevel == 40)
		{
			ClearStringstream(tmp);
			tmp << "Trying to write file " << z << ": " << OutputFileName << ".";
			Log(40, tmp.str());
		}
		
		//Check if the FilePath exists, and if not, create it!
		string FindFilePath = "";
		bool DirFound = false;
		y = 0;
		Log(30, "Checking path's existence...");
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
			WIN32_FIND_DATA FindDirectoryData;
			HANDLE FindDirectory = FindFirstFile((OutputDirectory + FindFilePath).c_str(), &FindDirectoryData);
			if (FindDirectory == INVALID_HANDLE_VALUE)
			{
				ClearStringstream(tmp);
				tmp << "Can't find output directory-part " << (OutputDirectory + FindFilePath) << ": Creating it...";
				Log(20, tmp.str());
				//Some error occurred. Probably can't find the directory, so let's create it
				if (CreateDirectory((OutputDirectory + FindFilePath).c_str(), NULL) == 0)
				{
					ClearStringstream(tmp);
					tmp << "Unable to extract file: " << RealFullFileName << ". CreateDirectory failed!";
					Log(0, tmp.str());
					DirFound = true;
					break;
				}
				Log(20, "Directory creating: Done!");
			}
			else
			{
				if (FindClose(FindDirectory) == 0)
				{
					ClearStringstream(tmp);
					tmp << "Warning: FindClose (Dir) failed!";
					Log(10, tmp.str());
				}
			}
		}
		Log(30, "Checking path's existence... Done!");
		if (DirFound)
		{
			continue;
		}
		
		HANDLE hFile = CreateFile(OutputFileName.c_str(), GENERIC_WRITE, 0, NULL, CreationDisposition, FILE_ATTRIBUTE_NORMAL, NULL);
		if (hFile == INVALID_HANDLE_VALUE)
		{
			ClearStringstream(tmp);
			tmp << "Unable to extract file: " << RealFullFileName << ". CreateFile failed!";
			Log(0, tmp.str());
			continue;
		}
		
		DWORD BytesWritten;
		if (WriteFile(hFile, MemBuffer.get(), DWORD(BufferSize), &BytesWritten, NULL) == 0)
		{
			ClearStringstream(tmp);
			tmp << "Unable to extract file: " << RealFullFileName << ". WriteFile failed!";
			Log(0, tmp.str());
			CloseHandle(hFile);
			continue;
		}
		
		MemBuffer.release();
		
		if (FlushFileBuffers(hFile) == 0)
		{
			ClearStringstream(tmp);
			tmp << "Unable to extract file: " << RealFullFileName << ". FlushFileBuffers failed!";
			Log(0, tmp.str());
			CloseHandle(hFile);
			continue;
		}
		
		if (CloseHandle(hFile) == 0)
		{
			ClearStringstream(tmp);
			tmp << "Unable to extract file: " << RealFullFileName << ". CloseHandle failed!";
			Log(0, tmp.str());
			continue;
		}
		
		g_pFullFileSystem->FindClose(SteamFileHandle);
	}
	Log(0, "Done extracting files!");
	
	g_pFullFileSystem->Shutdown();
	g_pFullFileSystem = NULL;
	Log(40, "Filesystem shutdown completed.");
	
	Files.clear();
	Log(20, "Shutdown completed.");
	Logger.close();
	
	return 0;
}
