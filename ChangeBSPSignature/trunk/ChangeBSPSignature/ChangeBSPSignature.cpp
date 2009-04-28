// ChangeBSPSignature
// Version 1.1
// 28 April 2009
// (c) Copyright 2009, DanielPharos

// This program changes the signature on BSP files.
// It cannot CONVERT different BSP file types.
// It was written so QuArK can use the Heavy Metal: FAKK2 compiler for American McGee's Alice.

#include <iostream>
#include <fstream>
#include <string.h>
#include <conio.h>
using namespace std;

// This is supposed to be a single byte datatype:
#define byte char

struct cGame
{
	char* ID;
	char* Name;
	byte* Signature;
	bool HasVersion;
	byte* Version;
};

const int GameNR = 10;
const cGame GameList [GameNR] = {
	{ "Alice", "American McGee's Alice", "FAKK", true, "\x2A\x00\x00\x00" },
	{ "DK", "Daikatana", "\x49\x42\x53\x50", true, "\x29\x00\x00\x00" },
	{ "HL", "Half-Life", "\x1E\x00\x00\x00", false, "\x00\x00\x00\x00" },
	{ "FAKK2", "Heavy Metal: FAKK2", "FAKK", true, "\x0C\x00\x00\x00" },
	{ "Q1", "Quake 1 or Hexen II", "\x1D\x00\x00\x00", false, "\x00\x00\x00\x00" },
	{ "Q2", "Quake 2", "\x49\x42\x53\x50", true, "\x26\x00\x00\x00" },
	{ "Q3", "Quake 3 or Nexuiz", "\x49\x42\x53\x50", true, "\x2E\x00\x00\x00" },
	{ "RTCW", "Return to Castle Wolfenstein", "\x49\x42\x53\x50", true, "\x2F\x00\x00\x00" },
	{ "SiN", "SiN or Star Wars: Jedi Knight 2 or Star Wars: Jedi Academy", "\x52\x42\x53\x50", true, "\x01\x00\x00\x00" },
	{ "W", "Warsow", "\x46\x42\x53\x50", true, "\x01\x00\x00\x00" }
};

/* // Note: Quake-3 and STVEF .BSPs, uses the same signature as Quake-2 .BSPs!
 cSignatureBspQ3      = $50534252; {"RBSP" 4-letter header}
 cSignatureMohaa      = $35313032;

(***********  Half-Life 2 .bsp format  ***********)

const
 cSignatureHL2        = $50534256; {"VBSP" 4-letter header, which HL2 contains}

 cVersionBspHL2       = $00000013; {HL2}
 cVersionBspHL2HDR    = $00000014; {HL2 with HDR lighting}*/

int main(int argc, char* argv[])
{
	bool DisplayUsage = false;
	cout << "ChangeBSPSignature - version 1.1" << endl;
	cout << endl;
	if (argc == 2)
	{
		if (!strcmp(argv[1], "-games"))
		{
			cout << "Supported games:" << endl;
			int i;
			for (i = 0; i < GameNR; i++)
			{
				cout << "\"" << GameList[i].ID << "\" = " << GameList[i].Name << endl;
			}
			return 0;
		}
	}
	if ((argc < 2) || (argc > 4))
	{
		cout << "Wrong number of arguments!" << endl;
		DisplayUsage = true;
	}
	if (DisplayUsage)
	{
		cout << "Usage:" << endl;
		cout << "ChangeBSPSignature bspfile [WantedGame [FileGame]]" << endl;
		cout << endl;
		cout << "Type 'ChangeBSPSignature -games' (without quotes) for a list of all supported games." << endl;
		cout << endl;
		return 1;
	}
	int i;
	int WantGameMode = -1;
	if (argc > 2)
	{
		for (i = 0; i < GameNR; i++)
		{
			if (!strcmp(argv[2], GameList[i].ID))
			{
				WantGameMode = i;
				break;
			}
		}
		if (WantGameMode == -1)
		{
			cout << "Invalid second parameter. Game not recognized." << endl;
			cout << "Type 'ChangeBSPSignature -games' (without quotes) for a list of all supported games." << endl;
			cout << endl;
			return 1;
		}
	}
	int FileGameMode = -1;
	if (argc > 3)
	{
		for (i = 0; i < GameNR; i++)
		{
			if (!strcmp(argv[3], GameList[i].ID))
			{
				FileGameMode = i;
				break;
			}
		}
	}
	fstream BSPFile;
	ios_base::openmode FileMode;
	if (WantGameMode == -1)
	{
		FileMode = ios::in | ios::binary;
	}
	else
	{
		FileMode = ios::in | ios::out | ios::binary;
	}
	BSPFile.open(argv[1], FileMode);
	if (!BSPFile.is_open())
	{
		cout << "Unable to open BSP file." << endl;
		cout << endl;
		return 1;
	}
	byte* BufferSignature;
	BufferSignature = new byte [4];
	BSPFile.seekg(0, ios::beg);
	BSPFile.read(BufferSignature, 4);
	byte* BufferVersion = NULL;
	if (GameList[FileGameMode].HasVersion)
	{
		BufferVersion = new byte [4];
		BSPFile.read(BufferVersion, 4);
	}
	if (FileGameMode == -1)
	{
		for (i = 0; i < GameNR; i++)
		{
			/*cout << int(BufferSignature[0]) << " : " << int(GameList[i].Signature[0]) << endl;
			cout << int(BufferSignature[1]) << " : " << int(GameList[i].Signature[1]) << endl;
			cout << int(BufferSignature[2]) << " : " << int(GameList[i].Signature[2]) << endl;
			cout << int(BufferSignature[3]) << " : " << int(GameList[i].Signature[3]) << endl;
			cout << int(BufferVersion[0]) << " : " << int(GameList[i].Version[0]) << endl;
			cout << int(BufferVersion[1]) << " : " << int(GameList[i].Version[1]) << endl;
			cout << int(BufferVersion[2]) << " : " << int(GameList[i].Version[2]) << endl;
			cout << int(BufferVersion[3]) << " : " << int(GameList[i].Version[3]) << endl;*/
			if (!memcmp(BufferSignature, GameList[i].Signature, 4))
			{
				if (GameList[i].HasVersion)
				{
					if (!memcmp(BufferVersion, GameList[i].Version, 4))
					{
						FileGameMode = i;
						break;
					}
				}
				else
				{
					FileGameMode = i;
					break;
				}
			}
		}
	}
	if (FileGameMode == -1)
	{
		cout << "Couldn't recognize BSP file game. This might not be a valid BSP file, or the game for which it is written is not supported by ChangeBSPSignature. Program terminated." << endl;
		delete [] BufferSignature;
		if (BufferVersion != NULL)
		{
			delete [] BufferVersion;
		}
		BSPFile.close();
		return 1;
	}
	cout << "BSP file game: " << GameList[FileGameMode].Name << endl;
	if (WantGameMode == -1)
	{
		cout << "No changes made." << endl;
		delete [] BufferSignature;
		if (BufferVersion != NULL)
		{
			delete [] BufferVersion;
		}
		BSPFile.close();
		return 0;
	}
	if (WantGameMode == FileGameMode)
	{
		cout << "BSP file game already correct. No changes made." << endl;
	}
	else
	{
		cout << "Changing BSP file game...";
		BSPFile.seekp(0, ios::beg);
		BSPFile.write(GameList[WantGameMode].Signature, 4);
		if (GameList[WantGameMode].HasVersion)
		{
			BSPFile.write(GameList[WantGameMode].Version, 4);
		}
		cout << " done!" << endl;
	}
	delete [] BufferSignature;
	if (BufferVersion != NULL)
	{
		delete [] BufferVersion;
	}
	BSPFile.close();
	return 0;
}
