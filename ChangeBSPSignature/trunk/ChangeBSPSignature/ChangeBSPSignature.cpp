// ChangeBSPSignature
// Version 1.0
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

int GameNR = 2;
char* Game [2] = { "Alice", "FAKK2" };
char* GameName [2] = { "American McGee's Alice", "Heavy Metal: FAKK2" };
byte* GameSignature [2] = { "FAKK\x2A\x00\x00\x00", "FAKK\x0C\x00\x00\x00" };

int main(int argc, char* argv[])
{
	bool DisplayUsage = false;
	cout << "ChangeBSPSignature - version 1.0" << endl;
	cout << endl;
	if (argc == 2)
	{
		if (argv[2] == "-games")
		{
			cout << "Supported games:" << endl;
			int i;
			for (i = 0; i < GameNR; i++)
			{
				cout << "'" << Game[i] << "' = " << GameName[i] << endl;
			}
			return 0;
		}
	}
	if (argc != 3)
	{
		cout << "Wrong number of arguments!" << endl;
		DisplayUsage = true;
	}
	if (DisplayUsage)
	{
		cout << "Usage:" << endl;
		cout << "ChangeBSPSignature bspfile game" << endl;
		cout << endl;
		cout << "Type 'ChangeBSPSignature -games' (without quotes) for a list of all supported games." << endl;
		cout << endl;
		return 1;
	}
	int i;
	int WantGameMode = -1;
	for (i = 0; i < GameNR; i++)
	{
		if (!strcmp(argv[2], Game[i]))
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
	fstream BSPFile;
	BSPFile.open(argv[1], ios::in | ios::out | ios::binary);
	if (!BSPFile.is_open())
	{
		cout << "Unable to open BSP file." << endl;
		cout << endl;
		return 1;
	}
	byte* Buffer;
	Buffer = new byte [8];
	BSPFile.seekg(0, ios::beg);
	BSPFile.read(Buffer, 8);

	int FileGameMode = -1;
	for (i = 0; i < GameNR; i++)
	{
		/*cout << int(Buffer[0]) << " : " << int(GameSignature[i][0]) << endl;
		cout << int(Buffer[1]) << " : " << int(GameSignature[i][1]) << endl;
		cout << int(Buffer[2]) << " : " << int(GameSignature[i][2]) << endl;
		cout << int(Buffer[3]) << " : " << int(GameSignature[i][3]) << endl;
		cout << int(Buffer[4]) << " : " << int(GameSignature[i][4]) << endl;
		cout << int(Buffer[5]) << " : " << int(GameSignature[i][5]) << endl;
		cout << int(Buffer[6]) << " : " << int(GameSignature[i][6]) << endl;
		cout << int(Buffer[7]) << " : " << int(GameSignature[i][7]) << endl;*/
		if (!memcmp(Buffer, GameSignature[i], 8))
		{
			FileGameMode = i;
			break;
		}
	}
	if (FileGameMode == -1)
	{
		cout << "Cannot recognize game. This might not be a BSP file, or the game for which it is written is not supported by ChangeBSPSignature. Press any key to continue, or ESCAPE to terminate." << endl;
		int key = getch();
		if (key == 27)
		{
			cout << "Program terminated." << endl;
			delete [] Buffer;
			BSPFile.close();
			return 0;
		}
		cout << "BSP file game: UNKNOWN" << endl;
	}
	else
	{
		cout << "BSP file game: " << GameName[FileGameMode] << endl;
	}
	if (WantGameMode == FileGameMode)
	{
		cout << "BSP file game already correct. No changes made." << endl;
	}
	else
	{
		cout << "Changing BSP file game...";
		BSPFile.seekp(0, ios::beg);
		BSPFile.write(GameSignature[WantGameMode], 8);
		cout << " done!" << endl;
	}
	delete [] Buffer;
	BSPFile.close();
	return 0;
}
