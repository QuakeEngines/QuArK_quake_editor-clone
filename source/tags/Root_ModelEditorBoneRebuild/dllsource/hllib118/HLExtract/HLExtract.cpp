#include "..\Lib\HLLib.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <string>
#include <iostream>
#include <vector>

BOOL bSilent = FALSE;

void PrintUsage()
{
	printf("Correct HLExtract usage:\n");
	printf(" -p <filepath>       (Package to load.)\n");
	printf(" -d <path>           (Destination extraction directory.)\n");
	printf(" -e <itempath>       (Item in package to extract.)\n");
	printf(" -l[d][f] [filepath] (List the contents of the package.)\n");
	printf(" -c                  (Console mode.)\n");
	printf(" -s                  (Silent mode.)\n");
	printf(" -m                  (Use file mapping.)\n");
	printf(" -v                  (Allow volatile access.)\n");
	printf("\n");
	printf("Example HLExtract usage:\n");
	printf("HLExtract.exe -p \"C:\\half-life.gcf\" -d \"C:\\backup\" -e \"root\\valve\\models\" -e \"root\\valve\\config.cfg\"\n");
	printf("HLExtract.exe -p \"C:\\half-life.gcf\" -c -m -v -s\n");
	printf("HLExtract.exe -p \"C:\\half-life.gcf\" -lf \"C:\\half-life.txt\" -m\n");
}

//
// CallBack()
// PackageUtility::Extract() callback.  If bResult is false szMessage contains an error message.
//
void CallBack(BOOL bResult, LPCSTR szMessage)
{
	printf("  %s\n", szMessage);
}

void SilentCallBack(BOOL bResult, LPCSTR szMessage)
{
	if(!bResult || (bResult && !bSilent))
	{
		printf("  %s\n", szMessage);
	}
}

//
// List()
// List the full path of all items in Item.
//
void List(FILE *pFile, CPackageUtility &PackageUtility, CDirectoryItem *Item, BOOL bListFolders, BOOL bListFiles)
{
	char cPath[512] = "";

	if(Item->GetType() == DirectoryItemFolder)
	{
		if(bListFolders)
		{
			PackageUtility.GetDirectoryItemPath(Item, cPath);
			fprintf(pFile, "%s\n", cPath);
		}

		CDirectoryFolder *Folder = static_cast<CDirectoryFolder *>(Item);
		for(DWORD i = 0; i < Folder->ItemCount(); i++)
		{
			List(pFile, PackageUtility, Folder->GetItem(i), bListFolders, bListFiles);
		}
	}
	else if(Item->GetType() == DirectoryItemFile)
	{
		if(bListFiles)
		{
			PackageUtility.GetDirectoryItemPath(Item, cPath);
			fprintf(pFile, "%s\n", cPath);
		}
	}
}

//
// Search()
// Search for all items containing cSearchWord.
//
void Search(const char *cSearchWord, int &iCount, CPackageUtility &PackageUtility, CDirectoryItem *Item)
{
	// Check if the item we are searching contains teh search word.
	if(strstr(Item->GetName(), cSearchWord) != 0)
	{
		// It does, get its full path.
		char cPath[512] = "";
		PackageUtility.GetDirectoryItemPath(Item, cPath);

		// Print the path.
		iCount++;
		printf("  Found %s: %s\n", Item->GetType() == DirectoryItemFolder ? "folder" : "file", cPath);
	}

	// If the item is a folder, search its sub items.
	if(Item->GetType() == DirectoryItemFolder)
	{
		CDirectoryFolder *Folder = static_cast<CDirectoryFolder *>(Item);
		for(DWORD i = 0; i < Folder->ItemCount(); i++)
		{
			Search(cSearchWord, iCount, PackageUtility, Folder->GetItem(i));
		}
	}
}

int main(int argc, char* argv[])
{
	if(GetHLLibVersion() != HLLIB_VERSION)
	{
		printf("Wrong HLLib version.\n");
		return 1;
	}

	char *cPackage = 0;
	char *cDestination = 0;
	std::vector<char *> Items;
	char *cList = 0;

	BOOL bList = FALSE;
	BOOL bListFolders = FALSE;
	BOOL bListFiles = FALSE;

	BOOL bConsoleMode = FALSE;
	BOOL bFileMapping = FALSE;
	BOOL bVolatileAccess = FALSE;

	// Process switches.
	for(int i = 1; i < argc; i++)
	{
		if(stricmp(argv[i], "-p") == 0)
		{
			if(cPackage == 0 && i + 1 < argc)
			{
				cPackage = argv[++i];
			}
			else
			{
				PrintUsage();
				return 2;
			}
		}
		else if(stricmp(argv[i], "-d") == 0)
		{
			if(cDestination == 0 && i + 1 < argc)
			{
				cDestination = argv[++i];
			}
			else
			{
				PrintUsage();
				return 2;
			}
		}
		else if(stricmp(argv[i], "-e") == 0)
		{
			if(i + 1 < argc)
			{
				Items.push_back(argv[++i]);
			}
			else
			{
				PrintUsage();
				return 2;
			}
		}
		else if(strnicmp(argv[i], "-l", 2) == 0)
		{
			if(bList)
			{
				PrintUsage();
				return 2;
			}

			bList = TRUE;

			if(stricmp(argv[i], "-l") == 0)
			{
				// By default list everything.
				bListFolders = TRUE;
				bListFiles = TRUE;
			}
			else
			{
				// List folders and files if specified.
				bListFolders = strcspn(argv[i], "dD") != strlen(argv[i]);
				bListFiles = strcspn(argv[i], "fF") != strlen(argv[i]);
			}

			// Check to see if we need to dump our list to a file.
			if(i + 1 < argc && *argv[i + 1] != '-')
			{
				cList = argv[++i];
			}
		}
		else if(stricmp(argv[i], "-c") == 0)
		{
			bConsoleMode = TRUE;
		}
		else if(stricmp(argv[i], "-s") == 0)
		{
			bSilent = TRUE;
		}
		else if(stricmp(argv[i], "-m") == 0)
		{
			bFileMapping = TRUE;
		}
		else if(stricmp(argv[i], "-v") == 0)
		{
			bVolatileAccess = TRUE;
		}
		else
		{
			PrintUsage();
			return 2;
		}
	}

	// Make sure we have something to do.
	if(cPackage == 0 || (Items.size() == 0 && !bList && !bConsoleMode))
	{
		PrintUsage();
		return 2;
	}

	// Create an appropriate package.
	// This function creates an appropriate package class based on the
	// filename extension alone.  This package must be destroyed at a
	// latter time by calling "delete Package;".
	CPackage *Package = CPackageFactory::Create(cPackage);

	// If CPackageFactory::Create() returns null the package extension
	// is unsupported.
	if(Package == 0)
	{
		printf("Error loading %s:\nUnsupported package type.\n", cPackage);
		return 3;
	}

	// Create a package utility.
	// A package utility has nothing to do with loading a package
	// but provides several useful functions for generating statistical
	// information on packages, extracting package items, and performing
	// other miscellaneous tasks.
	CPackageUtility PackageUtility = CPackageUtility(Package);

	// Open the package.
	// I know, ugly hack to gain CMappedPackage member access...  I don't
	// want to require runtime type checking.  CPackageFactory::Create will
	// always return a CMappedPackage object but this could change in later
	// versions.  To be extra cautious you could make your own package factory.
	if(!static_cast<CMappedPackage *>(Package)->Open(cPackage, bFileMapping, bVolatileAccess))
	{
		printf("Error loading %s:\n%s\n", cPackage, Package->GetLastError());
		delete Package;
		return 3;
	}

	if(!bSilent)
		printf("%s opened.\n", cPackage);

	// Extract the requested items.
	for(DWORD j = 0; j < Items.size(); j++)
	{
		// Find the item.
		CDirectoryItem *Item = PackageUtility.GetDirectoryItem(Items[j]);

		if(Item == 0)
		{
			printf("%s not found in package.\n", Items[j]);
			continue;
		}

		// Extract the item.
		// Item is extracted to cDestination\\Item->GetName().
		PackageUtility.Extract(Item, cDestination, &SilentCallBack);
	}

	// List items in package.
	if(bList)
	{
		if(!bSilent)
			printf("Listing...\n");

		FILE *pFile = stdout;

		if(cList != 0)
		{
			pFile = fopen(cList, "wt");

			if(pFile == 0)
			{
				printf("Error opening %s:\n%s\n", cList, "fopen() failed.");
				delete Package;
				return 3;
			}
		}

		List(pFile, PackageUtility, Package->GetRoot(), bListFolders, bListFiles);

		if(cList != 0)
		{
			fclose(pFile);
		}

		if(!bSilent)
			printf("Done.\n");
	}

	// Interactive console mode.
	// Commands: dir, cd, root, info, extract, find, type, cls, help, exit.
	if(bConsoleMode)
	{
		std::string sBuffer;	// Input string.
		char cCommand[1024];	// Input command  (i.e. first word in input string).
		char cArgument[1024];	// Input argument (i.e. rest of input string).

		char cTemp[1024];
		CDirectoryFolder *Folder = Package->GetRoot();	// Our current folder.

		while(TRUE)
		{
			// Command prompt.
			printf("%s>", Folder->GetName());

			// Get and parse line.
			std::getline(std::cin, sBuffer);
			
			*cCommand = *cArgument = 0;

			strcpy(cCommand, sBuffer.c_str());
			char *cSpace = strchr(cCommand, ' ');
			if(cSpace != 0)
			{
				strcpy(cArgument, cSpace + 1);
				*cSpace = 0;
			}

			// Cycle through commands.

			//
			// Directory listing.
			// Good example of CDirectoryItem::GetType().
			//
			if(stricmp(cCommand, "dir") == 0)
			{
				int iFolders = 0, iFiles = 0;

				*cTemp = 0;
				PackageUtility.GetDirectoryItemPath(Folder, cTemp);

				printf("Directory of %s:\n", cTemp);

				printf("\n");

				// List all folders in the current folder.
				for(DWORD i = 0; i < Folder->ItemCount(); i++)
				{
					if(Folder->GetItem(i)->GetType() == DirectoryItemFolder)
					{
						iFolders++;
						printf("  <%s>\n", Folder->GetItem(i)->GetName());
					}
				}

				// List all files in the current folder.
				for(DWORD i = 0; i < Folder->ItemCount(); i++)
				{
					if(Folder->GetItem(i)->GetType() == DirectoryItemFile)
					{
						iFiles++;
						printf("  %s\n", Folder->GetItem(i)->GetName());
					}
				}

				printf("\n");

				// Could also have used PackageUtility.GetFolderFolderCount() or
				// PackageUtility.GetFolderFileCount() (however both are
				// recursive so we would have gotten a different answer).

				printf("Summary:\n");
				printf("\n");
				printf("  %d Folder%s.\n", iFolders, iFolders != 1 ? "s" : "");
				printf("  %d File%s.\n", iFiles, iFiles != 1 ? "s" : "");
				printf("\n");
			}
			//
			// Change directory.
			// Good example of CDirectoryFolder::GetParent() and item casting.
			//
			else if(stricmp(cCommand, "cd") == 0)
			{
				if(*cArgument == 0)
				{
					printf("No argument for command cd supplied.\n");
				}
				else
				{
					if(stricmp(cArgument, ".") == 0)
					{

					}
					else if(stricmp(cArgument, "..") == 0)
					{
						if(Folder->GetParent() != 0)
						{
							Folder = Folder->GetParent();
						}
						else
						{
							printf("Folder does not have a parent.\n");
						}
					}
					else
					{
						BOOL bFound = FALSE;
						for(DWORD i = 0; i < Folder->ItemCount(); i++)
						{
							if(Folder->GetItem(i)->GetType() == DirectoryItemFolder && stricmp(cArgument, Folder->GetItem(i)->GetName()) == 0)
							{
								bFound = TRUE;
								Folder = static_cast<CDirectoryFolder *>(Folder->GetItem(i));
								break;
							}
						}

						if(!bFound)
						{
							printf("%s not found.\n", cArgument);
						}
					}
				}
			}
			//
			// Go to the root folder.
			//
			else if(stricmp(cCommand, "root") == 0)
			{
				Folder = Package->GetRoot();
			}
			//
			// Item information.
			// Good example of CPackageUtility helper functions.
			//
			else if(stricmp(cCommand, "info") == 0)
			{
				if(*cArgument == 0)
				{
					printf("No argument for command info supplied.\n");
				}
				else
				{
					CDirectoryItem *Item = 0;
					for(DWORD i = 0; i < Folder->ItemCount(); i++)
					{
						if(stricmp(cArgument, Folder->GetItem(i)->GetName()) == 0)
						{
							Item = Folder->GetItem(i);
							break;
						}
					}

					if(Item)
					{
						*cTemp = 0;
						PackageUtility.GetDirectoryItemPath(Item, cTemp);

						printf("Information for %s:\n", cTemp);

						printf("\n");

						if(Item->GetType() == DirectoryItemFolder)
						{
							CDirectoryFolder *InfoFolder = static_cast<CDirectoryFolder *>(Item);

							printf("  Type: Folder\n");
							printf("  Size: %d B\n", PackageUtility.GetDirectoryItemSize(InfoFolder));
							printf("  Size On Disk: %d B\n", PackageUtility.GetDirectoryItemSizeOnDisk(InfoFolder));
							printf("  Folders: %d\n", PackageUtility.GetFolderFolderCount(InfoFolder));
							printf("  Files: %d\n", PackageUtility.GetFolderFileCount(InfoFolder));
						}
						else if(Item->GetType() == DirectoryItemFile)
						{
							CDirectoryFile *InfoFile = static_cast<CDirectoryFile *>(Item);

							printf("  Type: File\n");
							printf("  Size: %d B\n", PackageUtility.GetDirectoryItemSize(InfoFile));
							printf("  Size On Disk: %d B\n", PackageUtility.GetDirectoryItemSizeOnDisk(InfoFile));

							// If we have a GCF file we can gather additional information about it.
							if(Package->GetPackageType() == HLLIB_PACKAGE_GCF)
							{
								CGCFFile *GCFFile = static_cast<CGCFFile *>(Package);

								// GetFileExists() tells us if the file has been downloaded by Steam.
								BOOL bExists;
								GCFFile->GetFileExists(InfoFile, bExists);

								printf("  Exists: %s\n", bExists ? "True" : "False");

								// GetItemsFlags() gets us additional item flags such as the encrypted flag.
								// Note: not all flags are known.
								DWORD dwFlags;
								GCFFile->GetItemsFlags(InfoFile, dwFlags);

								printf("  Flags: %.8x\n", dwFlags);
								printf("    Encrypted: %s\n", dwFlags & HLLIB_GCF_FLAG_ENCRYPTED ? "True" : "False");
								printf("    Local Copy: %s\n", dwFlags & HLLIB_GCF_FLAG_LOCAL_COPY ? "True" : "False");
								printf("    Local Copy No Overwrite: %s\n", dwFlags & HLLIB_GCF_FLAG_LOCAL_COPY_NO_OVERWRITE ? "True" : "False");
							}
						}

						printf("\n");
					}
					else
					{
						printf("%s not found.\n", cArgument);
					}
				}
			}
			//
			// Extract item.
			// Good example of CPackageUtility extract functions.
			//
			else if(stricmp(cCommand, "extract") == 0)
			{
				if(*cArgument == 0)
				{
					printf("No argument for command extract supplied.\n");
				}
				else
				{
					CDirectoryItem *Item = 0;
					for(DWORD i = 0; i < Folder->ItemCount(); i++)
					{
						if(stricmp(cArgument, Folder->GetItem(i)->GetName()) == 0)
						{
							Item = Folder->GetItem(i);
							break;
						}
					}

					if(Item)
					{
						// Extract the item.
						// Item is extracted to cDestination\\Item->GetName().
						PackageUtility.Extract(Item, cDestination, &CallBack);
					}
					else
					{
						printf("%s not found.\n", cArgument);
					}
				}
			}
			//
			// Find items.
			// Good example of recursive directory navigation (Search() function).
			//
			else if(stricmp(cCommand, "find") == 0)
			{
				if(*cArgument == 0)
				{
					printf("No argument for command find supplied.\n");
				}
				else
				{
					// Search for the requested items.
					printf("Searching for %s...\n", cArgument);

					printf("\n");

					int iCount = 0;
					Search(cArgument, iCount, PackageUtility, Folder);

					printf("\n");

					printf("  %d item%s found.\n", iCount, iCount != 1 ? "s" : "");

					printf("\n");
				}
			}
			//
			// Type files.
			// Good example of reading files into memory.
			//
			else if(stricmp(cCommand, "type") == 0)
			{
				if(*cArgument == 0)
				{
					printf("No argument for command type supplied.\n");
				}
				else
				{
					CDirectoryItem *Item = 0;
					for(DWORD i = 0; i < Folder->ItemCount(); i++)
					{
						if(Folder->GetItem(i)->GetType() == DirectoryItemFile && stricmp(cArgument, Folder->GetItem(i)->GetName()) == 0)
						{
							Item = Folder->GetItem(i);
							break;
						}
					}

					if(Item)
					{
						CDirectoryFile *File = static_cast<CDirectoryFile *>(Item);

						*cTemp = 0;
						PackageUtility.GetDirectoryItemPath(Item, cTemp);

						printf("Type for %s:\n", cTemp);

						printf("\n");

						DWORD dwSize;
						char *cData = 0;

						// Read file data.
						// The try catch is just for my sake,
						// HLLib should never throw exceptions.
						try
						{
							if(!Package->GetFileSize(File, dwSize))
								throw;

							cData = new char[dwSize];

							if(!Package->GetFileData(File, (unsigned char *)cData))
								throw;

							// Type file data.

							for(DWORD i = 0; i < dwSize; i++)
							{
								if(isprint(cData[i]) || isspace(cData[i]))
								{
									putc(cData[i], stdout);
								}
							}
						}
						catch(...)
						{
							sprintf("Error extracting %s:\n%s\n", File->GetName(), Package->GetLastError());
						}

						// Clean up.

						delete []cData;

						printf("\n");
					}
					else
					{
						printf("%s not found.\n", cArgument);
					}
				}
			}
			//
			// Clear screen.
			//
			else if(stricmp(cCommand, "cls") == 0)
			{
				system("cls");
			}
			else if(stricmp(cCommand, "help") == 0)
			{
				printf("Valid commands:\n");
				printf("\n");
				printf("dir            (Directory list.)\n");
				printf("cd <folder>    (Change directroy.)\n");
				printf("info <item>    (Item information.)\n");
				printf("extract <item> (Extract item.)\n");
				printf("find <item>    (Find item.)\n");
				printf("type <file>    (Type a file.)\n");
				printf("root           (Go to the root folder.)\n");
				printf("cls            (Clear the screen.)\n");
				printf("help           (Program help.)\n");
				printf("exit           (Quit program.)\n");
				printf("\n");
			}
			else if(stricmp(cCommand, "exit") == 0)
			{
				break;
			}
			else
			{
				printf("Unkown command: %s\n", cCommand);
			}
		}
	}

	// Close the package.
	Package->Close();

	if(!bSilent)
		printf("%s closed.\n", cPackage);

	// Destroy the package.
	delete Package;

	return 0;
}