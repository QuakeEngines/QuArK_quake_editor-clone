/*
 * VTFEdit
 * Copyright (C) 2005 Neil Jedrzejewski & Ryan Gregg
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

#pragma once

#include "stdafx.h"
#include "VTFOptions.h"
#include "VMTFileUtility.h"
#include "VTFFileUtility.h"

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Windows::Forms;
using namespace System::Data;
using namespace System::Drawing;

namespace VTFEdit
{
	public __gc class CBatchConvert : public System::Windows::Forms::Form
	{
	private:
		CVTFOptions *Options;

	public: 
		CBatchConvert(CVTFOptions *Options)
		{
			this->Options = Options;

			this->InitializeComponent();
		}
        
	protected: 
		void Dispose(Boolean disposing)
		{
			if (disposing && components)
			{
				components->Dispose();
			}
			__super::Dispose(disposing);
		}
	private: System::Windows::Forms::Button *  btnOptions;
	private: System::Windows::Forms::GroupBox *  grpProgress;
	private: System::Windows::Forms::ProgressBar *  barProgress;
	private: System::Windows::Forms::Button *  btnClose;
	private: System::Windows::Forms::Button *  btnConvert;
	private: System::Windows::Forms::GroupBox *  grpOptions;
	private: System::Windows::Forms::CheckBox *  chkCreateVMTFiles;
	private: System::Windows::Forms::Button *  btnOutputFolderBrowse;
	private: System::Windows::Forms::TextBox *  txtOutputFolder;
	private: System::Windows::Forms::Label *  lblOutputFolder;
	private: System::Windows::Forms::Label *  lblWADFile;
	private: System::Windows::Forms::GroupBox *  grpLog;
	private: System::Windows::Forms::RichTextBox *  txtLog;
	private: System::Windows::Forms::Button *  btnInputFolderBrowse;
	private: System::Windows::Forms::TextBox *  txtInputFolder;
	private: System::Windows::Forms::FolderBrowserDialog *  dlgOpenFolder;
	private: System::Windows::Forms::CheckBox *  chkRecursive;
	private: System::Windows::Forms::TextBox *  txtFilter;
	private: System::Windows::Forms::Label *  lblFilter;
	private: System::Windows::Forms::ToolTip *  tipMain;
	private: System::ComponentModel::IContainer *  components;

	private:
		/// <summary>
		/// Required designer variable.
		/// </summary>


		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		void InitializeComponent(void)
		{
			this->components = new System::ComponentModel::Container();
			System::Resources::ResourceManager *  resources = new System::Resources::ResourceManager(__typeof(VTFEdit::CBatchConvert));
			this->btnOptions = new System::Windows::Forms::Button();
			this->grpProgress = new System::Windows::Forms::GroupBox();
			this->barProgress = new System::Windows::Forms::ProgressBar();
			this->btnClose = new System::Windows::Forms::Button();
			this->btnConvert = new System::Windows::Forms::Button();
			this->grpOptions = new System::Windows::Forms::GroupBox();
			this->txtFilter = new System::Windows::Forms::TextBox();
			this->lblFilter = new System::Windows::Forms::Label();
			this->chkRecursive = new System::Windows::Forms::CheckBox();
			this->chkCreateVMTFiles = new System::Windows::Forms::CheckBox();
			this->btnOutputFolderBrowse = new System::Windows::Forms::Button();
			this->txtOutputFolder = new System::Windows::Forms::TextBox();
			this->lblOutputFolder = new System::Windows::Forms::Label();
			this->btnInputFolderBrowse = new System::Windows::Forms::Button();
			this->txtInputFolder = new System::Windows::Forms::TextBox();
			this->lblWADFile = new System::Windows::Forms::Label();
			this->grpLog = new System::Windows::Forms::GroupBox();
			this->txtLog = new System::Windows::Forms::RichTextBox();
			this->dlgOpenFolder = new System::Windows::Forms::FolderBrowserDialog();
			this->tipMain = new System::Windows::Forms::ToolTip(this->components);
			this->grpProgress->SuspendLayout();
			this->grpOptions->SuspendLayout();
			this->grpLog->SuspendLayout();
			this->SuspendLayout();
			// 
			// btnOptions
			// 
			this->btnOptions->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->btnOptions->Location = System::Drawing::Point(8, 344);
			this->btnOptions->Name = S"btnOptions";
			this->btnOptions->Size = System::Drawing::Size(80, 23);
			this->btnOptions->TabIndex = 5;
			this->btnOptions->Text = S"&Options";
			this->btnOptions->Click += new System::EventHandler(this, btnOptions_Click);
			// 
			// grpProgress
			// 
			this->grpProgress->Controls->Add(this->barProgress);
			this->grpProgress->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->grpProgress->Location = System::Drawing::Point(8, 136);
			this->grpProgress->Name = S"grpProgress";
			this->grpProgress->Size = System::Drawing::Size(360, 48);
			this->grpProgress->TabIndex = 3;
			this->grpProgress->TabStop = false;
			this->grpProgress->Text = S"Progress:";
			// 
			// barProgress
			// 
			this->barProgress->Location = System::Drawing::Point(8, 16);
			this->barProgress->Name = S"barProgress";
			this->barProgress->Size = System::Drawing::Size(344, 23);
			this->barProgress->TabIndex = 0;
			// 
			// btnClose
			// 
			this->btnClose->DialogResult = System::Windows::Forms::DialogResult::Cancel;
			this->btnClose->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->btnClose->Location = System::Drawing::Point(288, 344);
			this->btnClose->Name = S"btnClose";
			this->btnClose->Size = System::Drawing::Size(80, 23);
			this->btnClose->TabIndex = 1;
			this->btnClose->Text = S"&Close";
			this->btnClose->Click += new System::EventHandler(this, btnClose_Click);
			// 
			// btnConvert
			// 
			this->btnConvert->Enabled = false;
			this->btnConvert->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->btnConvert->Location = System::Drawing::Point(200, 344);
			this->btnConvert->Name = S"btnConvert";
			this->btnConvert->Size = System::Drawing::Size(80, 23);
			this->btnConvert->TabIndex = 0;
			this->btnConvert->Text = S"Con&vert";
			this->btnConvert->Click += new System::EventHandler(this, btnConvert_Click);
			// 
			// grpOptions
			// 
			this->grpOptions->Controls->Add(this->txtFilter);
			this->grpOptions->Controls->Add(this->lblFilter);
			this->grpOptions->Controls->Add(this->chkRecursive);
			this->grpOptions->Controls->Add(this->chkCreateVMTFiles);
			this->grpOptions->Controls->Add(this->btnOutputFolderBrowse);
			this->grpOptions->Controls->Add(this->txtOutputFolder);
			this->grpOptions->Controls->Add(this->lblOutputFolder);
			this->grpOptions->Controls->Add(this->btnInputFolderBrowse);
			this->grpOptions->Controls->Add(this->txtInputFolder);
			this->grpOptions->Controls->Add(this->lblWADFile);
			this->grpOptions->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->grpOptions->Location = System::Drawing::Point(8, 8);
			this->grpOptions->Name = S"grpOptions";
			this->grpOptions->Size = System::Drawing::Size(360, 120);
			this->grpOptions->TabIndex = 2;
			this->grpOptions->TabStop = false;
			this->grpOptions->Text = S"Options:";
			// 
			// txtFilter
			// 
			this->txtFilter->Location = System::Drawing::Point(88, 64);
			this->txtFilter->Name = S"txtFilter";
			this->txtFilter->Size = System::Drawing::Size(232, 20);
			this->txtFilter->TabIndex = 7;
			this->txtFilter->Text = S"*.tga";
			this->tipMain->SetToolTip(this->txtFilter, S"Wildcard filter.  Examples include *.* or *.tga or *.bmp;*.tga or *.vtf.");
			// 
			// lblFilter
			// 
			this->lblFilter->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->lblFilter->Location = System::Drawing::Point(8, 64);
			this->lblFilter->Name = S"lblFilter";
			this->lblFilter->Size = System::Drawing::Size(80, 23);
			this->lblFilter->TabIndex = 6;
			this->lblFilter->Text = S"Filter:";
			// 
			// chkRecursive
			// 
			this->chkRecursive->Checked = true;
			this->chkRecursive->CheckState = System::Windows::Forms::CheckState::Checked;
			this->chkRecursive->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->chkRecursive->Location = System::Drawing::Point(88, 88);
			this->chkRecursive->Name = S"chkRecursive";
			this->chkRecursive->Size = System::Drawing::Size(112, 24);
			this->chkRecursive->TabIndex = 8;
			this->chkRecursive->Text = S"&Recursive";
			this->tipMain->SetToolTip(this->chkRecursive, S"Include subdirectories.");
			// 
			// chkCreateVMTFiles
			// 
			this->chkCreateVMTFiles->Checked = true;
			this->chkCreateVMTFiles->CheckState = System::Windows::Forms::CheckState::Checked;
			this->chkCreateVMTFiles->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->chkCreateVMTFiles->Location = System::Drawing::Point(208, 88);
			this->chkCreateVMTFiles->Name = S"chkCreateVMTFiles";
			this->chkCreateVMTFiles->Size = System::Drawing::Size(112, 24);
			this->chkCreateVMTFiles->TabIndex = 9;
			this->chkCreateVMTFiles->Text = S"Create &VMT Files";
			this->tipMain->SetToolTip(this->chkCreateVMTFiles, S"Generate .vmt files for .vtf files. Ouput folder should be a material folder.");
			// 
			// btnOutputFolderBrowse
			// 
			this->btnOutputFolderBrowse->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->btnOutputFolderBrowse->Location = System::Drawing::Point(328, 40);
			this->btnOutputFolderBrowse->Name = S"btnOutputFolderBrowse";
			this->btnOutputFolderBrowse->Size = System::Drawing::Size(20, 20);
			this->btnOutputFolderBrowse->TabIndex = 5;
			this->btnOutputFolderBrowse->Text = S"...";
			this->btnOutputFolderBrowse->Click += new System::EventHandler(this, btnOutputFolderBrowse_Click);
			// 
			// txtOutputFolder
			// 
			this->txtOutputFolder->Location = System::Drawing::Point(88, 40);
			this->txtOutputFolder->Name = S"txtOutputFolder";
			this->txtOutputFolder->ReadOnly = true;
			this->txtOutputFolder->Size = System::Drawing::Size(232, 20);
			this->txtOutputFolder->TabIndex = 4;
			this->txtOutputFolder->Text = S"";
			this->tipMain->SetToolTip(this->txtOutputFolder, S"Destination image folder.");
			this->txtOutputFolder->TextChanged += new System::EventHandler(this, Path_TextChanged);
			// 
			// lblOutputFolder
			// 
			this->lblOutputFolder->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->lblOutputFolder->Location = System::Drawing::Point(8, 40);
			this->lblOutputFolder->Name = S"lblOutputFolder";
			this->lblOutputFolder->Size = System::Drawing::Size(80, 23);
			this->lblOutputFolder->TabIndex = 3;
			this->lblOutputFolder->Text = S"Output Folder:";
			// 
			// btnInputFolderBrowse
			// 
			this->btnInputFolderBrowse->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->btnInputFolderBrowse->Location = System::Drawing::Point(328, 16);
			this->btnInputFolderBrowse->Name = S"btnInputFolderBrowse";
			this->btnInputFolderBrowse->Size = System::Drawing::Size(20, 20);
			this->btnInputFolderBrowse->TabIndex = 2;
			this->btnInputFolderBrowse->Text = S"...";
			this->btnInputFolderBrowse->Click += new System::EventHandler(this, btnInputFolderBrowse_Click);
			// 
			// txtInputFolder
			// 
			this->txtInputFolder->Location = System::Drawing::Point(88, 16);
			this->txtInputFolder->Name = S"txtInputFolder";
			this->txtInputFolder->ReadOnly = true;
			this->txtInputFolder->Size = System::Drawing::Size(232, 20);
			this->txtInputFolder->TabIndex = 1;
			this->txtInputFolder->Text = S"";
			this->tipMain->SetToolTip(this->txtInputFolder, S"Source image folder.");
			this->txtInputFolder->TextChanged += new System::EventHandler(this, Path_TextChanged);
			// 
			// lblWADFile
			// 
			this->lblWADFile->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->lblWADFile->Location = System::Drawing::Point(8, 16);
			this->lblWADFile->Name = S"lblWADFile";
			this->lblWADFile->Size = System::Drawing::Size(80, 23);
			this->lblWADFile->TabIndex = 0;
			this->lblWADFile->Text = S"Input Folder:";
			// 
			// grpLog
			// 
			this->grpLog->Controls->Add(this->txtLog);
			this->grpLog->FlatStyle = System::Windows::Forms::FlatStyle::System;
			this->grpLog->Location = System::Drawing::Point(8, 192);
			this->grpLog->Name = S"grpLog";
			this->grpLog->Size = System::Drawing::Size(360, 144);
			this->grpLog->TabIndex = 4;
			this->grpLog->TabStop = false;
			this->grpLog->Text = S"Log:";
			// 
			// txtLog
			// 
			this->txtLog->DetectUrls = false;
			this->txtLog->Location = System::Drawing::Point(8, 16);
			this->txtLog->Name = S"txtLog";
			this->txtLog->ReadOnly = true;
			this->txtLog->Size = System::Drawing::Size(344, 120);
			this->txtLog->TabIndex = 0;
			this->txtLog->Text = S"";
			// 
			// dlgOpenFolder
			// 
			this->dlgOpenFolder->Description = S"Select a folder:";
			// 
			// tipMain
			// 
			this->tipMain->AutoPopDelay = 10000;
			this->tipMain->InitialDelay = 500;
			this->tipMain->ReshowDelay = 100;
			// 
			// CBatchConvert
			// 
			this->AcceptButton = this->btnConvert;
			this->AutoScaleBaseSize = System::Drawing::Size(5, 13);
			this->CancelButton = this->btnClose;
			this->ClientSize = System::Drawing::Size(376, 376);
			this->Controls->Add(this->btnOptions);
			this->Controls->Add(this->grpProgress);
			this->Controls->Add(this->btnClose);
			this->Controls->Add(this->btnConvert);
			this->Controls->Add(this->grpOptions);
			this->Controls->Add(this->grpLog);
			this->FormBorderStyle = System::Windows::Forms::FormBorderStyle::FixedDialog;
			this->Icon = (__try_cast<System::Drawing::Icon *  >(resources->GetObject(S"$this.Icon")));
			this->MaximizeBox = false;
			this->MinimizeBox = false;
			this->Name = S"CBatchConvert";
			this->ShowInTaskbar = false;
			this->StartPosition = System::Windows::Forms::FormStartPosition::CenterParent;
			this->Text = S"Batch Convert";
			this->grpProgress->ResumeLayout(false);
			this->grpOptions->ResumeLayout(false);
			this->grpLog->ResumeLayout(false);
			this->ResumeLayout(false);

		}		
		public: __property System::String *get_InputFolder()
		{
			return this->txtInputFolder->Text;
		}

		public: __property void set_InputFolder(System::String *sInputFolder)
		{
			if(System::IO::Directory::Exists(sInputFolder))
			{
				this->txtInputFolder->Text = sInputFolder;
			}
		}

		public: __property System::String *get_OutputFolder()
		{
			return this->txtOutputFolder->Text;
		}

		public: __property void set_OutputFolder(System::String *sOutputFolder)
		{
			if(System::IO::Directory::Exists(sOutputFolder))
			{
				this->txtOutputFolder->Text = sOutputFolder;
			}
		}

		public: __property System::String *get_Filter()
		{
			return this->txtFilter->Text;
		}

		public: __property void set_Filter(System::String *sFilter)
		{
			this->txtFilter->Text = sFilter;
		}

		public: __property bool get_Recurse()
		{
			return this->chkRecursive->Checked == true;
		}

		public: __property void set_Recurse(bool bRecurse)
		{
			this->chkRecursive->Checked = bRecurse;
		}

		public: __property bool get_CreateVMTFiles()
		{
			return this->chkCreateVMTFiles->Checked == true;
		}

		public: __property void set_CreateVMTFiles(bool bCreateVMTFiles)
		{
			this->chkCreateVMTFiles->Checked = bCreateVMTFiles;
		}

		private: System::Void CBatchConvert_Closing(System::Object *  sender, System::ComponentModel::CancelEventArgs *  e)
		{

		}

		private: System::Void btnInputFolderBrowse_Click(System::Object *  sender, System::EventArgs *  e)
		{
			this->dlgOpenFolder->SelectedPath = this->txtInputFolder->Text;
			if(this->dlgOpenFolder->ShowDialog() == System::Windows::Forms::DialogResult::OK)
			{
				this->txtInputFolder->Text = this->dlgOpenFolder->SelectedPath;
			}
		}

		private: System::Void btnOutputFolderBrowse_Click(System::Object *  sender, System::EventArgs *  e)
		{
			this->dlgOpenFolder->SelectedPath = this->txtOutputFolder->Text;
			if(this->dlgOpenFolder->ShowDialog() == System::Windows::Forms::DialogResult::OK)
			{
				this->txtOutputFolder->Text = this->dlgOpenFolder->SelectedPath;
			}
		}

		private: System::Void Path_TextChanged(System::Object *  sender, System::EventArgs *  e)
		{
			this->btnConvert->Enabled = System::IO::Directory::Exists(this->txtInputFolder->Text);
		}

		private: int CountFiles(System::String *sInputFolder, System::String *sFilter, bool bRecursive)
		{
			int count = 0;

			System::IO::DirectoryInfo *Folder = new System::IO::DirectoryInfo(sInputFolder);

			if (bRecursive)
			{
				System::IO::DirectoryInfo *Folders __gc[] = Folder->GetDirectories();

				for(int i = 0; i < Folders->Count; i++)
				{
					count += this->CountFiles(Folders[i]->FullName, sFilter, bRecursive);
				}
			}

			System::String *Filters __gc[] = sFilter->Split((S";")->ToCharArray());

			for(int i = 0; i < Filters->Count; i++)
			{
				System::IO::FileInfo *Files __gc[] = Folder->GetFiles(Filters[i]);
				count += Files->Count;
			}

			return count;
		}

		private: System::Void ConvertFolder(System::String *sInputFolder, System::String *sOutputFolder, System::String *sFilter, bool bRecursive, SVTFCreateOptions &VTFCreateOptions)
		{
			VTFLib::CVTFFile VTFFile = VTFLib::CVTFFile();

			System::IO::DirectoryInfo *Folder = new System::IO::DirectoryInfo(sInputFolder);

			this->Log(String::Concat(S"Entering ", sInputFolder, S"..."), System::Drawing::Color::Green);

			if (bRecursive)
			{
				System::IO::DirectoryInfo *Folders __gc[] = Folder->GetDirectories();

				for(int i = 0; i < Folders->Count; i++)
				{
					this->ConvertFolder(String::Concat(sInputFolder, S"\\", Folders[i]->Name), String::Concat(sOutputFolder, S"\\", Folders[i]->Name), sFilter, bRecursive, VTFCreateOptions);
				}
			}

			System::String *Filters __gc[] = sFilter->Split((S";")->ToCharArray());

			for(int i = 0; i < Filters->Count; i++)
			{
				System::IO::FileInfo *Files __gc[] = Folder->GetFiles(Filters[i]);

				for(int j = 0; j < Files->Count; j++)
				{
					this->Log(String::Concat(S"Processing ", Files[j]->Name, S"..."), System::Drawing::Color::Gray);

					if(String::Compare(Files[j]->Extension, S".vtf", true) == 0)  // Convert from .vtf.
					{
						String *sTGAName = Files[j]->Name->Substring(0, Files[j]->Name->Length - Files[j]->Extension->Length);
						String *sTGAFile = String::Concat(sOutputFolder, S"\\", sTGAName, S".tga");

						char *cFile = (char *)(System::Runtime::InteropServices::Marshal::StringToHGlobalAnsi(Files[j]->FullName)).ToPointer();

						if(VTFFile.Load(cFile))
						{
							vlUInt uiWidth = VTFFile.GetWidth(), uiHeight = VTFFile.GetHeight();
				
							vlByte *lpImageData = new vlByte[VTFFile.ComputeImageSize(uiWidth, uiHeight, 1, IMAGE_FORMAT_RGBA8888)];

							if(VTFFile.ConvertToRGBA8888(VTFFile.GetData(0, 0, 0, 0), lpImageData, uiWidth, uiHeight, VTFFile.GetFormat()))
							{
								// DevIL likes image data upside down...
								VTFFile.FlipImage(lpImageData, uiWidth, uiHeight);

								if(ilTexImage(uiWidth, uiHeight, 1, 4, IL_RGBA, IL_UNSIGNED_BYTE, lpImageData))
								{
									if(!System::IO::Directory::Exists(sOutputFolder))
									{
										try
										{
											System::IO::Directory::CreateDirectory(sOutputFolder);
										}
										catch(Exception *)
										{

										}
									}

									char *cTGAFile = (char *)(System::Runtime::InteropServices::Marshal::StringToHGlobalAnsi(sTGAFile)).ToPointer();
									if(ilSave(IL_TGA, cTGAFile))
									{
										this->Log(String::Concat(S"Wrote ", sTGAFile, S"."), System::Drawing::Color::Green);
									}
									else
									{
										this->Log(String::Concat(S"Error writing ", Files[j]->Name, S"."), System::Drawing::Color::Red);
									}
									System::Runtime::InteropServices::Marshal::FreeHGlobal((IntPtr)cTGAFile);
								}
								else
								{
									this->Log(String::Concat(S"Error creating ", Files[j]->Name, S"."), System::Drawing::Color::Red);
								}
							}
							else
							{
								this->Log(String::Concat(S"Error converting ", Files[j]->Name, S".", (new String(vlGetLastError()))->Replace(S"\n", S" ")), System::Drawing::Color::Red);
							}

							delete []lpImageData;
						}
						else
						{
							this->Log(String::Concat(S"Error loading ", Files[j]->Name, S".", (new String(vlGetLastError()))->Replace(S"\n", S" ")), System::Drawing::Color::Red);
						}

						System::Runtime::InteropServices::Marshal::FreeHGlobal((IntPtr)cFile);
					}
					else  // Convert to .vtf.
					{
						String *sVTFName = Files[j]->Name->Substring(0, Files[j]->Name->Length - Files[j]->Extension->Length);
						String *sVTFFile = String::Concat(sOutputFolder, S"\\", sVTFName, S".vtf");
						String *sVMTFile = String::Concat(sOutputFolder, S"\\", sVTFName, S".vmt");

						char *cFile = (char *)(System::Runtime::InteropServices::Marshal::StringToHGlobalAnsi(Files[j]->FullName)).ToPointer();

						// Load the image and convert it to RGBA.
						if(ilLoadImage(cFile))
						{
							bool bHasAlpha = ilGetInteger(IL_IMAGE_FORMAT) == IL_RGBA || ilGetInteger(IL_IMAGE_FORMAT) == IL_BGRA || ilGetInteger(IL_IMAGE_FORMAT) == IL_LUMINANCE_ALPHA;

							if(ilConvertImage(IL_RGBA, IL_UNSIGNED_BYTE))
							{
								VTFCreateOptions.ImageFormat = bHasAlpha ? Options->AlphaFormat : Options->NormalFormat;

								if(VTFFile.Create((vlUInt)ilGetInteger(IL_IMAGE_WIDTH), (vlUInt)ilGetInteger(IL_IMAGE_HEIGHT), ilGetData(), VTFCreateOptions))
								{
									if(!System::IO::Directory::Exists(sOutputFolder))
									{
										try
										{
											System::IO::Directory::CreateDirectory(sOutputFolder);
										}
										catch(Exception *)
										{

										}
									}

									char *cVTFFile = (char *)(System::Runtime::InteropServices::Marshal::StringToHGlobalAnsi(sVTFFile)).ToPointer();
									if(VTFFile.Save(cVTFFile))
									{
										this->Log(String::Concat(S"Wrote ", sVTFFile, S"."), System::Drawing::Color::Green);
										if(this->chkCreateVMTFiles->Checked && CVMTFileUtility::CreateDefaultMaterial(sVTFFile, S"LightmappedGeneric", bHasAlpha))
										{
											this->Log(String::Concat(S"Wrote ", sVMTFile, S"."), System::Drawing::Color::Green);
										}
									}
									else
									{
										this->Log(String::Concat(S"Error writing ", Files[j]->Name, S".", (new String(vlGetLastError()))->Replace(S"\n", S" ")), System::Drawing::Color::Red);
									}
									System::Runtime::InteropServices::Marshal::FreeHGlobal((IntPtr)cVTFFile);
								}
								else
								{
									this->Log(String::Concat(S"Error creating ", Files[j]->Name, S".", (new String(vlGetLastError()))->Replace(S"\n", S" ")), System::Drawing::Color::Red);
								}
							}
							else
							{
								this->Log(String::Concat(S"Error converting ", Files[j]->Name, S"."), System::Drawing::Color::Red);
							}
						}
						else
						{
							this->Log(String::Concat(S"Error loading ", Files[j]->Name, S"."), System::Drawing::Color::Red);
						}

						System::Runtime::InteropServices::Marshal::FreeHGlobal((IntPtr)cFile);
					}

					this->Log(String::Concat(Files[j]->Name, S" processed."), System::Drawing::Color::Gray);

					this->barProgress->Value++;
					this->barProgress->Refresh();
				}
			}

			this->Log(String::Concat(S"Exiting ", sInputFolder, S"."), System::Drawing::Color::Green);
		}

		private: System::Void btnConvert_Click(System::Object *  sender, System::EventArgs *  e)
		{
			this->Cursor = System::Windows::Forms::Cursors::WaitCursor;

			this->barProgress->Value = 0;
			this->barProgress->Maximum = this->CountFiles(this->txtInputFolder->Text, this->txtFilter->Text->Length == 0 ? S"*" : this->txtFilter->Text, this->chkRecursive->Checked);
			this->txtLog->Clear();

			SVTFCreateOptions VTFCreateOptions = CVTFFileUtility::GetCreateOptions(this->Options);

			this->ConvertFolder(this->txtInputFolder->Text, this->txtOutputFolder->Text->Length == 0 ? this->txtInputFolder->Text : this->txtOutputFolder->Text, this->txtFilter->Text->Length == 0 ? S"*" : this->txtFilter->Text, this->chkRecursive->Checked, VTFCreateOptions);

			this->Cursor = System::Windows::Forms::Cursors::Default;
		}

		private: System::Void btnClose_Click(System::Object *  sender, System::EventArgs *  e)
		{
			this->DialogResult = System::Windows::Forms::DialogResult::Cancel;
		}

		private: System::Void btnOptions_Click(System::Object *  sender, System::EventArgs *  e)
		{
			this->Options->ShowDialog();
		}

		private: void Log(String *sString, System::Drawing::Color cColor)
		{
			this->txtLog->Focus();
			this->txtLog->SelectionStart = this->txtLog->TextLength;
			this->txtLog->SelectionColor = cColor;
			this->txtLog->AppendText(String::Concat(sString, S"\n"));
			this->txtLog->Refresh();
		}
	};
}