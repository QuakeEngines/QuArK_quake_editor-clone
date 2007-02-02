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

#include "stdafx.h"
#include "VTFEdit.h"

int APIENTRY _tWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow)
{
	System::Threading::Thread::CurrentThread->ApartmentState = System::Threading::ApartmentState::STA;

	Application::EnableVisualStyles();
	Application::DoEvents();

	if(vlGetVersion() != VL_VERSION)
	{
		MessageBox::Show(S"Invalid VTFLib.dll version number.", S"VTFEdit", System::Windows::Forms::MessageBoxButtons::OK, System::Windows::Forms::MessageBoxIcon::Error);
		return 1;
	}

	if(hlGetInteger(HL_VERSION) != HL_VERSION_NUMBER)
	{
		MessageBox::Show(S"Invalid HLLib.dll version number.", S"VTFEdit", System::Windows::Forms::MessageBoxButtons::OK, System::Windows::Forms::MessageBoxIcon::Error);
		return 1;
	}

	ILuint uiImage;

	// Initialize DevIL.
	ilInit();

	ilEnable(IL_FILE_OVERWRITE);

	ilEnable(IL_ORIGIN_SET);
	ilOriginFunc(IL_ORIGIN_UPPER_LEFT);

	ilGenImages(1, &uiImage);
	ilBindImage(uiImage);

	Application::Run(new VTFEdit::CVTFEdit());

	// Shutdown DevIL.
	ilDeleteImages(1, &uiImage);
	ilShutDown();

	return 0;
}