//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "Demo1.h"
//---------------------------------------------------------------------------
#pragma link "TB97"
#pragma resource "*.dfm"
TDemoForm *DemoForm;
//---------------------------------------------------------------------------
__fastcall TDemoForm::TDemoForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::FormCreate(TObject *Sender)
{
	// Use the SetSlaveControl method of a TToolbar97 to configure a separate
	// top/bottom docked and left/right docked version of a control.
	// Please see TB97.DOC for more info on slave controls.

	// The line below tells it that FontCombo is the top/bottom docked version,
	// and FontButton is the left/right docked version. */
    EditToolbar->SetSlaveControl (FontCombo, FontButton);
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::FExitClick(TObject *Sender)
{
	Close();
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::VMenuClick(TObject *Sender)
{
	VStatusBar->Checked = StatusBar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::VToolbarsClick(TObject *Sender)
{
	VTMain->Checked = MainToolbar->Visible;
	VTEdit->Checked = EditToolbar->Visible;
	VTSample->Checked = SampleToolbar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::ToolbarPopupMenuPopup(TObject *Sender)
{
	TPMain->Checked = MainToolbar->Visible;
	TPEdit->Checked = EditToolbar->Visible;
	TPSample->Checked = SampleToolbar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::VTMainClick(TObject *Sender)
{
	MainToolbar->Visible = !MainToolbar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::VTEditClick(TObject *Sender)
{
	EditToolbar->Visible = !EditToolbar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::VTSampleClick(TObject *Sender)
{
	SampleToolbar->Visible = !SampleToolbar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::VStatusBarClick(TObject *Sender)
{
	StatusBar->Visible = !StatusBar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::FontButtonClick(TObject *Sender)
{
	ShowMessage ("A font dialog could come up here.");
}
//---------------------------------------------------------------------------
