//---------------------------------------------------------------------------
#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ComCtrls.hpp>
#include <vcl\Menus.hpp>
#include "TB97.hpp"
//---------------------------------------------------------------------------
class TDemoForm : public TForm
{
__published:	// IDE-managed Components
	TDock97 *TopDock;
	TToolbar97 *EditToolbar;
	TToolbarButton97 *FontButton;
	TToolbarButton97 *LeftButton;
	TToolbarButton97 *CenterButton;
	TToolbarButton97 *RightButton;
	TToolbarSep97 *MainSep1;
	TComboBox *FontCombo;
	TToolbar97 *MainToolbar;
	TToolbar97 *SampleToolbar;
	TToolbarButton97 *NewButton;
	TToolbarButton97 *OpenButton;
	TToolbarButton97 *SaveButton;
	TToolbarButton97 *PrintButton;
	TToolbarButton97 *PrintPreviewButton;
	TToolbarButton97 *CutButton;
	TToolbarButton97 *CopyButton;
	TToolbarButton97 *PasteButton;
	TToolbarSep97 *MainSep2;
	TToolbarSep97 *EditSep1;
    TEdit97 *SampleEdit1;
    TEdit97 *SampleEdit2;
	TDock97 *LeftDock;
	TMemo *Memo;
	TDock97 *RightDock;
	TDock97 *BottomDock;
	TStatusBar *StatusBar;
	TPopupMenu *ToolbarPopupMenu;
	TMenuItem *TPMain;
	TMenuItem *TPEdit;
	TMainMenu *MainMenu;
	TMenuItem *FMenu;
	TMenuItem *FExit;
	TMenuItem *VMenu;
	TMenuItem *VToolbars;
	TMenuItem *VTMain;
	TMenuItem *VTEdit;
	TMenuItem *VStatusBar;
    TMenuItem *TPSample;
    TMenuItem *VTSample;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FExitClick(TObject *Sender);
	void __fastcall VMenuClick(TObject *Sender);
	void __fastcall VToolbarsClick(TObject *Sender);
	void __fastcall ToolbarPopupMenuPopup(TObject *Sender);
	void __fastcall VTMainClick(TObject *Sender);
	void __fastcall VTEditClick(TObject *Sender);
	void __fastcall VTSampleClick(TObject *Sender);
	void __fastcall VStatusBarClick(TObject *Sender);
	void __fastcall FontButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TDemoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TDemoForm *DemoForm;
//---------------------------------------------------------------------------
#endif

