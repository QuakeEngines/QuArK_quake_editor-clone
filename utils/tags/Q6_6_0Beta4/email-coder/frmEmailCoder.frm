VERSION 5.00
Begin VB.Form frmEmailCoder 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Email Coder"
   ClientHeight    =   4185
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   7230
   Icon            =   "frmEmailCoder.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   4185
   ScaleWidth      =   7230
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox txtText 
      Height          =   2475
      Left            =   60
      MultiLine       =   -1  'True
      TabIndex        =   1
      Top             =   60
      Width           =   7155
   End
   Begin VB.CommandButton cmdGO 
      Caption         =   "Encode/Decode"
      Height          =   1395
      Left            =   900
      TabIndex        =   0
      Top             =   2700
      Width           =   5535
   End
End
Attribute VB_Name = "frmEmailCoder"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdGO_Click()
txtText.Enabled = False
Refresh

Dim z As Long, a As String, b As String
a = txtText.Text
For z = 1 To Len(a)
    b = b & Chr$(Rot13(Mid$(a, z, 1)))
Next z
txtText.Text = b

txtText.Enabled = True
End Sub
Function Rot13(Character As String) As Byte
If Len(Character) <> 1 Then Exit Function
Rot13 = Asc(Character)
If Character >= "a" And Character <= "z" Then
    Rot13 = Rot13 + 13
    If Rot13 > Asc("z") Then Rot13 = Rot13 - 26
End If
If Character >= "A" And Character <= "Z" Then
    Rot13 = Rot13 + 13
    If Rot13 > Asc("Z") Then Rot13 = Rot13 - 26
End If
End Function
