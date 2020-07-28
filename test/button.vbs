Option Compare Database

Private Sub cmdFil_Click()
 On Error GoTo cmdFil_Click_err:

 Me.Visible = False
 DoCmd.OpenForm "frmFil_1"

 Exit Sub

cmdFil_Click_err:
  Select Case Err.Number
     Case Else
        MsgBox Err.Number & " - " & Err.Description, vbCritical + vbOKOnly, "System Error ..."
  End Select

End Sub

Private Sub cmdRunR_Click()

    Shell "Rscript ""C:\enc\DBtest\hello.R""", vbNormalFocus
    ' Shell "Rscript ""C:\Path\To\R\script.R""" & " " & qryName & " " & csvPath, vbNormalFocus

    MsgBox "Successfully run script!", vbInformation, "R process ..."

End Sub

Private Sub cmExit_Click()
    On Error GoTo cmdExit_Click_err:

    If MsgBox("Vil du avslutte?", vbYesNo + vbQuestion, "System question ...") = vbYes Then

    DoCmd.Quit

    End If

    Exit Sub

cmdExit_Click_err:
    Select Case Err.Number
    Case Else
    MsgBox Err.Number & " - " & Err.Description, vbCritical + vbOKOnly, "System Error ..."

    End Select

End Sub
