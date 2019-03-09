# Keygen
can i get keygen for this software (incl : Hardisk ID and Serial )
Imports BarcodeLib
Imports RegMe.Properties
Imports System
Imports System.ComponentModel
Imports System.Drawing
Imports System.Drawing.Text
Imports System.IO
Imports System.Runtime.InteropServices
Imports System.Windows.Forms

Namespace RegMe
   Public Class frmMain
       Inherits Form

       Private fonts As PrivateFontCollection = New PrivateFontCollection()

       Private myFont As Font

       Private b As Barcode = New Barcode()

       Private components As IContainer = Nothing

       Private btnExit As Button

       Private pictureBox1 As PictureBox

       Private lblTitle As Label

       Private lblInfo As Label

       Private txtS1 As TextBox

       Private txtS2 As TextBox

       Private txtS3 As TextBox

       Private errorProvider1 As ErrorProvider

       Private barcode As PictureBox

       Public Function decrypt(toDeCrypt As String) As String
           Dim target As String = Me.getTarget("OK")
           Dim source As String = Me.getSource("OK")
           Dim text As String = Nothing
           For i As Integer = 0 To toDeCrypt.Length - 1
               If target.Contains(toDeCrypt.Substring(i, 1)) Then
                   Dim startIndex As Integer = target.IndexOf(toDeCrypt.Substring(i, 1))
                   text += source.Substring(startIndex, 1)
               End If
           Next
           Return text
       End Function

       Public Function getSource(ok As String) As String
           Dim text As String = Nothing
           For i As Integer = 32 To 127 - 1
               text += Char.ConvertFromUtf32(i)
           Next
           Return text
       End Function

       Public Function getTarget(ok As String) As String
           Dim text As String = Nothing
           For i As Integer = 126 To 32 Step -1
               text += Char.ConvertFromUtf32(i)
           Next
           Return text
       End Function

       Public Shared Declare Ansi Function Copyright Lib "A:\MI5_BIN\Qt5Text.dll" () As IntPtr

       Public Shared Declare Ansi Sub GetHDDVolumeSerial Lib "A:\MI5_BIN\Qt5Text.dll" ()

       Public Shared Declare Ansi Sub GetCPUID Lib "A:\MI5_BIN\Qt5Text.dll" ()

       Public Shared Declare Ansi Function GetFile Lib "A:\MI5_BIN\Qt5Text.dll" () As IntPtr

       Private Shared Declare Function AddFontMemResourceEx Lib "gdi32.dll" (pbFont As IntPtr, cbFont As UInteger, pdv As IntPtr, <[In]()> ByRef pcFonts As UInteger) As IntPtr

       Public Sub New()
           Me.InitializeComponent()
           Dim free3of As Byte() = AddressOf Resources.free3of9
           Dim intPtr As IntPtr = Marshal.AllocCoTaskMem(free3of.Length)
           Marshal.Copy(free3of, 0, intPtr, free3of.Length)
           Dim num As UInteger = 0U
           Me.fonts.AddMemoryFont(intPtr, AddressOf Resources.free3of9.Length)
           frmMain.AddFontMemResourceEx(intPtr, CUInt(AddressOf Resources.free3of9.Length), IntPtr.Zero, num)
           Marshal.FreeCoTaskMem(intPtr)
           Me.myFont = New Font(Me.fonts.Families(0), 40F)
       End Sub

       Private Sub Form1_Load(sender As Object, e As EventArgs)
           Me.lblInfo.Top = 150
           If Not File.Exists("A" + Me.decrypt("dBQUi?\UPBM*iJ9&*p:22")) Then
               Me.lblInfo.Text = "RAM Disk Not Found!"
           Else
               If File.Exists("D" + Me.decrypt("dBQUiBQKRU[YPKY\SpUPU")) Then
                   Dim text As String = Marshal.PtrToStringAnsi(frmMain.Copyright())
                   If text.Contains("2010") Then
                       frmMain.GetCPUID()
                       Dim text2 As String = Marshal.PtrToStringAnsi(frmMain.GetFile())
                       If text2 <> "mRS5+tFV+zO3X2a7zlmZrHp3fDxCW5PNIytRJYVHKsw=" Then
                           Me.lblInfo.Top = 150
                           Me.myFont = New Font(Me.fonts.Families(0), 20F)
                           Me.lblInfo.Font = Me.myFont
                           Me.lblInfo.Text = "Already register for " + Me.chkType(text2)
                       Else
                           Me.lblInfo.Top = 150
                           Me.myFont = New Font(Me.fonts.Families(0), 20F)
                           Me.lblInfo.Font = Me.myFont
                           Me.lblInfo.Text = "Wrong Key. Restart App!"
                           File.Delete("D" + Me.decrypt("dBQUiBQKRU[YPKY\SpUPU"))
                       End If
                   Else
                       Me.lblInfo.Text = Me.decrypt("G,/07~KHU~8529}")
                   End If
               Else
                   Dim text As String = Marshal.PtrToStringAnsi(frmMain.Copyright())
                   If text.Contains("2010") Then
                       frmMain.GetHDDVolumeSerial()
                       text = Marshal.PtrToStringAnsi(frmMain.GetFile())
                       Me.lblInfo.Top = 75
                       Me.lblInfo.Font = Me.myFont
                       Me.lblInfo.Text = text
                       Me.txtS1.Visible = True
                       Me.txtS2.Visible = True
                       Me.txtS3.Visible = True
                       Me.barcode.Visible = True
                       Me.genBarCode(text)
                       Me.txtS1.[Select]()
                   Else
                       Me.lblInfo.Text = Me.decrypt("G,/07~KHU~8529}")
                   End If
               End If
           End If
       End Sub

       Private Function chkType(rtnString As String) As String
           Dim result As String
           If rtnString = "AAAAnVfcJ6AhUwdVtJuhTKmlFPi+4aa4JqMhMeFV1ac=" Then
               result = Me.decrypt("LYQOJY")
           Else
               If rtnString = "CCCCnVfcJ6AhUwdVtJuhTKmlFPi+4aa4JqMhMeFV1ac=" Then
                   result = Me.decrypt("JOI[V")
               Else
                   If rtnString = "BBBBnVfcJ6AhUwdVtJuhTKmlFPi+4aa4JqMhMeFV1ac=" Then
                       result = Me.decrypt("J]\RYJ")
                   Else
                       If rtnString = "DDDDnVfcJ6AhUwdVtJuhTKmlFPi+4aa4JqMhMeFV1ac=" Then
                           result = Me.decrypt("[OQ\UPY")
                       Else
                           result = "NONE"
                       End If
                   End If
               End If
           End If
           Return result
       End Function

       Private Sub btnExit_Click(sender As Object, e As EventArgs)
           Application.[Exit]()
       End Sub

       Private Sub txtS1_TextChanged(sender As Object, e As EventArgs)
           Me.txtS1.CharacterCasing = CharacterCasing.Upper
           If Me.txtS1.Text.Length = 4 Then
               Me.txtS2.Focus()
           End If
       End Sub

       Private Sub txtS2_TextChanged(sender As Object, e As EventArgs)
           Me.txtS2.CharacterCasing = CharacterCasing.Upper
           If Me.txtS2.Text.Length = 4 Then
               Me.txtS3.Focus()
           End If
       End Sub

       Private Sub txtS3_TextChanged(sender As Object, e As EventArgs)
           Me.txtS3.CharacterCasing = CharacterCasing.Upper
           If Me.txtS3.Text.Length = 4 Then
               File.WriteAllText("D" + Me.decrypt("dBQUiBQKRU[YPKY\SpUPU"), Me.txtS1.Text + Me.txtS2.Text + Me.txtS3.Text)
               File.SetAttributes("D" + Me.decrypt("dBQUiBQKRU[YPKY\SpUPU"), FileAttributes.Hidden Or FileAttributes.System)
               frmMain.GetCPUID()
               Dim text As String = Marshal.PtrToStringAnsi(frmMain.GetFile())
               If text <> "mRS5+tFV+zO3X2a7zlmZrHp3fDxCW5PNIytRJYVHKsw=" Then
                   Me.txtS1.Visible = False
                   Me.txtS2.Visible = False
                   Me.txtS3.Visible = False
                   Me.barcode.Visible = False
                   Me.lblInfo.Top = 150
                   Me.myFont = New Font(Me.fonts.Families(0), 20F)
                   Me.lblInfo.Font = Me.myFont
                   Me.lblInfo.Text = "Successfully registered for " + Me.chkType(text)
               Else
                   MessageBox.Show("Wrong Serial!!!", "RegMe")
                   Me.txtS1.Text = ""
                   Me.txtS2.Text = ""
                   Me.txtS3.Text = ""
                   Me.txtS1.Focus()
               End If
           End If
       End Sub

       Private Sub genBarCode(genText As String)
           Me.errorProvider1.Clear()
           Dim num As Integer = 300
           Dim num2 As Integer = 75
           Me.b.set_Alignment(0)
           Dim tYPE As TYPE = 30
           Try
               If tYPE <> 0 Then
                   Me.barcode.BackgroundImage = Me.b.Encode(tYPE, genText, Color.Black, Color.White, num, num2)
               End If
           Catch ex As Exception
               MessageBox.Show(ex.Message)
           End Try
       End Sub

       Private Sub lblInfo_Click(sender As Object, e As EventArgs)
           Dim a As String = Me.decrypt("JENY~N]KKGOLZ~VYLY")
           frmMain.ShowInputDialog(a)
           If a = Me.decrypt("ZYRYJY") Then
               Me.lblInfo.Text = "Key Deleted. Restart App!"
               File.Delete("D" + Me.decrypt("dBQUiBQKRU[YPKY\SpUPU"))
           End If
       End Sub

       Private Shared Function ShowInputDialog(ByRef input As String) As DialogResult
           Dim clientSize As Size = New Size(300, 100)
           Dim form As Form = New Form()
           form.FormBorderStyle = FormBorderStyle.FixedDialog
           form.ClientSize = clientSize
           form.StartPosition = FormStartPosition.CenterScreen
           form.Text = "Key Delete Confirm!"
           Dim label As Label = New Label()
           label.Size = New Size(clientSize.Width - 10, 23)
           label.Location = New Point(5, 5)
           label.Text = "To unregister current key, type AUTH KEY"
           form.Controls.Add(label)
           Dim textBox As TextBox = New TextBox()
           textBox.Size = New Size(clientSize.Width - 10, 23)
           textBox.Location = New Point(5, 28)
           textBox.Text = input
           form.Controls.Add(textBox)
           Dim button As Button = New Button()
           button.DialogResult = DialogResult.OK
           button.Name = "okButton"
           button.Size = New Size(75, 23)
           button.Text = "&OK"
           button.Location = New Point(clientSize.Width - 80 - 80, 62)
           form.Controls.Add(button)
           Dim button2 As Button = New Button()
           button2.DialogResult = DialogResult.Cancel
           button2.Name = "cancelButton"
           button2.Size = New Size(75, 23)
           button2.Text = "&Cancel"
           button2.Location = New Point(clientSize.Width - 80, 62)
           form.Controls.Add(button2)
           form.AcceptButton = button
           form.CancelButton = button2
           Dim result As DialogResult = form.ShowDialog()
           input = textBox.Text
           Return result
       End Function

       Protected Overrides Sub Dispose(disposing As Boolean)
           If disposing AndAlso Me.components IsNot Nothing Then
               Me.components.Dispose()
           End If
           MyBase.Dispose(disposing)
       End Sub

       Private Sub InitializeComponent()
           Me.components = New Container()
           Me.btnExit = New Button()
           Me.pictureBox1 = New PictureBox()
           Me.lblTitle = New Label()
           Me.lblInfo = New Label()
           Me.txtS1 = New TextBox()
           Me.txtS2 = New TextBox()
           Me.txtS3 = New TextBox()
           Me.errorProvider1 = New ErrorProvider(Me.components)
           Me.barcode = New PictureBox()
           (CType(Me.pictureBox1, ISupportInitialize)).BeginInit()
           (CType(Me.errorProvider1, ISupportInitialize)).BeginInit()
           (CType(Me.barcode, ISupportInitialize)).BeginInit()
           MyBase.SuspendLayout()
           Me.btnExit.FlatAppearance.BorderSize = 0
           Me.btnExit.FlatStyle = FlatStyle.Flat
           Me.btnExit.Font = New Font("Verdana", 12F, FontStyle.Bold, GraphicsUnit.Point, 0)
           Me.btnExit.ForeColor = Color.White
           Me.btnExit.Location = New Point(430, 337)
           Me.btnExit.Name = "btnExit"
           Me.btnExit.Size = New Size(100, 50)
           Me.btnExit.TabIndex = 0
           Me.btnExit.Text = "Exit"
           Me.btnExit.UseVisualStyleBackColor = True
           AddHandler Me.btnExit.Click, AddressOf Me.btnExit_Click
           Me.pictureBox1.BackColor = Color.FromArgb(45, 45, 46)
           Me.pictureBox1.Location = New Point(0, 325)
           Me.pictureBox1.Name = "pictureBox1"
           Me.pictureBox1.Size = New Size(550, 75)
           Me.pictureBox1.TabIndex = 1
           Me.pictureBox1.TabStop = False
           Me.lblTitle.Font = New Font("Verdana", 30F, FontStyle.Bold)
           Me.lblTitle.ForeColor = Color.White
           Me.lblTitle.Location = New Point(0, 0)
           Me.lblTitle.Name = "lblTitle"
           Me.lblTitle.Size = New Size(175, 50)
           Me.lblTitle.TabIndex = 2
           Me.lblTitle.Text = "RegMe"
           Me.lblInfo.Font = New Font("Verdana", 24F, FontStyle.Italic, GraphicsUnit.Point, 0)
           Me.lblInfo.ForeColor = Color.White
           Me.lblInfo.Location = New Point(0, 75)
           Me.lblInfo.Name = "lblInfo"
           Me.lblInfo.Size = New Size(550, 50)
           Me.lblInfo.TabIndex = 3
           Me.lblInfo.Text = "RegMe"
           Me.lblInfo.TextAlign = ContentAlignment.MiddleCenter
           AddHandler Me.lblInfo.Click, AddressOf Me.lblInfo_Click
           Me.txtS1.Font = New Font("Verdana", 24F, FontStyle.Regular, GraphicsUnit.Point, 0)
           Me.txtS1.Location = New Point(109, 250)
           Me.txtS1.MaxLength = 4
           Me.txtS1.Name = "txtS1"
           Me.txtS1.Size = New Size(100, 46)
           Me.txtS1.TabIndex = 4
           Me.txtS1.TextAlign = HorizontalAlignment.Center
           Me.txtS1.Visible = False
           AddHandler Me.txtS1.TextChanged, AddressOf Me.txtS1_TextChanged
           Me.txtS2.Font = New Font("Verdana", 24F, FontStyle.Regular, GraphicsUnit.Point, 0)
           Me.txtS2.Location = New Point(225, 250)
           Me.txtS2.MaxLength = 4
           Me.txtS2.Name = "txtS2"
           Me.txtS2.Size = New Size(100, 46)
           Me.txtS2.TabIndex = 5
           Me.txtS2.TextAlign = HorizontalAlignment.Center
           Me.txtS2.Visible = False
           AddHandler Me.txtS2.TextChanged, AddressOf Me.txtS2_TextChanged
           Me.txtS3.Font = New Font("Verdana", 24F, FontStyle.Regular, GraphicsUnit.Point, 0)
           Me.txtS3.Location = New Point(341, 250)
           Me.txtS3.MaxLength = 4
           Me.txtS3.Name = "txtS3"
           Me.txtS3.Size = New Size(100, 46)
           Me.txtS3.TabIndex = 6
           Me.txtS3.TextAlign = HorizontalAlignment.Center
           Me.txtS3.Visible = False
           AddHandler Me.txtS3.TextChanged, AddressOf Me.txtS3_TextChanged
           Me.errorProvider1.ContainerControl = Me
           Me.barcode.BackgroundImageLayout = ImageLayout.Center
           Me.barcode.Location = New Point(75, 150)
           Me.barcode.Name = "barcode"
           Me.barcode.Size = New Size(400, 75)
           Me.barcode.TabIndex = 9
           Me.barcode.TabStop = False
           Me.barcode.Visible = False
           MyBase.AutoScaleDimensions = New SizeF(6F, 13F)
           MyBase.AutoScaleMode = AutoScaleMode.Font
           Me.BackColor = Color.FromArgb(60, 60, 61)
           MyBase.ClientSize = New Size(550, 400)
           MyBase.Controls.Add(Me.barcode)
           MyBase.Controls.Add(Me.txtS3)
           MyBase.Controls.Add(Me.txtS2)
           MyBase.Controls.Add(Me.txtS1)
           MyBase.Controls.Add(Me.lblInfo)
           MyBase.Controls.Add(Me.lblTitle)
           MyBase.Controls.Add(Me.btnExit)
           MyBase.Controls.Add(Me.pictureBox1)
           MyBase.FormBorderStyle = FormBorderStyle.None
           MyBase.Name = "frmMain"
           MyBase.StartPosition = FormStartPosition.CenterScreen
           Me.Text = "RegMe"
           AddHandler MyBase.Load, AddressOf Me.Form1_Load
           (CType(Me.pictureBox1, ISupportInitialize)).EndInit()
           (CType(Me.errorProvider1, ISupportInitialize)).EndInit()
           (CType(Me.barcode, ISupportInitialize)).EndInit()
           MyBase.ResumeLayout(False)
           MyBase.PerformLayout()
       End Sub
   End Class
End Namespace
