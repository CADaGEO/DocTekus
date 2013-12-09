Imports System.IO

Public Class IniFile

#Region "Déclarations"

    'Source du programme
    'http://www.developer.com/net/csharp/article.php/3287991/INI-Files-Will-Never-Die-How-To-in-NET.htm

    ' Déclarations
    Private Declare Ansi Function GetPrivateProfileString Lib "kernel32.dll" Alias "GetPrivateProfileStringA" _
      (ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal lpDefault As String, ByVal lpReturnedString As System.Text.StringBuilder, _
      ByVal nSize As Integer, ByVal lpFileName As String) As Integer
    Private Declare Ansi Function WritePrivateProfileString Lib "kernel32.dll" Alias "WritePrivateProfileStringA" _
      (ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal lpString As String, _
      ByVal lpFileName As String) As Integer
    Private Declare Ansi Function GetPrivateProfileInt Lib "kernel32.dll" Alias "GetPrivateProfileIntA" _
      (ByVal lpApplicationName As String, _
      ByVal lpKeyName As String, ByVal nDefault As Integer, ByVal lpFileName As String) As Integer
    Private Declare Ansi Function FlushPrivateProfileString Lib "kernel32.dll" Alias "WritePrivateProfileStringA" _
      (ByVal lpApplicationName As Integer, ByVal lpKeyName As Integer, ByVal lpString As Integer, ByVal lpFileName As String) As Integer
    Dim strFilename As String

#End Region

    ' Constructeur avec le nom du fichier
    Public Sub New(ByVal Filename As String)
        strFilename = Filename
    End Sub

    ' Propriété du nom du fichier en Read-Only
    ReadOnly Property FileName() As String
        Get
            Return strFilename
        End Get
    End Property

    Public Function GetAllLinesFromSection(ByVal Section As String)
        Dim colAllLines As New Collection
        Dim strIniLine As String = ""
        Dim myReader As StreamReader = New StreamReader(strFilename)
        'On lit le fichier jusqu'à tomber sur la bonne section
        Do
            strIniLine = myReader.ReadLine
        Loop Until strIniLine = "[" & Section & "]"
        'On écrit dans la collection jusqu'à changer de section ou arriver à la fin du fichier
        Do
            strIniLine = myReader.ReadLine
            If strIniLine.StartsWith("[") = False And strIniLine <> "" And strIniLine <> " " Then
                colAllLines.Add(strIniLine)
            Else
                Exit Do
            End If
        Loop
        myReader.Close()
        Return colAllLines
    End Function

    Public Function GetString(ByVal Section As String, ByVal Key As String, ByVal [Default] As String) As String
        ' Retourne un String depuis le fichier INI
        Dim intCharCount As Integer
        Dim objResult As New System.Text.StringBuilder(256)
        intCharCount = GetPrivateProfileString(Section, Key, [Default], objResult, objResult.Capacity, strFilename)
        If intCharCount > 0 Then GetString = Left(objResult.ToString, intCharCount)
    End Function

    Public Function GetInteger(ByVal Section As String, ByVal Key As String, ByVal [Default] As Integer) As Integer
        ' Retourne un integer depuis le fichier INI
        Return GetPrivateProfileInt(Section, Key, [Default], strFilename)
    End Function

    Public Function GetBoolean(ByVal Section As String, ByVal Key As String, ByVal [Default] As Boolean) As Boolean
        ' Retourne un booleen depuis le fichier INI
        Return (GetPrivateProfileInt(Section, Key, CInt([Default]), strFilename) = 1)
    End Function

    Public Sub WriteString(ByVal Section As String, ByVal Key As String, ByVal Value As String)
        ' Ecrit un String dans le fichier INI
        WritePrivateProfileString(Section, Key, Value, strFilename)
        Flush()
    End Sub

    Public Sub WriteInteger(ByVal Section As String, ByVal Key As String, ByVal Value As Integer)
        ' Ecrit un Integer dans le fichier INI
        WriteString(Section, Key, CStr(Value))
        Flush()
    End Sub

    Public Sub WriteBoolean(ByVal Section As String, ByVal Key As String, ByVal Value As Boolean)
        ' Ecrit un booleen dans le fichier INI
        WriteString(Section, Key, CStr(CInt(Value)))
        Flush()
    End Sub

    Private Sub Flush()
        ' Stores all the cached changes to your INI file
        FlushPrivateProfileString(0, 0, 0, strFilename)
    End Sub

End Class