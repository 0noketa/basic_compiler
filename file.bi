#ifndef file__sbp
#define file__sbp

#include once "crt.bi"
#include once "ab.bi"


Type OFile
	Protected:
		_name As String
		mode As String
		hdl As FILE Ptr
		_error As Long
		newline As String
	Public:
		Declare Constructor()
		Declare Sub Init(prm_name As String, prm_mode As String)
		Declare Destructor()
		Declare Function Error() As Long
		Declare Function IsOpened() As Long
		Declare Function AtEof() As Long
		Declare Function ReadLn() As String
		Declare Sub WriteLn( s As String )
End Type


Constructor OFile()
	_name = ""
	mode = "r"
	newline = Chr$(10)
	hdl = NULL
	_error = TRUE
End Constructor

Sub OFile.Init(prm_name As String, prm_mode As String)
	_name = prm_name
	mode = prm_mode
	newline = Chr$(10)
	hdl = fopen(_name, mode)
	_error = hdl <> NULL			
End Sub

Destructor OFile()
	If hdl <> NULL Then
		fclose(hdl)
	End If
End Destructor

Function OFile.Error() As Long
	Return _error
End Function

Function OFile.IsOpened() As Long
	Return hdl <> NULL
End Function

Function OFile.AtEof() As Long
	Return (hdl = NULL) Or (feof(hdl) <> 0)
End Function

Function OFile.ReadLn() As String
	Dim s As BytePtr
	Dim p As BytePtr
	Dim q As BytePtr
	Dim result As String

	If (mode <> "r") Or (hdl = NULL) Then  Return ""

	If feof(hdl) <> 0 Then  Return ""

	s = Allocate(SizeOf(Byte) * 512)
	If s = NULL Then  Return ""

	fgets(s, 510, hdl)
	p = strchr(s, 10)
	q = strchr(s, 13)

	If q <> NULL Then  *(q + 0) = 0
	If p <> NULL Then  *(p + 0) = 0

	result = MakeStr(s)

	Deallocate(s)
	Return result
End Function


Sub OFile.WriteLn( s As String )
	If (mode <> "w") Or (hdl = NULL) Then  Exit Sub

	s = s + newline
	fputs(StrPtr(s), hdl)
End Sub

#endif
