#ifndef lex__sbp
#define lex__sbp

#include once "crt.bi"
#include once "ab.bi"
#include once "file.bi"
#include once "array.bi"


Function IsNamHead(c As Long) As Long
	Return _
		( (Asc("A")<=c) And (c<=Asc("Z")) ) Or _
		( (Asc("a")<=c) And (c<=Asc("z")) ) Or _
		(c=Asc("_"))
End Function

Function IsNum(c As Long) As Long
	Return _
		(Asc("0")<=c) And (c<=Asc("9"))
End Function

Function IsNam(c As Long) As Long
	Return IsNamHead(c) Or IsNum(c)
End Function

Function IsNotAny(c As Long) As Long
	Return FALSE
End Function

Function IsQrt(c As Long) As Long
	Return (c = 34)
End Function

Function IsNotQrt(c As Long) As Long
	Return (c <> 34)
End Function

Type SrcFile
	Protected:
		current_token As String
		current_line As String
		qt_pos As Long
		_file As OFile
	Public:
		Declare Property CurrentToken() As String 
		Declare Property CurrentToken(s As String) 
		Declare Property CurrentLine() As String 
		Declare Property CurrentLine(s As String) 
		Declare Property QtPos() As Long
		Declare Property QtPos(n As Long)

		Declare Constructor()
		Declare Sub Init(_name As String)
		Declare Function AtEol() As Long
		Declare Sub NextLine()
		Declare Function ReadToken() As String
		Declare Sub UnReadToken()
		Declare Sub TrimLeft(ByRef s As String)

		Declare Function GetTokenArrayFromLine() As BoxedStrArray Ptr

		' from  OFile
		Declare Function Error() As Long
		Declare Function IsOpened() As Long
		Declare Function AtEof() As Long
		Declare Function ReadLn() As String
		Declare Sub WriteLn( s As String )
End Type


Property SrcFile.CurrentToken() As String
	return current_token
End Property
Property SrcFile.CurrentToken(s As String) 
	current_token = s
End Property
Property SrcFile.CurrentLine() As String 
	return current_line
End Property
Property SrcFile.CurrentLine(s As String) 
	current_line = s
End Property
Property SrcFile.QtPos() As Long
	return qt_pos
End Property
Property SrcFile.QtPos(n As Long)
	qt_pos = n
End Property


Constructor SrcFile()
	current_token = ""
	current_line = ""
End Constructor

Sub SrcFile.Init(_name As String)
	_file.Init(_name, "r")
	current_token = ""
	current_line = ReadLn()
End Sub

Function SrcFile.AtEol() As Long
	Return (current_line="")
End Function

Sub SrcFile.NextLine()
	current_line = ReadLn()

	TrimLeft(current_line)
	qt_pos = InStr(1, current_line, Chr(39))
	If qt_pos = 0 Then
		qt_pos = Len(current_line)
	Else
		' Print "; comment: ", current_line
		current_line = Mid$(current_line, 1, qt_pos - 1)
		' Print ";        : ", current_line
	End If
End Sub

Function SrcFile.ReadToken() As String
	Dim i As Long
	Dim l As Long
	Dim f As Function(param_c As Long) As Long
	Dim sizeOfLastNoise As Long
	Dim c As Long
	Dim result As String
	TrimLeft(current_line)

	If (AtEof()) Or (AtEol()) Then
		result = ""
		current_token = result
		Return result
	End If

	l= Len(current_line)
	c= Asc(Mid$(current_line, 1,1))
	sizeOfLastNoise= 0
	If IsNamHead(c) Then
		f = ProcPtr(IsNam)
	ElseIf IsNum(c) Then
		f = ProcPtr(IsNum)
	ElseIf IsQrt(c) Then
		f = ProcPtr(IsNotQrt)
		sizeOfLastNoise= 1
	Else
		f = ProcPtr(IsNotAny)
	End If

	i= 1
	c= Asc(Mid$(current_line, 1,1))
	Do
		i += 1
		If l<i Then
			sizeOfLastNoise= 0
			Exit Do
		End If
		c = Asc(Mid$(current_line, i,1))
	Loop While f(c)

	i += sizeOfLastNoise

	result = Left$(current_line, i - 1)
	current_line = Right$(current_line, l - (i - 1))
	current_token = result

	Return result
End Function

Sub SrcFile.UnReadToken()
	current_line= current_token +" "+current_line
	current_token=""
End Sub

Sub SrcFile.TrimLeft(ByRef s As String)
	Dim i As Long
	Dim l As Long
	Dim spcs As String
	i= 1
	l= Len(s)
	spcs = Chr(10) + Chr(13) + Chr(9) + " "
	While i<=l
		If InStr(1, spcs, Mid$(s, i,1))=0 Then
			Exit While
		End If
		i += 1
	Wend

	s= Mid$(s, i)
End Sub

Function SrcFile.GetTokenArrayFromLine() As BoxedStrArray Ptr
	Dim result As BoxedStrArray Ptr = NewBoxedStrArray()
	Dim s As String

	While Len(current_line) > 0
		s = ReadToken()
		result->AddStr(s)
	Wend

	NextLine()
	Return result
End Function

Function SrcFile.Error() As Long
	Return _file.Error()
End Function
Function SrcFile.IsOpened() As Long
	Return _file.IsOpened()
End Function
Function SrcFile.AtEof() As Long
	Return _file.AtEof()
End Function
Function SrcFile.ReadLn() As String
	Dim s As String
	s =  _file.ReadLn()
	Return s
End Function
Sub SrcFile.WriteLn( s As String )
	_file.WriteLn(s)
End Sub


#endif
