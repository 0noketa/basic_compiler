#ifndef src__sbp
#define src__sbp


#include once "crt.bi"
#include once "ab.bi"
#include once "lex.bi"
#include once "expr.bi"


Const max_vars_count = 256

Function UpperStr(s As String) As String
	Dim c As String
	Dim s2 As String
	Dim i As Long
	Dim j As Long

	s2 = ""
	j = 1
	For i = 1 To Len(s)
		s2 = s2 + Chr(toupper(Asc(Mid$(s, i, 1))))
	Next

	Return s2
End Function


Type VarDict
	Protected:
		_names(0 To max_vars_count) As String
		_types(0 To max_vars_count) As String
		_sizes(0 To max_vars_count) As Long
		_lbounds(0 To max_vars_count) As Long
		_attrs(0 To max_vars_count) As String
		_len As Long
	Public:
		Declare Constructor()
		Declare Function Count() As Long
		Declare Function GetVarName(_idx As Long) As String
		Declare Function GetVarIndex(_name As String) As Long
		Declare Function CorrectVarName(_name As String) As String
		Declare Function IsVar(_name As String) As Long
		Declare Function GetArrayLBound(_name As String) As Long
		Declare Function GetArrayLength(_name As String) As Long
		Declare Function IsArray(_name As String) As Long
		Declare Function IsExtern(_name As String) As Long
		Declare Function GetVarType(_name As String) As String
		Declare Sub AddVar(_name As String, _type As String, _size As Long, _lbound As Long, _attr As String)
		Declare Sub AddIntVar(_name As String)
		Declare Sub AddIntArray(_name As String, _len As Long, _lbound As Long)
		Declare Sub AddExternIntVar(_name As String)
End Type

Constructor VarDict()
	_len = 0
End Constructor
Function VarDict.Count() As Long
	Return _len
End Function
Function VarDict.GetVarName(_idx As Long) As String
	If (_idx < 0) Or (_idx >= _len) Then  Return ""
	Return _names(_idx)
End Function
Function VarDict.GetVarIndex(_name As String) As Long
	Dim i As Long
	Dim _name2 As String

	_name2 = UpperStr(_name)
	i = 0
	While i < _len
		If UpperStr(_names(i)) = _name2 Then  Return i

		i += 1
	Wend

	Return -1
End Function
Function VarDict.CorrectVarName(_name As String) As String
	Dim i As Long
	i = GetVarIndex(_name)
	Return GetVarName(i)
End Function
Function VarDict.IsVar(_name As String) As Long
	Return (GetVarIndex(_name) <> -1)
End Function
Function VarDict.GetArrayLBound(_name As String) As Long
	Dim i As Long = GetVarIndex(_name)
	If i = -1 Then  Return -1

	Return _lbounds(i)
End Function
Function VarDict.GetArrayLength(_name As String) As Long
	Dim i As Long = GetVarIndex(_name)
	If i = -1 Then  Return -1

	Return _sizes(i)
End Function
Function VarDict.IsArray(_name As String) As Long
	Return (GetArrayLength(_name) <> -1)
End Function
Function VarDict.IsExtern(_name As String) As Long
	Dim i As Long = GetVarIndex(_name)
	If i = -1 Then  Return (0 <> 0)

	Return (InStr(1, _attrs(i), "e") <> 0)
End Function
Function VarDict.GetVarType(_name As String) As String
	Dim i As Long = GetVarIndex(_name)
	If i = -1 Then  Return "error_type"

	Return _types(i)
End Function
Sub VarDict.AddVar(_name As String, _type As String, _size As Long, _lbound As Long, _attr As String)
	If IsVar(_name) Then  Return

	If _len < max_vars_count Then
		_names(_len) = _name
		_types(_len) = _type
		_sizes(_len) = _size
		_lbounds(_len) = _lbound
		_attrs(_len) = _attr
		_len += 1
	End If
End Sub
Sub VarDict.AddIntVar(_name As String)
	AddVar(_name, "Integer", -1, 0, "")
End Sub
Sub VarDict.AddIntArray(_name As String, _len As Long, _lbound As Long)
	AddVar(_name, "Integer", _len, _lbound, "")
End Sub
Sub VarDict.AddExternIntVar(_name As String)
	AddVar(_name, "Integer", -1, 0, "e")
End Sub



Dim Shared vars As VarDict

Function VarsCount() As Long
	Return vars.Count()
End Function

Function IsVar(s As String) As Long
	Return vars.IsVar(s)
End Function

Sub AddIntVar(s As String)
	If vars.IsVar(s) Then  Return

	vars.AddIntVar(s)
End Sub

Sub AddExternIntVar(s As String)
	If vars.IsVar(s) Then  Return

	vars.AddExternIntVar(s)
End Sub

Sub AddIntArray(s As String, _len As Long, _lbound As Long)
	If vars.IsVar(s) Then  Return

	vars.AddIntArray(s, _len, _lbound)
End Sub

Function UseVar(s As String) As String
	Dim result As String = s

	If IsVar(s) Then  Return vars.CorrectVarName(s)

	AddIntVar(s)

	Return result
End Function




Type BasicSrc
	Protected:
		_src As SrcFile
		lines_nums As Long Ptr
		lines As Expr Ptr
		lines_count As Long
	Public:
		Declare Constructor()
		Declare Sub Init( _name As String )
		Declare Destructor()
		Declare Sub loadArgs( e As Expr Ptr )
		Declare Function compileVal() As Expr Ptr
		Declare Function compileExprMul() As Expr Ptr
		Declare Function compileExprAdd() As Expr Ptr
		Declare Function compileExprCmp() As Expr Ptr
		Declare Function compileExpr() As Expr Ptr

		Declare Function InferExprType(e As Expr Ptr, update As Long) As String

		' from SrcFile
		Declare Function AtEol() As Long
		Declare Sub NextLine()
		Declare Function ReadToken() As String
		Declare Sub UnReadToken()
		Declare Sub TrimLeft(ByRef s As String)

		' from  OFile
		Declare Function Error() As Long
		Declare Function IsOpened() As Long
		Declare Function AtEof() As Long
		Declare Function ReadLn() As String
		Declare Sub WriteLn( s As String )
End Type


Constructor BasicSrc()
End Constructor

Sub BasicSrc.Init( _name As String )
	_src.Init(_name)

	_src.CurrentToken= ""
	lines_nums = NULL
	lines = NULL
End Sub

Destructor BasicSrc()
	If lines <> NULL Then Deallocate(lines)
	If lines_nums <> NULL Then Deallocate(lines_nums)
End Destructor

Sub BasicSrc.loadArgs( e As Expr Ptr )
	Dim s As String

	If ReadToken() = ")" Then  Exit Sub

	UnReadToken()

	Do
		e->AddArg( compileExpr() )
		s = ReadToken()
	Loop While s = ","

	If s <> ")" Then  UnReadToken()
End Sub

Function BasicSrc.compileVal() As Expr Ptr
	Dim e As Expr Ptr
	Dim s As String
	Dim s2 As String

	s= ReadToken()
	If s="(" Then
		e= compileExpr()
		ReadToken()
	ElseIf IsNum(Mid$(s, 1,1)) Then
		e= New Expr()
		e->SetOpr("val")
		e->SetVal(s)
	ElseIf IsNam(Mid$(s, 1,1)) Then
		s2 = ReadToken()
		If s2 = "(" Then
			e= New Expr()
			e->SetOpr("apply")
			e->SetVal(s)
			e->AddArg(compileExpr())
			ReadToken()
		Else
			UnReadToken()
			e= New Expr()
			e->SetOpr("val")
			e->SetVal(UseVar(s))
		End If
	Else
		UnReadToken()
		e= NULL
	End If
	Return e
End Function

Function BasicSrc.compileExprMul() As Expr Ptr
	Dim r As Expr Ptr
	Dim e As Expr Ptr
	Dim s As String

	r= compileVal()
	While TRUE
		s= ReadToken()
		If (s="*") Or (s="/") Or (s="MOD") Then
			e= New Expr()
			e->SetOpr(s)
			e->AddArg( r )
			e->AddArg( compileVal() )
			r= e
		Else
			If s<>"" Then  UnReadToken()

			Return r
			Exit While
		End If
	Wend
End Function
Function BasicSrc.compileExprAdd() As Expr Ptr
	Dim r As Expr Ptr
	Dim e As Expr Ptr
	Dim s As String

	r= compileExprMul()
	While TRUE
		s= ReadToken()
		If (s="+") Or (s="-") Then
			e= New Expr()
			e->SetOpr(s)
			e->AddArg( r )
			e->AddArg( compileExprMul() )
			r= e
		Else
			If s<>"" Then  UnReadToken()

			Return r
		End If
	Wend
End Function
Function BasicSrc.compileExprCmp() As Expr Ptr
	Dim r As Expr Ptr
	Dim e As Expr Ptr
	Dim s As String

	r= compileExprAdd()
	While TRUE
		s= ReadToken()
		If (s="<") Or (s=">") Or (s="=") Then
			e= New Expr()
			e->SetOpr(s)
			e->AddArg( r )
			e->AddArg( compileExprAdd() )
			r= e
		Else
			If s<>"" Then  UnReadToken()

			Return r
			Exit While
		End If
	Wend
End Function

Function BasicSrc.compileExpr() As Expr Ptr
	Return compileExprCmp()
End Function

Function BasicSrc.InferExprType(e As Expr Ptr, update As Long) As String
	Dim left_type As String
	Dim right_type As String
	Dim result As String
	Dim _val As String

	_val = e->GetVal()
	result = e->GetType()
	If e->OprIs("val") Then
		If IsNum(Mid$(_val, 1,1)) Then
			result = "Integer"
		ElseIf Mid$(_val, 1,1) = Chr(34)  Then
			result = "String"
		ElseIf IsVar(_val) Then
			result = "Integer"
		End If
	ElseIf e->OprIs("+") Then
		If e->GetArgc() = 2 Then
			left_type = InferExprType(e->GetArgv(0), update)
			right_type = InferExprType(e->GetArgv(1), update)
			result = left_type
		End If
	ElseIf e->OprIs("-") Or  e->OprIs("*") Or e->OprIs("/") Or e->OprIs("MOD") Then
		If e->GetArgc() = 2 Then
			left_type = InferExprType(e->GetArgv(0), update)
			right_type = InferExprType(e->GetArgv(1), update)

			If left_type <> right_type Then
				result = "ErrorType"
			Else
				result = left_type
			End If
		End If
	End If

	If update <> 0 Then  e->SetType(result)

	Return result
End Function



Function BasicSrc.AtEol() As Long
    Return _src.AtEol()
End Function
Sub BasicSrc.NextLine()
    _src.NextLine()
End Sub
Function BasicSrc.ReadToken() As String
    Return _src.ReadToken()
End Function
Sub BasicSrc.UnReadToken()
    _src.UnReadToken()
End Sub
Sub BasicSrc.TrimLeft(ByRef s As String)
    _src.TrimLeft(s)
End Sub


Function BasicSrc.Error() As Long
	Return _src.Error()
End Function
Function BasicSrc.IsOpened() As Long
	Return _src.IsOpened()
End Function
Function BasicSrc.AtEof() As Long
	Return _src.AtEof()
End Function
Function BasicSrc.ReadLn() As String
	Return _src.ReadLn()
End Function
Sub BasicSrc.WriteLn( s As String )
	_src.WriteLn(s)
End Sub


#endif

