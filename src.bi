#ifndef src__sbp
#define src__sbp


#include once "crt.bi"
#include once "ab.bi"
#include once "lex.bi"
#include once "expr.bi"


Const max_vars_count = 256

Dim Shared vars(0 To max_vars_count) As String
Dim Shared vars_count As Long = 0

Function IsVar(s As String) As Long
	Dim i As Long

	i = 0
	While i < vars_count
		If vars(i) = s Then  Return (0 = 0)

		i+= 1
	Wend

	Return (0 <> 0)
End Function

Function UseVar(s As String) As String
	Dim result As String = s

	If IsVar(s) Then  Return result

	If vars_count < max_vars_count Then
		vars(vars_count) = s
		vars_count+ = 1
	End If

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
	lines_nums = NULL
	lines = NULL
End Constructor

Sub BasicSrc.Init( _name As String )
	_src.Init(_name)

	_src.CurrentToken= ""
End Sub

Destructor BasicSrc()
	If lines <> NULL Then Deallocate(lines)
	If lines_nums <> NULL Then Deallocate(lines_nums)
End Destructor

Sub BasicSrc.loadArgs( e As Expr Ptr )
	Dim s As String

	If ReadToken() = ")" Then
		Exit Sub
	End If

	UnReadToken()

	Do
		e->AddArg( compileExpr() )
		s= ReadToken()
	Loop While s=","

	If s<>")" Then
		UnReadToken()
	End If
End Sub

Function BasicSrc.compileVal() As Expr Ptr
	Dim e As Expr Ptr
	Dim s As String

	s= ReadToken()
	If s="(" Then
		e= compileExpr()
		ReadToken()
	ElseIf IsNum(Mid$(s, 1,1)) Then
		e= New Expr()
		e->SetOpr("val")
		e->SetVal(s)
	ElseIf IsNam(Mid$(s, 1,1)) Then
		e= New Expr()
		e->SetOpr("val")
		e->SetVal(UseVar(s))
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
			If s<>"" Then
				UnReadToken()
			End If
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
			If s<>"" Then
				UnReadToken()
			End If
			Return r
			Exit While
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
			If s<>"" Then
				UnReadToken()
			End If

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

