#ifndef expr__sbp
#define expr__sbp

#include once "crt.bi"
#include once "ab.bi"


' Type Ctx
' End Type

Type Expr
	Protected:
		self As Expr Ptr
		' ctx As Ctx Ptr
		argv As Expr Ptr Ptr
		argc As Long
		opr As String
		_val As String
		_type As String

	Public:
		Declare Constructor()
		Declare Destructor()
		Declare Function MakeExpr() As Expr Ptr
		Declare Function Clone() As Expr Ptr
		Declare Function SetSelf(e As Expr Ptr) As Expr Ptr
		Declare Function SetOpr(s As String) As Expr Ptr
		Declare Function SetVal(s As String) As Expr Ptr
		Declare Function SetType(s As String) As Expr Ptr
		Declare Function AddArg(e As Expr Ptr) As Expr Ptr
		Declare Function GetSelf() As Expr Ptr
		Declare Function GetOpr() As String
		Declare Function GetVal() As String
		Declare Function GetType() As String
		Declare Function GetArgc() As Long
		Declare Function GetArgv(i As Long) As Expr Ptr
		Declare Function OprIs(s As String) As Long
End Type


Constructor Expr()
	self = NULL
	' ctx = NULL
	argv = Allocate(SizeOf(Expr Ptr))
	argc = 0
	opr = ""
	_val = ""
	_type = ""
End Constructor

Destructor Expr()
	Dim i As Long
	If argv <> NULL Then
		If 0 < argc Then
			For i = 0 To argc-1
				delete *(argv + i)
			Next
		End If

		Deallocate(argv)
	End If
End Destructor

Function Expr.MakeExpr() As Expr Ptr
	Dim e As Expr Ptr
	e = New Expr()
	' e->ctx = ctx
	Return e
End Function

Function Expr.Clone() As Expr Ptr
	Dim e As Expr Ptr
	Dim arg As Expr Ptr
	Dim i As Long

	e = New Expr()
	e->opr = opr
	' e->ctx = ctx
	e->_val = _val
	e->self = self
	For i = 0 To argc-1
		arg = (*(argv + i))->Clone()
		e->AddArg(arg)
	Next

	Return e
End Function

Function Expr.SetSelf(e As Expr Ptr) As Expr Ptr
	self = e
	return self
End Function

Function Expr.SetOpr(s As String) As Expr Ptr
	opr = s
	return self
End Function

Function Expr.SetVal(s As String) As Expr Ptr
	_val = s
	return self
End Function

Function Expr.SetType(s As String) As Expr Ptr
	_type = s
	return self
End Function

Function Expr.AddArg(e As Expr Ptr) As Expr Ptr
	argc+= 1
	argv= realloc(argv, SizeOf(Expr Ptr)*argc)
	*(argv+(argc-1)) = e
	return self
End Function

Function Expr.GetSelf() As Expr Ptr
	Return self
End Function

Function Expr.GetOpr() As String
	Return opr
End Function

Function Expr.GetVal() As String
	Return _val
End Function

Function Expr.GetType() As String
	Return _type
End Function


Function Expr.GetArgc() As Long
	Return argc
End Function

Function Expr.GetArgv(i As Long) As Expr Ptr
	If (0 <= i) And (i < argc) Then
		Return *(argv + i)
	Else
		Return NULL
	End If
End Function

Function Expr.OprIs(s As String) As Long
	Return (opr=s)
End Function




#endif
