
#define ARGS_NO_Q
#include once "crt.bi"
#include once "ab.bi"
' #include once "args.bi"
#include once "file.bi"
#include once "lex.bi"
#include once "expr.bi"
#include once "src.bi"


Dim Shared fIn_name As String
Dim Shared fOut_name As String

Type BasicCompiler
	Private:
		_src As BasicSrc
		lines_are_disabled As Long
		int_size As Long
	Public:
		Declare Constructor()
		Declare Sub Init(s As String)
		Declare Sub disableLineNumbers()
		Declare Sub setIntSize(n As Long)
		Declare Function wordName() As String
		Declare Function dataDefName() As String
		Declare Function registerName(code As String) As String
		Declare Sub printExprVal(e As Expr Ptr)
		Declare Sub printExprMul(e As Expr Ptr)
		Declare Sub printExprAdd(e As Expr Ptr)
		Declare Sub printExprCmp(e As Expr Ptr)
		' jumping  when result of e equals to cond 
		Declare Sub printCondJump(e As Expr Ptr, cond As Long, label As String)
		Declare Sub printExpr(e As Expr Ptr)
		Declare Sub printCondJmp(e As Expr Ptr, label As String)
		Declare Sub compile()
End Type


Constructor BasicCompiler()
End Constructor

Sub BasicCompiler.Init(s As String)
	_src.Init(s)

	lines_are_disabled = 0
	int_size = 32
End Sub


Sub BasicCompiler.disableLineNumbers()
	lines_are_disabled = 1
End Sub

Sub BasicCompiler.setIntSize(n As Long)
	if n <> 16 And n <> 32 And n <> 64 Then  Return

	int_size = n
End Sub

Function BasicCompiler.wordName() As String
	If int_size = 64 Then  Return "qword"
	If int_size = 32 Then  Return "dword"
	Return "word"
End Function

Function BasicCompiler.dataDefName() As String
	If int_size = 64 Then  Return "dq"
	If int_size = 32 Then  Return "dd"
	Return "dw"
End Function

Function BasicCompiler.registerName(code As String) As String
	If InStr(1, "acdb", code) = 0 Then  Return "error_reg"
	If int_size = 64 Then  Return "r" + code + "x"
	If int_size = 32 Then  Return "e" + code + "x"
	Return code + "x"
End Function

Sub BasicCompiler.printExprVal(e As Expr Ptr)
	Dim s As String
	If e->GetArgc() <> 0 Then
		print "* error *"
	Else
		s = e->GetVal()
		If e->GetType() = "Integer" Then  print ";int"
		If IsNum(Mid$(s, 1,1)) Then
			print "push " + wordName() + " " + s
		Else
			print "push " + wordName() + "[" + s + "]"
		End If
	End If
End Sub

Sub BasicCompiler.printExprMul(e As Expr Ptr)
	Dim i As Long
	Dim s As String
	s = e->GetOpr()
	If (s = "*") Or (s = "/") Or (s = "MOD") Then
		If e->GetArgc() <> 2 Then
			print "* error in mul/div/mod *"
		Else
			For i = 0 To 1
				printExprMul(e->GetArgv(i))
			Next
			print "pop " + registerName("c")
			print "pop " + registerName("a")
			If s = "*" Then
				print "imul " + registerName("c")
			Else
				print "xor " + registerName("d") + ", " + registerName("d")
				print "idiv " + registerName("c")
			End If

			If s = "MOD" Then
				print "push " + registerName("d")
			Else
				print "push " + registerName("a")
			End If
		End If
	Else
		printExprVal(e)
	End If
End Sub
Sub BasicCompiler.printExprAdd(e As Expr Ptr)
	Dim s As String
	Dim i As Long
	s = e->GetOpr()
	If ((s = "+") Or (s = "-")) And (e->GetArgc() = 2) Then
		For i = 0 To 1
			printExprAdd(e->GetArgv(i))
		Next
		print "pop " + registerName("c")
		print "pop " + registerName("a")
		If s = "+" Then
			print "add " + registerName("a") + ", " + registerName("c")
		Else
			print "sub " + registerName("a") + ", " + registerName("c")
		End If
		print "push " + registerName("a")
	Else
		printExprMul(e)
	End If
End Sub
Sub BasicCompiler.printExprCmp(e As Expr Ptr)
	Dim s As String
	s = e->GetOpr()
	If (s = "<") Or (s = ">") Or (s = "=") Then
		print "* not supported operation *"
	Else
		printExprAdd(e)
	End If
End Sub

' jumping  when result of e equals to cond 
Sub BasicCompiler.printCondJump(e As Expr Ptr, cond As Long, label As String)
	Dim s As String
	s = e->GetOpr()
	If (s = "<") Or (s = ">") Or (s = "=") Then
		If e->GetArgc() <> 2 Then
			print "* error in conditional expression *"
			Exit Sub
		End If

		If s = ">" Then
			If cond Then s = "jg" Else s = "jle"
		ElseIf s = "<" Then
			If cond Then s = "jl" Else s = "jge"
		ElseIf s = "=" Then
			If cond Then s = "je" Else s = "jne"
		Else
			print "* error not supported operation *"
		End If

		printExprCmp(e->GetArgv(0))
		printExprCmp(e->GetArgv(1))
		print "pop " + registerName("d")
		print "pop " + registerName("a")
		print "cmp " + registerName("a") + ", " + registerName("d")
		print s + " " + label
	Else
		If cond Then s = "jnz" Else s = "jz"
		printExprAdd(e)
		print "or " + registerName("a") + ", " + registerName("a")
		print s + " " + label
	End If
End Sub

Sub BasicCompiler.printExpr(e As Expr Ptr)
	_src.InferExprType(e, 1)
	printExprCmp(e)
End Sub

Sub BasicCompiler.printCondJmp(e As Expr Ptr, label As String)
End Sub

Sub BasicCompiler.compile()
	Dim e As Expr Ptr
	Dim i As Long
	Dim line_number As Long
	Dim new_line_number As Long
	Dim line_label_is_requird As Long
	Dim s As String

	If Not _src.IsOpened() Then
		Exit Sub
	End If

	print "bits " + Str(int_size)
	print "section .text"

	vars_count = 0
	line_number = 1
	While Not _src.AtEof()
		line_label_is_requird = 0
		If _src.AtEol() Then
			_src.NextLine()
			line_number += 1
			Continue While
		End If

		s = _src.ReadToken()
		If IsNum(Mid$(s, 1,1)) Then
			new_line_number = Int(Val(s))
			If new_line_number < line_number Then
				print "; error: line numbers can not be reused"
				Return
			End If
			line_number = new_line_number

			s = _src.ReadToken()
		End If

		If (s = "") Or (s = "REM") Or (s = "'") Then
			_src.NextLine()
			line_number += 1
			Continue While
		End If

		If lines_are_disabled = 0 Then
			print "BC_LINE_" + Str$(line_number) + ":"
		End If


		Do
			If s = "*" Then
				print _src.ReadToken() + ":"
				s = _src.ReadToken()
			End If

			If s = "GLOBAL" Then
				s = _src.ReadToken()
				print "global " + s
			ElseIf s = "EXTERN" Then
				s = _src.ReadToken()
				print "extern " + s
			ElseIf s = "GOTO" Then
				s = _src.ReadToken()
				If s = "*" Then
					s = _src.ReadToken()
				End If

				If IsNum(Mid$(s, 1,1)) Then
					s = "BC_LINE_" + s
				End If
				print "jmp " + s
			ElseIf s = "IF" Then
				e = _src.compileExpr()
				s = _src.ReadToken()
				If s = "THEN" Then
					s = _src.ReadToken()
				End If

				If IsNum(Mid$(s,1,1)) Then
					printCondJump(e,TRUE,"BC_LINE_" + s)
				Else
					printCondJump(e,FALSE,"BC_LINE_" + Str$(line_number) + ".END")
					line_label_is_requird = 1
				'	UnReadToken()
					Continue Do
				End If
			ElseIf s = "GOSUB" Or s = "CALL" Then
				s = _src.ReadToken()
				If s = "*" Then
					s = _src.ReadToken()
				End If

				If IsNum(Mid$(s, 1,1)) Then
					s = "BC_LINE_" + s
				End If
				print "call " + s
			ElseIf s = "RETURN" Then
				print "ret"
			ElseIf (s <> "") And (s <> ":") Then
				If s = "LET" Then
					s = _src.ReadToken()
				End If

				If _src.ReadToken()= "=" Then
					e = _src.compileExpr()
					printExpr(e)
				End If
				print "pop " + wordName() + "[" + UseVar(s) + "]"
			End If

			s = _src.ReadToken()
			If s = ":" Then
				s = _src.ReadToken()
			Else
				Exit Do
			End If
		Loop

		If lines_are_disabled = 0 Or line_label_is_requird = 1 Then
			print "BC_LINE_"+Str$(line_number) + ".END:"
			line_label_is_requird = 0
		End If

		_src.NextLine()
		line_number += 1
	Wend

	If 0 < vars_count Then
		print "section .data"
		For i = 0 To vars_count - 1
			print vars(i) + ": " + dataDefName() + " 0"
		Next
	End If
End Sub




Dim Shared no_lines As Long = 0
Dim Shared int_size As Long = 32


Sub compile(s As String)
	Dim fIn As BasicCompiler
	fIn.Init(s)
	If no_lines = 1 Then fIn.disableLineNumbers()
	fIn.setIntSize(int_size)
	fIn.compile()
End Sub

Function my_main cdecl Alias "main" (ByVal Argc As Long, ByVal Argv As ZString Ptr Ptr) As Long
	Dim i As Long
	Dim arg As String

	fOut_name = ""

	If argc <= 1 Then
		print "bc [option ...] [file ...]"
		print "  -nolines      supress implicit line_labels"
		print "  -o name       select output file (not implemented)"
		print "  -int N        select integer/pointer register size (16|32|64)"
		return 0
	End If

	i = 1
	While i < Argc
		arg = **(Argv + i)

		If Mid$(arg, 1,1) <> "-" Then
			Exit While
		End If

		If arg = "-o" Then
			i += 1
			If i < Argc Then
				fOut_name = **(Argv + i)
			End If
		ElseIf arg = "-int" Then
			i += 1
			If i < Argc Then
				int_size = CInt(**(Argv + i))
			End If
		ElseIf arg = "-nolines" Then
			no_lines = 1
		End If
		i += 1
	Wend

	If i < Argc Then
		fIn_name = **(Argv + i)
		If fOut_name = "" Then
			fOut_name = fIn_name + ".asm"
		End If

		compile(**(Argv + i))
	End If

	return 0
End Function

