
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
		use_thread_local As Long
	Public:
		Declare Constructor()
		Declare Sub Init(s As String)
		Declare Sub disableLineNumbers()
		Declare Sub setIntSize(n As Long)
		Declare Sub enableThreadLocalStorage(_enable As Long)
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
		Declare Function tryCompileDecl() As Long
		Declare Sub compile()

		' from BasicSrc
		Declare Function ReadToken() As String
		Declare Sub UnReadToken()
End Type


Constructor BasicCompiler()
End Constructor

Sub BasicCompiler.Init(s As String)
	_src.Init(s)

	lines_are_disabled = 0
	int_size = 32
	use_thread_local = 0
End Sub


Sub BasicCompiler.disableLineNumbers()
	lines_are_disabled = 1
End Sub

Sub BasicCompiler.setIntSize(n As Long)
	if n <> 16 And n <> 32 And n <> 64 Then  Exit Sub

	int_size = n
End Sub
Sub BasicCompiler.enableThreadLocalStorage(_enable As Long)
	use_thread_local = _enable
End Sub

Function BasicCompiler.wordName() As String
	If int_size = 64 Then
		Return "qword"
	ElseIf int_size = 32 Then
		Return "dword"
	Else
		Return "word"
	End If
End Function

Function BasicCompiler.dataDefName() As String
	If int_size = 64 Then
		Return "q"
	ElseIf int_size = 32 Then
		Return "d"
	Else
		Return "w"
	End If
End Function

Function BasicCompiler.registerName(code As String) As String
	If InStr(1, "acdb", code) = 0 Then 
		Return "error_reg"
	ElseIf int_size = 64 Then
		Return "r" + code + "x"
	ElseIf int_size = 32 Then
		Return "e" + code + "x"
	Else
		Return code + "x"
	End If
End Function

Sub BasicCompiler.printExprVal(e As Expr Ptr)
	Dim e2 As Expr Ptr
	Dim _len As Long
	Dim _lbound As Long
	Dim s As String

	If e->GetOpr() = "apply" Then
		s = e->GetVal()
		If UpperStr(s) = "LEN" Then
			If e->GetArgc() = 0 Then
				print "* error *"
				Exit Sub
			End If

			e2 = e->GetArgv(0)
			s = e2->GetVal()
			If vars.IsArray(s) Then
				print "push " + Str(vars.GetArrayLength(s))
			Else
				print "push 1"
			End If
		ELseIf UpperStr(s) = "LBOUND" Then
				If e->GetArgc() = 0 Then
				print "* error *"
				Exit Sub
			End If

			e2 = e->GetArgv(0)
			s = e2->GetVal()
			If vars.IsArray(s) Then
				print "push " + Str(vars.GetArrayLBound(s))
			Else
				print "push 0"
			End If
		ELseIf UpperStr(s) = "UBOUND" Then
			If e->GetArgc() = 0 Then
					print "* error *"
					Exit Sub
			End If

			e2 = e->GetArgv(0)
			s = e2->GetVal()
			If vars.IsArray(s) Then
				print "push " + Str(vars.GetArrayLBound(s) + vars.GetArrayLength(s) - 1)
			Else
				print "push 0"
			End If
		ELseIf vars.IsProc(s) Then
			If e->GetArgc() > 0 Then
				If vars.GetParamType(s) = "" Then
					print "; no param function was called with args"
					Exit Sub
				End If

				printExpr(e->GetArgv(0))
			End If

			print "call " + vars.CorrectVarName(s)
			If e->GetArgc() > 0 Then
				print "pop " + registerName("d")
			End If

			If vars.GetResultType(s) = "" Then
				print "; void was returned. check is it not inside expr?" 
			Else
				print "push " + registerName("a")
			End If
		ElseIf vars.IsArray(s) Then
			If e->GetArgc() = 0 Then
					print "* error *"
					Exit Sub
			End If

			printExpr(e->GetArgv(0))
			_lbound = vars.GetArrayLBound(s)

			If int_size = 16 Then
				print "pop " + registerName("b")
				If _lbound <> 0 Then
					print "sub " + registerName("b") + ", " + Str(_lbound)
				End If
				print "shl " + registerName("b") + ", " + Str(int_size / 8)
				print "add " + registerName("b") + ", " + s
				print "push " + wordName() + "[" + registerName("b") + "]"
			Else
				print "pop " + registerName("a")
				If _lbound <> 0 Then
					print "sub " + registerName("a") + ", " + Str(_lbound)
				End If
				print "push " + wordName() + "[" + vars.CorrectVarName(s) + "+" + registerName("a") + "*" + Str(int_size / 8) + "]"
			End If
		End If
	Else
		If e->GetArgc() <> 0 Then
			print "* error *"
			Exit Sub
		End If

		s = e->GetVal()
		If IsNum(Asc(Mid$(s, 1,1))) Then
			print "push " + wordName() + " " + s
		Else
			print "push " + wordName() + "[" + vars.CorrectVarName(s) + "]"
		End If
	End If
End Sub

Sub BasicCompiler.printExprMul(e As Expr Ptr)
	Dim i As Long
	Dim s As String
	s = e->GetOpr()
	If (s = "*") Or (s = "/") Or (UpperStr(s) = "MOD") Then
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

			If UpperStr(s) = "MOD" Then
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

Function BasicCompiler.tryCompileDecl() As Long
	Dim procType As String
	Dim paramType As String
	Dim resultType As String
	Dim s As String
	Dim _name As String

	s = ReadToken()
	If UpperStr(s) = "SUB" Then
		procType = "SUB"
		paramType = ""
		resultType = ""
	ElseIf UpperStr(s) = "FUNCTION" Then
		procType = "FUNCTION"
		paramType = ""
		resultType = "Integer"
	Else
		Print "; declaration of unknown element " + s
		Return (0<>0)
	End If

	_name = ReadToken()
	print "extern " + _name

	s = ReadToken()
	If UpperStr(s) = "CDECL" Then
		s = ReadToken()
	End If

	If s = "(" Then
		s = ReadToken()
		If s <> ")" Then
			paramType = "Integer"

			If UpperStr(s) = "BYVAL" Then
				s = ReadToken()
			ElseIf UpperStr(s) = "BYREF" Then
				Print "; decl of procedure with ByRef params are not implemented"
				Return (0<>0)
			End If

			s = ReadToken()
			If UpperStr(s) = "AS" Then
				s = ReadToken()
				If UpperStr(s) <> "INTEGER" And UpperStr(s) <> "LONG" Then
					Print "; decl of procedure with non integer/log params are not implemented"
					Return (0<>0)
				End If

				paramType = "Integer"

				s = ReadToken()
				If s = "," Then
					Print "; decl of procedure with >=2 params are not implemented"
					Return (0<>0)
				End If
			End If
		End If

		If s <> ")" Then
			Print "; decl with unknown element " + s
			Return (0<>0)
		End If

		s = ReadToken()
		If UpperStr(s) <> "AS" Then
			UnReadToken()
		Else
			s = ReadToken()
			If UpperStr(s) <> "INTEGER" And UpperStr(s) <> "LONG" Then
				Print "; decl of procedure with non integer/log params are not implemented"
				Return (0<>0)
			End If

			resultType = "Integer"
			If procType = "SUB" Then
				Print "; decl of SUB procedure with result type description will be treated as FUNCTION"
			End If
		End If
	Else
		UnReadToken()
	End If

	If resultType = "" Then
		If paramType = "" Then
			vars.AddExternVoidProc(_name)
		Else
			vars.AddExternIntProc(_name)
		End If
	Else
		If paramType = "" Then
			vars.AddExternNullaryIntFunc(_name)
		Else
			vars.AddExternIntFunc(_name)
		End If
	End If

	Return (0=0)
End Function

Sub BasicCompiler.compile()
	Dim e As Expr Ptr
	Dim i As Long
	Dim _len As Long
	Dim _lbound As Long
	Dim line_number As Long
	Dim new_line_number As Long
	Dim line_label_is_requird As Long
	Dim prefix As String
	Dim s As String
	Dim s2 As String

	If Not _src.IsOpened() Then  Exit Sub

	print "bits " + Str(int_size)
	print "section .text"

	line_number = 1
	While Not _src.AtEof()
		line_label_is_requird = 0
		If _src.AtEol() Then
			_src.NextLine()
			line_number += 1
			Continue While
		End If

		s = ReadToken()
		If IsNum(Asc(Mid$(s, 1,1))) Then
			new_line_number = Int(Val(s))
			If new_line_number < line_number Then
				print "; error: line numbers can not be reused"
				Exit Sub
			End If
			line_number = new_line_number

			s = ReadToken()
		End If

		If (s = "") Or (UpperStr(s) = "REM") Or (s = "'") Then
			_src.NextLine()
			line_number += 1
			Continue While
		End If

		If lines_are_disabled = 0 Then
			print "BC_LINE_" + Str$(line_number) + ":"
		End If

		' QB/VB label
		If IsNam(Asc(Mid$(s, 1,1))) Then
			If ReadToken() = ":" Then
				print s + ":"
				s = ReadToken()
			Else
				UnReadToken()
			End If
		End If

		Do
			If s = "*" Then
				print ReadToken() + ":"
				s = ReadToken()
			End If

			If UpperStr(s) = "GLOBAL" Then
				s = ReadToken()
				print "global " + s
			ElseIf UpperStr(s) = "EXTERN" Then
				s = ReadToken()
				print "extern " + s
				AddExternIntVar(s)
			ElseIf UpperStr(s) = "DECLARE" Then
				If Not tryCompileDecl() Then Exit Sub
			ElseIf UpperStr(s) = "DIM" Then
				s = ReadToken()
				If ReadToken() = "(" Then
					s2 = ReadToken()
					_len = CInt(s2)
					_lbound = 1
					If UpperStr(ReadToken()) = "TO" Then
						_lbound = _len
						s2 = ReadToken()
						_len = CInt(s2) - _lbound + 1
						ReadToken()
					End If
					AddIntArray(s, _len, _lbound)
				Else
					UnReadToken()
					AddIntVar(s)
				End If
			ElseIf UpperStr(s) = "GOTO" Then
				s = ReadToken()
				If s = "*" Then
					s = ReadToken()
				End If

				If IsNum(Asc(Mid$(s, 1,1))) Then
					s = "BC_LINE_" + s
					If lines_are_disabled = 1 Then
						print "; line labels are disabled"
						Exit Sub
					End If
				End If
				print "jmp " + s
			ElseIf UpperStr(s) = "IF" Then
				e = _src.compileExpr()
				s = ReadToken()
				If UpperStr(s) = "THEN" Then
					s = ReadToken()
				End If

				If IsNum(Asc(Mid$(s,1,1))) Then
					printCondJump(e,TRUE,"BC_LINE_" + s)

					If lines_are_disabled = 1 Then
						print "; line labels are disabled"
						Exit Sub
					End If
				Else
					printCondJump(e,FALSE,"BC_LINE_" + Str$(line_number) + "_END")
					line_label_is_requird = 1
					' s is first tkn inside THEN
					Continue Do
				End If
			ElseIf UpperStr(s) = "RETURN" Then
				print "ret"
			ElseIf (s <> "") And (s <> ":") Then
				prefix = ""
				If UpperStr(s) = "LET" Then
					prefix = "LET"
					s = ReadToken()
				ElseIf UpperStr(s) = "CALL" Or UpperStr(s) = "GOSUB" Then
					prefix = "CALL"
					s = ReadToken()
				End If

				If prefix = "CALL" Then
					If s = "*" Then  s = ReadToken()

					If ReadToken() <> "(" Then
						UnReadToken()
					ElseIf vars.IsProc(s) Then
						If vars.GetParamType(s) = "" Then
							If ReadToken() <> ")" Then
								print "; nullary or label was called with args: " + s
								Exit Sub
							End If
						Else
							If ReadToken() = ")" Then
								print "; non nullary was called without args: " + s
								Exit Sub
							Else
								UnReadToken()
							End If

							e = _src.compileVal()
							printExpr(e)
							ReadToken()
						End If
					End If

					If IsNum(Asc(Mid$(s, 1,1))) Then
						s = "BC_LINE_" + s

						If lines_are_disabled = 1 Then
							print "; line labels are disabled"
							Exit Sub
						End If
					End If

					If vars.IsProc(s) Then
						print "call " + vars.CorrectVarName(s)

						If vars.GetParamType(s) <> "" Then
							print "pop " + registerName("d")
						End If
					Else
						print "call " + s
					End If
				Else
					s2 = ReadToken()
					If s2 = "(" Then
						_lbound = vars.GetArrayLBound(s)
						UnReadToken()
						e = _src.compileVal()
						printExpr(e)
						If ReadToken() = "=" Then
							If int_size = 16 Then
								e = _src.compileExpr()
								printExpr(e)
								print "pop " + registerName("a")
								print "pop " + registerName("b")
								If _lbound <> 0 Then
									print "sub " + registerName("b") + ", " + Str(_lbound)
								End If
								print "shl " + registerName("b") + ", " + Str(int_size / 8)
								print "add " + registerName("b") + ", " + vars.CorrectVarName(s)
								print "mov [" + registerName("b") + "], " + registerName("a")
							Else
								e = _src.compileExpr()
								printExpr(e)
								print "pop " + registerName("d")
								print "pop " + registerName("a")
								If _lbound <> 0 Then
									print "sub " + registerName("a") + ", " + Str(_lbound)
								End If
								print "mov [" + vars.CorrectVarName(s) + "+" + registerName("a") + "*" + Str(int_size / 8) + "], " + registerName("d")
							End If
						End If
					ElseIf s2 = "=" Then
						e = _src.compileExpr()
						printExpr(e)
						print "pop " + wordName() + "[" + UseVar(s) + "]"
					End If
				End If
			End If

			s = ReadToken()
			If s = ":" Then
				s = ReadToken()
			Else
				Exit Do
			End If
		Loop

		If lines_are_disabled = 0 Or line_label_is_requird = 1 Then
			print "BC_LINE_"+Str$(line_number) + "_END:"
			line_label_is_requird = 0
		End If

		_src.NextLine()
		line_number += 1
	Wend

	If 0 < vars.Count() Then
		If use_thread_local Then
			print "section .tbss"
		Else
			print "section .bss"
		End If
		For i = 0 To vars.Count() - 1
			s = vars.GetVarName(i)
			If vars.IsExtern(s) Then  Continue For

			If vars.IsArray(s) Then
				print s + ": res" + dataDefName() + " " + Str(vars.GetArrayLength(s))
			End If
		Next

		If use_thread_local Then
			print "section .tdata"
		Else
			print "section .data"
		End If
		For i = 0 To vars.Count() - 1
			s = vars.GetVarName(i)
			If vars.IsExtern(s) Then  Continue For

			If Not vars.IsArray(s) Then
				print s + ": d" + dataDefName() + " 0"
			End If
		Next
	End If
End Sub

Function BasicCompiler.ReadToken() As String
	Return _src.ReadToken()
End Function
Sub BasicCompiler.UnReadToken()
	_src.UnReadToken()
End Sub





Dim Shared no_line As Long = 0
Dim Shared int_size As Long = 32
Dim Shared use_thread_local As Long = 0


Sub compile(s As String)
	Dim fIn As BasicCompiler : fIn.Init(s)
	If no_line = 1 Then fIn.disableLineNumbers()
	fIn.setIntSize(int_size)
	fIn.enableThreadLocalStorage(use_thread_local)
	fIn.compile()
End Sub

Function my_main cdecl Alias "main" (ByVal Argc As Long, ByVal Argv As ZString Ptr Ptr) As Long
	Dim i As Long
	Dim arg As String

	fOut_name = ""

	If argc <= 1 Then
		print "bc [option ...] [file ...]"
		print "  -noline       supress implicit line-labels"
		print "  -o name       select output file (not implemented)"
		print "  -int N        select integer/pointer register size (16|32|64)"
		print "  -tls          thread_local"
		
		return 0
	End If

	i = 1
	While i < Argc
		arg = **(Argv + i)

		If Mid$(arg, 1,1) <> "-" Then  Exit While

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
		ElseIf arg = "-noline" Then
			no_line = 1
		ElseIf arg = "-tls" Then
			use_thread_local = 1
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

