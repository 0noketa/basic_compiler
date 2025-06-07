' Option Explicit
#define ARGS_NO_Q
#include once "crt.bi"
#include once "ab.bi"
' #include once "args.bi"
#include once "file.bi"
#include once "lex.bi"
#include once "expr.bi"
#include once "statement.bi"
#include once "environment.bi"
#include once "src.bi"


Dim Shared fIn_name As String
Dim Shared fOut_name As String
Dim Shared vars As Environment

Type BasicCompiler
	Private:
		_src As BasicSrc
		lines_are_disabled As Long
		int_size As Long
		use_thread_local As Long
		_n_tmp_labels As Long
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
		Declare Sub printExprAssign(e As Expr Ptr)
		' jumping  when result of e equals to cond 
		Declare Sub printCondJump(e As Expr Ptr, cond As Long, label As String)
		Declare Function tryPrintStatement(stmt As Statement Ptr, ByRef line_number As Long) As Long
		Declare Function tryPrintRootStatement(stmt As Statement Ptr, ByRef line_number As Long) As Long
		Declare Sub printExpr(e As Expr Ptr)
		Declare Sub printCondJmp(e As Expr Ptr, label As String)
		Declare Function tryCompileDecl() As Long
		Declare Function tryLoadDeclsFromStatement(stmt As Statement Ptr, ByRef env As Environment Ptr) As Long
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
	if n <> 16 And n <> 32 And n <> 64 Then  Return

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
	If int_size = 64 Then
		Return "r" + code
	ElseIf int_size = 32 Then
		Return "e" + code
	Else
		Return code
	End If
End Function

Sub BasicCompiler.printExprVal(e As Expr Ptr)
	Dim e2 As Expr Ptr
	Dim _len As Long
	Dim _lbound As Long
	Dim s As String
	Dim i As Long

	If e->GetOpr() = EXPR_APPLY Then
		s = e->GetVal()
		If UCase(s) = "SIZE" Then
			If e->GetArgc() = 0 Then
				print "; no arg for " + s
				Return
			End If

			e2 = e->GetArgv(0)
			s = e2->GetVal()
			If vars.IsArray(s) Then
				print "push " + Str(vars.GetArrayLength(s)) + "  ; SIZE("+ s +")"
			Else
				print "; SIZE with non-array value. replaced with 1."
				print "push 1"
			End If
		ElseIf UCase(s) = "LEN" Then
			If e->GetArgc() = 0 Then
				print "; no arg for " + s
				Return
			End If

			e2 = e->GetArgv(0)
			s = e2->GetVal()
			If vars.IsArray(s) Then
				print "push " + Str(vars.GetArrayLength(s)) + "  ; LEN("+ s +")"
			ElseIf vars.IsStrVar(s) Then
				print "xor " + registerName("ax") + ", " + registerName("ax")
				print "mov " + registerName("dx") + ", " + registerName("sp")
				print "jmp BC_TMP_" + Str$(_n_tmp_labels + 1)
				print "BC_TMP_" + Str$(_n_tmp_labels) + ":"
				print "add " + registerName("dx") + ", " + Str$(int_size)
				print "add " + registerName("ax") + ", 1"
				print "BC_TMP_" + Str$(_n_tmp_labels + 1) + ":"
				print "cmp [" + registerName("dx") + "], 0"
				print "jnz BC_TMP_" + Str$(_n_tmp_labels)
				print "push " + registerName("ax")

				_n_tmp_labels += 2
			ElseIf vars.IsStrVal(s) Then
				print "push " + Str$(Len(s) - 2)
			Else
				print "; LEN with non-array value. replaced with 1."
				print "push 1"
			End If
		ELseIf UCase(s) = "ASC" Then
			If e->GetArgc() = 0 Then
				print "; no arg for " + s
				Return
			End If

			e2 = e->GetArgv(0)
			s = e2->GetVal()
			If vars.IsArray(s) Then
				print "push " + Str(vars.GetArrayLBound(s)) + "  ;  LBOUND("+ s +")"
			ElseIf vars.IsStrVar(s) Then
				print "mov " + registerName("ax") + ", " + registerName("sp")
				print "jmp BC_TMP_" + Str$(_n_tmp_labels + 1)
				print "BC_TMP_" + Str$(_n_tmp_labels) + ":"
				print "add " + registerName("ax") + ", " + Str$(int_size)
				print "BC_TMP_" + Str$(_n_tmp_labels + 1) + ":"
				print "cmp [" + registerName("ax") + "], 0"
				print "jnz BC_TMP_" + Str$(_n_tmp_labels)
				print "mov " + registerName("sp") + ", " + registerName("ax")
				print "push [" + registerName("ax") + " + " +  Str$(int_size) + "]"

				_n_tmp_labels += 2
			ElseIf vars.IsStrVal(s) Then
				print "push " + Str$(Asc(Mid$(s, 2, 1)))
			Else
				print "; ASC with non array value. replaced with 0."
				print "push 0"
			End If
		ELseIf UCase(s) = "LBOUND" Then
			If e->GetArgc() = 0 Then
				print "; no arg for " + s
				Return
			End If

			e2 = e->GetArgv(0)
			s = e2->GetVal()
			If vars.IsArray(s) Then
				print "push " + Str(vars.GetArrayLBound(s)) + "  ;  LBOUND("+ s +")"
			Else
				print "; LBOUND with non array value. replaced with 0."
				print "push 0"
			End If
		ELseIf UCase(s) = "UBOUND" Then
			If e->GetArgc() = 0 Then
				print "; no arg for " + s
				Return
			End If

			e2 = e->GetArgv(0)
			s = e2->GetVal()
			If vars.IsArray(s) Then
				print "push " + Str(vars.GetArrayLBound(s) + vars.GetArrayLength(s) - 1)  + "  ;  UBOUND("+ s +")"
			Else
				print "; UBOUND with non array value. replaced with 0."
				print "push 0"
			End If
		ELseIf vars.IsProc(s) Then
			If e->GetArgc() <> vars.CountParams(s) Then
				print "; error. invalid number of args:" + s
				Return
			End If

			For i = e->GetArgc() - 1 To 0 Step -1
				printExpr(e->GetArgv(i))
			Next

			print "call " + vars.CorrectProcName(s)

			For i = 0 To e->GetArgc() - 1 
				print "pop " + registerName("dx")
			Next

			If vars.GetResultType(s) = TYPE_VOID Then
				print "; void was returned. is it not inside expr?" 
			Else
				print "push " + registerName("ax")
			End If
		ElseIf vars.IsArray(s) Then
			If e->GetArgc() = 0 Then
				print "; error. array with empty indexer."
				Return
			ElseIf e->GetArgc() <> 1 Then
				print "; error. multi-dimensional array is not implemented."
				Return
			End If

			printExpr(e->GetArgv(0))
			_lbound = vars.GetArrayLBound(s)

			If int_size = 16 Then
				print "pop " + registerName("bx")
				If _lbound <> 0 Then
					print "sub " + registerName("bx") + ", " + Str(_lbound)
				End If
				print "shl " + registerName("bx") + ", " + Str(int_size / 8)
				print "add " + registerName("bx") + ", " + s
				print "push " + wordName() + "[" + registerName("bx") + "]"
			Else
				print "pop " + registerName("ax")
				If _lbound <> 0 Then
					print "sub " + registerName("ax") + ", " + Str(_lbound)
				End If
				print "push " + wordName() + "[" + vars.CorrectVarName(s) + "+" + registerName("ax") + "*" + Str(int_size / 8) + "]"
			End If
		End If
	Else
		If e->GetArgc() <> 0 Then
			print "; * error val is: " + e->GetOpr()
			Return
		End If

		s = e->GetVal()
		If vars.IsNumVal(s) Then
			print "push " + wordName() + " " + s
		Else
			If Not vars.IsVar(s) Then
				Print "; inline decl " + s
				vars.AddIntVar(s)
			End If
			print "push " + wordName() + "[" + vars.CorrectVarName(s) + "]"
		End If
	End If
End Sub

Sub BasicCompiler.printExprMul(e As Expr Ptr)
	Dim i As Long
	Dim s As String
	s = e->GetOpr()
	If (s = "*") Or (s = "/") Or (UCase(s) = "MOD") Then
		If e->GetArgc() <> 2 Then
			print "* error in mul/div/mod *"
		Else
			For i = 0 To 1
				printExprVal(e->GetArgv(i))
			Next
			print "pop " + registerName("cx")
			print "pop " + registerName("ax")
			If s = "*" Then
				print "imul " + registerName("cx")
			Else
				print "xor " + registerName("dx") + ", " + registerName("dx")
				print "idiv " + registerName("cx")
			End If

			If UCase(s) = "MOD" Then
				print "push " + registerName("dx")
			Else
				print "push " + registerName("ax")
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
			printExprMul(e->GetArgv(i))
		Next
		print "pop " + registerName("cx")
		print "pop " + registerName("ax")
		If s = "+" Then
			s = "add"
		Else
			s = "sub"
		End If
		print s + " " + registerName("ax") + ", " + registerName("cx")
		print "push " + registerName("ax")
	Else
		printExprMul(e)
	End If
End Sub
Sub BasicCompiler.printExprCmp(e As Expr Ptr)
	Dim s As String
	s = e->GetOpr()
	If (s = "<") Or (s = ">") Or (s = "=") Then
		Select Case s
		Case "<"
			s = "l"
		Case ">"
			s = "g"
		Case "="
			s = "e"
		End Select

		print "pop " + registerName("cx")
		print "pop " + registerName("dx")
		print "xor " + registerName("ax") + ", " + registerName("ax")
		print "cmp " + registerName("dx") + ", " + registerName("cx")
		print "mov" + s + " " + registerName("ax") + ", 1"
		print "push " + registerName("ax")
	Else
		printExprAdd(e)
	End If
End Sub

Sub BasicCompiler.printExprAssign(e As Expr Ptr)
	Dim dst As Expr Ptr
	Dim s As String

	If e->GetOpr() = EXPR_ASSIGN Then
		printExprCmp(e->GetArgv(1))

		dst = e->GetArgv(0)
		s = dst->GetVal()
		If dst->GetOpr() = EXPR_APPLY Then
			printExprCmp(dst->GetArgv(0))

			print "pop " + registerName("ax")
			If Not vars.IsVar(s) Then
				Print "; error. unknown array: " + s
			End If

			print "pop " + wordName() + "[" + vars.CorrectVarName(s) + "+" + registerName("ax") + "*" + Str(int_size / 8) + "]"
		Else
			If Not vars.IsVar(s) Then
				Print "; inline decl " + s
				vars.AddIntVar(s)
			End If
			print "pop " + wordName() + "[" + vars.CorrectVarName(s) + "]"
		End If
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
			Return
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
		print "pop " + registerName("dx")
		print "pop " + registerName("ax")
		print "cmp " + registerName("ax") + ", " + registerName("dx")
		print s + " " + label
	Else
		If cond Then s = "jnz" Else s = "jz"
		printExprAdd(e)
		print "or " + registerName("ax") + ", " + registerName("ax")
		print s + " " + label
	End If
End Sub

Sub BasicCompiler.printExpr(e As Expr Ptr)
	_src.InferExprType(e, @vars, TRUE)
	printExprCmp(e)
End Sub

Sub BasicCompiler.printCondJmp(e As Expr Ptr, label As String)
End Sub

' separate numbered_statement from others
Function BasicCompiler.tryPrintStatement(stmt As Statement Ptr, ByRef line_number As Long) As Long
	Dim e As Expr Ptr
	Dim i As Long
	Dim s As String
	Dim goto_without_keyword As Long
	Dim cond As Long

	If stmt = NULL Then  Return TRUE

	Select Case stmt->GetOpr()
		Case STMT_EMPTY
			Return TRUE
		Case STMT_DECL_VAR
			' Print "; Dim " + stmt->GetStr(0)
			Return TRUE
		Case STMT_DECL_PROC
			' Print "; Declare Sub " + stmt->GetStr(0)
			Return TRUE
		Case STMT_BEGIN_SUB
			' Print "; Sub " + stmt->GetStr(0)
			Return TRUE
		Case STMT_BEGIN_FUNCTION
			' Print "; Function " + stmt->GetStr(0)
			Return TRUE
		Case STMT_STATEMENTS
			' Print "; statements"
			For i = 0 To stmt->CountStatements() - 1
				' Print "; .statement" + Str$(i)
				If tryPrintStatement(stmt->GetStatement(i),  line_number) = FALSE Then
					Return FALSE
				End If
			Next
			Return TRUE
		Case STMT_LABEL
			Print stmt->GetLabel() + ":"
			Return tryPrintStatement(stmt->GetStatement(0),  line_number)
		Case STMT_GOTO
			s = stmt->GetStr(0)
			If vars.IsNumVal(s) Then
				print "jmp BC_LINE_" + s
			Else
				print "jmp " + s
			End If
			Return TRUE
		Case STMT_GOSUB
			If stmt->CountExprs() = 0 Then
				s = stmt->GetStr(0)
				If vars.IsNumVal(s) Then
					print "call BC_LINE_" + s
				Else
					print "call " + s
				End If
			Else
				e = stmt->GetExpr(0)

				printExpr(e)

				s = e->GetVal()
				If vars.GetResultType(s) <> TYPE_VOID Then
					Print "pop " + registerName("dx")
				End If
			End If

			Return TRUE
		Case STMT_RETURN
			print "ret"
			Return TRUE
		Case STMT_IF
			e = stmt->GetExpr(0)
			s = e->GetOpr()
			' Print ";IF(" + s + ")"
			If (s = "<") Or (s = ">") Or (s = "=") Then
				If e->GetArgc() <> 2 Then
					print "* error in conditional expression *"
					Return FALSE
				End If

				Select Case s 
				Case ">"
					s = "jle"
				Case "<"
					s = "jge"
				Case "="
					s = "jne"
				Case Else
					print "* error not supported operation (" + s + ") *"
					Return FALSE
				End Select

				printExprCmp(e->GetArgv(0))
				printExprCmp(e->GetArgv(1))
				print "pop " + registerName("dx")
				print "pop " + registerName("ax")
				print "cmp " + registerName("ax") + ", " + registerName("dx")
			Else
				If goto_without_keyword Then s = "jnz" Else s = "jz"
				printExprAdd(e)
				print "or " + registerName("ax") + ", " + registerName("ax")
			End If

			If stmt->CountStatements() >= 2 Then
				print s + " BC_LINE_" + Str$(line_number) + "_ELSE"
			Else
				' print s + " BC_LINE_" + Str$(line_number) + "_END"
				print s + " BC_LINE_" + Str$(line_number + 1) + "  ;next"
			End If

			If tryPrintStatement(stmt->GetStatement(0),  line_number) = FALSE Then
				Return FALSE
			End If

			If stmt->CountStatements() >= 2 Then
				' print "jmp BC_LINE_" + Str$(line_number) + "_END"
				print "jmp BC_LINE_" + Str$(line_number + 1) + "  ;next"
				print "BC_LINE_" + Str$(line_number) + "_ELSE:"

				If tryPrintStatement(stmt->GetStatement(1),  line_number) = FALSE Then
					Return FALSE
				End If
			End If

			Return TRUE
		Case STMT_LET
			printExprAssign(stmt->GetExpr(0))
			Return TRUE
	End Select

	Print ";unknown2:" + stmt->GetOpr()
	Return FALSE
End Function

Function BasicCompiler.tryPrintRootStatement(stmt As Statement Ptr, ByRef line_number As Long) As Long
	Dim e As Expr Ptr
	Dim i As Long
	Dim s As String
	Dim cond As Long

	If stmt = NULL Then  Return TRUE

	i = stmt->GetLineNumber()
	If i <> -1 Then
		If i < line_number Then
			print "; error: line numbers can not be reused "
			Return FALSE
		End If

		If line_number <> i Then  line_number = i
	End If

	print "BC_LINE_" + Str$(line_number) + ":"

	If tryPrintStatement(stmt, line_number) Then
		' print "BC_LINE_" + Str$(line_number) + "_END:"
		Return TRUE
	Else
		Return FALSE
	End IF
End Function


Function BasicCompiler.tryLoadDeclsFromStatement(stmt As Statement Ptr, ByRef env As Environment Ptr) As Long
	Dim e As Expr Ptr
	Dim i As Long
	Dim _lbound As Long
	Dim _ubound As Long
	Dim attr As String
	Dim _name As String
	Dim resultType As String
	Dim _params As BoxedStrArray Ptr

	If stmt = NULL Then  Return TRUE

	Select Case stmt->GetOpr()
		Case STMT_EMPTY
			Return TRUE
		Case STMT_LABEL_OR_CALL
			Return TRUE
		Case STMT_DECL_PROC
			Print "; decl proc! argc:", stmt->CountStrs()
			attr = stmt->GetStr(0)
			_name = stmt->GetStr(1)
			resultType = stmt->GetStr(2)
			_params = NULL
			
			If stmt->CountStrs() > 3 Then
				Print "; params!"
				_params = NewBoxedStrArray()
				For i = 3 To stmt->CountStrs() - 1
					Print "; param: ", stmt->GetStr(i)
					_params->AddStr(stmt->GetStr(i)) 
				Next
			End If

			If _params <> NULL Then
				Print ";decl proc " + _name + "/" + Str$(_params->Count() / 2) + " as " + resultType +" attr:("+attr+")"
			Else
				Print ";decl proc " + _name + " as " + resultType +" attr:("+attr+")"
			End If

			env->AddProc(_name, _params, resultType, attr)
			If _params <> NULL Then  Delete _params
			Return TRUE
		Case STMT_DECL_VAR
			attr = stmt->GetStr(0)
			_name = stmt->GetStr(1)
			Print ";decl var " + _name + " attr: " + attr
			If stmt->CountStrs() = 5 Then
				_lbound = Int(Val(stmt->GetStr(3)))
				_ubound = Int(Val(stmt->GetStr(4)))
				If InStr(1, attr, ATTR_EXTERN) Then
					env->AddExternIntArray(_name, _ubound - _lbound + 1, _lbound)
				ElseIf InStr(1, attr, ATTR_GLOBAL) Then
					env->AddGlobalIntArray(_name, _ubound - _lbound + 1, _lbound)
				ElseIf stmt->CountStrs() = 5 Then
					env->AddIntArray(_name, _ubound - _lbound + 1, _lbound)
				End If
			Else
				If InStr(1, attr, ATTR_EXTERN) Then
					env->AddExternIntVar(_name)
				ElseIf InStr(1, attr, ATTR_GLOBAL) Then
					env->AddGlobalIntVar(_name)
				Else
					env->AddIntVar(_name)
				End If
			End If
			Return TRUE
		Case STMT_STATEMENTS
			For i = 0 To stmt->CountStatements() - 1
				If tryLoadDeclsFromStatement(stmt->GetStatement(i), env) = FALSE Then
					Return FALSE
				End If
			Next
			Return TRUE
		Case STMT_LABEL
			' pass to bottom at here
			Return tryLoadDeclsFromStatement(stmt->GetStatement(0), env)
		Case STMT_GOTO
			Return TRUE
		Case STMT_GOSUB
			Return TRUE
		Case STMT_RETURN
			Return TRUE
		Case STMT_IF
			Return TRUE
		Case STMT_LET
			Return TRUE
	End Select

	Print ";unknown:" + stmt->GetOpr()
	Return FALSE
End Function


Sub BasicCompiler.compile()
	Dim e As Expr Ptr
	Dim i As Long
	Dim _name As String
	Dim resultType As String
	Dim paramType As String
	Dim _len As Long
	Dim _lbound As Long
	Dim abs_line_number As Long
	Dim line_number As Long
	Dim src As Statement Ptr
	' Dim prog() As Statement Ptr
	Dim prog As StatementArray Ptr
	Dim s As String
	Dim s2 As String

	If Not _src.IsOpened() Then  Return

	print "bits " + Str(int_size)
	print "section .text"

	' Redim prog(0)
	prog = vars.GetStatements()

	abs_line_number = 1
	While Not _src.AtEof()
		Print ";line " + Str$(abs_line_number)
		src = _src.loadLine()  ' NULLable

		' Redim Preserve prog(0 To abs_line_number)
		' prog(abs_line_number - 1) = src
		prog->AddMovedStatement(src)

		abs_line_number += 1
		If src <> NULL Then  Print ";line is " + src->GetOpr()
	Wend

	' as next line of last line
	src = NewStatement(STMT_RETURN)
	prog->AddMovedStatement(src)

	Print "; ---- loaded entire source ----"
	' close file at here
	
	' For i = LBound(prog) To UBound(prog)
	For i = 0 To prog->Count() - 1
		' src = prog(i)
		src = prog->GetStatement(i)

		If src = NULL Then	
		ElseIf tryLoadDeclsFromStatement(src, @vars) = FALSE Then
			Print "; errornous decl"
			Return
		End If
	Next

	Print "; ---- defined suspended definitions ----"

	' error
	vars.NormalizeStatements()

	abs_line_number = 1
	line_number = 0
	' For i = LBound(prog) To UBound(prog)
	For i = 0 To prog->Count() - 1
		' src = prog(i)
		src = prog->GetStatement(i)
		If src <> NULL Then
			If src->GetOpr() <> STMT_EMPTY Then
				Print "; ####src#### : " + src->ToString()
			End If

			If tryPrintRootStatement(src,  line_number) = FALSE Then
				Print "; failed to print"
				Return
			End If
		End If

		line_number += 1
		abs_line_number += 1
	Next

	For i = 0 To vars.CountProcs() - 1
		s = vars.GetProcName(i)
		If vars.AttrInProc(s, ATTR_EXTERN) Then
			Print "extern " + vars.CorrectProcName(s)
		ElseIf vars.AttrInProc(s, ATTR_GLOBAL) Then
			Print "global " + vars.CorrectProcName(s)
		End If
	Next

	If 0 < vars.CountVars() Then
		For i = 0 To vars.CountVars() - 1
			s = vars.GetVarName(i)
			If vars.AttrInVar(s, ATTR_EXTERN) Then
				Print "extern " + vars.CorrectVarName(s)
			ElseIf vars.AttrInVar(s, ATTR_GLOBAL) Then
				Print "global " + vars.CorrectVarName(s)
			End If
		Next

		If use_thread_local Then
			print "section .tbss"
		Else
			print "section .bss"
		End If
		For i = 0 To vars.CountVars() - 1
			s = vars.GetVarName(i)
			If vars.AttrInVar(s, "e") Then  Continue For

			If vars.IsArray(s) Then
				print vars.CorrectVarName(s) + ": res" + dataDefName() + " " + Str(vars.GetArrayLength(s))
			End If
		Next

		If use_thread_local Then
			print "section .tdata"
		Else
			print "section .data"
		End If
		For i = 0 To vars.CountVars() - 1
			s = vars.GetVarName(i)
			If vars.AttrInVar(s, "e") Then  Continue For

			If Not vars.IsArray(s) Then
				print s + ": d" + dataDefName() + " 0"
			End If
		Next
	End If


	' For i = LBound(prog) To UBound(prog)
	' 	Delete prog(i)
	' Next
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

