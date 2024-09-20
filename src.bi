#ifndef src__sbp
#define src__sbp


#include once "crt.bi"
#include once "ab.bi"
#include once "lex.bi"
#include once "expr.bi"
#include once "statement.bi"
#include once "environment.bi"




Type BasicSrc
	Protected:
		_src As SrcFile
		lines_nums As Long Ptr
		lines As Expr Ptr
		lines_count As Long
	Public:
		Declare Constructor()
		Declare Destructor()
		Declare Sub Init( _name As String )
		Declare Function tryLoadArgs(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
		Declare Function tryLoadExprVal(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
		Declare Function tryLoadExprMul(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
		Declare Function tryLoadExprAdd(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
		Declare Function tryLoadExprCmp(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
		Declare Function tryLoadExprAssignment(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
		Declare Function tryLoadExpr(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long

		Declare Function trySkipKeyword(tkns As BoxedStrArray Ptr, ByRef _start As Long, _end As Long, _keyword As String) As Long
		Declare Function trySkipUntilLastKeyword(tkns As BoxedStrArray Ptr, ByRef _start As Long, _end As Long, _keyword As String, ByRef out_idx As Long) As Long
		Declare Function trySkipAndLoadAs(tkns As BoxedStrArray Ptr, ByRef _start As Long, _end As Long, ByRef out_type As String) As Long

		Declare Function tryLoadProcDeclStatement(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_statement As Statement Ptr) As Long
		Declare Function tryLoadVarDeclStatement(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_statement As Statement Ptr) As Long

		Declare Function tryLoadCondStatement(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_statement As Statement Ptr) As Long
		Declare Function tryLoadLabelName(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_name As String) As Long
		Declare Function tryLoadLetStatement(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_statement As Statement Ptr) As Long

		' treats all single name statements as MAYBE_LABEL
		' includes:
		'   DO,LOOP,ELSE(block),BREAK,CONTINUE,WEND,NEXT,END,RETURN,
		'   PRIVATE,PROTECTED,PUBLIC
		'   and nullary call without "()"
		Declare Function tryLoadLabelledStatement(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_statement As Statement Ptr) As Long
		Declare Function tryLoadStatements(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_statement As Statement Ptr) As Long
		Declare Function tryLoadNumberedStatements(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_statement As Statement Ptr) As Long
		Declare Function loadLine() As Statement Ptr

		Declare Function tryCompileDecl() As Long

		Declare Sub compile()

		Declare Function InferExprType(e As Expr Ptr, env As Environment Ptr, update As Long) As String

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

' legacy stub
Function BasicSrc.tryLoadArgs(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
	Dim e As EXpr Ptr
	Dim e2 As EXpr Ptr
	Dim s As String
	Dim i As Long
	Dim j As Long

	out_next = _start
	out_expr = NULL
	If _start + 1 >= _end Then  Return FALSE
	If tkns->GetStr(_start + 1) = ")" Then  Return FALSE

	e = NewExpr("args")

	i = 0
	Do
		i += 1
		If tryLoadExpr(tkns, i, _end, j, e2) = FALSE Then
			Delete e
			Return FALSE
		End If

		e->AddArg(e2)
		i = j
		If i >= _end Then
			Delete e
			Return FALSE
		End If
	Loop While tkns->GetStr(i) = ","

	s = tkns->GetStr(i)
	If s <> ")" Then
		Delete e
		Return FALSE
	Else
		out_next = i + 1
		out_expr = e
	End If
End Function

' todo: split into loadVal and loadApply to accept (*(funcs + 1))()"
Function BasicSrc.tryLoadExprVal(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
	Dim e As Expr Ptr
	Dim e2 As Expr Ptr
	Dim s As String
	Dim s2 As String
	Dim i As Long
	Dim j As Long

	out_next = _start
	out_expr = NULL

	i = _start
	e = NULL

	If _start >= _end Then  Return FALSE

	s = tkns->GetStr(_start)
	If s = "(" Then
		If tryLoadExpr(tkns, _start + 1, _end,    i, e) Then
			If i >= _end _
				OrElse tkns->GetStr(i) <> ")" _
			Then
				Delete e
				Return FALSE
			End If

			out_next = i + 1
			out_expr = e
			Return TRUE
		Else
			Return FALSE
		End If
	ElseIf IsNum(Asc(Mid$(s, 1,1))) Then
		e = NewExpr(EXPR_VAL)
		e->SetVal(s)
		out_next = _start + 1
		out_expr = e
		Return TRUE
	ElseIf IsNamHead(Asc(Mid$(s, 1,1))) Then
		e = NewExpr(EXPR_VAL)
		e->SetVal(s)

		If _start + 1 >= _end _
			OrElse tkns->GetStr(_start + 1) <> "(" _
		Then
			out_next = _start + 1
			out_expr = e
			Print "; var in expr: " + s
			Return TRUE
		End If

		e->SetOpr(EXPR_APPLY)
		Print "; maybe apply in expr: " + s

		If _start + 2 >= _end Then
			Print ";    lack of bracket"
			Delete e
			Return FALSE
		ElseIf tkns->GetStr(_start + 2) = ")" Then
			Print ";    done as emply args"
			out_next = _start + 3
			out_expr = e2
			Return TRUE
		End If

		If tryLoadExpr(tkns, _start + 2, _end,    i, e2) = FALSE Then
			Print ";    failed to parse args"
			Delete e
			Return FALSE
		End If

		e->AddArg(e2)

		If i >= _end _
			OrElse tkns->GetStr(i) <> ")" _
		Then
			Print ";    multiple arg, or unknown element in args"
			Delete e
			Return FALSE
		End If

		out_next = i + 1
		out_expr = e
			Print ";    done as apply in expr: " + s
		Return TRUE
	End If

	Return FALSE
End Function

Function BasicSrc.tryLoadExprMul(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
	Dim r As Expr Ptr
	Dim e As Expr Ptr
	Dim e2 As Expr Ptr
	Dim s As String
	Dim i As Long
	Dim j As Long

	out_next = _start
	out_expr = NULL

	If tryLoadExprVal(tkns, _start, _end,    i, r) = FALSE Then
		Return FALSE
	End If

	Do
		If i >= _end Then
			out_next = i
			out_expr = r
			Return TRUE
		End If

		s = tkns->GetStr(i)
		If (s = "*") Or (s = "/") Or (UCase(s) = "MOD") Then
			If tryLoadExprVal(tkns, i + 1, _end,    j, e2) Then
				e = r
				r = NewExpr(UCase(s))
				r->AddArg(e)
				r->AddArg(e2)

				i = j
			Else
				' left operand is unknown value
				Delete r
				Return FALSE
			End If
		Else
			out_next = i
			out_expr = r
			Return TRUE
		End If
	Loop
End Function
Function BasicSrc.tryLoadExprAdd(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
	Dim r As Expr Ptr
	Dim e As Expr Ptr
	Dim e2 As Expr Ptr
	Dim s As String
	Dim i As Long
	Dim j As Long

	out_next = _start
	out_expr = NULL

	If tryLoadExprMul(tkns, _start, _end,    i, r) = FALSE Then
		Return FALSE
	End If

	Do
		If i >= _end Then
			out_next = i
			out_expr = r
			Return TRUE
		End If

		s = tkns->GetStr(i)
		If (s = "+") Or (s = "-") Then
			If tryLoadExprMul(tkns, i + 1, _end,    j, e2) Then
				e = r
				r = NewExpr(s)
				r->AddArg(e)
				r->AddArg(e2)

				i = j
			Else
				' left operand is unknown value
				Delete r
				Return FALSE
			End If
		Else
			out_next = i
			out_expr = r
			Return TRUE
		End If
	Loop
End Function
Function BasicSrc.tryLoadExprCmp(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
	Dim r As Expr Ptr
	Dim e As Expr Ptr
	Dim e2 As Expr Ptr
	Dim s As String
	Dim i As Long
	Dim j As Long

	out_next = _start
	out_expr = NULL

	If tryLoadExprAdd(tkns, _start, _end,    i, r) = FALSE Then
		Return FALSE
	End If

	Do
		If i >= _end Then
			out_next = i
			out_expr = r
			Return TRUE
		End If

		s = tkns->GetStr(i)
		If (s = "<") Or (s = ">") Or (s = "=") Then
			If tryLoadExprAdd(tkns, i + 1, _end,    j, e2) Then
				e = r
				r = NewExpr(s)
				r->AddArg(e)
				r->AddArg(e2)

				i = j
			Else
				' left operand is unknown value
				Delete r
				Return FALSE
			End If
		Else
			out_next = i
			out_expr = r
			Return TRUE
		End If
	Loop
End Function

Function BasicSrc.tryLoadExprAssignment(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
	Dim r As Expr Ptr
	Dim e As Expr Ptr
	Dim e2 As Expr Ptr
	Dim s As String
	Dim i As Long
	Dim j As Long

	out_next = _start
	out_expr = NULL

	If tryLoadExprVal(tkns, _start, _end,    i, r) = FALSE Then
		Return FALSE
	End If

	If i >= _end Then
		Delete r
		Return FALSE
	End If

	s = tkns->GetStr(i)
	If (s = "=") Or (s = ":=") Then
		If tryLoadExpr(tkns, i + 1, _end,    j, e2) Then
			e = r
			r = NewExpr(EXPR_ASSIGN)
			r->AddArg(e)
			r->AddArg(e2)

			out_next = j
			out_expr = r
			Return TRUE
		Else
			Print "; assigmnent lval is unknown value"
			Delete r
			Return FALSE
		End If
	Else
		Return FALSE
	End If
End Function

Function BasicSrc.tryLoadExpr(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_expr As Expr Ptr) As Long
	Return tryLoadExprCmp(tkns, _start, _end, out_next, out_expr)
End Function

Function BasicSrc.InferExprType(e As Expr Ptr, env As Environment Ptr, update As Long) As String
	Dim left_type As String
	Dim right_type As String
	Dim result As String
	Dim _val As String

	_val = e->GetVal()
	result = e->GetType()
	If e->OprIs("val") Then
		If IsNum(Asc(Mid$(_val, 1,1))) Then
			result = TYPE_INTEGER
		ElseIf Mid$(_val, 1,1) = Chr(34)  Then
			result = TYPE_STRING
		ElseIf env->IsVar(_val) Then
			result = env->GetResultType(_val)
		End If
	ElseIf e->OprIs("+") Then
		If e->GetArgc() = 2 Then
			left_type = InferExprType(e->GetArgv(0), env, update)
			right_type = InferExprType(e->GetArgv(1), env, update)
			result = left_type
		End If
	ElseIf e->OprIs("-") Or  e->OprIs("*") Or e->OprIs("/") Or e->OprIs("MOD") Then
		If e->GetArgc() = 2 Then
			left_type = InferExprType(e->GetArgv(0), env, update)
			right_type = InferExprType(e->GetArgv(1), env, update)

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

Function BasicSrc.trySkipKeyword(tkns As BoxedStrArray Ptr, ByRef _start As Long, _end As Long, _keyword As String) As Long
	If _start >= _end Then  Return FALSE
	If UCase(tkns->GetStr(_start)) = UCase(_keyword) Then
		_start += 1
		Return TRUE
	Else
		Return FALSE
	End If
End Function

Function BasicSrc.trySkipUntilLastKeyword(tkns As BoxedStrArray Ptr, ByRef _start As Long, _end As Long, _keyword As String, ByRef out_idx As Long) As Long
	Dim i As Long
	For i = _end - 1 To _start Step -1
		If UCase(tkns->GetStr(i)) = UCase(_keyword) Then
			out_idx = i
			Return TRUE
		End If
	Next

	out_idx = _end
	Return FALSE
End Function

Function BasicSrc.trySkipAndLoadAs(tkns As BoxedStrArray Ptr, ByRef _start As Long, _end As Long, ByRef out_type As String) As Long
	out_type = TYPE_INTEGER

	If trySkipKeyword(tkns, _start, _end, "AS") Then
		If _start < _end Then
			out_type = UCase(tkns->GetStr(_start))
			Return TRUE
		End If
	End If

	Return FALSE
End Function

Function BasicSrc.tryLoadProcDeclStatement(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_statement As Statement Ptr) As Long
	Dim isDecl As Long
	Dim procType As String
	Dim paramName As String
	Dim paramType As String
	Dim resultType As String
	Dim attr As String
	Dim s As String
	Dim _name As String

	If _start + 1 >= _end Then  Return FALSE

	attr = ""

	If trySkipKeyword(tkns, _start, _end, "EXTERN") Then
		Print "  ; extern"
		attr += "e"
	ElseIf trySkipKeyword(tkns, _start, _end, "GLOBAL") Then
		Print "  ; global"
		attr += "g"
	End If

	If trySkipKeyword(tkns, _start, _end, "DECLARE") Then
		Print "  ; decl"
		isDecl = TRUE
	Else
		isDecl = FALSE
	End If

	If trySkipKeyword(tkns, _start, _end, "EXTERN") Then
		Print "  ; extern2"
		If attr <> "" Then  Print "; duplicated access class declaration"
		attr += "e"
	ElseIf trySkipKeyword(tkns, _start, _end, "GLOBAL") Then
		Print "  ; global2"
		If attr <> "" Then  Print "; duplicated access class declaration"
		attr += "g"
	End If

	If trySkipKeyword(tkns, _start, _end, "SUB") Then
		procType = "SUB"
		paramType = TYPE_VOID
		resultType = TYPE_VOID
	ElseIf trySkipKeyword(tkns, _start, _end, "FUNCTION") Then
		procType = "FUNCTION"
		paramType = TYPE_VOID
		resultType = TYPE_INTEGER
	Else
		If isDecl <> FALSE Then  Print "; declaration of unknown element " + s
		Return FALSE
	End If

	_name = tkns->GetStr(_start)
	_start += 1
	Print "  ; name:" + _name

	If trySkipKeyword(tkns, _start, _end, "CDECL") Then
		Print "  ; cdecl"
	End If

	If trySkipKeyword(tkns, _start, _end, "(") Then
		If trySkipKeyword(tkns, _start, _end, ")") = FALSE Then
			paramType = TYPE_INTEGER

			If trySkipKeyword(tkns, _start, _end, "BYVAL") Then
			ElseIf trySkipKeyword(tkns, _start, _end, "BYREF") Then
				Print "; decl of procedure with ByRef params are not implemented"
				Return FALSE
			End If

			paramName = tkns->GetStr(_start)
			_start += 1

			If trySkipAndLoadAs(tkns, _start, _end,  s) Then
				If s = TYPE_LONG Then  s = TYPE_INTEGER

				If s <> TYPE_INTEGER Then
					Print "; decl of procedure with non integer/log params are not implemented"
					Return FALSE
				End If

				paramType = s
			End If

			If trySkipKeyword(tkns, _start, _end, ",") = FALSE Then
				Print "; decl of procedure with >=2 params are not implemented"
				Return FALSE
			ElseIf trySkipKeyword(tkns, _start, _end, ")") = FALSE Then
				Print "; decl with unknown element " + s
				Return FALSE
			End If
		End If

		If trySkipAndLoadAs(tkns, _start, _end, s) Then
			If s = TYPE_LONG Then  s = TYPE_INTEGER

			If s <> TYPE_INTEGER Then
				Print "; decl of procedure with non integer/log params are not implemented"
				Return FALSE
			End If

			resultType = s

			If procType = "SUB" Then
				Print "; decl of SUB procedure with result type description will be treated as FUNCTION"
			End If
		End If
	End If

	If isDecl <> FALSE Then
		out_statement = NewStatement(STMT_DECL_PROC)
	ElseIf procType = "SUB" Then
		out_statement = NewStatement(STMT_BEGIN_SUB)
	Else
		out_statement = NewStatement(STMT_BEGIN_FUNCTION)
	End If

	out_statement->AddStr(attr)

	out_statement->AddStr(_name)
	out_statement->AddStr(resultType)

	If paramType <> TYPE_VOID Then
		out_statement->AddStr(paramName)
		out_statement->AddStr(paramType)
	End If

	Return TRUE
End Function

Function BasicSrc.tryLoadVarDeclStatement(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_statement As Statement Ptr) As Long
	Dim s As String
	Dim _name As String
	Dim _type As String
	Dim is_array As Long
	Dim with_dim As Long
	Dim range_min As Long
	Dim range_max As Long
	Dim attr As String

	out_statement = NULL
	attr = ""

	If _start + 1 >= _end Then  Return FALSE

	If trySkipKeyword(tkns, _start, _end, "GLOBAL") Then
		attr += ATTR_GLOBAL
		trySkipKeyword(tkns, _start, _end, "DIM")
	ElseIf trySkipKeyword(tkns, _start, _end, "EXTERN") Then
		attr += ATTR_EXTERN
		trySkipKeyword(tkns, _start, _end, "DIM")
	ElseIf trySkipKeyword(tkns, _start, _end, "LOCAL") Then
		trySkipKeyword(tkns, _start, _end, "DIM")
	ElseIf trySkipKeyword(tkns, _start, _end, "DIM") Then
		If trySkipKeyword(tkns, _start, _end, "GLOBAL") Then
			attr += ATTR_GLOBAL
		ElseIf trySkipKeyword(tkns, _start, _end, "EXTERN") Then
			attr += ATTR_EXTERN
		ElseIf trySkipKeyword(tkns, _start, _end, "LOCAL") Then
		End If
	Else
		Return FALSE
	End If

	_name = tkns->GetStr(_start)

	If _start + 1 >= _end Then
		out_statement = NewStatement(STMT_DECL_VAR)
		out_statement->AddStr(attr)
		out_statement->AddStr(_name)
		out_statement->AddStr(TYPE_LONG)
		Return TRUE
	End If

	_start += 1
	is_array = FALSE
	If tkns->GetStr(_start) = "(" Then
		If _start + 2 >= _end Then
			Print "; error: array with no size is not implemented"
			Return FALSE
		End If

		is_array = TRUE
		range_min = 1
		range_max = Int(Val(tkns->GetStr(_start + 1)))

		If UCase(tkns->GetStr(_start + 2)) = "TO" Then
			If _start + 4 >= _end Then  Return FALSE

			range_min = range_max
			range_max = Int(Val(tkns->GetStr(_start + 3)))
			_start += 2
		End If

		If tkns->GetStr(_start + 2) <> ")" Then
			Return FALSE
		End If

		_start += 3

		If _start >= _end Then
			out_statement = NewStatement(STMT_DECL_VAR)
			out_statement->AddStr(attr)
			out_statement->AddStr(_name)
			out_statement->AddStr(TYPE_LONG)
			out_statement->AddStr(Str$(range_min))
			out_statement->AddStr(Str$(range_max))
			Return TRUE
		End If
	End If

	If _start + 1 >= _end Then  Return FALSE

	If trySkipKeyword(tkns, _start, _end, "AS") Then
		If _start >= _end Then  Return FALSE

		_type = UCase(tkns->GetStr(_start))

		If _start + 1 < _end Then  Return FALSE

		out_statement = NewStatement(STMT_DECL_VAR)
		out_statement->AddStr(attr)
		out_statement->AddStr(_name)
		out_statement->AddStr(_type)
		If is_array Then
			out_statement->AddStr(Str$(range_min))
			out_statement->AddStr(Str$(range_max))
		End If

		Return TRUE
	Else
		Return FALSE
	End If
End Function

Function BasicSrc.tryLoadLabelName(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_name As String) As Long
	Dim i As Long

	out_next = _start

	If _start >= _end Then  Return FALSE

	If UCase(tkns->GetStr(_start)) = "LABEL" Then  _start += 1
	If tkns->GetStr(_start) = "*" Then  _start += 1

	If _start >= _end Then  Return FALSE

	out_name = tkns->GetStr(_start)
	out_next = _start + 1
	Return TRUE
End Function

Function BasicSrc.tryLoadLetStatement(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_statement As Statement Ptr) As Long
	Dim s As String
	Dim i As Long
	Dim isLet As Long
	Dim e As Expr Ptr

	out_statement = NULL

	' "()"-less call have been taken as MAYBE_LABEL. others should have atleast 2 token.
	If _start + 1 >= _end Then  Return FALSE


	If UCase(s) = "LET" Then
		isLet = TRUE
		_start += 1
	Else
		isLet = FALSE
	End If

	If tryLoadExprAssignment(tkns, _start, _end,   out_next, e) Then
		out_statement = NewStatement(STMT_LET)
		out_statement->AddMovedExpr(e)
		Return TRUE
	Else
		If isLet Then  Print "; LET with unknown expr"
		Return FALSE
	End If
End Function


Function BasicSrc.tryLoadCondStatement(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_next As Long, ByRef out_statement As Statement Ptr) As Long
	Dim i As Long
	Dim s As String
	Dim _start2 As Long
	Dim then_start As Long
	Dim then_end As Long
	Dim els_start As Long
	Dim _expr As Expr Ptr
	Dim _statement As Statement Ptr

	If _start >= _end Then
		out_statement = NewStatement(STMT_EMPTY)
		Return TRUE
	End If


	out_statement = NULL
	s = tkns->GetStr(_start)

	' IF: "IF" expr (|"THEN") statements
	' IF.BLOCK: "IF" expr (|"THEN")
	If UCase(s) = "IF" Then
		If tryLoadExpr(tkns, _start + 1, _end,    then_start, _expr) Then
			trySkipKeyword(tkns, then_start, _end, "THEN")
			If then_start >= _end Then
				out_statement = NewStatement(STMT_BEGIN_IF)
				out_statement->AddMovedExpr(_expr)
				Return TRUE
			End If

			If trySkipUntilLastKeyword(tkns, then_start, _end, "ELSE",  then_end) Then
				els_start = then_end + 1
			Else
				then_end = _end
				els_start = _end
			End If
			Print ";IF", then_start, then_end, els_start, _end
			out_statement = NewStatement(STMT_IF)
			out_statement->AddMovedExpr(_expr)

			If then_start + 1 = then_end _
					AndAlso IsNum(Asc(Mid$(tkns->GetStr(then_start), 1, 1))) _
			Then
				Print "; goto-less goto @ IF ->" + tkns->GetStr(then_start)
				_statement = NewStatement(STMT_GOTO)
				_statement->AddStr(tkns->GetStr(then_start))
				out_statement->AddMovedStatement(_statement)
			ElseIf tryLoadStatements(tkns, then_start, then_end, _statement) Then
				out_statement->AddMovedStatement(_statement)
				_statement = NULL
			Else
				Delete out_statement
				out_statement = NULL
				Return FALSE
			End If

			If els_start = _end Then  Return TRUE

			If els_start + 1 = _end _
					AndAlso IsNum(Asc(Mid$(tkns->GetStr(els_start), 1, 1))) _
			Then
				_statement = NewStatement(STMT_GOTO)
				_statement->AddStr(tkns->GetStr(els_start))
				out_statement->AddMovedStatement(_statement)
			ElseIf tryLoadStatements(tkns, els_start, _end,  _statement) Then
				out_statement->AddMovedStatement(_statement)
			Else
				Delete out_statement
				out_statement = NULL
				Return FALSE
			End If

			Return TRUE
		Else
			Return FALSE
		End If
	End If
	
	' WHILE: "WHILE" expr (|"DO") statements
	' WHILE.BLOCK: "WHILE" expr (|"DO")
	If UCase(s) = "WHILE" Then
		If tryLoadExpr(tkns, _start + 1, _end,    _start2, _expr) Then
			trySkipKeyword(tkns, _start2, _end, "DO")
			If _start2 >= _end Then
				out_statement = NewStatement(STMT_BEGIN_WHILE)
				out_statement->AddMovedExpr(_expr)
				Return TRUE
			End If

			If tryLoadStatements(tkns, _start2, _end,    _statement) Then
				out_statement = NewStatement(STMT_BEGIN_WHILE)
				out_statement->AddMovedExpr(_expr)
				out_statement->AddMovedStatement(_statement)
				Return TRUE
			End If

			Return FALSE
		Else
			Return FALSE
		End If
	End If

	If UCase(s) = "GOTO" Then
		If tryLoadLabelName(tkns, _start + 1, _end,    i, s) Then
			out_statement = NewStatement(STMT_GOTO)
			out_statement->AddStr(s)
			Print "; goto" + s
			Return TRUE
		Else
			Print "; errornous goto"
		End If

		Return FALSE
	End If

	If (UCase(s) = "GOSUB") Or (UCase(s) = "CALL") Then
		If tryLoadLabelName(tkns, _start + 1, _end,    i, s) Then
			out_statement = NewStatement(STMT_GOSUB)
			out_statement->AddStr(s)
			Return TRUE
		End If

		Return FALSE
	End If

	If UCase(s) = "EXIT" Then
		If trySkipKeyword(tkns, _start + 1, _end, "SUB") Then
			out_statement = NewStatement(STMT_EXIT_SUB)
		ElseIf trySkipKeyword(tkns, _start + 1, _end, "FUNCTION") Then
			out_statement = NewStatement(STMT_EXIT_FUNCTION)
		ElseIf trySkipKeyword(tkns, _start + 1, _end, "WHILE") Then
			out_statement = NewStatement(STMT_EXIT_WHILE)
		ElseIf trySkipKeyword(tkns, _start + 1, _end, "FOR") Then
			out_statement = NewStatement(STMT_EXIT_FOR)
		ElseIf trySkipKeyword(tkns, _start + 1, _end, "DO") Then
			out_statement = NewStatement(STMT_EXIT_DO)
		Else
			out_statement = NewStatement(STMT_EXIT)

			If _start + 1 < _end Then
				If tryLoadExpr(tkns, _start + 1, _end,    _start2, _expr) Then
					out_statement->AddMovedExpr(_expr)
				End If
			End If
		End If
		Return TRUE
	End If

	If UCase(s) = "END" Then
		If trySkipKeyword(tkns, _start + 1, _end, "SUB") Then
			out_statement = NewStatement(STMT_END_SUB)
		ElseIf trySkipKeyword(tkns, _start + 1, _end, "FUNCTION") Then
			out_statement = NewStatement(STMT_END_FUNCTION)
		ElseIf trySkipKeyword(tkns, _start + 1, _end, "WHILE") Then
			out_statement = NewStatement(STMT_END_WHILE)
		ElseIf trySkipKeyword(tkns, _start + 1, _end, "FOR") Then
			out_statement = NewStatement(STMT_END_FOR)
		ElseIf trySkipKeyword(tkns, _start + 1, _end, "DO") Then
			out_statement = NewStatement(STMT_END_DO)
		ElseIf trySkipKeyword(tkns, _start + 1, _end, "IF") Then
			out_statement = NewStatement(STMT_END_IF)
		Else
			out_statement = NewStatement(STMT_END)

			If _start + 1 < _end Then
				If tryLoadExpr(tkns, _start + 1, _end,    _start2, _expr) Then
					out_statement->AddMovedExpr(_expr)
				End If
			End If
		End If
		Return TRUE
	End If

	If UCase(s) = "DO" Then
		_start += 1
		If trySkipKeyword(tkns, _start, _end, "WHILE") Then
			out_statement = NewStatement(STMT_BEGIN_DO_WHILE)

			If _start + 1 < _end Then
				If tryLoadExpr(tkns, _start + 1, _end,    _start2, _expr) Then
					out_statement->AddMovedExpr(_expr)
				End If
			Else
				Return FALSE
			End If
		ElseIf trySkipKeyword(tkns, _start, _end, "UNTIL") Then
			out_statement = NewStatement(STMT_BEGIN_DO_UNTIL)

			If _start + 1 < _end Then
				If tryLoadExpr(tkns, _start + 1, _end,    _start2, _expr) Then
					out_statement->AddMovedExpr(_expr)
				End If
			Else
				Return FALSE
			End If
		Else
			out_statement = NewStatement(STMT_BEGIN_DO)
		End If
		Return TRUE
	End If

	If UCase(s) = "LOOP" Then
		_start += 1
		If trySkipKeyword(tkns, _start, _end, "WHILE") Then
			out_statement = NewStatement(STMT_END_DO_WHILE)

			If _start + 1 < _end Then
				If tryLoadExpr(tkns, _start + 1, _end,    _start2, _expr) Then
					out_statement->AddMovedExpr(_expr)
				End If
			Else
				Return FALSE
			End If
		ElseIf trySkipKeyword(tkns, _start, _end, "UNTIL") Then
			out_statement = NewStatement(STMT_END_DO_UNTIL)

			If _start + 1 < _end Then
				If tryLoadExpr(tkns, _start + 1, _end,    _start2, _expr) Then
					out_statement->AddMovedExpr(_expr)
				End If
			Else
				Return FALSE
			End If
		Else
			out_statement = NewStatement(STMT_END_DO)
		End If
		Return TRUE
	End If

	If UCase(s) = "RETURN" Then
		out_statement = NewStatement(STMT_RETURN)

		If _start + 1 < _end Then
			If tryLoadExpr(tkns, _start + 1, _end,    _start2, _expr) Then
				Print "; RETURN with result is not implemented. result will be ignored."
				out_statement->AddMovedExpr(_expr)
			End If
		End If
		Return TRUE
	End If

	If UCase(s) = "LABEL" Then
		out_statement = NewStatement(STMT_LABEL)

		If _start + 1 < _end Then
			out_statement->SetLabel(tkns->GetStr(_start + 1))
		Else
			out_statement->SetLabel(s)
		End If
		Return TRUE
	End If

	If tryLoadLetStatement(tkns, _start, _end,    out_next, out_statement) Then
		Return TRUE
	End If

	' statement is only single name
	' and not a keyword that can make statement without any other element such as "RETURN"
	If IsNamHead((tkns->GetBoxedStr(_start))->ChrCodeAt(1)) Then
		If _start + 1 = _end _
			Or (_start + 1 < _end AndAlso tkns->GetStr(_start + 1) = ":") _
		Then
			out_statement = NewStatement(STMT_LABEL)
			out_statement->SetLabel(tkns->GetStr(_start))
			Return TRUE
		End If
	End If

	Return FALSE
End Function

' labelled (":"-separatable)
Function BasicSrc.tryLoadLabelledStatement(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_statement As Statement Ptr) As Long
	Dim label_name As String
	Dim _statement As Statement Ptr
	Dim _statement2 As Statement Ptr
	Dim _next As Long

	If _start >= _end Then
		out_statement = NewStatement(STMT_EMPTY)
		Return TRUE
	End If

	' *LABEL0
	' *LABEL1 LET I = 0
	If _start + 1 < _end _
			AndAlso tkns->GetStr(_start) = "*" _
			And IsNam((tkns->GetBoxedStr(_start + 1))->ChrCodeAt(1)) _
	Then
		label_name = tkns->GetStr(_start + 1)

		_statement = NewStatement(STMT_LABEL)
		_statement->SetLabel(label_name)

		If _start + 2 >= _end Then
			out_statement = _statement
			Return TRUE
		ElseIf tryLoadCondStatement(tkns, _start + 2, _end,    _next, _statement2) Then
			out_statement = NewStatement(STMT_STATEMENTS)
			_statement->AddMovedStatement(_statement)
			_statement->AddMovedStatement(_statement2)
			Return TRUE
		End If
	End If

	' (LABEL0)
	' (LABEL1) LET I = 0
	If _start + 3 < _end _
			AndAlso tkns->GetStr(_start) = "(" _
			And IsNam((tkns->GetBoxedStr(_start + 1))->ChrCodeAt(1)) _
			And tkns->GetStr(_start + 2) = ")" _
	Then
		label_name = tkns->GetStr(_start + 1)

		If _start + 3 = _end Then
			out_statement = NewStatement(STMT_LABEL)
			out_statement->SetLabel(label_name)
			Return TRUE
		ElseIf tryLoadCondStatement(tkns, _start + 3, _end,    _next, out_statement) Then
			out_statement->SetLabel(label_name)
			Return TRUE
		End If
	End If

	If tryLoadVarDeclStatement(tkns, _start, _end,  out_statement) Then
		Return TRUE
	End If

	If tryLoadProcDeclStatement(tkns, _start, _end,  out_statement) Then
		Return TRUE
	End If

	If tryLoadCondStatement(tkns, _start, _end,  _next, out_statement) Then
		Return TRUE
	End If

	Return FALSE
End Function

' ":"-separated
Function BasicSrc.tryLoadStatements(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_statement As Statement Ptr) As Long
	Dim _statement As Statement Ptr
	Dim dpt As Long
	Dim _start2 As Long
	Dim _end2 As Long
	Dim i As Long

	out_statement = NULL

	If trySkipKeyword(tkns, _start, _end, "REM") Then
		out_statement = NewStatement(STMT_EMPTY)
		Return TRUE
	End If

	dpt = 0
	_end2 = _end
	For i = _start To _end - 1
		If UCase(tkns->GetStr(i)) = "IF" Then  dpt += 1
		If UCase(tkns->GetStr(i)) = "ENDIF" Then  dpt -= 1
		If tkns->GetStr(i) = ":"  And dpt = 0 Then
			_end2 = i
			Exit For
		End If
	Next

	If tryLoadLabelledStatement(tkns, _start, _end2,   _statement) = FALSE Then
		Return FALSE
	End If

	If _end2 = _end Then
		out_statement = _statement
		Return TRUE
	End If

	If tkns->GetStr(_end2) <> ":" Then
		Delete _statement
		Return FALSE
	End If

	out_statement = NewStatement(STMT_STATEMENTS)
	out_statement->AddMovedStatement(_statement)

	If tryLoadStatements(tkns, _end2 + 1, _end,   _statement) Then
		out_statement->AddMovedStatement(_statement)
		Return TRUE
	End If

	Delete out_statement
	out_statement = NULL
	Return FALSE
End Function

' with _line number
Function BasicSrc.tryLoadNumberedStatements(tkns As BoxedStrArray Ptr, _start As Long, _end As Long, ByRef out_statement As Statement Ptr) As Long
	Dim line_number As Long = -1
	Dim _next As Long
	Dim s As String 

	out_statement = NULL
	If _start >= _end Then
		out_statement = NewStatement(STMT_EMPTY)
		Return TRUE
	End If

	s = tkns->GetStr(_start)
	If IsNum(Asc(Mid$(s, 1,1))) Then
		line_number = Int(Val(s))

		_start += 1
	End If

	If _start + 1 >= _end Then
		out_statement = NewStatement(STMT_EMPTY)
		out_statement->SetLineNumber(line_number)
		Return TRUE
	End If

	If tryLoadStatements(tkns, _start, _end,    out_statement) Then
		out_statement->SetLineNumber(line_number)
		Return TRUE
	End If

	Return FALSE
End Function

Sub RemoveEmptyStatementsFromTokens(src As BoxedStrArray Ptr)
	Dim i As Long
	Dim coloned As Long

	i = src->Count() - 1
	coloned = FALSE
	While i > 2
		If src->GetStr(i) = ":" Then
			If coloned Then  src->RemoveBoxedStr(i)

			coloned = TRUE
		Else
			coloned = FALSE
		End If

		i -= 1
	Wend
End Sub

Function BasicSrc.loadLine() As Statement Ptr
	Dim _line As BoxedStrArray Ptr
	Dim _line2 As BoxedStrArray Ptr
	Dim statements As Statement Ptr
	Dim e As Expr Ptr
	Dim i As Long
	Dim _len As Long
	Dim _lbound As Long
	Dim s As String
	Dim s2 As String
	Dim line_number As Long = -1
	Dim label_name As String = ""

	If Not IsOpened() Then
		Return NULL
	End If

	_line =  _src.GetTokenArrayFromLine()
	RemoveEmptyStatementsFromTokens(_line)
	print ";src: " + _line->Join(" ")

	While _line->Count() > 0 _
			AndAlso _line->GetStr(_line->Count() - 1) = "_"
		_line2 = _src.GetTokenArrayFromLine()
		print ";continues: " + _line2->Join(" ")
		_line->SetMovedBoxedStr(_line->Count() - 1, _line2->ExtractBoxedStr(0))

		For i = 1 To _line2->Count() - 1
			_line->AddMovedBoxedStr(_line2->ExtractBoxedStr(i))
		Next
		Delete _line2
	Wend
	print ";logical src line: " + _line->Join(" ")

	If _line->Count() = 0 Then
		Delete _line
		Return NewStatement(STMT_EMPTY)
	ElseIf tryLoadNumberedStatements(_line, 0, _line->Count(),    statements) Then
		Delete _line
		Return statements
	Else
		Delete _line
		Return NULL
	End If
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

