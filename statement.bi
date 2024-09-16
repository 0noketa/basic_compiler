#ifndef statement__sbp
#define statement__sbp

#include "comparable.bi"
#include "array.bi"
#include "expr.bi"

Const STMT_EMPTY = "EMPTY"
Const STMT_STATEMENTS = "STATEMENTS"
Const STMT_LABEL_OR_CALL = "MAYBE.LABEL_CALL"
Const STMT_LABEL = "LABEL"
Const STMT_LET = "LET"
Const STMT_GOTO = "GOTO"
Const STMT_GOSUB = "GOSUB"
Const STMT_RETURN = "RETURN"
Const STMT_IF = "IF"
Const STMT_ELSE = "ELSE"
Const STMT_WHILE = "WHILE"
Const STMT_DECL_VAR = "DECL_VAR"
Const STMT_DECL_PROC = "DECL_PROC"
Const STMT_BEGIN_IF = "BEGIN.IF"
Const STMT_BEGIN_WHILE = "BEGIN.WHILE"
Const STMT_BEGIN_FOR = "BEGIN.FOR"
Const STMT_BEGIN_DO = "BEGIN.DO"
Const STMT_BEGIN_DO_WHILE = "BEGIN.DO_WHILE"
Const STMT_BEGIN_DO_UNTIL = "BEGIN.DO_UNTIL"
Const STMT_BEGIN_SUB = "BEGIN.SUB"
Const STMT_BEGIN_FUNCTION = "BEGIN.FUNCTION"
Const STMT_EXIT = "EXIT"
Const STMT_EXIT_IF = "EXIT.IF"
Const STMT_EXIT_FOR = "EXIT.FOR"
Const STMT_EXIT_WHILE = "EXIT.WHILE"
Const STMT_EXIT_DO = "EXIT.DO"
Const STMT_EXIT_SUB = "EXIT.SUB"
Const STMT_EXIT_FUNCTION = "EXIT.FUNCTION"
Const STMT_END = "END"
Const STMT_END_IF = "END.IF"
Const STMT_END_FOR = "END.FOR"
Const STMT_END_WHILE = "END.WHILE"
Const STMT_END_DO = "END.DO"
Const STMT_END_DO_WHILE = "END.DO_WHILE"
Const STMT_END_DO_UNTIL = "END.DO_UNTIL"
Const STMT_END_SUB = "END.SUB"
Const STMT_END_FUNCTION = "END.FUNCTION"


Type Statement Extends IComparable
	Protected:
		_opr As String
		_line_number As Long
		_label As String
		_strs As BoxedStrArray Ptr
		_exprs_len As Long
		_exprs As Expr Ptr Ptr
		_statements_len As Long
		_statements As Statement Ptr Ptr
	Public:
		Declare Constructor()
		Declare Destructor()
		Declare Sub SetOpr(s As String)
		Declare Function GetOpr() As String
		Declare Sub SetLineNumber(s As Long)
		Declare Function GetLineNumber() As Long
		Declare Sub SetLabel(s As String)
		Declare Function GetLabel() As String
		Declare Sub AddStr(s As String)
		Declare Function GetStr(i As Long) As String
		Declare Function CountStrs() As Long
		Declare Sub AddMovedExpr(e As Expr Ptr)
		Declare Function GetExpr(i As Long) As Expr Ptr
		Declare Function CountExprs() As Long
		Declare Sub AddMovedStatement(e As Statement Ptr)
		Declare Function GetStatement(i As Long) As Statement Ptr
		Declare Function CountStatements() As Long
		Declare Function CompareTo(p As IComparable Ptr) As Long

		Declare Sub Simplify()
		Declare Function ToString() As String
End Type


Constructor Statement()
	_opr = ""
	_line_number = -1
	_label = ""
	_strs = NewBoxedStrArray()
	_exprs_len = 0
	_exprs = NULL
	_statements_len = 0
	_statements = NULL
End Constructor

Destructor Statement()
	Dim i As Long

	If _strs <> NULL Then  Delete _strs

	If _exprs <> NULL Then
		For i = 0 To _exprs_len - 1
			Delete *(_exprs + i)
		Next

		free(_exprs)
	End If

	If _statements <> NULL Then
		For i = 0 To _statements_len - 1
			Delete *(_statements + i)
		Next

		free(_statements)
	End If
End Destructor

Sub Statement.SetOpr(s As String)
	_opr = s
End Sub
Function Statement.GetOpr() As String
	Return _opr
End Function
Sub Statement.SetLineNumber(s As Long)
	_line_number = s
End Sub
Function Statement.GetLineNumber() As Long
	Return _line_number
End Function
Sub Statement.SetLabel(s As String)
	_label = s
End Sub
Function Statement.GetLabel() As String
	Return _label
End Function

Sub Statement.AddStr(s As String)
	_strs->AddStr(s)
End Sub
Function Statement.GetStr(i As Long) As String
	Return _strs->GetStr(i)
End Function
Function Statement.CountStrs() As Long
	Return _strs->Count()
End Function

Sub Statement.AddMovedExpr(e As Expr Ptr)
	If _exprs = NULL Then
		_exprs = malloc(SizeOf(Expr Ptr))
	Else
		_exprs = realloc(_exprs, SizeOf(Expr Ptr) * (_exprs_len + 1))
	End If

	If _exprs <> NULL Then
		*(_exprs + _exprs_len) = e
		_exprs_len = _exprs_len + 1
	End If
End Sub
Function Statement.GetExpr(i As Long) As Expr Ptr
	If _exprs = NULL Or i >= _exprs_len Then
		Return NULL
	Else
		Return *(_exprs + i)
	End If
End Function
Function Statement.CountExprs() As Long
	Return _exprs_len
End Function

Sub Statement.AddMovedStatement(e As Statement Ptr)
	If _statements = NULL Then
		_statements = malloc(SizeOf(Statement Ptr))
	Else
		_statements = realloc(_statements, SizeOf(Statement Ptr) * (_statements_len + 1))
	End If

	If _statements <> NULL Then
		*(_statements + _statements_len) = e
		_statements_len = _statements_len + 1
	End If
End Sub
Function Statement.GetStatement(i As Long) As Statement Ptr
	If _statements = NULL Or i >= _statements_len Then
		Return NULL
	Else
		Return *(_statements + i)
	End If
End Function
Function Statement.CountStatements() As Long
	Return _statements_len
End Function

Function Statement.CompareTo(p As IComparable Ptr) As Long
	Dim o As Statement Ptr = Cast(Statement Ptr, p)

	If GetLineNumber() < o->GetLineNumber() Then
		Return 1
	ElseIf GetLineNumber() > o->GetLineNumber() Then
		Return -1
	Else
		Return 0
	End If
End Function

Function Statement.ToString() As String
	Dim s As String

	If GetLineNumber() > -1 Then
		s = Str$(GetLineNumber) + " "
	Else
		s = "  "
	End If

	Select Case GetOpr()
	Case STMT_LET
		s += "Let " + GetExpr(0)->ToString()
		Return s
	Case STMT_IF
		s += "If " + GetExpr(0)->ToString() + " Then "
		s += GetStatement(0)->ToString()
		If CountStatements() > 1 Then  s += " Else " + GetStatement(1)->ToString()
		Return s
	Case STMT_WHILE
		s += "While " + GetExpr(0)->ToString() + " Do "
		s += GetStatement(0)->ToString()
		Return s
	Case STMT_BEGIN_IF
		s += "If " + GetExpr(0)->ToString() + " Then"
		Return s
	Case STMT_BEGIN_WHILE
		s += "While " + GetExpr(0)->ToString()
		Return s
	Case STMT_GOTO
		s += "Goto " + GetStr(0)
		Return s
	Case STMT_GOSUB
		s += "Gosub " + GetStr(0)
		Return s
	Case STMT_RETURN
		s += "Return"
		Return s
	Case STMT_STATEMENTS
		s += GetStatement(0)->ToString()
		If CountStatements() > 1 Then  s += " : " + GetStatement(1)->ToString()
		Return s
	Case STMT_LABEL
		s += GetLabel() + ":"
		Return s
	Case STMT_EMPTY
		' s += "' empty line"
		Return s
	Case Else
		Return "[stmt:" + GetOpr() + "]"
	End Select
End Function


Function NewStatement(_opr As String) As Statement Ptr
	Dim o As Statement Ptr = New Statement()
	o->SetOpr(_opr)
	Return o
End Function




Type StatementArray Extends BoxedArray
	Public:
		Declare Destructor()
		Declare Sub AddMovedStatement(p As Statement Ptr)
		Declare Function IndexOfStatement(p As Statement Ptr) As Long
		Declare Function IndexOfStatementByType(s As String) As Long
		Declare Function LastIndexOfStatementByType(s As String) As Long
		Declare Function tryGetRangeOfBlock(_start As Integer, ByRef out_start As Long, ByRef out_end As Long) As Long
		Declare Sub SetMovedStatement(idx As Long, o As Statement Ptr)
		Declare Function RemoveStatement(idx As Long) As Statement Ptr
		Declare Sub DeleteStatement(idx As Long)
		Declare Function GetStatement(idx As Long) As Statement Ptr
		Declare Function GetStr(idx As Long) As String
End Type

Destructor StatementArray()
	Dim i As Long
	For i = 0 To _length - 1
		If GetStatement(i) <> NULL Then  DeleteStatement(i)
	Next
End Destructor

Sub StatementArray.AddMovedStatement(p As Statement Ptr)
	AddItem(p)
End Sub

Function StatementArray.IndexOfStatement(p As Statement Ptr) As Long
	Return IndexOfItem(p)
End Function
Function StatementArray.IndexOfStatementByType(s As String) As Long
	Dim i As Long
	
	For i = 0 To Count() - 1
		If GetStatement(i)->GetOpr() = s Then  Return i
	Next

	Return -1
End Function
Function StatementArray.LastIndexOfStatementByType(s As String) As Long
	Dim i As Long
	
	For i = Count() - 1 To 0
		If GetStatement(i)->GetOpr() = s Then  Return i
	Next

	Return -1
End Function

Function StatementArray.tryGetRangeOfBlock(_start As Integer, ByRef out_start As Long, ByRef out_end As Long) As Long
	Dim stmt As Statement Ptr
	Dim s As String
	Dim block_end As String
	Dim i As Long
	Dim j As Long
	Dim k As Long

	out_start = -1
	out_end = -1
	If _start >= Count() Then  Return FALSE

	stmt = GetStatement(_start)
	If Mid$(stmt->GetOpr(), 1, 6) <> "BEGIN." Then
		Return FALSE
	End If

	s = Mid$(stmt->GetOpr(), 7)
	If s = STMT_BEGIN_IF Then
		block_end = STMT_ELSE
	Else
		block_end = "END." + s
	End If

	out_start = i

	block_end = ""
	i = _start + 1
	While i < Count()
		stmt = GetStatement(i)

		If Mid$(stmt->GetOpr(), 1, 6) = "BEGIN." Then
			If tryGetRangeOfBlock(i + 1,  j, k) Then
				i = k - 1
			End If
		ElseIf stmt->GetOpr() = block_end Then
			out_end = i
			Return TRUE
		End If

		i += 1
	Wend

	Return FALSE
End Function

Sub StatementArray.SetMovedStatement(idx As Long, o As Statement Ptr)
	Dim o0 As Statement Ptr = GetStatement(idx)
	Delete o0
	SetItem(idx, o)
End Sub
Function StatementArray.RemoveStatement(idx As Long) As Statement Ptr
	Dim o0 As Statement Ptr = GetStatement(idx)
	SetItem(idx, NULL)
	Return o0
End Function
Sub StatementArray.DeleteStatement(idx As Long)
	Dim o As Statement Ptr = RemoveStatement(idx)
	Delete o
End Sub

Function StatementArray.GetStatement(idx As Long) As Statement Ptr
	Return Cast(Statement Ptr, GetItem(idx))
End Function
Function StatementArray.GetStr(idx As Long) As String
	Dim p As Statement Ptr : p = Cast(Statement Ptr, GetItem(idx))
	If p <> NULL Then
		Return p->GetOpr()
	Else
		Return ""
	End If
End Function

Function NewStatementArray() As StatementArray Ptr
	Dim p As StatementArray Ptr : p = New StatementArray
	Return p
End Function




#endif
