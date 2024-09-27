#ifndef scope__sbp
#define scope__sbp


#include once "crt.bi"
#include once "ab.bi"
#include once "comparable.bi"
#include once "array.bi"
#include once "lex.bi"
#include once "expr.bi"
#include once "statement.bi"


Const MAX_VARS_COUNT = 256


Const ATTR_EXTERN = "e"
Const ATTR_GLOBAL = "g"
Const ATTR_STATIC = "s"


Type VarInfo Extends IComparable
	Protected:
		_name As String
		_type As String
		_param_types As BoxedStrArray Ptr
		_param_names As BoxedStrArray Ptr
		_size As Long
		_lbound As Long
		_attr As String
	Public:
		Declare Constructor()
		Declare Destructor()
		Declare Function GetName() As String
		Declare Sub SetName(s As String)
		Declare Function GetType() As String
		Declare Sub SetType(s As String)
		Declare Function CountParams() As Long
		Declare Function GetParamName(idx As Long) As String
		Declare Function GetParamType(idx As Long) As String
		Declare Function GetParamTypeByName(_name As String) As String
		Declare Sub AddParam(_name As String, _type As String)
		Declare Function GetSize() As Long
		Declare Sub SetSize(s As Long)
		Declare Function GetLBound() As Long
		Declare Sub SetLBound(s As Long)
		Declare Function GetAttr() As String
		Declare Sub SetAttr(s As String)
        Declare Function CompareTo(p As IComparable Ptr) As Long
End Type

Constructor VarInfo()
	_param_names = NewBoxedStrArray()
	_param_types = NewBoxedStrArray()
End Constructor
Destructor VarInfo()
	Delete _param_names
	Delete _param_types
End Destructor

Function VarInfo.GetName() As String
	Return _name
End Function
Sub VarInfo.SetName(s As String)
	_name = s
End Sub
Function VarInfo.GetType() As String
	Return _type
End Function
Sub VarInfo.SetType(s As String)
	_type = s
End Sub
Function VarInfo.CountParams() As Long
	Return _param_names->Count()
End Function
Function VarInfo.GetParamName(idx As Long) As String
	If idx < 0 Or idx >= _param_names->Count() Then  Return "<error_NAME>"
	Return _param_names->GetStr(idx)
End Function
Function VarInfo.GetParamType(idx As Long) As String
	If idx < 0 Or idx >= _param_types->Count() Then  Return TYPE_VOID
	Return _param_types->GetStr(idx)
End Function
Function VarInfo.GetParamTypeByName(_name As String) As String
	Dim idx As Long = _param_names->IndexOfStr(_name)
	If idx = -1 Then  Return TYPE_VOID
	Return GetParamType(idx)
End Function
Sub VarInfo.AddParam(_name As String, _type As String)
	_param_names->AddStr(_name)
	_param_types->AddStr(_type)
End Sub
Function VarInfo.GetSize() As Long
	Return _size
End Function
Sub VarInfo.SetSize(s As Long)
	_size = s
End Sub
Function VarInfo.GetLBound() As Long
	Return _lbound
End Function
Sub VarInfo.SetLBound(s As Long)
	_lbound = s
End Sub
Function VarInfo.GetAttr() As String
	Return _attr
End Function
Sub VarInfo.SetAttr(s As String)
	_attr = s
End Sub

Function VarInfo.CompareTo(p As IComparable Ptr) As Long
	Dim o As VarInfo Ptr = Cast(VarInfo Ptr, p)

	If GetSize() < o->GetSize() Then
		Return 1
	ElseIf GetSize() > o->GetSize() Then
		Return -1
	Else
		Return 0
	End If
End Function

Function NewVarInfo() As VarInfo Ptr
	Dim p As VarInfo Ptr = New VarInfo
	Return p 
End Function

Type VarInfoArray Extends BoxedArray
    Protected:
        _keys As BoxedStrArray Ptr
	Public:
		Declare Constructor()
		Declare Destructor()
		Declare Sub AddMovedVarInfo(p As VarInfo Ptr)
		Declare Function IndexOfVarInfo(p As VarInfo Ptr) As Long
		Declare Function IndexOfVarInfoByName(s As String, case_sensitive As Long) As Long
		Declare Function LastIndexOfVarInfoByName(s As String, case_sensitive As Long) As Long
		Declare Function GetVarInfo(idx As Long) As VarInfo Ptr
		Declare Sub SetMovedVarInfo(idx As Long, o As VarInfo Ptr)
		Declare Function ExtractVarInfo(idx As Long) As VarInfo Ptr
		Declare Function ExtractRemovedVarInfo(idx As Long) As VarInfo Ptr
		Declare Sub RemoveVarInfo(idx As Long)
		Declare Sub DeleteVarInfo(idx As Long)
		Declare Function GetVarName(idx As Long) As String
End Type

Constructor VarInfoArray()
    _keys = NewBoxedStrArray()
End Constructor
Destructor VarInfoArray()
	Dim i As Long
	For i = 0 To _length - 1
		If GetVarInfo(i) <> NULL Then  Delete GetVarInfo(i)
	Next
    Delete _keys
End Destructor

Sub VarInfoArray.AddMovedVarInfo(p As VarInfo Ptr)
	AddItem(p)
    _keys->AddStr(p->GetName())
End Sub

Function VarInfoArray.IndexOfVarInfo(p As VarInfo Ptr) As Long
	Return IndexOfItem(p)
End Function
Function VarInfoArray.IndexOfVarInfoByName(_name As String, case_sensitive As Long) As Long
    Dim i As Long
    Dim _name2 As String
    If case_sensitive Then
        _name2 = UCase(_name)
        i = 0
        For i = 0 To Count() - 1
            If UCase(_keys->GetStr(i)) = UCase(_name2) Then
                Return i
            End IF

            i += 1
        Next
    Else
    	Return _keys->IndexOfStr(_name)
    End If
End Function
Function VarInfoArray.LastIndexOfVarInfoByName(_name As String, case_sensitive As Long) As Long
    Dim i As Long
    Dim _name2 As String
    If case_sensitive Then
        _name2 = UCase(_name)
        i = 0
        For i = Count() - 1 To 0 Step -1
            If UCase(_keys->GetStr(i)) = UCase(_name2) Then
                Return i
            End IF
        Next

        Return -1
    Else
    	Return _keys->LastIndexOfStr(_name)
    End If
End Function

Sub VarInfoArray.SetMovedVarInfo(idx As Long, o As VarInfo Ptr)
	Dim o0 As VarInfo Ptr = GetVarInfo(idx)
	Delete o0
    _keys->SetMovedBoxedStr(idx, NewBoxedString(o->GetName()))
	SetItem(idx, o)
End Sub
Function VarInfoArray.ExtractVarInfo(idx As Long) As VarInfo Ptr
    Dim s As BoxedString Ptr = _keys->ExtractBoxedStr(idx)
	Dim o As VarInfo Ptr = GetVarInfo(idx)
    Delete s
	SetItem(idx, NULL)
	Return o
End Function
Function VarInfoArray.ExtractRemovedVarInfo(idx As Long) As VarInfo Ptr
	Dim o As VarInfo Ptr = GetVarInfo(idx)
    _keys->RemoveBoxedStr(idx)
	RemoveItem(idx)
	Return o
End Function
Sub VarInfoArray.RemoveVarInfo(idx As Long)
	Dim o As VarInfo Ptr = GetVarInfo(idx)
    _keys->RemoveBoxedStr(idx)
	RemoveItem(idx)
	Delete o
End Sub
Sub VarInfoArray.DeleteVarInfo(idx As Long)
	Dim o As VarInfo Ptr = ExtractVarInfo(idx)
	Delete o
End Sub

Function VarInfoArray.GetVarInfo(idx As Long) As VarInfo Ptr
	Return Cast(VarInfo Ptr, GetItem(idx))
End Function
Function VarInfoArray.GetVarName(idx As Long) As String
    Return _keys->GetStr(idx)
End Function

Function NewVarInfoArray() As VarInfoArray Ptr
	Dim p As VarInfoArray Ptr = New VarInfoArray
	Return p
End Function




Type Environment
	Protected:
		_vars As VarInfoArray Ptr
		_procs As VarInfoArray Ptr
        _statements As StatementArray Ptr
	Public:
		Declare Constructor()
		Declare Destructor()
		Declare Function CountVars() As Long
		Declare Function CountProcs() As Long
		Declare Function CountStatements() As Long
		Declare Function GetStatements() As StatementArray Ptr
        Declare Function GetVarInfo(_idx As Long) As VarInfo Ptr
        Declare Function GetProcInfo(_idx As Long) As VarInfo Ptr
		Declare Function GetVarName(_idx As Long) As String
		Declare Function GetProcName(_idx As Long) As String
		Declare Function GetVarIndex(_name As String) As Long
		Declare Function GetProcIndex(_name As String) As Long
		Declare Function CorrectVarName(_name As String) As String
		Declare Function CorrectProcName(_name As String) As String
		Declare Function IsVarOrProc(_name As String) As Long
		Declare Function IsProc(_name As String) As Long
		Declare Function IsVar(_name As String) As Long
		Declare Function GetArrayLBound(_name As String) As Long
		Declare Function GetArrayLength(_name As String) As Long
		Declare Function IsArray(_name As String) As Long
		Declare Function AttrInVar(_name As String, attr As String) As Long
		Declare Function AttrInProc(_name As String, attr As String) As Long
		Declare Function CountParams(proc_name As String) As Long
		Declare Function GetParamName(proc_name As String, idx As Long) As String
		Declare Function GetParamType(proc_name As String, idx As Long) As String
		Declare Function GetResultType(_name As String) As String

		Declare Sub AddProc(_name As String, _params As BoxedStrArray Ptr, _type As String, _attr As String)
		Declare Sub AddVar(_name As String, _type As String, _size As Long, _lbound As Long, _attr As String)

		Declare Sub AddLocalProc(_name As String, _params As BoxedStrArray Ptr, _result_type As String)
		Declare Sub AddIntVar(_name As String)
		Declare Sub AddIntArray(_name As String, _len As Long, _lbound As Long)

		Declare Sub AddExternProc(_name As String, _params As BoxedStrArray Ptr, _result_type As String)
		Declare Sub AddExternIntVar(_name As String)
		Declare Sub AddExternIntArray(_name As String, _len As Long, _lbound As Long)

		Declare Sub AddGlobalProc(_name As String, _params As BoxedStrArray Ptr, _result_type As String)
		Declare Sub AddGlobalIntVar(_name As String)
		Declare Sub AddGlobalIntArray(_name As String, _len As Long, _lbound As Long)

		Declare Sub SetExplicitLineNumbers()
		Declare Sub ReplaceTargetLineNumbersWithRealLineNumbers(line_index As Long, stmt As Statement Ptr)
		Declare Function FindLine(_start As Long, _target As Long) As Long
		Declare Sub NormalizeStatements()
End Type

Constructor Environment()
	_vars = NewVarInfoArray()
	_procs = NewVarInfoArray()
	_statements = NewStatementArray()
End Constructor
Destructor Environment()
    Delete _vars
    Delete _procs
	Delete _statements
End Destructor

Function Environment.CountVars() As Long
	Return _vars->Count()
End Function
Function Environment.CountProcs() As Long
	Return _procs->Count()
End Function
Function Environment.CountStatements() As Long
	Return _statements->Count()
End Function
Function Environment.GetStatements() As StatementArray Ptr
	Return _statements
End Function
Function Environment.GetVarInfo(_idx As Long) As VarInfo Ptr
	Return _vars->GetVarInfo(_idx)
End Function
Function Environment.GetProcInfo(_idx As Long) As VarInfo Ptr
	Return _procs->GetVarInfo(_idx)
End Function
Function Environment.GetVarName(_idx As Long) As String
	If (_idx < 0) Or (_idx >= _vars->Count()) Then  Return ""
	Return GetVarInfo(_idx)->GetName()
End Function
Function Environment.GetProcName(_idx As Long) As String
	If (_idx < 0) Or (_idx >= _procs->Count()) Then  Return ""
	Return GetProcInfo(_idx)->GetName()
End Function
Function Environment.GetVarIndex(_name As String) As Long
    Return _vars->IndexOfVarInfoByName(_name, FALSE)
End Function
Function Environment.GetProcIndex(_name As String) As Long
    Return _procs->IndexOfVarInfoByName(_name, FALSE)
End Function
Function Environment.CorrectVarName(_name As String) As String
	Return GetVarName(GetVarIndex(_name))
End Function
Function Environment.CorrectProcName(_name As String) As String
	Return GetProcName(GetProcIndex(_name))
End Function
Function Environment.IsVarOrProc(_name As String) As Long
	Return (IsVar(_name) Or IsProc(_name))
End Function
Function Environment.IsProc(_name As String) As Long
	Return (GetProcIndex(_name) <> -1)
End Function
Function Environment.IsVar(_name As String) As Long
	Return (GetVarIndex(_name) <> -1)
End Function
Function Environment.GetArrayLBound(_name As String) As Long
	Dim i As Long : i = GetVarIndex(_name)
	If i = -1 Then  Return -1

	Return GetVarInfo(i)->GetLBound()
End Function
Function Environment.GetArrayLength(_name As String) As Long
	Dim i As Long : i = GetVarIndex(_name)
	If i = -1 Then  Return -1

	Return GetVarInfo(i)->GetSize()
End Function
Function Environment.IsArray(_name As String) As Long
	Return (GetArrayLength(_name) <> -1)
End Function
Function Environment.AttrInVar(_name As String, attr As String) As Long
	Dim i As Long : i = GetVarIndex(_name)
	If i = -1 Then  Return FALSE

	Return (InStr(1, GetVarInfo(i)->GetAttr(), attr) <> 0)
End Function
Function Environment.AttrInProc(_name As String, attr As String) As Long
	Dim i As Long : i = GetProcIndex(_name)
	If i = -1 Then  Return FALSE

	Return (InStr(1, GetProcInfo(i)->GetAttr(), attr) <> 0)
End Function
Function Environment.CountParams(_name As String) As Long
	Dim i As Long : i = GetProcIndex(_name)
	If i = -1 Then  Return 0

	Return GetProcInfo(i)->CountParams()
End Function
Function Environment.GetParamName(_name As String, idx As Long) As String
	Dim i As Long : i = GetProcIndex(_name)
	If i = -1 Then  Return "<error_name>"

	Return GetProcInfo(i)->GetParamName(idx)
End Function
Function Environment.GetParamType(_name As String, idx As Long) As String
	Dim i As Long : i = GetProcIndex(_name)
	If i = -1 Then  Return "<error_type>"

	Return GetProcInfo(i)->GetParamType(idx)
End Function
Function Environment.GetResultType(_name As String) As String
	Dim i As Long : i = GetProcIndex(_name)
	If i = -1 Then  Return "<error_type>"

	Return GetProcInfo(i)->GetType()
End Function
Sub Environment.AddProc(_name As String, _params As BoxedStrArray Ptr, _type As String, _attr As String)
	If IsVarOrProc(_name) Then  Exit Sub

	Dim param_name As String
	Dim param_type As String
	Dim i As Long

	Dim info As VarInfo Ptr
	If _procs->Count() < MAX_VARS_COUNT Then
		info = NewVarInfo()
		info->SetName(_name)
		If _params <> NULL Then
			For i = 0 To _params->Count() - 1 Step 2
				param_name = _params->GetStr(i)
				param_type = _params->GetStr(i + 1)
				info->AddParam(param_name, param_type)
			Next
		End If
		info->SetType(_type)
		info->SetSize(1)
		info->SetLBound(0)
		info->SetAttr(_attr)

		_procs->AddMovedVarInfo(info)
	End If
End Sub
Sub Environment.AddVar(_name As String, _type As String, _size As Long, _lbound As Long, _attr As String)
	If IsVarOrProc(_name) Then  Exit Sub

	Dim info As VarInfo Ptr
	If _vars->Count() < MAX_VARS_COUNT Then
		info = NewVarInfo()
		info->SetName(_name)
		info->SetType(_type)
		info->SetSize(_size)
		info->SetLBound(_lbound)
		info->SetAttr(_attr)

		_vars->AddMovedVarInfo(info)
	End If
End Sub

Sub Environment.AddLocalProc(_name As String, _params As BoxedStrArray Ptr, _result_type As String)
	AddProc(_name, _params, _result_type, "")
End Sub
Sub Environment.AddIntVar(_name As String)
	AddVar(_name, TYPE_INTEGER, -1, 0, "")
End Sub
Sub Environment.AddIntArray(_name As String, _len As Long, _lbound As Long)
	AddVar(_name, TYPE_INTEGER, _len, _lbound, "")
End Sub

Sub Environment.AddExternProc(_name As String, _params As BoxedStrArray Ptr, _result_type As String)
	AddProc(_name, _params, _result_type, ATTR_EXTERN)
End Sub
Sub Environment.AddExternIntVar(_name As String)
	AddVar(_name, TYPE_INTEGER, -1, 0, ATTR_EXTERN)
End Sub
Sub Environment.AddExternIntArray(_name As String, _len As Long, _lbound As Long)
	AddVar(_name, TYPE_INTEGER, _len, _lbound, ATTR_EXTERN)
End Sub

Sub Environment.AddGlobalProc(_name As String, _params As BoxedStrArray Ptr, _result_type As String)
	AddProc(_name, _params, _result_type, ATTR_GLOBAL)
End Sub
Sub Environment.AddGlobalIntVar(_name As String)
	AddVar(_name, TYPE_INTEGER, -1, 0, ATTR_GLOBAL)
End Sub
Sub Environment.AddGlobalIntArray(_name As String, _len As Long, _lbound As Long)
	AddVar(_name, TYPE_INTEGER, _len, _lbound, ATTR_GLOBAL)
End Sub


Sub Environment.SetExplicitLineNumbers()
	Dim stmts As StatementArray Ptr = GetStatements()
	Dim stmt As Statement Ptr
	Dim i As Long
	Dim line_number As Long

	line_number = 1
	For i = 0 To stmts->Count() - 1
		stmt = stmts->GetStatement(i)
		If stmt->GetLineNumber() = -1 Then
			stmt->SetLineNumber(line_number)
		Else
			line_number = stmt->GetLineNumber()
		End If

		line_number += 1
	Next
End Sub

Sub Environment.ReplaceTargetLineNumbersWithRealLineNumbers(line_index As Long, stmt As Statement Ptr)
	Dim stmt2 As Statement Ptr
	Dim i As Long

	Select Case stmt->GetOpr()
	Case STMT_GOTO
		If IsNum(Asc(Mid$(stmt->GetStr(0), 1, 1))) Then
			i = FindLine(line_index, Val(stmt->GetStr(0)))
			If i = -1 Then
				Print "; error: line " + stmt->GetStr(0) + " does not exist"
				Return
			End If

			i += 1
			stmt->SetStr(0, Str$(i))
		End If
	Case STMT_GOSUB
		If IsNum(Asc(Mid$(stmt->GetStr(0), 1, 1))) Then
			i = FindLine(line_index, Val(stmt->GetStr(0)))
			If i = -1 Then
				Print "; error: line " + stmt->GetStr(0) + " does not exist"
				Return
			End If

			i += 1
			stmt->SetStr(0, Str$(i))
		End If
	Case Else
		For i = 0 To stmt->CountStatements() - 1
			stmt2 = stmt->GetStatement(i)
			If stmt2 = NULL Then
				Print "; NULL stmt!"
				Continue For
			End If

			ReplaceTargetLineNumbersWithRealLineNumbers(line_index, stmt2)
		Next
	End Select
End Sub

Function Environment.FindLine(_start As Long, _target As Long) As Long
	Dim stmts As StatementArray Ptr = GetStatements()
	Dim stmt As Statement Ptr
	Dim i As Long
	Dim j As Long
	Dim line_number As Long

	If _start < 0 Or _start >= stmts->Count() Then  Return -1

	' SetExplicitLineNumbers()

	stmt = stmts->GetStatement(_start)
	line_number = stmt->GetLineNumber()

	Print "; find: ", _target, " from:", _start, " (", line_number

	If line_number < _target Then
		Print ";  find forward"
		For i = _start To stmts->Count() - 1
			stmt = stmts->GetStatement(i)
			j = stmt->GetLineNumber()

			If j <> line_number Then
				' when target does not exits and target line number points between 2 lines. return higher as target
				If j > _target Then  Return i
				line_number = j
			End If

			If _target = line_number Then  Return i

		Print ";   .current: ", line_number

			line_number += 1
		Next
	Else
		Print ";  find backward"
		For i = _start To 0 Step -1
			stmt = stmts->GetStatement(i)
			j = stmt->GetLineNumber()

			If j <> line_number Then
				' when target does not exits and target line number points between 2 lines. return higher as target
				If j < _target Then  Return i + 1
				line_number = j
			End If

			If _target = line_number Then  Return i
		Print ";   .current: ", line_number

			line_number -= 1
		Next
	End If

	Return -1
End Function

Sub Environment.NormalizeStatements()
	Dim stmts As StatementArray Ptr = GetStatements()
	Dim stmt As Statement Ptr
	Dim i As Long
	Dim j As Long
	Dim line_number As Long
	Dim c As Long = 1073741824

	Print "; normalize smts"

	' set explicit line_numbers	
	SetExplicitLineNumbers()

	Print "; translate local line numbers"
	For i = 0 To stmts->Count() - 1
		stmt = stmts->GetStatement(i)
		ReplaceTargetLineNumbersWithRealLineNumbers(i, stmt)
	Next

	Print "; translate local line numbers (step2)"
	For i = 0 To stmts->Count() - 1
		stmt = stmts->GetStatement(i)
		stmt->SetLineNumber(i + 1)
	Next
End Sub




#endif
