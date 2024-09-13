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

Const TYPE_VOID = "VOID"
Const TYPE_INTEGER = "INTEGER"
Const TYPE_LONG = "LONG"
Const TYPE_STRING = "STRING"
Const TYPE_BOOL = "BOOL"

Const ATTR_EXTERN = "e"
Const ATTR_GLOBAL = "g"
Const ATTR_STATIC = "s"


Type VarInfo Extends IComparable
	Protected:
		_name As String
		_type As String
		_param_type As String
		_size As Long
		_lbound As Long
		_attr As String
	Public:
		Declare Function GetName() As String
		Declare Sub SetName(s As String)
		Declare Function GetType() As String
		Declare Sub SetType(s As String)
		Declare Function GetParamType() As String
		Declare Sub SetParamType(s As String)
		Declare Function GetSize() As Long
		Declare Sub SetSize(s As Long)
		Declare Function GetLBound() As Long
		Declare Sub SetLBound(s As Long)
		Declare Function GetAttr() As String
		Declare Sub SetAttr(s As String)
        Declare Function CompareTo(p As IComparable Ptr) As Long
End Type

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
Function VarInfo.GetParamType() As String
	Return _param_type
End Function
Sub VarInfo.SetParamType(s As String)
	_param_type = s
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
		Declare Sub SetMovedVarInfo(idx As Long, o As VarInfo Ptr)
		Declare Function RemoveVarInfo(idx As Long, ByRef out_name As BoxedString Ptr) As VarInfo Ptr
		Declare Sub DeleteVarInfo(idx As Long)
		Declare Function GetVarInfo(idx As Long) As VarInfo Ptr
		Declare Function GetName(idx As Long) As String
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
Function VarInfoArray.RemoveVarInfo(idx As Long, ByRef out_name As BoxedString Ptr) As VarInfo Ptr
	Dim o0 As VarInfo Ptr = GetVarInfo(idx)
    out_name = _keys->RemoveBoxedStr(idx)
	SetItem(idx, NULL)
	Return o0
End Function
Sub VarInfoArray.DeleteVarInfo(idx As Long)
    Dim s As BoxedString Ptr
	Dim o As VarInfo Ptr = RemoveVarInfo(idx, s)
    Delete s
	Delete o
End Sub

Function VarInfoArray.GetVarInfo(idx As Long) As VarInfo Ptr
	Return Cast(VarInfo Ptr, GetItem(idx))
End Function
Function VarInfoArray.GetName(idx As Long) As String
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
		Declare Function GetParamType(_name As String) As String
		Declare Function GetResultType(_name As String) As String
		Declare Sub AddProc(_name As String, _arg_type As String, _type As String, _attr As String)
		Declare Sub AddVar(_name As String, _type As String, _size As Long, _lbound As Long, _attr As String)
		Declare Sub AddVoidProc(_name As String)
		Declare Sub AddIntProc(_name As String)
		Declare Sub AddIntFunc(_name As String)
		Declare Sub AddNullaryIntFunc(_name As String)
		Declare Sub AddIntVar(_name As String)
		Declare Sub AddIntArray(_name As String, _len As Long, _lbound As Long)
		Declare Sub AddExternVoidProc(_name As String)
		Declare Sub AddExternIntProc(_name As String)
		Declare Sub AddExternNullaryIntFunc(_name As String)
		Declare Sub AddExternIntFunc(_name As String)
		Declare Sub AddExternIntVar(_name As String)
		Declare Sub AddExternIntArray(_name As String, _len As Long, _lbound As Long)

		Declare Sub AddGlobalVoidProc(_name As String)
		Declare Sub AddGlobalIntProc(_name As String)
		Declare Sub AddGlobalNullaryIntFunc(_name As String)
		Declare Sub AddGlobalIntFunc(_name As String)
		Declare Sub AddGlobalIntVar(_name As String)
End Type

Constructor Environment()
	_vars = NewVarInfoArray()
	_procs = NewVarInfoArray()
End Constructor
Destructor Environment()
    Delete _vars
    Delete _procs
End Destructor

Function Environment.CountVars() As Long
	Return _vars->Count()
End Function
Function Environment.CountProcs() As Long
	Return _procs->Count()
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
Function Environment.GetParamType(_name As String) As String
	Dim i As Long : i = GetProcIndex(_name)
	If i = -1 Then  Return "error_type"

	Return GetProcInfo(i)->GetParamType()
End Function
Function Environment.GetResultType(_name As String) As String
	Dim i As Long : i = GetProcIndex(_name)
	If i = -1 Then  Return "error_type"

	Return GetProcInfo(i)->GetType()
End Function
Sub Environment.AddProc(_name As String, _arg_type As String, _type As String, _attr As String)
	If IsVarOrProc(_name) Then  Exit Sub

	Dim info As VarInfo Ptr
	If _procs->Count() < MAX_VARS_COUNT Then
		info = NewVarInfo()
		info->SetName(_name)
		info->SetParamType(_arg_type)
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
		info->SetParamType("")
		info->SetType(_type)
		info->SetSize(_size)
		info->SetLBound(_lbound)
		info->SetAttr(_attr)

		_vars->AddMovedVarInfo(info)
	End If
End Sub

Sub Environment.AddVoidProc(_name As String)
	AddProc(_name, "", "", "")
End Sub
Sub Environment.AddIntProc(_name As String)
	AddProc(_name, TYPE_INTEGER, "", "")
End Sub
Sub Environment.AddNullaryIntFunc(_name As String)
	AddProc(_name, "", TYPE_INTEGER, "")
End Sub
Sub Environment.AddIntFunc(_name As String)
	AddProc(_name, TYPE_INTEGER, TYPE_INTEGER, "")
End Sub
Sub Environment.AddIntVar(_name As String)
	AddVar(_name, TYPE_INTEGER, -1, 0, "")
End Sub
Sub Environment.AddIntArray(_name As String, _len As Long, _lbound As Long)
	AddVar(_name, TYPE_INTEGER, _len, _lbound, "")
End Sub

Sub Environment.AddExternVoidProc(_name As String)
	AddProc(_name, "", "", "e")
End Sub
Sub Environment.AddExternIntProc(_name As String)
	AddProc(_name, TYPE_INTEGER, "", "e")
End Sub
Sub Environment.AddExternNullaryIntFunc(_name As String)
	AddProc(_name, "", TYPE_INTEGER, "e")
End Sub
Sub Environment.AddExternIntFunc(_name As String)
	AddProc(_name, TYPE_INTEGER, TYPE_INTEGER, "e")
End Sub
Sub Environment.AddExternIntVar(_name As String)
	AddVar(_name, TYPE_INTEGER, -1, 0, "e")
End Sub
Sub Environment.AddExternIntArray(_name As String, _len As Long, _lbound As Long)
	AddVar(_name, TYPE_INTEGER, _len, _lbound, "e")
End Sub

Sub Environment.AddGlobalVoidProc(_name As String)
	AddProc(_name, "", "", "g")
End Sub
Sub Environment.AddGlobalIntProc(_name As String)
	AddProc(_name, TYPE_INTEGER, "", "g")
End Sub
Sub Environment.AddGlobalNullaryIntFunc(_name As String)
	AddProc(_name, "", TYPE_INTEGER, "g")
End Sub
Sub Environment.AddGlobalIntFunc(_name As String)
	AddProc(_name, TYPE_INTEGER, TYPE_INTEGER, "g")
End Sub
Sub Environment.AddGlobalIntVar(_name As String)
	AddVar(_name, TYPE_INTEGER, -1, 0, "g")
End Sub

#endif
