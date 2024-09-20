
#ifndef array__sbp
#define array__sbp

' CLI-like Array

#include "comparable.bi"


Type BoxedArray Extends Object
	Protected:
		_cap As Long
		_length As Long
		_array As IComparable Ptr Ptr
	Public:
		' do not call directly. use NewBoxedArray instead of this.
		Declare Constructor()
		Declare Destructor()

		Declare Sub Shuffle(n As Long)
		Declare Sub Sort()

		Declare Virtual Function GetItem(i As Long) As IComparable Ptr
		Declare Virtual Sub SetItem(i As Long, v As IComparable Ptr)
		Declare Virtual Function ExtractItem(i As Long) As IComparable Ptr
		Declare Virtual Function IndexOfItem(v As IComparable Ptr, _start As Long = 0) As Long
		Declare Virtual Function LastIndexOfItem(v As IComparable Ptr) As Long
		Declare Virtual Sub AddItem(v As IComparable Ptr)
		Declare virtual Sub RemoveItem(idx As Long)
		Declare Virtual Function Length() As Long

		Declare Function Count() As Long
End Type

Constructor BoxedArray()
	_length = 0
	_cap = 16
	_array = malloc(SizeOf(IComparable Ptr) * _cap)

	Dim i As Long
	For i = 0 To _cap - 1
		(*(_array + i)) = NULL
	Next
End Constructor

Destructor BoxedArray()
	If _array <> NULL Then
		free(_array)
		_array = NULL
	End If
End Destructor

Sub BoxedArray.Shuffle(n As Long)
	Dim p As IComparable Ptr
	Dim q As IComparable Ptr
	Dim x As Long
	Dim i As Long, j As Long

	'Randomize 10
	While 0<n
		x = Length()
		i = Abs(Int(Rnd()*x)) Mod x
		j = Abs(Int(Rnd()*x)) Mod x
		p = GetItem(i)
		q = GetItem(j)
		SetItem(i, q)
		SetItem(j, p)
		n = n-1
	Wend
End Sub

Sub BoxedArray.Sort()
	Dim p As IComparable Ptr
	Dim q As IComparable Ptr
	Dim i As Long
	Dim j As Long
	Dim done As Long

	Do
		done = TRUE

		For i = 0 To Length() - 2
			For j = i + 1 To Length() - 1
				p = GetItem(i)
				q = GetItem(j)
				If p->CompareTo(q) = -1 Then
					SetItem(i, q)
					SetItem(j, p)
					done = FALSE
				End If
			Next
		Next
	Loop While done = FALSE
End Sub

Function BoxedArray.GetItem(i As Long) As IComparable Ptr
	If (0 <= i) And (i < _length) Then
		Return *(_array + i)
	Else
		Return NULL
	End If
End Function

Sub BoxedArray.SetItem(i As Long, v As IComparable Ptr)
	If (0 <= i) And (i < _length) Then
		*(_array + i) = v
	End If
End Sub

Function BoxedArray.ExtractItem(idx As Long) As IComparable Ptr
	Dim o As IComparable Ptr = GetItem(idx)
	SetItem(idx, NULL)
	Return o
End Function

Sub BoxedArray.AddItem(v As IComparable Ptr)
	If _length >= _cap Then
		_cap += 16
		_array = realloc(_array, SizeOf(IComparable Ptr) * _cap)
	End If

	_length = _length + 1
	SetItem(_length - 1, v)
End Sub

Sub BoxedArray.RemoveItem(idx As Long)
	If idx < _length Then
		memcpy(_array + idx, _array + idx + 1, SizeOf(IComparable Ptr) * (_cap - idx - 1))
	End If

	_length = _length - 1
End Sub

Function BoxedArray.IndexOfItem(v As IComparable Ptr, _start As Long = 0) As Long
	Dim i As Long
	For i = _start To _length - 1
		If v->CompareTo(*(_array + i)) = 0 Then 
			Return i
		End If
	Next
	Return -1
End Function

Function BoxedArray.LastIndexOfItem(v As IComparable Ptr) As Long
	Dim i As Long
	For i = _length - 1 To 0
		If v->CompareTo(*(_array + i)) = 0 Then 
			Return i
		End If
	Next
	Return -1
End Function

Function BoxedArray.Length() As Long
	Return _length
End Function

Function BoxedArray.Count() As Long
	Count = Length()
End Function

Function NewBoxedArray() As BoxedArray Ptr
	Dim p As BoxedArray Ptr : p = New BoxedArray
	Return p
End Function


Type BoxedIntArray Extends BoxedArray
	Public:
		Declare Destructor()
		Declare Sub AddMovedBoxedInt(p As BoxedInteger Ptr)
		Declare Sub AddInt(i As Long)
		Declare Function IndexOfBoxedInt(p As BoxedInteger Ptr, _start As Long = 0) As Long
		Declare Function IndexOfInt(n As Long, _start As Long = 0) As Long
		Declare Function LastIndexOfInt(n As Long) As Long
		Declare Function GetBoxedInt(idx As Long) As BoxedInteger Ptr
		Declare Function GetInt(idx As Long) As Long
End Type

Destructor BoxedIntArray()
	Dim i As Long
	For i = 0 To _length - 1
		If GetBoxedInt(i) <> NULL Then  Delete GetBoxedInt(i)
	Next
End Destructor

Sub BoxedIntArray.AddMovedBoxedInt(p As BoxedInteger Ptr)
	AddItem(p)
End Sub
Sub BoxedIntArray.AddInt(i As Long)
	Dim p As BoxedInteger Ptr : p = NewBoxedInteger(i)
	AddMovedBoxedInt(p)
End Sub

Function BoxedIntArray.IndexOfBoxedInt(p As BoxedInteger Ptr, _start As Long = 0) As Long
	IndexOfBoxedInt = IndexOfItem(p, _start)
End Function
Function BoxedIntArray.IndexOfInt(n As Long, _start As Long = 0) As Long
	Dim i As Long
	For i = _start To Count() - 1
		If GetInt(i) = n Then  Return i
	Next
	Return -1
End Function
Function BoxedIntArray.LastIndexOfInt(n As Long) As Long
	Dim i As Long
	For i = Count() - 1 To 0 Step -1
		If GetInt(i) = n Then  Return i
	Next
	Return -1
End Function

Function BoxedIntArray.GetBoxedInt(idx As Long) As BoxedInteger Ptr
	Return Cast(BoxedInteger Ptr, GetItem(idx))
End Function
Function BoxedIntArray.GetInt(idx As Long) As Long
	Dim p As BoxedInteger Ptr : p = Cast(BoxedInteger Ptr, GetItem(idx))
	If p <> NULL Then
		Return p->Value()
	Else
		Return 0
	End If
End Function

Function NewBoxedIntArray() As BoxedIntArray Ptr
	Dim p As BoxedIntArray Ptr : p = New BoxedIntArray
	Return p
End Function


Type BoxedStrArray Extends BoxedArray
	Public:
		Declare Destructor()
		Declare Sub AddMovedBoxedStr(p As BoxedString Ptr)
		Declare Sub AddStr(i As String)
		Declare Function IndexOfBoxedStr(p As BoxedString Ptr, _start As Long = 0) As Long
		Declare Function IndexOfStr(s As String, _start As Long = 0) As Long
		Declare Function LastIndexOfStr(s As String) As Long
		Declare Function GetBoxedStr(idx As Long) As BoxedString Ptr
		Declare Sub SetStr(idx As Long, s As String)
		Declare Sub SetMovedBoxedStr(idx As Long, o As BoxedString Ptr)
		Declare Function ExtractBoxedStr(idx As Long) As BoxedString Ptr
		Declare Function ExtractRemovedBoxedStr(idx As Long) As BoxedString Ptr
		Declare Sub RemoveBoxedStr(idx As Long)
		Declare Function GetStr(idx As Long) As String
		Declare Function Join(sep As string) As String
End Type

Destructor BoxedStrArray()
	Dim i As Long
	For i = 0 To _length - 1
		If GetBoxedStr(i) <> NULL Then  Delete GetBoxedStr(i)
	Next
End Destructor

Sub BoxedStrArray.AddMovedBoxedStr(p As BoxedString Ptr)
	AddItem(p)
End Sub
Sub BoxedStrArray.AddStr(i As String)
	Dim p As BoxedString Ptr : p = NewBoxedString(i)
	AddMovedBoxedStr(p)
End Sub

Function BoxedStrArray.IndexOfBoxedStr(p As BoxedString Ptr, _start As Long = 0) As Long
	Return IndexOfItem(p, _start)
End Function
Function BoxedStrArray.IndexOfStr(s As String, _start As Long = 0) As Long
	Dim i As Long
	For i = _start To Count() - 1
		If GetStr(i) = s Then  Return i
	Next
	Return -1
End Function
Function BoxedStrArray.LastIndexOfStr(s As String) As Long
	Dim i As Long
	For i = Count() - 1 To 0 Step -1
		If GetStr(i) = s Then  Return i
	Next
	Return -1
End Function

Sub BoxedStrArray.SetStr(idx As Long, s As String)
	Dim o As BoxedString Ptr = NewBoxedString(s)
	SetMovedBoxedStr(idx, o)
End Sub
Sub BoxedStrArray.SetMovedBoxedStr(idx As Long, o As BoxedString Ptr)
	Dim o0 As BoxedString Ptr = Cast(BoxedString Ptr, GetItem(idx))
	If o0 <> NULL Then  Delete o0
	SetItem(idx, o)
End Sub
Function BoxedStrArray.ExtractBoxedStr(idx As Long) As BoxedString Ptr
	Return Cast(BoxedString Ptr, ExtractItem(idx))
End Function
Function BoxedStrArray.ExtractRemovedBoxedStr(idx As Long) As BoxedString Ptr
	Dim o As BoxedString Ptr : o = ExtractBoxedStr(idx)
	RemoveItem(idx)
	Return o
End Function
Sub BoxedStrArray.RemoveBoxedStr(idx As Long)
	Dim o As BoxedString Ptr : o = ExtractRemovedBoxedStr(idx)
	If o <> NULL Then  Delete o
End Sub

Function BoxedStrArray.GetBoxedStr(idx As Long) As BoxedString Ptr
	Return Cast(BoxedString Ptr, GetItem(idx))
End Function
Function BoxedStrArray.GetStr(idx As Long) As String
	Dim p As BoxedString Ptr : p = GetBoxedStr(idx)
	If p <> NULL Then
		Return p->Value()
	Else
		Return ""
	End If
End Function
Function BoxedStrArray.Join(sep As string) As String
	Dim s As String = ""
	Dim i As Long
	If Count() = 0 Then  Return s

	s = GetStr(0)
	For i = 1 To Count() - 1
		s += sep + GetStr(i)
	Next

	Return s
End Function

Function NewBoxedStrArray() As BoxedStrArray Ptr
	Dim p As BoxedStrArray Ptr : p = New BoxedStrArray
	Return p
End Function


Type SlicedArray
	Protected:
		_array As BoxedArray Ptr
		_start As Long
		_end As Long
	Public:
		Declare Sub Init(_array As BoxedArray Ptr, _start As Long, _end As Long)
		Declare Function GetItem(i As Long) As IComparable Ptr
		Declare Function GetAbsIndex(i As Long) As Long
		Declare Function Count() As Long
End Type

Sub SlicedArray.Init(p As BoxedArray Ptr, i As Long, j As Long)
	_array = p
	_start = i
	_end = j
End Sub
Function SlicedArray.GetItem(i As Long) As IComparable Ptr
	Return _array->GetItem(_start + i)
End Function
Function SlicedArray.GetAbsIndex(i As Long) As Long
	Return _start + i
End Function
Function SlicedArray.Count() As Long
	Return _end - _start
End Function

#endif
