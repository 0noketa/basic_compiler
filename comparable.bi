#ifndef comparable__sbp
#define comparable__sbp

#include once "crt.bi"
#include once "ab.bi"


Type IComparable Extends Object
	Public:
		Declare Virtual Function CompareTo(o As IComparable Ptr) As Long
End Type

Function IComparable.CompareTo(o As IComparable Ptr) As Long
	Dim p1 As Long = Cast(Long, @This)
	Dim p2 As Long = Cast(Long, o)

	If p1 < p2 Then
		Return 1
	ElseIf p1 > p2 Then
		Return -1
	Else
		Return 0
	End If
End Function

Type BoxedInteger Extends IComparable
	Protected:
		_value As Long
	Public:
		Declare Constructor(i As Long)
		Declare Function Value() As Long

		Declare Function CompareTo(o As IComparable Ptr) As Long
End Type

Constructor BoxedInteger(i As Long)
	_value = i
End Constructor
Function BoxedInteger.Value() As Long
	Return _value
End Function

Function BoxedInteger.CompareTo(p As IComparable Ptr) As Long
	Dim o As BoxedInteger Ptr = Cast(BoxedInteger Ptr, p)

	If Value() < o->Value() Then
		Return 1
	ElseIf Value() > o->Value() Then
		Return -1
	Else
		Return 0
	End If
End Function

Function NewBoxedInteger(i As Long) As BoxedInteger Ptr
	Return New BoxedInteger(i)
End Function




Type BoxedString Extends IComparable
	Protected:
		_value As String
	Public:
		Declare Constructor(s As String)
		Declare Function Value() As String
		Declare Function Count() As Long
		Declare Function ChrAt(i As Long) As String
		Declare Function ChrCodeAt(i As Long) As Long

		Declare Function CompareTo(o As IComparable Ptr) As Long
End Type

Constructor BoxedString(s As String)
	_value = s
End Constructor
Function BoxedString.Value() As String
	Return _value
End Function
Function BoxedString.Count() As Long
	Return Len(_value)
End Function
Function BoxedString.ChrAt(i As Long) As String
	Return Mid$(_value, i, 1)
End Function
Function BoxedString.ChrCodeAt(i As Long) As Long
	Return Asc(ChrAt(i))
End Function

Function BoxedString.CompareTo(p As IComparable Ptr) As Long
	Dim o As BoxedString Ptr = Cast(BoxedString Ptr, p)
	Dim s As String = Value()
	Dim s2 As String = o->Value()

	Return strcmp(StrPtr(s), StrPtr(s2))
End Function

Function NewBoxedString(s As String) As BoxedString Ptr
	Return New BoxedString(s)
End Function



#endif
