#ifndef args__sbp
#define args__sbp
' /* options
' 	ARGS_NO_Q: trim double quortation
' */

#include once "crt.bi"
#include once "ab.bi"


Declare Function GetCommandLineA stdcall Lib "kernel32.dll" () As LPCSTR

Dim Shared Argc As DWord
Dim Shared Args_CommandLine As BytePtr

Args_CommandLine = GetCommandLineA()


Function Args_SkipSpc(p As BytePtr, i As DWord) As DWord
	While *(p + i) <> 0
		If *(p + i) <> 32 Then
			Exit While
		End If
		i = i + 1
	Wend
	Args_SkipSpc = i
End Function

Function Args_SkipArg(p As BytePtr, i As DWord) As DWord
	Dim is_str As DWord

	is_str = 0

	While *(p + i) <> 0
		If *(p + i) = 34 Then
			is_str = (is_str = 0)
		End If

		If (*(p + i) = 32) And (is_str = 0) Then
			Exit While
		End If

		i = i + 1
	Wend
	Args_SkipArg = i
End Function


sub Args_InitArgc()
	Dim p As BytePtr
	Dim i As DWord
	Dim j As DWord

	p = Args_CommandLine
	Argc = 0
	j = 0
	While p[j] <> 0
		i = Args_SkipSpc(p, j)
		j = Args_SkipArg(p, i)
		If *(p + i) <> 0 Then
			Argc = Argc + 1
		End If
	Wend
End Sub

Function Argv(n As DWord) As String
	Dim p As BytePtr
	Dim q As BytePtr
	Dim h As DWord
	Dim l As DWord
	Dim i As DWord
	Dim j As DWord
	Dim m As DWord

	If Argc <= n Then
		Argv = ""
		Exit Function
	End If

	p = Args_CommandLine
	m = 0
	j = 0
	While *(p + j) <> 0
		i = Args_SkipSpc(p, j)
		j = Args_SkipArg(p, i)
		If *(p + i) <> 0 Then
			If n = m Then
#ifdef ARGS_NO_Q
				If (*(p + i) = 34) And (i < j) Then
					l = j - i - 1
					h = i + 1
					If (i + 1 < j) And(*(p + (j - 1)) = 34) Then
						l = l - 1
					End If
				Else
					l = j - i
					h = i
				End If

				q = Allocate(l + 1)
				*(q + l) = 0
				memcpy(q, p + h, l)
				Argv = MakeStr(Cast(ZString Ptr, q))
				Deallocate(q)
#else
				q = Allocate(j - i + 1)
				*(q + (j - i)) = 0
				memcpy(q, p + i, j - i)
				Argv = MakeStr(Cast(ZString Ptr, q))
				Deallocate(q)
#endif
				Exit Function
			End If

			m = m + 1
		End If
	Wend
End Function


Args_InitArgc()


#endif
