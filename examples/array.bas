Extern Declare Sub PRINTI cdecl ()
Global Dim I As Integer
Global Declare Sub ENTRY cdecl ()
Dim A(0 To 64) As Integer


FIB:
	M = N
	N = 0
	I = 1
	J = 0
*FIB0
	A(LBound(A) + N) = I
	K = I
	I = I + J
	J = K

	If N < M  N = N + 1: Goto *FIB0
	Return

DUMP:
	M = N
	N = 0
Label DUMP0
	I = A(LBound(A) + N)
	Gosub PRINTI

	If N < M  N = N + 1: Goto Label DUMP0
	Return


ENTRY:
	N = 10
	Gosub FIB

	N = 10
	Gosub DUMP
	Return
