
Declare Sub println(n)
Declare Function f0()
Declare Function f1(n)
Declare Function f2(n As Integer) As Integer
Global ENTRY


ENTRY:
    I = f0()
    J = f1(123) + f2(f1(88) + 1)
    call println(I)
    call println(J)
    Return

