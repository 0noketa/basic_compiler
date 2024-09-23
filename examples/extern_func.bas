
Declare External Sub println(n)
Declare External Function f0()
Declare External Function f1(n)
Declare External Function f2(n As Integer) As Integer
Declare Global Sub ENTRY


ENTRY:
    I = f0()
    J = f1(123) + f2(f1(88) + 1)
    println(I)
    call println(J)
    Return

