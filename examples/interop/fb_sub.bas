Declare Sub myprint cdecl Alias "my_print" (n As Long)

Extern dat Alias "FBDATA" As Long
Dim Shared dat As Long

Sub proc cdecl Alias "FBPROC" ()
    myprint(dat)
    dat = 0
End Sub
