Declare Sub fb_proc cdecl Alias "FBPROC" ()
Declare Sub bc_proc cdecl Alias "BCPROC" ()

Extern dat Alias "FBDATA" As Long

Sub myprint cdecl Alias "my_print" (n As Long)
    print CInt(n)
End Sub


dat = 987
fb_proc

bc_proc
bc_proc

dat = 765
bc_proc

