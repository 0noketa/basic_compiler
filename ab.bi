#ifndef __ab4_bi__
#define __ab4_bi__

' default in lang fb
' Option ByVal

Type BytePtr As Byte Ptr
Type LPCSTR As ZString Ptr
Type DWord As ULong
Type Word As UShort
Type VoidPtr As Any Ptr

#define NULL 0


Function MakeStr(p As Byte Ptr) As String
    Dim p2 As ZString Ptr = p
    MakeStr = *p2
End Function



#endif
