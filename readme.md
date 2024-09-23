# BASIC compiler

## feature

### abstract and redundant syntax

``` vb
LOCAL DIM I AS INTEGER
DIM N AS INTEGER
LOCAL X AS INTEGER
EXTERN Y AS INTEGER
DIM A(8) AS INTEGER  ' means (1 TO 8)
DIM B(10 TO 19) AS INTEGER
IF I = 0  N = X
IF I = 1 THEN N = Y
IF I = 2 THEN _
    N = Z
LABEL0:
(LABEL1)
*LABEL2
LABEL LABEL3
```

### useful line numbers

``` basic
Extern Declare Sub PRINTI
Extern Declare Function INPUTI As Long
Global Declare Sub ENTRY
Global Dim I As Long

Dim J
Dim N
Dim M
Dim RESULT

ENTRY:
    I = 0 : J = 0 : N = 0 : M = 0 : RESULT = 0
    N = 8192
    GOSUB PUTPRIMES
    RETURN

ISPRIME:
1
    IF I < 4 RESULT = 1: RETURN
    J = 2: M = I
' line 4. every line has implicit line number.
    IF (I MOD J) = 0  RESULT = 0: RETURN
    IF J < M  J = J + 1: M = I / J: GOTO 4

    RESULT = 1
    RETURN

PUTPRIMES:
1  ' line numbers are reusable. older higher lines will be hidden.
    I = 1
    IF N = 0 RETURN
' line 4  
    GOSUB ISPRIME
    ' line 99 does not exist.
    ' then compiler expects imaginaly line 99 just before line 100.
    IF RESULT = 0 GOTO 99
        GOSUB PRINTI
        N = N - 1
100
    I = I + 1
    IF 0 < N GOTO 4

    RETURN
```

## limitation

### types are not implemented

currently available types are only LONG = INTEGER

### array functions are compile-time operators

builtin functions LEN/LBOUND/UBOUND can be used for array variables.  
but, currently array variables have no run-time information.  

### nested IF can't have ELSE except top-level

ok :

``` basic
IF (I = 0)  N = X  ELSE IF (I = 1)  N = Y : IF (X = Y)  N = N + 1
```

err :

``` basic
IF (I = 0)  N = X  ELSE IF (I = 1)  N = Y  ELSE  N = Z
```

### all kinds of block are not implemented

err :

``` basic
FOR I = 0 TO 9
  A(I) = I
NEXT I
```

err :

``` basic
IF I = 0 THEN
  N = X
END IF
```

err :

``` vb
SUB MYPROC
  MYPROC2()
END SUB
```

## plans

by priority.

* at least block-IF/WHILE/FOR/SUB should be implemented.  
* separated code generator(s).
* OPTION statement for language modification, such as implicit first line number in SUB-block (1 or global implicit line number).
* DIM-AS and ID-list.
* STRING with allocation options.
* ALIAS and CONST.
* pointers.
* local labels outside SUB, such as ".LABEL1:".
* TYPE/CLASS.
* SELECT-CASE.
* FreeBASIC-like EXTERN for separated declarations of public elements.
* DEF/DEF-PROC(equals to SUB with optional result)
