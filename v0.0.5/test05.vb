'
' main function
'
Sub Main(argv() As String)
   
    Dim ab0_1 As String
    Dim c() As String
    Dim d As String
    Dim e As String
    Dim n1 As Integer
    
    ' assign statement
    ab0_1 = "Hello "
    d = "World."

    e = ab0_1 & d

    Msgbox e

    Subroutine01 3, 5

    n1 = Func01(2, 3)
    Msgbox "n1 = " & CStr(n1)

End Sub


Sub Subroutine01(p1 As Integer, p2 As Integer)

    Dim c As Integer
    Dim d As String
    
    c = p1 + p2
    d = CStr(c)
    
    Msgbox d

End Sub


Function Func01(p1 As Integer, p2 As Integer) As Integer

    Func01 = p1 + p2
    
End Function

