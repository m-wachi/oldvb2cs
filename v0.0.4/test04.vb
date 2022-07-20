'
' main function
'
Sub Main(argv() As String)
   
    Dim ab0_1 As String
    Dim c() As String
    Dim d As String
    Dim e As String
    
    ' assign statement
    ab0_1 = "Hello "
    d = "World."

    e = ab0_1 & d

    Msgbox e

End Sub


Sub Subroutine01(p1 As Integer, p2 As Integer)

    Dim c As Integer
    Dim d As String
    
    c = p1 + p2
    d = CStr(d)
    
    Msgbox d

End Sub

