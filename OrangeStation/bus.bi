Type Buss
	rdram(&h7FFFFFF)  As UByte
	bios(&h400000) As UByte
	Declare Function read32(addr As UInteger) As UInteger
End Type
Dim Shared Bus As Buss
Function read8(addr As Ulong) As UByte
	Dim value As ULong
		Select Case addr
			Case 0 To &h7FFFFFF
				Print "Reading from System RAM!"
				Return 0
			Case &hBFC00000 To &hBFFFFFFF
				memcpy(@value, @bus.bios(addr And &h3FFFFF),4)
			Case Else 
				Print "Reading from Unhandled address " & Hex(addr)
		End Select 
		Return value
End Function
Function read16(addr As ULong) As Ushort
	Dim value As ULong
		Select Case addr
			Case 0 To &h7FFFFFF
				Print "Reading from System RAM!"
				Return 0
			Case &hBFC00000 To &hBFFFFFFF
				memcpy(@value, @bus.bios(addr And &h3FFFFF),4)
			Case Else 
				Print "Reading from Unhandled address " & Hex(addr)
		End Select 
		Return value
End Function
Function read32(addr As Ulong) As ULong
	Dim value As ULong
		Select Case addr
			Case 0 To &h7FFFFFF
				Print "Reading from System RAM!"
				Return 0
			Case &hBFC00000 To &hBFFFFFFF
				memcpy(@value, @bus.bios(addr And &h3FFFFF),4)
			Case Else 
				Print "Reading from Unhandled address " & Hex(addr)
		End Select 
		Return value
End Function
Function read64(addr As ULong) As ULongInt
	Dim value As ULong
		Select Case addr
			Case 0 To &h7FFFFFF
				memcpy(@value, @bus.rdram(addr And &h7FFFFFF),8)
				Print "Reading from System RAM!"
			Case &hBFC00000 To &hBFFFFFFF
				memcpy(@value, @bus.bios(addr And &h3FFFFF),4)
			Case Else 
				Print "Reading from Unhandled address " & Hex(addr)
		End Select 
		Return value
End Function 
Function read128(addr As ULong) As ULongInt
	Dim value(2) As ULongInt
	Return value()
End Function
sub write8(addr As ULong, value As UByte)
	Select Case addr
		Case 0 To &h7FFFFFF
				memcpy(@bus.rdram(addr And &h7FFFFFF),@value ,1)
		Case &hBFC00000 To &hBFFFFFFF
				Print "Writing to BIOS ROM"
			Case Else 
				Print "Writing to Unhandled address " & Hex(addr)
	End Select 
End Sub
sub write16(addr As ULong, value As UShort)
	Select Case addr
		Case 0 To &h7FFFFFF
				memcpy(@bus.rdram(addr And &h7FFFFFF),@value ,2)
		Case &hBFC00000 To &hBFFFFFFF
				Print "Writing to BIOS ROM"
			Case Else 
				Print "Writing to Unhandled address " & Hex(addr)
	End Select
End Sub
sub write32(addr As ULong, value As ULong)
	Select Case addr
		Case 0 To &h7FFFFFF
				memcpy(@bus.rdram(addr And &h7FFFFFF),@value ,4)
		Case &hBFC00000 To &hBFFFFFFF
				Print "Writing to BIOS ROM"
			Case Else 
				Print "Writing to Unhandled address " & Hex(addr)
	End Select
End Sub
sub write64(addr As ULong, value As ULongInt)
	Select Case addr
		Case 0 To &h7FFFFFF
				memcpy(@bus.rdram(addr And &h7FFFFFF),@value ,8)
		Case &hBFC00000 To &hBFFFFFFF
				Print "Writing to BIOS ROM"
			Case Else 
				Print "Writing to Unhandled address " & Hex(addr)
	End Select
End Sub
sub write128(addr As ULong, value0 As ULongInt, value1 As ULongInt)
	Select Case addr
		Case 0 To &h7FFFFFF
				memcpy(@bus.rdram(addr And &h7FFFFFF),@value0 ,8)
				memcpy(@bus.rdram((addr + 8) And &h7FFFFFF),@value1 ,8)
		Case &hBFC00000 To &hBFFFFFFF
				Print "Writing to BIOS ROM"
			Case Else 
				Print "Writing to Unhandled address " & Hex(addr)
	End Select
End Sub