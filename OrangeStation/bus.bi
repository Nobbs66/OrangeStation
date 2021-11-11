#Include Once "timer.bi"
Type Buss
	rdram(&h7FFFFFF)  As UByte
	bios(&h400000) As UByte
	scratch(&h2000) As UByte
	mch_drd As ULong
	rdram_sdevid As ULong
	mch_ricm As ULong
End Type
Dim Shared Bus As Buss
Declare Function readRDRAM(value As ULong) As ULong
Declare Sub writeRDRAM(value As ULong, addr As ULong)
Function read8(addr As ULong) As UByte
	Dim value As UByte
	If addr >= &h70000000 And addr < &h70004000 Then 
		memcpy(@value, @bus.scratch(addr And &h3FFF), 1)
	Else
		addr And= &h1FFFFFFF
		Select Case addr
			Case 0 To &h1FFFFFFF
				memcpy(@value, @bus.rdram(addr And &h1FFFFFF), 1)
			Case &h1FC00000 To &h1FFFFFFF
				memcpy(@value, @bus.bios(addr And &h3FFFFF), 1)
			Case &h10000000 To &h10FFFFFF
				If (addr >= &h10000000 And addr < &h10001830) Then value = read8Timer(addr)
			Case &h11000000 To &h11003FFF
				Print "Read8 from VU0 Code Memory"
			Case &h11004000 To &h11007FFF
				Print "Read8 from VU0 Data Memory"
			Case &h11008000 To &h1100BFFF
				Print "Read8 from VU1 Code Memory"
			Case &h1100C000 To &h1100FFFF
				Print "Read8 from VU1 Data Memory"
			Case &h12000000 To &h12001FFF
				Print "Read8 from GS Privileged Reg at 0x" & Hex(addr And &h1FFFFFFF)
			Case &h1C000000 To &h1C1FFFFF
				Print "Read8 from IOP RAM at 0x" & Hex(addr And &h1FFFFFFF)
			Case Else 
				Print "Invalid read8 at 0x" & Hex(addr And &h1FFFFFFF)
		End Select
	EndIf
	Return value
End Function
Function read16(addr As ULong) As Ushort
	Dim value As UShort
	If addr >= &h70000000 And addr < &h70004000 Then 
		memcpy(@value, @bus.scratch(addr And &h3FFF), 2)
	Else
		addr And= &h1FFFFFFF
		Select Case addr
			Case 0 To &h1FFFFFFF
				memcpy(@value, @bus.rdram(addr And &h1FFFFFF), 2)
			Case &h10000000 To &h10FFFFFF
				If (addr >= &h10000000 And addr < &h10001830) Then value = read16Timer(addr)
			Case &h1FC00000 To &h1FFFFFFF
				memcpy(@value, @bus.bios(addr And &h3FFFFF), 2)
			Case &h11000000 To &h11003FFF
				Print "Read16 from VU0 Code Memory"
			Case &h11004000 To &h11007FFF
				Print "Read16 from VU0 Data Memory"
			Case &h11008000 To &h1100BFFF
				Print "Read16 from VU1 Code Memory"
			Case &h1100C000 To &h1100FFFF
				Print "Read16 from VU1 Data Memory"
			Case &h12000000 To &h12001FFF
				Print "Read16 from GS Privileged Reg at 0x" & Hex(addr And &h1FFFFFFF)
			Case &h1C000000 To &h1C1FFFFF
				Print "Read16 from IOP RAM at 0x" & Hex(addr And &h1FFFFFFF)
			Case Else 
				Print "Invalid read16 at 0x" & Hex(addr And &h1FFFFFFF)
		End Select
	EndIf
	Return value
End Function
Function read32(addr As ULong) As Ulong
	Dim value As ULong
	If addr >= &h70000000 And addr < &h70004000 Then 
		memcpy(@value, @bus.scratch(addr And &h3FFF), 4)
	Else
		addr And= &h1FFFFFFF
		Select Case addr
			Case 0 To &h1FFFFFF
				memcpy(@value, @bus.rdram(addr And &h1FFFFFF), 4)
			Case &h1FC00000 To &h1FFFFFFF
				memcpy(@value, @bus.bios(addr And &h3FFFFF), 4)
			Case &h10000000 To &h10FFFFFF
				Print "Read32 at I/O reg 0x" & Hex(addr)
				If (addr = &h1000F430 Or addr = &h1000F440) Then value = readRDRAM(addr)
				If (addr >= &h10000000 And addr < &h10001830) Then value = read32Timer(addr)
			Case &h11000000 To &h11003FFF
				Print "Read32 from VU0 Code Memory"
			Case &h11004000 To &h11007FFF
				Print "Read32 from VU0 Data Memory"
			Case &h11008000 To &h1100BFFF
				Print "Read32 from VU1 Code Memory"
			Case &h1100C000 To &h1100FFFF
				Print "Read32 from VU1 Data Memory"
			Case &h12000000 To &h12001FFF
				Print "Read32 from GS Privileged Reg at 0x" & Hex(addr And &h1FFFFFFF)
			Case &h1C000000 To &h1C1FFFFF
				Print "Read32 from IOP RAM at 0x" & Hex(addr And &h1FFFFFFF)
			Case Else 
				Print "Invalid read32 at 0x" & Hex(addr And &h1FFFFFFF)
		End Select
	EndIf
	Return value
End Function
Function read64(addr As ULong) As ULongInt
	Dim value As ULongInt
	If addr >= &h70000000 And addr < &h70004000 Then 
		memcpy(@value, @bus.scratch(addr And &h3FFF), 8)
	Else
		addr And= &h1FFFFFFF
		Select Case addr
			Case 0 To &h1FFFFFFF
				memcpy(@value, @bus.rdram(addr And &h1FFFFFF), 8)
			Case &h1FC00000 To &h1FFFFFFF
				memcpy(@value, @bus.bios(addr And &h3FFFFF), 8)
			Case &h10000000 To &h10FFFFFF
				Print "Read64 from I/O Reg 0x" & Hex(addr)
			Case &h11000000 To &h11003FFF
				Print "Read64 from VU0 Code Memory"
			Case &h11004000 To &h11007FFF
				Print "Read64 from VU0 Data Memory"
			Case &h11008000 To &h1100BFFF
				Print "Read64 from VU1 Code Memory"
			Case &h1100C000 To &h1100FFFF
				Print "Read64 from VU1 Data Memory"
			Case &h12000000 To &h12001FFF
				Print "Read64 from GS Privileged Reg at 0x" & Hex(addr And &h1FFFFFFF)
			Case &h1C000000 To &h1C1FFFFF
				Print "Read64 from IOP RAM at 0x" & Hex(addr And &h1FFFFFFF)
			Case Else 
				Print "Invalid read64 at 0x" & Hex(addr And &h1FFFFFFF)
		End Select
	EndIf
	Return value
End Function
Sub write8(addr As ULong, value As UByte)
	If addr >= &h70000000 And addr < &h70004000 Then 
		memcpy(@bus.scratch(addr And &h3FFF), @value, 1)
	Else
		addr And= &h1FFFFFFF
		Select Case addr
			Case 0 To &h1FFFFFF
				memcpy(@bus.rdram(addr And &h1FFFFFF), @value, 1)
			Case &h10000000 To &h10FFFFFF
				Print "Write8 to IO 0x" & Hex(addr And &h1FFFFFFF)
				If (addr >= &h10000000 And addr < &h10001830) Then write8Timer(addr, value)
				If (addr = &h1000F180) Then Print Chr(value)
			Case &h11000000 To &h11003FFF
				Print "Write8 to VU0 Code Memory"
			Case &h11004000 To &h11007FFF
				Print "Write8 to VU0 Data Memory"
			Case &h11008000 To &h1100BFFF
				Print "Write8 to VU1 Code Memory"
			Case &h1100C000 To &h1100FFFF
				Print "Write8 to VU1 Data Memory"
			Case &h12000000 To &h12001FFF
				Print "Write8 to GS Privileged Reg at 0x" & Hex(addr And &h1FFFFFFF)
			Case &h1C000000 To &h1C1FFFFF
				Print "Write8 to IOP RAM at 0x" & Hex(addr And &h1FFFFFFF)
		End Select
	EndIf
End Sub
Sub write16(addr As ULong, value As UShort)
	If addr >= &h70000000 And addr < &h70004000 Then 
		If addr = &h70003f90 Then Beep
		memcpy(@bus.scratch(addr And &h3FFF), @value, 2)
	Else
		addr And= &h1FFFFFFF
		Select Case addr
			Case 0 To &h1FFFFFF
				memcpy(@bus.rdram(addr And &h1FFFFFF), @value, 2)
			Case &h10000000 To &h10FFFFFF
				Print "Write16 to IO 0x" & Hex(addr And &h1FFFFFFF)
				If (addr >= &h10000000 And addr < &h10001830) Then write16Timer(addr, value)
			Case &h11000000 To &h11003FFF
				Print "Write16 to VU0 Code Memory"
			Case &h11004000 To &h11007FFF
				Print "Write16 to VU0 Data Memory"
			Case &h11008000 To &h1100BFFF
				Print "Write16 to VU1 Code Memory"
			Case &h1100C000 To &h1100FFFF
				Print "Write16 to VU1 Data Memory"
			Case &h12000000 To &h12001FFF
				Print "Write16 to GS Privileged Reg at 0x" & Hex(addr And &h1FFFFFFF)
			Case &h1C000000 To &h1C1FFFFF
				Print "Write16 to IOP RAM at 0x" & Hex(addr And &h1FFFFFFF)
		End Select
	EndIf
End Sub
Sub write32(addr As ULong, value As Ulong)
	If addr >= &h70000000 And addr < &h70004000 Then 
		memcpy(@bus.scratch(addr And &h3FFF), @value, 4)
	Else
		addr And= &h1FFFFFFF
		Select Case addr
			Case 0 To &h1FFFFFF
				memcpy(@bus.rdram(addr And &h1FFFFFF), @value, 4)
			Case &h10000000 To &h10FFFFFF
				Print "Write32 to IO 0x" & Hex(addr And &h1FFFFFFF)
				If (addr >= &h10000000 And addr < &h10001830) Then write32Timer(addr, value)
				If (addr = &h1000F430 Or addr = &h1000F440) Then writeRDRAM(value, addr)
			Case &h11000000 To &h11003FFF
				Print "Write32 to VU0 Code Memory"
			Case &h11004000 To &h11007FFF
				Print "Write32 to VU0 Data Memory"
			Case &h11008000 To &h1100BFFF
				Print "Write32 to VU1 Code Memory"
			Case &h1100C000 To &h1100FFFF
				Print "Write32 to VU1 Data Memory"
			Case &h12000000 To &h12001FFF
				Print "Write32 to GS Privileged Reg at 0x" & Hex(addr And &h1FFFFFFF)
			Case &h1C000000 To &h1C1FFFFF
				Print "Write32 to IOP RAM at 0x" & Hex(addr And &h1FFFFFFF)
		End Select
	EndIf
End Sub
Sub write64(addr As ULong, value As ULongInt)
	Print #99, "Write 64"
	If addr >= &h70000000 And addr < &h70004000 Then 
		memcpy(@bus.scratch(addr And &h3FFF), @value, 8)
	Else
		addr And= &h1FFFFFFF
		Select Case addr
			Case 0 To &h1FFFFFF
				memcpy(@bus.rdram(addr And &h1FFFFFF), @value, 8)
			Case &h10000000 To &h10FFFFFF
				Print "Write64 to IO 0x" & Hex(addr And &h1FFFFFFF)
			Case &h11000000 To &h11003FFF
				Print "Write64 to VU0 Code Memory"
			Case &h11004000 To &h11007FFF
				Print "Write64 to VU0 Data Memory"
			Case &h11008000 To &h1100BFFF
				Print "Write64 to VU1 Code Memory"
			Case &h1100C000 To &h1100FFFF
				Print "Write64 to VU1 Data Memory"
			Case &h12000000 To &h12001FFF
				Print "Write64 to GS Privileged Reg at 0x" & Hex(addr And &h1FFFFFFF)
			Case &h1C000000 To &h1C1FFFFF
				Print "Write64 to IOP RAM at 0x" & Hex(addr And &h1FFFFFFF)
		End Select
	EndIf
	Print #99, "Exit 64"
End Sub
sub write128(addr As ULong, value0 As ULongInt, value1 As ULongInt)
	If addr >= &h70000000 And addr < &h70004000 Then 
		memcpy(@bus.scratch(addr And &h3FFF), @value0, 8)
		memcpy(@bus.scratch((addr + 8) And &h3FFF), @value1, 8)
	Else
		addr And= &h1FFFFFFF
		Select Case addr
			Case 0 To &h1FFFFFF
				memcpy(@bus.rdram(addr And &h7FFFFFF),@value0 ,8)
				memcpy(@bus.rdram((addr + 8) And &h7FFFFFF),@value1 ,8)
			Case &h10000000 To &h10FFFFFF
				Print "Write128 to I/O Reg at 0x" & Hex(addr And &h1FFFFFFF)
			Case &h11000000 To &h11003FFF
				Print "Write128 to VU0 Code Memory"
			Case &h11004000 To &h11007FFF
				Print "Write128 to VU0 Data Memory"
			Case &h11008000 To &h1100BFFF
				Print "Write128 to VU1 Code Memory"
			Case &h1100C000 To &h1100FFFF
				Print "Write128 to VU1 Data Memory"
			Case &h12000000 To &h12001FFF
				Print "Write128 to GS Privileged Reg at 0x" & Hex(addr And &h1FFFFFFF)
			Case &h1C000000 To &h1C1FFFFF
				Print "Write128 to IOP RAM at 0x" & Hex(addr And &h1FFFFFFF)
		End Select
	EndIf
End Sub

Sub writeRDRAM(value As ULong, addr As ULong)
	Select Case addr And &hFFFF
		Case &hf430
			Dim cmd As UShort = ((value Shr 16) And &hFFF)
			Dim subCMD As UByte = ((value Shr 6) And &hF)
			If ((bus.mch_drd shr 7) And 1) = 0 And cmd = &h21 And subCMD = 1 Then
				bus.rdram_sdevid = 0
				bus.mch_ricm = value And Not(1 Shl 31)
			EndIf	
		Case &hf440
			bus.mch_drd = value
	End Select
End Sub
Function readRDRAM(addr As ULong) As ULong
	Dim cmd As UShort = (bus.mch_ricm Shr 16) And &hFFF
	Dim subCMD As UByte = (bus.mch_ricm Shr 6) And &hF
	Dim lsb5 As UByte = bus.mch_ricm And &h1F
	Select Case addr
		Case &h1000F430
			Return 0
		Case &h1000F440
			If (Not(subCMD)) Then 
				Select Case cmd
					Case &h21
						If bus.rdram_sdevid < 2 Then
							bus.rdram_sdevid += 1
							Return &h1F
						EndIf
					Case &h23
						Return &h0D0D
					Case &h24
						Return &h90
					Case &h40
						Return lsb5
				End Select
			EndIf
	End Select
End Function