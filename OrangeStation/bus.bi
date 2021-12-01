#Include Once "timer.bi"
#Include Once "io.bi"
Type Buss
	rdram(&h7FFFFFF)  As UByte
	bios(&h400000) As UByte
	scratch(&h2000) As UByte
	iop_ram(&h200000) As UByte
	mch_drd As ULong
	rdram_sdevid As ULong
	mch_ricm As ULong
	intc_stat As ULong
End Type
Dim Shared Bus As Buss
Function read8(addr As ULong) As UByte
	Dim value As UByte
	If addr >= &h70000000 And addr < &h70004000 Then 
		memcpy(@value, @bus.scratch(addr And &h3FFF), 1)
	Else
		addr And= &h1FFFFFFF
		Select Case addr
			Case 0 To &h1FFFFFFF
				memcpy(@value, @bus.rdram(addr And &h1FFFFFF), 1)
			Case &h10000000 To &h10FFFFFF
				Print "[I/O]Read8 at 0x" & Hex(addr)
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
			Case &h1FC00000 To &h1FFFFFFF
				memcpy(@value, @bus.bios(addr And &h3FFFFF), 1)
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
				Print "[I/O]Read16 at 0x" & Hex(addr)
				If (addr >= &h10000000 And addr < &h10001830) Then value = read16Timer(addr)
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
			Case &h1FC00000 To &h1FFFFFFF
				memcpy(@value, @bus.bios(addr And &h3FFFFF), 2)
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
			Case &h10000000 To &h10FFFFFF
				'Print "[I/O]Read32 at 0x" & Hex(addr)
				If addr = ee_ioregs.INTC_STAT Then Return bus.intc_stat
				If addr = &H1000F130 Then Return 0
				If (addr >= &h10000000 And addr < &h10001830) Then value = read32Timer(addr)
				Select Case addr And &h1FFFFFFF
					Case &h1000F410
						Return 0
					Case ee_Ioregs.MCH_RICM
						Select Case ((bus.mch_ricm Shr 16) And &hFFF)
							Case &H21
								Print "MCH_RICM: 0x21"
								If (bus.rdram_sdevid < 2) Then  'PS2 has two RDRAM modules
									bus.rdram_sdevid  += 1
									Return &h1F
								EndIf
								Return 0
							Case &h23
								Print "MCH_RICM: 0x23"
								Return &h0D0D
							Case &h24
								Print "MCH_RICM: 0x24"
								Return &h0090
							Case &h40
								Print "MCH_RICM: 0x40"
								Return bus.mch_ricm And &h1F
							Case Else
								Print "Unknown MCH_RICM Read " & Hex((bus.mch_ricm Shr 16) And &hFFF)
								Return 0
						End Select
					Case ee_ioregs.MCH_DRD
						Return 0
				End Select
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
			Case &h1FC00000 To &h1FFFFFFF
				memcpy(@value, @bus.bios(addr And &h3FFFFF), 4)
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
	If addr = &hB000F180 Then 
		Cls
		For i As Integer = 0 To 10
			Print "FINALLY"
		Next
	EndIf
	If addr >= &h70000000 And addr < &h70004000 Then 
		memcpy(@bus.scratch(addr And &h3FFF), @value, 1)
	Else
	If (addr =	ee_Ioregs.STDIO_TX) Then 
					cls
				EndIf
		addr And= &h1FFFFFFF
		Select Case addr
			Case 0 To &h1FFFFFF
				memcpy(@bus.rdram(addr And &h1FFFFFF), @value, 1)
			Case &h10000000 To &h10FFFFFF
				Print "Write8 to IO 0x" & Hex(addr And &h1FFFFFFF)
				cls
				If (addr >= &h10000000 And addr < &h10001830) Then write8Timer(addr, value)
				
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
				If (addr =	ee_Ioregs.STDIO_TX) Then 
					Cls
					Print Chr(value)
				EndIf
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
				If (addr = ee_ioRegs.INTC_STAT) Then bus.intc_stat = value And Not(1 Shl 31)
				If (addr =ee_Ioregs.STDIO_TX) Then Print Chr(value)
				If addr = ee_ioregs.MCH_RICM Then 
					Print ""
					Print "MCH_RICM: 0x" & Hex(value)
					Print ""
					If ((((value Shr 16) And &hFFF) = &h21) AndAlso (((value Shr 6) And &hF) = 1) AndAlso (((bus.mch_drd Shr 7) And 1) = 0)) Then  
						Print "Clear SVDEV"
						bus.rdram_sdevid = 0
					EndIf
					bus.mch_ricm = value And &h7FFFFFFF
				EndIf
				If addr = ee_ioregs.MCH_DRD Then 
					Print "MCH_DRD: 0x" & Hex(value)
					bus.mch_drd = value
				EndIf
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

Function iop_read8(addr As ULong) As UByte
	Dim value As UByte
	If addr < &hC0000000 Then 
		addr And= &h1FFFFFFF
		Select Case addr 
			Case 0 To &h1FFFFF
				memcpy(@value, @bus.iop_ram(addr And &h1FFFFF), 1)
			Case &h1F400000 To &h1F500000
				Print "[CDVD] Read8 0x" & Hex(addr)
				sleep
			Case &h1F800000 To &h1F810000
				Print "[IOP] MMIO Access: 0x" & Hex(addr)
				Sleep
			Case &h1F900000 To &h1F900400
				Print "[IOP] Read SPU2 0x" & Hex(addr)
			Case &h1Fc00000 To &h1FFFFFFF
				memcpy(@value, @bus.bios(addr And &h1FFFFF), 1)
		End Select
	ElseIf addr >= &hFFFE00000 Then 
		Print "[IOP] Read8 Cache CTRL"
		Return &hFF
	EndIf
	Return value
End Function
Function iop_read16(addr As ULong) As UShort
	Dim value As Ushort
	If addr < &hC0000000 Then 
		addr And= &h1FFFFFFF
		Select Case addr 
			Case 0 To &h1FFFFF
				memcpy(@value, @bus.iop_ram(addr And &h1FFFFF), 2)
			Case &h1F800000 To &h1F810000
				Print "[IOP] MMIO Access: 0x" & Hex(addr)
				Sleep
			Case &h1F900000 To &h1F900400
				Print "[IOP] Read SPU2 0x" & Hex(addr)
			Case &h1Fc00000 To &h1FFFFFFF
				memcpy(@value, @bus.bios(addr And &h1FFFFF), 2)
		End Select
	ElseIf addr >= &hFFFE00000 Then 
		Print "[IOP] Read16 Cache CTRL"
		Return &hFFFF
	EndIf
	Return value
End Function
Function iop_read32(addr As ULong) As ULong
	Dim value As ULong
	If addr < &hC0000000 Then 
		addr And= &h1FFFFFFF
		Select Case addr 
			Case 0 To &h1FFFFF
				memcpy(@value, @bus.iop_ram(addr And &h1FFFFF), 4)
			Case &h1F800000 To &h1F810000
				Print "[IOP] MMIO Read Access: 0x" & Hex(addr)
				Sleep
			Case &h1F900000 To &h1F900400
				Print "[IOP] Read SPU2 0x" & Hex(addr)
			Case &h1Fc00000 To &h1FFFFFFF
				memcpy(@value, @bus.bios(addr And &h1FFFFF), 4)
		End Select
	ElseIf addr >= &hFFFE00000 Then 
		Print "[IOP] Read32 Cache CTRL"
		Return &hFFFFFFFF
	EndIf
	Return value
End Function
Sub iop_write8(value As UByte, addr As ULong)
	If addr < &hC0000000 Then 
		addr And= &h1FFFFFFF
		Select Case addr 
			Case 0 To &h1FFFFF
				memcpy(@bus.iop_ram(addr And &h1FFFFF), @value,  1)
				print "Write 8 0x" & hex(value)
				sleep
			Case &h1F800000 To &h1F810000
				Print "[IOP] MMIO Access: 0x" & Hex(addr)
			Case &h1F900000 To &h1F900400
				Print "[IOP] Read SPU2 0x" & Hex(addr)
			Case &h1Fc00000 To &h1FFFFFFF
		End Select
	ElseIf addr >= &hF0000000 then 
		Print "[IOP] Write Cache Control"
		Sleep
	EndIf
End Sub
Sub iop_write16(value As UShort, addr As ULong) 
	If addr < &hC0000000 Then 
		addr And= &h1FFFFFFF
		Select Case addr 
			Case 0 To &h1FFFFF
				memcpy(@bus.iop_ram(addr And &h1FFFFF), @value, 2)
				print "Write 16: 0x" & hex(value)
			Case &h1F800000 To &h1F810000
				Print "[IOP] MMIO Access: 0x" & Hex(addr)
			Case &h1F900000 To &h1F900400
				Print "[IOP] Read SPU2 0x" & Hex(addr)
			Case &h1Fc00000 To &h1FFFFFFF
		End Select
	ElseIf addr >= &hF0000000 then 
		Print "[IOP] Write Cache Control"
		sleep
	EndIf
End Sub
Sub iop_write32(value As ULong, addr As ULong) 
	If addr < &hC0000000 Then 
		addr And= &h1FFFFFFF
		Select Case addr 
			Case 0 To &h1FFFFF
				print "Write 32 0x" & hex(value) & " : " & Hex(addr)
				memcpy(@bus.iop_ram(addr And &h1FFFFF), @value, 4)
			Case &h1F800000 To &h1F810000
				Print "[IOP] MMIO Write Access: 0x" & Hex(addr)
			Case &h1F900000 To &h1F900400
				Print "[IOP] Write SPU2 0x" & Hex(addr)
			Case &h1Fc00000 To &h1FFFFFFF
				Print "[IOP] Write32 BIOS 0x" & Hex(addr)
		End Select
	ElseIf addr >= &hF0000000 then 
		Print "[IOP] Write Cache Control"
		Sleep
	EndIf
End Sub