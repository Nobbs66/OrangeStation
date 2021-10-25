Declare Sub fetchOP()
Type cpus
	Dim PC As UInteger
	Dim regs As UInteger
	Dim HI As ULongInt
	Dim LO As ULongInt
	Dim HI1 As ULongInt
	Dim LO1 As ULongInt
	Dim Opcode As UInteger
	Declare Sub fetchOP()
End Type
Dim Shared cpu As cpus
#Include "ee_instructions.bi"
Sub fetchOp()
	 cpu.Opcode = read32(cpu.PC)
	 Print Hex(cpu.Opcode)
End Sub
Sub init_EE()
	cpu.PC = &hBFC00000
End Sub
Sub run_EE()
	Do
		fetchOp()
		decodeOp()
		cpu.PC += 4
		Sleep
		
	Loop While Not MultiKey(SC_ESCAPE)
	fetchOp()
	decodeOp()
	Sleep(10000)
End Sub
