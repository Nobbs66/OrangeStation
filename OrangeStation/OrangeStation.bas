#Include "fbgfx.bi"
Using fb
#Include "crt.bi"
#Include "file.bi"
Open "tty.txt" For Output As #88
Open "log0.txt" For Output As #99

Dim Shared stepping As UByte
#Include "types.bi"

#Include Once "bus.bi"
#Include Once "cop0.bi"
#Include Once "ee.bi"


#Include Once "iop_cop0.bi"
#Include Once "iop.bi"


ScreenRes(640,1100,32)
Declare Sub loadBIOS
Sub loadBIOS()
	Dim regs(0 To 31) As uint128
	If FileExists("bios\bios.bin") Then 
	Open "bios\slimj.bin" For Binary As #1
	Else 
		Print "Please provide a valid bios rom"
	Sleep
	EndIf
	Get #1,, bus.bios()
	close #1
End Sub
Sub waitForStart()
	Do
		Cls 
		Print "press s"
		Sleep 1000	
	Loop Until MultiKey(SC_S)
End Sub
Sub main()
	waitForStart()
	loadBIOS()
	init_EE()
	init_IOP()
	Do
		run_EE(8)
		run_IOP(1)
	Loop Until MultiKey(SC_ESCAPE)
	Cls
	Print "Exiting"
	Sleep 2000
End Sub

main()