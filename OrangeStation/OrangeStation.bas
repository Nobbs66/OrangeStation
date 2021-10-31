#Include "fbgfx.bi"
Using fb
#Include "crt.bi"
#Include "file.bi"
Open "tty.txt" For Output As #88
Open "log0.txt" For Output As #99


#Include "types.bi"

#Include Once "bus.bi"
#Include Once "cop0.bi"
#Include Once "ee.bi"



ScreenRes(640,900,32)
Declare Sub loadBIOS
Sub loadBIOS()
	Dim regs(0 To 31) As uint128
	If FileExists("bios\bios.bin") Then 
		Open "bios\bios.bin" For Binary As #1
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
	run_EE()

	Print "Exiting"
	cls
	Sleep(2000)
End Sub


main()