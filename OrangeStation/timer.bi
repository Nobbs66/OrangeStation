Type ee_Timer
	TN_Count As ULong
	TN_Mode As ULong
	TN_Comp As ULong
	TN_Hold As ULong
	clocks As ULong
End Type
Dim Shared timers(4) As ee_Timer
Declare Sub updateTimers()
Declare Sub write32Timer(addr As ULong, value As ulong)
Sub write8Timer(addr As ULong, value As UByte)
	Print "writing timer: " & Hex(value)
	Select Case addr
		Case &h10000000
			timers(0).TN_Count = value 
		Case &h10000010
			timers(0).TN_Mode = value 
		Case &h10000020
			timers(0).TN_Comp = value 
		Case &h10000030
			timers(0).TN_Hold = value
		Case &h10000800
			timers(1).TN_Count = value 
		Case &h10000810
			timers(1).TN_Mode = value 	
		Case &h10000820
			timers(1).TN_Comp = value 
		Case &h10000830
			timers(1).TN_Hold = value
		Case &h10001000
			timers(2).TN_Count = value
		Case &h10001010
			timers(2).TN_Mode = value 	
		Case &h10001020
			timers(2).TN_Comp = value 
		Case &h10001800
			timers(3).TN_Count = value
		Case &h10001810
			timers(3).TN_Mode = value 
		Case &h10001820
			timers(3).TN_Comp = value 
	End Select 
End Sub
Sub write16Timer(addr As ULong, value As ushort)
	Print "writing timer: " & Hex(value)
	Select Case addr
		Case &h10000000
			timers(0).TN_Count = value 
		Case &h10000010
			timers(0).TN_Mode = value 
		Case &h10000020
			timers(0).TN_Comp = value 
		Case &h10000030
			timers(0).TN_Hold = value
		Case &h10000800
			timers(1).TN_Count = value 
		Case &h10000810
			timers(1).TN_Mode = value 	
		Case &h10000820
			timers(1).TN_Comp = value 
		Case &h10000830
			timers(1).TN_Hold = value
		Case &h10001000
			timers(2).TN_Count = value
		Case &h10001010
			timers(2).TN_Mode = value 	
		Case &h10001020
			timers(2).TN_Comp = value 
		Case &h10001800
			timers(3).TN_Count = value
		Case &h10001810
			timers(3).TN_Mode = value 
		Case &h10001820
			timers(3).TN_Comp = value 
	End Select 
End Sub
Sub write32Timer(addr As ULong, value As ULong)
	value And= &hFFFF
	Print "writing timer: " & Hex(value)
	Select Case addr
		Case &h10000000
			timers(0).TN_Count = value 
		Case &h10000010
			timers(0).TN_Mode = value 
		Case &h10000020
			timers(0).TN_Comp = value 
		Case &h10000030
			timers(0).TN_Hold = value
		Case &h10000800
			timers(1).TN_Count = value 
		Case &h10000810
			timers(1).TN_Mode = value 	
		Case &h10000820
			timers(1).TN_Comp = value 
		Case &h10000830
			timers(1).TN_Hold = value
		Case &h10001000
			timers(2).TN_Count = value
		Case &h10001010
			timers(2).TN_Mode = value 	
		Case &h10001020
			timers(2).TN_Comp = value 
		Case &h10001800
			timers(3).TN_Count = value
		Case &h10001810
			timers(3).TN_Mode = value 
		Case &h10001820
			timers(3).TN_Comp = value 
	End Select 
End Sub
Function read8Timer(addr As ULong) As UByte
	Dim value As UShort
	Select Case addr
		Case &h10000000
			value = timers(0).TN_Count And &HFF
		Case &h10000010
			value = timers(0).TN_Mode And &HFF
		Case &h10000020
			value = timers(0).TN_Comp And &HFF 
		Case &h10000030
			value = timers(0).TN_Hold And &HFF
		Case &h10000800
			value = timers(1).TN_Count And &HFF 
		Case &h10000810
			value = timers(1).TN_Mode And &HFF	
		Case &h10000820
			value = timers(1).TN_Comp And &HFF 
		Case &h10000830
			value = timers(1).TN_Hold And &HFF
		Case &h10001000
			value = timers(2).TN_Count And &HFF
		Case &h10001010
			value = timers(2).TN_Mode And &HFF	
		Case &h10001020
			value = timers(2).TN_Comp And &HFF 
		Case &h10001800
			value = timers(3).TN_Count And &HFF
		Case &h10001810
			value = timers(3).TN_Mode And &HFF
		Case &h10001820
			value = timers(3).TN_Comp And &HFF 
	End Select
	Return value
End Function
Function read16Timer(addr As ULong) As UShort
	Dim value As UShort
	Select Case addr
		Case &h10000000
			value = timers(0).TN_Count
		Case &h10000010
			value = timers(0).TN_Mode
		Case &h10000020
			value = timers(0).TN_Comp 
		Case &h10000030
			value = timers(0).TN_Hold
		Case &h10000800
			value = timers(1).TN_Count 
		Case &h10000810
			value = timers(1).TN_Mode	
		Case &h10000820
			value = timers(1).TN_Comp 
		Case &h10000830
			value = timers(1).TN_Hold
		Case &h10001000
			value = timers(2).TN_Count
		Case &h10001010
			value = timers(2).TN_Mode	
		Case &h10001020
			value = timers(2).TN_Comp 
		Case &h10001800
			value = timers(3).TN_Count
		Case &h10001810
			value = timers(3).TN_Mode
		Case &h10001820
			value = timers(3).TN_Comp 
	End Select
	Return value
End Function
Function read32Timer(addr As ULong) As ULong
	Dim value As ulong
	Select Case addr
		Case &h10000000
			value = timers(0).TN_Count
		Case &h10000010
			value = timers(0).TN_Mode
		Case &h10000020
			value = timers(0).TN_Comp 
		Case &h10000030
			value = timers(0).TN_Hold
		Case &h10000800
			value = timers(1).TN_Count 
		Case &h10000810
			value = timers(1).TN_Mode	
		Case &h10000820
			value = timers(1).TN_Comp 
		Case &h10000830
			value = timers(1).TN_Hold
		Case &h10001000
			value = timers(2).TN_Count
		Case &h10001010
			value = timers(2).TN_Mode	
		Case &h10001020
			value = timers(2).TN_Comp 
		Case &h10001800
			value = timers(3).TN_Count
		Case &h10001810
			value = timers(3).TN_Mode
		Case &h10001820
			value = timers(3).TN_Comp 
	End Select
	Return value
End Function
Sub tickTimer()
	For i As Integer = 0 To 3
		If timers(i).TN_Mode > 0  Then 
			timers(i).clocks += 1
			updateTimers()
		EndIf
	Next
End Sub
Sub updateTimers()
	For i As Integer = 0 To 3
		If timers(i).TN_Mode > 0 Then 
			Select Case timers(i).TN_Mode And 3
				Case 3 'HBLANK
					If timers(i).clocks = 9371 Then 
						timers(i).TN_Count += 1
						timers(i).clocks = 0 
						Print  #99, "Tn_Count " & timers(0).TN_Count
					EndIf
				Case Else 
					Print "Unsupported Timer Mode: " & (timers(i).TN_Mode And 3)
			End Select
		EndIf
	Next
End Sub
