Function sprintf(...)
Local aPar, cStr, nPar, nPos, cTok
Local nLen := 0, lUns, l0 := .F., lSign := .F., nDec

    aPar := HB_aParams()
    cStr := ""
    nPar := 2
    While !Empty(aPar[1])
	nPos := Len(aPar[1]) + 1
	cTok := Nil
	If '%' $ aPar[1]
	    nPos := At('%', aPar[1])
	    cTok := '%'
	End
	If '\' $ aPar[1] .And. At('\', aPar[1]) < nPos
	    nPos := At('\', aPar[1])
	    cTok := '\'
	End
	
	cStr += Left(aPar[1], nPos - 1)

	Do Case
    	    Case cTok == Nil
		Exit
	    Case cTok == '\'
		Switch aPar[1, ++nPos]
		    Case 't'
			cStr += '    '
			Exit
		    Case 'n'
			cStr += HB_OSNewLine()
			Exit
		    Default
			cStr += aPar[1, nPos]
			Exit
		End
	    Case cTok == '%'
		lUns := .F.
		Switch aPar[1, ++nPos]
		    Case '%'
			cStr += '%'
			Exit
		    Case '+'
			aPar[1] := Left(aPar[1], nPos - 1) + SubStr(aPar[1], nPos + 1)
			nPos := At('%', aPar[1]) - 1
			lSign := .T.
			Exit
		    Case '0'
			aPar[1] := Left(aPar[1], nPos - 1) + SubStr(aPar[1], nPos + 1)
			nPos := At('%', aPar[1]) - 1
			l0 := .T.
			Exit
		    Case '1'
		    Case '2'
		    Case '3'
		    Case '4'
		    Case '5'
		    Case '6'
		    Case '7'
		    Case '8'
		    Case '9'
			nLen := Val(SubStr(aPar[1], nPos))
			cTok := Left(aPar[1], nPos - 1)
			While aPar[1, nPos] $ '1234567890.'
			    nPos++
			End
			aPar[1] := cTok + SubStr(aPar[1], nPos)
			nPos := At('%', aPar[1]) - 1
			Exit
		    Case 'u'
			lUns := .T.
			nPos++
		    Case 'd'
		    Case 'l'
		    Case 'f'
			If nLen != 0
			    If nLen - Int(nLen) > 0.0
				nDec := Str(nLen)
				While Right(nDec, 1) == '0'
				  nDec := Left(nDec, Len(nDec) -1)
				End
				nDec := Val(SubStr(nDec, At('.', nDec) + 1))
			    Else
				nDec := 0
			    End
			    cTok := Str(IIf(lUns, Abs(aPar[nPar++]), aPar[nPar++]), nLen, nDec)
			Else
			    cTok += LTrim(Str(IIf(lUns, Abs(aPar[nPar++]), aPar[nPar++])))
			End
			If l0
			    If '-' $ cTok
				cTok:= StrTran(cTok, '-', ' ')
				cTok[1] := '-'
			    End
			    cTok := StrTran(cTok, ' ', '0')
			    l0 := .F.
			End
			If lSign .And. cTok[1] != '-'
			    If nLen == 0
				cTok:= '+' + cTok
			    Else
				cTok[1] := '+'
			    End
			    lSign := .F.
			End
			nLen := 0
			cStr += cTok
			Exit
		    Case 'c'
		    Case 's'
			If nLen == 0
			    nLen := Len(aPar[nPar])
			End
			cStr += PadL(aPar[nPar++], nLen)
			nLen := 0
			l0 := .F.
			lSign := .F.
			Exit
		End
	End

	aPar[1] := SubStr(aPar[1], nPos + 1)
    End
Return cStr
