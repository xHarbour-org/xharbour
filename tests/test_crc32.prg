PROCEDURE MAIN()
    Local cText := "xHarbour"
    Local nCrc

    nCrc := HB_CRC32(cText)
    ? "HB_CRC32 => " + FormatCrc(nCrc) + " Invert => "+ FormatCrc(nCrc, .t.)

    nCrc := HB_CRC_KERMIT(cText)
    ? "HB_CRC_KERMIT => " + FormatCrc(nCrc) + " Invert => "+ FormatCrc(nCrc, .t.)

    nCrc := HB_CRC_MCRF4XX(cText)
    ? "HB_CRC_MCRF4XX => " + FormatCrc(nCrc) + " Invert => "+ FormatCrc(nCrc, .t.)

    nCrc := HB_CCITT_FFFF(cText)
    ? "HB_CCITT_FFFF => " + FormatCrc(nCrc) + " Invert => "+ FormatCrc(nCrc, .t.)

    nCrc := HB_CRC_MODBUS(cText)
    ? "HB_CRC_MODBUS => " + FormatCrc(nCrc) + " Invert => "+ FormatCrc(nCrc, .t.)

    RETURN

Static Function FormatCrc(nCrc, lInvert)
    Local cCrc := NumToHex(nCrc)
    Local cCrc_Str
    Local nI

    lInvert = If(lInvert==nil, .f., lInvert)
    cCrc := PadL(NumToHex(nCrc), 4, "0")

    If lInvert
        cCrc_Str := HexToStr(cCrc)
        cCrc := ""
        For nI := Len(cCrc_Str) to 1 Step -1
            cCrc+=cCrc_Str[nI]
        Next
        cCrc := StrToHex(cCrc)
    EndIf

    Return cCrc
