/*
 * $Id$
 */

/* test program for CRC calculations in the algorithms:
*   - CRC32
*   - KERMIT
*   - MCRF4XX
*   - CCITT_FFFF
*   - MODBUS
*
* check the results at: https://crccalc.com/
* Eduardo Motta
*/

PROCEDURE MAIN()
    Local cText := "xHarbour"
    Local nCrc

    nCrc := HB_CRC32(cText)
    ? "HB_CRC32 => " + FormatCrc(nCrc) + " Invert => "+ FormatCrc(nCrc, .t.) // expected: HB_CRC32 => 132E2F34 Invert => 342F2E13

    nCrc := HB_CRC_KERMIT(cText)
    ? "HB_CRC_KERMIT => " + FormatCrc(nCrc) + " Invert => "+ FormatCrc(nCrc, .t.) // expected: HB_CRC_KERMIT => 3F0D Invert => 0D3F

    nCrc := HB_CRC_MCRF4XX(cText)
    ? "HB_CRC_MCRF4XX => " + FormatCrc(nCrc) + " Invert => "+ FormatCrc(nCrc, .t.) // expected: HB_CRC_MCRF4XX => 71B3 Invert => B371

    nCrc := HB_CCITT_FFFF(cText)
    ? "HB_CCITT_FFFF => " + FormatCrc(nCrc) + " Invert => "+ FormatCrc(nCrc, .t.) // expected: HB_CCITT_FFFF => 388D Invert => 8D38

    nCrc := HB_CRC_MODBUS(cText)
    ? "HB_CRC_MODBUS => " + FormatCrc(nCrc) + " Invert => "+ FormatCrc(nCrc, .t.) // expected: HB_CRC_MODBUS => E737 Invert => 37E7

    RETURN

Static Function FormatCrc(nCrc, lInvert)
    Local cCrc := NumToHex(nCrc)
    Local cCrc_Str
    Local nI

    lInvert = If(lInvert==nil, .f., lInvert)
    cCrc := NumToHex(nCrc)

    If lInvert
        cCrc_Str := HexToStr(cCrc)
        cCrc := ""
        For nI := Len(cCrc_Str) to 1 Step -1
            cCrc+=cCrc_Str[nI]
        Next
        cCrc := StrToHex(cCrc)
    EndIf

    Return cCrc
