/*
 * $Id: c_test.prg.c,v 1.1 2004/01/14 13:59:52 andijahja Exp $
 *
 * Test program for unicode support
 */

#Include "hbcc.ch"

Procedure main()
   Local aFile:="",hcs1,hcs2,c:="",pw,r,i,h,n:=0
   For i=0 To 255
      c+=Chr(i)
   Next
   h=FCreate("~1251.chr")
   FWrite(h,c)
   FClose(h)

   OutErr("Trying to register CS: 1 - ")
   hcs1:=hb_csreg("cp1251")
   If hb_csgeterror()==HB_CSERR_OK
      OutErr("OK. 2 - ")
   Else
      OutErr("Fail. 2 - ")
      n++
   EndIf
   hcs2:=hb_csreg("shiftjis")
   If hb_csgeterror()==HB_CSERR_OK
      OutErr("OK."+hb_osnewline())
   Else
      OutErr("Fail."+hb_osnewline())
      n++
   EndIf

   OutErr("Encoding attempts:"+hb_osnewline()+hb_osnewline())

   If hcs1!=HB_CSINVALID
      OutErr("Converting to UCS2 little Endian: ")
      r:=hb_cstocs(c,hcs1,HB_CSUCS2LE)
      If hb_csgeterror()==HB_CSERR_OK
         OutErr("OK."+hb_osnewline())
      Else
         OutErr("Fail."+hb_osnewline())
         n++
      EndIf
      r:=Chr(255)+Chr(254)+r
      h=FCreate("~ucs2le.chr")
      FWrite(h,r)
      FClose(h)

      OutErr("Converting to UCS2 big Endian: ")
      r:=hb_cstocs(c,hcs1,HB_CSUCS2LE)
      If hb_csgeterror()==HB_CSERR_OK
         OutErr("OK."+hb_osnewline())
      Else
         OutErr("Fail."+hb_osnewline())
         n++
      EndIf
      r:=Chr(254)+Chr(255)+r
      h=FCreate("~ucs2be.chr")
      FWrite(h,r)
      FClose(h)

      If hcs2!=HB_CSINVALID
         OutErr("Converting to shift-jis: ")
         r:=hb_cstocs(c,hcs1,hcs2)
         If hb_csgeterror()==HB_CSERR_OK
            OutErr("OK."+hb_osnewline())
         Else
            OutErr("Fail."+hb_osnewline())
            n++
         EndIf
         h=FCreate("~shjis.chr")
         FWrite(h,r)
         FClose(h)
      EndIf

      OutErr("Converting to UTF-8: ")
      r:=hb_cstocs(c,hcs1,HB_CSUTF8)
      If hb_csgeterror()==HB_CSERR_OK
         OutErr("OK."+hb_osnewline())
      Else
         OutErr("Fail."+hb_osnewline())
         n++
      EndIf
            h=FCreate("~utf8.chr")
      FWrite(h,Chr(239)+Chr(187)+Chr(191)+r)
      FClose(h)
      
      OutErr("Converting to UTF-7: ")
      r:=hb_cstocs(c,hcs1,HB_CSUTF7)
      If hb_csgeterror()==HB_CSERR_OK
         OutErr("OK."+hb_osnewline())
      Else
         OutErr("Fail."+hb_osnewline())
         n++
      EndIf
      h=FCreate("~utf7.chr")
      FWrite(h,r)
      FClose(h)
      
      OutErr("Comparing UTF-8 to UTF-7 transformation: ")
      If r==hb_cstocs(hb_cstocs(c,hcs1,HB_CSUTF8),HB_CSUTF8,HB_CSUTF7)
         OutErr("OK."+hb_osnewline())
      Else
         OutErr("Fail."+hb_osnewline())
         n++
      EndIf
   EndIf
   
   OutErr("Testing Base64 Encode: ")
   h=FCreate("~1251.b64")
   r:=hb_b64encode(c)
   FWrite(h,"Content-Type: text/plain; charset=US-ASCII; name=~1251.chr"+hb_osnewline())
   FWrite(h,"Content-transfer-encoding: base64"+hb_osnewline()+hb_osnewline())
   FWrite(h,r+hb_osnewline())
   FClose(h)
   If c==hb_b64decode(r)
      OutErr("OK."+hb_osnewline())
   Else
      OutErr("Fail."+hb_osnewline())
      n++
      h=FCreate("~1251b64.chr")
      FWrite(h,hb_b64decode(r))
      FClose(h)
   EndIf
   
   OutErr("Testing QP Encode: ")
   h=FCreate("~1251.qp")
   r:=hb_qpencode(c)
   FWrite(h,"Content-Type: text/plain; charset=US-ASCII; name=~1251.chr"+hb_osnewline())
   FWrite(h,"Content-transfer-encoding: Quoted-Printable"+hb_osnewline()+hb_osnewline())
   FWrite(h,r)
   FClose(h)
   If c==hb_qpdecode(r)
      OutErr("OK."+hb_osnewline())
   Else
      OutErr("Fail."+hb_osnewline())
      n++
      h=FCreate("~1251qp.chr")
      FWrite(h,hb_qpdecode(r))
      FClose(h)
   EndIf
   
   OutErr("Testing URLEncode: ")
   r:=hb_urlencode(c)
   h:=FCreate("~1251.ue")
   FWrite(h,r)
   FClose(h)
   If c==hb_urldecode(r)
      OutErr("OK."+hb_osnewline())
   Else
      OutErr("Fail."+hb_osnewline())
      n++
      h=FCreate("~1251ue.chr")
      FWrite(h,hb_urldecode(r))
      FClose(h)
   EndIf
   
   OutErr("Testing UUEncode: ")
   r=hb_uuencode(c)
   h=FCreate("~1251.uue")
   FWrite(h,"begin 644 ~1251.chr"+hb_osnewline()+r+"end")
   If c==hb_uudecode(r)
      OutErr("OK."+hb_osnewline())
   Else
      OutErr("Fail."+hb_osnewline())
      n++
      h=FCreate("~1251uu.chr")
      FWrite(h,hb_uudecode(r))
      FClose(h)
   EndIf
   
   OutErr("Testing XXEncode: ")
   r=hb_xxencode(c)
   h=FCreate("~1251.xxe")
   FWrite(h,"begin 644 ~1251.chr"+hb_osnewline()+r+"end")
   If c==hb_xxdecode(r)
      OutErr("OK."+hb_osnewline())
   Else
      OutErr("Fail."+hb_osnewline())
      n++
      h=FCreate("~1251xx.chr")
      FWrite(h,hb_xxdecode(r))
      FClose(h)
   EndIf
   
   OutErr("Testing YYEncode: ")
   r=hb_yyencode(c)
   h=FCreate("~1251.yye")
   FWrite(h,"=ybegin line=128 size="+AllTrim(Str(Len(c)))+" name=~1251.chr"+hb_osnewline()+r+hb_osnewline()+"=yend size="+AllTrim(Str(Len(c)))+" crc32="+hb_nltohx(hb_ncrc32(c))+hb_osnewline())
   If c==hb_yydecode(r)
      OutErr("OK."+hb_osnewline())
   Else
      OutErr("Fail."+hb_osnewline())
      n++
      h=FCreate("~1251yy.chr")
      FWrite(h,hb_yydecode(r))
      FClose(h)
   EndIf
   OutErr("Testing 32bit encryption: ")
   pw:="Some encryption string"+hb_osnewline()+"of any size and chars"
   r=hb_encrypt32(c,pw)
   h=FCreate("~1251.enc")
   FWrite(h,r)
   FClose(h)
   If c==hb_decrypt32(r,pw)
      OutErr("OK."+hb_osnewline())
   Else
      OutErr("Fail."+hb_osnewline())
      n++
      h=FCreate("~1251enc.chr")
      FWrite(h,hb_decrypt32(r,pw))
      FClose(h)
   EndIf

   OutErr("Testing 128bit encryption: ")
   mkcryptkey(pw)
   r=encrypt128(c)
   h=FCreate("~~1251.enc")
   FWrite(h,r)
   FClose(h)
   If c==decrypt128(r)
      OutErr("OK."+hb_osnewline())
   Else
      OutErr("Fail."+hb_osnewline())
      n++
      h=FCreate("~~1251enc.chr")
      FWrite(h,decrypt128(r))
      FClose(h)
   EndIf
   
   OutErr(Str(n,2)+" Errors encountered. Bye."+hb_osnewline())
   OutErr(hb_osnewline()+"Decoding attempts:"+hb_osnewline()+hb_osnewline())
/*
   If hcs1!=HB_CSINVALID
      OutErr("Converting from UCS2 little Endian: ")
      r:=hb_cstocs(c,hcs1,HB_CSUCS2LE)
      If hb_csgeterror()==HB_CSERR_OK
         OutErr("OK."+hb_osnewline())
      Else
         OutErr("Fail."+hb_osnewline())
         n++
      EndIf
      r:=Chr(255)+Chr(254)+r
      h=FCreate("~ucs2le.chr")
      FWrite(h,r)
      FClose(h)

*/
   Return

