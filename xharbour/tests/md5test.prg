/*
  $Id$
  Test based on test suite (Appendix A of RFC 1321):
  MD5("") = d41d8cd98f00b204e9800998ecf8427e
  MD5("a") = 0cc175b9c0f1b6a831c399e269772661
  MD5("abc") = 900150983cd24fb0d6963f7d28e17f72
  MD5("message digest") = f96b697d7cb7938d525a2f31aaf161d0
  MD5("abcdefghijklmnopqrstuvwxyz") = c3fcd3d76192e4007dfb496cca67e13b
  MD5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") = d174ab98d277d9f5a5611c2c9f419d9f
  MD5("12345678901234567890123456789012345678901234567890123456789012345678901234567890") = 57edf4a22be3c955ac49da2e2107b67a
*/

Procedure main()
   If HB_MD5("")=="d41d8cd98f00b204e9800998ecf8427e"
      OutStd('OK: HB_MD5("")',hb_osnewline())
   Else
      OutStd('Failure: HB_MD5("")',hb_osnewline())
      OutStd('   Expected "d41d8cd98f00b204e9800998ecf8427e",',hb_osnewline())
      OutStd('   Received "'+HB_MD5("")+'".',hb_osnewline())
   EndIf
   If HB_MD5("a")=="0cc175b9c0f1b6a831c399e269772661"
      OutStd('OK: HB_MD5("a")',hb_osnewline())
   Else
      OutStd('Failure: HB_MD5("a")',hb_osnewline())
      OutStd('   Expected "0cc175b9c0f1b6a831c399e269772661",',hb_osnewline())
      OutStd('   Received "'+HB_MD5("a")+'".',hb_osnewline())
   EndIf
   If HB_MD5("abc")=="900150983cd24fb0d6963f7d28e17f72"
      OutStd('OK: HB_MD5("abc")',hb_osnewline())
   Else
      OutStd('Failure: HB_MD5("abc")',hb_osnewline())
      OutStd('   Expected "900150983cd24fb0d6963f7d28e17f72",',hb_osnewline())
      OutStd('   Received "'+HB_MD5("abc")+'".',hb_osnewline())
   EndIf
   If HB_MD5("message digest")=="f96b697d7cb7938d525a2f31aaf161d0"
      OutStd('OK: HB_MD5("message digest")',hb_osnewline())
   Else
      OutStd('Failure: HB_MD5("message digest")',hb_osnewline())
      OutStd('   Expected "f96b697d7cb7938d525a2f31aaf161d0",',hb_osnewline())
      OutStd('   Received "'+HB_MD5("message digest")+'".',hb_osnewline())
   EndIf
   If HB_MD5("abcdefghijklmnopqrstuvwxyz")=="c3fcd3d76192e4007dfb496cca67e13b"
      OutStd('OK: HB_MD5("abcdefghijklmnopqrstuvwxyz")',hb_osnewline())
   Else
      OutStd('Failure: HB_MD5("abcdefghijklmnopqrstuvwxyz")',hb_osnewline())
      OutStd('   Expected "c3fcd3d76192e4007dfb496cca67e13b",',hb_osnewline())
      OutStd('   Received "'+HB_MD5("abcdefghijklmnopqrstuvwxyz")+'".',hb_osnewline())
   EndIf
   If HB_MD5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")=="d174ab98d277d9f5a5611c2c9f419d9f"
      OutStd('OK: HB_MD5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")',hb_osnewline())
   Else
      OutStd('Failure: HB_MD5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")',hb_osnewline())
      OutStd('   Expected "d174ab98d277d9f5a5611c2c9f419d9f",',hb_osnewline())
      OutStd('   Received "'+HB_MD5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")+'".',hb_osnewline())
   EndIf
   If HB_MD5("12345678901234567890123456789012345678901234567890123456789012345678901234567890")=="57edf4a22be3c955ac49da2e2107b67a"
      OutStd('OK: HB_MD5("12345678901234567890123456789012345678901234567890123456789012345678901234567890")',hb_osnewline())
   Else
      OutStd('Failure: HB_MD5("12345678901234567890123456789012345678901234567890123456789012345678901234567890")',hb_osnewline())
      OutStd('   Expected "57edf4a22be3c955ac49da2e2107b67a",',hb_osnewline())
      OutStd('   Received "'+HB_MD5("12345678901234567890123456789012345678901234567890123456789012345678901234567890")+'".',hb_osnewline())
   EndIf
   Return

