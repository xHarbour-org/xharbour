* $Id$
*
* Test program for legal character in variable name
*
* No kidding, xHarbour accept the following var names !
*
* To watch the show, use -DHB_COMMON_VARNAME_OFF when building harbour.exe
*
* Andi Jahja
*

   STATIC xa  := 1

   proc main()

   LOCAL this.is.a.test := 1
   LOCAL this\\\\\is\\\a.test := 1
   LOCAL aa := 1
   LOCAL aa := 1
   LOCAL aa := 1
   LOCAL aa := 1
   LOCAL aa := 1
   LOCAL aa := 1
   LOCAL aa := 1
   LOCAL aa := 1
   LOCAL aa := 1
   LOCAL aa := 1
   LOCAL aa := 1
   LOCAL aa := 1

   ? this.is.a.test
   ? this\\\\\is\\\a.test
   ? aa
   ? aa
   ? aa
   ? aa
   ? aa
   ? aa 
   ? aa 
   ? aa 
   ? aa 
   ? aa 
   ? aa 
   ? aa

   // etc. etc.
