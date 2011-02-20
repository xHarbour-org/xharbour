/*
 *  $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Miscellaneous functions: - KEYTIME()
 *
 * Copyright 2005 Pavel Tsarenko - <tpe2@mail.ru>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

Static pHandle, cTime, nsKey
Static nHour, nMin, nSec, nLast


Function KeyTime(nKey, cClockTime)
Local lActivated := .f.

if pHandle != nil
   HB_IdleDel(pHandle)
   pHandle := nil
endif

if nKey != nil .and. ValType(cClockTime) = 'C'

   nsKey := nKey
   cTime := cClockTime
   nHour := Val(Substr(cClockTime, 1, 2))
   nMin  := Val(Substr(cClockTime, 4, 2))
   nSec  := Val(Substr(cClockTime, 7, 2))
   nLast := -1
   pHandle := HB_IdleAdd({|| doKeyTime()})
   lActivated := .t.

endif
Return lActivated

Static function doKeyTime()
Local ccTime := Time()
Local nHr := Val(Substr(ccTime, 1, 2))
Local nMn := Val(Substr(ccTime, 4, 2))
Local nSc := Val(Substr(ccTime, 7, 2))

if nHour = 99
   if nHr > nLast
      __Keyboard(nsKey)
      nLast := nHr
      if nHr == 23
         HB_IdleDel(pHandle)
         pHandle := nil
      endif
   endif
elseif nMin = 99 .and. nHr == nHour
   if nMn > nLast
      __Keyboard(nsKey)
      nLast := nMn
      if nMn == 59
         HB_IdleDel(pHandle)
         pHandle := nil
      endif
   endif
elseif nSec = 99 .and. nHr == nHour .and. nMn == nMin
   if nSc > nLast
      __Keyboard(nsKey)
      nLast := nSc
      if nSc == 59
         HB_IdleDel(pHandle)
         pHandle := nil
      endif
   endif
elseif ccTime > cTime

   __Keyboard(nsKey)
   HB_IdleDel(pHandle)
   pHandle := nil

endif
Return nil
