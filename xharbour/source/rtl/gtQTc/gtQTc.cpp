/*
* $Id: gtQTc.cpp,v 1.5 2003/05/21 09:35:35 druzus Exp $
*/

/*
* Harbour Project source code:
* Video subsystem for QTLIB
*
* Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
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

/*
* See doc/license.txt for licensing terms.
*/

/*
*  This module is based on VIDMGR by Andrew Clarke and modified for
*  the Harbour project.
*/

/* NOTE: User programs should never call this layer directly! */

/* This definition has to be placed before #include "hbapigt.h" */
#define HB_GT_NAME	QTC

#include "hbset.h" /* For Ctrl+Break handling */
#include "hbvm.h" /* For Ctrl+Break handling */
#include "inkey.ch"
#include "gtQTc.h"
#include "qtconsole.h"
#include "qtconsoledoc.h"
#include "qtconsoleview.h"

/* the QT application will run in a different thread */
#include <pthread.h>
/*TODO: use my multiplatform threading lib */

#include <string.h>
#include <time.h>

#if defined(__DJGPP__)
	#include <pc.h>
	#include <sys\exceptn.h>
	#include <sys\farptr.h>
#elif defined(_MSC_VER)
	#include <signal.h>
#endif

/* For screen support */
#if defined(__POWERC) || (defined(__TURBOC__) && !defined(__BORLANDC__)) || (defined(__ZTC__) && !defined(__SC__))
	#define FAR far
#elif defined(HB_OS_DOS) && !defined(__DJGPP__) && !defined(__RSX32__) && !defined(__WATCOMC__)
	#define FAR _far
#else
	#define FAR
#endif

#if !defined(__DJGPP__)
	#ifndef MK_FP
		#define MK_FP( seg, off ) \
			((void FAR *)(((unsigned long)(seg) << 16)|(unsigned)(off)))
	#endif
#endif

#if defined(__WATCOMC__)
	#if defined(__386__)
		#define FAR
	#endif
	#include <signal.h>
#endif

static USHORT s_uiDispCount;

static int s_iStdIn, s_iStdOut, s_iStdErr;

/* Qt application parameters */
QTconsoleApp *qtcapp;
pthread_t th;
QApplication *qtapp;

void *start_qtapp( void *param )
{
	int argc = 1;
	char *argv[1];
	argv[0] = "DummyName";
	qtapp = new QApplication( argc, argv);

	qtapp->setFont(QFont("helvetica", 12));
	/* uncomment the following line, if you want a Windows 95 look*/
	// a.setStyle(WindowsStyle);

	qtcapp = new QTconsoleApp();
	qtapp->setMainWidget(qtcapp);

	qtcapp->show();

	return 0;
}

void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

	/*
	HB_SYMBOL_UNUSED( iFilenoStdin );
	HB_SYMBOL_UNUSED( iFilenoStdout );
	HB_SYMBOL_UNUSED( iFilenoStderr );
	*/

	/* stdin && stdout && stderr */
	s_iStdIn  = iFilenoStdin;
	s_iStdOut = iFilenoStdout;
	s_iStdErr = iFilenoStderr;

	s_uiDispCount = 0;

	/* Set the Ctrl+Break handler [vszakats] */


	/* */
	//HB_GT_FUNC(mouse_Init());

	/* Initialize qt application engine */
	//pthread_create( &th, 0, start_qtapp, 0 );
	start_qtapp( 0 );
}

void HB_GT_FUNC(gt_Exit( void ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

	HB_GT_FUNC(mouse_Exit());
	pthread_cancel( th );
}


int HB_GT_FUNC(gt_ExtendedKeySupport())
{
	return 1;
}

/*TODO*/
int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
   int ch = 0, ascii, state, key;
   HB_SYMBOL_UNUSED( eventmask );
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   if ( gtqt_keycount == 0 )
      return 0;

   /* TODO: CTRL-BREAK management */
   ch = gtqt_keycodes[ --gtqt_keycount ];
   ascii = ch & 0xff;
   state = ch & 0xff00;
   key = ch >> 16;

   ch = 0;
   if ( ( state & Qt::CTRL) != 0  )
   {
      switch( key ) {
      case Qt::Key_Up:  /*  Ctrl + Left arrow */
         ch = K_CTRL_UP;
         break;
      case Qt::Key_Down:  /* Ctrl + Right arrow */
         ch = K_CTRL_DOWN;
         break;
      case Qt::Key_Left:  /*  Ctrl + Left arrow */
         ch = K_CTRL_LEFT;
         break;
      case Qt::Key_Right:  /* Ctrl + Right arrow */
         ch = K_CTRL_RIGHT;
         break;
      case Qt::Key_Home:  /* Ctrl + Home */
         ch = K_CTRL_HOME;
         break;
      case Qt::Key_End:  /* Ctrl + End */
         ch = K_CTRL_END;
         break;
      case Qt::Key_PageUp:  /* Ctrl + Page Up */
         ch = K_CTRL_PGUP;
         break;
      case Qt::Key_PageDown:  /* Ctrl + Page Down */
         ch = K_CTRL_PGDN;
         break;
      case Qt::Key_Return:
         ch = K_CTRL_RET;
         break;
      case Qt::Key_Print:
         ch = K_CTRL_PRTSCR;
         break;
      case Qt::Key_Question:
         ch = K_CTRL_QUESTION;
         break;

      /* edit keys */
      case Qt::Key_Insert:
         ch = K_CTRL_INS;
         break;
      case Qt::Key_Delete:
         ch = K_CTRL_DEL;
         break;
      case Qt::Key_Backspace:
         ch = K_CTRL_BS;
         break;
      case Qt::Key_Tab:
         ch = K_CTRL_TAB;
         break;

      /* emulated cursors */
      case Qt::Key_E:
         ch = K_UP;
         break;
      case Qt::Key_X:
         ch = K_DOWN;
         break;
      case Qt::Key_S:
         ch = K_LEFT;
         break;
      case Qt::Key_D:
         ch = K_RIGHT;
         break;
      case Qt::Key_M:
         ch = K_RETURN;
         break;

      /* Keypad controls */
      case Qt::Key_5:
         if ( state & Qt::Keypad ) ch = KP_CTRL_5;
         break;
      case Qt::Key_Slash:
         if ( state & Qt::Keypad ) ch = KP_CTRL_SLASH;
         break;
      case Qt::Key_Asterisk:
         if ( state & Qt::Keypad ) ch = KP_CTRL_ASTERISK;
         break;
      case Qt::Key_Plus:
         if ( state & Qt::Keypad ) ch = KP_CTRL_PLUS;
         break;
      case Qt::Key_Minus:
         if ( state & Qt::Keypad ) ch = KP_CTRL_MINUS;
         break;

      /* functions */
      case Qt::Key_F1:
         ch = K_CTRL_F1;
         break;
      case Qt::Key_F2:
         ch = K_CTRL_F2;
         break;
      case Qt::Key_F3:
         ch = K_CTRL_F3;
         break;
      case Qt::Key_F4:
         ch = K_CTRL_F4;
         break;
      case Qt::Key_F5:
         ch = K_CTRL_F5;
         break;
      case Qt::Key_F6:
         ch = K_CTRL_F6;
         break;
      case Qt::Key_F7:
         ch = K_CTRL_F7;
         break;
      case Qt::Key_F8:
         ch = K_CTRL_F8;
         break;
      case Qt::Key_F9:
         ch = K_CTRL_F9;
         break;
      case Qt::Key_F10:
         ch = K_CTRL_F10;
         break;
      case Qt::Key_F11:
         ch = K_CTRL_F11;
         break;
      case Qt::Key_F12:
         ch = K_CTRL_F12;
         break;
      }
   }
   else if ( ( state & Qt::ALT) != 0 )
   {
      switch( key ) {
      case Qt::Key_Up:  /*  Ctrl + Left arrow */
         ch = K_ALT_UP;
         break;
      case Qt::Key_Down:  /* Ctrl + Right arrow */
         ch = K_ALT_DOWN;
         break;
      case Qt::Key_Left:  /*  ALT + Left arrow */
         ch = K_ALT_LEFT;
         break;
      case Qt::Key_Right:  /* ALT + Right arrow */
         ch = K_ALT_RIGHT;
         break;
      case Qt::Key_Home:  /* ALT + Home */
         ch = K_ALT_HOME;
         break;
      case Qt::Key_End:  /* ALT + End */
         ch = K_ALT_END;
         break;
      case Qt::Key_PageUp:  /* ALT + Page Up */
         ch = K_ALT_PGUP;
         break;
      case Qt::Key_PageDown:  /* ALT + Page Down */
         ch = K_ALT_PGDN;
         break;
      case Qt::Key_Return:
         ch = K_ALT_RETURN;
         break;
      case Qt::Key_Escape:
         ch = K_ALT_ESC;
         break;

      /* Keypad controls */
      case Qt::Key_5:
         if ( state & Qt::Keypad ) ch = KP_ALT_5;
         break;
      case Qt::Key_Slash:
         if ( state & Qt::Keypad ) ch = KP_ALT_SLASH;
         break;
      case Qt::Key_Asterisk:
         if ( state & Qt::Keypad ) ch = KP_ALT_ASTERISK;
         break;
      case Qt::Key_Plus:
         if ( state & Qt::Keypad ) ch = KP_ALT_PLUS;
         break;
      case Qt::Key_Minus:
         if ( state & Qt::Keypad ) ch = KP_ALT_MINUS;
         break;
      
      /* edit keys */
      case Qt::Key_Insert:
         ch = K_ALT_INS;
         break;
      case Qt::Key_Delete:
         ch = K_ALT_DEL;
         break;
      case Qt::Key_Backspace:
         ch = K_ALT_BS;
         break;
      case Qt::Key_Tab:
         ch = K_ALT_TAB;
         break;

      /* functions */
      case Qt::Key_F1:
         ch = K_ALT_F1;
         break;
      case Qt::Key_F2:
         ch = K_ALT_F2;
         break;
      case Qt::Key_F3:
         ch = K_ALT_F3;
         break;
      case Qt::Key_F4:
         ch = K_ALT_F4;
         break;
      case Qt::Key_F5:
         ch = K_ALT_F5;
         break;
      case Qt::Key_F6:
         ch = K_ALT_F6;
         break;
      case Qt::Key_F7:
         ch = K_ALT_F7;
         break;
      case Qt::Key_F8:
         ch = K_ALT_F8;
         break;
      case Qt::Key_F9:
         ch = K_ALT_F9;
         break;
      case Qt::Key_F10:
         ch = K_ALT_F10;
         break;
      case Qt::Key_F11:
         ch = K_ALT_F11;
         break;
      case Qt::Key_F12:
         ch = K_ALT_F12;
         break;
      }
   }
   else if ( ( state & Qt::SHIFT) != 0  )
   {
      switch ( key ) {

      /* edit keys */
      case Qt::Key_Tab:
         ch = K_SH_TAB;
         break;

      case Qt::Key_F1:
         ch = K_SH_F1;
         break;
      case Qt::Key_F2:
         ch = K_SH_F2;
         break;
      case Qt::Key_F3:
         ch = K_SH_F3;
         break;
      case Qt::Key_F4:
         ch = K_SH_F4;
         break;
      case Qt::Key_F5:
         ch = K_SH_F5;
         break;
      case Qt::Key_F6:
         ch = K_SH_F6;
         break;
      case Qt::Key_F7:
         ch = K_SH_F7;
         break;
      case Qt::Key_F8:
         ch = K_SH_F8;
         break;
      case Qt::Key_F9:
         ch = K_SH_F9;
         break;
      case Qt::Key_F10:
         ch = K_SH_F10;
         break;
      case Qt::Key_F11:
         ch = K_SH_F11;
         break;
      case Qt::Key_F12:
         ch = K_SH_F12;
         break;
      }
   }

   /* Now verifies functions and special keys */
   if ( ch == 0 )  /* IMPORTANT: shift may be ON! */
   {
      switch ( key ) {
      case Qt::Key_Up:
         ch = K_UP;
         break;
      case Qt::Key_Down:
         ch = K_DOWN;
         break;
      case Qt::Key_Left:
         ch = K_LEFT;
         break;
      case Qt::Key_Right:
         ch = K_RIGHT;
         break;
      case Qt::Key_Home:
         ch = K_HOME;
         break;
      case Qt::Key_End:
         ch = K_END;
         break;
      case Qt::Key_PageUp:
         ch = K_PGUP;
         break;
      case Qt::Key_PageDown:
         ch = K_PGDN;
         break;

      /* edit keys */
      case Qt::Key_Insert:
         ch = K_INS;
         break;
      case Qt::Key_Delete:
         ch = K_DEL;
         break;
      case Qt::Key_Backspace:
         ch = K_BS;
         break;
      case Qt::Key_Tab:
         ch = K_TAB;
         break;

      /* functions */
      case Qt::Key_F1:
         ch = K_F1;
         break;
      case Qt::Key_F2:
         ch = K_F2;
         break;
      case Qt::Key_F3:
         ch = K_F3;
         break;
      case Qt::Key_F4:
         ch = K_F4;
         break;
      case Qt::Key_F5:
         ch = K_F5;
         break;
      case Qt::Key_F6:
         ch = K_F6;
         break;
      case Qt::Key_F7:
         ch = K_F7;
         break;
      case Qt::Key_F8:
         ch = K_F8;
         break;
      case Qt::Key_F9:
         ch = K_F9;
         break;
      case Qt::Key_F10:
         ch = K_F10;
         break;
      case Qt::Key_F11:
         ch = K_F11;
         break;
      case Qt::Key_F12:
         ch = K_F12;
         break;
      }
   }

   if ( ch == 0 ) {
      ch = ascii;
   }

	return ch;
}

/*TODO*/
BOOL HB_GT_FUNC(gt_AdjustPos( BYTE * pStr, ULONG ulLen ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));
	return TRUE;
}

BOOL HB_GT_FUNC(gt_IsColor( void ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

	return true;
}

USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));
	return qtcapp->getDoc()->cols();
}

USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeigth()"));
	return qtcapp->getDoc()->rows();
}

void HB_GT_FUNC(gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod ))
{

	HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));
	HB_SYMBOL_UNUSED( iMethod );
	qtcapp->getDoc()->gotoXY( iCol, iRow );
}

/* TODO*/
static void HB_GT_FUNC(gt_SetCursorSize( char start, char end ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorSize(%d, %d)", (int) start, (int) end));
   if ( start > 14 || end > 14 )
      qtcapp->getDoc()->cursor( false );
   else
      qtcapp->getDoc()->cursor( true );
}

/* TODO*/
static void HB_GT_FUNC(gt_GetCursorSize( char * start, char *end ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorSize(%p, %p)", start, end));
   if ( qtcapp->getDoc()->cursor() ) {
      *start =6;
      *end = 7;
   }
   else {
      *start =32;
      *end = 32;
   }
}

USHORT HB_GT_FUNC(gt_GetCursorStyle( void ))
{
	char start, end;
	int rc;

	HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

	HB_GT_FUNC(gt_GetCursorSize( &start, &end ));

	if( ( start == 32 ) && ( end == 32 ) )
		rc = SC_NONE;

	else if( ( start == 6 ) && ( end == 7 ) )
		rc = SC_NORMAL;

	else if( ( start == 4 ) && ( end == 7 ) )
		rc = SC_INSERT;

	else if( ( start == 0 ) && ( end == 7 ) )
		rc = SC_SPECIAL1;

	else if( ( start == 0 ) && ( end == 3 ) )
		rc = SC_SPECIAL2;

	else
		rc = SC_NONE;

	return rc;
}

void HB_GT_FUNC(gt_SetCursorStyle( USHORT style ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", style));

	switch( style )
	{
	case SC_NONE:
		HB_GT_FUNC(gt_SetCursorSize( 32, 32 ));
		break;

	case SC_NORMAL:
		HB_GT_FUNC(gt_SetCursorSize( 6, 7 ));
		break;

	case SC_INSERT:
		HB_GT_FUNC(gt_SetCursorSize( 4, 7 ));
		break;

	case SC_SPECIAL1:
		HB_GT_FUNC(gt_SetCursorSize( 0, 7 ));
		break;

	case SC_SPECIAL2:
		HB_GT_FUNC(gt_SetCursorSize( 0, 3 ));
		break;

	default:
		break;
	}
}

static void HB_GT_FUNC(gt_xGetXY( USHORT cRow, USHORT cCol, BYTE * attr, BYTE * ch ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_xGetXY(%hu, %hu, %p, %p", cRow, cCol, ch, attr));
	qtcapp->getDoc()->readAttrib( cCol, cRow, (char *)attr );
	qtcapp->getDoc()->read( cCol, cRow, (char *)ch );
}

static void HB_GT_FUNC(gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %d", cRow, cCol, (int) attr, (int) ch));
	int oldatt = qtcapp->getDoc()->getAttrib();
	qtcapp->getDoc()->setAttrib( (char) attr );
	qtcapp->getDoc()->write( cCol,cRow, (char) ch );
	qtcapp->getDoc()->setAttrib( oldatt );
}

void HB_GT_FUNC(gt_Puts( USHORT cRow, USHORT cCol, BYTE attr, BYTE *str, ULONG len ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu", cRow, cCol, (int) attr, str, len));
	int oldatt = qtcapp->getDoc()->getAttrib();
	qtcapp->getDoc()->setAttrib( attr );
	qtcapp->getDoc()->write( cCol, cRow, (char *)str, len );
	qtcapp->getDoc()->setAttrib( oldatt );
}

int HB_GT_FUNC(gt_RectSize( USHORT rows, USHORT cols ))
{
	HB_SYMBOL_UNUSED( rows );
	HB_SYMBOL_UNUSED( cols );
	return qtcapp->getDoc()->rows() * qtcapp->getDoc()->cols() * 2;
}

void HB_GT_FUNC(gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * dest ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p", usTop, usLeft, usBottom, usRight, dest));
	qtcapp->getDoc()->getMem( usLeft, usTop, (char *)dest, usRight - usLeft +1, usBottom - usTop +1 );
}

void HB_GT_FUNC(gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * srce ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p", usTop, usLeft, usBottom, usRight, srce));
	qtcapp->getDoc()->setMem( usLeft, usTop, (char *)srce, usRight - usLeft +1, usBottom - usTop +1 );
}

void HB_GT_FUNC(gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr ))
{
	USHORT x, y;

	HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d", usTop, usLeft, usBottom, usRight, (int) attr));

	for( y = usTop; y <= usBottom; y++ )
	{
		for( x = usLeft; x <= usRight; x++ )
		{
			qtcapp->getDoc()->writeAttrib( x, y, (char)attr );
		}
	}
}

SHORT HB_GT_FUNC(gt_Col( void ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));
	return qtcapp->getDoc()->getCursX();
}

SHORT HB_GT_FUNC(gt_Row( void ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));
	return qtcapp->getDoc()->getCursY();
}

/* TODO: limit scrolling (now is full screen) */
void HB_GT_FUNC(gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr, SHORT sVert, SHORT sHoriz ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) attr, sVert, sHoriz));
	HB_SYMBOL_UNUSED( usLeft );
	HB_SYMBOL_UNUSED( usTop );
	HB_SYMBOL_UNUSED( usRight );
	HB_SYMBOL_UNUSED( usBottom );
	HB_SYMBOL_UNUSED( attr );
	HB_SYMBOL_UNUSED( sHoriz );
	char old_attr = qtcapp->getDoc()->getAttrib();

	qtcapp->getDoc()->setAttrib( (char) attr );
	qtcapp->getDoc()->scroll( (int) usTop, (int) usLeft, (int) usBottom,
      (int) usRight, (char) attr, (int)sVert, (int)sHoriz );
   qtcapp->getDoc()->setAttrib( old_attr );
}

void HB_GT_FUNC(gt_DispBegin( void ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));
	s_uiDispCount++;
	qtcapp->getDoc()->startChanging();
}

void HB_GT_FUNC(gt_DispEnd( void ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));
	qtcapp->getDoc()->endChanging();
	s_uiDispCount--;
}

/*TODO: implement this */
BOOL HB_GT_FUNC(gt_GetBlink())
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));
   return true;
}

/*TODO: implement this */
void HB_GT_FUNC(gt_SetBlink( BOOL bBlink ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));
	HB_SYMBOL_UNUSED( bBlink );

}

/* TODO: implement this */
void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));
	HB_SYMBOL_UNUSED( dFrequency );
	HB_SYMBOL_UNUSED( dDuration );
}

char * HB_GT_FUNC(gt_Version( void ))
{
	return "Harbour Terminal: QT lib (3.1) console";
}

USHORT HB_GT_FUNC(gt_DispCount())
{
	return s_uiDispCount;
}

void HB_GT_FUNC(gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG nLength ))
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, nLength));
	int col = qtcapp->getDoc()->getCursX();
	int row = qtcapp->getDoc()->getCursY();
	char attr = qtcapp->getDoc()->getAttrib();

	qtcapp->getDoc()->startChanging();
	qtcapp->getDoc()->setAttrib( (char) byAttr );
	qtcapp->getDoc()->gotoXY(uiCol, uiRow );
	for ( unsigned int i = 0; i < nLength; i ++ )
		qtcapp->getDoc()->write( (char) byChar );

	qtcapp->getDoc()->gotoXY( col, row );
	qtcapp->getDoc()->setAttrib( attr );
	qtcapp->getDoc()->endChanging();
}

USHORT HB_GT_FUNC(gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
						BYTE * szBox, BYTE byAttr ))
{
	USHORT ret = 1;
	SHORT Row;
	SHORT Col;
	SHORT Height;
	SHORT Width;

	qtcapp->getDoc()->startChanging();

	if( ( Left   >= 0 && Left   < HB_GT_FUNC(gt_GetScreenWidth())  )  ||
	    ( Right  >= 0 && Right  < HB_GT_FUNC(gt_GetScreenWidth())  )  ||
	    ( Top    >= 0 && Top    < HB_GT_FUNC(gt_GetScreenHeight()) )  ||
	    ( Bottom >= 0 && Bottom < HB_GT_FUNC(gt_GetScreenHeight()) ) )
	{

		/* Ensure that box is drawn from top left to bottom right. */
		if( Top > Bottom )
		{
			SHORT tmp = Top;
			Top = Bottom;
			Bottom = tmp;
		}
		if( Left > Right )
		{
			SHORT tmp = Left;
			Left = Right;
			Right = tmp;
		}

		/* Draw the box or line as specified */
		Height = Bottom - Top + 1;
		Width  = Right - Left + 1;

		HB_GT_FUNC(gt_DispBegin());

		if( Height > 1 && Width > 1 && Top >= 0 && Top < HB_GT_FUNC(gt_GetScreenHeight()) && Left >= 0 && Left < HB_GT_FUNC(gt_GetScreenWidth()) )
			HB_GT_FUNC(gt_xPutch( Top, Left, byAttr, szBox[ 0 ] )); /* Upper left corner */

		Col = ( Height > 1 ? Left + 1 : Left );
		if(Col < 0 )
		{
			Width += Col;
			Col = 0;
		}
		if( Right >= HB_GT_FUNC(gt_GetScreenWidth()) )
		{
			Width -= Right - HB_GT_FUNC(gt_GetScreenWidth());
		}

		if( Col <= Right && Col < HB_GT_FUNC(gt_GetScreenWidth()) && Top >= 0 && Top < HB_GT_FUNC(gt_GetScreenHeight()) )
			HB_GT_FUNC(gt_Replicate( Top, Col, byAttr, szBox[ 1 ], Width + ( (Right - Left) > 1 ? -2 : 0 ) )); /* Top line */

		if( Height > 1 && (Right - Left) > 1 && Right < HB_GT_FUNC(gt_GetScreenWidth()) && Top >= 0 && Top < HB_GT_FUNC(gt_GetScreenHeight()) )
			HB_GT_FUNC(gt_xPutch( Top, Right, byAttr, szBox[ 2 ] )); /* Upper right corner */

		if( szBox[ 8 ] && Height > 2 && Width > 2 )
		{
			for( Row = Top + 1; Row < Bottom; Row++ )
			{
				if( Row >= 0 && Row < HB_GT_FUNC(gt_GetScreenHeight()) )
				{
					Col = Left;
					if( Col < 0 )
						Col = 0; /* The width was corrected earlier. */
					else
						HB_GT_FUNC(gt_xPutch( Row, Col++, byAttr, szBox[ 7 ] )); /* Left side */
					HB_GT_FUNC(gt_Replicate( Row, Col, byAttr, szBox[ 8 ], Width - 2 )); /* Fill */
					if( Right < HB_GT_FUNC(gt_GetScreenWidth()) )
						HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
				}
			}
		}
		else
		{
			for( Row = ( Width > 1 ? Top + 1 : Top ); Row < ( (Right - Left ) > 1 ? Bottom : Bottom + 1 ); Row++ )
			{
				if( Row >= 0 && Row < HB_GT_FUNC(gt_GetScreenHeight()) )
				{
					if( Left >= 0 && Left < HB_GT_FUNC(gt_GetScreenWidth()) )
						HB_GT_FUNC(gt_xPutch( Row, Left, byAttr, szBox[ 7 ] )); /* Left side */
					if( ( Width > 1 || Left < 0 ) && Right < HB_GT_FUNC(gt_GetScreenWidth()) )
						HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
				}
			}
		}

		if( Height > 1 && Width > 1 )
		{
			if( Left >= 0 && Bottom < HB_GT_FUNC(gt_GetScreenHeight()) )
				HB_GT_FUNC(gt_xPutch( Bottom, Left, byAttr, szBox[ 6 ] )); /* Bottom left corner */

			Col = Left + 1;
			if( Col < 0 )
				Col = 0; /* The width was corrected earlier. */

			if( Col <= Right && Bottom < HB_GT_FUNC(gt_GetScreenHeight()) )
				HB_GT_FUNC(gt_Replicate( Bottom, Col, byAttr, szBox[ 5 ], Width - 2 )); /* Bottom line */

			if( Right < HB_GT_FUNC(gt_GetScreenWidth()) && Bottom < HB_GT_FUNC(gt_GetScreenHeight()) )
				HB_GT_FUNC(gt_xPutch( Bottom, Right, byAttr, szBox[ 4 ] )); /* Bottom right corner */
		}
		HB_GT_FUNC(gt_DispEnd());
		ret = 0;
	}

	qtcapp->getDoc()->endChanging();
	return ret;
}

USHORT HB_GT_FUNC(gt_BoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
	return HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr ));
}

USHORT HB_GT_FUNC(gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
	return HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr ));
}

USHORT HB_GT_FUNC(gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr ))
{
	USHORT ret = 1;
	if( Row >= 0 && Row < HB_GT_FUNC(gt_GetScreenHeight()) )
	{
		if( Left < 0 )
			Left = 0;
		else if( Left >= HB_GT_FUNC(gt_GetScreenWidth()) )
			Left = HB_GT_FUNC(gt_GetScreenWidth()) - 1;

		if( Right < 0 )
			Right = 0;
		else if( Right >= HB_GT_FUNC(gt_GetScreenWidth()) )
			Right = HB_GT_FUNC(gt_GetScreenWidth()) - 1;

		if( Left < Right )
			HB_GT_FUNC(gt_Replicate( Row, Left, byAttr, byChar, Right - Left + 1 ));
		else
			HB_GT_FUNC(gt_Replicate( Row, Right, byAttr, byChar, Left - Right + 1 ));
		ret = 0;
	}
	return ret;
}

USHORT HB_GT_FUNC(gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr ))
{
	USHORT ret = 1;
	SHORT Row;

	if( Col >= 0 && Col < HB_GT_FUNC(gt_GetScreenWidth()) )
	{
		if( Top < 0 )
			Top = 0;
		else if( Top >= HB_GT_FUNC(gt_GetScreenHeight()) )
			Top = HB_GT_FUNC(gt_GetScreenHeight()) - 1;

		if( Bottom < 0 )
			Bottom = 0;
		else if( Bottom >= HB_GT_FUNC(gt_GetScreenHeight()) )
			Bottom = HB_GT_FUNC(gt_GetScreenHeight()) - 1;

		if( Top <= Bottom )
			Row = Top;
		else
		{
			Row = Bottom;
			Bottom = Top;
		}
		qtcapp->getDoc()->startChanging();
		while( Row <= Bottom )
			HB_GT_FUNC(gt_xPutch( Row++, Col, byAttr, byChar ));
		qtcapp->getDoc()->endChanging();
		ret = 0;
	}
	return ret;
}


/***************************************************************************
* Return the display combination: monitor + video card
*
* INT 10 - VIDEO - GET DISPLAY COMBINATION CODE (PS,VGA/MCGA)
*         AX = 1A00h
* Return: AL = 1Ah if function was supported
*         BL = active display code (see below)
*         BH = alternate display code
*
* Values for display combination code:
*  00h    no display
*  01h    monochrome adapter w/ monochrome display
*  02h    CGA w/ color display
*  03h    reserved
*  04h    EGA w/ color display
*  05h    EGA w/ monochrome display
*  06h    PGA w/ color display
*  07h    VGA w/ monochrome analog display
*  08h    VGA w/ color analog display
*  09h    reserved
*  0Ah    MCGA w/ digital color display
*  0Bh    MCGA w/ monochrome analog display
*  0Ch    MCGA w/ color analog display
*  FFh    unknown display type
****************************************************************************/

/* TODO: Support this function */
static USHORT HB_GT_FUNC(gt_GetDisplay( void ))
{
	HB_TRACE( HB_TR_DEBUG, ("hb_gt_SetDisplay()") );
	return 0xff;
}

/* TODO: Support this mode */
BOOL HB_GT_FUNC(gt_SetMode( USHORT uiRows, USHORT uiCols ))
{
	HB_TRACE( HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", usRows, usCols) );
	HB_SYMBOL_UNUSED( uiRows );
	HB_SYMBOL_UNUSED( uiCols );
	return FALSE;
}

BOOL HB_GT_FUNC(gt_PreExt())
{
	return TRUE;
}

BOOL HB_GT_FUNC(gt_PostExt())
{
	return TRUE;
}

BOOL HB_GT_FUNC(gt_Suspend())
{
	return TRUE;
}

BOOL HB_GT_FUNC(gt_Resume())
{
	return TRUE;
}

void HB_GT_FUNC(gt_OutStd( BYTE * pbyStr, ULONG ulLen ))
{
	hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

void HB_GT_FUNC(gt_OutErr( BYTE * pbyStr, ULONG ulLen ))
{
	hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

extern "C" {

   HB_FUNC( HB_QTEVENTS )
   {
      qtapp->processEvents();
   }

   HB_FUNC( HB_QTAPPEXEC )
   {
      int iRet;
      HB_STACK_UNLOCK;
      iRet = qtapp->exec();
      HB_STACK_LOCK;
      hb_retni( iRet );
   }

   HB_FUNC( HB_QTAPPEXIT )
   {
      int iPar = hb_parni(1);
      qtapp->exit( iPar );
   }
}
