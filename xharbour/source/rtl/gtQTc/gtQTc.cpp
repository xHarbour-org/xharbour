/*
* $Id$
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
* The following parts are Copyright of the individual authors.
* www - http://www.harbour-project.org
*
* Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
*    hb_gt_CtrlBrkHandler()
*    hb_gt_CtrlBrkRestore()
*
* Copyright 1999 David G. Holm <dholm@jsd-llc.com>
*    hb_gt_ReadKey()
*
* See doc/license.txt for licensing terms.
*
*/

/*
*  This module is based on VIDMGR by Andrew Clarke and modified for
*  the Harbour project.
*/

/* NOTE: User programs should never call this layer directly! */

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
#if !defined(__DJGPP__)
	static char FAR * scrnPtr;
	static char FAR * scrnStealth = NULL;
	static char FAR * hb_gt_ScreenAddress( void );
	static int    scrnVirtual = FALSE;
	static USHORT scrnWidth = 0;
	static USHORT scrnHeight = 0;
	static SHORT  scrnPosRow = -1;
	static SHORT  scrnPosCol = -1;
#else
	static char * scrnPtr = NULL;
	static int    scrnVirtual = FALSE;
	static USHORT scrnWidth = 0;
	static USHORT scrnHeight = 0;
	static SHORT  scrnPosRow = -1;
	static SHORT  scrnPosCol = -1;
#endif

static BOOL s_bBreak; /* Used to signal Ctrl+Break to hb_inkeyPoll() */
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
	//qtapp->exec();

	return 0;
}

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
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
	//hb_mouse_Init();

	/* Initialize qt application engine */
	//pthread_create( &th, 0, start_qtapp, 0 );
	start_qtapp( 0 );
}

void hb_gt_Exit( void )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

	hb_mouse_Exit();
#if !defined(__DJGPP__)
if( scrnStealth != ( char * ) -1 )
	hb_xfree( scrnStealth );
#endif
	pthread_cancel( th );
}


int hb_gt_ExtendedKeySupport()
{
	return 0;
}

/*TODO*/
int hb_gt_ReadKey( HB_inkey_enum eventmask )
{
	int ch = -1;
	HB_SYMBOL_UNUSED( eventmask );
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

	return ch;
}

/*TODO*/
BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));
	return TRUE;
}

BOOL hb_gt_IsColor( void )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

	return true;
}

USHORT hb_gt_GetScreenWidth( void )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));
	return qtcapp->getDoc()->cols();
}

USHORT hb_gt_GetScreenHeight( void )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeigth()"));
	return qtcapp->getDoc()->rows();
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod )
{

	HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));
	HB_SYMBOL_UNUSED( iMethod );
	qtcapp->getDoc()->gotoXY( iCol, iRow );
	//qtapp->processEvents();
}

/* TODO*/
static void hb_gt_SetCursorSize( char start, char end )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorSize(%d, %d)", (int) start, (int) end));
}

/* TODO*/
static void hb_gt_GetCursorSize( char * start, char *end )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorSize(%p, %p)", start, end));
	*start =60;
	*end = 7;
}

USHORT hb_gt_GetCursorStyle( void )
{
	char start, end;
	int rc;

	HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

	hb_gt_GetCursorSize( &start, &end );

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

void hb_gt_SetCursorStyle( USHORT style )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", style));

	switch( style )
	{
	case SC_NONE:
		hb_gt_SetCursorSize( 32, 32 );
		break;

	case SC_NORMAL:
		hb_gt_SetCursorSize( 6, 7 );
		break;

	case SC_INSERT:
		hb_gt_SetCursorSize( 4, 7 );
		break;

	case SC_SPECIAL1:
		hb_gt_SetCursorSize( 0, 7 );
		break;

	case SC_SPECIAL2:
		hb_gt_SetCursorSize( 0, 3 );
		break;

	default:
		break;
	}
}

static void hb_gt_xGetXY( USHORT cRow, USHORT cCol, BYTE * attr, BYTE * ch )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_xGetXY(%hu, %hu, %p, %p", cRow, cCol, ch, attr));
	qtcapp->getDoc()->readAttrib( cCol, cRow, (char *)attr );
	qtcapp->getDoc()->read( cCol, cRow, (char *)ch );
}

static void hb_gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %d", cRow, cCol, (int) attr, (int) ch));
	int oldatt = qtcapp->getDoc()->getAttrib();
	//printf("Attrib: %x\n", attr );
	qtcapp->getDoc()->setAttrib( (char) attr );
	qtcapp->getDoc()->write( cCol,cRow, (char) ch );
	qtcapp->getDoc()->setAttrib( oldatt );
	//qtapp->processEvents();
}

void hb_gt_Puts( USHORT cRow, USHORT cCol, BYTE attr, BYTE *str, ULONG len )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu", cRow, cCol, (int) attr, str, len));
	int oldatt = qtcapp->getDoc()->getAttrib();
	qtcapp->getDoc()->setAttrib( attr );
	qtcapp->getDoc()->write( cCol, cRow, (char *)str, len );
	qtcapp->getDoc()->setAttrib( oldatt );
	//qtapp->processEvents();

}

int hb_gt_RectSize( USHORT rows, USHORT cols )
{
	HB_SYMBOL_UNUSED( rows );
	HB_SYMBOL_UNUSED( cols );
	return qtcapp->getDoc()->rows() * qtcapp->getDoc()->cols() * 2;
}

void hb_gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * dest )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p", usTop, usLeft, usBottom, usRight, dest));
	qtcapp->getDoc()->getMem( usTop, usLeft, (char *)dest, usBottom - usTop , usRight - usLeft );
}

void hb_gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * srce )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p", usTop, usLeft, usBottom, usRight, srce));
	qtcapp->getDoc()->setMem( usTop, usLeft, (char *)srce, usBottom - usTop , usRight - usLeft );
	//qtapp->processEvents();

}

void hb_gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr )
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

SHORT hb_gt_Col( void )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));
	return qtcapp->getDoc()->getCursX();
}

SHORT hb_gt_Row( void )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));
	return qtcapp->getDoc()->getCursY();
}

/* TODO: limit scrolling (now is full screen) */
void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr, SHORT sVert, SHORT sHoriz )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) attr, sVert, sHoriz));
	HB_SYMBOL_UNUSED( usLeft );
	HB_SYMBOL_UNUSED( usTop );
	HB_SYMBOL_UNUSED( usRight );
	HB_SYMBOL_UNUSED( usBottom );
	HB_SYMBOL_UNUSED( attr );
	HB_SYMBOL_UNUSED( sHoriz );

	qtcapp->getDoc()->scroll( sVert );
}

void hb_gt_DispBegin( void )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));
	s_uiDispCount++;
	qtcapp->getDoc()->startChanging();
}

void hb_gt_DispEnd( void )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));
	qtcapp->getDoc()->endChanging();
	s_uiDispCount--;
}

/*TODO: implement this */
BOOL hb_gt_GetBlink()
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));
}

/*TODO: implement this */
void hb_gt_SetBlink( BOOL bBlink )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));
	HB_SYMBOL_UNUSED( bBlink );

}

/* TODO: implement this */
void hb_gt_Tone( double dFrequency, double dDuration )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));
	HB_SYMBOL_UNUSED( dFrequency );
	HB_SYMBOL_UNUSED( dDuration );
}

char * hb_gt_Version( void )
{
	return "Harbour Terminal: QT lib (3.1) console";
}

USHORT hb_gt_DispCount()
{
	return s_uiDispCount;
}

void hb_gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG nLength )
{
	HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, nLength));
	int col = qtcapp->getDoc()->getCursX();
	int row = qtcapp->getDoc()->getCursY();
	char attr = qtcapp->getDoc()->getAttrib();

	qtcapp->getDoc()->startChanging();
	qtcapp->getDoc()->setAttrib( (char) byAttr );
	qtcapp->getDoc()->gotoXY(uiCol, uiRow );
	for ( int i = 0; i < nLength; i ++ )
		qtcapp->getDoc()->write( (char) byChar );

	qtcapp->getDoc()->gotoXY( col, row );
	qtcapp->getDoc()->setAttrib( attr );
	qtcapp->getDoc()->endChanging();
}

USHORT hb_gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
						BYTE * szBox, BYTE byAttr )
{
	USHORT ret = 1;
	SHORT Row;
	SHORT Col;
	SHORT Height;
	SHORT Width;

	qtcapp->getDoc()->startChanging();

	if( ( Left   >= 0 && Left   < hb_gt_GetScreenWidth()  )  ||
		( Right  >= 0 && Right  < hb_gt_GetScreenWidth()  )  ||
		( Top    >= 0 && Top    < hb_gt_GetScreenHeight() )  ||
		( Bottom >= 0 && Bottom < hb_gt_GetScreenHeight() ) )
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

		hb_gt_DispBegin();

		if( Height > 1 && Width > 1 && Top >= 0 && Top < hb_gt_GetScreenHeight() && Left >= 0 && Left < hb_gt_GetScreenWidth() )
			hb_gt_xPutch( Top, Left, byAttr, szBox[ 0 ] ); /* Upper left corner */

		Col = ( Height > 1 ? Left + 1 : Left );
		if(Col < 0 )
		{
			Width += Col;
			Col = 0;
		}
		if( Right >= hb_gt_GetScreenWidth() )
		{
			Width -= Right - hb_gt_GetScreenWidth();
		}

		if( Col <= Right && Col < hb_gt_GetScreenWidth() && Top >= 0 && Top < hb_gt_GetScreenHeight() )
			hb_gt_Replicate( Top, Col, byAttr, szBox[ 1 ], Width + ( (Right - Left) > 1 ? -2 : 0 ) ); /* Top line */

		if( Height > 1 && (Right - Left) > 1 && Right < hb_gt_GetScreenWidth() && Top >= 0 && Top < hb_gt_GetScreenHeight() )
			hb_gt_xPutch( Top, Right, byAttr, szBox[ 2 ] ); /* Upper right corner */

		if( szBox[ 8 ] && Height > 2 && Width > 2 )
		{
			for( Row = Top + 1; Row < Bottom; Row++ )
			{
				if( Row >= 0 && Row < hb_gt_GetScreenHeight() )
				{
					Col = Left;
					if( Col < 0 )
						Col = 0; /* The width was corrected earlier. */
					else
						hb_gt_xPutch( Row, Col++, byAttr, szBox[ 7 ] ); /* Left side */
					hb_gt_Replicate( Row, Col, byAttr, szBox[ 8 ], Width - 2 ); /* Fill */
					if( Right < hb_gt_GetScreenWidth() )
						hb_gt_xPutch( Row, Right, byAttr, szBox[ 3 ] ); /* Right side */
				}
			}
		}
		else
		{
			for( Row = ( Width > 1 ? Top + 1 : Top ); Row < ( (Right - Left ) > 1 ? Bottom : Bottom + 1 ); Row++ )
			{
				if( Row >= 0 && Row < hb_gt_GetScreenHeight() )
				{
					if( Left >= 0 && Left < hb_gt_GetScreenWidth() )
						hb_gt_xPutch( Row, Left, byAttr, szBox[ 7 ] ); /* Left side */
					if( ( Width > 1 || Left < 0 ) && Right < hb_gt_GetScreenWidth() )
						hb_gt_xPutch( Row, Right, byAttr, szBox[ 3 ] ); /* Right side */
				}
			}
		}

		if( Height > 1 && Width > 1 )
		{
			if( Left >= 0 && Bottom < hb_gt_GetScreenHeight() )
				hb_gt_xPutch( Bottom, Left, byAttr, szBox[ 6 ] ); /* Bottom left corner */

			Col = Left + 1;
			if( Col < 0 )
				Col = 0; /* The width was corrected earlier. */

			if( Col <= Right && Bottom < hb_gt_GetScreenHeight() )
				hb_gt_Replicate( Bottom, Col, byAttr, szBox[ 5 ], Width - 2 ); /* Bottom line */

			if( Right < hb_gt_GetScreenWidth() && Bottom < hb_gt_GetScreenHeight() )
				hb_gt_xPutch( Bottom, Right, byAttr, szBox[ 4 ] ); /* Bottom right corner */
		}
		hb_gt_DispEnd();
		ret = 0;
	}

	qtcapp->getDoc()->endChanging();
	return ret;
}

USHORT hb_gt_BoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr )
{
	return hb_gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr );
}

USHORT hb_gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr )
{
	return hb_gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr );
}

USHORT hb_gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr )
{
	USHORT ret = 1;
	if( Row >= 0 && Row < hb_gt_GetScreenHeight() )
	{
		if( Left < 0 )
			Left = 0;
		else if( Left >= hb_gt_GetScreenWidth() )
			Left = hb_gt_GetScreenWidth() - 1;

		if( Right < 0 )
			Right = 0;
		else if( Right >= hb_gt_GetScreenWidth() )
			Right = hb_gt_GetScreenWidth() - 1;

		if( Left < Right )
			hb_gt_Replicate( Row, Left, byAttr, byChar, Right - Left + 1 );
		else
			hb_gt_Replicate( Row, Right, byAttr, byChar, Left - Right + 1 );
		ret = 0;
	}
	return ret;
}

USHORT hb_gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr )
{
	USHORT ret = 1;
	SHORT Row;

	if( Col >= 0 && Col < hb_gt_GetScreenWidth() )
	{
		if( Top < 0 )
			Top = 0;
		else if( Top >= hb_gt_GetScreenHeight() )
			Top = hb_gt_GetScreenHeight() - 1;

		if( Bottom < 0 )
			Bottom = 0;
		else if( Bottom >= hb_gt_GetScreenHeight() )
			Bottom = hb_gt_GetScreenHeight() - 1;

		if( Top <= Bottom )
			Row = Top;
		else
		{
			Row = Bottom;
			Bottom = Top;
		}
		qtcapp->getDoc()->startChanging();
		while( Row <= Bottom )
			hb_gt_xPutch( Row++, Col, byAttr, byChar );
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
static USHORT hb_gt_GetDisplay( void )
{
	HB_TRACE( HB_TR_DEBUG, ("hb_gt_SetDisplay()") );
	return 0xff;
}

/* TODO: Support this mode */
BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
	HB_TRACE( HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", usRows, usCols) );
	HB_SYMBOL_UNUSED( uiRows );
	HB_SYMBOL_UNUSED( uiCols );
	return FALSE;
}

BOOL hb_gt_PreExt()
{
	return TRUE;
}

BOOL hb_gt_PostExt()
{
	return TRUE;
}

BOOL hb_gt_Suspend()
{
	return TRUE;
}

BOOL hb_gt_Resume()
{
	return TRUE;
}

void hb_gt_OutStd( BYTE * pbyStr, ULONG ulLen )
{
	hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

void hb_gt_OutErr( BYTE * pbyStr, ULONG ulLen )
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
      qtapp->exec();
   }
}
