/***************************************************************************
                          qtconsoleview.cpp  -  description
                             -------------------
    begin                : dom nov 17 13:34:51 CET 2002
    copyright            : (C) 2002 by Giancarlo
    email                : 
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include <qfont.h>
#include <qfontmetrics.h>
#include <qpainter.h>
#include "qtconsoleview.h"
#include <iostream>
#include <qtimer.h>

QTconsoleView::QTconsoleView(QWidget *parent, QTconsoleDoc *doc) : QWidget(parent)
{
  /** connect doc with the view*/
  connect(doc, SIGNAL(documentChanged()), this, SLOT(slotDocumentChanged()));
  pDoc = doc;
  // set the font to a default
  set_font( new QFont( "Courier", 12 ) );
  _rows = 0;
  _cols = 0;
  /** starts the blink timer */
  blinkStatus = 1;
  blinker = new QTimer( this );
  connect( blinker, SIGNAL( timeout() ), this, SLOT( slotBlink() ) );
  blinker->start( 500 );
}

QTconsoleView::~QTconsoleView()
{
	delete _font;
	delete _fmetric;
	delete blinker;
}

void QTconsoleView::slotDocumentChanged()
{
	// find a smarter repaint

	//repaint( calcRectOfChars( pDoc->_rectMody), false );
   update( calcRectOfChars( pDoc->_rectMody) );
}


/** Called on paint... It draws the frame buffer and puts in the x,y fields. */
void QTconsoleView::paintEvent( QPaintEvent *e )
{
	// resize view if document size changed
	if ( _rows != pDoc->rows() || _cols != pDoc->cols() )
		resizeOnDoc();

	QPainter p( this );

	p.setPen( black );
	p.setFont( *_font );

	QString bufstring;

	
	int olda1 = -1;
	int olda2 = -1;
	int charp = 0;

	QPen pen;

	// calculates where I will have to operate
	QRect area = charsFromRect( e->rect() );
	int posy = TOP_MARGIN + area.y()*cellHeight();
	for ( int row = area.y(); row <= area.bottom(); row++  ) {
		posy  = cellHeight() * row;
		charp = (row * _cols + area.x()) *2;
		for ( int col = area.x(); col <= area.right(); col ++, charp += 2 ) {
			char c = pDoc->buffer[ charp + 1];

			char attr = pDoc->buffer[ charp ];
			if ( c && ( attr & 0xf )!= olda1 ){
				olda1 = attr & 0xf;
				pen = penFromAttrib( olda1 );
			}
			if ( ( attr & 0x70) != olda2 ){
				olda2 = attr & 0x70;
				p.setBrush( brushFromAttrib( olda2 ) );
			}
			// draw background
			int posx = col * cellWidth( c );
			p.setPen( NoPen );
			p.drawRect( posx, posy , cellWidth( c ), cellHeight() );
			// draw cursor
			if ( pDoc->getCursX() == col && pDoc->getCursY() == row && blinkStatus) {
				//TODO: change this with a better cursor drawing function
				// that takes care of reversing color of character
				QBrush oldb = p.brush();
				p.setBrush( darkGray );
				p.drawRect( posx, posy , cellWidth( c ), cellHeight() );
				p.setBrush( oldb );
			}
			// draw character only if not blinking
			if ( c && !( ( attr & 0x80 ) && blinkStatus == 0 )  ) {
				p.setPen( pen );
				p.drawText( posx, posy+_fmetric->ascent() , QChar( c ) );
			}
		}
	}
	
}
/** Read property of QFont * _font. */
const QFont * QTconsoleView::get_font(){
	return _font;
}
/** Write property of QFont * _font. */
void QTconsoleView::set_font( QFont * _newVal){
	_font = _newVal;
	_fmetric = new QFontMetrics( *_font );
}
/** Resizes view to match new document measures */
void QTconsoleView::resizeOnDoc(){
	_rows = pDoc->rows();
	_cols = pDoc->cols();
	QSize sz = calcSizeOfChars( _cols, _rows );
	sz += QSize( 0, TOP_MARGIN );
	resize( sz );
	emit viewChanged();
}

/** Uses font metrics to calculate an area that must be updated
 */
QSize QTconsoleView::calcSizeOfChars( int cols, int rows )
{
	QSize ret( cols * cellWidth(), rows * cellHeight() );
	return ret;
}

	/** Returns a pen created basing on a dos attrib color */
QPen QTconsoleView::penFromAttrib( char c )
{
	QColor color;
	switch( c & 0x0f ) {
		case 0: color = black; break;
		case 1: color = darkBlue; break;
		case 2: color = darkGreen; break;
		case 3: color = darkCyan; break;
		case 4: color = darkRed; break;
		case 5: color = darkMagenta; break;
		case 6: color = darkYellow; break;
		case 7: color = lightGray; break;
		case 8: color = darkGray; break;
		case 9: color = blue; break;
		case 10: color = green; break;
		case 11: color = cyan; break;
		case 12: color = red; break;
		case 13: color = magenta; break;
		case 14: color = yellow; break;
		case 15: color = white; break;
	}
	return QPen( color );
}
/** Creates a solid brush for painting the background of a character. */
QBrush QTconsoleView::brushFromAttrib( char c )
{
	QColor color;
	switch( ( c & 0x70) >> 4 ) {
		case 0: color = black; break;
		case 1: color = darkBlue; break;
		case 2: color = darkGreen; break;
		case 3: color = darkCyan; break;
		case 4: color = darkRed; break;
		case 5: color = darkMagenta; break;
		case 6: color = darkYellow; break;
		case 7: color = lightGray; break;
	}
	return QBrush( color );
}
/** Calc a screen region given a rectangle in col/row character coordinates */
QRect QTconsoleView::calcRectOfChars( int x1, int y1, int width, int height )
{
	QRect ret( x1 * cellWidth(), y1 * cellHeight() + TOP_MARGIN,
		width * cellWidth(), height * cellHeight());
	return ret;
}
/** Calculates the rectangle in the character grid covered by a certain rectangle. */
QRect QTconsoleView::charsFromRect( int x, int y, int width, int height )
{
	int c1, c2, w, h;

	c1 = x / cellWidth();
	w = ( x + width) / cellWidth();
	if ( (x+width) % cellWidth() ) w++;
	c2 = ( y- TOP_MARGIN ) / cellHeight();
	h = (y+height - TOP_MARGIN ) / cellHeight();
	if ( (y+height - TOP_MARGIN ) % cellHeight() ) h++;

	if ( c1 < 0 ) c1 = 0;
	if ( c2 < 0 ) c2 = 0;
	if ( w > pDoc->cols() ) w = pDoc->cols();
	if ( h > pDoc->rows() ) h = pDoc->rows();
	return QRect( c1, c2, w-c1, h-c2 );
}

/** Changes the status of the cursor and the blinking characters, and call repaint to update the areas. */
void QTconsoleView::slotBlink()
{
	if ( blinkStatus ) blinkStatus = 0;
	else blinkStatus = 1;

	//find and force repainting of all blinking chars.
	for ( int i = 0; i < pDoc->rows(); i ++ ) {
		int pr = i * pDoc->cols() * 2;
		for ( int j = 0; j < pDoc->cols()*2; j+=2 ) {
			if ( (pDoc->buffer[j + pr] & 0x80) )  {
				//repaint( calcRectOfChars( j/2, i, 1, 1), false );
            update( calcRectOfChars( j/2, i, 1, 1) );
         }
		}
	}
	// repaint cursor
	//repaint( calcRectOfChars( pDoc->getCursX(), pDoc->getCursY(), 1, 1), false );
   update( calcRectOfChars( pDoc->getCursX(), pDoc->getCursY(), 1, 1) );
}

