/***************************************************************************
                          qtconsoleview.h  -  description
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

#ifndef QTCONSLEVIEW_H
#define QTCONSLEVIEW_H

#define TOP_MARGIN  0
// include files for QT
#include <qwidget.h>

// application specific includes
#include "qtconsoledoc.h"
/**
 * This class provides an incomplete base for your application view. 
 */                 

class QTconsoleView : public QWidget
{
  Q_OBJECT

protected:

  virtual void paintEvent( QPaintEvent *e );

  public:
    QTconsoleView(QWidget *parent=0, QTconsoleDoc* doc=0);
    ~QTconsoleView();
  /** Called on paint... It draws the frame buffer and puts in the x,y fields. */
  /** Write property of QFont * _font. */
  virtual void set_font( QFont * _newVal);
  /** Read property of QFont * _font. */
  virtual const QFont * get_font();
  /** Resizes view to match new document measures */
  void resizeOnDoc();
  /** Uses font metrics to calculate an area that must be updated
 */
  QSize calcSizeOfChars( int cols, int rows);

	/** Size of a character */
	inline int cellWidth( char ch = 'Z' ) {
	  return _fmetric->width( QChar ( ch ) )+1;
	}
	/** Size of a character */
	inline int cellHeight( ) {
	  return _fmetric->lineSpacing();
	}

	inline int rows() const { return _rows; }
	inline int cols() const { return _cols; }
  /** Returns a pen created basing on a dos attrib color */
  static QPen penFromAttrib( char c );
  /** Creates a solid brush for painting the background of a character. */
  static QBrush brushFromAttrib( char c );
  /** Calc a screen region given a rectangle in col/row character coordinates */
  QRect calcRectOfChars( int x1, int y1, int width, int height );
  QRect calcRectOfChars( QRect &r ) {
	  return calcRectOfChars( r.x(), r.y(), r.width(), r.height() );
  }
  /** Calculates the rectangle in the character grid covered by a certain rectangle. */
  QRect charsFromRect( int x1, int y1, int width, int height );
  QRect charsFromRect( const QRect &r ) {
	  return charsFromRect( r.x(), r.y(), r.width(), r.height() );
	  }

	
private: // Private attributes
  /** Font of the window */
  QFont * _font;
  /**  */
  QFontMetrics * _fmetric;

protected slots:
    void slotDocumentChanged( QRect );
  /** Changes the status of the cursor and the blinking characters, and call repaint to update the areas. */
  void slotBlink();
  
protected: // Protected attributes
  /**  */
  QTconsoleDoc * pDoc;
private:
  /** Rows of the document, recorded in the wiew to allow smart updating */
  int _rows;
  /** Colums of the document, recorded in the wiew to allow updating */
  int _cols;
  /** This timer "shots" each time the cursor and the blinking attrib must be refreshed */
  QTimer * blinker;
  /** 0 when cursor & blinking chars are off, 1 when they are on. */
  int blinkStatus;
  
	  
signals: // Signals
  /** Called when something important has changed in the view */
  void viewChanged();
};

#endif
