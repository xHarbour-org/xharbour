/***************************************************************************
                          qtconsoledoc.h  -  description
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
#ifndef QTCONSLEDOC_H
#define QTCONSLEDOC_H

// include files for QT
#include <qobject.h>

#define DEFROWS 25
#define DEFCOLS 80

// application specific includes

/**
  * the Document Class
  */

class QTconsoleDoc : public QObject
{
  Q_OBJECT

  friend class QTconsoleView;
  
  public:
    QTconsoleDoc();
    ~QTconsoleDoc();
    void newDoc();
    bool save();
    bool saveAs(const QString &filename);
    bool load(const QString &filename);
    bool isModified() const;
    int rows() const { return _rows; }
    int cols() const { return _cols; }
  /** Clears virtual screen with a given background color (default black ) */
  void clearScr( unsigned char attrib );
  void clearScr() { clearScr( _attrib ); }
    

    // todo, make it private
    char * buffer;
  /** Rectangle where modification has intervened */
  QRect _rectMody;

	/** Set standard attribute */
	void setAttrib( char a ) { _attrib = a; }

	/** Get standard attribute */
	char getAttrib() { return _attrib ; }
  /** Writes a string with the current attribute to the buffer; returns true if write has been done, or false if data can't be written. */
  bool write( int x, int y, char *data, int len=-1);
  /** Writes a char with the current attribute to the buffer; returns true if write has been done, or false if data can't be written. */
  bool write( int x, int y, char c );
  /** Writes a multiline string with the current attribute to the buffer; returns true if write has been done, or false if data can't be written. */
  bool write( int x, int y, char *data, int width, int height );
  bool write( char c );
  bool write( char *data, int len=-1 );
  bool writeAttrib( int x, int y, char attrib );

  /** Reads a block of data from the buffer */
  bool read( int x, int y, char *data, int width, int height=1 );
  /** Reads a single character from the buffer */
  bool read( int x, int y, char *data );
  /** Reads a single attrib */
  bool readAttrib( int x, int y, char *attrib );

  /** Read a block of data altogether with the attribute bytes */
  bool getMem( int x, int y, char *data, int width, int height=1 );
  /** Writes a block of data altogether with the attribute bytes */
  bool setMem( int x, int y, char *data, int width, int height=1 );

  /** X position of the virtual cursor */
  int cursX;
  /** Y position of the virtual cursor */
  int cursY;

  int getCursX() { return cursX; }
  int getCursY() { return cursY; }

  void gotoXY( int x, int y )
  {
	  // signal that the cursor has moved
	  _rectMody = QRect( cursX, cursY, 1 , 1 );
	  cursX = x;
	  cursY = y;
	  emit documentChanged();
	  // signal where the cursor had moved
	  _rectMody = QRect( cursX, cursY, 1 , 1 );
	  emit documentChanged();
	  
   }
  /** Adds a rectangle to the current modify area, that will be notified when the 
changes are comited to the views with setModified( true ) or endChanging() */
  void rectChanging( QRect r );

	/** Start a multi edit section */
	void startChanging() {
	  _changing++;
	}

	void endChanging() {
		if ( _changing );
         _changing --;
		if (! _changing ) {
			setModified();
		}
	}
	
  /** Scroll the text of a defined number of lines... */
  bool scroll( int lines = 1 );
	
  signals:
    void documentChanged();

  protected:
    bool modified;
    
  /** This variables contains the screen buffer, organized as a normal screen buffer.
 */
private: // Private attributes
  /** Colums width of the document */
  int _cols;
  /** Rows used insde the document */
  int _rows;
	void setModified( bool t=true ) {
		// ignore calls in multi line change statements
		if ( _changing ) return;
		modified = t;
		if ( t ) {
			emit documentChanged();
			_rectCommited = false;
		}
	}


	unsigned char _attrib;
  /** When changing is actived, the internal functions won't update the document. A direct call to endChanging() muist be made to commit all the modifications to the view(s). */
  int _changing;
  /** This variable is changed when _rectMody ( modified areas) is being
modified, but still not sent to the views. */
  bool _rectCommited;
};

#endif
