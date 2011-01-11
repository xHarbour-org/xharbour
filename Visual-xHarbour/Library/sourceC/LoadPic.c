/*
 * $Id$
 */

#ifndef CINTERFACE
   #define CINTERFACE 1
#endif

#include "hbapi.h"
#include "hbapiitm.h"

#include <windows.h>
#include <olectl.h>

//#include <comdef.h>

#include <stdio.h>

//#include <shlobj.h>
#include <ocidl.h>

#define HIMETRIC_PER_INCH 2540
#define MAP_LOGHIM_TO_PIX(ppli, x)  (ppli * x + HIMETRIC_PER_INCH / 2) / HIMETRIC_PER_INCH
#define MAP_PIX_TO_LOGHIM(x,ppli)   MulDiv(HIMETRIC_PER_INCH, (x), (ppli))

typedef struct tagPictures
{
  IPicture *Picture;       // pointer to the picture
  long PictureWidth;       // picture witdh (in pixels)
  long PictureHeight;      // picture height (in pixels)
  int PositionX;           // the X coordinate of the picture on the Window

  int PositionY;           // the Y coordinate of the picture on the Window
  OLE_XSIZE_HIMETRIC cx;   // Amount which will be used for copy horizontally in source picture
  OLE_YSIZE_HIMETRIC cy;   // Amount which will be used for copy vertically in source picture
  SIZE sz;
} Pictures;

//---------------------------------------------------------------------------------------------------

IPicture *LoadPicture( const unsigned char *data, size_t len, OLE_XSIZE_HIMETRIC *cx, OLE_YSIZE_HIMETRIC *cy)
{
  //HDC dcPictureLoad;
  IPicture *pic = NULL;
  HGLOBAL hGlobal = GlobalAlloc(0, len);
  LPVOID pvData = GlobalLock( hGlobal );
  IStream* pStream = NULL;

  memcpy(pvData,data,len);
  GlobalUnlock(hGlobal);

  // here's to create a second function called LoadStream()
  // that way it can be used to load streams stored in a database

  CreateStreamOnHGlobal( hGlobal, FALSE, &pStream );

  OleLoadPicture( pStream, (long) len, TRUE, &IID_IPicture, (void **) &pic );

  if( pic )
  {
     pStream->lpVtbl->Release( pStream );

     pic->lpVtbl->get_Width( pic, cx);
     pic->lpVtbl->get_Height( pic, cy);
  }

  return pic;
}

//---------------------------------------------------------------------------------------------------
IPicture *LoadResIcon( LPCTSTR cImage, OLE_XSIZE_HIMETRIC *cx, OLE_YSIZE_HIMETRIC *cy )
{
   HICON hIco;
   PICTDESC pd;
   IPicture *pic = 0;
   hIco=(HICON)LoadImage( GetModuleHandle( NULL ), cImage, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE );
   if( hIco )
   {
     //Create new IPicture interface
     pd.cbSizeofstruct=sizeof(PICTDESC);
     pd.picType=PICTYPE_ICON;
     pd.icon.hicon=hIco;
     OleCreatePictureIndirect(&pd,&IID_IPicture,TRUE,(LPVOID*)&pic);
     //DeleteObject(hRetBmp);
   }
   if( pic )
   {
      pic->lpVtbl->get_Width( pic, cx);
      pic->lpVtbl->get_Height( pic, cy);
   }
   return pic;
}

//---------------------------------------------------------------------------------------------------
IPicture *LoadResImage( HINSTANCE hInst, LPCTSTR cImage, OLE_XSIZE_HIMETRIC *cx, OLE_YSIZE_HIMETRIC *cy, int iType )
{
   PICTDESC pd;
   IPicture *pic = 0;
   if( iType == 1 )
   {
      HBITMAP hBmp;
      hBmp=(HBITMAP)LoadImage( hInst, cImage, IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR | LR_DEFAULTSIZE | LR_CREATEDIBSECTION );
      if( hBmp )
      {
         pd.cbSizeofstruct=sizeof(PICTDESC);
         pd.picType=PICTYPE_BITMAP;
         pd.bmp.hbitmap=hBmp;
         pd.bmp.hpal=NULL;
         OleCreatePictureIndirect(&pd,&IID_IPicture,TRUE,(LPVOID*)&pic);
      }
   }
   else if( iType == 2)
   {
      HICON hIco;
      hIco=(HICON)LoadImage( hInst, cImage, IMAGE_ICON, 0, 0, 0 );
      if( hIco )
      {
         pd.cbSizeofstruct=sizeof(PICTDESC);
         pd.picType=PICTYPE_ICON;
         pd.icon.hicon=hIco;
         OleCreatePictureIndirect(&pd,&IID_IPicture,TRUE,(LPVOID*)&pic);
      }
   }
   if( pic )
   {
      pic->lpVtbl->get_Width( pic, cx);
      pic->lpVtbl->get_Height( pic, cy);
   }
   return pic;
}

//---------------------------------------------------------------------------------------------------
IPicture *PictureFromBitmap( HBITMAP hBmp, OLE_XSIZE_HIMETRIC *cx, OLE_YSIZE_HIMETRIC *cy )
{
   PICTDESC pd;
   IPicture *pic = 0;
   if( hBmp )
   {
      //Create new IPicture interface
      pd.cbSizeofstruct=sizeof(PICTDESC);
      pd.picType=PICTYPE_BITMAP;
      pd.bmp.hbitmap=hBmp;
      pd.bmp.hpal=NULL;
      OleCreatePictureIndirect(&pd,&IID_IPicture,TRUE,(LPVOID*)&pic);
      //DeleteObject(hRetBmp);
   }
   if( pic )
   {
      pic->lpVtbl->get_Width( pic, cx);
      pic->lpVtbl->get_Height( pic, cy);
   }
   return pic;
}

//---------------------------------------------------------------------------------------------------
void PictureRemove( Pictures *pPicture )
{
   if( pPicture->Picture )
   {
      pPicture->Picture->lpVtbl->Release( pPicture->Picture );
      free(pPicture);
   }
}

SIZE PictureGetSize( Pictures *pPicture )
{
   SIZE sz;
   HDC hdc = GetDC(0);
   int nx = GetDeviceCaps(hdc, LOGPIXELSX);
   int ny = GetDeviceCaps(hdc, LOGPIXELSY);
   ReleaseDC( 0, hdc );
   sz.cx = MAP_LOGHIM_TO_PIX( pPicture->cx, nx);
   sz.cy = MAP_LOGHIM_TO_PIX( pPicture->cy, ny);
   return sz;
}

void PicturePaint( Pictures *pPicture, HDC hdc,int PositionX,int PositionY,int nWidth,int nHeight,BOOL lStretch,BOOL lRatio )
{
  int nx, ny;
  double nPer;
  RECT bounds;
  OLE_HANDLE hPal = 0;

  pPicture->PositionX = PositionX;
  pPicture->PositionY = PositionY;

  if( !pPicture->Picture )
  {
     return;
  }

  nx = GetDeviceCaps(hdc, LOGPIXELSX);
  ny = GetDeviceCaps(hdc, LOGPIXELSY);

  if( !lStretch )
  {
     lStretch = FALSE;
  }

  if( !lRatio )
  {
     lRatio = FALSE;
  }

  if( lRatio )
  {
     lStretch = FALSE;
  }

  pPicture->PictureWidth  = MAP_LOGHIM_TO_PIX(pPicture->cx, nx);
  pPicture->PictureHeight = MAP_LOGHIM_TO_PIX(pPicture->cy, ny);

  if( lRatio )
  {
     int iy;

     nPer =  (double) MAP_PIX_TO_LOGHIM(nWidth,nx) / (double) pPicture->cx;
     iy = ( (double) MAP_LOGHIM_TO_PIX( pPicture->cy, ny) )* nPer ;

     if( iy < nHeight )
     {
        nHeight = iy ;
     }
     else
     {
        nPer =  (double) MAP_PIX_TO_LOGHIM(nHeight,ny) / (double) pPicture->cy;
        nWidth = ( (double) MAP_LOGHIM_TO_PIX( pPicture->cx, nx) )* nPer ;
     }

     pPicture->sz.cx=nWidth;
     pPicture->sz.cy=nHeight;
  }
  else
  {
     if( nWidth == 0 || !lStretch )
     {
        nWidth = pPicture->PictureWidth;
     }

     if( nHeight == 0 || !lStretch )
     {
        nHeight = pPicture->PictureHeight;
     }
  }

  bounds.top    = pPicture->PositionY;
  bounds.bottom = pPicture->PositionY + nHeight;
  bounds.left   = pPicture->PositionX;
  bounds.right  = pPicture->PositionX + nWidth;

  pPicture->Picture->lpVtbl->get_hPal( pPicture->Picture, &hPal );

  if( hPal )
  {
     SelectPalette( hdc,(HPALETTE) hPal, FALSE );
     RealizePalette( hdc );
  }

  pPicture->Picture->lpVtbl->Render( pPicture->Picture, hdc, PositionX, PositionY, nWidth, nHeight, 0, pPicture->cy, pPicture->cx, -pPicture->cy, &bounds);

}

//---------------------------------------------------------------------------------------------------
Pictures *PictureLoadImageFromResource( HINSTANCE hInst, LPCSTR cResource, int iType )
{
   Pictures *pPicture = (Pictures*) malloc( sizeof( Pictures) );

   (hInst);

   pPicture->Picture = LoadResImage( hInst, cResource, &pPicture->cx, &pPicture->cy, iType);

   return pPicture;
}

//---------------------------------------------------------------------------------------------------
Pictures *PictureLoadBitmap( HBITMAP hBmp )
{
   Pictures *pPicture = (Pictures*) malloc( sizeof( Pictures) );

   pPicture->Picture = PictureFromBitmap( hBmp, &pPicture->cx, &pPicture->cy );

   return pPicture;
}

//---------------------------------------------------------------------------------------------------
Pictures *PictureLoadFromResource( HINSTANCE hInst, LPCSTR cResource, LPCSTR cType )
{
   Pictures *pPicture = (Pictures*) malloc( sizeof( Pictures) );
   HRSRC res = FindResource( hInst, cResource, cType );

   (hInst);

   if(res)
   {
      HGLOBAL mem = LoadResource( hInst, res);
      void *data = LockResource(mem);
      size_t sz = SizeofResource( hInst, res);

      pPicture->Picture = LoadPicture( (u_char*)data, sz, &pPicture->cx, &pPicture->cy);

      UnlockResource(mem);
      FreeResource(mem);
      return pPicture;
   }
   else
   {
      return NULL;
   }
}

//---------------------------------------------------------------------------------------------------
HBITMAP hBitmapFromResource( HINSTANCE hInst, LPCSTR cResource, LPCSTR cType )
{
   Pictures *pPicture = (Pictures*) malloc( sizeof( Pictures) );
   HRSRC res = FindResource( hInst, cResource, cType );
   HBITMAP bitmap = NULL;

   (hInst);

   if(res)
   {
      HGLOBAL mem = LoadResource( hInst, res);
      void *data = LockResource(mem);
      size_t sz = SizeofResource( hInst, res);

      pPicture->Picture = LoadPicture( (u_char*)data, sz, &pPicture->cx, &pPicture->cy);
      if( pPicture->Picture )
      {
         HBITMAP handle = NULL;
         pPicture->Picture->lpVtbl->get_Handle( pPicture->Picture, (unsigned int*)&handle);
         bitmap = (HBITMAP)CopyImage(handle, IMAGE_BITMAP, 0, 0, LR_COPYRETURNORG);
      }
      UnlockResource(mem);
      FreeResource(mem);
   }
   return bitmap;
}

//---------------------------------------------------------------------------------------------------

Pictures *PictureLoadFromFile(char* sFileName)
{
   Pictures *pPicture = (Pictures*) malloc( sizeof( Pictures ) );
   FILE * pFile;
   long lSize;
   char * buffer;

   pPicture->Picture = 0;
   pFile = fopen ( sFileName , "rb" );
   if( pFile )
   {
      // obtain file size.
      fseek (pFile , 0 , SEEK_END);
      lSize = ftell (pFile);
      rewind (pFile);

      // allocate memory to contain the whole file.
      buffer = (char*) malloc (lSize);

      // copy the file into the buffer.
      fread (buffer,1,lSize,pFile);

      /*** the whole file is loaded in the buffer. ***/
      pPicture->Picture = LoadPicture( (u_char*)buffer, lSize, &pPicture->cx, &pPicture->cy);

      fclose (pFile);
      free (buffer);
   }
   return pPicture;
}

PHB_ITEM SizeToArray( SIZE *siz  )
{
   PHB_ITEM aSize = hb_itemArrayNew(2);
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aSize, 1, hb_itemPutNL(element, siz->cx));
   hb_arraySet(aSize, 2, hb_itemPutNL(element, siz->cy));
   hb_itemRelease(element);
   return aSize;
}

HB_FUNC( PICTURESIZE )
{
   PHB_ITEM aSize;
   Pictures *pPicture = (Pictures*) hb_parnl(1);

   aSize = SizeToArray( &pPicture->sz );

   hb_itemReturnForward( aSize );
   hb_itemRelease( aSize );
}

HB_FUNC( LOAD_PICTURE )
{
   Pictures *pPicture = PictureLoadFromFile( (char*) hb_parc(1) );
   hb_retnl( (LONG) pPicture->Picture );
}

HB_FUNC( PICTURELOADFROMFILE )
{
  hb_retnl( (LONG) PictureLoadFromFile( (char*) hb_parc(1) ) );
}

HB_FUNC( PICTURELOADFROMRESOURCE )
{
  hb_retnl( (LONG) PictureLoadFromResource( (HINSTANCE) hb_parnl(1), hb_parc(2), hb_parc(3) ) );
}

HB_FUNC( HBITMAPFROMRESOURCE )
{
  hb_retnl( (LONG) hBitmapFromResource( (HINSTANCE) hb_parnl(1), hb_parc(2), hb_parc(3) ) );
}

HB_FUNC( PICTURELOADIMAGEFROMRESOURCE )
{
  hb_retnl( (LONG) PictureLoadImageFromResource( (HINSTANCE) hb_parnl(1), hb_parc(2), hb_parni(3) ) );
}

HB_FUNC( PICTURELOADBITMAP )
{
  hb_retnl( (LONG) PictureLoadBitmap( (HBITMAP) hb_parnl(1) ) );
}

HB_FUNC( PICTUREPAINT )
{
  PicturePaint( (Pictures *) hb_parnl(1), (HDC) hb_parnl(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6), hb_parl(7), hb_parl(8) );
}

HB_FUNC( PICTUREREMOVE )
{
  PictureRemove( (Pictures *) hb_parnl(1) );
}

HB_FUNC( PICTUREGETSIZE )
{
  SIZE sz;
  PHB_ITEM aSize;

  sz = PictureGetSize( (Pictures *) hb_parnl(1) );
  aSize = SizeToArray( &sz );

  hb_itemReturnForward( aSize );
  hb_itemRelease( aSize );
}

//---------------------------------------------------------------------------------------------------
HB_FUNC( HBITMAPFROMBUFFER )
{
   HBITMAP bitmap = NULL;
   OLE_XSIZE_HIMETRIC cx;
   OLE_YSIZE_HIMETRIC cy;

   IPicture *pPicture = LoadPicture( (u_char*) hb_parc(1), (size_t) hb_parclen(1), &cx, &cy);
   if( pPicture )
   {
      HBITMAP handle = NULL;
      pPicture->lpVtbl->get_Handle( pPicture, (unsigned int*)&handle);
      bitmap = (HBITMAP)CopyImage(handle, IMAGE_BITMAP, 0, 0, LR_COPYRETURNORG);
   }
   hb_retnl( (LONG) bitmap );
}

