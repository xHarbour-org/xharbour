#include "uuencode.h"
#include "hbapi.h"
#include "hbapifs.h"

//Assume pInput!=NULL, iInputLen>0.
char *UUEncode(unsigned char *pInput, unsigned int iInputLen){
    #define CODEOF(c) ((c)?((c)+' '):'`')
    int i,n;
    unsigned char c1, c2, c3;
    char *pRet, *pTmp;
    unsigned int iOutLen=(iInputLen+44)/45+((iInputLen+2)/3)*4;

    pRet=(char *)hb_xgrab(iOutLen+1);
    pTmp=pRet;

    while(iInputLen>0){
        n=(iInputLen>=45) ? 45 : iInputLen;
        for(*pTmp++=CODEOF(n),i=0; i<n; i+=3){
            c1=(i<n)?pInput[i]:0;
            c2=((i+1)<n)?pInput[i+1]:0;
            c3=((i+2)<n)?pInput[i+2]:0;
            *pTmp++=CODEOF(c1>>2);
            *pTmp++=CODEOF(((c1&03)<<4)|((c2>>4)&017));
            *pTmp++=CODEOF(((c2<<2)&074)|((c3>>6)&03));
            *pTmp++=CODEOF(c3&077);
        };
        iInputLen-=n;
        pInput+=n;
    };

    #undef CODEOF
    return pRet;
}

unsigned char* UUDecode(char *pszInput, unsigned int *pOutLen){
    #define CHAROF(c) (((*c)=='`') ? 0 : ((*c)-' ')); c++;
    int i, n, last=0, iInputLen=strlen(pszInput);
    unsigned char c1, c2, c3, c4, *pRet, *pTmp;
    *pOutLen=0;

    pRet=(unsigned char*)hb_xgrab((iInputLen*3)/4+1);
    pTmp=pRet;

    while(!last){
        n=CHAROF(pszInput);
        if(n<45)
            last=1;

        for(*pOutLen+=n, i=0; n>0; i+=4, n-=3){
            c1=CHAROF(pszInput);
            c2=CHAROF(pszInput);
            c3=CHAROF(pszInput);
            c4=CHAROF(pszInput);
            *pTmp++=(c1<<2)|((c2&060)>>4);
            *pTmp++=((c2&017)<<4)|(c3>>2);
            *pTmp++=((c3&03)<<6)|(c4);
        };
    };
    #undef CHAROF

    return pRet;
}



HB_FUNC( HB_UUENCODE )
{
   unsigned char *pcCode = hb_parc( 1 ) ;
   unsigned int uCodeLen = hb_parni( 2 );
   char * szUUEncode ;

   szUUEncode = UUEncode( pcCode , uCodeLen );
   hb_retc( szUUEncode );
}

HB_FUNC( HB_UUDECODE )
{
   unsigned char *pcCode = hb_parc( 1 ) ;
   unsigned int uCodeLen ;
   unsigned char * szUUEncode ;

   szUUEncode = UUDecode( pcCode , &uCodeLen );

   hb_retc( szUUEncode );
   hb_xfree( szUUEncode );
}


static long filelength( int handle )
{
    long nEnd = hb_fsSeek( handle, 0 , 2 );
    long nStart = hb_fsSeek( handle , 0 , 0 );
    return nEnd - nStart;
}


static char *filetoBuff(char *f,char *s)
{

   int i;
   int fh = hb_fsOpen( ( BYTE * ) s , 2 );
   i = hb_fsReadLarge( fh , ( BYTE * ) f , filelength( fh ) );
   f[ i ] = '\0';
   hb_fsClose( fh );
   return f   ;
}

HB_FUNC( HB_UUENCODEFILE )
{
   char *szInFile = hb_parc( 1 );
   char *szOutFile = hb_parc( 2 ) ;
   char *pcCode ;
   char *FromBuffer;
   int fh;
   int iSize;
   char * szUUEncode ;

   fh = hb_fsOpen( ( BYTE * ) szInFile , 2 );
   iSize = filelength( fh ); 
   FromBuffer = ( char * ) hb_xgrab( iSize + 1 );
   hb_fsClose(fh);
   pcCode = (char*) filetoBuff( FromBuffer , szInFile ) ;
   szUUEncode = UUEncode( pcCode , iSize );
   fh = hb_fsCreate( szOutFile,0) ;
   hb_fsWriteLarge( fh, szUUEncode, strlen( szUUEncode ));
   hb_fsClose( fh );
   hb_xfree( FromBuffer);
   hb_xfree( szUUEncode );

}
HB_FUNC( HB_UUDECODEFILE )
{
   char *szInFile = hb_parc( 1 );
   char *szOutFile = hb_parc( 2 ) ;
   unsigned char *pcCode ;
   char *FromBuffer;
   int fh;
   unsigned int iSize;
   char * szUUEncode ;

   fh = hb_fsOpen( ( BYTE * ) szInFile , 2 );
   iSize = filelength( fh ); 
   FromBuffer = ( char * ) hb_xgrab( iSize + 1 );
   hb_fsClose(fh);
   pcCode = (char*) filetoBuff( FromBuffer , szInFile ) ;
   szUUEncode = UUDecode( pcCode , &iSize  );
   fh = hb_fsCreate( szOutFile,0) ;
   hb_fsWriteLarge( fh, szUUEncode, strlen( szUUEncode ));
   hb_fsClose( fh );
   hb_xfree( FromBuffer);


}


