#include "base64.h"
#include "hbapi.h"
#include "hbapifs.h"
#define XTY62  '+'
#define XTY63  '/'
#define XTYPAD '='

static char Base64Table[64] = {
	'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
	'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
	'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
	'w','x','y','z','0','1','2','3','4','5','6','7','8','9',XTY62,XTY63
};

char *Base64Encode(const unsigned char *pcCode, unsigned int uCodeLen){
    unsigned int i, j=0, uOutLen=4*((uCodeLen+2)/3)+1;
    char *pRet= hb_xgrab(uOutLen+1);
    unsigned int tailCnt;

    for(i=0; i<uCodeLen/3; i++,pcCode+=3){ //codeLen/3 segments.
        pRet[j++]=Base64Table[pcCode[0]>>2];
        pRet[j++]=Base64Table[((pcCode[0]&3)<<4)|(pcCode[1]>>4)];
        pRet[j++]=Base64Table[((pcCode[1]&15)<<2)|(pcCode[2]>>6)];
        pRet[j++]=Base64Table[pcCode[2]&0X3F];
    };

    tailCnt=uCodeLen % 3;
    if(tailCnt==1){
        pRet[j++]=Base64Table[pcCode[0]>>2];
        pRet[j++]=Base64Table[(pcCode[0]&3)<<4];
        pRet[j++]=XTYPAD;
        pRet[j++]=XTYPAD;
    }else if(tailCnt==2){
        pRet[j++]=Base64Table[pcCode[0]>>2];
        pRet[j++]=Base64Table[((pcCode[0]&3)<<4)|(pcCode[1]>>4)];
        pRet[j++]=Base64Table[(pcCode[1]&15)<<2];
        pRet[j++]=XTYPAD;
    };

    pRet[j++]=XTYPAD;
    pRet[j]='\0';
    return pRet;
}

//assume input != NULL.
unsigned char *Base64Decode(const char *pcszInput, unsigned int *puOutLen)
   {
    int iSize = strlen(pcszInput)+1 ;
    char map[256], i, c, *pBuf = hb_xgrab(iSize);
    unsigned int j, uBufLen;
    unsigned int uSegCnt;
    unsigned int uTailCnt;
    unsigned char *pRet,*pTmp;
    char *ptr;

    memset(map,0XFF,256);
    for(i='A',map['A']=0; i<'Z'; map[i+1]=map[i]+1,i++);
    for(i='a',map['a']=26; i<='z'; map[i+1]=map[i]+1,i++);
    for(i='0',map['0']=52; i<='9'; map[i+1]=map[i]+1,i++);
    map[XTY62]=62; map[XTY63]=63;

    uBufLen=0; c=pcszInput[0];
    while(c!='\0' && c!=XTYPAD){
        pBuf[uBufLen]=map[c];

        if(pBuf[uBufLen]>=0)
            uBufLen++;
        c=*(++pcszInput);
    };
    pBuf[uBufLen]='\0';

    uSegCnt=uBufLen/4;
//  unsigned char *pRet=new unsigned char[(uSegCnt+1)*3], *pTmp=pRet;
    pRet =  hb_xgrab((uSegCnt+1)*3),pTmp=pRet;

    //full segments.
    ptr=pBuf;
    for(j=0; j<uSegCnt; j++){
        *pTmp++=((ptr[0]<<2)|(ptr[1]>>4));
        *pTmp++=((ptr[1]<<4)|(ptr[2]>>2));
        *pTmp++=((ptr[2]<<6)|ptr[3]);
        ptr+=4;
    };
    *puOutLen=uSegCnt*3;

    uTailCnt=uBufLen%4;
    if(uTailCnt==2){
        *pTmp++=((ptr[0]<<2)|(ptr[1]>>4));
        (*puOutLen)++;
    }else if(uTailCnt==3){
        *pTmp++=((ptr[0]<<2)|(ptr[1]>>4));
        *pTmp++=((ptr[1]<<4)|(ptr[2]>>2));
        (*puOutLen)+=2;
    };


    hb_xfree( pBuf );
    return pRet;
}

HB_FUNC( HB_BASE64ENCODE )
{
   const unsigned char *pcCode = hb_parc( 1 ) ;
   unsigned int uCodeLen = hb_parni( 2 );
   char * szBase64Encode ;

   szBase64Encode = Base64Encode( pcCode , uCodeLen );
   hb_retc( szBase64Encode );
}

HB_FUNC( HB_BASE64DECODE )
{
   const char *pcCode = hb_parc( 1 ) ;
   unsigned int uCodeLen ;
   unsigned char * szBase64Encode ;

   szBase64Encode = Base64Decode( pcCode , &uCodeLen );

   hb_retc( szBase64Encode );
   hb_xfree( szBase64Encode );
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

HB_FUNC( HB_BASE64ENCODEFILE )
{
   char *szInFile = hb_parc( 1 );
   char *szOutFile = hb_parc( 2 ) ;
   const char *pcCode ;
   char *FromBuffer;
   int fh;
   int iSize;
   char * szBase64Encode ;

   fh = hb_fsOpen( ( BYTE * ) szInFile , 2 );
   iSize = filelength( fh ); 
   FromBuffer = ( char * ) hb_xgrab( iSize + 1 );
   hb_fsClose(fh);
   pcCode = (char*) filetoBuff( FromBuffer , szInFile ) ;
   szBase64Encode = Base64Encode( pcCode , iSize );
   fh = hb_fsCreate( szOutFile,0) ;
   hb_fsWriteLarge( fh, szBase64Encode, strlen( szBase64Encode ));
   hb_fsClose( fh );
   hb_xfree( FromBuffer);
   hb_xfree( szBase64Encode );

}
HB_FUNC( HB_BASE64DECODEFILE )
{
   char *szInFile = hb_parc( 1 );
   char *szOutFile = hb_parc( 2 ) ;
   const char *pcCode ;
   char *FromBuffer;
   int fh;
   int iSize;
   char * szBase64Encode ;

   fh = hb_fsOpen( ( BYTE * ) szInFile , 2 );
   iSize = filelength( fh ); 
   FromBuffer = ( char * ) hb_xgrab( iSize + 1 );
   hb_fsClose(fh);
   pcCode = (char*) filetoBuff( FromBuffer , szInFile ) ;
   szBase64Encode = Base64Decode( pcCode , &iSize  );
   fh = hb_fsCreate( szOutFile,0) ;
   hb_fsWriteLarge( fh, szBase64Encode, strlen( szBase64Encode ));
   hb_fsClose( fh );
   hb_xfree( FromBuffer);


}






