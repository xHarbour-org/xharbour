#define _WIN32_WINNT   0x0400

#include <shlobj.h>
#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

/*--------------------------------------------------------------------------*/
// not case sensitive
int _strnxComp(char *string1, char *string2, UINT len)
{
 UINT x ;
  //OutputDebugString("_strnxComp");
 for(x=0 ; x<len ; x++)
   if(toupper(string1[x]) != toupper(string2[x]))
     return 0 ;
 return 1 ;
}
/*--------------------------------------------------------------------------*/
char* _strltrim(char* pString)
{
 UINT x =0 ;
 while ((pString[x]==32) && pString[x])
      x++ ;
 return (pString+x) ;
}

/*--------------------------------------------------------------------------*/
int _strnComp(char *string1, char *string2, UINT len)
{
 UINT x ;
 //OutputDebugString("_strnComp");
 for(x=0 ; x<len ; x++)
   if(string1[x] != string2[x])
     return 0 ;
 return 1 ;
}
/*--------------------------------------------------------------------------*/
UINT _lenstr(char* string)
{
 UINT nlen = 0;
 //OutputDebugString("_lenstr");
 while (string[nlen] != 0)
      nlen++ ;

 return nlen;
}
 /*--------------------------------------------------------------------------*/
void _strset(char* pDest, char* pSource,int lenSource)
{
 INT x ;

  //OutputDebugString("_strset");
 for (x=0 ; x<=lenSource ; x++)
   (pDest[x] = pSource[x]);
 pDest[x] = 0 ;

 return ;
}
/*--------------------------------------------------------------------------*/
UINT _searcharray(char** array, char* cText, UINT lenarray,
                  UINT lentext, int lMatchCase)
{

UINT i;
 //OutputDebugString("_searcharray");
 for (i=0;i<lenarray;i++)
   if (lentext == _lenstr(array[i]))
     if ( lMatchCase )
       {
       if (_strnComp(array[i],cText,lentext))
         return i ;
       }
     else
       if (_strnxComp(array[i],cText,lentext))
         return i;



 return 0 ;
}

/*--------------------------------------------------------------------------*/
/* NOTE: returns the actual position 1 index based */
/* StartFrom must be 0 based */
UINT _atn(char* chars, char* string,UINT StartFrom,
          UINT Target, UINT len_chars, UINT len )
{
 UINT x ;
 UINT Counter = 0;
 //OutputDebugString("_atn");
 if( len >= len_chars && StartFrom <= len-len_chars)
   for(x=StartFrom;x<=(len-len_chars);x++)
     if(_strnComp(string+x,chars,len_chars) )
       if(++Counter==Target)
         return (x+1);
 return 0;
}
/*--------------------------------------------------------------------------*/
void _stradd(char* pDest, char* pSource,int lenSource)
{
 INT x ;
  //OutputDebugString("_stradd");
 while (pDest[0])
    pDest++ ;
 for (x=0 ; x<=lenSource ; x++)
   (pDest[x] = pSource[x]);
 pDest[x] = 0 ;

 return ;
}
/*--------------------------------------------------------------------------*/
char* _rPad(char* string , UINT len)
{
  UINT x;
  int PastTheEnd = 0;
  //OutputDebugString("_rPad");
  for (x=0 ; x<len ; x++)
     {
      if (string[x]==0)
         PastTheEnd = 1;
      if (PastTheEnd)
        string[x] = 32 ;
     }
  string[x]=0;

  return string ;
}






/*--------------------------------------------------------------------------*/
HB_FUNC(CCOLORCODE)
{
// parameters
char* cText   ;
int  lInComment ;


// local vars
char*   cGood = "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890." ;
char*   cNumbers = "1234567890.";
UINT   lengood = 64 ;

HDC hDC        ;
int leftcol    ;
int cxChar     ;
int cyChar     ;
int Row        ;
int MaxCol     ;

int lencPaint  ;
int nWritten   ;
UINT lenText   ;
UINT nPos      ;
UINT i,x         ;
char cWord[31] ;
int type       ;
char cResWord[256];
char* pCurrent ;
char cPaint[1024]  ;
char* cSingle = " ";


// list of reserved and standard words
char* aReserved[]=
{
"While"     ,"EndDo"    ,"Case"      ,"Otherwise" ,"EndCase"   ,"If"      ,
"Else"      ,"ElseIf"   ,"EndIf"     ,"Begin"     ,"Sequence"  ,"Break"   ,
"Loop"      ,"Recover"  ,"End"       ,"For"       ,"To"        ,"Next"    ,
"Step"      ,"Announce" ,"Declare"   ,"External"  ,"Field"     ,"Function",
"Func"      ,"Method"   ,"Meth"      ,"Class"     ,"EndClass"  ,"Var"     ,
"Init"      ,"Procedure","Proc"      ,"Static"    ,"Local"     ,"MemVar"  ,
"Parameters","Private"  ,"Public"    ,"Request"   ,"Return"    ,"Command" ,
"Define"    ,"Error"    ,"IfDef"     ,"IfnDef"    ,"Include"   ,"StdOut"  ,
"UnDef"     ,"xCommand" ,"xTranslate","Exit"      ,"Do"        ,".T."     ,
".F."       ,".AND."    ,".NOT."     ,".OR."      ,"NIL"       ,"SET"     ,
"ON"        ,"OFF"
};

int lenReserved = 62;


// initiate params
cText      = hb_parc(1);
lInComment = hb_parl(2);
hDC        = (HDC)hb_parni(3);
leftcol    = hb_parni(4);
cxChar     = hb_parni(5);
cyChar     = hb_parni(6);
Row        = hb_parni(7);
MaxCol     = hb_parni(8);

cPaint[0]  = 0;
nPos       = 0;
type       = 1;
nWritten   = 0;

// function starts here
lenText = _lenstr(cText);
pCurrent= _strltrim(cText);
cResWord[0] = 0 ;

if (pCurrent[0]== '*' && !(_strnComp(pCurrent,"*/",2)))
  {
   _strset(cPaint,cText,lenText);
   nPos = lenText;
   type = 2;
  }
else
  if (lInComment)
    if ((nPos = _atn("*/",cText,0,1,2,lenText))> 0)
      {
       _strset(cPaint,cText,nPos+2);
       lInComment = 0 ;
       nPos+=2;
       type = 2;
      }
    else
      {
       _strset(cPaint,cText,lenText);
       nPos = lenText;
       type = 2;
      };

/*
if (cPaint[0])
  {
   lencPaint =_lenstr(cPaint);
   if (leftcol < nWritten+lencPaint+1)
     {
      SetTextColor(hDC,_parnl(9,type));
      TextOut(hDC,(nWritten-leftcol+1)*cxChar,(Row-1)*cyChar,cPaint,
      lencPaint);
     }
   nWritten+=lencPaint;
  }
*/




while (nPos < lenText)
  {


   // check for inline comment
   if ((nPos<lenText-1) && (_strnComp(cText+nPos,"//",2 )))
     {
      if (cPaint[0]) goto display;
      _strset(cPaint,cText+nPos,lenText-nPos);
      nPos = lenText ;
      type = 2;
     }


   // check for multiline comment
   else if  ((nPos<lenText-1) && (_strnComp(cText+nPos,"/*",2 )))
          {
           if (cPaint[0]) goto display;
           if (i = _atn("*/",cText+nPos,2,1,2,lenText-nPos))
             {
              _strset(cPaint,cText+nPos,i+1);
              lInComment = 0 ;
              nPos+=(i+1);
              type = 2;
             }
           else
             {
              _strset(cPaint,cText+nPos,lenText-nPos);
              lInComment = 1;
              nPos = lenText ;
              type = 2;
             }
          }

        // check for string constant
        else if ((cText[nPos]=='"') || (cText[nPos]=='\''))
                {
                if (cPaint[0]) goto display;
                cSingle[0] = cText[nPos];
                if (i = _atn(cSingle,cText+nPos,1,1,1,lenText-nPos))
                  {
                   _strset(cPaint,cText+nPos,i-1);
                   nPos+=i;
                   type = 3;
                  }
                else
                  {
                   _strset(cPaint,cText+nPos,lenText-nPos);
                   nPos = lenText ;
                   type = 4;
                  }
               }


             // check for reserved words
             else
               {
                cSingle[0]=cText[nPos];
                if (_atn(cSingle,cGood,0,1,1,lengood))
                  {
                     i=1;
                     while (((nPos+i) < lenText) && (i < 30))
                          {
                           cSingle[0] = cText[nPos+i];
                           if (!_atn(cSingle,cGood,0,1,1,lengood))
                             break;
                           i++;
                          }
                     _strset(cWord,cText+nPos,i-1);
                     if (_searcharray(aReserved,cWord,lenReserved,i,0))
                       {
                        if (cPaint[0]) goto display ;
                        _stradd(cPaint,cText+nPos,i-1);
                        nPos+=i;
                        type=5;
                       }
                     else
                       {
                        for (x = 0 ; x < i ; x++ )
                          {
                           cSingle[0]=cWord[x];
                           if (!_atn(cSingle,cNumbers,0,1,1,11))
                             break;
                          }
                        if (x==i)
                          {
                           if (cPaint[0]) goto display ;
                           _stradd(cPaint,cText+nPos,i-1);
                           nPos+=i;
                           type=3;

                          }
                        else
                          {
                           _stradd(cPaint,cText+nPos,i-1);
                           nPos+=i;
                           type = 1;
                           continue;
                          }
                       }

                  }

                // regular odd code
                else
                  {
                   cSingle[0] = cText[nPos];
                   _stradd(cPaint,cSingle,1);
                   nPos++;
                   type = 1;
                   continue;
                  }


               }

   while ((nPos<lenText) && (cText[nPos]== 32))
        {
         _stradd(cPaint," ",1);
         nPos++;
        }

   display:
           if (cPaint[0])
            {
             lencPaint =_lenstr(cPaint);
             if (leftcol < nWritten+lencPaint+1)
              {
               SetTextColor(hDC,hb_parnl(9,type));
               TextOut(hDC,(nWritten-leftcol+1)*cxChar,(Row-1)*cyChar,cPaint,
                       lencPaint);
              }
             nWritten+=lencPaint;
             cPaint[0] = 0;
            }


  }

  lencPaint =_lenstr(cPaint);
   if (leftcol < nWritten+lencPaint+1)
    {
     _rPad(cPaint,max(0,MaxCol-(nWritten-leftcol)));
     SetTextColor(hDC,hb_parnl(9,type));
     TextOut(hDC,(nWritten-leftcol+1)*cxChar,(Row-1)*cyChar,
     cPaint,max(0,MaxCol-(nWritten-leftcol)));
    }

  hb_storl(lInComment,2);
 return ;

}
