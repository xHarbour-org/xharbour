/*
 * xHarbour Project source code:
 * Curl lib low level (client api) interface code.
 *
 * Copyright 2005 Luiz Rafael Culik Guimaraes <luiz at xharbour.com.br>
 * www - http://www.xharbour.org
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
 * See doc/license.txt for licensing terms.
 *
 */

#include "windows.h"
#include <curl/curl.h>
#include <curl/types.h>
#include <curl/easy.h>
#include <hbapi.h>
#include <hbapiitm.h>
#include <hbvmpub.h>
#include <hbvm.h>

#include <sys/stat.h>
#include <hbcurl.ch>
typedef struct _FTPFILE
{
   char *filename;
   FILE *stream;
} FTPFILE, *PFTPFILE;

typedef struct _CURLHANDLE
{
   CURL *curl;
   struct curl_slist *sHttpHeader; 
   struct curl_httppost *sHttpPostf;
   struct curl_httppost *sHttpPostl;   
   struct curl_slist *sQuote;
   struct curl_slist *sPostQuote;   
   struct curl_slist *sPreQuote;
   FILE * szFile;
   PHB_ITEM pProgress;
   FTPFILE  fFile;
} CURLHANDLE,*PCURLHANDLE;


int hb_WriteFtpDownload(void *buffer, size_t size, size_t nmemb, void *stream)
{ 
   PFTPFILE out = ( PFTPFILE ) stream;
   if( out && !out->stream)
   {

      out->stream = fopen( out->filename, "wb" );

      if( !out->stream )
      {
         return -1;
      }

   }

   return fwrite( buffer, size, nmemb, out->stream );
}

int my_progress_func(void *Bar,
                     double t, /* dltotal */
                     double d, /* dlnow */
                     double ultotal,
                     double ulnow)
{  
   PHB_ITEM p =hb_itemPutND( NULL, (ulnow > 0 ? ulnow : d ) );
   PHB_ITEM p1=hb_itemPutND( NULL, (ultotal > 0  ?ultotal : t ) );
  

   hb_vmEvalBlockV( ( PHB_ITEM ) Bar, 2, p, p1 );
   hb_itemRelease( p );
   hb_itemRelease( p1 );

   return 0;
}

HB_FUNC(CURL_GLOBAL_INIT)
{
   CURLcode err;
   err=curl_global_init( hb_parnl( 1 ) );
   hb_retnl( err ) ;
}

HB_FUNC(CURL_EASY_INIT)
{
   PCURLHANDLE  pConn= (PCURLHANDLE) hb_xgrab(sizeof(CURLHANDLE ) );
   /* get a curl handle */
   memset(pConn,0,sizeof(CURLHANDLE));
   pConn->curl = curl_easy_init();
   hb_retptr((void*)pConn);
}

HB_FUNC(CURL_EASY_SETOPT)
{
   PCURLHANDLE  pConn= (PCURLHANDLE) hb_parptr( 1 );
   CURLcode res;
   int iOption = hb_parni( 2 );
   switch (iOption)
   {
   case HB_CURLOPT_INFILESIZE_LARGE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t) hb_parnl( 3 ) );
      break;
   case HB_CURLOPT_RESUME_FROM_LARGE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_RESUME_FROM_LARGE, (curl_off_t) hb_parnl( 3 ) );
      break;     
   case HB_CURLOPT_MAXFILESIZE_LARGE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_MAXFILESIZE_LARGE, (curl_off_t) hb_parnl( 3 ) );
      break;   
   case HB_CURLOPT_POSTFIELDSIZE_LARGE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_POSTFIELDSIZE_LARGE, (curl_off_t) hb_parnl( 3 ) );
      break;   
   case HB_CURLOPT_PORT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PORT, hb_parnl( 3 ) );
      break;      
   case HB_CURLOPT_TIMEOUT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_TIMEOUT, hb_parnl( 3 ) );
      break;      

   case HB_CURLOPT_INFILESIZE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_INFILESIZE, hb_parnl( 3 ) );
      break;               

   case HB_CURLOPT_LOW_SPEED_TIME:
      res = curl_easy_setopt(pConn->curl, CURLOPT_LOW_SPEED_TIME, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_RESUME_FROM:
      res = curl_easy_setopt(pConn->curl, CURLOPT_RESUME_FROM, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_CRLF:
      res = curl_easy_setopt(pConn->curl, CURLOPT_CRLF, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_SSLVERSION:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLVERSION, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_TIMECONDITION:
      res = curl_easy_setopt(pConn->curl, CURLOPT_TIMECONDITION, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_TIMEVALUE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_TIMEVALUE, hb_parnl( 3 ) );
      break;                  
   case HB_CURLOPT_VERBOSE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_VERBOSE, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_HEADER:
      res = curl_easy_setopt(pConn->curl, CURLOPT_HEADER, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_NOPROGRESS:
      res = curl_easy_setopt(pConn->curl, CURLOPT_NOPROGRESS, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_FAILONERROR:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FAILONERROR, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_UPLOAD:
      res = curl_easy_setopt(pConn->curl, CURLOPT_UPLOAD, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_POST:
      res = curl_easy_setopt(pConn->curl, CURLOPT_POST, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_FTPLISTONLY:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTPLISTONLY, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_FTPAPPEND:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTPAPPEND, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_NETRC:
      res = curl_easy_setopt(pConn->curl, CURLOPT_NETRC, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_FOLLOWLOCATION:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FOLLOWLOCATION, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_TRANSFERTEXT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_TRANSFERTEXT, hb_parnl( 3 ) );
      break;               
   case HB_CURLOPT_PUT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PUT, hb_parnl( 3 ) );
      break;               
      
   case HB_CURLOPT_AUTOREFERER:
      res = curl_easy_setopt(pConn->curl, CURLOPT_AUTOREFERER, hb_parnl( 3 ) );
      break;    
  
   case HB_CURLOPT_PROXYPORT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PROXYPORT, hb_parnl( 3 ) );
      break;  

   case HB_CURLOPT_POSTFIELDSIZE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_POSTFIELDSIZE, hb_parnl( 3 ) );
      break;
   
   case HB_CURLOPT_HTTPPROXYTUNNEL:
      res = curl_easy_setopt(pConn->curl, CURLOPT_HTTPPROXYTUNNEL, hb_parnl( 3 ) );
      break;   
  
   case HB_CURLOPT_SSL_VERIFYPEER:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSL_VERIFYPEER, hb_parnl( 3 ) );
      break;
    
   case HB_CURLOPT_MAXREDIRS:
      res = curl_easy_setopt(pConn->curl, CURLOPT_MAXREDIRS, hb_parnl( 3 ) );
      break;    
  
   case HB_CURLOPT_FILETIME:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FILETIME, hb_parnl( 3 ) );
      break;
    
   case HB_CURLOPT_MAXCONNECTS:
      res = curl_easy_setopt(pConn->curl, CURLOPT_MAXCONNECTS, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_CLOSEPOLICY:
      res = curl_easy_setopt(pConn->curl, CURLOPT_CLOSEPOLICY, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_FRESH_CONNECT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FRESH_CONNECT, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_FORBID_REUSE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FORBID_REUSE, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_CONNECTTIMEOUT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_CONNECTTIMEOUT, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_HTTPGET:
      res = curl_easy_setopt(pConn->curl, CURLOPT_HTTPGET, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_SSL_VERIFYHOST:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSL_VERIFYHOST, hb_parnl( 3 ) );
      break;
   case HB_CURLOPT_HTTP_VERSION:
      res = curl_easy_setopt(pConn->curl, CURLOPT_HTTP_VERSION, hb_parnl( 3 ) );
      break;
     
   case HB_CURLOPT_FTP_USE_EPSV:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTP_USE_EPSV, hb_parnl( 3 ) );
      break;
    
   case HB_CURLOPT_SSLENGINE_DEFAULT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLENGINE_DEFAULT, hb_parnl( 3 ) );
      break;
            
   case HB_CURLOPT_DNS_USE_GLOBAL_CACHE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_DNS_USE_GLOBAL_CACHE, hb_parnl( 3 ) );
      break;
    
   case HB_CURLOPT_DNS_CACHE_TIMEOUT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_DNS_CACHE_TIMEOUT, hb_parnl( 3 ) );
      break;
          
   case HB_CURLOPT_COOKIESESSION:
      res = curl_easy_setopt(pConn->curl, CURLOPT_COOKIESESSION, hb_parnl( 3 ) );
      break;
    
   case HB_CURLOPT_BUFFERSIZE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_BUFFERSIZE, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_NOSIGNAL:
      res = curl_easy_setopt(pConn->curl, CURLOPT_NOSIGNAL, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_PROXYTYPE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PROXYTYPE, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_UNRESTRICTED_AUTH:
      res = curl_easy_setopt(pConn->curl, CURLOPT_UNRESTRICTED_AUTH, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_FTP_USE_EPRT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTP_USE_EPRT, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_HTTPAUTH:
      res = curl_easy_setopt(pConn->curl, CURLOPT_HTTPAUTH, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_FTP_CREATE_MISSING_DIRS:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTP_CREATE_MISSING_DIRS, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_PROXYAUTH:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PROXYAUTH, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_FTP_RESPONSE_TIMEOUT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTP_RESPONSE_TIMEOUT , hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_IPRESOLVE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_IPRESOLVE, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_MAXFILESIZE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_MAXFILESIZE, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_FTP_SSL:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTP_SSL, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_TCP_NODELAY:
      res = curl_easy_setopt(pConn->curl, CURLOPT_TCP_NODELAY, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_FTPSSLAUTH:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTPSSLAUTH, hb_parnl( 3 ) );
      break;
      
   case HB_CURLOPT_IGNORE_CONTENT_LENGTH:
      res = curl_easy_setopt(pConn->curl, CURLOPT_IGNORE_CONTENT_LENGTH, hb_parnl( 3 ) );
      break;
  
   case HB_CURLOPT_URL:
      res = curl_easy_setopt(pConn->curl, CURLOPT_URL, hb_parcx( 3 ) );
      break;        

   case HB_CURLOPT_PROXY:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PROXY, hb_parcx( 3 ) );
      break;        
    
   case HB_CURLOPT_USERPWD:
      res = curl_easy_setopt(pConn->curl, CURLOPT_USERPWD, hb_parcx( 3 ) );
      break;        
  
   case HB_CURLOPT_PROXYUSERPWD:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PROXYUSERPWD, hb_parcx( 3 ) );
      break;        
   
   case HB_CURLOPT_RANGE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_RANGE, hb_parcx( 3 ) );
      break;        

   case HB_CURLOPT_INFILE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_INFILE, pConn->szFile );
      break;        

   case HB_CURLOPT_ERRORBUFFER:
      res = curl_easy_setopt(pConn->curl, CURLOPT_ERRORBUFFER, hb_parcx( 3 ) );
      break;        
      
   case HB_CURLOPT_POSTFIELDS:
      res = curl_easy_setopt(pConn->curl, CURLOPT_POSTFIELDS , hb_parcx( 3 ) );
      break;              

   case HB_CURLOPT_REFERER:
      res = curl_easy_setopt(pConn->curl, CURLOPT_REFERER , hb_parcx( 3 ) );
      break;        

   case HB_CURLOPT_FTPPORT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTPPORT , hb_parcx( 3 ) );
      break;        

   case HB_CURLOPT_USERAGENT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_USERAGENT , hb_parcx( 3 ) );
      break;        
        
   case HB_CURLOPT_COOKIE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_COOKIE , hb_parcx( 3 ) );
      break;          
  
   case HB_CURLOPT_HTTPHEADER :
   {
      PHB_ITEM pHttpHeaders = hb_param( 3, HB_IT_ARRAY );
      ULONG ulPos  ; 
      ULONG ulArrayPos = pHttpHeaders->item.asArray.value->ulLen;
      for ( ulPos = 0; ulPos < ulArrayPos; ulPos ++ )
      {
         curl_slist_append( pConn->sHttpHeader, hb_arrayGetCPtr( pHttpHeaders, ulPos + 1 ) );
      }   
      res = curl_easy_setopt(pConn->curl, CURLOPT_HTTPHEADER , pConn->sHttpHeader ); 
   }      
      break;          
  
   case HB_CURLOPT_HTTPPOST:
   {
      PHB_ITEM pHttpPost = hb_param( 3, HB_IT_ARRAY );
      ULONG ulPos ; 
      ULONG ulArrayPos = pHttpPost->item.asArray.value->ulLen;
      for ( ulPos = 0; ulPos < ulArrayPos; ulPos ++ )
      {
         PHB_ITEM pArray = hb_arrayGetItemPtr( pHttpPost, ulPos + 1 );
         curl_formadd(&pConn->sHttpPostf,
               &pConn->sHttpPostl,
               CURLFORM_COPYNAME, hb_arrayGetCPtr( pArray, 1 ),
               CURLFORM_FILE, hb_arrayGetCPtr( pArray, 2 ),
               CURLFORM_END);                  
      }   
      res = curl_easy_setopt(pConn->curl, CURLOPT_HTTPPOST , pConn->sHttpPostf );
   }      
      break;          
  
   case HB_CURLOPT_SSLCERT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLCERT , hb_parcx( 3 ) );
      break;          
  
   case HB_CURLOPT_SSLKEYPASSWD:  
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLKEYPASSWD , hb_parcx( 3 ) );
      break;          
        
   case HB_CURLOPT_QUOTE:
   {
      PHB_ITEM pHttpHeaders = hb_param( 3, HB_IT_ARRAY );
      ULONG ulPos ; 
      ULONG ulArrayPos = pHttpHeaders->item.asArray.value->ulLen;
      for ( ulPos = 0; ulPos < ulArrayPos; ulPos ++ )
      {
         curl_slist_append( pConn->sQuote, hb_arrayGetCPtr( pHttpHeaders, ulPos + 1 ) );
      }   
      res = curl_easy_setopt(pConn->curl, CURLOPT_QUOTE , pConn->sQuote ); 
   }   
      
      break;          
  
   case HB_CURLOPT_WRITEHEADER:
      res = curl_easy_setopt(pConn->curl, CURLOPT_WRITEHEADER , hb_parcx( 3 ) ); //pointer or file *
      break;          
  
   case HB_CURLOPT_COOKIEFILE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_COOKIEFILE , hb_parcx( 3 ) );
      break;          
        
   case HB_CURLOPT_CUSTOMREQUEST:
      res = curl_easy_setopt(pConn->curl, CURLOPT_CUSTOMREQUEST , hb_parcx( 3 ) );
      break;         
  
   case HB_CURLOPT_STDERR:
      res = curl_easy_setopt(pConn->curl, CURLOPT_STDERR , hb_parcx( 3 ) ); //File *
      break;           
  
   case HB_CURLOPT_POSTQUOTE:
   {
      PHB_ITEM pHttpHeaders = hb_param( 3, HB_IT_ARRAY );
      ULONG ulPos ; 
      ULONG ulArrayPos = pHttpHeaders->item.asArray.value->ulLen;
      for ( ulPos = 0; ulPos < ulArrayPos; ulPos ++ )
      {
         curl_slist_append( pConn->sPostQuote, hb_arrayGetCPtr( pHttpHeaders, ulPos + 1 ) );
      }   
      res = curl_easy_setopt(pConn->curl, CURLOPT_POSTQUOTE , pConn->sQuote ); 
   }         
      break;         
  
   case HB_CURLOPT_WRITEINFO: //verificar
      res = curl_easy_setopt(pConn->curl, CURLOPT_WRITEINFO , hb_parcx( 3 ) );
      break;           
        
   case HB_CURLOPT_PROGRESSDATA:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PROGRESSDATA , hb_parcx( 3 ) );
      break;             
  
   case HB_CURLOPT_INTERFACE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_INTERFACE , hb_parcx( 3 ) );
      break;               
  
   case HB_CURLOPT_KRB4LEVEL:
      res = curl_easy_setopt(pConn->curl, CURLOPT_KRB4LEVEL , hb_parcx( 3 ) );
      break;               
   
   case HB_CURLOPT_CAINFO:
      res = curl_easy_setopt(pConn->curl, CURLOPT_CAINFO , hb_parcx( 3 ) );
      break;                 

   case HB_CURLOPT_TELNETOPTIONS:
      res = curl_easy_setopt(pConn->curl, CURLOPT_TELNETOPTIONS , hb_parcx( 3 ) ); //usa curl_slist
      break;                 
  
   case HB_CURLOPT_RANDOM_FILE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_RANDOM_FILE , hb_parcx( 3 ) ); 
      break;                   
  
   case HB_CURLOPT_EGDSOCKET:
      res = curl_easy_setopt(pConn->curl, CURLOPT_EGDSOCKET , hb_parcx( 3 ) ); 
      break;                   
  
   case HB_CURLOPT_COOKIEJAR:
      res = curl_easy_setopt(pConn->curl, CURLOPT_COOKIEJAR , hb_parcx( 3 ) ); 
      break;        
  
   case HB_CURLOPT_SSL_CIPHER_LIST:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSL_CIPHER_LIST , hb_parcx( 3 ) ); 
      break;          
  
   case HB_CURLOPT_SSLCERTTYPE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLCERTTYPE , hb_parcx( 3 ) ); 
      break;          
  
   case HB_CURLOPT_SSLKEY:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLKEY , hb_parcx( 3 ) );
      break;          
  
   case HB_CURLOPT_SSLKEYTYPE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLKEYTYPE , hb_parcx( 3 ) );
      break;          
  
   case HB_CURLOPT_SSLENGINE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLENGINE , hb_parcx( 3 ) ); 
      break;            
  
   case HB_CURLOPT_PREQUOTE:      
   {
      PHB_ITEM pHttpHeaders = hb_param( 3, HB_IT_ARRAY );
      ULONG ulPos ; 
      ULONG ulArrayPos = pHttpHeaders->item.asArray.value->ulLen;
      for ( ulPos = 0; ulPos < ulArrayPos; ulPos ++ )
      {
         curl_slist_append( pConn->sQuote, hb_arrayGetCPtr( pHttpHeaders, ulPos + 1 ) );
      }   
      res = curl_easy_setopt(pConn->curl, CURLOPT_PREQUOTE , pConn->sPreQuote ); 
   }         
      break;              
  
   case HB_CURLOPT_DEBUGDATA:
      res = curl_easy_setopt(pConn->curl, CURLOPT_DEBUGDATA , hb_parcx( 3 ) ); // usa pointer
      break;              
            
   case HB_CURLOPT_CAPATH:
      res = curl_easy_setopt(pConn->curl, CURLOPT_CAPATH , hb_parcx( 3 ) ); 
      break; 
      
 /*  case HB_CURLOPT_SHARE:
       res = curl_easy_setopt(pConn->curl, CURLOPT_SHARE , hb_parcx( 3 ) ); 
       break;              */
        
   case HB_CURLOPT_ENCODING:
      res = curl_easy_setopt(pConn->curl, CURLOPT_ENCODING , hb_parcx( 3 ) ); 
      break;   
  
   case HB_CURLOPT_PRIVATE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PRIVATE , hb_parcx( 3 ) ); 
      break;   
          
   case HB_CURLOPT_HTTP200ALIASES:
      res = curl_easy_setopt(pConn->curl, CURLOPT_HTTP200ALIASES , hb_parcx( 3 ) );  // USA struct curl_slist structs
      break;   

   case HB_CURLOPT_SSL_CTX_DATA:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSL_CTX_DATA , hb_parcx( 3 ) );  //usa pointer
      break;      
      
   case HB_CURLOPT_NETRC_FILE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_NETRC_FILE , hb_parcx( 3 ) ); 
      break;   
  
   case HB_CURLOPT_SOURCE_USERPWD:
      res = curl_easy_setopt(pConn->curl, CURLOPT_USERPWD , hb_parcx( 3 ) ); 
      break;     
  
   case HB_CURLOPT_SOURCE_PREQUOTE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PREQUOTE , hb_parcx( 3 ) ); //usa pointer
      break;   
  
   case HB_CURLOPT_SOURCE_POSTQUOTE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_POSTQUOTE , hb_parcx( 3 ) );  //usa pointer
      break;     
      
   case HB_CURLOPT_IOCTLDATA:
      res = curl_easy_setopt(pConn->curl, CURLOPT_IOCTLDATA , hb_parcx( 3 ) );  //pointer
      break;       

   case HB_CURLOPT_SOURCE_URL:
      res = curl_easy_setopt(pConn->curl, CURLOPT_URL , hb_parcx( 3 ) ); 
      break;     
  
   case HB_CURLOPT_SOURCE_QUOTE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_QUOTE , hb_parcx( 3 ) ); 
      break;     
  
   case HB_CURLOPT_FTP_ACCOUNT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTP_ACCOUNT , hb_parcx( 3 ) ); 
      break;     
  
   case HB_CURLOPT_COOKIELIST:
      res = curl_easy_setopt(pConn->curl, CURLOPT_COOKIELIST , hb_parcx( 3 ) ); 
      break;
   case HB_CURLOPT_SETUPLOADFILE:
   {
      res = 1;

      if ( pConn->szFile )
      {
         fclose( pConn->szFile );
      }

      pConn->szFile = fopen( hb_parc( 3 ), "rb" );
   }
      break;
   case HB_CURLOPT_SETDOWNLOADFILE:
   {
      res = 1;
      //You can download many files at same session, if we using same session,
      // of previus download file, lets make sure that we have all files closed
      if( pConn->fFile.filename )
      {
         hb_xfree( pConn->fFile.filename );
         pConn->fFile.filename = NULL;
      }

      if(pConn->fFile.stream)
      {
         fclose( pConn->fFile.stream );
      }

      pConn->fFile.filename = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );
      strcpy( ( char * ) pConn->fFile.filename, (char * ) hb_parc( 3 ) );
      pConn->fFile.stream = NULL;
      curl_easy_setopt(pConn->curl, CURLOPT_WRITEFUNCTION, hb_WriteFtpDownload );  
      curl_easy_setopt(pConn->curl, CURLOPT_WRITEDATA, &pConn->fFile );
   }
      break;
   case HB_CURLOPT_CLOSEUPLOADFILE:
   {
      res = 1;
      fclose( pConn->szFile );
   }
      break;
   case HB_CURLOPT_SETPROGRESS:
   {
     PHB_ITEM pBar = hb_param( 3, HB_IT_BLOCK );
  
     if( pBar )
     {
      pConn->pProgress = hb_itemNew( pBar ) ;
      res = 1 ;
      curl_easy_setopt( pConn->curl, CURLOPT_PROGRESSFUNCTION, my_progress_func );
      curl_easy_setopt( pConn->curl, CURLOPT_PROGRESSDATA, (void*) pConn->pProgress );
     } 

   }
}  
   
   hb_retnl( res );
}             

HB_FUNC(CURL_EASY_PERFORM)
{
   PCURLHANDLE  pConn= (PCURLHANDLE) hb_parptr( 1 );
   CURLcode err;

   err = curl_easy_perform( pConn->curl );
   hb_retnl( err ) ;
}

HB_FUNC(CURL_EASY_CLEANUP)
{
   PCURLHANDLE  pConn= (PCURLHANDLE) hb_parptr( 1 );

   if( pConn->fFile.filename )
   {
      hb_xfree( pConn->fFile.filename );
      pConn->fFile.filename = NULL;
   }

   if( pConn->fFile.stream )
      fclose( pConn->fFile.stream) ; 

   if ( pConn->sHttpHeader )
      curl_slist_free_all( pConn->sHttpHeader ); 
      
   if ( pConn->sHttpPostf )      
      curl_formfree( pConn->sHttpPostf );
      
   if ( pConn->sHttpPostl )   
      curl_formfree( pConn->sHttpPostl );  
      
   if ( pConn->sQuote )   
      curl_slist_free_all( pConn->sQuote );
      
   if ( pConn->sPostQuote )   
      curl_slist_free_all( pConn->sPostQuote );  
      
   if ( pConn->sPreQuote )   
      curl_slist_free_all( pConn->sPreQuote );   
      
   if (pConn->pProgress)
   {
      hb_itemRelease( ( PHB_ITEM ) pConn->pProgress );
      pConn->pProgress = NULL;
   }
      
   curl_easy_cleanup( pConn->curl)   ;
}   

HB_FUNC( CURL_GLOBAL_CLEANUP )
{
   curl_global_cleanup();
}
