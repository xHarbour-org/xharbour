#include <curl/curl.h>
#include <curl/types.h>
#include <curl/easy.h>
#include <hbapi.h>
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
   HB_ITEM p;
   HB_ITEM p1;
   p.type = HB_IT_NIL;
   p1.type = HB_IT_NIL;
   hb_itemPutND( &p, (ulnow > 0 ? ulnow : d ) );
   hb_itemPutND( &p1, (ultotal > 0  ?ultotal : t ) );
   hb_vmEvalBlockV( ( PHB_ITEM ) Bar, 2, &p, &p1 );  

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
   case HB_INFILESIZE_LARGE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t) hb_parnl( 3 ) );
      break;
   case HB_RESUME_FROM_LARGE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_RESUME_FROM_LARGE, (curl_off_t) hb_parnl( 3 ) );
      break;     
   case HB_MAXFILESIZE_LARGE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_MAXFILESIZE_LARGE, (curl_off_t) hb_parnl( 3 ) );
      break;   
   case HB_POSTFIELDSIZE_LARGE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_POSTFIELDSIZE_LARGE, (curl_off_t) hb_parnl( 3 ) );
      break;   
   case HB_PORT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PORT, hb_parnl( 3 ) );
      break;      
   case HB_TIMEOUT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_TIMEOUT, hb_parnl( 3 ) );
      break;      

   case HB_INFILESIZE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_INFILESIZE, hb_parnl( 3 ) );
      break;               

   case HB_LOW_SPEED_TIME:
      res = curl_easy_setopt(pConn->curl, CURLOPT_LOW_SPEED_TIME, hb_parnl( 3 ) );
      break;               
   case HB_RESUME_FROM:
      res = curl_easy_setopt(pConn->curl, CURLOPT_RESUME_FROM, hb_parnl( 3 ) );
      break;               
   case HB_CRLF:
      res = curl_easy_setopt(pConn->curl, CURLOPT_CRLF, hb_parnl( 3 ) );
      break;               
   case HB_SSLVERSION:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLVERSION, hb_parnl( 3 ) );
      break;               
   case HB_TIMECONDITION:
      res = curl_easy_setopt(pConn->curl, CURLOPT_TIMECONDITION, hb_parnl( 3 ) );
      break;               
   case HB_TIMEVALUE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_TIMEVALUE, hb_parnl( 3 ) );
      break;                  
   case HB_VERBOSE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_VERBOSE, hb_parnl( 3 ) );
      break;               
   case HB_HEADER:
      res = curl_easy_setopt(pConn->curl, CURLOPT_HEADER, hb_parnl( 3 ) );
      break;               
   case HB_NOPROGRESS:
      res = curl_easy_setopt(pConn->curl, CURLOPT_NOPROGRESS, hb_parnl( 3 ) );
      break;               
   case HB_FAILONERROR:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FAILONERROR, hb_parnl( 3 ) );
      break;               
   case HB_UPLOAD:
      res = curl_easy_setopt(pConn->curl, CURLOPT_UPLOAD, hb_parnl( 3 ) );
      break;               
   case HB_POST:
      res = curl_easy_setopt(pConn->curl, CURLOPT_POST, hb_parnl( 3 ) );
      break;               
   case HB_FTPLISTONLY:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTPLISTONLY, hb_parnl( 3 ) );
      break;               
   case HB_FTPAPPEND:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTPAPPEND, hb_parnl( 3 ) );
      break;               
   case HB_NETRC:
      res = curl_easy_setopt(pConn->curl, CURLOPT_NETRC, hb_parnl( 3 ) );
      break;               
   case HB_FOLLOWLOCATION:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FOLLOWLOCATION, hb_parnl( 3 ) );
      break;               
   case HB_TRANSFERTEXT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_TRANSFERTEXT, hb_parnl( 3 ) );
      break;               
   case HB_PUT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PUT, hb_parnl( 3 ) );
      break;               
      
   case HB_AUTOREFERER:
      res = curl_easy_setopt(pConn->curl, CURLOPT_AUTOREFERER, hb_parnl( 3 ) );
      break;    
  
   case HB_PROXYPORT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PROXYPORT, hb_parnl( 3 ) );
      break;  

   case HB_POSTFIELDSIZE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_POSTFIELDSIZE, hb_parnl( 3 ) );
      break;
   
   case HB_HTTPPROXYTUNNEL:
      res = curl_easy_setopt(pConn->curl, CURLOPT_HTTPPROXYTUNNEL, hb_parnl( 3 ) );
      break;   
  
   case HB_SSL_VERIFYPEER:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSL_VERIFYPEER, hb_parnl( 3 ) );
      break;
    
   case HB_MAXREDIRS:
      res = curl_easy_setopt(pConn->curl, CURLOPT_MAXREDIRS, hb_parnl( 3 ) );
      break;    
  
   case HB_FILETIME:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FILETIME, hb_parnl( 3 ) );
      break;
    
   case HB_MAXCONNECTS:
      res = curl_easy_setopt(pConn->curl, CURLOPT_MAXCONNECTS, hb_parnl( 3 ) );
      break;
  
   case HB_CLOSEPOLICY:
      res = curl_easy_setopt(pConn->curl, CURLOPT_CLOSEPOLICY, hb_parnl( 3 ) );
      break;
  
   case HB_FRESH_CONNECT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FRESH_CONNECT, hb_parnl( 3 ) );
      break;
  
   case HB_FORBID_REUSE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FORBID_REUSE, hb_parnl( 3 ) );
      break;
  
   case HB_CONNECTTIMEOUT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_CONNECTTIMEOUT, hb_parnl( 3 ) );
      break;
  
   case HB_HTTPGET:
      res = curl_easy_setopt(pConn->curl, CURLOPT_HTTPGET, hb_parnl( 3 ) );
      break;
  
   case HB_SSL_VERIFYHOST:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSL_VERIFYHOST, hb_parnl( 3 ) );
      break;
   case HB_HTTP_VERSION:
      res = curl_easy_setopt(pConn->curl, CURLOPT_HTTP_VERSION, hb_parnl( 3 ) );
      break;
     
   case HB_FTP_USE_EPSV:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTP_USE_EPSV, hb_parnl( 3 ) );
      break;
    
   case HB_SSLENGINE_DEFAULT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLENGINE_DEFAULT, hb_parnl( 3 ) );
      break;
            
   case HB_DNS_USE_GLOBAL_CACHE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_DNS_USE_GLOBAL_CACHE, hb_parnl( 3 ) );
      break;
    
   case HB_DNS_CACHE_TIMEOUT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_DNS_CACHE_TIMEOUT, hb_parnl( 3 ) );
      break;
          
   case HB_COOKIESESSION:
      res = curl_easy_setopt(pConn->curl, CURLOPT_COOKIESESSION, hb_parnl( 3 ) );
      break;
    
   case HB_BUFFERSIZE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_BUFFERSIZE, hb_parnl( 3 ) );
      break;
  
   case HB_NOSIGNAL:
      res = curl_easy_setopt(pConn->curl, CURLOPT_NOSIGNAL, hb_parnl( 3 ) );
      break;
  
   case HB_PROXYTYPE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PROXYTYPE, hb_parnl( 3 ) );
      break;
  
   case HB_UNRESTRICTED_AUTH:
      res = curl_easy_setopt(pConn->curl, CURLOPT_UNRESTRICTED_AUTH, hb_parnl( 3 ) );
      break;
  
   case HB_FTP_USE_EPRT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTP_USE_EPRT, hb_parnl( 3 ) );
      break;
  
   case HB_HTTPAUTH:
      res = curl_easy_setopt(pConn->curl, CURLOPT_HTTPAUTH, hb_parnl( 3 ) );
      break;
  
   case HB_FTP_CREATE_MISSING_DIRS:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTP_CREATE_MISSING_DIRS, hb_parnl( 3 ) );
      break;
  
   case HB_PROXYAUTH:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PROXYAUTH, hb_parnl( 3 ) );
      break;
  
   case HB_FTP_RESPONSE_TIMEOUT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTP_RESPONSE_TIMEOUT , hb_parnl( 3 ) );
      break;
  
   case HB_IPRESOLVE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_IPRESOLVE, hb_parnl( 3 ) );
      break;
  
   case HB_MAXFILESIZE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_MAXFILESIZE, hb_parnl( 3 ) );
      break;
  
   case HB_FTP_SSL:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTP_SSL, hb_parnl( 3 ) );
      break;
  
   case HB_TCP_NODELAY:
      res = curl_easy_setopt(pConn->curl, CURLOPT_TCP_NODELAY, hb_parnl( 3 ) );
      break;
  
   case HB_FTPSSLAUTH:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTPSSLAUTH, hb_parnl( 3 ) );
      break;
      
   case HB_IGNORE_CONTENT_LENGTH:
      res = curl_easy_setopt(pConn->curl, CURLOPT_IGNORE_CONTENT_LENGTH, hb_parnl( 3 ) );
      break;
  
   case HB_URL:
      res = curl_easy_setopt(pConn->curl, CURLOPT_URL, hb_parcx( 3 ) );
      break;        

   case HB_PROXY:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PROXY, hb_parcx( 3 ) );
      break;        
    
   case HB_USERPWD:
      res = curl_easy_setopt(pConn->curl, CURLOPT_USERPWD, hb_parcx( 3 ) );
      break;        
  
   case HB_PROXYUSERPWD:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PROXYUSERPWD, hb_parcx( 3 ) );
      break;        
   
   case HB_RANGE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_RANGE, hb_parcx( 3 ) );
      break;        

   case HB_INFILE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_INFILE, pConn->szFile );
      break;        

   case HB_ERRORBUFFER:
      res = curl_easy_setopt(pConn->curl, CURLOPT_ERRORBUFFER, hb_parcx( 3 ) );
      break;        
      
   case HB_POSTFIELDS:
      res = curl_easy_setopt(pConn->curl, CURLOPT_POSTFIELDS , hb_parcx( 3 ) );
      break;              

   case HB_REFERER:
      res = curl_easy_setopt(pConn->curl, CURLOPT_REFERER , hb_parcx( 3 ) );
      break;        

   case HB_FTPPORT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTPPORT , hb_parcx( 3 ) );
      break;        

   case HB_USERAGENT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_USERAGENT , hb_parcx( 3 ) );
      break;        
        
   case HB_COOKIE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_COOKIE , hb_parcx( 3 ) );
      break;          
  
   case HB_HTTPHEADER :
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
  
   case HB_HTTPPOST:
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
  
   case HB_SSLCERT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLCERT , hb_parcx( 3 ) );
      break;          
  
   case HB_SSLKEYPASSWD:  
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLKEYPASSWD , hb_parcx( 3 ) );
      break;          
        
   case HB_QUOTE:
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
  
   case HB_WRITEHEADER:
      res = curl_easy_setopt(pConn->curl, CURLOPT_WRITEHEADER , hb_parcx( 3 ) ); //pointer or file *
      break;          
  
   case HB_COOKIEFILE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_COOKIEFILE , hb_parcx( 3 ) );
      break;          
        
   case HB_CUSTOMREQUEST:
      res = curl_easy_setopt(pConn->curl, CURLOPT_CUSTOMREQUEST , hb_parcx( 3 ) );
      break;         
  
   case HB_STDERR:
      res = curl_easy_setopt(pConn->curl, CURLOPT_STDERR , hb_parcx( 3 ) ); //File *
      break;           
  
   case HB_POSTQUOTE:
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
  
   case HB_WRITEINFO: //verificar
      res = curl_easy_setopt(pConn->curl, CURLOPT_WRITEINFO , hb_parcx( 3 ) );
      break;           
        
   case HB_PROGRESSDATA:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PROGRESSDATA , hb_parcx( 3 ) );
      break;             
  
   case HB_INTERFACE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_INTERFACE , hb_parcx( 3 ) );
      break;               
  
   case HB_KRB4LEVEL:
      res = curl_easy_setopt(pConn->curl, CURLOPT_KRB4LEVEL , hb_parcx( 3 ) );
      break;               
   
   case HB_CAINFO:
      res = curl_easy_setopt(pConn->curl, CURLOPT_CAINFO , hb_parcx( 3 ) );
      break;                 

   case HB_TELNETOPTIONS:
      res = curl_easy_setopt(pConn->curl, CURLOPT_TELNETOPTIONS , hb_parcx( 3 ) ); //usa curl_slist
      break;                 
  
   case HB_RANDOM_FILE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_RANDOM_FILE , hb_parcx( 3 ) ); 
      break;                   
  
   case HB_EGDSOCKET:
      res = curl_easy_setopt(pConn->curl, CURLOPT_EGDSOCKET , hb_parcx( 3 ) ); 
      break;                   
  
   case HB_COOKIEJAR:
      res = curl_easy_setopt(pConn->curl, CURLOPT_COOKIEJAR , hb_parcx( 3 ) ); 
      break;        
  
   case HB_SSL_CIPHER_LIST:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSL_CIPHER_LIST , hb_parcx( 3 ) ); 
      break;          
  
   case HB_SSLCERTTYPE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLCERTTYPE , hb_parcx( 3 ) ); 
      break;          
  
   case HB_SSLKEY:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLKEY , hb_parcx( 3 ) );
      break;          
  
   case HB_SSLKEYTYPE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLKEYTYPE , hb_parcx( 3 ) );
      break;          
  
   case HB_SSLENGINE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSLENGINE , hb_parcx( 3 ) ); 
      break;            
  
   case HB_PREQUOTE:      
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
  
   case HB_DEBUGDATA:
      res = curl_easy_setopt(pConn->curl, CURLOPT_DEBUGDATA , hb_parcx( 3 ) ); // usa pointer
      break;              
            
   case HB_CAPATH:
      res = curl_easy_setopt(pConn->curl, CURLOPT_CAPATH , hb_parcx( 3 ) ); 
      break; 
      
 /*  case HB_SHARE:
       res = curl_easy_setopt(pConn->curl, CURLOPT_SHARE , hb_parcx( 3 ) ); 
       break;              */
        
   case HB_ENCODING:
      res = curl_easy_setopt(pConn->curl, CURLOPT_ENCODING , hb_parcx( 3 ) ); 
      break;   
  
   case HB_PRIVATE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_PRIVATE , hb_parcx( 3 ) ); 
      break;   
          
   case HB_HTTP200ALIASES:
      res = curl_easy_setopt(pConn->curl, CURLOPT_HTTP200ALIASES , hb_parcx( 3 ) );  // USA struct curl_slist structs
      break;   

   case HB_SSL_CTX_DATA:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SSL_CTX_DATA , hb_parcx( 3 ) );  //usa pointer
      break;      
      
   case HB_NETRC_FILE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_NETRC_FILE , hb_parcx( 3 ) ); 
      break;   
  
   case HB_SOURCE_USERPWD:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SOURCE_USERPWD , hb_parcx( 3 ) ); 
      break;     
  
   case HB_SOURCE_PREQUOTE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SOURCE_PREQUOTE , hb_parcx( 3 ) ); //usa pointer
      break;   
  
   case HB_SOURCE_POSTQUOTE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SOURCE_POSTQUOTE , hb_parcx( 3 ) );  //usa pointer
      break;     
      
   case HB_IOCTLDATA:
      res = curl_easy_setopt(pConn->curl, CURLOPT_IOCTLDATA , hb_parcx( 3 ) );  //pointer
      break;       

   case HB_SOURCE_URL:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SOURCE_URL , hb_parcx( 3 ) ); 
      break;     
  
   case HB_SOURCE_QUOTE:
      res = curl_easy_setopt(pConn->curl, CURLOPT_SOURCE_QUOTE , hb_parcx( 3 ) ); 
      break;     
  
   case HB_FTP_ACCOUNT:
      res = curl_easy_setopt(pConn->curl, CURLOPT_FTP_ACCOUNT , hb_parcx( 3 ) ); 
      break;     
  
   case HB_COOKIELIST:
      res = curl_easy_setopt(pConn->curl, CURLOPT_COOKIELIST , hb_parcx( 3 ) ); 
      break;
   case HB_SETUPLOADFILE:
   {
      res = 1;

      if ( pConn->szFile )
      {
         fclose( pConn->szFile );
      }

      pConn->szFile = fopen( hb_parc( 3 ), "rb" );
   }
      break;
   case HB_SETDOWNLOADFILE:
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
   case HB_CLOSEUPLOADFILE:
   {
      res = 1;
      fclose( pConn->szFile );
   }
      break;
   case HB_SETPROGRESS:
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
