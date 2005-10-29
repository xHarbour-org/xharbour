#include "hbcurl.ch"

#define TRUE 1
#define FALSE 0
#define LOCAL_FILE      "test1"
#define UPLOAD_FILE_AS  "while-uploading.txt"
#define REMOTE_URL      "ftp://localhost/"+UPLOAD_FILE_AS
#define RENAME_FILE_TO  "renamed-and-fine.txt"
#define CURL_GLOBAL_SSL (1<<0)
#define CURL_GLOBAL_WIN32 (1<<1)
#define CURL_GLOBAL_ALL (CURL_GLOBAL_SSL|CURL_GLOBAL_WIN32)
#define CURL_GLOBAL_NOTHING 0
#define CURL_GLOBAL_DEFAULT CURL_GLOBAL_ALL



function main
  Local nh
  Local aGauge
  Local nSize := filesize("test1")
  Local c
  aGauge := GaugeNew( 4, 5,6,74 , "W/B", "W+/B" ,'²')
  GaugeDisplay( aGauge )

  nh := CURL_EASY_INIT()


  curl_easy_setopt(nh, HB_CURLOPT_SETUPLOADFILE, "test1") 

  /* In windows, this will init the winsock stuff */
  c:=curl_global_init(CURL_GLOBAL_ALL)

  if !empty(nh) 

    curl_easy_setopt(nh, HB_CURLOPT_UPLOAD, TRUE) 


    curl_easy_setopt(nh,HB_CURLOPT_URL, REMOTE_URL)
//    curl_easy_setopt(nh, HB_CURLOPT_VERBOSE, 1)
    curl_easy_setopt(nh, HB_CURLOPT_USERPWD, "rafael:kl6qaxv9")
    curl_easy_setopt(nh, HB_CURLOPT_SETPROGRESS,{|nPos,nLen| GaugeUpdate(aGauge,(nPos/nLen)*100) })
    curl_easy_setopt(nh, HB_CURLOPT_NOPROGRESS, FALSE);    

    curl_easy_setopt(nh, HB_CURLOPT_POSTQUOTE, {"RNFR " +UPLOAD_FILE_AS,"RNTO " + RENAME_FILE_TO})

    curl_easy_setopt(nh, HB_CURLOPT_INFILE, "")
    if (nSize <  (1024*1024)*2000)
    curl_easy_setopt(nh, HB_CURLOPT_INFILESIZE,nSize)    
    else
    curl_easy_setopt(nh, HB_CURLOPT_INFILESIZE_LARGE,nSize)
    endif

    res = curl_easy_perform(nh)    
    curl_easy_setopt(nh, HB_CURLOPT_CLOSEUPLOADFILE) 
    // Now lets download an file
    curl_easy_setopt(nh, HB_CURLOPT_UPLOAD, FALSE)     
    curl_easy_setopt(nh, HB_CURLOPT_URL,"ftp://localhost/xbuild.zip")
    curl_easy_setopt(nh,HB_CURLOPT_SETDOWNLOADFILE,"xbuildtest.zip")
    res = curl_easy_perform(nh)    
    /* always cleanup */
    curl_easy_cleanup(nh)

  endif


  curl_global_cleanup()
  return nil

