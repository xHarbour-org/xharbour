  #define HB_FILE 1
  #define HB_URL  2
  #define HB_PORT 3
  #define HB_PROXY 4
  #define HB_USERPWD 5
  #define HB_PROXYUSERPWD 6
  #define HB_RANGE 7
  #define HB_INFILE 9
  #define HB_ERRORBUFFER 10
  #define HB_WRITEFUNCTION 11
  #define HB_READFUNCTION 12
  #define HB_TIMEOUT 13
  #define HB_INFILESIZE 14
  #define HB_POSTFIELDS 15
  #define HB_REFERER 16
  #define HB_FTPPORT 17
  #define HB_USERAGENT 18
  #define HB_LOW_SPEED_LIMIT 19
  #define HB_LOW_SPEED_TIME 20
  #define HB_RESUME_FROM 21
  #define HB_COOKIE 22
  #define HB_HTTPHEADER 23
  #define HB_HTTPPOST 24
  #define HB_SSLCERT 25
  #define HB_SSLCERTPASSWD 26
  #define HB_SSLKEYPASSWD 26
  #define HB_CRLF 27
  #define HB_QUOTE 28
  #define HB_WRITEHEADER 29
  #define HB_COOKIEFILE 31
  #define HB_SSLVERSION 32
  #define HB_TIMECONDITION 33
  #define HB_TIMEVALUE 34
  #define HB_CUSTOMREQUEST 36
  #define HB_STDERR 37
  #define HB_POSTQUOTE 39
  #define HB_WRITEINFO 40
  #define HB_VERBOSE 41      /* talk a lot */
  #define HB_HEADER 42       /* throw the header out too */
  #define HB_NOPROGRESS 43   /* shut off the progress meter */
  #define HB_NOBODY 44       /* use HEAD to get http document */
  #define HB_FAILONERROR 45  /* no output on http error codes >= 300 */
  #define HB_UPLOAD 46       /* this is an upload */
  #define HB_POST 47         /* HTTP POST method */
  #define HB_FTPLISTONLY 48  /* Use NLST when listing ftp dir */
  #define HB_FTPAPPEND 50    /* Append instead of overwrite on upload! */
  #define HB_NETRC 51
  #define HB_FOLLOWLOCATION 52  /* use Location: Luke! */
  #define HB_TRANSFERTEXT 53 /* transfer data in text/ASCII format */
  #define HB_PUT 54          /* HTTP PUT */
  #define HB_PROGRESSFUNCTION 56
  #define HB_PROGRESSDATA 57
  #define HB_AUTOREFERER 58
  #define HB_PROXYPORT 59
  #define HB_POSTFIELDSIZE 60
  #define HB_HTTPPROXYTUNNEL 61
  #define HB_INTERFACE 62
  #define HB_KRB4LEVEL 63
  #define HB_SSL_VERIFYPEER 64
  #define HB_CAINFO 65
  #define HB_MAXREDIRS 68
  #define HB_FILETIME 69
  #define HB_TELNETOPTIONS 70
  #define HB_MAXCONNECTS 71
  #define HB_CLOSEPOLICY 72
  #define HB_FRESH_CONNECT 74
  #define HB_FORBID_REUSE 75
  #define HB_RANDOM_FILE 76
  #define HB_EGDSOCKET 77
  #define HB_CONNECTTIMEOUT 78
  #define HB_HEADERFUNCTION 79
  #define HB_HTTPGET 80
  #define HB_SSL_VERIFYHOST 81
  #define HB_COOKIEJAR 82
  #define HB_SSL_CIPHER_LIST 83
  #define HB_HTTP_VERSION 84
  #define HB_FTP_USE_EPSV 85
  #define HB_SSLCERTTYPE 86
  #define HB_SSLKEY 87
  #define HB_SSLKEYTYPE 88
  #define HB_SSLENGINE 89
  #define HB_SSLENGINE_DEFAULT 90
  #define HB_DNS_USE_GLOBAL_CACHE 91 /* To becomeO BSOLETE soon */
  #define HB_DNS_CACHE_TIMEOUT 92
  #define HB_PREQUOTE 93
  #define HB_DEBUGFUNCTION 94
  #define HB_DEBUGDATA 95
  #define HB_COOKIESESSION 96
  #define HB_CAPATH 97
  #define HB_BUFFERSIZE 98
  #define HB_NOSIGNAL 99
  #define HB_SHARE 100
  #define HB_PROXYTYPE 101
  #define HB_ENCODING 102
  #define HB_PRIVATE 103
  #define HB_HTTP200ALIASES 104
  #define HB_UNRESTRICTED_AUTH 105
  #define HB_FTP_USE_EPRT 106
  #define HB_HTTPAUTH 107
  #define HB_SSL_CTX_FUNCTION 108
  #define HB_SSL_CTX_DATA 109
  #define HB_FTP_CREATE_MISSING_DIRS 110
  #define HB_PROXYAUTH 111
  #define HB_FTP_RESPONSE_TIMEOUT 112
  #define HB_IPRESOLVE 113
  #define HB_MAXFILESIZE 114
  #define HB_INFILESIZE_LARGE 115
  #define HB_RESUME_FROM_LARGE 116
  #define HB_MAXFILESIZE_LARGE 117
  #define HB_NETRC_FILE 118
  #define HB_FTP_SSL 119
  #define HB_POSTFIELDSIZE_LARGE 120
  #define HB_TCP_NODELAY 121
  #define HB_SOURCE_USERPWD 123
  #define HB_SOURCE_PREQUOTE 127
  #define HB_SOURCE_POSTQUOTE 128
  #define HB_FTPSSLAUTH 129
  #define HB_IOCTLFUNCTION 130
  #define HB_IOCTLDATA 131
  #define HB_SOURCE_URL 132
  #define HB_SOURCE_QUOTE 133
  #define HB_FTP_ACCOUNT 134
  #define HB_COOKIELIST 135
  #define HB_IGNORE_CONTENT_LENGTH 136
  #define HB_SETUPLOADFILE 137
  #define HB_CLOSEUPLOADFILE 138
  #define HB_SETPROGRESS 139
  #define HB_SETDOWNLOADFILE 140