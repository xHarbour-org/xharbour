///// comm.h
/////     purpose : prototypes for for TCommPort, serial communictaions API encapsulation
/////    copyright: Harold Howe, bcbdev.com 1996-1999.
/////    notice   : This file provides an object that encapsulates the win32 serial port routines.
/////               This file may be distributed and used freely for program development,
/////               but it may not be resold as part of a software development library.
/////               In other words, don't take the source and attempt to sell it to other developers.
#if defined (HBCOMM_HAS_DLL)
#    if defined (HBCOMM_BUILD_DLL)
#      define HBCOMM_API __declspec (dllexport)
#    else
#      define HBCOMM_API  __declspec (dllimport)
#    endif /* ZIP_BUILD_DLL */
#else
#  define HBCOMM_API
#endif     /* ZIP_HAS_DLL */

#ifndef COMM_H
#define COMM_H
#define       EHANDSHAKEOFF       0
#define       EHANDSHAKEHARDWARE  1
#define       EHANDSHAKESOFTWARE  2
#define       EHANDSHAKEHARDWAREDTR  3
#define       EHANDSHAKEHARDWARERTS  4


#include <windows.h>
#include <string>


// When the comm port class encounters an error, it throws an ECommError exception.
// The Error member of the exception object describes what went wrong. Some of the
// items should never happen. Others are fairly common place. You should pay special
// attention to the OPEN_ERROR type. This is the error that occurs when opening the port
// fails because another app already has the port open.
class HBCOMM_API ECommError
{
public:
    enum ErrorType
    {
        BAD_SERIAL_PORT    ,
        BAD_BAUD_RATE      ,
        BAD_PORT_NUMBER    ,
        BAD_STOP_BITS      ,
        BAD_PARITY         ,
        BAD_BYTESIZE       ,
        PORT_ALREADY_OPEN  ,
        PORT_NOT_OPEN      ,
        OPEN_ERROR         ,
        WRITE_ERROR        ,
        READ_ERROR         ,
        CLOSE_ERROR        ,
        PURGECOMM          ,
        FLUSHFILEBUFFERS   ,
        GETCOMMSTATE       ,
        SETCOMMSTATE       ,
        SETUPCOMM          ,
        SETCOMMTIMEOUTS    ,
        CLEARCOMMERROR     ,
        SETCOMMOPTIONS     ,
        MODEMSTATUSERROR   
    };

    ECommError( ErrorType error);

    ErrorType Error;
    DWORD     Errno;   // Errno == return value from GetLastError. Can be used with FormatMessage
};

class HBCOMM_API TCommPort
{
  public:
    TCommPort();
    ~TCommPort();
    void OpenCommPort(void);
    void CloseCommPort(void);

    void SetCommPort(const std::string & port);
    std::string GetCommPort(void);
    void SetBaudRate(unsigned int newBaud);
    unsigned int GetBaudRate(void);
    void SetParity(BYTE newParity); // see source for possible values
    BYTE GetParity(void);
    void SetByteSize(BYTE newByteSize);
    BYTE GetByteSize(void);
    void SetStopBits(BYTE newStopBits);
    BYTE GetStopBits(void);

    void SetCommDCBProperties(DCB &properties);  // avoid using DCB interface
    void GetCommDCBProperties(DCB &properties);  // Use SetBaudRate et al instead

    void GetCommProperties(COMMPROP &properties);

    void WriteString(const char *outString);
    void WriteBuffer(BYTE  *buffer, unsigned int ByteCount);
    void WriteBufferSlowly(BYTE *buffer, unsigned int ByteCount);
    int ReadString(char *string, unsigned int MaxBytes);
    int ReadBytes(BYTE *bytes, unsigned int byteCount);
    void DiscardBytes(unsigned int MaxBytes);
    void PurgeCommPort(void);
    void PurgeOCommPort(void);
    void FlushCommPort(void);

    void  PutByte(BYTE value);
    BYTE  GetByte();
    unsigned int BytesAvailable(void);
    unsigned int BytesOAvailable(void);
    bool GetConnected()
    {
        return m_CommOpen;
    }

    HANDLE GetHandle() // allow access to the handle in case the user needs to
    {                  // do something hardcore. Avoid this if possible
        return m_hCom;
    }
   DWORD dwBufferSize ;
   //New Methods
   void SetRts(unsigned int newBaud);
   void SetDtr(unsigned int newBaud);
   BOOL ConfigDialog(void);
   void OpenPort(void);
   void SetCommOptions( int iOption);
   void SetupHandshaking(int eHandshake);
   void GetPortStatus(DWORD *a);
  private:
    // Note: the destructor of the commport class automatically closes the
    //       port. This makes copy construction and assignment impossible.
    //       That is why I privatize them, and don't define them. In order
    //       to make copy construction and assignment feasible, we would need
    //       to employ a reference counting scheme.
    TCommPort(const TCommPort &);            // privatize copy construction
    TCommPort & operator=(const TCommPort&);  // and assignment.

    void VerifyOpen()
    {
        if(!m_CommOpen)
            throw ECommError(ECommError::PORT_NOT_OPEN) ;
    }
    void VerifyClosed()
    {
        if(m_CommOpen)
            throw ECommError(ECommError::PORT_ALREADY_OPEN) ;
    }

  // this stuff is private because we want to hide these details from clients
    bool          m_CommOpen;
    COMMTIMEOUTS  m_TimeOuts;
    std::string   m_CommPort;
    DCB           m_dcb;        // a DCB is a windows structure used for configuring the port
    HANDLE        m_hCom;       // handle to the comm port.
    COMMCONFIG    m_config;
};

#endif
