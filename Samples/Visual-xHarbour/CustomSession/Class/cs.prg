/*
   CustomSession version 1.3 - 2010 - author Ella Ilona Stern
 
   TODO columns
   1 = job type: C = compiled, S = script
   2 = filename ( usually an executable, or the URL of a script - ShellExecute also supports document types with a program associated to the file suffix )
   3 = filename for job results ( in case of scripts and the browser present, a copy of the document found in the browser when OnDocumentComplete is fired )
   4 = job identifier - for notices
   5 = first launch after time value in [7] - in time format ( "00:00:00"=now; NIL=stopped )
   6 = launching frequency - in time format ( NIL=no repeated launches )
   7 = last launched - in time format
   8 = launching condition: with or without passing a session file name as first parameter ( "Y" or "N" )
   9 = error condition: E = exclude job from list; R = retry before excluding; C = continue
  10 = Notify - show "balloon" style error message over the tray
  11 = retry count  in case of error - the maximum number of trials
  12 = retry number in case of error - currently set ( -1 = in error management; 0 no problem; >0 retrying before being excluded )
  13 = job query list ( in case of compiled programs it's the command line parameter list )
  14 = job tick - base for launching frequency
   
   STATUS values
   I = initialized
   U = up - in process of starting or connecting
   W = waiting
   B = busy - performing a read or write on the harddisk
   L = looping - server only - in process of checking the TODO list
   D = down - in process of stopping or disconnecting
   S = stopped or disconnected       
*/   

#include "hbclass.ch"
#include "fileio.ch"
#ifndef SW_SHOW
   #define SW_SHOW 5
#endif

#define CUSE_JOBTYPE   1
#define CUSE_JOBFILE   2
#define CUSE_JOBRESP   3
#define CUSE_JOBID     4
#define CUSE_JOBNEXT   5
#define CUSE_JOBFREQ   6
#define CUSE_JOBLAST   7
#define CUSE_JOBSEND   8
#define CUSE_JOBERAC   9
#define CUSE_JOBERNO  10
#define CUSE_JOBERRC  11
#define CUSE_JOBERRN  12
#define CUSE_JOBQL    13
#define CUSE_JOBTICK  14

#define CUSE_SCRURL    1
#define CUSE_SCRRES    2
#define CUSE_SCRID     3
#define CUSE_SCRERR    4

class CustomSession
hidden:
   Data ID                     // Server or Client
   Data TimeOut init .f.       // when clients aren't checking in for a while, the server stops
   Data File init ""           // session file name
   Data CTime init "00:00:00"  // time of latest client check in
   Data STime init "00:00:00"  // time of latest server check in
   Data Notice init hash()     // keys are job identifiers in the server's Todo list
   Data Handle init -1         // session file handle
   Data BS init 128            // buffer size - don't alter it without good reason
   Data Status init "I"        // server or clent status
   Data Tick init "00:01:00"   // time period for checking in
   Data Last init "00:00:00"   // last write operation in the session file
   Data From init "00:00:00"   // session starting time
   Data Limit init "00:05:00"  // session time-out; it should be 2-3 times the Tick value
   Data Browser init NIL       // server only - browser object desired
   Data Notify  init NIL       // server only - Balloon notifycations desired
   Data Todo init {}           // server only - job list
   Data Docs init {"", "", "", 0} // 1 = Url (via http) ; 2 = local file ; 3 = job identifier ; 4 = number of errors
   Method IsTimingOk()
   Method Loop()
   Method JobPos()
   Method SetNotice()
   Method ManageError()
exported:
   Data EM init ""             // last set error message
   Method Init() constructor
   Method StartServer()
   Method StopServer()
   Method Read()
   Method Write()  
   Method Client()
   Method Server()   
   Method IsTime()
   Method InsertJob()
   Method Connect()
   Method Disconnect()
   Method SaveDoc()
   Method GetNotice()
   Method GetTimes() inline { ::STime, ::CTime }
   Method GetJobs()
   Method SetJobQuery()
   Method SetJobResult()
   Method SetJobErrMan()
   Method SetJobSchedule()
   Method SetJobStatus()
   Method SetScriptError()
   Method SetJobNotify()
endclass



METHOD Init( lServer, lTimeOut, oBrowser, oNotify ) CLASS CustomSession
   if pCount()<1
      alert( "Missing CustomSession Init parameter(s)" )
      return NIL
   endif
   if valtype(lServer) <> "L"
      alert( "Wrong CustomSession type" )
      return NIL
   endif
   if lTimeOut == NIL
      lTimeOut := .f.
   endif
   if valtype( lTimeOut )<>"L"
      alert( "Wrong timeout value" )
      return NIL
   endif
   if lServer .and. oBrowser<>NIL .and. valtype(oBrowser)<>"O"
      alert("Wrong CustomSession Browser reference")
      return NIL
   endif   
   if lServer .and. oNotify<>NIL .and. valtype(oNotify)<>"O"
      alert("Wrong NotifyIcon reference")
      return NIL
   endif   
   
   if lTimeOut == .t.
      ::TimeOut := .t.
   endif
   if lServer .and. oBrowser<>NIL
      ::Browser := oBrowser   // InternetExplorer
   endif
   if lServer .and. oNotify<>NIL
      ::Notify := oNotify     // the NotifyIcon
   endif
   ::ID := if( lServer, "S", "C" )
RETURN Self



METHOD GetJobs() CLASS CustomSession
   local i, aJobs:=array( len(::Todo), 3 )
   for i:=1 to len(::Todo)
      aJobs[i, 1] := ::Todo[i, CUSE_JOBID]
      aJobs[i, 2] := if( ::Todo[i, CUSE_JOBNEXT]==NIL, .f., .t. ) // started
      aJobs[i, 3] := ::Todo[i, CUSE_JOBERNO]
   next
RETURN aJobs



METHOD SetJobNotify( cJobId, lNotify ) CLASS CustomSession
   local nPos
   nPos := ::JobPos( cJobId )
   if nPos < 1
      return .f.
   endif
   ::Todo[nPos, CUSE_JOBERNO] := lNotify
RETURN .t.



METHOD SetJobStatus( cJobId, lStart ) CLASS CustomSession
   local nPos
   nPos := ::JobPos( cJobId )
   if nPos < 1
      return .f.
   endif
   
   if lStart // start job
      
      if ::Todo[nPos, CUSE_JOBTICK] == NIL
         ::EM := cJobId + " - job schedule is not set - cannot start"
         return .f.
      endif
      
      ::Todo[nPos, CUSE_JOBNEXT] := "00:00:00"
      ::Todo[nPos, CUSE_JOBFREQ] := ::Todo[nPos, CUSE_JOBTICK]
      ::Todo[nPos, CUSE_JOBERRN] := 0
      if ::Todo[nPos, CUSE_JOBERAC]=="E" .and. ::Todo[nPos, CUSE_JOBERRC] > 0
         ::Todo[nPos, CUSE_JOBERAC] := "R"
      endif
      
   else      // stop job
      ::Todo[nPos, CUSE_JOBNEXT] := NIL
      ::Todo[nPos, CUSE_JOBFREQ] := NIL
      ::Todo[nPos, CUSE_JOBERRN] := 0
   endif
RETURN .t.



METHOD ManageError( cJobId ) CLASS CustomSession
   local nPos, cAction, nRep
   nPos := ::JobPos( cJobId )
   if nPos < 1
      return .f.
   endif
   cAction := ::Todo[nPos, CUSE_JOBERAC]  // continue
   
   if cAction == "R"  // retry before excluding
      ::Todo[nPos, CUSE_JOBERAC] := "E"
      ::Todo[nPos, CUSE_JOBERRN] := ::Todo[nPos, CUSE_JOBERRC]
   endif
   
   if cAction == "E"  // exclude
      nRep := --::Todo[nPos, CUSE_JOBERRN]
      if nRep < 1
         ::Todo[nPos, CUSE_JOBNEXT] := NIL
         ::Todo[nPos, CUSE_JOBFREQ] := NIL
         ::Todo[nPos, CUSE_JOBERRN] := 0
      endif
   endif
   
   if ::Todo[nPos, CUSE_JOBERNO] == .t.  // notify
      with object ::Notify    // specific to VxH
         :Visible := .f.
         :BalloonTipTitle := "CustomSession error - job: " + cJobId
         :BalloonTipText := ::EM
         :Visible := .t.
      end
   endif   
RETURN .t.



METHOD JobPos( cJobId ) CLASS CustomSession
   local i, nPos, cJob
   if valtype( cJobId ) <> "C"
      ::EM += " - Wrong job ID"
      return 0
   endif
   cJob:=upper( alltrim( cJobId ) )
   if empty( cJob )
      ::EM += " - Empty job ID"
      return 0
   endif
   nPos := 0
   for i:=1 to len( ::Todo )
      if cJob==::Todo[i, CUSE_JOBID] .and. len(cJob)==len(::Todo[i, CUSE_JOBID])
         nPos := i
         exit
      endif
   next
RETURN nPos



METHOD SetJobQuery( cJobId, xList ) CLASS CustomSession
   local i, cList:="", elem, lGet, nLen
   local nPos := ::JobPos( cJobId ), cType:=valtype( xList ) 
   if nPos < 1
      return .f.
   endif
   if !( cType $ "CH" )
      ::EM := "Wrong list type: " + cType
      return .f.
   endif
   
   if cType=="C"
      xList := alltrim( xList )
      if empty( xList )
         ::EM := "Empty parameter list"
         return .f.
      endif
      ::Todo[nPos, CUSE_JOBQL] := xList
      return .t.
   endif
   
   nLen := len( xList )
   if nLen < 1 // hash
      ::EM := "Empty parameter list"
      return .f.
   endif
   
   lGet := if( ::Todo[nPos, CUSE_JOBTYPE]=="S", .t., .f. )
   for i:=1 to nLen
      elem := hgetpairat( xList, i )
      if lGet
         cList += cstr(elem[1]) + "=" + cstr(elem[2]) + if( i<nLen, "&", "" )
      else
         cList += cstr(elem[2]) +  if( i<nLen, " ", "" )
      endif
   next   
   ::Todo[nPos, CUSE_JOBQL] := cList   
RETURN .t.



METHOD SetJobResult( cJobId, cFile ) CLASS CustomSession
   local nPos := ::JobPos( cJobId )
   if nPos < 1
      return .f.
   endif   
   if valtype(cFile)<>"C"
      ::EM := "Wrong file name"
      return .f.
   endif
   cFile := alltrim( cFile )
   
   ::Todo[nPos, CUSE_JOBRESP] := cFile
RETURN .t.



METHOD SetJobErrMan( cJobId, cAction, nTrials, lNotify ) CLASS CustomSession
   local nPos := ::JobPos( cJobId )
   if nPos < 1
      return .f.
   endif
   cAction := upper( cAction )
   if !( cAction $ "ERC" )
      ::EM := "Wrong 'Action on error' type: " + cAction
      return .f.
   endif
   if nTrials==NIL .or. cAction<>"R"
      nTrials := 0
   endif
   if nTrials<0 .or. nTrials>5
      ::EM := "Wrong retry value: " + str(nTrials)
      return .f.
   endif
   if lNotify<>NIL .and. valtype( lNotify )<>"L"
      ::EM := "Wrong notification value " + cStr( lNotify )
      return .f.      
   endif
   if ::Notify==NIL .or. lNotify==NIL
      lNotify := .f.
   endif
   
   ::Todo[nPos, CUSE_JOBERAC] := cAction
   ::Todo[nPos, CUSE_JOBERNO] := lNotify
   ::Todo[nPos, CUSE_JOBERRC] := nTrials
   ::Todo[nPos, CUSE_JOBERRN] := 0
RETURN .t.



METHOD SetJobSchedule( cJobId, cNext, cFreq ) CLASS CustomSession
   local nPos := ::JobPos( cJobId )
   if nPos < 1
      return .f.
   endif 
   if cNext<>NIL .and. !( valtype(cNext)=="C" .and. ::IsTime(cNext) )
      ::EM := "Wrong starting time"
      return .f.
   endif
   if cFreq<>NIL
      if !( valtype(cFreq)=="C" .and. ::IsTime(cFreq) ) .or. cFreq=="00:00:00"
         ::EM := "Wrong execution frequency"
         return .f.
      endif
   endif
   
   ::Todo[nPos, CUSE_JOBNEXT] := cNext
   ::Todo[nPos, CUSE_JOBFREQ] := cFreq
   ::Todo[nPos, CUSE_JOBTICK] := cFreq   
RETURN .t.



METHOD SetScriptError( lYes ) CLASS CustomSession
   if !empty(::Docs[CUSE_SCRID])
      if lYes
         ::Docs[CUSE_SCRERR]++
      else
         ::Docs[CUSE_SCRERR] := 0
      endif
   endif
RETURN Self



METHOD IsTimingOk( cTick, cLimit ) CLASS CustomSession  
   if cTick <> NIL
      if !::IsTime(cTick) .or. cTick<"00:00:30" .or. cTick>"00:05:00"
         ::EM := "Wrong check-in time period, or out of limits: " + cTick
         return .f.
      endif
      ::Tick := cTick
   endif
   if cLimit <> NIL
      if !::IsTime(cLimit) .or. cLimit<"00:01:00" .or. cLimit>"00:10:00"
         ::EM := "Wrong session time-out value, or out of limits: " + cLimit
         return .f.
      endif
      ::Limit := cLimit
   endif
return .t.



METHOD GetNotice( cKey ) CLASS CustomSession
   if pcount()<>1 .or. valtype(cKey)<>"C" .or. empty(alltrim(cKey))
      ::EM := "Wrong GetNotice parameter"
      return NIL
   endif
   if !hhaskey( ::Notice, cKey )
      ::EM := "Inexistent key: " + cKey
      return NIL
   endif
return ::Notice[cKey]



METHOD SetNotice( cKey, cVal ) CLASS CustomSession
   if ::ID == "C"
      ::EM := "Wrong method call"
      return .f.
   endif
   if pcount()<>2 .or. valtype(cKey)<>"C" .or. valtype(cVal)<>"C" .or. empty(alltrim(cKey))
      ::EM := "Wrong SetNotice parameter(s)"
      return .f.
   endif
   if !hhaskey( ::Notice, cKey )
      ::EM := "Inexistent key: " + cKey
      return NIL
   endif
   ::Notice[cKey] := cVal
return .t.



METHOD SaveDoc() CLASS CustomSession
   local i, lOk, cStatus
   if ::ID == "C"
      ::EM := "Wrong method call"
      return .f.
   endif
   if ::Browser == NIL
      ::EM := "Wrong context - no Browser object"
      return .f.
   endif
   if empty( ::Docs[CUSE_SCRID] )  // no script
      return .t.
   endif
   if valtype( ::Browser:Document ) <> "O"
      ::EM := "No document in browser"
      return .f.
   endif
   
   if empty( ::Docs[CUSE_SCRRES] ) // no need to save the response
      
      lOk := .t.
      if ::Docs[CUSE_SCRERR]<1
         ::SetNotice( ::Docs[CUSE_SCRID], time() )
      else
         ::EM := "Navigation error "+::Docs[CUSE_SCRURL]
         lOk := .f.
      endif

   else               // proper saving

      cStatus := ::Status
      ::Status := "B"
      for i:=1 to 3
         lOk := memowrit( ::Docs[CUSE_SCRRES], ::Browser:Document:Body:InnerText )
         if lOk
            exit
         endif
         threadsleep(200)
      next   
      ::Status := cStatus  
   
      if lOk 
         if ::Docs[CUSE_SCRERR]<1
            ::SetNotice( ::Docs[CUSE_SCRID], time() )
         else
            ::EM := "Navigation error in " + ::Docs[CUSE_SCRURL]
            lOk := .f.
         endif
      else
         ::EM := "Couldn't save in " + ::Docs[CUSE_SCRRES]
      endif
      
   endif    // end proper saving
   
   if !lOk
      ::ManageError( ::Docs[CUSE_SCRID] )
   endif
   
   ::Docs[CUSE_SCRURL] := ""
   ::Docs[CUSE_SCRRES] := ""
   ::Docs[CUSE_SCRID] := ""
   ::Docs[CUSE_SCRERR] := 0
   
Return lOk



METHOD IsTime( cVal ) CLASS CustomSession
   local cSub
   cVal := alltrim( cVal )
   if len(cVal)<>8 .or. cVal[3]<>":" .or. cVal[6]<>":"
      return .f.
   endif
   cSub := left(cVal, 2)
   if cSub<"00" .or. cSub>"23"
      return .f.
   endif
   cSub := substr( cVal, 4, 2)
   if cSub<"00" .or. cSub>"59"
      return .f.
   endif
   cSub := substr( cVal, 7, 2)
   if cSub<"00" .or. cSub>"59"
      return .f.
   endif   
return .t.



METHOD InsertJob( cJob, cAction, cFile, cResponse, cTime, cFreq, cSend, cError, lNotify, nTrials ) CLASS CustomSession
   local i, lOk
   if ::ID == "C"
      ::EM := "Wrong method call"
      return .f.
   endif

   if pcount() < 3
      ::EM := "Insufficient input parameters"
      return .f.
   endif
   if valtype(cJob)<>"C" .or. empty(alltrim(cJob))
      ::EM := "Wrong job identifier"
      return .f.
   else
      cJob := upper( cJob )
   endif
   if ( cJob like "[A-Z0-9_]{1,20}" ) == .F.
      ::EM := "Wrong job id: " + cJob
      return .f.
   endif
   
   if valtype(cAction)<>"C" .or. empty(alltrim(cAction)) .or. !( upper(cAction) $ "CS" )
      ::EM := "Wrong job type: " + cAction
      return .f.
   else
      cAction := upper( cAction )
   endif
   if ::Browser==NIL .and. cAction=="S"
      ::EM := "Wrong context (no browser) for script"
      return .f.
   endif
   
   if valtype(cFile)<>"C" .or. empty(cFile)
      ::EM := "Wrong file name"
      return .f.
   endif
   if cResponse == NIL 
      cResponse := ""
   else
      if valtype(cResponse)<>"C"
         ::EM := "Wrong response file"
         return .f.
      endif
   endif

   if cTime<>NIL .and. !( valtype(cTime)=="C" .and. ::IsTime(cTime) )
      ::EM := "Wrong starting time"
      return .f.
   endif
   if cFreq<>NIL
      if !( valtype(cFreq)=="C" .and. ::IsTime(cFreq) ) .or. cFreq=="00:00:00"
         ::EM := "Wrong execution frequency"
         return .f.
      endif
   endif
   
   if cSend==NIL
      cSend := "N"   // by default no session information is passed to executables
   else
      if valtype( cSend )<>"C" .or. !( upper(cSend) $ "YN" )
         ::EM := "Wrong session information passing option"
         return .f.
      endif
      cSend := upper( cSend )
   endif
   
   if cError==NIL
      cError := "E"  // by default in case of error the job is excluded from the joblist
   else
      if valtype( cError )<>"C" .or. !( upper(cError) $ "ERC" )
         ::EM := "Incorrect error condition handling option"
         return .f.
      endif
      cError := upper( cError )
   endif

   if cError == "E"         // exclude
      nTrials := 0
   elseif cError == "R"     // retry
      if nTrials==NIL .or. valtype(nTrials)<>"N" .or. nTrials<0 .or. nTrials>5
         ::EM := "Wrong retry value: " + str(nTrials)
         return .f.
      endif
   else                     // continue
      nTrials := 0
   endif
   
   if lNotify<>NIL .and. valtype( lNotify ) <> "L"
      ::EM := "Wrong notification value " + cStr( lNotify )
      return .f.      
   endif
   if ::Notify==NIL .or. lNotify==NIL
      lNotify := .f.
   endif
   
   lOk := .t.
   for i:=1 to len( ::Todo )
      if cJob==::Todo[i, CUSE_JOBID] .and. len(cJob)==len(::Todo[i, CUSE_JOBID])
         lOk := .f.
         exit
      endif
   next
   if !lOk
      ::EM := "Job identifier already exists: " + cJob
      return .f.
   endif
     
   aadd( ::Todo, { cAction, cFile, cResponse, cJob, cTime, cFreq, time(), cSend, cError, lNotify, nTrials, 0,  NIL, cFreq } )
   ::Notice[cJob] := ""
   
RETURN .t.



METHOD StartServer( cTick, cLimit ) CLASS CustomSession
   local cPath, cDate, cTime
   if ::ID == "C"
      ::EM := "Wrong method call"
      return .f.
   endif
   if !( ::Status $ "IS" )
      return .f.
   endif
   if !::IsTimingOk( cTick, cLimit )
      return .f.
   endif
   
   cPath := curdrive() + ":\" + curdir()
   cDate := dtos( date() )
   cTime := strzero( 1000*seconds(), 8 )
   
   ::File := cPath + "\cs" + cDate + cTime + ".txt"
   
   ::Handle := fcreate( ::File, FC_NORMAL )
   if ferror() > 0
      ::EM := "error creating new session file: "+str( ferror() )
      return .f.
   endif
   
   ::Status := "U"
   
   fclose( ::Handle )   
   if ferror() > 0
      ::EM := "error closing new session file: "+str( ferror() )
      return .f.
   endif
   ::Handle := fopen( ::File, FO_SHARED|FO_READWRITE )
   if ferror() > 0
      ::EM := "error reopening session file: "+str( ferror() )
      return .f.
   endif 

   ::From := time()
   ::Last := ::From   

   if !::Write()
      return .f.
   endif
   
   ::Status := "W"
RETURN .t.



METHOD StopServer() CLASS CustomSession
   local i, lWrite, lClose
   if ::ID == "C"
      ::EM := "Wrong method call"
      return .f.
   endif
   if ::Status $ "IS"
      return .t.
   endif
   
   if ::Status == "B"
      for i:=1 to 3
         threadsleep(200)
         if ::Status <> "B"
            exit
         endif
      next
      if ::Status == "B"
         ::EM := "Stopping server failed - file busy"
         return .f.
      endif
   endif  
   
   if ::Browser<>NIL .and. ::Browser:Busy
      for i:=1 to 3
         threadsleep(500)
         if !::Browser:Busy
            exit
         endif
      next
      if ::Browser:Busy
         ::EM := "Stopping server failed - browser busy"
         return .f.
      endif
   endif

   ::Status := "D"
   
   ::EM := ""
   lWrite := ::Write()
   lClose := fclose( ::Handle )
   if !lClose
      ::EM += " Session file close error"
   endif
   if !lWrite .or. !lClose
      return .f.
   endif
   
   ::Status := "S"   
RETURN .t.



METHOD Server() CLASS CustomSession
   local cNow
   if ::Status<>"W"
      return .t.
   endif
   cNow := time()
   
   if elaptime( ::Last, cNow ) > ::Tick
      
      if ::ID == "C"
         ::EM := "Wrong method call"
         return .f.
      endif      
      
      if !::Read()
         return .f.
      endif
      if ::TimeOut .and. elaptime( ::CTime, cNow )>::Limit
         ::EM := "Client timeout"
         return .f.
      endif
      
      if !::Write()  // server check in
         return .f.
      endif
      
   endif
   
   ::Status := "L"
   if !::Loop()
      ::Status := "D"
      return .f.
   endif
   ::Status := "W"
RETURN .t.



METHOD Loop() CLASS CustomSession
   
   local i, nJob, nOut, nRetry
   local lSuccess, lOk:=.t.
   local cFile, cParams:=NIL, cPrev
   if ::ID == "C"
      ::EM := "Wrong method call"
      return .f.
   endif
   
   for nJob := 1 to len( ::Todo )
      
      if ::Status == "D" // shutdown in progress
         lOk := .f.
         exit
      elseif ::Status == "B" // save or noticing in progress
         for i:=1 to 3
            threadsleep(200)
            if ::Status <> "B"
               exit
            endif
         next
      endif
      
      if ::Todo[nJob, CUSE_JOBERRN] < 1  // no retry in progress
         if ::Todo[nJob, CUSE_JOBNEXT] == NIL // excluded from todo list
            loop
         endif
         if ::Todo[nJob, CUSE_JOBNEXT] <> "00:00:00"  // not immediate launch
            if elaptime( ::Todo[nJob, CUSE_JOBLAST], time() ) < ::Todo[nJob, CUSE_JOBNEXT]
               loop
            endif
         endif
      endif
      
      lSuccess := .t.
      cParams := ::Todo[nJob, CUSE_JOBQL]
      
      if ::Todo[nJob, CUSE_JOBTYPE] == "C"  // compiled
         
         cFile := '"'+ ::Todo[nJob, CUSE_JOBFILE] + '"'
         if ::Todo[nJob, CUSE_JOBSEND] == "Y"
            if cParams == NIL
               cParams := '"' + ::File + '"'
            else
               cParams := '"' + ::File + '" ' + cParams
            endif
         endif
         
         nOut := ShellExecute( , , cFile, cParams, , SW_SHOW )
         if nOut <= 32 // failure
            lSuccess :=.f.
            ::EM := "Error "+ltrim(str(nOut))+" when running "+cFile
         endif
         
      elseif ::Todo[nJob, CUSE_JOBTYPE] == "S"  // script
         
         if !empty( ::Docs[CUSE_SCRID] ) // previous browsing in progress
            loop
         endif
         
         ::Docs[CUSE_SCRURL] := ::Todo[nJob, CUSE_JOBFILE]  // url
         if cParams <> NIL                  // parameters are passed in URL "as is" ( GET method )
            ::Docs[CUSE_SCRURL] += "?" + cParams
         endif

         ::Docs[CUSE_SCRRES] := ::Todo[nJob, CUSE_JOBRESP]  // local "result" file
         ::Docs[CUSE_SCRID] := ::Todo[nJob, CUSE_JOBID]    // job id
         ::Docs[CUSE_SCRERR] := 0  // counter for navigation errors
         
         ::Browser:Url := ::Docs[CUSE_SCRURL] // this is specific to VxH - starts navigation
         
      endif

      ::Todo[nJob, CUSE_JOBLAST] := time()  // register attempt
      ::Todo[nJob, CUSE_JOBNEXT] := ::Todo[nJob, CUSE_JOBFREQ]
      
      if lSuccess
         if ::Todo[nJob, CUSE_JOBTYPE] == "C"  // register latest start time as notice
            ::Todo[nJob, CUSE_JOBERRN] := 0    // remove retry condition
            ::SetNotice( ::Todo[nJob, CUSE_JOBID], ::Todo[nJob, CUSE_JOBLAST] )
         endif
      else                                     // in case of compiled jobs
         if !::ManageError( ::Todo[nJob, CUSE_JOBID] )
            alert( ::EM )
         endif
      endif

   next
   
RETURN lOk



method Client() class CustomSession
   local cNow
   if ::Status <> "W"
      return .t.
   endif
   cNow := time()
   
   if elaptime( ::Last, cNow ) < ::Tick
      return .t.
   endif
      
   if ::ID == "S"
      ::EM := "Wrong method call"
      ::Status := "D"
      return .f.
   endif   
      
   if !::Read()
      ::Status := "D"
      return .f.
   endif
   if elaptime( ::STime, cNow ) > ::Limit
      ::EM := "Server timeout"
      ::Status := "D"
      return .f.
   endif
      
   if !::Write()  // client check in
      ::Status := "D"
      return .f.
   endif   
return .t.



method Connect( cFile, cTick, cLimit ) class CustomSession
   if ::ID == "S"
      ::EM := "Wrong method call"
      return .f.
   endif
   if !(::Status $ "IS")
      ::EM := "Session already started"
      return .f.
   endif
   if valtype(cFile)<>"C" .or. empty(alltrim(cFile))
      ::EM := "Invalid session file"
      return .f.
   endif
   if !::IsTimingOk( cTick, cLimit )
      return .f.
   endif 
   
   ::File := cFile
   ::Handle := fopen( ::File, FO_SHARED|FO_READWRITE )
   if ::Handle < 1
      ::EM := "Session file open error: " + str( ferror() )
      ::Handle := -1
      return .f.
   endif
   
   ::Status := "U"
   ::From := time()
   ::Last := ::From
   
   if !::Read()
      return .f.
   endif
   
   if !::Write()
      return .f.
   endif
   
   ::Status := "W"
return .t.



method Disconnect() class CustomSession
   local i, lRead, lWrite, lClose, cEM
   if ::ID == "S"
      ::EM := "Wrong method call"
      return .f.
   endif
   if ::Status $ "ISB"
      return .t.
   endif
   
   if ::Status == "B"
      for i:=1 to 3
         threadsleep(200)
         if ::Status <> "B"
            exit
         endif
      next
      if ::Status == "B"
         ::EM := "Failed to disconnect - session file busy"
         return .f.
      endif
   endif
   
   ::Status := "D"
   
   cEM := ""
   lRead := ::Read()
   if !lRead
      cEm := ::EM
   endif
   lWrite := ::Write()
   if !lWrite
      cEm += "  " + ::EM
   endif
   lClose := fclose( ::Handle )
   if !lClose
      cEM += "  Session file close error"
   endif
   if !lRead .or. !lWrite .or. !lClose
      ::EM := cEM
      return .f.
   endif
   
   ::Status := "S"
return .t.



method Read() class CustomSession
   local i, cBuf, nRead, aData, lOk:=.t.
   if !( ::Status $ "WUD" )
      ::EM := "Not ready to read"
      return .f.
   endif
   if ::Status == "W"
      ::Status := "B"
   endif
   
   fseek( ::Handle, 0 )
   cBuf:=replicate( chr(0), ::BS )
   nRead:=fread( ::Handle, @cBuf, ::BS )
   if nRead<::BS .or. ferror()>0
      ::EM := "Session file read error"
      if ::Status == "B"
         ::Status := "W"
      endif
      return .f.
   endif
   
   aData := hb_atokens( rtrim( cBuf ), "|" )
   if len( aData ) >= 2
      if ::ID == "C"
         lOk := ::IsTime( aData[1] )
         if lOk
            ::STime := aData[1]
         endif
      else
         lOk := ::IsTime( aData[2] )
         if lOk
            ::CTime := aData[2]
         endif
      endif      
   else
      lOk := .f.
   endif
   if !lOk 
      ::EM := "Wrong session file"
      if ::Status == "B"
         ::Status := "W"
      endif      
      return .f.
   endif
   
   if ::ID == "C"
      for i:=3 to len( aData )
         cBuf := aData[i]
         nRead := at( "=", cBuf )
         if nRead > 1 
            ::Notice[ left(cBuf, nRead-1) ] := substr( cBuf, nRead+1 )
         endif
      next
   endif
   
   if ::Status == "B"
      ::Status := "W"
   endif
   
RETURN .t.



method Write() class CustomSession
   local i, elem, cBuf, nWritten, nLen, cTime
   if !(::Status $ "WUD")
      ::EM := "Not ready to write"
      return .f.
   endif
   if ::Status == "W"
      ::Status := "B"
   endif

   cTime := time()
   if ::ID == "S"
      cBuf := cTime + "|" + ::CTime + "|"
      ::STime := cTime
   else
      cBuf := ::Stime + "|" + cTime + "|"
      ::CTime := cTime
   endif
   nLen := len(cBuf)
   
   if ::ID == "S"
      for i:=1 to len( ::Notice )
         elem := hgetpairat( ::Notice, i )
         cBuf += elem[1] + "=" + elem[2] + "|"
      next
      cBuf := padr( cBuf, ::BS )
      nLen := ::BS
   endif
   
   fseek( ::Handle, 0 )
   nWritten:=fwrite( ::Handle, cBuf, nLen )
   if nWritten<nLen .or. ferror()>0
      ::EM := "Session file write error"
      if ::Status == "B"
         ::Status := "W"
      endif
      return .f.
   endif
   
   hb_fcommit( ::Handle )  // enforces refresh
   ::Last := cTime
   if ::Status == "B"
      ::Status := "W"
   endif
RETURN .t.