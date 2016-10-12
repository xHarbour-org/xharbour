#include "sqlrdd.ch"
#include "mysql.ch" 
 
#define CRLF Chr(13)+Chr(10)

REQUEST SQLRDD
REQUEST SR_MYSQL

PROCEDURE Main()
   LOCAL nCnn, cConnection, cGroep, nTxtHandle, aSub:={}

   SET CENTURY ON 
   SET DATE FORMAT TO "DD/MM/YYYY"
      
   TRY   
      cConnection:="HST=bekz.net;UID=xH_mysql;PWD=jdnc4587;DTB=xharbour"       
      //cConnection:="HST=mysql.xharbour.com;UID=xH_mysql;PWD=jd1c5gu8;DTB=xharbour"       
      nCnn := SR_AddConnection( 3, cConnection)  
   CATCH oErr
      ? "Connection error! " + CRLF + oErr : Operation + CRLF + oErr : Description
      RETURN .f.
   END
  
   IF nCnn < 0
      ? "Connection error. See SQL1.LOG for details."
      RETURN .f.
   ENDIF 

   ? "Connected" 

   SR_RecnoName( "ID" )  
   SR_UseDeleteds( .F. )
   
   cToday:=Str(Year(Date()),4,0) + PadL(Month(Date()),2,"0") + PadL(day(Date()),2,"0")
   
   // Payments openen --> Payment
   USE Payments_Detail NEW VIA "SQLRDD" CONNECTION nCnn ALIAS Payments
   SET INDEX TO Tracking_NR

   // Order_Detail openen --> Detail
   USE Order_Detail NEW VIA "SQLRDD" CONNECTION nCnn ALIAS Detail
   SET FILTER TO Detail->ART_Type > 0 .AND. Detail->ART_Type < 5 .AND. Detail->SUB_END >= cToday
   
   WHILE !Detail->(EOF())
      IF Payments->(dbSeek( Detail->Tracking_NR ))
         IF Payments->Payed=1 .OR. Payments->Distr_order="1"
            Aadd(aSub, Detail->Customer_ID)
         ENDIF   
      ENDIF   
      Detail->(dbSkip())
   ENDDO
   
   // Users openen --> Users
   USE Users NEW VIA "SQLRDD" CONNECTION nCnn ALIAS Users
   SET INDEX TO ID
   
   SR_RecnoName( "ID2" )
   
   // Users_Detail openen --> Users_detail
   USE users_detail NEW VIA "SQLRDD" CONNECTION nCnn ALIAS Users_detail
   SET INDEX TO Customer_ID
   
   nTxtHandle:=FCreate( CurDrive() + ":\" + If(Empty(CurDir()), "", CurDir() + "\" ) + "Users.dat" )
   
   fSeek(nTxtHandle,0,2)             

   DO WHILE !Users_detail->(EOF())
   
      // Heeft deze user Update & Service Subscription?
      IF AScan(aSub, Users_detail->Customer_ID) > 0                               .OR. ;
         ( !Empty(Users_detail->EMP2) .AND. AScan(aSub, Users_detail->EMP2) > 0 ) .OR. ;
         Users_detail->DEVELOPER = 1                                              .OR. ;
         Users_detail->ADMIN = 1                                                  .OR. ;
         Users_detail->PARTNER = 1                                                .OR. ;
         Users_detail->LOCAL_XHARBOUR_BUILDER = 1
        
         cGroep := "beta.vxh,xharbour.builder"
        
      ELSE
      
         cGroep := ""  
      
      ENDIF
    
      IF Users_detail->DEVELOPER = 1 
         cGroep += ",xharbour.developer,beta.vxh"
      ENDIF
    
      IF Users_detail->PARTNER = 1 .OR. Users_detail->ADMIN = 1
         cGroep += ",partner"
      ENDIF
    
      IF Users_detail->BETA_VXH = 1 
         cGroep += ",beta.vxh"
      ENDIF
    
      IF Users_detail->VXH_DEV = 1 
         cGroep += ",vxh.development"
      ENDIF
        
      IF Users_detail->BETA_USER = 1 
         cGroep += ",xharbour.builder.beta"
      ENDIF
      
      IF Users_detail->DEV_DOCS = 1 
         cGroep += ",dev.docs"
      ENDIF

      
      // Write to USERS.DAT
      IF Users->(dbSeek(Users_detail->Customer_ID)) .AND. !Empty( cGroep ) .AND. !Empty( Users->EMAIL )

         ? Users->EMAIL
         
         IF Left(cGroep,1)=","
            cGroep:=Substr(cGroep,2)
         ENDIF
       
         fWrite(nTxtHandle, AllTrim(Users->EMAIL) + ":" + AllTrim(Users->PASSWORD) + ":None:*::" + cGroep + CRLF)
      ENDIF
      
        
      Users_detail->(dbSkip())              
      
   ENDDO
   
   CLOSE DATA
   
   fClose(nTxtHandle)

RETURN