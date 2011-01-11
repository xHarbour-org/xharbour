/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Msg.prg                                                                                            *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*
#Include 'vxh.ch'


//----------------------------------------------------------------------------------------------------//
Function MsgInfo(cText,cTitle)
   
   IF Empty(cTitle)
      cTitle:="Information"
   ENDIF
   
   MessageBox( GetActiveWindow() , cStr(cText), cStr(cTitle), MB_OK | MB_ICONINFORMATION )
   
RETURN NIL


//----------------------------------------------------------------------------------------------------//
Function MsgAlert(cText,cTitle)
   
   IF Empty(cTitle)
      cTitle:="Attention"
   ENDIF
   
   MessageBox( GetActiveWindow() , cStr(cText), cStr(cTitle), MB_OK | MB_ICONEXCLAMATION)
   
RETURN NIL


//----------------------------------------------------------------------------------------------------//
Function MsgStop(cText,cTitle)
   
   IF Empty(cTitle)
      cTitle:="Stop"
   ENDIF   
   
   MessageBox( GetActiveWindow() , cStr(cText), cStr(cTitle), MB_OK | MB_ICONSTOP )
   
RETURN NIL


//----------------------------------------------------------------------------------------------------//
Function MsgYesNo( cText, cTitle )
   
   IF Empty(cTitle)
      cTitle:="Select an option"
   ENDIF   
   
RETURN MessageBox( GetActiveWindow(), cStr(cText), cStr(cTitle), MB_YESNO | MB_ICONQUESTION ) == IDYES   


//----------------------------------------------------------------------------------------------------//
Function MsgRetryCancel( cText, cTitle )
   
   IF Empty(cTitle)
      cTitle:="Select an option"
   ENDIF   
   
RETURN MessageBox( GetActiveWindow(), cStr(cText), cStr(cTitle), MB_RETRYCANCEL | MB_ICONQUESTION  ) == IDRETRY   

