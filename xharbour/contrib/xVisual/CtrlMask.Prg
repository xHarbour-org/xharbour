
// xHarbour xIde project
// AJ Wos

/*

TODO:

- correct grid painting - 0,0 align
- add asymetric grid ( x <> y )
- add lSnap flag
- revise (improve) snapping
- revise min size protection for controls
- add notifications for object inspector
- add external object (selective+group) selection/deselection
- add object tabbing                                <- done
- add send to the back/front
- mouse right click
- tooltip with position coords
- add UNDO ( only last operation as in Delphi ).
- direct text editing ( on char ? pass keystrokes to selected control ?)
- Use methods instead of calls
- menu editor
- parent form must disable processing commands in editor mode.
- improve visibility of the selection rectangle
  ( replace focus rect with your own. )

*/


*************************************************


#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"
#include "debug.ch"
#include "accel.ch"

#define ASWORD( nUword ) if( ( nUword > 32768 ),(nUword-65536),nUword )

GLOBAL EXTERNAL hThisInstance

#define MINX 5
#define MINY 5

#define HANDLESIZE 5

*-----------------------------------------------------------------------------*

CLASS oCtrlMask FROM TWindow

   DATA aSelected
   DATA aHot
   DATA aSiblings
   DATA moving
   DATA sizing
   DATA selecting
   DATA nClick
   DATA MouseDown
   DATA aEditRect
   DATA aClickPt

   METHOD New() CONSTRUCTOR
   METHOD OnGetDlgCode()
   METHOD OnRButtonDown()
   METHOD OnLButtonDown()
   METHOD OnLButtonUp()
   METHOD OnPaint()
   METHOD OnKeyDown()
   METHOD OnSysKeyDown()
   METHOD GetSiblings()
   METHOD OnMouseMove()
   METHOD MoveCtrl()
   METHOD Refresh()

ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( oParent ) CLASS oCtrlMask

   ::left      := 0 //aPt[1]
   ::top       := 0 //aPt[2]
   ::width     := oParent:width
   ::height    := oParent:height
   ::style     := WS_CHILD+WS_VISIBLE  //+WS_BORDER //+WS_THICKFRAME
   ::name      := "CTRLMASK"
   ::color     := GetStockObject(NULL_BRUSH)
   ::ExStyle   := WS_EX_TRANSPARENT
   ::Cursor    := 0 //LoadCursor(,IDC_WAIT)
   ::Caption   :=""
   ::lRegister :=.T.
   ::WndProc   := 'FormProc'
   ::Msgs      := -1
   ::FrameWnd  :=.F.
   ::FormType  := 1
   ::aHot      :={}
   ::aSiblings :={}
   ::moving:=.F.
   ::sizing:=.F.
   ::selecting:=.F.
   ::mousedown:=.F.
   ::nClick:=HTCLIENT
   ::aEditRect:=NIL
   ::aSelected:={}
   ::aClickPt:={}



   RETURN( super:new( oParent ) )


*-----------------------------------------------------------------------------*

METHOD OnGetDlgCode( )

   RETURN(DLGC_WANTARROWS+DLGC_WANTTAB)

*-----------------------------------------------------------------------------*

METHOD OnSysKeyDown(wParam,lParam)

    // set focus to main frame

   RETURN(NIL)


*-----------------------------------------------------------------------------*

METHOD OnRButtonDown()


   messagebox(::Parent:Handle,"TrackPopupMenu comes here")
   // use menu class to create trackpopup menu

   SetFocus(::Handle)

   RETURN(NIL)


*-----------------------------------------------------------------------------*

// no snapping to grid with keyboard - precision movements

// Ctrl+arrows => moving
// Shift+arrows => sizing
// Esc => unselect
// Del => remove


// ALT key to swich over to main window
// Check in OnSysKeyDown

// TAB next control ( even if none selected )


METHOD OnKeyDown( wParam, lParam )

     LOCAL cKbd
     LOCAL lShift
     LOCAL lCtrl
     LOCAL a
     LOCAL aRect
     LOCAL acRect
     LOCAL lOnBorder:=.T.
     LOCAL n
     LOCAL l:=0
     LOCAL t:=0
     LOCAL b:=0
     LOCAL r:=0

     IF Len(::aSelected) > 0

         cKbd  :=GetKeyboardState()
         lShift:=( cKbd[VK_SHIFT+1]   < 0  ) // size
         lCtrl :=( cKbd[VK_CONTROL+1] < 0  ) // move

         DO CASE
         CASE wParam==VK_RETURN // special properties ?
            messagebox(::Parent:Handle,"Popup menu ? text editing ? oBj inspector ?")
            SetFocus(::Handle)
            RETURN(NIL)

         CASE wParam==VK_DELETE
            messagebox(::Parent:Handle,"Delete without warning")
            SetFocus(::Handle)
            RETURN(NIL)

         CASE wParam==VK_ESCAPE // ESC unselect ?
            ::aSelected:={}
            ::nClick:=HTCLIENT
            InvalidateRect(::Parent:Handle,,.T.)
            SetFocus(::Handle)
//            WinSetCursor(LoadCursor(,IDC_ARROW))
            RETURN(NIL)

         CASE wParam==VK_TAB
            IF (n:=aScan(::aSiblings,{|h| h==::aSelected[1,1]}))==0
               IF len(::aSiblings)>0
                  n:=1
               ENDIF
            ENDIF
            IF n > 0
               IF lShift
                  n:=IF(n==1,len(::aSiblings),n-1)
               ELSE
                  n:=IF(len(::aSiblings) = n,1,n+1)
               ENDIF
               IF (::aSiblings[n] <> ::aSelected[1,1] ) .OR. len(::aSelected) > 1
                  aRect:=GetWindowRect(::aSiblings[n])
                  RectScreenToClient(::Handle,aRect)
                  ::aSelected:={{::aSiblings[n],aclone(aRect),0,0,{},{}}}   // handle,rect,anchorX,anchorY,aHot
                  InvalidateRect(::Parent:Handle,,.T.)
               ENDIF
            ENDIF
            RETURN(NIL)

           // select next control


         CASE (!lCtrl .AND. !lShift) .OR. ( lShift .AND. lCtrl )
            RETURN(NIL)

         CASE wParam==VK_DOWN // down
            lOnBorder:=.F.
            acRect:=GetClientRect(::Handle)
            b:=1
            IF lShift
               t:=1
            ENDIF
            FOR EACH a IN ::aSelected
               aRect:=GetWindowRect(a[1])
               RectScreenToClient(::Handle,aRect)
               a[2]:=aClone(aRect)
               IF a[2,4] >= acRect[4]
                  lOnBorder:=.T.
                  EXIT
               ENDIF
            NEXT

         CASE wParam==VK_UP //up
            lOnBorder:=.F.
            acRect:=GetClientRect(::Handle)
            b:=-1
            IF lShift
               t:=-1
            ENDIF
            FOR EACH a IN ::aSelected
               aRect:=GetWindowRect(a[1])
               RectScreenToClient(::Handle,aRect)
               a[2]:=aClone(aRect)
               IF (lCtrl .AND. (a[2,2] <= 0)) .OR. (lShift .AND. (a[2,4]-a[2,2] <= MINY ))
                  lOnBorder:=.T.
                  EXIT
               ENDIF
            NEXT

         CASE wParam==VK_LEFT // left
            lOnBorder:=.F.
            acRect:=GetClientRect(::Handle)
            r:=-1
            IF lShift
               l:=-1
            ENDIF
            FOR EACH a IN ::aSelected
               aRect:=GetWindowRect(a[1])
               RectScreenToClient(::Handle,aRect)
               a[2]:=aClone(aRect)
               IF (lCtrl .AND. (a[2,1] <= 0 )) .OR. (lShift .AND. (a[2,3]-a[2,1] <= MINX ))
                  lOnBorder:=.T.
                  EXIT
               ENDIF
            NEXT

         CASE wParam==VK_RIGHT // right
            lOnBorder:=.F.
            acRect:=GetClientRect(::Handle)
            r:=1
            IF lShift
               l:=1
            ENDIF
            FOR EACH a IN ::aSelected
               aRect:=GetWindowRect(a[1])
               RectScreenToClient(::Handle,aRect)
               a[2]:=aClone(aRect)
               IF a[2,3] >= acRect[3]
                  lOnBorder:=.T.
                  EXIT
               ENDIF
            NEXT

         ENDCASE

         IF !lOnBorder
            FOR EACH a IN ::aSelected
               InvalidateRect(::Parent:Handle,a[2],.T.) // for statics
               a[2,1]+=l
               a[2,2]+=t
               a[2,3]+=r
               a[2,4]+=b
               MoveWindow(a[1],a[2,1],a[2,2],a[2,3]-a[2,1],a[2,4]-a[2,2],.T.)
            NEXT
         ENDIF

     ENDIF

     RETURN(NIL)

*-----------------------------------------------------------------------------*

METHOD OnPaint(hDC)

   LOCAL acRect
   //LOCAL hBrush
   LOCAL aPt
   LOCAL a
   LOCAL n:=0

   IF !empty(::aSelected)
      SetTextColor(hDC,RGB(0,0,0))
      SetBkColor(hDC,RGB(255,255,255))
      IF ::mousedown
         IF len(::aSelected) > 1
            FOR EACH a In ::aSelected
               DrawFocusRect(hDC,a[6])
            NEXT
         ELSE
            DrawFocusRect(hDC,::aEditRect)
         ENDIF
      ELSE
         //hBrush:=GetStockObject( if( len(::aSelected) > 1, DKGRAY_BRUSH, BLACK_BRUSH ) )
         FOR EACH a In ::aSelected
           n++
           ::aSelected[n,5]:=DrawHandles(hDC,a[2],if( len(::aSelected) > 1,COLOR_BTNSHADOW+1,COLOR_WINDOWTEXT+1))
         NEXT
      ENDIF
   ENDIF

   RETURN(NIL)

*-----------------------------------------------------------------------------*

METHOD Refresh()

   LOCAL aRect
   LOCAL a

   FOR EACH a IN ::aSelected
      aRect:=GetWindowRect(a[1])
      RectScreenToClient(::Handle,aRect)
      a[2]:=aClone(aRect)
   NEXT
   InvalidateRect(::Parent:Handle,,.T.)
   ReleaseCapture()
   ::BringToTop()
   ::SetFocus()

   RETURN(NIL)

*-----------------------------------------------------------------------------*

// client area coordinates

METHOD OnLButtonDown( wParam, x, y )

   LOCAL aPt:={ASWORD(x),ASWORD(y)}
   LOCAL hChild
   LOCAL aRect
   LOCAL aWRect
   LOCAL lFound:=.F.
   LOCAL nPos
   LOCAL hDC
   LOCAL a

   ClientToScreen(::Handle,aPt)

   FOR EACH hChild IN ::aSiblings
      aWRect:=GetWindowRect(hChild)
      IF PtInRect(aWRect,aPt)
         lFound:=.T.
         IF aScan(::aSelected,{|a| a[1]==hChild })==0
            aRect:=aClone(aWRect)
            RectScreenToClient(::Handle,aRect)
            ::aSelected:={{hChild,aclone(aRect),x-aRect[1],y-aRect[2],{},{}}}   // handle,rect,anchorX,anchorY,aHot
         ENDIF
         IF Len(::aSelected)==1 .and. (nPos:=aScan(::aSelected[1,5],{|a| ptInRect(a[1],{x,y})})) > 0 // <= aHot
            ::nClick:=::aSelected[1,5,nPos,2]
            WinSetCursor(LoadCursor(,::aSelected[1,5,nPos,3]))
            ::sizing:=.T.
         ELSE
            ::nClick:=HTCAPTION
            WinSetCursor(LoadCursor(hThisInstance,"FIST"))
            FOR EACH a IN ::aSelected
                a[3]:=x-a[2,1] // anchor X
                a[4]:=y-a[2,2] // anchor Y
            NEXT
            ::moving:=.T.
         ENDIF
         ::aEditRect:=::MoveCtrl(::Handle,{x,y})
         EXIT
      ENDIF
   NEXT

   IF !lFound
      ::aSelected:={}
      ::nClick:=HTCLIENT
      WinSetCursor(LoadCursor(,IDC_SIZEALL))
      ::aEditRect:={x,y,x,y}
      ::selecting:=.T.
      ::aClickPt:={x,y}
    ENDIF

   InvalidateRect(::Parent:Handle,,.T.)
   ::mousedown:=.T.
   SetFocus(::Handle)
   SetCapture(::Handle)

   RETURN(NIL)

*-----------------------------------------------------------------------------*

// client coordinates

METHOD OnMouseMove( aParam,x,y)

   LOCAL aPt:={ASWORD(x),ASWORD(y)}
   LOCAL hChild
   LOCAL lFound := .F.
   LOCAL nPos
   LOCAL n
   LOCAL hDC
   LOCAL a

   IF ::mousedown
      hDC:=GetDC(::Handle)
      SetTextColor(hDC,RGB(0,0,0))
      SetBkColor(hDC,RGB(255,255,255))
      IF len(::aSelected) > 1
         FOR EACH a In ::aSelected
            DrawFocusRect(hDC,a[6])
         NEXT
      ELSE
        DrawFocusRect(hDC,::aEditRect)
      ENDIF
     // ::aOriginal:=aclone(::aEditRect)

      IF len(::aSelected) > 1
         ::MoveCtrl(::Handle,aPt)
         FOR EACH a In ::aSelected
           DrawFocusRect(hDC,a[6])
         NEXT
      ELSE
       ::aEditRect:=::MoveCtrl(::Handle,aPt)
        DrawFocusRect(hDC,::aEditRect)
      ENDIF
      ReleaseDc(::Handle,hDC)

   ELSE
      ClientToScreen(::Handle,aPt)
      FOR EACH hChild IN ::aSiblings
         IF PtInRect(GetWindowRect(hChild),aPt)
            IF Len(::aSelected) == 1 .and. ::aSelected[1,1]==hChild
               IF (nPos:=aScan(::aSelected[1,5],{|a| ptInRect(a[1],{x,y})})) > 0
                  WinSetCursor(LoadCursor(,::aSelected[1,5,nPos,3]))
               ELSE
                  WinSetCursor(LoadCursor(hThisInstance,"HAND"))
               ENDIF
            ELSEIF len(::aSelected) > 1
               WinSetCursor(LoadCursor(hThisInstance,"HAND"))
            ELSE
               WinSetCursor(LoadCursor(hThisInstance,"FINGER"))
            ENDIF
            lFound:=.T.
            EXIT
         ENDIF
      NEXT
      IF !lFound
         WinSetCursor(LoadCursor(,IDC_ARROW))
      ENDIF
   ENDIF

   RETURN(NIL)

      
*-----------------------------------------------------------------------------*

// client coordinates

METHOD OnLButtonUp( wParam, x, y )

   LOCAL aPt:={ASWORD(x),ASWORD(y)}
   LOCAL aTemp
   LOCAL hChild
   LOCAL aWRect
   LOCAL aRect
   LOCAL a

   if !::mousedown // if dragged pressed from different window
      RETURN(NIL)
   endif

   ::mousedown:=.F.

   If ::moving
      ::moving:=.F.
   Endif
   if ::sizing
      ::sizing:=.T.
   endif
   ::MoveCtrl( ,aPt)
   FOR EACH a IN ::aSelected
      MoveWindow(a[1],a[6,1],a[6,2],a[6,3]-a[6,1],a[6,4]-a[6,2],.T. )
      aRect:=GetWindowRect(a[1])
      RectScreenToClient(::Handle,aRect)
      a[2]:=aClone(aRect)
   NEXT

   IF ::selecting
      ::selecting:=.F.
      WinSetCursor(LoadCursor(,IDC_ARROW))

      FOR EACH hChild IN ::aSiblings
         aWRect:=GetWindowRect(hChild)
         aRect:=aClone(aWRect)
         RectScreenToClient(::Handle,aRect)
         IF !empty(IntersectRect( aRect, ::aEditRect ))
            aAdd(::aSelected,{hChild, aclone(aRect),0,0,{},{}}) // handle,rect,anchorX,anchorY,aHot
         ENDIF

      NEXT
   ENDIF

   InvalidateRect(::Parent:Handle,,.T.)
   ReleaseCapture()
   ::BringToTop()
   ::SetFocus()

   RETURN(NIL)

*-----------------------------------------------------------------------------*

// to be revised

METHOD MoveCtrl(hWnd,aPt)

   LOCAL aRect
   LOCAL AnchorX
   LOCAL AnchorY
   LOCAL aChild
   LOCAL aOriginal

   FOR EACH aChild IN ::aSelected

   AnchorX:=aChild[3]
   AnchorY:=aChild[4]
   aOriginal:=SnapRect(aChild[2],::Parent:xGrid,::Parent:yGrid)

   DO CASE
   CASE ::nClick==HTCAPTION
      //MoveWindow(hWnd,aPt[1]-AnchorX,aPt[2]-::AnchorY,aOriginal[3]-aOriginal[1],aOriginal[4]-aOriginal[2],.t.)

      aRect:=SnapRect({aPt[1]-AnchorX,aPt[2]-AnchorY,;
              aPt[1]+aOriginal[3]-aOriginal[1]-AnchorX,;
              aPt[2]+aOriginal[4]-aOriginal[2]-AnchorY},::Parent:xGrid,::Parent:yGrid)

   CASE ::nClick==HTTOP
      aPt[2]:=Snap(Min(aPt[2],aOriginal[4]-MINY),::Parent:yGrid)
      //MoveWindow(hWnd,aOriginal[1],aPt[2],aOriginal[3]-aOriginal[1],aOriginal[4]-aPt[2],.t.)
      aRect:={aOriginal[1],aPt[2],aOriginal[3],aOriginal[4]}

   CASE ::nClick==HTBOTTOM
      aPt[2]:=Snap(Max(aPt[2],aOriginal[2]+MINY),::Parent:yGrid)
      //MoveWindow(hWnd,aOriginal[1],aOriginal[2],aOriginal[3]-aOriginal[1],aPt[2]-aOriginal[2],.t.)
      aRect:={aOriginal[1],aOriginal[2],aOriginal[3],aPt[2]}

   CASE ::nClick==HTLEFT
      aPt[1]:=Snap(Min(aPt[1],aOriginal[3]-MINX),::Parent:xGrid)
      //MoveWindow(hWnd,aPt[1],aOriginal[2],aOriginal[3]-aPt[1],aOriginal[4]-aOriginal[2],.t.)
      aRect:={aPt[1],aOriginal[2],aOriginal[3],aOriginal[4]}

   CASE ::nClick==HTRIGHT
      aPt[1]:=Snap(Max(aPt[1],aOriginal[1]+MINX),::Parent:xGrid)
      //MoveWindow(hWnd,aOriginal[1],aOriginal[2],aPt[1]-aOriginal[1],aOriginal[4]-aOriginal[2],.t.)
      aRect:={aOriginal[1],aOriginal[2],aPt[1],aOriginal[4]}

   CASE ::nClick==HTTOPLEFT
      aPt[1]:=Snap(Min(aPt[1],aOriginal[3]-MINX),::Parent:xGrid)
      aPt[2]:=Snap(Min(aPt[2],aOriginal[4]-MINY),::Parent:yGrid)
      //MoveWindow(hWnd,aOriginal[1],aOriginal[2],aPt[1]-aOriginal[1],aOriginal[4]-aOriginal[2],.t.)
      aRect:={aPt[1],aPt[2],aOriginal[3],aOriginal[4]}

   CASE ::nClick==HTBOTTOMLEFT
      aPt[1]:=Snap(Min(aPt[1],aOriginal[3]-MINX),::Parent:xGrid)
      aPt[2]:=Snap(Max(aPt[2],aOriginal[2]+MINY),::Parent:yGrid)
      //MoveWindow(hWnd,aPt[1],aOriginal[2],aOriginal[3]-aPt[1],aPt[2]-aOriginal[2],.t.)
      aRect:={aPt[1],aOriginal[2],aOriginal[3],aPt[2]}
      view aRect

   CASE ::nClick==HTTOPRIGHT
      aPt[1]:=Snap(Max(aPt[1],aOriginal[1]+MINX),::Parent:xGrid)
      aPt[2]:=Snap(Min(aPt[2],aOriginal[4]-MINY),::Parent:yGrid)
      //MoveWindow(hWnd,aOriginal[1],aPt[2],aPt[1]-aOriginal[1],aOriginal[4]-aOriginal[2],.t.)
      aRect:={aOriginal[1],aPt[2],aPt[1],aOriginal[4]}

   CASE ::nClick==HTBOTTOMRIGHT
      aPt[1]:=Snap(Max(aPt[1],aOriginal[1]+MINX),::Parent:xGrid)
      aPt[2]:=Snap(Max(aPt[2],aOriginal[2]+MINY),::Parent:yGrid)
      //MoveWindow(hWnd,aOriginal[1],aOriginal[2],aPt[1]-aOriginal[1],aPt[2]-aOriginal[2],.t.)
      aRect:={aOriginal[1],aOriginal[2],aPt[1],aPt[2]}

   ENDCASE

       aChild[6]:=aclone(aRect)

   NEXT

   IF ::nClick==HTCLIENT
      aRect:={Min(aPt[1],::aClickPt[1]),Min(aPt[2],::aClickPt[2]),Max(aPt[1],::aClickPt[1]),Max(aPt[2],::aClickPt[2])}

   ENDIF

   ::aEditRect:=aRect

   RETURN(aRect)

*-----------------------------------------------------------------------------*

METHOD GetSiblings()

   LOCAL hWnd:=GetWindow(::Parent:Handle,GW_CHILD)

   ::aSiblings:={}
   DO WHILE !empty(hWnd)
      IF hWnd<>::Handle
         aAdd(::aSiblings,hWnd)
      ENDIF
      hWnd:=GetWindow(hWnd,GW_HWNDNEXT)
   ENDDO

   RETURN(::aSiblings)

*-----------------------------------------------------------------------------*

FUNCTION Snap(x,nGrain)

   RETURN(if(nGrain==0,x,round((x/nGrain),0)*nGrain))

*-----------------------------------------------------------------------------*

FUNCTION SnapRect(aRect,xGrain,yGrain)

   aRect[1]:=Snap(aRect[1],xGrain)
   aRect[2]:=Snap(aRect[2],yGrain)
   aRect[3]:=Snap(aRect[3],xGrain)
   aRect[4]:=Snap(aRect[4],yGrain)

   RETURN(aRect)

*-----------------------------------------------------------------------------*



FUNCTION DrawHandles(hDC,aRect,hBrush)

   LOCAL nTop
   LOCAL nLeft
   LOCAL aHot:=array(8)

   aHot[1]:={{aRect[1]  ,aRect[2]  ,aRect[1]+HANDLESIZE,aRect[2]+HANDLESIZE},HTTOPLEFT    ,IDC_SIZENWSE}
   aHot[2]:={{aRect[3]-HANDLESIZE,aRect[2]  ,aRect[3]  ,aRect[2]+HANDLESIZE},HTTOPRIGHT   ,IDC_SIZENESW}
   aHot[3]:={{aRect[1]  ,aRect[4]-HANDLESIZE,aRect[1]+HANDLESIZE,aRect[4]  },HTBOTTOMLEFT ,IDC_SIZENESW}
   aHot[4]:={{aRect[3]-HANDLESIZE,aRect[4]-HANDLESIZE,aRect[3]  ,aRect[4]  },HTBOTTOMRIGHT,IDC_SIZENWSE}

   nLeft:=(aRect[1]+aRect[3])/2-2
   nTop :=aRect[2]
   aHot[5]:={{nLeft  ,nTop ,nLeft+HANDLESIZE, nTop+HANDLESIZE},HTTOP,IDC_SIZENS}

   nLeft:=(aRect[1]+aRect[3])/2-2
   nTop :=aRect[4]-HANDLESIZE
   aHot[6]:={{nLeft  ,nTop ,nLeft+HANDLESIZE, nTop+HANDLESIZE},HTBOTTOM,IDC_SIZENS}

   nLeft:=aRect[1]
   nTop :=(aRect[2]+aRect[4])/2-2
   aHot[7]:={{nLeft  ,nTop ,nLeft+HANDLESIZE, nTop+HANDLESIZE},HTLEFT,IDC_SIZEWE}

   nLeft:=aRect[3]-HANDLESIZE
   nTop :=(aRect[2]+aRect[4])/2-2
   aHot[8]:={{nLeft  ,nTop ,nLeft+HANDLESIZE, nTop+HANDLESIZE},HTRIGHT,IDC_SIZEWE}

   aEval( aHot, {|a| FillRect(hDC, a[1], hBrush)})

   RETURN(aHot)

*-----------------------------------------------------------------------------*

FUNCTION RectScreenToClient(hWnd,aRect)

   LOCAL aPt

   aPt:={aRect[1],aRect[2]}
   ScreenToClient(hWnd,aPt)
   aRect[1]:=aPt[1]
   aRect[2]:=aPt[2]

   aPt:={aRect[3],aRect[4]}
   ScreenToClient(hWnd,aPt)
   aRect[3]:=aPt[1]
   aRect[4]:=aPt[2]

   RETURN(aRect)

*-----------------------------------------------------------------------------*

FUNCTION RectClientToScreen(hWnd,aRect)

   LOCAL aPt

   aPt:={aRect[1],aRect[2]}
   ClientToScreen(hWnd,aPt)
   aRect[1]:=aPt[1]
   aRect[2]:=aPt[2]

   aPt:={aRect[3],aRect[4]}
   ClientToScreen(hWnd,aPt)
   aRect[3]:=aPt[1]
   aRect[4]:=aPt[2]

   RETURN(aRect)










