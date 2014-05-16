#include "winapi.ch"
#include "hbclass.ch"
#include "commctrl.ch"
#include "wingdi.ch"

#define ALIGN_DEFAULT   0
#define ALIGN_LEFT      1
#define ALIGN_CENTER    2
#define ALIGN_RIGHT     3

#define WM_VXH_SHOWMODE               WM_APP + 1
#define WM_VXH_FREECALLBACK           WM_APP + 2
#define WM_VXH_DESTRUCTOBJECT         WM_APP + 3

#xtranslate __CONCAT <x> <y> => <x>[<y>]

#xcommand PROPERTY <p> [ROOT <r>] [DEFAULT <d>] [HELP <h>] [<hid: NOTPUBLIC>];
           =>  ;
           if ! <.hid.> .AND. Type( "m->VXHIDE" ) != "U" ;;
              DATA <p> PUBLISHED [INIT <d>] ;;
              DATA __CONCAT __a_ <p> INIT {<(p)>,[<r>],[<h>],[<d>]} ;;
           else ;;
              DATA <p> [INIT <d>] ;;
           endif ;;

#xcommand PROPERTY <p> [ROOT <r>] GET <bget> [DEFAULT <d>] [<prot: PROTECTED>] [HELP <h>] [<hid: NOTPUBLIC>] [MIN <m>] [MAX <x>] ;
           =>  ;
           DATA __CONCAT x <p>  [<prot>] [INIT <d>] ;;
           if ! <.hid.> .AND. Type( "m->VXHIDE" ) != "U"  ;;
              DATA __CONCAT __a_ <p> INIT {<(p)>,[<r>],[<h>],[<d>]} ;;
              ACCESS <p>    INLINE <bget> PERSISTENT ;;
           else ;;
              ACCESS <p>    INLINE <bget> ;;
           endif ;;
           ASSIGN <p>(v) INLINE [v := MAX( <m>, v ),] [v := MIN( <x>, v ),] ::x<p> := v, v

#xcommand PROPERTY <p> [ROOT <r>] SET <bset> [DEFAULT <d>] [<prot: PROTECTED>] [HELP <h>] [<hid: NOTPUBLIC>] [MIN <m>] [MAX <x>] ;
           =>  ;
           DATA __CONCAT x <p> [<prot>] [INIT <d>] ;;
           if ! <.hid.> .AND. Type( "m->VXHIDE" ) != "U"  ;;
              DATA __CONCAT __a_ <p> INIT {<(p)>,[<r>],[<h>],[<d>]} ;;
              ACCESS <p>    INLINE ::x<p> PERSISTENT ;;
           else ;;
              ACCESS <p>    INLINE ::x<p> ;;
           endif ;;
           ASSIGN <p>(v) INLINE [v := MAX( <m>, v ),] [v := MIN( <x>, v ),] <bset>, ::x<p> := v, v

#xcommand PROPERTY <p> [ROOT <r>] GET <bget> SET <bset> [DEFAULT <d>] [<prot: PROTECTED>] [HELP <h>] [<hid: NOTPUBLIC>] [MIN <m>] [MAX <x>] ;
           =>  ;
           DATA __CONCAT x <p> [<prot>] [INIT <d>] ;;
           if ! <.hid.> .AND. Type( "m->VXHIDE" ) != "U"  ;;
              DATA __CONCAT __a_ <p> INIT {<(p)>,[<r>],[<h>],[<d>]} ;;
              ACCESS <p>    INLINE <bget> PERSISTENT ;;
           else ;;
              ACCESS <p>    INLINE <bget> ;;
           endif ;;
           ASSIGN <p>(v) INLINE [v := MAX( <m>, v ),] [v := MIN( <x>, v ),] <bset>, ::x<p> := v, v
