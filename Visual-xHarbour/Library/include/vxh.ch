#include "winapi.ch"
#include "hbclass.ch"
#include "commctrl.ch"
#include "wingdi.ch"

#xtranslate __CONCAT <x> <y> => <x>[<y>]

#xcommand VXH_METHOD <p>([<params,...>]) [<hid: NOTPUBLIC>];
           =>  ;
           if ! <.hid.> ;;
              DATA __CONCAT __m_ <p> INIT {<(p)>} ;;
           endif ;;
           METHOD <p>([<params>])

#xcommand VXH_METHOD <p>([<params,...>]) INLINE <Code,...> [<hid: NOTPUBLIC>];
           =>  ;
           if ! <.hid.> ;;
              DATA __CONCAT __m_ <p> INIT {<(p)>} ;;
           endif ;;
           METHOD <p>([<params>]) [INLINE <Code>]


#xcommand PROPERTY <p> [ROOT <r>] [DEFAULT <d>] [HELP <h>] [<hid: NOTPUBLIC>];
           =>  ;
           if ! <.hid.> ;;
              DATA <p> PUBLISHED [INIT <d>] ;;
              DATA __CONCAT __a_ <p> INIT {<(p)>,[<r>],[<h>],[<d>]} ;;
           else ;;
              DATA <p> [INIT <d>] ;;
           endif ;;

#xcommand PROPERTY <p> [ROOT <r>] GET <bget> [DEFAULT <d>] [<prot: PROTECTED>] [HELP <h>] [<hid: NOTPUBLIC>] [MIN <m>] [MAX <x>] ;
           =>  ;
           DATA __CONCAT x <p>  [<prot>] [INIT <d>] ;;
           if ! <.hid.> ;;
              DATA __CONCAT __a_ <p> INIT {<(p)>,[<r>],[<h>],[<d>]} ;;
              ACCESS <p>    INLINE <bget> PERSISTENT ;;
           else ;;
              ACCESS <p>    INLINE <bget> ;;
           endif ;;
           ASSIGN <p>(v) INLINE [v := MAX( <m>, v ),] [v := MIN( <x>, v ),] ::x<p> := v, v

#xcommand PROPERTY <p> [ROOT <r>] SET <bset> [DEFAULT <d>] [<prot: PROTECTED>] [HELP <h>] [<hid: NOTPUBLIC>] [MIN <m>] [MAX <x>] ;
           =>  ;
           DATA __CONCAT x <p> [<prot>] [INIT <d>] ;;
           if ! <.hid.> ;;
              DATA __CONCAT __a_ <p> INIT {<(p)>,[<r>],[<h>],[<d>]} ;;
              ACCESS <p>    INLINE ::x<p> PERSISTENT ;;
           else ;;
              ACCESS <p>    INLINE ::x<p> ;;
           endif ;;
           ASSIGN <p>(v) INLINE [v := MAX( <m>, v ),] [v := MIN( <x>, v ),] Eval( {|Self,v|(Self,v), <bset>}, Self, @v ), ::x<p> := v, v

#xcommand PROPERTY <p> [ROOT <r>] GET <bget> SET <bset> [DEFAULT <d>] [<prot: PROTECTED>] [HELP <h>] [<hid: NOTPUBLIC>] [MIN <m>] [MAX <x>] ;
           =>  ;
           DATA __CONCAT x <p> [<prot>] [INIT <d>] ;;
           if ! <.hid.> ;;
              DATA __CONCAT __a_ <p> INIT {<(p)>,[<r>],[<h>],[<d>]} ;;
              ACCESS <p>    INLINE <bget> PERSISTENT ;;
           else ;;
              ACCESS <p>    INLINE <bget> ;;
           endif ;;
           ASSIGN <p>(v) INLINE [v := MAX( <m>, v ),] [v := MIN( <x>, v ),] Eval( {|Self,v|(Self,v), <bset>}, Self, @v ), ::x<p> := v, v



