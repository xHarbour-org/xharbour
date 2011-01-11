#include "winapi.ch"
#include "hbclass.ch"
#include "commctrl.ch"
#include "wingdi.ch"

#xtranslate __CONCAT <x> <y> => <x>[<y>]

#xcommand PROPERTY <p>  [AS <astype>] [INDEX <i>] [INDEX1 <j>] READ <rf> [DEFAULT <d>] [<prot: PROTECTED>] [MIN <m>] [MAX <x>];
           =>  ;
           DATA __CONCAT x <p> [AS <astype>] [<prot>] [INIT <d>] ;
           ; ACCESS <p>    INLINE ::<rf> PERSISTENT ;
           ; ASSIGN <p>(v) INLINE [v := MAX( <m>, v ),] [v := MIN( <x>, v ),] ::<rf> := v, v

#xcommand PROPERTY <p>  [AS <astype>] [INDEX <i>] [INDEX1 <j>] READ <rf> WRITE <wf> [DEFAULT <d>] [<prot: PROTECTED>] [MIN <m>] [MAX <x>];
           =>  ;
           DATA __CONCAT x <p> [AS <astype>] [<prot>] [INIT <d>] ;
           ; ACCESS <p>    INLINE ::<rf> PERSISTENT ;
           ; ASSIGN <p>(v) INLINE [v := MAX( <m>, v ),] [v := MIN( <x>, v ),] ::<rf> := v, ::<wf>( [ <i>,] [ <j>,] v ), v


#xcommand PROPERTY <p>  [AS <astype>] [INDEX <i>] [INDEX1 <j>] READ <rf> WRITE <wf> [DEFAULT <d>] [<prot: PROTECTED>] [MIN <m>] [MAX <x>] INVERT;
           =>  ;
           DATA __CONCAT x <p> [AS <astype>] [<prot>] [INIT <d>] ;
           ; ACCESS <p>    INLINE ::<rf> PERSISTENT ;
           ; ASSIGN <p>(v) INLINE [v := MAX( <m>, v ),] [v := MIN( <x>, v ),] ::<wf>( [ <i>,] [ <j>,] @v ), ::<rf> := v, v


#xcommand PROPERTY <p>  [AS <astype>] [INDEX <i>] [INDEX1 <j>] READ <rf> WRITE <wf> [DEFAULT <d>] [<prot: PROTECTED>] [MIN <m>] [MAX <x>] HIDDEN;
           =>  ;
           DATA __CONCAT x <p> [AS <astype>] [<prot>] [INIT <d>] ;
           ; ACCESS <p>    INLINE ::<rf> ;
           ; ASSIGN <p>(v) INLINE [v := MAX( <m>, v ),] [v := MIN( <x>, v ),] ::<rf> := v, ::<wf>( [<i>,] [ <j>,] v ), v



//----------------------------------------------------------------------------------------------------------------------------------


#xcommand PROPERTY <p> [PARAM <i>] GET <rf> [DEFAULT <d>] [<prot: PROTECTED>];
           =>  ;
           DATA __CONCAT x <p>  [<prot>] [INIT <d>] ;
           ; ACCESS <p>    INLINE <rf> PERSISTENT ;
           ; ASSIGN <p>(v) INLINE ::x<p> := v, v

#xcommand PROPERTY <p> [PARAM <i>] SET <wf> [DEFAULT <d>] [<prot: PROTECTED>];
           =>  ;
           DATA __CONCAT x <p> [<prot>] [INIT <d>] ;
           ; ACCESS <p>    INLINE ::x<p> PERSISTENT ;
           ; ASSIGN <p>(v) INLINE ::<wf>( [ <i>,] @v ), ::x<p> := v, v

#xcommand PROPERTY <p> [PARAM <i>] GET <rf> SET <wf> [DEFAULT <d>] [<prot: PROTECTED>];
           =>  ;
           DATA __CONCAT x <p> [<prot>] [INIT <d>] ;
           ; ACCESS <p>    INLINE <rf> PERSISTENT ;
           ; ASSIGN <p>(v) INLINE ::<wf>( [ <i>,] @v ), ::x<p> := v, v
