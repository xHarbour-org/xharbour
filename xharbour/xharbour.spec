#
# $Id: xharbour.spec,v 1.54 2004/04/30 19:42:22 druzus Exp $
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>,
# Dave Pearson <davep@davep.org>
# xHarbour RPM spec file
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

######################################################################
# Conditional build:
# --with adsrdd      - build ads RDD
# --with mysql       - build mysql lib
# --with odbc        - build build odbc lib
# --with hrbsh       - build /etc/profile.d/harb.sh (not necessary)
# --without nf       - do not build nanforum lib
# --without ct       - do not build clipper tools lib
######################################################################

######################################################################
## Definitions.
######################################################################

# please add your distro suffix if it not belong to the one recognized below
# and remember that order checking can be important

%define platform %(release=$(rpm -q --queryformat='%{VERSION}' mandrake-release 2>/dev/null) && echo "mdk$release"|tr -d ".")
%if "%{platform}" == ""
%define platform %(release=$(rpm -q --queryformat='%{VERSION}' redhat-release 2>/dev/null) && echo "rh$release"|tr -d ".")
%if "%{platform}" == ""
%define platform %(release=$(rpm -q --queryformat='%{VERSION}' conectiva-release 2>/dev/null) && echo "cl$release"|tr -d ".")
%if "%{platform}" == ""
%define platform %(release=$(rpm -q --queryformat='%{VERSION}' aurox-release 2>/dev/null) && echo "aur$release"|tr -d ".")
%if "%{platform}" == ""
%define platform %([ -f /etc/pld-release ] && cat /etc/pld-release|sed -e '/1/ !d' -e 's/[^0-9]//g' -e 's/^/pld/')
%endif
%endif
%endif
%endif

%define name     xharbour
%define dname    xHarbour
%define version  0.92.0
%define releasen 0
%define prefix   /usr
%define hb_pref  xhb
%define hb_gt    crs
%define hb_gpm   yes
%define hb_mt    MT
%define hb_mgt   yes
%define hb_lnkso yes
%define hb_cc    export HB_COMPILER=gcc
%define hb_cflag export C_USR="-DHB_FM_STATISTICS_OFF -O3"
%define hb_arch  export HB_ARCHITECTURE=linux
%define hb_cmt   export HB_MT=%{hb_mt}
%define hb_cgt   export HB_GT_LIB=gt%{hb_gt}
%define hb_cgpm  export HB_GPM_MOUSE=%{hb_gpm}
%define hb_cmgt  export HB_MULTI_GT=%{hb_mgt}
%define hb_bdir  export HB_BIN_INSTALL=%{prefix}/bin
%define hb_idir  export HB_INC_INSTALL=%{prefix}/include/%{name}
%define hb_ldir  export HB_LIB_INSTALL=%{prefix}/lib/%{name}
%define hb_plat  export HB_PLAT=%{platform}
%define hb_opt   export HB_GTALLEG=%{?_with_allegro:yes}
%define hb_env   %{hb_cc} ; %{hb_cflag} ; %{hb_arch} ; %{hb_cmt} ; %{hb_cgt} ; %{hb_cgpm} ; %{hb_cmgt} ; %{hb_bdir} ; %{hb_idir} ; %{hb_ldir}; %{hb_plat}; %{hb_opt}

%define hb_host  www.xharbour.org
%define readme   README.RPM
######################################################################
## Preamble.
######################################################################

Summary:        Free software Clipper compatible compiler
Summary(pl):    Darmowy kompilator kompatybilny z jЙzykiem Clipper.
Summary(pt_BR): Um compilador Clipper compativel Gratis 
Summary(ru):    Свободный компилятор, совместимый с языком Clipper.
Name:           %{name}
Version:        %{version}
Release:        %{releasen}%{platform}
Prefix:         %{prefix}
Copyright:      GPL (plus exception)
Group:          Development/Languages
Vendor:         %{hb_host}
URL:            http://%{hb_host}/
Source:         %{name}-%{version}.src.tar.gz
Packager:       PrzemysЁaw Czerpak <druzus@polbox.com> Luiz Rafael Culik Guimaraes <culikr@uol.com.br>
BuildPrereq:    gcc binutils bash bison ncurses ncurses-devel gpm-devel
Requires:       gcc binutils bash sh-utils %{name}-lib = %{version}
Provides:       %{name} harbour
BuildRoot:      /tmp/%{name}-%{version}-root

%description
%{dname} is a CA-Clipper compatible compiler for multiple platforms. This
package includes a compiler, pre-processor, header files, virtual machine
and documentation.

See README.RPM in the documentation directory for information specific to
this RPM distribution.

%description -l pl
%{dname} to kompatybilny z jЙzykiem CA-Clipper kompilator rozwijany na
wielu rС©nych platformach. Ten pakiet zawiera kompilator, preprocesor,
zbiory nagЁСwkowe, wirtualn╠ maszynЙ oraz dokumentacjЙ.

%description -l pt_BR
%{dname} ┌ um compilador Clipper compativel para multiplas plataformas.
Esse pacote contem um compilador, um pr┌-processador, arquivos de cabe┤alho
uma maquina virtual e documenta┤фo.

%description -l ru
%{dname} - многоплатформенный компилятор, совместимый с языком CA-Clipper.
Этот пакет содержит компилятор, препроцессор, файлы заголовков, виртуальную
машину и документацию.


######################################################################
## main shared lib
######################################################################

%package lib
Summary:        Shared runtime libaries for %{dname} compiler
Summary(pl):    Dzielone bilioteki dla kompilatora %{dname}
Summary(ru):    Совместно используемые библиотеки для компилятора %{dname}
Group:          Development/Languages
Provides:       lib%{name}.so lib%{name}mt.so

%description lib
%{dname} is a Clipper compatible compiler.
This package provides %{dname} runtime shared libraries for programs
linked dynamically.

%description -l pl lib
%{dname} to kompatybilny z jЙzykiem CA-Clipper kompilator.
Ten pakiet udostЙpnia dzielone bilioteki kompilatora %{dname}
dla programСw konsolidowanych dynamicznie.

%description -l pt_BR lib
%{dname} ┌ um compilador compativel com o Clipper.
Esse pacote %{dname} provem as bibliotecas compartilhadas para programas
linkados dinamicamente.

%description -l ru lib
%{dname} - компилятор, совместимый с языком CA-Clipper.
Этот пакет содержит совместно используемые библиотеки %{dname},
необходимые для работы динамически скомпонованных программ.


######################################################################
## static libs
######################################################################

%package static
Summary:        Static runtime libaries for %{dname} compiler
Summary(pl):    Statyczne bilioteki dla kompilatora %{dname}
Summary(ru):    Статические библиотеки для компилятора %{dname}
Group:          Development/Languages
Requires:       %{name} = %{version}

%description static
%{dname} is a Clipper compatible compiler.
This package provides %{dname} static runtime libraries for static
program linking.

%description -l pl static
%{dname} to kompatybilny z jЙzykiem CA-Clipper kompilator.
Ten pakiet udostЙpnia statyczne bilioteki dla kompilatora %{dname}
niezbЙdne do statycznej konsolidacji programСw.

%description -l pt_BR static
%{dname} ┌ um compilador compativel com o clippe.
Esse pacote %{dname} provem as bibliotecas  de run time staticas para linkagem
dos os programas

%description -l ru static
%{dname} - компилятор, совместимый с языком CA-Clipper.
Этот пакет содержит статические библиотеки компилятора %{dname},
необходимые для статической компоновки программ.


%package contrib
Summary:        Contrib runtime libaries for %{dname} compiler
Summary(pl):    Bilioteki z drzewa contrib dla kompilatora %{dname}
Summary(pt_BR): Libs contrib para %{dname}
Summary(ru):    Библиотеки из дерева contrib для компилятора %{dname}
Group:          Development/Languages
Requires:       %{name} = %{version}

%description contrib
%{dname} is a Clipper compatible compiler.
This package provides %{dname} contrib libraries for program linking.

%description -l pl contrib
%{dname} to kompatybilny z jЙzykiem CA-Clipper kompilator.
Ten pakiet udostЙpnia statyczne bilioteki z drzewa contrib dla
kompilatora %{dname}.

%description -l pt_BR contrib
%{dname} ┌ um compilador compativel com o clippe.
Esse pacote %{dname} provem as bibliotecas contrib para linkagem
dos programas.

%description -l ru contrib
%{dname} - компилятор, совместимый с языком CA-Clipper.
Этот пакет содержит статические библиотеки %{dname} из дерева contrib.


######################################################################
## PP
######################################################################

%package pp
Summary:        Clipper/Harbour/xBase compatible Pre-Processor, DOT prompt and interpreter
Summary(pl):    Kompatybilny z Clipper/Harbour/xBase Preprocesor i interpreter
Summary(ru):    Совместимый с Clipper/Harbour/xBase препроцессор и интерпретатор
Copyright:      GPL
Group:          Development/Languages
Requires:       %{name} = %{version}

%description pp
%{dname} is a Clipper compatible compiler.
This package provides %{dname} PP. It has 3 personalities which are tied
tightly together.
1. What is supposed to be 100% Clipper compatible Pre-Processor
   (with some extensions).
2. DOT prompt, which suppose to allow most of Clipper syntax.
3. Finally, PP is a limited Clipper/Harbour/xBase Interpreter. Subject 
   to those same few limitations it can execute most of Harbour syntax.
   You can write your own xBase scripts by adding to your .prg files
   #!/usr/bin/pprun

%description -l pl pp
%{dname} to kompatybilny z jЙzykiem CA-Clipper kompilator.
Ten pakiet udostЙpnia %{dname} PP, ktСry daje trzy narzЙdzia w jednym.
1. W 100% kompatybilny z Clipperem preprocesor (z pewnymi rozeszerzeniami)
2. ╕rodowisko DOT, w ktСrym mo©na u©ywaФ wiЙkszo╤ci skЁadni Clippera
3. PP to tak©e nieco ograniczony interpreter Clippera. Z uwzglЙdnieniem
   wspomnianych kilku ograniczeЯ potrafi on uruchomiФ wiЙkszo╤Ф skЁadni
   Harbour. Mo©esz napisaФ swСj wЁasny skrypt xBase dodaj╠c do pliku .prg
   #!/usr/bin/pprun

%description -l pt_BR pp
%{dname} ┌ um compilador Clipper compativel.
Esse pacote provem o %{dname} PP. Ele tem 3 caracteristicas dependentes
uma da outra.
1. Que e supostamente ser um Pre-Processor 100% compativel com o Clipper
   (com algumas extenssДes).
2. DOT prompt, que supostamente permite a maioria das syntaxes do Clipper.
3. Finalmente, PP ┌ um limitado Interpretador Clipper/Harbour/xBase . Sujeito  
   com algumas limita┤Дes que pode executar a maioria da syntaxe do Harbour.
   Voce pode escrever seus proprios scritps em .prg ao adicionar as seus arquivos 
   .prg #!/usr/bin/pprun

%description -l ru pp
%{dname} - компилятор, совместимый с языком CA-Clipper.
Этот пакет содержит препроцессор %{dname}, который состоит из трех тесно
связанных частей.
1. 100%-совместимый с Clipper препроцессор (с некоторыми расширениями).
2. DOT Prompt, в котором можно использовать большинство конструкций Clipper.
3. Кроме того, PP - ограниченный интерпретатор Clipper. За исключением
   нескольких описанных ограничений, он может выполнять большинство
   конструкций Harbour. Можно создавать собственные xBase-скрипты путем
   добавления в начало .prg-файла строки:
      #!/usr/bin/pprun


######################################################################
## Preperation.
######################################################################

%prep 
%setup -c %{name}
rm -rf $RPM_BUILD_ROOT

######################################################################
## Build.
######################################################################

%build
%{hb_env}

make -r

# build contrib libraries
libs="%{!?_without_ct: libct} %{!?_without_nf: libnf} %{?_with_adsrdd: rdd_ads} %{?_with_mysql: mysql}"
for l in $libs
do
    pushd contrib/$l
        make -r
    popd
done

######################################################################
## Install.
######################################################################

%install

# Install harbour itself.

%{hb_env}

export _DEFAULT_BIN_DIR=$HB_BIN_INSTALL
export _DEFAULT_INC_DIR=$HB_INC_INSTALL
export _DEFAULT_LIB_DIR=$HB_LIB_INSTALL
export HB_BIN_INSTALL=$RPM_BUILD_ROOT/$HB_BIN_INSTALL
export HB_INC_INSTALL=$RPM_BUILD_ROOT/$HB_INC_INSTALL
export HB_LIB_INSTALL=$RPM_BUILD_ROOT/$HB_LIB_INSTALL

mkdir -p $HB_BIN_INSTALL
mkdir -p $HB_INC_INSTALL
mkdir -p $HB_LIB_INSTALL

make -r -i install

# install contrib libraries
libs="%{!?_without_ct: libct} %{!?_without_nf: libnf} %{?_with_adsrdd: rdd_ads} %{?_with_mysql: mysql}"
for l in $libs
do
    pushd contrib/$l
        make -r -i install
    popd
done

# Keep the size of the binaries to a minimim.
strip $HB_BIN_INSTALL/harbour
# Keep the size of the libraries to a minimim.
strip --strip-debug $HB_LIB_INSTALL/*

mkdir -p $RPM_BUILD_ROOT/etc/harbour
install -m644 source/rtl/gtcrs/hb-charmap.def $RPM_BUILD_ROOT/etc/harbour/hb-charmap.def
cat > $RPM_BUILD_ROOT/etc/harbour.cfg <<EOF
CC=gcc
CFLAGS=-c -I$_DEFAULT_INC_DIR -O3
VERBOSE=YES
DELTMP=YES
EOF

if [ "%{?_with_hrbsh:1}" ]; then
mkdir -p $RPM_BUILD_ROOT/etc/profile.d
cat > $RPM_BUILD_ROOT/etc/profile.d/harb.sh <<EOF
%{hb_cc}
%{hb_arch}
%{hb_bdir}
%{hb_idir}
%{hb_ldir}
%{hb_cgt}
export HB_LEX="SIMPLEX"
export C_USR="-DHB_FM_STATISTICS_OFF -O3"
EOF
chmod 755 $RPM_BUILD_ROOT/etc/profile.d/harb.sh
fi

[ "%{?_with_odbc:1}" ]    || rm -f $RPM_BUILD_ROOT/%{prefix}/lib/%{name}/libhbodbc.a
[ "%{?_with_allegro:1}" ] || rm -f $RPM_BUILD_ROOT/%{prefix}/lib/%{name}/libgtalleg.a

# check if we should rebuild tools with shared libs
if [ "%{hb_lnkso}" = yes ]
then
    unset HB_GTALLEG
    export L_USR="-L${HB_LIB_INSTALL} -l%{name} -lncurses -lslang -lgpm -L/usr/X11R6/lib -lX11"
    #export L_USR="-L${HB_LIB_INSTALL} -l%{name} -lncurses -lslang -lgpm -L/usr/X11R6/lib -lX11 %{?_with_allegro: %(allegro-config --static)}"
    export PRG_USR="\"-D_DEFAULT_INC_DIR='${_DEFAULT_INC_DIR}'\""
    for utl in hbmake hbrun hbpp hbdoc xbscript
    do
        pushd utils/${utl}
        rm -fR "./${HB_ARCHITECTURE}"
        make -r install
        strip ${HB_BIN_INSTALL}/${utl}
        popd
    done
fi
ln -s xbscript ${HB_BIN_INSTALL}/pprun
ln -s xbscript ${HB_BIN_INSTALL}/xprompt

# remove unused files
rm -f ${HB_BIN_INSTALL}/hbdoc ${HB_BIN_INSTALL}/hbtest

# Create a README file for people using this RPM.
cat > doc/%{readme} <<EOF
This RPM distribution of %{dname} includes extra commands to make compiling
and linking with %{dname} a little easier. There are compiler and linker
wrappers called "%{hb_pref}cc", "%{hb_pref}cmp", "%{hb_pref}lnk" and "%{hb_pref}mk".

"%{hb_pref}cc" is a wrapper to the harbour compiler only. It only sets environment
variables. The result of its work is a C file.

Use "%{hb_pref}cmp" exactly as you would use the harbour compiler itself.
The main difference with %{hb_pref}cmp is that it results in an object file,
not a C file that needs compiling down to an object. %{hb_pref}cmp also
ensures that the harbour include directory is seen by the harbour compiler.

"%{hb_pref}lnk" simply takes a list of object files and links them together
with the harbour virtual machine and run-time library to produce an
executable. The executable will be given the basename of the first object
file if not directly set by the "-o" command line switch.

"%{hb_pref}mk" tries to produce an executable from your .prg file. It's a simple
equivalent of cl.bat from the CA-Clipper distribution.

All these scripts accept command line switches:
-o<outputfilename>      # output file name
-static                 # link with static %{dname} libs
-fullstatic             # link with all static libs
-shared                 # link with shared libs (default)
-mt                     # link with multi-thread libs
-gt<hbgt>               # link with <hbgt> GT driver, can be repeated to
                        # link with more GTs. The first one will be
                        #      the default at runtime
-fmstat                 # link with the memory statistics lib
-nofmstat               # do not link with the memory statistics lib (default)
-main=<main_func>       # set the name of main program function/procedure.
                        # if not set then 'MAIN' is used or if it doesn't
                        # exist the name of first public function/procedure
                        # in first linked object module (link)

Link options work only with "%{hb_pref}lnk" and "%{hb_pref}mk" and have no effect
in "%{hb_pref}cc" and "%{hb_pref}cmp".
Other options are passed to %{dname}/C compiler/linker.
To save compatibility with older rpm distributions, "gharbour" can be used
as a synonym of "%{hb_pref}cmp", and "harbour-link" as synonym of "%{hb_pref}lnk"

An example compile/link session looks like:
----------------------------------------------------------------------
druzus@uran:~/tmp$ cat foo.prg
function main()
? "Hello, World!"
return nil

druzus@uran:~/tmp$ %{hb_pref}cmp foo
xHarbour Compiler build 0.80.0 (SimpLex)
Copyright 1999-2003, http://www.xharbour.org http://www.harbour-project.org/
Compiling 'foo.prg'...
Lines 5, Functions/Procedures 2
Generating C source output to 'foo.c'... Done.

druzus@uran:~/tmp$ %{hb_pref}lnk foo.o

druzus@uran:~/tmp$ strip foo

druzus@uran:~/tmp$ ls -l foo
-rwxrwxr-x    1 druzus   druzus       3824 maj 17 02:46 foo
----------------------------------------------------------------------

or using %{hb_pref}mk only:
----------------------------------------------------------------------
druzus@uran:~/tmp$ cat foo.prg
function main()
? "Hello, World!"
return nil

druzus@uran:~/tmp$ %{hb_pref}mk foo
xHarbour Compiler build 0.80.0 (SimpLex)
Copyright 1999-2003, http://www.xharbour.org http://www.harbour-project.org/
Compiling 'foo.prg'...
Lines 5, Functions/Procedures 2
Generating C source output to 'foo.c'... Done.

druzus@uran:~/tmp$ ls -l foo
-rwxrwxr-x    1 druzus   druzus       3824 maj 17 02:46 foo
----------------------------------------------------------------------


In this RPM you will find additional wonderful tools: /usr/bin/pprun
You can run clipper/xbase compatible source files with it if you only
put in their first line:
#!/usr/bin/pprun

For example:
----------------------------------------------------------------------
druzus@uran:~/tmp$ cat foo.prg
#!/usr/bin/pprun
function main()
? "Hello, World!, This is a script !!! :-)"
return nil

druzus@uran:~/tmp$ chmod +x foo.prg

druzus@uran:~/tmp$ ./foo.prg


I hope this RPM is useful. Have fun with %{dname}.

Many thanks to Dave Pearson <davep@davep.org>

Przemyslaw Czerpak <druzus@polbox.com>
EOF

######################################################################
## Post install
######################################################################
%post lib
/sbin/ldconfig

######################################################################
## Post uninstall
######################################################################
#%postun lib
#/sbin/ldconfig

######################################################################
## Clean.
######################################################################

%clean
rm -rf $RPM_BUILD_ROOT

######################################################################
## File list.
######################################################################

%files
%defattr(-,root,root,755)
%doc ChangeLog
%doc doc/*.txt
%doc doc/%{readme}
%doc doc/en/
%doc doc/es/

%dir /etc/harbour
/etc/harbour.cfg
%{?_with_hrbsh:/etc/profile.d/harb.sh}
/etc/harbour/hb-charmap.def
%{prefix}/bin/harbour
%{prefix}/bin/hb-mkslib
%{prefix}/bin/%{hb_pref}-build
%{prefix}/bin/%{hb_pref}cc
%{prefix}/bin/%{hb_pref}cmp
%{prefix}/bin/%{hb_pref}lnk
%{prefix}/bin/%{hb_pref}mk
%{prefix}/bin/gharbour
%{prefix}/bin/harbour-link
#%{prefix}/bin/hbtest
%{prefix}/bin/hbrun
%{prefix}/bin/hbpp
%{prefix}/bin/hbmake
%dir %{prefix}/include/%{name}
%{prefix}/include/%{name}/*

%files static
%defattr(-,root,root,755)
%dir %{prefix}/lib/%{name}
%{prefix}/lib/%{name}/libcodepage.a
%{prefix}/lib/%{name}/libcommon.a
%{prefix}/lib/%{name}/libdb*.a
%{prefix}/lib/%{name}/libdebug.a
%{prefix}/lib/%{name}/libfm*.a
%{prefix}/lib/%{name}/libgt*.a
%{?_with_odbc: %{prefix}/lib/%{name}/libhbodbc.a}
%{prefix}/lib/%{name}/liblang.a
%{prefix}/lib/%{name}/libmacro*.a
%{prefix}/lib/%{name}/libnulsys*.a
%{prefix}/lib/%{name}/libpp*.a
%{prefix}/lib/%{name}/librdd*.a
%{prefix}/lib/%{name}/librtl*.a
%{prefix}/lib/%{name}/libsamples.a
%{prefix}/lib/%{name}/libvm*.a

%files contrib
%defattr(-,root,root,755)
%dir %{prefix}/lib/%{name}
%{!?_without_ct: %{prefix}/lib/%{name}/libct*.a}
%{!?_without_nf: %{prefix}/lib/%{name}/libnf*.a}
%{?_with_adsrdd: %{prefix}/lib/%{name}/librddads*.a}
%{?_with_mysql: %{prefix}/lib/%{name}/libmysql*.a}

%files lib
%defattr(-,root,root,755)
%dir %{prefix}/lib/%{name}
%{prefix}/lib/%{name}/*.so
%{prefix}/lib/*.so

%files pp
%defattr(-,root,root,755)
%doc utils/xbscript/xbscript.txt
%{prefix}/bin/xbscript
%{prefix}/bin/pprun
%{prefix}/bin/xprompt

######################################################################
## Spec file Changelog.
######################################################################

%changelog
* Sun Mar 07 2004 Phil Krylov <phil@newstar.rinet.ru>
- Russian translation added.

* Thu Sep 11 2003 Przemyslaw Czerpak <druzus@polbox.com>
- automatic platform checking - now MDK and RH, please add others

* Sat Aug 09 2003 Przemyslaw Czerpak <druzus@polbox.com>
- removed ${RPM_OPT_FLAGS} from C_USR

* Wed Jul 23 2003 Przemyslaw Czerpak <druzus@polbox.com>
- fixed file (user and group) owner for RPMs builded from non root account
- shared lib names changed from [x]harbour{mt,}.so to
  [x]harbour{mt,}-<version>.so and soft links with short names created
- 0.82 version set

* Wed Apr 30 2003 Przemyslaw Czerpak <druzus@polbox.com>
- new tool "%{hb_pref}-build" (%{hb_pref}cmp, %{hb_pref}lnk, %{hb_pref}mk) added -
  compiler/linker wrapper.
- new tool "hb-mkslib" (build shared libraries from static ones and object
  files).
- shared libraries added.
- binary package divided.

* Fri Mar 08 2002 Dave Pearson <davep@davep.org>
- Fixed gharbour so that it should work no matter the case of the name of
  the PRG file.

* Wed Feb 13 2002 Dave Pearson <davep@davep.org>
- Fixed a bug in harbour-link which meant that, since the environment
  changes of Jan 17 2002, users could not specify which GT library they
  wanted their application linked against.

* Tue Jan 22 2002 Dave Pearson <davep@davep.org>
- Used the "name" macro a lot more, especially in some paths.

* Thu Jan 17 2002 Dave Pearson <davep@davep.org>
- Removed the use of the /etc/profile.d scripts for setting the
  harbour environment variables. The settings are now placed
  directly in gharbour and harbour-link. This means that this .spec
  file should be more useful on RPM using platforms other than RedHat.

* Wed Dec 19 2001 Dave Pearson <davep@davep.org>
- Added a platform ID to the name of the RPM.

* Mon Dec 17 2001 Dave Pearson <davep@davep.org>
- todo.txt is now called TODO.

* Tue Aug 21 2001 Dave Pearson <davep@davep.org>
- Added todo.txt as a doc file.

* Sun Jul 22 2001 Dave Pearson <davep@davep.org>
- harbour-link now fully respects the setting of $HB_GT_LIB.
- HB_GT_LIB wasn't set in the csh startup script. Fixed.

* Fri Jul 20 2001 Dave Pearson <davep@davep.org>
- Added the setting of $HB_GT_LIB to the environment (ncurses is used).
- Added support for installing hbmake.

* Mon Jun 28 2001 Dave Pearson <davep@davep.org>
- Changed the gharbour script so that it only invokes the C compiler if a C
  file was output. This stops any error messages when someone is using the
  -g option to output other target types.

* Mon Mar 19 2001 Dave Pearson <davep@davep.org>
- Reinstated hbrun in the files section.

* Tue Feb 20 2001 Dave Pearson <davep@davep.org>
- Added README.RPM to the documentation directory.

* Sat Jan 06 2001 Dave Pearson <davep@davep.org>
- The gharbour script now passes the harbour include directory, using -I, 
  to harbour.

* Thu Aug 24 2000 Dave Pearson <davep@davep.org>
- Changed the files section so that hbrun doesn't get installed. It isn't
  useful on GNU/Linux systems.

* Tue Aug 22 2000 Dave Pearson <davep@davep.org>
- Changed the 'egcs' requirement to 'gcc'.

* Mon Aug 21 2000 Przemyslaw Czerpak <druzus@polbox.com>
- Polish translation added
- BuildRoot marco added. Now you can build the package from normal user
  account.
- bison and flex added to BuildPrereq list
- Debug information is stripped from installed files.

* Wed Aug 02 2000 Dave Pearson <davep@davep.org>
- Removed hbtest from the list of files installed into the bin directory.
- Added 'bash' and 'sh-utils' to the list of required packages.
  
* Tue Aug 01 2000 Dave Pearson <davep@davep.org>
- Added harbour environment scripts to /etc/profile.d.
- Added generation of gharbour and harbour-link commands.

* Mon Jul 31 2000 Dave Pearson <davep@davep.org>
- Re-worked the layout of the spec file to make it cleaner and easier to 
  read and maintain.
- The latest harbour ChangeLog is now installed into the RPM's doc
  directory.
- The content of the RPM's doc directory reflects the layout and content of 
  the harbour source's doc directory.
