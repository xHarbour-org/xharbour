#
# $Id: xharbour.spec,v 1.3 2003/05/29 20:44:23 druzus Exp $
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>,
# Dave Pearson <davep@davep.org>
# xHarbour RPM spec file
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

######################################################################
## Definitions.
######################################################################

%define name     xharbour
%define dname    xHarbour
%define version  0.80.0
%define releasen 1
%define platform rh73
%define prefix   /usr
%define hb_pref  xhb
%define hb_gt    crs
%define hb_mt    MT
%define hb_mgt   yes
%define hb_libs  vm pp rtl rdd dbfcdx dbfntx odbc macro common lang codepage gtnul gtcrs gtsln gtcgi gtstd gtpca debug

%define hb_cc    export HB_COMPILER="gcc"
%define hb_cflag export C_USR="-DHB_FM_STATISTICS_OFF -O3"
%define hb_arch  export HB_ARCHITECTURE="linux"
%define hb_cmt   export HB_MT="%{hb_mt}"
%define hb_cgt   export HB_GT_LIB="gt%{hb_gt}"
%define hb_cmgt  export HB_MULTI_GT="%{hb_mgt}"
%define hb_bdir  export HB_BIN_INSTALL="%{prefix}/bin/"
%define hb_idir  export HB_INC_INSTALL="%{prefix}/include/%{name}/"
%define hb_ldir  export HB_LIB_INSTALL="%{prefix}/lib/%{name}/"
%define hb_env   %{hb_cc} ; %{hb_cflag} ; %{hb_arch} ; %{hb_cmt} ; %{hb_cgt} ; %{hb_cmgt} ; %{hb_bdir} ; %{hb_idir} ; %{hb_ldir}
%define hb_host  www.xharbour.org
%define readme   README.RPM
######################################################################
## Preamble.
######################################################################

Summary:        Free software Clipper compatible compiler
Summary(pl):    Darmowy kompilator kompatybilny z jêzykiem Clipper.
Name:           %{name}
Version:        %{version}
Release:        %{releasen}%{platform}
Prefix:         %{prefix}
Copyright:      GPL (plus exception)
Group:          Development/Languages
Vendor:         %{hb_host}
URL:            http://%{hb_host}/
Source:         %{name}-%{version}.src.tar.gz
Packager:       Przemys³aw Czerpak <druzus@polbox.com>
BuildPrereq:    gcc binutils bash flex bison ncurses ncurses-devel slang-devel gpm-devel
Requires:       gcc binutils bash sh-utils %{name}-lib = %{version}
Provides:       %{name}
BuildRoot:      /tmp/%{name}-%{version}-root

%description
%{dname} is a CA-Clipper compatible compiler for multiple platforms. This
package includes a compiler, pre-processor, header files, virtual machine
and documentation.

See README.RPM in the documentation directory for information specific to
this RPM distribution.

%description -l pl
%{dname} to kompatybilny z jêzykiem CA-Clipper kompilator rozwijany na
wielu ró¿nych platformach. Ten pakiet zawiera kompilator, preprocesor,
zbiory nag³ówkowe, wirtualn± maszynê oraz dokumentacjê.


######################################################################
## main shared lib
######################################################################

%package lib
Summary:        Shared runtime libaries for %{dname} compiler
Summary(pl):    Dzielone bilioteki dla kompilatora %{dname}
Group:          Development/Languages

%description lib
%{dname} is a Clipper compatible compiler.
This package provides %{dname} runtime shared libraries for programs
linked dynamically.

%description -l pl lib
%{dname} to kompatybilny z jêzykiem CA-Clipper kompilator.
Ten pakiet udostêpnia dzielone bilioteki kompilatora %{dname}
dla programów konsolidowanych dynamicznie.


######################################################################
## static libs
######################################################################

%package static
Summary:        Static runtime libaries for %{dname} compiler
Summary(pl):    Statyczne bilioteki dla kompilatora %{dname}
Group:          Development/Languages
Requires:       %{name} = %{version}

%description static
%{dname} is a Clipper compatible compiler.
This package provides %{dname} static runtime libraries for static
program linking.

%description -l pl static
%{dname} to kompatybilny z jêzykiem CA-Clipper kompilator.
Ten pakiet udostêpnia statyczne bilioteki dla kompilatora %{dname}
niezbêdne do statycznej konsolidacji programów.

######################################################################
## PP
######################################################################

%package pp
Summary:        Clipper/Harbour/xBase compatible Pre-Processor, DOT prompt and interpreter
Summary(pl):    Kompatybilny z Clipper/Harbour/xBase Preprocesor i interpreter
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
%{dname} to kompatybilny z jêzykiem CA-Clipper kompilator.
Ten pakiet udostêpnia %{dname} PP, który daje trzy narzêdzia w jednym.
1. W 100% kompatybilny z Clipperem preprocesor (z pewnymi rozeszerzeniami)
2. ¦rodowisko DOT, w którym mo¿na u¿ywaæ wiêkszo¶ci sk³adni Clippera
3. PP to tak¿e nieco ograniczony interpreter Clippera. Z uwzglêdnieniem
   wspomnianych kilku ograniczeñ potrafi on uruchomiæ wiêkszo¶æ sk³adni
   Harbour. Mo¿esz napisaæ swój w³asny skrypt dodaj±c do pliku .prg
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
make

######################################################################
## Install.
######################################################################

%install

# Install harbour itself.

%{hb_env}
_DEFAULT_INC_DIR=$HB_INC_INSTALL
export HB_BIN_INSTALL=$RPM_BUILD_ROOT/$HB_BIN_INSTALL
export HB_INC_INSTALL=$RPM_BUILD_ROOT/$HB_INC_INSTALL
export HB_LIB_INSTALL=$RPM_BUILD_ROOT/$HB_LIB_INSTALL

mkdir -p $HB_BIN_INSTALL
mkdir -p $HB_INC_INSTALL
mkdir -p $HB_LIB_INSTALL

make -i install

# Keep the size of the binaries to a minimim.
strip --strip-debug $HB_BIN_INSTALL/*
# Keep the size of the libraries to a minimim.
strip --strip-debug $HB_LIB_INSTALL/*

install bin/hb-mkslib.sh $HB_BIN_INSTALL/hb-mkslib

pushd $HB_LIB_INSTALL
LIBS=""
LIBSMT=""
for l in %{hb_libs}
do
    ls="lib${l}.a"
    if [ -f lib${l}mt.a ]
    then
	lm="lib${l}mt.a"
    else
	lm="${ls}"
    fi
    if [ -f $ls ]
    then
	LIBS="$LIBS $ls"
    fi
    if [ -f $lm ]
    then
	LIBSMT="$LIBSMT $lm"
    fi
done
$HB_BIN_INSTALL/hb-mkslib lib%{name}.so $LIBS
[ $HB_MT != "MT" ] || $HB_BIN_INSTALL/hb-mkslib lib%{name}mt.so $LIBSMT
cd ..
for l in lib%{name}.so lib%{name}mt.so
do
    [ -f %{name}/$l ] && ln -s %{name}/$l $l
done
LD_LIBRARY_PATH="$HB_LIB_INSTALL:$LD_LIBRARY_PATH"
popd

# Add a harbour compiler wrapper.
cat > $HB_BIN_INSTALL/%{hb_pref}-build <<EOF
#!/bin/bash

if [ \$# == 0 ]; then
    echo "syntax: \$0 [<options,...>] <file>[.prg|.o]"
    exit 1
elif [ "\$*" == "mk-links" ]; then
    DIR="\${0%/*}"
    NAME="\${0##*/}"
    if [ "\${DIR}" != "\${NAME}" ]; then
	for n in %{hb_pref}cc %{hb_pref}cmp %{hb_pref}mk %{hb_pref}lnk gharbour harbour-link; do
	    ln -sf "\${NAME}" "\${DIR}/\${n}"
	done
    fi
    exit
fi

## default parameters
HB_STATIC="no"
HB_MT=""
HB_GT="%{hb_gt}"


HB_GT_REQ=""
_TMP_FILE_="/tmp/hb-build-\$USER-\$\$.c"

## parse params
P=( "\$@" ); n=0; DIROUT="."; FILEOUT=""
while [ \$n -lt \${#P[@]} ]; do
    v=\${P[\$n]}; p=""
    case "\$v" in
	-o*)
	    d="\${v#-o}"; p="\${v}"
	    if [ -d "\${d}" ]; then
		DIROUT="\${d%/}"
	    elif [ -d "\${d%/*}" ]; then
		DIROUT="\${d%/*}"; FILEOUT="\${d##*/}"; p="-o\${d%.*}"
	    elif [ -n "\${d}" ]; then
		FILEOUT="\${d}"; p="-o\${d%.*}"
	    fi ;;
	-static)    HB_STATIC="yes" ;;
	-shared)    HB_STATIC="no" ;;
	-mt)        HB_MT="MT" ;;
	-gt*)       HB_GT_REQ="\${HB_GT_REQ} \${v#-gt}" ;;
	-*)         p="\${v}" ;;
	*)          [ -z \${FILEOUT} ] && FILEOUT="\${v##*/}"; p="\${v}" ;;
    esac
    [ -n "\$p" ] && PP[\$n]="\$p"
    n=\$[\$n + 1]
done
P=( "\${PP[@]}" )

case "\${HB_MT}" in
    [Mm][Tt]|[Yy][Ee][Ss]|1)	HB_MT="MT";;
    *)	HB_MT="";;
esac

SYSTEM_LIBS="-lm -lncurses -lslang -lgpm"
# use pthread system library for MT programs
if [ "\${HB_MT}" = "MT" ]; then
    SYSTEM_LIBS="-lpthread \${SYSTEM_LIBS}"
fi

[ -z "\${HB_GT_REQ}" ] && HB_GT_REQ="\${HB_GT}"
HB_GT_REQ=\`echo \${HB_GT_REQ}|tr a-z A-Z\`

# set environment variables
%{hb_cc}
%{hb_arch}
[ -z "\${HB_BIN_INSTALL}" ] && %{hb_bdir}
[ -z "\${HB_INC_INSTALL}" ] && %{hb_idir}
[ -z "\${HB_LIB_INSTALL}" ] && %{hb_ldir}

# be sure that %{name} binaries are in your path
export PATH="\${HB_BIN_INSTALL}:\${PATH}"

HB_PATHS="-I\${HB_INC_INSTALL}"
GCC_PATHS="\${HB_PATHS} -L\${HB_LIB_INSTALL}"

if [ "\${HB_STATIC}" = "yes" ]; then
    HARBOUR_LIBS=""
    for l in %{hb_libs}
    do
	[ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.a" ] && l="\${l}mt"
	[ -f "\${HB_LIB_INSTALL}/lib\${l}.a" ] && HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
    done
    HARBOUR_LIBS="-Wl,--start-group \${HARBOUR_LIBS} -Wl,--end-group"
else
    if [ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib%{name}mt.so" ]; then
	HARBOUR_LIBS="-l%{name}mt"
    else
	HARBOUR_LIBS="-l%{name}"
    fi
fi

FOUTC="\${DIROUT}/\${FILEOUT%.*}.c"
FOUTO="\${DIROUT}/\${FILEOUT%.*}.o"
FOUTE="\${DIROUT}/\${FILEOUT%.[Pp][Rr][Gg]}"
FOUTE="\${FOUTE%.[oc]}"

hb_cc()
{
    harbour "\$@" \${HB_PATHS} && [ -f "\${FOUTC}" ] 
}

hb_link()
{
    if [ -n "\${HB_GT_REQ}" ]; then
	hb_gt_request \${HB_GT_REQ} > \${_TMP_FILE_} && \\
	gcc "\$@" "\${_TMP_FILE_}" \${GCC_PATHS} \${SYSTEM_LIBS} \${HARBOUR_LIBS} -o "\${FOUTE}"
    else
	gcc "\$@" \${GCC_PATHS} \${SYSTEM_LIBS} \${HARBOUR_LIBS} -o "\${FOUTE}"
    fi
}

hb_cmp()
{
    hb_cc "\$@" && \\
    gcc -g -c "\${FOUTC}" -o "\${FOUTO}" \${GCC_PATHS} && \\
    rm -f "\${FOUTC}"
}

hb_gt_request()
{
    echo "#include \\"hbapi.h\\""
    if [ "\${HB_STATIC}" = "yes" ]; then
	for gt in "\$@"; do
	    echo "extern HB_FUNC( HB_GT_\${gt} );"
	done
	echo "void hb_gt_ForceLink_build( void )"
	echo "{"
	for gt in "\$@"; do
	    echo "HB_FUNCNAME( HB_GT_\${gt} )();"
	done
	echo "}"
    fi
    if [ -n "\$1" ]; then
	echo "#include \\"hbinit.h\\""
	echo "extern char * s_defaultGT;"
	echo "HB_CALL_ON_STARTUP_BEGIN( hb_gt_SetDefault_build )"
	echo "   s_defaultGT = \\"\$1\\";"
	echo "HB_CALL_ON_STARTUP_END( hb_gt_SetDefault_build )"
    fi
}

hb_cleanup()
{
    rm -f "\${_TMP_FILE_}"
}

trap hb_cleanup EXIT &>/dev/null

## get basename
HB="\${0##*/}"

case "\${HB}" in
    *cc)
	hb_cc "\${P[@]}"
	;;
    *cmp|gharbour)
	hb_cmp "\${P[@]}"
	;;
    *lnk|harbour-link)
	hb_link "\${P[@]}"
	;;
    *mk)
	hb_cmp "\${P[@]}" && \\
	hb_link "\${FOUTO}" && \\
	strip "\${FOUTE}" && \\
	rm -f "\${FOUTO}"
	;;
esac
EOF
chmod 755 $HB_BIN_INSTALL/%{hb_pref}-build
$HB_BIN_INSTALL/%{hb_pref}-build mk-links


# Create PP
pushd tests
$HB_BIN_INSTALL/xhbmk pp -n -w -es2 -D_DEFAULT_INC_DIR=\"$_DEFAULT_INC_DIR\"
install pp $HB_BIN_INSTALL/pp
ln -s pp $HB_BIN_INSTALL/pprun
install rp_dot.ch $HB_INC_INSTALL/
popd


# Create a README file for people using this RPM.
cat > doc/%{readme} <<EOF
This RPM distribution of %{dname} includes extra commands to make compiling
and linking with harbour a little easier. There are a compiler and linker
wrappers called "%{hb_pref}cc", "%{hb_pref}cmp", "%{hb_pref}lnk" and "%{hb_pref}mk"

"%{hb_pref}cc" is a wrapper to harbour compiler only. It only sets environment
variables. The result of its work is an C file.

Use "%{hb_pref}cmp" exactly as you would use the harbour compiler itself.
The main difference with %{hb_pref}cmp is that it results in an object file,
not a C file that needs compiling down to an object. %{hb_pref}cmp also
ensures that the harbour include directory is seen by the harbour compiler.

"%{hb_pref}lnk" simply takes a list of object files and links them together
with the harbour virtual machine and run-time library to produce an
executable. The executable will be given basename of the first object
file if not directly set bu "-o" command line switch

"%{hb_pref}mk" try to produce executable from your .prg file. It's a simple
equivalent of cl.bat from CA-Clipper distribution.

all this scripts accept command line switches:
-o<outputfilename>      # output file name
-static                 # link with static libs
-shared                 # link with shared libs (default)
-mt                     # link with multi-thread libs
-gt<hbgt>               # link with <hbgt> GT driver, can be repeated to
                        # link with more GTs. The first one will be default
                        # on runtime

link options work only with "%{hb_pref}lnk" and "%{hb_pref}mk" and has no effect
in "%{hb_pref}cc" and "%{hb_pref}cmp"
To save compatibility with older rpm distribution "gharbour" can be used
as synonym of "%{hb_pref}cmp" and "harbor-link" as synonym of "%{hb_pref}lnk"

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

Dave Pearson <davep@davep.org>
Przemyslaw Czerpak <druzus@polbox.com>
EOF

######################################################################
## Post install
######################################################################
#%post lib
#/sbin/ldconfig

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

%{prefix}/bin/harbour
%{prefix}/bin/hb-mkslib
%{prefix}/bin/%{hb_pref}-build
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
%dir %{prefix}/lib/%{name}
%{prefix}/lib/%{name}/*.a

%files lib
%dir %{prefix}/lib/%{name}
%{prefix}/lib/%{name}/*.so
%{prefix}/lib/*.so

%files pp
%doc tests/pp.txt
%doc tests/*.txt
%{prefix}/bin/pp
%{prefix}/bin/pprun

######################################################################
## Spec file Changelog.
######################################################################

%changelog
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
