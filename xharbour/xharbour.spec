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
%define hb_gt    gtcrs
%define hb_mt    MT
%define hb_mgt   yes
%define hb_libs  vm pp rtl rdd dbfcdx dbfntx odbc macro common lang codepage gtnul gtcrs gtsln gtcgi gtstd gtpca debug

%define hb_cc    export HB_COMPILER=gcc
%define hb_cflag export C_USR="-DHB_FM_STATISTICS_OFF -O3"
%define hb_arch  export HB_ARCHITECTURE=linux
%define hb_cmt   export HB_MT=%{hb_mt}
%define hb_cgt   export HB_GT_LIB=%{hb_gt}
%define hb_cmgt  export HB_MULTI_GT=%{hb_mgt}
%define hb_bdir  export HB_BIN_INSTALL=%{prefix}/bin/
%define hb_idir  export HB_INC_INSTALL=%{prefix}/include/%{name}/
%define hb_ldir  export HB_LIB_INSTALL=%{prefix}/lib/%{name}/
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
BuildPrereq:    gcc binutils bash flex bison ncurses ncurses-devel slang-devel
Requires:       gcc binutils bash sh-utils %{name}-lib
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
popd


# Add a harbour compiler wrapper.
cat > $HB_BIN_INSTALL/%{hb_pref}-build <<EOF
#!/bin/bash

## defaults
HB_STATIC="no"
HB_MT=""
HB_GT_LIB=%{hb_gt}

## parse params
n=0
P=( "\$@" )
DIROUT="."
FILEOUT=""
while [ \$n -lt \${#P[@]} ]; do
    v=\${P[\$n]}
    case "\$v" in
	-o*)
	    d="\${v#-o}"
	    if [ -d "\${d}" ]; then
		DIROUT="\${d%/}"
	    elif [ -d "\${d%/*}" ]; then
		DIROUT="\${d%/*}"
		FILEOUT="\${d##*/}"
		v="-o\${d%.*}"
	    elif [ -n "\${d}" ]; then
		FILEOUT="\${d}"
		v="-o\${d%.*}"
	    fi
	    ;;
	-static)
	    HB_STATIC="yes"
	    v=""
	    ;;
	-shared)
	    HB_STATIC="no"
	    v=""
	    ;;
	-mt)
	    HB_MT="MT"
	    v=""
	    ;;
	-gt*)
	    HB_GT_LIB="\${v#-}"
	    v=""
	    ;;
	-*)
	    ;;
	*)
	    [ -z \${FILEOUT} ] && FILEOUT="\${v##*/}"
	    ;;
    esac
    if [ -n "\$v" ]; then
      PP[\$n]="\$v"
    fi
    n=\$[\$n + 1]
done
P=( "\${PP[@]}" )

if [ \$# == 0 ]; then
    echo "syntax: \$0 [<options,...>] <file>[.prg|.o]"
    exit 1
fi

## get basename
HB="\${0##*/}"

case "\${HB_MT}" in
    [Mm][Tt]|[Yy][Ee][Ss]|1)	HB_MT="MT";;
    *)	HB_MT="";;
esac

# Attempt to get the GT library setting from the user's environment.
if [ "\${HB_STATIC}" != "yes" ]; then
    HB_GT_LIB=%{hb_gt}
fi

# Work out which system library is needed for the choice of GT library.
if [ "\${HB_STATIC}" = "yes" ]; then
    case "\${HB_GT_LIB}" in
        gtcrs*)	TERM_LIB="-lncurses -lgpm";;
        gtsln*)	TERM_LIB="-lslang -lgpm";;
        *)	TERM_LIB="";;
    esac
else
    TERM_LIB="-lncurses -lslang -lgpm"
fi

SYSTEM_LIBS="-lm \${TERM_LIB}"
# use pthread system library for MT programs
if [ "\${HB_MT}" = "MT" ]; then
    SYSTEM_LIBS="-lpthread \${SYSTEM_LIBS}"
fi

# set environment variables
%{hb_cc}
%{hb_cflag}
%{hb_arch}
%{hb_bdir}
%{hb_idir}
%{hb_ldir}
export HB_GT_LIB
export HB_MT

# be sure that harbour binaries are in your path
export PATH="\${HB_BIN_INSTALL}:\${PATH}"


HB_PATHS="-I\${HB_INC_INSTALL}"
GCC_PATHS="\${HB_PATHS} -L\${HB_LIB_INSTALL}"

if [ "\${HB_STATIC}" = "yes" ]; then
    HARBOUR_LIBS=""
    for l in %{hb_libs}
    do
	case "\${l}" in
	    gtnul) ;;
	    gt*)
		if [ "\${l}" != "\${HB_GT_LIB}" ]
		then
		    l="--"
		fi
		;;
	    *) ;;
	esac
	if [ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.a" ]
	then
	    l="\${l}mt"
	fi
	if [ -f "\${HB_LIB_INSTALL}/lib\${l}.a" ]
	then
	    HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
	fi
    done
    HARBOUR_LIBS="-Wl,--start-group \${HARBOUR_LIBS} -Wl,--end-group"
else
    if [ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib%{name}mt.so" ]
    then
	HARBOUR_LIBS="-l%{name}mt"
    else
	HARBOUR_LIBS="-l%{name}"
    fi
fi

FOUTC="\${DIROUT}/\${FILEOUT%.*}.c"
FOUTO="\${DIROUT}/\${FILEOUT%.*}.o"
FOUTE="\${DIROUT}/\${FILEOUT%.[Pp][Rr][Gg]}"
FOUTE="\${FOUTE%.[oc]}"

case "\${HB}" in
    *cmp|gharbour)
	harbour "\${P[@]}" \${HB_PATHS} && [ -f "\${FOUTC}" ] && \\
	    gcc -g -c "\${FOUTC}" -o "\${FOUTO}" \${GCC_PATHS} && \\
	    rm -f "\${FOUTC}"
	;;
    *lnk|harbour-link)
	gcc "\${P[@]}" -L\${HB_LIB_INSTALL} \${SYSTEM_LIBS} \${HARBOUR_LIBS} -o "\${FOUTE}"
	;;
    *mk)
	harbour "\${P[@]}" \${HB_PATHS} && [ -f "\${FOUTC}" ] && \\
	    gcc -g -c "\${FOUTC}" -o "\${FOUTO}" \${GCC_PATHS} && \\
	    rm -f "\${FOUTC}" && \\
	    gcc "\${FOUTO}" -L\${HB_LIB_INSTALL} \${SYSTEM_LIBS} \${HARBOUR_LIBS} -o "\${FOUTE}" && \\
	    strip "\${FOUTE}" && \\
	    rm -f "\${FOUTO}"
	;;
esac
EOF
chmod 755 $HB_BIN_INSTALL/%{hb_pref}-build
for i in %{hb_pref}cmp %{hb_pref}lnk %{hb_pref}mk gharbour harbour-link
do
    ln -s %{hb_pref}-build $HB_BIN_INSTALL/$i
done


# Create a README file for people using this RPM.
cat > doc/%{readme} <<EOF
This RPM distribution of %{dname} includes three extra commands to make
compiling and linking with harbour a little easier. There are a compiler
and linker wrappers called "%{hb_pref}cmp", "%{hb_pref}lnk" and "%{hb_pref}mk"

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
-shared                 # link with shared libs
-mt			# link with multi-thread libs
-gt<hbgt>		# link with <hbgt> GT driver (only for -static)

link options work only with "%{hb_pref}lnk" and "%{hb_pref}mk" and has no effect
in "%{hb_pref}cmp"
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

I hope this RPM is useful. Have fun with %{dname}.

Dave Pearson <davep@davep.org>
Przemyslaw Czerpak <druzus@polbox.com>
EOF

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
