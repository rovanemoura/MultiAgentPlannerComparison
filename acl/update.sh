#! /bin/sh
# Run ./update.sh to rebuild all images to include patches.

acldir="`dirname $0`"
if test "$acldir" != "."; then
    echo cd $acldir
    cd $acldir
fi

set -eu

usage ()
{
    cat << EOF
usage: update.sh [-u] [--proxy proxy] [--proxy-auth auth]
                 [--newspace size] [--oldspace size] 
                 [--skip-ide]
                 [--lisp-heap-start addr] [--lisp-heap-size size] 
                 [--aclmalloc-heap-start addr] [--aclmalloc-heap-size size] 

Without any arguments, rebuild images to contain all downloaded patches.
With the -u argument, download new updates before rebuilding images.

--proxy allows specification of a proxy host and port.  It is typically 
something like "cobweb:3128".

--proxy-auth allows specification of proxy basic authentication.  It is
typically your password for the proxy.

You may specify --proxy without --proxy-auth.

The --*-heap-size arguments must be one of

  size := number + suffix
  number := [0-9]+
  suffix := m | k

The "m" and "k" in the suffix stand for "megabytes" and "kilobytes".

The --newspace, --oldspace and --*-heap-start arguments use
the same format as the --*-heap-size arguments above.

More information can be obtained from 
  http://franz.com/support/documentation/current/doc/building-images.htm
EOF
    exit 1
}

download=
proxy=
proxyauth=
host="franz.com"
ide=xxx

while test $# -gt 0; do
    case $1 in
	--help) usage ;;
	-u) download=xxx ;;
	--skip-ide) ide= ;;
	--c-heap-start|--aclmalloc-heap-start)
	    shift
	    if test $# -eq 0; then usage; fi
	    export ACL_BUILD_ACLMALLOC_HEAP_START=$1
	    ;;
	--c-heap-size|--aclmalloc-heap-size)
	    shift
	    if test $# -eq 0; then usage; fi
	    export ACL_BUILD_ACLMALLOC_HEAP_SIZE=$1
	    ;;
	--lisp-heap-start)
	    shift
	    if test $# -eq 0; then usage; fi
	    export ACL_BUILD_LISP_HEAP_START=$1
	    ;;
	--lisp-heap-size)
	    shift
	    if test $# -eq 0; then usage; fi
	    export ACL_BUILD_LISP_HEAP_SIZE=$1
	    ;;
	--newspace)
	    shift
	    if test $# -eq 0; then usage; fi
	    export ACL_BUILD_NEWSPACE=$1
	    ;;
	--oldspace)
	    shift
	    if test $# -eq 0; then usage; fi
	    export ACL_BUILD_OLDSPACE=$1
	    ;;
	--proxy)
	    shift
	    if test $# -eq 0; then usage; fi
	    proxy=$1
	    ;;
	--proxy-auth)
	    shift
	    if test $# -eq 0; then usage; fi
	    proxyauth=$1
	    ;;
	--host)
	    shift
	    if test $# -eq 0; then usage; fi
	    host=$1
	    ;;
	*)
	    usage
            exit 1
	    ;;
    esac
    shift
done

if test -n "$download"; then
    echo 'Will download updates before rebuilding images.'
else
    echo 'Will *not* download updates before rebuilding images.'
fi

if test -n "$proxy"; then
    echo Using proxy $proxy
    if test -n "$proxyauth"; then
	echo "   ...with proxy basic authentication: $proxyauth"
    fi
fi

tempfile=update$$
trap "/bin/rm -f $tempfile" 0

if test -n "$download"; then
    rm -f $tempfile
    cat <<EOF > $tempfile
(sys:update-allegro
  :host "$host"
EOF
    if test -n "$proxy"; then
	cat <<EOF >> $tempfile
  :proxy "$proxy"
EOF
    fi
    if test -n "$proxyauth"; then
	cat <<EOF >> $tempfile
  :proxy-basic-authorization "$proxyauth"
EOF
    fi
    cat <<EOF >> $tempfile
)
EOF
    cat $tempfile
    echo ./alisp -qq -batch -L $tempfile -kill
    ./alisp -qq -batch -L $tempfile -kill
    rm -f $tempfile
fi

rm -f TRIAL NON-TRIAL

./alisp -L update2.cl -qq

do_bu=1

## Keep in sync with *images-to-update* in update1.cl
images="alisp.dxl alisp8.dxl mlisp.dxl mlisp8.dxl \
        allegro.dxl allegro-ansi.dxl allegro-express.dxl
        clim.dxl"
# composer.dxl removed to keep in sync with update1.cl

if test ! -f alisp; then
    cp -p alisp alisp.bak
fi

if test -f clim; then
    rm -f clim
    ln -f mlisp clim
fi

if test -f clim8; then
    rm -f clim8
    ln -f mlisp8 clim8
fi

if test -f composer; then
    rm -f composer
    ln -f mlisp composer
fi

if test -f composer8; then
    rm -r composer8
    ln -f mlisp8 composer8
fi

env="env ACL_LOCALE=C ACL_UPDATING_IMAGES=t"

rm -f UPDATED

for image in $images; do
    if test ! -f $image; then
	continue
    fi
    case $image in
	allegro*)
	    if test -z "$ide"; then
		continue
	    fi
	    ;;
    esac

    base="`echo ${image} | sed 's/.dxl//'`"
    lisp=$base

    if test $do_bu -ne 0; then
	do_bu=0
	echo "Doing bundle check.  Output going to tmpbu.build..."
	echo "The bundle check may take several minutes."
	cat << EOF > $tempfile
(build-lisp-image "tmpbu.dxl" :verbose t :include-ide nil :include-devel-env t :internal-debug "tmpbu.debug")
(exit 0 :quiet t)
EOF
	cat $tempfile | $env ./$lisp -I "$base".dxl -qq -batch -backtrace-on-error 2>&1 > tmpbu.build
	cat << EOF > $tempfile
(when (fboundp 'excl::update-bundle-check)
  (unless (excl::update-bundle-check t)
    (excl::update-bundle-files)))
(exit 0 :quiet t)
EOF
	cat $tempfile | $env ./$lisp -I tmpbu.dxl -qq -batch -backtrace-on-error 2>&1 >> tmpbu.build
	rm -f tmpbu.dxl
	echo "Finished bundle check."
    fi

    if test ! -f $lisp; then
	echo "Sorry, $lisp does not exist."
	exit 1
    fi

    orig_image="$base"orig.dxl
    old_image="$base"old.dxl

    if test ! -f $orig_image; then
	mv $image $orig_image
	chmod a-w $orig_image
	prev=$orig_image
    fi
    if test -f $image; then
	mv $image $old_image
	prev=$old_image
    fi

    if test -f require-search-list.cl; then
	rsl=`cat require-search-list.cl`
    else
	rsl=
    fi

    set +e
    echo "Building $image..."
    cat << EOF > $tempfile
(setq *print-startup-message*
  (let ((r (reverse *print-startup-message*)))
    (reverse
     (pushnew '(:external-format . t) r :test #'equal))))
(when excl::*pll-file*
  (setq excl::*pll-file*
    (or (sys:tmp-mnt-frobber excl::*pll-file*) excl::*pll-file*)))
$rsl
(build-lisp-image "$image" :pll-from-sys t :verbose t :internal-debug "$base.debug")
(exit 0 :quiet t)
EOF
    $env ./$lisp -I $orig_image -qq -batch -backtrace-on-error -L $tempfile 2>&1 > $base.build
    status=$?
    set -e
    if test $status -ne 0; then
	rm -f $image
	mv $prev $image
	case $image in
	    allegro*)
		cat <<EOF
NOTE: "${image}.dxl" failed to rebuild.  If you do not use the IDE/GUI,
      then you can safety ignore this error.  It is likely a problem with
      the GTK2 installation on your system.
EOF
		if test "`uname -s`" = "Darwin"; then
		    cat <<EOF

      Since you are on Mac OS X, please make sure X11 and GTK2 are
      installed correctly:
        http://franz.com/support/documentation/current/doc/installation.htm#macosx-x11-2
      Note that we used to use the Macports version of GTK, but now use
      a Frameworks version, which is much easier to install.

      Also, please make sure that X11.app is running.  On some systems
      X11.app starts automatically, but this is not always the case.

EOF
		fi
		cat <<EOF
      If you had previously built $image successfully and no
      changes to your system have been made, then check the file
         $base.build
      for clues as to the error.

      If you cannot find and fix the problem, please contact
      support@franz.com for help.
EOF
		;;
	    *) 
		echo "Building $image failed.  Will restore previous image."
		echo "Check $base.build for build errors."
		exit 1
		;;
	esac
    else
	echo $image >> UPDATED
    fi
done

for dxl in *.dxl; do
    case $dxl in
	instsrc.dxl|*old.dxl|*orig.dxl) ;;
	*)
	    if ! egrep "^$dxl\$" UPDATED > /dev/null; then
		cat <<EOF
Warning: "$dxl" was not a recognized image and will not be rebuilt
         by $0.
EOF
	    fi
	    ;;
    esac
done

rm -f UPDATEALLEGROPENDING.txt
