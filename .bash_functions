# this file contains user bash functions that will be sourced by .bashrc

function calc()     { echo $@ | bc -l; }
function m()
{
  #echo "OLD PRELOAD = " $LD_PRELOAD
  if [[ "$LD_PRELOAD" == *"libtcmalloc_minimal"* ]]; then

        ld_array=`(echo $LD_PRELOAD | sed -e 's/:/\n'/g)`
        export LD_PRELOAD_NEW=
        for a in `echo $ld_array`;  do if [[ "$a" != *"libtcmalloc_minimal"* ]]; then export LD_PRELOAD_NEW=$LD_PRELOAD_NEW$a':'; fi; done
        export LD_PRELOAD=${LD_PRELOAD_NEW%?}
  fi
  #echo "NEW PRELOAD = " $LD_PRELOAD
  /usr/bin/emacs ${@}
}
function mm()       { m -nw $@; }
function mkcd()     { mkdir "$1"; cd "$1" ; }
function lc()       { cat $@ | tr '[:upper:]' '[:lower:]'; }
function uc()       { cat $@ | tr '[:lower:]' '[:upper:]'; }
function grepfn()
{
    SearchPath="$1"
    SearchString="$2";
    shift 2;
    for x in `find "$SearchPath" -name "$SearchString" | grep -v ".svn" `;
    do
        grep -nH "$@" $x;
    done
}
function greps()    { ps -ef | grep "$@" | grep -v grep ; }
function grepsm()
{
    echo -n `date +%T` "   ";
    ps --no-header -e -o "pid,size,vsz,rss,cmd" | grep "$1" | grep -v grep;
}

function lcfile() # convert argument filenames to lower case
{
    for x in $@ ; do
        mv $x `echo $x | tr '[:upper:]' '[:lower:]'`
    done
}

function ucfile() # convert argument filenames to upper case
{
    for x in $@ ; do
        mv $x `echo $x | tr '[:lower:]' '[:upper:]'`
    done
}

function mrg()
{
    for rev in $@; do
	echo "**** Processing revision ${rev}";
	for fpath in `svn log -vr ${rev} ../trunk | grep trunk | cut -f3- -d'/'`; do
	    echo ${fpath};
	    svn merge -c ${rev} ../trunk/${fpath} ${fpath};
	done;
    done
}

function svnup()
{
    case "$1" in
        q)
           svn up ~/src/gauss/trunk/gauss ~/src/qlib/trunk
           ;;
        g)
           pushd ~/src/gauss/trunk/ext_linux64/qlib > /dev/null;
	   mv LATEST xLATEST;
 	   mv ORIG LATEST;
           svn up ~/src/gauss/trunk;
	   mv LATEST ORIG;
	   mv xLATEST LATEST;
           popd > /dev/null;
           ;;
        *)
           pushd ~/src/gauss/trunk/ext_linux64/qlib > /dev/null;
	   mv LATEST xLATEST;
 	   mv ORIG LATEST;
           svn up ~/src/gauss/trunk ~/src/gauss/trunk/gauss ~/src/qlib/trunk;
	   mv LATEST ORIG;
	   mv xLATEST LATEST;
           popd > /dev/null;
           ;;
    esac
}

function tree()
{
    if [[ "$1" == "" ]]; then
        TreePath=.
    else
        TreePath=$1
    fi
    ls -R $TreePath | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'
}
