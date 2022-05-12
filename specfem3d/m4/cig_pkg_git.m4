# -*- Autoconf -*-


## ------------------------------------ ##
## Autoconf macro for source using GIT. ##
## ------------------------------------ ##

# CIG_PKG_GIT
# Determine GIT information for source, if available.
AC_DEFUN([CIG_PKG_GIT], [
pushd $srcdir > /dev/null
AC_CHECK_FILE([.git/config], [[
$1_RELEASE_VERSION=0
eval $1_GIT_REVISION=`git describe`
eval $1_GIT_HASH=`git log -1 --pretty=format:%H`
eval $1_GIT_DATE=\"`git log -1 --pretty=format:%ci`\"
eval $1_GIT_BRANCH=`git rev-parse --abbrev-ref HEAD`
]],[[
$1_RELEASE_VERSION=1
$1_GIT_REVISION="unknown"
$1_GIT_HASH="unknown"
$1_GIT_DATE="unknown"
$1_GIT_BRANCH="unknown"
]])
popd > /dev/null
AC_DEFINE_UNQUOTED([$1_RELEASE_VERSION], [$$1_RELEASE_VERSION], [Set to 0 if source is from GIT, 1 otherwise.])
AC_DEFINE_UNQUOTED([$1_GIT_REVISION], ["$$1_GIT_REVISION"], [Define git revision commit for package source.])
AC_DEFINE_UNQUOTED([$1_GIT_HASH], ["$$1_GIT_HASH"], [Define GIT hash for package source.])
AC_DEFINE_UNQUOTED([$1_GIT_DATE], ["$$1_GIT_DATE"], [Define date of GIT commit for package source.])
AC_DEFINE_UNQUOTED([$1_GIT_BRANCH], ["$$1_GIT_BRANCH"], [Define GIT branch for package source.])
]) dnl CIG_PKG_GIT


dnl end of file
