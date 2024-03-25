AC_INIT("DESCRIPTION")
AC_CHECK_LIB([z], [compress], , AC_MSG_ERROR([zlib not found]))
AC_CHECK_LIB([z], [uncompress], , AC_MSG_ERROR([zlib not found]))

# Dynamically generate list of sources from subdirectories via shell
SUBDIR_SOURCES_CPP="gdx/src/gdlib/gmsstrm.cpp gdx/src/gdlib/utils.cpp gdx/src/gxfile.cpp Read.cpp Write.cpp Utilities.cpp RcppExports.cpp"

AC_SUBST(SUBDIR_SOURCES_CPP)
AC_CONFIG_FILES([src/Makevars])

AC_OUTPUT
