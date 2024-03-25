AC_INIT("DESCRIPTION")
AC_CHECK_LIB([z], [compress], , AC_MSG_ERROR([zlib not found]))
AC_CHECK_LIB([z], [uncompress], , AC_MSG_ERROR([zlib not found]))

# Dynamically generate list of sources from subdirectories via shell
SUBDIR_SOURCES_CPP="$(cd src/ && ls {.,./gdx/src/gdlib,./gdx/src,./gdx/src/global,./gdx/src/rtl}/*.cpp | tr '\n' ' ')"
SUBDIR_SOURCES_C="$(cd src/ && ls ./gdx/src/rtl/*.c | tr '\n' ' ')"

AC_SUBST(SUBDIR_SOURCES_CPP)
AC_SUBST(SUBDIR_SOURCES_C)
AC_CONFIG_FILES([src/Makevars])

AC_OUTPUT
