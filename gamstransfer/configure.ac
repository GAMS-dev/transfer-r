AC_INIT("DESCRIPTION")
AC_CHECK_LIB([z], [compress], , AC_MSG_ERROR([zlib not found]))
AC_CHECK_LIB([z], [uncompress], , AC_MSG_ERROR([zlib not found]))

# Dynamically generate list of sources from subdirectories via shell
SUBDIR_SOURCES="$(cd src/ && ls {.,./gdx/src}/*.cpp | tr '\n' ' ')"

AC_SUBST(SUBDIR_SOURCES)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
