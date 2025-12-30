AC_INIT("DESCRIPTION")
AC_CHECK_LIB([z], [compress], , AC_MSG_ERROR([zlib not found]))
AC_CHECK_LIB([z], [uncompress], , AC_MSG_ERROR([zlib not found]))

# Define the zlib source files
zlib_units="gdx/zlib/adler32.c gdx/zlib/compress.c gdx/zlib/crc32.c gdx/zlib/deflate.c \
            gdx/zlib/gzclose.c gdx/zlib/gzlib.c gdx/zlib/gzread.c gdx/zlib/gzwrite.c \
            gdx/zlib/infback.c gdx/zlib/inffast.c gdx/zlib/inflate.c gdx/zlib/inftrees.c \
            gdx/zlib/trees.c gdx/zlib/uncompr.c gdx/zlib/zutil.c"

# Dynamically generate list of sources from subdirectories via shell
SUBDIR_SOURCES_CPP="\
    gdx/src/rtl/p3io.cpp \
    gdx/src/rtl/p3utils.cpp \
    gdx/src/rtl/sysutils_p3.cpp \
    gdx/src/rtl/system_p3.cpp \
    gdx/src/rtl/p3platform.cpp \
    gdx/src/rtl/math_p3.cpp \
    gdx/src/gdlib/gmsstrm.cpp \
    gdx/src/gdlib/utils.cpp \
    gdx/src/gdlib/strutilx.cpp \
    gdx/src/gxfile.cpp \
    Read.cpp \
    Write.cpp \
    Utilities.cpp \
    RcppExports.cpp"

SUBDIR_SOURCES_C="$zlib_units gdx/src/rtl/dtoaLoc.c "

AC_SUBST(SUBDIR_SOURCES_CPP)
AC_SUBST(SUBDIR_SOURCES_C)
AC_CONFIG_FILES([src/Makevars])

AC_OUTPUT
