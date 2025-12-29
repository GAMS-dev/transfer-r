AC_INIT("DESCRIPTION")
AC_CHECK_LIB([z], [compress], , AC_MSG_ERROR([zlib not found]))
AC_CHECK_LIB([z], [uncompress], , AC_MSG_ERROR([zlib not found]))

# Define the zlib source files
zlib_units="zlib/adler32.c zlib/compress.c zlib/crc32.c zlib/deflate.c \
            zlib/gzclose.c zlib/gzlib.c zlib/gzread.c zlib/gzwrite.c \
            zlib/infback.c zlib/inffast.c zlib/inflate.c zlib/inftrees.c \
            zlib/trees.c zlib/uncompr.c zlib/zutil.c"

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

SUBDIR_SOURCES_C="gdx/src/rtl/dtoaLoc.c $zlib_units"

AC_SUBST(SUBDIR_SOURCES_CPP)
AC_SUBST(SUBDIR_SOURCES_C)
AC_CONFIG_FILES([src/Makevars])

AC_OUTPUT
