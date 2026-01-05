setwd("./gamstransfer")
# all files in the main directory src/gdx
gdx_ignore = list.files("src/gdx", full.names=TRUE, all.files=TRUE)

# remove src/gdx/src from ignored files
src_dirs = c("src/gdx/src")
gdx_ignore = gdx_ignore[gdx_ignore != src_dirs]

# remove src/gdx/zlib from ignored files
zlib_dirs = c("src/gdx/zlib")
gdx_ignore = gdx_ignore[gdx_ignore != zlib_dirs]
print(gdx_ignore)
# process src/gdx/zlib

# get all files and directories in src/gdx/zlib
gdx_zlib_all = list.files("src/gdx/zlib", full.names=TRUE)

# get all c, h, o files
gdx_zlib_c_h = list.files("src/gdx/zlib", 
pattern="(*\\.c$)|(*\\.h)|(*\\.o)", full.names=TRUE)

# ignore list = all src/gdx/zlib/ - gdx_zlib_c_h
zlib_ignore = setdiff(gdx_zlib_all, gdx_zlib_c_h)

# process src/gdx/src

# get all files and directories in src/gdx/src
gdx_src_all = list.files("src/gdx/src", full.names=TRUE)

# get all cpp, h, o files
gdx_src_cpp_h = list.files("src/gdx/src", 
pattern="(*\\.cpp$)|(*\\.h)|(*\\.o)", full.names=TRUE)

# ignore list = all src/gdx/src/ - gdx_src_cpp_h
gdx_src_ignore = setdiff(gdx_src_all, gdx_src_cpp_h)

# exclude gdlib, rtl and global from ignore list
src_subdirs = c("src/gdx/src/gdlib", "src/gdx/src/global", "src/gdx/src/rtl")
gdx_src_ignore = setdiff(gdx_src_ignore, src_subdirs)


# append ignore lists from the zlib, src/gdx and src/gdx/src
ignore_list = c(zlib_ignore, gdx_src_ignore, gdx_ignore)

# keep license file
zlib_license = "src/gdx/zlib/LICENSE"
gdx_license = "src/gdx/LICENSE"

ignore_list = setdiff(ignore_list, gdx_license)
ignore_list = setdiff(ignore_list, zlib_license)
print(ignore_list)
usethis::use_build_ignore(ignore_list)
