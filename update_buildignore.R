setwd("./gamstransfer")
# all files in the main directory src/gdx
gdx_ignore = list.files("src/gdx", full.names=TRUE, all.files=TRUE)

# remove src/gdx/src from ignored files
src_dirs = c("src/gdx/src")
gdx_ignore = gdx_ignore[gdx_ignore != src_dirs]

# process src/gdx/src

# get all files and directories in src/gdx/src
gdx_src_all = list.files("src/gdx/src", full.names=TRUE)

# get all cpp, h, o files
gdx_src_cpp_h = list.files("src/gdx/src", 
pattern="(*\\.cpp$)|(*\\.h)|(*\\.o)", full.names=TRUE)

# ignore list = all src/gdx/src/ - gdx_src_cpp_h
ignore_list = setdiff(gdx_src_all, gdx_src_cpp_h)

# exclude gdlib, rtl and global from ignore list
src_subdirs = c("src/gdx/src/gdlib", "src/gdx/src/global", "src/gdx/src/rtl")
ignore_list = setdiff(ignore_list, src_subdirs)

# append ignore lists from the src/gdx and src/gdx/src
ignore_list = append(gdx_ignore, ignore_list)

# keep license file
gdx_license = "src/gdx/LICENSE"
ignore_list = setdiff(ignore_list, gdx_license)
usethis::use_build_ignore(ignore_list)
