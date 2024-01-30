setwd("./gamstransfer")
gdx_ignore = list.files("src/gdx", full.names=TRUE)
gdx_ignore = gdx_ignore[gdx_ignore != "src/gdx/src"]

gdx_src_all = list.files("src/gdx/src", full.names=TRUE)
gdx_src_cpp_h = list.files("src/gdx/src", pattern="(*\\.cpp$)|(*\\.h)", full.names=TRUE)
ignore_list = setdiff(gdx_src_all, gdx_src_cpp_h)
ignore_list = append(gdx_ignore, ignore_list)
usethis::use_build_ignore(ignore_list)
