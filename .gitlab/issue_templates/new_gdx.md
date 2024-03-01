## Background
GTR depends on GDX API. In the repository, GDX is used as a submodule. However, when shipping/building the package, unnecessary files should be ignored. This is done using .Rbuildignore file. This file can be updated by running update_buildignore.R script. This should be done every time a version of GDX API is released.
## When new GDX API is released
- pull and commit the latest version of the main branch of gdx submodule
- run update_buildignore.R and commit the changes
- update the changelog
- manually inspect the source build for unnecessary files in src
