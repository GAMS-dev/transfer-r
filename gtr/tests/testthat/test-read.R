library(gtr)

# read all symbols
## relaxed gdx
## regular gdx
test_that("read all symbols", {
  c = Container$new()
  c$read("trialgdx.gdx")

  # check if all symbols are read
  
})
# read one symbol

# read 5 symbols

# read alias

# read element text

test_that("readwritetest", {

print(system2(command="pwd"))
tryCatch(
  expr = {
ret =system2(paste0("gams ", testthat::test_path("data.gms"), " gdx=data.gdx"), 
stdout = TRUE, stderr = TRUE)
  }
,
error = function(e) {
  print(e)
  print(paste0("gams ", testthat::test_path("data.gms"), " gdx=data.gdx"))
  print(system2(command="less data.gms"))
}
)
a = 0
expect_equal(a, 0, info=print(ret))
# m = Container$new()
# # read all symbols
# m$read(testthat::test_path("data.gdx"))

# # write everything
# m$write(testthat::test_path("gt.gdx"))

# ret <-system2(paste0("gdxdiff ", testthat::test_path("data.gdx"), " ", testthat::test_path("gt.gdx")))


})
# test_that("read set", {
# c = Container$new()
# c$read("trialgdx.gdx")
# expect_equal(nrow(c$data$i$records), 2)
# })

# test_that("read another set", {
# c = Container$new()
# c$read("trialgdx.gdx")
# expect_equal(nrow(c$data$j$records), 3)
# })

# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
