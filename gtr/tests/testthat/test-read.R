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

message("here1")
message(system2(command="pwd"))
message("here1")
message(system2(command="which", args="gams"))
tryCatch(
  expr = {
ret = system2(command="gams", args= 
paste0(testthat::test_path("data.gms"), " gdx=data.gdx"))#, 
# stdout = TRUE, stderr = TRUE)

m = Container$new()
# read all symbols
m$read(testthat::test_path("data.gdx"))

# write everything
m$write(testthat::test_path("gt.gdx"))

ret <-system2(command="gdxdiff", args=
paste0(testthat::test_path("data.gdx"), " ", testthat::test_path("gt.gdx")))

expect_equal(ret, 0)

  }
,
error = function(e) {
  message(e)
  message(paste0("gams ", testthat::test_path("data.gms"), " gdx=data.gdx"))
}
)


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
