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


test_that("read set", {
c = Container$new()
c$read("trialgdx.gdx")
expect_equal(nrow(c$data$i$records), 2)
})

test_that("read another set", {
c = Container$new()
c$read("trialgdx.gdx")
expect_equal(nrow(c$data$j$records), 3)
})

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
