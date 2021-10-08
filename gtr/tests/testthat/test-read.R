library(gtr)

test_that("read set", {
c = Container$new("trialgdx.gdx")
c$read()
expect_equal(nrow(c$data$i$records), 2)
})

test_that("read another set", {
c = Container$new("trialgdx.gdx")
c$read()
expect_equal(nrow(c$data$j$records), 3)
})

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
