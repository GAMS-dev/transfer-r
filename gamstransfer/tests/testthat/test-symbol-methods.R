#
# GAMS - General Algebraic Modeling System R API
#
# Copyright (c) 2017-2026 GAMS Software GmbH <support@gams.com>
# Copyright (c) 2017-2026 GAMS Development Corp. <support@gams.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# Additional unit tests targeting the .Symbol base class (statistics getters,
# UEL management, domain violations, toDense, equals) that are shared by
# Set/Parameter/Variable/Equation but were not previously exercised by the
# read/write focused suite in test-read.R.

library(gamstransfer)

gams_checked <- -1
skip_if_no_gams <- function() {
  if (gams_checked == -1) {
    ret <- system2(command = "gams", stdout = NULL, stderr = NULL)
    if (ret == 127) {
      gams_checked <- 0
    } else {
      gams_checked <- 1
    }
  }

  if (gams_checked == 0) {
    testthat::skip("GAMS Unavailable")
  }
}

test_that("Symbol statistic getters (Parameter)", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = c("a", "b", "c"))
  p <- Parameter$new(m, "p", i, records = data.frame(i = c("a", "b", "c"), value = c(-5, 2, 10)))

  expect_equal(p$getMaxValue(), 10)
  expect_equal(p$getMinValue(), -5)
  expect_equal(p$getMeanValue(), (10 - 5 + 2) / 3)
  expect_equal(p$getMaxAbsValue(), 10)
  expect_equal(p$whereMax(), 3)
  expect_equal(p$whereMin(), 1)
  expect_equal(p$whereMaxAbs(), 3)
  expect_equal(p$countNA(), 0)
  expect_equal(p$countEps(), 0)
  expect_equal(p$countUndef(), 0)
  expect_equal(p$countPosInf(), 0)
  expect_equal(p$countNegInf(), 0)

  # scalars fall through the `column not in records` branch and use
  # the symbol default value instead
  scalar_p <- Parameter$new(m, "scalarp")
  expect_equal(scalar_p$getMaxValue(), NA)

  # Set statistics are always NA
  expect_true(is.na(i$getMaxValue()))
  expect_true(is.na(i$whereMax()))
  expect_true(is.na(i$countNA()))

  # `whereMax` rejects more than one column (Parameter always coerces its
  # `columns` argument to "value", so use a Variable to hit this branch)
  v <- Variable$new(m, "v", domain = i, records = data.frame(i = c("a", "b", "c"), level = c(1, 2, 3)))
  expect_error(v$whereMax(column = c("level", "marginal")), "At most one")
})

test_that("Symbol UEL management (getUELs/setUELs/addUELs/removeUELs/renameUELs/reorderUELs)", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = c("i1", "i2", "i3"))

  expect_equal(i$getUELs(), c("i1", "i2", "i3"))
  expect_equal(i$getUELs(dimension = 1, codes = 2), "i2")
  expect_error(i$getUELs(codes = 2), "must specify")
  expect_error(i$getUELs(dimension = 5), "must be integers")
  expect_error(i$getUELs(ignoreUnused = "no"), "must be type logical")
  expect_error(i$getUELs(dimension = 1, codes = -1), "must be integers")

  # addUELs
  expect_error(i$addUELs("i1"), "should not") # "i1" is already a value in the column
  i$addUELs("i4")
  expect_true("i4" %in% i$getUELs(ignoreUnused = FALSE))
  expect_error(i$addUELs(1), "must be type")
  expect_error(i$addUELs("i5", dimension = 5), "must be integers")

  # setUELs (rename = FALSE re-levels the factor, dropping data not in `uels`)
  j <- Set$new(m, "j", records = c("a", "b"))
  j$setUELs(c("a", "b", "c"))
  expect_equal(j$getUELs(ignoreUnused = FALSE), c("a", "b", "c"))
  expect_error(j$setUELs(1), "must be type")
  expect_error(j$setUELs("a", rename = "no"), "must be type logical")
  expect_error(j$setUELs("a", dimension = 5), "must be integers")

  # renameUELs unnamed vector form
  k <- Set$new(m, "k", records = c("x", "y"))
  k$renameUELs(c("x1", "y1"))
  expect_equal(as.character(k$records[, 1]), c("x1", "y1"))
  expect_error(k$renameUELs(c("dup", "dup")), "Multiple UELs")
  expect_error(k$renameUELs(c("only-one")), "does not match")
  expect_error(k$renameUELs(c("a", "b"), allowMerge = "no"), "must be type logical")
  expect_error(k$renameUELs(c("a", "b"), dimension = 9), "must be integers")

  # renameUELs named-vector (map) form, without merging
  l <- Set$new(m, "l", records = c("p", "q", "r"))
  l$renameUELs(c(p = "p1", q = "q1"))
  expect_true(all(c("p1", "q1", "r") %in% l$getUELs(ignoreUnused = FALSE)))

  # renameUELs named-vector (map) form, with merging two UELs together
  n <- Set$new(m, "n", records = c("u", "v", "w"))
  n$renameUELs(c(u = "merged", v = "merged"), allowMerge = TRUE)
  expect_true("merged" %in% n$getUELs(ignoreUnused = TRUE))

  # reorderUELs: default (data order) and explicit order
  o <- Set$new(m, "o", records = c("z", "y", "x"))
  o$reorderUELs()
  expect_equal(levels(o$records[, 1])[1:3], c("z", "y", "x"))
  o$reorderUELs(c("x", "y", "z"))
  expect_equal(levels(o$records[, 1]), c("x", "y", "z"))
  expect_error(o$reorderUELs(c("x", "y")), "must contain all uels")
  expect_error(o$reorderUELs(uels = 1), "must be type")
  expect_error(o$reorderUELs(dimension = 8), "must be integers")

  # removeUELs: explicit uels and drop-unused (uels = NULL)
  p1 <- Set$new(m, "p1", records = c("r1", "r2", "r3"))
  p1$removeUELs("r2")
  expect_false("r2" %in% p1$getUELs(ignoreUnused = FALSE))
  p1$addUELs("unused")
  p1$removeUELs()
  expect_false("unused" %in% p1$getUELs(ignoreUnused = FALSE))
  expect_error(p1$removeUELs(uels = 1), "must be type")
  expect_error(p1$removeUELs(dimension = 8), "must be integers")
})

test_that("Symbol domain violations", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = paste0("i", 1:3))
  p <- Parameter$new(m, "p", i,
    records = data.frame(i = c("i1", "i3", "i9"), value = c(1, 2, 3))
  )

  dv <- p$getDomainViolations()
  expect_equal(length(dv), 1)
  expect_true(inherits(dv[[1]], "DomainViolation"))
  expect_equal(dv[[1]]$violations, "i9")

  # DomainViolation$format()
  expect_true(grepl("DomainViolation", format(dv[[1]])))
  expect_true(grepl("i9", format(dv[[1]])))

  expect_true(p$hasDomainViolations())
  expect_equal(p$countDomainViolations(), 1)
  expect_equal(nrow(p$findDuplicateRecords()), 0)

  p$dropDomainViolations()
  expect_false(p$hasDomainViolations())
  expect_equal(nrow(p$records), 2)

  # no domain violations -> NULL / no-op
  j <- Set$new(m, "j", records = "j1")
  q <- Parameter$new(m, "q", j, records = data.frame(j = "j1", value = 1))
  expect_null(q$getDomainViolations())
  q$dropDomainViolations() # should not error

  # dimension == 0 (scalar) -> NULL
  s <- Parameter$new(m, "s", records = 1)
  expect_null(s$getDomainViolations())
})

test_that("Symbol toDense", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = c("i1", "i2"))
  j <- Set$new(m, "j", records = c("j1", "j2"))
  p <- Parameter$new(m, "p", c(i, j), records = data.frame(
    i = c("i1", "i2"), j = c("j1", "j2"), value = c(4, 8)
  ))

  dense <- p$toDense()
  expect_equal(dim(dense), c(2, 2))
  expect_equal(dense[1, 1], 4)
  expect_equal(dense[2, 2], 8)
  expect_equal(dense[1, 2], 0) # default fill value

  # scalar toDense
  s <- Parameter$new(m, "s", records = 42)
  expect_equal(s$toDense(), 42)

  # invalid `column` argument
  expect_error(p$toDense(column = 1), "must be type str")

  # symbol must be valid -- force an inconsistent records shape by bypassing
  # setRecords()'s validation and assigning raw records directly
  bad <- Parameter$new(m, "bad", i)
  bad$records <- data.frame(i = "i1", value = 1, extra = 2)
  expect_false(bad$isValid())
  expect_error(bad$toDense(), "invalid")

  # empty (NULL) records -> NULL
  empty_par <- Parameter$new(m, "emptyp", i)
  expect_null(empty_par$toDense())
})

test_that("Symbol equals / shape / domainLabels / description edge cases", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = c("a", "b"))
  p1 <- Parameter$new(m, "p1", i, records = data.frame(i = c("a", "b"), value = c(1, 2)))

  m2 <- Container$new()
  i2 <- Set$new(m2, "i", records = c("a", "b"))
  p2 <- Parameter$new(m2, "p1", i2, records = data.frame(i = c("a", "b"), value = c(1, 2)))

  expect_true(p1$equals(p2))
  expect_equal(p1$shape, 2)
  expect_equal(p1$domainLabels, "i")

  # renaming domain labels
  p1$domainLabels <- "custom"
  expect_equal(colnames(p1$records)[1], "custom")

  # wrong-length domainLabels errors
  expect_error({
    p1$domainLabels <- c("a", "b")
  }, "not equal to symbol dimension")

  # description validation
  expect_error({
    p1$description <- 5
  }, "must be type character")
  expect_error({
    p1$description <- c("a", "b")
  }, "character vector of length")
  expect_error({
    p1$description <- strrep("x", 300)
  }, "must have length")

  # isValid input validation
  expect_error(p1$isValid(verbose = "no"), "must be logical")
  expect_error(p1$isValid(force = "no"), "must be logical")

  # dimension setter growing/shrinking domain
  q <- Parameter$new(m, "q", i)
  q$dimension <- 0
  expect_equal(q$dimension, 0)
  q$dimension <- 2
  expect_equal(q$dimension, 2)
  expect_equal(q$domain[[2]], "*")
  expect_error({
    q$dimension <- -1
  }, "must be")
})

test_that("Symbol copy() across containers, including the class-mismatch guard", {
  skip_if_no_gams()
  src <- Container$new()
  i <- Set$new(src, "i", records = c("a", "b"))
  p <- Parameter$new(src, "p", i, records = data.frame(i = c("a", "b"), value = c(1, 2)))

  # a lone symbol copy (without its domain set) loses the regular domain
  # link, but the records themselves still transfer correctly
  dest <- Container$new()
  p$copy(dest)
  expect_true(dest$hasSymbols("p"))
  expect_equal(dest["p"]$records$value, p$records$value)

  # copying again without overwrite errors
  expect_error(p$copy(dest), "already exists")

  # copying with overwrite = TRUE but mismatched symbol type errors
  dest2 <- Container$new()
  Set$new(dest2, "p", records = c("a"))
  expect_error(p$copy(dest2, overwrite = TRUE), "Cannot copy a symbol of type")

  # copy with overwrite = TRUE and a matching type overwrites fields in place
  dest3 <- Container$new()
  Parameter$new(dest3, "p", "i", records = data.frame(i = c("a", "b"), value = c(0, 0)))
  p$copy(dest3, overwrite = TRUE)
  expect_equal(dest3["p"]$records$value, c(1, 2))
  expect_equal(dest3["p"]$description, p$description)

  # invalid arguments
  expect_error(p$copy(destination = "notacontainer"), "must be of type")
  expect_error(p$copy(dest, overwrite = "no"), "must be of type")
})
