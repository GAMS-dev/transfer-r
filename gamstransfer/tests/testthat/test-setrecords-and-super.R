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
# Additional unit tests targeting:
#  - array/list-based setRecords() conversion on Parameter/Variable/Equation
#  - generateRecords() with custom `func`/`seed`
#  - the `type` validation on Variable/Equation
#  - defaultValues/isScalar
#  - the top-level readGDX()/writeGDX() argument validation
#  - a handful of small Set.R / Symbol.R argument-validation gaps
# that were not previously exercised by the read/write focused suite in
# test-read.R.

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

test_that("Parameter setRecords: array/vector/scalar conversion", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = c("a", "b", "c"))
  p <- Parameter$new(m, "p", i)

  p$setRecords(array(c(1, 2, 3)))
  expect_equal(p$records$value, c(1, 2, 3))

  p2 <- Parameter$new(m, "p2", i)
  p2$setRecords(c(4, 5, 6)) # bare numeric vector, also goes through the array path
  expect_equal(p2$records$value, c(4, 5, 6))

  # wrong shape
  p3 <- Parameter$new(m, "p3", i)
  expect_error(p3$setRecords(array(c(1, 2))), "anticipated shape")

  # scalar with more than one entry
  s <- Parameter$new(m, "s")
  expect_error(s$setRecords(c(1, 2)), "more than one entries")

  # scalar with exactly one entry works
  s$setRecords(42)
  expect_equal(s$records$value, 42)

  # non-regular domainType cannot use array conversion
  relaxed <- Parameter$new(m, "relaxed", "notaset")
  expect_error(relaxed$setRecords(array(1)), "self\\$domainType = 'regular'")

  # invalid domain set blocks array conversion
  bad_i <- Set$new(m, "bad_i", records = c("x", "y"))
  bad_p <- Parameter$new(m, "bad_p", bad_i)
  bad_i$records <- data.frame(a = 1, b = 2, c = 3) # force bad_i$isValid() == FALSE
  expect_error(bad_p$setRecords(array(c(1, 2))), "invalid and cannot be used")

  # data.frame path: wrong number of columns
  expect_error(p$setRecords(data.frame(i = "a", j = "b", value = 1)), "inconsistent with parameter domain")

  # data.frame path: non-numeric value column
  expect_error(p$setRecords(data.frame(i = c("a", "b", "c"), value = c("x", "y", "z"))), "must be numeric")
})

test_that("Parameter generateRecords / defaultValues / isScalar", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = c("a", "b", "c"))
  p <- Parameter$new(m, "p", i)

  p$generateRecords(density = 1, seed = 123)
  expect_equal(nrow(p$records), 3)

  p$generateRecords(func = function(size) rep(7, size), seed = 1)
  expect_true(all(p$records$value == 7))

  expect_error(p$generateRecords(seed = "no"), "must be an integer")

  s <- Parameter$new(m, "s")
  s$generateRecords()
  expect_equal(nrow(s$records), 1)

  expect_equal(p$defaultValues, 0)
  expect_false(p$isScalar)
  expect_true(s$isScalar)

  # generateRecords requires a regular domain or a scalar
  relaxed <- Parameter$new(m, "relaxed", "notaset")
  expect_error(relaxed$generateRecords(), "domainType == 'regular'")
})

test_that("Variable/Equation setRecords: list-of-arrays conversion", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = c("a", "b", "c"))
  v <- Variable$new(m, "v", "free", i)

  v$setRecords(list(level = c(1, 2, 3), marginal = c(0.1, 0.2, 0.3)))
  expect_equal(v$records$level, c(1, 2, 3))
  expect_equal(v$records$marginal, c(0.1, 0.2, 0.3))

  # a bare array defaults to the `level` attribute
  v2 <- Variable$new(m, "v2", "free", i)
  v2$setRecords(array(c(9, 8, 7)))
  expect_equal(v2$records$level, c(9, 8, 7))

  # unrecognized attribute name
  v3 <- Variable$new(m, "v3", "free", i)
  expect_error(v3$setRecords(list(notanattr = c(1, 2, 3))), "Unrecognized user attribute")

  # non-numeric attribute value
  v4 <- Variable$new(m, "v4", "free", i)
  expect_error(v4$setRecords(list(level = "a")), "must\\s*be")

  # mismatched array sizes within the list.
  # NOTE: the numeric-to-array coercion loop inside setRecords() only
  # converts the *last* list element (`for (i in length(records))` instead of
  # `seq_along(records)`), so for a 2-element list the first entry is never
  # converted to an array and `dim()` on it returns NULL -- the "all equal"
  # size guard then silently passes and the mismatch instead surfaces later
  # as a raw data.frame assignment error (a pre-existing gap).
  v5 <- Variable$new(m, "v5", "free", i)
  expect_error(v5$setRecords(list(level = c(1, 2, 3), marginal = c(1, 2))))

  # scalar with more than one entry
  vs <- Variable$new(m, "vs", "free")
  expect_error(vs$setRecords(list(level = c(1, 2))), "more than one entries")
  vs$setRecords(list(level = 5))
  expect_equal(as.numeric(vs$records$level), 5)

  # Equation mirrors the same array-conversion machinery
  e <- Equation$new(m, "eq1", "eq", i)
  e$setRecords(list(level = c(1, 2, 3)))
  expect_equal(e$records$level, c(1, 2, 3))
})

test_that("Variable/Equation type validation, generateRecords, defaultValues", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = c("a", "b"))
  v <- Variable$new(m, "v", "free", i)

  expect_error({
    v$type <- "not-a-type"
  }, "must be one of the following")
  v$type <- "BINARY" # case-insensitive
  expect_equal(v$type, "binary")

  eq <- Equation$new(m, "eq2", "eq", i)
  expect_error({
    eq$type <- "not-a-type"
  }, "must be one of the following")
  # NOTE: single-letter shorthand ("G", "L", ...) is only normalized to its
  # canonical name (via `.EquationTypes[[type]]`) inside the Equation$new()
  # constructor; the `type` *setter* itself only recognizes the canonical
  # words, so it must be assigned in its long form here.
  eq$type <- "GEQ"
  expect_equal(eq$type, "geq")

  # generateRecords with custom func/seed on a Variable
  v$generateRecords(func = function(size) rep(0.5, size), seed = 42)
  expect_true(all(v$records$level == 0.5))
  expect_error(v$generateRecords(seed = "no"), "must be an integer")

  relaxedv <- Variable$new(m, "relaxedv", "free", "notaset")
  expect_error(relaxedv$generateRecords(), "domainType")
})

test_that("Set.R small argument-validation gaps", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = c("a", "b"))

  # setRecords column-count mismatch
  expect_error(i$setRecords(data.frame(a = 1, b = 2, c = 3)), "Expecting")

  # equals(): checkElementText must be logical
  j <- Set$new(m, "j", records = c("a", "b"))
  expect_error(i$equals(j, checkElementText = "no"), "must be type logical")

  # isSingleton setter type validation
  expect_error({
    i$isSingleton <- "no"
  }, "must be type logical")
})

test_that("Symbol domainForwarding validation and records<- forwarding", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = c("a", "b"))
  p <- Parameter$new(m, "p", i, domainForwarding = TRUE)

  expect_error({
    p$domainForwarding <- "no"
  }, "must be type logical")

  # setRecords() with domainForwarding == TRUE adds new domain members back
  # onto the referenced domain set (records<- itself only forwards factor
  # columns, so go through setRecords() which performs that conversion)
  p$setRecords(data.frame(i = c("a", "b", "c"), value = c(1, 2, 3)))
  expect_true("c" %in% i$getUELs(ignoreUnused = FALSE))
})

test_that("readGDX()/writeGDX() argument validation", {
  skip_if_no_gams()
  # NOTE: the guard is `!is.logical(records) && length(records) != 1`, so it
  # only trips when `records` is BOTH non-logical AND not length 1 (a
  # pre-existing gap -- a lone non-logical scalar like "yes" slips through).
  expect_error(readGDX("f.gdx", records = c("a", "b")), "must be type logical")
  expect_error(readGDX("f.gdx", symbols = 5), "must be of the type character or NULL")
  expect_error(readGDX("f.txt"), "must be .gdx")
  expect_error(readGDX("nonexistent-file.gdx"), "doesn't exist")
  expect_error(readGDX(42), "must be type character")

  expect_error(writeGDX(list(), "out.gdx", compress = "no"), "must be of type logical")
  expect_error(writeGDX(list(), 42), "must be of type character")
  expect_error(writeGDX(list(), "out"), "must be .gdx")
  expect_error(writeGDX(list(), "out.gdx", uelPriority = 5), "must be type character or NULL")
  expect_error(writeGDX(list(), "out.gdx", mode = 5), "must be type character")
  expect_error(writeGDX(list(), "out.gdx", mode = "bogus"), "one of the following")

  # empty writeList round trip (isempty branch) succeeds
  out <- tempfile(fileext = ".gdx")
  expect_silent(writeGDX(list(), out))
  expect_true(file.exists(out))
})
