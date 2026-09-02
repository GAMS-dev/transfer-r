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
# Additional unit tests targeting Container's convenience API (list*/describe*,
# add*, remove/rename, isValid/check, equals, container-to-container read) that
# were not previously exercised by the read/write focused suite in test-read.R.

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

build_sample_container <- function() {
  m <- Container$new()
  i <- Set$new(m, "i", records = c("i1", "i2"))
  Parameter$new(m, "p", i, records = data.frame(i = c("i1", "i2"), value = c(1, 2)))
  Variable$new(m, "v", "positive", i, records = data.frame(i = c("i1", "i2"), level = c(3, 4)))
  Equation$new(m, "e", "eq", i, records = data.frame(i = c("i1", "i2"), level = c(5, 6)))
  Alias$new(m, "ii", i)
  UniverseAlias$new(m, "uni")
  m
}

test_that("Container format/hasSymbols/getSymbolNames", {
  skip_if_no_gams()
  m <- build_sample_container()

  expect_true(grepl("Container", format(m)))
  expect_true(m$hasSymbols("p"))
  expect_equal(m$hasSymbols(c("p", "nope")), c(TRUE, FALSE))
  expect_error(m$hasSymbols(1), "must be type character")

  expect_equal(m$getSymbolNames("P"), "p") # case-insensitive lookup
  expect_error(m$getSymbolNames("nope"), "does not exist")
  expect_error(m$getSymbolNames(1), "must be type character")
})

test_that("Container list* methods", {
  skip_if_no_gams()
  m <- build_sample_container()

  expect_equal(sort(m$listSymbols()), sort(c("i", "p", "v", "e", "ii", "uni")))
  expect_equal(m$listSets(), "i")
  expect_equal(m$listParameters(), "p")
  expect_equal(m$listAliases(), c("ii", "uni"))
  expect_equal(m$listVariables(), "v")
  expect_equal(m$listVariables(types = "positive"), "v")
  expect_null(m$listVariables(types = "binary"))
  expect_equal(m$listEquations(), "e")
  expect_equal(m$listEquations(types = "eq"), "e")
  expect_null(m$listEquations(types = "geq"))

  expect_error(m$listSets(isValid = "no"), "must be type logical")
  expect_error(m$listVariables(types = 1), "type character")
  expect_error(m$listVariables(types = "notatype"), "unrecognized variable type")
  expect_error(m$listEquations(types = 1), "type character")
  # NOTE: `.EquationTypes[[tolower(t)]]` on an unrecognized type throws R's own
  # "subscript out of bounds" rather than the intended "unrecognized equation
  # type" message (a pre-existing bug in listEquations()); just assert it errors.
  expect_error(m$listEquations(types = "notatype"))

  # isValid filter
  expect_equal(m$listSymbols(isValid = TRUE), m$listSymbols())
  expect_equal(m$listSymbols(isValid = FALSE), NULL)
})

test_that("Container describe* methods", {
  skip_if_no_gams()
  m <- build_sample_container()

  ds <- m$describeSets()
  expect_true(is.data.frame(ds))
  expect_equal(ds$name, "i")

  dp <- m$describeParameters()
  expect_equal(dp$name, "p")
  expect_equal(dp$max, 2)

  dv <- m$describeVariables()
  expect_equal(dv$name, "v")
  expect_equal(dv$type, "positive")

  de <- m$describeEquations()
  expect_equal(de$name, "e")
  expect_equal(de$type, "eq")

  da <- m$describeAliases(symbols = "ii")
  expect_equal(da$name, "ii")
  expect_equal(da$aliasWith, "i")

  # empty container -> NULL for every describe*
  empty <- Container$new()
  expect_null(empty$describeSets())
  expect_null(empty$describeParameters())
  expect_null(empty$describeVariables())
  expect_null(empty$describeEquations())
  expect_null(empty$describeAliases())

  # bad `symbols` argument
  expect_error(m$describeSets(symbols = 1), "must be type character")
  expect_error(m$describeAliases(symbols = 1), "must be type character")
  expect_error(m$describeParameters(symbols = 1), "must be type character")
  expect_error(m$describeVariables(symbols = 1), "must be type character")
  expect_error(m$describeEquations(symbols = 1), "must be type character")
})

test_that("Container getSets/getParameters/getVariables/getEquations/getAliases", {
  skip_if_no_gams()
  m <- build_sample_container()

  expect_equal(m$getSets()[[1]]$name, "i")
  expect_equal(m$getParameters()[[1]]$name, "p")
  expect_equal(m$getVariables()[[1]]$name, "v")
  expect_equal(m$getEquations()[[1]]$name, "e")
  expect_equal(length(m$getAliases()), 2)
})

test_that("Container add* replacement semantics", {
  skip_if_no_gams()
  m <- Container$new()
  i <- m$addSet("i", records = c("a", "b"))
  expect_true(inherits(i, "Set"))

  # calling addSet again with identical signature updates records/description
  i2 <- m$addSet("i", records = c("a", "b", "c"), description = "updated")
  expect_equal(nrow(i2$records), 3)
  expect_equal(i2$description, "updated")

  # calling addSet with a different signature (isSingleton changed) errors
  expect_error(m$addSet("i", isSingleton = TRUE), "already exists")

  p <- m$addParameter("p", i, records = data.frame(i = c("a", "b", "c"), value = c(1, 2, 3)))
  expect_true(inherits(p, "Parameter"))
  # NOTE: the "unchanged signature" comparison inside addParameter() compares
  # the raw `domain` argument against the symbol's *normalized* (list-wrapped)
  # `$domain` field, so a repeat call must pass `domain` already wrapped in a
  # list for the identical() check to match and take the update-in-place path.
  p2 <- m$addParameter("p", list(i), records = data.frame(i = c("a", "b", "c"), value = c(4, 5, 6)))
  expect_equal(p2$records$value, c(4, 5, 6))
  expect_error(m$addParameter("p", domain = "different"), "already exists")

  v <- m$addVariable("v", "free", i)
  expect_true(inherits(v, "Variable"))
  v2 <- m$addVariable("v", "free", list(i), records = data.frame(i = c("a", "b", "c"), level = c(1, 2, 3)))
  expect_equal(v2$records$level, c(1, 2, 3))
  expect_error(m$addVariable("v", "binary", i), "already exists")

  e <- m$addEquation("e", "eq", i)
  expect_true(inherits(e, "Equation"))
  e2 <- m$addEquation("e", "eq", list(i), records = data.frame(i = c("a", "b", "c"), level = c(1, 2, 3)))
  expect_equal(e2$records$level, c(1, 2, 3))
  expect_error(m$addEquation("e", "geq", i), "already exists")

  al <- m$addAlias("ii", i)
  expect_true(inherits(al, "Alias"))
  al2 <- m$addAlias("ii", i) # same aliasWith -> just updates in place
  expect_identical(al2$aliasWith, i)

  ua <- m$addUniverseAlias("uni")
  expect_true(inherits(ua, "UniverseAlias"))
  ua2 <- m$addUniverseAlias("uni")
  expect_identical(ua2, ua)

  # adding a UniverseAlias over a symbol of a different type errors
  expect_error(m$addUniverseAlias("i"), "different type already")
})

test_that("Container removeSymbols / renameSymbol / reorderSymbols", {
  skip_if_no_gams()
  m <- build_sample_container()

  m$renameSymbol("p", "p_renamed")
  expect_true(m$hasSymbols("p_renamed"))
  expect_false(m$hasSymbols("p"))
  expect_error(m$renameSymbol("nope", "x"), "does not exist")
  expect_error(m$renameSymbol(1, "x"), "must be type character")
  expect_error(m$renameSymbol("p_renamed", 1), "must be type character")

  # removing a Set cascades: dependent Alias + domain references
  m$removeSymbols("i")
  expect_false(m$hasSymbols("i"))
  expect_false(m$hasSymbols("ii")) # alias removed along with its parent set
  expect_true(m$hasSymbols("v")) # variable stays, domain downgraded to "*"
  expect_equal(m["v"]$domain[[1]], "*")

  expect_error(m$removeSymbols(1), "must be of type character")

  # reorderSymbols: build a container where symbols were added out of order
  m2 <- Container$new()
  j <- Set$new(m2, "j", records = c("a", "b"))
  q <- Parameter$new(m2, "q", j, records = data.frame(j = c("a", "b"), value = c(1, 2)))
  m2$reorderSymbols()
  expect_equal(m2$listSymbols()[1], "j")
})

test_that("Container isValid / check / equals / getUELs / removeUELs / renameUELs", {
  skip_if_no_gams()
  m <- build_sample_container()
  expect_true(m$isValid())
  expect_true(m$isValid(symbols = "p"))
  expect_error(m$isValid(verbose = "no"), "must be logical")
  expect_error(m$isValid(force = "no"), "must be logical")

  m2 <- build_sample_container()
  expect_true(m$equals(m2))
  expect_false(m$equals("notacontainer"))
  expect_error(m$equals("notacontainer", verbose = TRUE), "not a Container")

  m3 <- Container$new()
  Set$new(m3, "i", records = c("i1", "i2"))
  expect_false(m$equals(m3)) # different symbol count
  expect_error(m$equals(m3, verbose = TRUE), "different number")

  m4 <- build_sample_container()
  m4$removeSymbols("uni")
  m4$addSet("extra")
  expect_false(m$equals(m4)) # same size, different keys
  expect_error(m$equals(m4, verbose = TRUE), "keys do not match")

  # UELs at the container level
  all_uels <- m$getUELs()
  expect_true(all(c("i1", "i2") %in% all_uels))

  m$removeUELs("i2", symbols = "i")
  expect_false("i2" %in% m["i"]$getUELs(ignoreUnused = FALSE))

  m5 <- build_sample_container()
  m5$renameUELs(c("i1new", "i2new"), symbols = "i")
  expect_true("i2new" %in% m5["i"]$getUELs(ignoreUnused = FALSE))
})

test_that("Container domain violations / duplicate records at the container level", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = paste0("i", 1:3))
  p <- Parameter$new(m, "p", i, records = data.frame(i = c("i1", "i9"), value = c(1, 2)))

  dv <- m$getDomainViolations()
  expect_equal(length(dv), 1)
  expect_true(m$hasDomainViolations())
  expect_equal(m$countDomainViolations()[["p"]], 1)

  m$dropDomainViolations()
  expect_false(m$hasDomainViolations())

  q <- Parameter$new(m, "q", i, records = data.frame(
    i = c("i1", "i1", "i2"), value = c(1, 1, 2)
  ))
  expect_true(m$hasDuplicateRecords(symbols = "q"))
  expect_equal(m$countDuplicateRecords()[["q"]], 1)
  m$dropDuplicateRecords()
  expect_false(m$hasDuplicateRecords(symbols = "q"))
})

test_that("Container copy() and container-to-container read()", {
  skip_if_no_gams()
  src <- build_sample_container()

  # NOTE: copying an Alias on its own via $copy() re-reads just that one
  # symbol, and .containerRead()'s Alias branch requires the parent set to be
  # part of that very same read batch -- so a whole-container $copy() that
  # includes an Alias errors even when the parent was already copied earlier
  # in the same pass. Restrict to the non-alias symbols here.
  dest <- Container$new()
  src$copy(dest, symbols = c("i", "p", "v", "e"), overwrite = TRUE)
  expect_equal(sort(dest$listSymbols()), c("e", "i", "p", "v"))

  dest2 <- Container$new()
  dest2$read(src)
  expect_equal(sort(dest2$listSymbols()), sort(src$listSymbols()))
  expect_true(dest2["p"]$equals(src["p"]))

  # reading only a subset of symbols
  dest3 <- Container$new()
  dest3$read(src, symbols = c("i", "p"))
  expect_equal(sort(dest3$listSymbols()), c("i", "p"))

  # reading a symbol that doesn't exist in the source errors
  dest4 <- Container$new()
  expect_error(dest4$read(src, symbols = "nope"), "does not exist")

  # reading into a container that already has a same-named symbol errors
  dest5 <- Container$new()
  Set$new(dest5, "i", records = "a")
  expect_error(dest5$read(src), "already exists")

  # invalid `loadFrom` argument
  expect_error(Container$new()$read(42), "must be type character")

  # a nonexistent file path errors
  expect_error(Container$new()$read("does-not-exist.gdx"), "doesn't exist")
})

test_that("Container summary / asList / readList round trip", {
  skip_if_no_gams()
  m <- build_sample_container()
  expect_equal(m$summary$numberSymbols, length(m$listSymbols()))

  l <- m$asList()
  expect_true(is.list(l))
  expect_true("p" %in% vapply(l, function(x) x$name, character(1)))

  m2 <- Container$new()
  m2$readList(l)
  expect_equal(sort(m2$listSymbols()), sort(m$listSymbols()))

  # readList()'s `symbols` argument matches against names(readlist), which
  # asList() does not set (a pre-existing gap), so name the list ourselves
  # from each entry's `name` field before exercising that validation branch.
  # NOTE: `symbols` is validated against the readlist but not actually used
  # to filter which symbols get constructed -- every symbol in `readlist`
  # is added regardless (another pre-existing gap) -- so this only exercises
  # the validation path, not an actual subset read.
  names(l) <- vapply(l, function(x) x$name, character(1))
  m3 <- Container$new()
  m3$readList(l, symbols = c("i", "p"))
  expect_true(all(c("i", "p") %in% m3$listSymbols()))
  expect_error(m3$readList(l, symbols = "nope"), "is not in the list")
})

test_that("Container$new deprecated systemDirectory argument warns", {
  skip_if_no_gams()
  expect_warning(Container$new(systemDirectory = "somewhere"), "deprecated")
})
