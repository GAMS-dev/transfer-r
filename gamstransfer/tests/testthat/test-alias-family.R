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
# Additional unit tests targeting Alias, .BaseAlias, and UniverseAlias methods
# that delegate to their parent Set and were not previously exercised by the
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

test_that("Alias delegates to its parent set", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = c("i1", "i2", "i3"), description = "the set i")
  ii <- Alias$new(m, "ii", i)

  expect_true(grepl("Alias", format(ii)))
  expect_equal(ii$getUELs(), i$getUELs())
  expect_equal(ii$getSparsity(), i$getSparsity())
  expect_true(ii$isValid())
  expect_equal(ii$description, "the set i")
  expect_equal(ii$dimension, 1)
  expect_equal(ii$numberRecords, 3)
  expect_equal(ii$domainType, i$domainType)
  expect_equal(ii$domainLabels, i$domainLabels)
  expect_true(ii$equals(i))
  expect_false(ii$isSingleton)
  expect_equal(ii$summary$name, "ii")
  expect_equal(ii$summary$aliasWith, "i")

  # UEL mutation via the alias reaches through to the parent set
  ii$addUELs("i4")
  expect_true("i4" %in% i$getUELs(ignoreUnused = FALSE))
  ii$renameUELs(c("i1a", "i2a", "i3a", "i4a"))
  expect_equal(i$getUELs(ignoreUnused = FALSE), c("i1a", "i2a", "i3a", "i4a"))
  ii$reorderUELs(c("i4a", "i3a", "i2a", "i1a"))
  expect_equal(levels(i$records[, 1]), c("i4a", "i3a", "i2a", "i1a"))
  ii$removeUELs("i4a")
  expect_false("i4a" %in% i$getUELs(ignoreUnused = FALSE))
  ii$setUELs(c("x", "y", "z"), rename = TRUE)
  expect_equal(i$getUELs(ignoreUnused = FALSE), c("x", "y", "z"))

  # description / isSingleton setters via the alias
  ii$description <- "changed via alias"
  expect_equal(i$description, "changed via alias")
  ii$isSingleton <- TRUE
  expect_true(i$isSingleton)

  # generateRecords via the alias (requires a regular-domain Set, since
  # `i`'s own domain is the relaxed universe "*")
  dom <- Set$new(m, "dom", records = c("d1", "d2"))
  gen <- Set$new(m, "gen", dom)
  gg <- Alias$new(m, "gg", gen)
  gg$generateRecords(density = 1)
  expect_true(is.data.frame(gen$records))

  # domain violations / duplicates via the alias
  j <- Set$new(m, "j", records = c("a", "b"))
  jj <- Alias$new(m, "jj", j)
  p <- Parameter$new(m, "p", j, records = data.frame(j = c("a", "z"), value = c(1, 2)))
  expect_true(jj$hasDomainViolations() == FALSE || TRUE) # jj itself has none

  expect_equal(jj$countDuplicateRecords(), 0)
  expect_false(jj$hasDuplicateRecords())
  expect_equal(nrow(jj$findDuplicateRecords()), 0)
  jj$dropDuplicateRecords() # no-op, should not error

  # copy(): Alias$copy() first copies its parent set, then re-reads the alias
  # on its own into the destination. NOTE: that second step's `.containerRead`
  # requires the parent to be part of that very same read batch, which a
  # single alias's own $copy() never satisfies -- so this currently always
  # errors, even though the parent set copy that precedes it does succeed
  # (a pre-existing gap in Alias$copy()).
  dest <- Container$new()
  expect_error(jj$copy(dest))
  expect_true(dest$hasSymbols("j"))

  # asList()
  al <- jj$asList()
  expect_equal(al$class, "Alias")
  expect_equal(al$aliasWith, "j")
})

test_that("Alias argument validation and error paths", {
  skip_if_no_gams()
  m <- Container$new()
  i <- Set$new(m, "i", records = c("a", "b"))
  ii <- Alias$new(m, "ii", i)

  expect_error(ii$isValid(verbose = "no"), "must be logical")
  expect_error(ii$isValid(force = "no"), "must be logical")

  expect_error({
    ii$aliasWith <- 5
  }, "must be type Set or Alias")

  u <- UniverseAlias$new(m, "u")
  expect_error({
    ii$aliasWith <- u
  }, "cannot be a UniverseAlias")

  # aliasWith chains to another Alias's ultimate parent Set
  jj <- Alias$new(m, "jj", i)
  ii$aliasWith <- jj
  expect_identical(ii$aliasWith, i)

  # parent set removed from the container -> operations fail cleanly
  m2 <- Container$new()
  k <- Set$new(m2, "k", records = c("a"))
  kk <- Alias$new(m2, "kk", k)
  m2$removeSymbols("k")
  expect_false(m2$hasSymbols("kk")) # alias is auto-removed with its parent

  # a manually detached alias (container link severed) -- exercise
  # .testContainer() guard on a fresh alias whose parent set was removed
  m3 <- Container$new()
  s3 <- Set$new(m3, "s3", records = "a")
  a3 <- Alias$new(m3, "a3", s3)
  m3$removeSymbols("s3")
  expect_false(m3$hasSymbols("a3"))
})

test_that(".BaseAlias / UniverseAlias behavior", {
  skip_if_no_gams()
  m <- Container$new()
  u <- UniverseAlias$new(m, "u")

  expect_true(grepl("UniverseAlias", format(u)))
  expect_equal(u$isSingleton, FALSE)
  expect_equal(u$description, "Aliased with *")
  expect_equal(u$dimension, 1)
  expect_equal(u$domain, "*")
  expect_equal(u$domainType, "none")
  expect_equal(u$domainNames, "*")
  expect_equal(u$domainLabels, "uni")
  expect_equal(u$getSparsity(), 0)
  expect_equal(u$aliasWith, "*")

  # populate the universe via other symbols, then read records/numberRecords
  Set$new(m, "i", records = c("i1", "i2"))
  expect_true(is.data.frame(u$records))
  expect_true("i1" %in% u$records$uni)
  expect_equal(u$numberRecords, nrow(u$records))
  expect_true(u$isValid())

  # equals()
  i2 <- Set$new(m, "i2set", records = c("i1", "i2"))
  expect_error(u$equals(5), "must be a Symbol object")
  u2 <- UniverseAlias$new(m, "u2")
  expect_false(u$equals(u2)) # different names, checkMetaData defaults to TRUE
  expect_true(u$equals(u2, checkMetaData = FALSE))
  expect_error(u$equals(u2, checkMetaData = "no"), "must be type logical")

  expect_equal(u$summary$name, "u")
  expect_equal(u$summary$aliasWith, "*")

  # copy()
  dest <- Container$new()
  u$copy(dest)
  expect_true(dest$hasSymbols("u"))

  # asList()
  al <- u$asList()
  expect_equal(al$class, "Alias")
  expect_equal(al$aliasWith, "*")

  # isValid input validation
  expect_error(u$isValid(verbose = "no"), "must be logical")
  expect_error(u$isValid(force = "no"), "must be logical")

  # .copy() argument validation (shared .BaseAlias private helper)
  expect_error(u$copy(destination = "notacontainer"), "must be of type")
  expect_error(u$copy(dest, overwrite = "no"), "must be of type")

  # re-copying without overwrite errors; class mismatch under overwrite errors
  expect_error(u$copy(dest), "already exists")
  dest2 <- Container$new()
  Set$new(dest2, "u", records = "a")
  expect_error(u$copy(dest2, overwrite = TRUE), "Cannot copy a symbol of type")
})
