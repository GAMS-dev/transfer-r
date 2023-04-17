library(gamstransfer)

test_that("readwritetest", {
  m = Container$new()

  # read all symbols
  m$read(testthat::test_path("testdata", "biggdxtest.gdx"))

  # write everything
  m$write(testthat::test_path("gt.gdx"))

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("testdata", "biggdxtest.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)
  expect_equal(ret, 0)
}
)


test_that("test_num_1", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i", records = c("a", "b"))
  expect_true(is.data.frame(i$records))
  expect_equal(nrow(i$records), 2)

  j <- Set$new(m, "j", records = c("c", "d"))
  expect_true(is.data.frame(j$records))
  expect_equal(nrow(j$records), 2)

  recs <- data.frame(list("i" = c("a", "b"), 
  "j" = c("c", "d"), "values" = c(1, 1)))
  a <- Parameter$new(m, "a", c("i", "j"), records = recs)
  expect_true(is.data.frame(a$records))
  expect_equal(nrow(a$records), 2)

  m$write("gt.gdx")

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("testdata", "test1.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)

  expect_equal(ret, 0)
}
)

test_that("test_num_2", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i")
  expect_true(is.null(i$records))

  j <- Set$new(m, "j")
  expect_true(is.null(j$records))

  recs <- data.frame(list("i" = c("a", "b"), 
  "j" = c("c", "d"), "values" = c(1, 1)))
  a <- Parameter$new(m, "a", c(i, j), recs, domainForwarding = TRUE)

  expect_true(is.data.frame(i$records))
  expect_equal(as.character(i$records$uni), c("a", "b"))

  expect_true(is.data.frame(j$records))
  expect_equal(as.character(j$records$uni), c("c", "d"))

  m$write("gt.gdx")

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("testdata", "test2.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)

  expect_equal(ret, 0)
}
)

test_that("test_num_3", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i")
  expect_true(is.null(i$records))

  j <- Alias$new(m, "j", i)
  expect_true(is.null(j$records))

  recs <- data.frame(list("i" = c("a", "b"), 
  "j" = c("a", "b"), "values" = c(1, 1)))
  a <- Parameter$new(m, "a", c(i, j), recs, domainForwarding = TRUE)

  expect_true(is.data.frame(i$records))
  expect_equal(as.character(i$records$uni), c("a", "b"))

  expect_true(is.data.frame(j$records))
  expect_equal(as.character(j$records$uni), c("a", "b"))
  expect_equal(nrow(a$records), 2)

  m$write("gt.gdx")

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("testdata", "test3.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)

  expect_equal(ret, 0)
}
)

test_that("test_num_4", {
  m <- Container$new()

  i <- Set$new(m, "i")
  expect_true(is.null(i$records))

  j <- Set$new(m, "j", i)
  expect_true(is.null(j$records))

  k <- Set$new(m, "k", j)
  expect_true(is.null(k$records))

  l = Set$new(m, "l", k, records = c("a", "b"), domainForwarding = TRUE )
  expect_true(is.data.frame(i$records))
  expect_equal(as.character(i$records$uni), c("a", "b"))

  expect_true(is.data.frame(j$records))
  expect_equal(as.character(j$records$i), c("a", "b"))

  expect_true(is.data.frame(k$records))
  expect_equal(as.character(k$records$j), c("a", "b"))

  expect_true(is.data.frame(l$records))
  expect_equal(as.character(l$records$k), c("a", "b"))

  m$write("gt.gdx")

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("testdata", "test4.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)

  expect_equal(ret, 0)
}
)

test_that("test_num_5", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i")
  expect_true(is.null(i$records))

  recs <- data.frame(list("i" = "c", "element_text" = "desc for elem 'c'"))
  j <- Set$new(m, "j", i, records = recs, domainForwarding = TRUE)
  expect_true(is.data.frame(i$records))
  expect_equal(as.character(i$records$uni), c("c"))
  expect_true(is.data.frame(j$records))
  expect_equal(as.character(j$records$i), c("c"))

  k <- Set$new(m, "k", j)
  expect_true(is.data.frame(i$records))
  expect_equal(as.character(i$records$uni), c("c"))
  expect_true(is.data.frame(j$records))
  expect_equal(as.character(j$records$i), c("c"))
  expect_true(is.null(k$records))

  l <- Set$new(m, "l", k, records = c("a", "b"), domainForwarding = TRUE)
  expect_true(is.data.frame(i$records))
  expect_equal(as.character(i$records$uni), c("c", "a", "b"))
  expect_true(is.data.frame(j$records))
  expect_equal(as.character(j$records$i), c("c", "a", "b"))

  expect_true(is.data.frame(k$records))
  expect_equal(as.character(k$records$j), c("a", "b"))

  expect_true(is.data.frame(l$records))
  expect_equal(as.character(l$records$k), c("a", "b"))

  m$write("gt.gdx")

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("testdata", "test5.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)

  expect_equal(ret, 0)
}
)

test_that("test_num_6", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i", records = c("c", "a", "b"))
  expect_true(is.data.frame(i$records))
  m$write("gt.gdx")

  m2 = Container$new(testthat::test_path("testdata", "test6_uels.gdx"))
  expect_true(inherits(m2, "Container"))
  expect_true(is.data.frame(m2["foo"]$records))

  expect_equal(as.character(m$getUniverseSet()), as.character(m2["foo"]$records$uni))
}
)

test_that("test_num_7", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i", records = c("c", "a", "b"))
  expect_true(is.data.frame(i$records))

  m$write("gt.gdx", uelPriority = list("a"))

  m2 = Container$new(testthat::test_path("testdata", "test7_uels.gdx"))
  expect_true(inherits(m2, "Container"))
  expect_true(is.data.frame(m2["foo"]$records))

  expect_equal(c("a", "c", "b"), as.character(m2["foo"]$records$uni))
  
}
)

test_that("test_num_8", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i", records = c("a", "b"))
  expect_equal(as.character(i$records$uni), c("a", "b"))

  j <- Alias$new(m, "j", i)
  expect_equal(as.character(j$records$uni), c("a", "b"))
  expect_equal(j$aliasWith$name, "i")

  k <- Alias$new(m, "k", j)
  expect_equal(as.character(k$records$uni), c("a", "b"))
  expect_equal(j$aliasWith$name, "i")

  #try writing
  m$write("out.gdx")
}
)

test_that("test_num_9", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i", list("*", "*"), records = 
  data.frame(list("col1"=c("a","b"), "col2"=c("c","d"))))

  j <- Parameter$new(m, "j", "*", records = 
  data.frame(list("col1" = c("e", "f"), "col2" = c(1, 1))))
  expect_equal(as.character(i$records$col1), c("a", "b"))
  expect_equal(as.character(i$records$col2), c("c", "d"))

  expect_equal(as.character(m$getUniverseSet()), c("a", "c", "b", "d", "e", "f"))

  #just try writing to see if there are any errors
  m$write("out.gdx")
}
)

test_that("test_num_10", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i")
  expect_true(is.null(i$records))

  j <- Set$new(m, "j")
  expect_true(is.null(j$records))

  a <- Parameter$new(m, "a", list(i, j), domainForwarding=TRUE)

  df <- data.frame(list("i"= c("a", "b"),
  "j" = c("c", "d"), "value" = c(1, 1)))

  a$records <- df

  expect_equal(as.character(i$records$uni), c("a", "b"))
  expect_equal(as.character(j$records$uni), c("c", "d"))

  #try writing
  m$write("out.gdx")
}
)

test_that("test_num_11", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i", records = c("a", "b", "c"))
  expect_equal(as.character(i$records$uni), c("a", "b", "c"))


  j <- Parameter$new(m, "j", i, 
  records = data.frame("j"=c("a", "c"), "val" = c(1, 2)))
  expect_equal(as.character(j$records$j), c("a", "c"))
  expect_equal(j$records$value, c(1, 2))

  m$removeSymbols(c("i", "j"))

  i <- Set$new(m, "i", records = c("a", "b", "c", "d"))
  j <- Parameter$new(m, "j", i, 
  records = data.frame("i"= c("a", "c"), "val" = c(1, 2)))
  expect_equal(j$toDense(), array(c(1,0,2,0)))
  m$write("out.gdx")
}
)

test_that("test_num_12", {
  testthat::expect_warning(Container$new(testthat::test_path("testdata", "test12.gdx")))
}
)

test_that("test_num_13", {
  m  = Container$new()
  i = Set$new(m, "i", records=c("a", "b", "c"))
  j = Set$new(m, "j", records=c("d", "e", "f"))
  a = Parameter$new(m, "a", i, records=data.frame("i"=c("a","c"),"val"=c(1, 2) ))
  b = Parameter$new(m, "b", j, records=data.frame("j"=c("e","f"),"val"=c(1, 2) ))

  m$removeSymbols("i")
  expect_equal(unlist(m$data$keys()), c("j", "a", "b"))

  m$removeSymbols(c("a", "b"))
  expect_equal(unlist(m$data$keys()), c("j"))

  m$write("out.gdx")
}
)


test_that("test_num_14", {
  m  = Container$new()
  i = Set$new(m, "i", records=c("a", "b", "c"))

  expect_equal(unlist(m$data$keys()), c("i"))

  m$renameSymbol("i", "h")
  expect_equal(unlist(m$data$keys()), c("h"))

  m$write("out.gdx")
}
)

# test_that("test_num_15", {
#   m  = Container$new()
#   i = Set$new(m, "i", records=c("a", "b", "c"))
#   a = Parameter$new(m, "a", i, records=data.frame(c("aa", "c"), c(1, 2)))

#   expect_equal(a$findDomainViolations(), 1)
#     expect_equal(a$isValid(), FALSE)
# }
# )

test_that("test_num_16", {
  m  = Container$new()
  i = Set$new(m, "i")
  j = Set$new(m, "j", i, records = c("a", "b"), domainForwarding = TRUE)
  k = Set$new(m, "k")
  l = Set$new(m, "l", k, records = c("c"), domainForwarding = TRUE)
  a = Parameter$new(m, "a", l, records = data.frame(c("aa", "c"), c(1, 2)), domainForwarding = TRUE)

  # check container
  expect_equal(m$.requiresStateCheck, TRUE)
  expect_equal(m$isValid(), TRUE)
  expect_equal(m$.requiresStateCheck, FALSE)

  expect_equal(as.character(m["l"]$records$k), c("c", "aa"))
  expect_equal(as.character(m["k"]$records$uni), c("c", "aa"))
  expect_equal(as.character(m["j"]$records$i), c("a", "b"))
  expect_equal(as.character(m["i"]$records$uni), c("a", "b"))

  expect_equal(as.character(m$getUniverseSet()), c("a", "b", "c", "aa"))
  expect_equal(a$isValid(), TRUE)
  expect_equal(l$isValid(), TRUE)
}
)

test_that("test_num_17", {
  m  = Container$new()
  m$addSet("i")
  m$addSet("j", m["i"], records = c("a", "b"), domainForwarding = TRUE)
  m$addSet("k")
  m$addSet("l", m["k"], records = c("c"), domainForwarding = TRUE)

  m$addParameter("a", m["l"], records = data.frame(c("aa", "c"), c(1, 2)), domainForwarding = TRUE)

  # check container
  expect_equal(m$.requiresStateCheck, TRUE)
  expect_equal(m$isValid(), TRUE)
  expect_equal(m$.requiresStateCheck, FALSE)

  expect_equal(as.character(m["l"]$records$k), c("c", "aa"))
  expect_equal(as.character(m["k"]$records$uni), c("c", "aa"))
  expect_equal(as.character(m["j"]$records$i), c("a", "b"))
  expect_equal(as.character(m["i"]$records$uni), c("a", "b"))

  expect_equal(as.character(m$getUniverseSet()), c("a", "b", "c", "aa"))

  expect_equal(m$listSymbols(isValid = FALSE), NULL)
}
)

test_that("test_num_18", {

    default_values = list(
    "binary" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 0.0,
        "upper" = 1.0,
        "scale" = 1.0
    ),
    "integer" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 0.0,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    ),
    "positive" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 0.0,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    ),
    "negative" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = SpecialValues$NEGINF,
        "upper" = 0.0,
        "scale" = 1.0
    ),
    "free" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = SpecialValues$NEGINF,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    ),
    "sos1" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 0.0,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    ),
    "sos2" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 0.0,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    ),
    "semicont" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 1.0,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    ),
    "semiint" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 1.0,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    )
  )
  m  = Container$new()
  df = data.frame("domain"=c("i0"))

  types = c(
    "binary",
    "integer",
    "positive",
    "negative",
    "free",
    "sos1",
    "sos2",
    "semicont",
    "semiint"
  )

  for (i in types) {
    varname = paste0("a_", i)
    m$addVariable(varname, i, "*", records = df)

    expect_equal(colnames(m[varname]$records),
    c("domain", "level", "marginal", "lower", "upper", "scale"))

    expect_equal(m[varname]$records[1, "level"], 
    default_values[[i]][["level"]])

    expect_equal(m[varname]$records[1, "marginal"], 
    default_values[[i]][["marginal"]])

    expect_equal(m[varname]$records[1, "lower"], 
    default_values[[i]][["lower"]])

    expect_equal(m[varname]$records[1, "upper"], 
    default_values[[i]][["upper"]])

    expect_equal(m[varname]$records[1, "scale"], 
    default_values[[i]][["scale"]])
  }
}
)

test_that("test_num_19", {

  default_values = list(
      "eq" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = 0.0,
          "scale" = 1.0
      ),
      "geq" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "leq" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = SpecialValues$NEGINF,
          "upper" = 0.0,
          "scale" = 1.0
      ),
      "nonbinding" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = SpecialValues$NEGINF,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "cone" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "external" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = 0.0,
          "scale" = 1.0
      ),
      "boolean" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = 0.0,
          "scale" = 1.0
      )
  )

  m  = Container$new()
  df = data.frame("domain"= c("i0"))

  types = c(
  "eq", "geq", "leq", "nonbinding", "cone", "external", "boolean"
  )

  for (i in types) {
    eqname = paste0("a_", i)
    m$addEquation(eqname, i, "*", records = df)

    expect_equal(colnames(m[eqname]$records),
    c("domain", "level", "marginal", "lower", "upper", "scale"))

    expect_equal(m[eqname]$records[1, "level"], 
    default_values[[i]][["level"]])

    expect_equal(m[eqname]$records[1, "marginal"], 
    default_values[[i]][["marginal"]])

    expect_equal(m[eqname]$records[1, "lower"], 
    default_values[[i]][["lower"]])

    expect_equal(m[eqname]$records[1, "upper"], 
    default_values[[i]][["upper"]])

    expect_equal(m[eqname]$records[1, "scale"], 
    default_values[[i]][["scale"]])
  }
}
)

test_that("test_num_20", {
  m = Container$new()
  df=  data.frame(list("i0", 1, 1, 1, 1, 1))
  colnames(df)= c("domain", "marginal", "lower", "scale", "upper", "level")

  type = c(
    "binary",
    "integer",
    "positive",
    "negative",
    "free",
    "sos1",
    "sos2",
    "semicont",
    "semiint"
  )

  for (i in type) {
    varname = paste0("a_", i)
    m$addVariable(varname, i, "*", records = df)
    expect_equal(colnames(m[varname]$records),
    c("domain", "level", "marginal", "lower", "upper", "scale"))
  }
}
)

test_that("test_num_21", {
  m = Container$new()
  df=  data.frame(list("i0", 1, 1, 1, 1, 1))
  colnames(df)= c("domain", "marginal", "lower", "scale", "upper", "level")

  type = c(
    "eq", "geq", "leq", "nonbinding", "cone", "external", "boolean"
  )

  for (i in type) {
    eqname = paste0("a_", i)
    m$addEquation(eqname, i, "*", records = df)
    expect_equal(colnames(m[eqname]$records),
    c("domain", "level", "marginal", "lower", "upper", "scale"))
  }
}
)


test_that("test_num_22", {
  m = Container$new()
  m$read(testthat::test_path("testdata", "trnsport.gdx"), records = FALSE)

  for (i in m$data$values()) {
    expect_equal(m[i$name]$records, NULL)
  }
}
)


test_that("test_num_23", {
  m = Container$new()
  m$read(testthat::test_path("testdata", "trnsport.gdx"), c("i", "j", "x"))

  expect_equal(m["i"]$domainType, "none")
  expect_equal(m["j"]$domainType, "none")
  expect_equal(m["x"]$domainType, "regular")
}
)

test_that("test_num_24", {
  m = Container$new()
  m$read(testthat::test_path("testdata", "trnsport.gdx"), c("x"))

  expect_equal(unlist(m$data$keys()), "x")
  expect_equal(m["x"]$domainType, "relaxed")
}
)

test_that("test_num_25", {
  m = Container$new(testthat::test_path("testdata", "test25.gdx"))

  expect_equal(unlist(m$data$keys()), "i")
  expect_equal(m["i"]$domain, c("i"))
  expect_equal(m["i"]$domainType, "relaxed")
}
)


test_that("test_num_26", {
  m = Container$new()
  i = Set$new(m, "i", "p")
  expect_equal(i$domainType, "relaxed")

  j = Set$new(m, "j", i, records = 
  data.frame(i=c("c"), c("desc for elem 'c'")), domainForwarding=TRUE)

  df = data.frame("p" =c("c"), "element_text" = c(""))
  df$p = factor(df$p, ordered = TRUE)
  expect_equal(i$records, df)

  df = data.frame("i" =c("c"), "element_text" = c("desc for elem 'c'"))
  df$i = factor(df$i, ordered = TRUE)
  expect_equal(j$records, df)

  k = Set$new(m ,"k", "j")
  expect_equal(k$records, NULL)

  l = Set$new(m, "l", k, records = c("a", "b"), domainForwarding = TRUE)

  # test l
  df = data.frame("k"=c("a", "b"), "element_text"=c("", ""))
  df$k = factor(df$k, ordered=TRUE)
  expect_equal(l$records, df)

  # test k
  df = data.frame("j"=c("a", "b"), "element_text"=c("",""))
  df$j = factor(df$j, ordered = TRUE)
  expect_equal(k$records, df)

  # test j
  df = data.frame("i"=c("c"), "element_text"=c("desc for elem 'c'"))
  df$i = factor(df$i, ordered = TRUE)
  expect_equal(j$records, df)

  # test i
  df = data.frame("p"=c("c"), "element_text"=c(""))
  df$p = factor(df$p, ordered = TRUE)
  expect_equal(i$records, df)

}
)

test_that("test_num_27", {
  m = Container$new()
  m$read(testthat::test_path("testdata", "test27.gdx"))

  # write everything
  m$write(testthat::test_path("gt.gdx"))

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("testdata", "test27.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)
  expect_equal(ret, 0)
}
)

test_that("test_num_28", {
  m = Container$new()

  i = Set$new(m, "i")
  a_eq = Equation$new(m, "a_eq", "eq", i)
  a_geq = Equation$new(m, "a_geq", "geq", i)
  a_leq = Equation$new(m, "a_leq", "leq", i)
  a_nonbinding = Equation$new(m, "a_nonbinding", "nonbinding", i)
  a_cone = Equation$new(m, "a_cone", "cone", i)
  a_external = Equation$new(m, "a_external", "external", i)
  a_boolean = Equation$new(m, "a_boolean", "boolean", i)
  # try writing

  expect_equal(m$write("gt.gdx"), NULL)
}
)

test_that("test_num_29", {
  m = Container$new()

  i = Set$new(m, "i")
  a_binary = Variable$new(m, "a_binary", "binary", i)
  a_integer = Variable$new(m, "a_integer", "integer", i)
  a_positive = Variable$new(m, "a_positive", "positive", i)
  a_negative = Variable$new(m, "a_negative", "negative", i)
  a_free = Variable$new(m, "a_free", "free", i)
  a_sos1 = Variable$new(m, "a_sos1", "sos1", i)
  a_sos2 = Variable$new(m, "a_sos2", "sos2", i)
  a_semicont = Variable$new(m, "a_semicont", "semicont", i)
  a_semiint = Variable$new(m, "a_semiint", "semiint", i)

  # try writing
  expect_equal(m$write("gt.gdx"), NULL)
}
)

test_that("test_num_30", {
  m = Container$new()
  i = Set$new(m, "i")

  a = Parameter$new(m, "a", i)
  # try writing
  expect_equal(m$write("gt.gdx"), NULL)
}
)

test_that("test_num_31", {
  m = Container$new()
  i = Set$new(m, "i", "j")
  j = Set$new(m, "j", "i")

  a = Parameter$new(m, "a", c(i, j))
  m$removeSymbols("j")
  j = Set$new(m, "j", "i")
  expect_true(a$isValid(verbose=TRUE))

}
)

test_that("test_num_32", {
  m = Container$new()
  i = Set$new(m, "i")
  m$isValid()

  expect_equal(m$.requiresStateCheck, FALSE)

  j = Set$new(m, "j")

  expect_true(m$.requiresStateCheck)
}
)

test_that("test_num_33", {
  m = Container$new()
  i = Set$new(m, "i")
  j = Set$new(m, "j")
  k = Set$new(m, "k")

  i$domain = j
  j$domain = k
  k$domain = i

  expect_error(m$.__enclos_env__$private$validSymbolOrder())
}
)

test_that("test_num_34", {
  m = Container$new()
  i = Set$new(m, "i")
  j = Alias$new(m, "j", i)
  j$setRecords(c("a", "b"))
  df = data.frame("uni"=c("a", "b"), "element_text"=c("",""))
  df$uni = factor(df$uni, ordered=TRUE)

  expect_equal(i$records, df)
}
)

test_that("test_num_35", {
  m = Container$new()
  i = Set$new(m, "i")
  j = Alias$new(m, "j", i)
  j$description = "just a test"
  expect_equal(i$description, "just a test")
}
)

test_that("test_num_36", {
  m = Container$new()
  i = Set$new(m, "i")
  k = Set$new(m, "k")
  j = Alias$new(m, "j", i)
  j$domain = c(k, k)
  expect_equal(i$domainNames, c("k", "k"))
}
)

test_that("test_num_37", {
  m = Container$new()
  i = Set$new(m, "i")
  j = Alias$new(m, "j", i)
  j$isSingleton = TRUE
  expect_equal(i$isSingleton, TRUE)
}
)

test_that("test_num_38", {
  m = Container$new()
  i = Set$new(m, "i")

  expect_equal(i$dimension, 1)
  expect_equal(i$domain, c("*"))

  i$dimension = 2
  expect_equal(i$dimension, 2)
  expect_equal(i$domain, c("*", "*"))

  i$dimension = 0
  expect_equal(i$dimension, 0)
  expect_equal(i$domain, list())

  a = Parameter$new(m, "a")
  expect_equal(a$dimension, 0)
  expect_equal(a$domain, list())
  expect_equal(a$isScalar, TRUE)

  a$dimension = 2
  expect_equal(a$domain, list("*", "*"))
  expect_equal(a$isScalar, FALSE)

  a$dimension = 0
  expect_equal(a$domain, list())
  expect_equal(a$isScalar, TRUE)

  ip = Alias$new(m, "ip", i)
  ip$dimension = 2
  expect_equal(ip$dimension, 2)
  expect_equal(i$dimension, 2)
  expect_equal(ip$domain, list("*", "*"))
  expect_equal(i$domain, list("*", "*"))
}
)

test_that("test_num_39", {
  m = Container$new()
  i = Set$new(m, "i")
  expect_equal(i$numberRecords, 0)
  m$removeSymbols("i")

  i = Set$new(m, "i", records = c("a", "b"))
  expect_equal(i$numberRecords, 2)

  j = Set$new(m, "j", i, records = c("a", "c"))

  # expect_true(is.na(j$numberRecords)) #because NA
}
)

test_that("test_num_40", {
  m = Container$new(testthat::test_path("testdata", "test40.gdx"))
  expect_true(m["a"]$domainType == "regular")
  expect_true(is.null(m["a"]$records))
}
)

test_that("test_num_41", {
  # test for matrix input of parameter records 2D
  m = Container$new()
  i = Set$new(m, "i", records = c("a","b"))
  j = Set$new(m, "j", records = c("x","y","z"))

  recs = matrix(c(1:6), nrow = 2, ncol=3)
  d = Parameter$new(m, "d", c(i, j), records = recs)

  df = data.frame(i = c("a","a", "a", "b","b","b"), 
  j = c("x", "y", "z", "x", "y", "z"),
  value = c(1,3,5,2,4,6))
  df[,1] = factor(df[,1], ordered = TRUE)
  df[,2] = factor(df[,2], ordered = TRUE)


  expect_equal(d$records, df)

  # test for array input of parameter records 2D
  m$removeSymbols("d")
  recs = array(c(1:6), dim=c(2,3))
  d = Parameter$new(m, "d", c(i, j), records = recs)
  expect_equal(d$records, df)

  #test for array 3D
  recs = array(c(1:12), dim=c(2,3,2))
  k = Set$new(m, "k", records = c("alpha", "beta"))
  d3 = Parameter$new(m, "d3", c(i, j, k), records = recs)

  df = data.frame(i = c("a","a", "a", "a","a","a", "b", "b","b","b", "b", "b"), 
  j = c("x", "x", "y", "y", "z", "z", "x", "x", "y", "y", "z", "z"),
  k = c("alpha","beta","alpha","beta","alpha","beta",
  "alpha","beta","alpha","beta","alpha","beta"),
  value = c(1,7,3,9,5,11,2,8,4,10,6,12))
  df[,1] = factor(df[,1], ordered = TRUE)
  df[,2] = factor(df[,2], ordered = TRUE)
  df[,3] = factor(df[,3], ordered = TRUE)
  expect_equal(d3$records, df)
}
)

test_that("test_num_42", {
  m = Container$new()
  i = Set$new(m, "i", records = paste0("i", c(1:5)))
  j = Set$new(m, "j", records = paste0("j", c(1:5)))
  recs=data.frame("i"=c("i1","i2","i3","i4","i5"), 
  "j"=c("j1","j2","j3","j4","j5"), "val"=c(1,1,1,1,1))
  a = Parameter$new(m, "a", domain=c(i, j), records = recs)
  ap = Parameter$new(m, "ap", domain=c(i, j), records = a$toDense())
  expect_equal(a$records, ap$records)

  v = Variable$new(m, "v", domain=c(i, j), records = list("level"=a$toDense()))

  df = data.frame(i=i$records[,1], 
  j=j$records[,1],
  level= c(1,1,1,1,1),
  marginal=c(0,0,0,0,0),
  lower = replicate(5, -Inf),
  upper = replicate(5, Inf),
  scale = replicate(5, 1)
  )

  expect_equal(v$records, df)

  v2 = Variable$new(m, "v2", domain=c(i, j),
  records = list(
    "level" = a$toDense(),
    marginal = a$toDense(),
    lower = a$toDense(),
    upper = a$toDense(),
    scale = a$toDense()
  )
  )

  e = Equation$new(m, "e", "eq", domain=c(i, j),
  records = list(
    "level" = a$toDense(),
    marginal = a$toDense(),
    lower = a$toDense(),
    upper = a$toDense(),
    scale = a$toDense()
  )
  )

  df = data.frame(i=i$records[,1], 
  j=j$records[,1],
  level= c(1,1,1,1,1),
  marginal = replicate(5, 1),
  lower = replicate(5, 1),
  upper = replicate(5, 1),
  scale = replicate(5, 1)
  )

  expect_equal(v2$records, df)
  expect_equal(e$records, df)
}
)

# test converting arrays to records for Parameters, Varaibles, Equations
test_that("test_num_43", {
  m = Container$new()
  i = Set$new(m, "i", records = paste0("i", c(1:5)))
  j = Set$new(m, "j", records = paste0("j", c(1:5)))
  recs=data.frame("i"=c("i1","i2","i3","i4","i5"), 
  "j"=c("j1","j2","j3","j4","j5"), "val"=c(0,0,0,0,0))
  recs[2,"val"] = 1
  recs[4, "val"] = SpecialValues$EPS
  recs = recs[-c(1,3,5),]
  row.names(recs) <- NULL

  recs2 = data.frame("i"=c("i1","i2","i3","i4","i5"), 
  "j"=c("j1","j2","j3","j4","j5"), "val"=c(0,0,0,0,0))
  recs2["val"] = SpecialValues$EPS

  a = Parameter$new(m, "a", domain=c(i, j), records = recs)
  ap = Parameter$new(m, "ap", domain=c(i, j), records = recs2)

  v = Variable$new(m, "v", domain=c(i, j),
  records = list(
    "level"=a$toDense(),
    "marginal"= ap$toDense()
  ))

  e = Equation$new(m, "e", "eq", domain=c(i, j),
  records = list(
    "level"=a$toDense(),
    "marginal"= ap$toDense()
  ))

  cols = v$domainLabels
  cols = append(cols, c("level", "marginal", "lower", "upper", "scale"))

  recs=data.frame("i"=c("i1","i2","i3","i4","i5"), 
  "j"=c("j1","j2","j3","j4","j5"), "level"=c(0,0,0,0,0),
  "marginal"=replicate(5, SpecialValues$EPS),
  "lower" = replicate(5, SpecialValues$NEGINF),
  "upper" = replicate(5, SpecialValues$POSINF),
  "scale" = replicate(5, 1))
  recs[,1] = factor(recs[,1], ordered=TRUE)
  recs[,2] = factor(recs[,2], ordered=TRUE)
  recs[2,"level"] = 1
  recs[4, "level"] = SpecialValues$EPS

  expect_equal(v$records, recs)

  recs=data.frame("i"=c("i1","i2","i3","i4","i5"), 
  "j"=c("j1","j2","j3","j4","j5"), "level"=c(0,0,0,0,0),
  "marginal"=replicate(5, SpecialValues$EPS),
  "lower" = replicate(5, 0),
  "upper" = replicate(5, 0),
  "scale" = replicate(5, 1))
  recs[,1] = factor(recs[,1], ordered=TRUE)
  recs[,2] = factor(recs[,2], ordered=TRUE)
  recs[2,"level"] = 1
  recs[4, "level"] = SpecialValues$EPS
  
  expect_equal(e$records, recs)
}
)

test_that("test_num_44", {
  m = Container$new(testthat::test_path("testdata", "test44.gdx"))
  expect_true(m["i"]$type == "free")

  m$write("out.gdx")

  m2 = Container$new(testthat::test_path("out.gdx"))
  expect_true(m2["i"]$type == "free")
}
)

test_that("test_num_45", {
  m = Container$new()
  i = Set$new(m, "i", records = paste0("i", 1:5))
  j = Set$new(m, "j", i, records = paste0("i", 1:5))

  m$removeSymbols("i")

  expect_true(is.null(i$refContainer))
  expect_true(unlist(m$data$keys()) == c("j"))
  expect_true(j$isValid())
  expect_equal(j$domain, "*")

}
)

test_that("test_num_46", {
  h = ConstContainer$new()
  h$read(testthat::test_path("testdata", "biggdxtest.gdx"))

  m = Container$new(h)
  # write everything
  m$write(testthat::test_path("gt.gdx"))

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("testdata", "biggdxtest.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)
  expect_equal(ret, 0)
}
)

test_that("test_num_47", {
  m = ConstContainer$new(testthat::test_path("testdata", "test47.gdx"))
  expect_true(m["a"]$domainType == "regular")
  expect_true(is.null(m["a"]$records))
}
)

test_that("test_num_48", {
  m = Container$new()
  i = Set$new(m, "i", domain=c("k"), records = paste0("i", 1:5))
  j = Set$new(m, "j", domain=c("*"), records = paste0("j", 1:5))
  k = Set$new(m, "k", domain = c("*", "l"), records = data.frame(paste0("k", 1:5), paste0("l", 1:5)))
  l = Set$new(m, "l", i, records=paste0("i", 1:2))
  m$write("data.gdx")

  m = ConstContainer$new("data.gdx")

  expect_true(m["i"]$domainType == "relaxed")
  expect_true(m["j"]$domainType == "none")
  expect_true(m["k"]$domainType == "relaxed")
  expect_true(m["l"]$domainType == "regular")

  expect_true(is.null(m["i"]$records))
  expect_true(is.null(m["j"]$records))
  expect_true(is.null(m["k"]$records))
  expect_true(is.null(m["l"]$records))
}
)


test_that("test_num_49", {
  m = ConstContainer$new(testthat::test_path("testdata", "test49.gdx"))
  expect_equal(m["i"]$domainType, "relaxed")
}
)

test_that("test_num_50", {
  m = Container$new()
  i = Set$new(m, "i", domain=c("k"), records = paste0("i", 1:5))
  j = Set$new(m, "j", domain=c("*"), records = paste0("j", 1:5))  
  k = Set$new(m, "k", domain = c("*", "l"), records = data.frame(paste0("k", 1:5), paste0("l", 1:5)))
  l = Set$new(m, "l", i, records=paste0("i", 1:2))
  m$write("data.gdx")


  m = ConstContainer$new()
  m$read(testthat::test_path("data.gdx"), symbols="i")
  expect_true(!is.null(m["i"]))

  m = ConstContainer$new()
  m$read(testthat::test_path("data.gdx"), symbols= c("i", "j"))
  expect_true(!is.null(m["i"]))
  expect_true(!is.null(m["j"]))

  m = ConstContainer$new()
  expect_error(m$read(testthat::test_path("data.gdx"), symbols= c("i", "j", "dummy")))
}
)

test_that("test_num_51", {
  expect_error(ConstContainer$new("dummy.gdx"))
  expect_error(Container$new("dummy.gdx"))

  m = Container$new()
  expect_error(m$read("dummy.gdx"))
}
)

test_that("test_num_52", {
  expect_error(ConstContainer$new(data.frame()))
  expect_error(Container$new(data.frame()))

  m = Container$new()
  expect_error(m$read(data.frame()))
}
)

test_that("test_num_53", {
  expect_warning(ConstContainer$new(testthat::test_path("testdata", "test53.gdx")))
}
)

test_that("test_num_54", {
  m = Container$new(testthat::test_path("testdata", "trnsport.gdx"))

  for (i in unlist(m$data$keys())) {
    expect_true(is.list(m[i]$summary))
  }

  expect_true(is.vector(m$listSymbols()))
  expect_true(is.vector(m$listParameters()))
  expect_true(is.vector(m$listSets()))
  expect_true(is.null(m$listAliases()))
  expect_true(is.vector(m$listVariables()))
  expect_true(is.vector(m$listEquations()))

  expect_equal(length(m$listVariables(types="free")), 1)
  expect_equal(length(m$listVariables(types="positive")), 1)

  expect_equal(length(m$listEquations(types="geq")), 1)
  expect_equal(length(m$listEquations(types="eq")), 1)
  expect_equal(length(m$listEquations(types="leq")), 1)
  expect_equal(length(m$listEquations(types=c("geq", "leq"))), 2)

  expect_true(is.data.frame(m$describeSets()))
  expect_true(is.data.frame(m$describeParameters()))
  expect_true(is.data.frame(m$describeVariables()))
  expect_true(is.data.frame(m$describeEquations()))

  expect_equal(nrow(m$describeEquations(m$listEquations(types = "geq"))), 1)
  expect_equal(nrow(m$describeEquations(m$listEquations(types = "eq"))), 1)
  expect_equal(nrow(m$describeEquations(m$listEquations(types = "leq"))), 1)
  
}
)

test_that("test_num_55", {
  m = ConstContainer$new(testthat::test_path("testdata", "trnsport.gdx"))
  old_names = unlist(m$data$keys())
  for (i in unlist(m$data$keys())) {
    expect_true(is.null(m[i]$records))
  }

  m = ConstContainer$new()
  m$read(testthat::test_path("testdata", "trnsport.gdx"))
  for (i in unlist(m$data$keys())) {
    expect_true(is.data.frame(m[i]$records))
  }

  m = ConstContainer$new(testthat::test_path("testdata", "test55.gdx"))
  new_names = unlist(m$data$keys)
  for (i in unlist(m$data$keys())) {
    expect_true(is.null(m[i]$records))
  }

  expect_true(!identical(old_names, new_names))
}
)

test_that("test_num_56", {
  m = ConstContainer$new()
  m$read(testthat::test_path("testdata", "trnsport.gdx"))

  for (i in unlist(m$data$keys())) {
    expect_true(is.data.frame(m[i]$records))
  }
}
)

test_that("test_num_57", {
df = data.frame(expand.grid(c("a1","a2"), 
c("b1","b2"), c("c1","c2"), c("d1","d2")), 
stringsAsFactors=TRUE)

df$value = 1
m = Container$new()
t = Parameter$new(m, "t", domain=replicate(4, "*"))
t$records = df

expect_true(t$isValid())
}
)

# test make sure describe* are consistent between Container and ConstContainer
test_that("test_num_58", {
  m = Container$new(testthat::test_path("testdata", "trnsport_with_alias.gdx"))

  m2 = ConstContainer$new()
  m2$read(testthat::test_path("testdata", "trnsport_with_alias.gdx"))

  expect_true(identical(m2$describeSets(), m$describeSets()))
  expect_true(identical(m2$describeSets(append(m2$listSets(), m2$listAliases())),
  m$describeSets(append(m$listSets(), m$listAliases()))))

  expect_equal(m2$describeParameters(), m$describeParameters())
  expect_equal(m2$describeVariables(), m$describeVariables())
  expect_equal(m2$describeEquations(), m$describeEquations())

}
)

# test read from another Container
test_that("test_num_59", {
  m = Container$new(testthat::test_path("testdata", "trnsport_with_alias.gdx"))
  expect_true(m$isValid())

  m2 = Container$new(m)
  expect_true(m2$isValid())
}
)

# test read from another Container with invalid symbols
test_that("test_num_60", {
m = Container$new()
i = Set$new(m, "i", records = paste0("i",1:10))
j = Set$new(m, "j", records = paste0("j",1:10))

a = Parameter$new(m, "a", c(i, j))
a$.__enclos_env__$private$.records = "ham"
expect_true(!a$isValid())
expect_error(Container$new(m))

m2 = Container$new()
m2$read(m, c("i", "j"))
expect_equal(unlist(m2$data$keys()), c("i", "j"))
expect_error(m2$read(m, "a"))
}
)


# test converting arrays with EPS (across several columns) values for Parameters, Variables, Equations
test_that("test_num_61", {
  m = Container$new()
  i = Set$new(m, "i", records = paste0("i", c(1:5)))
  j = Set$new(m, "j", records = paste0("j", c(1:5)))
  recs=data.frame("i"=c("i1","i2","i3","i4","i5"), 
  "j"=c("j1","j2","j3","j4","j5"), "val"=c(0,1,0,SpecialValues$EPS,0))
  recs <- recs[-c(1,3,5),]
  a = Parameter$new(m, "a", domain=c(i, j), records = recs)

  recs=data.frame("i"=c("i1","i2","i3","i4","i5"), 
  "j"=c("j1","j2","j3","j4","j5"), "val"=replicate(5,SpecialValues$EPS))

  ap = Parameter$new(m, "ap", domain=c(i, j), records = recs)

  v = Variable$new(m, "v", domain=c(i, j), records = 
  list("level"=a$toDense(), "marginal"=ap$toDense()))

  e = Equation$new(m, "e", "eq", domain=c(i, j),
  records = list(
    "level" = a$toDense(),
    marginal = ap$toDense()
  )
  )

  df = data.frame(i=i$records[,1], 
  j=j$records[,1],
  level= c(0,1,0,SpecialValues$EPS,0),
  marginal=replicate(5, SpecialValues$EPS),
  lower = replicate(5, -Inf),
  upper = replicate(5, Inf),
  scale = replicate(5, 1)
  )

  expect_equal(v$records, df)

  df = data.frame(i=i$records[,1], 
  j=j$records[,1],
  level= c(0,1,0,SpecialValues$EPS,0),
  marginal = replicate(5, SpecialValues$EPS),
  lower = replicate(5, 0),
  upper = replicate(5, 0),
  scale = replicate(5, 1)
  )
  expect_equal(e$records, df)
}
)

# test symbol isValid if categories are not set properly (directly)
test_that("test_num_62", {
df = data.frame(rev(expand.grid(rev(list(paste0("h", 1:10), 
paste0("m", 1:10),paste0("s", 1:10))))), stringsAsFactors = TRUE)

colnames(df) = c("h_1", "m_2", "s_3")

df[["value"]] = runif(nrow(df), 0, 100)

m = Container$new()
hrs = Set$new(m, "h", records = unique(df$h_1))
mins = Set$new(m, "m", records = unique(df$m_2))
secs = Set$new(m, "s", records = unique(df$s_3))

a = Parameter$new(m, "a", c(hrs, mins, secs))

# set records directly
a$records = df

expect_true(a$isValid())
}
)

# test symbol isValid if categories are set properly (directly)
test_that("test_num_63", {
df = data.frame(rev(expand.grid(rev(list(paste0("h", 1:10), 
paste0("m", 1:10),paste0("s", 1:10))))), stringsAsFactors = TRUE)

colnames(df) = c("h_1", "m_2", "s_3")

df[["value"]] = runif(nrow(df), 0, 100)

m = Container$new()
hrs = Set$new(m, "h", records = unique(df$h_1))
mins = Set$new(m, "m", records = unique(df$m_2))
secs = Set$new(m, "s", records = unique(df$s_3))

a = Parameter$new(m, "a", c(hrs, mins, secs))

df$h_1 = factor(df$h_1, levels=levels(hrs$records$uni), ordered = TRUE)
df$m_2 = factor(df$m_2, levels=levels(mins$records$uni), ordered = TRUE)
df$s_3 = factor(df$s_3, levels=levels(secs$records$uni), ordered = TRUE)
# set records directly
a$records = df

expect_true(a$isValid())
}
)

# test symbol isValid if categories are not linked to the proper set (directly)
test_that("test_num_64", {
df = data.frame(rev(expand.grid(rev(list(paste0("h", 1:10), 
paste0("m", 1:10),paste0("s", 1:10))))), stringsAsFactors = TRUE)

colnames(df) = c("h_1", "m_2", "s_3")

df[["value"]] = runif(nrow(df), 0, 100)

m = Container$new()
hrs = Set$new(m, "h", records = unique(df$h_1))
mins = Set$new(m, "m", records = unique(df$m_2))
secs = Set$new(m, "s", records = unique(df$s_3))

a = Parameter$new(m, "a", c(hrs, mins, secs))

dumb_set = append(levels(df$h_1), "new_element")

df$h_1 = factor(df$h_1, levels=dumb_set, ordered = TRUE)
df$m_2 = factor(df$m_2, levels=levels(mins$records$uni), ordered = TRUE)
df$s_3 = factor(df$s_3, levels=levels(secs$records$uni), ordered = TRUE)
# set records directly
a$records = df

expect_true(a$isValid())
}
)

# test Exception when attempting to set a symbol name with invalid characters
test_that("test_num_65", {
m = Container$new()
expect_error(Set$new(m, "milk&meat"))
expect_error(Set$new(m, "_milk&meat"))
}
)

# test that name.setter (symbols and aliases) cannot set name if it already exists in container
test_that("test_num_66", {
m = Container$new()
i = Set$new(m, "i")
j = Set$new(m, "j")

ip = Alias$new(m, "ip", i)
jp = Alias$new(m, "jp", j)
expr <- function() j$name = "i"
expect_error(expr())
expr <- function() jp$name = "ip"
expect_error(expr())
}
)

# test utility functions
test_that("test_num_67", {
m = Container$new()
i = Set$new(m, "i", records = paste0("i", 1:10))
a = Parameter$new(m, "a", i, records=data.frame(i$records[,1], 1:10))
expect_equal(a$getMaxValue(), 10.0)
expect_equal(a$getMinValue(), 1.0)
expect_equal(a$getMeanValue(), 5.5)
expect_equal(a$getMaxAbsValue(), 10.0)
expect_equal(a$whereMax(), 10)
expect_equal(a$whereMaxAbs(), 10)
expect_equal(a$whereMin(), 1)
expect_equal(a$countNA(), 0)
expect_equal(a$countEps(), 0)
expect_equal(a$countUndef(), 0)
expect_equal(a$countPosInf(), 0)
expect_equal(a$countNegInf(), 0)
}
)

# test passing .toDense() of multiple dimensions to setRecords
test_that("test_num_68", {
m = Container$new()
i = Set$new(m, "i", records = paste0("i", 1:10))
j = Set$new(m, "j", records = paste0("j", 1:10))

a0 = Parameter$new(m, "a0", records=10)
a1 = Parameter$new(m, "a1", i, records=data.frame(i=paste0("i", 1:10), 1:10))


df = data.frame(rev(expand.grid(rev(list(paste0("i", 1:10), 
paste0("j", 1:5))))), stringsAsFactors = TRUE)
values = replicate(50, 0)
count = 1
for (i1 in 1:10) {
  for (j1 in 1:5) {
    values[count] = i1 + j1
    count = count + 1
  }
}
df$values = values
a2 = Parameter$new(m, "a2", c(i, j), records=df)


df = data.frame(rev(expand.grid(rev(list(paste0("i", 1:10), 
paste0("j", 1:5), paste0("i", 1:10))))), stringsAsFactors = TRUE)

values = replicate(500, 0)
count = 1
for (i1 in 1:10) {
  for (j1 in 1:5) {
    for (ip1 in 1:10) {
    values[count] = i1 + j1 + ip1
    count = count + 1
    }
  }
}
df$values = values

a3 = Parameter$new(m, "a3", c(i, j, i), records=df)

a0p = Parameter$new(m, "a0p", records=a0$toDense())
a1p = Parameter$new(m, "a1p", i, records = a1$toDense())
a2p = Parameter$new(m, "a2p", c(i, j), records = a2$toDense())
a3p = Parameter$new(m, "a3p", c(i, j, i), records = a3$toDense())

v0 = Variable$new(m, "v0", records = a0$toDense())
v1 = Variable$new(m, "v1", domain=i, records=a1$toDense())
v2 = Variable$new(m, "v2", domain=c(i, j), records= a2$toDense())
v3 = Variable$new(m, "v3", domain=c(i, j, i), records=a3$toDense())

v0p = Variable$new(m, "v0p", records = v0$toDense())
v1p = Variable$new(m, "v1p", domain=i, records=v1$toDense())
v2p = Variable$new(m, "v2p", domain=c(i, j), records= v2$toDense())
v3p = Variable$new(m, "v3p", domain=c(i, j, i), records=v3$toDense())

e0 = Equation$new(m, "e0", "eq", records = a0$toDense())
e1 = Equation$new(m, "e1", "eq", domain=i, records=a1$toDense())
e2 = Equation$new(m, "e2", "eq", domain=c(i, j), records= a2$toDense())
e3 = Equation$new(m, "e3", "eq", domain=c(i, j, i), records=a3$toDense())

e0p = Equation$new(m, "e0p", "eq", records = e0$toDense())
e1p = Equation$new(m, "e1p", "eq", domain=i, records=e1$toDense())
e2p = Equation$new(m, "e2p", "eq", domain=c(i, j), records= e2$toDense())
e3p = Equation$new(m, "e3p", "eq", domain=c(i, j, i), records=e3$toDense())

expect_equal(a0$records, a0p$records)
expect_equal(a1$records, a1p$records)
# expect_equal(as.numeric(a2$records), as.numeric(a2p$records))
# expect_equal(as.numeric(a3$records), as.numeric(a3p$records))

expect_equal(v0$records, v0p$records)
expect_equal(v1$records, v1p$records)
expect_equal(v2$records, v2p$records)
expect_equal(v3$records, v3p$records)

expect_equal(e0$records, e0p$records)
expect_equal(e1$records, e1p$records)
expect_equal(e2$records, e2p$records)
expect_equal(e3$records, e3p$records)}
)

# test passing .toDense() of multiple dimensions to setRecords (named list structure)
test_that("test_num_69", {
m = Container$new()
i = Set$new(m, "i", records = paste0("i", 1:10))
j = Set$new(m, "j", records = paste0("j", 1:10))

a0 = Parameter$new(m, "a0", records=10)
a1 = Parameter$new(m, "a1", i, records=data.frame(i=paste0("i", 1:10), 1:10))


df = data.frame(rev(expand.grid(rev(list(paste0("i", 1:10), 
paste0("j", 1:5))))), stringsAsFactors = TRUE)
values = replicate(50, 0)
count = 1
for (i1 in 1:10) {
  for (j1 in 1:5) {
    values[count] = i1 + j1
    count = count + 1
  }
}
df$values = values
a2 = Parameter$new(m, "a2", c(i, j), records=df)


df = data.frame(rev(expand.grid(rev(list(paste0("i", 1:10), 
paste0("j", 1:5), paste0("i", 1:10))))), stringsAsFactors = TRUE)

values = replicate(500, 0)
count = 1
for (i1 in 1:10) {
  for (j1 in 1:5) {
    for (ip1 in 1:10) {
    values[count] = i1 + j1 + ip1
    count = count + 1
    }
  }
}
df$values = values

a3 = Parameter$new(m, "a3", c(i, j, i), records=df)

a0dict = list(
  level = a0$toDense(),
  marginal = a0$toDense(),
  lower = a0$toDense(),
  upper = a0$toDense(),
  scale = a0$toDense()
)

a1dict = list(
  level = a1$toDense(),
  marginal = a1$toDense(),
  lower = a1$toDense(),
  upper = a1$toDense(),
  scale = a1$toDense()
)

a2dict = list(
  level = a2$toDense(),
  marginal = a2$toDense(),
  lower = a2$toDense(),
  upper = a2$toDense(),
  scale = a2$toDense()
)

a3dict = list(
  level = a3$toDense(),
  marginal = a3$toDense(),
  lower = a3$toDense(),
  upper = a3$toDense(),
  scale = a3$toDense()
)

a0p = Parameter$new(m, "a0p", records=a0$toDense())
a1p = Parameter$new(m, "a1p", i, records = a1$toDense())
a2p = Parameter$new(m, "a2p", c(i, j), records = a2$toDense())
a3p = Parameter$new(m, "a3p", c(i, j, i), records = a3$toDense())

v0 = Variable$new(m, "v0", records = a0dict)
v1 = Variable$new(m, "v1", domain=i, records=a1dict)
v2 = Variable$new(m, "v2", domain=c(i, j), records= a2dict)
v3 = Variable$new(m, "v3", domain=c(i, j, i), records=a3dict)

v0p = Variable$new(m, "v0p", records = a0dict)
v1p = Variable$new(m, "v1p", domain=i, records=a1dict)
v2p = Variable$new(m, "v2p", domain=c(i, j), records= a2dict)
v3p = Variable$new(m, "v3p", domain=c(i, j, i), records=a3dict)

e0 = Equation$new(m, "e0", "eq", records = a0dict)
e1 = Equation$new(m, "e1", "eq", domain=i, records=a1dict)
e2 = Equation$new(m, "e2", "eq", domain=c(i, j), records= a2dict)
e3 = Equation$new(m, "e3", "eq", domain=c(i, j, i), records=a3dict)

e0p = Equation$new(m, "e0p", "eq", records = a0dict)
e1p = Equation$new(m, "e1p", "eq", domain=i, records=a1dict)
e2p = Equation$new(m, "e2p", "eq", domain=c(i, j), records= a2dict)
e3p = Equation$new(m, "e3p", "eq", domain=c(i, j, i), records=a3dict)

expect_equal(a0$records, a0p$records)
expect_equal(a1$records, a1p$records)
# expect_equal(as.numeric(a2$records), as.numeric(a2p$records))
# expect_equal(as.numeric(a3$records), as.numeric(a3p$records))

expect_equal(v0$records, v0p$records)
expect_equal(v1$records, v1p$records)
expect_equal(v2$records, v2p$records)
expect_equal(v3$records, v3p$records)

expect_equal(e0$records, e0p$records)
expect_equal(e1$records, e1p$records)
expect_equal(e2$records, e2p$records)
expect_equal(e3$records, e3p$records)}
)

# shape test by passing eigen values and eigen vectors
test_that("test_num_70", {
  m = Container$new(testthat::test_path("testdata", "test70.gdx"))

  e = eigen(m["a"]$toDense())
  val = e$values
  vec = e$vectors
  eval = Parameter$new(m, "eval", m["i"], records = val)
  evec = Parameter$new(m, "evec", c(m["i"], m["j"]), records = vec)

  expect_true(m$isValid())
}
)

test_that("test_num_71", {
arr0 = array(c(1:270), dim=c(5,6,9))
m = Container$new()
i = Set$new(m, "i", records=paste0("i",1:5))
j = Set$new(m, "j", records=paste0("j",1:6))
k = Set$new(m, "k", records=paste0("i",1:9))

h = Parameter$new(m, "h", c(i, j, k), records = arr0)
hp = Parameter$new(m, "hp", c(i, j, k), records = h$toDense())

expect_equal(h$shape(), c(5,6,9))
expect_equal(hp$shape(), c(5,6,9))
expect_equal(dim(h$toDense()), c(5, 6, 9))
expect_equal(dim(hp$toDense()), c(5, 6, 9))

expect_equal(h$records, hp$records)
expect_equal(arr0, h$toDense())
expect_equal(arr0, hp$toDense())

v = Variable$new(m, "v", "free", c(i, j, k), records=arr0)
vp = Variable$new(m, "vp", "free", c(i, j, k), records=v$toDense())


expect_equal(v$shape(), c(5,6, 9))
expect_equal(vp$shape(), c(5,6, 9))
expect_equal(dim(v$toDense()), c(5,6, 9))
expect_equal(dim(vp$toDense()), c(5,6, 9))
expect_equal(v$records, vp$records)
expect_equal(arr0, v$toDense())
expect_equal(arr0, vp$toDense())

e = Equation$new(m, "e", "eq", c(i, j, k), records = arr0)
ep = Equation$new(m, "ep", "eq", c(i, j, k), records = e$toDense())

expect_equal(e$shape(), c(5,6, 9))
expect_equal(ep$shape(), c(5,6, 9))
expect_equal(dim(e$toDense()), c(5,6, 9))
expect_equal(dim(ep$toDense()), c(5,6, 9))
expect_equal(e$records, ep$records)
expect_equal(arr0, e$toDense())
expect_equal(arr0, ep$toDense())

}
)

test_that("test_num_72", {
  m = Container$new(testthat::test_path("testdata", "test72.gdx"))

  expect_equal(m["dim0"]$shape(), dim(m["dim0"]$toDense()))
  expect_equal(m["dim1"]$shape(), dim(m["dim1"]$toDense()))
  expect_equal(m["dim2"]$shape(), dim(m["dim2"]$toDense()))
  expect_equal(m["dim3"]$shape(), dim(m["dim3"]$toDense()))
}
)

test_that("test_num_73", {
  m = Container$new(testthat::test_path("testdata", "test73.gdx"))
  m["ProfitA"]$setRecords(cumsum(m["ProfitM"]$toDense()))

  expect_equal(m["ProfitA"]$numberRecords, m["t"]$numberRecords)

}
)

test_that("test_num_74", {
m = Container$new()
d = Parameter$new(m, "d")
e = Parameter$new(m, "e")
expect_true(m$hasSymbols("d"))
expect_true(m$hasSymbols("e"))
expect_true(m$hasSymbols("D"))
expect_true(m$hasSymbols("E"))
expect_equal(m$hasSymbols(c("d", "e")), c(TRUE, TRUE))

expect_false(m$hasSymbols("f"))
expect_equal(m$hasSymbols(c("F", "g")), c(FALSE, FALSE))

expect_equal(m$getSymbolNames(c("D", "d")), c("d", "d"))
expect_equal(m$getSymbolNames("D"), "d")
expect_error(m$getSymbolNames("F"))

}
)

# test case sensitive rename and remove
test_that("test_num_75", {
m = Container$new()
d = Parameter$new(m, "d")
e = Parameter$new(m, "e")

# case insensitive remove
m$removeSymbols("D")
expect_equal(unlist(m$data$keys()), "e")

d = Parameter$new(m, "d")

# case insensitive rename
m$renameSymbol("D", "d_new")
expect_equal(unlist(m$data$keys()), c("e","d_new"))

# change case using rename
expect_error(m$renameSymbol("d_new","D_neW"))
expect_equal(tolower(unlist(m$data$keys())), tolower(c("e", "D_neW")))

}
)

#test getsymbols
test_that("test_num_76", {
m = Container$new()
d = Parameter$new(m, "d")
e = Parameter$new(m, "e")

expect_equal(length(m$getSymbols(m$listSymbols())), 2)
symbolobjects = m$getSymbols(m$listSymbols())

expect_true(inherits(symbolobjects[[1]], "Parameter"))
expect_true(inherits(symbolobjects[[2]], "Parameter"))

expect_error(m$getSymbols("f"))

symobject = m$getSymbols("d")
expect_true(inherits(symobject[[1]], "Parameter"))

symobject = m$getSymbols("D")
expect_true(inherits(symobject[[1]], "Parameter"))

expect_error(m$getSymbols(200))

}
)

#test listSymbols
test_that("test_num_76", {

m  = Container$new()
i = Set$new(m, "i", records=c("a", "b", "c"))
a = Parameter$new(m, "a", i, records=data.frame(c("aa", "c"), c(1, 2)))

expect_equal(a$isValid(), TRUE)
# expect_equal(m$listSymbols(isValid=TRUE), "i")
# expect_equal(m$listSymbols(isValid=FALSE), "a")
}
)

#test symbol duplicate record methods
test_that("test_num_77", {
m = Container$new()
df = data.frame(matrix(NA, nrow=30, ncol=2))
df[1:10, 1] = paste0("i", 1:10)
df[11:20, 1] = paste0("j", 1:10)
df[21:30, 1] = paste0("i", 1:10)
df[1:10, 2] = 1:10
df[11:20, 2] = 1:10
df[21:30, 2] = 1:10
a = Parameter$new(m, "a", domain="*", records = df)
b = Parameter$new(m, "b", domain="*", records= data.frame(paste0("i",1:10), 1:10))

# expect_equal(a$isValid(), FALSE)
expect_equal(a$findDuplicateRecords(keep="first"), a$records[21:30, ])
expect_equal(a$findDuplicateRecords(keep="last"), a$records[1:10, ])
expect_equal(a$findDuplicateRecords(keep=FALSE), a$records[append(1:10, 21:30), ])
expect_equal(a$countDuplicateRecords(), 10)
expect_true(a$hasDuplicateRecords())
a$dropDuplicateRecords(keep="first")
expect_equal(as.character(a$records[,1]), append(paste0("i", 1:10), paste0("j", 1:10)))

m$removeSymbols("a")

a = Parameter$new(m, "a", domain="*", records= df)
a$dropDuplicateRecords(keep="last")
expect_equal(as.character(a$records[,1]), append(paste0("j", 1:10), paste0("i", 1:10)))

expect_true(b$isValid())
expect_equal(b$countDuplicateRecords(), 0)
expect_true(!b$hasDuplicateRecords())
bcopy = b
b$dropDuplicateRecords()
expect_equal(bcopy, b)
expect_true(m$isValid())

}
)

#test container duplicate record methods
test_that("test_num_78", {
m = Container$new()
df = data.frame(matrix(NA, nrow=30, ncol=2))
df[1:10, 1] = paste0("i", 1:10)
df[11:20, 1] = paste0("j", 1:10)
df[21:30, 1] = paste0("i", 1:10)
df[1:10, 2] = 1:10
df[11:20, 2] = 1:10
df[21:30, 2] = 1:10
a = Parameter$new(m, "a", "*", records=df)
b = Parameter$new(m, "b", "*", records=data.frame(paste0("i",1:10), 1:10))
c = Parameter$new(m, "c", "*", records=data.frame(paste0("j",1:10), 1:10))

expect_true(m$hasDuplicateRecords())
expect_equal(m$countDuplicateRecords(), list("a"=10))
m$dropDuplicateRecords()
expect_true(m$isValid())
}
)

#test getUELs method
test_that("test_num_79", {
m = Container$new()
i = Set$new(m, "i", domain=c("*", "*"), records=data.frame(c("i1","i2","i3"), c("i4","i5","i6")))
j = Set$new(m, "j", domain=c("*", "*"), records=data.frame(c("j1","j2","j3"), c("j4","j5","j6")))

expect_equal(i$getUELs(dimension=1), c("i1","i2","i3"))
expect_equal(i$getUELs(dimension=2), c("i4","i5","i6"))
expect_equal(i$getUELs(), paste0("i", 1:6))
expect_error(i$getUELs(dimension=3))
expect_error(i$getUELs(codes=0))

expect_equal(i$getUELs(dimension=1), paste0("i", 1:3))
expect_equal(i$getUELs(dimension=2), paste0("i", 4:6))
expect_error(i$getUELs(dimension=3))
expect_error(i$getUELs(codes=0))

expect_equal(i$getUELs(dimension=1, codes=1), "i1")
expect_equal(i$getUELs(dimension=1, codes=3), "i3")
expect_equal(i$getUELs(dimension=1, codes=c(1, 3)), c("i1", "i3"))
expect_error(i$getUELs(codes=100, dimension=0))

expect_equal(j$getUELs(dimension=1), paste0("j", 1:3))
expect_equal(j$getUELs(dimension=2), paste0("j", 4:6))
expect_equal(j$getUELs(), paste0("j", 1:6))
expect_error(j$getUELs(dimension=3))

expect_equal(m$getUELs("i"), i$getUELs())
expect_equal(m$getUELs("j"), j$getUELs())
expect_equal(m$getUELs(), append(i$getUELs(), j$getUELs()))
expect_error(m$getUELs("k"))
}
)

#test removeUELs method
test_that("test_num_80", {
m = Container$new()
i = Set$new(m, "i", domain=c("*", "*"), records=data.frame(c("i1","i2","i3"), c("i4","i5","i6")))
i$removeUELs("hi", 1)
i$removeUELs("hi", 2)
i$removeUELs("hi")

expect_equal(i$getUELs(), paste0("i", 1:6))

i$removeUELs("i1", 1)
expect_equal(i$getUELs(), paste0("i",2:6))

m = Container$new()
i = Set$new(m, "i", domain=c("*", "*"), records=data.frame(c("i1","i2","i3"), c("i4","i5","i6")))
ip = Alias$new(m, "ip", i)
ip$removeUELs("hi", 1)
ip$removeUELs("hi", 2)
ip$removeUELs("hi")

expect_equal(ip$getUELs(), paste0("i", 1:6))

ip$removeUELs("i1", 1)
expect_equal(ip$getUELs(), paste0("i",2:6))

m = Container$new()
i = Set$new(m, "i", domain=c("*", "*"), records=data.frame(c("i1","i2","i3"), c("i4","i5","i6")))
i$removeUELs("i4")
expect_equal(i$getUELs(), append(paste0("i", 1:3), c("i5", "i6")))

m = Container$new()
i = Set$new(m, "i", domain=c("*", "*"), records=data.frame(c("i1","i2","i3"), c("i4","i5","i6")))
ip = Alias$new(m, "ip", i)
ip$removeUELs("i4")
expect_equal(ip$getUELs(), append(paste0("i", 1:3), c("i5", "i6")))

m = Container$new()
i = Set$new(m, "i", domain=c("*", "*"), records=data.frame(c("i1","i2","i3"), c("i4","i5","i6")))
j = Set$new(m, "j", domain=c("*", "*"), records=data.frame(c("j1","j2","j3"), c("j4","j5","j6")))
ip = Alias$new(m, "ip", i)
m$removeUELs(c("i1","j4"))
expect_equal(m$getUELs(), append(paste0("i",2:6), paste0("j",c(1:3,5,6))))
}
)

#test renameUELs method
test_that("test_num_81", {
m = Container$new()
i = Set$new(m, "i", domain=c("*", "*"), records=data.frame(c("i1","i2","i5"), c("i4","i5","i6")))
i$renameUELs(c("jammin"="java"), 1)
i$renameUELs(c("jammin"="java"), 2)
i$renameUELs(c("jammin"="java"))
m$renameUELs(c("jammin"="java"))

i$renameUELs(c(i2="java"))
expect_equal(i$getUELs(1), c("i1", "java", "i5"))

m = Container$new()
i = Set$new(m, "i", domain=c("*", "*"), records=data.frame(c("i1","i2","i5"), c("i4","i5","i6")))
expect_equal(m$getUELs(), c("i1","i2","i5","i4","i6"))
m$renameUELs(c(i2="java"))
expect_equal(m$getUELs(), c("i1","java","i5","i4","i6"))
}
)

#test addUELs method
test_that("test_num_82", {
m = Container$new()
i = Set$new(m, "i", domain=c("*", "*"), records=data.frame(c("i1","i2","i5"), c("i4","i5","i6")))
i$addUELs("ham", 1)
expect_equal(i$getUELs(1), c("i1","i2","i5","ham"))

i$addUELs("cheese", 2)
expect_equal(i$getUELs(2), c("i4","i5","i6","cheese"))

m$removeUELs()
expect_equal(m$getUELs(), c("i1","i2","i5","i4","i6"))
}
)

#test reorderUELs method
test_that("test_num_83", {
m = Container$new()
i = Set$new(m, "i", records=c("i1","i2","i3"))
a = Parameter$new(m, "a",i, records=data.frame(paste0("i",c(1,3,2)), c(1,3,2)))

expect_equal(a$getUELs(1), c("i1","i3","i2"))
a$reorderUELs(i$getUELs(1), 1)
expect_equal(a$getUELs(), c("i1","i2","i3"))
}
)

#test setUELs method
test_that("test_num_84", {
m = Container$new()
i = Set$new(m, "i", records=c("a","b","c"))
j = Parameter$new(m, "j",i, records=data.frame(c("a","c"), c(1,2)))

expect_equal(j$getUELs(1), c("a","c"))

j$setUELs(c("j1","a","c","j4"), 1, rename=TRUE)
expect_equal(j$getUELs(1), c("j1","a","c","j4"))
expect_equal(as.character(j$records[, 1]), c("j1","a"))


m = Container$new()
i = Set$new(m, "i", records=c("a","b","c"))
j = Parameter$new(m, "j",i, records=data.frame(c("a","c"), c(1,2)))

expect_equal(j$getUELs(1), c("a","c"))

j$setUELs(c("j1","a","c","j4"), 1, rename=FALSE)
expect_equal(j$getUELs(1), c("j1","a","c","j4"))
expect_equal(as.character(j$records[, 1]), c("a","c"))

#multiple dimensions
m = Container$new()
i = Set$new(m, "i", records=c("a","b","c"))
j = Parameter$new(m, "j",c(i,i), records=data.frame(c("a","c"),c("b","a"), c(1,2)))
j$setUELs(c("j1","a","c","j4"), rename=FALSE)
expect_equal(j$getUELs(1), c("j1","a","c","j4"))
expect_equal(j$getUELs(2), c("j1","a","c","j4"))

expect_equal(as.character(j$records[, 1]), c("a","c"))
expect_true(is.na(j$records[1, 2]))
expect_equal(as.character(j$records[, 1]), c("a","c"))
}
)

#test getDomainViolations method
test_that("test_num_85", {
m = Container$new()
i = Set$new(m, "i", records=c("SeaTtle", "hamburg"))
j = Set$new(m, "j", i, records=c("seattle", "Hamburg"))
ip = Alias$new(m, "ip", i)
jp = Alias$new(m, "jp", j)

expect_true(i$isValid())
expect_true(j$isValid())
expect_equal(i$getDomainViolations(), NULL)
expect_equal(j$getDomainViolations(), NULL)
expect_equal(ip$getDomainViolations(), NULL)
expect_equal(jp$getDomainViolations(), NULL)

# test container getdomainviolations
expect_true(is.null(m$getDomainViolations()))

m = Container$new()
i = Set$new(m, "i", records=c("SeaTtle", "hamburg"))
j = Set$new(m, "j", i, records=c(" seattle", "Hamburg"))
ip = Alias$new(m, "ip", i)
jp = Alias$new(m, "jp", j)

expect_true(i$isValid())
expect_true(j$isValid())
expect_equal(i$getDomainViolations(), NULL)
expect_equal(j$getDomainViolations()[[1]]$violations, c(" seattle"))
expect_equal(ip$getDomainViolations(), NULL)
expect_equal(jp$getDomainViolations()[[1]]$violations, c(" seattle"))

expect_equal(length(m$getDomainViolations()),  2)
dv = m$getDomainViolations()
expect_equal(dv[[1]]$symbol, j)
expect_equal(dv[[1]]$dimension, 1)
expect_equal(dv[[1]]$domain, i)
expect_equal(dv[[1]]$violations, c(" seattle"))

# finddomainviolations  for alias
df = jp$findDomainViolations()
expect_equal(as.character(df[[1]]), " seattle")

expect_true(jp$hasDomainViolations())
expect_equal(jp$countDomainViolations(), 1)
jp$dropDomainViolations()
expect_equal(as.character(j$records$i), "Hamburg")

}
)

#test findDomainViolations method
test_that("test_num_86", {
m = Container$new()
i = Set$new(m, "i", records=c("j1", "j2"))
a = Parameter$new(m, "a", i, records= data.frame(paste0("j",1:10), 1:10))
a2 = Parameter$new(m, "a2", c(i,i), records= data.frame(paste0("j",1:10),paste0("j",1:10), 1:10))
expect_true(a$isValid())
expect_true(is.data.frame(a$findDomainViolations()))
expect_equal(a$findDomainViolations(), a$records[3:10,])

expect_equal(a$countDomainViolations(), 8)
expect_true(a$hasDomainViolations())
#container hasDomainViolations()
expect_true(m$hasDomainViolations())

#container countDomainViolations()
expect_equal(m$countDomainViolations()$a, 8)
expect_equal(m$countDomainViolations()$a2, 8)

a$dropDomainViolations()
expect_equal(as.character(a$records[,1]), c("j1","j2") )
expect_false(a$hasDomainViolations())
expect_equal(a$countDomainViolations(), 0)
expect_equal(a$findDuplicateRecords(), data.frame())
expect_true(a$isValid())

# test dropdomainviolations for container
m = Container$new()
i = Set$new(m, "i", records=c("j1", "j2"))
a = Parameter$new(m, "a", i, records= data.frame(paste0("j",1:10), 1:10))
a2 = Parameter$new(m, "a2", c(i,i), records= data.frame(paste0("j",1:10),paste0("j",1:10), 1:10))

m$dropDomainViolations()
expect_equal(as.character(a$records[,1]), c("j1","j2") )
expect_false(m$hasDomainViolations())
empty_named_list = list()
names(empty_named_list) = character(0)
expect_equal(m$countDomainViolations(), empty_named_list)
}
)


#test overwriting sets
test_that("test_num_87", {
m = Container$new()
i = Set$new(m, "i", records=c("a", "b", "c"))

expect_true(inherits(m$addSet("i", records=c("f","b","c")), "Set"))
expect_error(m$addSet("i","a",records=c("f","b","c")))
expect_true(inherits(m$addSet("i",records=c("f","b","c"), description="hamburger"), "Set"))

expect_equal(m["i"]$description, "hamburger")
expect_true(inherits(m$addSet("i",records=c("f","b","c")), "Set"))
expect_equal(m["i"]$description, "hamburger")

expect_error(Set$new(m, "i", records=c("f","b","c")))
}
)

#test overwriting Parameters
test_that("test_num_88", {
m = Container$new()
i = Parameter$new(m, "i", "*", records=data.frame(paste0("i",1:5), 1:5))

expect_true(inherits(m$addParameter("i", "*", records=data.frame(paste0("i",1:5), 1:5)), "Parameter"))
expect_error(m$addParameter("i","a",records=data.frame(paste0("i",1:5), 1:5)))
expect_true(inherits(m$addParameter("i","*",records=data.frame(paste0("i",1:5), 1:5), description="hamburger"), "Parameter"))

expect_equal(m["i"]$description, "hamburger")
expect_true(inherits(m$addParameter("i","*",records=data.frame(paste0("i",1:5), 1:5)), "Parameter"))
expect_equal(m["i"]$description, "hamburger")

expect_error(Parameter$new(m, "i", records=data.frame(paste0("i",1:5), 1:5)))

# overwriting parameter with domain
m = Container$new()
i = Set$new(m, "i", records=paste0("i",1:5))
j = Set$new(m, "j", records=paste0("j",1:5))
d = Parameter$new(m, "d", domain=c(i, j), records=diag(5))

# test filtering zeros
expect_equal(as.character(d$records$i), paste0("i",1:5))
expect_equal(as.character(d$records$j), paste0("j",1:5))
expect_equal(as.numeric(d$records$value),replicate(5, 1))

# overwriting
m$addParameter("d", , domain=c(i, j), records=matrix(0, 5, 5))
expect_true(is.null(d$records))
}
)

#test overwriting Variables
test_that("test_num_89", {
m = Container$new()
recs= data.frame(paste0("i",1:5), 1:5)
colnames(recs) = c("1", "level")
i = Variable$new(m, "i", "free", "*", records=recs)

expect_true(inherits(m$addVariable("i", "free", "*", records=recs), "Variable"))
expect_error(m$addVariable("i", "free","a",records=recs))
expect_true(inherits(m$addVariable("i", "free","*",records=recs, description="hamburger"), "Variable"))

expect_equal(m["i"]$description, "hamburger")
expect_true(inherits(m$addVariable("i", "free","*",records=recs), "Variable"))
expect_equal(m["i"]$description, "hamburger")

expect_error(Variable$new(m, "i", "free", records=recs))
}
)

#test overwriting Equations
test_that("test_num_90", {
m = Container$new()
recs= data.frame(paste0("i",1:5), 1:5)
colnames(recs) = c("1", "level")
i = Equation$new(m, "i", "eq", "*", records=recs)

expect_true(inherits(m$addEquation("i", "eq", "*", records=recs), "Equation"))
expect_error(m$addEquation("i", "eq","a",records=recs))
expect_true(inherits(m$addEquation("i", "eq","*",records=recs, description="hamburger"), "Equation"))

expect_equal(m["i"]$description, "hamburger")
expect_true(inherits(m$addEquation("i", "eq","*",records=recs), "Equation"))
expect_equal(m["i"]$description, "hamburger")

expect_error(Equation$new(m, "i", "eq", records=recs))
}
)

#test overwriting Alias
test_that("test_num_91", {
m = Container$new()
i = Set$new(m, "i", records=c("a","b","c"))
j = Set$new(m, "j", records=c("i","j","k"))
ip = m$addAlias("ip", i)

expect_true(inherits(ip, "Alias"))
expect_equal(ip$aliasWith, i)
expect_error(m$addAlias("j", i))

ip = m$addAlias("ip", j)
expect_true(inherits(ip, "Alias"))
expect_equal(ip$aliasWith, j)

}
)

#change column headings when changing domain
test_that("test_num_92", {
m = Container$new()
i = Set$new(m, "i", records=paste0("i",1:5))

a = Parameter$new(m, "a", c(i,i), records=data.frame(p_1=paste0("i",1:5), p_2=paste0("i",1:5), 1:5))
a$domain = c("p","p")
expect_equal(a$domain, c("p","p"))
expect_true(a$isValid())
expect_equal(colnames(a$records), c("p_1","p_2","value"))

m = Container$new()
i = Set$new(m, "i", records=paste0("i",1:5))
ip = Alias$new(m, "ip", i)

a = Parameter$new(m, "a", c(i,ip), records=data.frame(p_1=paste0("i",1:5), p_2=paste0("i",1:5), 1:5))
a$domain = c("p","p")
expect_equal(a$domain, c("p","p"))
expect_true(a$isValid())
expect_equal(colnames(a$records), c("p_1","p_2","value"))

m = Container$new()
i = Set$new(m, "i", records=paste0("i",1:5))
r = Set$new(m, "r", records=paste0("r",1:5))

a = Parameter$new(m, "a", c(i,i), records=data.frame(r_1=paste0("i",1:5), r_2=paste0("i",1:5), 1:5))
a$domain = c(r,r)
expect_equal(a$domain, c(r,r))
expect_true(a$isValid())
expect_equal(colnames(a$records), c("r_1","r_2","value"))
}
)

# white space removal and user category survival
test_that("test_num_93", {
recs = data.frame(uni=paste0("i",1:3), stringsAsFactors = TRUE)
levels(recs[,1]) = append(levels(recs[,1]), "j1")
expect_equal(levels(recs[,1]), c("i1","i2","i3","j1"))

m = Container$new()
i = Set$new(m, "i", records=recs)

expect_equal(levels(i$records$uni), c("i1","i2","i3","j1"))

recs= data.frame(uni=paste0("i",1:3), value=1:3, stringsAsFactors = TRUE)
levels(recs[,1]) = append(levels(recs[,1]), "j1")
expect_equal(levels(recs[,1]), c("i1","i2","i3","j1"))

a = Parameter$new(m, "a", i, records=recs)
expect_equal(levels(a$records$uni), c("i1","i2","i3","j1"))

recs= data.frame(uni=paste0("i",1:3), level=1:3, stringsAsFactors = TRUE)
levels(recs[,1]) = append(levels(recs[,1]), "j1")
expect_equal(levels(recs[,1]), c("i1","i2","i3","j1"))

v = Variable$new(m, "v", "free", i, records=recs)
expect_equal(levels(v$records$uni), c("i1","i2","i3","j1"))

e = Equation$new(m, "e", "eq", i, records=recs)
expect_equal(levels(e$records$uni), c("i1","i2","i3","j1"))

}
)

test_that("test_num_94", {
m = Container$new()
i = Set$new(m, "i", records=c(" i1 ", "i2", "i3"))
expect_equal(i$getUELs(), c(" i1", "i2", "i3"))
i$setUELs(c(" i1 ", " i2 ", "i3"))
expect_equal(i$getUELs(), c(" i1", " i2", "i3"))

m = Container$new()
i = Set$new(m, "i", records=c(" i1 ", "i2", "i3"))
ip = Alias$new(m, "ip", i)
expect_equal(ip$getUELs(), c(" i1", "i2", "i3"))
ip$setUELs(c(" i1 ", " i2 ", "i3"))
expect_equal(i$getUELs(), c(" i1", " i2", "i3"))
expect_equal(i$getUELs(), ip$getUELs())

m = Container$new()
i = Set$new(m, "i", records=c(" i1 ", "i2", "i3"))
i$reorderUELs(c("i2", " i1", "i3"))
expect_equal(i$getUELs(), c("i2", " i1", "i3"))

m = Container$new()
i = Set$new(m, "i", records=c(" i1 ", "i2", "i3"))
ip = Alias$new(m, "ip", i)
ip$reorderUELs(c("i2", " i1", "i3"))
expect_equal(ip$getUELs(), c("i2", " i1", "i3"))
expect_equal(i$getUELs(), ip$getUELs())

m = Container$new()
i = Set$new(m, "i", records=c(" i1 ", "i2", "i3"))
i$renameUELs(c("i1   ", "  i2   ", " i3   "))
expect_equal(i$getUELs(), c("i1", "  i2", " i3"))

m$renameUELs(c("i1"="cheeseburgerz "))
expect_equal(i$getUELs(), c("cheeseburgerz", "  i2", " i3"))

m = Container$new()
i = Set$new(m, "i", records=c(" i1 ", "i2", "i3"))
ip = Alias$new(m, "ip", i)
ip$renameUELs(c("i1   ", "  i2   ", " i3   "))
expect_equal(ip$getUELs(), c("i1", "  i2", " i3"))
expect_equal(i$getUELs(), ip$getUELs())

m = Container$new()
i = Set$new(m, "i", records=c(" i1 ", "i2", "i3"))
i$addUELs("i4   ")
expect_equal(i$getUELs(), c(" i1", "i2", "i3", "i4"))
m$renameUELs(c("i1"="cheeseburgerz "))
expect_equal(i$getUELs(), c(" i1", "i2", "i3", "i4"))

m = Container$new()
i = Set$new(m, "i", records=c(" i1 ", "i2", "i3"))
ip = Alias$new(m, "ip", i)
ip$addUELs("i4   ")
expect_equal(ip$getUELs(), c(" i1", "i2", "i3", "i4"))
m$renameUELs(c("i1"="cheeseburgerz "))
expect_equal(i$getUELs(), ip$getUELs())
}
)

# test reading and writing UniverseAlias with Container
test_that("test_num_95", {
  m = Container$new()

  # read all symbols
  m$read(testthat::test_path("testdata", "test95.gdx"))

  # write everything
  m$write(testthat::test_path("gt.gdx"))

  expect_true(inherits(m["h"], "UniverseAlias"))
  expect_true(inherits(m["ip"], "Alias"))
  expect_true(inherits(m["i"], "Set"))

  expect_equal(m$listSets(), "i")
  expect_equal(m$listAliases(), c("ip", "h"))

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("testdata", "test95.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)
  expect_equal(ret, 0)

}
)

# test reading and writing UniverseAlias with ConstContainer
test_that("test_num_96", {
  m = ConstContainer$new()

  # read all symbols
  # uses the same gdx from test95
  m$read(testthat::test_path("testdata", "test95.gdx"))

  m2 = Container$new(m)
  # write everything
  m2$write(testthat::test_path("gt.gdx"))

  expect_true(inherits(m2["h"], "UniverseAlias"))
  expect_true(inherits(m2["ip"], "Alias"))
  expect_true(inherits(m2["i"], "Set"))

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("testdata", "test95.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)
  expect_equal(ret, 0)

}
)

# UniverseAlias UEL functions
test_that("test_num_97", {

  m = Container$new()
  i = Set$new(m, "i", records=paste0("i", 1:5))
  ip = UniverseAlias$new(m, "ip")
  p0 = Parameter$new(m, "p0", ip, records=data.frame(paste0("i",1:5), 1:5))

  expect_equal(m$getUELs(), paste0("i",1:5))
  m$renameUELs(c("i1" = "cheeseburgerz"))
  expect_equal(m$getUELs(), c("cheeseburgerz", "i2","i3","i4", "i5"))

  p0$addUELs("mustard")
  expect_equal(p0$getUELs(), c("cheeseburgerz", "i2","i3","i4", "i5", "mustard"))

  m$removeUELs()

  expect_equal(m$getUELs(), c("cheeseburgerz", "i2","i3","i4", "i5"))

  m$renameUELs(c("i2 " = "cheeseburgerz"))
  expect_equal(m$getUELs(), c("cheeseburgerz", "i2","i3","i4", "i5"))
}
)

# UniverseAlias as domain
test_that("test_num_98", {
  m = Container$new()
  i = Set$new(m, "i", records=paste0("i", 1:5))
  ip = UniverseAlias$new(m, "ip")
  p0 = Parameter$new(m, "p0", ip, records=data.frame(paste0("i",1:5), 1:5))

  expect_true(p0$isValid())
  expect_equal(p0$domain[[1]], ip)
  expect_equal(p0$domainType, "regular")
  expect_equal(p0$dimension, 1)
  expect_true(is.null(p0$getDomainViolations()))
  expect_equal(nrow(p0$findDomainViolations()), 0)
  expect_true(!p0$hasDomainViolations())
  expect_equal(p0$countDomainViolations(), 0)
  expect_true(!p0$hasDuplicateRecords())
  expect_equal(p0$findDuplicateRecords(), data.frame())
  expect_equal(p0$countDuplicateRecords(), 0)
  expect_equal(p0$summary, 
  list(name="p0", isScalar=FALSE, domainObjects = list(ip), domainNames = "ip",
  dimension=1, description="", numberRecords=5, domainType="regular"))

  m$removeSymbols("ip")
  expect_true(p0$isValid())
  expect_equal(p0$domain[[1]], "*")
  expect_equal(p0$domainType, "none")
  expect_equal(p0$dimension, 1)
  expect_true(is.null(p0$getDomainViolations()))
  expect_true(!p0$hasDomainViolations())
  expect_equal(nrow(p0$findDomainViolations()), 0)
  expect_equal(p0$countDomainViolations(), 0)
  expect_true(!p0$hasDuplicateRecords())
  expect_equal(p0$findDuplicateRecords(), data.frame())
  expect_equal(p0$countDuplicateRecords(), 0)
  expect_equal(p0$summary, 
  list(name="p0", isScalar=FALSE, domainObjects = "*", domainNames = "*",
  dimension=1, description="", numberRecords=5, domainType="none"))

  expect_true(!ip$isValid())
  expect_equal(ip$summary,
  list("name" = "ip",
  aliasWith_name = "*",
  domainNames = "*",
  dimension = 1,
  description = "Aliased with *",
  numberRecords = NA,
  domainType = "none"))
}
)

# remove symbols including alias
test_that("test_num_99", {
m = Container$new()
i = Set$new(m, "i")
ii = Alias$new(m, "ii", i)
j = Set$new(m, "j", domain=ii)

m$removeSymbols("ii")
# should affect j
expect_equal(j$domain, "*")

ii = Alias$new(m, "ii", i)
m$removeSymbols("i")

expect_true(is.null(m["ii"]))
expect_equal(j$domain, "*")
}
)

# GDX read errors
test_that("test_num_100", {
  m = Container$new()
  i = Set$new(m, "i")

  # read all symbols: expect error because i already exists
  expect_error(m$read(testthat::test_path("testdata","test95.gdx")))

  m2 = ConstContainer$new()
  m2$read(testthat::test_path("testdata","test95.gdx"))

  # read all symbols: expect error because i already exists
  expect_error(m$read(m2))

  #read constcontainer into container
  m = Container$new(m2)
  expect_equal(m$listSymbols(), m2$listSymbols())

  # expect error on reading only alias
  m = Container$new()
  expect_error(m$read(testthat::test_path("testdata","test95.gdx"), symbols="ip"))

  # expect error if user wants to read a symbol from another container but it doesn't exist
  m = Container$new()
  expect_error(m$read(m2, symols="j"))

  # expect error on reading only alias
  m2 = Container$new()
  expect_error(m2$read(testthat::test_path("testdata","test95.gdx"), symols="ip"))
}
)

# alias duplicaterecords
test_that("test_num_101", {
m = Container$new()
i = Set$new(m, "i", records=replicate(5, "i1"))
ip = Alias$new(m, "ip", i)

expect_equal(ip$countDuplicateRecords(), 4)
expect_equal(ip$findDuplicateRecords(), ip$records[2:5, ])
expect_true(ip$hasDuplicateRecords())
ip$dropDuplicateRecords()
expect_equal(nrow(i$records), 1)
}
)

#summary test
test_that("test_num_102", {
  m = Container$new()
  expect_error(m$read(testthat::test_path("testdata", "trnsport_with_alias.gdx"), records="true"))

  m = Container$new()
  m$read(testthat::test_path("testdata", "trnsport_with_alias.gdx"))
  expect_equal(m["i"]$summary, list(
    name = "i",
    isSingleton = FALSE,
    domainObjects = "*",
    domainNames = "*",
    dimension = 1,
    description = "canning plants",
    numberRecords = 2,
    domainType = "none"
  ))

  expect_equal(m["d"]$summary, list(
    name = "d",
    isScalar = FALSE,
    domainObjects = c(m["i"], m["j"]),
    domainNames = c("i","j"),
    dimension = 2,
    description = "distance in thousands of miles",
    numberRecords = 6,
    domainType = "regular"
  ))

  expect_equal(m["x"]$summary, list(
    name = "x",
    type = "positive",
    domainObjects = c(m["i"], m["j"]),
    domainNames = c("i","j"),
    dimension = 2,
    description = "shipment quantities in cases",
    numberRecords = 6,
    domainType = "regular"
  ))

  expect_equal(m["demand"]$summary, list(
    name = "demand",
    type = "geq",
    domainObjects = list(m["j"]),
    domainNames = "j",
    dimension = 1,
    description = "satisfy demand at market j",
    numberRecords = 3,
    domainType = "regular"
  ))

  expect_equal(m["ip"]$summary, list(
    name = "ip",
    aliasWith = m["i"],
    aliasWith_name = "i",
    isSingleton = FALSE,
    domainObjects = "*",
    domainNames = "*",
    dimension = 1,
    description = "canning plants",
    numberRecords = 2,
    domainType = "none"
  ))

  # constcontainer
  m = ConstContainer$new()
  expect_error(m$read(testthat::test_path("testdata", "trnsport_with_alias.gdx"), records="true"))

  m = ConstContainer$new()
  m$read(testthat::test_path("testdata", "trnsport_with_alias.gdx"))
  expect_equal(m["i"]$summary, list(
    name = "i",
    isSingleton = FALSE,
    domainNames = "*",
    dimension = 1,
    description = "canning plants",
    numberRecords = 2,
    domainType = "none"
  ))

  expect_equal(m["d"]$summary, list(
    name = "d",
    isScalar = FALSE,
    domainNames = c("i","j"),
    dimension = 2,
    description = "distance in thousands of miles",
    numberRecords = 6,
    domainType = "regular"
  ))

  expect_equal(m["x"]$summary, list(
    name = "x",
    type = "positive",
    domainNames = c("i","j"),
    dimension = 2,
    description = "shipment quantities in cases",
    numberRecords = 6,
    domainType = "regular"
  ))

  expect_equal(m["demand"]$summary, list(
    name = "demand",
    type = "geq",
    domainNames = "j",
    dimension = 1,
    description = "satisfy demand at market j",
    numberRecords = 3,
    domainType = "regular"
  ))

  expect_equal(m["ip"]$summary, list(
    name = "ip",
    aliasWith_name = "i",
    isSingleton = FALSE,
    domainNames = "*",
    dimension = 1,
    description = "canning plants",
    numberRecords = 2,
    domainType = "none"
  ))
}
)

#test partial write
test_that("test_num_103", {
m = Container$new(testthat::test_path("testdata", "trnsport.gdx"))
m$write("partial_write.gdx", symbols="a")

m1 = Container$new("partial_write.gdx")
expect_equal(m1["a"]$domain, "i")
expect_equal(m1["a"]$domainType, "relaxed")
}
)

#test symbols argument container methods
test_that("test_num_104", {
m = Container$new()
i = Set$new(m, "i", records=paste0("i", 1:10))
j = Set$new(m, "j", records=paste0("j", 1:5))
p_dv = Parameter$new(m, "p_dv", domain=c(i, j), records = data.frame(c("i1","i12","i0","i5"), c("j2", "j6", "j1", "j0"), 11:14))
p_dup = Parameter$new(m, "p_dup", domain=c(i, j), records = data.frame(c("i1","i1","i3","i5"), c("j2", "j2", "j1", "j2"), c(11, 13, 12, 14)))
dv = m$getDomainViolations(symbols="p_dv")
expect_equal(length(dv), 2)

expect_true(m$hasDomainViolations())
expect_true(!m$hasDomainViolations(symbols="p_dup"))
expect_true(m$hasDomainViolations(symbols="p_dv"))

expect_true(m$hasDuplicateRecords())
expect_true(!m$hasDuplicateRecords(symbols="p_dv"))
expect_true(m$hasDuplicateRecords(symbols="p_dup"))

m$dropDomainViolations(symbols="p_dup") # should do nothing
expect_true(m$hasDomainViolations())
dv = m$getDomainViolations(symbols="p_dv")
expect_equal(length(dv), 2)

m$dropDuplicateRecords(symbols="p_dv") # should do nothing
dups = m$countDuplicateRecords(symbols="p_dup")
expect_equal(dups, list(p_dup=1))

m$dropDomainViolations(symbols="p_dv")
expect_true(!m$hasDomainViolations())
dv = m$getDomainViolations(symbols="p_dv")
expect_true(is.null(dv))

m$dropDuplicateRecords(symbols="p_dv") # should do nothing
dups = m$countDuplicateRecords(symbols="p_dup")
expect_equal(dups, list(p_dup=1))
}
)

# test equals
test_that("test_num_105", {
# set comparison
m = Container$new()
i = Set$new(m, "i")
j = Set$new(m, "j")
expect_false(i$equals(j))
expect_true(i$equals(j, checkMetaData = FALSE))
expect_error(i$equals(j, checkMetaData = FALSE, verbose=10))

# error because of domain type mismatch
m = Container$new()
i = Set$new(m, "i")
j = Set$new(m, "j", i)
expect_false(i$equals(j, checkMetaData = FALSE))
expect_error(v1$equals(v2, checkMetaData = FALSE, checkElementText=6))

# error because of validity
j$records = data.frame(i=c("j1","j2"), j= c("k1","k2"), k = c("l1","l2"))
expect_error(i$equals(j, checkMetaData = FALSE))
expect_error(j$equals(i, checkMetaData = FALSE))

# other type error
expect_error(i$equals(2))

#columns arg
p1 = Parameter$new(m, "p1")
p2 = Parameter$new(m, "p2")

v1 = Variable$new(m, "v1")
v2 = Variable$new(m, "v2")

expect_error(v1$equals(v2, checkUELs=2, checkMetaData=FALSE))
expect_error(v1$equals(v2, checkMetaData=6))

expect_error(v1$equals(v2, checkMetaData=FALSE, atol=TRUE))
expect_error(v1$equals(v2, checkMetaData=FALSE, rtol=TRUE))

expect_error(v1$equals(v2, checkMetaData=FALSE, atol= c(1, 2)))

expect_error(v1$equals(v2, columns=c("blah1", "blah2"), checkMetaData=FALSE, rtol= 0))

m = Container$new()
i = Set$new(m, "i", records=c("i1","i2"))
j = Set$new(m, "j", records=c("j1","j2"))

e0 = Equation$new(m, "e0", type = "eq", description="empty eq from m")
e1 = Equation$new(m, "e1", type = "eq", domain=c(i, j))
e2 = Equation$new(m, "e2", type = "eq", domain= i)
e2a = Equation$new(m, "e2a", type = "eq", domain= i, records=data.frame(i="i1", level=1))
e2b = Equation$new(m, "e2b", type = "eq", domain= i)
e3 = Equation$new(m, "e3", type = "eq", domain = j)

expect_false(e2$equals(e3))
expect_false(e2$equals(e2a))
expect_false(e2$equals(e2b))
expect_true(e2$equals(e2b, checkMetaData=FALSE))

m2 = Container$new()
e02 = Equation$new(m2, "e0", type="eq", description="empty eq from m2")
expect_true(e02$equals(e0, checkMetaData=FALSE))
expect_false(e02$equals(e0, checkMetaData=TRUE))

m2$removeSymbols("e0")
v02 = Variable$new(m2, "e0", description="empty var from m2")
expect_true(v02$equals(e0, checkMetaData=FALSE))
expect_false(v02$equals(e0, checkMetaData=TRUE))

m = Container$new()
i = Set$new(m, "i", records=c("i1","i2"))
i$setUELs(c("i1","i2","i3","i4"))

m2 = Container$new()
i2 = Set$new(m2, "i", records=c("i1","i2"))
expect_false(i$equals(i2, checkUELs=TRUE))
expect_true(i$equals(i2, checkUELs=FALSE))

# element text
m = Container$new()
i = Set$new(m, "i", records=data.frame(c("i1","i2")))

m2 = Container$new()
i2 = Set$new(m2, "i", records=data.frame(c("i1","i2"), c("first elemnt", "second element")))

expect_true(i$equals(i2, checkElementText=FALSE))
expect_false(i$equals(i2))

# compare records
m = Container$new()
i1 = Set$new(m, "i1", records=data.frame(c("i1","i2")))
i2 = Set$new(m, "i2", records=data.frame(c("i2","i3")))
expect_false(i1$equals(i2, checkMetaData=FALSE))

#parameter records comparison
m = Container$new()
p1 = Parameter$new(m, "p1", records=SpecialValues[["NA"]])
p2 = Parameter$new(m, "p2", records=SpecialValues[["NA"]])
p3 = Parameter$new(m, "p3", records=SpecialValues[["EPS"]])
expect_true(p1$equals(p2, checkMetaData=FALSE))
expect_false(p1$equals(p3, checkMetaData=FALSE))

m = Container$new()
p1 = Parameter$new(m, "p1", records=SpecialValues[["EPS"]])
p2 = Parameter$new(m, "p2", records=SpecialValues[["EPS"]])
p3 = Parameter$new(m, "p3", records=SpecialValues[["UNDEF"]])
expect_true(p1$equals(p2, checkMetaData=FALSE))
expect_false(p1$equals(p3, checkMetaData=FALSE))

m = Container$new()
p1 = Parameter$new(m, "p1", records=SpecialValues[["UNDEF"]])
p2 = Parameter$new(m, "p2", records=SpecialValues[["UNDEF"]])
p3 = Parameter$new(m, "p3", records=SpecialValues[["NA"]])
expect_true(p1$equals(p2, checkMetaData=FALSE))
expect_false(p1$equals(p3, checkMetaData=FALSE))

#scalar
m = Container$new()
p1 = Parameter$new(m, "p1", records=10)
p2 = Parameter$new(m, "p2", records=10)
p3 = Parameter$new(m, "p3", records=5)
expect_true(p1$equals(p2, checkMetaData=FALSE))
expect_false(p1$equals(p3, checkMetaData=FALSE))

# 1D param
m = Container$new()
i = Set$new(m, "i", records=c("i1","i2","i3"))
p1 = Parameter$new(m, "p1", domain=i, records=data.frame(c("i1","i2","i3"), c(2,4,5)))
p2 = Parameter$new(m, "p2", domain=i, records=data.frame(c("i1","i2","i3"), c(3,5,5)))
expect_false(p1$equals(p2, checkMetaData=FALSE))

# 2D param with special values
m = Container$new()
i = Set$new(m, "i", records=c("i1","i2","i3"))
j = Set$new(m, "j", records=c("j1","j2","j3"))
p1 = Parameter$new(m, "p1", domain=c(i, j), records=data.frame(c("i1","i2","i3"),c("j1","j2","j3"), c(SpecialValues[["NA"]],4,SpecialValues$UNDEF)))
p2 = Parameter$new(m, "p2", domain=c(i, j), records=data.frame(c("i1","i2","i3"),c("j1","j2","j3"), c(SpecialValues$UNDEF,4,SpecialValues[["NA"]])))
p3 = Parameter$new(m, "p3", domain=c(i, j), records=data.frame(c("i1","i2","i3"),c("j1","j2","j3"), c(SpecialValues[["NA"]],5,SpecialValues$UNDEF)))

expect_false(p1$equals(p2, checkMetaData=FALSE))
expect_false(p1$equals(p3, checkMetaData=FALSE))

# variable
m = Container$new()
v0 = Variable$new(m, "v0", records=data.frame(level=3))
v0p = Variable$new(m, "v0p", records=data.frame(level=2.99))
expect_true(v0$equals(v0p, checkMetaData=FALSE, atol=0.1))
expect_true(v0$equals(v0p, columns="level",checkMetaData=FALSE, atol=0.1))
expect_true(v0$equals(v0p, checkMetaData=FALSE, atol=0.1))
expect_true(v0$equals(v0p,columns=c("level", "marginal"), checkMetaData=FALSE, atol=0.1))
expect_false(v0$equals(v0p, columns=c("level", "marginal"), checkMetaData=FALSE, atol=0))
expect_error(v0$equals(v0p, checkMetaData=FALSE, atol=c(level=0, scale=0.1), rtol=c(marginal=0, scale=0.1)))
expect_error(v0$equals(v0p, checkMetaData=FALSE, atol=c(level=0, scale=0.1), verbose=TRUE))

m = Container$new()
i = Set$new(m, "i", records=paste0("i",1:5))
v1 = Variable$new(m, "v1", domain=i, records=data.frame(c("i1","i4","i5"), level=c(3, 4, 5)))
v1p = Variable$new(m, "v1p",domain=i, records=data.frame(c("i1","i4","i5"), level=c(2.95, 4.03, 5.09)))
expect_true(v1$equals(v1p, columns="level", checkMetaData=FALSE, atol=0.1))
expect_true(v1$equals(v1p, checkMetaData=FALSE, atol=0.1))
expect_false(v1$equals(v1p, columns=c("level", "marginal"), checkMetaData=FALSE, atol=0))
expect_error(v1$equals(v1p, checkMetaData=FALSE, atol=c(level=0, scale=0.1), rtol=c(marginal=0, scale=0.1)))
expect_error(v1$equals(v1p, checkMetaData=FALSE, atol=c(level=0, scale=0.1), verbose=TRUE))

m = Container$new()
i = Set$new(m, "i", records=paste0("i",1:5))
j = Set$new(m, "j", records=paste0("j",1:5))
v2 = Variable$new(m, "v2", domain=c(i, j), records=data.frame(c("i1","i4","i5"), c("j2","j4","j1"), level=c(3, NA, 5), marginal=c(NaN, 4, 5)))
v2p = Variable$new(m, "v2p",domain=c(i, j), records=data.frame(c("i1","i4","i5"), c("j2","j4","j1"), level=c(2.95, NA, 5.09), marginal=c(NaN, 4, 5)))

expect_true(v1$equals(v1p, columns="level", checkMetaData=FALSE, atol=0.1))
expect_true(v1$equals(v1p, checkMetaData=FALSE, atol=0.1))
expect_true(v1$equals(v1p, columns=c("level", "marginal"), checkMetaData=FALSE, atol=0.1))
expect_false(v1$equals(v1p, columns=c("level", "marginal"), checkMetaData=FALSE, atol=0))
expect_error(v1$equals(v1p, checkMetaData=FALSE, atol=c(level=0, scale=0.1), rtol=c(marginal=0, scale=0.1)))
expect_error(v1$equals(v1p, checkMetaData=FALSE, atol=c(level=0, scale=0.1), verbose=TRUE))

}
)

# test equals for alias and unialias
test_that("test_num_106", {
m = Container$new()
i = Set$new(m, "i", records=c("i1","i2","i3"))
j = Set$new(m, "j", records=c("i1","i2","i3"))

ip = Alias$new(m, "ip", i)
jp = Alias$new(m, "jp", j)

expect_true(i$equals(j, checkMetaData=FALSE))
expect_true(ip$equals(jp, checkMetaData=FALSE))
expect_false(ip$equals(jp, checkMetaData=TRUE))
expect_true(ip$equals(j, checkMetaData=FALSE))
expect_true(i$equals(jp, checkMetaData=FALSE))

u = UniverseAlias$new(m, "u")
up = UniverseAlias$new(m, "up")

expect_true(u$equals(up, checkMetaData=FALSE))
expect_false(u$equals(up))
}
)

# test equals for container and constcontainer
test_that("test_num_107", {
m = Container$new()
i = Set$new(m, "i")

m1 = Container$new()
i1 = Set$new(m1, "i")

expect_true(m$equals(m1))
j = Set$new(m1, "j")
expect_false(m$equals(m1))
expect_error(m$equals(m1, verbose=TRUE))

k = Set$new(m, "k")
expect_false(m$equals(m1))
expect_error(m$equals(m1,verbose=TRUE))
}
)

# test partial domain_forwarding
test_that("test_num_107", {
m = Container$new()
i = Set$new(m, "i")
j = Set$new(m, "j")
recs=data.frame(i=c("a","b","c"), j = c("d","e","f"), val=c(1,2,3))
d = Parameter$new(m, "d",domain=c(i, j), records= recs, domainForwarding=
c(TRUE, FALSE))

expect_true(is.null(j$records))
expect_equal(nrow(i$records), 3)

m = Container$new()
i = Set$new(m, "i")
j = Set$new(m, "j")
recs=data.frame(i=c("a","b","c"), j = c("d","e","f"), val=c(1,2,3))
expect_error(Parameter$new(m, "d1",domain=c(i, j), records= recs, domainForwarding=
2))
expect_error(Parameter$new(m, "d2",domain=c(i, j), records= recs, domainForwarding=
C(TRUE, TRUE, FALSE)))

d = Parameter$new(m, "d",domain=c(i, j), records= recs, domainForwarding=
c(FALSE, TRUE))

expect_true(is.null(i$records))
expect_equal(nrow(j$records), 3)

m = Container$new()
i = Set$new(m, "i")
j = Set$new(m, "j")
recs=data.frame(i=c("a","b","c"), j = c("d","e","f"), val=c(1,2,3))
d = Parameter$new(m, "d",domain=c(i, j), records= recs, domainForwarding=
TRUE)

expect_equal(nrow(i$records), 3)
expect_equal(nrow(j$records), 3)

m = Container$new()
i = Set$new(m, "i")
j = Set$new(m, "j")
recs=data.frame(i=c("a","b","c"), j = c("d","e","f"), val=c(1,2,3))
d = Parameter$new(m, "d",domain=c(i, j), records= recs, domainForwarding=
FALSE)
expect_true(is.null(i$records))
expect_true(is.null(j$records))
}
)

# test generateRecords
test_that("test_num_108", {
m = Container$new()
i = Set$new(m, "i", records=paste0("i",1:5))
isub = Set$new(m, "isub", domain=i)
isub$generateRecords()
expect_equal(nrow(isub$records), 5)

p = Parameter$new(m, "p",domain=i)
p$generateRecords(density=1)
expect_equal(nrow(p$records), 5)

p$generateRecords(density=0.5)
expect_equal(nrow(p$records), 2)

# error because runif expects argument n and not size
# expect_error(p$generateRecords(density=1, func=runif))

rg = function(size) {
	return(runif(size))
}

p$generateRecords(density=1, func=rg)
expect_equal(nrow(p$records), 5)

rg_error = function() {
	return(1)
}

# expect_error(p$generateRecords(func=runif))

j = Set$new(m, "j", records=paste0("j",1:3))
d = Parameter$new(m, "d", domain=c(i, j))
d$generateRecords()
expect_equal(nrow(d$records), 15)

}
)

# test generateRecords for variable and equation
test_that("test_num_109", {
m = Container$new()
i = Set$new(m, "i", records=paste0("i",1:5))
e = Equation$new(m, "e", "eq", domain=i)
e$generateRecords()
expect_equal(nrow(e$records), 5)
expect_true(all(e$records$marginal == 0))
expect_true(all(e$records$lower == 0))
expect_true(all(e$records$upper == 0))
expect_true(all(e$records$scale == 1))

rg = function(size) {
        return(runif(n=size))
}
e$generateRecords(func=rg)
expect_equal(nrow(e$records), 5)
expect_true(all(e$records$marginal == 0))
expect_true(all(e$records$lower == 0))
expect_true(all(e$records$upper == 0))
expect_true(all(e$records$scale == 1))

e$generateRecords(func=list(scale=rg))
expect_equal(nrow(e$records), 5)
expect_true(all(e$records$marginal == 0))
expect_true(all(e$records$lower == 0))
expect_true(all(e$records$upper == 0))
expect_true(all(e$records$level == 0))

e$generateRecords(func=list(scale=rg, marginal=rg))
expect_equal(nrow(e$records), 5)
expect_true(all(e$records$lower == 0))
expect_true(all(e$records$upper == 0))
expect_true(all(e$records$level == 0))
expect_equal(colnames(e$records), c("i", "level", "marginal", "lower", "upper", "scale"))


#test the same for variables
m = Container$new()
i = Set$new(m, "i", records=paste0("i",1:5))
v = Variable$new(m, "v", "binary", domain=i)
v$generateRecords()
expect_equal(nrow(v$records), 5)
expect_true(all(v$records$marginal == 0))
expect_true(all(v$records$lower == 0))
expect_true(all(v$records$upper == 1))
expect_true(all(v$records$scale == 1))

rg = function(size) {
        return(runif(n=size))
}
v$generateRecords(func=rg)
expect_equal(nrow(v$records), 5)
expect_true(all(v$records$marginal == 0))
expect_true(all(v$records$lower == 0))
expect_true(all(v$records$upper == 1))
expect_true(all(v$records$scale == 1))

v$generateRecords(func=list(scale=rg))
expect_equal(nrow(v$records), 5)
expect_true(all(v$records$marginal == 0))
expect_true(all(v$records$lower == 0))
expect_true(all(v$records$upper == 1))
expect_true(all(v$records$level == 0))

v$generateRecords(func=list(scale=rg, marginal=rg))
expect_equal(nrow(v$records), 5)
expect_true(all(v$records$lower == 0))
expect_true(all(v$records$upper == 1))
expect_true(all(v$records$level == 0))
expect_equal(colnames(v$records), c("i", "level", "marginal", "lower", "upper", "scale"))

# try scalar variables and equations
m = Container$new()
e0 = Equation$new(m, "e0", "eq")
v0 = Variable$new(m, "v0", "binary")

e0$generateRecords()
expect_equal(colnames(e0$records), c("level", "marginal", "lower", "upper", "scale"))
expect_true(all(e0$records$marginal == 0))
expect_true(all(e0$records$lower == 0))
expect_true(all(e0$records$upper == 0))
expect_true(all(e0$records$scale == 1))

v0$generateRecords()
expect_equal(colnames(v0$records), c("level", "marginal", "lower", "upper", "scale"))
expect_true(all(v0$records$marginal == 0))
expect_true(all(v0$records$lower == 0))
expect_true(all(v0$records$upper == 1))
expect_true(all(v0$records$scale == 1))
}
)

# test generateRecords for multidimensional
test_that("test_num_110", {
m = Container$new()
i = Set$new(m, "i", records=paste0("i", 1:50))
j = Set$new(m, "j", records=paste0("j", 1:50))
k = Set$new(m, "k", records=paste0("k", 1:50))
l = Set$new(m, "l", records=paste0("l", 1:50))

# create and define the symbol `a` with `regular` domains
a = Set$new(m, "a", c(i, j, k, l))

# generate the records
a$generateRecords(density = 0.05)
expect_equal(nrow(a$records), (50**4)/20)
}
)

# write empty gdx
test_that("test_num_111", {
m = Container$new()
expect_true(is.null(m$write("empty.gdx")))

}
)

# test copy
test_that("test_num_112", {
m = Container$new()
i = Set$new(m, "i", records=paste0("i", 1:50))
p = Parameter$new(m, "p")
v = Variable$new(m, "v")
e = Equation$new(m, "e", type="eq")

m2 = Container$new()
i$copy(m2)
p$copy(m2)
v$copy(m2)
e$copy(m2)

m3 = Container$new()
i = Set$new(m3, "i")
p = Parameter$new(m3, "p")
v = Variable$new(m3, "v")
e = Equation$new(m3, "e", type="eq")

expect_error(i$copy(m2))
expect_true(is.null(i$copy(m2, overwrite=TRUE )))
expect_error(p$copy(m2))
expect_true(is.null(p$copy(m2, overwrite=TRUE )))
expect_error(v$copy(m2))
expect_true(is.null(v$copy(m2, overwrite=TRUE )))
expect_error(e$copy(m2))
expect_true(is.null(e$copy(m2, overwrite=TRUE )))

m = Container$new()
i = Set$new(m, "i")
m2 = Container$new()
i1 = Parameter$new(m2, "i")
p = Parameter$new(m2, "p")
expect_error(i$copy(m2))
expect_error(i$copy(i))
expect_error(p$copy(m, overwrite="true"))

m = Container$new()
i = Set$new(m, "i")
p = Parameter$new(m, "p", domain=i)

m2 = Container$new()
p$copy(m2)

expect_equal(m2["p"]$domain[[1]], "i")
expect_equal(m2["p"]$domainType, "relaxed")

i2 = Set$new(m2, "i", domain=c("*","*"))
i2$copy(m, overwrite=TRUE)

expect_equal(p$domain[[1]]$dimension, 2)

# todo
expect_equal(p$isValid(force=TRUE), FALSE)

m = Container$new()
i = Set$new(m, "i", records=c("i1","i2"))
m2 = Container$new()
i2 = Set$new(m2, "i", records=c("newi", "newi_1"))
i2$copy(m, overwrite=TRUE)
expect_equal(as.character(i$records[[1]]), c("newi", "newi_1"))


}
)

# test copy for alias and universe alias
test_that("test_num_113", {
  m = Container$new()
  i = Set$new(m, "i")
  ii = Alias$new(m, "ii", i)

  m2 = Container$new()
  # ii$copy(m2)
  # expect_equal(m2$listSymbols(), c("i","ii"))

  u = UniverseAlias$new(m, "u")
  u$copy(m2)

  # expect_equal(m2$listSymbols(), c("i","ii","u"))
  expect_equal(m2$listSymbols(), c("u"))
}
)

# test copy container method
test_that("test_num_114", {
  m = Container$new(testthat::test_path("testdata", "trnsport.gdx"))

  m2 = Container$new()
  m$copy(m2)

  expect_equal(m2$listSymbols(), m$listSymbols())


  m2 = Container$new()
  m$copy(m2, symbols=c("f","d"))

  expect_equal(m2$listSymbols(), c("f","d"))

  expect_error(m$copy(m2, "d"))
  m$copy(m2, "d", overwrite=TRUE)
  expect_equal(m2$listSymbols(), c("f","d"))
}
)

# test copy ConstContainer method
test_that("test_num_115", {
  m = ConstContainer$new(testthat::test_path("testdata", "trnsport.gdx"))

  m2 = Container$new()
  m$copy(m2)

  expect_equal(m2$listSymbols(), m$listSymbols())


  m2 = Container$new()
  m$copy(m2, symbols=c("f","d"))

  expect_equal(m2$listSymbols(), c("f","d"))

  expect_error(m$copy(m2, "d"))
  m$copy(m2, "d", overwrite=TRUE)
  expect_equal(m2$listSymbols(), c("f","d"))
}
)

# test copy for constalias and constuniverse alias
test_that("test_num_116", {
  m = ConstContainer$new(testthat::test_path("testdata", "test95.gdx"))

  m2 = Container$new()

  m["h"]$copy(m2)

  # expect_equal(m2$listSymbols(), c("i","ii","u"))
  expect_equal(m2$listSymbols(), c("h"))
}
)


# test reading symbols with unused uels from gdx
test_that("test_num_117", {
  m = Container$new()
  i = Set$new(m, "i", records=paste0("i", 1:5))
  isub = Set$new(m, "isub", domain=i, records=c("i1","i2"))
  isub$setUELs(paste0("i",1:5))
  m$write("gt.gdx")

  m2 = Container$new(testthat::test_path("gt.gdx"))

  expect_equal(m["isub"]$getUELs(), paste0("i",1:5))
}
)

# test isScalar for symbols
test_that("test_num_118", {
  m = Container$new()
  p = Parameter$new(m, "p")
  expect_true(p$isScalar)

  v = Variable$new(m, "v", type="free")
  expect_true(v$isScalar)

  e = Equation$new(m, "e", type="eq")
  expect_true(e$isScalar)

  m$write("gt.gdx")
  m2 = ConstContainer$new()
  m2$read(testthat::test_path("gt.gdx"))

  expect_true(m2["p"]$isScalar)
  expect_true(m2["v"]$isScalar)
  expect_true(m2["e"]$isScalar)
}
)

# test Scalars with more records isvalid
test_that("test_num_119", {
  m = Container$new()
  p = Parameter$new(m, "p")
  p$setRecords(data.frame(value=c(1,2)))
  expect_false(p$isValid())

  v = Variable$new(m, "v", type="free")
  v$setRecords(data.frame(level=c(1,2)))
  expect_false(v$isValid())

  e = Equation$new(m, "e", type="eq")
  e$setRecords(data.frame(level=c(1,2)))
  expect_false(e$isValid())
}
)

# test description of length > 255
test_that("test_num_119", {
  m = Container$new()
  i = Set$new(m, "i")
  a = paste0(rep("m",255), collapse = "")
  i$description = a

  a = paste0(rep("m",256), collapse = "")


  expect_error({i$description = a})
}
)

# reorderUELs with null argument
test_that("test_num_120", {
  m = Container$new()
  i = Set$new(m, "i", records = paste0("i", c(1,2,4,3)))
  i$setUELs(paste0("i", 1:5))

  expect_equal(i$getUELs(), paste0("i", 1:5))

  i$reorderUELs()
  expect_equal(i$getUELs(), paste0("i", c(1,2,4,3, 5)))

  a = Parameter$new(m, "p", domain=c(i, i), records=data.frame(i=c("i1","i5","i3"), i=c("i2","i4","i1"), value=c(1,3,4)))
  expect_equal(a$getUELs(1), c("i1","i5","i3"))
  expect_equal(a$getUELs(2), c("i2","i4","i1"))

  a$setUELs(uels=paste0("i", 1:5), dimension=1)
  a$setUELs(uels=paste0("i", 1:5), dimension=2)

  expect_equal(a$getUELs(1), paste0("i", 1:5))
  expect_equal(a$getUELs(2), paste0("i", 1:5))

  a$reorderUELs(dimension=1)
  expect_equal(a$getUELs(1), c("i1","i5","i3", "i2","i4"))

  a$reorderUELs(dimension=2)
  expect_equal(a$getUELs(2), c("i2","i4","i1", "i3","i5"))
}
)
