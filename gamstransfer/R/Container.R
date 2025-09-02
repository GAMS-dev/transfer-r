#
# GAMS - General Algebraic Modeling System R API
#
# Copyright (c) 2017-2025 GAMS Software GmbH <support@gams.com>
# Copyright (c) 2017-2025 GAMS Development Corp. <support@gams.com>
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

#' @useDynLib gamstransfer, .registration = TRUE
#' @exportPattern "^[[:alpha:]]+"
#' @importFrom Rcpp evalCpp
#' @importFrom collections dict
#' @import R.utils
#' @title Container Class
#' @description The main object class within GAMS Transfer is called
#' Container. The Container is the vessel that allows symbols to be
#' linked together (through their domain definitions), it enables
#' implicit set definitions, it enables structural manipulations of the
#' data (matrix generation), and it allows the user to perform different
#' read/write operations.
#' Please visit https://transfer-r.readthedocs.io/en/latest/
#' for detailed documentation of this package.
#'
#' @field data is a named list containing all symbol data
#' @examples
#' m <- Container$new()
#' i <- m$addSet("i")
#' j <- m$addSet("j")
#' all_symbols <- m$listSymbols()
#' set_description <- m$describeSets()

#' # create a container and read the file trnsport.gdx
#' m = Container$new(system.file("extdata", "trnsport.gdx", package = "gamstransfer"))
#' # access symbol named "x" from the container
#' x = m["x"]
#' # list all symbols
#' all_symbols = m$listSymbols()
#' # list all sets
#' all_sets = m$listSets()
#' # check if the container contains symbol named "i"
#' has_i = m$hasSymbols("i")
#' # get a summary of the description of all sets in the Container
#' set_description = m$describeSets()

Container <- R6::R6Class(
  "Container",
  public = list(
    data = NULL,
    .lc_data = NULL,
    .requiresStateCheck = NULL,
    #' @description
    #' Create a new container simply by initializing an object.
    #' @param loadFrom optional argument to point to the GDX file being
    #' read into the Container
    initialize = function(loadFrom = NULL, systemDirectory = NULL) {
      # systemDirectory can be passed but won't be used
      if (!is.null(systemDirectory)) {
        warning(paste0(
          "The argument `systemDirectory` is deprecated as ",
          "of gamstransfer 3.0. This argument is no longer needed and ",
          "will be removed in some future version of the package\n"
        ))
      }
      self$data <- collections::ordered_dict()
      # another dict for lowercase names to original case
      self$.lc_data <- collections::dict()

      self$.requiresStateCheck <- TRUE

      if (!missing(loadFrom)) {
        self$read(loadFrom)
      }
    },
    `[` = function(key) {
      if (self$.lc_data$has(tolower(key))) {
        # key exists
        true_case_key <- self$.lc_data$get(tolower(key))
        return(self$data$get(true_case_key))
      } else {
        return(invisible(NULL))
      }
    },
    `[<-` = function(key, values) {
      self$data$set(key, values)
      self$.lc_data$set(tolower(key), key)
      return(invisible(self))
    },
    format = function(...) {
      paste0(
        "GAMS Transfer: R6 object of class",
        " Container"
      )
    },
    hasSymbols = function(names) {
      if (!is.character(names)) {
        stop("The argument `names` must be type character\n")
      }
      if (length(names) == 1) {
        return(self$.lc_data$has(tolower(names)))
      } else {
        return(unlist(
          lapply(names, function(x) {
            self$.lc_data$has(tolower(x))
          }),
          use.names = FALSE
        ))
      }
    },
    getSymbolNames = function(names) {
      if (!is.character(names)) {
        stop("The argument `names` must be type character\n")
      }
      return(unlist(lapply(names, function(x) {
        if (!self$.lc_data$has(tolower(x))) {
          stop(paste0("Symbol ", x, " does not exist\n"))
        } else {
          self$.lc_data$get(tolower(x))
        }
      }), use.names = FALSE))
    },
    listSymbols = function(isValid = NULL) {
      if (!is.null(isValid)) {
        if (!is.logical(isValid)) {
          stop("The argument 'isValid' must be type logical\n")
        }

        correct_validity_data <- unlist(lapply(
          unlist(self$data$keys()),
          function(x) {
            if (self[x]$isValid() == isValid) {
              return(x)
            }
          }
        ), use.names = FALSE)

        return(correct_validity_data)
      } else {
        return(unlist(self$data$keys(), use.names = FALSE))
      }
    },
    listSets = function(isValid = NULL) {
      if (!(is.logical(isValid) || is.null(isValid))) {
        stop("Argument `isValid` must be type logical or NULL \n")
      }
      sets <- NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self[s], "Set")) {
          if (is.null(sets)) {
            sets <- s
          } else {
            sets <- append(sets, s)
          }
        }
      }
      return(sets)
    },
    listParameters = function(isValid = NULL) {
      if (!(is.logical(isValid) || is.null(isValid))) {
        stop("Argument `isValid` must be type logical or NULL \n")
      }
      parameters <- NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self[s], "Parameter")) {
          if (is.null(parameters)) {
            parameters <- s
          } else {
            parameters <- append(parameters, s)
          }
        }
      }
      return(parameters)
    },
    listAliases = function(isValid = NULL) {
      if (!(is.logical(isValid) || is.null(isValid))) {
        stop("Argument `isValid` must be type logical or NULL \n")
      }
      aliases <- NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self[s], ".BaseAlias")) {
          if (is.null(aliases)) {
            aliases <- s
          } else {
            aliases <- append(aliases, s)
          }
        }
      }
      return(aliases)
    },
    listVariables = function(isValid = NULL, types = NULL) {
      if (!(is.logical(isValid) || is.null(isValid))) {
        stop("Argument `isValid` must be type logical or NULL \n")
      }

      if (is.null(types)) {
        types <- .varTypes
      }

      if (!(is.character(types))) {
        stop("Argument `types` myst be type character or NULL \n")
      }

      for (t in types) {
        if (!any(.varTypes == tolower(t))) {
          stop(paste0("User input unrecognized variable type: ", t, " \n"))
        }
      }

      variables <- NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self[s], "Variable") &&
          any(types == self[s]$type)) {
          if (is.null(variables)) {
            variables <- s
          } else {
            variables <- append(variables, s)
          }
        }
      }
      return(variables)
    },
    listEquations = function(isValid = NULL, types = NULL) {
      if (!(is.logical(isValid) || is.null(isValid))) {
        stop("Argument `isValid` must be type logical or NULL \n")
      }

      if (is.null(types)) {
        types <- unlist(unique(.EquationTypes))
      }

      if (!is.character(types)) {
        stop("Argument `types` myst be type character or NULL \n")
      }

      for (t in types) {
        if (is.null(.EquationTypes[[tolower(t)]])) {
          stop(paste0("User input unrecognized equation type: ", t, " \n"))
        }
      }

      equations <- NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self[s], "Equation") &&
          any(types == self[s]$type)) {
          if (is.null(equations)) {
            equations <- s
          } else {
            equations <- append(equations, s)
          }
        }
      }
      return(equations)
    },
    describeSets = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$listSets()
        if (is.null(symbols)) {
          return()
        }
      } else {
        if (!is.character(symbols)) {
          stop(paste0("Argument `symbols` must be type character or NULL \n"))
        }
      }

      colNames <- c(
        "name",
        "isSingleton",
        "domain",
        "domainType",
        "dimension",
        "numberRecords",
        "sparsity"
      )
      df <- data.frame(matrix(NA,
        nrow =
          length(symbols), ncol = length(colNames)
      ))
      rowCount <- 0

      symbols <- self$getSymbolNames(symbols)
      for (i in symbols) {
        if (any(self$listSets() == i) || any(self$listAliases() == i)) {
          symDescription <- list(
            i,
            self[i]$isSingleton,
            paste(self[i]$domainNames, sep = "", collapse = " "),
            self[i]$domainType,
            self[i]$dimension,
            self[i]$numberRecords,
            self[i]$getSparsity()
          )
          rowCount <- rowCount + 1
          df[rowCount, ] <- symDescription
        }
      }


      colnames(df) <- colNames

      if (length(intersect(symbols, self$listAliases())) != 0) {
        alias_with <- c()
        is_alias <- c()
        for (i in nrow(df)) {
          name <- df[i, 1]
          is_alias <- append(
            is_alias,
            inherits(self[name], ".BaseAlias")
          )
          if (inherits(self[name], "Alias")) {
            alias_with <- append(alias_with, self[name]$aliasWith$name)
          } else if (inherits(self[name], "UniverseAlias")) {
            alias_with <- append(alias_with, self[name]$aliasWith)
          }
        }
        df$isAlias <- is_alias
        df$aliasWith <- alias_with
        append(colNames, "isAlias", 3)
        append(colNames, "aliasWith", 4)
        df <- df[, colNames]
      }
      if (rowCount > 0) {
        df <- df[1:rowCount, ]
        return(df[order(df[, 1]), ])
      } else {
        return(NULL)
      }
    },
    describeAliases = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$listAliases()
        if (is.null(symbols)) {
          return()
        }
      } else {
        if (!is.character(symbols)) {
          stop(paste0("Argument `symbols` must be type character or NULL\n"))
        }
      }

      colNames <- list(
        "name",
        "aliasWith",
        "isSingleton",
        "domain",
        "domainType",
        "dimension",
        "numberRecords",
        "sparsity"
      )
      df <- data.frame(matrix(NA,
        nrow =
          length(symbols), ncol = length(colNames)
      ))
      rowCount <- 0
      symbols <- self$getSymbolNames(symbols)
      for (i in symbols) {
        if (any(self$listAliases() == i)) {
          if (inherits(self[i], "Alias")) {
            aliasName <- self[i]$aliasWith$name
          } else if (inherits(self[i], "UniverseAlias")) {
            aliasName <- self[i]$aliasWith
          }

          symDescription <- list(
            i,
            aliasName,
            self[i]$isSingleton,
            paste(self[i]$domainNames, sep = "", collapse = " "),
            self[i]$domainType,
            self[i]$dimension,
            self[i]$numberRecords,
            self[i]$getSparsity()
          )
          rowCount <- rowCount + 1
          df[rowCount, ] <- symDescription
        }
      }
      colnames(df) <- colNames
      if (rowCount > 0) {
        df <- df[1:rowCount, ]
        return(df[order(df[, 1]), ])
      } else {
        return(NULL)
      }
    },
    describeParameters = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$listParameters()
        if (is.null(symbols)) {
          return()
        }
      } else {
        if (!is.character(symbols)) {
          stop(paste0("Argument `symbols` must be type character or NULL\n"))
        }
      }

      colNames <- list(
        "name",
        "domain",
        "domainType",
        "dimension",
        "numberRecords",
        "min",
        "mean",
        "max",
        "whereMin",
        "whereMax",
        "sparsity"
      )
      df <- data.frame(matrix(NA,
        nrow =
          length(symbols), ncol = length(colNames)
      ))
      rowCount <- 0

      symbols <- self$getSymbolNames(symbols)
      for (i in symbols) {
        if (any(self$listParameters() == i)) {
          symDescription <- list(
            i,
            paste(self[i]$domainNames, sep = "", collapse = " "),
            self[i]$domainType,
            self[i]$dimension,
            self[i]$numberRecords,
            self[i]$getMinValue("value"),
            self[i]$getMeanValue("value"),
            self[i]$getMaxValue("value"),
            self[i]$whereMin("value"),
            self[i]$whereMax("value"),
            self[i]$getSparsity()
          )
          rowCount <- rowCount + 1
          df[rowCount, ] <- symDescription
        }
      }

      colnames(df) <- colNames
      if (rowCount > 0) {
        df <- df[1:rowCount, ]
        return(df[order(df[, 1]), ])
      } else {
        return(NULL)
      }
    },
    describeVariables = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$listVariables()
        if (is.null(symbols)) {
          return()
        }
      } else {
        if (!is.character(symbols)) {
          stop(paste0("Argument `symbols` must be type character or NULL\n"))
        }
      }

      colNames <- list(
        "name",
        "type",
        "domain",
        "domainType",
        "dimension",
        "numberRecords",
        "sparsity",
        "minLevel",
        "meanLevel",
        "maxLevel",
        "whereMaxAbsLevel"
      )
      df <- data.frame(matrix(NA,
        nrow =
          length(symbols), ncol = length(colNames)
      ))
      rowCount <- 0

      symbols <- self$getSymbolNames(symbols)
      for (i in symbols) {
        if (any(self$listVariables() == i)) {
          symDescription <- list(
            i,
            self[i]$type,
            paste(self[i]$domainNames, sep = "", collapse = " "),
            self[i]$domainType,
            self[i]$dimension,
            self[i]$numberRecords,
            self[i]$getSparsity(),
            self[i]$getMinValue("level"),
            self[i]$getMeanValue("level"),
            self[i]$getMaxValue("level"),
            self[i]$whereMaxAbs("level")
          )
          rowCount <- rowCount + 1
          df[rowCount, ] <- symDescription
        }
      }

      colnames(df) <- colNames
      if (rowCount > 0) {
        df <- df[1:rowCount, ]
        return(df[order(df[, 1]), ])
      } else {
        return(NULL)
      }
    },
    describeEquations = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$listEquations()
        if (is.null(symbols)) {
          return()
        }
      } else {
        if (!is.character(symbols)) {
          stop(paste0("Argument `symbols` must be type character or NULL\n"))
        }
      }

      colNames <- list(
        "name",
        "type",
        "domain",
        "domainType",
        "dimension",
        "numberRecords",
        "sparsity",
        "minLevel",
        "meanLevel",
        "maxLevel",
        "whereMaxAbsLevel"
      )
      df <- data.frame(matrix(NA,
        nrow =
          length(symbols), ncol = length(colNames)
      ))
      rowCount <- 0

      symbols <- self$getSymbolNames(symbols)
      for (i in symbols) {
        if (any(self$listEquations() == i)) {
          symDescription <- list(
            i,
            self[i]$type,
            paste(self[i]$domainNames, sep = "", collapse = " "),
            self[i]$domainType,
            self[i]$dimension,
            self[i]$numberRecords,
            self[i]$getSparsity(),
            self[i]$getMinValue("level"),
            self[i]$getMeanValue("level"),
            self[i]$getMaxValue("level"),
            self[i]$whereMaxAbs("level")
          )
          rowCount <- rowCount + 1
          df[rowCount, ] <- symDescription
        }
      }
      colnames(df) <- colNames
      if (rowCount > 0) {
        df <- df[1:rowCount, ]
        return(df[order(df[, 1]), ])
      } else {
        return(NULL)
      }
    },
    getSets = function(isValid = NULL) {
      return(self$getSymbols(self$listSets(isValid)))
    },
    getParameters = function(isValid = NULL) {
      return(self$getSymbols(self$listParameters(isValid)))
    },
    getVariables = function(isValid = NULL) {
      return(self$getSymbols(self$listVariables(isValid)))
    },
    getEquations = function(isValid = NULL) {
      return(self$getSymbols(self$listEquations(isValid)))
    },
    getAliases = function(isValid = NULL) {
      return(self$getSymbols(self$listAliases(isValid)))
    },
    read = function(loadFrom, symbols = NULL, records = TRUE) {
      # read metadata
      # get all symbols and metadata from c++
      # process it and populate various fields

      # check if records is logical
      if (!is.logical(records) && length(records) != 1) {
        stop("records must be type logical\n")
      }

      # is.character will also check vector of strings
      if (!(is.character(symbols)) && !(is.null(symbols))) {
        stop("argument symbols must be of the type character or NULL\n")
      }

      if (is.character(loadFrom)) {
        loadFrom <- R.utils::getAbsolutePath(path.expand(loadFrom))

        ext = tools::file_ext(loadFrom)
        if (!file.exists(loadFrom)) {
          if (ext != "") {
            stop(paste0("File ", loadFrom, " doesn't exist\n"))
          }
          else {
            # try if adding .gdx returns in file exists
            if (file.exists(paste0(loadFrom, ".gdx"))) {
              loadFrom = paste0(loadFrom, ".gdx")
            }
            else {
              stop(paste0("File ", loadFrom, " doesn't exist\n"))
            }
          }
        }

        private$.gdxRead(loadFrom, symbols, records)
      } else if (inherits(loadFrom, "Container")) {
        private$.containerRead(loadFrom, symbols, records)
      } else {
        stop(paste0(
          "Argument `loadFrom` must be type character or ",
          "an instance of another Container\n"
        ))
      }
    },
    removeSymbols = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$listSymbols()
      }

      # is.character also checks vector of strings
      if (!is.character(symbols)) {
        stop("Argument 'symbols' must be of type character or NULL\n")
      }

      symbols <- self$getSymbols(symbols)
      setOrAliasBool <- unlist(lapply(symbols, function(s) {
        return(inherits(s, c("Set", ".BaseAlias")))
      }), use.names = FALSE)
      setOrAliasObj <- symbols[setOrAliasBool]

      lapply(symbols, function(sym) {
        sym$container <- NULL
        sym$.requiresStateCheck <- TRUE
        self$data$remove(sym$name)
        self$.lc_data$remove(tolower(sym$name))
        return()
      })


      # remove alias if parent set is removed
      lapply(self$data$keys(), function(s) {
        if (inherits(self[s], "Alias")) {
          identical_bool <- unlist(lapply(
            setOrAliasObj,
            function(x) {
              return(identical(x, self[s]$aliasWith))
            }
          ), use.names = FALSE)

          if (any(identical_bool)) {
            self$removeSymbols(self[s]$name)
          }
        }
      })

      # remove domain references
      lapply(self$data$keys(), function(s) {
        new_dom <- unlist(lapply(self[s]$domain, function(d) {
          identical_bool <- unlist(lapply(
            setOrAliasObj,
            function(x) {
              return(identical(x, d))
            }
          ), use.names = FALSE)

          if (any(identical_bool)) {
            return("*")
          } else {
            return(d)
          }
        }), use.names = FALSE)
        self[s]$domain <- new_dom
      })

      # if there were any sets or aliases removed from the data list
      # then reset check flag for all symbols
      if (length(setOrAliasObj) != 0) {
        lapply(self$listSymbols(), function(x) {
          self[x]$.requiresStateCheck <- TRUE
        })
      }

      # reset the check flag for the container
      self$.requiresStateCheck <- TRUE
    },
    renameSymbol = function(oldName = NULL, newName = NULL) {
      if (!is.character(oldName)) {
        stop("Argument 'oldName' must be type character\n")
      }

      if (!is.character(newName)) {
        stop("Argument 'newName' must be type character\n")
      }

      if (!self$hasSymbols(oldName)) {
        stop(paste0("Symbol ", oldName, " does not exist\n"))
      }

      if (oldName != newName) {
        sym <- self$getSymbols(oldName)[[1]]
        sym$name <- newName
        self$.requiresStateCheck <- TRUE
      }
    },
    addSet = function(
        name, domain = "*", isSingleton = FALSE,
        records = NULL, domainForwarding = FALSE, description = "") {
      if (!self$hasSymbols(name)) {
        obj <- Set$new(
          self, name, domain, isSingleton,
          records, domainForwarding, description
        )
        return(obj)
      } else {
        tryCatch(
          {
            m <- Container$new()
            obj <- Set$new(
              m, self$getSymbolNames(name), domain, isSingleton,
              records, domainForwarding, description
            )
          },
          error = function(e) {
            stop(e)
          }
        )

        symobj <- self$getSymbols(name)[[1]]
        if (inherits(symobj, "Set") &&
          identical(symobj$domain, domain) &&
          symobj$isSingleton == isSingleton &&
          all(symobj$domainForwarding == domainForwarding)
        ) {
          symobj$setRecords(records)

          if (description != "") {
            symobj$description <- description
          }
          return(symobj)
        } else {
          stop(paste0(
            "Attempting to add symbol ",
            name, ", however, ",
            "one already exists in the Container. Symbol replacement ",
            "is only possible if the symbol is first removed from the ",
            "Container with the removeSymbols() method. Overwriting symbol ",
            "`records` and `description` are possible if all other fields ",
            "have not changed\n"
          ))
        }
      }
    },
    addParameter = function(
        name, domain = NULL,
        records = NULL, domainForwarding = FALSE, description = "") {
      if (!self$hasSymbols(name)) {
        obj <- Parameter$new(
          self, name, domain, records,
          domainForwarding, description
        )
        return(obj)
      } else {
        tryCatch(
          {
            m <- Container$new()
            obj <- Parameter$new(
              m, self$getSymbolNames(name), domain,
              records, domainForwarding, description
            )
          },
          error = function(e) {
            stop(e)
          }
        )

        symobj <- self$getSymbols(name)[[1]]
        if (inherits(symobj, "Parameter") &&
          identical(symobj$domain, domain) &&
          all(symobj$domainForwarding == domainForwarding)
        ) {
          symobj$setRecords(records)

          if (description != "") {
            symobj$description <- description
          }
          return(symobj)
        } else {
          stop(paste0(
            "Attempting to add symbol ",
            name, ", however, ",
            "one already exists in the Container. Symbol replacement ",
            "is only possible if the symbol is first removed from the ",
            "Container with the removeSymbols() method. Overwriting symbol ",
            "`records` and `description` are possible if all other fields ",
            "have not changed\n"
          ))
        }
      }
    },
    addVariable = function(
        name, type = "free", domain = NULL,
        records = NULL, domainForwarding = FALSE, description = "") {
      if (!self$hasSymbols(name)) {
        obj <- Variable$new(
          self, name, type, domain, records,
          domainForwarding, description
        )
        return(obj)
      } else {
        tryCatch(
          {
            m <- Container$new()
            obj <- Variable$new(
              m, self$getSymbolNames(name), type, domain,
              records, domainForwarding, description
            )
          },
          error = function(e) {
            stop(e)
          }
        )

        symobj <- self$getSymbols(name)[[1]]
        if (inherits(symobj, "Variable") &&
          symobj$type == type &&
          identical(symobj$domain, domain) &&
          all(symobj$domainForwarding == domainForwarding)
        ) {
          symobj$setRecords(records)

          if (description != "") {
            symobj$description <- description
          }
          return(symobj)
        } else {
          stop(paste0(
            "Attempting to add symbol ",
            name, ", however, ",
            "one already exists in the Container. Symbol replacement ",
            "is only possible if the symbol is first removed from the ",
            "Container with the removeSymbols() method. Overwriting symbol ",
            "`records` and `description` are possible if all other fields ",
            "have not changed\n"
          ))
        }
      }
    },
    addEquation = function(
        name, type, domain = NULL,
        records = NULL, domainForwarding = FALSE, description = "") {
      if (!self$hasSymbols(name)) {
        obj <- Equation$new(
          self, name, type, domain, records,
          domainForwarding, description
        )
        return(obj)
      } else {
        tryCatch(
          {
            m <- Container$new()
            obj <- Equation$new(
              m, self$getSymbolNames(name), type, domain,
              records, domainForwarding, description
            )
          },
          error = function(e) {
            stop(e)
          }
        )

        symobj <- self$getSymbols(name)[[1]]
        if (inherits(symobj, "Equation") &&
          symobj$type == type &&
          identical(symobj$domain, domain) &&
          all(symobj$domainForwarding == domainForwarding)
        ) {
          symobj$setRecords(records)

          if (description != "") {
            symobj$description <- description
          }
          return(symobj)
        } else {
          stop(paste0(
            "Attempting to add symbol ",
            name, ", however, ",
            "one already exists in the Container. Symbol replacement ",
            "is only possible if the symbol is first removed from the ",
            "Container with the removeSymbols() method. Overwriting symbol ",
            "`records` and `description` are possible if all other fields ",
            "have not changed\n"
          ))
        }
      }
    },
    addAlias = function(name, aliasWith) {
      if (!self$hasSymbols(name)) {
        obj <- Alias$new(
          self, name, aliasWith
        )
        return(obj)
      } else {
        if (!inherits(aliasWith, c("Set", "Alias"))) {
          stop("GAMS `aliasWith` must be type set or Alias\n")
        }

        if (inherits(aliasWith, "Alias")) {
          parent <- alias_with_input
          while (!inherits(parent, "Set")) {
            parent <- parent$aliasWith
          }
          alias_with_input <- parent
        }

        symobj <- self$getSymbols(name)[[1]]
        if (inherits(symobj, "Alias")) {
          symobj$aliasWith <- aliasWith
          return(symobj)
        } else {
          stop(paste0(
            "Attempting to add an Alias symbol named ", name,
            " however, a symbol with this name but different type already exists ",
            "in the container. Symbol replacement is only possible if this symbols ",
            "is first removed from the Container with the ",
            "removeSymbols() method\n"
          ))
        }
      }
    },
    addUniverseAlias = function(name) {
      if (!self$hasSymbols(name)) {
        obj <- UniverseAlias$new(
          self, name
        )
        return(obj)
      } else {
        symobj <- self$getSymbols(name)[[1]]
        if (inherits(symobj, "UniverseAlias")) {
          return(symobj)
        } else {
          stop(paste0(
            "Attempting to add a UniverseAlias symbol named ", name,
            " however, a symbol with this name but different type already ",
            "exists in the container. Symbol replacement is only possible ",
            "if this symbols is first removed from the Container with the ",
            "removeSymbols() method\n"
          ))
        }
      }
    },
    getSymbols = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$listSymbols()
      }

      if (!is.character(symbols)) {
        stop("The argument symbols must be type character or NULL \n")
      }

      objisnull <- unlist(lapply(symbols, self$hasSymbols), use.names = FALSE)
      if (any(objisnull == FALSE)) {
        stop(paste0("Symbol ", i, " does not appear in the container \n"))
      }

      # all symbols exist in the container
      return(unlist(
        lapply(symbols, function(x) {
          return(self[self$getSymbolNames(x)])
        }),
        use.names = FALSE
      ))
    },
    write = function(
        writeTo, symbols = NULL,
        compress = FALSE, uelPriority = NULL, mode = NULL) {
      if (!is.logical(compress)) {
        stop(paste0(
          "'compress' must be of type logical; ",
          "default False (no compression)\n"
        ))
      }

      if (!is.character(writeTo)) {
        stop("The argument writeTo must be of type character\n")
      } else {
        writeTo <- R.utils::getAbsolutePath(path.expand(writeTo))
      }

      if (!(is.character(uelPriority) || is.null(uelPriority))) {
        stop("'uelPriority' must be type character or NULL\n")
      }

      if (is.null(mode)) {
        mode <- "mapped"
      }
      if (!(is.character(mode) && length(mode) == 1)) {
        stop("Argument `mode` must be type character
        of length 1\n")
      }

      if (!any(c("string", "mapped") == mode)) {
        stop("Argument `mode` must be one of the following: 'string', 'mapped'\n")
      }

      if (mode == "string") {
        mode_int <- 1
      } else {
        mode_int <- 2
      }
      isempty <- (length(self$listSymbols()) == 0)
      enable <- NA

      if (!isempty) {
        if (is.null(symbols)) {
          symbols <- unlist(self$data$keys())
          enable <- replicate(length(symbols), TRUE)
        } else {
          enable <- replicate(length(self$listSymbols()), FALSE)

          allSymbols <- as.list(1:length(self$listSymbols()))
          names(allSymbols) <- self$listSymbols()

          allSymDict <- collections::dict(allSymbols)

          symbols <- self$getSymbolNames(symbols)
          idx <- unlist(lapply(symbols, function(s) {
            allSymDict$get(s)
          }), use.names = FALSE)
          enable[idx] <- TRUE
        }

        if (!identical(self$listSymbols(), self$listSymbols(isValid = TRUE))) {
          stop(paste0(
            "There are symbol(s) in Container that are not valid;",
            "all symbols must be valid before writing",
            " (i.e., <symbol object>$isValid() == TRUE)\n"
          ))
        }

        if (private$isValidSymbolOrder() == FALSE) {
          self$reorderSymbols()
        }
      }

      .CPP_gdxWrite(
        self, enable,
        writeTo, uelPriority, compress, mode_int
      )
    },
    reorderSymbols = function() {
      orderedSymbols <- private$validSymbolOrder()
      l1 <- lapply(orderedSymbols, function(s) self[s])
      names(l1) <- orderedSymbols
      self$data <- collections::ordered_dict(l1)
    },
    isValid = function(symbols = NULL, verbose = FALSE, force = FALSE) {
      if (is.null(symbols)) {
        symbols <- self$data$as_list()
      } else {
        symNames <- self$getSymbolNames(symbols)
        symbols <- self$getSymbols(symbols)
        names(symbols) <- symNames
      }

      if (!is.logical(verbose)) {
        stop("Argument 'verbose' must be logical\n")
      }

      if (!is.logical(force)) {
        stop("Argument 'force' must be logical\n")
      }

      if (force == TRUE) {
        self$.requiresStateCheck <- TRUE
      }

      if (self$.requiresStateCheck == TRUE) {
        tryCatch(
          {
            private$check(symbols)
            return(TRUE)
          },
          error = function(e) {
            if (verbose == TRUE) {
              message(e)
            }
            return(FALSE)
          }
        )
      } else {
        return(TRUE)
      }
    },
    .linkDomainCategories = function() {
      for (i in self$listSymbols()) {
        if (!inherits(self[i], "Alias")) {
          self[i]$.linkDomainCategories()
        }
      }
    },
    getUELs = function(symbols = NULL, ignoreUnused = FALSE) {
      if (is.null(symbols)) {
        symbols <- self$data$as_list()
      } else {
        symbols <- self$getSymbols(symbols)
      }
      uel_all_symbols <- lapply(symbols, function(s) {
        if (!inherits(s, "UniverseAlias")) {
          s$getUELs(ignoreUnused = ignoreUnused)
        }
      })
      uel_all_symbols <- unique(unlist(uel_all_symbols, use.names = FALSE))
      return(uel_all_symbols)
    },
    removeUELs = function(uels = NULL, symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$data$as_list()
      } else {
        symbols <- self$getSymbols(symbols)
      }

      lapply(symbols, function(s) {
        if (!inherits(s, "UniverseAlias")) {
          s$removeUELs(
            uels = uels,
            dimension = 1:s$dimension
          )
        }
      })
      return(invisible(NULL))
    },
    renameUELs = function(uels, symbols = NULL, allowMerge = FALSE) {
      if (is.null(symbols)) {
        symbols <- self$data$as_list()
      } else {
        symbols <- self$getSymbols(symbols)
      }

      lapply(symbols, function(s) {
        if (!inherits(s, "UniverseAlias")) {
          s$renameUELs(
            uels = uels,
            dimension = 1:s$dimension, allowMerge
          )
        }
      })
      return(invisible(NULL))
    },
    getDomainViolations = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$data$values()
      } else {
        symbols <- self$getSymbols(symbols)
      }

      n_dim <- unlist(lapply(symbols, function(s) s$dimension),
        use.names = FALSE
      )

      cont_dom_violations <- list(replicate(length(symbols) * sum(n_dim), NA))
      dom_violation_count <- 0

      for (s in symbols) {
        dom_violations <- s$getDomainViolations()
        if (is.null(dom_violations)) next
        cont_dom_violations[(dom_violation_count + 1):(dom_violation_count +
          length(dom_violations))] <- dom_violations
        dom_violation_count <- dom_violation_count + length(dom_violations)
      }
      if (dom_violation_count == 0) {
        return(invisible(NULL))
      } else {
        return(cont_dom_violations[1:dom_violation_count])
      }
    },
    hasDomainViolations = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$data$values()
      } else {
        symbols <- self$getSymbols(symbols)
      }

      return(any(unlist(lapply(
        symbols,
        function(s) {
          s$hasDomainViolations()
        }
      ), use.names = FALSE) == TRUE))
    },
    countDomainViolations = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$data$as_list()
      } else {
        symNames <- self$getSymbolNames(symbols)
        symbols <- self$getSymbols(symbols)
        names(symbols) <- symNames
      }

      dv <- lapply(
        symbols,
        function(s) s$countDomainViolations()
      )
      return(dv[dv != 0])
    },
    dropDomainViolations = function(symbols = NULL) {
      lapply(
        names(self$countDomainViolations(symbols)),
        function(s) self[s]$dropDomainViolations()
      )
      return(invisible(NULL))
    },
    countDuplicateRecords = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$data$as_list()
      } else {
        symNames <- self$getSymbolNames(symbols)
        symbols <- self$getSymbols(symbols)
        names(symbols) <- symNames
      }

      dups <- lapply(
        symbols,
        function(s) {
          return(s$countDuplicateRecords())
        }
      )
      dups <- dups[dups > 0]
      return(dups)
    },
    hasDuplicateRecords = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$data$values()
      } else {
        symbols <- self$getSymbols(symbols)
      }

      has_dups <- lapply(
        symbols,
        function(s) {
          return(s$hasDuplicateRecords())
        }
      )
      return(any(has_dups == TRUE))
    },
    dropDuplicateRecords = function(symbols = NULL, keep = "first") {
      lapply(
        names(self$countDuplicateRecords(symbols)),
        function(s) {
          self[s]$dropDuplicateRecords(keep)
        }
      )
      return(invisible(NULL))
    },
    equals = function(other, verbose = FALSE) {
      if (!inherits(other, "Container")) {
        if (verbose) {
          stop("The argument `other` is not a Container\n")
        } else {
          return(FALSE)
        }
      }

      if (self$data$size() != other$data$size()) {
        if (verbose) {
          stop(paste0(
            "Containers contain different number ",
            "of symbols.\n self: ",
            self$data$size(), "\n other :", other$data$size(), "\n"
          ))
        } else {
          return(FALSE)
        }
      }

      self_data_keys <- unlist(self$data$keys(), use.names = FALSE)
      other_data_keys <- unlist(other$data$keys(), use.names = FALSE)
      diff_keys <- setdiff(self_data_keys, other_data_keys)
      if (length(diff_keys) != 0) {
        if (verbose) {
          stop(paste0(
            "Container `data` field keys do not match.",
            " Keys not present in `other` :",
            toString(diff_keys)
          ))
        } else {
          return(FALSE)
        }
      }

      for (s in self$data$keys()) {
        selfsym <- self[s]
        othersym <- other[s]
        if (!selfsym$equals(othersym, verbose = verbose)) {
          return(FALSE)
        }
      }
      # if didn't return false until here then its true
      return(TRUE)
    },
    copy = function(destination, symbols = NULL, overwrite = FALSE) {
      if (is.null(symbols)) {
        symbols <- self$data$values()
      } else {
        symbols <- self$getSymbols(symbols)
      }

      for (s in symbols) {
        s$copy(destination, overwrite)
      }
    },
    readList = function(readlist, symbols = NULL, records = TRUE) {
      if (is.null(symbols)) {
        symbolsToRead <- lapply(readlist, "[[", 2)
      } else {
        if (!is.character(symbols)) {
          stop("The argument `symbols` must be type character\n")
        }
        sym_list <- names(readlist)
        for (s in symbols) {
          if (!any(s == sym_list)) {
            stop(paste0("The symbol ", s, "is not in the list being read.\n"))
          }
        }
        symbolsToRead <- symbols
      }

      # readlist only contains symbols to be read
      for (m in readlist) {
        if (m$name == "*") next
        domain <- private$.getDomainGDXRead(m, symbolsToRead)
        if (m$class == "Parameter") {
          Parameter$new(
            self, m$name, domain,
            records = m$records,
            domainForwarding = FALSE,
            description = m$description,
            from_gdx = TRUE
          )
        } else if (m$class == "Set") {
          Set$new(
            self, m$name, domain, m$isSingleton,
            records = m$records,
            domainForwarding = FALSE,
            m$description,
            from_gdx = TRUE
          )
        } else if (m$class == "Variable") {
          type <- m$type
          if (tolower(type) == "unknown") type <- "free"
          Variable$new(
            self, m$name, type, domain,
            records = m$records,
            domainForwarding = FALSE,
            description = m$description,
            from_gdx = TRUE
          )
        } else if (m$class == "Equation") {
          type <- m$type
          Equation$new(
            self, m$name, type, domain,
            records = m$records,
            domainForwarding = FALSE,
            description = m$description,
            from_gdx = TRUE
          )
        } else if (m$class == "Alias" || m$class == "UniverseAlias") {
          if (m$aliasWith == "*") {
            # universe alias
            UniverseAlias$new(self, m$name, from_gdx = TRUE)
          } else {
            if (!any(symbolsToRead == m$aliasWith)) {
              stop(paste0(
                "Cannot create the Alias symbol ", m$name,
                " because the parent set (", m$aliasWith, ") is not ",
                "being read into the Container. Alias symbols ",
                "require the parent set object to exist in the Container.",
                " Add ", m$aliasWith, " to the list of symbols to read.\n"
              ))
            } else {
              Alias$new(
                self, m$name, self[m$aliasWith],
                from_gdx = TRUE
              )
            }
          }
        }
      }

      # if (records == TRUE) {
      #   for (s in readlist) {
      #     if (is.null(s$records) || inherits(self[s$name],
      #     ".BaseAlias")) {
      #       next
      #     }

      #     self[s$name]$records = s$records;
      #   }
      # }
    },
    asList = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols <- self$data$values()
      } else {
        symbols <- self$getSymbols(symbols)
      }

      if (length(self$listSymbols()) == 0) {
        return(list())
      }

      l <- lapply(self$getSymbols(), function(s) {
        return(s$asList())
      })
      names(l) <- symbols
      return(l)
    }
  ),
  active = list(
    summary = function() {
      return(list(
        numberSymbols = length(self$listSymbols())
      ))
    }
  ),
  private = list(
    gdx_specVals_write = list(),
    .gdxRead = function(loadFrom, symbols, records) {
      # check if container contains any of the symbols already
      if (!is.null(symbols)) {
        sym_already_exists <- self$hasSymbols(symbols)
        if (any(sym_already_exists == TRUE)) {
          s <- which(sym_already_exists == TRUE)
          stop(paste0(
            "Attempting to add symbol ",
            symbols[s[1]], ", however,",
            " one already exists in the Container. Symbol replacement",
            " is only possible if the symbol is first removed from the",
            "Container with the removeSymbols() method.\n"
          ))
        }
      }

      readlist <- .CPP_readSuper(
        symbols, loadFrom,
        records
      )

      self$readList(readlist, NULL, records = records)
    },
    .getDomainGDXRead = function(m, symbolsToRead) {
      if (m$class == "Alias" || m$class == "UniverseAlias") {
        return(NULL)
      }
      if (m$domainType == "none" || m$domainType == "relaxed") {
        return(m$domain)
      } else {
        domain <- unlist(lapply(m$domain, function(d) {
          if (is.null(self[d]) ||
            (!is.null(self[d]) && !any(symbolsToRead == d))) {
            return(d)
          } else {
            return(self[d])
          }
        }), use.names = FALSE)
        return(domain)
      }
    },
    .containerRead = function(loadFrom, symbols, records) {
      syms <- unlist(loadFrom$data$keys())

      if (is.null(symbols)) {
        symbolsToRead <- syms
      } else {
        symbol_in_source <- loadFrom$hasSymbols(symbols)

        if (any(symbol_in_source == FALSE)) {
          s <- which(symbol_in_source == FALSE)
          stop(paste0(
            "User specified to read symbol ",
            symbols[s], " but it does ",
            "not exist in the source container\n"
          ))
        } else {
          symbolsToRead <- symbols
        }
      }
      # sort the symbols argument to preserve the order from original container
      symbolsToRead <- intersect(syms, symbolsToRead)

      sym_already_exists <- self$hasSymbols(symbolsToRead)

      if (any(sym_already_exists == TRUE)) {
        s <- which(sym_already_exists == TRUE)
        stop(paste0(
          "Attempting to add symbol ",
          symbols[s[1]], ", however,",
          " one already exists in the Container. Symbol replacement",
          " is only possible if the symbol is first removed from the",
          "Container with the removeSymbols() method.\n"
        ))
      }

      sym_is_valid <- lapply(
        symbolsToRead,
        function(x) {
          s_loadfrom <- loadFrom[x]
          return(s_loadfrom$isValid())
        }
      )
      if (any(sym_is_valid == FALSE)) {
        s <- which(sym_is_valid == FALSE)
        stop(paste0(
          "Cannot read symbol ", s, " because it is invalid, ",
          "use $isValid(verbose=TRUE) method to debug symbol state\n"
        ))
      }

      for (s in symbolsToRead) {
        s_loadfrom <- loadFrom[s]
        if (length(s_loadfrom$domainNames) == 1 &&
          is.na(s_loadfrom$domainNames)) {
          dnames <- NULL
        } else {
          dnames <- s_loadfrom$domainNames
        }

        if (inherits(s_loadfrom, "Set")) {
          Set$new(
            self, s_loadfrom$name, dnames,
            s_loadfrom$isSingleton,
            records = s_loadfrom$records,
            domainForwarding = FALSE,
            s_loadfrom$description
          )
        } else if (inherits(s_loadfrom, "Parameter")) {
          Parameter$new(
            self, s_loadfrom$name, dnames,
            domainForwarding = FALSE,
            records = s_loadfrom$records,
            description = s_loadfrom$description
          )
        } else if (inherits(s_loadfrom, "Variable")) {
          Variable$new(
            self, s_loadfrom$name, s_loadfrom$type,
            dnames,
            domainForwarding = FALSE,
            records = s_loadfrom$records,
            description = s_loadfrom$description
          )
        } else if (inherits(s_loadfrom, "Equation")) {
          Equation$new(
            self, s_loadfrom$name, s_loadfrom$type, dnames,
            domainForwarding = FALSE,
            records = s_loadfrom$records,
            description = s_loadfrom$description
          )
        } else if (inherits(s_loadfrom, "Alias")) {
          if (!any(symbolsToRead == s_loadfrom$aliasWith$name)) {
            stop(paste0(
              "Cannot create the Alias symbol ", s, " because ",
              "the parent set (", s_loadfrom$aliasWith, ") is not ",
              "being read into the in the Container. Alias symbols ",
              "require the parent set object to exist in the Container. Add ",
              s_loadfrom$aliasWith, " to the list of symbols to read."
            ))
          } else {
            Alias$new(
              self, s_loadfrom$name, self[s_loadfrom$aliasWith$name]
            )
          }
        } else if (inherits(s_loadfrom, "UniverseAlias")) {
          UniverseAlias$new(self, s_loadfrom$name)
        }
      }

      private$.linkDomainObjects(symbolsToRead, loadFrom)
    },
    .linkDomainObjects = function(symbols, loadFrom) {
      symbol_is_alias <- unlist(lapply(symbols, function(s) {
        inherits(self[s], ".BaseAlias")
      }), use.names = FALSE)
      symbol_not_alias <- symbols[!symbol_is_alias]

      lapply(symbol_not_alias, function(s) {
        d <- unlist(lapply(loadFrom[s]$domain, function(j) {
          if (is.character(j)) {
            return(j)
          } else {
            if (any(symbol_not_alias == j$name)) {
              return(self[j$name])
            } else {
              return(j$name)
            }
          }
        }), use.names = FALSE)

        if (self[s]$dimension == 1) {
          self[s]$domain <- d[[1]]
        } else {
          self[s]$domain <- d
        }
      })

      return(invisible(NULL))
    },
    check = function(symbols) {
      if (self$.requiresStateCheck == TRUE) {
        # check for cycles only when all symbols are checked
        # skip this check when user passes a subset of symbols
        if (length(symbols) == self$data$size()) {
          private$validSymbolOrder()
        }

        # make sure that all symbols have consistent naming
        lapply(names(symbols), function(n) {
          if (n != self[n]$name) {
            stop(paste0(
              "Container `data` field is inconsistent with the ",
              "symbol object name (", n, " != ", self[n]$name, "). Update ",
              "symbol name with <symbol>$name = <name from `data` field> \n"
            ))
          }
        })

        # make sure that all symbols reference the correct Container instance
        lapply(symbols, function(n) {
          if (!identical(self, n$container)) {
            stop(paste0(
              "Symbol ", self$name, " has a broken container ",
              "reference. Update symbol reference with <symbol>$container ",
              "= <new_container>\n"
            ))
          }
        })

        sym_valid <- unlist(lapply(symbols, function(s) {
          return(s$isValid())
        }), use.names = FALSE)

        if (any(sym_valid == FALSE)) {
          stop(
            "Container contains invalid symbols; ",
            "invalid symbols can be found with the $listSymbols() ",
            "method. Debug invalid symbol(s) by running $",
            "isValid(verbose=TRUE, force=TRUE) method on the symbol object.\n"
          )
        }

        self$.requiresStateCheck <- FALSE
      }
    },
    validSymbolOrder = function() {
      symbolsToSort <- self$listSymbols()
      orderedSymbols <- replicate(length(symbolsToSort), NA)

      idx <- 1
      orderedSymCount <- 0
      while (length(symbolsToSort) != 0) {
        sym <- symbolsToSort[[idx]]
        # special 1D sets (universe domain & relaxed sets)
        if (inherits(self[sym], "Set") &&
          self[sym]$dimension == 1 &&
          is.character(self[sym]$domain[[1]])
        ) {
          orderedSymCount <- orderedSymCount + 1
          orderedSymbols[orderedSymCount] <- sym
          symbolsToSort <- symbolsToSort[-idx]
          idx <- 1
        } else {
          doi <- unlist(lapply(self[sym]$domain, function(i) {
            if (is.character(i)) {
              return(TRUE)
            } else if ((orderedSymCount != 0) &&
              (inherits(i, c("Set", ".BaseAlias"))) &&
              any(orderedSymbols[1:orderedSymCount] == i$name)) {
              return(TRUE)
            } else {
              return(FALSE)
            }
          }), use.names = FALSE)

          if (all(doi == TRUE)) {
            orderedSymCount <- orderedSymCount + 1
            orderedSymbols[orderedSymCount] <- sym
            symbolsToSort <- symbolsToSort[-1]
            idx <- 1
          } else {
            idx <- idx + 1
          }
        }

        if (idx == length(symbolsToSort) + 1 & length(symbolsToSort) != 0) {
          inherits_set <- unlist(lapply(
            symbolsToSort,
            function(s) inherits(self[s], "Set")
          ), use.names = FALSE)

          symString <- symbolsToSort[inherits_set]
          symString <- paste(symString)

          stop(paste0(
            "Error: Graph cycle detected among symbols: ",
            symString, " -- must resolve circular domain referencing\n"
          ))
        }
      }
      return(orderedSymbols)
    },
    isValidSymbolOrder = function() {
      validOrder <- private$validSymbolOrder()
      currentOrder <- unlist(self$data$keys())
      h <- c()
      isSetAlias <- unlist(lapply(currentOrder, function(s) {
        return(inherits(self[s], c("Set", "Alias")))
      }), use.names = FALSE)

      set_alias_index <- which(isSetAlias)
      if (.is.integer0(set_alias_index)) {
        return(TRUE)
      }

      order_valid <- unlist(lapply(set_alias_index, function(idx) {
        if (idx <= match(currentOrder[idx], validOrder)) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      }), use.names = FALSE)

      if (all(order_valid == TRUE)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  )
)

#' @export
`[.Container` <- function(obj, ...) obj$`[`(...)

#' @export
`[<-.Container` <- function(obj, ..., value) obj$`[<-`(..., value)
