#
# GAMS - General Algebraic Modeling System Python API
#
# Copyright (c) 2017-2022 GAMS Software GmbH <support@gams.com>
# Copyright (c) 2017-2022 GAMS Development Corp. <support@gams.com>
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

#' @title Container Class
#' @description The main object class within GAMS Transfer is called 
#' Container. The Container is the vessel that allows symbols to be 
#' linked together (through their domain definitions), it enables 
#' implicit set definitions, it enables structural manipulations of the 
#' data (matrix generation), and it allows the user to perform different 
#' read/write operations.The Container class inherits from an abstract
#' BaseContainer class. To access the functions common to Container and
#' ConstContainer, please use help(BaseContainer)
#' @field data is a named list containing all symbol data
#' @field systemDirectory is the path to GAMS System directory
#' @export
Container <- R6::R6Class (
  "Container",
  inherit = .BaseContainer,
  public = list(
    .requiresStateCheck = NULL,
    #' @description
    #' Create a new container simply by initializing an object.
    #' @param loadFrom optional argument to point to the GDX file being 
    #' read into the Container
    #' @param systemDirectory optional argument for the absolute path to 
    #' GAMS system directory
    #' @examples
    #' Container$new()
    initialize = function(loadFrom=NULL, systemDirectory=NULL) {

      super$initialize(systemDirectory)

      self$.requiresStateCheck = TRUE

      if (!missing(loadFrom)) {
        self$read(loadFrom)
      }
    },

    #' @description main method to read loadFrom, can be provided 
    #' with a list of symbols to read in subsets, `records` controls 
    #' if symbol records are loaded or just metadata
    #' @param loadFrom name of the file to load data from as a string
    #' @param symbols optional argument to specify the names of the 
    #' symbols to be read (string or a list of strings)
    #' @param records optional logical argument to specify whether to 
    #' read symbol records (logical)
    read = function(loadFrom, symbols=NULL, records=TRUE) {
      # read metadata
      # get all symbols and metadata from c++
      # process it and populate various fields

      # check if records is logical
      if (!is.logical(records) && length(records) != 1) {
        stop("records must be type logical\n")
      }

      # is.character will also check vector of strings
      if (!(is.character(symbols)) && !(is.list(symbols)) 
      && !(is.null(symbols))) {
        stop("argument symbols must be of the type list, string, or NULL\n")
      }

      if (is.list(symbols)) {
        if (!all(unlist(lapply(symbols, is.character)))) {
          stop("argument `symbols`` must contain only type string\n")
        }
        # convert symbols argument to a vector
        symbols = unlist(symbols)
      }

      if (is.character(loadFrom)) {
        namesplit = strsplit(loadFrom, "\\.")
        ext = tail(unlist(namesplit), 1)
        if (ext != "gdx") {
          stop("check filename extension, must be .gdx\n")
        }
        loadFrom = R.utils::getAbsolutePath(path.expand(loadFrom))
        if (!file.exists(loadFrom)) {
          stop(paste0("File ", loadFrom, " doesn't exist\n"))
        }
        private$.gdxRead(loadFrom, symbols, records)
      }
      else if (inherits(loadFrom, c("Container", "ConstContainer"))) {
        private$.containerRead(loadFrom, symbols, records)
      }
      else {
        stop("Argument `loadFrom` must be type character, 
        an instance of another Container, or an instance of a 
        ConstContainer. \n")
      }
    },

    #' @description provides a universe for all symbols
    getUniverseSet = function() {
      uni = c()
      for (i in self$listSymbols(isValid = TRUE)) {
        if (!is.null(self[i]$records)) {
          if (self[i]$dimension > 0) {
            df = self[i]$records[, (1:self[i]$dimension)]
            if (is.factor(df)) {
              uni = append(uni, levels(df))
            }
            else {
              uni = append(uni, c(t(df)))
            }
          }
        }
      }

      if (length(uni) != 0) {
        return(unique(uni))
      }
      else {
        return(NULL)
      }
    },

    #' @description removes symbols from the Container
    #' @param symbols a string specifying the symbol name or a list of symbols 
    #' to be removed from the container
    removeSymbols = function(symbols = NULL) {
      # is.character also checks vector of strings
      if (!(is.character(symbols) || is.list(symbols))) {
        stop("Argument 'symbols' must be of type string, list, or vector\n")
      }

      # if only one element is character in a vector, entire vector is character
      # so the following check is needed only for lists
      if (is.list(symbols)) {
        if (!all(unlist(lapply(symbols, is.character)))) {
          stop("Argument 'symbols' must contain only type character\n")
        }
      }

      symbols = self$getSymbols(symbols)
      setOrAliasBool = unlist(lapply(symbols, function(s) {
        return(inherits(s, c("Set", ".BaseAlias")))
      }), use.names = FALSE)
      setOrAliasObj = symbols[setOrAliasBool]

      lapply(symbols, function(sym) {
        sym$refContainer <- NULL
        sym$.requiresStateCheck <- TRUE
        self$data$remove(sym$name)
        self$.lc_data$remove(tolower(sym$name))
        return()
      })


      # remove alias if parent set is removed
      lapply(self$data$keys(), function(s) {
        if (inherits(self[s], "Alias")) {
          identical_bool = unlist(lapply(setOrAliasObj, 
          function(x) return(identical(x, self[s]$aliasWith))), use.names=FALSE)

          if (any(identical_bool)) {
            self$removeSymbols(self[s]$name)
          }
        }
      })

      # remove domain references
      lapply(self$data$keys(), function(s) {
        new_dom = unlist(lapply(self[s]$domain, function(d) {
          identical_bool = unlist(lapply(setOrAliasObj, 
          function(x) return(identical(x, d))), use.names=FALSE)

          if (any(identical_bool)) {
            return("*")
          }
          else {
            return(d)
          }
        }), use.names = FALSE)
        self[s]$domain = new_dom
      })

      # if there were any sets or aliases removed from the data list
      # then reset check flag for all symbols
      if (length(setOrAliasObj) != 0) {
        lapply(self$listSymbols(), function(x) {
          self[x]$.requiresStateCheck = TRUE
        })
      }

      # reset the check flag for the container
      self$.requiresStateCheck = TRUE
    },

    #' @description rename a symbol in the Container
    #' @param oldName string specifying the old name of the symbol
    #' @param newName string specifying the new name of the symbol
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
        sym = self$getSymbols(oldName)[[1]]
        sym$name = newName
        self$.requiresStateCheck = TRUE
      }
    },

    #' @description There are two different ways to create a GAMS set and 
    #' add it to a Container. One is using the Set constructor and 
    #' the other is using addSet method which calls the Set constructor
    #' internally.
    #' addSet is a Container method to add a Set.
    #' @param name string argument for name of the set
    #' @param domain an optional argument specifying a list of strings, 
    #' a string. default value is "*".
    #' @param isSingleton an optional logical argument specifying if a set
    #'  is singleton. Default value is FALSE.
    #' @param records specify set records as a string vector or a dataframe.
    #' @param domainForwarding an optional logical argument to specify 
    #' domain forwarding. Default value is FALSE.
    #' @param description string specifying description for the set
    #' @return a Set object
    addSet = function(name, domain = "*", isSingleton = FALSE,
    records = NULL, domainForwarding=FALSE, description = "") {
      if (!self$hasSymbols(name)) {
        obj = Set$new(
        self, name, domain, isSingleton,
        records, domainForwarding, description)
        return(obj)
      }
      else {
        tryCatch(
        {
          m = Container$new()
          obj = Set$new(m, self$getSymbolNames(name), domain, isSingleton,
        records, domainForwarding, description)
        },
        error = function(e) {
          stop(e)

        }
        )

        symobj = self$getSymbols(name)[[1]]
        if (inherits(symobj, "Set")
        && identical(symobj$domain, domain)
        && symobj$isSingleton == isSingleton
        && all(symobj$domainForwarding == domainForwarding)
        ) {
          symobj$setRecords(records)

          if (description != "") {
            symobj$description = description
          }
          return(symobj)
        }
        else {
            stop(paste0("Attempting to add symbol ", 
            name, ", however,
             one already exists in the Container. Symbol replacement
             is only possible if the symbol is first removed from the 
            Container with the removeSymbols() method. Overwriting symbol
            `records` and `description` are possible if all other fields have
             not changed\n"))
        }
      }
    },

    #' @description There are two different ways to create a GAMS parameter and 
    #' add it to a Container. One is using the Parameter constructor and 
    #' the other is using addParameter method which calls the 
    #' Parameter constructor internally.
    #' addParameter is a Container method to add a Parameter.
    #' @param name string argument for name of the Parameter
    #' @param domain an optional argument specifying a list of strings, 
    #' a string. default value is NULL.
    #' @param records specify set records as a vector or a dataframe.
    #' @param domainForwarding an optional logical argument to specify 
    #' domain forwarding. Default value is FALSE.
    #' @param description string specifying description for the set
    #' @return a Parameter object
    addParameter = function(name, domain = NULL,
    records = NULL, domainForwarding=FALSE, description = "") {
      if (!self$hasSymbols(name)) {
        obj = Parameter$new(
          self, name, domain, records,
          domainForwarding, description)
          return(obj)
      }
      else {
         tryCatch(
        {
          m = Container$new()
          obj = Parameter$new(m, self$getSymbolNames(name), domain,
        records, domainForwarding, description)
        },
        error = function(e) {
          stop(e)

        }
        )

        symobj = self$getSymbols(name)[[1]]
        if (inherits(symobj, "Parameter")
        && identical(symobj$domain, domain)
        && all(symobj$domainForwarding == domainForwarding)
        ) {
          symobj$setRecords(records)

          if (description != "") {
            symobj$description = description
          }
          return(symobj)
        }
        else {
            stop(paste0("Attempting to add symbol ", 
            name, ", however,
             one already exists in the Container. Symbol replacement
             is only possible if the symbol is first removed from the 
            Container with the removeSymbols() method. Overwriting symbol
            `records` and `description` are possible if all other fields have
             not changed\n"))
        }
      }
    },

    #' @description There are two different ways to create a GAMS Variable and 
    #' add it to a Container. One is using the Variable constructor and 
    #' the other is using addVariable method which calls the Parameter 
    #' constructor internally.
    #' addVariable is a Container method to add a Variable.
    #' @param name string argument for name of the Variable
    #' @param type Type of variable being created [binary, integer, 
    #' positive, negative, free, sos1, sos2, semicont, semiint]. The default
    #' is "free"
    #' @param domain an optional argument specifying a list of strings, 
    #' a string. default value is NULL.
    #' @param records specify set records as a vector or a dataframe.
    #' @param domainForwarding an optional logical argument to specify 
    #' domain forwarding. Default value is FALSE.
    #' @param description string specifying description for the set
    #' @return a Variable object
    addVariable = function(name, type="free", domain = NULL,
    records = NULL, domainForwarding=FALSE, description = "") {
      if (!self$hasSymbols(name)) {
        obj = Variable$new(
          self, name, type, domain, records,
          domainForwarding, description)
          return(obj)
      }
      else {
         tryCatch(
        {
          m = Container$new()
          obj = Variable$new(m, self$getSymbolNames(name), type, domain,
        records, domainForwarding, description)
        },
        error = function(e) {
          stop(e)

        }
        )

        symobj = self$getSymbols(name)[[1]]
        if (inherits(symobj, "Variable")
        && symobj$type == type
        && identical(symobj$domain, domain)
        && all(symobj$domainForwarding == domainForwarding)
        ) {
          symobj$setRecords(records)

          if (description != "") {
            symobj$description = description
          }
          return(symobj)
        }
        else {
            stop(paste0("Attempting to add symbol ", 
            name, ", however,
             one already exists in the Container. Symbol replacement
             is only possible if the symbol is first removed from the 
            Container with the removeSymbols() method. Overwriting symbol
            `records` and `description` are possible if all other fields have
             not changed\n"))
        }

      }
    },

    #' @description There are two different ways to create a GAMS Equation and 
    #' add it to a Container. One is using the Equation constructor and 
    #' the other is using addEquation method which calls the Equation 
    #' constructor internally.
    #' addEquation is a Container method to add a Equation.
    #' @param name string argument for name of the Equation
    #' @param type Type of equation being created [eq (or E/e), geq 
    #' (or G/g), leq (or L/l), nonbinding (or N/n), external (or X/x)]
    #' @param domain an optional argument specifying a list of strings, 
    #' a string. default value is "*".
    #' @param records specify set records as a vector or a dataframe.
    #' @param domainForwarding an optional logical argument to specify 
    #' domain forwarding. Default value is FALSE.
    #' @param description string specifying description for the set
    #' @return a Equation object
    addEquation = function(name, type, domain = NULL, 
    records = NULL, domainForwarding=FALSE, description = "") {
      if (!self$hasSymbols(name)) {
        obj = Equation$new(
          self, name, type, domain, records,
          domainForwarding, description)
          return(obj)
      }
      else {
         tryCatch(
        {
          m = Container$new()
          obj = Equation$new(m, self$getSymbolNames(name), type, domain,
        records, domainForwarding, description)
        },
        error = function(e) {
          stop(e)

        }
        )

        symobj = self$getSymbols(name)[[1]]
        if (inherits(symobj, "Equation")
        && symobj$type == type
        && identical(symobj$domain, domain)
        && all(symobj$domainForwarding == domainForwarding)
        ) {
          symobj$setRecords(records)

          if (description != "") {
            symobj$description = description
          }
          return(symobj)
        }
        else {
            stop(paste0("Attempting to add symbol ", 
            name, ", however,
             one already exists in the Container. Symbol replacement
             is only possible if the symbol is first removed from the 
            Container with the removeSymbols() method. Overwriting symbol
            `records` and `description` are possible if all other fields have
             not changed\n"))
        }

      }
    },

    #' @description There are two different ways to create a GAMS Alias and 
    #' add it to a Container. One is using the Alias constructor and 
    #' the other is using addAlias method which calls the Alias 
    #' constructor internally.
    #' addAlias is a Container method to add a Alias.
    #' @param name string argument for name of the Alias
    #' @param aliasWith string argument for the set/alias we want to add
    #' an alias for
    addAlias = function(name, aliasWith) {
      if (!self$hasSymbols(name)) {
        obj = Alias$new(
        self, name, aliasWith)
        return(obj)
      }
      else {
        if (!inherits(aliasWith, c("Set", "Alias"))) {
          stop("GAMS `aliasWith` must be type set or Alias\n")
        }

        if (inherits(aliasWith, "Alias")) {
          parent = alias_with_input
          while (!inherits(parent, "Set")) {
            parent = parent$aliasWith
          }
          alias_with_input = parent
        }

        symobj = self$getSymbols(name)[[1]]
        if (inherits(symobj, "Alias")) {
          symobj$aliasWith = aliasWith
          return(symobj)
        }
        else {
          stop(paste0("Attempting to add an Alias symbol named ", name, 
          " however, a symbol with this name but different type already exists 
          in the container. Symbol replacement is only possible if this symbols 
          is first removed from the Container with the 
          removeSymbols() method\n"))
        }
      }
    },

    addUniverseAlias = function(name) {
      if (!self$hasSymbols(name)) {
        obj = UniverseAlias$new(
        self, name)
        return(obj)
      }
      else {

        symobj = self$getSymbols(name)[[1]]
        if (inherits(symobj, "UniverseAlias")) {
          return(symobj)
        }
        else {
          stop(paste0("Attempting to add a UniverseAlias symbol named ", name, 
          " however, a symbol with this name but different type already exists 
          in the container. Symbol replacement is only possible if this symbols 
          is first removed from the Container with the 
          removeSymbols() method\n"))
        }
      }
    },

    #' @description returns a list of object references for `Symbols`
    #' @param symbols character, string, or vector of Symbols for which 
    #' the user wants object references
    #' @returns a list of object references to symbols
    getSymbols = function(symbols) {
      if (!(is.character(symbols) || is.list(symbols))) {
        stop("The argument symbols must be type character, list, or vector \n")
      }

      if (is.list(symbols)) {
        if (!all(unlist(lapply(symbols, is.character)))) {
          stop("Argument 'symbols' must contain only type character\n")
        }
      }

      objisnull = unlist(lapply(symbols, self$hasSymbols), use.names = FALSE)
      if (any(objisnull == FALSE)) {
        stop(paste0("Symbol ", i, " does not appear in the container \n"))
      }

      # all symbols exist in the container
      return(unlist(lapply(symbols, function(x) {
        return(self[self$getSymbolNames(x)])
      }),
      use.names = FALSE))
    },

    #' @description a write method to write to a writeTo GDX file
    #' @param writeTo name of the GDX file to write to
    #' @param compress write tge GDX file in compressed format by setting
    #' compress = TRUE.
    #' @param uelPriority Advanced users might want to specify an order 
    #' to their UEL list (i.e., the universe set); The UEL 
    #' ordering follows that dictated by the data. As a convenience, it 
    #' is possible to prepend the UEL list with a user specified order 
    #' using the uel_priority argument.
    write = function(writeTo, symbols=NULL, 
    compress = FALSE, uelPriority = NULL) {

      if (!is.logical(compress)) {
        stop("'compress' must be of type logical; 
        default False (no compression)\n")
      }

      if (!is.character(writeTo)) {
        stop("The argument writeTo must be of type string\n")
      }
      else {
        namesplit = strsplit(writeTo, "\\.")
        ext = tail(unlist(namesplit), 1)
        if (ext != "gdx") {
          stop("check filename extension, must be .gdx\n")
        }

        writeTo = R.utils::getAbsolutePath(path.expand(writeTo))
      }

      if (!is.null(uelPriority)) {
        if (!(is.character(uelPriority) || is.list(uelPriority))) {
          stop("'uelPriority' must be type list or str\n")
        }

        if (is.list(uelPriority)) {
          if (!all(unlist(lapply(uelPriority, is.character), 
          use.names = FALSE))) {
            stop("Argument `uelPriority` must contain only type character\n")
          }
        }
      }

      isempty = (length(self$listSymbols()) == 0)
      if (!isempty) {
        if (is.null(symbols)) {
          symbols = unlist(self$data$keys())
          enable = replicate(length(symbols), TRUE)
        }
        else {
          enable = replicate(length(self$listSymbols()), FALSE)

          allSymbols = as.list(1:length(self$listSymbols()))
          names(allSymbols) = self$listSymbols()

          allSymDict = collections::dict(allSymbols)

          symbols = self$getSymbolNames(symbols)
          idx=unlist(lapply(symbols, function(s) {
            allSymDict$get(s)
          }), use.names = FALSE)
          enable[idx] = TRUE
        }

        if (!identical(self$listSymbols(), self$listSymbols(isValid=TRUE) )) {
          stop(paste0("There are symbol(s) in Container that are not valid;",
          "all symbols must be valid before writing",
          " (i.e., <symbol object>$isValid() == TRUE)\n"))
        }

        if (private$isValidSymbolOrder() == FALSE) {
          self$reorderSymbols()
        }

        if (is.null(uelPriority)) {
          reorder = NA
          is_uel_priority = FALSE
        }
        else {
          universe = self$getUniverseSet()
          if ((is.null(universe)) ||
          (!setequal(intersect(uelPriority, universe), uelPriority))) {
            stop("uelPriority must be a subset of the universe, check 
            spelling of an element in uelPriority? Also check 
            getUniverseSet() method for the assumed Universe Set.\n")
          }

          reorder = uelPriority
          reorder = append(reorder, universe)
          reorder = unlist(unique(reorder))
          is_uel_priority = TRUE
        }
      }
      else {
        is_uel_priority = FALSE
        enable = NA
        reorder = NA
      }
      CPP_gdxWriteSuper(self$data$as_list(), enable, self$systemDirectory, 
      writeTo, reorder, is_uel_priority, compress)
    },

    #' @description reorder symbols in order to avoid domain violations
    reorderSymbols = function() {
      orderedSymbols = private$validSymbolOrder()
      l1 = lapply(orderedSymbols, function(s) self[s])
      names(l1) = orderedSymbols
      self$data = collections::ordered_dict(l1)
    },

    #' @description TRUE if all the symbols is in the Container are 
    #' valid, throw exceptions if verbose=True, check all symbols if 
    #' force=TRUE.
    #' @param verbose type logical
    #' @param force type logical
    isValid = function(symbols=NULL, verbose=FALSE, force=FALSE) {
      if (is.null(symbols)) {
        symbols = self$data$as_list()
      }
      else {
        symNames = self$getSymbolNames(symbols)
        symbols = self$getSymbols(symbols)
        names(symbols) = symNames
      }

      if (!is.logical(verbose)) {
        stop("Argument 'verbose' must be logical\n")
      }

      if (!is.logical(force)) {
        stop("Argument 'force' must be logical\n")
      }

      if (force == TRUE) {
        self$.requiresStateCheck = TRUE
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
    }
    else {
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

    getUELs = function(symbols=NULL, ignoreUnused = FALSE) {
      if (is.null(symbols)) {
        symbols = self$data$as_list()
      }
      else {
        symbols = self$getSymbols(symbols)
      }
      uel_all_symbols = lapply(symbols, function(s) {
        if (!inherits(s, "UniverseAlias")) {
          s$getUELs(ignoreUnused=ignoreUnused)
        }
      })
      uel_all_symbols = unique(unlist(uel_all_symbols, use.names = FALSE))
      return(uel_all_symbols)
    },

    removeUELs = function(uels = NULL, symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$data$as_list()
      }
      else {
        symbols = self$getSymbols(symbols)
      }

      lapply(symbols, function(s) {
        if (!inherits(s, "UniverseAlias")) {
          s$removeUELs(uels= uels, 
          dimension = 1:s$dimension)
        }
      })
      return(invisible(NULL))
    },

    renameUELs = function(uels, symbols=NULL, allowMerge=FALSE) {
      if (is.null(symbols)) {
        symbols = self$data$as_list()
      }
      else {
        symbols = self$getSymbols(symbols)
      }

      lapply(symbols, function(s) {
        if (!inherits(s, "UniverseAlias")) {
          s$renameUELs(uels= uels, 
          dimension = 1:s$dimension, allowMerge)
        }
      })
      return(invisible(NULL))
    },

    getDomainViolations = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$data$values()
      }
      else {
        symbols = self$getSymbols(symbols)
      }

      n_dim = unlist(lapply(symbols, function(s) s$dimension), 
      use.names = FALSE)

      cont_dom_violations = list(replicate(length(symbols) * sum(n_dim), NA))
      dom_violation_count = 0

      for (s in symbols) {
        dom_violations = s$getDomainViolations()
        if (is.null(dom_violations)) next
        cont_dom_violations[(dom_violation_count+1):(dom_violation_count + 
        length(dom_violations))] = dom_violations
        dom_violation_count = dom_violation_count + length(dom_violations)
      }
      if (dom_violation_count == 0) {
        return(invisible(NULL))
      }
      else {
        return(cont_dom_violations[1:dom_violation_count])
      }
    },

    hasDomainViolations = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$data$values()
      }
      else {
        symbols = self$getSymbols(symbols)
      }

      return(any(unlist(lapply(symbols, 
      function(s) {s$hasDomainViolations()}), use.names=FALSE) == TRUE))
    },

    countDomainViolations = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$data$as_list()
      }
      else {
        symNames = self$getSymbolNames(symbols)
        symbols = self$getSymbols(symbols)
        names(symbols) = symNames
      }

      dv = lapply(symbols, 
      function(s) s$countDomainViolations())
      return(dv[dv != 0])
    },

    dropDomainViolations = function(symbols=NULL) {
      lapply(names(self$countDomainViolations(symbols)), 
      function(s) self[s]$dropDomainViolations())
      return(invisible(NULL))
    },

    countDuplicateRecords = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$data$as_list()
      }
      else {
        symNames = self$getSymbolNames(symbols)
        symbols = self$getSymbols(symbols)
        names(symbols) = symNames
      }

      dups = lapply(symbols, 
      function(s) return(s$countDuplicateRecords()))
      dups = dups[dups > 0]
      return(dups)
    },

    hasDuplicateRecords = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$data$values()
      }
      else {
        symbols = self$getSymbols(symbols)
      }

      has_dups = lapply(symbols, 
      function(s) return(s$hasDuplicateRecords()))
      return(any(has_dups==TRUE))
    },

    dropDuplicateRecords = function(symbols=NULL, keep = "first") {
      lapply(names(self$countDuplicateRecords(symbols)), 
      function(s) {self[s]$dropDuplicateRecords(keep)})
      return(invisible(NULL))
    },

    equals = function(other, verbose=FALSE) {

      if (!inherits(other, ".BaseContainer")) {
        if (verbose) {
          stop("The argument `other` is not a Container\n")
        }
        else {
          return(FALSE)
        }
      }

      if (self$data$size() != other$data$size()) {
        if (verbose) {
          stop(paste0("Containers contain different number ",
          "of symbols.\n self: ", 
          self$data$size(), "\n other :", other$data$size(), "\n"))
        }
        else {
          return(FALSE)
        }
      }

      self_data_keys = unlist(self$data$keys(), use.names = FALSE)
      other_data_keys = unlist(other$data$keys(), use.names = FALSE)
      diff_keys = setdiff(self_data_keys, other_data_keys)
      if (length(diff_keys) != 0) {
        if (verbose) {
          stop(paste0("Container `data` field keys do not match.",
          " Keys not present in `other` :", 
          toString(diff_keys)))
        }
        else {
          return(FALSE)
        }
      }

      for (s in self$data$keys()) {
        selfsym = self[s]
        othersym = other[s]
        if (!selfsym$equals(othersym, verbose=verbose)) {
          return(FALSE)
        }
      }
      # if didn't return false until here then its true
      return(TRUE)
    },

    copy = function(destination, symbols=NULL, overwrite=FALSE) {
      if (is.null(symbols)) {
        symbols = self$data$values()
      }
      else {
        symbols = self$getSymbols(symbols)
      }

      for (s in symbols) {
        s$copy(destination, overwrite)
      }
    }

  ),
  private = list(
    gdx_specVals_write = list(),

    .gdxRead = function(loadFrom, symbols, records) {
        # check if container contains any of the symbols already
        if (!is.null(symbols)) {
          sym_already_exists = self$hasSymbols(symbols)
          if (any(sym_already_exists == TRUE)) {
            s = which(sym_already_exists == TRUE)
            stop(paste0("Attempting to add symbol ", 
            symbols[s[1]], ", however,",
            " one already exists in the Container. Symbol replacement",
            " is only possible if the symbol is first removed from the", 
            "Container with the removeSymbols() method.\n"))
          }
        }

        if (is.null(symbols)) {
          cpp_syminput = ""
        }
        else {
          cpp_syminput = symbols
        }

        readlist = CPP_readSuper(cpp_syminput, loadFrom, 
        self$systemDirectory, records, is.null(symbols))

        acronyms = readlist[[1]]
        if (acronyms$nAcronyms != 0) {
          self$acronyms = acronyms[["acronyms"]]
        }

        readData = readlist[-1]
        rm("readlist")
        aliasList = list()
        aliasCount = 0

        symbolsToRead = unlist(lapply(readData, "[[", 1))

        # readData only contains symbols to be read
        for (m in readData) {
            if (m$type == .gdxSymbolTypes()[["GMS_DT_PAR"]]) {
              Parameter$new(
                self, m$name, m$domain,
                domainForwarding=FALSE,
                description = m$expltext)
            }
            else if (m$type == .gdxSymbolTypes()[["GMS_DT_SET"]]) {
                Set$new(
                self, m$name, m$domain, as.logical(m$subtype),
                records = NULL,
                domainForwarding=FALSE,
                m$expltext)
                if (m$subtype != 0 && m$subtype != 1) {
                  stop(paste0("Unknown set classification with 
                  GAMS Subtype ", m$subtype, "cannot load set ", m$name))
                }
            }
            else if (m$type == .gdxSymbolTypes()[["GMS_DT_VAR"]]) {
                type = which(.VarTypeSubtype() == m$subtype)
                if (is.integer0(type)) {
                  type = "free"
                }
                else {
                  type = names(.VarTypeSubtype())[[type]]
                }
                Variable$new(
                self, m$name, type, m$domain,
                domainForwarding = FALSE,
                description = m$expltext)
            }
            else if (m$type == .gdxSymbolTypes()[["GMS_DT_EQU"]]) {
                type = which(.EqTypeSubtype() == m$subtype)
                if (is.integer0(type)) {
                  type = "eq"
                }
                else {
                  type = names(.EqTypeSubtype())[[type]]
                }

                Equation$new(
                self, m$name, type, m$domain,
                domainForwarding = FALSE,
                description = m$expltext)
            }
            else if (m$type == .gdxSymbolTypes()[["GMS_DT_ALIAS"]]) {
                aliasCount = aliasCount + 1
                aliasList = append(aliasList, list(m))
            }

        }

        # do alias last
        for (m in aliasList) {
          if (m$aliasfor == "*") {
                # universe alias
                UniverseAlias$new(self, m$name)
          }
          else {
            if (!any(symbolsToRead == self[m$aliasfor]$name)) {
              stop(paste0("Cannot create the Alias symbol ", m, " because 
              the parent set (", self[m$aliasfor], ") is not 
              being read into the in the Container. Alias symbols 
              require the parent set object to exist in the Container. Add ",
              self[m$aliasfor], " to the list of symbols to read."))
            }
            else {
              Alias$new(
              self, m$name, self[m$aliasfor])
            }
          }

        }

        if (records == TRUE) {
          for (s in readData) {
            if (is.null(s$records) || inherits(self[s$name], 
            c(".BaseAlias"))) {
              next
            }

            recs = data.frame(s$records)
            common_attr = colnames(recs)[(self[s$name]$dimension+1):length(recs)]

            if (self[s$name]$dimension == 0) {
              dlabels = c()
            }
            else {
              dnames = self[s$name]$domainNames
              dnames[dnames == "*"] = "uni"
              is_dup = duplicated(dnames)

              if (any(is_dup)) {
                dlabels = paste0(dnames, 1:self[s$name]$dimension)
              }
              else {
                dlabels = dnames
              }
            }

            columnNames = append(dlabels, common_attr)
            colnames(recs) = columnNames
            self[s$name]$setRecords(recs)

            # map acronyms to NA
            if (!is.null(self$acronyms)) {
              if (inherits(self[s$name], c("Parameter", 
              "Variable", "Equation"))) {
                records = self[s$name]$records
                for (a in self$acronyms) {
                  records[(records 
                  == a * 1e301)] = SpecialValues[["NA"]]
                }
                self[s$name]$records = records
              }
            }
          }

          private$.linkDomainObjects(symbolsToRead)
        }

    },

    .containerRead = function(loadFrom, symbols, records) {
      syms = unlist(loadFrom$data$keys())

      if (is.null(symbols)) {
          symbolsToRead = syms
      }
      else {
        symbol_in_source = loadFrom$hasSymbols(symbols)

        if (any(symbol_in_source == FALSE)) {
          s = which(symbol_in_source == FALSE)
          stop(paste0("User specified to read symbol ", 
          symbols[s], " but it does 
          not exist in the source container\n"))
        }
        else {
          symbolsToRead = symbols
        }
      }
      # sort the symbols argument to preserve the order from original container
      symbolsToRead = intersect(syms, symbolsToRead)

      sym_already_exists = self$hasSymbols(symbolsToRead)

      if (any(sym_already_exists == TRUE)) {
        s = which(sym_already_exists == TRUE)
        stop(paste0("Attempting to add symbol ", 
        symbols[s[1]], ", however,",
        " one already exists in the Container. Symbol replacement",
        " is only possible if the symbol is first removed from the", 
        "Container with the removeSymbols() method.\n"))
      }

      if (inherits(loadFrom, "Container")) {
        sym_is_valid = lapply(symbolsToRead, 
        function(x) {
          s_loadfrom = loadFrom[x]
          return(s_loadfrom$isValid())
        })
        if (any(sym_is_valid == FALSE)) {
          s = which(sym_is_valid == FALSE)
          stop(paste0("Cannot read symbol ", s, " because it is invalid, 
          use $isValid(verbose=TRUE) method to debug symbol state\n"))
        }
      }

        for (s in symbolsToRead) {
          s_loadfrom = loadFrom[s]
          if (length(s_loadfrom$domainNames) == 1 
          && is.na(s_loadfrom$domainNames)) {
            dnames = NULL
          }
          else {
            dnames = s_loadfrom$domainNames
          }
          if (inherits(s_loadfrom, c("Set", ".ConstSet"))) {
            Set$new(
            self, s_loadfrom$name, dnames, 
            s_loadfrom$isSingleton,
            records = s_loadfrom$records,
            domainForwarding=FALSE,
            s_loadfrom$description)
          }
          else if (inherits(s_loadfrom, c("Parameter", ".ConstParameter"))) {
            Parameter$new(
            self, s_loadfrom$name, dnames,
            domainForwarding=FALSE,
            records = s_loadfrom$records,
            description = s_loadfrom$description)
          }
          else if (inherits(s_loadfrom, c("Variable", ".ConstVariable"))) {
            Variable$new(
            self, s_loadfrom$name, s_loadfrom$type, 
            dnames,
            domainForwarding = FALSE,
            records = s_loadfrom$records,
            description = s_loadfrom$description)
          }
          else if (inherits(s_loadfrom, c("Equation", ".ConstEquation"))) {
            Equation$new(
            self, s_loadfrom$name, s_loadfrom$type, dnames,
            domainForwarding = FALSE,
            records = s_loadfrom$records,
            description = s_loadfrom$description)
          }
          else if (inherits(s_loadfrom, ".ConstAlias")) {
            if (!any(symbolsToRead == s_loadfrom$aliasWith)) {
              stop(paste0("Cannot create the Alias symbol ", s, " because 
              the parent set (", s_loadfrom$aliasWith, ") is not 
              being read into the in the Container. Alias symbols 
              require the parent set object to exist in the Container. Add ",
              s_loadfrom$aliasWith, " to the list of symbols to read."))
            }
            else {
              Alias$new(
                self, s_loadfrom$name, self[s_loadfrom$aliasWith])
            }
          }
          else if (inherits(s_loadfrom, "Alias")) {
            if (!any(symbolsToRead == s_loadfrom$aliasWith$name)) {
              stop(paste0("Cannot create the Alias symbol ", s, " because 
              the parent set (", s_loadfrom$aliasWith, ") is not 
              being read into the in the Container. Alias symbols 
              require the parent set object to exist in the Container. Add ",
              s_loadfrom$aliasWith, " to the list of symbols to read."))
            }
            else {
              Alias$new(
                self, s_loadfrom$name, self[s_loadfrom$aliasWith$name])
            }
          }
          else if (inherits(s_loadfrom, c("UniverseAlias", 
          ".ConstUniverseAlias"))) {
            UniverseAlias$new(self, s_loadfrom$name)
          }
        }

        private$.linkDomainObjects(symbolsToRead)
    },

    .linkDomainObjects = function(symbols) {
      symbol_is_alias = unlist(lapply(symbols, function(s) {
        inherits(self[s], ".BaseAlias")}), use.names=FALSE)
      symbol_not_alias = symbols[!symbol_is_alias]

      lapply(symbol_not_alias, function(s) {
        d = unlist(lapply(self[s]$domain, function(j) {
          if (is.character(j) && (any(symbol_not_alias == j)) && (j != s)) {
               return(self[j])
          }
          else {
            return(j)
          }
        }), use.names = FALSE)
        if (self[s]$dimension == 1) {
          self[s]$domain = d[[1]]
        }
        else {
          self[s]$domain = d
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
            stop(paste0("Container `data` field is inconsistent with the symbol 
            object name (", n, " != ", self[n]$name, "). Update 
            symbol name with <symbol>$name = <name from `data` field> \n"))
          }
          })

        # make sure that all symbols reference the correct Container instance
        lapply(symbols, function(n) {
          if (!identical(self, n$refContainer)) {
            stop(paste0("Symbol ", self$name, " has a broken container 
            reference. Update symbol reference with <symbol>$refContainer 
            = <new_container>\n"))
          }
          })

        sym_valid = unlist(lapply(symbols, function(s) 
        return(s$isValid())), use.names = FALSE)

        if (any(sym_valid == FALSE)) {
          stop("Container contains invalid symbols; ",
          "invalid symbols can be found with the $listSymbols() ",
          "method. Debug invalid symbol(s) by running .",
          "isValid(verbose=TRUE, force=TRUE) method on the symbol object.\n")
        }

        self$.requiresStateCheck = FALSE
      }
    },

    validSymbolOrder = function() {

      symbolsToSort = self$listSymbols()
      orderedSymbols = replicate(length(symbolsToSort), NA)

      idx = 1
      orderedSymCount = 0
      while (length(symbolsToSort) != 0) {
        sym = symbolsToSort[[idx]]
        # special 1D sets (universe domain & relaxed sets)
        if (inherits(self[sym], "Set") &&
        self[sym]$dimension == 1 &&
        is.character(self[sym]$domain[[1]])
        ) {
          orderedSymCount = orderedSymCount + 1
          orderedSymbols[orderedSymCount] = sym
          symbolsToSort = symbolsToSort[-idx]
          idx = 1
        }
        else {
          doi = unlist(lapply(self[sym]$domain, function(i) {
            if (is.character(i)) {
             return(TRUE)
            }
            else if ((orderedSymCount != 0) && 
            (inherits(i, c("Set", ".BaseAlias"))) &&
            any(orderedSymbols[1:orderedSymCount] == i$name)) {
               return(TRUE)
            }
            else {
              return(FALSE)
            }
          }), use.names = FALSE)

          if (all(doi == TRUE)) {
            orderedSymCount = orderedSymCount + 1
            orderedSymbols[orderedSymCount] = sym
            symbolsToSort = symbolsToSort[-1]
            idx = 1
          }
          else {
            idx = idx + 1
          }

        }

        if (idx == length(symbolsToSort) + 1 & length(symbolsToSort) != 0) {
          inherits_set = unlist(lapply(symbolsToSort, 
           function(s) inherits(self[s], "Set")), use.names = FALSE)

          symString = symbolsToSort[inherits_set]
          symString = paste(symString)

          stop(paste0("Error: Graph cycle detected among symbols: ",
          symString, " -- must resolve circular domain referencing\n"))
        }
      }
      return(orderedSymbols)
    },

    isValidSymbolOrder = function() {
      validOrder = private$validSymbolOrder()
      currentOrder = unlist(self$data$keys())
      h = c()
      isSetAlias = unlist(lapply(currentOrder, function(s) {
        return(inherits(self[s], c("Set", "Alias")))
      }), use.names = FALSE)

      set_alias_index = which(isSetAlias)
      if (is.integer0(set_alias_index)) return(TRUE)

      order_valid = unlist(lapply(set_alias_index, function(idx) {
        if (idx <= match(currentOrder[idx], validOrder)) {
          return(TRUE)
        }
        else {
          return(FALSE)
        }
      }), use.names = FALSE)

      if (all(order_valid == TRUE)) {
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    }

  )
  )
