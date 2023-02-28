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

gams_description_max_length = 255
.gdxSymbolTypes = function(){
  return(CPP_getGDXSymbolTypes())
}


.VarTypeSubtype = function() {
  return(CPP_getGDXVarTypeSubtype())
}

.EqTypeSubtype = function() {
  return(CPP_getGDXEqTypeSubtype())
}

.SetTypeSubtype = function() {
  return(CPP_getGDXSetTypeSubtype())
}

.EquationTypes = c(
eq = "eq",
E = "eq",
e = "eq",
geq = "geq",
G = "geq",
g = "geq",
leq = "leq",
L = "leq",
l = "leq",
nonbinding = "nonbinding",
N = "nonbinding",
n = "nonbinding",
cone = "cone",
C = "cone",
c = "cone",
external = "external",
X = "external",
x = "external",
boolean = "boolean",
B = "boolean",
b = "boolean"
)

.varTypes = c(
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


SpecialValues = list(
  "NA" = NA, # cannot be anything else
  "EPS" = -0.0,
  "UNDEF" = NaN,
  "POSINF" = Inf,
  "NEGINF" = -Inf,
  "isNA" = function(x) return(is.na(x) & !is.nan(x)),
  "isEps" = function(x) {
    isna = is.na(x)
    iseps_logical = ((x == 0) & (sign(1/x) == -1))
    iseps_logical[isna] = FALSE
    return(iseps_logical)
    },
  "isUndef" = function(x) return(is.nan(x)),
  "isPosInf" = function(x) return(is.infinite(x) & sign(x) == 1),
  "isNegInf" = function(x) return(is.infinite(x) & sign(x) == -1)
  )

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

      if (!identical(self$listSymbols(), self$listSymbols(isValid=TRUE) )) {
        stop(paste0("There are symbol(s) in Container that are not valid;",
         "all symbols must be valid before writing",
         " (i.e., <symbol object>$isValid() == TRUE)\n"))
      }

      if (private$isValidSymbolOrder() == FALSE) {
        self$reorderSymbols()
      }

      if (is.null(uelPriority)) {
        CPP_gdxWriteSuper(self$data$as_list(), enable, self$systemDirectory, 
        writeTo, NA, FALSE, compress)
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
        reorder = unique(reorder)

        CPP_gdxWriteSuper(self$data$as_list(), enable, self$systemDirectory, 
        writeTo, unlist(reorder), TRUE, compress)
      }
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
            self[s$name]$setRecords(data.frame(s$records))

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

#' @title Symbol Abstract Class
#' @description An abstract symbol class from 
#' which the classes Set, Parameter, Variable, 
#' and Equation are inherited.
.Symbol <- R6Class(
  ".Symbol",
  inherit = .BaseSymbol,
  public = list(
  .requiresStateCheck = NULL,
  initialize = function(container, name,
                        type, subtype, 
                        domain,
                        description,
                        domainForwarding) {

    super$initialize(type, subtype)


    self$.requiresStateCheck = TRUE

    #' @field refContainer reference to the Container that the symbol 
    #' belongs to. Type Container.
    self$refContainer = container
    self$refContainer$.requiresStateCheck = TRUE
    #' @field name name of the symbol
    self$name <- name
    container[name] = self

    self$records = NULL

    self$domain = domain

    self$description = description
    self$domainForwarding = domainForwarding

  },

  getUELs = function(dimension=NULL, codes=NULL, ignoreUnused = FALSE) {
    if (self$dimension == 0) return(c())
    if (is.null(dimension)) {
      if (!is.null(codes)) {
        stop("User must specify `dimension` if retrieving UELs with the 
        `codes` argument\n")
      }
      dimension = 1:self$dimension
    }

    if (!(is.integer(dimension) || is.numeric(dimension)) || 
    !all(dimension %% 1 == 0) || 
    any(dimension < 1) || any(dimension > self$dimension)) {
      stop(paste0("All elements of the argument ", 
      " `dimension` must be integers in [1, ", 
      self$dimension, "]\n"))
    }

    if (!is.logical(ignoreUnused)){
      stop("The argument `ignoreUnused` must be type logical\n")
    }

    if (!is.null(codes) && (!(is.numeric(codes) || is.integer(codes)) || 
    !all(codes %% 1 == 0) || !all(codes >= 1))) {
      stop(paste0("The argument `codes` must be integers ", 
      " or a vector of integers >= 1\n"))
    }

    if (!self$isValid()) {
      stop("The symbol must be valid in order to manage UELs\n")
    }

    uels = unlist(lapply(dimension, function(d) {
      if (ignoreUnused) {
        uels_d = levels(droplevels(self$records[, d]))
      }
      else {
        uels_d = levels(self$records[, d])
      }

      if (!is.null(codes)) {
        uels_d = uels_d[codes]
      }
      return(uels_d)
    }), use.names = FALSE)

    return(unique(uels))
  },

  setUELs = function(uels, dimension = NULL, rename=FALSE) {
    if (is.null(dimension)) {
      dimension = 1:self$dimension
    }
    # input check
    if (!(is.integer(dimension) || is.numeric(dimension)) || 
    !all(dimension %% 1 == 0) || 
    any(dimension < 1) || any(dimension > self$dimension)) {
      stop(paste0("All elements of the argument 
      `dim` must be integers in [1, ", 
      self$dimension, "]\n"))
    }

    if (!is.character(uels)) {
      stop("The argument uels must be type `character` \n")
    }

    if (!is.logical(rename)) {
      stop("The argument `rename` must be type logical")
    }

    if (!self$isValid()) {
      stop("The symbol has to be valid to set UELs \n")
    }

    # remove trailing whitespaces from uels
    uels = trimws(uels, which="right")

    for (d in dimension) {
      if (rename) {
        levels(private$.records[, d]) = uels
      }
      else {
        private$.records[, d] = 
        factor(as.character(private$.records[, d]), levels=uels, 
        ordered = TRUE)
      }
    }

  },

  reorderUELs = function(uels, dimension=NULL) {
    # input check
    if (is.null(dimension)) dimension =1:self$dimension

    if (!(is.integer(dimension) || is.numeric(dimension)) || 
    !all(dimension %% 1 == 0) || 
    any(dimension < 1) || any(dimension > self$dimension)) {
      stop(paste0("All elements of the argument 
      `dim` must be integers in [1, ", 
      self$dimension, "]\n"))
    }

    if (!is.character(uels)) {
      stop("The argument `uels` must be type `character` \n")
    }

    if (!self$isValid()) {
      stop("The symbol has to be valid to reorder UELs \n")
    }

    for (d in dimension) {
      if ((length(uels) != length(levels(private$.records[, d])))) {
        stop("The argument `uels` must 
        contain all uels that need to be reordered")
      }
      else {
        if (length(setdiff(uels, private$.records[,d])) != 0) {
          stop("The argument `uels` must 
          contain all uels that need to be reordered")
        }
      }
      private$.records[, d] = factor(private$.records[, d], levels=uels)
    }
  },

  addUELs = function(uels, dimension=NULL) {
    if (is.null(dimension)) dimension =1:self$dimension

    # input check
    if (!(is.integer(dimension) || is.numeric(dimension)) || 
    !all(dimension %% 1 == 0) || 
    any(dimension < 1) || any(dimension > self$dimension)) {
      stop(paste0("All elements of the argument 
      `dim` must be integers in [1, ", 
      self$dimension, "]\n"))
    }

    if (!is.character(uels)) {
      stop("The argument uels must be type `character` \n")
    }

    if (!self$isValid()) {
      stop("The symbol has to be valid to add UELs \n")
    }

    # remove trailing whitespaces from uels
    uels = trimws(uels, which="right")

    for (d in dimension) {

      if (length(setdiff(uels, private$.records[,d])) == 0) {
        stop("The argument `uels` should not
        contain existing uels")
      }

      private$.records[, d] = factor(private$.records[, d], 
      levels=append(levels(private$.records[, d]), uels))
    }
  },

  removeUELs = function(uels=NULL, dimension=NULL) {
    if (!is.null(dimension)) {
      # input check
      if (!(is.integer(dimension) || is.numeric(dimension)) || 
      !all(dimension %% 1 == 0) || 
      any(dimension < 1) || any(dimension > self$dimension)) {
        stop(paste0("All elements of the argument 
        `dim` must be integers in [1, ", 
        self$dimension, "]\n"))
      }
    }
    else {
      dimension = 1:self$dimension
    }

    if (!is.null(uels)) {
      if (!is.character(uels)) {
        stop("The argument `uels`` must be type `character` \n")
      }
    }

    if (!self$isValid()) {
      stop("The symbol has to be valid to remove UELs \n")
    }

    for (d in dimension) {
      if (!is.null(uels)) {
        # remove from values and from levels
        # private$.records[, d] = 
        # (private$.records[, d])[private$.records[, d] != uels]
       private$.records[, d] = 
        factor(private$.records[, d], 
        levels = setdiff(levels(private$.records[, d]), uels))
      }
      else {
        # remove unused levels
        private$.records[, d] = droplevels(private$.records[, d])
      }
    }
  },

  renameUELs = function(uels, dimension=NULL, allowMerge=FALSE) {
    if (!is.null(dimension)) {
      # input check
      if (!(is.integer(dimension) || is.numeric(dimension)) || 
      !all(dimension %% 1 == 0) || 
      any(dimension < 1) || any(dimension > self$dimension)) {
        stop(paste0("All elements of the argument 
        `dim` must be integers in [1, ", 
        self$dimension, "]\n"))
      }
    }
    else {
      dimension = 1:self$dimension
    }

    if (!is.logical(allowMerge)) {
      stop("The argument `allowMerge` must be type logical\n")
    }

    if (!self$isValid()) {
      stop("The symbol has to be valid to add UELs \n")
    }

    if (!is.character(uels)) {
      stop("The argument uels must be type `character` \n")
    }

    # remove trailing whitespaces from uels
    uels = trimws(uels, which="right")

    # for list input add names
    if (is.null(names(uels))) {
      lapply(dimension, function(d) {
        if (length(levels(private$.records[, d])) != length(uels)) {
          stop(paste0("User passed a vector of length ", length(uels), 
          " which does not match the length of existing uels: ", 
          length(levels(private$.records[, d])), "\n"))
        }

        if (allowMerge == TRUE) {
          levels(private$.records[, d]) = unique(uels)
        }
        else {
          # make sure that the integer mapping is unaltered

          if (any(duplicated(uels) == TRUE)) {
            stop("Multiple UELs cannot be renamed to a UEL. 
            Use `allowMerge=TRUE`\n")
          }

          if (length(intersect(levels(private$.records[, d]), uels)) != 0) {
            stop("UEL cannot be renamed to an existing UEL. 
            Use `allowMerge=TRUE`.\n")
          }

          levels(private$.records[, d]) = uels
        }
      })
    }
    else {
      # user has provided a UEL map named vector
      # no duplicate keys
      if (any(duplicated(names(uels)) == TRUE)) {
        stop("A UEL cannot be renamed more than once in a single call. 
        names(uels) must be unique")
      }
      if (allowMerge == TRUE) {
        # user has provided uelmap
        old_uels = names(uels)

        lapply(dimension, function(d) {
          # get current levels
          cur_uels = levels(private$.records[, d])
          new_uels = cur_uels

          idx = match(old_uels, cur_uels)
          isna_idx = is.na(idx)
          idx = idx[!isna_idx]
          new_uels[idx] = uels[!isna_idx]

          # set current levels
          levels(private$.records[, d]) = new_uels
        })
      }
      else {
        # user has provided uelmap
        old_uels = names(uels)

        lapply(dimension, function(d) {

          # get current levels
          cur_uels = levels(private$.records[, d])
          new_uels = cur_uels

          idx = match(old_uels, cur_uels)
          isna_idx = is.na(idx)
          idx = idx[!isna_idx]
          new_uels[idx] = uels[!isna_idx]


          # don't allow more than one uels to be mapped to a same uel
          if (any(duplicated(new_uels[idx]) == TRUE)) {
            stop("Multiple UELs cannot be renamed to a UEL. Use `allowMerge=TRUE`\n")
          }

          # a uel cannot be mapped to an existing uel
          if (length(intersect(levels(private$.records[, d]), new_uels[idx])) != 0) {
            stop("UEL cannot be renamed to an existing UEL. Use `allowMerge=TRUE`.\n")
          }

          # set current levels
          levels(private$.records[, d]) = new_uels
        })
      }
    }
  },

  getDomainViolations = function() {
    if (!self$isValid()) {
      stop("The object must be valid to get domain violations\n")
    }
    if (self$dimension == 0 || is.null(self$records)) return()

    it_vec = 1:self$dimension
    is_set_alias = unlist(lapply(it_vec, function(x) {
      inherits(self$domain[[x]], c("Set", ".BaseAlias"))
    }), use.names = FALSE)
    it_vec = it_vec[is_set_alias]

    added_uel_all = lapply(it_vec, function(d) {
      setdiff(tolower(self$getUELs(d, ignoreUnused=TRUE)), 
      tolower(self$domain[[d]]$getUELs(ignoreUnused=TRUE)))
    })

    length_added_uel = unlist(lapply(added_uel_all, length), use.names = FALSE)
    it_vec = it_vec[length_added_uel > 0]

    dom_violations = lapply(it_vec, function(d) {
      DomainViolation$new(self, d, self$domain[[d]], added_uel_all[[d]])
    })

    if (length(dom_violations) == 0) return(invisible(NULL))

    return(dom_violations)
  },

  findDomainViolations = function() {
    violations = self$getDomainViolations()

    if (is.null(violations)) return(data.frame())

    idx = lapply(violations, function(dv) {
      set_dv = unique(dv$violations)

      idx = lapply(set_dv, function(v) {
        return(which(self$records[, dv$dimension] == v, arr.ind = TRUE))
      })
      return(unlist(idx, use.names=FALSE))
    })

    return(self$records[unlist(unique(idx)), ])
  },

  hasDomainViolations = function() {
    df = self$findDomainViolations()
    if ((nrow(df) == 0) && (length(df) == 0)) {
      return(FALSE)
    }
    else {
      return(TRUE)
    }
  },

  countDomainViolations = function() {
    df = self$findDomainViolations()
    return(nrow(df))
  },

  dropDomainViolations = function() {
    violations = self$getDomainViolations()

    if (is.null(violations)) return()

    idx = lapply(violations, function(dv) {
      set_dv = unique(dv$violations)

      idx = lapply(set_dv, function(v) {
        return(which(self$records[, dv$dimension] == v, arr.ind = TRUE))
      })
      return(unlist(idx, use.names=FALSE))
    })
    private$.records = private$.records[-unlist(unique(idx)), ]
    rownames(private$.records) <- NULL
    return(invisible(NULL))
  },

  countDuplicateRecords = function() {
    return(nrow(self$findDuplicateRecords()))
  },

  findDuplicateRecords = function(keep="first") {
    idx = private$.get_duplicate_index(keep)
    if (is.integer0(idx)) {
      return(data.frame())
    }
    else {
      return(self$records[idx,])
    }
  },

  hasDuplicateRecords = function() {
    return(self$countDuplicateRecords() > 0)
  },

  dropDuplicateRecords = function(keep = "first") {
    idx = private$.get_duplicate_index(keep)

    if (!is.integer0(idx)) {
      self$records = self$records[-idx, ]
      rownames(self$records) <- NULL
    }
    return(invisible(NULL))
  },

  #' @description getCardinality get the full cartesian product of the domain
  getCardinality = function() {
    tryCatch(
      {
        if (self$domainType == "relaxed" | self$domainType == "none"){
          return(NA)
        }
        else {
          card = 1
          for (i in self$domain) {
            card = card * i$numberRecords
          }
          return(card)
        }
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  },

  #' @description getSparsity get the sparsity of the symbol w.r.t the cardinality
  getSparsity = function() {
    tryCatch(
      {
        if (self$domainType == "relaxed" | self$domainType == "none"){
          return(NA)
        }
        else {
          return(1 - self$numberRecords/self$getCardinality())
        }
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  },

  #' @description TRUE if the symbol is in a valid format, 
  #' throw exceptions if verbose=True, recheck a symbol if force=True
  #' @param verbose type logical
  #' @param force type logical
  isValid = function(verbose=FALSE, force=FALSE) {
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
          private$check()
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

  shape = function() {
    if (self$domainType == "regular") {
      shapelist = c()
      for (d in self$domain) {
        shapelist = append(shapelist, nrow(d$records))
      }
      return(shapelist)
    }

    if (!is.null(self$records)) {
      if (self$dimension == 0) {
        return(c())
      }

      if (self$domainType == "none" || self$domainType == "relaxed") {
        shapelist = c()
        for (i in (1:self$dimension)) {
          shapelist = append(shapelist, length(unique(self$records[, i])))
        }
        return(shapelist)
      }
    }
    else {
      return(NULL)
    }
  },

  #' @description toDense convert symbol to a dense matrix/array format
  #' @param column column to be converted to dense format.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  toDense = function(column = "level") {
    if (!is.character(column)) {
      stop("Argument 'column' must be type str\n")
    }
    if (inherits(self, "Parameter")) {
      column = "value"
    }
    else {
      if (!any(private$.attr() == column)) {
        stop(paste0("Argument 'column' must be one 
        of the following: ", toString(private$.attr()), "\n"))
      }
    }

    if (self$isValid() == FALSE) {
      stop("Cannot create dense array (i.e., matrix) format because symbol 
      is invalid -- use .isValid(verbose=TRUE) to debug symbol state.\n")
    }

    if (!is.null(self$records)) {
      if (self$dimension  == 0) {
        return(self$records[[column]])
      }
      else {
        if (self$domainType == "regular") {
          if (self$hasDomainViolations()) {
            stop("Cannot create dense array because there are domain violations i.e.,
             the UELs in the symbol are not a subset of UELs in the domain set/s\n")
          }


          idx = lapply(1:self$dimension, function(d) {
            return(as.numeric(factor(self$records[,d], levels = levels(self$domain[[d]]$records[, 1]))) )
          })

        }
        else {
          idx = lapply(1:self$dimension, function(d) {
            return(as.numeric(factor(self$records[,d], levels = levels(self$records[, d]))) )
          })
        }

        a = array(0, dim = self$shape())
        a[matrix(unlist(idx), ncol=length(idx))] = self$records[, column]
        return(a)

      }
    }
    else {
      return(NULL)
    }
  },

  equals = function(other, columns=NULL, checkUELs=TRUE, 
  checkElementText=TRUE, checkMetaData=TRUE, rtol=NULL, atol=NULL,
  verbose=FALSE) {
    if (inherits(other, "Alias")) {
      other = other$aliasWith
    }

    tryCatch(
      {
        private$.check_equal(other, columns, checkUELs, 
        checkElementText, checkMetaData, rtol, atol)
        return(TRUE)
      },
      error = function(e) {
        if (verbose == TRUE) {
          message(e)
        }
        return(FALSE)
      }
    )
  },

  .linkDomainCategories = function() {
      private$.records[, 1:self$dimension] = lapply(1:self$dimension, function(n) {
        i  = self$domain[[n]]
        return(factor(private$.records[, n], 
        levels = levels(i$records[, 1]), ordered = TRUE))
      })
  }
  ),

  active = list(

    records = function(records_input) {
      if (missing(records_input)) {
        return(private$.records)
      }
      else {
        private$.records = records_input
        if (!is.null(self$records)) {
          if (any(self$domainForwarding == TRUE)) {
            private$domain_forwarding(self$domainForwarding)

            for (i in self$refContainer$listSymbols()) {
              self$refContainer[i]$.requiresStateCheck = TRUE
            }

            self$refContainer$.requiresStateCheck = TRUE
          }
          else {
              self$.requiresStateCheck = TRUE
              if (inherits(self$refContainer, "Container")) {
                self$refContainer$.requiresStateCheck = TRUE
              }
          }
        }
      }
    },

    domainForwarding = function(domain_forwarding_input) {
      if (missing(domain_forwarding_input)) {
        return(private$.domain_forwarding)
      }
      else {
        if (!is.logical(domain_forwarding_input)) {
          stop("Argument 'domainForwarding' must be type logical\n")

          if (!any(c(1, self$dimension) == length(domain_forwarding) )) {
            stop("The argument `domainForwarding` must be of length 1 or <symbol>$dimension \n")
          }
        }
        else {
          private$.domain_forwarding = domain_forwarding_input
        }
      }
    },

    description = function(description_input) {
      if (missing(description_input)) {
        return(private$.description)
      }
      else {
        if (!is.character(description_input)) {
          stop("Symbol 'description' must be type character\n")
        }

        if (length(description_input) >= gams_description_max_length) {
          stop(paste0("Symbol 'description' must have length ",
          gams_description_max_length, " or smaller\n"))
        }

        if (!is.null(private$.description)) {
          if (private$.description != description_input) {
            self$.requiresStateCheck = TRUE
            self$refContainer$.requiresStateCheck = TRUE
          }
          private$.description = description_input
        }
        else {
          self$.requiresStateCheck = TRUE
          self$refContainer$.requiresStateCheck = TRUE
          private$.description = description_input
        }
      }
    },

    dimension = function(dimension_input) {
      if (missing(dimension_input)) {
        return(length(self$domain))
      }
      else {
        if (!((inherits(dimension_input, c("numeric", "integer"))) && 
           (dimension_input %% 1 == 0) && (dimension_input >= 0) &&
           (dimension_input <= .gdxSymbolTypes()[["GMS_MAX_INDEX_DIM"]]))) {
            stop(paste0("Symbol 'dimension' must be 
           an integer in [0, ", .gdxSymbolTypes()[["GMS_MAX_INDEX_DIM"]], "]\n"))
           }

        if (length(self$domain) > dimension_input) {
          if (dimension_input == 0) {
            self$domain = list()
          }
          else {
            self$domain = self$domain[1:dimension_input]
          }
        }
        else if (length(self$domain) < dimension_input) {
           new = self$domain
           new = append(new, replicate(dimension_input - 
           length(self$domain), "*"))
           self$domain = new
        }
        else {
        }
      }
    },

    domain = function(domain_input) {

      if (missing(domain_input)) {
        return(private$.domain)
      }
      else {
        if (is.null(domain_input)) {
          domain_input = list()
        }

        if (!(is.list(domain_input) || is.vector(domain_input))) {
          domain_input = list(domain_input)
        }

        if (length(domain_input) > .gdxSymbolTypes()[["GMS_MAX_INDEX_DIM"]]) {
          stop(paste0("Argument 'domain' length cannot be > ", 
          .gdxSymbolTypes()[["GMS_MAX_INDEX_DIM"]], "\n"))
        }
        domain_arg_check = unlist(lapply(domain_input, function(d) {
          return((inherits(d, c("Set", ".BaseAlias")) && d$dimension == 1)
        || is.character(d))}), use.names = FALSE)
        if (any(domain_arg_check == FALSE)) {
          stop("All 'domain' elements must be either one dimensional Set/Alias/UniverseAlias
          , or must be type Character\n")
        }

        # check change of domain
        if (!identical(private$.domain, domain_input)) {
            self$.requiresStateCheck = TRUE
            if (inherits(self$refContainer, "Container")) {
              self$refContainer$.requiresStateCheck = TRUE
            }
            private$.domain = domain_input
            if (self$dimension == 0) return()

            if (!is.null(self$records)) {
              temp_colnames=colnames(self$records)
              temp_colnames[1:self$dimension] = self$domainLabels
              colnames(private$.records) = temp_colnames
            }
        }

      }
    },

    refContainer = function(ref_container_input) {
      if (missing(ref_container_input)) {
        return(private$.ref_container)
      }
      else {
        if (is.null(ref_container_input)) {
          private$.ref_container = NULL
          self$.requiresStateCheck = TRUE
          return()
        }

        if (!inherits(ref_container_input, "Container")) {
          stop("Symbol 'container' must be type Container\n")
        }
        if (is.null(self$refContainer)){
          if (!identical(self$refContainer, ref_container_input)) {
            self$.requiresStateCheck = TRUE
          }
          private$.ref_container = ref_container_input
        }
        else {
          self$.requiresStateCheck = TRUE
          private$.ref_container = ref_container_input
        }
      }
    },
    name = function(name_input) {
      if (missing(name_input)) {
        return(private$.name)
      }
      else {
        if (!is.character(name_input)) {
          stop("GAMS symbol 'name' must be type chracter\n")
        }

        if (nchar(name_input) > private$symbolMaxLength) {
          stop(paste0("GAMS symbol 'name' is too long,",
          " max is ", private$symbolMaxLength, " characters\n"))
        }

        if (self$refContainer$hasSymbols(name_input)) {
          stop(paste0("A symbol with the name ", name_input, 
          " already exists in the container\n"))
        }

        if (substr(name_input, 1, 1) == "_") {
          stop("Valid GAMS names cannot begin with a `_`character.\n")
        }

        if (grepl("^[a-zA-Z0-9_]+$", name_input) == FALSE) {
          stop("Detected an invalid GAMS symbol name. GAMS names can only 
          contain alphanumeric characters (letters and numbers) and 
          the `_` character.\n")
        }

        if (is.null(private$.name)) {
          self$.requiresStateCheck = TRUE
          private$.name = name_input
        }
        else {
          if (private$.name != name_input) {
            self$.requiresStateCheck = TRUE

            refcontainer = private$.ref_container

            refcontainer[name_input] = refcontainer[private$.name]
            refcontainer$data$remove(private$.name)
            refcontainer$.lc_data$remove(tolower(private$.name))
          }
          private$.name = name_input
        }
      }
    },

    numberRecords = function() {
      if (self$isValid() == TRUE) {
        if (!is.null(self$records)) {
          return(nrow(self$records))
        }
        else {
          return(0)
        }
      }
      else {
        return(NA)
      }
    },

    domainType = function() {
      regularCheck = unlist(lapply(self$domain, function(d) {
        return(inherits(d, c("Set", ".BaseAlias")))
       }), use.names = FALSE)

      if (all(regularCheck == TRUE) && self$dimension != 0) {
          return("regular")
      }
      else if (all(self$domain == "*")) {
        return("none")
      }
      else if (self$dimension == 0) {
        return("none")
      }
      else {
        return("relaxed")
      }
    },

    domainNames = function() {
      if (self$dimension == 0) return(NA)

      d = unlist(lapply(self$domain, function(i) {
        if (inherits(i, c("Set", ".BaseAlias"))) {
          return(i$name)
        }
        else {
          return(i)
        }
      }), use.names = FALSE)
      return(d)
    },

    domainLabels = function() {

      dom_is_univ = (self$domain == "*")
      dom_temp = self$domain
      dom_temp[dom_is_univ] = "uni"
      column_names = lapply(seq_along(dom_temp), function(i) {
        if (is.character(dom_temp[[i]])) {
          return(paste0(dom_temp[[i]], "_", i))
        }
        else {
          return(paste0(dom_temp[[i]]$name, "_", i))
        }

      })

      return(unlist(column_names, use.names=FALSE))
    }

  ),

  private = list(
    .domain_forwarding = NULL,
    .description = NULL,
    .domain = NULL,
    .ref_container = NULL,
    .name = NULL,
    .records = NULL,
    symbolMaxLength = 63,
    descriptionMaxLength = 255,

    .generate_records_index = function(density) {
      if (!(is.numeric(density)) && all(density >= 0 && density <= 1)) {
        stop("The argument `density` must be numeric in the range [0, 1]\n")
      }

      if (!any(c(1,self$dimension) == length(density))) {
        stop("The argument `density` must be of length: ", 
        self$dimension, " or 1, the user provided: ", length(density), "\n")
      }

      # get the full cartesian product
      dom_recs = lapply(self$domain, function(d) return(d$records[,1]))
      length_dom_recs = unlist(lapply(dom_recs, function(x) {return(length(x))}), use.names=FALSE)
      final_nrecs = floor(density * length_dom_recs)
      if (any(final_nrecs == 0)) return(data.frame())

      if (length(density) == 1) {
        # if the length is 1 then apply density on records dataframe instead

        # drop unused levels from a set
        dom_recs = lapply(dom_recs, function(x) return(droplevels(x)))

        # cartesian product
        recs = expand.grid(dom_recs)
        colnames(recs) = self$domainLabels

        # sample indices based on density
        idx = sample(1:nrow(recs), floor(density * nrow(recs)), replace = FALSE)

        # drop rows
        recs = recs[sort(idx), 1:length(recs), drop = FALSE]

        # drop unused levels
        recs = droplevels(recs)
      }
      else {
        rec_idx = lapply(1:length(dom_recs), function(i) { 
          rec = dom_recs[[i]]
          idx = sample(1:length(rec), floor(density[i] * length(rec)), replace = FALSE)
          return(rec[sort(idx)])
        })

        dom_recs = rec_idx
        dom_recs = lapply(dom_recs, function(x) return(droplevels(x)))

        recs = expand.grid(dom_recs)
        colnames(recs) = self$domainLabels
      }

      #reset row indices
      rownames(recs) <- NULL
      return(recs)
    },
    .check_equal = function(other, columns= NULL, checkUELs=TRUE, 
      checkElementText=TRUE, checkMetaData=TRUE, rtol=NULL, atol=NULL) {

      if (self$dimension != other$dimension) {
        stop(paste0("Symbol dimension do not match ", self$dimension, 
        " != ", other$dimension, "\n"))
      }

      if (self$domainType != other$domainType) {
        stop(paste0("Symbol domain types do not match `", self$domainType, 
        "`` != `", other$domainType, "`\n"))
      }

      if (!identical(self$domain, other$domain)) {
        stop(paste0("Symbol domains do not match \n"))
      }

      if (self$numberRecords != other$numberRecords) {
        stop(paste0("Symbols do not have same number of records ", 
        self$numberRecords, " != ", other$numberRecords, "\n"))
      }

      # check metadata
      if (checkMetaData) {
        if (self$name != other$name) {
          stop("Symbol names do not match ", 
          self$name, " != ", other$name, "\n" )
        }

        if (self$description != other$description) {
          stop("Symbol descriptions do not match ", 
          self$description, " != ", other$description, "\n" )
        }

        if (class(self)[1] != class(other)[1]) {
          stop("Symbol types do not match ", 
          class(self)[1], " != ", class(other)[1], "\n" )
        }
      }

      # check UELs
      if (checkUELs) {
        if (self$numberRecords != 0) {
          selfUELs = self$getUELs()
          otherUELs = other$getUELs()
          if (!all(selfUELs == otherUELs)) {
            stop(paste0("Symbol UELs do not match \n",
            "self: ", toString(selfUELs), "\n",
            "other: ", toString(otherUELs), "\n"))
          }
        }
      }

      if (inherits(self, c("Set", "Alias"))) {
        private$.check_set_records_equal(other, checkElementText)
      }
      else if (inherits(self, c("Parameter", "Variable", 
      "Equation"))) {
        private$.check_numeric_records_equal(other, columns, rtol, atol)
      }
    },

    .check_equals_common_args = function(other, checkUELs, checkMetaData, verbose) {
      # mandatory checks
      if (!self$isValid()) {
        stop(paste0("Cannot compare objects because ", s$name, " is not valid. 
        Use ", s$name, "$isValid(verbose=TRUE) to get more details\n"))
      }

      if (!inherits(other, c(".Symbol", ".BaseAlias"))) {
        stop("The argument `other` must be a Symbol object")
      }

      if (!other$isValid()) {
        stop(paste0("Cannot compare objects because ", other$name, " is invalid. Use ",
        other$name, "$isValid(verbose=TRUE) to debug.\n"))
      }

      if (!is.logical(checkUELs)) {
        stop("The argument `checkUELs` must be type logical")
      }

      if (!is.logical(checkMetaData)) {
        stop("The argument `checkMetaData` must be type logical")
      }

      if (!is.logical(verbose)) {
        stop("The argument `verbose` must be type logical")
      }
    },

    .check_equals_numeric_args = function(atol, rtol) {
      if (!(is.numeric(atol) && length(atol) == 1)) {
        stop("The argument `atol` must be type numeric of length 1 \n")
      }

      if (!(is.numeric(rtol) && length(rtol) == 1)) {
        stop("The argument `rtol` must be type numeric of length 1 \n")
      }
    },

    .check_set_records_equal = function(other, checkElementText) {
      if (self$numberRecords == 0) return()
      #merge both dataframes by column_names
      merged = merge(self$records, other$records, 
      by.x=self$domainLabels, by.y=other$domainLabels,
      all=TRUE)

      isna_check = is.na(merged[(self$dimension+1):length(merged)])

      if (any(isna_check)) {
        error_df = head(merged[as.logical(
        rowSums(isna_check)),][1:self$dimension])
        strmsg="symbol records do not match. Unmatched rows below\n"
        strdf = paste0(capture.output(error_df), collapse="\n")
        stop(paste0(strmsg, strdf, "\n"))
      }

      if (checkElementText) {
        el_text_mismatch = (merged[, "element_text.x"] != merged[, "element_text.y"])

        if (any(el_text_mismatch)) {
          error_df = head(merged[el_text_mismatch, ])
          strmsg="symbol element_text does not match. Unmatched rows below\n"
          strdf = paste0(capture.output(error_df), collapse="\n")
          stop(paste0(strmsg, strdf, "\n"))
        }
      }


    },

    .check_numeric_records_equal = function(other, columns, rtol, atol) {
      if (self$numberRecords == 0) return()

      # columns = unique(append(names(rtol), names(atol)))
      if (is.null(columns)) {
        if (inherits(self, c("Variable", "Equation"))) {
          columns = private$.attr()
        }
        else {
          #parameter
          columns = "value"
        }
      }

      if (self$dimension == 0) {
        # now compare numerical records
        for (attr in columns) {
          # check for special values
          count = 0
          fnames = c("EPS", "NA", "UNDEF", "POSINF", "NEGINF")
          is_special = FALSE
          for (f in c(SpecialValues$isEps, SpecialValues$isNA, 
          SpecialValues$isUndef, SpecialValues$isPosInf, 
          SpecialValues$isNegInf)) {
            count = count + 1
            is_special_self = f(self$records[, attr])
            is_special_other = f(other$records[, attr])

            if (any( is_special_self !=  is_special_other)) {
              stop(paste0("Symbols with ", fnames[count], " special values 
              do not match in the ", attr, " column.\n"))
            }
            is_special = (is_special || is_special_self)
          }
          if (is_special) next

          if (!is.null(names(atol))) {
            atol_attr = atol[[attr]]
          }
          else {
            atol_attr = atol
          }

          if (!is.null(names(rtol))) {
              rtol_attr = rtol[[attr]]
          }
          else {
            rtol_attr = rtol
          }
          # check numerical equality subject to tolerance
          lhs = abs(self$records[,attr] - other$records[, attr])
          rhs = atol_attr + rtol_attr * abs(other$records[, attr])

          if (lhs > rhs) {
            stop(paste0("Symbol records contain numeric differences in the ", 
            attr, " attribute that are outside the specified tolerances 
            rtol=", rtol_attr, ", atol=", atol_attr, "\n"))
          }
        }
      }
      else {
        #merge both dataframes by column_names
        merged = merge(self$records, other$records, 
        by.x=self$domainLabels, by.y=other$domainLabels,
        all=TRUE)
        error_df = head(merged[as.logical(
          rowSums(is.na(merged[(self$dimension+1):length(merged)]))),][1:self$dimension])

        strmsg="symbol records do not match. Unmatched rows below\n"
        strdf = paste0(capture.output(error_df), collapse="\n")
        if (any(is.na(merged[,self$dimension:length(merged)]))) {
          stop(paste0(strmsg, strdf, "\n"))
        }

        # now compare numerical records
        for (attr in columns) {
          attrs_x = paste0(attr, ".x")
          attrs_y = paste0(attr, ".y")
          small_merged = merged[1:self$dimension]
          small_merged[, c(attrs_x, attrs_y)] = 
          merged[c(attrs_x, attrs_y)]

          # check for special values
          count = 0
          fnames = c("EPS", "NA", "UNDEF", "POSINF", "NEGINF")
          for (f in c(SpecialValues$isEps, SpecialValues$isNA, 
            SpecialValues$isUndef, SpecialValues$isPosInf, 
            SpecialValues$isNegInf)) {
            count = count + 1
            idx_self = f(small_merged[, attrs_x])
            idx_other = f(small_merged[, attrs_y])
            if (any(idx_self != idx_other)) {
              stop(paste0("Symbols with ", fnames[count], " special values 
              do not match in the ", attr, " column.\n"))
            }

            if (any(idx_self)) {
              # drop special values
              small_merged = small_merged[-which(idx_self),]
            }
            if (nrow(small_merged) == 0) break
          }


          if (nrow(small_merged) == 0) next

          if (!is.null(names(atol))) {
            if (!is.null(atol[[attr]])) {
              atol_attr = atol[[attr]]
            }
            else {
              stop(paste0("User passed a named vector for the argument `atol` but the attribute ", 
              attr, " is missing\n"))
            }
          }
          else {
            atol_attr = atol
          }

          if (!is.null(names(rtol))) {
            if (!is.null(rtol[[attr]])) {
              rtol_attr = rtol[[attr]]
            }
            else {
              stop(paste0("User passed a named vector for the argument `rtol` but the attribute ", 
              attr, " is missing\n"))
            }
          }
          else {
            rtol_attr = rtol
          }

          # check numerical equality subject to tolerance
          lhs = abs(small_merged[,paste0(attr, ".x")] - small_merged[, paste0(attr, ".y")])
          rhs = atol_attr + rtol_attr * abs(small_merged[, paste0(attr, ".y")])

          if (any(lhs > rhs)) {
            stop(paste0("Symbol records contain numeric differences in the ", 
            attr, " attribute that are outside the specified tolerances 
            rtol=", rtol_attr, ", atol=", atol_attr, "\n"))
          }
        }

      }
    },


    check = function() {
      if (self$.requiresStateCheck == TRUE) {
        # if regular domain, symbols in domain must be valid
        if (self$domainType == "regular") {
          for (i in self$domain) {
            if (!self$refContainer$hasSymbols(i$name)) {
              stop(paste0("symbol defined over domain symbol ",
              i$name, " however, the object referenced is not in the", 
              " Container anymore -- must reset domain for symbol ", 
              self$name, "\n"))

            }
            if (!identical(i, self$refContainer[i$name])) {
              stop(paste0("symbol defined over domain symbol ",
              i$name, " however, the symbol with name ", i$name, 
              " in the container is different. Seems to be a broken link.
               -- must reset domain for symbol ",
              self$name))
            }
          }

          for (i in self$domain) {
            if (i$isValid() != TRUE) {
              stop(paste0("symbol defined over domain symbol ",
              i$name, " however, this object is not a valid object ",
              "in the Container -- all domain objects must be valid.\n"))
            }
          }
        }
        # if records exist, check consistency
        if (!is.null(self$records)) {
          if (inherits(self, "Set")){
            if (length(self$records) != self$dimension + 1) {
              stop(paste0("Symbol 'records' does not have", 
              " the correct number of columns (<symbol dimension> + 1)\n"))
            }
          }
          if (inherits(self, "Parameter")) {
            if (length(self$records) != self$dimension + 1) {
              stop(paste0("Symbol 'records' does not have", 
              " the correct number of columns (<symbol dimension> + 1)\n"))

              if (self$dimension == 0 && nrow(self$records != 1)) {
              stop(paste0("Symbol 'records' does not have", 
              " the correct number of columns (<symbol dimension> + 1)\n"))
              }
            }
          }

          if (inherits(self, c("Variable", "Equation"))) {
            if (length(self$records) != 
            self$dimension + length(private$.attr())) {
              stop(paste0("Symbol 'records' does not have", 
              " the correct number of columns ", 
              self$dimension + length(private$.attr()), "\n"))
            }
          }

          # check if records are dataframe
          if (!is.data.frame(self$records)){
            stop("Symbol 'records' must be type dataframe\n")
          }

          # check column names and order
          cols = self$domainLabels
          if (inherits(self, "Set")) {
            cols = append(cols, "element_text")
          }
          else if(inherits(self, "Parameter")) {
            cols = append(cols, "value")
          }
          else if (inherits(self, c("Variable", "Equation"))) {
            cols = append(cols, private$.attr())
          }

          if (!identical(cols, colnames(self$records))) {
            stop(paste0("Records columns must be named 
            and ordered as: ", toString(cols),"\n"))
          }

          if (!is.character(cols)) {
            stop("Domain columns in symbol 
            'records' must be of type character\n")
          }

          # check if all data columns are float
          if (inherits(self, c("Variable", "Parameter", "Equation" ))) {
            for (i in (self$dimension + 1):length(self$records)) {
              if (!(is.numeric(self$records[, i]) || 
              all(SpecialValues$isNA(self$records[, i])))) {
                stop("Data in column ", i, " must be numeric or NA\n")
              }
            }
          }
        }

      }
      self$.requiresStateCheck = FALSE
    },

    domain_forwarding = function(dom_forwarding) {
    if (length(dom_forwarding) == 1) {
      dim_enabled = replicate(self$dimension, TRUE)
    }
    else {
      dim_enabled = dom_forwarding
    }

    dim_to_forward = seq_len(self$dimension)
    dim_to_forward = dim_to_forward[dim_enabled]
    # find symbols to grow
    for (diter in dim_to_forward) {
      d = self$domain[[diter]]
      dl = self$domainLabels[diter]
      to_grow = list()
      while (inherits(d, "Set")) {
        to_grow = append(to_grow, d$name)
        d = d$domain[[1]]
      }
      # reverse the to_grow list because when the records are set, we check domain
      # domain_forwarding for domain sets is FALSE until specified explicitly 
      # so we should grow parent sets first and then children
      to_grow = rev(to_grow)

      for (i in to_grow) {
        dim = (self$refContainer[i]$domainLabels)[1]
        if (!is.null(self$refContainer[i]$records)) {
          recs = self$refContainer[i]$records

          if (self$refContainer[i]$dimension > 1) {
            stop("attempting to forward a domain set that has dimension > 1\n")
          }

          df = self$records[dl]
          colnames(df) = dim
          df[["element_text"]] = ""
          recs1 = factor(append(as.character(recs[, 1]), as.character(df[,dim])),
          levels = unique(append(levels(recs[, 1]), levels(df[,dim]))))
          recs2 = append(recs[, 2], df$element_text)
          cnames =colnames(recs)
          recs= data.frame(recs1, recs2)
          colnames(recs) = cnames
          # recs = rbind(recs, df)
          recs = recs[!duplicated(recs[[dim]]),]
          rownames(recs) <- NULL
        }
        else {
          recs = self$records[dl]
          colnames(recs) = dim
          recs[["element_text"]] = ""
          recs = recs[!duplicated(recs[[dim]]),]
          rownames(recs) <- NULL
        }
        self$refContainer[i]$records = recs
      }
    }
  },

  .get_duplicate_index = function(keep) {
    if (keep != FALSE && keep != "first" && keep != "last") {
      stop("The argument `keep` must be one of the following:
      `first`, `last`, or FALSE\n")
    }

    if (keep == "first") {
      fl = FALSE
      idx = which(duplicated(data.frame(lapply(1:self$dimension, 
      function(d) tolower(self$records[[d]]))), fromLast =fl) == TRUE)
    }
    else if (keep == "last") {
      fl = TRUE
      idx = which(duplicated(data.frame(lapply(1:self$dimension, 
      function(d) tolower(self$records[[d]]))), fromLast =fl) == TRUE)
    }
    else {
      idx_first = which(duplicated(data.frame(lapply(1:self$dimension, 
      function(d) tolower(self$records[[d]]))), fromLast =FALSE) == TRUE)
      idx_last = which(duplicated(data.frame(lapply(1:self$dimension, 
      function(d) tolower(self$records[[d]]))), fromLast =TRUE) == TRUE)
      idx = append(idx_last, idx_first)
    }
    return(idx)
  }
  )
)

#' @title Set Class
#' @description A class for Set objects. This class inherits from an abstract Symbol class.
#' The documentation for methods common to all symbols can be accessed via help(Symbol)
#'  or help(BaseSymbol).
#' @field description description of symbol
#' @field dimension of symbol
#' @field domainForwarding flag that forces set elements to be recursively 
#' included in all parent sets (i.e., implicit set growth)
#' @field domainLabels column headings for the records dataframe
#' @field domainNames string version of domain names
#' @field domainType none, relaxed or regular depending on state of 
#' domain links
#' @field isSingleton logical if symbol is a singleton set
#' @field name name of symbol
#' @field numberRecords 	number of symbol records
#' @field records the main symbol records
#' @field refContainer reference to the Container that the symbol belongs to
#' @field summary output a list of only the metadata
Set <- R6Class(
  "Set",
  inherit = .Symbol,
  public = list(
    #' @description There are two different ways to create a GAMS set and 
    #' add it to a Container. One is using the Set constructor and 
    #' the other is using addSet method which calls the Set constructor
    #' internally.
    #' addSet is a Container method to add a Set.
    #' @param container A reference to the Container object that the symbol 
    #' is being added to
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
    initialize = function(container=NULL, name=NULL,
                          domain="*", isSingleton=FALSE,
                          records = NULL, 
                          domainForwarding = FALSE,
                          description="") {
      self$isSingleton <- isSingleton
      type = .gdxSymbolTypes()[["GMS_DT_SET"]]

      if (!isSingleton) {
        subtype = .SetTypeSubtype()[["set"]]
      }
      else {
        subtype = .SetTypeSubtype()[["singleton_set"]]
      }

      super$initialize(container, name,
                      type, subtype,
                      domain, description, domainForwarding)

      if (!is.null(records)) {
        self$setRecords(records)
      }
      invisible(self)
    },

    #' main convenience method to set standard dataframe formatted records
    #' @param records specify set records as a string vector or a dataframe.
    setRecords = function(records) {
      # check if records is a dataframe and make if not
      records = data.frame(records)
      c = length(records)

      if (c == self$dimension) {
        # no element text
        records["element_text"] = ""
      }
      else if (c == self$dimension + 1) {

      }
      else {
        stop(paste0("The argument 'records' is of length ",
        c, " Expecting ", self$dimension + 1, "\n"))
      }
      columnNames = self$domainLabels
      columnNames = append(columnNames, "element_text")

      if (self$dimension == 0) {
        colnames(records) = columnNames
        self$records = records
        return()
      }

      records[, 1:self$dimension] = lapply(seq_along(self$domain), function(d) {
        if (is.factor(records[, d])) {
          levels(records[, d]) = trimws(levels(records[, d]), which="right")
        }
        else {
          records[, d] = factor(records[, d], levels = unique(records[, d]), ordered=TRUE)
          levels(records[, d]) = trimws(levels(records[, d]), which="right")
        }
        return(records[, d])
      })

      records = data.frame(records)

      colnames(records) = columnNames
      self$records = records
    },

    # set/alias
    equals = function(other, checkUELs=TRUE, 
    checkElementText=TRUE, checkMetaData=TRUE,
    verbose=FALSE) {
      if (!is.logical(checkElementText)) {
        stop("The argument `checkElementText` must be type logical")
      }

      super$.check_equals_common_args(other, checkUELs,
      checkMetaData, verbose)

      super$equals(other, checkUELs=checkUELs,
      checkElementText=checkElementText, checkMetaData=checkMetaData,
      verbose=verbose)
    },

    generateRecords = function(density = 1) {
      recs = super$.generate_records_index(density)
      if (nrow(recs) != 0) {
        recs$element_text = ""
      }

      private$.records = recs
    }
  ),

  active = list(
    isSingleton = function(is_singleton_input) {
      if (missing(is_singleton_input)) {
        return(private$.is_singleton)
      }
      else {
        if (!is.logical(is_singleton_input)) {
          stop("Argument 'is_singleton' must be type logical\n")
        }
        private$.is_singleton = is_singleton_input
      }
    },

    summary = function() {
      return(list(
        "name" = self$name,
        "isSingleton" = self$isSingleton,
        "domainObjects" = self$domain,
        "domainNames" = self$domainNames,
        "dimension" = self$dimension,
        "description" = self$description,
        "numberRecords" = self$numberRecords,
        "domainType" = self$domainType
      ))
    }
  ),
  private = list(
    .is_singleton = NULL
  )
  )

#' @title Parameter Class
#' @description A class for Parameter objects. This class inherits from an abstract 
#' Symbol class.The documentation for methods common to all symbols can be accessed 
#' via help(Symbol) or help(BaseSymbol).
#' countEPS, countNA, countNegInf, countPosInf, countUndef, getCardinality,
#' getSparsity, getMaxValue, getMinValue, getMeanValue, getMaxAbsValue,
#' isValid, toDense, whereMax, whereMaxAbs, whereMin.
#' @field description description of symbol
#' @field dimension of symbol
#' @field domainForwarding flag that forces set elements to be recursively 
#' included in all parent sets (i.e., implicit set growth)
#' @field domainLabels column headings for the records dataframe
#' @field domainNames string version of domain names
#' @field domainType none, relaxed or regular depending on state of domain links
#' @field name name of symbol
#' @field numberRecords 	number of symbol records
#' @field records the main symbol records
#' @field refContainer reference to the Container that the symbol belongs to
#' @field shape a list describing the array dimensions if records were
#'  converted with $toDense()
#' @field summary output a list of only the metadata
Parameter <- R6Class(
  "Parameter",
  inherit = .Symbol,
  public = list(
    #' @description There are two different ways to create a GAMS 
    #' parameter and add it to a Container. One is using the 
    #' Parameter constructor and the other is using addParameter 
    #' method which calls the Parameter constructor internally.
    #' addParameter is a Container method to add a Parameter.
    #' @param container A reference to the Container object that the symbol is 
    #' added to
    #' @param name string argument for name of the Parameter
    #' @param domain an optional argument specifying a list of strings, 
    #' a string. default value is NULL.
    #' @param records specify set records as a vector, matrix, array,
    #'  or a dataframe.
    #' @param domainForwarding an optional logical argument to specify 
    #' domain forwarding. Default value is FALSE.
    #' @param description string specifying description for the set
    #' @return a Parameter object
    initialize = function(container=NULL, name=NULL,
                          domain=NULL,records = NULL,
                          domainForwarding = FALSE,
                          description="") {

      type = .gdxSymbolTypes()[["GMS_DT_PAR"]]
      super$initialize(container, name,
                      type, 0, 
                      domain, description, domainForwarding)

      if (!is.null(records)) {
        self$setRecords(records)
      }
    },

    #' main convenience method to set standard dataframe formatted records
    #' @param records specify set records as a vector, matrix, 
    #' array or a dataframe.
    setRecords = function(records) {
      if (inherits(records, c("array", "numeric", "integer"))) { # checks for matrix + arrays + vectors + numbers
        if ((length(records) > 1) && (self$domainType != "regular")) {
          stop(paste0(
            "Data conversion for non-scalar array (i.e., matrix) format into 
            records is only possible for symbols where 
            self$domainType = 'regular'. 
            Must define symbol with specific domain set objects, 
            symbol domainType is currently ",self$domainType,".\n" ))
        }

        for (i in self$domain) {
          if (i$isValid() == FALSE) {
            stop(paste0(
              "Domain set ", i$name, " is invalid and cannot be used to convert array-to-records.
               Use $isValid(verbose = TRUE) to debug this domain set symbol before proceeding.\n"
            ))
          }
        }
        # convert vector and numeric input to an array
        if (inherits(records, c("numeric", "integer"))) {
          records = array(records)
        }

        if (self$dimension >= 1) {
          if (!all(dim(records) == self$shape())) {
            stop(paste0("User passed array/matrix/numeric with shape ", toString(dim(records)), " but anticipated 
            shape was ", toString(self$shape()), " based on domain set information -- 
            must reconcile before array-to-records conversion is possible.\n"))
          }
        }

        tryCatch(
          {
            values = as.numeric(aperm(records))
          },
          error = function(cond) {
            stop("error converting array to numeric type\n")
          },
          warning = function(cond) {
            stop("error converting array to numeric type\n")
          }
        )

        if (self$dimension == 0) {
          if (length(records) > 1) {
            stop("A scalar provided with more than one entries.\n")
          }
          else {
            self$records = data.frame(value=records)
          }
          return()
        }

        #everything from here on is a parameter
        listOfDomains = replicate(self$dimension, list(NA))
        for (i in seq_along(self$domain)) {
          d = self$domain[[i]]
          listOfDomains[[i]] = d$records[,1]
        }
        df = rev(expand.grid(rev(listOfDomains), stringsAsFactors = FALSE)) # ij is a dataframe
        colnames(df) = self$domainLabels
        attr(df, "out.attrs") <- NULL

        df["value"] = values
        # drop zeros but not EPS
        colrange = (self$dimension + 1):length(df)

        logicalVector = ((df[,colrange] == 0) & 
        !(sign(1/df[,colrange])==-1) )
        df = df[(!logicalVector),]

        row.names(df) <- NULL
        if (nrow(df) == 0) {
          self$records = NULL
        }
        else {
          self$records = df
          self$.linkDomainCategories()
        }
      }
      else {
        # check if records is a dataframe and make if not
        records = data.frame(records)

        # check dimensionality of dataframe
        r = nrow(records)
        c = length(records)

        if (c != (self$dimension + 1)) {
          stop(paste0("Dimensionality of records ", c - 1, 
          " is inconsistent with parameter domain specification ", 
          self$dimension))
        }

        columnNames = self$domainLabels
        columnNames = append(columnNames, "value")

        #if records "value" is not numeric, stop.
        val_column = records[,length(records)]
        if (!(is.numeric(val_column) || all(SpecialValues$isNA(val_column)))) {
            stop("All entries in the 'values' column of a parameter 
            must be numeric.\n")
        }

        if (self$dimension == 0) {
          colnames(records) = columnNames
          self$records = records
          return()
        }

        records[, 1:self$dimension] = lapply(seq_along(self$domain), 
        function(d) {
          if (is.factor(records[, d])) {
            levels(records[, d]) = trimws(levels(records[, d]), which="right")
          }
          else {
            records[, d] = factor(records[, d], levels = 
            unique(records[, d]), ordered=TRUE)
            levels(records[, d]) = trimws(levels(records[, d]), which="right")
          }
          return(records[, d])
        })

        records = data.frame(records)
        colnames(records) = columnNames
        self$records = records
      }
    },

    # par
    equals = function(other, checkUELs=TRUE, 
    checkMetaData=TRUE, rtol=0, atol=0,
    verbose=FALSE) {
      super$.check_equals_common_args(other, checkUELs,
      checkMetaData, verbose)

      super$.check_equals_numeric_args(atol, rtol)

      super$equals(other, checkUELs=checkUELs,
      checkMetaData=checkMetaData,rtol=rtol, atol=atol,
      verbose=verbose)
    },

    generateRecords = function(density = 1, func=NULL, seed=NULL) {
      if(!((self$domainType == "regular") || (self$dimension == 0))) {
        stop("Cannot generate records for the symbol unless the symbol has 
        domain objects for all dimension, i.e., <symbol>$domainType == 'regular'
        or the symbol is a scalar\n")
      }

      if (!is.null(seed)) {
        if (!(is.numeric(seed) && round(seed) == seed)) {
          stop("The argument `seed` must be an integer\n")
        }
        set.seed(seed)
      }

      if (!(is.function(func) || is.null(func) || inherits(func, "list"))) {
        "The argument `func` must be of type function or NULL\n"
      }

      if (self$dimension != 0) {
        recs = super$.generate_records_index(density)
      }
      else {
        recs = data.frame(1)
      }

      tryCatch(
      {
        if (is.null(func)) {
          recs$value = runif(n = nrow(recs))
        }
        else {
          recs$value = func(size = nrow(recs))
        }
      },
      error = function(e) {
          message(paste0(e, "\n"))
      }
      )

      private$.records = recs
      set.seed(NULL)
    }
  ),

  active = list(
    isScalar = function(isScalar_input) {
      if (missing(isScalar_input)) {
        if (length(self$domain) == 0) {
          return(TRUE)
        }
        else {
          return(FALSE)
        }
      }
    },

    summary = function() {
      return(list(
        "name" = self$name,
        "isScalar" = self$isScalar,
        "domainObjects" = self$domain,
        "domainNames" = self$domainNames,
        "dimension" = self$dimension,
        "description" = self$description,
        "numberRecords" = self$numberRecords,
        "domainType" = self$domainType
      ))
    }
  )
)

#' @title Variable Class
#' @description A class for Variable objects. This class inherits from an abstract 
#' Symbol class.The documentation for methods common to all symbols can be
#' accessed via help(Symbol) or help(BaseSymbol).
#' countEPS, countNA, countNegInf, countPosInf, countUndef, getCardinality,
#' getSparsity, getMaxValue, getMinValue, getMeanValue, getMaxAbsValue,
#' isValid, toDense, whereMax, whereMaxAbs, whereMin.
#' @field description description of symbol
#' @field dimension of symbol
#' @field domainForwarding flag that forces set elements to be recursively 
#' included in all parent sets (i.e., implicit set growth)
#' @field domainLabels column headings for the records dataframe
#' @field domainNames string version of domain names
#' @field domainType none, relaxed or regular depending on state of domain links
#' @field name name of symbol
#' @field numberRecords 	number of symbol records
#' @field records the main symbol records
#' @field refContainer reference to the Container that the symbol belongs to
#' @field shape a list describing the array dimensions if records were
#'  converted with $toDense()
#' @field summary output a list of only the metadata
#' @field type type of variable (string)
Variable <- R6Class(
  "Variable",
  inherit = .Symbol,
  public = list(

    #' @description There are two different ways to create a GAMS Variable and 
    #' add it to a Container. One is using the Variable constructor and 
    #' the other is using addVariable method which calls the Parameter 
    #' constructor internally.
    #' addVariable is a Container method to add a Variable.
    #' @param container A reference to the Container object that the 
    #' symbol is being added to
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
    initialize = function(container = NULL, name = NULL, 
                          type = "free",
                          domain = NULL, records = NULL,
                          domainForwarding = FALSE,
                          description="") {

      self$type = type

      symtype = .gdxSymbolTypes()[["GMS_DT_VAR"]]
      symsubtype = .VarTypeSubtype()[[type]]

      super$initialize(container, name,
                      symtype, symsubtype, 
                      domain, description, domainForwarding)

      if (!is.null(records)) {
        self$setRecords(records)
      }
    },

    #' main convenience method to set standard dataframe formatted records
    #' @param records specify set records as a vector, matrix, 
    #' array or a dataframe.
    setRecords = function(records) {
      # if list containing array or just an array
      # exclude data frame accept everything else
      if (inherits(records, c("list", "array", "numeric", "integer"))) {
        if (is.array(records) || inherits(records, "numeric")){
          records= list(level = records) # default to level
        }

        if (inherits(records, "list")) {
          #check if user attributes are valid
          if (length(intersect(private$.attr(), names(records))) == 0) {
            stop(paste0("Unrecognized user attribute detected in `records`. 
            The attributes must be one of the following", toString(private$.attr()),
            "and must be passed as names of a named list.\n"))
          }
          # check if elements of the list are arrays or numerics
          for (i in length(records)) {
            if (!(is.numeric(records[[i]]) || all(SpecialValues$isNA(records[[i]])))) {
              stop("All elements of the named list `records` must 
              be type numeric.\n")
            }
          }
        }

        # here everything is a named list
        # convert lists with numeric entries to array
        # if vectors, convert them to arrays
        for (i in length(records)) {
          if (inherits(records[[i]], c("numeric", "integer"))) {
            records[[i]] = array(records[[i]])
          }
        }

        # check if all records have equal size
        size1 = dim(records[[1]])
        # size = lapply(records, dim)

        for (i in seq_along(records)) {
          if(!all(dim(records[[i]]) == size1)) {
            stop("array sizes passed into records must be all equal.\n")
          }
        }

        if ((length(records[[1]]) > 1) && (self$domainType != "regular")) {
          stop(paste0(
            "Data conversion for non-scalar array (i.e., matrix) format into 
            records is only possible for symbols where 
            self$domainType = 'regular'. 
            Must define symbol with specific domain set objects, 
            symbol domainType is currently ",self$domainType,".\n" ))
        }

        for (i in self$domain) {
          if (i$isValid() == FALSE) {
            stop(paste0(
              "Domain set ", i$name, " is invalid and cannot be used to convert array-to-records.
               Use $isValid(verbose = TRUE) to debug this domain set symbol before proceeding.\n"
            ))
          }
        }

        if (self$dimension >= 2) {
          for (i in names(records)) {
            recs = records[[i]]
            if (!all(dim(recs) == self$shape())) {
              stop(paste0("User passed array/matrix with shape ", toString(dim(recs)), " but anticipated 
              shape was ", toString(self$shape()), " based on domain set information -- 
              must reconcile before array-to-records conversion is possible.\n"))
            }
          }
        }

        values = list()
        valuenames = names(records)
        for (i in seq_along(records)) {
          tryCatch(
            {
              values[[i]] = as.numeric(records[[i]])
            },
            error = function(cond) {
              stop("error converting array to numeric type\n")
            },
            warning = function(cond) {
              stop("error converting array to numeric type\n")
            }
          )
        }

        if (self$dimension == 0) {
          self$records = data.frame(matrix(nrow=1, ncol=length(private$.attr())))
          colnames(self$records) = private$.attr()

          for (i in seq_along(records)) {
            if (length(records[[i]]) > 1) {
              stop("A scalar provided with more than one entries.\n")
            }
            else {
              self$records[names(records)[[i]]] = records[[i]]
            }
          }
          for (i in private$.attr()) {
            if (is.na(self$records[[i]])) {
              self$records[i] = private$.default_values[[private$.type]][[i]]
            }
          }
          return()
        }

        #everything from here on is a parameter
        listOfDomains = replicate(self$dimension, list(NA))
        for (i in seq_along(self$domain)) {
          d = self$domain[[i]]
          listOfDomains[[i]] = d$records[,1]
        }
        df = expand.grid(listOfDomains, stringsAsFactors = FALSE) # ij is a dataframe
        colnames(df) = self$domainLabels
        attr(df, "out.attrs") <- NULL
        for (i in seq_along(values)) {
          df[valuenames[[i]]] = values[[i]]
        }

        # drop zeros but not EPS
        colrange = (self$dimension + 1):length(df)
        df1 = df[colrange]
        rsum = rowSums(df1)
        iseps = ((df1 == 0) & 
        (sign(1/df1)==-1) )
        iseps_rowsums= rowSums(iseps)
        df = df[which(!(rsum==0 & iseps_rowsums == 0)),]

        row.names(df) <- NULL
        if (nrow(df) == 0) {
          self$records = NULL
        }
        else {
          usr_colnames = colnames(df)
          columnNames = self$domainLabels
          if (self$dimension +  1 > length(usr_colnames)) {
            usr_attr = NULL
          }
          else {
            usr_attr=  usr_colnames[(self$dimension + 1):length(usr_colnames)]
          }
          for (i in setdiff(private$.attr(), usr_attr)) {
            df[i] = private$.default_values[[private$.type]][[i]]
          }

          # reorder columns
          correct_order = c()
          if (self$dimension > 0) {
            correct_order = colnames(df)[(1:self$dimension)]
          }
          correct_order = append(correct_order, private$.attr())
          df = df[, correct_order]

          #rename columns
          columnNames = append(columnNames, private$.attr())
          colnames(df) = columnNames

          self$records = df
          self$.linkDomainCategories()
        }

      }
      else {
        # check if records is a dataframe and make if not
        records = data.frame(records)
        usr_colnames = colnames(records)

        columnNames = self$domainLabels
        if (self$dimension +  1 > length(usr_colnames)) {
          usr_attr = NULL
        }
        else {
          usr_attr=  usr_colnames[(self$dimension + 1):length(usr_colnames)]
        }

        for (i in setdiff(private$.attr(), usr_attr)) {
          records[i] = private$.default_values[[private$.type]][[i]]
        }

        #check dimensionality
        if (length(records) != self$dimension + length(private$.attr())) {
          stop(cat(paste0("Dimensionality of records ", (length(records)-length(private$.attr())),
          " is inconsistent with the variable domain specification ", 
          self$dimension, " must resolve before records can be added\n\n",
          "NOTE:",
          "columns not named ", toString(private$.attr()),
          " will be interpreted as domain columns, check that the data.frame conforms ",
          "to the required notation.\n",
          "User passed data.frame with columns: ", toString(usr_colnames), "\n")))
        }

        # check if numeric
        for (i in (self$dimension + 1):length(records)) {
          if (!(is.numeric(records[[i]]) || 
          all(SpecialValues$isNA(records[[i]])))) {
            stop(paste0("All elements of the, `" , colnames(records)[i], 
            "` column of `records` not type numeric or NA.\n"))
          }
        }

        # reorder columns
        correct_order = c()
        if (self$dimension > 0) {
          correct_order = colnames(records)[(1:self$dimension)]
        }
        correct_order = append(correct_order, private$.attr())
        records = records[, correct_order]

        #rename columns
        columnNames = append(columnNames, private$.attr())

        if (self$dimension == 0) {
          colnames(records) = columnNames
          self$records = records
          return()
        }

        records[, 1:self$dimension] = lapply(seq_along(self$domain), 
        function(d) {
          if (is.factor(records[, d])) {
            levels(records[, d]) = trimws(levels(records[, d]), which="right")
          }
          else {
            records[, d] = factor(records[, d], levels = 
            unique(records[, d]), ordered=TRUE)
            levels(records[, d]) = trimws(levels(records[, d]), which="right")
          }
          return(records[, d])
        })

        records = data.frame(records)
        colnames(records) = columnNames
        self$records = records
        # self$.linkDomainCategories()

      }
    },

    # var.equ
    equals = function(other, columns=NULL, checkUELs=TRUE, 
    checkMetaData=TRUE, rtol=0, atol=0,
    verbose=FALSE) {
      super$.check_equals_common_args(other, checkUELs,
      checkMetaData, verbose)

      super$.checkColumnsArgument(columns)

      super$.check_equals_numeric_args(atol, rtol)

      super$equals(other, columns=columns, checkUELs=checkUELs,
      checkMetaData=checkMetaData,rtol=rtol, atol=atol,
      verbose=verbose)
    },

    generateRecords = function(density = 1, func=NULL, seed=NULL) {
      if(!((self$domainType == "regular") || (self$dimension == 0))) {
        stop("Cannot generate records for the symbol unless the symbol has 
        domain objects for all dimension, i.e., <symbol>$domainType == 'regular'
        or the symbol is a scalar\n")
      }

      if (!is.null(seed)) {
        if (!(is.numeric(seed) && round(seed) == seed)) {
          stop("The argument `seed` must be an integer\n")
        }
        set.seed(seed)
      }

      if (is.function(func)) {
        func = list("level" = func)
      }
      else if (is.null(func)) {
        temp_fun = function(size) {
          return(runif(n=size))
        }
        func = list("level" = temp_fun)
      }
      else if (inherits(func, "list")) {
        attr_names = names(func)
        if (length(intersect(attr_names, private$.attr())) != length(attr_names)) {
          stop(paste0("the names of the named list `func` must be 
          one of the following: ", toString(private$.attr()), "\n"))
        }

        lapply(func, function(f) {
          if (!is.function(f)) {
            stop("All arguments of the named list `func` must be functions\n")
          }
        })
      }
      else {
        "The argument `func` must be of type function, named list, or NULL\n"
      }

      if (self$dimension != 0) {
        recs = super$.generate_records_index(density)
      }
      else {
        recs = data.frame(1)
      }

      tryCatch(
      {
        for (attr in names(func)) {
          recs[[attr]] = func[[attr]](size = nrow(recs))
        }

        # fill missing attributes with default values
        missing_attr = setdiff(private$.attr(), names(func))
        for (m in missing_attr) {
          recs[[m]] = private$.default_values[[private$.type]][[m]]
        }

        # rearrange recs
        all_colnames = colnames(recs)
        if (self$dimension != 0) {
          indx_colnames = all_colnames[1:self$dimension]
          value_colnames = private$.attr()
          correct_colnames = append(indx_colnames, value_colnames)
          recs = recs[correct_colnames]
        }
        else {
          recs = recs[private$.attr()]
        }
      },
      error = function(e) {
          message(paste0(e, "\n"))
      }
      )

      private$.records = recs
      set.seed(NULL)
    }


  ),

  active = list(
    type = function(type_input) {
      if (missing(type_input)) {
        return(private$.type)
      }
      else {
        if (!any(.varTypes == type_input)) {
          stop(cat(paste0("Argument 'type' must be one of the following:\n\n",
          " 1. 'binary' \n",
          " 2. 'integer' \n",
          " 3. 'positive' \n",
          " 4. 'negative' \n",
          " 5. 'free' \n",
          " 6. 'sos1' \n",
          " 7. 'sos2' \n",
          " 8. 'semicont' \n",
          " 9. 'semiint'\n"
          )))
        }

        private$.type = type_input
      }
    },

    summary = function() {
      return(list(
        "name" = self$name,
        "type" = self$type,
        "domainObjects" = self$domain,
        "domainNames" = self$domainNames,
        "dimension" = self$dimension,
        "description" = self$description,
        "numberRecords" = self$numberRecords,
        "domainType" = self$domainType
      ))
    }
  ),

  private = list(
    .type= NULL,

    .default_values = list(
      "binary" = list(
          "level"= 0.0,
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
  )
  )

#' @title Equation Class
#' @description A class for Equation objects. This class inherits from an abstract 
#' symbol class.The documentation for methods common to all symbols can be
#' accessed via help(Symbol) or help(BaseSymbol).
#' countEPS, countNA, countNegInf, countPosInf, countUndef, getCardinality,
#' getSparsity, getMaxValue, getMinValue, getMeanValue, getMaxAbsValue,
#' isValid, toDense, whereMax, whereMaxAbs, whereMin.
#' @field description description of symbol
#' @field dimension of symbol
#' @field domainForwarding flag that forces set elements to be recursively 
#' included in all parent sets (i.e., implicit set growth)
#' @field domainLabels column headings for the records dataframe
#' @field domainNames string version of domain names
#' @field domainType none, relaxed or regular depending on state of domain links
#' @field name name of symbol
#' @field numberRecords 	number of symbol records
#' @field records the main symbol records
#' @field refContainer reference to the Container that the symbol belongs to
#' @field shape a list describing the array dimensions if records were
#'  converted with $toDense()
#' @field summary output a list of only the metadata
#' @field type type of variable (string)
Equation <- R6Class(
  "Equation",
  inherit = .Symbol,
  public = list(

    #' @description There are two different ways to create a GAMS Equation and 
    #' add it to a Container. One is using the Equation constructor and 
    #' the other is using addEquation method which calls the Equation 
    #' constructor internally.
    #' addEquation is a Container method to add a Equation.
    #' @param container A reference to the Container object that the symbol 
    #' is being added to
    #' @param name string argument for name of the Equation
    #' @param type Type of equation being created [eq (or E/e), geq 
    #' (or G/g), leq (or L/l), nonbinding (or N/n), external (or X/x)]
    #' @param domain an optional argument specifying a list of strings, 
    #' a string. default value is NULL.
    #' @param records specify set records as a vector or a dataframe.
    #' @param domainForwarding an optional logical argument to specify 
    #' domain forwarding. Default value is FALSE.
    #' @param description string specifying description for the set
    #' @return a Equation object
    initialize = function(container=NULL, name=NULL, 
                          type=NULL,
                          domain=NULL,
                          records = NULL,
                          domainForwarding=FALSE,
                          description="") {

      self$type = type
      # call from outside
      type = .EquationTypes[[type]]

      symtype = .gdxSymbolTypes()[["GMS_DT_EQU"]]
      symsubtype = .EqTypeSubtype()[[type]]


      super$initialize(container, name,
                      symtype, symsubtype, 
                      domain, description, domainForwarding)
      if (!is.null(records)) {
        self$setRecords(records)
      }
    },

    #' main convenience method to set standard dataframe formatted records
    #' @param records specify set records as a vector, matrix, 
    #' array or a dataframe.
    setRecords = function(records) {
      if (inherits(records, c("list", "array", "numeric", "integer"))) {

        if (is.array(records) || inherits(records, "numeric")){
          records= list(level = records) # default to level
        }

        if (inherits(records, "list")) {
          #check if user attributes are valid
          if (length(intersect(private$.attr(), names(records))) == 0) {
            stop(paste0("Unrecognized user attribute detected in `records`. 
            The attributes must be one of the following", toString(private$.attr()),
            "and must be passed as names of a named list.\n"))
          }
          # check if elements of the list are arrays or numerics
          for (i in length(records)) {
            if (!(is.numeric(records[[i]]) || all(SpecialValues$isNA(records[[i]])))) {
              stop("All elements of the named list `records` must 
              be type numeric.\n")
            }
          }
        }

        # here everything is a named list
        # convert lists with numeric entries to array
        # if vectors, convert them to arrays
        for (i in length(records)) {
          if (inherits(records[[i]], c("numeric", "integer"))) {
            records[[i]] = array(records[[i]])
          }
        }

        # check if all records have equal size
        size1 = dim(records[[1]])
        # size = lapply(records, dim)

        for (i in seq_along(records)) {
          if(!all(dim(records[[i]]) == size1)) {
            stop("array sizes passed into records must be all equal.\n")
          }
        }

        if ((length(records[[1]]) > 1) && (self$domainType != "regular")) {
          stop(paste0(
            "Data conversion for non-scalar array (i.e., matrix) format into 
            records is only possible for symbols where 
            self$domainType = 'regular'. 
            Must define symbol with specific domain set objects, 
            symbol domainType is currently ",self$domainType,".\n" ))
        }

        for (i in self$domain) {
          if (i$isValid() == FALSE) {
            stop(paste0(
              "Domain set ", i$name, " is invalid and cannot be used to convert array-to-records.
               Use $isValid(verbose = TRUE) to debug this domain set symbol before proceeding.\n"
            ))
          }
        }

        if (self$dimension >= 2) {
          for (i in names(records)) {
            recs = records[[i]]
            if (!all(dim(recs) == self$shape())) {
              stop(paste0("User passed array/matrix with shape ", toString(dim(recs)), " but anticipated 
              shape was ", toString(self$shape()), " based on domain set information -- 
              must reconcile before array-to-records conversion is possible.\n"))
            }
          }
        }

        values = list()
        valuenames = names(records)

        for (i in seq_along(records)) {
          tryCatch(
            {
              values[[i]] = as.numeric(records[[i]])
            },
            error = function(cond) {
              stop("error converting array to numeric type\n")
            },
            warning = function(cond) {
              stop("error converting array to numeric type\n")
            }
          )
        }
        if (self$dimension == 0) {
          self$records = data.frame(matrix(nrow=1, ncol=length(private$.attr())))
          colnames(self$records) = private$.attr()

          for (i in seq_along(records)) {
            if (length(records[[i]]) > 1) {
              stop("A scalar provided with more than one entries.\n")
            }
            else {
              self$records[names(records)[[i]]] = records[[i]]
            }
          }
          for (i in private$.attr()) {
            if (is.na(self$records[[i]])) {
              self$records[i] = private$.default_values[[private$.type]][[i]]
            }
          }
          return()
        }

        #everything from here on is a parameter
        listOfDomains = replicate(self$dimension, list(NA))
        for (i in seq_along(self$domain)) {
          d = self$domain[[i]]
          listOfDomains[[i]] = d$records[,1]
        }
        df = expand.grid(listOfDomains, stringsAsFactors = FALSE) # ij is a dataframe
        colnames(df) = self$domainLabels
        attr(df, "out.attrs") <- NULL
        for (i in seq_along(values)) {
          df[valuenames[[i]]] = values[[i]]
        }

        # drop zeros but not EPS
        colrange = (self$dimension + 1):length(df)
        df1 = df[colrange]
        rsum = rowSums(df1)
        iseps = ((df1 == 0) & 
        (sign(1/df1)==-1) )
        iseps_rowsums= rowSums(iseps)
        df = df[which(!(rsum==0 & iseps_rowsums == 0)),]

        row.names(df) <- NULL

        if (nrow(df) == 0) {
          self$records = NULL
        }
        else {
          usr_colnames = colnames(df)

          columnNames = self$domainLabels
          if (self$dimension +  1 > length(usr_colnames)) {
            usr_attr = NULL
          }
          else {
            usr_attr=  usr_colnames[(self$dimension + 1):length(usr_colnames)]
          }

          for (i in setdiff(private$.attr(), usr_attr)) {
            df[i] = private$.default_values[[private$.type]][[i]]
          }

          # reorder columns
          correct_order = c()
          if (self$dimension > 0) {
            correct_order = colnames(df)[(1:self$dimension)]
          }
          correct_order = append(correct_order, private$.attr())
          df = df[, correct_order]

          #rename columns
          columnNames = append(columnNames, private$.attr())
          colnames(df) = columnNames

          self$records = df
          self$.linkDomainCategories()
        }

      }
      else {
        # check if records is a dataframe and make if not
        records = data.frame(records)

        usr_colnames = colnames(records)
        columnNames = self$domainLabels

        if (self$dimension +  1 > length(usr_colnames)) {
          usr_attr = NULL
        }
        else {
          usr_attr=  usr_colnames[(self$dimension + 1):length(usr_colnames)]
        }

        usr_attr=  usr_colnames[(self$dimension + 1):length(usr_colnames)]

        for (i in setdiff(private$.attr(), usr_attr)) {
          records[i] = private$.default_values[[private$.type]][[i]]
        }

        #check dimensionality
        if (length(records) != self$dimension + length(private$.attr())) {
          stop(cat(paste0("Dimensionality of records ", (length(records)-length(private$.attr())),
          " is inconsistent with equation domain specification ", 
          self$dimension, " must resolve before records can be added\n\n",
          "NOTE:",
          "columns not named ", toString(private$.attr()),
          " will be interpreted as domain columns, check that the data.frame conforms ",
          "to the required notation.\n",
          "User passed data.frame with columns: ", toString(usr_colnames), "\n")))
        }

        # check if numeric
        for (i in (self$dimension + 1):length(records)) {
          if (!(is.numeric(records[[i]]) || 
          all(SpecialValues$isNA(records[[i]])))) {
            stop(paste0("All elements of the, `" , colnames(records)[i], 
            "` column of `records` not type numeric or NA.\n"))
          }
        }

        # reorder columns
        correct_order = c()
        if (self$dimension > 0) {
          correct_order = colnames(records)[(1:self$dimension)]
        }
        correct_order = append(correct_order, private$.attr())
        records = records[, correct_order]

        columnNames = append(columnNames, private$.attr())

        if (self$dimension == 0) {
          colnames(records) = columnNames
          self$records = records
          return()
        }

        records[, 1:self$dimension] = lapply(seq_along(self$domain), 
        function(d) {
          if (is.factor(records[, d])) {
            levels(records[, d]) = trimws(levels(records[, d]), which="right")
          }
          else {
            records[, d] = factor(records[, d], levels = 
            unique(records[, d]), ordered=TRUE)
            levels(records[, d]) = trimws(levels(records[, d]), which="right")
          }
          return(records[, d])
        })

        records = data.frame(records)
        colnames(records) = columnNames
        self$records = records
        # self$.linkDomainCategories()

      }
    },

    # var.equ
    equals = function(other, columns=NULL, checkUELs=TRUE, 
    checkMetaData=TRUE, rtol=0, atol=0,
    verbose=FALSE) {
      super$.check_equals_common_args(other, checkUELs,
      checkMetaData, verbose)

      super$.checkColumnsArgument(columns)

      super$.check_equals_numeric_args(atol, rtol)

      super$equals(other, columns=columns, checkUELs=checkUELs,
      checkMetaData=checkMetaData,rtol=rtol, atol=atol,
      verbose=verbose)
    },

    generateRecords = function(density = 1, func=NULL, seed=NULL) {
      if(!((self$domainType == "regular") || (self$dimension == 0))) {
        stop("Cannot generate records for the symbol unless the symbol has 
        domain objects for all dimension, i.e., <symbol>$domainType == 'regular'
        or the symbol is a scalar\n")
      }

      if (!is.null(seed)) {
        if (!(is.numeric(seed) && round(seed) == seed)) {
          stop("The argument `seed` must be an integer\n")
        }
        set.seed(seed)
      }

      if (is.function(func)) {
        func = list("level" = func)
      }
      else if (is.null(func)) {
        temp_fun = function(size) {
          return(runif(n=size))
        }
        func = list("level" = temp_fun)
      }
      else if (inherits(func, "list")) {
        attr_names = names(func)
        if (length(intersect(attr_names, private$.attr())) != length(attr_names)) {
          stop(paste0("the names of the named list `func` must be 
          one of the following: ", toString(private$.attr()), "\n"))
        }

        lapply(func, function(f) {
          if (!is.function(f)) {
            stop("All arguments of the named list `func` must be functions\n")
          }
        })
      }
      else {
        "The argument `func` must be of type function, named list, or NULL\n"
      }

      if (self$dimension != 0) {
        recs = super$.generate_records_index(density)
      }
      else {
        recs = data.frame(1)
      }

      tryCatch(
      {
        for (attr in names(func)) {
          recs[[attr]] = func[[attr]](size = nrow(recs))
        }

        # fill missing attributes with default values
        missing_attr = setdiff(private$.attr(), names(func))
        for (m in missing_attr) {
          recs[[m]] = private$.default_values[[private$.type]][[m]]
        }

        # rearrange recs
        # rearrange recs
        all_colnames = colnames(recs)
        if (self$dimension != 0) {
          indx_colnames = all_colnames[1:self$dimension]
          value_colnames = private$.attr()
          correct_colnames = append(indx_colnames, value_colnames)
          recs = recs[correct_colnames]
        }
        else {
          recs = recs[private$.attr()]
        }
      },
      error = function(e) {
          message(paste0(e, "\n"))
      }
      )

      private$.records = recs
      set.seed(NULL)
    }
  ),

  active = list(
    type = function(type_input) {
      if (missing(type_input)) {
        return(private$.type)
      }
      else {
        if (!any(.EquationTypes == type_input)) {
          stop(cat(paste0("Argument 'type' must be one of the following:\n\n",
              "1. 'eq', 'E', or 'e' -- equality\n",
              "2. 'geq', 'G', or 'g' -- greater than or equal to inequality\n",
              "3. 'leq', 'L', or 'l'  -- less than or equal to inequality\n",
              "4. 'nonbinding', 'N', or 'n'  -- nonbinding relationship\n",
              "5. 'cone', 'C', or 'c' -- cone equation\n",
              "6. 'external', 'X', or 'x' -- external equation\n",
              "7. 'boolean', 'B', or 'b' -- boolean equation\n"
          )))
        }

        private$.type = type_input
      }
    },

    summary = function() {
      return(list(
        "name" = self$name,
        "type" = self$type,
        "domainObjects" = self$domain,
        "domainNames" = self$domainNames,
        "dimension" = self$dimension,
        "description" = self$description,
        "numberRecords" = self$numberRecords,
        "domainType" = self$domainType
      ))
    }
  ),
  private = list(
    .type = NULL,

    .default_values = list(
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
  )
)

.BaseAlias <- R6Class(
  ".BaseAlias",
  public = list(
    .gams_type = NULL,
    .gams_subtype = NULL,
    .isUniverseAlias = NULL,
    .requiresStateCheck = NULL,

    #' @description There are two different ways to create a GAMS Alias and 
    #' add it to a Container. One is using the Alias constructor and 
    #' the other is using addAlias method which calls the Alias 
    #' constructor internally.
    #' addAlias is a Container method to add a Alias.
    #' @param container A reference to the Container object that the symbol 
    #' is being added to
    #' @param name string argument for name of the Alias
    #' @param aliasFor string argument for the set/alias we want to add
    #' an alias for
    initialize = function(container=NULL, name=NULL) {
      self$.requiresStateCheck = TRUE
      self$refContainer = container
      self$name = name
      refcontainer = self$refContainer
      refcontainer[name] = self
      self$.gams_type = .gdxSymbolTypes()[["GMS_DT_ALIAS"]]
      self$.gams_subtype = 1
    }
  ),

  active = list(
    refContainer = function(ref_container_input) {
      if (missing(ref_container_input)) {
        return(private$.ref_container)
      }
      else {
        if (is.null(ref_container_input)) {
          private$.ref_container = NULL
          self$.requiresStateCheck = TRUE
          return()
        }
        if (!inherits(ref_container_input, "Container")) {
          stop("Symbol 'container' must be type Container\n")
        }
        if (is.null(self$refContainer)){
          if (!identical(self$refContainer, ref_container_input)) {
            self$.requiresStateCheck = TRUE
          }
          private$.ref_container = ref_container_input
        }
        else {
          self$.requiresStateCheck = TRUE
          private$.ref_container = ref_container_input
        }
      }
    },

    name = function(name_input) {
      if (missing(name_input)) {
        return(private$.name)
      }
      else {
        private$.testRefContainer()
        if (!is.character(name_input)) {
          stop("GAMS symbol 'name' must be type chracter\n")
        }

        if (nchar(name_input) > private$symbolMaxLength) {
          stop(paste0("GAMS symbol 'name' is too long,",
          " max is ", private$symbolMaxLength, " characters"))
        }

        if (self$refContainer$hasSymbols(name_input)) {
          stop(paste0("A symbol with the name ", name_input, 
          " already exists in the container\n"))
        }

        if (is.null(private$.name)) {
          self$.requiresStateCheck = TRUE
          private$.name = name_input
        }
        else {
          if(private$.name != name_input) {
            self$.requiresStateCheck = TRUE
          }
          private$.name = name_input
        }

      }
    }
  ),

  private = list(
    .testRefContainer = function() {
      if (!inherits(self$refContainer, "Container")) {
        stop("UniverseAlias/Alias is no longer referring a Container object\n")
      }
    }
  )

)

#' @title Alias Class
#' @description A class for Alias objects.
#' @field aliasWith aliased object
#' @field description description of symbol
#' @field dimension of symbol
#' @field domainForwarding flag that forces set elements to be recursively 
#' included in all parent sets (i.e., implicit set growth)
#' @field domainLabels column headings for the records dataframe
#' @field domainNames string version of domain names
#' @field domainType none, relaxed or regular depending on state of domain links
#' @field isSingleton if symbol is a singleton set
#' @field name name of symbol
#' @field numberRecords 	number of symbol records
#' @field records the main symbol records
#' @field refContainer reference to the Container that the symbol belongs to
#' @field summary output a list of only the metadata
Alias <- R6Class(
  "Alias",
  inherit = .BaseAlias,
  public = list(

    #' @description There are two different ways to create a GAMS Alias and 
    #' add it to a Container. One is using the Alias constructor and 
    #' the other is using addAlias method which calls the Alias 
    #' constructor internally.
    #' addAlias is a Container method to add a Alias.
    #' @param container A reference to the Container object that the symbol 
    #' is being added to
    #' @param name string argument for name of the Alias
    #' @param aliasFor string argument for the set/alias we want to add
    #' an alias for
    initialize = function(container=NULL, name=NULL, 
                          aliasFor=NULL) {
      super$initialize(container, name)
      self$aliasWith = aliasFor
      self$.isUniverseAlias = FALSE
    },

    format = function(...) paste0("GAMS Transfer: R6 object of class Alias. 
    Use ", self$name, "$summary for details"),

    getUELs = function(dimension =NULL, codes=NULL, ignoreUnused = FALSE) {
      super$.testRefContainer()
      private$.testParentSet()
      self$aliasWith$getUELs(dimension, codes, ignoreUnused)
    },

    setUELs = function(uels, dimension=NULL, rename=FALSE) {
      super$.testRefContainer()
      private$.testParentSet()
      self$aliasWith$setUELs(uels, dimension, rename)
    },

    reorderUELs = function(uels, dimension=NULL) {
      super$.testRefContainer()
      private$.testParentSet()
      self$aliasWith$reorderUELs(uels, dimension)
    },

    addUELs = function(uels, dimension=NULL) {
      super$.testRefContainer()
      private$.testParentSet()
      self$aliasWith$addUELs(uels, dimension)
    },

    removeUELs = function(uels=NULL, dimension=NULL) {
      super$.testRefContainer()
      private$.testParentSet()
      self$aliasWith$removeUELs(uels, dimension)
    },

    renameUELs = function(uels, dimension=NULL, allowMerge=FALSE) {
      super$.testRefContainer()
      private$.testParentSet()
      self$aliasWith$renameUELs(uels, dimension, allowMerge)
    },

    #' @description getCardinality get the full cartesian product of the domain
    getCardinality = function() {
      super$.testRefContainer()
      private$.testParentSet()
      return(self$refContainer[self$aliasWith$name]$getCardinality())
    },


    #' @description getSparsity get the sparsity of the symbol 
    #' w.r.t the cardinality
    getSparsity = function() {
      super$.testRefContainer()
      private$.testParentSet()
      return(self$refContainer[self$aliasWith$name]$getSparsity())
    },

    #' @description TRUE if the symbol is in a valid format, 
    #' throw exceptions if verbose=True, recheck a symbol if force=True
    #' @param verbose type logical
    #' @param force type logical
    isValid = function(verbose=FALSE, force=FALSE) {
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
            private$check()
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

    getDomainViolations = function() {
      super$.testRefContainer()
      private$.testParentSet()
      return(self$aliasWith$getDomainViolations())
    },

    findDomainViolations = function() {
      super$.testRefContainer()
      private$.testParentSet()
      return(self$aliasWith$findDomainViolations())
    },

    hasDomainViolations = function() {
      super$.testRefContainer()
      private$.testParentSet()
      return(self$aliasWith$hasDomainViolations())
    },

    countDomainViolations = function() {
      super$.testRefContainer()
      private$.testParentSet()
      return(self$aliasWith$countDomainViolations())
    },

    dropDomainViolations = function() {
      super$.testRefContainer()
      private$.testParentSet()
      return(self$aliasWith$dropDomainViolations())
    },

    countDuplicateRecords = function() {
      super$.testRefContainer()
      private$.testParentSet()

      return(self$aliasWith$countDuplicateRecords())
    },

    findDuplicateRecords = function(keep="first") {
      super$.testRefContainer()
      private$.testParentSet()

      return(self$aliasWith$findDuplicateRecords(keep))
    },

    hasDuplicateRecords = function() {
      super$.testRefContainer()
      private$.testParentSet()

      return(self$aliasWith$hasDuplicateRecords())
    },

    dropDuplicateRecords = function(keep="first") {
      super$.testRefContainer()
      private$.testParentSet()

      return(self$aliasWith$dropDuplicateRecords(keep))
    },

    #' main convenience method to set standard dataframe formatted records
    #' @param records specify set records as a string vector or a dataframe.
    setRecords = function(records) {
      super$.testRefContainer()
      private$.testParentSet()
      return(self$refContainer[self$aliasWith$name]$setRecords(records))
    },

    # set/alias
    equals = function(other, checkUELs=TRUE, 
    checkElementText=TRUE, checkMetaData=TRUE,
    verbose=FALSE) {
      super$.testRefContainer()
      private$.testParentSet()

      self$aliasWith$equals(other, checkUELs=checkUELs,
      checkElementText=checkElementText,
      checkMetaData=checkMetaData, verbose=verbose)
    },

    generateRecords = function(density = 1) {
      super$.testRefContainer()
      private$.testParentSet()

      self$aliasWith$generateRecords(density)
    }
  ),

  active = list(

    aliasWith = function(alias_with_input) {
      if (missing(alias_with_input)) {
        return(private$.aliasWith)
      }
      else {
        super$.testRefContainer()
        if ((inherits(alias_with_input, "UniverseAlias"))) {
          stop("GAMS 'aliasWith' cannot be a UniverseAlias. Create a new UniverseAlias symbol instead\n")
        }
        if (!(inherits(alias_with_input, c("Set", "Alias")))) {
          stop("GAMS 'aliasWith' must be type Set or Alias\n")
        }

        if (inherits(alias_with_input, "Alias")) {
          parent = alias_with_input
          while (!inherits(parent, "Set")) {
            parent = parent$aliasWith
          }
          alias_with_input = parent
        }
        if (is.null(private$.aliasWith)) {
          private$.aliasWith = alias_with_input
        }
        else {
          if (!identical(private$.aliasWith, alias_with_input)) {
            self$.requiresStateCheck = TRUE
            self$refContainer$.requiresStateCheck = TRUE
            private$.aliasWith = alias_with_input
          }
        }
      }
    },

    isSingleton = function(is_singleton) {
      super$.testRefContainer()
      private$.testParentSet()
      if (missing(is_singleton)) {
        refcontainer = self$refContainer
        sym = refcontainer[self$aliasWith$name]
        return(sym$isSingleton)
      }
      else {
        refcontainer = self$refContainer
        sym = refcontainer[self$aliasWith$name]
        sym$isSingleton = is_singleton
      }
    },

    description = function(description_input) {
      super$.testRefContainer()
      private$.testParentSet()
      if (missing(description_input)) {
        refcontainer = self$refContainer
        aliaswithname = self$aliasWith$name
        sym = refcontainer[aliaswithname]
        return(sym$description)
      }
      else {
        refcontainer = self$refContainer
        aliaswithname = self$aliasWith$name
        sym = refcontainer[aliaswithname]
        sym$description = description_input
      }
    },

    dimension = function(dimension_input) {
      super$.testRefContainer()
      private$.testParentSet()
      if (missing(dimension_input)) {
        return(self$refContainer[self$aliasWith$name]$dimension)
      }
      else {
        refcontainer = self$refContainer
        sym = refcontainer[self$aliasWith$name]
        sym$dimension = dimension_input
      }
    },

    records = function(records_input) {
      super$.testRefContainer()
      private$.testParentSet()
      if (missing(records_input)) {
        return(self$refContainer[self$aliasWith$name]$records)
      }
      else {
        self$refContainer[self$aliasWith$name]$records = records_input
      }

    },

    domain = function(domain_input) {
      super$.testRefContainer()
      private$.testParentSet()
      if (missing(domain_input)) {
        return(self$refContainer[self$aliasWith$name]$domain)
      }
      else {
        refcontainer = self$refContainer
        sym = refcontainer[self$aliasWith$name]
        sym$domain = domain_input
      }
    },

    numberRecords = function() {
      super$.testRefContainer()
      private$.testParentSet()
      return(self$refContainer[self$aliasWith$name]$numberRecords)
    },

    domainType = function() {
      super$.testRefContainer()
      private$.testParentSet()
      return(self$refContainer[self$aliasWith$name]$domainType)
    },

    domainNames = function() {
      super$.testRefContainer()
      private$.testParentSet()
      return(self$refContainer[self$aliasWith$name]$domainNames)
    },

    domainLabels = function() {
      super$.testRefContainer()
      private$.testParentSet()
      return(self$refContainer[self$aliasWith$name]$domainLabels)
    },

    summary = function() {
    super$.testRefContainer()
    private$.testParentSet()
    return(list(
      "name" = self$name,
      "aliasWith" = self$aliasWith,
      "aliasWith_name" = self$aliasWith$name,
      "isSingleton" = self$isSingleton,
      "domainObjects" = self$domain,
      "domainNames" = self$domainNames,
      "dimension" = self$dimension,
      "description" = self$description,
      "numberRecords" = self$numberRecords,
      "domainType" = self$domainType
    ))
    }
  ),

  private = list(
    symbolMaxLength = 63,
    .ref_container = NULL,
    .name = NULL,
    .aliasWith = NULL,

    check = function() {
      if (self$.requiresStateCheck == TRUE) {
        if (self$refContainer[self$aliasWith$name]$isValid() == FALSE) {
          stop(paste0("Alias is not valid because parent set ", self$aliasWith$name,
          "is not valid\n"))
        }
      }
    },

    .testParentSet = function() {
      if (!self$refContainer$hasSymbols(self$aliasWith$name)) {
        stop(paste0("Parent set ", self$aliasWith$name, " of alias ", 
        self$name, " is no longer in the container and cannot 
        be referenced\n"))
      }
    }
  )
)


#' @title Alias Class
#' @description A class for Alias objects.
#' @field aliasWith aliased object
#' @field description description of symbol
#' @field dimension of symbol
#' @field domainForwarding flag that forces set elements to be recursively 
#' included in all parent sets (i.e., implicit set growth)
#' @field domainLabels column headings for the records dataframe
#' @field domainNames string version of domain names
#' @field domainType none, relaxed or regular depending on state of domain links
#' @field isSingleton if symbol is a singleton set
#' @field name name of symbol
#' @field numberRecords 	number of symbol records
#' @field records the main symbol records
#' @field refContainer reference to the Container that the symbol belongs to
#' @field summary output a list of only the metadata
UniverseAlias <- R6Class(
  "UniverseAlias",
  inherit = .BaseAlias,
  public = list(

    #' @description There are two different ways to create a GAMS Alias and 
    #' add it to a Container. One is using the Alias constructor and 
    #' the other is using addAlias method which calls the Alias 
    #' constructor internally.
    #' addAlias is a Container method to add a Alias.
    #' @param container A reference to the Container object that the symbol 
    #' is being added to
    #' @param name string argument for name of the Alias
    #' @param aliasFor string argument for the set/alias we want to add
    #' an alias for
    initialize = function(container=NULL, name=NULL) {
      super$initialize(container, name)
      private$.aliasWith = "*"
      lockBinding("aliasWith", self)
      self$.isUniverseAlias = TRUE
    },

    format = function(...) paste0("GAMS Transfer: R6 object of class UniverseAlias. 
    Use ", self$name, "$summary for details"),

    getUELs = function(ignoreUnused = FALSE) {
      if (self$isValid()) {
        return(self$refContainer$getUELs(ignoreUnused = ignoreUnused))
      }
      else {
        return(NULL)
      }
    },

    #' @description getCardinality get the full cartesian product of the domain
    getCardinality = function() {
      return(nrow(self$records))
    },


    #' @description getSparsity get the sparsity of the symbol 
    #' w.r.t the cardinality
    getSparsity = function() {
      return(0)
    },

    #' @description TRUE if the symbol is in a valid format, 
    #' throw exceptions if verbose=True, recheck a symbol if force=True
    #' @param verbose type logical
    #' @param force type logical
    isValid = function(verbose=FALSE, force=FALSE) {
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
            private$check()
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

    equals = function(other,checkMetaData=TRUE,
    verbose=FALSE) {
      # mandatory checks
      if (!self$isValid()) {
        stop(paste0("Cannot compare objects because ", s$name, " is not valid. 
        Use ", s$name, "$isValid(verbose=TRUE) to get more details\n"))
      }

      if (!inherits(other, c(".Symbol", ".BaseAlias"))) {
        stop("The argument `other` must be a Symbol object")
      }

      if (!other$isValid()) {
        stop(paste0("Cannot compare objects because ", other$name, " is invalid. Use ",
        other$name, "$isValid(verbose=TRUE) to debug.\n"))
      }

      if (inherits(other, "Alias")) {
        other = other$aliasWith
      }

      if (!is.logical(checkMetaData)) {
        stop("The argument `checkMetaData` must be type logical")
      }

      tryCatch(
        {
          if (checkMetaData) {
            if (self$name != other$name) {
              stop("Symbol names do not match ", 
              self$name, " != ", other$name, "\n" )
            }
          }
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
  ),

  active = list(

    aliasWith = function(alias_with_input) {
      if (missing(alias_with_input)) {
        return(private$.aliasWith)
      }
    },

    isSingleton = function(is_singleton) {
      return(FALSE)
    },

    description = function(description_input) {
      return("Aliased with *")
    },

    dimension = function(dimension_input) {
      return(1)
    },

    records = function(records_input) {
      if (!self$isValid()) return(NULL)
      df = data.frame(self$refContainer$getUELs())
      colnames(df) = "*"
      return(df)
    },

    domain = function(domain_input) {
      return("*")
    },

    numberRecords = function() {
      if (!self$isValid()) return(NA)

      return(nrow(self$records))
    },

    domainType = function() {
      return("none")
    },

    domainNames = function() {
      return("*")
    },

    domainLabels = function() {
      return("*")
    },

    summary = function() {
    return(list(
      "name" = self$name,
      "aliasWith_name" = self$aliasWith,
      "domainNames" = self$domainNames,
      "dimension" = self$dimension,
      "description" = self$description,
      "numberRecords" = self$numberRecords,
      "domainType" = self$domainType
    ))
    }
  ),

  private = list(
    symbolMaxLength = 63,
    .ref_container = NULL,
    .name = NULL,
    .aliasWith = NULL,

    check = function() {
      if (self$.requiresStateCheck == TRUE) {
        super$.testRefContainer()
      }
    },

    .testParentSet = function() {
      if (!self$refContainer$hasSymbols(self$aliasWith$name)) {
        stop(paste0("Parent set ", self$aliasWith$name, " of alias ", 
        self$name, " is no longer in the container and cannot 
        be referenced\n"))
      }
    }
  )
)

#' @title ConstContainer Class
#' @description ConstContainer class is a data-focused read-only object that 
#' will provide a snapshot of the data target being read. The ConstContainer 
#' can be created by reading a GDX file. This class is specially useful for 
#' the users who are only interested in post-processing data from a GAMS model 
#' run. The ConstContainer class inherits from an abstract
#' BaseContainer class. To access the functions common to Container and
#' ConstContainer, please use help(BaseContainer).
#' @field data is a named list containing all symbol data
#' @field systemDirectory is the path to GAMS System directory
#' @export
ConstContainer <- R6::R6Class (
  "ConstContainer",
  inherit = .BaseContainer,
  public = list(
    #' @description
    #' Create a new ConstContainer simply by initializing an object.
    #' @param systemDirectory optional argument for the absolute path to 
    #' GAMS system directory
    #' @examples
    #' ConstContainer$new()
    initialize = function(loadFrom=NULL, systemDirectory=NULL) {

      super$initialize(systemDirectory)

      if (!missing(loadFrom)) {
      self$read(loadFrom, records = FALSE)

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
      if (!is.logical(records)) {
        stop("records must be type logical\n")
      }

      if (!(is.character(symbols)) && !(is.list(symbols)) && !(is.null(symbols))) {
        stop("argument symbols must be of the type list or string\n")
      }

      if (is.list(symbols)) {
        if (!all(unlist(lapply(symbols, is.character)))) {
          stop("argument symbols must contain only type string\n")
        }
        # convert symbols to a vector
        symbols = unlist(symbols)
      }

      if (!is.character(loadFrom)) {
        stop("The argument loadFrom must be of type string\n")
      }
      else {
        namesplit = strsplit(loadFrom, "\\.")
        ext = tail(unlist(namesplit), 1)
        if (ext != "gdx") {
          stop("check filename extension, must be .gdx\n")
        }
        loadFrom = R.utils::getAbsolutePath(path.expand(loadFrom))
        if (!file.exists(loadFrom)) {
          stop(paste0("File ", loadFrom, " doesn't exist\n"))
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
      symbolsToRead = unlist(lapply(readData, "[[", 1))
      # reset data
      self$data = collections::ordered_dict()

      # reset lower case data
      self$.lc_data = collections::dict()

      aliasList = list()
      aliasCount = 0
      for (m in readData) {
        if (m$type == .gdxSymbolTypes()[["GMS_DT_PAR"]]) {
          .ConstParameter$new(
            self, m$name, m$domain, records = NULL,
            description = m$expltext, domaintype= m$domaintype,
            numberRecords=m$numRecs)
        }
        else if (m$type == .gdxSymbolTypes()[["GMS_DT_SET"]]) {
            dt = m$domaintype
            if ((length(m$domain) == 1) && (m$name == m$domain[1])) {
              dt = 2 # for relaxed domain type
            }

            .ConstSet$new(
            self, m$name, m$domain, as.logical(m$subtype),
            records = NULL,
            m$expltext,dt, m$numRecs)
            if (!any(c(0,1) == m$subtype)) {
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
            .ConstVariable$new(
            self, m$name, type, m$domain,
            description = m$expltext, domaintype = m$domaintype,
            numberRecords=m$numRecs)
        }
        else if (m$type == .gdxSymbolTypes()[["GMS_DT_EQU"]]) {
            type = which(.EqTypeSubtype() == m$subtype)
            if (is.integer0(type)) {
              type = "eq"
            }
            else {
              type = names(.EqTypeSubtype())[[type]]
            }

            .ConstEquation$new(
            self, m$name, type, m$domain,
            description = m$expltext, domaintype = m$domaintype,
            numberRecords=m$numRecs)
        }
        else if (m$type == .gdxSymbolTypes()[["GMS_DT_ALIAS"]]) {
            dt = m$domaintype
            if ((length(m$domain) == 1) && (m$name == m$domain[1])) {
              dt = 2 # for relaxed domain type
            }

            if (m$aliasfor == "*") {
              .ConstUniverseAlias$new(self, m$name, m$aliasfor, m$domain,
              m$expltext, dt, m$numRecs
              )
            }
            else {
              .ConstAlias$new(self, m$name, m$aliasfor, m$domain, as.logical(m$subtype),
              m$expltext, dt, m$numRecs)
            }
        }
      }

      # set acronyms to NA
      if (records == TRUE) {

        for (s in readData) {
          if (is.null(s$records)) {
            next
          }
          self[s$name]$setRecords(s$records)

          if (!is.null(self$acronyms)) {
            if (inherits(self[s$name], c(".ConstParameter", 
            ".ConstVariable", ".ConstEquation"))) {
              for (a in self$acronyms) {
                self[s$name]$records[(self[s$name]$records 
                == a * 1e301)] = SpecialValues[["NA"]]
              }
            }
          }
        }
      }
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
        if (!identical(selfsym, othersym)) {
          if (verbose) {
            stop("Symbols named `", s, "` in both containers are not identical")
          }
          else {
            return(FALSE)
          }
        }
      }

      # if didn't return false until here then its true
      return(TRUE)
    }

  )
  )


.ConstSymbol <- R6Class(
  ".ConstSymbol",
  inherit = .BaseSymbol,
  public = list(
    records = NULL,
    description = NULL,
    dimension = NULL,
    domain = NULL,
    refContainer = NULL,
    name = NULL,
    numberRecords = NULL,
    domainType = NULL,
    domainLabels = NULL,
  initialize = function(container, name,
                        type, subtype, 
                        domain,
                        description, domaintype,
                        numberRecords) {

    super$initialize(type, subtype)

    self$refContainer = container
    lockBinding("refContainer", self)

    self$name = name
    lockBinding("name", self)
    container[name] = self

    self$domain = domain
    lockBinding("domain", self)

    self$dimension = length(domain)
    lockBinding("dimension", self)

    self$domainType = private$.domain_type_map[[domaintype]]
    lockBinding("domainType", self)

    self$description = description
    lockBinding("description", self)

    self$numberRecords = numberRecords
    lockBinding("numberRecords", self)

    self$domainLabels = private$.get_domain_labels()
    lockBinding("domainLabels", self)
  },

  getCardinality = function() {
    tryCatch(
      {
        if (length(self$domainNames) == 0) {
          return(NA)
        }
        else {
          for (n in self$domainNames) {
            if (!self$refContainer$hasSymbols(n)) {
              return(NA)
            }
          }
          card = 1
          for (n in self$domainNames) {
            domainSym = self$refContainer[self$refContainer$getSymbolNames(n)]
            card = card * domainSym$numberRecords
          }
          return(card)
        }
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  },

  getSparsity = function() {
    tryCatch(
      {
        if (length(self$domainNames) == 0) {
          return(NA)
        }
        else {
          for (n in self$domainNames) {
            if (!self$refContainer$hasSymbols(n)) {
              return(NA)
            }
          }

          for (n in self$domainNames) {
            return(1 - self$numberRecords/self$getCardinality())
          }
        }
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  }

  ),

  active = list(
    domainNames = function() {
      if (is.null(self$domain) || (length(self$domain) == 0)) {
        return(NA)
      }
      else {
        return(self$domain)
      }
    }
  ),
  private = list(
    symbolMaxLength = 63,
    descriptionMaxLength = 255,
    .domain_type_map = list("none", "relaxed", "regular"),
    .set_records = function(records) {
      self$records = records
      lockBinding("records", self)
    },

    .get_domain_labels = function() {
      dom_is_univ = (self$domain == "*")
      dom_temp = self$domain
      dom_temp[dom_is_univ] = "uni"
      column_names = lapply(seq_along(dom_temp), function(i) {
        return(paste0(dom_temp[[i]], "_", i))
      })

      return(column_names)
    }
  )
)

.ConstSet <- R6Class(
  ".ConstSet",
  inherit = .ConstSymbol,
  public = list(
    isSingleton = NULL,
    initialize = function(container=NULL, name=NULL,
                          domain="*", isSingleton=FALSE,
                          records = NULL,
                          description="", domaintype=NULL,
                          numberRecords=NULL) {
      self$isSingleton = isSingleton
      lockBinding("isSingleton", self)

      type = .gdxSymbolTypes()[["GMS_DT_SET"]]
      if (!isSingleton) {
        subtype = .SetTypeSubtype()[["set"]]
      }
      else {
        subtype = .SetTypeSubtype()[["singleton_set"]]
      }

      super$initialize(container, name,
                      type, subtype,
                      domain, description, domaintype,
                      numberRecords)

      if (!is.null(records)) {
        self$setRecords(records)
      }
      invisible(self)
    },

    setRecords = function(records) {
      records = data.frame(records)
      columnNames = self$domainLabels
      columnNames = append(columnNames, "element_text")
      colnames(records) = columnNames
      super$.set_records(records)
    }
  ),

  active = list(
    summary = function() {
      return(list(
        "name" = self$name,
        "isSingleton" = self$isSingleton,
        "domainNames" = self$domainNames,
        "dimension" = self$dimension,
        "description" = self$description,
        "numberRecords" = self$numberRecords,
        "domainType" = self$domainType
      ))
    }
  ),
  private = list(
    .is_singleton = NULL
  )
  )

.ConstParameter <- R6Class(
  ".ConstParameter",
  inherit = .ConstSymbol,
  public = list(
    isScalar = NULL,
    initialize = function(container=NULL, name=NULL,
                          domain=NULL,records = NULL,
                          description="", domaintype=NULL,
                          numberRecords = NULL) {
      type = .gdxSymbolTypes()[["GMS_DT_PAR"]]
      super$initialize(container, name,
                      type, 0, 
                      domain, description, domaintype,
                      numberRecords)

      if (!is.null(records)) {
        self$setRecords(records)
      }
      self$isScalar = (self$dimension == 0)
    },

    setRecords = function(records) {
      records = data.frame(records)
      columnNames = self$domainLabels
      columnNames = append(columnNames, "value")
      colnames(records) = columnNames
      super$.set_records(records)
    }
  ),

  active = list(
    summary = function() {
      return(list(
        "name" = self$name,
        "isScalar" = self$isScalar,
        "domainNames" = self$domainNames,
        "dimension" = self$dimension,
        "description" = self$description,
        "numberRecords" = self$numberRecords,
        "domainType" = self$domainType
      ))
    }
  )
)

.ConstVariable <- R6Class(
  ".ConstVariable",
  inherit = .ConstSymbol,
  public = list(
    type = NULL,
    initialize = function(container = NULL, name = NULL, 
                          type = "free",
                          domain = NULL, records = NULL,
                          description="", domaintype=NULL,
                          numberRecords=NULL) {
      self$type = type
      lockBinding("type", self)

      symtype = .gdxSymbolTypes()[["GMS_DT_VAR"]]
      symsubtype = .VarTypeSubtype()[[type]]

      super$initialize(container, name,
                      symtype, symsubtype, 
                      domain, description, domaintype,
                      numberRecords)
      if (!is.null(records)) {
        self$setRecords(records)
      }
    },

    setRecords = function(records) {
        records = data.frame(records)
        columnNames = self$domainLabels
        columnNames = append(columnNames, private$.attr())
        colnames(records) = columnNames
        super$.set_records(records)
    }

  ),

  active = list(
    summary = function() {
      return(list(
        "name" = self$name,
        "type" = self$type,
        "domainNames" = self$domainNames,
        "dimension" = self$dimension,
        "description" = self$description,
        "numberRecords" = self$numberRecords,
        "domainType" = self$domainType
      ))
    }
  )
)

.ConstEquation <- R6Class(
  ".ConstEquation",
  inherit = .ConstSymbol,
  public = list(
    type = NULL,
    initialize = function(container=NULL, name=NULL, 
                          type=NULL,
                          domain=NULL,
                          records = NULL,
                          description="", domaintype=NULL,
                          numberRecords = NULL) {
      self$type = type
      lockBinding("type", self)

      # call from outside
      type = .EquationTypes[[type]]

      symtype = .gdxSymbolTypes()[["GMS_DT_EQU"]]
      symsubtype = .EqTypeSubtype()[[type]]

      super$initialize(container, name,
                      symtype, symsubtype, 
                      domain, description, domaintype, numberRecords)
      if (!is.null(records)) {
        self$setRecords(records)
      }
    },

    setRecords = function(records) {
        records = data.frame(records)
        columnNames = self$domainLabels
        columnNames = append(columnNames, private$.attr())
        colnames(records) = columnNames
        super$.set_records(records)
    }
  ),

  active = list(
    summary = function() {
      return(list(
        "name" = self$name,
        "type" = self$type,
        "domainNames" = self$domainNames,
        "dimension" = self$dimension,
        "description" = self$description,
        "numberRecords" = self$numberRecords,
        "domainType" = self$domainType
      ))
    }
  )
)

.ConstAlias <- R6Class(
  ".ConstAlias",
  inherit = .ConstSymbol,
  public = list(
    .gams_type = NULL,
    .gams_subtype = NULL,
    aliasWith = NULL,
    isSingleton = NULL,
    initialize = function(container=NULL, name=NULL, aliasFor=NULL, 
                          domain, isSingleton, 
                          description, domainType, numberRecords
                          ) {
      super$initialize(container, name, .gdxSymbolTypes()[["GMS_DT_ALIAS"]], 
      isSingleton, domain, description, domainType, numberRecords)

      self$aliasWith = aliasFor
      lockBinding("aliasWith", self)

      self$isSingleton = isSingleton
      lockBinding("isSingleton", self)
    },

    getCardinality = function() {
      return(self$refContainer[self$aliasWith]$getCardinality())
    },

    getSparsity = function() {
      return(self$refContainer[self$aliasWith]$getSparsity())
    },

    setRecords = function(records) {
      records = data.frame(records)
      columnNames = self$domainLabels
      columnNames = append(columnNames, "element_text")
      colnames(records) = columnNames
      super$.set_records(records)
    }
  ),

  active = list(
    summary = function() {
    return(list(
      "name" = self$name,
      "aliasWith_name" = self$aliasWith,
      "isSingleton" = self$isSingleton,
      "domainNames" = self$domainNames,
      "dimension" = self$dimension,
      "description" = self$description,
      "numberRecords" = self$numberRecords,
      "domainType" = self$domainType
    ))
    }
  )
)

.ConstUniverseAlias <- R6Class(
  ".ConstUniverseAlias",
  inherit = .ConstSymbol,
  public = list(
    .gams_type = NULL,
    .gams_subtype = NULL,
    aliasWith = NULL,
    isSingleton = NULL,
    domainLabels = NULL,
    initialize = function(container=NULL, name=NULL, aliasFor=NULL, 
                          domain, 
                          description, domainType, numberRecords
                          ) {
      super$initialize(container, name, .gdxSymbolTypes()[["GMS_DT_ALIAS"]], 
      FALSE, domain, description, domainType, numberRecords)

      self$aliasWith = aliasFor
      lockBinding("aliasWith", self)

      self$isSingleton = FALSE
      lockBinding("isSingleton", self)

      unlockBinding("domainLabels", self)
      self$domainLabels = "*"
      lockBinding("domainLabels", self)
    },

    getCardinality = function() {
      return(self$numberRecords)
    },

    getSparsity = function() {
      return(0)
    },

    setRecords = function(records) {
      records = data.frame(records)
      records = subset(records, select=-2)
      columnNames = "*"
      colnames(records) = columnNames
      super$.set_records(records)
    }
  ),

  active = list(
    summary = function() {
    return(list(
      "name" = self$name,
      "aliasWith_name" = self$aliasWith,
      "domainNames" = self$domainNames,
      "dimension" = self$dimension,
      "description" = self$description,
      "numberRecords" = self$numberRecords,
      "domainType" = self$domainType
    ))
    }
  )
)

find_gams <- function() {
  if (Sys.info()['sysname'] == "Windows") {
    gams_exe = "gams.exe"
  }
  else {
    gams_exe = "gams"
  }
  paths = Sys.getenv("PATH")
  paths_split = unlist(strsplit(paths, .Platform$path.sep))
  sysDirPath = NULL
  for (p in paths_split) {
    if (file.exists(paste0(p, .Platform$file.sep, gams_exe))) {
      sysDirPath = p
    }
  }
  if (is.null(sysDirPath)) {
  stop("Could not find a GAMS installation, must manually specify system directory\n")
  }
  return(sysDirPath)
}

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

#' @title BaseContainer Class
#' @description BaseContainer class is an abstract class that is inherited by 
#' the Container class and ConstContainer class.
#' @field data is a named list containing all symbol data
#' @field systemDirectory is the path to GAMS System directory
#' @export
.BaseContainer <- R6::R6Class (
  ".BaseContainer",
  public = list(
    systemDirectory = NULL,
    data = NULL,
    .lc_data = NULL,
    acronyms = NULL,
    #' @description
    #' Create a new ConstContainer simply by initializing an object.
    #' @param systemDirectory optional argument for the absolute path to 
    #' GAMS system directory
    #' @examples
    #' ConstContainer$new()
    initialize = function(systemDirectory=NULL) {

      if (is.null(systemDirectory)) {
        self$systemDirectory = find_gams()

      }
      else {
        if (R.utils::isAbsolutePath(systemDirectory)) {
          self$systemDirectory = systemDirectory
        }
        else {
          stop("must enter valid full (absolute) path to 
          GAMS system directory\n")
        }
      }

      self$acronyms = list()
      self$data = collections::ordered_dict()
      # another dict for lowercase names to original case
      self$.lc_data = collections::dict()
    },

    `[` = function(key) {
      if (self$data$has(key)) {
        return(self$data$get(key))
      }
      else {
        return(invisible(NULL))
      }
    },

    `[<-` = function(key, values) {
      self$data$set(key, values)
      self$.lc_data$set(tolower(key), key)
      return(invisible(self))
    },

    format = function(...) paste0("GAMS Transfer: R6 object of class ", 
    class(self)[1]),

    hasSymbols = function(names) {
      if (!is.character(names)) {
        stop("The argument `names` must be type character\n")
      }

      return(unlist(lapply(names, function(x) self$.lc_data$has(tolower(x))), use.names=FALSE))
    },

    getSymbolNames = function(names) {
      if (!is.character(names)) {
        stop("The argument `names` must be type character\n")
      }
      return(unlist(lapply(names, function(x) {
        if (!self$.lc_data$has(tolower(x))) {
          stop(paste0("Symbol ", x, " does not exist\n"))
        }
        else {
          self$.lc_data$get(tolower(x))
        }
      }), use.names = FALSE))
    },

    #' @description list all symbols in the container if isValid = NULL
    #' list all valid symbols in the container if  isValid = TRUE
    #' list all invalid symbols in the container if isValid = FALSE
    #' @param isValid an optional logical argument
    #' @return a vector of symbols
    listSymbols = function(...) {
      args = list(...)
      isValid = args[["isValid"]]

      if (!is.null(isValid)) {
        if (!is.logical(isValid)) {
          stop("The argument 'isValid' must be type logical\n")
        }

        correct_validity_data = unlist(lapply(unlist(self$data$keys()), function(x) {
          if (self[x]$isValid() == isValid) {
            return(x)
          }
        }), use.names = FALSE)

        return(correct_validity_data)
      }
      else {
        return(unlist(self$data$keys()))
      }
    },

    #' @description list all sets in the container if isValid = NULL
    #' list all valid sets in the container if  isValid = TRUE
    #' list all invalid sets in the container if isValid = FALSE
    #' @param isValid an optional logical argument
    #' @return a vector of symbols
    listSets = function(...) {
      args = list(...)
      isValid = args[["isValid"]]

      if (!(is.logical(isValid) || is.null(isValid))) {
        stop("Argument `isValid` must be type logical or NULL \n")
      }
      sets = NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self[s], c("Set", ".ConstSet")) ) {
          if (is.null(sets)) {
            sets = s
          }
          else {
            sets = append(sets, s)
          }
        }
      }
      return(sets)
    },

    #' @description list all parameters in the container if isValid = NULL
    #' list all valid parameters in the container if  isValid = TRUE
    #' list all invalid parameters in the container if isValid = FALSE
    #' @param isValid an optional logical argument
    #' @return a vector of symbols
    listParameters = function(...) {
      args = list(...)
      isValid = args[["isValid"]]

      if (!(is.logical(isValid) || is.null(isValid))) {
        stop("Argument `isValid` must be type logical or NULL \n")
      }
      parameters = NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self[s], c("Parameter",".ConstParameter"))) {
          if (is.null(parameters)) {
            parameters = s
          }
          else {
            parameters = append(parameters, s)
          }
        }
      }
      return(parameters)
    },

    #' @description list all aliases in the container if isValid = NULL
    #' list all valid aliases in the container if  isValid = TRUE
    #' list all invalid aliases in the container if isValid = FALSE
    #' @param isValid an optional logical argument
    #' @return a vector of symbols
    listAliases = function(...) {
      args = list(...)
      isValid = args[["isValid"]]

      if (!(is.logical(isValid) || is.null(isValid))) {
        stop("Argument `isValid` must be type logical or NULL \n")
      }
      aliases = NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self[s], c(".BaseAlias", ".ConstAlias", 
        ".ConstUniverseAlias"))) {
          if (is.null(aliases)) {
            aliases = s
          }
          else {
            aliases = append(aliases, s)
          }
        }
      }
      return(aliases)
    },

    #' @description list all variables in the container if isValid = NULL
    #' list all valid variables in the container if  isValid = TRUE
    #' list all invalid variables in the container if isValid = FALSE
    #' @param isValid an optional logical argument
    #' @param types an optional logical argument to list a subset of 
    #' equation types
    #' @return a vector of symbols
    listVariables = function(...) {
      args = list(...)
      isValid = args[["isValid"]]
      types = args[["types"]]

      if (!(is.logical(isValid) || is.null(isValid))) {
        stop("Argument `isValid` must be type logical or NULL \n")
      }

      if (!(is.character(types) || is.list(types)|| is.null(types))) {
        stop("Argument `types` myst be type character, list, vector, or NULL \n")
      }

      if (is.list(types)) {
        if (!all(unlist(lapply(types, is.character)))) {
          stop("Argument 'types' must contain only type character\n")
        }
      }

      if (is.null(types)) {
        types = .varTypes
      }

      for (t in types) {
        if (!any(.varTypes == t)) {
          stop(paste0("User input unrecognized variable type: ", t, " \n"))
        }
      }

      variables = NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self[s], c("Variable", ".ConstVariable"))
        && any(types == self[s]$type)) {
          if (is.null(variables)) {
            variables = s
          }
          else {
          variables = append(variables, s)
          }
        }
      }
      return(variables)
    },

    #' @description list all equations in the container if isValid = NULL
    #' list all valid equations in the container if  isValid = TRUE
    #' list all invalid equations in the container if isValid = FALSE
    #' @param isValid an optional logical argument
    #' @param types an optional logical argument to list a subset of 
    #' equation types
    #' @return a vector of symbols
    listEquations = function(...) {
      args = list(...)
      isValid = args[["isValid"]]
      types = args[["types"]]

      if (!(is.logical(isValid) || is.null(isValid))) {
        stop("Argument `isValid` must be type logical or NULL \n")
      }

      if (!(is.character(types) || is.list(types) || is.null(types))) {
        stop("Argument `types` myst be type character, list, vector, or NULL \n")
      }


      if ( is.list(types) && !all(unlist(lapply(types, is.character)))) {
        stop("Argument 'types' must contain only type character\n")
      }

      if (is.null(types)) {
        types = unlist(unique(.EquationTypes))
      }

      for (t in types) {
        if (is.null(.EquationTypes[[t]])) {
          stop(paste0("User input unrecognized equation type: ", t, " \n"))
        }
      }

      equations = NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self[s], c("Equation", ".ConstEquation"))
        && any(types == self[s]$type)) {
          if (is.null(equations)) {
            equations = s
          }
          else {
          equations = append(equations, s)
          }
        }
      }
      return(equations)
    },

    #' @description create a summary table with descriptive statistics for Sets
    #' @param symbols an optional argument of type string or a list of sets 
    #' to describe. The default value is NULL which assumes all sets.
    describeSets = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$listSets()
        if (is.null(symbols)) return()
      }
      else {
        if (!(is.list(symbols) || is.character(symbols))) {
          stop("Argument `symbols` must be type character, 
          list, vector, or NULL \n")
        }
      }

      if (is.list(symbols)) {
        if (!all(unlist(lapply(symbols, is.character) ))) {
          stop("Argument `symbols` must contain elements of type character\n")
        }
      }

      colNames = c("name",
            "isSingleton",
            "domain",
            "domainType",
            "dim",
            "numberRecs",
            "cardinality",
            "sparsity"
            )
      df = data.frame(matrix(NA, nrow = 
      length(symbols), ncol = length(colNames)))
      rowCount = 0

      symbols = self$getSymbolNames(symbols)
      for (i in symbols) {
        if (any(self$listSets() == i)|| any(self$listAliases() == i)) {
          symDescription = list(
            i,
            self[i]$isSingleton,
            paste(self[i]$domainNames, sep = "", collapse = " "),
            self[i]$domainType,
            self[i]$dimension,
            self[i]$numberRecords,
            self[i]$getCardinality(),
            self[i]$getSparsity()
          )
          rowCount = rowCount + 1
          df[rowCount, ] = symDescription
        }
      }


      colnames(df) = colNames

      if (length(intersect(symbols, self$listAliases())) != 0) {
        alias_with = c()
        is_alias = c()
        for (i in nrow(df)) {
          name =df[i, 1]
          is_alias = append(is_alias, 
          inherits(self[name], c(".BaseAlias", ".ConstAlias")))
          if (inherits(self, "Container")) {
            if (inherits(self[name], "Alias")) {
             alias_with = append(alias_with, self[name]$aliasWith$name)
            }
            else if (inherits(self[name], "UniverseAlias")) {
              alias_with = append(alias_with, self[name]$aliasWith)
            }
          }
          else if (inherits(self, "ConstContainer")) {
            alias_with = append(alias_with, self[name]$aliasWith)
          }
        }
        df$isAlias = is_alias
        df$aliasWith = alias_with
        append(colNames, "isAlias", 3)
        append(colNames, "aliasWith", 4)
        df <- df[, colNames]
      }
      if (rowCount > 0) {
        df = df[1:rowCount, ]
        return(df[order(df[, 1]), ])
      }
      else {
        return(NULL)
      }
    },

    #' @description create a summary table with descriptive 
    #' statistics for Aliases
    #' @param symbols an optional argument of type string or a list of aliases 
    #' to describe. The default value is NULL which assumes all aliases.
    describeAliases = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$listAliases()
        if (is.null(symbols)) return()
      }
      else {
        if (!(is.list(symbols) || is.character(symbols))) {
          stop("Argument `symbols` must be type character, 
          list, vector, or NULL \n")
        }
      }

      if (is.list(symbols)) {
        if (!all(unlist(lapply(symbols, is.character) ))) {
          stop("Argument `symbols` must contain elements of type character\n")
        }
      }

      colNames = list("name",
            "aliasWith",
            "isSingleton",
            "domain",
            "domainType",
            "dim",
            "numberRecs",
            "cardinality",
            "sparsity"
            )
      df = data.frame(matrix(NA, nrow = 
      length(symbols), ncol = length(colNames)))
      rowCount = 0
      symbols = self$getSymbolNames(symbols)
      for (i in symbols) {
        if (any(self$listAliases() == i)) {
          if(inherits(self, "Container")) {
            if (inherits(self[name], "Alias")) {
             alias_with = append(alias_with, self[name]$aliasWith$name)
            }
            else if (inherits(self[name], "UniverseAlias")) {
              alias_with = append(alias_with, self[name]$aliasWith)
            }
          }
          else if (inherits(self, "ConstContainer")) {
            aliasName = self[i]$aliasWith
          }
          symDescription = list(
            i,
            aliasName,
            self[i]$isSingleton,
            paste(self[i]$domainNames, sep = "", collapse = " "),
            self[i]$domainType,
            self[i]$dimension,
            self[i]$numberRecords,
            self[i]$getCardinality(),
            self[i]$getSparsity()
          )
          rowCount = rowCount + 1
          df[rowCount, ] = symDescription
        }
      }
      colnames(df) = colNames
      if (rowCount > 0) {
        df = df[1:rowCount, ]
        return(df[order(df[, 1]), ])
      }
      else {
        return(NULL)
      }
    },

    #' @description create a summary table with descriptive statistics 
    #' for Parameters
    #' @param symbols an optional argument of type string or a 
    #' list of parameters to describe. The default value is 
    #' NULL which assumes all parameters.
    describeParameters = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols = self$listParameters()
        if (is.null(symbols)) return()
      }
      else {
        if (!(is.list(symbols) || is.character(symbols))) {
          stop("Argument `symbols` must be type character, 
          list, vector, or NULL \n")
        }
      }

      if (is.list(symbols)) {
        if (!all(unlist(lapply(symbols, is.character) ))) {
          stop("Argument `symbols` must contain elements of type character\n")
        }
      }

      colNames = list(
            "name",
            "isScalar",
            "domain",
            "domainType",
            "dim",
            "numRecs",
            "minValue",
            "meanValue",
            "maxValue",
            "whereMin",
            "whereMax",
            "countEps",
            "countNa",
            "countUndef",
            "cardinality",
            "sparsity"
            )
      df = data.frame(matrix(NA, nrow = 
      length(symbols), ncol = length(colNames)))
      rowCount = 0

      symbols = self$getSymbolNames(symbols)
      for (i in symbols) {
        if (any(self$listParameters() == i)) {
          symDescription = list(
            i,
            self[i]$isScalar,
            paste(self[i]$domainNames, sep = "", collapse = " "),
            self[i]$domainType,
            self[i]$dimension,
            self[i]$numberRecords,
            self[i]$getMinValue("value"),
            self[i]$getMeanValue("value"),
            self[i]$getMaxValue("value"),
            self[i]$whereMin("value"),
            self[i]$whereMax("value"),
            self[i]$countEps("value"),
            self[i]$countNA("value"),
            self[i]$countUndef("value"),
            self[i]$getCardinality(),
            self[i]$getSparsity()
          )
          rowCount = rowCount + 1
          df[rowCount, ] = symDescription
        }
      }

      colnames(df) = colNames
      if (rowCount > 0) {
        df = df[1:rowCount, ]
        return(df[order(df[, 1]),])
      }
      else {
        return(NULL)
      }
    },

    #' @description create a summary table with descriptive 
    #' statistics for Variables
    #' @param symbols an optional argument of type string 
    #' or a list of Variables to describe. The default value 
    #' is NULL which assumes all variables.
    describeVariables = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$listVariables()
        if (is.null(symbols)) return()
      }
      else {
        if (!(is.list(symbols) || is.character(symbols))) {
          stop("Argument `symbols` must be type character, 
          list, vector, or NULL \n")
        }
      }

      if (is.list(symbols)) {
        if (!all(unlist(lapply(symbols, is.character) ))) {
          stop("Argument `symbols` must contain elements of type character\n")
        }
      }
      colNames = list(
            "name",
            "type",
            "domain",
            "domainType",
            "dim",
            "numRecs",
            "cardinality",
            "sparsity",
            "minLevel",
            "meanLevel",
            "maxLevel",
            "whereMaxAbsLevel",
            "countEpsLevel",
            "minMarginal",
            "meanMarginal",
            "maxMarginal",
            "whereMaxAbsMarginal",
            "countEpsMarginal"
            )
      df = data.frame(matrix(NA, nrow = 
      length(symbols), ncol = length(colNames)))
      rowCount = 0

      symbols = self$getSymbolNames(symbols)
      for (i in symbols) {
        if (any(self$listVariables() == i)) {
          symDescription = list(
            i,
            self[i]$type,
            paste(self[i]$domainNames, sep = "", collapse = " "),
            self[i]$domainType,
            self[i]$dimension,
            self[i]$numberRecords,
            self[i]$getCardinality(),
            self[i]$getSparsity(),
            self[i]$getMinValue("level"),
            self[i]$getMeanValue("level"),
            self[i]$getMaxValue("level"),
            self[i]$whereMaxAbs("level"),
            self[i]$countEps("level"),
            self[i]$getMinValue("marginal"),
            self[i]$getMeanValue("marginal"),
            self[i]$getMaxValue("marginal"),
            self[i]$whereMaxAbs("marginal"),
            self[i]$countEps("marginal")
          )
          rowCount = rowCount + 1
          df[rowCount, ] = symDescription
        }
      }

      colnames(df) = colNames
      if (rowCount > 0) {
        df = df[1:rowCount, ]
        return(df[order( df[,1]),])
      }
      else {
        return(NULL)
      }
    },
    #' @description create a summary table with descriptive 
    #' statistics for Equations
    #' @param symbols an optional argument of type string or 
    #' a list of equations to describe. The default value is 
    #' NULL which assumes all equations.
    describeEquations = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$listEquations()
        if (is.null(symbols)) return()
      }
      else {
        if (!(is.list(symbols) ||is.character(symbols))) {
          stop("Argument `symbols` must be type character, 
          list, vector, or NULL \n")
        }
      }

      if (is.list(symbols)) {
        if (!all(unlist(lapply(symbols, is.character) ))) {
          stop("Argument `symbols` must contain elements of type character\n")
        }
      }
      colNames = list(
            "name",
            "type",
            "domain",
            "domainType",
            "dim",
            "numRecs",
            "cardinality",
            "sparsity",
            "minLevel",
            "meanLevel",
            "maxLevel",
            "whereMaxAbsLevel",
            "countEpsLevel",
            "minMarginal",
            "meanMarginal",
            "maxMarginal",
            "whereMaxAbsMarginal",
            "countEpsMarginal"
            )
      df = data.frame(matrix(NA, nrow = 
      length(symbols), ncol = length(colNames)))
      rowCount = 0

      symbols = self$getSymbolNames(symbols)
      for (i in symbols) {
        if (any(self$listEquations() == i)) {
          symDescription = list(
            i,
            self[i]$type,
            paste(self[i]$domainNames, sep = "", collapse = " "),
            self[i]$domainType,
            self[i]$dimension,
            self[i]$numberRecords,
            self[i]$getCardinality(),
            self[i]$getSparsity(),
            self[i]$getMinValue("level"),
            self[i]$getMeanValue("level"),
            self[i]$getMaxValue("level"),
            self[i]$whereMaxAbs("level"),
            self[i]$countEps("level"),
            self[i]$getMinValue("marginal"),
            self[i]$getMeanValue("marginal"),
            self[i]$getMaxValue("marginal"),
            self[i]$whereMaxAbs("marginal"),
            self[i]$countEps("marginal")
          )
          rowCount = rowCount + 1
          df[rowCount, ] = symDescription
        }
      }
      colnames(df) = colNames
      if (rowCount > 0) {
        df = df[1:rowCount, ]
        return(df[order(df[, 1]),])
      }
      else {
        return(NULL)
      }
    }
  )
  )

#' @title BaseSymbol Abstract Class
#' @description An abstract BaseSymbol class from which the Symbol class is inherited.
.BaseSymbol <- R6Class(
  ".BaseSymbol",
  public = list(
    .gams_type = NULL,
    .gams_subtype = NULL,
  initialize = function(type, subtype) {

    self$.gams_type = type
    self$.gams_subtype = subtype
  },

  format = function(...) paste0("GAMS Transfer: R6 object of class ", 
  class(self)[1], ". Use ", self$name, "$summary for details"),

  #' @description getMaxValue get the maximum value
  #' @param columns columns over which one wants to get the maximum.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  getMaxValue = function(columns=NULL) {
    private$.getMetric(columns, "max")
  },

  #' @description getMinValue get the minimum value in value column
  #' @param columns columns over which one wants to get the minimum.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  getMinValue = function(columns=NULL) {
    private$.getMetric(columns, "min")
  },

  #' @description getMeanValue get the mean value in value column
  #' @param columns columns over which one wants to get the mean.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  getMeanValue = function(columns=NULL) {
    private$.getMetric(columns, "mean")
  },

  #' @description getMaxAbsValue get the maximum absolute value in value column
  #' @param columns columns over which one wants to get the 
  #' maximum absolute value.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  getMaxAbsValue = function(columns=NULL) {
    private$.getMetric(columns, "maxAbs")
  },

  #' @description whereMax find the row number in records data frame with a 
  #' maximum value (return first instance only)
  #' @param columns columns over which one wants to find the 
  #' domain entry of records with a maximum value.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  whereMax = function(column=NULL) {
    return(private$.whereMetric(column, "max"))
  },

  #' @description whereMaxAbs find the row number in records data frame 
  #' with a maximum absolute value (return first instance only)
  #' @description whereMax find the domain entry of records with a 
  #' maximum absolute value (return first instance only)
  #' @param columns columns over which one wants to find the 
  #' domain entry of records with a maximum value.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  whereMaxAbs = function(column=NULL) {
    return(private$.whereMetric(column, "maxAbs"))
  },

  #' @description whereMin find the the row number in records data frame 
  #' with a minimum value (return first instance only)
  #' @description whereMax find the domain entry of records with a 
  #' minimum value (return first instance only)
  #' @param columns columns over which one wants to find the 
  #' domain entry of records with a maximum value.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  whereMin = function(column=NULL) {
    return(private$.whereMetric(column, "min"))
  },

  #'@description countNA total number of SpecialValues[["NA"]] in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues[["NA"]].
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  countNA = function(columns=NULL) {
    return(private$.countSpecialValue(columns, "isNA"))
  },

  #' @description countEps total number of SpecialValues$EPS in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues$EPS.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  countEps = function(columns=NULL) {
    return(private$.countSpecialValue(columns, "isEps"))
  },

  #'@description countUndef total number of SpecialValues$UNDEF in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues$UNDEF.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  countUndef = function(columns=NULL) {
    return(private$.countSpecialValue(columns, "isUndef"))
  },

  #'@description countPosInf total number of 
  #' SpecialValues$POSINF in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues$POSINF.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.  
  countPosInf = function(columns=NULL) {
    return(private$.countSpecialValue(columns, "isPosInf"))
  },

  #'@description countNegInf total number of 
  #' SpecialValues$NEGINF in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues$NEGINF.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.  
  countNegInf = function(columns=NULL) {
    return(private$.countSpecialValue(columns, "isNegInf"))
  }

  ),

  private = list(
    .getMetric = function(columns, metric) {
      if (is.null(self$records) || inherits(self, c("Set", ".ConstSet"))) {
        return(NA)
      }

      columns = private$.checkColumnsArgument(columns)

      tryCatch(
        {
          if (metric == "max") {
            return(max(self$records[,columns]))
          }
          else if (metric == "min") {
            return(min(self$records[, columns]))
          }
          else if (metric == "mean") {
            return(mean(self$records[,columns]))
          }
          else if (metric == "maxAbs") {
            return(max(abs(self$records[,columns])))
          }
        },
        error = function(cond) return(NA),
        warning = function(cond) return(NA)
      )
    },

    .whereMetric = function(column, metric) {
      if (is.null(self$records) || inherits(self, c("Set", ".ConstSet"))) {
        return(NA)
      }

      column = private$.checkColumnsArgument(column)

      if (length(column) > 1) {
        stop("At most one `column` can be specified\n")
      }

      tryCatch(
        {
          if (metric == "min") {
            whereMetricVal = which.min(self$records[,column])
          }
          else if (metric == "max") {
            whereMetricVal = which.max(self$records[,column])
          }
          else if (metric == "maxAbs") {
            whereMetricVal = which.max(abs(self$records[,column]))
          }

            if (is.integer0(whereMetricVal)) {
              return(NA)
            }
            else {
              return(whereMetricVal)
            }
        },
        error = function(cond) return(NA),
        warning = function(cond) return(NA)
      )
    },

    .countSpecialValue = function(columns, specialValueFunc) {
      if (is.null(self$records) || inherits(self, c("Set", ".ConstSet"))) {
        return(NA)
      }
      columns = private$.checkColumnsArgument(columns)
      tryCatch(
        {
          return(sum(SpecialValues[[specialValueFunc]](self$records[,columns])))
        },
        error = function(cond)  return(NA),
        warning = function(cond) return(NA)
      )
    },

    .checkColumnsArgument = function(columns) {
      if (inherits(self, c("Parameter",".ConstParameter"))) {
        columns = "value"
      }
      else {
        if (!is.null(columns)) {
          if (!is.character(columns)) {
            stop("The argument `columns` must be type character\n")
          }

          diff = setdiff(columns, private$.attr())
          if (length(diff) != 0) {
            stop(paste0("User entered columns (", toString(columns), ") must be a subset
            of valid numeric columns ", toString(private$.attr()), "\n"))
          }
        }
        else {
          columns = "level"
        }
      }
      return(columns)
    },
    .attr = function() {
      return(c("level", "marginal", "lower", "upper", "scale"))
    }
  )
)

DomainViolation <- R6::R6Class (
  "DomainViolation",
  public = list(
    symbol=NULL,
    dimension = NULL,
    domain= NULL,
    violations = NULL,
    initialize = function(symbol, dimension, domain,violations) {
      self$symbol = symbol
      self$dimension = dimension
      self$domain = domain
      self$violations = violations
      lockBinding("symbol", self)
      lockBinding("dimension", self)
      lockBinding("domain", self)
      lockBinding("violations", self)
    },
    format = function(...) {
      paste0("GAMS Transfer: DomainViolation with properties: \n", 
      "Symbol: ", self$symbol$name, "\n",
      "dimension: ", self$dimension, "\n",
      "domain: ", self$domain$name, "\n",
      "violations: ", toString(self$violations))
    }
  )
)


`[..BaseContainer` <- function(obj, ...) obj$`[`(...)
`[<-..BaseContainer` <- function(obj, ..., value) obj$`[<-`(..., value)
