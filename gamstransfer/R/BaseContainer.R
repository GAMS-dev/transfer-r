#
# GAMS - General Algebraic Modeling System Python API
#
# Copyright (c) 2017-2023 GAMS Software GmbH <support@gams.com>
# Copyright (c) 2017-2023 GAMS Development Corp. <support@gams.com>
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
          stop(paste0("must enter valid full (absolute) path to the",
          "GAMS system directory\n"))
        }
      }

      self$acronyms = list()
      self$data = collections::ordered_dict()
      # another dict for lowercase names to original case
      self$.lc_data = collections::dict()
    },

    `[` = function(key) {
      if (self$.lc_data$has(tolower(key))) {
        # key exists
        true_case_key = self$.lc_data$get(tolower(key))
        return(self$data$get(true_case_key))
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
          stop(paste0("Argument `symbols` must be type character, ",
          "list, vector, or NULL \n"))
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
          stop(paste0("Argument `symbols` must be type character, ",
          "list, vector, or NULL \n"))
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
          stop(paste0("Argument `symbols` must be type character, ",
          "list, vector, or NULL \n"))
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
          stop(paste0("Argument `symbols` must be type character, ",
          "list, vector, or NULL \n"))
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
          stop(paste0("Argument `symbols` must be type character, ",
          "list, vector, or NULL \n"))
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
            stop(paste0("User entered columns (", toString(columns), 
            ") must be a subset of valid numeric columns ", 
            toString(private$.attr()), "\n"))
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


`[..BaseContainer` <- function(obj, ...) obj$`[`(...)
`[<-..BaseContainer` <- function(obj, ..., value) obj$`[<-`(..., value)
