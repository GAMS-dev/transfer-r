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

GMS_DT_SET = 0
GMS_DT_PAR = 1
GMS_DT_VAR = 2
GMS_DT_EQU = 3
GMS_DT_ALIAS = 4
GMS_DT_MAX = 5

GMS_VARTYPE_BINARY =  1
GMS_VARTYPE_INTEGER = 2
GMS_VARTYPE_POSITIVE = 3
GMS_VARTYPE_NEGATIVE = 4
GMS_VARTYPE_FREE  =   5
GMS_VARTYPE_SOS1  =  6
GMS_VARTYPE_SOS2  =   7
GMS_VARTYPE_SEMICONT = 8
GMS_VARTYPE_SEMIINT =  9

GMS_EQUTYPE_E =       0
GMS_EQUTYPE_G  =      1
GMS_EQUTYPE_L   =     2
GMS_EQUTYPE_N   =     3
GMS_EQUTYPE_X =       4
GMS_EQUTYPE_C   =     5
GMS_EQUTYPE_B   =     6

GMS_EQU_USERINFO_BASE = 53
gams_description_max_length = 255


SpecialValues = list(
  "NA" = NA, # cannot be anything else
  "EPS" = -0.0,
  "UNDEF" = NaN,
  "POSINF" = Inf,
  "NEGINF" = -Inf
  )
#' @title Container Class
#' @description The main object class within GAMS Transfer is called 
#' Container. The Container is the vessel that allows symbols to be 
#' linked together (through their domain definitions), it enables 
#' implicit set definitions, it enables structural manipulations of the 
#' data (matrix generation), and it allows the user to perform different 
#' read/write operations.
#' @field data is a named list containing all symbol data
#' @field systemDirectory is the path to GAMS System directory
#' @export
Container <- R6::R6Class (
  "Container",
  public = list(
    systemDirectory = NULL,
    data = NULL,
    acronyms = NULL,
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

      if (missing(systemDirectory)) {
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
      self$data = list()
      self$.requiresStateCheck = TRUE

      if (!missing(loadFrom)) {
      self$read(loadFrom, symbols="all")

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
    read = function(loadFrom, symbols="all", records=TRUE) {
      # read metadata
      # get all symbols and metadata from c++
      # process it and populate various fields

      # check if records is logical
      if (!is.logical(records)) {
        stop("records must be type logical\n")
      }

      if (!(is.character(symbols)) && !(is.list(symbols))) {
        stop("argument symbols must be of the type list or string\n")
      }

      for (s in symbols) {
        if (!is.character(s)) {
          stop("argument symbols must contain only type string\n")
        }
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
      # check acronyms
      acrInfo = checkAcronyms(loadFrom, self$systemDirectory)
      nAcr = acrInfo[["nAcronyms"]]
      if (nAcr != 0) {
        warning("GDX file contains acronyms. 
        Acronyms are not supported and are set to GAMS NA.\n")
        self$acronyms = acrInfo[["acronyms"]]
      }

      # get names for all symbols
      metadata = getSymbols(loadFrom, self$systemDirectory)
      syms = lapply(metadata, "[[", 1)

      if (is.character(symbols) && symbols == "all") {
        symbolsToRead = syms
      }
      else {
        symbolsToRead = list()
        for (s in symbols) {
          if (any(syms == s)) {
            symbolsToRead = append(symbolsToRead, s)
          }
        }
      }
      if (length(symbolsToRead) == 0){
        return()
      }

      # check if container exists any of the symbols already
      for (s in symbolsToRead) {
        if (!is.null(self$data[[s]])) {
          stop(paste0("Attempting to add symbol ", s, ", however,",
          " one already exists in the Container. Symbol replacement",
          " is only possible if the symbol is first removed from the", 
          "Container with the removeSymbol() method.\n"))
        }
      }

      aliasList = list()
      aliasCount = 0
      for (m in metadata) {
         if (any(symbolsToRead == m$name)) {
            if (m$type == GMS_DT_PAR) {
              Parameter$new(
                self, m$name, m$domain,
                domainForwarding=FALSE,
                description = m$expltext)
            }
            else if (m$type == GMS_DT_SET) {
                if (m$subtype == 0) {
                Set$new(
                self, m$name, m$domain, FALSE,
                records = NULL,
                domainForwarding=FALSE,
                m$expltext)
                }
                else if (m$subtype == 1) {
                Set$new(
                self, m$name, m$domain, TRUE,
                records = NULL,
                domainForwarding=FALSE, 
                m$expltext)
                }
                else {
                  stop(paste0("Unknown set classification with 
                  GAMS Subtype ", m$subtype, "cannot load set ", m$name))
                }
            }
            else if (m$type == GMS_DT_VAR) {
                type = which(VarTypeSubtype() == m$subtype)
                if (is.integer0(type)) {
                  type = "free"
                }
                else {
                  type = names(VarTypeSubtype())[[type]]
                }
                Variable$new(
                self, m$name, type, m$domain,
                domainForwarding = FALSE,
                description = m$expltext)
            }
            else if (m$type == GMS_DT_EQU) {
                type = names(EqTypeSubtype())[[which(EqTypeSubtype() 
                == m$subtype)]]
                if (is.integer0(type)) {
                  stop(paste0("Unknown equation classification with 
                  GAMS Subtype ", m$subtype, "cannot load equation ", m$name))
                }
                Equation$new(
                self, m$name, type, m$domain,
                domainForwarding = FALSE,
                description = m$expltext)
            }
            else if (m$type == GMS_DT_ALIAS) {
              aliasCount = aliasCount + 1
              aliasList = append(aliasList, list(m))
            }
            else {
                stop("incorrect data type.\n")
            }
         }
      }

      # do alias last
      for (m in aliasList) {
      Alias$new(
        self, m$name, self$data[[m$aliasfor]])
      }

      if (records == TRUE) {
        symbolrecords = readSymbols(unlist(symbolsToRead),
        loadFrom, self$systemDirectory)

        for (s in symbolrecords) {
          if (is.null(s$records)) {
            next
          }
          self$data[[s$names]]$setRecords(s$records)

          if (!is.null(self$acronyms)) {
            if (inherits(self$data[[s$names]], "Parameter")
            | inherits(self$data[[s$names]], "Variable")
            | inherits(self$data[[s$names]], "Equation")) {
              for (a in self$acronyms) {
                self$data[[s$names]]$records[(self$data[[s$names]]$records 
                == a * 1e301)] = SpecialValues[["NA"]]
              }
            }
          }
        }

        private$linkDomainObjects(symbolsToRead)
        self$.linkDomainCategories()
      }
    },

    #' @description provides a universe for all symbols
    getUniverseSet = function() {
      uni = c()
      for (i in self$listSymbols(isValid = TRUE)) {
        if (!is.null(self$data[[i]]$records)) {
          if (self$data[[i]]$dimension > 0) {
            df = self$data[[i]]$records[, (1:self$data[[i]]$dimension)]
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
      if (!(is.character(symbols) || is.vector(symbols) || is.list(symbols))) {
        stop("Argument 'name' must be of type string, list, or vector\n")
      }

      if (!all(unlist(lapply(symbols, is.character)))) {
        stop("Argument 'name' must contain only type character\n")
      }

      setOrAlias = list()
      for (n in symbols) {
        sym = self$data[[n]]
        sym$refContainer <- NULL
        sym$.requiresStateCheck <- TRUE

        if (inherits(sym, "Set") || inherits(sym, "Alias")) {
          setOrAlias = append(setOrAlias, sym)
        }
        self$data[[n]] <- NULL
      }

      # if there were any sets or aliases removed from the data list
      # then reset check flag for all symbols
      if (length(setOrAlias) != 0) {
        for (i in self$listSymbols()) {
          self$data[[i]]$.requiresStateCheck = TRUE
        }
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

      if (is.null(self$data[[oldName]])) {
        stop(paste0("Symbol ", oldName, " does not exist\n"))
      }

      if (oldName != newName) {
        sym = self$data[[oldName]]
        sym$name = newName
        self$.requiresStateCheck = TRUE
      }
    },

    #' @description list all symbols in the container if isValid = NULL
    #' list all valid symbols in the container if  isValid = TRUE
    #' list all invalid symbols in the container if isValid = FALSE
    #' @param isValid an optional logical argument
    #' @return a vector of symbols
    listSymbols = function(isValid = NULL) {
      if (!is.null(isValid)) {
        assertthat::assert_that(is.logical(isValid),
        msg = "argument 'isValid' must be type logical\n")
        l = NULL
        for (d in self$data) {
          if (d$isValid() == isValid) {
            if (is.null(l)) {
              l = d$name
            }
            else {
              l = append(l, d$name)
            }
          }
        }
        return(l)
      }
      else {
        return(names(self$data))
      }
    },

    #' @description list all sets in the container if isValid = NULL
    #' list all valid sets in the container if  isValid = TRUE
    #' list all invalid sets in the container if isValid = FALSE
    #' @param isValid an optional logical argument
    #' @return a vector of symbols
    listSets = function(isValid = NULL) {
      sets = NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self$data[[s]], "Set") |
        inherits(self$data[[s]], "Alias") ) {
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
    listParameters = function(isValid = NULL) {
      parameters = NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self$data[[s]], "Parameter")) {
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
    listAliases = function(isValid = NULL) {
      aliases = NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self$data[[s]], "Alias")) {
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
    #' @return a vector of symbols
    listVariables = function(isValid = NULL) {
      variables = NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self$data[[s]], "Variable")) {
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
    #' @return a vector of symbols
    listEquations = function(isValid=NULL) {
      equations = NULL
      for (s in self$listSymbols(isValid)) {
        if (inherits(self$data[[s]], "Equation")) {
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
      Set$new(
      self, name, domain, isSingleton,
      records, domainForwarding, description)
      return(self$data[[name]])
    },
    #' @description There are two different ways to create a GAMS parameter and 
    #' add it to a Container. One is using the Parameter constructor and 
    #' the other is using addParameter method which calls the Parameter constructor
    #' internally.
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
      Parameter$new(
        self, name, domain, records,
        domainForwarding, description)
        return(self$data[[name]])
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
      Variable$new(
        self, name, type, domain, records,
        domainForwarding, description)
        return(self$data[[name]])
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
      Equation$new(
        self, name, type, domain, records,
        domainForwarding, description)
        return(self$data[[name]])
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
      Alias$new(
      self, name, aliasWith)
      return(self$data[[name]])
    },

    #' @description create a summary table with descriptive statistics for Sets
    #' @param symbols an optional argument of type string or a list of sets 
    #' to describe. The default value is NULL which assumes all sets.
    describeSets = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$listSets()
      }
      colNames = list("name",
            "isAlias",
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
      for (i in symbols) {
        if (any(self$listSets() == i)) {
          symDescription = list(
            i,
            self$data[[i]]$isAlias,
            self$data[[i]]$isSingleton,
            paste(self$data[[i]]$domainNames, sep = "", collapse = " "),
            self$data[[i]]$domainType,
            self$data[[i]]$dimension,
            self$data[[i]]$numberRecords,
            self$data[[i]]$getCardinality(),
            self$data[[i]]$getSparsity()
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
      for (i in symbols) {
        if (any(self$listParameters() == i)) {
          symDescription = list(
            i,
            self$data[[i]]$isScalar,
            paste(self$data[[i]]$domainNames, sep = "", collapse = " "),
            self$data[[i]]$domainType,
            self$data[[i]]$dimension,
            self$data[[i]]$numberRecords,
            self$data[[i]]$getMinValue("value"),
            self$data[[i]]$getMeanValue("value"),
            self$data[[i]]$getMaxValue("value"),
            self$data[[i]]$whereMin("value"),
            self$data[[i]]$whereMax("value"),
            self$data[[i]]$countEps("value"),
            self$data[[i]]$countNA("value"),
            self$data[[i]]$countUndef("value"),
            self$data[[i]]$getCardinality(),
            self$data[[i]]$getSparsity()
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

      for (i in symbols) {
        if (any(self$listVariables() == i)) {
          symDescription = list(
            i,
            self$data[[i]]$type,
            paste(self$data[[i]]$domainNames, sep = "", collapse = " "),
            self$data[[i]]$domainType,
            self$data[[i]]$dimension,
            self$data[[i]]$numberRecords,
            self$data[[i]]$getCardinality(),
            self$data[[i]]$getSparsity(),
            self$data[[i]]$getMinValue("level"),
            self$data[[i]]$getMeanValue("level"),
            self$data[[i]]$getMaxValue("level"),
            self$data[[i]]$whereMaxAbs("level"),
            self$data[[i]]$countEps("level"),
            self$data[[i]]$getMinValue("marginal"),
            self$data[[i]]$getMeanValue("marginal"),
            self$data[[i]]$getMaxValue("marginal"),
            self$data[[i]]$whereMaxAbs("marginal"),
            self$data[[i]]$countEps("marginal")
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

      for (i in symbols) {
        if (any(self$listEquations() == i)) {
          symDescription = list(
            i,
            self$data[[i]]$type,
            paste(self$data[[i]]$domainNames, sep = "", collapse = " "),
            self$data[[i]]$domainType,
            self$data[[i]]$dimension,
            self$data[[i]]$numberRecords,
            self$data[[i]]$getCardinality(),
            self$data[[i]]$getSparsity(),
            self$data[[i]]$getMinValue("level"),
            self$data[[i]]$getMeanValue("level"),
            self$data[[i]]$getMaxValue("level"),
            self$data[[i]]$whereMaxAbs("level"),
            self$data[[i]]$countEps("level"),
            self$data[[i]]$getMinValue("marginal"),
            self$data[[i]]$getMeanValue("marginal"),
            self$data[[i]]$getMaxValue("marginal"),
            self$data[[i]]$whereMaxAbs("marginal"),
            self$data[[i]]$countEps("marginal")
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

    printSpecialValues = function() {
      print(private$gdx_specVals_write)
    },

    #' @description a write method to write to a gdxout GDX file
    #' @param gdxout name of the GDX file to write to
    #' @param compress write tge GDX file in compressed format by setting
    #' compress = TRUE.
    #' @param uelPriority Advanced users might want to specify an order 
    #' to their UEL list (i.e., the universe set); The UEL 
    #' ordering follows that dictated by the data. As a convenience, it 
    #' is possible to prepend the UEL list with a user specified order 
    #' using the uel_priority argument.
    write = function(gdxout, compress = FALSE, uelPriority = NULL) {
      if (!is.logical(compress)) {
        stop("'compress' must be of type logical; 
        default False (no compression)\n")
      }

      if (!is.character(gdxout)) {
        stop("The argument gdxout must be of type string\n")
      }
      else {
        namesplit = strsplit(gdxout, "\\.")
        ext = tail(unlist(namesplit), 1)
        if (ext != "gdx") {
          stop("check filename extension, must be .gdx\n")
        }

        gdxout = R.utils::getAbsolutePath(path.expand(gdxout))
      }

      if (!is.null(uelPriority)) {
        if (!(is.character(uelPriority) || is.list(uelPriority))) {
          stop("'uelPriority' must be type list or str\n")
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
      # private$validSymbolOrder()

      # remap special values
      specialValsGDX = getSpecialValues(gdxout, self$systemDirectory)

      for (s in self$data) {
        # no mapping required for alias
        if (inherits(s, "Alias") || inherits(s, "Set")) next
        if (is.null(s$records)) next
        colrange = (s$dimension + 1):length(s$records)
        s$records[, colrange][is.nan(
          s$records[, colrange])] = 
          specialValsGDX[["UNDEF"]]

        s$records[,colrange][is.na(
          s$records[,colrange])] = 
          specialValsGDX[["NA"]]

        s$records[,colrange][
          ((s$records[, colrange] == Inf)
        & (sign(s$records[, colrange]) 
        == 1))] = specialValsGDX[["POSINF"]]

        s$records[, colrange][
          ((s$records[, colrange] == -Inf)
        &(sign(s$records[, colrange]) 
        == -1))] = specialValsGDX[["NEGINF"]]

        s$records[,colrange][
          ((s$records[,colrange] == 0)
        & (sign(1/s$records[,colrange]) 
        == -1))] = specialValsGDX[["EPS"]]
      }

      if (is.null(uelPriority)) {
        gdxWriteSuper(self$data, self$systemDirectory, 
        gdxout, NA, FALSE, compress)
      }
      else {
        universe = self$getUniverseSet()
        if ((is.null(universe)) ||
        (!setequal(intersect(uelPriority, universe), uelPriority))) {
          stop("uelPriority must be a subset of the universe, check 
          spelling of an element in uelPriority? Also check 
          getUniverseSet() method for assumed UniverseSet.\n")
        }

        reorder = uelPriority
        reorder = append(reorder, universe)
        reorder = unique(reorder)

        gdxWriteSuper(self$data, self$systemDirectory, 
        gdxout, unlist(reorder), TRUE, compress)
      }
    },
    #' @description reorder symbols in order to avoid domain violations
    reorderSymbols = function() {
      orderedSymbols = private$validSymbolOrder()
      datacopy = self$data
      self$data = list()
      for (s in orderedSymbols) {
        self$data[[s]] = datacopy[[s]]
      }
    },

    #' @description TRUE if all the symbols is in the Container are 
    #' valid, throw exceptions if verbose=True, check all symbols if 
    #' force=TRUE.
    #' @param verbose type logical
    #' @param force type logical
    isValid = function(verbose=FALSE, force=FALSE) {
      assertthat::assert_that(is.logical(verbose), 
      msg = "Argument 'verbose' must be logical")

      assertthat::assert_that(is.logical(force), 
      msg = "Argument 'force' must be logical")

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

    .linkDomainCategories = function() {
      for (i in self$listSymbols()) {
        if (!inherits(self$data[[i]], "Alias")) {
          self$data[[i]]$.linkDomainCategories()
        }
      }
    }

  ),
  private = list(
    gdx_specVals_write = list(),

    linkDomainObjects = function(symbols) {
      for (s in symbols) {
        if (! inherits(self$data[[s]], "Alias")) {
          d = list()
          for (j in self$data[[s]]$domain) {
            if ((is.character(j) && (j == s)) || 
            (inherits(j, "Set") && (identical(j,s)))) {
              d = append(d, j)
            }
            else if (is.character(j) && (any(symbols == j)) && (j != s)) {
               d = append(d, self$data[[j]])
            }
            else {
              d = append(d, j)
            }
          }

          self$data[[s]]$domain = d
        }
      }
    },

    check = function() {
      if (self$.requiresStateCheck == TRUE) {
         private$validSymbolOrder()

        if (!setequal(self$listSymbols(), self$listSymbols(isValid = TRUE))) {
          stop(paste0("Container contains invalid symbols; ",
          "invalid symbols can be found with the .listSymbols() ",
          "method. Debug invalid symbol(s) by running .",
          "isValid(verbose=TRUE, force=TRUE) method on the symbol object.\n"))
        }
        self$.requiresStateCheck = FALSE
      }
    },

    remap_special_values = function(syms) {
      for (s in syms) {
        for (c in names(self$data[[s]]$records)) {
          idx = list()
          for (specVal in names(SpecialValues)) {
            idx[[specVal]] = which(self$data[[s]]$records[[c]] == 
            private$gdx_specVals_write[specVal])
          }

          for (specVal in names(SpecialValues)) {
            if (any(idx[[specVal]])) {
              for (i in idx[[specVal]]) {
                self$data[[s]]$records[[c]][[i]] = SpecialValues[specVal]
              }
            }
          }
        }
      }
    },

    validSymbolOrder = function() {
      orderedSymbols = list()
      symbolsToSort = self$listSymbols()

      idx = 1
      while (length(symbolsToSort) != 0) {
        sym = symbolsToSort[[idx]]
        # special 1D sets (universe domain & relaxed sets)
        if (inherits(self$data[[sym]], "Set") &&
        self$data[[sym]]$dimension == 1 &&
        is.character(self$data[[sym]]$domain[[1]])
        ) {
          orderedSymbols = append(orderedSymbols, sym)
          symbolsToSort = symbolsToSort[-idx]
          idx = 1
        }
        else {
          doi = list()
          for (i in self$data[[sym]]$domain) {
            if (is.character(i)) {
              doi = append(doi, TRUE)
            }
            else if ((inherits(i, "Set") | inherits(i, "Alias")) &
            any(orderedSymbols == i$name)) {
               doi = append(doi, TRUE)
            }
            else {
              doi = append(doi, FALSE)
            }
          }

          if (all(doi == TRUE)) {
            orderedSymbols = append(orderedSymbols, sym)
            symbolsToSort = symbolsToSort[-1]
            idx = 1
          }
          else {
            idx = idx + 1
          }

        }

        if (idx == length(symbolsToSort) + 1 & length(symbolsToSort) != 0) {
          symString = ""
          for (s in symbolsToSort) {
            if (inherits(self$data[[s]], "Set")) {
              symString = paste(symString, s)
            }
          }

          stop(paste0("Error: Graph cycle detected among symbols: ",
          symString, " -- must resolve circular domain referencing\n"))
        }
      }
      return(orderedSymbols)
    },

    isValidSymbolOrder = function() {
      validOrder = private$validSymbolOrder()
      currentOrder = names(self$data)
      h = c()
      for (i in seq_along(self$data)) {
        symName = names(self$data)[i]
        if ( inherits(self$data[[symName]], "Set") | 
        inherits(self$data[[symName]], "Alias")) {
          if (i <= match(symName, validOrder)){
            h = append(h, TRUE)
          }
          else {
            h = append(h, FALSE)
          }
        }
        else {
          h = append(h, TRUE)
        }
      }

      if (all(h) == TRUE) {
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    }

  )
  )

VarTypeSubtype = function() {
  return(list(
  "binary" = GMS_VARTYPE_BINARY,
  "integer" = GMS_VARTYPE_INTEGER,
  "positive" = GMS_VARTYPE_POSITIVE,
  "negative" = GMS_VARTYPE_NEGATIVE,
  "free" = GMS_VARTYPE_FREE,
  "sos1" = GMS_VARTYPE_SOS1,
  "sos2" = GMS_VARTYPE_SOS2,
  "semicont" = GMS_VARTYPE_SEMICONT,
  "semiint" = GMS_VARTYPE_SEMIINT
  ))
}

EqTypeSubtype = function() {
  return(list(
  "eq" = GMS_EQUTYPE_E + GMS_EQU_USERINFO_BASE,
  "geq" = GMS_EQUTYPE_G + GMS_EQU_USERINFO_BASE,
  "leq" = GMS_EQUTYPE_L + GMS_EQU_USERINFO_BASE,
  "nonbinding" = GMS_EQUTYPE_N + GMS_EQU_USERINFO_BASE,
  "external" = GMS_EQUTYPE_X + GMS_EQU_USERINFO_BASE,
  "cone" = GMS_EQUTYPE_C + GMS_EQU_USERINFO_BASE,
  "boolean" = GMS_EQUTYPE_B + GMS_EQU_USERINFO_BASE
  ))
}
SetTypeSubtype = function() {
  return(list(
  "set" = 0,
  "singleton_set" = 1
  ))
}

#' @title Symbol Abstract Class
#' @description An abstract symbol class from which the classes Set, Parameter, Variable, 
#' and Equation are inherited.
Symbol <- R6Class(
  "Symbol",
  public = list(
    .gams_type = NULL,
    .gams_subtype = NULL,
  .requiresStateCheck = NULL,

  initialize = function(container, name,
                        type, subtype, 
                        domain,
                        description,
                        domainForwarding) {
    self$.gams_type = type
    self$.gams_subtype = subtype

    self$.requiresStateCheck = TRUE
    #' @field refContainer reference to the Container that the symbol 
    #' belongs to. Type Container.
    self$refContainer = container
    self$refContainer$.requiresStateCheck = TRUE
    #' @field name name of the symbol
    self$name <- name
    self$refContainer$data[[name]] = self

    self$records = NULL

    self$domain = domain

    self$dimension = length(self$domain)
    self$description = description
    self$domainForwarding = domainForwarding

  },

  findDomainViolations = function() {
    if (self$dimension == 0) {
      return(NULL)
    }

    idx = which(is.na(self$records[, (1:self$dimension)]), arr.ind = TRUE)
    if (self$dimension > 1) {
      return(idx[, 1])
    }
    else {
      return(idx)
    }
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

  #' @description getMaxValue get the maximum value
  #' @param columns columns over which one wants to get the maximum.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  getMaxValue = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", toString(columns), ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)\n"))
      }

      if (is.null(self$records)) {
        return(NA)
      }

      if (inherits(self, "Set")) {
        return(NA)
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column '", toString(columns), "' must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension+1):length(self$records)]
        , "\n"))
      }
    }
    else {
      #columns argument is NULL
        if (inherits(self, "Parameter")) {
          columns = "value"
        }
        else {
          # variable or equation
          columns = "level"
        }
    }

    tryCatch(
      {
        return(max(self$records[[columns]]))
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  },

  #' @description getMinValue get the minimum value in value column
  #' @param columns columns over which one wants to get the minimum.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  getMinValue = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input '", toString(columns), 
        "', however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)\n"))
      }

      if (is.null(self$records)) {
        return(NA)
      }

      if (inherits(self, "Set")) {
        return(NA)
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension+1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column '", columns, "' must be a subset",
        " of valid numeric columns", 
        colnames(self$records[,(self$dimension+1):length(self$records)])
        , "\n"))
      }
    }
    else {
      #columns argument is NULL
        if (inherits(self, "Parameter")) {
          columns = "value"
        }
        else {
          # variable or equation
          columns = "level"
        }
    }
    tryCatch(
      {
        return(min(self$records[, columns]))
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )

  },

  #' @description getMeanValue get the mean value in value column
  #' @param columns columns over which one wants to get the mean.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  getMeanValue = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", toString(columns), ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)\n"))
      }

      if (is.null(self$records)) {
        return(NA)
      }

      if (inherits(self, "Set")) {
        return(NA)
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension+1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", toString(columns), " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension+1):length(self$records)]
        ,"\n"))
      }
    }
    else {
      #columns argument is NULL
        if (inherits(self, "Parameter")) {
          columns = "value"
        }
        else {
          # variable or equation
          columns = "level"
        }
    }

    tryCatch(
      {
        meanVal = mean(self$records[[columns]])
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  },

  #' @description getMaxAbsValue get the maximum absolute value in value column
  #' @param columns columns over which one wants to get the 
  #' maximum absolute value.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  getMaxAbsValue = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", toString(columns), ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)\n"))
      }

      if (is.null(self$records)) {
        return(NA)
      }

      if (inherits(self, "Set")) {
        return(NA)
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension+1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", toString(columns), " must be a subset",
        " of valid numeric columns", 
        colnames(self$records[,(self$dimension+1):length(self$records)])
        , "\n"))
      }
    }
    else {
      #columns argument is NULL
        if (inherits(self, "Parameter")) {
          columns = "value"
        }
        else {
          # variable or equation
          columns = "level"
        }
    }

    tryCatch(
      {
        return(max(abs(self$records[[columns]])))
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  },

  #' @description whereMax find the row number in records data frame with a 
  #' maximum value (return first instance only)
  #' @param columns columns over which one wants to find the 
  #' domain entry of records with a maximum value.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  whereMax = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", toString(columns), ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)\n"))
      }

      if (is.null(self$records)) {
        return(NA)
      }

      if (inherits(self, "Set")) {
        return(NA)
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension+1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", toString(columns), " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension+1):length(self$records)]
        ,"\n"))
      }
    }
    else {
      #columns argument is NULL
        if (inherits(self, "Parameter")) {
          columns = "value"
        }
        else {
          # variable or equation
          columns = "level"
        }
    }

    tryCatch(
      {
        whereMaxVal = which.max(self$records[[columns]])
        if (is.integer0(whereMaxVal)) {
          return(NA)
        }
        else {
          return(whereMaxVal)
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

  #' @description whereMaxAbs find the row number in records data frame 
  #' with a maximum absolute value (return first instance only)
  #' @description whereMax find the domain entry of records with a 
  #' maximum absolute value (return first instance only)
  #' @param columns columns over which one wants to find the 
  #' domain entry of records with a maximum value.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  whereMaxAbs = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", toString(columns), ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)\n"))
      }

      if (is.null(self$records)) {
        return(NA)
      }

      if (inherits(self, "Set")) {
        return(NA)
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", toString(columns), " must be a subset",
        " of valid numeric columns", 
        toString(colnames(self$records)[(self$dimension + 1):length(self$records)])
        , "\n"))
      }
    }
    else {
      #columns argument is NULL
        if (inherits(self, "Parameter")) {
          columns = "value"
        }
        else {
          # variable or equation
          columns = "level"
        }
    }

    tryCatch(
      {
        whereMaxVal = which.max(abs(self$records[[columns]]))
        if (is.integer0(whereMaxVal)) {
          return(NA)
        }
        else {
          return(whereMaxVal)
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

  #' @description whereMin find the the row number in records data frame 
  #' with a minimum value (return first instance only)
  #' @description whereMax find the domain entry of records with a 
  #' minimum value (return first instance only)
  #' @param columns columns over which one wants to find the 
  #' domain entry of records with a maximum value.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  whereMin = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", toString(columns), ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)\n"))
      }

      if (is.null(self$records)) {
        return(NA)
      }

      if (inherits(self, "Set")) {
        return(NA)
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", toString(columns), " must be a subset",
        " of valid numeric columns", 
        toString(colnames(self$records)[(self$dimension + 1):length(self$records)])
        , "\n"))
      }
    }
    else {
      #columns argument is NULL
        if (inherits(self, "Parameter")) {
          columns = "value"
        }
        else {
          # variable or equation
          columns = "level"
        }
    }

    tryCatch(
      {
          whereMinVal = which.min(self$records[[columns]])
          if (is.integer0(whereMinVal)) {
            return(NA)
          }
          else {
            return(whereMinVal)
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

  #'@description countNA total number of SpecialValues[["NA"]] in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues[["NA"]].
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  countNA = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", toString(columns), ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)\n"))
      }

      if (is.null(self$records)) {
        return(NA)
      }

      if (inherits(self, "Set")) {
        return(NA)
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", toString(columns), " must be a subset",
        " of valid numeric columns", 
        toString(colnames(self$records)[(self$dimension+1):length(self$records)])
        , "\n"))
      }
    }
    else {
      #columns argument is NULL
        if (inherits(self, "Parameter")) {
          columns = "value"
        }
        else {
          # variable or equation
          columns = "level"
        }
    }

    tryCatch(
      {
        return(sum(is.na(self$records[[columns]])))
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  },

  #' @description countEps total number of SpecialValues$EPS in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues$EPS.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  countEps = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", toString(columns), ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)\n"))
      }

      if (is.null(self$records)) {
        return(NA)
      }

      if (inherits(self, "Set")) {
        return(NA)
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", toString(columns), " must be a subset",
        " of valid numeric columns", 
        toString(colnames(self$records[(self$dimension + 1):length(self$records)])
        , "\n")))
      }
    }
    else {
      #columns argument is NULL
        if (inherits(self, "Parameter")) {
          columns = "value"
        }
        else {
          # variable or equation
          columns = "level"
        }
    }

    tryCatch(
      {
        return(sum(
          (self$records[,columns] == SpecialValues$EPS) &&
          (sign(self$records[,columns]) == sign(SpecialValues$EPS))
          ))
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )

  },

  #'@description countUndef total number of SpecialValues$UNDEF in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues$UNDEF.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  countUndef = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", toString(columns), ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)\n"))
      }

      if (is.null(self$records)) {
        return(NA)
      }

      if (inherits(self, "Set")) {
        return(NA)
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", toString(columns), " must be a subset",
        " of valid numeric columns", 
        toString(colnames(self$records)[(self$dimension + 1):length(self$records)])
        , "\n"))
      }
    }
    else {
      #columns argument is NULL
        if (inherits(self, "Parameter")) {
          columns = "value"
        }
        else {
          # variable or equation
          columns = "level"
        }
    }

    tryCatch(
      {
        return(sum(is.nan(self$records[[columns]])))
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  },

  #'@description countPosInf total number of 
  #' SpecialValues$POSINF in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues$POSINF.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.  
  countPosInf = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", toString(columns), ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)\n"))
      }

      if (is.null(self$records)) {
        return(NA)
      }

      if (inherits(self, "Set")) {
        return(NA)
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", toString(columns), " must be a subset",
        " of valid numeric columns", 
        toString(colnames(self$records)[(self$dimension+1):length(self$records)])
        , "\n"))
      }
    }
    else {
      #columns argument is NULL
        if (inherits(self, "Parameter")) {
          columns = "value"
        }
        else {
          # variable or equation
          columns = "level"
        }
    }

    tryCatch(
      {
        return(sum(self$records[[columns]] == SpecialValues$POSINF))
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  },

  #'@description countNegInf total number of 
  #' SpecialValues$NEGINF in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues$NEGINF.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.  
  countNegInf = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)){
        stop(paste0("User input ", toString(columns), ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)\n"))
      }

      if (is.null(self$records)) {
        return(NA)
      }

      if (inherits(self, "Set")) {
        return(NA)
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension+1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", toString(columns), " must be a subset",
        " of valid numeric columns", 
        toString(colnames(self$records)[(self$dimension+1):length(self$records)])
        , "\n"))
      }
    }
    else {
      #columns argument is NULL
        if (inherits(self, "Parameter")) {
          columns = "value"
        }
        else {
          # variable or equation
          columns = "level"
        }
    }

    tryCatch(
      {
        return(sum(self$records[[columns]] == SpecialValues$NEGINF))
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
    assertthat::assert_that(is.logical(verbose), 
    msg = "Argument 'verbose' must be logical")

    assertthat::assert_that(is.logical(force), 
    msg = "Argument 'force' must be logical")

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
        a = array(0, dim = self$shape())
        idx = lapply(self$records[,1:self$dimension], as.numeric)
        a[matrix(unlist(idx), ncol=length(idx))] = self$records[, column]
        return(a)
      }
    }
    else {
      return(NULL)
    }
  },

  .linkDomainCategories = function() {
    if ((!is.null(self$records)) &&(!inherits(self, "Alias"))) {
      for (n in seq_along(self$domain)) {
        i  = self$domain[[n]]
        if (((inherits(i, "Alias")) || (inherits(i, "Set"))) 
        && (!is.null(i$records))) {
          if (i$isValid()) {
            private$.records[, n] = factor(private$.records[, n], levels = i$records[, 1], ordered = TRUE)
          }
          else {
            private$.records[, n] = factor(private$.records[, n], 
            levels = unique(private$.records[, n]), ordered = TRUE)
          }
        }
        else {
          private$.records[, n] = factor(private$.records[, n], 
          levels = unique(private$.records[, n]), ordered = TRUE)
        }
      }
    }
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
          if (self$domainForwarding == TRUE) {
            private$domain_forwarding()
            if (inherits(self$refContainer, "Container")) {
              self$refContainer$.linkDomainCategories()
            }

            for (i in self$refContainer$listSymbols()) {
              self$refContainer$data[[i]]$.requiresStateCheck = TRUE
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
          stop("Argument 'domain_forwarding' must be type logical\n")
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
        assertthat::assert_that(
           (is.numeric(dimension_input)) && 
           (dimension_input %% 1 == 0) && (dimension_input >= 0),
           msg = "Symbol 'dimension' must be type 
           int (greater than or equal to 0)")

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

        for (d in domain_input) {
          assertthat::assert_that((inherits(d, "Set") || 
          inherits(d, "Alias") ||
          is.character(d)),
          msg = "All 'domain' elements must be type Set, Alias, or Character"
          )
          if ((inherits(d, "Set") || 
          inherits(d, "Alias"))) {
            assertthat::assert_that( d$dimension == 1,
            msg = "All linked 'domain' elements must be one dimensional"
            )
          }
        }

      assertthat::assert_that(length(domain_input) <= 20,
      msg = "Argument 'domain' length cannot be > 20")
        domaintemp = list()
        for (d in domain_input) {
          if (is.character(d)) {
            if (inherits(self$refContainer$data[[d]], "Set") ||
            inherits(self$refContainer$data[[d]], "Alias")) {
              domaintemp = append(domaintemp, d)
              # domaintemp = append(domaintemp, 
              # self$refContainer$data[[d]])
            }
            else {
              # attach as a plain string
              domaintemp = append(domaintemp, d)
            }
          }
          else {
            if ((inherits(d, "Set"))) {
              if (identical(d$refContainer,self$refContainer)) {
                domaintemp = append(domaintemp, d)
              }
              else {
                stop("domain elements cannot belong to a different container\n")
              }
            }
            else if (inherits(d, "Alias")) {
              if (identical(d$aliasWith$refContainer, self$refContainer)) {
                domaintemp = append(domaintemp, d)
              }
              else {
                stop("domain elements cannot belong to a different container\n")
              }
            }
          }
        }
        private$.domain = domaintemp
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

        if (!is.null(self$refContainer$data[[name_input]])) {
          stop(paste0("A symbol with the name ", name_input, 
          " already exists in the container\n"))
        }

        if (is.null(private$.name)) {
          self$.requiresStateCheck = TRUE
          private$.name = name_input
        }
        else {
          if (private$.name != name_input) {
            self$.requiresStateCheck = TRUE

            refcontainer = private$.ref_container

            datalist = refcontainer$data
            names(datalist)[names(datalist)== private$.name] = name_input
            refcontainer$data = datalist
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
      regularCheck = list()
      for (d in self$domain) {
        if (inherits(d, "Set") | inherits(d, "Alias")) {
          regularCheck = append(regularCheck, TRUE)
        }
        else {
            regularCheck = append(regularCheck, FALSE)
        }
      }
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
      d = NA
      for (i in self$domain) {
        if (inherits(i, "Set") | inherits(i, "Alias")) {
          if (any(is.na(d))) {
            d = i$name
          }
          else {
            d = append(d, i$name)
          }
        }
        else {
          if (any(is.na(d))) {
            d = i
          }
          else {
            d = append(d, i)
          }
        }
      }
      return(d)
    },

    domainLabels = function() {
      column_names = list()
      for (i in seq_along(self$domain)) {
        if (is.character(self$domain[[i]])) {
          d = self$domain[[i]]
        }
        else {
          d = self$domain[[i]]$name
        }

        if (d != "*") {
          column_names = append(column_names, paste0(d, "_", i))
        }
        else {
          column_names = append(column_names, paste0("uni_", i))
        }
      }
      return(column_names)
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

    .attr = function() {
      return(c("level", "marginal", "lower", "upper", "scale"))
    },

    check = function() {
      if (self$.requiresStateCheck == TRUE) {
        # if regular domain, symbols in domain must be valid
        if (self$domainType == "regular") {
          for (i in self$domain) {
            if (!any(names(self$refContainer$data) == i$name)) {
              stop(paste0("symbol defined over domain symbol ",
              i$name, " however, the object referenced is not in the", 
              " Container anymore -- must reset domain for symbol ", 
              self$name, "\n"))

            }
            if (!identical(i, self$refContainer$data[[i$name]])) {
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

          if (inherits(self, "Variable") | inherits(self, "Equation")){
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
          else if (inherits(self, "Variable") ||
          inherits(self, "Equation")) {
            cols = append(cols, private$.attr())
          }

          if (!identical(unlist(cols), colnames(self$records))) {
            stop(paste0("Records columns must be named and ordered as: ", toString(cols),"\n"))
          }

          if (!all(unlist(lapply(cols, is.character) ))) {
            stop("Domain columns in symbol 'records' must be of type character\n")
          }

          # check if columns are factors
          for (i in self$domainLabels) {
            if (!is.factor(self$records[[i]])) {
              stop(paste0("Domain information in column ", i, " must be a factor\n"))
            }
          }

          # check if domain has records
          for (i in self$domain) {
            if (inherits(i, "Set") || inherits(i, "Alias")) {
              if (is.null(i$records)) {
                stop(paste0("Referenced domain set ", i$name, " does 
                not does not contain records; ",
                "cannot properly establish domain information link."))
              }
            }
          }

          #check if factors are ordered
          for (i in self$domainLabels) {
            if (!is.ordered(self$records[[i]])) {
              stop(paste0("Domain information in column ", i, 
              " must be an ORDERED factor\n"))
            }
          }
          
          # check for domain violations
          if (self$dimension != 0) {
            nullrecords = self$records[,1:self$dimension][is.null(self$records[,1:self$dimension])]
            narecords = self$records[,1:self$dimension][is.na(self$records[,1:self$dimension])]

            if (length(nullrecords) != 0 || 
            length(narecords) != 0 ) {
              stop(paste0("Symbol 'records' contain domain violations;",
              " ensure that all domain elements have",
              " been mapped properly to a factor\n"))
            }
          }

          # drop duplicates
          if (self$dimension != 0) {
            if (nrow(self$records) != nrow(unique(self$records[unlist(self$domainLabels)]))) {
              stop(paste0("Symbol 'records' contain non-unique",
               " domain members; ensure that only unique members exist\n"))
            }
          }

          # check if all data columns are float
          if (inherits(self, "Variable") | 
          inherits(self, "Parameter") | 
          inherits(self, "Equation")) {
            for (i in (self$dimension + 1):length(self$records)) {
              if (!all(is.numeric(self$records[, i]))) {
                stop("Data in column", i, " must be numeric\n")
              }
            }
          }
        }

      }
      self$.requiresStateCheck = FALSE
    },

    domain_forwarding = function() {
    # find symbols to grow
    for (diter in seq_len(self$dimension)) {
      d = self$domain[[diter]]
      dl = self$domainLabels[[diter]]
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
        dim = (self$refContainer$data[[i]]$domainLabels)[[1]]
        if (!is.null(self$refContainer$data[[i]]$records)) {
          recs = self$refContainer$data[[i]]$records
          assert_that((self$refContainer$data[[i]]$dimension == 1),
          msg = "attempting to forward a domain set that has dimension > 1")

          df = self$records[dl]
          colnames(df) = dim
          df[["element_text"]] = ""
          recs = rbind(recs, df)
          recs = recs[!duplicated(recs[[dim]]),]
        }
        else {
          recs = unique(self$records[dl])
          colnames(recs) = dim
          recs[["element_text"]] = ""
        }
        self$refContainer$data[[i]]$records = recs
      }
    }
  }
  )
)

#' @title Set Class
#' @description A class for Set objects. This class inherits from an abstract symbol class.
#' To access the documentation for the methods getCardnality, getSparsity, 
#' and isValid, please use help(Symbol).
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
  inherit = Symbol,
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
      if (!isSingleton) {
        type = GMS_DT_SET
        subtype = SetTypeSubtype()[["set"]]
      }
      else {
        type = GMS_DT_SET
        subtype = SetTypeSubtype()[["singleton_set"]]
      }

      super$initialize(container, name,
                      type, subtype,
                      domain, description, domainForwarding)

      if (!is.null(records)) {
        self$setRecords(records)
      }
      private$.is_alias = FALSE
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
      colnames(records) = columnNames

      self$records = records
      self$.linkDomainCategories()
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
        "domain_objects" = self$domain,
        "domainNames" = self$domainNames,
        "dimension" = self$dimension,
        "description" = self$description,
        "numberRecords" = self$numberRecords,
        "domainType" = self$domainType
      ))
    },

    isAlias = function() {
      return(private$.is_alias)
    }
  ),
  private = list(
    .is_singleton = NULL,
    .is_alias = NULL
  )
  )

#' @title Parameter Class
#' @description A class for Parameter objects. This class inherits from an abstract 
#' symbol class.To access the documentation for any of the following methods, 
#' use help(Symbol).
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
  inherit = Symbol,
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

      type = GMS_DT_PAR
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
      if (is.array(records)) { # checks for matrix + arrays
        if ((length(records) > 1) && (self$domainType != "regular")) {
          stop(paste0(
            "Data conversion for non-scalar array (i.e., matrix) format into 
            records is only possible for symbols where 
            self.domain_type = 'regular'. 
            Must define symbol with specific domain set objects, 
            symbol domain_type is currently ",self$domainType,".\n" ))
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
          if (!all(dim(records) == self$shape())) {
            stop(paste0("User passed array/matrix with shape ", toString(dim(records)), " but anticipated 
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

        assertthat::assert_that(
          c == self$dimension + 1,
          msg = paste0("Dimensionality of records ", c - 1, 
          " is inconsistent with parameter domain specification ", 
          self$dimension)
        )

        columnNames = self$domainLabels
        columnNames = append(columnNames, "value")
        # columnNames = self$getColLabelsForRecords()
        colnames(records) = columnNames

        #if records "value" is not numeric, stop.
        if (any(!is.numeric(records[,length(records)]))) {
          stop("All entries in the 'values' column of a parameter must be numeric.\n")
        }
        self$records = records
        self$.linkDomainCategories()
      }
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
        "is_scalar" = self$isScalar,
        "domain_objects" = self$domain,
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
#' symbol class.To access the documentation for any of the following methods, 
#' use help(Symbol).
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
  inherit = Symbol,
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

      symtype = GMS_DT_VAR
      symsubtype = VarTypeSubtype()[[type]]

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
      if ( (is.list(records) && is.array(records[[1]])) || is.array(records)) {

        if (is.array(records)){
          records= list(level = records) # default to level
        }

        #check if user attributes are valid
        if (length(intersect(private$.attr(), names(records))) == 0) {
          stop(paste0("Unrecognized user attribute detected in `records`. 
          The attributes must be one of the following", toString(private$.attr()),
          "and must be passed as names of a named list.\n"))
        }
        # check if all records have equal size
        size1 = dim(records[[1]])
        size = lapply(records, dim)

        for (i in seq_along(records)) {
          if(!all(dim(records[[i]]) == size1)) {
            stop("array sizes passed into records must be all equal.\n")
          }
        }

        if ((length(records) > 1) && (self$domainType != "regular")) {
          stop(paste0(
            "Data conversion for non-scalar array (i.e., matrix) format into 
            records is only possible for symbols where 
            self.domain_type = 'regular'. 
            Must define symbol with specific domain set objects, 
            symbol domain_type is currently ",self$domainType,".\n" ))
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
          " is inconsistent with equation domain specification ", 
          self$dimension, " must resolve before records can be added\n\n",
          "NOTE:",
          "columns not named ", toString(private$.attr()),
          " will be interpreted as domain columns, check that the data.frame conforms",
          "to the required notation.\n",
          "User passed data.frame with columns: ", toString(usr_colnames), "\n")))
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
        colnames(records) = columnNames

        self$records = records
        self$.linkDomainCategories()

      }
    }

  ),

  active = list(
    type = function(type_input) {
      if (missing(type_input)) {
        return(private$.type)
      }
      else {
        if (!any(private$.var_types == type_input)) {
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
        "domain_objects" = self$domain,
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
    .var_types = c(
      "binary",
      "integer",
      "positive",
      "negative",
      "free",
      "sos1",
      "sos2",
      "semicont",
      "semiint"
    ),

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
#' symbol class.To access the documentation for any of the following methods, 
#' use help(Symbol).
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
  inherit = Symbol,
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
      type = private$.equationTypes[[type]]

      symtype = GMS_DT_EQU
      symsubtype = EqTypeSubtype()[[type]]


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
      if ( (is.list(records) && is.array(records[[1]])) || is.array(records)) {

        if (is.array(records)){
          records= list(level = records) # default to level
        }

        #check if user attributes are valid
        if (length(intersect(private$.attr(), names(records))) == 0) {
          stop(paste0("Unrecognized user attribute detected in `records`. 
          The attributes must be one of the following", toString(private$.attr()),
          "and must be passed as names of a named list.\n"))
        }
        # check if all records have equal size
        size1 = dim(records[[1]])
        size = lapply(records, dim)

        for (i in seq_along(records)) {
          if(!all(dim(records[[i]]) == size1)) {
            stop("array sizes passed into records must be all equal.\n")
          }
        }

        if ((length(records) > 1) && (self$domainType != "regular")) {
          stop(paste0(
            "Data conversion for non-scalar array (i.e., matrix) format into 
            records is only possible for symbols where 
            self.domain_type = 'regular'. 
            Must define symbol with specific domain set objects, 
            symbol domain_type is currently ",self$domainType,".\n" ))
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
          " will be interpreted as domain columns, check that the data.frame conforms",
          "to the required notation.\n",
          "User passed data.frame with columns: ", toString(usr_colnames), "\n")))
        }

        # reorder columns
        correct_order = c()
        if (self$dimension > 0) {
          correct_order = colnames(records)[(1:self$dimension)]
        }
        correct_order = append(correct_order, private$.attr())
        records = records[, correct_order]

        columnNames = append(columnNames, private$.attr())
        colnames(records) = columnNames

        self$records = records
        self$.linkDomainCategories()

      }
    }
  ),

  active = list(
    type = function(type_input) {
      if (missing(type_input)) {
        return(private$.type)
      }
      else {
        if (!any(private$.equationTypes == type_input)) {
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
        "domain_objects" = self$domain,
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
    .equationTypes = list(
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
    ),

    .default_values = list(
      "eq" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = SpecialValues$NEGINF,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "geq" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = SpecialValues$NEGINF,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "leq" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = SpecialValues$NEGINF,
          "upper" = SpecialValues$POSINF,
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
          "lower" = SpecialValues$NEGINF,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "external" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = SpecialValues$NEGINF,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "boolean" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = SpecialValues$NEGINF,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      )
    )
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
  public = list(
    .gams_type = NULL,
    .gams_subtype = NULL,
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
    initialize = function(container=NULL, name=NULL, 
                          aliasFor=NULL) {
      self$.requiresStateCheck = TRUE
      self$refContainer = container
      self$name = name
      refcontainer = self$refContainer
      refcontainer$data[[name]] = self
      self$.gams_type = GMS_DT_ALIAS
      self$.gams_subtype = 1
      private$.is_alias = TRUE
      self$aliasWith = aliasFor
    },


    #' @description getCardinality get the full cartesian product of the domain
    getCardinality = function() {
      return(self$refContainer$data[[self$aliasWith$name]]$getCardinality())
    },


    #' @description getSparsity get the sparsity of the symbol 
    #' w.r.t the cardinality
    getSparsity = function() {
      return(self$refContainer$data[[self$aliasWith$name]]$getSparsity())
    },

    #' @description TRUE if the symbol is in a valid format, 
    #' throw exceptions if verbose=True, recheck a symbol if force=True
    #' @param verbose type logical
    #' @param force type logical
    isValid = function(verbose=FALSE, force=FALSE) {
      assertthat::assert_that(is.logical(verbose),
      msg = "Argument 'verbose' must be logical")

      assertthat::assert_that(is.logical(force),
      msg = "Argument 'force' must be logical")

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


    #' main convenience method to set standard dataframe formatted records
    #' @param records specify set records as a string vector or a dataframe.
    setRecords = function(records) {
      return(self$refContainer$data[[self$aliasWith$name]]$setRecords(records))
    }
  ),

  active = list(
    refContainer = function(ref_container_input) {
      if (missing(ref_container_input)) {
        return(private$.ref_container)
      }
      else {
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
          " max is ", private$symbolMaxLength, " characters"))
        }

        if (!is.null(self$refContainer$data[[name_input]])) {
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
    },

    aliasWith = function(alias_with_input) {
      if (missing(alias_with_input)) {
        return(private$.aliasWith)
      }
      else {
        if (!((inherits(alias_with_input, "Set")) || 
        (inherits(alias_with_input, "Alias") ))) {
          stop("GAMS 'alias_with' must be type Set or Alias\n")
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
            if (inherits(self$refContainer, "Container")) {
              self$refContainer$.requiresStateCheck = TRUE
            }
          }
        }
      }
    },

    isSingleton = function(is_singleton) {
      if (missing(is_singleton)) {
        refcontainer = self$refContainer
        sym = refcontainer$data[[self$aliasWith$name]]
        return(sym$isSingleton)
      }
      else {
        refcontainer = self$refContainer
        sym = refcontainer$data[[self$aliasWith$name]]
        sym$isSingleton = is_singleton
      }
    },

    description = function(description_input) {
      if (missing(description_input)) {
        refcontainer = self$refContainer
        aliaswithname = self$aliasWith$name
        sym = refcontainer$data[[aliaswithname]]
        return(sym$description)
      }
      else {
        refcontainer = self$refContainer
        aliaswithname = self$aliasWith$name
        sym = refcontainer$data[[aliaswithname]]
        sym$description = description_input
      }
    },

    dimension = function(dimension_input) {
      if (missing(dimension_input)) {
        return(self$refContainer$data[[self$aliasWith$name]]$dimension)
      }
      else {
        refcontainer = self$refContainer
        sym = refcontainer$data[[self$aliasWith$name]]
        sym$dimension = dimension_input
      }
    },

    records = function(records_input) {
      return(self$refContainer$data[[self$aliasWith$name]]$records)
    },

    domain = function(domain_input) {
      if (missing(domain_input)) {
        return(self$refContainer$data[[self$aliasWith$name]]$domain)
      }
      else {
        refcontainer = self$refContainer
        sym = refcontainer$data[[self$aliasWith$name]]
        sym$domain = domain_input
      }
    },

    numberRecords = function() {
      return(self$refContainer$data[[self$aliasWith$name]]$numberRecords)
    },

    domainType = function() {
      return(self$refContainer$data[[self$aliasWith$name]]$domainType)
    },

    domainNames = function() {
      return(self$refContainer$data[[self$aliasWith$name]]$domainNames)
    },

    domainLabels = function() {
      return(self$refContainer$data[[self$aliasWith$name]]$domainLabels)
    },

    summary = function() {
    return(list(
      "name" = self$name,
      "alias_with" = self$aliasWith,
      "alias_with_name" = self$aliasWith$name,
      "isSingleton" = self$isSingleton,
      "domain_objects" = self$domain,
      "domainNames" = self$domainNames,
      "dimension" = self$dimension,
      "description" = self$description,
      "numberRecords" = self$numberRecords
    ))
    },

    isAlias = function() {
      return(private$.is_alias)
    }
  ),

  private = list(
    symbolMaxLength = 63,
    .ref_container = NULL,
    .name = NULL,
    .aliasWith = NULL,
    .is_alias = NULL,

    check = function() {
      if (self$.requiresStateCheck == TRUE) {
        if (self$refContainer$data[[self$aliasWith$name]]$isValid() == FALSE) {
          stop(paste0("Alias symbol is not valid because parent set ", self$aliasWith$name,
          "is not valid\n"))
        }
      }
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

#is.nan function for dataframe
is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

#is.infinite function for dataframe
# is.infinite.data.frame <- function(x)
# do.call(cbind, lapply(x, is.infinite))
