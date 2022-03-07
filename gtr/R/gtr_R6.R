# library(R6)
# library(assertthat)

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
#' This is GDX Container class
#' @description Container is the unified object that contains data
#' @field data is a list containgin all symbols
#' @examples
#' Container$new()
#' @export
Container <- R6::R6Class (
  "Container",
  public = list(
    systemDirectory = NULL,
    data = NULL,
    acronyms = NULL,
    .requiresStateCheck = NULL,
    #' @description
    #' Create a new container
    #' @details read a file using 
    #' \href{../../gtr/html/Container.html#method-read}{\code{$read()}}
    #' @param load_from name of the GDX file to load data from
    #' @param system_directory optional argument for the path to GAMS System directory
    initialize = function(load_from=NULL, system_directory=NULL) {

      if (missing(system_directory)) {
        self$systemDirectory = find_gams()

      }
      else {
        if (R.utils::isAbsolutePath(system_directory)) {
          self$systemDirectory = system_directory
        }
        else {
          stop("must enter valid full (absolute) path to GAMS system_directory\n")
        }
      }

      self$acronyms = list()
      self$data = list()
      self$.requiresStateCheck = TRUE

      if (!missing(load_from)) {
      self$read(load_from, symbols="all")

      }
    },

    #' @description read data from a GDX file
    #' @details 
    #' `$read()` reads a file
    #' @param load_from name of the file to load data from
    #' @symbols optional argument to specify the names of the symbols to be read
    #' @values optional boolean argument to specify whether to read symbol records
    read = function(load_from, symbols="all", values=TRUE) {
      # read metadata
      # get all symbols and metadata from c++
      # process it and populate various fields

      # check if values is boolean
      if (!is.logical(values)) {
        stop("values must be type logical\n")
      }

      if (!(is.character(symbols)) && !(is.list(symbols))) {
        stop("argument symbols must be of the type list or string\n")
      }

      for (s in symbols) {
        if (!is.character(s)) {
          stop("argument symbols must contain only type string\n")
        }
      }

      if (!is.character(load_from)) {
        stop("The argument load_from must be of type string\n")
      }
      else {
        namesplit = strsplit(load_from, "\\.")
        ext = tail(unlist(namesplit), 1)
        if (ext != "gdx") {
          stop("check filename extension, must be .gdx\n")
        }
        load_from = R.utils::getAbsolutePath(path.expand(load_from))
        if (!file.exists(load_from)) {
          stop(paste0("File ", load_from, " doesn't exist\n"))
        }
      }
      # check acronyms
      acrInfo = checkAcronyms(load_from, self$systemDirectory)
      nAcr = acrInfo[["nAcronyms"]]
      if (nAcr != 0) {
        warning("GDX file contains acronyms. 
        Acronyms are not supported and are set to GAMS NA.\n")
        self$acronyms = acrInfo[["acronyms"]]
      }

      # get names for all symbols
      metadata = getSymbols(load_from, self$systemDirectory)
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
            # m1 = m[-1]
            if (m$type == GMS_DT_PAR) {
              Parameter$new(
                self, m$name, m$domain,
                domain_forwarding=FALSE,
                description = m$expltext)
            }
            else if (m$type == GMS_DT_SET) {
                if (m$subtype == 0) {
                Set$new(
                self, m$name, m$domain, FALSE,
                records = NULL,
                domain_forwarding=FALSE,
                m$expltext)
                }
                else {
                Set$new(
                self, m$name, m$domain, TRUE,
                records = NULL,
                domain_forwarding=FALSE, 
                m$expltext)
                }
            }
            else if (m$type == GMS_DT_VAR) {
                type = names(VarTypeSubtype())[[which(VarTypeSubtype() == m$subtype)]]
                Variable$new(
                self, m$name, type, m$domain,
                domain_forwarding = FALSE,
                description = m$expltext)
            }
            else if (m$type == GMS_DT_EQU) {
                type = names(EqTypeSubtype())[[which(EqTypeSubtype() == m$subtype)]]
                Equation$new(
                self, m$name, type, m$domain,
                domain_forwarding = FALSE,
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

      if (values == TRUE) {
        symbolrecords = readSymbols(unlist(symbolsToRead),
        load_from, self$systemDirectory)

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

    getUniverseSet = function() {
      uni = list()
      for (i in self$listSymbols(isValid = TRUE)) {
        if (!is.null(self$data[[i]]$records)) {
          if (self$data[[i]]$dimension > 0) {
            uni = append(uni, data.frame(unlist(x = 
            self$data[[i]]$records[, (1:self$data[[i]]$dimension)]))[, 1])
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

    removeSymbols = function(name = NULL) {
      if (!(is.character(name) || is.vector(name) || is.list(name))) {
        stop("Argument 'name' must be of type string, list, or vector\n")
      }

      if (!all(unlist(lapply(name, is.character)))) {
        stop("Argument 'name' must contain only type character\n")
      }

      for (n in name) {
        self$data[[n]] <- NULL
      }

      self$.requiresStateCheck = TRUE
    },

    renameSymbol = function(old_name = NULL, new_name = NULL) {
      if (!is.character(old_name)) {
        stop("Argument 'old_name' must be type character\n")
      }

      if (!is.character(new_name)) {
        stop("Argument 'new_name' must be type character\n")
      }

      if (is.null(self$data[[old_name]])) {
        stop(paste0("Symbol ", old_name, " does not exist\n"))
      }

      if (old_name != new_name) {
        sym = self$data[[old_name]]
        sym$name = new_name
        self$.requiresStateCheck = TRUE
      }
    },

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

    addSet = function(name, domain = "*", is_singleton = FALSE,
    records = NULL, domain_forwarding=FALSE, description = "") {
      Set$new(
      self, name, domain, is_singleton,
      records, domain_forwarding, description)
      return(self$data[[name]])
    },

    addParameter = function(name, domain = list(),
    records = NULL, domain_forwarding=FALSE, description = "") {
      Parameter$new(
        self, name, domain, records,
        domain_forwarding, description)
        return(self$data[[name]])
    },

    addVariable = function(name, type="free", domain = list(),
    records = NULL, domain_forwarding=FALSE, description = "") {
      Variable$new(
        self, name, type, domain, records,
        domain_forwarding, description)
        return(self$data[[name]])
    },

    addEquation = function(name, type, domain = list(), 
    records = NULL, domain_forwarding=FALSE, description = "") {
      Equation$new(
        self, name, type, domain, records,
        domain_forwarding, description)
        return(self$data[[name]])
    },

    addAlias = function(name, alias_with) {
      Alias$new(
      self, name, alias_with)
      return(self$data[[name]])
    },

    describeSets = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$listSets()
      }
      colNames = list("name",
            "is_alias",
            "is_singleton",
            "domain",
            "domain_type",
            "dim",
            "num_recs",
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
            self$data[[i]]$isAlias(),
            self$data[[i]]$isSingleton,
            paste(self$data[[i]]$domain_names(), sep = "", collapse = " "),
            self$data[[i]]$domain_type(),
            self$data[[i]]$dimension,
            self$data[[i]]$number_records,
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

    describeParameters = function(symbols = NULL) {
      if (is.null(symbols)) {
        symbols = self$listParameters()
      }
      colNames = list(
            "name",
            "is_scalar",
            "domain",
            "domain_type",
            "dim",
            "num_recs",
            "min_value",
            "mean_value",
            "max_value",
            "where_min",
            "where_max",
            "count_eps",
            "count_na",
            "count_undef",
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
            paste(self$data[[i]]$domain_names(), sep = "", collapse = " "),
            self$data[[i]]$domain_type(),
            self$data[[i]]$dimension,
            self$data[[i]]$number_records,
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

    describeVariables = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$listVariables()
      }
      colNames = list(
            "name",
            "type",
            "domain",
            "domain_type",
            "dim",
            "num_recs",
            "cardinality",
            "sparsity",
            "min_level",
            "mean_level",
            "max_level",
            "where_max_abs_level",
            "count_eps_level",
            "min_marginal",
            "mean_marginal",
            "max_marginal",
            "where_max_abs_marginal",
            "count_eps_marginal"
            )
      df = data.frame(matrix(NA, nrow = 
      length(symbols), ncol = length(colNames)))
      rowCount = 0

      for (i in symbols) {
        if (any(self$listVariables() == i)) {
          symDescription = list(
            i,
            self$data[[i]]$type,
            paste(self$data[[i]]$domain_names(), sep = "", collapse = " "),
            self$data[[i]]$domain_type(),
            self$data[[i]]$dimension,
            self$data[[i]]$number_records,
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

    describeEquations = function(symbols=NULL) {
      if (is.null(symbols)) {
        symbols = self$listEquations()
      }
      colNames = list(
            "name",
            "type",
            "domain",
            "domain_type",
            "dim",
            "num_recs",
            "cardinality",
            "sparsity",
            "min_level",
            "mean_level",
            "max_level",
            "where_max_abs_level",
            "count_eps_level",
            "min_marginal",
            "mean_marginal",
            "max_marginal",
            "where_max_abs_marginal",
            "count_eps_marginal"
            )
      df = data.frame(matrix(NA, nrow = 
      length(symbols), ncol = length(colNames)))
      rowCount = 0

      for (i in symbols) {
        if (any(self$listEquations() == i)) {
          symDescription = list(
            i,
            self$data[[i]]$type,
            paste(self$data[[i]]$domain_names(), sep = "", collapse = " "),
            self$data[[i]]$domain_type(),
            self$data[[i]]$dimension,
            self$data[[i]]$number_records,
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

    write = function(gdxout, compress = FALSE, uel_priority = NULL) {
      if (!is.logical(compress)) {
        stop("'compress' must be of type bool; default False (no compression)\n")
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

      if (!is.null(uel_priority)) {
        if (!(is.character(uel_priority) || is.list(uel_priority))) {
          stop("'uel_priority' must be type list or str\n")
        }
      }

      if (!identical(self$listSymbols(), self$listSymbols(isValid=TRUE) )) {
        stop(paste0("There are symbol(s) in Container that are not valid;",
         "all symbols must be valid before writing",
         " (i.e., <symbol object>$isValid() == TRUE)\n"))
      }

      private$validSymbolOrder()

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

      if (is.null(uel_priority)) {
        gdxWriteSuper(self$data, self$systemDirectory, 
        gdxout, NA, FALSE, compress)
      }
      else {
        universe = self$getUniverseSet()
        if ((is.null(universe)) ||
        (!setequal(intersect(uel_priority, universe), uel_priority))) {
          stop("uel_priority must be a subset of the universe, check 
          spelling of an element in uel_priority? Also check 
          getUniverseSet() method for assumed UniverseSet.\n")
        }

        reorder = uel_priority
        reorder = append(reorder, universe)
        reorder = unique(reorder)

        gdxWriteSuper(self$data, self$systemDirectory, 
        gdxout, unlist(reorder), TRUE, compress)
      }


    },

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

        if (all(self$listSymbols() != self$listSymbols(isValid = TRUE))) {
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
        sym = symbolsToSort[idx]
        # special 1D sets (universe domain & relaxed sets)
        if (inherits(self$data[[sym]], "Set") &&
        self$data[[sym]]$dimension == 1 &&
        is.character(self$data[[sym]]$domain[1])
        ) {
          orderedSymbols = append(orderedSymbols, sym)
          symbolsToSort = symbolsToSort[-1]
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

    reOrderSymbols = function() {
      orderedSymbols = private$validSymbolOrder()
      datacopy = self$data
      self$data = list()
      for (s in orderedSymbols) {
        self$data[[s]] = datacopy[[s]]
      }
    },

    isValidSymbolOrder = function() {
      validOrder = private$validSymbolOrder()
      print("valid order")
      print(validOrder)
      currentOrder = names(self$data)
      h = list()
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
                        domain_forwarding) {
    self$.gams_type = type
    self$.gams_subtype = subtype

    self$.requiresStateCheck = TRUE
    self$ref_container = container
    self$ref_container$.requiresStateCheck = TRUE

    self$name <- name
    self$ref_container$data[[name]] = self

    self$records = NULL

    self$domain = domain

    self$dimension = length(self$domain)
    self$description = description
    self$domain_forwarding = domain_forwarding

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

  domain_type = function() {
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

  getCardinality = function() {
    tryCatch(
      {
        if (self$domain_type() == "relaxed" | self$domain_type() == "none"){
          return(NA)
        }
        else {
          card = 1
          for (i in self$domain) {
            card = card * i$number_records
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
        if (self$domain_type() == "relaxed" | self$domain_type() == "none"){
          return(NA)
        }
        else {
          return(1 - self$number_records/self$getCardinality())
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

  getMaxValue = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
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
        stop(paste0("User entered column '", columns, "' must be a subset",
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

  getMinValue = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input '", columns, 
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

  getMeanValue = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
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
        stop(paste0("User entered column ", columns, " must be a subset",
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
  getMaxAbsValue = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
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
        stop(paste0("User entered column ", columns, " must be a subset",
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

  whereMax = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
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
        stop(paste0("User entered column ", columns, " must be a subset",
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

  whereMaxAbs = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
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
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension + 1):length(self$records)]
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

  whereMin = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
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
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension + 1):length(self$records)]
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

  countNA = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
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
        stop(paste0("User entered column ", columns, " must be a subset",
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

  countEps = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
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
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records[(self$dimension + 1):length(self$records)]
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
        return(sum(self$records[,columns] == SpecialValues$EPS))
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )

  },

  countUndef = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
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
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension + 1):length(self$records)]
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

  countPosinf = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
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
        stop(paste0("User entered column ", columns, " must be a subset",
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

  countNeginf = function(columns=NULL) {
    if (!is.null(columns)) {
      if (!is.character(columns)){
        stop(paste0("User input ", columns, ", however it is only possible to",
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
        stop(paste0("User entered column ", columns, " must be a subset",
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

  domain_names = function() {
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
  },

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
    if (self$domain_type() == "regular") {
      shapelist = list()
      for (d in self$domain) {
        shapelist = append(shapelist, nrow(d$records))
      }
      return(shapelist)
    }

    if (!is.null(self$records)) {
      if (self$dimension == 0) {
        return(list())
      }

      if (self$domain_type() == "none" || self$domain_type() == "relaxed") {
        shapelist = list()
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
        a = array(0, dim = unlist(self$shape()))
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
            private$.records[, n] = factor(private$.records[, n], ordered = TRUE)
          }
        }
        else {
          private$.records[, n] = factor(private$.records[, n], ordered = TRUE)
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
          if (self$domain_forwarding == TRUE) {
            private$domainForwarding()
            if (inherits(self$ref_container, "Container")) {
              self$ref_container$.linkDomainCategories()
            }

            for (i in self$ref_container$listSymbols()) {
              self$ref_container$data[[i]]$.requiresStateCheck = TRUE
            }

            self$ref_container$.requiresStateCheck = TRUE
          }
          else {
              self$.requiresStateCheck = TRUE
              if (inherits(self$ref_container, "Container")) {
                self$ref_container$.requiresStateCheck = TRUE
              }
          }
        }
      }
    },

    domain_forwarding = function(domain_forwarding_input) {
      if (missing(domain_forwarding_input)) {
        return(private$.domain_forwarding)
      }
      else {
        if (!is.logical(domain_forwarding_input)) {
          stop("Argument 'domain_forwarding' must be type bool\n")
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
            self$ref_container$.requiresStateCheck = TRUE
          }
          private$.description = description_input
        }
        else {
          self$.requiresStateCheck = TRUE
          self$ref_container$.requiresStateCheck = TRUE
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
            if (inherits(self$ref_container$data[[d]], "Set") ||
            inherits(self$ref_container$data[[d]], "Alias")) {
              domaintemp = append(domaintemp, d)
              # domaintemp = append(domaintemp, 
              # self$ref_container$data[[d]])
            }
            else {
              # attach as a plain string
              domaintemp = append(domaintemp, d)
            }
          }
          else {
            if ((inherits(d, "Set"))) {
              if (identical(d$ref_container,self$ref_container)) {
                domaintemp = append(domaintemp, d)
              }
              else {
                stop("domain elements cannot belong to a different container\n")
              }
            }
            else if (inherits(d, "Alias")) {
              if (identical(d$aliasWith$ref_container, self$ref_container)) {
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

    ref_container = function(ref_container_input) {
      if (missing(ref_container_input)) {
        return(private$.ref_container)
      }
      else {
        if (!inherits(ref_container_input, "Container")) {
          stop("Symbol 'container' must be type Container\n")
        }
        if (is.null(self$ref_container)){
          if (!identical(self$ref_container, ref_container_input)) {
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

        if (!is.null(self$ref_container$data[[name_input]])) {
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
    
    number_records = function() {
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
        if (self$domain_type() == "regular") {
          for (i in self$domain) {
            if (!any(names(self$ref_container$data) == i$name)) {
              stop(paste0("symbol defined over domain symbol ",
              i$name, " however, the object referenced is not in the", 
              " Container anymore -- must reset domain for symbol ", 
              self$name, "\n"))

            }
            if (!identical(i, self$ref_container$data[[i$name]])) {
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
          cols = self$domainLabels()
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
          for (i in self$domainLabels()) {
            if (!is.factor(self$records[[i]])) {
              stop(paste0("Domain information in column ", i, " must be a factor\n"))
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
            if (nrow(self$records) != nrow(unique(self$records))) {
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

    domainForwarding = function() {
    # find symbols to grow
    for (diter in seq_len(self$dimension)) {
      d = self$domain[[diter]]
      dl = self$domainLabels()[[diter]]
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
        dim = (self$ref_container$data[[i]]$domainLabels())[[1]]
        if (!is.null(self$ref_container$data[[i]]$records)) {
          recs = self$ref_container$data[[i]]$records
          assert_that((self$ref_container$data[[i]]$dimension == 1),
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
        self$ref_container$data[[i]]$records = recs
      }
    }
  }
  )
)

Set <- R6Class(
  "Set",
  inherit = Symbol,
  public = list(
    initialize = function(container=NULL, gams_name=NULL,
                          domain="*", is_singleton=FALSE,
                          records = NULL, 
                          domain_forwarding = FALSE,
                          description="") {
      self$isSingleton <- is_singleton
      if (!is_singleton) {
        type = GMS_DT_SET
        subtype = SetTypeSubtype()[["set"]]
      }
      else {
        type = GMS_DT_SET
        subtype = SetTypeSubtype()[["singleton_set"]]
      }

      super$initialize(container, gams_name,
                      type, subtype,
                      domain, description, domain_forwarding)

      if (!is.null(records)) {
        self$setRecords(records)
      }
      private$is_alias = FALSE
      invisible(self)
    },

    setRecords = function(records) {
      # # if records is a list, unlist
      # if (is.list(records)) {
      #   records = unlist(records)
      # }

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
        stop(paste0("The argument 'records' is of length",
        c, " Expecting ", self$dimension + 1, "\n"))
      }
      columnNames = self$domainLabels()
      columnNames = append(columnNames, "element_text")
      # columnNames = self$getColLabelsForRecords()
      colnames(records) = columnNames

      self$records = records
      self$.linkDomainCategories()
    },

    isAlias = function() {
      return(private$is_alias)
    },

    summary = function() {
      return(list(
        "name" = self$name,
        "is_singleton" = self$isSingleton,
        "domain_objects" = self$domain,
        "domain_names" = self$domain_names(),
        "dimension" = self$dimension,
        "description" = self$description,
        "number_records" = self$number_records,
        "domain_type" = self$domain_type()
      ))
    }
  ),

  active = list(
    isSingleton = function(is_singleton) {
      if (missing(is_singleton)) {
        return(private$is_singleton)
      }
      else {
        if (!is.logical(is_singleton)) {
          stop("Argument 'is_singleton' must be type bool\n")
        }
        private$is_singleton = is_singleton
      }

      }
  ),
  private = list(
    is_singleton = NULL,
    is_alias = NULL
  )
  )

Parameter <- R6Class(
  "Parameter",
  inherit = Symbol,
  public = list(
    initialize = function(container=NULL, gams_name=NULL,
                          domain=list(),records = NULL,
                          domain_forwarding = FALSE,
                          description="") {

      type = GMS_DT_PAR
      super$initialize(container, gams_name,
                      type, 0, 
                      domain, description, domain_forwarding)

      if (!is.null(records)) {
        self$setRecords(records)
      }
    },

    setRecords = function(records) {
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

      columnNames = self$domainLabels()
      columnNames = append(columnNames, "value")
      # columnNames = self$getColLabelsForRecords()
      colnames(records) = columnNames

      #if records "value" is not numeric, stop.
      if (any(!is.numeric(records[,length(records)]))) {
        stop("All entries in the 'values' column of a parameter must be numeric.\n")
      }
      self$records = records
      self$.linkDomainCategories()
    },

    summary = function() {
      return(list(
        "name" = self$name,
        "is_scalar" = self$isScalar,
        "domain_objects" = self$domain,
        "domain_names" = self$domain_names(),
        "dimension" = self$dimension,
        "description" = self$description,
        "number_records" = self$number_records,
        "domain_type" = self$domain_type()
      ))
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
    }
  )
)

Variable <- R6Class(
  "Variable",
  inherit = Symbol,
  public = list(
    initialize = function(container = NULL, gams_name = NULL, 
                          type = "free",
                          domain = list(), records = NULL,
                          domain_forwarding = FALSE,
                          description="") {

      self$type = type

      symtype = GMS_DT_VAR
      symsubtype = VarTypeSubtype()[[type]]

      super$initialize(container, gams_name,
                      symtype, symsubtype, 
                      domain, description, domain_forwarding)
      if (!is.null(records)) {
        self$setRecords(records)
      }
    },

    setRecords = function(records) {
      # check if records is a dataframe and make if not
      records = data.frame(records)
      usr_colnames = colnames(records)

      columnNames = self$domainLabels()
      if (self$dimension +  1 > length(usr_colnames)) {
        usr_attr = NULL
      }
      else {
        usr_attr=  usr_colnames[(self$dimension + 1):length(usr_colnames)]
      }

      for (i in setdiff(private$.attr(), usr_attr)) {
        records[i] = private$default_values[[private$.type]][[i]]
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
        "User passed data.frame with columns: ", usr_colnames, "\n")))
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
    },

    summary = function() {
      return(list(
        "name" = self$name,
        "type" = self$type,
        "domain_objects" = self$domain,
        "domain_names" = self$domain_names(),
        "dimension" = self$dimension,
        "description" = self$description,
        "number_records" = self$number_records,
        "domain_type" = self$domain_type()
      ))
    }
  ),

  active = list(
    type = function(type_input) {
      if (missing(type_input)) {
        return(private$.type)
      }
      else {
        if (!any(private$var_types == type_input)) {
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
    }
  ),

  private = list(
    .type= NULL,
    var_types = c(
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

    default_values = list(
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



Equation <- R6Class(
  "Equation",
  inherit = Symbol,
  public = list(

    initialize = function(container=NULL, gams_name=NULL, 
                          type=NULL,
                          domain=list(),
                          records = NULL,
                          domain_forwarding=FALSE,
                          description="") {

      self$type = type
      # if (is.integer(type)) {
      #   # call from container
      #   symtype = GMS_DT_EQU
      #   symsubtype = type
      # }
      # else {
        # call from outside
        type = private$equationTypes[[type]]
        # if (!is.null(equationTypes[[type]])) {
        #   type = equationTypes$type
        # }
        symtype = GMS_DT_EQU
        symsubtype = EqTypeSubtype()[[type]]
      # }

      super$initialize(container, gams_name,
                      symtype, symsubtype, 
                      domain, description, domain_forwarding)
      if (!is.null(records)) {
        self$setRecords(records)
      }
    },

    setRecords = function(records) {
      # check if records is a dataframe and make if not
      records = data.frame(records)

      usr_colnames = colnames(records)
      columnNames = self$domainLabels()

      if (self$dimension +  1 > length(usr_colnames)) {
        usr_attr = NULL
      }
      else {
        usr_attr=  usr_colnames[(self$dimension + 1):length(usr_colnames)]
      }

      usr_attr=  usr_colnames[(self$dimension + 1):length(usr_colnames)]

      for (i in setdiff(private$.attr(), usr_attr)) {
        records[i] = private$default_values[[private$.type]][[i]]
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
        "User passed data.frame with columns: ", usr_colnames, "\n")))
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
    },

    summary = function() {
      return(list(
        "name" = self$name,
        "type" = self$type,
        "domain_objects" = self$domain,
        "domain_names" = self$domain_names(),
        "dimension" = self$dimension,
        "description" = self$description,
        "number_records" = self$number_records,
        "domain_type" = self$domain_type()
      ))
    }
  ),

  active = list(
    type = function(type_input) {
      if (missing(type_input)) {
        return(private$.type)
      }
      else {
        if (!any(private$equationTypes == type_input)) {
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
    }
  ),
  private = list(
    .type = NULL,
    equationTypes = list(
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

    default_values = list(
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

Alias <- R6Class(
  "Alias",
  public = list(
    .gams_type = NULL,
    .gams_subtype = NULL,
    .requiresStateCheck = NULL,
    initialize = function(container=NULL, gams_name=NULL, 
                          alias_for=NULL) {
      self$.requiresStateCheck = TRUE
      self$ref_container = container
      self$name = gams_name
      self$ref_container$data[[gams_name]] = self
      self$.gams_type = GMS_DT_ALIAS
      self$.gams_subtype = 1
      private$is_alias = TRUE
      self$aliasWith = alias_for
    },

    isAlias = function() {
      return(private$is_alias)
    },

    getCardinality = function() {
      return(self$ref_container$data[[self$aliasWith$name]]$getCardinality())
    },

    getSparsity = function() {
      return(self$ref_container$data[[self$aliasWith$name]]$getSparsity())
    },

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
    domain_names = function() {
      return(self$ref_container$data[[self$aliasWith$name]]$domain_names())
    },

    domain_type = function() {
      return(self$ref_container$data[[self$aliasWith$name]]$domain_type())
    },

    setRecords = function(records) {
      return(self$ref_container$data[[self$aliasWith$name]]$setRecords(records))
    },

    domainLabels = function() {
      return(self$ref_container$data[[self$aliasWith$name]]$domainLabels())
    },

    summary = function() {
    return(list(
      "name" = self$name,
      "alias_with" = self$aliasWith,
      "alias_with_name" = self$aliasWith$name,
      "is_singleton" = self$isSingleton(),
      "domain_objects" = self$domain,
      "domain_names" = self$domain_names(),
      "dimension" = self$dimension,
      "description" = self$description,
      "number_records" = self$number_records
    ))
    }

  ),

  active = list(
    ref_container = function(ref_container_input) {
      if (missing(ref_container_input)) {
        return(private$.ref_container)
      }
      else {
        if (!inherits(ref_container_input, "Container")) {
          stop("Symbol 'container' must be type Container\n")
        }
        if (is.null(self$ref_container)){
          if (!identical(self$ref_container, ref_container_input)) {
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

        if (!is.null(self$ref_container$data[[name_input]])) {
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
          private$.aliasWith = parent
        }
        private$.aliasWith = alias_with_input
      }
    },

    isSingleton = function(is_singleton) {
      if (missing(is_singleton)) {
        refcontainer = self$ref_container
        sym = refcontainer$data[[self$aliasWith$name]]
        return(sym$isSingleton)
      }
      else {
        refcontainer = self$ref_container
        sym = refcontainer$data[[self$aliasWith$name]]
        sym$isSingleton = is_singleton
      }
    },

    description = function(description_input) {
      if (missing(description_input)) {
        refcontainer = self$ref_container
        aliaswithname = self$aliasWith$name
        sym = refcontainer$data[[aliaswithname]]
        return(sym$description)
      }
      else {
        refcontainer = self$ref_container
        aliaswithname = self$aliasWith$name
        sym = refcontainer$data[[aliaswithname]]
        sym$description = description_input
      }
    },

    dimension = function(dimension_input) {
      if (missing(dimension_input)) {
        return(self$ref_container$data[[self$aliasWith$name]]$dimension)
      }
      else {
        refcontainer = self$ref_container
        sym = refcontainer$data[[self$aliasWith$name]]
        sym$dimension = dimension_input
      }
    },

    records = function(records_input) {
      return(self$ref_container$data[[self$aliasWith$name]]$records)
    },

    domain = function(domain_input) {
      if (missing(domain_input)) {
        return(self$ref_container$data[[self$aliasWith$name]]$domain)
      }
      else {
        refcontainer = self$ref_container
        sym = refcontainer$data[[self$aliasWith$name]]
        sym$domain = domain_input
      }
    },

    number_records = function() {
      return(self$ref_container$data[[self$aliasWith$name]]$number_records)
    }
  ),

  private = list(
    symbolMaxLength = 63,
    .ref_container = NULL,
    .name = NULL,
    .aliasWith = NULL,
    is_alias = NULL,
    is_singleton = NULL,

    # lblTypeSubtype = function() {
    #   return(list(
    #   "alias" = list(GMS_DT_ALIAS, 1)
    #   ))
    # },

    check = function() {
      if (self$.requiresStateCheck == TRUE) {
        if (self$ref_container$data[[self$aliasWith$name]]$isValid() == FALSE) {
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
  stop("Could not find a GAMS installation, must manually specify system_directory\n")
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
