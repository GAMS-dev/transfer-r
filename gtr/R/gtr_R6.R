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

Container <- R6::R6Class (
  "Container",
  public = list(
    systemDirectory = NULL,
    data = NULL,
    acronyms = NULL,
    initialize = function(load_from=NA, system_directory=NA) {

      if (missing(system_directory)) {
        self$systemDirectory = find_gams()

      }
      else {
        if (R.utils::isAbsolutePath(system_directory)) {
          self$systemDirectory = system_directory
        }
        else {
          stop("must enter valid full (absolute) path to GAMS system_directory")
        }
      }

      if (!missing(load_from)) {
        self$read(load_from, symbols="all")
      }

      self$acronyms = list()
      self$data = list()
      private$requiresStateCheck = TRUE
    },

    read = function(load_from, symbols="all", values=TRUE) {
      # read metadata
      # get all symbols and metadata from c++
      # process it and populate various fields

      # check if values is boolean
      if (!is.logical(values)) {
        stop("values must be type logical")
      }

      if (!(is.character(symbols)) && !(is.list(symbols))) {
        stop("argument symbols must be of the type list or string")
      }

      for (s in symbols) {
        if (!is.character(s)) {
          stop("argument symbols must contain only type string")
        }
      }

      if (!is.character(load_from)) {
        stop("The argument load_from must be of type string")
      }
      else {
        namesplit = strsplit(load_from, "\\.")
        ext = tail(unlist(namesplit), 1)
        if (ext != "gdx") {
          stop("check filename extension, must be .gdx")
        }
        load_from = R.utils::getAbsolutePath(load_from)
        if (!file.exists(load_from)) {
          stop(paste0("File ", load_from, " doesn't exist"))
        }
      }
      # check acronyms
      print("system directory")
      print(self$systemDirectory)
      acrInfo = checkAcronyms(load_from, self$systemDirectory)
      nAcr = acrInfo[["nAcronyms"]]
      if (nAcr != 0) {
        warning("GDX file contains acronyms. 
        Acronyms are not supported and are set to GAMS NA.")
        self$acronyms = acrInfo[["acronyms"]]
      }

      # get names for all symbols
      syms = getSymbolNames(load_from, self$systemDirectory)

      if (is.character(symbols) && symbols == "all") {
        symbolsToRead = syms
      }
      else {
        symbolsToRead = list()
        for (s in symbols) {
          if (s %in% syms) {
            symbolsToRead = append(symbolsToRead, s)
          }
        }
      }
      # check if load_from file exists
      if (!file.exists(load_from)) {
        stop(paste0("The file ", load_from, " doesn't exist."))
      }

      # check if container exists any of the symbols already
      for (s in symbolsToRead) {
        if (!is.null(self$data[[s]])) {
          stop(paste0("Attempting to add symbol ", s, ", however,",
          " one already exists in the Container. Symbol replacement",
          " is only possible if the symbol is first removed from the", 
          "Container with the removeSymbol() method."))
        }
      }
      print("get metadata")
      metadata = getSymbols(load_from, self$systemDirectory)
      print("metadata obtained")
      aliasList = list()
      aliasCount = 0
      for (m in metadata) {
         if (m$name %in% symbolsToRead) {
            m1 = m[-1]
            if (m$type == GMS_DT_PAR) {
              Parameter$new(
                self, m$name, m$domain,
                description = m$expltext)
            }
            else if (m$type == GMS_DT_SET) {
                if (m$subtype == 0) {
                Set$new(
                self, m$name, m$domain, FALSE,
                NA, m$expltext)
                }
                else {
                Set$new(
                self, m$name, m$domain, TRUE,
                NA, m$expltext)
                }
            }
            else if (m$type == GMS_DT_VAR) {
                Variable$new(
                self, m$name, m$type, m$domain,
                 description=m$expltext)
            }
            else if (m$type == GMS_DT_EQU) {
                Equation$new(
                self, m$name, m$subtype, m$domain,
                 description = m$expltext)
            }
            else if (m$type == GMS_DT_ALIAS) {
              aliasCount = aliasCount + 1
              aliasList = append(aliasList, list(m))
            }
            else {
                stop("incorrect data type.")
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

        print("symbol_read_done")

        for (s in symbolrecords) {
          self$data[[s$names]]$setRecords(s$records)

          if (!is.null(self$acronyms)) {
            if (inherits(self$data[[s$names]], "Parameter")
            | inherits(self$data[[s$names]], "Variable")
            | inherits(self$data[[s$names]], "Equation")) {
              for (d in 1:self$data[[s$names]]$dimension) {
                for (a in self$acronyms) {
                  col = self$data[[s$names]]$records[,d]
                  if (any(col == a * 1e301)) {
                    idx = which(col == a * 1e301)
                    for (id in idx) {
                      self$data[[s$names]]$records[id, d] = 
                      SpecialValues[["NA"]]
                    }
                  }
                }
              }

            }
          }
        }

        private$linkDomainObjects(symbolsToRead)

        # check validity
        validSymbols = self$listSymbols(isValid = TRUE)
      }
    },

    removeSymbols = function(name = NA) {
      if (!(is.character(name) || is.vector(name) || is.list(name))) {
        stop("Argument 'name' must be of type string, list, or vector")
      }

      if (!all(unlist(lapply(name, is.character)))) {
        stop("Argument 'name' must contain only type character")
      }

      for (n in names) {
        self$data[[n]] <- NULL
      }

      private$checkOn()
    },

    renameSymbols = function(old_name = NA, new_name = NA) {
      if (is.character(old_name)) {
        stop("Argument 'old_name' must be type character")
      }

      if (is.character(new_name)) {
        stop("Argument 'new_name' must be type character")
      }

      if (is.null(self$data[[old_name]])) {
        stop(paste0("Symbol ", old_name, " does not exist"))
      }

      if (old_name != new_name) {
        self$data[[old_name]]$name = new_name
        self$checkOn()
      }
    },

    listSymbols = function(isValid = NULL) {
      if (!is.null(isValid)) {
        assertthat::assert_that(is.logical(isValid),
        msg = "argument 'isValid' must be type logical")
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

    addSet = function(name, domain = NA, is_singleton = FALSE,
    records = NA, description = "") {
      Set$new(
      self, name, domain, is_singleton,
      records, description)
      return(self$data[[name]])
    },

    addParameter = function(name, domain = NA,
    records = NA, description = "") {
      Parameter$new(
        self, name, domain, records,
        description)
        return(self$data[[name]])
    },

    addVariable = function(name, type="free", domain = NA,
    records = NA, description = "") {
      Variable$new(
        self, name, type, domain,
        description)
        return(self$data[[name]])
    },

    addEquation = function(name, domain = NA, type,
    records = NA, description = "") {
      Equation$new(
        self, name, type, domain,
        description)
        return(self$data[[name]])
    },

    addAlias = function(name, alias_with) {
      Alias$new(
      self, name, alias_with)
      return(self$data[[name]])
    },

    describeSets = function(symbols=NA) {
      if (is.na(symbols)) {
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
        if (i %in% self$listSets()) {
          symDescription = list(
            i,
            self$data[[i]]$isAlias(),
            self$data[[i]]$isSingleton,
            paste(self$data[[i]]$domain_names(), sep = "", collapse = " "),
            self$data[[i]]$domain_type(),
            self$data[[i]]$dimension,
            self$data[[i]]$number_records(),
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
        return(NA)
      }
    },

    describeParameters = function(symbols = NA) {
      if (is.na(symbols)) {
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
        if (i %in% self$listParameters()) {
          symDescription = list(
            i,
            self$data[[i]]$isScalar,
            paste(self$data[[i]]$domain_names(), sep = "", collapse = " "),
            self$data[[i]]$domain_type(),
            self$data[[i]]$dimension,
            self$data[[i]]$number_records(),
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
        return(NA)
      }
    },

    describeVariables = function(symbols=NA) {
      if (is.na(symbols)) {
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
        if (i %in% self$listVariables()) {
          symDescription = list(
            i,
            self$data[[i]]$type,
            paste(self$data[[i]]$domain_names(), sep = "", collapse = " "),
            self$data[[i]]$domain_type(),
            self$data[[i]]$dimension,
            self$data[[i]]$number_records(),
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
        return(NA)
      }
    },

    describeEquations = function(symbols=NA) {
      if (is.na(symbols)) {
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
        if (i %in% self$listEquations()) {
          symDescription = list(
            i,
            self$data[[i]]$type,
            paste(self$data[[i]]$domain_names(), sep = "", collapse = " "),
            self$data[[i]]$domain_type(),
            self$data[[i]]$dimension,
            self$data[[i]]$number_records(),
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
        return(NA)
      }
    },

    printSpecialValues = function() {
      print(private$gdx_specVals_write)
    },

    write = function(gdxout) {
      if (!is.character(gdxout)) {
        stop("The argument gdxout must be of type string")
      }
      else {
        namesplit = strsplit(gdxout, "\\.")
        ext = tail(unlist(namesplit), 1)
        if (ext != "gdx") {
          stop("check filename extension, must be .gdx")
        }
        gdxout = R.utils::getAbsolutePath(gdxout)
        # if (!file.exists(gdxout)) {
        #   stop(paste0("File ", gdxout, " doesn't exist"))
        # }
      }

      if (!identical(self$listSymbols(), self$listSymbols(isValid=TRUE) )) {
        stop(paste0("There are symbol(s) in Container that are not valid;",
         "all symbols must be valid before writing",
         " (i.e., <symbol object>$isValid() == TRUE)"))
      }

      private$validSymbolOrder()

      # remap special values
      specialValsGDX = getSpecialValues(gdxout, self$systemDirectory)
      # for (v in names(SpecialValues)) {
        for (s in self$data) {
          # no mapping required for alias
          if (s$type == GMS_DT_ALIAS || s$type == GMS_DT_SET) next
          colrange = (s$dimension + 1):length(s$records)
          s$records[,colrange][is.nan(
            s$records[,colrange])] = 
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

        #   if (length(s$records) != 1) {
        #     for (c in length(s$records)) {
        #       if (v == "EPS") {
        #         idx = which((s$records[c] == SpecialValues[v]) && 
        #         (sign(1/s$records[c]==-1) ))
        #         s$records[idx, c] = specialValsGDX[v]
        #       }
        #       else if (v == "NA") {
        #         idx = which(( is.na(s$records[c])) && 
        #         (sign(1/s$records[c]==-1) ))
        #         s$records[idx, c] = specialValsGDX[v]
        #       }
        #       else if (v == "UNDEF") {
        #         idx = which(( is.na(s$records[c])) && 
        #         (sign(1/s$records[c]==-1) ))
        #         s$records[idx, c] = specialValsGDX[v]
        #       }
        #       else {
        #         idx = which(s$records[c] == SpecialValues[v])
        #         s$records[idx, c] = specialValsGDX[v]
        #       }
        #     }
        #   }
        #   else {
        #     if (v == "EPS"){
        #       idx = which((s$records == SpecialValues[v]) && 
        #       (sign(1/s$records==-1) ))
        #       s$records[idx] = specialValsGDX[v]
        #     }
        #     else {
        #       idx = which(s$records == SpecialValues[v])
        #       s$records[idx] = SpecialValues[v]
        #     }
        #   }

        }
      # }
      gdxWriteSuper(self$data, self$systemDirectory, gdxout)

    },

    isValid = function(verbose=FALSE, force=FALSE) {
      assertthat::assert_that(is.logical(verbose), 
      msg = "Argument 'verbose' must be logical")
      
      assertthat::assert_that(is.logical(force), 
      msg = "Argument 'force' must be logical")

      if (force == TRUE) {
        private$requiresStateCheck = TRUE
      }

      if (private$requiresStateCheck == TRUE) {
        private$check()
      }
    }

  ),
  private = list(
    requiresStateCheck = NULL,
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
            else if (is.character(j) && (j %in% symbols) && (j != s)) {
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
      if (private$requiresStateCheck == TRUE) {
         private$validSymbolOrder()

        if (all(self$listSymbols() != self$listSymbols(isValid = TRUE))) {
          stop(paste0("Container contains invalid symbols; ",
          "invalid symbols can be found with the .listSymbols() ",
          "method. Debug invalid symbol(s) by running .",
          "isValid(verbose=TRUE, force=TRUE) method on the symbol object."))
        }
        private$checkOff()
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
            else if ( (inherits(i, "Set") | inherits(i, "Alias")) &
            i$name %in% orderedSymbols) {
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
          symString, " -- must resolve circular domain referencing"))
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
    },

    checkOn = function() {
      if (private$requiresStateCheck==FALSE) {
        private$requiresStateCheck = TRUE
      }
    },

    checkOff = function() {
      if (private$requiresStateCheck==TRUE) {
        private$requiresStateCheck = FALSE
      }
    }
  )
  )


Symbol <- R6Class(
  "Symbol",
  public = list(
  type = NULL,
  subtype = NULL,
  records = NULL,

  initialize = function(container=NA, name=NA,
                        type=NA, subtype=NA, 
                        domain=NA,
                        description=NA) {


    private$requiresStateCheck = TRUE
    self$ref_container = container

    # if (!is.null(self$ref_container$data[[name]])) {
    #   stop(paste0("Attempting to add symbol ", s, ", however,",
    #   " one already exists in the Container. Symbol replacement",
    #   " is only possible if the symbol is first removed from the", 
    #   "Container with the removeSymbol() method."))
    # }
    # else {
    #   self$ref_container$data[[name]] = self
    # }
    self$name <- name
    self$ref_container$data[[name]] = self
    self$type = type
    self$subtype = subtype

    self$records = NA

    self$domain = domain

    self$dimension = length(self$domain)
    self$description = description

  },

  summary = function() {
    list(
      "name" = self$name,
      "type" = self$type,
      "subtype" = self$subtype,
      "domain" = self$domain,
      "dimension" = self$dimension,
      "records" = NA,
      "number_records" = self$number_records()
    )
  },

  number_records = function() {
    if (any(!is.na(self$records))) {
      return(nrow(self$records))
    }
    else {
      return(0)
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
            card = card * i$number_records()
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
          return(1 - self$number_records()/self$getCardinality())
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

  getMaxValue = function(columns=NA) {
    if (!is.na(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)"))
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column '", columns, "' must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension+1):length(self$records)]
        ))
      }
    }

    tryCatch(
      {
        if (inherits(self, "Parameter")) {
          if (is.na(columns)) {
            columns = "value"
          }
          return(max(self$records[[columns]]))
        }
        else if (inherits(self, "Variable") | inherits(self, "Equation")) {
          if (is.na(columns)) {
            columns = "level"
          }

          return(max(self$records[, columns]))
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

  getMinValue = function(columns=NA) {
    if (!is.na(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input '", columns, 
        "', however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)"))
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension+1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column '", columns, "' must be a subset",
        " of valid numeric columns", 
        colnames(self$records[,(self$dimension+1):length(self$records)])
        ))
      }
    }
    tryCatch(
      {
        if (inherits(self, "Parameter")) {
          if (is.na(columns)) {
            columns = "value"
          }
          return(min(self$records[[columns]]))
        }
        else if (inherits(self, "Variable") | inherits(self, "Equation")) {
          if (is.na(columns)) {
            columns = "level"
          }

          return(min(self$records[, columns]))

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

  getMeanValue = function(columns=NA) {
    if (!is.na(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)"))
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension+1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension+1):length(self$records)]
        ))
      }
    }

    tryCatch(
      {
        if (inherits(self, "Parameter")) {
          if (is.na(columns)) {
            columns = "value"
          }
          meanVal = mean(self$records[[columns]])
          if (is.nan(meanVal)) {
            return(NA)
          }
          else {
              return(meanVal)
          }

        }
        else if (inherits(self, "Variable") | inherits(self, "Equation")) {
          if (is.na(columns)) {
            columns = "level"
          }

          meanVal = mean(colMeans(self$records[columns]))
          if (is.nan(meanVal)) {
            return(NA)
          }
          else {
              return(meanVal)
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
  },
  getMaxAbsValue = function(columns=NA) {
    if (!is.na(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)"))
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension+1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records[,(self$dimension+1):length(self$records)])
        ))
      }
    }
    tryCatch(
      {
        if (inherits(self, "Parameter")) {
          if (is.na(columns)) {
            columns = "value"
          }
          return(max(abs(self$records[[columns]])))
        }
        else if (inherits(self, "Variable") | inherits(self, "Equation")) {
          if (is.na(columns)) {
            columns = "level"
          }

          return(max(abs(self$records[, columns])))

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
  whereMax = function(columns=NA) {
    if (!is.na(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)"))
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension+1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension+1):length(self$records)]
        ))
      }
    }
    tryCatch(
      {
        if (inherits(self, "Parameter")) {
          if (is.na(columns)) {
            columns = "value"
          }
          whereMaxVal = which.max(self$records[[columns]])
          if (is.integer0(whereMaxVal)) {
            return(NA)
          }
          else {
            return(whereMaxVal)
          }
        }
        else if (inherits(self, "Variable") | inherits(self, "Equation")) {
          if (is.na(columns)) {
            columns = "level"
          }

          whereMaxVal = which.max(self$records[, columns])
          if (is.integer0(whereMaxVal)) {
            return(NA)
          }
          else {
            return(whereMaxVal)
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
  },

  whereMaxAbs = function(columns=NA) {
    if (!is.na(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)"))
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension + 1):length(self$records)]
        ))
      }
    }

    tryCatch(
      {
        if (inherits(self, "Parameter")) {
          if (is.na(columns)) {
            columns = "value"
          }

          whereMaxVal = which.max(abs(self$records[[columns]]))
          if (is.integer0(whereMaxVal)) {
            return(NA)
          }
          else {
            return(whereMaxVal)
          }
        }
        else if (inherits(self, "Variable") | inherits(self, "Equation")) {
          if (is.na(columns)) {
            columns = "level"
          }

        whereMaxVal = which.max(abs(self$records[, columns]))
        if (is.integer0(whereMaxVal)) {
          return(NA)
        }
        else {
          return(whereMaxVal)
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
  },
  whereMin = function(columns=NA) {
    if (!is.na(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)"))
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension + 1):length(self$records)]
        ))
      }
    }
    tryCatch(
      {
        if (inherits(self, "Parameter")) {
          if (is.na(columns)) {
            columns = "value"
          }
          whereMinVal = which.min(self$records[[columns]])
          if (is.integer0(whereMinVal)) {
            return(NA)
          }
          else {
            return(whereMinVal)
          }
        }
        else if (inherits(self, "Variable") | inherits(self, "Equation")) {
          if (is.na(columns)) {
            columns = "level"
          }

        whereMinVal = which.min(self$records[, columns])
        if (is.integer0(whereMinVal)) {
          return(NA)
        }
        else {
          return(whereMinVal)
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

  },

  countNA = function(columns=NA) {
    if (!is.na(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)"))
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension+1):length(self$records)]
        ))
      }
    }

    tryCatch(
      {
        if (inherits(self, "Parameter")) {
          if (is.na(columns)) {
            columns = "value"
          }
          return(sum(is.na(self$records[[columns]])))
        }
        else if (inherits(self, "Variable") | inherits(self, "Equation")) {
          if (is.na(columns)) {
            columns = "level"
          }


          return(sum(is.na(self$records[,columns])))
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

  countEps = function(columns=NA) {
    if (!is.na(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)"))
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records[(self$dimension + 1):length(self$records)]
        )))
      }
    }
    tryCatch(
      {
        if (inherits(self, "Parameter")) {
          if (is.na(columns)) {
            columns = "value"
          }
          return(sum(self$records[[columns]] == SpecialValues$EPS))
        }
        else if (inherits(self, "Variable") | inherits(self, "Equation")) {
          if (is.na(columns)) {
            columns = "level"
          }

          return(sum(self$records[,columns] == SpecialValues$EPS))
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

  countUndef = function(columns=NA) {
    if (!is.na(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)"))
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension + 1):length(self$records)]
        ))
      }
    }
    tryCatch(
      {
        if (inherits(self, "Parameter")) {
          if (is.na(columns)) {
            columns = "value"
          }
          return(sum(is.nan(self$records[[columns]])))
        }
        else if (inherits(self, "Variable") | inherits(self, "Equation")) {
          if (is.na(columns)) {
            columns = "level"
          }

          return(sum(is.nan(self$records[,columns])))
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

  countPosinf = function(columns=NA) {
    if (!is.na(columns)) {
      if (!is.character(columns)) {
        stop(paste0("User input ", columns, ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)"))
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension + 1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension+1):length(self$records)]
        ))
      }
    }
    tryCatch(
      {
        if (inherits(self, "Parameter")) {
          if (is.na(columns)) {
            columns = "value"
          }
          return(sum(self$records[[columns]] == SpecialValues$POSINF))
        }
        else if (inherits(self, "Variable") | inherits(self, "Equation")) {
          if (is.na(columns)) {
            columns = "level"
          }

          return(sum(self$records[,columns] == SpecialValues$POSINF))
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

  countNeginf = function(columns=NA) {
    if (!is.na(columns)) {
      if (!is.character(columns)){
        stop(paste0("User input ", columns, ", however it is only possible to",
        " select one column at a time (i.e. argument 'column' must be type",
        " character)"))
      }

      if (!setequal(intersect(columns, 
      colnames(self$records)[(self$dimension+1):length(self$records)]), 
      columns)) {
        stop(paste0("User entered column ", columns, " must be a subset",
        " of valid numeric columns", 
        colnames(self$records)[(self$dimension+1):length(self$records)]
        ))
      }
    }
    tryCatch(
      {
        if (inherits(self, "Parameter")) {
          if (is.na(columns)){
            columns = "value"
          }
          return(sum(self$records[[columns]] == SpecialValues$NEGINF))
        }
        else if (inherits(self, "Variable") | inherits(self, "Equation")) {
          if (is.na(columns)) {
            columns = "level"
          }

          return(sum(self$records[,columns] == SpecialValues$NEGINF))
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
      private$requiresStateCheck = TRUE
    }

    if (private$requiresStateCheck == TRUE) {
      tryCatch(
        {
          private$check()
          return(TRUE)
        },
        error = function(e) {
          message(e)
          return(FALSE)
        }
      )
    }

  },

  domainForwarding = function() {
    # find symbols to grow
    for (diter in seq_len(self$dimension)) {
      d = self$domain[diter]
      dl = self$domainLabels()[diter]
      to_grow = NULL
      while (inherits(d, "Set")) {
        if (is.null(to_grow)) {
          to_grow = d
        }
        else {
          to_grow = append(to_grow, d)
        }
        d = d$domain
      }

      for (i in to_grow) {
        dim = (i$domainLabels)[1]
        if (is.null(self$ref_container$data[[i]]$records)) {
          recs = self$ref_container$data[[i]]$records
          assert_that((self$ref_container$data[[i]]$dimension == 1),
          msg = "attempting to forward a domain set that has dimension > 1")

          df = self$records["dl"]
          colnames(df) = dim
          df[["element_text"]] = ""
          # recs = 
        }
        else {
          recs = unique(self$records[[dl]])
          colnames(recs) = dim
          recs[["element_text"]] = ""
        }
        self$ref_container$data[[i]]$records = recs
      }
    }
  }
  ),
  active = list(
    description = function(description_input) {
      if (missing(description_input)){
        return(private$.description)
      }
      else {
        if (!is.character(description_input)) {
          stop("Symbol 'description' must be type character")
        }

        if (length(description_input) >= gams_description_max_length) {
          stop(paste0("Symbol 'description' must have length ",
          gams_description_max_length, " or smaller"))
        }

        if (!is.na(private$.description)) {
          if (private$.description != description_input){
            private$checkOn()
          }
          else {
            private$.description = description_input
          }
        }
        else {
          private$checkOn()
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
           (is.integer(dimension_input)) && (dimension_input >= 0),
           msg = "Symbol 'dimension' must be type int (greater than or equal to 0)")

        if (length(self$domain) > dimension_input) {
          self$domain = self$domain[1:dimension_input]
        }
        else if (length(self$domain) < dimension_input) {
           new = self$domain
           new = append(new, replicate(dimension_input - length(self$domain), "*"))
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
                stop("domain elements cannot belong to a different container")
              }
            }
            else if (inherits(d, "Alias")) {
              if (identical(d$aliasWith$ref_container, self$ref_container)) {
                domaintemp = append(domaintemp, d)
              }
              else {
                stop("domain elements cannot belong to a different container")
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
          stop("Symbol 'container' must be type Container")
        }
        if (is.null(self$ref_container)){
          if (!identical(self$ref_container, ref_container_input)) {
            private$checkOn()
          }
          private$.ref_container = ref_container_input
        }
        else {
          private$checkOn()
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
          stop("GAMS symbol 'name' must be type chracter")
        }

        if (nchar(name_input) > private$symbolMaxLength) {
          stop(paste0("GAMS symbol 'name' is too long,",
          " max is ", private$symbolMaxLength, " characters"))
        }

        if (!is.null(self$ref_container$data[[name_input]])) {
          stop(paste0("A symbol with the name ", name_input, 
          " already exists in the container"))
        }

        if (is.na(private$.name)) {
          private$checkOn()
          private$.name = name_input
        }
        else {
          if(private$.name != name_input) {
            private$checkOn()
          }
          private$.name = name_input
        }
      }
    }
  ),

  private = list(
    .description = NA,
    .domain = NA,
    .ref_container = NA,
    .name = NA,
    requiresStateCheck = NA,
    symbolMaxLength = 63,
    descriptionMaxLength = 255,

    lblTypeSubtype = function() {
      return(list(
      "set" = list(GMS_DT_SET, 0),
      "parameter" = list(GMS_DT_PAR, 0),
      "singleton_set" = list(GMS_DT_SET, 1),
      "binary" = list(GMS_DT_VAR, GMS_VARTYPE_BINARY),
      "integer" = list(GMS_DT_VAR, GMS_VARTYPE_INTEGER),
      "positive" = list(GMS_DT_VAR, GMS_VARTYPE_POSITIVE),
      "negative" = list(GMS_DT_VAR, GMS_VARTYPE_NEGATIVE),
      "free" = list(GMS_DT_VAR, GMS_VARTYPE_FREE),
      "sos1" = list(GMS_DT_VAR, GMS_VARTYPE_SOS1),
      "sos2" = list(GMS_DT_VAR, GMS_VARTYPE_SOS2),
      "semicont" = list(GMS_DT_VAR, GMS_VARTYPE_SEMICONT),
      "semiint" = list(GMS_DT_VAR, GMS_VARTYPE_SEMIINT),
      "eq" = list(GMS_DT_EQU, GMS_EQUTYPE_E + GMS_EQU_USERINFO_BASE),
      "geq" = list(GMS_DT_EQU, GMS_EQUTYPE_G + GMS_EQU_USERINFO_BASE),
      "leq" = list(GMS_DT_EQU, GMS_EQUTYPE_L + GMS_EQU_USERINFO_BASE),
      "nonbinding" = list(GMS_DT_EQU, GMS_EQUTYPE_N + GMS_EQU_USERINFO_BASE),
      "external" = list(GMS_DT_EQU, GMS_EQUTYPE_X + GMS_EQU_USERINFO_BASE),
      "cone" = list(GMS_DT_EQU, GMS_EQUTYPE_C + GMS_EQU_USERINFO_BASE),
      "boolean" = list(GMS_DT_EQU, GMS_EQUTYPE_B + GMS_EQU_USERINFO_BASE),
      "alias" = list(GMS_DT_ALIAS, 1)
      ))

    },

    attr = function() {
      return(list("level", "marginal", "lower", "upper", "scale"))
    },

    check = function() {
      if (private$requiresStateCheck == TRUE) {
        # if regular domain, symbols in domain must be valid
        if (self$domain_type() == "regular") {
          for (i in self$domain) {
            if (!( ( (inherits(i, "Set") || inherits(i, "Alias")) && 
            (i$name %in% names(self$ref_container$data)) ) ||
            ( (is.character(i)) && 
            (i %in% names(self$ref_container$data)) )
             )) {
              stop(paste0("symbol defined over domain symbol ",
              i$name, " however, the object reference is not in the", 
              " Container anymore -- must reset domain for symbol ", 
              self$name))
            }
          }

          for (i in self$domain) {
            if (i$isValid() != TRUE) {
              stop(paste0("symbol defined over domain symbol ",
              i$name, " however, this object is not a valid object ",
              "in the Container -- all domain objects must be valid."))
            }
          }
        }

        # if records exist, check consistency
        if (!is.null(self$records)) {
          if (inherits(self, "Set")){
            if (length(self$records) != self$dimension + 1) {
              stop(paste0("Symbol 'records' does not have", 
              " the correct number of columns (<symbol dimension> + 1)"))
            }
          }
          if (inherits(self, "Parameter")) {
            if (length(self$records) != self$dimension + 1) {
              stop(paste0("Symbol 'records' does not have", 
              " the correct number of columns (<symbol dimension> + 1)"))

              if (self$dimension == 0 && nrow(self$records != 1)) {
              stop(paste0("Symbol 'records' does not have", 
              " the correct number of columns (<symbol dimension> + 1)"))
              }
            }
          }
          if (inherits(self, "Variable") | inherits(self, "Equation")){
            if (length(self$records) != 
            self$dimension + length(private$attr())) {
              stop(paste0("Symbol 'records' does not have", 
              " the correct number of columns ", 
              self$dimension + length(private$attr())))
            }
          }
          # check if records are dataframe
          if (!is.data.frame(self$records)){
            stop("Symbol 'records' must be type dataframe")
          }

          # check column names and order
          cols = self$domainLabels()
          if (inherits(self, "Set")) {
            cols = append(cols, "element_text")
          }
          else if(inherits(self, "Parameter")) {
            cols = append(cols, "value")
          }
          else if (inherits(self, "Vairable") ||
          inherits(self, "Equation")) {
            cols = append(cols, private$attr())
          }
          # cols = self$getColLabelsForRecords()
          if (!all(unlist(lapply(cols, is.character) ))) {
            stop("Domain columns in symbol 'records' must be of type character")
          }

          # check for domain violations

          if (self$dimension != 0) {
            nullrecords = self$records[,1:self$dimension][is.null(self$records[,1:self$dimension])]
            narecords = self$records[,1:self$dimension][is.na(self$records[,1:self$dimension])]

            if (length(nullrecords) != 0 || 
            length(narecords) != 0 ) {
              stop(paste0("Symbol 'records' contain domain violations;",
              " ensure that all domain elements have",
              " been mapped properly to a category"))
            }
          }

          # drop duplicates
          if (self$dimension != 0) {
            if (nrow(self$records) != nrow(unique(self$records))) {
              print(paste0("Symbol 'records' contain non-unique",
               " domain members; ensure that only unique members exist"))
            }
          }

          # check if all data columns are float
          if (inherits(self, "Variable") | 
          inherits(self, "Parameter") | 
          inherits(self, "Equation")) {
            for (i in (self$dimension + 1):length(self$records)) {
              if (!all(is.numeric(self$records[, i]))) {
                stop("Data in column", i, " must be numeric")
              }
            }
          }
        }

      }
      private$checkOff()
    },
    checkOn = function() {
      if (private$requiresStateCheck == FALSE) {
        private$requiresStateCheck == TRUE
      }
    },
    checkOff = function() {
      if (private$requiresStateCheck == TRUE) {
        private$requiresStateCheck == FALSE
      }
    }
  )
)

Set <- R6Class(
  "Set",
  inherit = Symbol,
  public = list(

    records = NA,
    initialize = function(container=NA, gams_name=NA,
                          domain="*", is_singleton=FALSE,
                          records = NA, description=NA) {
      self$isSingleton <- is_singleton
      if (!is_singleton) {
        type = super$lblTypeSubtype()[["set"]][[1]]
        subtype = super$lblTypeSubtype()[["set"]][[2]]
      }
      else {
        type = super$lblTypeSubtype()[["singleton_set"]][[1]]
        subtype = super$lblTypeSubtype()[["singleton_set"]][[2]]
      }

      super$initialize(container, gams_name,
                      type, subtype,
                      domain, description)

      if (any(!is.na(records))) {
        self$setRecords(records)
      }
      private$is_alias = FALSE
      invisible(self)
    },
    setRecords = function(records) {
      # # if records is a list, unlist
      # if (is.list(records)) {
      #   print("records not list")
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
        c, " Expecting ", self$dimension + 1))
      }
      self$records = records
      columnNames = self$domainLabels()
      columnNames = append(columnNames, "element_text")
      # columnNames = self$getColLabelsForRecords()
      colnames(self$records) = columnNames
    },
    isAlias = function() {
      return(private$is_alias)
    }


  ),
  active = list(
    isSingleton = function(is_singleton) {
      if (missing(is_singleton)) {
        return(private$is_singleton)
      }
      else {
        if (!is.logical(is_singleton)) {
          stop("Argument 'is_singleton' must be type bool")
        }
        if (missing(is_singleton)) {
          return(private$is_singleton)
        }
        else {
          private$is_singleton = is_singleton
        }
      }

      }
  ),
  private = list(
    is_singleton = NA,
    is_alias = NA
  )
  )

Parameter <- R6Class(
  "Parameter",
  inherit = Symbol,
  public = list(
    isScalar = NULL,

    initialize = function(container=NA, gams_name=NA,
                          domain=NA,records=NA,
                          description=NA ) {

      type = super$lblTypeSubtype()[["parameter"]][[1]]
      super$initialize(container, gams_name,
                      type, NA, 
                      domain, description)
      if (any(!is.na(records))) {
        self$setRecords(records)
      }
      if (self$dimension == 0) {
        self$isScalar = TRUE
      }
      else {
         self$isScalar = FALSE
      }
    },

    setRecords = function(records) {
      # check if records is a dataframe and make if not
      # print("sign in param")
      # print(sign(1/records[["values"]]))
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

      self$records = records
      columnNames = self$domainLabels()
      columnNames = append(columnNames, "value")
      # columnNames = self$getColLabelsForRecords()
      colnames(self$records) = columnNames

      #if records "value" is not numeric, stop.
      if (any(!is.numeric(self$records[,length(self$records)]))) {
        stop("All entries in the 'values' column of a parameter must be numeric.")
      }
      # convert values column to numeric
      # self$records$value = as.numeric(as.character(self$records$value))
    }

  )
)

Variable <- R6Class(
  "Variable",
  inherit = Symbol,
  public = list(
    type = NULL,
    initialize = function(container=NA, gams_name=NA, 
                          type="free",
                          domain=NA, records=NA,
                            description=NA) {
      if (is.integer(type)){
        symtype = type
      }
      else {
         symtype = super$lblTypeSubtype()[[type]][[1]]
      }
      self$type = type

      super$initialize(container, gams_name,
                      symtype, NA, 
                      domain, description)
      if (any(!is.na(records))) {
        self$setRecords(records)
      }
    },
    setRecords = function(records) {
      # check if records is a dataframe and make if not
      records = data.frame(records)
      self$records = records
      columnNames = self$domainLabels()
      columnNames = append(columnNames, private$attr())
      # columnNames = self$getColLabelsForRecords()
      colnames(self$records) = columnNames
    }
  )
  )

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
)

Equation <- R6Class(
  "Equation",
  inherit = Symbol,
  public = list(

    initialize = function(container=NA, gams_name=NA, 
                          type=NA,
                          domain=NA,
                          records = NA,
                          description=NA) {
      if (is.integer(type)) {
        # call from container
        symtype = GMS_DT_EQU
        symsubtype = type
      }
      else {
        # call from outside 
        if (!is.null(equationTypes[[type]])) {
          type = equationTypes$type
        }
        else {
          stop("error. Wrong equation type")
        }
        symtype = super$lblTypeSubtype()[[type]][[1]]
        symsubtype = super$lblTypeSubtype()[[type]][[2]]
      }

      super$initialize(container, gams_name,
                      symtype, symsubtype, 
                      domain, description)
      if (any(!is.na(records))) {
        self$setRecords(records)
      }
    },
    setRecords = function(records) {
      # check if records is a dataframe and make if not
      records = data.frame(records)
      self$records = records
      columnNames = self$domainLabels()
      columnNames = append(columnNames, private$attr())
      # columnNames = self$getColLabelsForRecords()
      colnames(self$records) = columnNames
    }    
  ),

  private = list(
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
    type = NULL,
    initialize = function(container=NA, gams_name=NA, 
                          alias_for=NA) {
      private$requiresStateCheck = TRUE
      self$ref_container = container
      self$name = gams_name
      self$ref_container$data[[gams_name]] = self
      self$type = private$lblTypeSubtype()[["alias"]][[1]]
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
        private$requiresStateCheck = TRUE
      }

      if (private$requiresStateCheck == TRUE) {
        tryCatch(
          {
            private$check()
            return(TRUE)
          },
          error = function(e) {
            message(e)
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

    number_records = function() {
      return(self$ref_container$data[[self$aliasWith$name]]$number_records())
    },

    domainLabels = function() {
      return(self$ref_container$data[[self$aliasWith$name]]$domainLabels())
    },

    summary = function() {
    list(
      "name" = self$name,
      "alias_with" = self$aliasWith,
      "alias_with_name" = self$aliasWith$name,
      "is_singleton" = self$isSingleton(),
      "is_alias" = self$isAlias,
      "domain_objects" = self$domain,
      "domain_names" = self$domain_names(),
      "dimension" = self$dimension,
      "description" = self$description,
      "number_records" = self$number_records
    )
    }

  ),

  active = list(
    ref_container = function(ref_container_input) {
      if (missing(ref_container_input)) {
        return(private$.ref_container)
      }
      else {
        if (!inherits(ref_container_input, "Container")) {
          stop("Symbol 'container' must be type Container")
        }
        if (is.null(self$ref_container)){
          if (!identical(self$ref_container, ref_container_input)) {
            private$checkOn()
          }
          private$.ref_container = ref_container_input
        }
        else {
          private$checkOn()
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
          stop("GAMS symbol 'name' must be type chracter")
        }

        if (nchar(name_input) > private$symbolMaxLength) {
          stop(paste0("GAMS symbol 'name' is too long,",
          " max is ", private$symbolMaxLength, " characters"))
        }

        if (!is.null(self$ref_container$data[[name_input]])) {
          stop(paste0("A symbol with the name ", name_input, 
          " already exists in the container"))
        }

        if (is.na(private$.name)) {
          private$checkOn()
          private$.name = name_input
        }
        else {
          if(private$.name != name_input) {
            private$checkOn()
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
          stop("GAMS 'alias_with' must be type Set or Alias")
        }

        if (inherits(alias_with_input, "Alias")) {
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
        return(private$is_singleton)
      }
      else {
        self$ref_container$data[[self$aliasWith$name]]$isSingleton = is_singleton
      }
    },
    description = function(description_input) {
      if (missing(description_input)) {
        return(self$ref_container$data[[self$aliasWith$name]]$description)
      }
      else {
        self$ref_container$data[[self$aliasWith$name]]$description = description_input
      }
    },
    dimension = function(dimension_input) {
      if (missing(dimension_input)) {
        return(self$ref_container$data[[self$aliasWith$name]]$dimension)
      }
      else {
        self$ref_container$data[[self$aliasWith$name]]$dimension = dimension_input
      }
    },
    records = function(records_input) {
      if (missing(records_input)) {
        return(self$ref_container$data[[self$aliasWith$name]]$records)
      }
      else {
        self$ref_container$data[[self$aliasWith$name]]$records = records_input
      }
    },

    domain = function(domain_input) {
      if (missing(domain_input)) {
        return(self$ref_container$data[[self$aliasWith$name]]$domain)
      }
      else {
        self$ref_container$data[[self$aliasWith$name]]$domain = domain_input
      }
    }
  ),

  private = list(
    symbolMaxLength = 63,
    .ref_container = NA,
    .name = NA,
    .aliasWith = NA,
    is_alias = NA,
    requiresStateCheck = NA,
    is_singleton = NA,

    lblTypeSubtype = function() {
      return(list(
      "alias" = list(GMS_DT_ALIAS, 1)
      ))
    },

    check = function() {
      if (private$requiresStateCheck == TRUE) {
        if (self$ref_container$data[[self$aliasWith$name]]$isValid() == FALSE) {
          stop(paste0("Alias symbol is not valid because parent set ", self$aliasWith$name,
          "is not valid"))
        }
      }
    },

    checkOn = function() {
      if (private$requiresStateCheck==FALSE) {
        private$requiresStateCheck = TRUE
      }
    },

    checkOff = function() {
      if (private$requiresStateCheck==TRUE) {
        private$requiresStateCheck = FALSE
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
  stop("Could not find a GAMS installation, must manually specify system_directory")
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
