library(R6)
library(assertthat)

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



SpecialValues = list(
  "NA" = NA,
  "EPS" = -0.0,
  "UNDEF" = NaN,
  "POSINF" = Inf,
  "NEGINF" = -Inf
  )

Container <- R6::R6Class (
  "Container",
  public= list(
    SysDir = NULL,
    data = NULL,
    acronyms = NULL,
    initialize = function(load_from=NA, system_directory=NA) {
      if (missing(system_directory)) {
        self$SysDir = find_gams()
      }
      else {
        self$SysDir = system_directory
      }

      self$data = list()
      if (!missing(load_from)) {
        self$read(load_from, symbols="all")
      }
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
      acrInfo = checkAcronyms(load_from, self$SysDir)
      nAcr = acrInfo[["nAcronyms"]]
      if (nAcr != 0) {
        warning("GDX file contains acronyms. 
        Acronyms are not supported and are set to GAMS NA.")
        self$acronyms = acrInfo[["acronyms"]]
      }

      # get names for all symbols
      syms = getSymbolNames(load_from, self$SysDir)

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
      metadata = getSymbols(load_from, self$SysDir)
      aliasList = list()
      aliasCount = 0
      for (m in metadata) {
         if (m$name %in% symbolsToRead) {
            m1 = m[-1]
            if (m$type == GMS_DT_PAR) {
              Parameter$new(
                self, m$name, m$domain,
                description=m$expltext)
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
              print(paste0("domain: ", m$domain))
                Variable$new(
                self, m$name, m$type, m$domain,
                 description=m$expltext)
            }
            else if (m$type == GMS_DT_EQU) {
                Equation$new(
                self, m$name, m$subtype, m$domain,
                 description=m$expltext)
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
      print("aliasList: ")
      print(aliasList)
      for (m in aliasList) {
      Alias$new(
        self, m$name, self$data[[m$aliasfor]])
      }

      if (values == TRUE) {

        symbolrecords = readSymbols(unlist(symbolsToRead),
        load_from, self$SysDir)

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
                      self$data[[s$names]]$records[id, d] = SpecialValues[["NA"]]
                    }
                  }
                }
              }

            }
          }
        }
      }
    },

    listSymbols = function(isValid=NULL) {
      if (!is.null(isValid)) {
        l = list()
        for (d in self$data) {
          if (d$isValid() == isValid) {
            l = append(l, d$name)
          }
        }
        return(l)
      }
      else {
        return(names(self$data))
      }
    },

    listSets = function() {
      sets = list()
      for (s in self$listSymbols()) {
        if(inherits(self$data[[s]], "Set") |
        inherits(self$data[[s]], "Alias") ) {
          sets = append(sets, s)
        }
      }
      return(sets)
    },
    listParameters = function() {
      parameters = list()
      for (s in self$listSymbols()) {
        if(inherits(self$data[[s]], "Parameter")) {
          parameters = append(parameters, s)
        }
      }
      return(parameters)
    },
    listAliases = function() {
      aliases = list()
      for (s in self$listSymbols()) {
        if (inherits(self$data[[s]], "Alias")) {
          aliases = append(Aliases, s)
        }
      }
      return(aliases)
    },
    listVariables = function() {
      variables = list()
      for (s in self$listSymbols()) {
        if(inherits(self$data[[s]], "Variable")) {
          variables = append(variables, s)
        }
      }
      return(variables)
    },
    listEquations = function() {
      equations = list()
      for (s in self$listSymbols()) {
        if(inherits(self$data[[s]], "Equation")) {
          equations = append(equations, s)
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
      df = data.frame()
      for (i in symbols) {
        if (i %in% self$listSets()) {
          symDescription = list(
            i,
            self$data[[i]]$isAlias(),
            self$data[[i]]$isSingleton,
            self$data[[i]]$domainNames,
            self$data[[i]]$domainType,
            self$data[[i]]$dimension,
            self$data[[i]]$number_records(),
            self$data[[i]]$getCardinality(),
            self$data[[i]]$getSparsity()
          )
          if (length(df) != 0) {
            df = rbind(df, setNames(symDescription, colNames))
          }
          else {
            df = data.frame(symDescription)
            colnames(df) = colNames
          }

        }
      }
      if (length(df) != 0) {
        return(df[order( df[,1]),])
      }
      else {
         return(NA)
      }
    },

    describeParameters = function(symbols=NA) {
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
            "cardinality",
            "sparsity"
            )
      df = data.frame()
      for (i in symbols) {
        if (i %in% self$listParameters()) {
          symDescription = list(
            i,
            self$data[[i]]$isScalar,
            self$data[[i]]$domainNames,
            self$data[[i]]$domainType,
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
          if (length(df) != 0) {
            df = rbind(df, setNames(symDescription, colNames))
          }
          else {
            df = data.frame(symDescription)
            colnames(df) = colNames
          }

        }
      }
      if (length(df) != 0) {
        return(df[order( df[,1]),])
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
      df = data.frame()
      for (i in symbols) {
        if (i %in% self$listVariables()) {
          symDescription = list(
            i,
            self$data[[i]]$type,
            self$data[[i]]$domainNames,
            self$data[[i]]$domainType,
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
          if (length(df) != 0) {
            df = rbind(df, setNames(symDescription, colNames))
          }
          else {
            df = data.frame(symDescription)
            colnames(df) = colNames
          }

        }
      }
      if (length(df) != 0) {
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
      df = data.frame()
      for (i in symbols) {
        if (i %in% self$listEquations()) {
          symDescription = list(
            i,
            self$data[[i]]$type,
            self$data[[i]]$domainNames,
            self$data[[i]]$domainType,
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
          if (length(df) != 0) {
            df = rbind(df, setNames(symDescription, colNames))
          }
          else {
            df = data.frame(symDescription)
            colnames(df) = colNames
          }

        }
      }
      if (length(df) != 0) {
        return(df[order( df[,1]),])
      }
      else {
         return(NA)
      }

    },

    printSpecialValues = function() {
      print(private$gdx_specVals_write)
    },

    write = function(gdxout) {
      private$validSymbolOrder()
      # remap special values
      specialValsGDX = getSpecialValues(gdxout, self$SysDir)
      # for (v in names(SpecialValues)) {
        for (s in self$data) {
          # no mapping required for alias
          if (s$type == GMS_DT_ALIAS || s$type == GMS_DT_SET) next
          colrange=(s$dimension+1):length(s$records)
          s$records[,colrange][is.nan(
            s$records[,colrange])] = 
            specialValsGDX[["UNDEF"]]

          s$records[,colrange][is.na(
            s$records[,colrange])] = 
            specialValsGDX[["NA"]]

          s$records[,colrange][
            ((s$records[,colrange]==Inf)
          & (sign(s$records[,colrange]) 
          == 1))] = specialValsGDX[["POSINF"]]

          s$records[,colrange][
            ((s$records[,colrange]==-Inf)
          &(sign(s$records[,colrange]) 
          == -1))] = specialValsGDX[["NEGINF"]]
          
          s$records[,colrange][
            ((s$records[,colrange] == 0)
          &(sign(1/s$records[,colrange]) 
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
      gdxWriteSuper(self$data, self$SysDir, gdxout)

    },

    isValid = function(verbose=FALSE, force=FALSE) {
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
    check = function() {
      if (private$requiresStateCheck == TRUE) {
         private$validSymbolOrder()
        if (all(self$listSymbols != self$listSymbols(isValid = TRUE) )) {
          print("error! Container contains invalid symbols")
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
            if (is.character(i)){
              doi = append(doi, TRUE)
            }
            else if ( (inherits(i, "Set") | inherits(i, "Alias")) &
            i$gams_name %in% orderedSymbols) {
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
          stop("Error: Graph cycle detected")
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
  gams_name = NULL,
  type = NULL,
  subtype = NULL,
  ref_container = NULL,
  domain = NULL,
  dimension = NULL,
  records = NULL,
  expltext = NULL,
  domainstr = NULL,
  domainNames = NULL,
  domainType = NULL,
  initialize = function(container=NA, name=NA,
                        type=NA, subtype=NA, 
                        domain=NA,
                        expltext=NA) {
    print("domain in symbol: ")
    print(domain)
    self$gams_name <- name
    self$type = type
    self$subtype = subtype
    private$requiresStateCheck = TRUE
    self$ref_container = container
    self$records = NA
    for (d in self$domain) {
      assert_that((inherits(d, "Set") || 
      inherits(d, "Alias") ||
      is.character(d)),
      msg = "All 'domain' elements must be typ Set, Alias, or Character"
      )
      if ((inherits(d, "Set") || 
      inherits(d, "Alias"))) {
        assert_that( d$dimension == 1,
        msg = "All linked 'domain' elements must be one dimensional"
        )
      }
    }

    self$domain = list()
    for (d in domain) {
      if (is.character(d)) {
        if (inherits(self$ref_container$data[[d]], "Set") ||
         inherits(self$ref_container$data[[d]], "Alias")) {
          self$domain = append(self$domain, 
          self$ref_container$data[[d]])
        }
        else {
          # attach as a plain string
          self$domain = append(self$domain, d)
        }
      }
      else {
        if ((inherits(d, "Set"))) {
          if (identical(d$ref_container,self$ref_container)) {
            self$domain = append(self$domain, d)
          }
          else {
            stop("domain elements cannot belong to a different container")
          }
        }
        else if (inherits(d, "Alias")) {
          if (identical(d$alias_with$ref_container, self$ref_container)) {
            self$domain = append(self$domain, d)
          }
          else {
            stop("domain elements cannot belong to a different container")
          }
        }
      }
    }
    
    self$dimension = length(self$domain)
    self$domainNames = self$domain_names()
    self$domainType = self$domain_type()
    self$domainstr = list()
    for (d in domain) {
      if (is.character(d)) {
        self$domainstr = append(self$domainstr, d)
      }
      else {
        self$domainstr = append(self$domainstr, d$gams_name)
      }
    }
    self$expltext = expltext

    container$data[[name]] = self
  },

  summary = function() {
    list(
      "gams_name" = self$gams_name,
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
      else if(self$dimension == 0) {
        return("none")
      }
      else {
         return("relaxed")
      }
  },
  getCardinality = function() {
    if (self$domain_type() == "relaxed" | self$domain_type() == "none"){
      return(NA)
    }
    else {
       card = 1
       for (i in self$domain){
         card = card * i$number_records()
       }
       return(card)
    }
  },
  getSparsity = function(){
    if (self$domain_type() == "relaxed" | self$domain_type() == "none"){
      return(NA)
    }
    else {
      return(1 - self$number_records()/self$getCardinality())
    }
  },
  getMaxValue = function(columns=NA) {
    if (inherits(self, "Parameter")) {
      if (is.na(columns)){
        columns = "value"
      }
      return(max(self$records[[columns]]))
    }
    else if (inherits(self, "Variable") | inherits(self, "Equation")) {
      if (is.na(columns)) {
        columns = "level"
      }

      if (!setequal(intersect(columns, 
      colnames(self$records[,(self$dimension+1):length(self$records)])), 
      columns)) {
        print("error! column name not valid")
        return()
      }
      else {
        return(max(self$records[, columns]))
      }
    }
  },
  getMinValue = function(columns=NA) {
    if (inherits(self, "Parameter")) {
      if (is.na(columns)){
        columns = "value"
      }
      return(min(self$records[[columns]]))
    }
    else if (inherits(self, "Variable") | inherits(self, "Equation")) {
      if (is.na(columns)) {
        columns = "level"
      }

      if (!setequal(intersect(columns, 
      colnames(self$records[,(self$dimension+1):length(self$records)])), 
      columns)) {
        print("error! column name not valid")
        return()
      }
      else {
        return(min(self$records[, columns]))
      }
    }
  },
  getMeanValue = function(columns=NA) {
    if (inherits(self, "Parameter")) {
      if (is.na(columns)){
        columns = "value"
      }
      return(mean(self$records[[columns]]))
    }
    else if (inherits(self, "Variable") | inherits(self, "Equation")) {
      if (is.na(columns)) {
        columns = "level"
      }

      if (!setequal(intersect(columns, 
      colnames(self$records[,(self$dimension+1):length(self$records)])), 
      columns)) {
        print("error! column name not valid")
        return()
      }
      else {
        return(mean(colMeans(self$records[columns])))
      }
    }
  },
  getMaxAbsValue = function(columns=NA) {
    if (inherits(self, "Parameter")) {
      if (is.na(columns)){
        columns = "value"
      }
      return(max(abs(self$records[[columns]])))
    }
    else if (inherits(self, "Variable") | inherits(self, "Equation")) {
      if (is.na(columns)) {
        columns = "level"
      }

      if (!setequal(intersect(columns, 
      colnames(self$records[,(self$dimension+1):length(self$records)])), 
      columns)) {
        print("error! column name not valid")
        return()
      }
      else {
        return(max(abs(self$records[, columns])))
      }
    }
  },
  whereMax = function(columns=NA) {
    if (inherits(self, "Parameter")) {
      if (is.na(columns)){
        columns = "value"
      }
      return(which.max(self$records[[columns]]))
    }
    else if (inherits(self, "Variable") | inherits(self, "Equation")) {
      if (is.na(columns)) {
        columns = "level"
      }

      if (!is.character(columns)){
        print("error! Only one column allowed.")
      }

      if (!setequal(intersect(columns, 
      colnames(self$records[,(self$dimension+1):length(self$records)])), 
      columns)) {
        print("error! column name not valid")
        return()
      }
      else {
        return(which.max(self$records[, columns]))
      }
    }
  },
  whereMaxAbs = function(columns=NA) {
    if (inherits(self, "Parameter")) {
      if (is.na(columns)){
        columns = "value"
      }
      return(which.max(abs(self$records[[columns]])))
    }
    else if (inherits(self, "Variable") | inherits(self, "Equation")) {
      if (is.na(columns)) {
        columns = "level"
      }

      if (!is.character(columns)){
        print("error! Only one column allowed.")
      }

      if (!setequal(intersect(columns, 
      colnames(self$records[,(self$dimension+1):length(self$records)])), 
      columns)) {
        print("error! column name not valid")
        return()
      }
      else {
        return(which.max(abs(self$records[, columns])))
      }
    }
  },
  whereMin = function(columns=NA) {
    if (inherits(self, "Parameter")) {
      if (is.na(columns)){
        columns = "value"
      }
      return(which.min(self$records[[columns]]))
    }
    else if (inherits(self, "Variable") | inherits(self, "Equation")) {
      if (is.na(columns)) {
        columns = "level"
      }

      if (!is.character(columns)){
        print("error! Only one column allowed.")
      }

      if (!setequal(intersect(columns, 
      colnames(self$records[,(self$dimension+1):length(self$records)])), 
      columns)) {
        print("error! column name not valid")
        return()
      }
      else {
        return(which.min(self$records[, columns]))
      }
    }
  },
  countNA = function(columns=NA) {
    if (inherits(self, "Parameter")) {
      if (is.na(columns)){
        columns = "value"
      }
      return(sum(self$records[[columns]] == SpecialValues[["NA"]]))
    }
    else if (inherits(self, "Variable") | inherits(self, "Equation")) {
      if (is.na(columns)) {
        columns = "level"
      }

      if (!setequal(intersect(columns, 
      colnames(self$records[,(self$dimension+1):length(self$records)])), 
      columns)) {
        print("error! column name not valid")
        return()
      }
      else {
        return(sum(self$records[,columns] == SpecialValues[["NA"]]))
      }
    }
  },
  countEps = function(columns=NA) {
    if (inherits(self, "Parameter")) {
      if (is.na(columns)){
        columns = "value"
      }
      return(sum(self$records[[columns]] == SpecialValues$EPS))
    }
    else if (inherits(self, "Variable") | inherits(self, "Equation")) {
      if (is.na(columns)) {
        columns = "level"
      }

      if (!setequal(intersect(columns, 
      colnames(self$records[,(self$dimension+1):length(self$records)])), 
      columns)) {
        print("error! column name not valid")
        return()
      }
      else {
        return(sum(self$records[,columns] == SpecialValues$EPS))
      }
    }
  },
  countUndef = function(columns=NA) {
    if (inherits(self, "Parameter")) {
      if (is.na(columns)){
        columns = "value"
      }
      return(sum(self$records[[columns]] == SpecialValues$UNDEF))
    }
    else if (inherits(self, "Variable") | inherits(self, "Equation")) {
      if (is.na(columns)) {
        columns = "level"
      }

      if (!setequal(intersect(columns, 
      colnames(self$records[,(self$dimension+1):length(self$records)])), 
      columns)) {
        print("error! column name not valid")
        return()
      }
      else {
        return(sum(self$records[,columns] == SpecialValues$UNDEF))
      }
    }
  },
  countPosinf = function(columns=NA) {
    if (inherits(self, "Parameter")) {
      if (is.na(columns)){
        columns = "value"
      }
      return(sum(self$records[[columns]] == SpecialValues$POSINF))
    }
    else if (inherits(self, "Variable") | inherits(self, "Equation")) {
      if (is.na(columns)) {
        columns = "level"
      }

      if (!setequal(intersect(columns, 
      colnames(self$records[,(self$dimension+1):length(self$records)])), 
      columns)) {
        print("error! column name not valid")
        return()
      }
      else {
        return(sum(self$records[,columns] == SpecialValues$POSINF))
      }
    }
  },
  countNeginf = function(columns=NA) {
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

      if (!setequal(intersect(columns, 
      colnames(self$records[,(self$dimension+1):length(self$records)])), 
      columns)) {
        print("error! column name not valid")
        return()
      }
      else {
        return(sum(self$records[,columns] == SpecialValues$NEGINF))
      }
    }
  },
  domain_names = function() {
    d = list()
    for (i in self$domain) {
      if (inherits(i, "Set") | inherits(i, "Alias")) {
        d = append(d, i$name)
      }
    }
    if (length(d) == 0) {
      return(NA)
    }
    else {
      return(d)
    }
  },
  getColLabelsForRecords = function() {
    column_names = list()
    for (i in seq_along(self$domain)) {
      if (is.character(self$domain[[i]])) {
        d = self$domain[[i]]
      }
      else if (inherits(self$domain[[i]], "Symbol")) {
        d = self$domain[[i]]$gams_name
      }
      else {
        print("unknown data type of domain")
      }
      if (d != "*") {
        column_names = append(column_names, paste0(d, "_", i))
      }
      else {
        column_names = paste0(column_names, paste0("uni_", i))
      }
    }
    if (self$type == GMS_DT_SET) {
      if(!is.na(self$expltext)) {
        column_names = append(column_names, "element_text")
      }
    }
    else if (self$type == GMS_DT_PAR) {
        column_names = append(column_names, "value")
    }
    else if (self$type == GMS_DT_VAR | self$type == GMS_DT_EQU) {
        column_names = append(column_names, private$attr())
    }
    else {
      print("not supported yet!")
    }
    return(column_names)
  },

  isValid = function(verbose=FALSE, force=FALSE) {
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
      if (self$requiresStateCheck == TRUE) {
        # if regular domain, symbols in domain must be valid
        if (self$domainType == "regular") {
          for (i in self$domain) {
            if (!(i %in% self$ref_container$data) ) {
              print("error! domain label not in the container")
            }
          }

          for (i in self$domain) {
            if (i$isValid() != TRUE) {
              print("error! domain label is not valid")
            }
          }
        }

        # if records exist, check consistency
        if (!is.null(self$records)) {
          if (inherits(self, "Set")){
            if (length(self$records) != self$dimension + 1) {
              print("error! records dimensions not correct")
            }
          }
          if (inherits(self, "Parameter")){
            if (length(self$records) != self$dimension + 1) {
              print("error! records dimensions not correct")
              if (self$dimension == 0 && nrow(self$records != 1)) {
                print("error! scalar with inconsistent dimension")
              }
            }
          }
          if (inherits(self, "Variable") | inherits(self, "Equation")){
            if (length(self$records) != self$dimension + length(private$attr())) {
              print("error! records dimensions not correct")
            }
          }
          # check if records are dataframe
          if (!is.data.frame(self$records)){
            print("Error! records must be dataframe")
          }

          # check column names and order
          cols = self$getColLabelsForRecords()
          if (!all(colnames(self$records) == cols) ){
            print("Error! columns incorrectly named")
          }

          # domain columns must be factor/character type

          # check for domain violations
          if (any(is.null(self$records[,1:self$dimension])) ||
          any(is.na(self$records[,1:self$dimension]) )) {
            print("domain violation detected")
          }

          # drop duplicates
          if (self$dimension != 0) {
            if (nrow(self$records) != nrow(unique(self$records))) {
              print("Error! duplicate rows detected")
            }
          }

          # check if all data columns are float
          if (inherits(self, "Set") | 
          inherits(self, "Parameter") | 
          inherits(self, "Equation")) {
            for (i in (self$dimension + 1):length(self$records)) {
              if (!all(is.numeric(self$records[, i]))) {
                print("all values should be numeric")
              }
            }
          }
        }

      }
      private$checkOff()
    },
    checkOn = function() {
      if (private$requireStateCheck == FALSE) {
        private$requireStateCheck == TRUE
      }
    },
    checkOff = function() {
      if (private$requireStateCheck == TRUE) {
        private$requireStateCheck == FALSE
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
      if (!is_singleton){
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
        stop(paste0("The argument 'records' is of length",c, " Expecting ", self$dimension + 1))
      }
      self$records = records
      columnNames = self$getColLabelsForRecords()
      colnames(self$records) = columnNames
    },
    isAlias = function() {
      return(private$is_alias)
    }


  ),
  active = list(
    isSingleton = function(is_singleton){
      if (missing(is_singleton)) {
        return(private$is_singleton)
      }
      else {
         private$is_singleton = is_singleton
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
      print("records in parameter setrecords")
      print(records)
      records = data.frame(records)

      # check dimensionality of dataframe
      r = nrow(records)
      c = length(records)
      print(paste0("parameter name: ", self$gams_name))
      print(paste0("parameter dimension: ", self$dimension))
      assertthat::assert_that(
        c == self$dimension + 1,
        msg = paste0("Dimensionality of records ", c - 1, 
        " is inconsistent with parameter domain specification ", 
        self$dimension)
      )

      self$records = records
      columnNames = self$getColLabelsForRecords()
      colnames(self$records) = columnNames

      # convert values column to numeric
      self$records$value = as.numeric(as.character(self$records$value))
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
      print("error source")
      print(records)
      print(paste0("name: ", self$gams_name))
      self$records = records
      columnNames = self$getColLabelsForRecords()
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
      columnNames = self$getColLabelsForRecords()
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
  inherit = Symbol,

  public = list(
    is_alias = NULL,
    alias_with = NULL,
    initialize = function(container=NA, gams_name=NA, 
                          alias_for=NA) {
      self$ref_container = container
      container$data[[gams_name]] = self

      self$gams_name = gams_name
      self$type = super$lblTypeSubtype()[["alias"]][[1]]
      self$is_alias = TRUE
      self$alias_with = private$setAlias(alias_for)
    }
  )
  ,
  private = list(
    setAlias = function(alias_with) {
      if (!(inherits(alias_with, "Set") | inherits(alias_with, "Alias"))) {
        stop("GAMS 'alias_with' must be type Set or Alias")
      }

      if (inherits(alias_with, "Alias")) {
        parent = alias_with
        while (!inherits(parent, "Set")) {
          parent = parent$alias_with
        }
        alias_with = parent
      }
      return(alias_with)
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
  for (p in paths_split) {
    if (file.exists(paste0(p, .Platform$file.sep, gams_exe))) {
      sysDirPath = p
    }
  }
  return(sysDirPath)
}

#is.nan function for dataframe
is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))

#is.infinite function for dataframe
# is.infinite.data.frame <- function(x)
# do.call(cbind, lapply(x, is.infinite))
