library(R6)

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
    Filename = NULL,
    SysDir = NULL,
    data = NULL,
    initialize = function(gdxname=NA, sysDirpath=NA) {
      if (missing(sysDirpath)) {
        self$SysDir = find_gams()
      }
      else {
        self$SysDir = sysDirpath
      }

      self$data = list()
      if (!missing(gdxname)) {

        self$Filename <- gdxname
        # get all symbols and metadata from c++
        # process it and populate various fields
        metadata = getSymbols(self$Filename, self$SysDir)
        for (m in metadata) {
            m1 = m[-1]
            if (m$type == GMS_DT_PAR) {
              Parameter$new(
                self, m$name, m$domain,
                m$expltext)
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
                m$expltext)
            }
            else if (m$type == GMS_DT_EQU) {
                Equation$new(
                self, m$name, m$subtype, m$domain,
                m$expltext)
            }
            else if (m$type == GMS_DT_ALIAS) {
                Alias$new(
                self, m$name, self$data[[m$aliasfor]])
            }
            else {
                print("Wrong data type")
            }
        }
        # setSpecialValues(self$Filename, self$SysDir)
        # private$gdx_specVals_write =
        # getSpecialValues(self$Filename, self$SysDir)
      }
      else {
        self$Filename <- NA
      }
    },
    listSets = function() {
      print(paste0("sets: ", self$sets))
    },
    printSpecialValues = function() {
      print(private$gdx_specVals_write)
    },

    read = function(symNames=NA) {
      if (missing(symNames)) {
        # read all
        symNames = names(self$data)
      }
      # else {
      #   for (s in symNames) {
      #     if (is.null(self$data[[s]])) {
      #       print(paste0("symbol ", s, " not in the container!"))
      #     }
      #     else {
      #       self$data[[s]]$records = as.data.frame(
      #       readSymbol(s, self$Filename, self$SysDir))
      #     }
      #     # private$remap_special_values(s)
      #   }
      # }
    symbolrecords = readSymbols(symNames,
    self$Filename, self$SysDir)

    for (s in symbolrecords) {
      self$data[[s$names]]$setRecords(s$records)
    }

    },
    write = function(gdxout){
      private$validSymbolOrder()
      print("writefunction")
      gdxWriteSuper(self$data, self$SysDir, gdxout)
    }
  ),
  private = list(
    gdx_specVals_write = list(),
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
    listSymbols = function() {
      return(names(self$data))
    },
    validSymbolOrder = function() {
      orderedSymbols = list()
      symbolsToSort = private$listSymbols()

      idx = 1
      while (length(symbolsToSort) != 0) {
        sym = symbolsToSort[idx]
        # special 1D sets (universe domain & relaxed sets)
        if (inherits(self$data[[sym]], "Set") &
        self$data[[sym]]$dimension == 1 &
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
    # getOrderedSymbols = function() {
    #   print("reorderhere")
    #   orderedSymbols = list()

    #   symbols = private$listSymbols()
    #   print(symbols)
    #   # append all sets with universe domain
    #   setmap = list()
    #   nodes = list() # unique elements
    #   for (s in symbols) {
    #     if (inherits(self$data[[s]], "Set")) {
    #       if (self$data[[s]]$domain == "*") {
    #         domainlist = "*"

    #         orderedSymbols = append(orderedSymbols, s)
    #         nodes = unique(append(nodes, c(s, domainlist)))
    #         setmap[[s]] = domainlist

    #         }
    #       else if (self$data[[s]]$dimension ==1) {
    #         domainlist = self$data[[s]]$domain$gams_name
    #         print("domainlist: ")
    #         print(domainlist)
    #          nodes = unique(append(nodes, c(s, domainlist)))
    #         setmap[[s]] = unlist(domainlist)
    #       }
    #       else {

    #       }
    #     }
    #   }
    #   print("here")
    #   print(setmap)
    #   print("nodes")
    #   print(nodes)
    #   # append all 1D sets\
    #   adj = list()
    #   print(paste0("length: ", length(setmap)))
    #   if (length(setmap) != 0) {
    #     print("here2")
    #     for (n in nodes) {
    #       for (s in names(setmap)) {
    #         print(paste0("s ",s))
    #         if (n %in% setmap[[s]]) {
    #           adj[[n]] = append(adj[[n]], s)

    #         }
    #       }
    #     }
    #   }
      
    #   print("here1")
    #   print(adj)
    #   sortedSets = private$dfs(adj)
    #   print("sorted sets")
    #   print(sortedSets)

    #   if (length(setdiff(sortedSets, nodes)) != 0 &
    #   length(setdiff(nodes, sortedSets)) != 0) {
    #     print("error cycle detected")
    #   }

    #   sortedSets = sortedSets[-1]
    #   print("orderedsymbols 1")
    #   print(orderedSymbols)
    #   for (s in sortedSets) {
    #     if (!(s %in% orderedSymbols)) {
    #       orderedSymbols = append(orderedSymbols, s)
    #     }
    #   }
    #   print("orderedsymbols 2")
    #   print(orderedSymbols)

    #   # 1 D alias
    #   for (s in symbols) {
    #     if ( inherits(self$data[[s]], "Alias") &
    #       !(s %in% orderedSymbols) ) {
    #       orderedSymbols = append(orderedSymbols, s)
    #       }
    #   }

    #   # all other sets and aliases
    #   for (s in symbols) {
    #     if ( (inherits(self$data[[s]], "Set") | inherits(self$data[[s]], "Alias"))
    #     & !(s %in% orderedSymbols) ) {
    #     orderedSymbols = append(orderedSymbols, s)
    #     }
    #   }

    #   #everything else
    #   for (s in symbols) {
    #     if ( inherits(self$data[[s]], "Parameter") | 
    #     inherits(self$data[[s]], "Variable") |
    #     inherits(self$data[[s]], "Equation")
    #     & !(s %in% orderedSymbols) ) {
    #     orderedSymbols = append(orderedSymbols, s)
    #     }
    #   }

    #   print("ordered symbols")
    #   print(orderedSymbols)
    # },
    # dfs = function(graph1) {
    #   print("graph")
    #   print(graph1)
    #   stack = list("*")
    #   visited = list()
    #   while(length(stack) != 0) {
    #     vertex = stack[[1]]
    #     print(paste0("vertex: ", vertex))
    #     stack = stack[-1]
    #     if (!is.na(vertex %in% visited)){
    #       print(paste0("visited ", visited))
    #       visited = append(visited, vertex)
    #       print(paste0("diff: ", setdiff(graph1[[vertex]], visited)))
    #       stack = append(stack, unlist(setdiff(graph1[vertex], visited)))
    #     }
    #   }
    #   return(visited)
    # },

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
  initialize = function(container=NA, name=NA,
                        type=NA, subtype=NA, 
                        domain=NA,
                        expltext=NA) {
    self$gams_name <- name
    self$type = type
    self$subtype = subtype
    self$ref_container = container
    self$records = NA
    self$domain = list()
    for (d in domain) {
      if (d == "*") {
      self$domain = append(self$domain, 
      "*")
      }
      else {
      self$domain = append(self$domain, 
      self$ref_container$data[[d]])
      }

    }
    self$dimension = length(self$domain)
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
      return(length(self$records))
    }
    else {
      return(NA)
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
  }
  ),
  private = list(
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
      if (!is_singleton){
        type = super$lblTypeSubtype()[["set"]][1]
        subtype = super$lblTypeSubtype()[["set"]][2]
      }
      else {
        type = super$lblTypeSubtype()[["singleton_set"]][1]
        subtype = super$lblTypeSubtype()[["singleton_set"]][2]
      }

      super$initialize(container, gams_name,
                      type, subtype,
                      domain, description)

      if (any(!is.na(records))) {
        self$setRecords(records)
      }
      invisible(self)
    },
    setRecords = function(records) {
      if (is.data.frame(records)) {
        c = length(records)
        if (c == self$dimension) {
          # no element text
          records["element_text"] = ""
        }
        else if (c == self$dimension + 1) {
          names(records)[c] <- "element_text"
        }
        else {
          print("inconsistent records size!")
        }
      }
      colnames = self$getColLabelsForRecords()
      self$records = as.data.frame(records, col.names = colnames)
    }

  )
  )

Parameter <- R6Class(
  "Parameter",
  inherit = Symbol,
  public = list(

    initialize = function(container=NA, gams_name=NA,
                          domain=NA,records=NA,
                          description=NA ){
      
      type = super$lblTypeSubtype()[["parameter"]][1]
      super$initialize(container, gams_name,
                      type, NA, 
                      domain, description)
      if (any(!is.na(records))) {
        self$setRecords(records)
      }
    },

    setRecords = function(records) {
      colnames = self$getColLabelsForRecords()
      self$records = as.data.frame(records, col.names = colnames)
    }

  )
)

Variable <- R6Class(
  "Variable",
  inherit = Symbol,
  public = list(

    initialize = function(container=NA, gams_name=NA, 
                          type="free",
                          domain=NA, records=NA,
                            description=NA) {
      if (is.integer(type)){
        symtype = type
      }
      else {
         symtype = super$lblTypeSubtype()[[type]][1]
      }

      super$initialize(container, gams_name,
                      symtype, NA, 
                      domain, description)
      if (any(!is.na(records))) {
        self$setRecords(records)
      }
    },
    setRecords = function(records) {
      colnames = self$getColLabelsForRecords()
      # print("in records")
      # print(colnames)
      self$records = as.data.frame(records, col.names = colnames)
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
        symtype = super$lblTypeSubtype()[[type]][1]
        symsubtype = super$lblTypeSubtype()[[type]][2]
      }

      super$initialize(container, gams_name,
                      symtype, symsubtype, 
                      domain, description)
      if (any(!is.na(records))) {
        self$setRecords(records)
      }
      # if (any(!is.na(records))) {
      #   colnames = self$getColLabelsForRecords()
      #   self$records = as.data.frame(records, col.names = colnames)
      # }
    },
    setRecords = function(records) {
      colnames = self$getColLabelsForRecords()
      self$records = as.data.frame(records, col.names = colnames)
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
      self$type = "alias"
      self$is_alias = TRUE
      self$alias_with = private$setAlias(alias_for)
    }
  )
  ,
  private = list(
    setAlias = function(alias_with) {
      if (!(inherits(alias_with, "Set") | inherits(alias_with, "Alias"))) {
        print("GAMS 'alias_with' must be type Set or Alias")
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

