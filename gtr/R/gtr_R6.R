library(R6)
GMS_DT_SET = 0
GMS_DT_PAR = 1
GMS_DT_VAR = 2
GMS_DT_EQU = 3
GMS_DT_ALIAS = 4
GMS_DT_MAX = 5

SpecialValues = list(
  "NA" = NA,
  "EPS" = -0.0,
  "UNDEF" = NaN,
  "POSINF" = Inf,
  "NEGINF" = -Inf
  )

Container <- R6::R6Class(
  "Container",
  public= list(
    Filename = NULL,
    SysDir = NULL,
    data = NULL,
    initialize = function(gdxname=NA, sysDirpath=NA){

      if (missing(sysDirpath)){
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
                m$dimension, m$expltext)
            }
            else if (m$type == GMS_DT_SET) {
                Set$new(
                self, m$name, m$domain,
                m$dimension, NA, m$expltext)
            }
            else if (m$type == GMS_DT_VAR) {
                Variable$new(
                self, m$name, m$type, m$subtype, m$domain,
                m$dimension, m$expltext)
            }
            else if (m$type == GMS_DT_EQU) {
                Equation$new(
                self, m$name, m$type, m$subtype, m$domain,
                m$dimension, m$expltext)
            }
            else {
                print("Wrong data type")
            }
        }

        private$gdx_specVals_write =
        getSpecialValues(self$Filename, self$SysDir)
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

    read = function(symName=NA) {
      if (missing(symName)) {
        # read all
      for (d in names(self$data)) {
        self$data[[d]]$records = as.data.frame(
          readSymbol(d, self$Filename, self$SysDir))
          private$remap_special_values(d)
      }
      }
      else {
        for (s in symName) {
          if (is.null(self$data[[s]])) {
            print(paste0("symbol ", s, " not in the container!"))
          }
          else {
            self$data[[s]]$records = as.data.frame(
            readSymbol(s, self$Filename, self$SysDir))
          }
          private$remap_special_values(s)
        }
      }


    },
      write = function(symName=NA){
        if (missing(symName)) {
          print("provide symbol name!")
        }
        else {
          gdxWriteTrial(self$data[[symName]]$records, symName,
          self$SysDir, self$data[[symName]]$dimension, self$data[[symName]]$type)
        }
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
    }
  )
  )

Symbol <- R6Class(
  "Symbol",
public=list(
  gams_name = NULL,
  type = NULL,
  subtype = NULL,
  ref_container = NULL,
  domain = NULL,
  dimension = NULL,
  records = NULL,
  expltext = NULL,
  initialize = function(container=NA, name=NA,
                        type=NA, subtype=NA, 
                        domain=NA, dimension=NA,
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
    self$dimension = dimension
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
    for (i in seq_along(self$domain)){
      if (is.character(self$domain[[i]])){
        d = self$domain[[i]]
      }
      else if (inherits(self$domain[[i]], "Symbol")){
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
    if (self$type == "set" | self$type == "singleton_set") {
      if(!is.na(self$expltext)) {
        column_names = append(column_names, "element_text")
      }
    }
    else if (self$type == "parameter" | self$type == "scalar") {
        column_names = append(column_names, "value")
    }
    else {
      print("not supported yet!")
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
                          domain="*", dimension=NA,
                          element_text = NA,
                          description=NA, records = NA){
      super$initialize(container, gams_name,
                      "set", NA,
                      domain, dimension, description)
      if (any(!is.na(records))) {
        if (!is.na(element_text)) {
          if (length(element_text) == length(records)){
            records["element_text"] = element_text
          }
          else {
            print("Error! element text and records must be same length")
          }
        }
        else {
          records["element_text"] = ""
        }

        colnames = self$getColLabelsForRecords()
        self$records = as.data.frame(records, col.names = colnames)
      }
    }

  )
  )

Parameter <- R6Class(
  "Parameter",
  inherit = Symbol,
  public = list(

    initialize = function(container=NA, gams_name=NA,
                          domain=NA, dimension=NA,
                          expltext=NA, records=NA){
      super$initialize(container, gams_name,
                      "parameter", NA, 
                      domain, dimension, expltext)
        if (any(!is.na(records))) {
          colnames = self$getColLabelsForRecords()
          self$records = as.data.frame(records, col.names = colnames)
      }
    }

  )
  )

Variable <- R6Class(
  "Variable",
  inherit = Symbol,
  public = list(

    initialize = function(container=NA, gams_name=NA, 
                          type=NA, subtype=NA,
                          domain=NA, dimension=NA,
                          expltext=NA){
      super$initialize(container, gams_name,
                      "variable", subtype, 
                      domain, dimension, expltext)
    }
  )
  )

Equation <- R6Class(
  "Equation",
  inherit = Symbol,
  public = list(

    initialize = function(container=NA, gams_name=NA, 
                          type=NA, subtype=NA,
                          domain=NA, dimension=NA,
                          expltext=NA){
      super$initialize(container, gams_name,
                      "equation", subtype, 
                      domain, dimension, expltext)
    }
  )
  )

find_gams <- function() {
  systemDirectory = "";

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

