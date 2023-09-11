#' @title Set Class
#' @description A class for Set objects. This class inherits from an abstract Symbol class.
#' The documentation for methods common to all symbols can be accessed via help(Symbol).
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
      # check if named list
      no_label = FALSE # assume column labels exist
      if (is.null(names(records))) {
        no_label = TRUE
      }

      # check if records is a dataframe and make if not
      records = data.frame(records)
      c = length(records)

      if (c > self$dimension + 1 || c < self$dimension) {
        stop(paste0("The argument 'records' is of length ",
        c, " Expecting ", self$dimension + 1, "\n"))
      }

      if (no_label) {
        columnNames = super$.get_default_domain_labels()
      }
      else {
        columnNames = colnames(records)[1:self$dimension]
      }

      if (c == self$dimension + 1) {
        columnNames = append(columnNames, "element_text")
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
      return(invisible(NULL))
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
    },

    copy = function(destination = NULL, overwrite = FALSE) {
      newsym = private$.copy(destination, overwrite)
      if (is.null(newsym)) return(invisible(NULL))

      newsym$isSingleton = self$isSingleton
      return(invisible(NULL))
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
        "description" = self$description,
        "isSingleton" = self$isSingleton,
        "domain" = self$domainNames,
        "domainType" = self$domainType,
        "dimension" = self$dimension,
        "numberRecords" = self$numberRecords
      ))
    }
  ),
  private = list(
    .is_singleton = NULL
  )
  )