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
#' @field container reference to the Container that the symbol belongs to
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
        return(self$container$getUELs(ignoreUnused = ignoreUnused))
      }
      else {
        return(NULL)
      }
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
        stop(paste0("Cannot compare objects because ", s$name, 
        " is not valid. Use ", s$name, 
        "$isValid(verbose=TRUE) to get more details\n"))
      }

      if (!inherits(other, c(".Symbol", ".BaseAlias"))) {
        stop("The argument `other` must be a Symbol object")
      }

      if (!other$isValid()) {
        stop(paste0("Cannot compare objects because ", other$name, 
        " is invalid. Use ", other$name, "$isValid(verbose=TRUE) to debug.\n"))
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
    },

    copy = function(destination = NULL, overwrite = FALSE) {

      # copy alias
      private$.copy(destination, overwrite)
      return(invisible(NULL))
    },

    toList = function() {
      l = list(
        class = "Alias",
        name = self$name,
        aliasWith = "*"
      )
      return(l)
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
      df = data.frame(self$container$getUELs())
      colnames(df) = "uni"
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
      return("uni")
    },

    summary = function() {
    return(list(
      "name" = self$name,
      "description" = self$description,
      "aliasWith" = self$aliasWith
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
        super$.testContainer()
      }
    },

    .testParentSet = function() {
      if (!self$container$hasSymbols(self$aliasWith$name)) {
        stop(paste0("Parent set ", self$aliasWith$name, " of alias ", 
        self$name, " is no longer in the container and cannot ",
        "be referenced\n"))
      }
    }
  )
)
