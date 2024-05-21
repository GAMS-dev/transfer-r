#
# GAMS - General Algebraic Modeling System R API
#
# Copyright (c) 2017-2024 GAMS Software GmbH <support@gams.com>
# Copyright (c) 2017-2024 GAMS Development Corp. <support@gams.com>
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

#' @title UniverseAlias Class
#' @description A class for Alias objects that are aliased to the Universe set.
#' Please visit https://www.gams.com/latest/docs/API_R_GAMSTRANSFER.html
#' for detailed documentation of this package.
#'
#' #' @examples
#' # create a container
#' m = Container$new()
#' # add a UniverseAlias
#' u = UniverseAlias$new(m, "u")

UniverseAlias <- R6::R6Class(
  "UniverseAlias",
  inherit = .BaseAlias,
  public = list(

    initialize = function(container=NULL, name=NULL, ...) {
      args = list(...)
      from_gdx = args[["from_gdx"]]
      if (is.null(from_gdx)) from_gdx=FALSE

      super$initialize(container, name, from_gdx=from_gdx)
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

    getSparsity = function() {
      return(0)
    },

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

    asList = function() {
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
