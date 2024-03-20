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

#' @title Set Class
#' @description A class for Set objects. This class inherits from an abstract Symbol class.
#' The documentation for methods common to all symbols can be accessed via help(.Symbol).
#' Please visit https://www.gams.com/latest/docs/API_R_GAMSTRANSFER.html
#' for detailed documentation of this package.
#'
#' @examples
#' # create a container
#' m = Container$new()
#' # add a set
#' i = Set$new(m, "i")
#' # access records
#' i_recs = i$records
Set <- R6::R6Class(
  "Set",
  inherit = .Symbol,
  public = list(
    initialize = function(container=NULL, name=NULL,
                          domain="*", isSingleton=FALSE,
                          records = NULL, 
                          domainForwarding = FALSE,
                          description="") {
      self$isSingleton <- isSingleton

      super$initialize(container, name,
                      domain, description, domainForwarding)

      if (!is.null(records)) {
        self$setRecords(records)
      }
      invisible(self)
    },

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
    },

    asList = function() {
      l = list(
               class = "Set",
               name= self$name,
               description = self$description,
               isSingleton = self$isSingleton,
               domain = self$domainNames,
               domainType = self$domainType,
               dimension = self$dimension,
               numberRecords = self$numberRecords,
               records = self$records
      )
      return(l)
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
