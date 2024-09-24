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

.BaseAlias <- R6::R6Class(
  ".BaseAlias",
  public = list(
    .isUniverseAlias = NULL,
    .requiresStateCheck = NULL,
    initialize = function(container = NULL, name = NULL, ...) {
      args <- list(...)
      from_gdx <- args[["from_gdx"]]

      self$.requiresStateCheck <- TRUE
      if (from_gdx) {
        private$.ref_container <- container
        private$.name <- name
      } else {
        self$container <- container
        self$name <- name
      }
      container[name] <- self
    }
  ),
  active = list(
    container = function(ref_container_input) {
      if (missing(ref_container_input)) {
        return(private$.ref_container)
      } else {
        if (is.null(ref_container_input)) {
          private$.ref_container <- NULL
          self$.requiresStateCheck <- TRUE
          return()
        }
        if (!inherits(ref_container_input, "Container")) {
          stop("Symbol 'container' must be type Container\n")
        }
        if (is.null(self$container)) {
          if (!identical(self$container, ref_container_input)) {
            self$.requiresStateCheck <- TRUE
          }
          private$.ref_container <- ref_container_input
        } else {
          self$.requiresStateCheck <- TRUE
          private$.ref_container <- ref_container_input
        }
      }
    },
    name = function(name_input) {
      if (missing(name_input)) {
        return(private$.name)
      } else {
        private$.testContainer()
        if (!is.character(name_input)) {
          stop("GAMS symbol 'name' must be type chracter\n")
        }

        if (nchar(name_input) > private$symbolMaxLength) {
          stop(paste0(
            "GAMS symbol 'name' is too long,",
            " max is ", private$symbolMaxLength, " characters"
          ))
        }

        if (self$container$hasSymbols(name_input)) {
          stop(paste0(
            "A symbol with the name ", name_input,
            " already exists in the container\n"
          ))
        }

        if (is.null(private$.name)) {
          self$.requiresStateCheck <- TRUE
          private$.name <- name_input
        } else {
          if (private$.name != name_input) {
            self$.requiresStateCheck <- TRUE
          }
          private$.name <- name_input
        }
      }
    }
  ),
  private = list(
    .testContainer = function() {
      if (!inherits(self$container, "Container")) {
        stop("UniverseAlias/Alias is no longer referring a Container object\n")
      }
    },
    .copy = function(destination = NULL, overwrite = FALSE) {
      if (!inherits(destination, "Container")) {
        stop("The argument `destination` must be of type `Container`\n")
      }

      if (!(is.logical(overwrite) && (length(overwrite) == 1))) {
        stop("The argument `overwrite` must be of type `logical`\n")
      }

      if (is.null(destination[self$name])) {
        # symbol doesn't exist in the destination container
        destination$read(self$container, self$name)
        return(NULL)
      } else {
        # symbol exists in the destination container
        if (!overwrite) {
          stop(paste0("Symbol ", self$name, " already exists in `destination`\n"))
        }
        newsym <- destination[self$name]

        if (class(newsym)[1] != class(self)[1]) {
          stop(paste0(
            "Cannot copy a symbol of type ", class(self)[1],
            " to `destination` symbol type ", class(newsym)[1],
            ". To overwrite, the symbols must be of same type"
          ))
        }

        return(newsym)
      }
    }
  )
)
