#
# GAMS - General Algebraic Modeling System Python API
#
# Copyright (c) 2017-2023 GAMS Software GmbH <support@gams.com>
# Copyright (c) 2017-2023 GAMS Development Corp. <support@gams.com>
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
Alias <- R6Class(
  "Alias",
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
    initialize = function(container=NULL, name=NULL, 
                          aliasFor=NULL) {
      super$initialize(container, name)
      self$aliasWith = aliasFor
      self$.isUniverseAlias = FALSE
    },

    format = function(...) paste0("GAMS Transfer: R6 object of class Alias. 
    Use ", self$name, "$summary for details"),

    getUELs = function(dimension =NULL, codes=NULL, ignoreUnused = FALSE) {
      super$.testContainer()
      private$.testParentSet()
      self$aliasWith$getUELs(dimension, codes, ignoreUnused)
    },

    setUELs = function(uels, dimension=NULL, rename=FALSE) {
      super$.testContainer()
      private$.testParentSet()
      self$aliasWith$setUELs(uels, dimension, rename)
    },

    reorderUELs = function(uels=NULL, dimension=NULL) {
      super$.testContainer()
      private$.testParentSet()
      self$aliasWith$reorderUELs(uels, dimension)
    },

    addUELs = function(uels, dimension=NULL) {
      super$.testContainer()
      private$.testParentSet()
      self$aliasWith$addUELs(uels, dimension)
    },

    removeUELs = function(uels=NULL, dimension=NULL) {
      super$.testContainer()
      private$.testParentSet()
      self$aliasWith$removeUELs(uels, dimension)
    },

    renameUELs = function(uels, dimension=NULL, allowMerge=FALSE) {
      super$.testContainer()
      private$.testParentSet()
      self$aliasWith$renameUELs(uels, dimension, allowMerge)
    },

    #' @description getSparsity get the sparsity of the symbol 
    #' w.r.t the cardinality
    getSparsity = function() {
      super$.testContainer()
      private$.testParentSet()
      return(self$container[self$aliasWith$name]$getSparsity())
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

    getDomainViolations = function() {
      super$.testContainer()
      private$.testParentSet()
      return(self$aliasWith$getDomainViolations())
    },

    findDomainViolations = function() {
      super$.testContainer()
      private$.testParentSet()
      return(self$aliasWith$findDomainViolations())
    },

    hasDomainViolations = function() {
      super$.testContainer()
      private$.testParentSet()
      return(self$aliasWith$hasDomainViolations())
    },

    countDomainViolations = function() {
      super$.testContainer()
      private$.testParentSet()
      return(self$aliasWith$countDomainViolations())
    },

    dropDomainViolations = function() {
      super$.testContainer()
      private$.testParentSet()
      return(self$aliasWith$dropDomainViolations())
    },

    countDuplicateRecords = function() {
      super$.testContainer()
      private$.testParentSet()

      return(self$aliasWith$countDuplicateRecords())
    },

    findDuplicateRecords = function(keep="first") {
      super$.testContainer()
      private$.testParentSet()

      return(self$aliasWith$findDuplicateRecords(keep))
    },

    hasDuplicateRecords = function() {
      super$.testContainer()
      private$.testParentSet()

      return(self$aliasWith$hasDuplicateRecords())
    },

    dropDuplicateRecords = function(keep="first") {
      super$.testContainer()
      private$.testParentSet()

      return(self$aliasWith$dropDuplicateRecords(keep))
    },

    #' main convenience method to set standard dataframe formatted records
    #' @param records specify set records as a string vector or a dataframe.
    setRecords = function(records) {
      super$.testContainer()
      private$.testParentSet()
      return(self$container[self$aliasWith$name]$setRecords(records))
    },

    # set/alias
    equals = function(other, checkUELs=TRUE, 
    checkElementText=TRUE, checkMetaData=TRUE,
    verbose=FALSE) {
      super$.testContainer()
      private$.testParentSet()

      self$aliasWith$equals(other, checkUELs=checkUELs,
      checkElementText=checkElementText,
      checkMetaData=checkMetaData, verbose=verbose)
    },

    generateRecords = function(density = 1) {
      super$.testContainer()
      private$.testParentSet()

      self$aliasWith$generateRecords(density)
    },

    copy = function(destination = NULL, overwrite = FALSE) {
      super$.testContainer()
      private$.testParentSet()

      # copy parent sets
      self$aliasWith$copy(destination, overwrite)

      # copy alias
      private$.copy(destination, overwrite)
    },

    toList = function() {
      if (inherits(self$aliasWith, c("Set", "Alias"))) {
        aliasWithName = self$aliasWith$name
      }
      else {
        aliasWithName = selt$aliasWith
      }
      l = list(
        class = "Alias",
        name = self$name,
        aliasWith = aliasWithName
      )
      return(l)
    }
  ),

  active = list(

    aliasWith = function(alias_with_input) {
      if (missing(alias_with_input)) {
        return(private$.aliasWith)
      }
      else {
        super$.testContainer()
        if ((inherits(alias_with_input, "UniverseAlias"))) {
          stop("GAMS 'aliasWith' cannot be a UniverseAlias. Create a new UniverseAlias symbol instead\n")
        }
        if (!(inherits(alias_with_input, c("Set", "Alias")))) {
          stop("GAMS 'aliasWith' must be type Set or Alias\n")
        }

        if (inherits(alias_with_input, "Alias")) {
          parent = alias_with_input
          while (!inherits(parent, "Set")) {
            parent = parent$aliasWith
          }
          alias_with_input = parent
        }
        if (is.null(private$.aliasWith)) {
          private$.aliasWith = alias_with_input
        }
        else {
          if (!identical(private$.aliasWith, alias_with_input)) {
            self$.requiresStateCheck = TRUE
            self$container$.requiresStateCheck = TRUE
            private$.aliasWith = alias_with_input
          }
        }
      }
    },

    isSingleton = function(is_singleton) {
      super$.testContainer()
      private$.testParentSet()
      if (missing(is_singleton)) {
        container = self$container
        sym = container[self$aliasWith$name]
        return(sym$isSingleton)
      }
      else {
        container = self$container
        sym = container[self$aliasWith$name]
        sym$isSingleton = is_singleton
      }
    },

    description = function(description_input) {
      super$.testContainer()
      private$.testParentSet()
      if (missing(description_input)) {
        container = self$container
        aliaswithname = self$aliasWith$name
        sym = container[aliaswithname]
        return(sym$description)
      }
      else {
        container = self$container
        aliaswithname = self$aliasWith$name
        sym = container[aliaswithname]
        sym$description = description_input
      }
    },

    dimension = function(dimension_input) {
      super$.testContainer()
      private$.testParentSet()
      if (missing(dimension_input)) {
        return(self$container[self$aliasWith$name]$dimension)
      }
      else {
        container = self$container
        sym = container[self$aliasWith$name]
        sym$dimension = dimension_input
      }
    },

    records = function(records_input) {
      super$.testContainer()
      private$.testParentSet()
      if (missing(records_input)) {
        return(self$container[self$aliasWith$name]$records)
      }
      else {
        self$container[self$aliasWith$name]$records = records_input
      }

    },

    domain = function(domain_input) {
      super$.testContainer()
      private$.testParentSet()
      if (missing(domain_input)) {
        return(self$container[self$aliasWith$name]$domain)
      }
      else {
        container = self$container
        sym = container[self$aliasWith$name]
        sym$domain = domain_input
      }
    },

    numberRecords = function() {
      super$.testContainer()
      private$.testParentSet()
      return(self$container[self$aliasWith$name]$numberRecords)
    },

    domainType = function() {
      super$.testContainer()
      private$.testParentSet()
      return(self$container[self$aliasWith$name]$domainType)
    },

    domainNames = function() {
      super$.testContainer()
      private$.testParentSet()
      return(self$container[self$aliasWith$name]$domainNames)
    },

    domainLabels = function() {
      super$.testContainer()
      private$.testParentSet()
      return(self$container[self$aliasWith$name]$domainLabels)
    },

    summary = function() {
    super$.testContainer()
    private$.testParentSet()
    if (inherits(self$aliasWith, c("Set", "Alias"))) {
      aliasWithName = self$aliasWith$name
    }
    else {
      aliasWithName = selt$aliasWith
    }
    return(list(
      "name" = self$name,
      "description" = self$description,
      "aliasWith" = aliasWithName,
      "isSingleton" = self$isSingleton,
      "domain" = self$domainNames,
      "domainType" = self$domainType,
      "dimension" = self$dimension,
      "numberRecords" = self$numberRecords
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
        if (self$container[self$aliasWith$name]$isValid() == FALSE) {
          stop(paste0("Alias is not valid because parent set ", self$aliasWith$name,
          "is not valid\n"))
        }
      }
    },

    .testParentSet = function() {
      if (!self$container$hasSymbols(self$aliasWith$name)) {
        stop(paste0("Parent set ", self$aliasWith$name, " of alias ", 
        self$name, " is no longer in the container and cannot 
        be referenced\n"))
      }
    }
  )
)
