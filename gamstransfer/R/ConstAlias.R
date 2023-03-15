#
# GAMS - General Algebraic Modeling System Python API
#
# Copyright (c) 2017-2022 GAMS Software GmbH <support@gams.com>
# Copyright (c) 2017-2022 GAMS Development Corp. <support@gams.com>
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

.ConstAlias <- R6Class(
  ".ConstAlias",
  inherit = .ConstSymbol,
  public = list(
    .gams_type = NULL,
    .gams_subtype = NULL,
    aliasWith = NULL,
    isSingleton = NULL,
    initialize = function(container=NULL, name=NULL, aliasFor=NULL, 
                          domain, isSingleton, 
                          description, domainType, numberRecords
                          ) {
      super$initialize(container, name, .gdxSymbolTypes()[["GMS_DT_ALIAS"]], 
      isSingleton, domain, description, domainType, numberRecords)

      self$aliasWith = aliasFor
      lockBinding("aliasWith", self)

      self$isSingleton = isSingleton
      lockBinding("isSingleton", self)
    },

    getCardinality = function() {
      if (private$.is_parent_set()) {
        return(self$refContainer[self$aliasWith]$getCardinality())
      }
      else {
        return(NA)
      }
    },

    getSparsity = function() {
      if (private$.is_parent_set()) {
        return(self$refContainer[self$aliasWith]$getSparsity())
      }
      else {
        return(NA)
      }
    },

    setRecords = function(records) {
      records = data.frame(records)
      columnNames = self$domainLabels
      columnNames = append(columnNames, "element_text")
      colnames(records) = columnNames
      super$.set_records(records)
    },

    copy = function(destination = NULL, overwrite = FALSE) {

      if (private$.is_parent_set()) {
        # copy parent sets
        self$aliasWith$copy(destination, overwrite)
      }

      # copy alias
      private$.copy(destination, overwrite)
    }
  ),

  active = list(
    summary = function() {
    if (private$.is_parent_set()) {
      alias_with_name = self$aliasWith$name
    }
    else {
      alias_with_name = self$aliasWith
    }

    return(list(
      "name" = self$name,
      "aliasWith_name" = alias_with_name,
      "isSingleton" = self$isSingleton,
      "domainNames" = self$domainNames,
      "dimension" = self$dimension,
      "description" = self$description,
      "numberRecords" = self$numberRecords,
      "domainType" = self$domainType
    ))
    }
  ),

  private = list(
    .is_parent_set = function() {
      return(inherits(self$aliasWith, ".ConstSet"))
    }
  )

)
