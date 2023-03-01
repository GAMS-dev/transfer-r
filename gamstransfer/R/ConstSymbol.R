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

.ConstSymbol <- R6Class(
  ".ConstSymbol",
  inherit = .BaseSymbol,
  public = list(
    records = NULL,
    description = NULL,
    dimension = NULL,
    domain = NULL,
    refContainer = NULL,
    name = NULL,
    numberRecords = NULL,
    domainType = NULL,
    domainLabels = NULL,
  initialize = function(container, name,
                        type, subtype, 
                        domain,
                        description, domaintype,
                        numberRecords) {

    super$initialize(type, subtype)

    self$refContainer = container
    lockBinding("refContainer", self)

    self$name = name
    lockBinding("name", self)
    container[name] = self

    self$domain = domain
    lockBinding("domain", self)

    self$dimension = length(domain)
    lockBinding("dimension", self)

    self$domainType = private$.domain_type_map[[domaintype]]
    lockBinding("domainType", self)

    self$description = description
    lockBinding("description", self)

    self$numberRecords = numberRecords
    lockBinding("numberRecords", self)

    self$domainLabels = private$.get_domain_labels()
    lockBinding("domainLabels", self)
  },

  getCardinality = function() {
    tryCatch(
      {
        if (length(self$domainNames) == 0) {
          return(NA)
        }
        else {
          for (n in self$domainNames) {
            if (!self$refContainer$hasSymbols(n)) {
              return(NA)
            }
          }
          card = 1
          for (n in self$domainNames) {
            domainSym = self$refContainer[self$refContainer$getSymbolNames(n)]
            card = card * domainSym$numberRecords
          }
          return(card)
        }
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  },

  getSparsity = function() {
    tryCatch(
      {
        if (length(self$domainNames) == 0) {
          return(NA)
        }
        else {
          for (n in self$domainNames) {
            if (!self$refContainer$hasSymbols(n)) {
              return(NA)
            }
          }

          for (n in self$domainNames) {
            return(1 - self$numberRecords/self$getCardinality())
          }
        }
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  }

  ),

  active = list(
    domainNames = function() {
      if (is.null(self$domain) || (length(self$domain) == 0)) {
        return(NA)
      }
      else {
        return(self$domain)
      }
    }
  ),
  private = list(
    symbolMaxLength = 63,
    descriptionMaxLength = 255,
    .domain_type_map = list("none", "relaxed", "regular"),
    .set_records = function(records) {
      self$records = records
      lockBinding("records", self)
    },

    .get_domain_labels = function() {
      dom_is_univ = (self$domain == "*")
      dom_temp = self$domain
      dom_temp[dom_is_univ] = "uni"
      column_names = lapply(seq_along(dom_temp), function(i) {
        return(paste0(dom_temp[[i]], "_", i))
      })

      return(column_names)
    }
  )
)
