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
          dense = 1
          for (i in self$domainNames) {
            domainSym = self$refContainer[self$refContainer$getSymbolNames(i)]
            dense = dense * domainSym$numberRecords
          }

          return(1 - self$numberRecords/dense)
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

  copy = function(destination = NULL, overwrite = FALSE) {
    private$.copy(destination, overwrite)
    return(invisible(NULL))
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
      if (self$dimension == 0) return(c())
      domain_label_input = self$domainNames
      domain_label_input[domain_label_input == "*"] = "uni"
      dup_labels = duplicated(domain_label_input)
      if (any(dup_labels)) {
        domain_label_input = paste0(domain_label_input, 1:self$dimension)
      }

      return(domain_label_input)
    },

    .copy = function(destination = NULL, overwrite = FALSE) {
      if (!inherits(destination, "Container")) {
        stop("The argument `destination` must be of type `Container`\n")
      }

      if (!(is.logical(overwrite) && (length(overwrite) == 1))) {
        stop("The argument `overwrite` must be of type `logical`\n")
      }

      if (is.null(destination[self$name])){
        # symbol doesn't exist in the destination container
        destination$read(self$refContainer, self$name)
        return(NULL)
      }
      else {
        # symbol exists in the destination container
        if (!overwrite) {
          stop(paste0("Symbol ", self$name, 
          " already exists in `destination`\n"))
        }
        newsym = destination[self$name]

        symbol_types = c("Set", "Parameter", "Variable", "Equation", "Alias")
        constsymbol_types = c(".ConstSet", ".ConstParameter", 
        ".ConstVariable", ".ConstEquation", ".ConstAlias")

        for (s in 1:length(symbol_types)) {
          if (inherits(newsym, symbol_types[s]) && 
          !inherits(self, c(constsymbol_types[s]))) {
            stop(paste0("Cannot copy a symbol of type ", class(self)[1], 
            " to `destination` symbol type ", class(newsym)[1], 
            ". To overwrite, the symbols must be of same type"))
          }
        }

        # copy all fields of one symbol to another
        newsym$records = self$records
        newsym$description = self$description
        newsym$domain = self$domain
        if (self$dimension == 0) return(NULL)

        # constsymbol domain is anyway relaxed so no need to adjust domain
        return(newsym)
      }
  }

  )
)
