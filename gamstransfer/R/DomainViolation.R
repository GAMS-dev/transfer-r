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

DomainViolation <- R6::R6Class (
  "DomainViolation",
  public = list(
    symbol=NULL,
    dimension = NULL,
    domain= NULL,
    violations = NULL,
    initialize = function(symbol, dimension, domain,violations) {
      self$symbol = symbol
      self$dimension = dimension
      self$domain = domain
      self$violations = violations
      lockBinding("symbol", self)
      lockBinding("dimension", self)
      lockBinding("domain", self)
      lockBinding("violations", self)
    },
    format = function(...) {
      paste0("GAMS Transfer: DomainViolation with properties: \n", 
      "Symbol: ", self$symbol$name, "\n",
      "dimension: ", self$dimension, "\n",
      "domain: ", self$domain$name, "\n",
      "violations: ", toString(self$violations))
    }
  )
)