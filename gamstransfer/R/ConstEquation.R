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

.ConstEquation <- R6Class(
  ".ConstEquation",
  inherit = .ConstSymbol,
  public = list(
    type = NULL,
    initialize = function(container=NULL, name=NULL, 
                          type=NULL,
                          domain=NULL,
                          records = NULL,
                          description="", domaintype=NULL,
                          numberRecords = NULL) {
      self$type = type
      lockBinding("type", self)

      # call from outside
      type = .EquationTypes[[type]]

      symtype = .gdxSymbolTypes()[["GMS_DT_EQU"]]
      symsubtype = .EqTypeSubtype()[[type]]

      super$initialize(container, name,
                      symtype, symsubtype, 
                      domain, description, domaintype, numberRecords)
      if (!is.null(records)) {
        self$setRecords(records)
      }
    },

    setRecords = function(records) {
        records = data.frame(records)
        columnNames = c()
        if (self$dimension != 0) {
          columnNames = self$domainLabels
        }
        columnNames = append(columnNames, private$.attr())
        colnames(records) = columnNames
        super$.set_records(records)
    }
  ),

  active = list(
    summary = function() {
      return(list(
        "name" = self$name,
        "type" = self$type,
        "domainNames" = self$domainNames,
        "dimension" = self$dimension,
        "description" = self$description,
        "numberRecords" = self$numberRecords,
        "domainType" = self$domainType
      ))
    }
  )
)
