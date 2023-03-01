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

.ConstParameter <- R6Class(
  ".ConstParameter",
  inherit = .ConstSymbol,
  public = list(
    isScalar = NULL,
    initialize = function(container=NULL, name=NULL,
                          domain=NULL,records = NULL,
                          description="", domaintype=NULL,
                          numberRecords = NULL) {
      type = .gdxSymbolTypes()[["GMS_DT_PAR"]]
      super$initialize(container, name,
                      type, 0, 
                      domain, description, domaintype,
                      numberRecords)

      if (!is.null(records)) {
        self$setRecords(records)
      }
      self$isScalar = (self$dimension == 0)
    },

    setRecords = function(records) {
      records = data.frame(records)
      columnNames = self$domainLabels
      columnNames = append(columnNames, "value")
      colnames(records) = columnNames
      super$.set_records(records)
    }
  ),

  active = list(
    summary = function() {
      return(list(
        "name" = self$name,
        "isScalar" = self$isScalar,
        "domainNames" = self$domainNames,
        "dimension" = self$dimension,
        "description" = self$description,
        "numberRecords" = self$numberRecords,
        "domainType" = self$domainType
      ))
    }
  )
)
