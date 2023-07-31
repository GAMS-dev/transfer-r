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

#' @title Parameter Class
#' @description A class for Parameter objects. This class inherits from an abstract 
#' Symbol class.The documentation for methods common to all symbols can be accessed 
#' via help(Symbol).
#' countEPS, countNA, countNegInf, countPosInf, countUndef,
#' getSparsity, getMaxValue, getMinValue, getMeanValue, getMaxAbsValue,
#' isValid, toDense, whereMax, whereMaxAbs, whereMin.
#' @field description description of symbol
#' @field dimension of symbol
#' @field domainForwarding flag that forces set elements to be recursively 
#' included in all parent sets (i.e., implicit set growth)
#' @field domainLabels column headings for the records dataframe
#' @field domainNames string version of domain names
#' @field domainType none, relaxed or regular depending on state of domain links
#' @field name name of symbol
#' @field numberRecords 	number of symbol records
#' @field records the main symbol records
#' @field refContainer reference to the Container that the symbol belongs to
#' @field shape a list describing the array dimensions if records were
#'  converted with $toDense()
#' @field summary output a list of only the metadata
Parameter <- R6Class(
  "Parameter",
  inherit = .Symbol,
  public = list(
    #' @description There are two different ways to create a GAMS 
    #' parameter and add it to a Container. One is using the 
    #' Parameter constructor and the other is using addParameter 
    #' method which calls the Parameter constructor internally.
    #' addParameter is a Container method to add a Parameter.
    #' @param container A reference to the Container object that the symbol is 
    #' added to
    #' @param name string argument for name of the Parameter
    #' @param domain an optional argument specifying a list of strings, 
    #' a string. default value is NULL.
    #' @param records specify set records as a vector, matrix, array,
    #'  or a dataframe.
    #' @param domainForwarding an optional logical argument to specify 
    #' domain forwarding. Default value is FALSE.
    #' @param description string specifying description for the set
    #' @return a Parameter object
    initialize = function(container=NULL, name=NULL,
                          domain=NULL,records = NULL,
                          domainForwarding = FALSE,
                          description="") {

      type = .gdxSymbolTypes()[["GMS_DT_PAR"]]
      super$initialize(container, name,
                      type, 0, 
                      domain, description, domainForwarding)

      if (!is.null(records)) {
        self$setRecords(records)
      }
    },

    #' main convenience method to set standard dataframe formatted records
    #' @param records specify set records as a vector, matrix, 
    #' array or a dataframe.
    setRecords = function(records) {
      if (inherits(records, c("array", "numeric", "integer"))) { # checks for matrix + arrays + vectors + numbers
        if ((length(records) > 1) && (self$domainType != "regular")) {
          stop(paste0(
            "Data conversion for non-scalar array (i.e., matrix) format into ",
            "records is only possible for symbols where ",
            "self$domainType = 'regular'. ",
            "Must define symbol with specific domain set objects, ",
            "symbol domainType is currently ",self$domainType,".\n" ))
        }

        for (i in self$domain) {
          if (i$isValid() == FALSE) {
            stop(paste0(
              "Domain set ", i$name, 
              " is invalid and cannot be used to convert array-to-records. ",
               "Use $isValid(verbose = TRUE) to debug this domain set ",
               "symbol before proceeding.\n"
            ))
          }
        }
        # convert vector and numeric input to an array
        if (inherits(records, c("numeric", "integer"))) {
          records = array(records)
        }

        if (self$dimension >= 1) {
          if (!all(dim(records) == self$shape())) {
            stop(paste0("User passed array/matrix/numeric with shape ", 
            toString(dim(records)), " but anticipated shape was ", 
            toString(self$shape()), " based on domain set information -- ",
            "must reconcile before array-to-records conversion is possible.\n"))
          }
        }

        tryCatch(
          {
            values = as.numeric(aperm(records))
          },
          error = function(cond) {
            stop("error converting array to numeric type\n")
          },
          warning = function(cond) {
            stop("error converting array to numeric type\n")
          }
        )

        if (self$dimension == 0) {
          if (length(records) > 1) {
            stop("A scalar provided with more than one entries.\n")
          }
          else {
            self$records = data.frame(value=records)
          }
          return()
        }

        #everything from here on is a parameter
        listOfDomains = replicate(self$dimension, list(NA))
        for (i in seq_along(self$domain)) {
          d = self$domain[[i]]
          listOfDomains[[i]] = d$records[,1]
        }
        df = rev(expand.grid(rev(listOfDomains), stringsAsFactors = FALSE)) # ij is a dataframe
        columnNames = super$.get_default_domain_labels()

        colnames(df) = columnNames
        attr(df, "out.attrs") <- NULL

        df["value"] = values
        # drop zeros but not EPS
        colrange = (self$dimension + 1):length(df)

        logicalVector = ((df[,colrange] == 0) & 
        !(sign(1/df[,colrange])==-1) )
        df = df[(!logicalVector),]

        row.names(df) <- NULL
        if (nrow(df) == 0) {
          self$records = NULL
        }
        else {
          self$records = df
          self$.linkDomainCategories()
        }
      }
      else {

        no_label = FALSE # assume column labels exist
        if (is.null(names(records))) {
          no_label = TRUE
        }
        # check if records is a dataframe and make if not
        records = data.frame(records)

        # check dimensionality of dataframe
        r = nrow(records)
        c = length(records)

        if (c > (self$dimension + 1) || c < self$dimension) {
          stop(paste0("Dimensionality of records ", c - 1, 
          " is inconsistent with parameter domain specification ", 
          self$dimension))
        }

        if (no_label) {
          columnNames = super$.get_default_domain_labels()
        }
        else {
          if (self$dimension == 0) {
            columnNames = c()
          }
          else {
            columnNames = colnames(records)[1:self$dimension]
          }
        }

        if (c == self$dimension + 1) {
          columnNames = append(columnNames, "value")


          #if records "value" is not numeric, stop.
          val_column = records[,length(records)]
          if (!(is.numeric(val_column) || all(SpecialValues$isNA(val_column)))) {
              stop("All entries in the 'value' column of a parameter ",
              "must be numeric.\n")
          }
        }

        if (self$dimension == 0) {
          colnames(records) = columnNames
          self$records = records
          return()
        }

        records[, 1:self$dimension] = lapply(seq_along(self$domain), 
        function(d) {
          if (is.factor(records[, d])) {
            levels(records[, d]) = trimws(levels(records[, d]), which="right")
          }
          else {
            records[, d] = factor(records[, d], levels = 
            unique(records[, d]), ordered=TRUE)
            levels(records[, d]) = trimws(levels(records[, d]), which="right")
          }
          return(records[, d])
        })

        records = data.frame(records)
        colnames(records) = columnNames
        self$records = records
      }
      return(invisible(NULL))
    },

    # par
    equals = function(other, checkUELs=TRUE, 
    checkMetaData=TRUE, rtol=0, atol=0,
    verbose=FALSE) {
      super$.check_equals_common_args(other, checkUELs,
      checkMetaData, verbose)

      super$.check_equals_numeric_args(atol, rtol)

      super$equals(other, checkUELs=checkUELs,
      checkMetaData=checkMetaData,rtol=rtol, atol=atol,
      verbose=verbose)
    },

    generateRecords = function(density = 1, func=NULL, seed=NULL) {
      if(!((self$domainType == "regular") || (self$dimension == 0))) {
        stop("Cannot generate records for the symbol unless the symbol has ",
        "domain objects for all dimension, i.e., <symbol>$domainType == ",
        "'regular' or the symbol is a scalar\n")
      }

      if (!is.null(seed)) {
        if (!(is.numeric(seed) && round(seed) == seed)) {
          stop("The argument `seed` must be an integer\n")
        }
        set.seed(seed)
      }

      if (!(is.function(func) || is.null(func) || inherits(func, "list"))) {
        "The argument `func` must be of type function or NULL\n"
      }

      if (self$dimension != 0) {
        recs = super$.generate_records_index(density)
      }
      else {
        recs = data.frame(1)
      }

      tryCatch(
      {
        if (is.null(func)) {
          recs$value = runif(n = nrow(recs))
        }
        else {
          recs$value = func(size = nrow(recs))
        }
      },
      error = function(e) {
          message(paste0(e, "\n"))
      }
      )

      private$.records = recs
      set.seed(NULL)
    },

    .getDefaultValues = function(columns=NULL) {
      return(0)
    }
  ),

  active = list(
    isScalar = function() {
      return(self$dimension == 0)
    },

    summary = function() {
      return(list(
        "name" = self$name,
        "isScalar" = self$isScalar,
        "domainObjects" = self$domain,
        "domainNames" = self$domainNames,
        "dimension" = self$dimension,
        "description" = self$description,
        "numberRecords" = self$numberRecords,
        "domainType" = self$domainType
      ))
    }
  )
)
