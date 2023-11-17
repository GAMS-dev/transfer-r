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

#' @title Equation Class
#' @description A class for Equation objects. This class inherits from an abstract 
#' symbol class.The documentation for methods common to all symbols can be
#' accessed via help(Symbol).
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
#' @field container reference to the Container that the symbol belongs to
#' @field shape a list describing the array dimensions if records were
#'  converted with $toDense()
#' @field summary output a list of only the metadata
#' @field type type of variable (string)
Equation <- R6Class(
  "Equation",
  inherit = .Symbol,
  public = list(

    #' @description There are two different ways to create a GAMS Equation and 
    #' add it to a Container. One is using the Equation constructor and 
    #' the other is using addEquation method which calls the Equation 
    #' constructor internally.
    #' addEquation is a Container method to add a Equation.
    #' @param container A reference to the Container object that the symbol 
    #' is being added to
    #' @param name string argument for name of the Equation
    #' @param type Type of equation being created [eq (or E/e), geq 
    #' (or G/g), leq (or L/l), nonbinding (or N/n), external (or X/x)]
    #' @param domain an optional argument specifying a list of strings, 
    #' a string. default value is NULL.
    #' @param records specify set records as a vector or a dataframe.
    #' @param domainForwarding an optional logical argument to specify 
    #' domain forwarding. Default value is FALSE.
    #' @param description string specifying description for the set
    #' @return a Equation object
    initialize = function(container=NULL, name=NULL, 
                          type=NULL,
                          domain=NULL,
                          records = NULL,
                          domainForwarding=FALSE,
                          description="") {

      self$type = type
      # call from outside
      type = .EquationTypes[[type]]

      super$initialize(container, name,
                      domain, description, domainForwarding)
      if (!is.null(records)) {
        self$setRecords(records)
      }
    },

    #' main convenience method to set standard dataframe formatted records
    #' @param records specify set records as a vector, matrix, 
    #' array or a dataframe.
    setRecords = function(records) {
      if (inherits(records, c("list", "array", "numeric", "integer"))) {

        if (is.array(records) || inherits(records, "numeric")) {
          records= list(level = records) # default to level
        }

        usr_attr = intersect(private$.attr(), names(records))
        if (inherits(records, "list")) {
          #check if user attributes are valid
          if (length(usr_attr) < length(names(records))) {
            stop(paste0("Unrecognized user attribute detected in `records`. ",
            "The attributes must be one of the following ", 
            toString(private$.attr()),
            " and must be passed as names of a named list.\n"))
          }
          # check if elements of the list are arrays or numerics
          for (i in length(records)) {
            if (!(is.numeric(records[[i]]) || 
            all(SpecialValues$isNA(records[[i]])))) {
              stop("All elements of the named list `records` must ",
              "be type numeric.\n")
            }
          }
        }

        # here everything is a named list
        # convert lists with numeric entries to array
        # if vectors, convert them to arrays
        for (i in length(records)) {
          if (inherits(records[[i]], c("numeric", "integer"))) {
            records[[i]] = array(records[[i]])
          }
        }

        # check if all records have equal size
        size1 = dim(records[[1]])

        for (i in seq_along(records)) {
          if(!all(dim(records[[i]]) == size1)) {
            stop("array sizes passed into records must be all equal.\n")
          }
        }

        if (self$dimension != 0) {
          if (self$domainType != "regular") {
            stop(paste0(
              "Data conversion for non-scalar array (i.e., matrix) ",
              "format into records is only possible for symbols where ",
              "self$domainType = 'regular'. ",
              "Must define symbol with specific domain set objects, ",
              "symbol domainType is currently ", self$domainType, ".\n" ))
          }
        }


        for (i in self$domain) {
          if (i$isValid() == FALSE) {
            stop(paste0(
              "Domain set ", i$name, " is invalid and cannot be used to ",
              "convert array-to-records. Use $isValid(verbose = TRUE) to ",
              "debug this domain set symbol before proceeding.\n"
            ))
          }
        }

        if (self$dimension >= 2) {
          for (i in names(records)) {
            recs = records[[i]]
            if (!all(dim(recs) == self$shape())) {
              stop(paste0("User passed array/matrix with shape ", 
              toString(dim(recs)), " but anticipated shape was ", 
              toString(self$shape()), 
              " based on domain set information -- must reconcile ",
              "before array-to-records conversion is possible.\n"))
            }
          }
        }

        values = list()
        valuenames = names(records)

        for (i in seq_along(records)) {
          tryCatch(
            {
              values[[i]] = as.numeric(records[[i]])
            },
            error = function(cond) {
              stop("error converting array to numeric type\n")
            },
            warning = function(cond) {
              stop("error converting array to numeric type\n")
            }
          )
        }
        if (self$dimension == 0) {
          self$records = data.frame(matrix(nrow=1, ncol=length(usr_attr)))
          colnames(self$records) = usr_attr

          for (i in seq_along(records)) {
            if (length(records[[i]]) > 1) {
              stop("A scalar provided with more than one entries.\n")
            }
            else {
              self$records[names(records)[[i]]] = records[[i]]
            }
          }
          return()
        }

        #everything from here on is a non-scalar
        listOfDomains = replicate(self$dimension, list(NA))
        for (i in seq_along(self$domain)) {
          d = self$domain[[i]]
          listOfDomains[[i]] = d$records[,1]
        }
        df = expand.grid(listOfDomains, stringsAsFactors = FALSE) # ij is a dataframe
        colnames(df) = super$.get_default_domain_labels()
        attr(df, "out.attrs") <- NULL
        for (i in seq_along(values)) {
          df[valuenames[[i]]] = values[[i]]
        }

        # drop zeros but not EPS
        colrange = (self$dimension + 1):length(df)
        df1 = df[colrange]
        rsum = rowSums(df1)
        iseps = ((df1 == 0) & 
        (sign(1/df1)==-1) )
        iseps_rowsums= rowSums(iseps)
        df = df[which(!(rsum==0 & iseps_rowsums == 0)),]

        row.names(df) <- NULL

        if (nrow(df) == 0) {
          if(self$dimension == 0) {
            df = data.frame()
          }
          else {
            df = df[, 1:self$dimension, drop=FALSE]
          }
        }
        else {
          # reorder columns
          correct_order = c()
          if (self$dimension > 0) {
            correct_order = colnames(df)[(1:self$dimension)]
          }
          correct_order = append(correct_order, usr_attr)
          df = df[, correct_order]
        }
        self$records = df
        self$.linkDomainCategories()

      }
      else {
        # check if records is a dataframe and make if not
        records = data.frame(records)

        usr_colnames = colnames(records)
        if (self$dimension == 0) {
          columnNames = c()
        }
        else {
          columnNames = usr_colnames[1:self$dimension]
        }
        if (any(duplicated(columnNames))) {
          columnNames = super$.get_default_domain_labels()
        }

        if (self$dimension +  1 > length(usr_colnames)) {
          usr_attr = NULL
        }
        else {
          usr_attr=  usr_colnames[(self$dimension + 1):length(usr_colnames)]
        }

        usr_attr=  usr_colnames[(self$dimension + 1):length(usr_colnames)]

        #check dimensionality
        if ((length(records) < self$dimension) ||
          (length(records) > self$dimension + length(private$.attr()))) {
          stop(cat(paste0("Dimensionality of records ", (length(records)-length(private$.attr())),
          " is inconsistent with equation domain specification ", 
          self$dimension, " must resolve before records can be added\n\n",
          "NOTE:",
          "columns not named ", toString(private$.attr()),
          " will be interpreted as domain columns, check that the data.frame conforms ",
          "to the required notation.\n",
          "User passed data.frame with columns: ", toString(usr_colnames), "\n")))
        }

        # check if numeric
        if (self$dimension + 1 <= length(records)) {
          for (i in (self$dimension + 1):length(records)) {
            if (!(is.numeric(records[[i]]) || 
            all(SpecialValues$isNA(records[[i]])))) {
              stop(paste0("All elements of the, `" , colnames(records)[i], 
              "` column of `records` not type numeric or NA.\n"))
            }
          }
        }

        # reorder columns
        correct_order = c()
        if (self$dimension > 0) {
          correct_order = colnames(records)[(1:self$dimension)]
        }
        correct_order = append(correct_order, private$.attr())
        correct_order = intersect(correct_order, usr_colnames)
        records = records[correct_order]

        if (self$dimension == 0) {
          colnames(records) = correct_order
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

        colnames(records) = correct_order
        self$records = records

      }
      return(invisible(NULL))
    },

    # var.equ
    equals = function(other, columns=NULL, checkUELs=TRUE, 
    checkMetaData=TRUE, rtol=0, atol=0,
    verbose=FALSE) {
      super$.check_equals_common_args(other, checkUELs,
      checkMetaData, verbose)

      super$.checkColumnsArgument(columns)

      super$.check_equals_numeric_args(atol, rtol)

      super$equals(other, columns=columns, checkUELs=checkUELs,
      checkMetaData=checkMetaData,rtol=rtol, atol=atol,
      verbose=verbose)
    },

    generateRecords = function(density = 1, func=NULL, seed=NULL) {
      if(!((self$domainType == "regular") || (self$dimension == 0))) {
        stop("Cannot generate records for the symbol unless the symbol has ",
        "domain objects for all dimension, i.e., <symbol>$domainType == 'regular'",
        "or the symbol is a scalar\n")
      }

      if (!is.null(seed)) {
        if (!(is.numeric(seed) && round(seed) == seed)) {
          stop("The argument `seed` must be an integer\n")
        }
        set.seed(seed)
      }

      if (is.function(func)) {
        func = list("level" = func)
      }
      else if (is.null(func)) {
        temp_fun = function(size) {
          return(runif(n=size))
        }
        func = list("level" = temp_fun)
      }
      else if (inherits(func, "list")) {
        attr_names = names(func)
        if (length(intersect(attr_names, private$.attr())) != length(attr_names)) {
          stop(paste0("the names of the named list `func` must be ",
          "one of the following: ", toString(private$.attr()), "\n"))
        }

        lapply(func, function(f) {
          if (!is.function(f)) {
            stop("All arguments of the named list `func` must be functions\n")
          }
        })
      }
      else {
        "The argument `func` must be of type function, named list, or NULL\n"
      }

      if (self$dimension != 0) {
        recs = super$.generate_records_index(density)
      }
      else {
        recs = data.frame(1)
      }

      tryCatch(
      {
        for (attr in names(func)) {
          recs[[attr]] = func[[attr]](size = nrow(recs))
        }

        # fill missing attributes with default values
        missing_attr = setdiff(private$.attr(), names(func))
        for (m in missing_attr) {
          recs[[m]] = private$.default_values[[private$.type]][[m]]
        }

        # rearrange recs
        # rearrange recs
        all_colnames = colnames(recs)
        if (self$dimension != 0) {
          indx_colnames = all_colnames[1:self$dimension]
          value_colnames = private$.attr()
          correct_colnames = append(indx_colnames, value_colnames)
          recs = recs[correct_colnames]
        }
        else {
          recs = recs[private$.attr()]
        }
      },
      error = function(e) {
          message(paste0(e, "\n"))
      }
      )

      private$.records = recs
      set.seed(NULL)
    },

    copy = function(destination = NULL, overwrite = FALSE) {
      newsym = private$.copy(destination, overwrite)
      if (is.null(newsym)) return(invisible(NULL))

      newsym$type = self$type
    },

    toList = function() {
      l = list(
               type = "Equation",
               name= self$name,
               description = self$description,
               subtype = self$type,
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
    defaultValues = function() {
      return(private$.getDefaultValues())
    },

    isScalar = function() {
      return(self$dimension == 0)
    },

    type = function(type_input) {
      if (missing(type_input)) {
        return(private$.type)
      }
      else {
        if (!any(.EquationTypes == tolower(type_input))) {
          stop(cat(paste0("Argument 'type' must be one of the following:\n\n",
              "1. 'eq', 'E', or 'e' -- equality\n",
              "2. 'geq', 'G', or 'g' -- greater than or equal to inequality\n",
              "3. 'leq', 'L', or 'l'  -- less than or equal to inequality\n",
              "4. 'nonbinding', 'N', or 'n'  -- nonbinding relationship\n",
              "5. 'cone', 'C', or 'c' -- cone equation\n",
              "6. 'external', 'X', or 'x' -- external equation\n",
              "7. 'boolean', 'B', or 'b' -- boolean equation\n"
          )))
        }

        private$.type = tolower(type_input)
      }
    },

    summary = function() {
      return(list(
        "name" = self$name,
        "description" = self$description,
        "type" = self$type,
        "domain" = self$domainNames,
        "domainType" = self$domainType,
        "dimension" = self$dimension,
        "numberRecords" = self$numberRecords
      ))
    }
  ),
  private = list(
    .type = NULL,

    .default_values = list(
      "eq" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = 0.0,
          "scale" = 1.0
      ),
      "geq" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "leq" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = SpecialValues$NEGINF,
          "upper" = 0.0,
          "scale" = 1.0
      ),
      "nonbinding" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = SpecialValues$NEGINF,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "cone" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "external" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = 0.0,
          "scale" = 1.0
      ),
      "boolean" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = 0.0,
          "scale" = 1.0
      )
    ),

    .getDefaultValues = function(columns=NULL) {
      if (is.null(columns)) {
        columns = private$.attr()
      }

      if (length(columns) == 1) {
        return(private$.default_values[[self$type]][[columns]])
      }
      else {
        def_vals = unlist(lapply(columns, function(c) { 
          return(private$.default_values[[self$type]][[c]]) }), 
          use.names=FALSE)
        names(def_vals) = columns
        return(def_vals)
      }
    }
  )
)
