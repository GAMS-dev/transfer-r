#' @title Symbol Abstract Class
#' @description An abstract symbol class from 
#' which the classes Set, Parameter, Variable, 
#' and Equation are inherited.
.Symbol <- R6Class(
  ".Symbol",
  public = list(
  .requiresStateCheck = NULL,
  .gams_type = NULL,
  .gams_subtype = NULL,
  initialize = function(container, name,
                        type, subtype, 
                        domain,
                        description,
                        domainForwarding) {

    self$.gams_type = type
    self$.gams_subtype = subtype


    self$.requiresStateCheck = TRUE

    #' @field refContainer reference to the Container that the symbol 
    #' belongs to. Type Container.
    self$refContainer = container # also sets the check flag

    #' @field name name of the symbol
    self$name <- name
    container[name] = self

    self$domain = domain

    self$description = description
    self$domainForwarding = domainForwarding

  },

  format = function(...) paste0("GAMS Transfer: R6 object of class ", 
  class(self)[1], ". Use ", self$name, "$summary for details"),

  #' @description getMaxValue get the maximum value
  #' @param columns columns over which one wants to get the maximum.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  getMaxValue = function(columns=NULL) {
    if (is.null(self$records) || inherits(self, "Set")) {
      return(NA)
    }

    columns = private$.checkColumnsArgument(columns)

    max_vals = unlist(lapply(columns, function(c) {
      if (!is.null(self$records) && is.null(self$records[[c]])) {
        if (inherits(self, "Parameter")) return(0)
        default_value = self$.getDefaultValues(columns=c)
        return(default_value)
      }
      else {
        return(private$.getMetric(c, "max"))
      }
    }), use.names =  FALSE)

    return(max(max_vals))
  },

  #' @description getMinValue get the minimum value in value column
  #' @param columns columns over which one wants to get the minimum.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  getMinValue = function(columns=NULL) {
    if (is.null(self$records) || inherits(self, "Set")) {
      return(NA)
    }

    columns = private$.checkColumnsArgument(columns)

    min_vals = unlist(lapply(columns, function(c) {
      if (!is.null(self$records) && is.null(self$records[[c]])) {
        if (inherits(self, "Parameter")) return(0)
        default_value = self$.getDefaultValues(columns=c)
        return(default_value)
      }
      else {
        return(private$.getMetric(c, "min"))
      }
    }), use.names =  FALSE)

    return(min(min_vals))
  },

  #' @description getMeanValue get the mean value in value column
  #' @param columns columns over which one wants to get the mean.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  getMeanValue = function(columns=NULL) {
    if (is.null(self$records) || inherits(self, "Set")) {
      return(NA)
    }

    columns = private$.checkColumnsArgument(columns)

    mean_vals = unlist(lapply(columns, function(c) {
      if (!is.null(self$records) && is.null(self$records[[c]])) {
        if (inherits(self, "Parameter")) return(0)
        default_value = self$.getDefaultValues(columns=c)
        return(default_value)
      }
      else {
        return(private$.getMetric(c, "mean"))
      }
    }), use.names =  FALSE)

    return(mean(mean_vals))
  },

  #' @description getMaxAbsValue get the maximum absolute value in value column
  #' @param columns columns over which one wants to get the 
  #' maximum absolute value.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  getMaxAbsValue = function(columns=NULL) {
    if (is.null(self$records) || inherits(self, "Set")) {
      return(NA)
    }

    columns = private$.checkColumnsArgument(columns)

    max_abs_vals = unlist(lapply(columns, function(c) {
      if (!is.null(self$records) && is.null(self$records[[c]])) {
        if (inherits(self, "Parameter")) return(0)
        default_value = self$.getDefaultValues(columns=c)
        return(abs(default_value))
      }
      else {
        return(private$.getMetric(c, "maxAbs"))
      }
    }), use.names =  FALSE)

    return(max(abs(max_abs_vals)))
  },

  #' @description whereMax find the row number in records data frame with a 
  #' maximum value (return first instance only)
  #' @param columns columns over which one wants to find the 
  #' domain entry of records with a maximum value.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  whereMax = function(column=NULL) {
    if (is.null(self$records) || inherits(self, "Set")) {
      return(NA)
    }

    column = private$.checkColumnsArgument(column)

    if (length(column) > 1) {
      stop("At most one `column` can be specified\n")
    }

    if (!is.null(self$records) && is.null(self$records[[column]])) {
      return(1)
    }
    else {
      return(private$.whereMetric(column, "max"))
    }
  },

  #' @description whereMaxAbs find the row number in records data frame 
  #' with a maximum absolute value (return first instance only)
  #' @description whereMax find the domain entry of records with a 
  #' maximum absolute value (return first instance only)
  #' @param columns columns over which one wants to find the 
  #' domain entry of records with a maximum value.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  whereMaxAbs = function(column=NULL) {
    if (is.null(self$records) || inherits(self, "Set")) {
      return(NA)
    }

    column = private$.checkColumnsArgument(column)

    if (length(column) > 1) {
      stop("At most one `column` can be specified\n")
    }

    if (!is.null(self$records) && is.null(self$records[[column]])) {
      return(1)
    }
    else {
      return(private$.whereMetric(column, "maxAbs"))
    }
  },

  #' @description whereMin find the the row number in records data frame 
  #' with a minimum value (return first instance only)
  #' @description whereMax find the domain entry of records with a 
  #' minimum value (return first instance only)
  #' @param columns columns over which one wants to find the 
  #' domain entry of records with a maximum value.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  whereMin = function(column=NULL) {
    if (is.null(self$records) || inherits(self, "Set")) {
      return(NA)
    }

    column = private$.checkColumnsArgument(column)

    if (length(column) > 1) {
      stop("At most one `column` can be specified\n")
    }

    if (!is.null(self$records) && is.null(self$records[[column]])) {
      return(1)
    }
    else {
      return(private$.whereMetric(column, "min"))
    }
  },

  #'@description countNA total number of SpecialValues[["NA"]] in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues[["NA"]].
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  countNA = function(columns=NULL) {
    if (is.null(self$records) || inherits(self, "Set")) {
      return(NA)
    }
    columns = private$.checkColumnsArgument(columns)

    return(private$.countSpecialValue(columns, "isNA"))
  },

  #' @description countEps total number of SpecialValues$EPS in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues$EPS.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  countEps = function(columns=NULL) {
    if (is.null(self$records) || inherits(self, "Set")) {
      return(NA)
    }
    columns = private$.checkColumnsArgument(columns)

    return(private$.countSpecialValue(columns, "isEps"))
  },

  #'@description countUndef total number of SpecialValues$UNDEF in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues$UNDEF.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  countUndef = function(columns=NULL) {
    if (is.null(self$records) || inherits(self, "Set")) {
      return(NA)
    }
    columns = private$.checkColumnsArgument(columns)

    return(private$.countSpecialValue(columns, "isUndef"))
  },

  #'@description countPosInf total number of 
  #' SpecialValues$POSINF in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues$POSINF.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.  
  countPosInf = function(columns=NULL) {
    if (is.null(self$records) || inherits(self, "Set")) {
      return(NA)
    }
    columns = private$.checkColumnsArgument(columns)

    return(private$.countSpecialValue(columns, "isPosInf"))

  },

  #'@description countNegInf total number of 
  #' SpecialValues$NEGINF in value column
  #' @param columns columns in which one wants to count the number of 
  #' SpecialValues$NEGINF.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.  
  countNegInf = function(columns=NULL) {
    if (is.null(self$records) || inherits(self, "Set")) {
      return(NA)
    }
    columns = private$.checkColumnsArgument(columns)

    return(private$.countSpecialValue(columns, "isNegInf"))
  },

  getUELs = function(dimension=NULL, codes=NULL, ignoreUnused = FALSE) {
    if (self$dimension == 0) return(c())
    if (is.null(dimension)) {
      if (!is.null(codes)) {
        stop("User must specify `dimension` if retrieving UELs with the ",
        "`codes` argument\n")
      }
      dimension = 1:self$dimension
    }

    if (!(is.integer(dimension) || is.numeric(dimension)) || 
    !all(dimension %% 1 == 0) || 
    any(dimension < 1) || any(dimension > self$dimension)) {
      stop(paste0("All elements of the argument ", 
      " `dimension` must be integers in [1, ", 
      self$dimension, "]\n"))
    }

    if (!is.logical(ignoreUnused)){
      stop("The argument `ignoreUnused` must be type logical\n")
    }

    if (!is.null(codes) && (!(is.numeric(codes) || is.integer(codes)) || 
    !all(codes %% 1 == 0) || !all(codes >= 1))) {
      stop(paste0("The argument `codes` must be integers ", 
      " or a vector of integers >= 1\n"))
    }

    if (!self$isValid()) {
      stop("The symbol must be valid in order to manage UELs\n")
    }

    uels = unlist(lapply(dimension, function(d) {
      if (ignoreUnused) {
        uels_d = levels(droplevels(self$records[, d]))
      }
      else {
        uels_d = levels(self$records[, d])
      }

      if (!is.null(codes)) {
        uels_d = uels_d[codes]
      }
      return(uels_d)
    }), use.names = FALSE)

    return(unique(uels))
  },

  setUELs = function(uels, dimension = NULL, rename=FALSE) {
    if (is.null(dimension)) {
      dimension = 1:self$dimension
    }
    # input check
    if (!(is.integer(dimension) || is.numeric(dimension)) || 
    !all(dimension %% 1 == 0) || 
    any(dimension < 1) || any(dimension > self$dimension)) {
      stop(paste0("All elements of the argument ",
      "`dim` must be integers in [1, ", 
      self$dimension, "]\n"))
    }

    if (!is.character(uels)) {
      stop("The argument uels must be type `character` \n")
    }

    if (!is.logical(rename)) {
      stop("The argument `rename` must be type logical")
    }

    if (!self$isValid()) {
      stop("The symbol has to be valid to set UELs \n")
    }

    # remove trailing whitespaces from uels
    uels = trimws(uels, which="right")

    for (d in dimension) {
      if (rename) {
        levels(private$.records[, d]) = uels
      }
      else {
        private$.records[, d] = 
        factor(as.character(private$.records[, d]), levels=uels, 
        ordered = TRUE)
      }
    }

  },

  reorderUELs = function(uels = NULL, dimension = NULL) {
    # input check
    if (is.null(dimension)) dimension =1:self$dimension

    if (!(is.integer(dimension) || is.numeric(dimension)) || 
    !all(dimension %% 1 == 0) || 
    any(dimension < 1) || any(dimension > self$dimension)) {
      stop(paste0("All elements of the argument ",
      "`dim` must be integers in [1, ", 
      self$dimension, "]\n"))
    }

    if (!(is.null(uels) || is.character(uels))) {
      stop("The argument `uels` must be type `character` \n")
    }

    if (!self$isValid()) {
      stop("The symbol has to be valid to reorder UELs \n")
    }


    for (d in dimension) {
      if (is.null(uels)) {
        unique_used = unique(private$.records[, d])
        unused_uels = setdiff(levels(private$.records[, d]), unique_used)
        uels = c(as.character(unique_used), unused_uels)
      }
      else {
        if ((length(uels) != length(levels(private$.records[, d])))) {
          stop(paste0("The argument `uels` must ",
          "contain all uels that need to be reordered"))
        }
        else {
          if (length(setdiff(uels, levels(private$.records[,d]))) != 0) {
            stop(paste0("The argument `uels` must ",
            "contain all uels that need to be reordered"))
          }
        }
      }
      private$.records[, d] = factor(private$.records[, d], levels=uels)
    }
  },

  addUELs = function(uels, dimension=NULL) {
    if (is.null(dimension)) dimension =1:self$dimension

    # input check
    if (!(is.integer(dimension) || is.numeric(dimension)) || 
    !all(dimension %% 1 == 0) || 
    any(dimension < 1) || any(dimension > self$dimension)) {
      stop(paste0("All elements of the argument ",
      "`dim` must be integers in [1, ", 
      self$dimension, "]\n"))
    }

    if (!is.character(uels)) {
      stop("The argument uels must be type `character` \n")
    }

    if (!self$isValid()) {
      stop("The symbol has to be valid to add UELs \n")
    }

    # remove trailing whitespaces from uels
    uels = trimws(uels, which="right")

    for (d in dimension) {

      if (length(setdiff(uels, private$.records[,d])) == 0) {
        stop("The argument `uels` should not ",
        "contain existing uels")
      }

      private$.records[, d] = factor(private$.records[, d], 
      levels=append(levels(private$.records[, d]), uels))
    }
  },

  removeUELs = function(uels=NULL, dimension=NULL) {
    if (!is.null(dimension)) {
      # input check
      if (!(is.integer(dimension) || is.numeric(dimension)) || 
      !all(dimension %% 1 == 0) || 
      any(dimension < 1) || any(dimension > self$dimension)) {
        stop(paste0("All elements of the argument ",
        "`dim` must be integers in [1, ", 
        self$dimension, "]\n"))
      }
    }
    else {
      dimension = 1:self$dimension
    }

    if (!is.null(uels)) {
      if (!is.character(uels)) {
        stop("The argument `uels`` must be type `character` \n")
      }
    }

    if (!self$isValid()) {
      stop("The symbol has to be valid to remove UELs \n")
    }

    for (d in dimension) {
      if (!is.null(uels)) {
        # remove from values and from levels
        # private$.records[, d] = 
        # (private$.records[, d])[private$.records[, d] != uels]
       private$.records[, d] = 
        factor(private$.records[, d], 
        levels = setdiff(levels(private$.records[, d]), uels))
      }
      else {
        # remove unused levels
        private$.records[, d] = droplevels(private$.records[, d])
      }
    }
  },

  renameUELs = function(uels, dimension=NULL, allowMerge=FALSE) {
    if (!is.null(dimension)) {
      # input check
      if (!(is.integer(dimension) || is.numeric(dimension)) || 
      !all(dimension %% 1 == 0) || 
      any(dimension < 1) || any(dimension > self$dimension)) {
        stop(paste0("All elements of the argument ",
        "`dim` must be integers in [1, ", 
        self$dimension, "]\n"))
      }
    }
    else {
      dimension = 1:self$dimension
    }

    if (!is.logical(allowMerge)) {
      stop("The argument `allowMerge` must be type logical\n")
    }

    if (!self$isValid()) {
      stop("The symbol has to be valid to add UELs \n")
    }

    if (!is.character(uels)) {
      stop("The argument uels must be type `character` \n")
    }

    # remove trailing whitespaces from uels
    uels = trimws(uels, which="right")

    # for list input add names
    if (is.null(names(uels))) {
      lapply(dimension, function(d) {
        if (length(levels(private$.records[, d])) != length(uels)) {
          stop(paste0("User passed a vector of length ", length(uels), 
          " which does not match the length of existing uels: ", 
          length(levels(private$.records[, d])), "\n"))
        }

        if (allowMerge == TRUE) {
          levels(private$.records[, d]) = unique(uels)
        }
        else {
          # make sure that the integer mapping is unaltered

          if (any(duplicated(uels) == TRUE)) {
            stop("Multiple UELs cannot be renamed to a UEL. ",
            "Use `allowMerge=TRUE`\n")
          }

          if (length(intersect(levels(private$.records[, d]), uels)) != 0) {
            stop("UEL cannot be renamed to an existing UEL. 
            Use `allowMerge=TRUE`.\n")
          }

          levels(private$.records[, d]) = uels
        }
      })
    }
    else {
      # user has provided a UEL map named vector
      # no duplicate keys
      if (any(duplicated(names(uels)) == TRUE)) {
        stop("A UEL cannot be renamed more than once in a single call. 
        names(uels) must be unique")
      }
      if (allowMerge == TRUE) {
        # user has provided uelmap
        old_uels = names(uels)

        lapply(dimension, function(d) {
          # get current levels
          cur_uels = levels(private$.records[, d])
          new_uels = cur_uels

          idx = match(old_uels, cur_uels)
          isna_idx = is.na(idx)
          idx = idx[!isna_idx]
          new_uels[idx] = uels[!isna_idx]

          # set current levels
          levels(private$.records[, d]) = new_uels
        })
      }
      else {
        # user has provided uelmap
        old_uels = names(uels)

        lapply(dimension, function(d) {

          # get current levels
          cur_uels = levels(private$.records[, d])
          new_uels = cur_uels

          idx = match(old_uels, cur_uels)
          isna_idx = is.na(idx)
          idx = idx[!isna_idx]
          new_uels[idx] = uels[!isna_idx]


          # don't allow more than one uels to be mapped to a same uel
          if (any(duplicated(new_uels[idx]) == TRUE)) {
            stop("Multiple UELs cannot be renamed to a UEL. Use `allowMerge=TRUE`\n")
          }

          # a uel cannot be mapped to an existing uel
          if (length(intersect(levels(private$.records[, d]), new_uels[idx])) != 0) {
            stop("UEL cannot be renamed to an existing UEL. Use `allowMerge=TRUE`.\n")
          }

          # set current levels
          levels(private$.records[, d]) = new_uels
        })
      }
    }
  },

  getDomainViolations = function() {
    if (!self$isValid()) {
      stop("The object must be valid to get domain violations\n")
    }
    if (self$dimension == 0 || is.null(self$records)) return()

    it_vec = 1:self$dimension
    is_set_alias = unlist(lapply(it_vec, function(x) {
      inherits(self$domain[[x]], c("Set", ".BaseAlias"))
    }), use.names = FALSE)
    it_vec = it_vec[is_set_alias]

    added_uel_all = lapply(it_vec, function(d) {
      setdiff(tolower(self$getUELs(d, ignoreUnused=TRUE)), 
      tolower(self$domain[[d]]$getUELs(ignoreUnused=TRUE)))
    })

    length_added_uel = unlist(lapply(added_uel_all, length), use.names = FALSE)
    it_vec = it_vec[length_added_uel > 0]

    dom_violations = lapply(it_vec, function(d) {
      DomainViolation$new(self, d, self$domain[[d]], added_uel_all[[d]])
    })

    if (length(dom_violations) == 0) return(invisible(NULL))

    return(dom_violations)
  },

  findDomainViolations = function() {
    violations = self$getDomainViolations()

    if (is.null(violations)) return(data.frame())

    idx = lapply(violations, function(dv) {
      set_dv = unique(dv$violations)

      idx = lapply(set_dv, function(v) {
        return(which(self$records[, dv$dimension] == v, arr.ind = TRUE))
      })
      return(unlist(idx, use.names=FALSE))
    })

    return(self$records[unlist(unique(idx)), , drop=FALSE])
  },

  hasDomainViolations = function() {
    df = self$findDomainViolations()
    if ((nrow(df) == 0) && (length(df) == 0)) {
      return(FALSE)
    }
    else {
      return(TRUE)
    }
  },

  countDomainViolations = function() {
    df = self$findDomainViolations()
    return(nrow(df))
  },

  dropDomainViolations = function() {
    violations = self$getDomainViolations()

    if (is.null(violations)) return()

    idx = lapply(violations, function(dv) {
      set_dv = unique(dv$violations)

      idx = lapply(set_dv, function(v) {
        return(which(self$records[, dv$dimension] == v, arr.ind = TRUE))
      })
      return(unlist(idx, use.names=FALSE))
    })
    private$.records = private$.records[-unlist(unique(idx)), , drop=FALSE]
    rownames(private$.records) <- NULL
    return(invisible(NULL))
  },

  countDuplicateRecords = function() {
    return(nrow(self$findDuplicateRecords()))
  },

  findDuplicateRecords = function(keep="first") {
    idx = private$.get_duplicate_index(keep)
    if (is.integer0(idx)) {
      return(data.frame())
    }
    else {
      return(self$records[idx, , drop = FALSE])
    }
  },

  hasDuplicateRecords = function() {
    return(self$countDuplicateRecords() > 0)
  },

  dropDuplicateRecords = function(keep = "first") {
    idx = private$.get_duplicate_index(keep)

    if (!is.integer0(idx)) {
      self$records = self$records[-idx, , drop=FALSE]
      rownames(self$records) <- NULL
    }
    return(invisible(NULL))
  },

  #' @description getSparsity get the sparsity of the symbol w.r.t the cardinality
  getSparsity = function() {
    tryCatch(
      {
        if (self$domainType == "relaxed" | self$domainType == "none"){
          return(NA)
        }
        else {
          dense = 1
          for (i in self$domain) {
            dense = dense * i$numberRecords
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

  shape = function() {
    if (self$domainType == "regular") {
      shapelist = c()
      for (d in self$domain) {
        shapelist = append(shapelist, nrow(d$records))
      }
      return(shapelist)
    }

    if (!is.null(self$records)) {
      if (self$dimension == 0) {
        return(c())
      }

      if (self$domainType == "none" || self$domainType == "relaxed") {
        shapelist = c()
        for (i in (1:self$dimension)) {
          shapelist = append(shapelist, length(unique(self$records[, i])))
        }
        return(shapelist)
      }
    }
    else {
      return(NULL)
    }
  },

  #' @description toDense convert symbol to a dense matrix/array format
  #' @param column column to be converted to dense format.
  #' This is an optional argument which defaults to `value` for parameter
  #'  and `level` for variable and equation. For variables and equations, 
  #' alternate column/columns can be provided using the columns argument.
  toDense = function(column = "level") {
    if (!is.character(column)) {
      stop("Argument 'column' must be type str\n")
    }
    if (inherits(self, "Parameter")) {
      column = "value"
    }
    else {
      if (!any(private$.attr() == column)) {
        stop(paste0("Argument 'column' must be one ",
        "of the following: ", toString(private$.attr()), "\n"))
      }
    }

    if (self$isValid() == FALSE) {
      stop("Cannot create dense array (i.e., matrix) format because symbol ",
      "is invalid -- use $isValid(verbose=TRUE) to debug symbol state.\n")
    }

    if (is.null(self$records)) return(NULL)

    if (self$dimension  == 0) {
      if (is.null(self$records[[column]])) {
        if (inherits(self, "Parameter")) {
          def_value = self$.getDefaultValues()
        }
        else {
          def_value = self$.getDefaultValues(columns=column)
        }
        return(def_value)
      }
      else {
        return(self$records[[column]])
      }
    }

    if (self$domainType == "regular") {
      if (self$hasDomainViolations()) {
        stop(paste0("Cannot create dense array because there are",
        " domain violations i.e., the UELs in the symbol"),
          " are not a subset of UELs in the domain set/s\n")
      }

      # check if the symbol has unused levels
      has_unused = unlist(lapply(1:self$dimension, function(d) {
        unique_recs = unique(as.character(self$domain[[d]]$records[ ,1]))
        all_levels = levels(self$domain[[d]]$records[ ,1])
        # unused uels at the end are okay.
        diff = setdiff(all_levels[1:length(unique_recs)], unique_recs)
        return(length(diff) != 0)
      }), use.names = FALSE)

      if (any(has_unused)) {
        dim = which(has_unused == TRUE)[1]
        stop(paste0("Cannot create dense array because there ",
        "are unused UELs in the domain symbol ",
          self$domain[[dim]]$name, ". Use ", 
          self$domain[[dim]]$name, "$removeUELs()", 
          " to remove the unused UELs or ",  self$domain[[dim]]$name, 
          "$reorderUELs() to move them to the end.\n"))
      }

      # check if the order of the uels is the same as records
      is_unsorted = unlist(lapply(1:self$dimension, function(d) {
        return(is.unsorted(as.integer(self$domain[[d]]$records[ ,1])))
      }), use.names = FALSE)

      if (any(is_unsorted)) {
        dim = which(is_unsorted == TRUE)[1]
        stop(paste0("Cannot create dense array because the order ", 
        "of the symbol UELs for the domain symbol ", 
        self$domain[[dim]]$name, " is not the same ",
        "as that of symbol records. Use ", 
        self$domain[[dim]]$name, "$reorderUELs() to ",
        "reorder the UELs according to the records\n"))
      }

      idx = lapply(1:self$dimension, function(d) {
        return(as.numeric(factor(self$records[,d], 
        levels = self$domain[[d]]$records[, 1])) )
      })

    }
    else {
      idx = lapply(1:self$dimension, function(d) {
        return(as.numeric(factor(self$records[,d], 
        levels = levels(self$records[, d]))) )
      })
    }

    a = array(0, dim = self$shape())
    if (is.null(self$records[[column]])) {
      if (inherits(self, "Parameter")) {
        def_value = self$.getDefaultValues()
      }
      else {
        def_value = self$.getDefaultValues(columns=column)
      }
      a[matrix(unlist(idx), ncol=length(idx))] = def_value
    }
    else {
      a[matrix(unlist(idx), ncol=length(idx))] = self$records[, column]
    }
    return(a)

  },

  equals = function(other, columns=NULL, checkUELs=TRUE, 
  checkElementText=TRUE, checkMetaData=TRUE, rtol=NULL, atol=NULL,
  verbose=FALSE) {
    if (inherits(other, "Alias")) {
      other = other$aliasWith
    }

    tryCatch(
      {
        private$.check_equal(other, columns, checkUELs, 
        checkElementText, checkMetaData, rtol, atol)
        return(TRUE)
      },
      error = function(e) {
        if (verbose == TRUE) {
          message(e)
        }
        return(FALSE)
      }
    )
  },

  .linkDomainCategories = function() {
      private$.records[, 1:self$dimension] = lapply(1:self$dimension, function(n) {
        i  = self$domain[[n]]
        return(factor(private$.records[, n], 
        levels = levels(i$records[, 1]), ordered = TRUE))
      })
  },

  copy = function(destination = NULL, overwrite = FALSE) {
    private$.copy(destination, overwrite)
    return(invisible(NULL))
  }
  ),

  active = list(

    records = function(records_input) {
      if (missing(records_input)) {
        return(private$.records)
      }
      else {
        private$.records = records_input
        self$.requiresStateCheck = TRUE
        self$refContainer$.requiresStateCheck = TRUE

        if (!is.null(private$.records)) {
          if (any(self$domainForwarding == TRUE)) {
            private$domain_forwarding(self$domainForwarding)

            for (i in self$refContainer$listSymbols()) {
              self$refContainer[i]$.requiresStateCheck = TRUE
            }

            self$refContainer$.requiresStateCheck = TRUE
          }
        }
      }
    },

    domainForwarding = function(domain_forwarding_input) {
      if (missing(domain_forwarding_input)) {
        return(private$.domain_forwarding)
      }
      else {
        if (!is.logical(domain_forwarding_input)) {
          stop("Argument 'domainForwarding' must be type logical\n")

          if (!any(c(1, self$dimension) == length(domain_forwarding) )) {
            stop("The argument `domainForwarding` must be of length 1 or <symbol>$dimension \n")
          }
        }
        else {
          private$.domain_forwarding = domain_forwarding_input
        }
      }
    },

    description = function(description_input) {
      if (missing(description_input)) {
        return(private$.description)
      }
      else {
        if (!is.character(description_input)) {
          stop("Symbol 'description' must be type character\n")
        }

        if (length(description_input) != 1) {
          stop(paste0("Symbol `description` cannot be a",
          " character vector of length greater than 1\n"))
        }

        if (nchar(description_input) > gams_description_max_length) {
          stop(paste0("Symbol 'description' must have length ",
          gams_description_max_length, " or smaller\n"))
        }

        if (!is.null(private$.description)) {
          if (private$.description != description_input) {
            self$.requiresStateCheck = TRUE
            private$.ref_container$.requiresStateCheck = TRUE
          }
        }
        private$.description = description_input
      }
    },

    dimension = function(dimension_input) {
      if (missing(dimension_input)) {
        return(length(self$domain))
      }
      else {
        if (!((inherits(dimension_input, c("numeric", "integer"))) && 
           (dimension_input %% 1 == 0) && (dimension_input >= 0) &&
           (dimension_input <= .gdxSymbolTypes()[["GMS_MAX_INDEX_DIM"]]))) {
            stop(paste0("Symbol 'dimension' must be ",
           "an integer in [0, ", .gdxSymbolTypes()[["GMS_MAX_INDEX_DIM"]], "]\n"))
           }

        if (length(self$domain) > dimension_input) {
          if (dimension_input == 0) {
            self$domain = list()
          }
          else {
            self$domain = self$domain[1:dimension_input]
          }
        }
        else if (length(self$domain) < dimension_input) {
           new = self$domain
           new = append(new, replicate(dimension_input - 
           length(self$domain), "*"))
           self$domain = new
        }
        else {
        }
      }
    },

    domain = function(domain_input) {

      if (missing(domain_input)) {
        return(private$.domain)
      }
      else {
        if (is.null(domain_input)) {
          domain_input = list()
        }

        if (!(is.list(domain_input) || is.vector(domain_input))) {
          domain_input = list(domain_input)
        }

        if (length(domain_input) > .gdxSymbolTypes()[["GMS_MAX_INDEX_DIM"]]) {
          stop(paste0("Argument 'domain' length cannot be > ", 
          .gdxSymbolTypes()[["GMS_MAX_INDEX_DIM"]], "\n"))
        }

        lapply(domain_input, function(d) {
          if (!((inherits(d, c("Set", ".BaseAlias")) && d$dimension == 1)
                || is.character(d))) {
            stop("All 'domain' elements must be either one dimensional ", 
                 "Set/Alias/UniverseAlias, or must be type Character\n")
          }
        }
        )

        # check change of domain
        if (!is.null(private$.domain)) {
          if (!identical(private$.domain, domain_input)) {
              self$.requiresStateCheck = TRUE
              private$.ref_container$.requiresStateCheck = TRUE
          }
        }

        private$.domain = domain_input

      }
    },

    refContainer = function(ref_container_input) {
      if (missing(ref_container_input)) {
        return(private$.ref_container)
      }
      else {
        if (is.null(ref_container_input)) {
          private$.ref_container = NULL
          self$.requiresStateCheck = TRUE
          return()
        }

        if (!inherits(ref_container_input, "Container")) {
          stop("Symbol 'container' must be type Container\n")
        }
        if (!is.null(private$.ref_container)){
          if (!identical(private$.ref_container, ref_container_input)) {
            # set flag for old container
            private$.ref_container$.requiresStateCheck = TRUE
            self$.requiresStateCheck = TRUE
          }
        }
        #assign
        private$.ref_container = ref_container_input
        # set flag for new container
        ref_container_input$.requiresStateCheck = TRUE
        self$.requiresStateCheck = TRUE
      }
    },
    name = function(name_input) {
      if (missing(name_input)) {
        return(private$.name)
      }
      else {
        if (!is.character(name_input)) {
          stop("GAMS symbol 'name' must be type chracter\n")
        }

        if (nchar(name_input) > private$symbolMaxLength) {
          stop(paste0("GAMS symbol 'name' is too long,",
          " max is ", private$symbolMaxLength, " characters\n"))
        }

        if (private$.ref_container$.lc_data$has(name_input)) {
          stop(paste0("A symbol with the name ", name_input, 
            " already exists in the container\n"))
        }

        if (substr(name_input, 1, 1) == "_") {
          stop("Valid GAMS names cannot begin with a `_`character.\n")
        }

        if (grepl("^[a-zA-Z0-9_]+$", name_input) == FALSE) {
          stop("Detected an invalid GAMS symbol name. GAMS names can only ",
          "contain alphanumeric characters (letters and numbers) and ",
          "the `_` character.\n")
        }

        if (is.null(private$.name)) {
          self$.requiresStateCheck = TRUE
          private$.name = name_input
        }
        else {
          if (private$.name != name_input) {
            self$.requiresStateCheck = TRUE

            refcontainer = private$.ref_container

            refcontainer[name_input] = refcontainer[private$.name]
            refcontainer$data$remove(private$.name)
            refcontainer$.lc_data$remove(tolower(private$.name))
          }
          private$.name = name_input
        }
      }
    },

    numberRecords = function() {
      if (self$isValid() == TRUE) {
        if (!is.null(self$records)) {
          if (self$dimension == 0) {
            return(1)
          }
          else {
            return(nrow(self$records))
          }
        }
        else {
          return(0)
        }
      }
      else {
        return(NA)
      }
    },

    domainType = function() {
      regularCheck = unlist(lapply(self$domain, function(d) {
        return(inherits(d, c("Set", ".BaseAlias")))
       }), use.names = FALSE)

      if (all(regularCheck == TRUE) && self$dimension != 0) {
          return("regular")
      }
      else if (all(self$domain == "*")) {
        return("none")
      }
      else if (self$dimension == 0) {
        return("none")
      }
      else {
        return("relaxed")
      }
    },

    domainNames = function() {
      if (self$dimension == 0) return(NA)

      d = unlist(lapply(self$domain, function(i) {
        if (inherits(i, c("Set", ".BaseAlias"))) {
          return(i$name)
        }
        else {
          return(i)
        }
      }), use.names = FALSE)
      return(d)
    },

    domainLabels = function(domain_label_input) {
      if (missing(domain_label_input)) {
        if (self$dimension == 0) return(NULL)

        return(colnames(self$records)[1:self$dimension])
      }
      else {
        if (length(domain_label_input) != self$dimension) {
          stop(paste0("Length of `domainLabels` (", length(domain_label_input),
          ") not equal to symbol dimension (", self$dimension, ").\n"))
        }

        dup_labels = duplicated(domain_label_input)
        if (!any(dup_labels)) {
          colnames(self$records) = domain_label_input
        }
        else {
          domain_label_input = paste0(domain_label_input, 1:self$dimension)
        }
        colnames(self$records) = domain_label_input
      }
    }

  ),

  private = list(
    .domain_forwarding = NULL,
    .description = NULL,
    .domain = NULL,
    .ref_container = NULL,
    .name = NULL,
    .records = NULL,
    symbolMaxLength = 63,
    descriptionMaxLength = 255,

    .getMetric = function(columns, metric) {
      tryCatch(
        {
          if (metric == "max") {
            return(max(self$records[,columns]))
          }
          else if (metric == "min") {
            return(min(self$records[, columns]))
          }
          else if (metric == "mean") {
            return(mean(self$records[,columns]))
          }
          else if (metric == "maxAbs") {
            return(max(abs(self$records[,columns])))
          }
        },
        error = function(cond) return(NA),
        warning = function(cond) return(NA)
      )
    },

    .whereMetric = function(column, metric) {
      tryCatch(
        {
          if (metric == "min") {
            whereMetricVal = which.min(self$records[,column])
          }
          else if (metric == "max") {
            whereMetricVal = which.max(self$records[,column])
          }
          else if (metric == "maxAbs") {
            whereMetricVal = which.max(abs(self$records[,column]))
          }

          if (is.integer0(whereMetricVal)) {
            return(NA)
          }
          else {
            return(whereMetricVal)
          }
        },
        error = function(cond) return(NA),
        warning = function(cond) return(NA)
      )
    },

    .countSpecialValue = function(columns, specialValueFunc) {

      tryCatch(
        {
          special_val_count = unlist(lapply(columns, function(c) {
            if (is.null(self$records[[c]])) {
              if (inherits(self, "Parameter")) return(0)

              if (SpecialValues[[specialValueFunc]](self$.getDefaultValues(columns=c))) {
                return(self$numberRecords)
              }
              else {
                return(0)
              }
            }
            else {
              return(SpecialValues[[specialValueFunc]](self$records[,c]))
            }
          }), use.names = FALSE)
          return(sum(special_val_count))
        },
        error = function(cond)  return(NA),
        warning = function(cond) return(NA)
      )
    },

    .checkColumnsArgument = function(columns) {
      if (inherits(self, "Parameter")) {
        columns = "value"
      }
      else {
        if (!is.null(columns)) {
          if (!is.character(columns)) {
            stop("The argument `columns` must be type character\n")
          }

          diff = setdiff(columns, private$.attr())
          if (length(diff) != 0) {
            stop(paste0("User entered columns (", toString(columns), 
            ") must be a subset of valid numeric columns ", 
            toString(private$.attr()), "\n"))
          }
        }
        else {
          columns = "level"
        }
      }
      return(columns)
    },

    .get_default_value = function(column) {
      if (inherits(self, "Parameter")) {
        return(0)
      }
      else {
        if (inherits(self, "Variable")) {
          return(.variable_default_values[[self$type]][[column]])
        }
      }
    },

    .attr = function() {
      return(c("level", "marginal", "lower", "upper", "scale"))
    },

    .get_default_domain_labels = function() {
      if (self$dimension == 0) return(c())

      domain_label_input = self$domainNames
      domain_label_input[domain_label_input == "*"] = "uni"
      dup_labels = duplicated(domain_label_input)
      if (any(dup_labels)) {
        domain_label_input = paste0(domain_label_input, "_", 1:self$dimension)
      }
      return(domain_label_input)
    },

    .generate_records_index = function(density) {
      if (!(is.numeric(density)) && all(density >= 0 && density <= 1)) {
        stop("The argument `density` must be numeric in the range [0, 1]\n")
      }

      if (!any(c(1,self$dimension) == length(density))) {
        stop("The argument `density` must be of length: ", 
        self$dimension, " or 1, the user provided: ", length(density), "\n")
      }

      # get the full cartesian product
      dom_recs = lapply(self$domain, function(d) return(d$records[,1]))
      length_dom_recs = unlist(lapply(dom_recs, function(x) {return(length(x))}), use.names=FALSE)
      final_nrecs = floor(density * length_dom_recs)
      if (any(final_nrecs == 0)) return(data.frame())

      if (length(density) == 1) {
        # if the length is 1 then apply density on records dataframe instead

        # drop unused levels from a set
        dom_recs = lapply(dom_recs, function(x) return(droplevels(x)))

        # cartesian product
        recs = expand.grid(dom_recs)
        colnames(recs) = private$.get_default_domain_labels()

        # sample indices based on density
        idx = sample(1:nrow(recs), floor(density * nrow(recs)), replace = FALSE)

        # drop rows
        recs = recs[sort(idx), 1:length(recs), drop = FALSE]

        # drop unused levels
        recs = droplevels(recs)
      }
      else {
        rec_idx = lapply(1:length(dom_recs), function(i) { 
          rec = dom_recs[[i]]
          idx = sample(1:length(rec), floor(density[i] * length(rec)), replace = FALSE)
          return(rec[sort(idx)])
        })

        dom_recs = rec_idx
        dom_recs = lapply(dom_recs, function(x) return(droplevels(x)))

        recs = expand.grid(dom_recs)
        colnames(recs) = private$.get_default_domain_labels()
      }

      #reset row indices
      rownames(recs) <- NULL
      return(recs)
    },
    .check_equal = function(other, columns= NULL, checkUELs=TRUE, 
      checkElementText=TRUE, checkMetaData=TRUE, rtol=NULL, atol=NULL) {

      if (self$dimension != other$dimension) {
        stop(paste0("Symbol dimension do not match ", self$dimension, 
        " != ", other$dimension, "\n"))
      }

      if (self$domainType != other$domainType) {
        stop(paste0("Symbol domain types do not match `", self$domainType, 
        "`` != `", other$domainType, "`\n"))
      }

      if (self$dimension != 0 && self$domainType == "regular") {
        for (d in 1:self$dimension) {
          if (inherits(self$domain[[d]], ".Symbol")) {
            if (!self$domain[[d]]$equals(other$domain[[d]])) {
              stop(paste0("Symbol domains for dimension ", d ,
              " do not match.\n"))
            }
          }
        }
      }

      if (self$numberRecords != other$numberRecords) {
        stop(paste0("Symbols do not have same number of records ", 
        self$numberRecords, " != ", other$numberRecords, "\n"))
      }

      if (self$dimension != 0) {
        if (any(self$domainLabels != other$domainLabels)) {
          stop(paste0("Symbols domain labels do not match ", 
          toString(self$domainLabels), " != ", 
          toString(other$domainLabels), "\n"))
        }
      }

      # check metadata
      if (checkMetaData) {
        if (self$name != other$name) {
          stop("Symbol names do not match ", 
          self$name, " != ", other$name, "\n" )
        }

        if (self$description != other$description) {
          stop("Symbol descriptions do not match ", 
          self$description, " != ", other$description, "\n" )
        }

        if (class(self)[1] != class(other)[1]) {
          stop("Symbol types do not match ", 
          class(self)[1], " != ", class(other)[1], "\n" )
        }
      }

      # check UELs
      if (checkUELs) {
        if (self$numberRecords != 0) {
          selfUELs = self$getUELs()
          otherUELs = other$getUELs()
          if (!all(selfUELs == otherUELs)) {
            stop(paste0("Symbol UELs do not match \n",
            "self: ", toString(selfUELs), "\n",
            "other: ", toString(otherUELs), "\n"))
          }
        }
      }

      if (inherits(self, c("Set", "Alias"))) {
        private$.check_set_records_equal(other, checkElementText)
      }
      else if (inherits(self, c("Parameter", "Variable", 
      "Equation"))) {

        private$.check_numeric_records_equal(other, columns, rtol, atol)
      }
    },

    .check_equals_common_args = function(other, checkUELs, checkMetaData, verbose) {
      # mandatory checks
      if (!self$isValid()) {
        stop(paste0("Cannot compare objects because ", s$name, 
        " is not valid. Use ", s$name,
        "$isValid(verbose=TRUE) to get more details\n"))
      }

      if (!inherits(other, c(".Symbol", ".BaseAlias"))) {
        stop("The argument `other` must be a Symbol object")
      }

      if (!other$isValid()) {
        stop(paste0("Cannot compare objects because ", other$name, 
        " is invalid. Use ", other$name, 
        "$isValid(verbose=TRUE) to debug.\n"))
      }

      if (!is.logical(checkUELs)) {
        stop("The argument `checkUELs` must be type logical")
      }

      if (!is.logical(checkMetaData)) {
        stop("The argument `checkMetaData` must be type logical")
      }

      if (!is.logical(verbose)) {
        stop("The argument `verbose` must be type logical")
      }
    },

    .check_equals_numeric_args = function(atol, rtol) {
      if (!(is.numeric(atol) && length(atol) == 1)) {
        stop("The argument `atol` must be type numeric of length 1 \n")
      }

      if (!(is.numeric(rtol) && length(rtol) == 1)) {
        stop("The argument `rtol` must be type numeric of length 1 \n")
      }
    },

    .check_set_records_equal = function(other, checkElementText) {
      if (self$numberRecords == 0) return()
      #merge both dataframes by domain column_names
      merged = merge(self$records, other$records, 
      by.x=self$domainLabels, by.y=other$domainLabels,
      all=TRUE)

      if (self$dimension + 1 < length(merged)) {
        # element text column exists
        isna_check = is.na(merged[(self$dimension+1):length(merged)])

        if (any(isna_check)) {
          error_df = head(merged[as.logical(
          rowSums(isna_check)),][1:self$dimension])
          strmsg="symbol records do not match. Unmatched rows below\n"
          strdf = paste0(capture.output(error_df), collapse="\n")
          stop(paste0(strmsg, strdf, "\n"))
        }
      }
      else {
        # no element text column
        if (nrow(merged) != nrow(self$records)) {
          # if number of records is the same in self and other
          # and if the merged dataframe has different number of records
          stop("symbol records do not match. Unmatched rows are present.\n")
        }
      }

      if (checkElementText) {
        if (is.null(merged$element_text.x)) {
          # at least one of the two dataframes doesn't have element_text column
          if (!is.null(merged$element_text)) {
            # only one data frame has element_text column
            if (!all(merged$element_text == "")) {
              stop("symbol element_text does not match.\n")
            }
          }
        }
        else {
          el_text_mismatch = (merged[, "element_text.x"] != merged[, "element_text.y"])

          if (any(el_text_mismatch)) {
            error_df = head(merged[el_text_mismatch, ])
            strmsg="symbol element_text does not match. Unmatched rows below\n"
            strdf = paste0(capture.output(error_df), collapse="\n")
            stop(paste0(strmsg, strdf, "\n"))
          }
        }
      }


    },

    .check_numeric_records_equal = function(other, columns, rtol, atol) {
      if (self$numberRecords == 0) return()

      # columns = unique(append(names(rtol), names(atol)))
      if (is.null(columns)) {
        if (inherits(self, c("Variable", "Equation"))) {
          columns = private$.attr()
        }
        else {
          #parameter
          columns = "value"
        }
      }

      if (self$dimension == 0) {
        # now compare numerical records
        for (attr in columns) {
          self_column_exists = !is.null(self$records[[attr]])
          other_column_exists = !is.null(other$records[[attr]])

          if (inherits(self, "Parameter")) {
            def_values = 0
          }
          else {
            def_values = self$.getDefaultValues(columns=attr)
          }
          if (self_column_exists && !other_column_exists) {
            if (any(self$records[[attr]] != replicate(self$numberRecords, def_values))) {
              stop(paste0("symbol records do not match. ", other$name, "$records is considered to be 
              at the default value of ", def_values, "\n"))
            }
          }
          else if (!self_column_exists && other_column_exists) {
            if (any(other$records[[attr]] != replicate(self$numberRecords, def_values))) {
              stop(paste0("symbol records do not match. ", self$name, "$records is considered to be 
              at the default value of ", def_values, "\n"))
            }
          }
          else if (!self_column_exists && !other_column_exists) {
            next
          }
          else {
            # check for special values
            count = 0
            fnames = c("EPS", "NA", "UNDEF", "POSINF", "NEGINF")
            is_special = FALSE
            for (f in c(SpecialValues$isEps, SpecialValues$isNA, 
            SpecialValues$isUndef, SpecialValues$isPosInf, 
            SpecialValues$isNegInf)) {
              count = count + 1
              is_special_self = f(self$records[, attr])
              is_special_other = f(other$records[, attr])

              if (any( is_special_self !=  is_special_other)) {
                stop(paste0("Symbols with ", fnames[count], " special values 
                do not match in the ", attr, " column.\n"))
              }
              is_special = (is_special || is_special_self)
            }
            if (is_special) next

            if (!is.null(names(atol))) {
              atol_attr = atol[[attr]]
            }
            else {
              atol_attr = atol
            }

            if (!is.null(names(rtol))) {
                rtol_attr = rtol[[attr]]
            }
            else {
              rtol_attr = rtol
            }
            # check numerical equality subject to tolerance
            lhs = abs(self$records[,attr] - other$records[, attr])
            rhs = atol_attr + rtol_attr * abs(other$records[, attr])

            if (lhs > rhs) {
              stop(paste0("Symbol records contain numeric differences in the ", 
              attr, " attribute that are outside the specified tolerances rtol=", 
              rtol_attr, ", atol=", atol_attr, "\n"))
            }
          }
        }
      }
      else {
        #merge both dataframes by column_names
        merged = merge(self$records, other$records, 
        by.x=self$domainLabels, by.y=other$domainLabels,
        all=TRUE)

        if (self$dimension + 1 < length(merged)) {
          error_df = head(merged[as.logical(
            rowSums(is.na(merged[(self$dimension+1):length(merged)]))),][1:self$dimension])

          strmsg="symbol records do not match. Unmatched rows below\n"
          strdf = paste0(capture.output(error_df), collapse="\n")
          if (any(is.na(merged[,self$dimension:length(merged)]))) {
            stop(paste0(strmsg, strdf, "\n"))
          }

        }
        else {
          # no element text column
          if (nrow(merged) != nrow(self$records)) {
            # if number of records is the same in self and other
            # and if the merged dataframe has different number of records
            stop("symbol records do not match. Unmatched rows are present.\n")
          }
        }

        # now compare numerical records
        for (attr in columns) {
          self_column_exists = !is.null(self$records[[attr]])
          other_column_exists = !is.null(other$records[[attr]])

          if (inherits(self, "Parameter")) {
            def_values = 0
          }
          else {
            def_values = self$.getDefaultValues(columns=attr)
          }

          if (self_column_exists && !other_column_exists) {
            if (any(self$records[[attr]] != replicate(self$numberRecords, def_values))) {
              stop(paste0("symbol records do not match. ", other$name, "$records is considered to be 
              at the default value of ", def_values, "\n"))
            }
          }
          else if (!self_column_exists && other_column_exists) {
            if (any(other$records[[attr]] != replicate(self$numberRecords, def_values))) {
              stop(paste0("symbol records do not match. ", self$name, "$records is considered to be 
              at the default value of ", def_values, "\n"))
            }
          }
          else if (!self_column_exists && !other_column_exists) {
            next
          }
          else {
            attrs_x = paste0(attr, ".x")
            attrs_y = paste0(attr, ".y")
            small_merged = merged[1:self$dimension]
            small_merged[, c(attrs_x, attrs_y)] = 
            merged[c(attrs_x, attrs_y)]

            # check for special values
            count = 0
            fnames = c("EPS", "NA", "UNDEF", "POSINF", "NEGINF")
            for (f in c(SpecialValues$isEps, SpecialValues$isNA, 
              SpecialValues$isUndef, SpecialValues$isPosInf, 
              SpecialValues$isNegInf)) {
              count = count + 1
              idx_self = f(small_merged[, attrs_x])
              idx_other = f(small_merged[, attrs_y])
              if (any(idx_self != idx_other)) {
                stop(paste0("Symbols with ", fnames[count], " special values ",
                "do not match in the ", attr, " column.\n"))
              }

              if (any(idx_self)) {
                # drop special values
                small_merged = small_merged[-which(idx_self),]
              }
              if (nrow(small_merged) == 0) break
            }

            if (nrow(small_merged) == 0) next

            if (!is.null(names(atol))) {
              if (!is.null(atol[[attr]])) {
                atol_attr = atol[[attr]]
              }
              else {
                stop(paste0("User passed a named vector for the ", 
                "argument `atol` but the attribute ", 
                attr, " is missing\n"))
              }
            }
            else {
              atol_attr = atol
            }

            if (!is.null(names(rtol))) {
              if (!is.null(rtol[[attr]])) {
                rtol_attr = rtol[[attr]]
              }
              else {
                stop(paste0("User passed a named vector for the argument ", 
                "`rtol` but the attribute ", attr, " is missing\n"))
              }
            }
            else {
              rtol_attr = rtol
            }

            # check numerical equality subject to tolerance
            lhs = abs(small_merged[,paste0(attr, ".x")] - 
            small_merged[, paste0(attr, ".y")])
            rhs = atol_attr + rtol_attr * abs(small_merged[, paste0(attr, ".y")])

            if (any(lhs > rhs)) {
              stop(paste0("Symbol records contain numeric differences in the ", 
              attr, " attribute that are outside the specified tolerances rtol="
              , rtol_attr, ", atol=", atol_attr, "\n"))
            }

          }
        }

      }
    },


    check = function() {
      if (self$.requiresStateCheck == TRUE) {
        # if regular domain, symbols in domain must be valid
        if (self$domainType == "regular") {
          for (i in self$domain) {
            if (!self$refContainer$hasSymbols(i$name)) {
              stop(paste0("symbol defined over domain symbol ",
              i$name, " however, the object referenced is not in the", 
              " Container anymore -- must reset domain for symbol ", 
              self$name, "\n"))

            }
            if (!identical(i, self$refContainer[i$name])) {
              stop(paste0("symbol defined over domain symbol ",
              i$name, " however, the symbol with name ", i$name, 
              " in the container is different. Seems to be a broken link.",
               "-- must reset domain for symbol ",
              self$name))
            }

            if (i$isValid() != TRUE) {
              stop(paste0("symbol defined over domain symbol ",
              i$name, " however, this object is not a valid object ",
              "in the Container -- all domain objects must be valid.\n"))
            }

            if (i$dimension != 1) {
              stop(paste0("Dimensionality of all domain symbols must be 1. ",
              "The domain symbol ", i$name, " has dimension = ", 
              i$dimension, ".\n"))
            }

            if (i$isSingleton) {
              stop(paste0("Singleton sets cannot be used as domain sets. ",
              "The domain symbol ", i$name, " is a singleton set.\n"))
            }
          }
        }
        # if records exist, check consistency
        if (!is.null(self$records)) {
          if (inherits(self, c("Set", "Parameter"))) {
            if (length(self$records) > self$dimension + 1 || length(self$records) < self$dimension) {
              stop(paste0("Symbol 'records' does not have", 
              " the correct number of columns {<symbol dimension>, <symbol dimension> + 1)}\n"))
            }
          }


          if (inherits(self, c("Variable", "Equation"))) {
            if ((length(self$records) < self$dimension) ||
            (length(self$records) >
            self$dimension + length(private$.attr()))) {
              stop(paste0("Symbol 'records' does not have", 
              " the correct number of columns [", self$dimension,
              ", ", self$dimension + length(private$.attr()), "]\n"))
            }
          }

          # check if records are dataframe
          if (!is.data.frame(self$records)){
            stop("Symbol 'records' must be type dataframe\n")
          }

          # check if scalars have only 1 record
          if (inherits(self, c("Parameter", "Variable", "Equation"))) {
            if (self$isScalar && nrow(self$records) > 1) {
              stop("Scalar symbols cannot have more than one record entry\n")
            }
          }

          # check if domainLabels are unique
          if (any(duplicated(self$domainLabels))) {
            stop("Symbol domainLabels must be unique\n")
          }

          # check column names and order
          cols = c()
          record_ncol = length(self$records)
          if (inherits(self, "Set")) {
            if (record_ncol == self$dimension + 1) {
              cols = "element_text"
            }
          }
          else if(inherits(self, "Parameter")) {
            if (record_ncol == self$dimension + 1) {
              cols = "value"
            }
          }
          else if (inherits(self, c("Variable", "Equation"))) {
            cols = private$.attr()
          }

          if (record_ncol >= self$dimension + 1) {
            colname_recs = colnames(self$records)[(self$dimension + 1):record_ncol]
            intersect_colnames = intersect(cols, colname_recs)

            if (!identical(intersect_colnames, colname_recs)) {
              stop(paste0("Records columns must be named 
              and ordered as: ", toString(cols),"\n"))
            }
          }


          # check if all data columns are float
          if (inherits(self, c("Variable", "Parameter", "Equation" ))) {
            if (length(self$records) > self$dimension ) {
              for (i in (self$dimension + 1):length(self$records)) {
                if (!(is.numeric(self$records[, i]) || 
                all(SpecialValues$isNA(self$records[, i])))) {
                  stop("Data in column ", i, " must be numeric or NA\n")
                }
              }
            }
          }

          # check if all domain columns are factors
          if (self$dimension != 0) {
            for (i in 1:self$dimension) {
              if (!is.factor(self$records[, i])) {
                stop(paste0("Domain information in column ",
                colnames(self$records)[i], "must be type factor\n"))
              }
            }
          }
        }

      }
      self$.requiresStateCheck = FALSE
    },

    domain_forwarding = function(dom_forwarding) {
    if (length(dom_forwarding) == 1) {
      dim_enabled = replicate(self$dimension, TRUE)
    }
    else {
      dim_enabled = dom_forwarding
    }

    dim_to_forward = seq_len(self$dimension)
    dim_to_forward = dim_to_forward[dim_enabled]
    # find symbols to grow
    for (diter in dim_to_forward) {
      d = self$domain[[diter]]
      to_grow = list()
      while (inherits(d, "Set")) {
        to_grow = append(to_grow, d$name)
        d = d$domain[[1]]
      }
      # reverse the to_grow list because when the records are set, we check domain
      # domain_forwarding for domain sets is FALSE until specified explicitly 
      # so we should grow parent sets first and then children
      to_grow = rev(to_grow)

      for (i in to_grow) {
        dim = (self$refContainer[i]$domainNames)[1]
        if (dim == "*") dim = "uni"
        if (!is.null(self$refContainer[i]$records)) {
          recs = self$refContainer[i]$records

          if (self$refContainer[i]$dimension > 1) {
            stop("attempting to forward a domain set that has dimension > 1\n")
          }
          if (is.null(recs$element_text)) {
            df = self$records[diter]
            colnames(df) = dim
            recs1 = factor(append(as.character(recs[, 1]), as.character(df[,dim])),
            levels = unique(append(levels(recs[, 1]), levels(df[,dim]))))
            cnames =colnames(recs)
            recs= data.frame(recs1)
            colnames(recs) = cnames
            recs = recs[!duplicated(recs[[dim]]), , drop=FALSE]
            rownames(recs) <- NULL
          }
          else {
            df = self$records[diter]
            colnames(df) = dim
            df[["element_text"]] = ""
            recs1 = factor(append(as.character(recs[, 1]), as.character(df[,dim])),
            levels = unique(append(levels(recs[, 1]), levels(df[,dim]))))
            recs2 = append(recs[, 2], df$element_text)
            cnames =colnames(recs)
            recs= data.frame(recs1, recs2)
          }
          colnames(recs) = cnames
          recs = recs[!duplicated(recs[[dim]]), , drop= FALSE]
          rownames(recs) <- NULL
        }
        else {
          recs = self$records[diter]
          colnames(recs) = dim
          recs = recs[!duplicated(recs[[dim]]), , drop=FALSE]
          rownames(recs) <- NULL
        }
        self$refContainer[i]$records = recs
      }
    }
  },

  .get_duplicate_index = function(keep) {
    if (keep != FALSE && keep != "first" && keep != "last") {
      stop("The argument `keep` must be one of the following:
      `first`, `last`, or FALSE\n")
    }

    if (keep == "first") {
      fl = FALSE
      idx = which(duplicated(data.frame(lapply(1:self$dimension, 
      function(d) tolower(self$records[[d]]))), fromLast =fl) == TRUE)
    }
    else if (keep == "last") {
      fl = TRUE
      idx = which(duplicated(data.frame(lapply(1:self$dimension, 
      function(d) tolower(self$records[[d]]))), fromLast =fl) == TRUE)
    }
    else {
      idx_first = which(duplicated(data.frame(lapply(1:self$dimension, 
      function(d) tolower(self$records[[d]]))), fromLast =FALSE) == TRUE)
      idx_last = which(duplicated(data.frame(lapply(1:self$dimension, 
      function(d) tolower(self$records[[d]]))), fromLast =TRUE) == TRUE)
      idx = append(idx_last, idx_first)
    }
    return(idx)
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
        stop(paste0("Symbol ", self$name, " already exists in `destination`\n"))
      }
      newsym = destination[self$name]

      if (class(newsym)[1] != class(self)[1]) {
        stop(paste0("Cannot copy a symbol of type ", class(self)[1], 
        " to `destination` symbol type ", class(newsym)[1], 
        ". To overwrite, the symbols must be of same type"))
      }

      # copy all fields of one symbol to another
      newsym$records = self$records
      newsym$description = self$description
      newsym$domain = self$domain
      newsym$domainForwarding = self$domainForwarding
      if (self$dimension == 0) return(NULL)

      for (d in 1:self$dimension) {
        if (!inherits(self$domain[[d]], c("Set", "Alias"))) {
          next
        }

        if ( !is.null(destination[self$domain[[d]]$name]) &&
          self$domain[[d]]$equals(destination[self$domain[[d]]])) {
            newsym$domain[[d]] = destination[self$domain[[d]]$name]
        }
        else {
          newsym$domain[[d]] = self$domain[[d]]$name
        }
      }
      return(newsym)
    }
  }

  )
)
