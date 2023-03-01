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

#' @title ConstContainer Class
#' @description ConstContainer class is a data-focused read-only object that 
#' will provide a snapshot of the data target being read. The ConstContainer 
#' can be created by reading a GDX file. This class is specially useful for 
#' the users who are only interested in post-processing data from a GAMS model 
#' run. The ConstContainer class inherits from an abstract
#' BaseContainer class. To access the functions common to Container and
#' ConstContainer, please use help(BaseContainer).
#' @field data is a named list containing all symbol data
#' @field systemDirectory is the path to GAMS System directory
#' @export
ConstContainer <- R6::R6Class (
  "ConstContainer",
  inherit = .BaseContainer,
  public = list(
    #' @description
    #' Create a new ConstContainer simply by initializing an object.
    #' @param systemDirectory optional argument for the absolute path to 
    #' GAMS system directory
    #' @examples
    #' ConstContainer$new()
    initialize = function(loadFrom=NULL, systemDirectory=NULL) {

      super$initialize(systemDirectory)

      if (!missing(loadFrom)) {
      self$read(loadFrom, records = FALSE)

      }
    },

    #' @description main method to read loadFrom, can be provided 
    #' with a list of symbols to read in subsets, `records` controls 
    #' if symbol records are loaded or just metadata
    #' @param loadFrom name of the file to load data from as a string
    #' @param symbols optional argument to specify the names of the 
    #' symbols to be read (string or a list of strings)
    #' @param records optional logical argument to specify whether to 
    #' read symbol records (logical)
    read = function(loadFrom, symbols=NULL, records=TRUE) {
      # read metadata
      # get all symbols and metadata from c++
      # process it and populate various fields

      # check if records is logical
      if (!is.logical(records)) {
        stop("records must be type logical\n")
      }

      if (!(is.character(symbols)) && !(is.list(symbols)) && !(is.null(symbols))) {
        stop("argument symbols must be of the type list or string\n")
      }

      if (is.list(symbols)) {
        if (!all(unlist(lapply(symbols, is.character)))) {
          stop("argument symbols must contain only type string\n")
        }
        # convert symbols to a vector
        symbols = unlist(symbols)
      }

      if (!is.character(loadFrom)) {
        stop("The argument loadFrom must be of type string\n")
      }
      else {
        namesplit = strsplit(loadFrom, "\\.")
        ext = tail(unlist(namesplit), 1)
        if (ext != "gdx") {
          stop("check filename extension, must be .gdx\n")
        }
        loadFrom = R.utils::getAbsolutePath(path.expand(loadFrom))
        if (!file.exists(loadFrom)) {
          stop(paste0("File ", loadFrom, " doesn't exist\n"))
        }
      }

      if (is.null(symbols)) {
        cpp_syminput = ""
      }
      else {
        cpp_syminput = symbols
      }

      readlist = CPP_readSuper(cpp_syminput, loadFrom, 
      self$systemDirectory, records, is.null(symbols))

      acronyms = readlist[[1]]
      if (acronyms$nAcronyms != 0) {
        self$acronyms = acronyms[["acronyms"]]
      }

      readData = readlist[-1]
      symbolsToRead = unlist(lapply(readData, "[[", 1))
      # reset data
      self$data = collections::ordered_dict()

      # reset lower case data
      self$.lc_data = collections::dict()

      aliasList = list()
      aliasCount = 0
      for (m in readData) {
        if (m$type == .gdxSymbolTypes()[["GMS_DT_PAR"]]) {
          .ConstParameter$new(
            self, m$name, m$domain, records = NULL,
            description = m$expltext, domaintype= m$domaintype,
            numberRecords=m$numRecs)
        }
        else if (m$type == .gdxSymbolTypes()[["GMS_DT_SET"]]) {
            dt = m$domaintype
            if ((length(m$domain) == 1) && (m$name == m$domain[1])) {
              dt = 2 # for relaxed domain type
            }

            .ConstSet$new(
            self, m$name, m$domain, as.logical(m$subtype),
            records = NULL,
            m$expltext,dt, m$numRecs)
            if (!any(c(0,1) == m$subtype)) {
              stop(paste0("Unknown set classification with 
              GAMS Subtype ", m$subtype, "cannot load set ", m$name))
            }
        }
        else if (m$type == .gdxSymbolTypes()[["GMS_DT_VAR"]]) {
            type = which(.VarTypeSubtype() == m$subtype)
            if (is.integer0(type)) {
              type = "free"
            }
            else {
              type = names(.VarTypeSubtype())[[type]]
            }
            .ConstVariable$new(
            self, m$name, type, m$domain,
            description = m$expltext, domaintype = m$domaintype,
            numberRecords=m$numRecs)
        }
        else if (m$type == .gdxSymbolTypes()[["GMS_DT_EQU"]]) {
            type = which(.EqTypeSubtype() == m$subtype)
            if (is.integer0(type)) {
              type = "eq"
            }
            else {
              type = names(.EqTypeSubtype())[[type]]
            }

            .ConstEquation$new(
            self, m$name, type, m$domain,
            description = m$expltext, domaintype = m$domaintype,
            numberRecords=m$numRecs)
        }
        else if (m$type == .gdxSymbolTypes()[["GMS_DT_ALIAS"]]) {
            dt = m$domaintype
            if ((length(m$domain) == 1) && (m$name == m$domain[1])) {
              dt = 2 # for relaxed domain type
            }

            if (m$aliasfor == "*") {
              .ConstUniverseAlias$new(self, m$name, m$aliasfor, m$domain,
              m$expltext, dt, m$numRecs
              )
            }
            else {
              .ConstAlias$new(self, m$name, m$aliasfor, m$domain, as.logical(m$subtype),
              m$expltext, dt, m$numRecs)
            }
        }
      }

      # set acronyms to NA
      if (records == TRUE) {

        for (s in readData) {
          if (is.null(s$records)) {
            next
          }
          self[s$name]$setRecords(s$records)

          if (!is.null(self$acronyms)) {
            if (inherits(self[s$name], c(".ConstParameter", 
            ".ConstVariable", ".ConstEquation"))) {
              for (a in self$acronyms) {
                self[s$name]$records[(self[s$name]$records 
                == a * 1e301)] = SpecialValues[["NA"]]
              }
            }
          }
        }
      }
    },

    equals = function(other, verbose=FALSE) {

      if (!inherits(other, ".BaseContainer")) {
        if (verbose) {
          stop("The argument `other` is not a Container\n")
        }
        else {
          return(FALSE)
        }
      }

      if (self$data$size() != other$data$size()) {
        if (verbose) {
          stop(paste0("Containers contain different number ",
          "of symbols.\n self: ", 
          self$data$size(), "\n other :", other$data$size(), "\n"))
        }
        else {
          return(FALSE)
        }
      }

      self_data_keys = unlist(self$data$keys(), use.names = FALSE)
      other_data_keys = unlist(other$data$keys(), use.names = FALSE)
      diff_keys = setdiff(self_data_keys, other_data_keys)
      if (length(diff_keys) != 0) {
        if (verbose) {
          stop(paste0("Container `data` field keys do not match.",
          " Keys not present in `other` :", 
          toString(diff_keys)))
        }
        else {
          return(FALSE)
        }
      }

      for (s in self$data$keys()) {
        selfsym = self[s]
        othersym = other[s]
        if (!identical(selfsym, othersym)) {
          if (verbose) {
            stop("Symbols named `", s, "` in both containers are not identical")
          }
          else {
            return(FALSE)
          }
        }
      }

      # if didn't return false until here then its true
      return(TRUE)
    }

  )
  )
