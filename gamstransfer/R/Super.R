#
# GAMS - General Algebraic Modeling System R API
#
# Copyright (c) 2017-2024 GAMS Software GmbH <support@gams.com>
# Copyright (c) 2017-2024 GAMS Development Corp. <support@gams.com>
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

#' @title readGDX
#' @description read a GDX file to a list without creating symbol
#' or container objects
#' @param loadFrom name of the GDX file being read (string)
#' @param symbols optional argument - vector of strings
#' containing the symbol names to be read
#' @param records optional logical argument - TRUE (default) to read
#' the symbol records, FALSE to only read the meta data.
#' Please visit https://www.gams.com/latest/docs/API_R_GAMSTRANSFER.html
#' for detailed documentation of this package.
#'
#' @examples
#' read_list = readGDX(system.file("extdata", "trnsport.gdx", package = "gamstransfer"))

readGDX = function(loadFrom, symbols=NULL, records=TRUE) {
    # check if records is logical
    if (!is.logical(records) && length(records) != 1) {
    stop("records must be type logical\n")
    }

    # is.character will also check vector of strings
    if (!(is.character(symbols)) && !(is.null(symbols))) {
    stop("argument symbols must be of the type character or NULL\n")
    }

    if (is.character(loadFrom)) {
        namesplit = strsplit(loadFrom, "\\.")
        ext = utils::tail(unlist(namesplit), 1)
        if (ext != "gdx") {
            stop("check filename extension, must be .gdx\n")
        }
        loadFrom = R.utils::getAbsolutePath(path.expand(loadFrom))
        if (!file.exists(loadFrom)) {
            stop(paste0("File ", loadFrom, " doesn't exist\n"))
        }
        # read call here
        readlist = .CPP_readSuper(symbols, loadFrom,
            records)
        sym_names = unlist(lapply(readlist, "[[", 2), use.names = FALSE)
        names(readlist) = sym_names
    }
    else {
        stop(paste0("Argument `loadFrom` must be type character\n"))
    }

    return(readlist)
}

#' @title writeGDX
#' @description write a GDX file from a list containing symbol data
#' and metadata
#' @param writeList list containing symbol data and metadata
#' @param writeTo name of the output GDX file
#' @param symbols optional argument - vector of strings
#' containing the symbol names to be read
#' @param compress optional logical argument. TRUE to produce a
#' compressed GDX file
#' @param uelPriority Specify the priority UELs
#' @param mode optional string argument to specify the write
#' mode ("string", "mapped").
#' Please visit https://www.gams.com/latest/docs/API_R_GAMSTRANSFER.html
#' for detailed documentation of this package.
#'
#' @examples
#' \dontrun{
#' writeGDX(list(), "gt.gdx")
#' }
#'
writeGDX = function(writeList, writeTo, symbols=NULL,
    compress = FALSE, uelPriority = NULL, mode = NULL) {
    if (!is.logical(compress)) {
    stop(paste0("'compress' must be of type logical; ",
    "default False (no compression)\n"))
    }

    if (!is.character(writeTo)) {
    stop("The argument writeTo must be of type character\n")
    }
    else {
    namesplit = strsplit(writeTo, "\\.")
    ext = utils::tail(unlist(namesplit), 1)
    if (ext != "gdx") {
        stop("check filename extension, must be .gdx\n")
    }

    writeTo = R.utils::getAbsolutePath(path.expand(writeTo))
    }

    if (!(is.character(uelPriority) || is.null(uelPriority))) {
    stop("'uelPriority' must be type character or NULL\n")
    }

    if (is.null(mode)) {
    mode = "mapped"
    }
    if (!(is.character(mode) && length(mode) == 1)) {
    stop("Argument `mode` must be type character 
    of length 1\n")
    }

    if (!any(c("string", "mapped") == mode)) {
    stop("Argument `mode` must be one of the following: 'string', 'mapped'\n")
    }

    if (mode == "string") {
    mode_int = 1
    }
    else {
    mode_int = 2
    }

    isempty = (length(writeList) == 0)
    enable = NA

    if (!isempty) {
        allSymbols = unlist(lapply(writeList, "[[", 2), use.names = FALSE)
        if (is.null(symbols)) {
          symbols = allSymbols
          enable = replicate(length(symbols), TRUE)
        }
        else {
          enable = replicate(length(writeList), FALSE)

          allSymbolsList = as.list(1:length(writeList))
          names(allSymbolsList) = allSymbols

          allSymDict = collections::dict(allSymbolsList)

          idx=unlist(lapply(symbols, function(s) {
            allSymDict$get(s)
          }), use.names = FALSE)
          enable[idx] = TRUE
        }

        # assuming validity
        # assuming valid order

        # if (private$isValidSymbolOrder() == FALSE) {
        #   self$reorderSymbols()
        # }
      }

      .CPP_gdxWriteSuper(writeList, enable,
      writeTo, uelPriority, compress, mode_int)

}