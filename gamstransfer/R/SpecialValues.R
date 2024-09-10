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

#' @title SpecialValues list object
#' @description This list contains GAMS special values and
#' helper functions to check if a given value is a GAMS
#' special value.
#' Please visit https://transfer-r.readthedocs.io/en/latest/
#' for detailed documentation of this package.
#'
#' @examples
#' # check the value of GAMS special value NA
#' NA_val <- SpecialValues[["NA"]]
#' # check the value of GAMS special value EPS
#' EPS_val <- SpecialValues[["EPS"]]
#' # check if a value is GAMS special value `NA`
#' isNA_check <- SpecialValues$isNA(0)
SpecialValues <- list(
  "NA" = NA, # cannot be anything else
  "EPS" = -0.0,
  "UNDEF" = NaN,
  "POSINF" = Inf,
  "NEGINF" = -Inf,
  "isNA" = function(x) {
    return(is.na(x) & !is.nan(x))
  },
  "isEps" = function(x) {
    isna <- is.na(x)
    iseps_logical <- ((x == 0) & (sign(1 / x) == -1))
    iseps_logical[isna] <- FALSE
    return(iseps_logical)
  },
  "isUndef" = function(x) {
    return(is.nan(x))
  },
  "isPosInf" = function(x) {
    return(is.infinite(x) & sign(x) == 1)
  },
  "isNegInf" = function(x) {
    return(is.infinite(x) & sign(x) == -1)
  }
)
