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

#' @title gamstransfer
#' @name gamstransfer
#' @docType package
#' @useDynLib gamstransfer, .registration = TRUE
#' @description A package to maintain GAMS data outside a GAMS script
#' @details GAMS Transfer is a package to maintain GAMS data outside a GAMS script
#'  in a programming language like Python, Matlab or R. It allows the user 
#' to add GAMS symbols (Sets, Aliases, Parameters, Variables and Equations),
#'  to manipulate GAMS symbols, as well as read/write symbols to different 
#' data endpoints. GAMS Transfer's main focus is the highly efficient transfer
#'  of data between GAMS and the target programming language, while keeping 
#' those operations as simple as possible for the user. In order to achieve this,
#'  symbol records - the actual and potentially large-scale data sets - are 
#' stored in native data structures of the corresponding programming languages. 
#' The benefits of this approach are threefold: (1) The user is usually very 
#' familiar with these data structures, (2) these data structures come with a 
#' large tool box for various data operations, and (3) optimized methods for 
#' reading from and writing to GAMS can transfer the data as a bulk - resulting
#'  in the high performance of this package. 
NULL