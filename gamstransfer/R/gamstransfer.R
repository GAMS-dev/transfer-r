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