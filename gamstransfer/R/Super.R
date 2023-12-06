readGDX = function(loadFrom, symbols=NULL, records=TRUE, 
 systemDirectory = NULL) {
    # check if records is logical
    if (!is.logical(records) && length(records) != 1) {
    stop("records must be type logical\n")
    }

    # is.character will also check vector of strings
    if (!(is.character(symbols)) && !(is.null(symbols))) {
    stop("argument symbols must be of the type character or NULL\n")
    }

    if (is.null(systemDirectory)) {
        sysDirPath = find_gams()
        if (is.null(sysDirPath)) {
        stop("Could not find a GAMS installation in the environment ", 
        "variable `PATH`. Specify the GAMS system ",
        "directory via the argument `systemDirectory`\n")
        }
        systemDirectory = sysDirPath
    }
    else {
        if (!R.utils::isAbsolutePath(systemDirectory)) {
            stop(paste0("must enter valid full (absolute) path to the",
            "GAMS system directory\n"))
        }
    }

    if (is.character(loadFrom)) {
        namesplit = strsplit(loadFrom, "\\.")
        ext = tail(unlist(namesplit), 1)
        if (ext != "gdx") {
            stop("check filename extension, must be .gdx\n")
        }
        loadFrom = R.utils::getAbsolutePath(path.expand(loadFrom))
        if (!file.exists(loadFrom)) {
            stop(paste0("File ", loadFrom, " doesn't exist\n"))
        }
        # read call here
        readlist = CPP_readSuper(symbols, loadFrom, 
            systemDirectory, records)
        sym_names = unlist(lapply(readlist, "[[", 2), use.names = FALSE)
        names(readlist) = sym_names
    }
    else {
        stop(paste0("Argument `loadFrom` must be type character\n"))
    }

    return(readlist)
}

writeGDX = function(writeList, writeTo, symbols=NULL, 
    compress = FALSE, uelPriority = NULL, mode = NULL, 
    systemDirectory = NULL) {
    if (!is.logical(compress)) {
    stop(paste0("'compress' must be of type logical; ",
    "default False (no compression)\n"))
    }

    if (!is.character(writeTo)) {
    stop("The argument writeTo must be of type character\n")
    }
    else {
    namesplit = strsplit(writeTo, "\\.")
    ext = tail(unlist(namesplit), 1)
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

      CPP_gdxWriteSuper(writeList, systemDirectory, enable,
      writeTo, uelPriority, compress, mode_int)

}