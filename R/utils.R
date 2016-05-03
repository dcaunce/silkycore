
.SUPCHARS <- c("\u1D43", "\u1D47", "\u1D48", "\u1D49", "\u1DA0", "\u1D4D", "\u02B0", "\u2071",
               "\u02B2", "\u1D4F", "\u02E1", "\u1D50", "\u207F", "\u1D52", "\u1D56", "\u02B3", "\u02E2",
               "\u1D57", "\u1D58", "\u1D5B", "\u02B7", "\u02E3", "\u02B8", "\u1DBB")

ignore <- function(...) {
    if (length(args) > 0)
        warning(paste(paste0("Ignoring argument '", names(list(...)),"'"), collapse='\n'))
}

reject <- function(formats, code=NULL, ...) {

    message <- format(formats[1], ...)
    error <- simpleError(message)
    
    for (name in names(formats)) {
    
        if (name != "") {
            message <- format(formats[[name]], ...)
            error[[name]] <- message
        }
    }
    
    error$code <- code

    stop(error)
}

.analysisInfoCache <- new.env()
.resultsInfoCache <- new.env()

loadAnalysisInfo <- function(packageName, analysisName) {

    name <- paste0(packageName, "::", analysisName)

    if (name %in% names(.analysisInfoCache)) {
        
        info <- .analysisInfoCache[[name]]
        
    } else {

        location <- system.file("silky", paste0(tolower(analysisName), ".a.yaml"), package=packageName)
        if (location == "")
            location <- system.file("inst", "silky", paste0(tolower(analysisName), ".a.yaml"), package=packageName, mustWork=TRUE)
        
        info <- yaml::yaml.load_file(location)

        .analysisInfoCache[[name]] <- info
    }

    info
}

loadResultsInfo <- function(packageName, analysisName) {
    
    name <- paste0(packageName, "::", analysisName)
    
    if (name %in% names(.resultsInfoCache)) {
        
        info <- .resultsInfoCache[[name]]
        
    } else {
        
        location <- system.file("silky", paste0(tolower(analysisName), ".r.yaml"), package=packageName)
        if (location == "")
            location <- system.file("inst", "silky", paste0(tolower(analysisName), ".r.yaml"), package=packageName, mustWork=TRUE)
        
        info <- yaml::yaml.load_file(location)
        
        .resultsInfoCache[[name]] <- info
    }
    
    info
}

columnType <- function(column) {

    if (inherits(column, "ordered")) {
        return("ordinal")
    } else if (inherits(column, "factor")) {
        return("nominal")
    } else {
        return("continuous")
    }
}

columnTypeRDescription <- function(column) {

    if (is.ordered(column)) {
        return("an ordered factor")
    } else if (is.factor(column)) {
        return("a factor")
    } else {
        return("numeric")
    }
}

cap1st <- function(s) {
    paste0(toupper(substring(s,1,1)), substring(s, 2))
}

format <- function(str, ..., context="normal") {

    args <- list(...)

    if (grepl("{}", str, fixed=TRUE)) {
    
        for (token in args)
            str <- sub("{}", stringify(token, context), str, fixed=TRUE)
        
    } else {
    
        if (grepl("\\{[0-9]+\\}", str)) {
    
            i <- 0
            for (token in args) {
        
                str <- gsub(paste0("{", i, "}"), stringify(token, context), str, fixed=TRUE)
                i <- i + 1
            }
            
        }
        if (grepl("\\{[a-zA-Z]+\\}", str)) {
        
            for (name in names(args)) {
                if (name != "" && is.null(args[[name]]) == FALSE)
                    str <- gsub(paste0("{", name, "}"), stringify(args[[name]], context), str, fixed=TRUE)
            }
        }
    }

    str
}

spaces <- function(n) {
    s <- ''
    if (n > 0)
        s <- paste(rep(' ', n), collapse='')
    s
}

dotPos <- function(x) {
    floor(log10(x))
}

nDigits <- function(x, negSign=TRUE) {
    
    # calcs no. digits before the decimal point
    
    n <- 1
    
    if (x > 1) {
        n <- base::floor(base::log10(x)) + 1
    } else if (x < -1) {
        n <- base::floor(base::log10(abs(x))) + 1
    }
    
    if (x < 0 && negSign)
        n <- n + 1
    
    n
}

silkyMeasureElements <- function(elems, sf=3, scl=1e-3, sch=1e7) {
    
    # non-scientific
    dp <- 0
    maxns <- 0   # max
    minns <- 0
    
    # scientific
    maxexp <- 0  # max scientific exponent
    minexp <- 0  # min scientific exponent
    negman <- FALSE  # are any of the mantissas negative
    
    # string
    maxstr <- 4
    
    maxsupwidth <- 0  # max superscripts width
    
    for (elem in elems) {
        
        sups <- integer()
        
        if (inherits(elem, "Cell")) {
            sups <- elem$sups
            elem <- elem$value
        }
        
        if (is.null(elem)) {
            
            maxstr <- max(maxstr, 4)  # width of 'null'
            
        } else if (is.na(elem)) {
            
            # do nothing
            
        } else if (is.infinite(elem)) {
            
            maxstr <- max(maxstr, 4)  # width of '-Inf'
            
        } else if (inherits(elem, "character")) {
            
            maxstr <- max(maxstr, nchar(elem))
        }
        else if ( ! is.numeric(elem)) {
            
            maxstr <- 2 + nchar(class(elem)[1])
        }
        else if (elem == 0) {
            
            dp <- max(dp, sf-1)
        }
        else if (abs(elem) > scl && abs(elem) < sch) {
            
            # non-scientific values
            
            dp <- max(dp, (sf - floor(log10(abs(elem))) - 1))
            
            maxns <- max(maxns, elem)
            minns <- min(minns, elem)
            
        } else {
            
            # scientific values
            
            exp <- floor(log10(abs(elem)))
            man <- elem / (10 ^ exp)
            
            maxexp <- max(maxexp, exp)
            minexp <- min(minexp, exp)
            if (man < 0)
                negman <- TRUE
        }
        
        if (length(sups) > 0)
            maxsupwidth <- max(maxsupwidth, 1 + length(sups))
    }
    
    maxnsw <- nDigits(maxns)
    minnsw <- nDigits(minns)
    
    nswidth <- max(maxnsw, minnsw)  # non-scientific width
        
    if (dp > 0)
        nswidth <- nswidth + 1 + dp # add a decimal point
    
    swidth <- 0  # scientific width
    expwidth <- 0
    
    if (maxexp > 0 || minexp < 0) {
    
        expwidth <- max(nDigits(maxexp), nDigits(minexp, negSign=FALSE)) + 2  # +2 for the e and the sign
        manwidth <- sf + 1  # sf + room for a decimal point
        if (negman)
            manwidth <- manwidth + 1  # add room for a minus sign
            
        swidth <- manwidth + expwidth
    }
    
    width <- max(swidth, nswidth, maxstr)
    width <- width + maxsupwidth

    list(sf=sf, dp=dp, width=width, expwidth=expwidth, supwidth=maxsupwidth)
}

silkyFormatElement <- function(elem, w=NULL, expw=NULL, supw=0, dp=2, sf=3, scl=1e-3, sch=1e7) {
    
    sups <- integer()
    supspad <- ''
    
    if (inherits(elem, "Cell")) {
        sups <- elem$sups
        elem <- elem$value
        
        if (is.null(w) == FALSE)
            w <- w - supw
        thissupw <- length(sups)
        if (thissupw > 0)
            thissupw <- thissupw + 1  # add 1 for the space
            
        supspad <- repstr(' ', supw - thissupw)
    }
    
    if (is.null(elem)) {
        
        width <- 4
        padstr <- spaces(max(w - 4, 0))
        str <- paste0("null", padstr)
        
    } else if (is.na(elem)) {
        
        if (is.null(w))
            str <- ''
        else
            str <- repstr(' ', w)
        
    } else if (is.infinite(elem)) {
        
        if (elem > 0)
            str <- "Inf"
        else
            str <- "-Inf"
        
    } else if (inherits(elem, "character")) {
        
        width <- nchar(elem)
        padstr <- spaces(max(w - width, 0))
        str <- paste0(elem, padstr)
        
    } else if ( ! is.numeric(elem)) {
        
        str <- paste0("[", class(elem)[1], "]")
        
    } else if (elem == 0 || (abs(elem) > scl && abs(elem) < sch)) {
        
        # non-scientific values

        str <- sprintf(paste0("%", w, ".", dp, "f"), elem)
        
    } else {
        
        # scientific values
        
        exponent <- floor(log10(abs(elem)))
        
        sign <- ifelse(exponent >= 0, '+', '-')
        mantissa <- elem / (10^exponent)
        exponent <- abs(exponent)
        
        expstr <- base::format(exponent, scientific=FALSE)
        
        exppad <- ''
        if ( ! is.null(expw))
            exppad <- spaces(expw-nchar(expstr)-2)  # 1 for the +/-, 1 for the e
        expstr <- paste0('e', exppad, sign, expstr)
        
        if ( ! is.null(w))
            manstr <- base::formatC(x=mantissa, width=w-nchar(expstr), digits=sf-1, format="f")
        else
            manstr <- base::formatC(x=mantissa, digits=sf-1, format="f")
        
        str <- paste0(manstr, expstr)
    }
    
    if (length(sups) > 0)
        str <- paste0(str, ' ', paste(.SUPCHARS[sups+1], collapse=''))
    str <- paste0(str, supspad)
    
    str
}

repstr <- function(value, n, join='') {
    if (n > 0)
        return(paste(rep(value, n), collapse=join))
    else
        return('')
}

stringify <- function(value, context="normal") {

    if (context == "R") {
    
        if (is.null(value))
            return("NULL")
        else
            return(paste0(value))
            
    } else {
        
        if (is.null(value))
            return("null")
        else if (identical(value, TRUE))
            return("true")
        else if (identical(value, FALSE))
            return("false")
        else
            return(paste0(value))    
    }
}

extractErrorMessage <- function(error) {
    
    split <- base::strsplit(as.character(error), ":")[[1]]
    last <- split[[length(split)]]
    base::trimws(last)
}

rethrow <- function(error) {
    
    message <- extractErrorMessage(error)
    stop(message, call.=FALSE)
}


