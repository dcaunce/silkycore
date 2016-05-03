
Options <- R6::R6Class(
    "Options",
    private=list(
        .options=NA,
        .listeners=NA),
    public=list(
        initialize=function(package=NULL, name=NULL, json=NULL, ...) {
            
            private$.options <- new.env()
            private$.listeners <- list()
            
            if (is.character(package) && is.character(name)) {
                info <- loadAnalysisInfo(package, name)
                for (option in info$options) {
                    if ("default" %in% names(option) && "name" %in% names(option))
                        private$.options[[option$name]] <- option$default
                }
            }
            
            if ( ! is.null(json)) {
                opts <- rjson::fromJSON(json)
                for (name in names(opts))
                    private$.options[[name]] <- opts[[name]]
            }
            
            opts <- list(...)
            for (name in names(opts))
                private$.options[[name]] <- opts[[name]]
            
            private$.options[["levels"]] <- self$levels
        },
        dataset=function() {
            private$.options[["dataset"]]
        },
        values=function() {
            private$.options
        },
        eval=function(value, ...) {
            
            if (class(value) == "character") {
                
                vars <- list(...)
                for (name in names(vars))
                    private$.options[[name]] <- vars[[name]]
                
                nch <- nchar(value)
                if ( ! is.na(suppressWarnings(as.numeric(value))))
                    value <- as.numeric(value)
                else if (nch > 0 && substring(value, 1, 1) == "(" && substring(value, nch) == ")")
                    value <- self$.eval(text=value)
                else
                    value <- silkycore::format(value, ...)
                
                if (length(names(vars)) > 0)
                    rm(list=names(vars), envir=private$.options)
            }
            
            value
        },
        .eval=function(text) {
            
            value <- try(base::eval(parse(text=text), envir=private$.options), silent=TRUE)
            
            if (inherits(value, "try-error")) {
                reason <- extractErrorMessage(value)
                stop(format("Could not evaluate '{text}'\n    {reason}", text=text, reason=reason), call.=FALSE)
            }
            value
        },
        set=function(...) {
            
            values <- list(...)
            for (name in names(values))
                private$.options[[name]] <- values[[name]]
            
            for (listener in private$.listeners)
                listener(names(values))
        },
        setValue=function(name, value) {
            private$.options[[name]] <- value
            
            for (listener in private$.listeners)
                listener(name)
        },
        get=function(name) {
            private$.options[[name]]
        },
        levels=function(x) {
            str <- substitute(x)
            expr <- parse(text=paste0("base::levels(dataset[[", str, "]])"))
            v = eval.parent(expr)
            v
        },
        addChangeListener=function(listener) {
            private$.listeners[[length(private$.listeners)+1]] <- listener
        })
)
