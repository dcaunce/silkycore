
ResultElement <- R6Class("ResultElement",
    private=c(
        .name="",
        .title="",
        .index=0,
        .visible="TRUE",
        .options=NA,
        .updated=FALSE),
    active=list(
        name=function() private$.name,
        options=function() private$.options,
        visible=function() {
            vis <- private$.options$eval(private$.visible, name=private$.name, index=private$.index)
            if (is.logical(vis))
                return(vis)
            else
                return(length(vis) > 0)
        },
        title=function(value) {
            
            if (missing(value))
                return(paste0(private$.options$eval(private$.title, name=private$.name, index=private$.index)))
            
            private$.title <- value
            invisible(self)
        }),
    public=list(
        initialize=function(name="", index=0, options=Options()) {
            private$.name <- name
            private$.title <- name
            private$.index <- as.integer(index)
            private$.options <- options
            private$.visible <- paste0(TRUE)
            private$.updated <- FALSE
            
            private$.options$addChangeListener(self$.optionsChanged)
        },
        .update=function() {
            private$.updated <- TRUE
        },
        .optionsChanged=function(...) {
            private$.updated <- FALSE
        },
        .has=function(name) {
            paste0(".", name) %in% names(private)
        },
        .setDef=function(name, value) {
            if (self$.has(name))
                private[[paste0(".", name)]] <- value
        },
        .setup=function(def) {
            
            for (name in names(def)) {
                value <- def[[name]]
                self$.setDef(name, value)
            }
        },
        asString=function() {
            ""
        },
        print=function() {
            cat(self$asString())
        }
    ))

Results <- R6Class("Results",
    private=list(
        .title="no title",
        .elements=list(),
        .options=NA),
    public=list(
        initialize=function(defs=NULL, options=NULL) {
            
            if (is.null(options))
                options <- Options$new()
            private$.options <- options

            if ( ! is.null(defs)) {
                
                if ( ! is.null(defs$title))
                    private$.title <- defs$title
                
                for (def in defs$results) {
                    type <- make.names(def$type)  # we run an eval, so we have to sanitize the names
                    name <- make.names(def$name)
                    element <- eval(parse(text=format("{type}$new('{name}', options=private$.options)", type=type, name=name)))
                    element$.setup(def)
                    self$append(element)
                }
            }
        },
        .update=function() {
            for (element in private$.elements)
                element$.update()
        },
        append=function(element) {
            private$.elements[[element$name]] <- element
        },
        get=function(name) {
            private$.elements[[name]]
        },
        print=function() {
            cat(self$asString())
        },
        asString=function() {
            
            pieces <- character()
            
            pieces <- c(pieces, '\n\n ', toupper(private$.title), '\n')
            
            for (element in private$.elements)
                if (element$visible)
                    pieces <- c(pieces, element$asString())
                                
            pieces <- c(pieces, '\n\n')
            
            return(paste0(pieces, collapse=''))
        },
        asProtoBuf=function() {
            
            initProtoBuf()
            
            resultsBuf <- RProtoBuf::new(silkycoms.AnalysisResponse.Results)
            
            for (element in private$.elements) {
                
                if (element$visible == FALSE)
                    next()
                
                elem <- RProtoBuf::new(silkycoms.ResultsElement,
                    name=element$name,
                    title=element$title,
                    status=silkycoms.AnalysisStatus$ANALYSIS_COMPLETE,
                    text=element$asString())
                
                resultsBuf$add("elements", elem)
                
                #resultsBuf$add("elements", element$asProtoBuf())
            }
            
            resultsBuf
        })
)
