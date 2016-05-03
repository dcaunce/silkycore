
ResultElement <- R6::R6Class("ResultElement",
    private=c(
        .name="",
        .key=NA,
        .title="",
        .index=0,
        .visible="TRUE",
        .options=NA,
        .updated=FALSE),
    active=list(
        name=function() private$.name,
        options=function() private$.options,
        visible=function() {
            vis <- private$.options$eval(private$.visible, key=private$.key, name=private$.name, index=private$.index)
            if (is.logical(vis))
                return(vis)
            else
                return(length(vis) > 0)
        },
        title=function(value) {
            
            if (missing(value))
                return(paste0(private$.options$eval(private$.title, key=private$.key, name=private$.name, index=private$.index)))
            
            private$.title <- value
            invisible(self)
        },
        path=function() {
            if ("ResultsElement" %in% class(self$.parent))
                return(paste(self$.parent$path, self$name, sep="/"))
            else
                return(self$name)
        },
        root=function() {
            parent <- self
            while ("ResultElement" %in% class(parent))
                parent <- parent$.parent
            parent
        },
        analysis=function() {
            root <- self$root
            if (base::identical(root, self))
                analysis <- NULL
            else
                analysis <- root$analysis
            analysis
        }),
    public=list(
        initialize=function(key="", index=0, options=Options$new()) {
            
            private$.key <- key
            private$.name <- rjson::toJSON(key)
            private$.title <- private$.name
            
            private$.index <- as.integer(index)
            private$.options <- options
            private$.visible <- paste0(TRUE)
            private$.updated <- FALSE
            
            private$.options$addChangeListener(self$.optionsChanged)
        },
        .update=function() {
            private$.updated <- TRUE
        },
        .render=function() {
            
        },
        .optionsChanged=function(...) {
            private$.updated <- FALSE
        },
        .has=function(name) {
            paste0(".", name) %in% names(private)
        },
        .setDef=function(name, value) {
            if (self$.has(name)) {
                private[[paste0(".", name)]] <- value
                private$.updated <- FALSE
            }
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
        },
        .parent=NA
    ))

Results <- R6Class("Results",
    private=list(
        .title="no title",
        .elements=list(),
        .options=NA,
        .analysis=NA),
    active=list(
        analysis=function() private$.analysis),
    public=list(
        initialize=function(defs=NULL, options=NULL, analysis=NULL) {
            
            if (is.null(options))
                options <- Options$new()
            private$.options <- options
            private$.analysis <- analysis

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
        .render=function() {
            for (element in private$.elements) {
                if (element$visible)
                    element$.render()
            }
        },
        append=function(element) {
            element$.parent <- self
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
            
            group <- RProtoBuf::new(silkycoms.ResultsGroup)
            
            for (element in private$.elements) {
                
                if (element$visible == FALSE)
                    next()
                
                group$add("elements", element$asProtoBuf())
            }
            
            results <- RProtoBuf::new(silkycoms.ResultsElement,
                name=private$.name,
                title=private$.title,
                group=group)
            
            results
        })
)
