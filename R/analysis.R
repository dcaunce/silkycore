
Analysis <- R6Class("Analysis",
    private=list(
        .id=0,
        .name="",
        .package="",
        .options=NA,
        .results=NA,
        .status="none",
        .init=function() NULL,
        .run=function() NULL,
        .state=NA,
        read=NA,
        .statePath=NA),
    active=list(
        id=function() private$.id,
        name=function() private$.name,
        options=function() private$.options,
        results=function() private$.results,
        status=function() private$.status,
        state=function() private$.state),
    public=list(
        initialize=function(package, name, id=0, options=NULL, state=NULL) {
            
            private$.package <- package
            private$.name    <- name
            
            private$.id <- id
            
            if (is.null(options))
                options <- Options$new()
            private$.options <- options
            
            if (is.null(state))
                state <- State$new()
            private$.state <- state
            
            info <- loadResultsInfo(package, name)
            private$.results <- silkycore::Results$new(info, options)
            
            private$.options$addChangeListener(private$.optionsChangedHandler)
        },
        check=function() {
            silkycore::check(private$.package, private$.name, private$.options)
        },
        init=function() {
            if (private$.status != "none")
                return()
            
            self$check()
            self$results$.update()
            
            #self$.readState()
            
            private$.init()
            
            private$.status <- "inited"
        },
        run=function() {
            self$init()
            private$.status <- "running"
            private$.run()
            private$.status <- "complete"
            
            #self$.saveState()
        },
        print=function() {
            self$init()
            self$results$print()
        },
        .setReadDatasetSource=function(read) {
            private$read <- read
        },
        .setStatePathSource=function(statePath) {
            private$.statePath <- statePath
        },
        .readState=function() {
            
            private$.state <- State$new()
            
            stateInfo <- loadStateInfo(private$.package, private$.name)
            
            try({
                if (is.function(private$.statePath)) {
                    statePath <- private$.statePath()
                    if (base::file.exists(statePath)) {
                        conn <- file(statePath, open="rb", raw=TRUE)
                        private$.state$.deserialize(conn)
                    }
                }
            })
        },
        .saveState=function() {
            
            if (is.function(private$.statePath)) {
                statePath <- private$.statePath()
                conn <- file(statePath, open="wb", raw=TRUE)
                private$.state$.serialize(conn)
                base::close(conn)
            }
        },
        readDataset=function() {

            columns <- character()
            
            env <- self$options$values()
            info <- loadAnalysisInfo(private$.package, private$.name)
            
            for (opt in info$options) {
                
                if (opt$type == "Variables" || opt$type == "Variable") {
                    value <- env[[opt$name]]
                    columns <- c(columns, value)
                }
            }

            self$options$set(dataset=private$read(columns))
        },
        optionsChangedHandler=function(optionNames) {
            private$.status <- "none"
        },
        asProtoBuf=function() {
            
            self$init()
            initProtoBuf()
            
            response <- RProtoBuf::new(silkycoms.AnalysisResponse)
            response$id = self$id
            response$results <- self$results$asProtoBuf();
            
            if (private$.status == "inited") {
                response$status <- silkycoms.AnalysisStatus$ANALYSIS_INITED;
            } else if (private$.status == "running") {
                response$status <- silkycoms.AnalysisStatus$ANALYSIS_RUNNING;
            } else if (private$.status == "complete") {
                response$status <- silkycoms.AnalysisStatus$ANALYSIS_COMPLETE;
            } else {
                response$status <- silkycoms.AnalysisStatus$ANALYSIS_ERROR;
            }
            
            response
        })
)
