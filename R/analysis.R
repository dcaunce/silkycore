
Analysis <- R6Class("Analysis",
    private=list(
        .id=0,
        .name="",
        .package="",
        .options=NA,
        .results=NA,
        .inited=FALSE,
        read=NA),
    active=list(
        id=function() private$.id,
        name=function() private$.name,
        options=function() private$.options,
        results=function() private$.results),
    public=list(
        initialize=function(package, name, id=0, options=NULL) {
            
            private$.package <- package
            private$.name    <- name
            
            private$.id <- id
            
            if (is.null(options))
                options <- Options()
            private$.options <- options
            
            private$.results <- silkycore::Results$new(package, name, options)
            
            private$.options$addChangeListener(private$.optionsChangedHandler)
        },
        check=function() {
            silkycore::check(private$.package, private$.name, private$.options)
        },
        init=function() {
            if (private$.inited)
                return()
            
            self$check()
            self$results$.update()
            private$.inited <- TRUE
        },
        run=function() {
            self$init()
        },
        print=function() {
            self$init()
            self$results$print()
        },
        .setReadDataset=function(read) {
            private$read <- read
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
            private$.inited <- FALSE
        },
        asProtoBuf=function() {
            
            self$init()
            initProtoBuf()
            
            response <- RProtoBuf::new(silkycoms.AnalysisResponse)
            response$id = self$id
            response$results <- self$results$asProtoBuf();
            response$status <- silkycoms.AnalysisStatus$ANALYSIS_COMPLETE
            response
        })
)
