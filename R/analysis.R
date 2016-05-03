
Analysis <- R6::R6Class("Analysis",
    private=list(
        .datasetId="",
        .analysisId="",
        .name="",
        .package="",
        .options=NA,
        .results=NA,
        .status="none",
        .init=function() NULL,
        .run=function() NULL,
        .state=NA,
        .readDataset=NA,
        .readDatasetHeader=NA,
        .requiresData=TRUE,
        .statePathSource=NA,
        .resourcesPathSource=NA,
        .info=NA),
    active=list(
        analysisId=function() private$.analysisId,
        name=function() private$.name,
        options=function() private$.options,
        results=function() private$.results,
        status=function() private$.status,
        state=function() private$.state),
    public=list(
        initialize=function(package, name, datasetId="", analysisId="", options=NULL, state=NULL) {
            
            private$.package <- package
            private$.name    <- name
            
            private$.analysisId <- analysisId
            private$.datasetId  <- datasetId
            
            if (is.null(options))
                options <- Options$new()
            private$.options <- options
            
            if (is.null(state))
                state <- State$new()
            private$.state <- state
            
            private$.info <- loadAnalysisInfo(package, name)
            
            resultsInfo <- loadResultsInfo(package, name)
            private$.results <- silkycore::Results$new(resultsInfo, options, self)
            
            private$.options$addChangeListener(private$.optionsChangedHandler)
        },
        check=function() {
            silkycore::check(private$.package, private$.name, private$.options)
        },
        init=function() {
            if (private$.status != "none")
                return()
            
            dataOptionName <- NULL
            
            for (option in private$.info$options) {
                if (option$type == "Dataset"
                    && is.null(private$.options$get(option$name))
                    && is.null(option$default)) {
                    
                    dataOptionName <- option$name
                    dataset <- self$readDataset(headerOnly=TRUE)
                    private$.options$setValue(option$name, dataset)
                    break()
                }
            }
            
            self$check()
            self$results$.update()
            
            #self$.readState()
            
            private$.init()
            
            if ( ! is.null(dataOptionName))
                private$.options$setValue(dataOptionName, NULL)
            
            private$.status <- "inited"
        },
        run=function() {
            
            dataOptionName <- NULL

            for (option in private$.info$options) {
                if (option$type == "Dataset"
                    && is.null(private$.options$get(option$name))
                    && is.null(option$default)) {
                    
                    dataset <- self$readDataset(headerOnly=FALSE)
                    private$.options$setValue(option$name, dataset)
                    break()
                }
            }
            
            self$check()
            self$results$.update()
            private$.status <- "running"
            private$.run()
            
            if ( ! is.null(dataOptionName))
                private$.options$setValue(dataOptionName, NULL)
            
            private$.status <- "complete"
            
            #self$.saveState()
        },
        print=function() {
            self$init()
            self$results$print()
        },
        render=function() {
            private$.results$.render()
        },
        .render=function(funName, width, height, name, key, ...) {
            
            render <- private[[funName]]
            
            if (is.function(render) && is.function(private$.resourcesPathSource)) {
                
                paths <- private$.resourcesPathSource(name, "png")
                base::Encoding(paths$rootPath) <- 'UTF-8'
                base::Encoding(paths$relPath) <- 'UTF-8'
                
                fullPath <- paste0(paths$rootPath, '/', paths$relPath)
                print(fullPath)
                
                #multip <- .ppi / 96
                multip <- 2
                
                grDevices::png(type="cairo", filename=fullPath, width=width * multip, height=height * multip, bg="transparent", res=72 * multip)
                render(name, key)
                grDevices::dev.off()
                
                return(paths$relPath)
            }
            
            return('')
        },
        .setReadDatasetSource=function(read) {
            private$.readDataset <- read
        },
        .setReadDatasetHeaderSource=function(read) {
            private$.readDatasetHeader <- read
        },
        .setStatePathSource=function(statePath) {
            private$.statePathSource <- statePath
        },
        .setResourcesPathSource=function(resourcesPathSource) {
            private$.resourcesPathSource <- resourcesPathSource
        },
        .readState=function() {
            
            private$.state <- State$new()
            
            stateInfo <- loadStateInfo(private$.package, private$.name)
            
            try({
                if (is.function(private$.statePathSource)) {
                    statePath <- private$.statePathSource()
                    if (base::file.exists(statePath)) {
                        conn <- file(statePath, open="rb", raw=TRUE)
                        private$.state$.deserialize(conn)
                    }
                }
            })
        },
        .saveState=function() {
            
            if (is.function(private$.statePathSource)) {
                statePath <- private$.statePathSource()
                conn <- file(statePath, open="wb", raw=TRUE)
                private$.state$.serialize(conn)
                base::close(conn)
            }
        },
        readDataset=function(headerOnly=FALSE) {
            
            columns <- character()
            
            env <- self$options$values()
            
            for (option in private$.info$options) {
                
                if (option$type == "Variables" || option$type == "Variable") {
                    value <- env[[option$name]]
                    columns <- c(columns, value)
                }
            }
            
            if (headerOnly)
                dataset <- private$.readDatasetHeader(columns)
            else
                dataset <- private$.readDataset(columns)
            
            dataset
        },
        optionsChangedHandler=function(optionNames) {
            private$.status <- "none"
        },
        asProtoBuf=function() {
            
            self$init()
            initProtoBuf()
            
            response <- RProtoBuf::new(silkycoms.AnalysisResponse)
            response$datasetId  <- private$.datasetId
            response$analysisId <- self$analysisId
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
