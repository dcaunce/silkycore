
Cell <- R6Class(
    "Cell",
    public=list(
        value=NA,
        sups=numeric(),
        initialize=function(v=NA) {
            self$value <- v
            self$sups <- numeric()
        },
        setValue=function(v) {
            self$value <- v
        },
        addSup=function(sup) {
            self$sups <- c(self$sups, sup)
        },
        asProtoBuf=function() {
            initProtoBuf()
            cell <- RProtoBuf::new(silkycoms.ResultsCell)
            
            vc <- class(self$value)
            
            if (vc == "integer") {
                if (self$value == -2147483647)
                    cell$o <- silkycoms.Cell.Other.MISSING
                else
                    cell$i <- self$value
            } else if (vc == "numeric")
                cell$d <- self$value
            else if (vc == "")
                cell$s <- self$value
            else
                cell$o <- silkycoms.Cell.Other$NOT_A_NUMBER
            
            cell$footnotes <- self$sups
            
            cell
        })
)

Column <- R6Class("Column",
    private=list(
        .name="",
        .title="",
        .cells = list(),
        .format = "",
        .width = 0,
        .measures=list(),
        .measured=FALSE,
        .contentExpr=".",
        .visibleExpr="TRUE",
        .options=NULL),
    active=list(
        name=function() private$.name,
        title=function() private$.title,
        cells=function() private$.cells,
        width=function() {
            if ( ! private$.measured)
                self$.measure()
            private$.width
        },
        visible=function() {
            private$.options$eval(private$.visibleExpr)
        }),
    public=list(
        initialize=function(name, title=name, content=".", visible=TRUE, options=Options()) {
            private$.name <- name
            private$.title <- title
            
            private$.measured <- FALSE
            private$.cells <- list()
            
            private$.visibleExpr <- paste0(visible)
            private$.contentExpr <- content
            
            private$.options <- options
        },
        addCell=function(value=NA, ...) {
            
            if (is.na(value))
                value <- private$.options$eval(private$.contentExpr, ...)
            
            if (inherits(value, "Cell"))
                cell <- value
            else
                cell <- Cell$new(value)
            
            private$.cells[[length(private$.cells)+1]] <- cell
            private$.measured <- FALSE
        },
        setCell=function(row, value) {
            if (row > length(private$.cells))
                stop(format("Row '{}' does not exist in the table", row), call.=FALSE)
            private$.cells[[row]]$setValue(value)
            private$.measured <- FALSE
        },
        getCell=function(row) {
            if (row > length(private$.cells))
                stop(format("Row '{}' does not exist in the table", row), call.=FALSE)
            private$.cells[[row]]
        },
        clear=function() {
            private$.cells <- list()
            private$.measured <- FALSE
        },
        addSup=function(row, sup) {
            private$.cells[[row]]$addSup(sup)
            private$.measured <- FALSE
        },
        .measure=function() {
            titleWidth <- nchar(private$.title)
            private$.measures <- silkyMeasureElements(private$.cells)
            private$.width <- max(private$.measures$width, titleWidth)
            private$.measured <- TRUE
        },
        .titleForPrint=function(width=NULL) {
            
            if (is.null(width))
                width <- self$width
            w <- nchar(private$.title)
            pad <- spaces(max(0, width - w))
            
            paste0(private$.title, pad)
        },
        .cellForPrint=function(i, measures=NULL, width=NA) {
            if ( ! private$.measured)
                self$.measure()
            
            if (is.null(measures))
                measures <- private$.measures
            
            if ( ! is.na(width))
                measures$width <- width
            
            silkyFormatElement(private$.cells[[i]],
                w=measures$width,
                dp=measures$dp,
                sf=measures$sf,
                expw=measures$expwidth,
                supw=measures$supwidth)
        },
        asProtoBuf=function() {
            initProtoBuf()
            
            column <- RProtoBuf::new(silkycoms.ResultsColumn,
            name=private$.name,
                title=private$.title,
                format=private$.format)
            
            for (cell in private$.cells)
                column$add("cells", cell$asProtoBuf())
            
            column
        }
    )
)
