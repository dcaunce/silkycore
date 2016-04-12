
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
    private = list(
        .format = "",
        .width = 0,
        .measures=list(),
        .measured=FALSE,
        .contentExpr=".",
        .visibleExpr="TRUE",
        .options=NULL),
    public = list(
        .name="",
        .title="",
        .cells = list(),
        initialize=function(name, title=name, content=".", visible=TRUE, options=Options()) {
            self$.name <- name
            self$.title <- title
            
            private$.measured <- FALSE
            self$.cells <- list()
            
            private$.visibleExpr <- paste0(visible)
            private$.contentExpr <- content
            
            private$.options <- options
        },
        .addCell=function(value=NA, ...) {
            
            if (is.na(value))
                value <- private$.options$eval(private$.contentExpr, ...)
            
            if (inherits(value, "Cell"))
                cell <- value
            else
                cell <- Cell$new(value)
            
            self$.cells[[length(self$.cells)+1]] <- cell
            private$.measured <- FALSE
        },
        .setCell=function(row, value) {
            if (row > length(self$.cells))
                stop(format("Row '{}' does not exist in the table", row), call.=FALSE)
            self$.cells[[row]]$setValue(value)
            private$.measured <- FALSE
        },
        .getCell=function(row) {
            if (row > length(self$.cells))
                stop(format("Row '{}' does not exist in the table", row), call.=FALSE)
            self$.cells[[row]]
        },
        .clear=function() {
            self$.cells <- list()
            private$.measured <- FALSE
        },
        .addSup=function(row, sup) {
            self$.cells[[row]]$addSup(sup)
            private$.measured <- FALSE
        },
        .measure=function() {
            titleWidth <- nchar(self$.title)
            private$.measures <- silkyMeasureElements(self$.cells)
            private$.width <- max(private$.measures$width, titleWidth)
            private$.measured <- TRUE
        },
        width=function() {
            if ( ! private$.measured)
                self$.measure()
            private$.width
        },
        visible=function() {
            private$.options$eval(private$.visibleExpr)
        },
        .titleForPrint=function(width=NULL) {
            
            if (is.null(width))
                width <- self$width()
            w <- nchar(self$.title)
            pad <- spaces(max(0, width - w))
            
            paste0(self$.title, pad)
        },
        .cellForPrint=function(i, measures=NULL, width=NA) {
            if ( ! private$.measured)
                self$.measure()
            
            if (is.null(measures))
                measures <- private$.measures
            
            if ( ! is.na(width))
                measures$width <- width
            
            silkyFormatElement(self$.cells[[i]],
                w=measures$width,
                dp=measures$dp,
                sf=measures$sf,
                expw=measures$expwidth,
                supw=measures$supwidth)
        },
        asProtoBuf=function() {
            initProtoBuf()
            
            column <- RProtoBuf::new(silkycoms.ResultsColumn,
                name=self$.name,
                title=self$.title,
                format=private$.format)
            
            for (cell in self$.cells)
                column$add("cells", cell$asProtoBuf())
            
            column
        }
    )
)
