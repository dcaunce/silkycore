
Footnotes <- R6Class("Footnotes",
    private=list(
        .notes=NA),
    active=list(
        notes=function() private$.notes),
    public=list(
        initialize=function() {
            private$.notes <- list()
        },
        clear=function() {
            private$.notes <- list()
        },
        addNote=function(message) {
            
            # adds a footnote, returns the index (0-indexed)
            
            for (i in seq_along(self$notes)) {
                if (message == self$notes[[i]])
                    return(i-1)
            }
            
            private$.notes[[length(private$.notes)+1]] <- message
            return(length(private$.notes)-1)
        })
)

Table <- R6Class("Table",
    inherit=ResultElement,
    private=list(
        .columns=list(),
        .rowCount=0,
        .rowNames=character(),
        .rowsExpr="0",
        .rowsValue="0",
        .margin=1,
        .padding=2,
        .marstr=" ",
        .padstr="  ",
        .footnotes=NULL,
        .swapRowsColumns=FALSE),
    active=list(
        width=function() {
            
            if ( ! private$.swapRowsColumns) {
                
                w <- 0
                for (column in private$.columns) {
                    if (column$visible())
                        w <- w + private$.padding + column$width() + private$.padding
                }
                
            } else {
                
                w <- private$.padding + self$.widthWidestHeader() + private$.padding
                for (i in seq_len(private$.rowCount))
                    w <- w + private$.padding + self$.widthWidestCellInRow(i)$width + private$.padding
            }
            
            max(w, nchar(self$title))
        }
    ),
    public=list(
        initialize=function(name="", index=0, options=Options(), swapRowsColumns=FALSE) {
            
            super$initialize(name=name, options=options)
            
            private$.index <- as.integer(index)
            private$.swapRowsColumns <- swapRowsColumns
            private$.columns <- list()
            private$.rowCount <- 0
            private$.rowsExpr <- "1"
            private$.rowNames <- list()
            private$.margin <- 1
            private$.marstr <- spaces(private$.margin)
            private$.padding <- 2
            private$.padstr <- spaces(private$.padding)
            private$.footnotes <- Footnotes$new()
        },
        .setDef=function(name, value) {

            if (name == "title")
                self$title = value
            else if (name == "columns")
                self$.setColumnsDef(value)
            else if (name == "rows")
                self$.setRowsDef(value)
            else
                super$.setDef(name, value)
        },
        .setRowsDef=function(value) {
            private$.rowsExpr <- paste0(value)
            private$.updated <- FALSE
        },
        .setColumnsDef=function(columnDefs) {
            
            for (columnDef in columnDefs) {
                
                if (is.null(columnDef$title))
                    columnDef$title <- columnDef$name
                if (is.null(columnDef$content))
                    columnDef$content <- "."
                if (is.null(columnDef$visible))
                    columnDef$visible <- TRUE
                
                self$addColumn(columnDef$name, columnDef$title, columnDef$content, columnDef$visible)
            }
        },
        .update=function() {
            
            if (private$.updated)
                return()
            
            error <- NULL

            rowsValue <- try(private$.options$eval(private$.rowsExpr, name=private$.name, index=private$.index), silent=TRUE)
            
            if (inherits(rowsValue, "try-error")) {
                error <- rowsValue
                rowsValue <- 0
            }
            
            if (identical(rowsValue, private$.rowsValue))
                return()
            
            private$.rowsValue <- rowsValue
            
            oldNames <- private$.rowNames
            oldRows <- self$getRows()
            
            if (is.numeric(private$.rowsValue) && private$.rowsValue > 0) {
                newNames <- paste(1:private$.rowsValue)
            } else if (is.character(private$.rowsValue)) {
                newNames <- private$.rowsValue
            } else {
                newNames <- character()
            }
            
            self$clearRows()
            
            for (i in seq_along(newNames)) {
                
                newName <- newNames[[i]]
                index <- which(oldNames == newName)
                
                if (length(index) > 0) {
                    
                    newRow <- oldRows[[ index[1] ]]
                    self$addRow(newName, newRow)
                    
                } else {
                    
                    self$addRow(newName)
                }
            }
            
            if ( ! is.null(error))
                rethrow(error)
            
            private$.updated <- TRUE
        },
        clearRows=function() {
            private$.rowNames <- list()
            for (column in private$.columns)
                column$.clear()
            private$.rowCount <- 0
            private$.footnotes$clear()
        },
        addColumn=function(name, title=name, content=".", visible=TRUE) {
            
            column <- Column$new(name=name, title=title, content=content, visible, options=private$.options)
            i <- 1
            
            while (i <= private$.rowCount) {
                rowName <- private$.rowNames[[i]]
                column$.addCell(name=rowName, index=i)
                i <- i + 1
            }
            
            private$.columns[[name]] <- column
        },
        addRow=function(name=NULL, values=NULL) {
            
            private$.rowNames[length(private$.rowNames)+1] <- list(name)
            private$.rowCount <- private$.rowCount + 1
            
            for (column in private$.columns) {
                if (column$.name %in% names(values))
                    column$.addCell(values[[column$.name]], name=name, index=private$.rowCount)
                else
                    column$.addCell(name=name, index=private$.rowCount)
            }
        },
        rowCount=function() {
            private$.rowCount
        },
        setCell=function(rowNo, col, value) {
            private$.columns[[col]]$.setCell(rowNo, value)
        },
        getCell=function(rowNo, col) {
            column <- private$.columns[[col]]
            if (is.null(column))
                stop(format("Column '{}' does not exist in the table", col), call.=FALSE)
            column$.getCell(rowNo)
        },
        getRows=function() {
            
            rows <- list()
            
            for (i in seq_len(private$.rowCount))
                rows[[i]] <- self$getRow(i)
            
            rows
        },
        getRow=function(row) {
            
            v <- list()
            
            if (is.character(row)) {
                rowNo <- match(row, private$.rowNames)
                if (is.na(index))
                    stop(format("Row '{}' does not exist in the table", row), call.=FALSE)
            } else if (is.numeric(row)) {
                rowNo <- row
            } else {
                stop(format("Table$getRow() expects a row name or a row number (character or numeric)", row), call.=FALSE)
            }
            
            if (rowNo > private$.rowCount)
                stop(format("Row '{}' does not exist in the table", row), call.=FALSE)
            
            for (column in private$.columns)
                v[[column$.name]] <- column$.getCell(rowNo)
            
            v
        },
        addFootnote=function(rowNo, colNo, note) {
            index <- private$.footnotes$addNote(note)
            private$.columns[[colNo]]$.addSup(rowNo, index)
        },
        .widthWidestCellInRow=function(row) {
            
            maxWidthWOSup <- 0
            maxSupInRow <- 0  # widest superscripts
            
            for (column in private$.columns) {
                if (column$visible()) {
                    cell <- column$.getCell(row)
                    measurements <- silkyMeasureElements(list(cell))
                    widthWOSup <- measurements$width - measurements$supwidth
                    maxWidthWOSup <- max(maxWidthWOSup, widthWOSup)
                    maxSupInRow <- max(maxSupInRow, measurements$supwidth)
                }
            }
            
            list(width=maxWidthWOSup + maxSupInRow, supwidth=maxSupInRow)
        },
        .widthWidestHeader=function() {
            width <- 0
            
            for (column in private$.columns) {
                if (column$visible())
                    width <- max(width, nchar(column$.title))
            }
            
            width
        },
        asString=function() {
            
            pieces <- character()
            
            pieces <- c(pieces, self$.titleForPrint())
            pieces <- c(pieces, self$.headerForPrint())
            i <- 1
            
            if ( ! private$.swapRowsColumns) {
                
                for (i in seq_len(private$.rowCount))
                    pieces <- c(pieces, self$.rowForPrint(i))
                
            } else {
                
                for (i in seq_along(private$.columns)) {
                    if (i == 1)
                        next()  # the first is already printed in the header
                    if (private$.columns[[i]]$visible())
                        pieces <- c(pieces, self$.rowForPrint(i))
                }
            }
            
            pieces <- c(pieces, self$.footerForPrint())
            pieces <- c(pieces, '\n')
            
            paste0(pieces, collapse="")
        },
        .titleForPrint=function() {
            
            pieces <- character()
            
            w <- nchar(self$title)
            wid <- self$width
            padright <- repstr(' ', wid - w)
            
            pieces <- c(pieces, '\n')
            pieces <- c(pieces, private$.marstr, self$title, padright, private$.marstr, '\n')
            pieces <- c(pieces, private$.marstr, repstr('\u2500', wid), private$.marstr, '\n')
            
            paste0(pieces, collapse="")
        },
        .headerForPrint=function() {
            
            pieces <- character()
            
            wid <- self$width
            pieces <- c(pieces, private$.marstr)
            
            if ( ! private$.swapRowsColumns) {
            
                for (column in private$.columns) {
                    if (column$visible())
                        pieces <- c(pieces, private$.padstr, column$.titleForPrint(), private$.padstr)
                }
                
            } else {
                
                column <- private$.columns[[1]]
                
                pieces <- c(pieces, private$.padstr, spaces(self$.widthWidestHeader()), private$.padstr)
                
                for (i in seq_len(private$.rowCount)) {
                    text <- paste(column$.getCell(i)$value)
                    rowWidth <- self$.widthWidestCellInRow(i)$width
                    w <- nchar(text)
                    pad <- spaces(max(0, rowWidth - w))
                    
                    pieces <- c(pieces, private$.padstr, text, pad, private$.padstr)
                }
            }
            
            pieces <- c(pieces, private$.marstr, '\n')
            
            pieces <- c(pieces, private$.marstr, repstr('\u2500', wid), private$.marstr, '\n')
            
            paste0(pieces, collapse="")
        },
        .footerForPrint=function() {
            
            pieces <- character()
            
            wid <- self$width
            
            pieces <- c(private$.marstr, repstr('\u2500', wid), private$.marstr, '\n')
            
            for (i in seq_along(private$.footnotes$notes)) {
                
                # determine if the corresponding superscript is visible
                
                supVisible <- FALSE
                
                for (column in private$.columns) {
                    if (column$visible()) {
                        for (cell in column$.cells) {
                            if ((i-1) %in% cell$sups) {
                                supVisible <- TRUE
                                break()
                            }
                        }
                    }
                    if (supVisible)
                        break()
                }
                
                if (supVisible) {
                
                    note <- private$.footnotes$notes[[i]]
                    
                    lines <- strwrap(note, width=(wid-private$.padding-2))
                    first <- TRUE
                    
                    for (line in lines) {
                        
                        pieces <- c(pieces, private$.marstr)
                        
                        if (first) {
                            pieces <- c(pieces, .SUPCHARS[i], ' ')
                            first <- FALSE
                        } else {
                            pieces <- c(pieces, '  ')
                        }
                        
                        pieces <- c(pieces, line, private$.marstr, '\n')
                    }
                }
            }
            
            paste0(pieces, collapse="")
        },
        .rowForPrint=function(i) {
            
            pieces <- character()
            
            pieces <- c(pieces, private$.marstr)
            
            if ( ! private$.swapRowsColumns) {
            
                for (column in private$.columns) {
                    if (column$visible()) {
                        width <- column$width()
                        pieces <- c(pieces, private$.padstr, column$.cellForPrint(i, width=width), private$.padstr)
                    }
                }
                
            } else {
                
                column <- private$.columns[[i]]
                
                width <- self$.widthWidestHeader()
                
                pieces <- c(pieces, private$.padstr, column$.titleForPrint(width), private$.padstr)
                
                for (j in seq_along(column$.cells)) {
                    widest <- self$.widthWidestCellInRow(j)
                    width <- widest$width
                    supwidth <- widest$supwidth
                    
                    cell <- column$.cells[[j]]
                    measurements <- silkyMeasureElements(list(cell))
                    measurements$width <- max(measurements$width, width)
                    measurements$supwidth  <- supwidth
                    
                    pieces <- c(pieces, private$.padstr, column$.cellForPrint(j, measurements), private$.padstr)
                }
                
            }
            
            pieces <- c(pieces, private$.marstr, '\n')
            
            paste0(pieces, collapse="")
        },
        asProtoBuf=function() {
            initProtoBuf()
            
            table <- RProtoBuf::new(silkycoms.ResultsTable)
            
            for (column in private$.columns)
                table$add("columns", column$asProtoBuf())
    
            element <- RProtoBuf::new(silkycoms.ResultsElement,
                name=.name,
                title=.title,
                table=table)
            
            element
        }
    )
)

Tables <- R6Class("Tables",
    inherit=ResultElement,
    private=list(
        .tables=NA,
        .tableNames=NA,
        .template=NA,
        .tablesExpr="0",
        .tablesValue=0),
    public=list(
        initialize=function(name="", index=0, options=Options()) {
            super$initialize(name, index, options)
        },
        get=function(name) {
            
            index <- which(name == private$.tableNames)
            if (length(index) > 0)
                table <- private$.tables[[ index[1] ]]
            else
                table <- NULL
            
            table
        },
        .setDef=function(name, value) {
            if (name == "tables")
                self$.setTablesDef(value)
            else if (name == "template")
                self$.setTemplateDef(value)
            else
                super$.setDef(name, value)
        },
        .setTemplateDef=function(templateDef) {
            private$.template <- templateDef
            private$.updated <- FALSE
        },
        .setTablesDef=function(tablesExpr) {
            private$.tablesExpr <- paste0(tablesExpr)
            private$.updated <- FALSE
        },
        .update=function() {
            
            if (private$.updated)
                return()

            if (length(private$.template) == 0)
                return()
            
            error <- NULL
            
            tablesValue <- try(private$.options$eval(private$.tablesExpr, name=private$.name, index=private$.index), silent=TRUE)
            
            if (inherits(tablesValue, "try-error")) {
                error <- tablesValue
                tablesValue <- 0
            }
            
            private$.tablesValue <- tablesValue
            
            oldNames <- private$.tableNames
            oldTables <- private$.tables
            
            if (is.numeric(private$.tablesValue) && private$.tablesValue > 0) {
                newNames <- paste(1:private$.tablesValue)
            } else if (is.character(private$.tablesValue)) {
                newNames <- private$.tablesValue
            } else {
                newNames <- character()
            }
            
            private$.tableNames <- newNames
            private$.tables <- list()
            
            for (i in seq_along(newNames)) {
                
                newName <- newNames[[i]]
                index <- which(oldNames == newName)
                
                if (length(index) > 0) {
                    
                    table <- oldTables[[ index[1] ]]
                    table$.update()
                    private$.tables[[i]] <- table
                    
                } else {
                    
                    table <- Table$new(newName, i, private$.options)
                    table$.setup(private$.template)
                    table$.update()
                    private$.tables[[i]] <- table
                }
            }
            
            if ( ! is.null(error))
                rethrow(error)
            
            private$.updated <- TRUE
        },
        clear=function() {
            private$.tableNames <- character()
            private$.tables <- list()
        },
        asString=function() {
            
            pieces <- c(' ', private$.title, '\n')
            
            for (table in private$.tables) {
                if (table$visible)
                    pieces <- c(pieces, table$asString())
            }
            
            return(paste0(pieces, collapse=""))
        },
        asProtoBuf=function() {
            initProtoBuf()
            
            group <- RProtoBuf::new(silkycoms.ResultsGroup)
            
            for (table in private$.tables)
                group$add("elements", table$asProtoBuf())
            
            RProtoBuf::new(silkycoms.ResultsElement,
                name=self$name,
                title=self$title,
                group=group)
            
        })
)


