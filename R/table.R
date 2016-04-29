
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
        .rowKeys=character(),
        .rowNames=character(),
        .rowsExpr="0",
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
                    if (column$visible)
                        w <- w + private$.padding + column$width + private$.padding
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
        initialize=function(key="", index=0, options=Options(), swapRowsColumns=FALSE) {
            
            super$initialize(key=key, options=options)
            
            private$.index <- as.integer(index)
            private$.swapRowsColumns <- swapRowsColumns
            private$.columns <- list()
            private$.rowCount <- 0
            private$.rowsExpr <- "1"
            private$.rowKeys <- list()
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
            
            newKeys <- try(private$.options$eval(private$.rowsExpr, key=private$.key, name=private$.name, index=private$.index), silent=TRUE)
            
            if (inherits(newKeys, "try-error")) {
                error <- newKeys
                newKeys <- character()
            } else if (is.character(newKeys)) {
                # all good
            } else if (is.numeric(newKeys) && newKeys[1] > 0) {
                newKeys <- paste(1:newKeys)
            } else {
                newKeys <- character()
            }
            
            if (identical(newKeys, private$.rowKeys))
                return()
            
            oldKeys <- private$.rowKeys
            oldRows <- self$getRows()
            
            self$clearRows()
            
            for (i in seq_along(newKeys)) {
                
                newKey <- newKeys[[i]]
                index <- which(oldKeys == newKey)
                
                if (length(index) > 0) {
                    
                    newRow <- oldRows[[ index[1] ]]
                    self$addRow(newKey, newRow)
                    
                } else {
                    
                    self$addRow(newKey)
                }
            }
            
            private$.rowKeys <- newKeys
            
            if ( ! is.null(error))
                rethrow(error)
            
            private$.updated <- TRUE
        },
        clearRows=function() {
            private$.rowKeys <- list()
            for (column in private$.columns)
                column$clear()
            private$.rowCount <- 0
            private$.footnotes$clear()
        },
        addColumn=function(name, title=name, content=".", visible=TRUE) {
            
            column <- Column$new(name=name, title=title, content=content, visible, options=private$.options)
            i <- 1
            
            while (i <= private$.rowCount) {
                rowKey <- private$.rowKeys[[i]]
                column$addCell(name=rowKey, index=i)
                i <- i + 1
            }
            
            private$.columns[[name]] <- column
        },
        addRow=function(key=NULL, values=NULL) {
            
            private$.rowKeys[length(private$.rowKeys)+1] <- list(key)
            private$.rowCount <- private$.rowCount + 1
            
            for (column in private$.columns) {
                if (column$name %in% names(values))
                    column$addCell(values[[column$name]], key=key, index=private$.rowCount)
                else
                    column$addCell(key=key, index=private$.rowCount)
            }
        },
        rowCount=function() {
            private$.rowCount
        },
        setCell=function(rowNo, col, value) {
            private$.columns[[col]]$setCell(rowNo, value)
        },
        getCell=function(rowNo, col) {
            column <- private$.columns[[col]]
            if (is.null(column))
                stop(format("Column '{}' does not exist in the table", col), call.=FALSE)
            column$getCell(rowNo)
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
                rowNo <- match(row, private$.rowKeys)
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
                v[[column$name]] <- column$getCell(rowNo)
            
            v
        },
        addFootnote=function(rowNo, colNo, note) {
            index <- private$.footnotes$addNote(note)
            private$.columns[[colNo]]$addSup(rowNo, index)
        },
        .widthWidestCellInRow=function(row) {
            
            maxWidthWOSup <- 0
            maxSupInRow <- 0  # widest superscripts
            
            for (column in private$.columns) {
                if (column$visible) {
                    cell <- column$getCell(row)
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
                if (column$visible)
                    width <- max(width, nchar(column$title))
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
                    if (private$.columns[[i]]$visible)
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
                    if (column$visible)
                        pieces <- c(pieces, private$.padstr, column$.titleForPrint(), private$.padstr)
                }
                
            } else {
                
                column <- private$.columns[[1]]
                
                pieces <- c(pieces, private$.padstr, spaces(self$.widthWidestHeader()), private$.padstr)
                
                for (i in seq_len(private$.rowCount)) {
                    text <- paste(column$getCell(i)$value)
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
                    if (column$visible) {
                        for (cell in column$cells) {
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
                    if (column$visible) {
                        width <- column$width
                        pieces <- c(pieces, private$.padstr, column$.cellForPrint(i, width=width), private$.padstr)
                    }
                }
                
            } else {
                
                column <- private$.columns[[i]]
                
                width <- self$.widthWidestHeader()
                
                pieces <- c(pieces, private$.padstr, column$.titleForPrint(width), private$.padstr)
                
                for (j in seq_along(column$cells)) {
                    widest <- self$.widthWidestCellInRow(j)
                    width <- widest$width
                    supwidth <- widest$supwidth
                    
                    cell <- column$cells[[j]]
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
                name=private$.name,
                title=self$title,
                table=table)
            
            element
        }
    )
)

