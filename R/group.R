
Group <- R6Class("Group",
    inherit=ResultElement,
    private=list(
        .items=NA,
        .itemNames=NA,
        .itemKeys=NA,
        .template=NA,
        .itemsExpr="0",
        .itemsValue=0),
    public=list(
        initialize=function(name="", index=0, options=Options()) {
            super$initialize(name, index, options)
        },
        get=function(key) {
            
            index <- which(key == private$.itemKeys)
            if (length(index) > 0)
                item <- private$.items[[ index[1] ]]
            else
                item <- NULL
            
            item
        },
        .setDef=function(name, value) {
            if (name == "items")
                self$.setItemsDef(value)
            else if (name == "template")
                self$.setTemplateDef(value)
            else
                super$.setDef(name, value)
        },
        .setTemplateDef=function(templateDef) {
            private$.template <- templateDef
            private$.updated <- FALSE
        },
        .setItemsDef=function(itemsExpr) {
            private$.itemsExpr <- paste0(itemsExpr)
            private$.updated <- FALSE
        },
        .update=function() {
            
            if (private$.updated)
                return()

            if (length(private$.template) == 0)
                return()
            
            error <- NULL
            
            newKeys <- try(private$.options$eval(private$.itemsExpr, key=private$.key, name=private$.name, index=private$.index), silent=TRUE)
            
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
            
            oldKeys  <- private$.itemKeys
            oldItems <- private$.items
            
            private$.itemKeys <- newKeys
            private$.items <- list()
            
            for (i in seq_along(newKeys)) {
                
                newKey <- newKeys[[i]]
                index <- which(oldKeys == newKey)
                
                if (length(index) > 0) {
                    
                    item <- oldItems[[ index[1] ]]
                    item$.update()
                    private$.items[[i]] <- item
                    
                } else {
                    
                    item <- Table$new(newKey, i, private$.options)
                    item$.setup(private$.template)
                    item$.update()
                    item$.parent = self
                    private$.items[[i]] <- item
                }
            }
            
            if ( ! is.null(error))
                rethrow(error)
            
            private$.updated <- TRUE
        },
        clear=function() {
            private$.itemKeys <- character()
            private$.items <- list()
        },
        asString=function() {
            
            pieces <- c(' ', private$.title, '\n')
            
            for (item in private$.items) {
                if (item$visible)
                    pieces <- c(pieces, item$asString())
            }
            
            return(paste0(pieces, collapse=""))
        },
        asProtoBuf=function() {
            initProtoBuf()
            
            group <- RProtoBuf::new(silkycoms.ResultsGroup)
            
            for (item in private$.items) {
                if (item$visible)
                    group$add("elements", item$asProtoBuf())
            }
            
            RProtoBuf::new(silkycoms.ResultsElement,
                name=self$name,
                title=self$title,
                group=group)
            
        })
)
