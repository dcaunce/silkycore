
Image <- R6Class("Image",
    inherit=ResultElement,
    private=list(
        .state=NA
    ),
    active=list(
        state=function() {
            if (class(private$.state) != "State")
                private$.state <- State$new()
            private$.state
        }
    ),
    public=list(
        initialize=function(name="", index=0, options=Options()) {
            super$initialize(name, index, options)
        },
        render=function() {
            
        },
        asString=function() {
            
            pieces <- c(' ', private$.title, '\n')
            pieces <- c(pieces, "IMAGE")
            
            return(paste0(pieces, collapse=""))
        },
        asProtoBuf=function() {
            initProtoBuf()
            
            group <- RProtoBuf::new(silkycoms.ResultsGroup)
            
            for (item in private$.items)
                group$add("elements", item$asProtoBuf())
            
            RProtoBuf::new(silkycoms.ResultsElement,
                name=self$name,
                title=self$title,
                group=group)
            
        })
)
