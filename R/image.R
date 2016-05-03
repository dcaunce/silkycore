
Image <- R6::R6Class("Image",
    inherit=ResultElement,
    private=list(
        .state=NA,
        .path="",
        .width=400,
        .height=300,
        .renderInitFun=NA,
        .renderFun=NA),
    active=list(
        state=function() {
            if (class(private$.state) != "State")
                private$.state <- State$new()
            private$.state
        }
    ),
    public=list(
        initialize=function(key="", index=0, options=Options$new()) {
            super$initialize(key, index, options)
        },
        .render=function(path, ...) {
            if ( ! is.character(private$.renderFun))
                return()
            
            private$.path <- self$analysis$.render(
                funName=private$.renderFun,
                key=private$.key,
                name=private$.name,
                width=private$.width,
                height=private$.height,
                ...)
        },
        .renderInit=function(path, ...) {
            
        },
        asString=function() {
            
            pieces <- c(' ', private$.title, '\n')
            pieces <- c(pieces, '\n ', private$.path, '\n')
            
            return(paste0(pieces, collapse=""))
        },
        asProtoBuf=function() {
            initProtoBuf()
            
            image <- RProtoBuf::new(silkycoms.ResultsImage,
                width=private$.width,
                height=private$.height,
                path=private$.path)
            
            RProtoBuf::new(silkycoms.ResultsElement,
                name=self$name,
                title=self$title,
                image=image)
        })
)
