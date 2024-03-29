
silkyC <- function(path=".") {

    inputPath <- paste(path, "inst", "silky", sep="/")
    inputFiles <- list.files(inputPath, "*.a.yaml$", full.names=TRUE)
    
    if (length(inputFiles) == 0)
        stop(paste("No .a.yaml files could be found at:", inputPath), call. = FALSE)
    
    for (filePath in inputFiles) {

        info <- yaml::yaml.load_file(filePath)

        generateHeader(path, info)
        generateBody(path, info)
        generateOptionsJS(path, info)
        generateSrcJS(path, info)
        browserifyJS(path, info)
    }
}

browserifyJS <- function(path, info) {
    
    outputPath <- paste(path, "inst", "silky", sep="/")
    if ( ! dir.exists(outputPath))
        dir.create(outputPath)
    
    cmd <- format('browserify "{ipath}/ui/{name}.src.js" --outfile "{opath}/{name}.js" --standalone module',
                  ipath=path,
                  name=tolower(info$name),
                  opath=outputPath)
    
    system(cmd)
}

generateOptionsJS <- function(path, info) {

    outputPath <- paste(path, "ui", sep="/")
    if ( ! dir.exists(outputPath))
        dir.create(outputPath)
    
    content <- "\n// This file is automatically generated, you probably don't want to edit this\n\n"
    
    options <- info$options

    content <- paste0(content, "'use strict';\n\n")
    content <- paste0(content, "module.exports = ", rjson::toJSON(options), ";")
    
    filename <- paste0(tolower(info$name), ".options.js")
    con <- file(paste(outputPath, filename, sep="/"))
    writeLines(content, con)
    close(con)
}

generateSrcJS <- function(path, info) {
    
    # only generates if the file doesn't already exist
    
    outputPath <- paste(path, "ui", sep="/")
    if ( ! dir.exists(outputPath))
        dir.create(outputPath)
    
    filename <- paste0(tolower(info$name), ".src.js")
    filepath <- paste(outputPath, filename, sep="/")
    
    if (file.exists(filepath))
        return()
    
    content <- "\n"
    
    content <- format("{content}// This file is an automatically generated template, it will not be subsequently\n", content=content)
    content <- format("{content}// overwritten by the compiler, and may be edited\n", content=content)
    
    content <- format("{content}\nvar options = require('./{name}.options')", name=tolower(info$name), content=content)
    
    con <- file(filepath)
    writeLines(content, con)
    close(con)
}

generateResults <- function(path, info) {

    outputPath <- paste(path, 'inst', 'silky', sep='/')
    if ( ! dir.exists(outputPath))
        dir.create(outputPath)
    
    #resultsProtoPath <- system.file("results.proto", package="silkycore")
    #if (resultsProtoPath == "")
    #    resultsProtoPath <- system.file("inst", "results.proto", package="silkycore")
    #
    #if (resultsProtoPath == "")
    #    stop("results.proto not found!", call.=FALSE)
    #
    #con <- file(resultsProtoPath)
    #lines <- readLines(con)
    #close(con)
    
    content <- "\n# This file is automatically generated, you probably don't want to edit this\n\n"
    
    #content <- format("{}{}", content, paste(lines, collapse='\n'))

    content <- format("{content}message {name} {\n\n", name=info$name, content=content)
    
    i <- 1

    for (option in info$options) {
    
        hasDefault <- FALSE
        if ("default" %in% names(option))
            hasDefault <- TRUE
    
        if (option$type == "Bool") {
        
            if (hasDefault)
                content <- format("{content}  optional bool {name} = {i} [default = {def}];\n", name=option$name, def=option$default, i=i, content=content)
            else
                content <- format("{content}  optional bool {name} = {i};\n", name=option$name, def=option$default, i=i, content=content)
                
            i <- i + 1
                
        } else if (option$type == "Int") {
        
            if (hasDefault)
                content <- format("{content}  optional int32 {name} = {i} [default = {def}];\n", name=option$name, def=option$default, i=i, content=content)
            else
                content <- format("{content}  optional int32 {name} = {i};\n", name=option$name, def=option$default, i=i, content=content)
                
            i <- i + 1
            
        } else if (option$type == "Variables") {
        
            content <- format("{content}  repeated string {name} = {i};\n", name=option$name, i=i, content=content)
            i <- i + 1
        }
    }
    
    content <- format("{content}\n}\n", content=content)
    
    filename <- paste0(tolower(info$name), ".proto")
    con <- file(paste(outputPath, filename, sep="/"))
    writeLines(content, con)
    close(con)
}

generateBody <- function(path, info) {

    # only generates if the file doesn't already exist
    
    outputPath <- paste(path, 'R', sep='/')
    if ( ! dir.exists(outputPath))
        dir.create(outputPath)

    filename <- paste0(tolower(info$name), ".b.R")
    filepath <- paste(outputPath, filename, sep="/")

    if (file.exists(filepath))
        return()
    
    content <- "\n"
    
    content <- format("{content}# This file is an automatically generated template, it will not be subsequently\n", content=content)
    content <- format("{content}# overwritten by the compiler, and may be edited\n\n", content=content)
    
    content <- format("{content}{name}Class <- R6Class(\n", package=info$package, name=info$name, content=content)
    content <- format("{content}  \"{name}Class\",\n", name=info$name, content=content)
    content <- format("{content}  inherit=silkycore::Analysis,\n", name=info$name, content=content)
    content <- format("{content}  public=list(", content=content)
    content <- format("{content}\n    run=function() {", content=content)
    content <- format("{content}\n      super$run()", content=content)
    content <- format("{content}\n", content=content)
    content <- format("{content}\n      # `private$.options` contains the options", content=content)
    content <- format("{content}\n      # `private$.options$dataset()` contains the data", content=content)
    content <- format("{content}\n      # `private$.results` contains the results object (to populate)", content=content)
    content <- format("{content}\n    }", content=content)
    content <- format("{content})\n", content=content)
    
    content <- format("{content})\n", content=content)

    con <- file(filepath)
    writeLines(content, con)
    close(con)
}

generateHeader <- function(path, info) {
    
    outputPath <- paste(path, 'R', sep='/')
    if ( ! dir.exists(outputPath))
        dir.create(outputPath)
    
    content <- "\n# This file is automatically generated, you probably don't want to edit this\n\n"

    content <- format("{content}{name} <- function(", name=info$name, content=content)  # function name
    
    sep <- ""

    for (option in info$options) {
    
        if ("default" %in% names(option)) {
            
            if (is.null(option$default))
                def <- "NULL"
            else if (is.logical(option$default))
                def <- ifelse(option$default, "TRUE", "FALSE")
            else if (option$type == "List")
                def <- paste0('"', option$def, '"')
            else if (option$type == "Terms")
                def <- "list()"
            else
                def <- option$default
            
            content <- format("{content}{sep}\n  {name} = {def}", name=option$name, def=def, content=content, sep=sep)
            
        } else {
            
            content <- format("{content}{sep}\n  {name}", name=option$name, content=content, sep=sep, context="R")
        }
        
        sep <- ","
    }
    
    content <- format("{content}) {\n\n", content=content)
    
    content <- format("{content}  options <- silkycore::Options$new(", content=content)
    sep <- "\n"
    for (option in info$options) {
        content <- format("{content}{sep}    {name}={name}", name=option$name, content=content, sep=sep)
        sep <- ",\n"
    }
    content <- format("{content})\n\n", content=content)

    content <- format("{content}  analysis <- {name}Class$new(package='{package}', name='{name}', options=options)\n", package=info$package, name=info$name, content=content)
    content <- format("{content}  analysis$run()\n", content=content)
    content <- format("{content}\n  analysis\n", content=content)

    content <- format("{content}}\n", content=content)
    
    filename <- paste0(tolower(info$name), ".h.R")
    con <- file(paste(outputPath, filename, sep="/"))
    writeLines(content, con)
    close(con)
}
