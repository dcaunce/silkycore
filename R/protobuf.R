
initProtoBuf <- function() {

    library("RProtoBuf")
    
    resultsProtoPath <- system.file("silkycoms.proto", package="silkycore")
    if (resultsProtoPath == "")
        resultsProtoPath <- system.file("inst", "silkycoms.proto", package="silkycore")
    if (resultsProtoPath == "")
        stop("silkycore silkycoms.proto not found!", call.=FALSE)
    
    RProtoBuf::readProtoFiles(resultsProtoPath)
}
