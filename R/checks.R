
check <- function(packageName, analysisName, options) {
    
    env <- options$values()

    info <- loadAnalysisInfo(packageName, analysisName)
    
    dataset <- options$dataset()

    checkDataset(dataset)
    
    for (opt in info$options) {
        
        value <- env[[opt$name]]
                
        if (opt$type == "Bool") {
            checkBool(value, opt, dataset)
        } else if (opt$type == "Int") {
            checkInt(value, opt, dataset)
        } else if (opt$type == "Variables") {
            checkVariables(value, opt, dataset)
        } else if (opt$type == "List") {
            checkList(value, opt, dataset)
        }
    }
}

checkDataset <- function(dataset) {

    if (inherits(dataset, "data.frame") == FALSE)
        reject("Argument 'dataset' must be a data.frame")
}

checkBool <- function(value, info, dataset=NULL) {

    if (length(value) == 1 && value != FALSE && value != TRUE)
        reject("Argument '{a}' must be either TRUE or FALSE", code="a_must_be_true_or_false", a=info$name)
}

checkList <- function(value, info, dataset=NULL) {
    
    if ( ! (value %in% info$options)) {
        options <- paste("'", info$options, "'", collapse=", ", sep="")
        reject("Argument '{a}' must be one of {options}", code="a_must_be_one_of", a=info$name, options=options)
    }
}

checkInt <- function(value, info, dataset=NULL) {

    if (is.numeric(value) == FALSE)
        reject("Argument '{a}' must be numeric", code="a_must_be_an_integer", a=info$name)
    if (is.integer(value) == FALSE && abs(value - as.integer(value)) > .000000001)
        reject("Argument '{a}' must be a whole number", code="a_must_be_an_integer", a=info$name)
    if ("min" %in% names(info) && value <= info$min)
        reject("Argument '{a}' must be greater than or equal to {b}", code="a_must_be_greater_than_or_equal_to_b", a=info$name, b=info$min)
    if ("max" %in% names(info) && value >= info$max)
        reject("Argument '{a}' must be less than or equal to {b}", code="a_must_be_less_than_or_equal_to_b", a=info$name, b=info$max)
}

checkVariables <- function(value, info, dataset) {

    if (length(value) == 0)
        return()

    if (is.character(value) == FALSE)
        reject("Argument '{a}' must be a character vector", code="a_is_not_a_string", a=info$name)

    notInDataset <- value[ ! (value %in% names(dataset))]
    if (length(notInDataset) == 1) {
    
        reject("Argument '{a}' contains '{b}' which is not present in the dataset", code="a_is_not_in_b", a=info$name, b=notInDataset)
        
    } else if (length(notInDataset) > 1) {
    
        b <- paste(paste0("'", notInDataset, "'"), collapse=", ")
        reject("Argument '{a}' contains {b} which are not present in the dataset", code="a_are_not_in_b", a=info$name, b=b)
    }
    
    if ("permitted" %in% names(info)) {
    
        permitted <- strsplit(info$permitted, "|", fixed=TRUE)[[1]]
        
        for (columnName in value) {
        
            column <- dataset[[columnName]]
            type <- columnType(column)
            
            if ((type %in% permitted) == FALSE)
                reject("Argument '{a}' specifies column '{b}' which is (and must not be) {c}", code="b_is_wrong_measure_type", a=info$name, b=columnName, c=columnTypeRDescription(column))
        }
    }
    
    if (is.null(info$rejectInf) || info$rejectInf == TRUE) {  # Infs rejected by default
    
        for (columnName in value) {
        
            column <- dataset[[columnName]]
            if (any(is.infinite(column)))
                reject("Argument '{a}' specifies column '{b}' which contains (and must not) infinite values", code="b_contains_infinite_values", a=info$name, b=columnName)
        }        
    }
    
    if (is.null(info$rejectMissing) == FALSE && info$rejectMissing) {  # missings not rejected by default
    
        for (columnName in value) {
        
            column <- dataset[[columnName]]
            if (any(is.na(column)))
                reject("Argument '{a}' specifies column '{b}' which contains (and must not) missing values (NAs)", code="b_contains_missing_values", a=info$name, b=columnName)
        }        
    }
}