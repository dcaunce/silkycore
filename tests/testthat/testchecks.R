
options(warn=-1)

expect_silky_error <- function(object, message, code=NULL) {

    expect_error(eval(quote(object)), message, fixed=TRUE)

    err <- try(eval(quote(object)), silent=TRUE)
    expect_error(err)
    condition <- attr(err, "condition")
    
    expect_equal(condition$code, code)
}


# checkDataset

expect_silent(checkDataset(data.frame()))
expect_error(checkDataset("fred"), "Argument 'dataset' must be a data.frame")
expect_error(checkDataset(NULL), "Argument 'dataset' must be a data.frame")



# checkBool

expect_silent(checkBool(TRUE))
expect_silent(checkBool(FALSE))

expect_silky_error(checkBool("fred", list(name="on")),
    "Argument 'on' must be either TRUE or FALSE",
    "a_must_be_true_or_false")



# checkInt

expect_silent(checkInt( 15, list(name="count")))
expect_silent(checkInt(-20, list(name="count")))
expect_silent(checkInt( -5, list(name="count", min=-10)))
expect_silent(checkInt(  5, list(name="count", min=4)))
expect_silent(checkInt(  0, list(name="count", min=-10, max=10)))

expect_silky_error(checkInt(13.00001, list(name="count")),        
    "Argument 'count' must be a whole number",
    "a_must_be_an_integer")
expect_silky_error(checkInt(-5, list(name="count", min=0)),
    "Argument 'count' must be greater than or equal to 0",
    "a_must_be_greater_than_or_equal_to_b")
expect_silky_error(checkInt(15, list(name="count", max=10)),
    "Argument 'count' must be less than or equal to 10",
    "a_must_be_less_than_or_equal_to_b")



# checkVariables

dataset <- data.frame(x=runif(5), y=runif(5))

expect_silent(checkVariables(c(),         list(name="dependent"), dataset))
expect_silent(checkVariables(c("x"),      list(name="dependent"), dataset))
expect_silent(checkVariables(c("x", "y"), list(name="dependent"), dataset))

expect_silky_error(checkVariables(c(7, 2),     list(name="dependent"), dataset),
    "Argument 'dependent' must be a character vector",
    "a_is_not_a_string")
expect_silky_error(checkVariables(c("z"),      list(name="dependent"), dataset),
    "Argument 'dependent' contains 'z' which is not present in the dataset",
    "a_is_not_in_b")
expect_silky_error(checkVariables(c("z", "a"), list(name="dependent"), dataset),
    "Argument 'dependent' contains 'z', 'a' which are not present in the dataset",
    "a_are_not_in_b")


dataset <- data.frame(x=as.factor(rep(1:2, 5)), y=runif(10))

expect_silent(checkVariables(c("x"), list(name="factor"),                            dataset))
expect_silent(checkVariables(c("y"), list(name="dependent", permitted="continuous"), dataset))
expect_silent(checkVariables(c("x"), list(name="factor",    permitted="nominal"),    dataset))
expect_silent(checkVariables(c("y"), list(name="dependent", permitted="nominal|ordinal|continuous"), dataset))

expect_silky_error(checkVariables(c("x"), list(name="dependent", permitted="continuous"), dataset),
    "Argument 'dependent' specifies column 'x' which is (and must not be) a factor",
    "b_is_wrong_measure_type")
expect_silky_error(checkVariables(c("y"), list(name="factor", permitted="nominal"), dataset),
    "Argument 'factor' specifies column 'y' which is (and must not be) numeric",
    "b_is_wrong_measure_type")


dataset <- data.frame(x=c(NA, 1:4), y=c(Inf, 1:4))

expect_silent(checkVariables(c("x"), list(name="independent"), dataset))
expect_silent(checkVariables(c("y"), list(name="dependent", rejectInf=FALSE), dataset))

expect_silky_error(checkVariables("x", list(name="independent", rejectMissing=TRUE), dataset),
    "Argument 'independent' specifies column 'x' which contains (and must not) missing values (NAs)",
    "b_contains_missing_values")
expect_silky_error(checkVariables("y", list(name="dependent"), dataset),
    "Argument 'dependent' specifies column 'y' which contains (and must not) infinite values",
    "b_contains_infinite_values")




