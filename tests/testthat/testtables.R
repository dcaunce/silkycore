
age <- c(16, 18, 13, 15, 20)
gender <- as.factor(c("male", "female", "male", "female", "female"))
iq <- c(100, 123, 100, 86, 94)
ses <- as.factor(c("low", "low", "high", "high", "low"))

dataset <- data.frame(age=age, gender=gender, iq=iq, ses=ses)

options <- Options$new()
options$set(vars=c("age", "gender", "iq"))
options$set(dataset=dataset)

columns <- list(
    list(
        name="var",
        title="Var",
        content="(key)"
    ),
    list(
        name="value",
        title="Value"
    )
)

table <- Table$new(options=options)
table$.setDef("title", "Bruce")
table$.setDef("columns", columns)
table$.update()

table$.setDef("rows", "(notexist)")
expect_error(table$.update(), "Could not evaluate '(notexist)'\n    object 'notexist' not found", fixed=TRUE)
table$.setDef("rows", "(vars)")
expect_silent(table$.update())
expect_equal(table$rowCount(), 3)

table$setCell(1, "value", 16)
table$setCell(2, "value", "male")
table$setCell(3, "value", 100)

expect_equal(table$getCell(1, 1)$value, "age")
expect_equal(table$getCell(2, 1)$value, "gender")
expect_equal(table$getCell(3, 1)$value, "iq")

expect_equal(table$getCell(1, "var")$value, "age")
expect_equal(table$getCell(2, "var")$value, "gender")
expect_equal(table$getCell(3, "var")$value, "iq")

expect_equal(table$getCell(1, "value")$value, 16)
expect_equal(table$getCell(2, "value")$value, "male")
expect_equal(table$getCell(3, "value")$value, 100)

row <- table$getRow(1)

expect_equal(length(row), 2)
expect_equal(row$var$value, "age")
expect_equal(row$value$value, 16)

expect_error(table$getCell(1, "doesnt-exist")$value, "Column 'doesnt-exist' does not exist in the table", fixed=TRUE)
expect_error(table$getCell(5, "var")$value, "Row '5' does not exist in the table", fixed=TRUE)

options$set(vars=c("age", "gender", "ses"))

expect_equal(table$rowCount(), 3)

# TABLES

tables <- Group$new(options=options)
tables$.setDef("tables", "(vars)")

template <- list(
    title="(name)",
    columns=list(
        list(name="1st", title="1th")
    )
)

tables$.setDef("template", template)
#show(tables)

