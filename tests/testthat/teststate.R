
options <- Options$new()
options$set(vars=c("a", "b", "c"))

state <- State$new(options=options)
state$.setChildrenExpr("(vars)")
state$.update()

state$getChild("a")$setValue("value", "a")
state$getChild("b")$setValue("value", "b")
state$getChild("c")$setValue("value", "c")
state$setValue("value", "d")

options$set(vars=c("b", "a"))
state$.update()

expect_equal("a", state$getChild("a")$getValue("value"))
expect_equal("b", state$getChild("b")$getValue("value"))
expect_equal(NULL, state$getChild("c"))


state2 <- State$new()
state2$.deserialize(state$.serialize())

expect_identical(state$.serialize(), state2$.serialize())

