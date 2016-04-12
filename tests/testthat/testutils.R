
expect_equal(format("fish{}", "finger"), "fishfinger")
expect_equal(format("the {fish} was {delish}", fish="fish", delish="delish"), "the fish was delish")
expect_equal(format("the {1} goes before {0}", 7, 4), "the 4 goes before 7")
expect_equal(format("it simply isn't {}", TRUE), "it simply isn't true")
expect_equal(format("that's patently {}", FALSE, context="R"), "that's patently FALSE")

expect_equal(format("the {fish} was {delish}", fish="fish"), "the fish was {delish}")

expect_equal(silkyFormatElement(1.5, dp=4), "1.5000")
expect_equal(silkyFormatElement(-1.5, dp=4), "-1.5000")
expect_equal(silkyFormatElement(600000), "600000.00")
expect_equal(silkyFormatElement(-15000, dp=1, w=10), "  -15000.0")
expect_equal(silkyFormatElement(8e7), "8.00e+7")
expect_equal(silkyFormatElement(-2e-7), "-2.00e-7")
expect_equal(silkyFormatElement(10e7), "1.00e+8")
expect_equal(silkyFormatElement(15e7, w=10), "   1.50e+8")

expect_equal(silkyFormatElement(-15e7, w=10, expw=4), " -1.50e +8")
expect_equal(silkyFormatElement(0.8e-8, w=14, expw=8), "  8.00e     -9")

expect_equal(silkyMeasureElements(0, sf=4), list(sf=4, dp=3, width=5, expwidth=3, supwidth=0))
expect_equal(silkyMeasureElements(c(425, 70, 1), sf=4), list(sf=4, dp=3, width=7, expwidth=3, supwidth=0))
expect_equal(silkyMeasureElements(c(1024, 70, 11), sf=4), list(sf=4, dp=2, width=7, expwidth=3, supwidth=0))
expect_equal(silkyMeasureElements(c(1e8, -1e5), sf=4), list(sf=4, dp=0, width=8, expwidth=3, supwidth=0))
expect_equal(silkyMeasureElements(c(1e5, 1), sf=4), list(sf=4, dp=3, width=10, expwidth=3, supwidth=0))
expect_equal(silkyMeasureElements(c(1.1e-3, 1), sf=4), list(sf=4, dp=6, width=8, expwidth=3, supwidth=0))

expect_equal(silkyMeasureElements(c(-1.234e8, -6e66, 1), sf=4), list(sf=4, dp=3, width=10, expwidth=4, supwidth=0))
expect_equal(silkyMeasureElements(-1.34e-57, sf=3), list(sf=3, dp=0, width=9, expwidth=4, supwidth=0))


