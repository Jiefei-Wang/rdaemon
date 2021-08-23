test_that("existsGlobalVariable",{
    expect_false(existsGlobalVariable("testGlobals"))
}
)


test_that("setGlobalVariable",{
    setGlobalVariable("testGlobals", 1)
    expect_true(existsGlobalVariable("testGlobals"))
}
)

test_that("getGlobalVariable",{
    expect_equal(getGlobalVariable("testGlobals"), 1)
}
)

test_that("unsetGlobalVariable",{
    unsetGlobalVariable("testGlobals")
    expect_false(existsGlobalVariable("testGlobals"))
}
)
