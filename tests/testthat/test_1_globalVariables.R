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

test_that("setGlobalVariableWithLongName",{
    name <- paste0(sample(letters, getNameMaxLen(), replace = TRUE), collapse = "")
    setGlobalVariable(name, 1)
    expect_true(existsGlobalVariable(name))
    unsetGlobalVariable(name)
}
)