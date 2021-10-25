test_that("existsGlobalVariable",{
    expect_false(existsGlobalVariable("rdaemon"))
}
)


test_that("setGlobalVariable",{
    setGlobalVariable("rdaemon", 1)
    expect_true(existsGlobalVariable("rdaemon"))
}
)

test_that("getGlobalVariable",{
    expect_equal(getGlobalVariable("rdaemon"), 1)
}
)

test_that("unsetGlobalVariable",{
    unsetGlobalVariable("rdaemon")
    expect_false(existsGlobalVariable("rdaemon"))
}
)

test_that("setGlobalVariableWithLongName",{
    name <- truncateLongName(
        paste0(sample(letters, getNameMaxLen(), replace = TRUE), collapse = ""),
        warn = FALSE
        )
    setGlobalVariable(name, 1)
    expect_true(existsGlobalVariable(name))
    unsetGlobalVariable(name)
}
)