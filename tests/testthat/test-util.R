test_that("previous_sunday() catches Date errors", {
    expect_error(previous_sunday("A"), ".*needs a date.*")
    expect_error(previous_sunday(5), ".*needs a date.*")
})
