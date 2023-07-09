test_that("ensure_date() catches Date errors", {
    expect_error(ensure_date("A"), "Supply a date.*")
    expect_error(ensure_date(5), "Supply a date.*")
})

test_that("previous_sunday() works for Sundays", {
    expect_equal(previous_sunday(as.Date("2023-07-09")), as.Date("2023-07-02"))
})

test_that("previous_sunday() works for non-Sundays", {
    expect_equal(previous_sunday(as.Date("2023-07-10")), as.Date("2023-07-09"))
    expect_equal(previous_sunday(as.Date("2023-07-15")), as.Date("2023-07-09"))
})

test_that("get_Advent_start() works", {
    expect_equal(get_Advent_start(2022), as.Date("2022-11-27"))
    expect_equal(get_Advent_start(2023), as.Date("2023-12-03"))
    expect_equal(get_Advent_start(2024), as.Date("2024-12-01"))
    expect_equal(get_Advent_start(2025), as.Date("2025-11-30"))
})

start2022 = as.Date("2022-11-27")
start2023 = as.Date("2023-12-03")
start2024 = as.Date("2024-12-01")
test_that("get_kalendar_start() works for dates after Advent's beginning", {
    expect_equal(get_kalendar_start(as.Date("2022-11-27")), start2022)
    expect_equal(get_kalendar_start(as.Date("2022-11-30")), start2022)
    expect_equal(get_kalendar_start(as.Date("2023-12-03")), start2023)
    expect_equal(get_kalendar_start(as.Date("2023-12-10")), start2023)
})

test_that("get_kalendar_start() works for dates before Advent's beginning", {
    expect_equal(get_kalendar_start(as.Date("2023-10-03")), start2022)
    expect_equal(get_kalendar_start(as.Date("2025-10-30")), start2024)
})
