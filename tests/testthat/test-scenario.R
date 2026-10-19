# Run from repository root: testthat::test_file("tests/testthat/test-scenario.R")
library(testthat)
library(jsonlite)
paths <- c("R/scenario.R","../../R/scenario.R","../R/scenario.R")
scenario_path <- paths[file.exists(paths)][1]
if (is.na(scenario_path)) stop("R/scenario.R ni mogoče najti.")
source(scenario_path,local=TRUE)

sample_scenario <- function() {
  scenario_build(
    events=list(
      list(event_name="Initial",event_time=0,event_cashflow=-2000),
      list(event_name="Payments",event_time=1:12,event_cashflow=350)
    ),
    horizon=12,
    display=list(mode="age",starting_age=42,starting_year=2026),
    returns=list(r0=.06,r1=.04,r2=.025,t0=3,t1=7,t2=11),
    preferences=list(model_mode="advanced",asset_strategy="glide",
      stock_begin=75,stock_finish=15,borrowing_rate=8,
      inflation=2.5,real_values=TRUE,amount_frequency="monthly")
  )
}

test_that("saving and loading version-1 JSON preserves all model settings", {
  original <- sample_scenario()
  json <- jsonlite::toJSON(original,auto_unbox=TRUE,pretty=TRUE)
  restored <- scenario_parse(jsonlite::fromJSON(json,simplifyVector=FALSE))
  expect_equal(restored,original)
  expect_equal(restored$events[[1]]$event_time,0)
  expect_equal(restored$events[[2]]$event_time,1:12)
  expect_equal(restored$preferences$amount_frequency,"monthly")
  expect_equal(restored$preferences$borrowing_rate,8)
  expect_equal(restored$returns$t0,3)
  expect_equal(restored$display$starting_age,42)
})

test_that("older files without preferences recover the manual rates and defaults", {
  original <- sample_scenario()
  original$preferences <- NULL
  file <- jsonlite::fromJSON(jsonlite::toJSON(original,auto_unbox=TRUE),
                             simplifyVector=FALSE)
  result <- scenario_parse(file)
  expect_equal(result$preferences$model_mode,"advanced")
  expect_equal(result$returns$r0,.06)
  expect_equal(result$preferences$amount_frequency,"annual")
  expect_equal(result$preferences$borrowing_rate,6)
  expect_false(result$preferences$real_values)
})

test_that("partially saved preferences receive defaults without losing known fields", {
  original <- sample_scenario()
  original$preferences <- list(model_mode="simple",stock_begin=90)
  result <- scenario_parse(original)
  expect_equal(result$preferences$model_mode,"simple")
  expect_equal(result$preferences$stock_begin,90)
  expect_equal(result$preferences$stock_finish,20)
  expect_equal(result$preferences$asset_strategy,"glide")
  expect_equal(result$preferences$inflation,2)
})

test_that("events beyond the horizon are preserved like user-entered events", {
  original <- sample_scenario()
  original$events[[1]]$event_time <- -1
  original$events[[2]]$event_time <- 10:21
  restored <- scenario_parse(original)
  expect_equal(restored$events[[1]]$event_time,-1)
  expect_equal(restored$events[[2]]$event_time,10:21)
})

test_that("invalid or unsupported settings are rejected before UI restore", {
  bad <- sample_scenario()
  bad$preferences$borrowing_rate <- 45
  expect_error(scenario_parse(bad),"obrestna mera dolga")
  bad <- sample_scenario()
  bad$preferences$amount_frequency <- "weekly"
  expect_error(scenario_parse(bad),"pogostost zneskov")
  bad <- sample_scenario()
  bad$events[[1]]$event_cashflow <- Inf
  expect_error(scenario_parse(bad),"Dogodek")
  bad <- sample_scenario()
  bad$version <- 99
  expect_error(scenario_parse(bad),"podprt scenarij")
})
