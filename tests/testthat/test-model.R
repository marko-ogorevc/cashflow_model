# Run from repository root: testthat::test_file("tests/testthat/test-model.R")
library(testthat)
library(tidyverse)
library(rlist)
library(ggplot2)
model_paths <- c("R/model.R", "../../R/model.R", "../R/model.R")
model_path <- model_paths[file.exists(model_paths)][1]
if (is.na(model_path)) stop("R/model.R ni mogoče najti. Zaženi test iz korena projekta ali tests/testthat.")
source(model_path, local = TRUE)

event <- function(name, years, amount) {
  list(event_name=name, event_time=years, event_cashflow=amount)
}
base <- function(end=10) list(event("baseline", 0:end, 0))
rates <- function(end=10, r=0.05) tibble(t=0:end, rate=rep(r,end+1))

test_that("constant rates are correctly generated", {
  got <- get_var_rate(c(0, 10), t0=0, r0=.05, t1=5, r1=.05, t2=10, r2=.05)$rate
  expect_equal(got$rate, rep(.05,11))
})

test_that("variable return schedule is piecewise linear", {
  got <- get_var_rate(c(0,10),t0=0,r0=.06,t1=5,r1=.04,t2=10,r2=.02)$rate
  expect_equal(got$rate[c(1,6,11)], c(.06,.04,.02), tolerance=1e-12)
})

test_that("one initial investment compounds ten periods", {
  events <- c(base(), list(event("initial",0,10000)))
  timeline <- plot_events(events,rates())$timeline
  expect_equal(tail(timeline$value_time,1),10000*1.05^10,tolerance=1e-7)
  expect_equal(tail(timeline$net_flows,1),10000,tolerance=1e-7)
  expect_equal(tail(timeline$return_effect,1),10000*(1.05^10-1),tolerance=1e-7)
})

test_that("ten end-of-year payments follow ordinary annuity formula", {
  events <- c(base(), list(event("annual",1:10,1000)))
  result <- tail(plot_events(events,rates())$timeline$value_time,1)
  expected <- 1000*((1.05^10-1)/.05)
  expect_equal(result,expected,tolerance=1e-7)
})

test_that("zero interest and negative flows yield exact balance", {
  events <- c(base(5),list(event("capital",0,5000),event("expenses",1:5,-1000)))
  t <- plot_events(events,rates(5,0))$timeline
  expect_equal(tail(t$value_time,1),0,tolerance=1e-8)
  expect_equal(tail(t$return_effect,1),0,tolerance=1e-8)
})

test_that("solve annual investment at zero interest", {
  events <- c(base(),list(event("goal",10,-10000)))
  got <- get_yearly_investment(events,event_time=1:10,discount_rate=rates(10,0))
  expect_equal(got,1000,tolerance=1e-8)
})

test_that("solve year at zero interest", {
  events <- c(base(),list(event("goal",10,-5000)))
  got <- get_period(events,event_time=1,event_cashflow=1000,discount_rate=rates(10,0))
  expect_equal(got,5)
})

test_that("calendar and age labels do not change underlying periods", {
  relative <- 0:40
  expect_equal((relative+30)-30,relative)
  expect_equal((relative+2026)-2026,relative)
})

test_that("negative balances accrue fixed borrowing interest", {
  events <- c(base(3),list(event("loan",0,-1000)))
  d <- plot_events(events,rates(3,.05),borrowing_rate=.10)$timeline
  expect_equal(d$value_time,c(-1000,-1100,-1210,-1331),tolerance=1e-8)
  expect_equal(tail(d$cumulative_debt_interest,1),331,tolerance=1e-8)
  expect_equal(tail(d$cumulative_gains,1),0,tolerance=1e-8)
})

test_that("crossing from debt to assets switches return rate next period", {
  events <- c(base(3),list(event("loan",0,-1000),event("repayment",2,1500)))
  d <- plot_events(events,rates(3,.05),borrowing_rate=.10)$timeline
  expect_equal(d$value_time,c(-1000,-1100,290,304.5),tolerance=1e-8)
  expect_equal(sum(d$debt_interest),210,tolerance=1e-8)
  expect_equal(sum(d$investment_gain),14.5,tolerance=1e-8)
})

test_that("inverse amount solver uses debt rate and reproduces zero balance", {
  events <- c(base(2),list(event("debt",0,-1000)))
  amount <- get_yearly_investment(events,1:2,rates(2,.05),borrowing_rate=.10)
  expect_equal(amount,1210/2.1,tolerance=1e-6)
  final <- plot_events(c(events,list(event("solved",1:2,amount))),
                       rates(2,.05),borrowing_rate=.10)$timeline
  expect_equal(tail(final$value_time,1),0,tolerance=1e-5)
})

test_that("inverse year solver uses debt rate and agrees with simulation", {
  events <- c(base(2),list(event("debt",0,-1000)))
  year <- get_period(events,1,700,rates(2,.05),borrowing_rate=.10)
  expect_equal(year,2)
  final <- plot_events(c(events,list(event("solved",1:year,700))),
                       rates(2,.05),borrowing_rate=.10)$timeline
  expect_gte(tail(final$value_time,1),0)
})

test_that("inverse solvers handle a switch from debt to investment earnings", {
  events <- c(base(3),list(event("debt",0,-500)))
  amount <- get_yearly_investment(events,1:3,rates(3,.08),borrowing_rate=.12)
  result <- simulate_final_balance(events,rates(3,.08),.12,1:3,amount)
  expect_equal(result,0,tolerance=1e-5)
})
