# Model validation

These are regression/reference tests. They deliberately use independent financial formulas as benchmarks.

From the project root run:

```r
install.packages("testthat")
testthat::test_file("tests/testthat/test-model.R")
```

Covered: fixed and changing interest rates; single deposit; ordinary annuity; zero-interest withdrawals; inverse annual contribution; inverse period; time-label translation.

**Conventions:** initial investment at t=0 compounds through t=10; recurring contributions at t=1,...,10 happen at the ends of the respective periods. The model applies a return to the prior period's balance, then the period's cashflow.

**Caution:** Tests have been committed but not executed in a local R runtime by the assistant. Treat any failing tests as diagnostic findings, not as grounds for silently changing financial conventions. More tests are needed for negative rates, duplicate event names, events outside the horizon, and inverse solutions with changing returns.
