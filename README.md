# Cashflow model

An interactive financial cashflow simulator built in R Shiny. Add one-off and recurring cashflows, specify time-varying annual return assumptions, and inspect projected account balances and cashflows.

## Run locally

Open `cashflow_model.Rproj` in RStudio and install the packages:

```r
install.packages(c("shiny", "bslib", "plotly", "shinyalert", "rlist",
                   "gridExtra", "tidyverse", "jsonlite"))
shiny::runApp()
```

## How to use

1. Enter an event label, starting year, optional ending year, and cashflow in euros. Positive values represent inflows; negative values represent outflows.
2. Choose **Add event** to add it to the calculation.
3. Adjust the three return assumptions and breakpoints.
4. Review account balance, annual cashflows and expected return in the Overview tab.
5. Use **Solve amount** to calculate the constant annual contribution for an entered period, or **Solve end** for an estimated end year, based on the events already added.

All values are illustrative; the model uses deterministic returns, with no taxes, inflation adjustment or stochastic risk modelling.

## Development status

The `redesign-v1` branch contains a first UI refresh using bslib and Bootstrap 5. Core numerical functions are inherited from the December 2023 prototype, and have **not yet been validated with automated tests**. Review numerical edge cases before relying on results. Existing hosted Shiny deployments are not updated automatically when this branch changes.
