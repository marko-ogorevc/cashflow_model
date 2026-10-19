# Version 1 scenario format. New settings are optional for backwards compatibility.
scenario_number <- function(value, name, default=NULL, lower=-Inf, upper=Inf,
                            integer=FALSE) {
  if (is.null(value)) value <- default
  v <- suppressWarnings(as.numeric(value))
  if (length(v)!=1L || !is.finite(v) || v<lower || v>upper ||
      (integer && v!=round(v)))
    stop(paste0("Neveljavna nastavitev: ",name,"."))
  v
}

scenario_choice <- function(value, name, choices, default=NULL) {
  if (is.null(value)) value <- default
  if (!is.character(value) || length(value)!=1L ||
      is.na(value) || !(value %in% choices))
    stop(paste0("Neveljavna nastavitev: ",name,"."))
  value
}

scenario_boolean <- function(value, name, default=FALSE) {
  if (is.null(value)) value <- default
  if (!is.logical(value) || length(value)!=1L || is.na(value))
    stop(paste0("Neveljavna nastavitev: ",name,"."))
  value
}

# The internal timeline is recreated on import, never persisted.
scenario_valid_events <- function(events, horizon) {
  if (!is.list(events) || length(events)>200L)
    stop("Neveljaven seznam dogodkov.")
  lapply(events,function(e) {
    if (!is.list(e) || !all(c("event_name","event_time","event_cashflow") %in% names(e)))
      stop("Dogodek nima zahtevanih polj.")
    nm <- e$event_name
    years <- unlist(e$event_time,use.names=FALSE)
    amount <- unlist(e$event_cashflow,use.names=FALSE)
    if (!is.character(nm) || length(nm)!=1L || is.na(nm) ||
        !nzchar(trimws(nm)) || identical(nm,".internal_timeline") ||
        !is.numeric(years) || !length(years) || length(years)>1000L ||
        any(!is.finite(years)) || any(years!=round(years)) ||
        any(years<0 | years>horizon) ||
        !is.numeric(amount) || length(amount)!=1L || !is.finite(amount))
      stop("Dogodek vsebuje neveljavne podatke ali leta zunaj načrta.")
    list(event_name=nm,event_time=as.numeric(years),event_cashflow=as.numeric(amount))
  })
}

scenario_build <- function(events,horizon,display,returns,preferences) {
  list(format="cashflow-model",version=1,horizon=horizon,
       events=events,display=display,returns=returns,
       preferences=preferences)
}

# Validate and normalize everything before updating any Shiny inputs.
# Missing fields from older version-1 files get documented default values.
scenario_parse <- function(model) {
  if (!is.list(model) || !identical(model$format,"cashflow-model") ||
      !identical(suppressWarnings(as.numeric(model$version)),1))
    stop("Datoteka ni podprt scenarij Cashflow Model.")
  h <- scenario_number(model$horizon,"trajanje načrta",lower=1,upper=150,integer=TRUE)
  events <- scenario_valid_events(model$events,h)
  d <- model$display
  if (is.null(d)) d <- list()
  if (!is.list(d)) stop("Neveljavne nastavitve časovne osi.")
  display <- list(
    mode=scenario_choice(d$mode,"časovna os",c("relative","age","calendar"),"relative"),
    starting_age=scenario_number(d$starting_age,"starost",30,0,120),
    starting_year=scenario_number(d$starting_year,"začetno leto",2026,1900,2200)
  )
  r <- model$returns
  if (is.null(r)) r <- list()
  if (!is.list(r)) stop("Neveljavne predpostavke donosnosti.")
  returns <- list(
    r0=scenario_number(r$r0,"začetna donosnost",.05,0,.1),
    r1=scenario_number(r$r1,"vmesna donosnost",.025,0,.1),
    r2=scenario_number(r$r2,"končna donosnost",.01,0,.1),
    t0=scenario_number(r$t0,"prvo leto",50,-10000,10000),
    t1=scenario_number(r$t1,"drugo leto",60,-10000,10000),
    t2=scenario_number(r$t2,"tretje leto",80,-10000,10000)
  )
  p <- model$preferences
  if (is.null(p)) p <- list()
  if (!is.list(p)) stop("Neveljavne dodatne nastavitve.")
  preferences <- list(
    model_mode=scenario_choice(p$model_mode,"način modeliranja",
                               c("simple","advanced"),"simple"),
    asset_strategy=scenario_choice(p$asset_strategy,"naložbena strategija",
                                   c("glide","equities","balanced","bonds"),"glide"),
    stock_begin=scenario_number(p$stock_begin,"začetni delež delnic",80,0,100),
    stock_finish=scenario_number(p$stock_finish,"končni delež delnic",20,0,100),
    borrowing_rate=scenario_number(p$borrowing_rate,"obrestna mera dolga",6,0,20),
    inflation=scenario_number(p$inflation,"inflacija",2,-10,30),
    real_values=scenario_boolean(p$real_values,"realne vrednosti",FALSE),
    amount_frequency=scenario_choice(p$amount_frequency,"pogostost zneskov",
                                     c("annual","monthly"),"annual")
  )
  scenario_build(events,h,display,returns,preferences)
}
