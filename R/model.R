# Calculation and plotting functions extracted unchanged from app.R.
# Validation first: do not alter formulas without a corresponding test.
##### FUNCTIONS ####

#this is the main function. Add event. It just saves the event name, event time and event cashflow to the list or json
#event cashflow can be either positive or negative. It goes into the account, e.g. savings +, 

add_event <-
  function(event_list,
           event_name,
           event_time,
           event_cashflow) {
    
    #take arguments and add the to the end of the list / json. R works best with lists.
    event_details <- list(
      event_name = event_name,
      event_time = event_time,
      event_cashflow = event_cashflow
    )
    
    event_list <- list.append(event_list, event_details)
    
    return(event_list)
    
  }

# get variable rate. Rate / discount rate need not be constant. It can chanege at 3 different times. t0 default 0 / at the begining
# t1 any time later and is t2 til the person dies.  each point has coresponding rate "r", r0, r1, r2

#arguments fo the ger variable rate function are period (min, max), ant rates (r) at times (t)
get_var_rate <- function(period = c(0, 100),
                         t0 = 0,
                         r0 = 0.05,
                         t1 = NA,
                         r1 = NA,
                         t2 = NA,
                         r2 = NA) {
  
  
  if (is.na(r1) | r1 == "") {
    t1 <- t0
    r1 <- r0
    
  }
  
  
  if (is.na(r2) | r2 == "") {
    t2 <- t1
    r2 <- r1
  }
  
  # this just adds slope to the function. it does not need to have steps.
  var_rate <- tibble(t = period[1]:period[2]) %>%
    mutate(rate = case_when(
      t <= t0 ~ r0,
      t >= t2 ~ r2,
      # this is the slope of t > t0 and t < t1
      t > t0 & t < t1 ~ r0 - (r0 - r1) * (t - t0) / (t1 - t0),
      #the slope on the other end of the function
      t >= t1 & t < t2 ~ r1 - (r1 - r2) * (t - t1) / (t2 - t1)
    ))
  
  # this is a plot. it takes var_rate data.frame / tibble and draws time t on x-axis and rate r on y-axis
  # it cannot be negative -> limits(0,NA) and labels on the y-axis are (%) per cent
  rate_plot <- ggplot(var_rate, aes(x = t, y = rate)) +
    geom_line() +
    scale_y_continuous(
      name = "Letna donosnost",
      labels = function(x)
        scales::percent(x),
      limits = c(0, NA)
    ) +
    scale_x_continuous(name = "Leto modela",
                       limits = c(period[1], period[2])) +
    theme_minimal()
  
  return(list("rate" = var_rate, "rate_plot" = rate_plot))
  
}


# Simulate the same end-of-period cashflow and debt/return rules as plot_events.
# Used by both inverse solvers without building ggplot objects.
simulate_final_balance <- function(event_list, discount_rate, borrowing_rate=0.06,
                                   extra_years=integer(), extra_cashflow=0) {
  stopifnot(length(borrowing_rate)==1, is.finite(borrowing_rate), borrowing_rate>=0)
  flows <- events_to_db(event_list)
  years <- seq(min(flows$event_time), max(flows$event_time))
  amounts <- vapply(years, function(y) sum(flows$event_cashflow[flows$event_time==y]), numeric(1))
  if (length(extra_years)) {
    if (any(!extra_years %in% years)) stop("Izbrano obdobje je zunaj časovnice.")
    amounts <- amounts + as.numeric(years %in% extra_years)*extra_cashflow
  }
  rates <- discount_rate$rate[match(years,discount_rate$t)]
  if (anyNA(rates)) stop("Manjkajo stopnje donosnosti za časovnico.")
  balance <- 0
  for (i in seq_along(years)) {
    if (i>1) balance <- balance * (1 + if (balance<0) borrowing_rate else rates[i-1])
    balance <- balance + amounts[i]
  }
  balance
}

# Find a constant additional annual flow that makes the terminal balance zero.
get_yearly_investment <- function(event_list, event_time, discount_rate,
                                  borrowing_rate=0.06) {
  event_time <- as.numeric(event_time)
  if (!length(event_time) || any(!is.finite(event_time)))
    stop("Neveljavno obdobje vplačil.")
  terminal <- function(amount) simulate_final_balance(
    event_list,discount_rate,borrowing_rate,event_time,amount)
  zero <- terminal(0)
  if (abs(zero)<1e-8) return(0)
  # The terminal balance is monotonic in an additional cashflow.
  bound <- max(1,abs(zero))
  for (i in seq_len(60)) {
    lower <- terminal(-bound)
    upper <- terminal(bound)
    if (lower<=0 && upper>=0)
      return(uniroot(terminal,c(-bound,bound),tol=1e-8)$root)
    bound <- bound*2
  }
  stop("Zneska ni mogoče izračunati v izbranem obdobju.")
}

# Find the earliest end year for which the added annual flow meets the goal.
get_period <- function(event_list, event_time, event_cashflow, discount_rate,
                       borrowing_rate=0.06) {
  start <- as.numeric(event_time)
  amount <- as.numeric(event_cashflow)
  years <- events_to_db(event_list)$event_time
  if (length(start)!=1 || !is.finite(start) || length(amount)!=1 ||
      !is.finite(amount)) stop("Neveljavno začetno leto ali znesek.")
  candidates <- seq(start,max(years))
  for (last in candidates) {
    final <- simulate_final_balance(event_list,discount_rate,borrowing_rate,
                                    seq(start,last),amount)
    if (final>=-1e-8) return(last)
  }
  NA_real_
}


# list event names from the event list
# sapply is like for each element in event list do function(x) and return the vector of values / but faster :)

get_event_names <- function(event_list) {
  event_names <-  sapply(event_list,
                         function(x)
                           x$event_name) %>% c()
  return(event_names)
}


# deletes the chosen event (event_name) from the event_list / json like structure
remove_event <- function(event_name, event_list) {
  event_list <-
    event_list[-which(get_event_names(event_list) == event_name)]
  
  return(event_list)
  
}

#get event info. return data of the chosen event (event_name) from the event list

get_event_info <- function(event_name, event_list) {
  
  single_event <- event_list[which(get_event_names(event_list) == event_name)]
  
  return(single_event)
  
}


#convert event json like structure to data.frame (tibble in R works best)
events_to_db <- function(event_list) {
  #lapply is list apply, which is "for each loop"
  event_db <- lapply(event_list, function(x) {
    tibble(
      event_name = x$event_name,
      event_time = x$event_time,
      event_cashflow = x$event_cashflow
    )
    
  }) %>% bind_rows()
  
  return(event_db)
  
}


#plot events and cashflow and balance


plot_events <- function(event_list, discount_rate, borrowing_rate = 0.06) {
  stopifnot(length(borrowing_rate) == 1, is.finite(borrowing_rate), borrowing_rate >= 0)
  events_db <- events_to_db(event_list)
  
  # Exclude the internal zero-flow scaffold from the displayed event chart.
  user_events <- events_db %>% filter(event_name != ".internal_timeline")
  event_plot <- ggplot(user_events, aes(x=event_time,y=event_cashflow)) +
    geom_hline(yintercept=0,color="#cbd5e1",linewidth=.4) +
    geom_col(aes(fill=ifelse(event_cashflow>=0,"Prilivi","Odlivi")),width=.8) +
    scale_fill_manual(values=c("Prilivi"="#4A9B83","Odlivi"="#C66B66"),name=NULL) +
    scale_y_continuous(labels=function(x) paste0(format(round(x),big.mark=" ",
      scientific=FALSE,trim=TRUE)," €"),name="Evri") +
    scale_x_continuous(name="Leto modela") +
    labs(fill="Dogodek") +
    theme_minimal(base_size=12) +
    theme(panel.grid.minor=element_blank(),legend.position="bottom",
          legend.title=element_blank())
  if (nrow(user_events)==0)
    event_plot <- ggplot(tibble(event_time=c(min(events_db$event_time),
                                      max(events_db$event_time)),event_cashflow=0),
                        aes(event_time,event_cashflow)) +
      geom_hline(yintercept=0,color="#cbd5e1") +
      scale_x_continuous(name="Leto modela") +
      scale_y_continuous(name=NULL) + theme_minimal(base_size=12)

  timeline <- tibble(event_time = seq(min(events_db$event_time), max(events_db$event_time))) %>%
    left_join(events_db %>% group_by(event_time) %>%
      summarise(value = sum(event_cashflow), .groups = "drop"), by="event_time") %>%
    left_join(discount_rate, by=c("event_time"="t")) %>%
    mutate(value = replace_na(value, 0), rate = replace_na(rate, 0),
           lag_rate = replace_na(lag(rate), 0),
           value_time = 0.0, investment_gain = 0.0, debt_interest = 0.0)
  for (i in seq_len(nrow(timeline))) {
    if (i == 1) {
      timeline$value_time[i] <- timeline$value[i]
    } else {
      previous <- timeline$value_time[i-1]
      if (previous < 0) {
        timeline$debt_interest[i] <- -previous * borrowing_rate
        timeline$value_time[i] <- previous - timeline$debt_interest[i] + timeline$value[i]
      } else {
        timeline$investment_gain[i] <- previous * timeline$lag_rate[i]
        timeline$value_time[i] <- previous + timeline$investment_gain[i] + timeline$value[i]
      }
    }
  }
  timeline <- timeline %>%
    mutate(net_flows = cumsum(value),
           cumulative_gains = cumsum(investment_gain),
           cumulative_debt_interest = cumsum(debt_interest),
           return_effect = value_time - net_flows)
  balance_plot <- ggplot(timeline, aes(x=event_time)) +
    geom_hline(yintercept=0, color="#cbd5e1", linewidth=.3) +
    geom_line(aes(y=value_time, color="Skupno stanje"), linewidth=1.1) +
    geom_line(aes(y=net_flows, color="Neto denarni tokovi"), linewidth=.85, linetype="dashed") +
    scale_color_manual(values=c("Skupno stanje"="#245b7b",
                                "Neto denarni tokovi"="#4A9B83"), name=NULL) +
    scale_y_continuous(labels=function(x) paste0(format(round(x), big.mark=" ", scientific=FALSE), " €"),
                       name="Evri") +
    scale_x_continuous(name="Leto modela") +
    theme_minimal(base_size=12) +
    theme(legend.position="bottom", panel.grid.minor=element_blank())
  return(list(event_plot=event_plot, balance_plot=balance_plot, timeline=timeline))
}

