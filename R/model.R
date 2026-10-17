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


#get yearly investment caclulates annual investment in a way that NPV (net present value of everything is 0)
get_yearly_investment <-
  function(event_list,
           event_time,
           discount_rate) {
    #test for discount rate ### need to specify more exemptions and exit / stop messages and warnings...
    
    #calculates discount factor at each time step. if rate was constant then 1/(1+r)^t
    #but rate needs not be constant so cumulative product (1+r0)*(1+r1)+(1+r2)...
    discount_factor <- discount_rate %>%
      mutate(
        lag_rate = lag(rate, 1),
        lag_rate = replace_na(lag_rate, 0),
        discount_factor = 1 / cumprod(1 + lag_rate)
      )
    
    
    
    cashflows <- events_to_db(event_list) %>%
      left_join(discount_factor, by = c("event_time" = "t")) %>%
      mutate(
        discount_factor = discount_factor / max(discount_factor),
        NPV = event_cashflow * discount_factor
      )
    
    accumulation <-
      tibble(t = min(cashflows$event_time):max(cashflows$event_time)) %>%
      left_join(discount_factor, by = "t") %>%
      mutate(discount_factor = discount_factor / max(discount_factor)) %>%
      filter(t >= min(event_time),
             t <= max(event_time))
    
    annual_investment <-
      -sum(cashflows$NPV) / sum(accumulation$discount_factor)
    
    return(annual_investment)
    
  }

get_period <- function(event_list,
                       event_time,
                       event_cashflow,
                       discount_rate) {
  
  #cacluate discount factor at each step
  discount_factor <- discount_rate %>%
    mutate(
      lag_rate = lag(rate, 1),
      lag_rate = replace_na(lag_rate, 0),
      #rate r need not be constant, hence cumproduct
      discount_factor = 1 / cumprod(1 + lag_rate)
    )
  
  #creates cashflows, first creat data.frame from event list
  cashflows <- events_to_db(event_list) %>%
    #join date from the discount factor
    left_join(discount_factor, by = c("event_time" = "t")) %>%
    # mutate is generate. it creats variables.
    mutate(
      discount_factor = discount_factor / max(discount_factor),
      #NPV is net present value. product of discount factor and value
      NPV = event_cashflow * discount_factor
    )
  
  # this variable answers how much has accumulated and maybe i forgot what thi is all about.
  # I honestly have no clue. 
  #aha!!! this one gives the last period if one want to know how many years would it take to reach a goal with a predermined annual investment or something
  
  
  accumulation <-
  #it take the perido from the min time event to max time event
    tibble(t = min(cashflows$event_time):max(cashflows$event_time)) %>%
    #add discount factor
    left_join(discount_factor, by = "t") %>%
    # calculate relative discount factor
    mutate(discount_factor = discount_factor / max(discount_factor)) %>%
    #apparently it can be different
    filter(t >= min(event_time)) %>%
    #and generate values for goal with is discounted cashflow
    mutate(
      goal = cumsum(discount_factor * event_cashflow),
      #and just test if the goal is reached
      goal_reached = ifelse(goal >= -sum(cashflows$NPV), TRUE, FALSE)
    ) %>%
    #and when it's reached
    filter(goal_reached) %>%
    #take the minimum of time and that is the answer
    summarise(t_end = min(t))
  
  
  return(accumulation$t_end)
  
  
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


plot_events <- function(event_list, discount_rate) {
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
           value_time = 0.0)
  for (i in seq_len(nrow(timeline))) {
    timeline$value_time[i] <- if (i == 1) timeline$value[i] else
      timeline$value_time[i-1] * (1 + timeline$lag_rate[i]) + timeline$value[i]
  }
  timeline <- timeline %>%
    mutate(net_flows = cumsum(value),
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

