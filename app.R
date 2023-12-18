
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Or use this link https://markoogorevc.shinyapps.io/Cashflow_model/
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#
# install.packages("shinythemes")
library(shiny)
library(shinythemes)
library(plotly)
library(shinyalert)
library(rlist)
library(gridExtra)
library(tidyverse)
library(jsonlite)


#UI ####

ui <- fluidPage(# Application title
  titlePanel("Cashflow model v0.2"),
  shinythemes::themeSelector(),
  
  # This is a sidebar
  sidebarLayout(
    sidebarPanel(
      verticalLayout(#vertical 1
        
        wellPanel(
          fluidRow(column(
            8,
            textInput(inputId = "event_name",
                      label = "Label")
          ),
          column(
            4,
            br(),
            actionButton(
              inputId = "gen_names",
              label = "Get",
              width = '100%'
            )
          )),
          fluidRow(
            column(4,
                   textInput(inputId = "event_start",
                             label = "From")),
            column(4,
                   textInput(inputId = "event_end",
                             label = "To")),
            column(
              4,
              br(),
              actionButton(
                inputId = "gen_period",
                label = "Solve",
                width = '100%'
              )
            )
          ),
          fluidRow(column(
            8,
            textInput(inputId = "event_cashflow",
                      label = "Cashflow")
          ),
          
          column(
            4,
            br(),
            actionButton(
              inputId = "get_cashflow",
              label = "Solve",
              width = '100%'
            )
          ),),
          fluidRow(column(
            4,
      
    # action button -> Ok that adds events linked to the         
            
            actionButton(inputId = "ok_button",
                         label = "Add event")
          ),
          column(
            4,
            actionButton(inputId = "delete_list",
                         label = "Delete events")
          ))
        )),
      
      #vertical 2
      wellPanel(
        fluidRow(column(
          8,
          sliderInput(
            inputId = "r0",
            label = "t0 rate",
            value = 0.05,
            min = 0,
            max = 0.1,
            step = 0.005
          )
        ),
        column(
          4,
          numericInput(
            inputId = "t0",
            label = "t0 break",
            value = 50,
            min = 0,
            max = 100,
            step = 1
          ),
        )),
        
        fluidRow(
          column(
            8,
            sliderInput(
              inputId = "r1",
              label = "t1 rate",
              value = 0.025,
              min = 0,
              max = 0.1,
              step = 0.005
            )
          ),
          column(
            4,
            numericInput(
              inputId = "t1",
              label = "t1 break",
              value = 60,
              min = 0,
              max = 100,
              step = 1
            ),
          )
        ),
        fluidRow(
          column(
            8,
            sliderInput(
              inputId = "r2",
              label = "t2 rate",
              value = 0.01,
              min = 0,
              max = 0.1,
              step = 0.005
            )
          ),
          column(
            4,
            numericInput(
              inputId = "t2",
              label = "t2 break",
              value = 80,
              min = 0,
              max = 100,
              step = 1
            )
          )
        )
        
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(fluidPage(tabsetPanel(
      tabPanel(
        "Overview",
        
        plotlyOutput("balance_plot_output", height = "200px"),
        plotlyOutput("cashflow_plot_output", height = "200px"),
        plotlyOutput("interest_plot_output", height = "200px")
      )
      ,
      tabPanel("Events",
               verbatimTextOutput(outputId = "event_list"))
    )))
  ))


# SERVER ####


server <- function(input, output, session) {
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
        name = "Expected Returns",
        labels = function(x)
          scales::percent(x),
        limits = c(0, NA)
      ) +
      scale_x_continuous(name = "Time",
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
    
    event_plot <- ggplot(events_db,
                         aes(x = event_time,
                             y = event_cashflow)) +
      geom_col(aes(fill = event_name)) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(
        labels = function(x)
          paste0("€", x / 1000, "k"),
        name = "Cashflow"
      ) +
      scale_x_continuous(name = "Time") +
      scale_fill_discrete(name = "Event") +
      theme_minimal()+
      theme(
        legend.position = "none"
      )
    
    
    #plot 2
    
    timeline <-
      tibble(event_time = min(events_db$event_time):max(events_db$event_time)) %>%
      left_join(events_db %>%
                  group_by(event_time) %>%
                  summarise(value = sum(event_cashflow))) %>%
      left_join(discount_rate, by = c("event_time"  = "t")) %>%
      mutate(
        value = replace_na(value, 0),
        t = 1:(n()),
        lag_rate = lag(rate, 1),
        lag_rate = replace_na(lag_rate, 0),
        value_time = 0
      )
    
    for (t in timeline$t) {
      if (t == 1) {
        timeline$value_time[t] = (timeline$value[t])
      } else if (t > 1) {
        timeline$value_time[t] = (timeline$value_time[t - 1]*(1 + timeline$lag_rate[t]) + timeline$value[t]) 
      }
      
      
    }
    
    balance_plot <- timeline %>%
      mutate(
        direction = ifelse(value_time >= 0, "up", "down")
      ) %>% 
      ggplot(aes(x = event_time, y = value_time)) +
      # geom_area(fill = "green")  +
      geom_col(aes(fill = direction)) +
      scale_fill_discrete(name = "")+
      scale_y_continuous(
        labels = function(x)
          paste0("€", x / 1000, "k"),
        name = "Balance"
      ) +
      scale_x_continuous(name = "Time") +
      theme_minimal()+
      theme(
        legend.position = "none"
      )
    
    return(list("event_plot" = event_plot, "balance_plot" = balance_plot))
    
  }
  
  ### LOGIC ####
  
  rv <- reactiveValues()
  
  
  rv$myList <- list()
  
  observeEvent(input$ok_button, {
    if (input$event_name == "" |
        is.na(as.numeric(input$event_start)) |
        is.na(as.numeric(input$event_cashflow))) {
      shinyalert(title = "fill missing forms",
                 text = {
                   case_when(
                     input$event_name == "" ~ "Label",
                     
                     is.na(as.numeric(input$event_start)) ~ "Event start (and end) as numeric",
                     TRUE ~ "Cashflow (+ for inflow, - outflow)"
                   )
                 })
      
    } else {
      rv$myList <- isolate(rv$myList) %>%
        add_event(
          event_name = input$event_name,
          event_time = seq(
            from = as.numeric(input$event_start),
            to = as.numeric(ifelse(
              input$event_end == "",
              as.numeric(input$event_start),
              as.numeric(input$event_end)
            ))
          ),
          event_cashflow = as.numeric(input$event_cashflow)
        )
      
      updateTextInput(session, "event_name", value = "")
      updateTextInput(session, "event_start", value = "")
      updateTextInput(session, "event_end", value = "")
      updateTextInput(session, "event_cashflow", value = "")
      
    }
    
  })
  
  observeEvent(input$delete_list, {
    rv$myList <- list()
    
    
  })
  
  
  observeEvent(input$gen_period, {
    if (is.na(as.numeric(input$event_start))) {
      shinyalert(title = "missing form data",
                 text = "Must have start")
      
    } else {
      updateTextInput(
        session,
        "event_end",
        value = get_period(
          event_list = rv$myList,
          event_time = as.numeric(input$event_start),
          event_cashflow = as.numeric(input$event_cashflow),
          discount_rate = var_rate()$rate
        )
      )
      
      
    }
  })
  
  
  observeEvent(input$get_cashflow, {
    if (is.na(as.numeric(input$event_start))) {
      shinyalert(title = "missing form data",
                 text = "type from / to")
      
    } else {
      updateTextInput(
        session,
        "event_cashflow",
        value = get_yearly_investment(
          event_list = rv$myList,
          event_time = seq(
            from = as.numeric(input$event_start),
            to = as.numeric(ifelse(
              input$event_end == "",
              as.numeric(input$event_start),
              as.numeric(input$event_end)
            ))
          ),
          discount_rate = var_rate()$rate
        )
      )
    }
    
    
  })
  
  
  output$event_list <- renderPrint({
    toJSON(rv$myList, prety = TRUE)
  })
  
  
  var_rate = reactive({
    get_var_rate(
      period = range(events_to_db(rv$myList)$event_time),
      r0 = input$r0,
      t0 = input$t0,
      r1 = input$r1,
      t1 = input$t1,
      r2 = input$r2,
      t2 = input$t2
    )
  })
  
  output$balance_plot_output <- renderPlotly({
    if (length(rv$myList) > 0) {
      my_plots <- plot_events(rv$myList, var_rate()$rate)
      
      ggplotly(my_plots$balance_plot)
      
    }
  })
  
  
  output$cashflow_plot_output <- renderPlotly({
    if (length(rv$myList) > 0) {
      my_plots <- plot_events(rv$myList, var_rate()$rate)
      
      ggplotly(my_plots$event_plot)
      
    }
  })
  
  output$interest_plot_output <- renderPlotly({
    if (length(rv$myList) > 0) {
      ggplotly(var_rate()$rate_plot)
      
    }
  })
  
  #### get and modify events ####
  observeEvent(input$gen_names, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(modalDialog(
      
      
      selectInput(
        inputId = "list_events",
        label = "Select event",
        selected = NULL,
        choices = get_event_names(rv$myList)
      ),
      footer=tagList(
        actionButton(
          inputId = "submit_name",
          label = "Select"),
        modalButton("Cancel")
      )
    ))
  })
  
  
  observeEvent(input$submit_name, {
    removeModal()
    
    event_info <- get_event_info(isolate(input$list_events), isolate(rv$myList))
    
    updateTextInput(session, "event_name", value = event_info[[1]]$event_name)
    updateTextInput(session, "event_start", value = min(event_info[[1]]$event_time))
    updateTextInput(session, "event_end", value =  max(event_info[[1]]$event_time))
    updateTextInput(session, "event_cashflow", value = event_info[[1]]$event_cashflow)
    rv$myList <- remove_event(input$list_events, rv$myList)
    
  })
  
  
  
}


# Run the application
shinyApp(ui = ui, server = server)
