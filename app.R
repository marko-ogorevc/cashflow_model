# Cashflow Model — presentation refresh (2026)
library(shiny)
library(bslib)
library(plotly)
library(shinyalert)
library(rlist)
library(gridExtra)
library(tidyverse)
library(jsonlite)

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  bg = "#fafbfc",
  fg = "#17273d",
  primary = "#245b7b",
  base_font = font_google("Inter")
)

ui <- fluidPage(
  theme = app_theme,
  useShinyalert(),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      :root { --ink:#17273d; --muted:#64748b; --line:#e3e9f2; --surface:#fff; }
      body { background:#fafbfc; }
      .container-fluid { max-width:1550px; padding:22px 26px 45px; }
      .app-header { display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:12px; margin:7px 0 23px; }
      .app-title { font-size:clamp(27px,3vw,37px); font-weight:780; letter-spacing:-.045em; margin:0; color:var(--ink); }
      .app-subtitle { color:var(--muted); font-size:14px; margin:5px 0 0; }
      .version-pill { border:1px solid #cddbf3; border-radius:999px; color:#245b7b; background:#eef4ff; font-size:12px; padding:7px 12px; font-weight:650; }
      .panel-card { background:#fff; border:1px solid var(--line); border-radius:11px; padding:20px; margin-bottom:17px; box-shadow:none; }
      .panel-heading { font-size:16px; font-weight:750; margin:0 0 5px; }
      .panel-description { font-size:12px; color:var(--muted); margin:0 0 18px; line-height:1.5; }
      .form-control { border-radius:9px; border-color:#dbe3ef; box-shadow:none; }
      .control-label { font-size:12px; font-weight:650; color:#40526d; }
      .btn { border-radius:9px; font-weight:650; }
      .btn-primary { background:#245b7b; border-color:#245b7b; }
      .btn-outline-secondary { color:#52657e; border-color:#ced8e7; }
      .action-row { display:flex; flex-wrap:wrap; gap:8px; margin-top:12px; }
      .action-row .btn { flex:1; min-width:105px; }
      .field-with-action { display:flex; align-items:flex-end; gap:9px; }
      .field-with-action .form-group { flex:1; min-width:0; }
      .field-with-action .btn { margin-bottom:15px; }
      .rate-row { padding:11px 0 0; border-top:1px solid #edf1f6; }
      .rate-row:first-of-type { border-top:0; padding-top:0; }
      .main-tabs .nav-tabs { border-bottom:1px solid #e4eaf3; margin-bottom:18px; }
      .main-tabs .nav-link { border:0; border-radius:8px; font-weight:650; color:#65758c; margin-right:8px; }
      .main-tabs .nav-link.active { background:#edf4f6; color:#245b7b; }
      .metric-card { background:#fff; border:1px solid var(--line); border-radius:11px; margin-bottom:15px; padding:15px; min-height:90px; } 
      .metric-card span { display:block; color:#64748b; font-size:12px; margin-bottom:8px; }
      .metric-card strong { display:block; color:#17273d; font-size:clamp(18px,2vw,25px); font-weight:750; }
      .chart-card { border:1px solid var(--line); background:white; border-radius:13px; padding:15px 15px 4px; margin-bottom:15px; }
      .chart-heading { font-weight:730; font-size:15px; margin:0 0 6px; }
      .chart-note { font-size:12px; color:var(--muted); margin-bottom:4px; }
      .literacy-note { max-width:900px; padding:3px 5px; color:#64748b; font-size:12px; line-height:1.6; }
      .literacy-note strong { color:#334155; }
      .literacy-note p { margin-top:5px; }
      .empty-help { font-size:12px; color:#64748b; margin:8px 0 0; }
      #event_list { white-space:pre-wrap; word-break:break-word; background:#f9fbff; border:1px solid #e4eaf3; border-radius:10px; padding:16px; min-height:150px; }
      @media(max-width:767px) { .container-fluid { padding:14px 12px 28px; } .panel-card { padding:15px; } }
    "))
  ),
  div(class = "app-header",
      div(
        h1("Moja finančna prihodnost", class = "app-title"),
        p("Preveri, kako varčevanje, večji izdatki in čas vplivajo na tvoje prihranke.", class = "app-subtitle")
      ),
      span("Finančni načrt · preizkusna različica", class = "version-pill")
  ),
  fluidRow(
    column(
      width = 4,
      div(class = "panel-card",
          h3("Časovna os", class="panel-heading"),
          p("Izberi oznake let. Izračun ostane enak.", class="panel-description"),
          radioButtons("time_mode", NULL,
            choices=c("Leta od začetka"="relative", "Starost"="age", "Koledarska leta"="calendar"),
            selected="relative", inline=FALSE),
          conditionalPanel("input.time_mode == 'age'",
            numericInput("starting_age", "Trenutna starost", value=30, min=0, max=120, step=1)),
          conditionalPanel("input.time_mode == 'calendar'",
            numericInput("starting_year", "Začetno leto", value=2026, min=1900, max=2200, step=1)),
          numericInput("horizon", "Trajanje načrta (let)", value=40, min=1, max=150, step=1),
          textOutput("timeline_description")
      ),
      div(class = "panel-card",
          h3("Prihodki in izdatki", class = "panel-heading"),
          p("Vnesi letne zneske v evrih: prilivi so pozitivni, izdatki negativni. Za mesečni znesek ga najprej pomnoži z 12.", class = "panel-description"),
          div(class = "field-with-action",
              textInput("event_name", "Kaj se zgodi?", placeholder = "npr. letno varčevanje, nakup avtomobila"),
              actionButton("gen_names", "Uredi vnos", class = "btn-outline-secondary")
          ),
          fluidRow(
            column(4, textInput("event_start", "Od leta", placeholder = "0")),
            column(4, textInput("event_end", "Do leta", placeholder = "30")),
            column(4, div(style="padding-top:25px;", actionButton("gen_period", "Izračunaj leto", class="btn-outline-secondary", width="100%")))
          ),
          div(class="field-with-action",
              textInput("event_cashflow", "Letni znesek (€)", placeholder="npr. -12000"),
              actionButton("get_cashflow", "Izračunaj znesek", class="btn-outline-secondary")
          ),
          div(class="action-row",
              actionButton("ok_button", "Dodaj", class="btn-primary"),
              actionButton("delete_list", "Počisti", class="btn-outline-secondary")
          ),
          p("Časovnica se pripravi samodejno. Prazno polje »Do leta« pomeni enkratni dogodek.", class="empty-help")
      ),
      div(class="panel-card",
          h3("Donosnost prihrankov", class="panel-heading"),
          p("Predpostavljena letna donosnost se med izbranimi leti postopno spreminja. Donosnost ni zagotovljena.", class="panel-description"),
          div(class="rate-row",
              fluidRow(column(8,sliderInput("r0", "Začetna donosnost",value=.05,min=0,max=.1,step=.005)),
                       column(4,numericInput("t0","Prvo leto",value=50,min=-10000,max=10000,step=1)))
          ),
          div(class="rate-row",
              fluidRow(column(8,sliderInput("r1","Vmesna donosnost",value=.025,min=0,max=.1,step=.005)),
                       column(4,numericInput("t1","Drugo leto",value=60,min=-10000,max=10000,step=1)))
          ),
          div(class="rate-row",
              fluidRow(column(8,sliderInput("r2","Končna donosnost",value=.01,min=0,max=.1,step=.005)),
                       column(4,numericInput("t2","Tretje leto",value=80,min=-10000,max=10000,step=1)))
          )
      )
    ),
    column(
      width = 8,
      div(class="panel-card main-tabs",
          tabsetPanel(
            tabPanel("Pregled",
                     fluidRow(
                       column(4, div(class="metric-card", span("Končno stanje"), strong(textOutput("final_balance", inline=TRUE)))),
                       column(4, div(class="metric-card", span("Neto denarni tokovi"), strong(textOutput("net_contributions", inline=TRUE)))),
                       column(4, div(class="metric-card", span("Učinek donosnosti"), strong(textOutput("total_returns", inline=TRUE))))
                     ),
                     div(class="chart-card",
                         h3("Stanje, denarni tokovi in donosnost",class="chart-heading"),
                         p("Primerjava skupnega stanja s kumulativnimi neto denarnimi tokovi. Razlika predstavlja učinek predpostavljene donosnosti.",class="chart-note"),
                         plotlyOutput("balance_plot_output",height="260px")
                     ),
                     div(class="chart-card",
                         h3("Letni prilivi in izdatki",class="chart-heading"),
                         plotlyOutput("cashflow_plot_output",height="230px")
                     ),
                     div(class="chart-card",
                         h3("Predpostavljena donosnost",class="chart-heading"),
                         plotlyOutput("interest_plot_output",height="210px")
                     )
            ),
            tabPanel("Moji vnosi",
                     h3("Vneseni dogodki",class="chart-heading"),
                     p("Časovnica se ustvari samodejno in ni prikazana med dogodki.",class="chart-note"),
                     tableOutput("event_list")
            )
          )
      )
    )
  ),
  div(class="literacy-note",
      strong("Dobro je vedeti"),
      p("Rezultat je izračun na podlagi tvojih predpostavk, ne napoved. Model ne upošteva inflacije, davkov ali negotovosti donosov. Zato ga uporabljaj za primerjanje scenarijev.")
  )
)

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
    
    event_plot <- ggplot(events_db,
                         aes(x = event_time,
                             y = event_cashflow)) +
      geom_col(aes(fill = event_name)) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(
        labels = function(x)
          paste0("€", x / 1000, "k"),
        name = "Letni denarni tok"
      ) +
      scale_x_continuous(name = "Leto modela") +
      scale_fill_discrete(name = "Dogodek") +
      theme_minimal()+
      theme(
        legend.position = "none"
      )
    
    
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
                                  "Neto denarni tokovi"="#b17f48"), name=NULL) +
      scale_y_continuous(labels=function(x) paste0(format(round(x), big.mark=" ", scientific=FALSE), " €"),
                         name="Evri") +
      scale_x_continuous(name="Leto modela") +
      theme_minimal(base_size=12) +
      theme(legend.position="bottom", panel.grid.minor=element_blank())
    return(list(event_plot=event_plot, balance_plot=balance_plot, timeline=timeline))
  }
  
  ### LOGIC ####
  
  rv <- reactiveValues()
  
  
  # A zero-cashflow scaffold keeps the 0–80-year horizon visible.
  # It is internal to the model and is not an editable user event.
  initial_timeline <- function(horizon = 40) {
    list(list(
      event_name = ".internal_timeline",
      event_time = 0:as.integer(horizon),
      event_cashflow = 0
    ))
  }
  rv$myList <- initial_timeline()

  # Model periods are always relative to the beginning; display is a reversible mapping.
  origin <- reactive({
    if (input$time_mode == "age") return(as.numeric(input$starting_age))
    if (input$time_mode == "calendar") return(as.numeric(input$starting_year))
    0
  })
  display_year <- reactive({
    function(period) period + origin()
  })
  to_period <- reactive({
    function(display_value) as.numeric(display_value) - origin()
  })
  output$timeline_description <- renderText({
    paste0("Prikaz od ", display_year()(0), " do ",
           display_year()(input$horizon), if (input$time_mode == "calendar") "." else " let")
  })
  observeEvent(input$horizon, {
    req(is.finite(input$horizon), input$horizon >= 1, input$horizon <= 150)
    events <- Filter(function(x) x$event_name != ".internal_timeline", rv$myList)
    rv$myList <- c(initial_timeline(input$horizon), events)
  }, ignoreInit=TRUE)

  # Placeholders are examples only; actual conversion happens when data are submitted.
  observeEvent(list(input$time_mode, input$starting_age, input$starting_year), {
    updateTextInput(session, "event_start", placeholder=as.character(display_year()(0)))
    updateTextInput(session, "event_end", placeholder=as.character(display_year()(input$horizon)))
  }, ignoreInit=FALSE)

  
  observeEvent(input$ok_button, {
    if (input$event_name == "" |
        is.na(as.numeric(input$event_start)) |
        is.na(as.numeric(input$event_cashflow))) {
      shinyalert(title = "Preveri podatke",
                 text = {
                   case_when(
                     input$event_name == "" ~ "Ime dogodka",
                     
                     is.na(as.numeric(input$event_start)) ~ "Začetno in končno leto morata biti števili",
                     TRUE ~ "Vnesi znesek (pozitiven priliv, negativen izdatek)"
                   )
                 })
      
    } else {
      rv$myList <- isolate(rv$myList) %>%
        add_event(
          event_name = input$event_name,
          event_time = seq(
            from = to_period()(input$event_start),
            to = to_period()(ifelse(
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
    rv$myList <- initial_timeline(input$horizon)
    
    
  })
  
  
  observeEvent(input$gen_period, {
    if (is.na(as.numeric(input$event_start))) {
      shinyalert(title = "Manjkajo podatki",
                 text = "Vnesi začetno leto")
      
    } else {
      updateTextInput(
        session,
        "event_end",
        value = display_year()(get_period(
          event_list = rv$myList,
          event_time = to_period()(input$event_start),
          event_cashflow = as.numeric(input$event_cashflow),
          discount_rate = var_rate()$rate
        ))
      )
      
      
    }
  })
  
  
  observeEvent(input$get_cashflow, {
    if (is.na(as.numeric(input$event_start))) {
      shinyalert(title = "Manjkajo podatki",
                 text = "Vnesi začetno in končno leto")
      
    } else {
      updateTextInput(
        session,
        "event_cashflow",
        value = get_yearly_investment(
          event_list = rv$myList,
          event_time = seq(
            from = to_period()(input$event_start),
            to = to_period()(ifelse(
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
  
  
  output$event_list <- renderTable({
    user_events <- Filter(function(x) x$event_name != ".internal_timeline", rv$myList)
    if (length(user_events) == 0) return(NULL)
    tibble(
      Dogodek = vapply(user_events, function(x) as.character(x$event_name), character(1)),
      Od = vapply(user_events, function(x) display_year()(min(x$event_time)), numeric(1)),
      Do = vapply(user_events, function(x) display_year()(max(x$event_time)), numeric(1)),
      `Letni znesek (€)` = vapply(user_events, function(x) as.numeric(x$event_cashflow[1]), numeric(1))
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s", digits = 2)
  
  
  # Translate breakpoint inputs when the user's time origin changes.
  # Preserve the underlying relative breakpoints, including user edits.
  previous_origin <- reactiveVal(0)
  observeEvent(origin(), {
    new_origin <- origin()
    old_origin <- previous_origin()
    shift <- new_origin - old_origin
    if (is.finite(shift) && shift != 0) {
      for (id in c("t0", "t1", "t2")) {
        current <- input[[id]]
        if (!is.null(current) && is.finite(current)) {
          updateNumericInput(session, id, value=current + shift)
        }
      }
    }
    previous_origin(new_origin)
  }, ignoreInit=FALSE)

  var_rate = reactive({
    get_var_rate(
      period = if (length(rv$myList) > 0) range(events_to_db(rv$myList)$event_time) else c(0, 100),
      r0 = input$r0,
      t0 = to_period()(input$t0),
      r1 = input$r1,
      t1 = to_period()(input$t1),
      r2 = input$r2,
      t2 = to_period()(input$t2)
    )
  })
  
  format_time_axis <- function(plot_object) {
    times <- range(events_to_db(rv$myList)$event_time)
    ticks <- pretty(times, n=7)
    ticks <- ticks[ticks >= times[1] & ticks <= times[2]]
    plotly::layout(plot_object,
      xaxis=list(title=if (input$time_mode == "age") "Starost" else
                   if (input$time_mode == "calendar") "Koledarsko leto" else "Leta od začetka",
        tickmode="array", tickvals=ticks, ticktext=display_year()(ticks)))
  }

  model_results <- reactive({
    req(length(rv$myList) > 0)
    plot_events(rv$myList, var_rate()$rate)$timeline
  })
  euro_label <- function(x) {
    paste0(format(round(x, 0), big.mark=" ", scientific=FALSE, trim=TRUE), " €")
  }
  output$final_balance <- renderText({
    euro_label(tail(model_results()$value_time, 1))
  })
  output$net_contributions <- renderText({
    euro_label(tail(model_results()$net_flows, 1))
  })
  output$total_returns <- renderText({
    euro_label(tail(model_results()$return_effect, 1))
  })

  output$balance_plot_output <- renderPlotly({
    if (length(rv$myList) > 0) {
      my_plots <- plot_events(rv$myList, var_rate()$rate)
      
      format_time_axis(ggplotly(my_plots$balance_plot))
      
    }
  })
  
  
  output$cashflow_plot_output <- renderPlotly({
    if (length(rv$myList) > 0) {
      my_plots <- plot_events(rv$myList, var_rate()$rate)
      
      format_time_axis(ggplotly(my_plots$event_plot))
      
    }
  })
  
  output$interest_plot_output <- renderPlotly({
    if (length(rv$myList) > 0) {
      format_time_axis(ggplotly(var_rate()$rate_plot))
      
    }
  })
  
  #### get and modify events ####
  observeEvent(input$gen_names, {
    if (length(rv$myList) <= 1) {
      shinyalert(title = "Ni vnosov", text = "Najprej dodaj prihodek ali izdatek.")
      return()
    }
    # display a modal dialog with a header, textinput and action buttons
    showModal(modalDialog(
      
      
      selectInput(
        inputId = "list_events",
        label = "Izberi dogodek",
        selected = NULL,
        choices = setdiff(get_event_names(rv$myList), ".internal_timeline")
      ),
      footer=tagList(
        actionButton(
          inputId = "submit_name",
          label = "Izberi"),
        modalButton("Prekliči")
      )
    ))
  })
  
  
  observeEvent(input$submit_name, {
    removeModal()
    
    event_info <- get_event_info(isolate(input$list_events), isolate(rv$myList))
    
    updateTextInput(session, "event_name", value = event_info[[1]]$event_name)
    updateTextInput(session, "event_start", value = display_year()(min(event_info[[1]]$event_time)))
    updateTextInput(session, "event_end", value = display_year()(max(event_info[[1]]$event_time)))
    updateTextInput(session, "event_cashflow", value = event_info[[1]]$event_cashflow)
    rv$myList <- remove_event(input$list_events, rv$myList)
    
  })
  
  
  
}


# Run the application
shinyApp(ui = ui, server = server)
