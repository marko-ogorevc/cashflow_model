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
    tags$script(src="i18n.js"),
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
      .intro-switch { margin-bottom:14px; }
      .subtle-help {color:#64748b;font-size:12px;}
      .scenario-actions { display:flex; gap:8px; flex-wrap:wrap; }
      .scenario-actions .btn { flex:1; min-width:110px; }
      .scenario-actions .form-group { margin:0; }
      .scenario-help { font-size:12px; color:#64748b; line-height:1.6; }
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
      div(class="d-flex align-items-center gap-2",
          selectInput("language",NULL,choices=c("Slovenščina"="sl","English"="en"),selected="sl",width="145px"),
          span("Finančni načrt · preizkusna različica", class = "version-pill"))
  ),
  fluidRow(
    column(
      width = 4,
      div(class="panel-card",
          h3("Scenariji", class="panel-heading"),
          p("Začni s pripravljenim primerom ali shrani svoj načrt.", class="panel-description"),
          selectInput("example_scenario", "Pripravljeni primer",
            choices=c("Izberi primer ..."="", "Dolgoročno varčevanje"="saving",
                      "Večji prihodnji izdatek"="expense",
                      "Različni denarni tokovi"="mixed"), selected=""),
          actionButton("load_example", "Naloži primer", class="btn-primary", width="100%"),
          tags$hr(),
          div(class="scenario-actions",
              downloadButton("save_scenario", "Shrani JSON", class="btn-outline-secondary"),
              fileInput("open_scenario", "Naloži shranjeni JSON", accept=c(".json","application/json"))
          ),
          p("Nalaganje zamenja trenutni načrt. Datoteka ostane na tvoji napravi.", class="scenario-help")
      ),
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
          radioButtons("amount_frequency","Znesek je",choices=c("Letni"="annual","Mesečni"="monthly"),
                       selected="annual",inline=TRUE),
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
          h3("Naložbena strategija",class="panel-heading"),
          radioButtons("model_mode",NULL,choices=c(
            "Preprosto: delnice in obveznice"="simple",
            "Napredno: ročne stopnje donosnosti"="advanced"),selected="simple"),
          conditionalPanel("input.model_mode == 'simple'",
            selectInput("asset_strategy","Strategija",choices=c(
              "Postopni prehod k obveznicam"="glide",
              "Samo delnice"="equities",
              "Uravnoteženo (60 / 40)"="balanced",
              "Samo obveznice"="bonds"),selected="glide"),
            conditionalPanel("input.asset_strategy == 'glide'",
              sliderInput("stock_begin","Začetni delež delnic (%)",min=0,max=100,value=80,step=5),
              sliderInput("stock_finish","Končni delež delnic (%)",min=0,max=100,value=20,step=5)),
            h4("Razporeditev naložb skozi čas",style="font-size:13px;font-weight:700;margin-top:14px;"),
            plotlyOutput("allocation_plot",height="185px"),
            p("Delnice / obveznice: od začetka do konca načrta.",class="subtle-help"),
            p("Ilustrativna predpostavka: delnice 7 %, obveznice 3 % letno. Donos ni zagotovljen.",
              class="subtle-help")
          ),
          sliderInput("borrowing_rate", "Letna obrestna mera zadolževanja (%)",
                      min=0,max=20,value=6,step=.5),
          p("Negativno stanje se samodejno obravnava kot dolg. Obrestna mera dolga je stalna.",
            class="subtle-help"),
          conditionalPanel("input.model_mode == 'advanced'",
          div(
          h3("Ročne predpostavke donosnosti",class="panel-heading"),
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
      )))
    ),
    column(
      width = 8,
      div(class="panel-card",
          h3("Prikaz rezultatov",class="panel-heading"),
          checkboxInput("real_values","V današnji kupni moči",FALSE),
          conditionalPanel("input.real_values",
            numericInput("inflation","Letna inflacija (%)",2,min=-10,max=30,step=.5)),
          actionButton("save_comparison","Shrani trenutno projekcijo za primerjavo",
                       class="btn-outline-secondary"),
          actionButton("clear_comparison","Odstrani primerjavo",class="btn-outline-secondary"),
          p("Shranjena primerjava se ohrani do konca seje ali ponovnega shranjevanja.",class="subtle-help")
      ),
      div(class="panel-card main-tabs",
          tabsetPanel(
            tabPanel("Pregled",
                     fluidRow(
                       column(4, div(class="metric-card", span("Končno stanje"), strong(textOutput("final_balance", inline=TRUE)))),
                       column(4, div(class="metric-card", span("Neto denarni tokovi"), strong(textOutput("net_contributions", inline=TRUE)))),
                       column(4, div(class="metric-card", span("Neto učinek obrestovanja"), strong(textOutput("total_returns", inline=TRUE))))
                     ),
                     div(class="chart-card",
                         h3("Stanje, denarni tokovi in donosnost",class="chart-heading"),
                         p("Primerjava skupnega stanja s kumulativnimi neto denarnimi tokovi. Razlika predstavlja učinek predpostavljene donosnosti.",class="chart-note"),
                         plotlyOutput("balance_plot_output",height="300px"),
                         textOutput("comparison_summary")
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
  observeEvent(input$language, {
    session$sendCustomMessage("cashflow-language", input$language)
  }, ignoreInit = FALSE)

  # All existing mathematical functions are loaded without altering their formulas.
  source('R/model.R', local = TRUE)

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

  # Scenario files save periods relative to the start, independently of display labels.
  scenario_events <- function(x) {
    if (!is.list(x)) stop("Neveljaven seznam dogodkov.")
    lapply(x, function(e) {
      if (!is.list(e) || !all(c("event_name","event_time","event_cashflow") %in% names(e)))
        stop("Dogodek nima zahtevanih polj.")
      nm <- e$event_name
      tt <- unlist(e$event_time, use.names=FALSE)
      v <- unlist(e$event_cashflow, use.names=FALSE)
      if (!is.character(nm) || length(nm)!=1 || is.na(nm) ||
          !nzchar(nm) || identical(nm,".internal_timeline") ||
          !is.numeric(tt) || !length(tt) || any(!is.finite(tt)) ||
          any(tt != round(tt)) || length(tt)>1000 ||
          !is.numeric(v) || length(v)!=1 || !is.finite(v) ||
          any(abs(tt)>10000))
        stop("Dogodek vsebuje neveljavne podatke.")
      list(event_name=nm,event_time=as.numeric(tt),event_cashflow=as.numeric(v))
    })
  }

  example_data <- function(which) {
    switch(which,
      saving=list(horizon=40, events=list(
        list(event_name="Začetni prihranki",event_time=0,event_cashflow=5000),
        list(event_name="Redno letno varčevanje",event_time=1:40,event_cashflow=2400))),
      expense=list(horizon=30, events=list(
        list(event_name="Začetni prihranki",event_time=0,event_cashflow=20000),
        list(event_name="Letno varčevanje",event_time=1:30,event_cashflow=3000),
        list(event_name="Večji nakup",event_time=12,event_cashflow=-35000))),
      mixed=list(horizon=45, events=list(
        list(event_name="Začetni vložek",event_time=0,event_cashflow=15000),
        list(event_name="Redno vlaganje",event_time=1:25,event_cashflow=4000),
        list(event_name="Enkratni strošek",event_time=8,event_cashflow=-18000),
        list(event_name="Dodatni prejemek",event_time=20,event_cashflow=25000),
        list(event_name="Letni dvigi",event_time=30:45,event_cashflow=-6000))),
      stop("Najprej izberi primer.")
    )
  }

  apply_scenario <- function(scenario) {
    events <- scenario_events(scenario$events)
    horizon <- as.numeric(scenario$horizon)
    if (length(horizon)!=1 || !is.finite(horizon) || horizon<1 ||
        horizon>150 || horizon!=round(horizon) || length(events)>200)
      stop("Neveljavno trajanje načrta ali preveč dogodkov.")
    updateNumericInput(session, "horizon", value=horizon)
    rv$myList <- c(initial_timeline(horizon), events)
    invisible(NULL)
  }

  observeEvent(input$load_example, {
    if (identical(input$example_scenario,"")) {
      shinyalert(title="Izberi primer",text="Izberi enega od pripravljenih scenarijev.")
      return()
    }
    tryCatch(apply_scenario(example_data(input$example_scenario)),
      error=function(e) shinyalert(title="Napaka",text=conditionMessage(e)))
  })

  output$save_scenario <- downloadHandler(
    filename=function() paste0("cashflow-scenario-",Sys.Date(),".json"),
    content=function(file) {
      events <- Filter(function(e) e$event_name!=".internal_timeline",isolate(rv$myList))
      model <- list(
        format="cashflow-model",version=1,horizon=isolate(input$horizon),
        events=events,
        display=list(mode=isolate(input$time_mode),
                     starting_age=isolate(input$starting_age),
                     starting_year=isolate(input$starting_year)),
        returns=list(r0=isolate(input$r0),r1=isolate(input$r1),r2=isolate(input$r2),
                     t0=isolate(to_period()(input$t0)),
                     t1=isolate(to_period()(input$t1)),
                     t2=isolate(to_period()(input$t2)))
      )
      model$preferences <- list(
        borrowing_rate=isolate(input$borrowing_rate),
        model_mode=isolate(input$model_mode),
        asset_strategy=isolate(input$asset_strategy),
        stock_begin=isolate(input$stock_begin),
        stock_finish=isolate(input$stock_finish),
        inflation=isolate(input$inflation),
        real_values=isolate(input$real_values)
      )
      jsonlite::write_json(model,file,pretty=TRUE,auto_unbox=TRUE)
    }
  )
  observeEvent(input$open_scenario, {
    tryCatch({
      if (is.null(input$open_scenario$size) || input$open_scenario$size>2e6)
        stop("Datoteka je prevelika (največ 2 MB).")
      model <- jsonlite::read_json(input$open_scenario$datapath,simplifyVector=FALSE)
      if (!identical(model$format,"cashflow-model") ||
          !identical(as.numeric(model$version),1))
        stop("Datoteka ni podprt scenarij Cashflow Model.")
      # Validate all content before changing UI or reactive state.
      events <- scenario_events(model$events)
      h <- as.numeric(model$horizon)
      if (length(h)!=1 || !is.finite(h) || h<1 || h>150 ||
          h!=round(h) || length(events)>200) stop("Neveljavno trajanje načrta.")
      d <- model$display; rt <- model$returns
      if (!d$mode %in% c("relative","age","calendar")) stop("Neveljaven prikaz časa.")
      age <- as.numeric(d$starting_age); year <- as.numeric(d$starting_year)
      rvect <- vapply(rt[c("r0","r1","r2","t0","t1","t2")],
                      function(v) as.numeric(v), numeric(1))
      if (length(age)!=1 || !is.finite(age) || age<0 || age>120 ||
          length(year)!=1 || !is.finite(year) || year<1900 || year>2200 ||
          any(!is.finite(rvect)) || any(rvect[1:3]<0) ||
          any(rvect[1:3]>.1) || any(abs(rvect[4:6])>10000))
        stop("Neveljavne nastavitve scenarija.")
      prefs <- model$preferences
      if (!is.null(prefs) && !is.null(prefs$borrowing_rate)) {
        loan <- as.numeric(prefs$borrowing_rate)
        if (length(loan)!=1 || !is.finite(loan) || loan<0 || loan>20)
          stop("Neveljavna obrestna mera zadolževanja.")
        updateSliderInput(session,"borrowing_rate",value=loan)
      }
      if (!is.null(prefs)) {
        pm <- prefs$model_mode
        ps <- prefs$asset_strategy
        if (!pm %in% c("simple","advanced") ||
            !ps %in% c("glide","equities","balanced","bonds"))
          stop("Neveljavna naložbena strategija.")
        numeric_prefs <- c(stock_begin=as.numeric(prefs$stock_begin),
          stock_finish=as.numeric(prefs$stock_finish),
          inflation=as.numeric(prefs$inflation))
        if (any(!is.finite(numeric_prefs)) ||
            any(numeric_prefs[c("stock_begin","stock_finish")]<0) ||
            any(numeric_prefs[c("stock_begin","stock_finish")]>100) ||
            abs(numeric_prefs["inflation"])>30)
          stop("Neveljavne naložbene ali inflacijske predpostavke.")
        updateRadioButtons(session,"model_mode",selected=pm)
        updateSelectInput(session,"asset_strategy",selected=ps)
        updateSliderInput(session,"stock_begin",value=numeric_prefs["stock_begin"])
        updateSliderInput(session,"stock_finish",value=numeric_prefs["stock_finish"])
        updateNumericInput(session,"inflation",value=numeric_prefs["inflation"])
        updateCheckboxInput(session,"real_values",value=isTRUE(prefs$real_values))
      }
      # Apply a different display origin only after all numeric inputs are validated.
      updateRadioButtons(session,"time_mode",selected=d$mode)
      updateNumericInput(session,"starting_age",value=age)
      updateNumericInput(session,"starting_year",value=year)
      updateNumericInput(session,"horizon",value=h)
      updateSliderInput(session,"r0",value=rvect[1])
      updateSliderInput(session,"r1",value=rvect[2])
      updateSliderInput(session,"r2",value=rvect[3])
      new_origin <- if (d$mode=="age") age else if (d$mode=="calendar") year else 0
      for (i in seq_along(c("t0","t1","t2")))
        updateNumericInput(session,c("t0","t1","t2")[i],value=rvect[i+3]+new_origin)
      previous_origin(new_origin)
      rv$myList <- c(initial_timeline(h),events)
      shinyalert(title="Scenarij naložen",text="Načrt je uspešno obnovljen.")
    },error=function(e) shinyalert(title="Scenarija ni bilo mogoče naložiti",
                                   text=conditionMessage(e)))
  })

  comparison <- reactiveVal(NULL)
  observeEvent(input$save_comparison, { comparison(display_results()) })
  observeEvent(input$clear_comparison, { comparison(NULL) })

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
          event_cashflow = as.numeric(input$event_cashflow) *
            if (identical(input$amount_frequency,"monthly")) 12 else 1
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
        value = (get_yearly_investment(
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
        )) / if (identical(input$amount_frequency,"monthly")) 12 else 1
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

  allocation_data <- reactive({
    req(input$horizon, input$asset_strategy)
    years <- 0:as.integer(input$horizon)
    start <- if (input$asset_strategy=="equities") 100 else
      if (input$asset_strategy=="bonds") 0 else
      if (input$asset_strategy=="balanced") 60 else input$stock_begin
    finish <- if (input$asset_strategy=="glide") input$stock_finish else start
    stocks <- pmax(0,pmin(100,start+(finish-start)*years/max(1,input$horizon)))
    tibble(t=years, stocks=stocks, bonds=100-stocks)
  })
  output$allocation_plot <- renderPlotly({
    d <- allocation_data()
    english <- identical(input$language,"en")
    stock_name <- if (english) "Stocks" else "Delnice"
    bond_name <- if (english) "Bonds" else "Obveznice"
    p <- plotly::plot_ly(d, x=~t) %>%
      plotly::add_trace(y=~stocks, type="scatter", mode="lines", stackgroup="one",
        name=stock_name, line=list(width=0.5,color="#4A9B83"),
        fillcolor="#4A9B83", hovertemplate=paste0("%{y:.0f}%<extra>",stock_name,"</extra>")) %>%
      plotly::add_trace(y=~bonds, type="scatter", mode="lines", stackgroup="one",
        name=bond_name,line=list(width=0.5,color="#91A4AB"),
        fillcolor="#91A4AB",hovertemplate=paste0("%{y:.0f}%<extra>",bond_name,"</extra>"))
    ticks <- pretty(range(d$t),n=5)
    ticks <- ticks[ticks>=min(d$t) & ticks<=max(d$t)]
    plotly::layout(p,
      xaxis=list(title=list(text=""),tickmode="array",tickvals=ticks,
                 ticktext=display_year()(ticks),showgrid=FALSE,zeroline=FALSE),
      yaxis=list(title=list(text=""),range=c(0,100),ticksuffix="%",
                 dtick=25,zeroline=FALSE),
      margin=list(l=38,r=8,t=4,b=35),
      legend=list(orientation="h",x=.5,xanchor="center",y=-.22),
      paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })

  var_rate = reactive({
    if (identical(input$model_mode,"simple")) {
      years <- seq(min(events_to_db(rv$myList)$event_time),
                   max(events_to_db(rv$myList)$event_time))
      total <- max(1,input$horizon)
      start <- if (input$asset_strategy=="equities") 100 else
               if (input$asset_strategy=="bonds") 0 else
               if (input$asset_strategy=="balanced") 60 else input$stock_begin
      finish <- if (input$asset_strategy=="glide") input$stock_finish else start
      share <- pmax(0,pmin(100,start+(finish-start)*years/total))/100
      rate_data <- tibble(t=years,rate=share*.07+(1-share)*.03)
      return(list(rate=rate_data,
                  rate_plot=ggplot(rate_data,aes(t,rate))+geom_line(color="#245b7b")+
                  scale_y_continuous(labels=scales::percent)+labs(x="Leto modela",y="Donosnost")+
                  theme_minimal()))
    }
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
  
  # Explicit tick labels avoid Plotly's scientific notation (e.g. 2e+05).
  euro_axis <- function(x) {
    vapply(x, function(v) {
      if (abs(v) < 1e-8) return("0 €")
      if (abs(v) >= 1e6) return(paste0(format(round(v/1e6, 1), trim=TRUE, scientific=FALSE), "m €"))
      if (abs(v) >= 1000) return(paste0(format(round(v/1000, 0), trim=TRUE, scientific=FALSE), "k €"))
      paste0(format(round(v, 0), trim=TRUE, scientific=FALSE), " €")
    }, character(1))
  }
  format_time_axis <- function(plot_object, money = FALSE) {
    times <- range(events_to_db(rv$myList)$event_time)
    ticks <- pretty(times, n=7)
    ticks <- ticks[ticks >= times[1] & ticks <= times[2]]
    opts <- list(xaxis=list(title=list(text=""), tickmode="array",
                       tickvals=ticks, ticktext=display_year()(ticks)),
                 legend=list(orientation="h", x=.5, xanchor="center", y=-.19),
                 margin=list(l=65,r=18,t=12,b=58),
                 paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
    if (money) {
      yvalues <- unlist(lapply(plot_object$x$data, function(tr) as.numeric(tr$y)))
      yvalues <- yvalues[is.finite(yvalues)]
      if (length(yvalues)) {
        yticks <- pretty(range(c(0,yvalues)),n=5)
        opts$yaxis <- list(title=list(text=""),tickmode="array",
                           tickvals=yticks,ticktext=euro_axis(yticks),
                           zeroline=TRUE,zerolinecolor="#d3dce4")
      }
    } else opts$yaxis <- list(title=list(text=""))
    do.call(plotly::layout,c(list(p=plot_object),opts))
  }

  model_results <- reactive({
    req(length(rv$myList) > 0)
    plot_events(rv$myList, var_rate()$rate, borrowing_rate=input$borrowing_rate/100)$timeline
  })
  euro_label <- function(x) {
    paste0(format(round(x, 0), big.mark=" ", scientific=FALSE, trim=TRUE), " €")
  }
  output$final_balance <- renderText({
    euro_label(tail(display_results()$value_time, 1))
  })
  output$net_contributions <- renderText({
    euro_label(tail(display_results()$net_flows, 1))
  })
  output$total_returns <- renderText({
    euro_label(tail(display_results()$return_effect, 1))
  })

  display_results <- reactive({
    d <- model_results()
    if (isTRUE(input$real_values)) {
      factor <- (1+input$inflation/100)^d$event_time
      d$value_time <- d$value_time/factor
      d$net_flows <- d$net_flows/factor
      d$return_effect <- d$value_time-d$net_flows
    }
    d
  })
  output$comparison_summary <- renderText({
    old <- comparison()
    if (is.null(old)) return("Za primerjavo shrani trenutno projekcijo in spremeni predpostavke.")
    now <- tail(display_results()$value_time,1)
    prior <- tail(old$value_time,1)
    paste0("Razlika glede na shranjeni scenarij: ",euro_label(now-prior))
  })
  output$balance_plot_output <- renderPlotly({
    if (length(rv$myList) > 0) {
      my_plots <- plot_events(rv$myList, var_rate()$rate, borrowing_rate=input$borrowing_rate/100)
      
      d <- display_results()
      graph <- ggplot(d,aes(event_time)) +
        geom_line(aes(y=value_time,color="Trenutni scenarij"),linewidth=1.25) +
        geom_line(aes(y=net_flows,color="Neto tokovi"),linewidth=.8,linetype="dashed") +
        scale_color_manual(values=c("Trenutni scenarij"="#245B7B","Neto tokovi"="#4A9B83",
                                   "Primerjava"="#8995A5"),name=NULL) +
        labs(x=NULL,y=NULL)+theme_minimal()+
        theme(legend.position="bottom")
      prior <- comparison()
      if (!is.null(prior))
        graph <- graph+geom_line(data=prior,aes(x=event_time,y=value_time,
            color="Primerjava"),linetype="dotted",inherit.aes=FALSE)
      format_time_axis(ggplotly(graph), money=TRUE)
      
    }
  })
  
  
  output$cashflow_plot_output <- renderPlotly({
    if (length(rv$myList) > 0) {
      my_plots <- plot_events(rv$myList, var_rate()$rate, borrowing_rate=input$borrowing_rate/100)
      
      chart <- ggplotly(my_plots$event_plot)
      # Plotly trace names are created by ggplotly, not by the HTML translator.
      # Translate at render time so language changes invalidate this plot.
      lang <- input$language
      if (identical(lang, "en")) {
        for (i in seq_along(chart$x$data)) {
          trace <- chart$x$data[[i]]
          if (!is.null(trace$name)) {
            trace$name <- gsub("Prilivi", "Inflows", trace$name, fixed=TRUE)
            trace$name <- gsub("Odlivi", "Outflows", trace$name, fixed=TRUE)
          }
          if (!is.null(trace$hovertemplate)) {
            trace$hovertemplate <- gsub("Prilivi", "Inflows", trace$hovertemplate, fixed=TRUE)
            trace$hovertemplate <- gsub("Odlivi", "Outflows", trace$hovertemplate, fixed=TRUE)
          }
          chart$x$data[[i]] <- trace
        }
      }
      format_time_axis(chart, money=TRUE)
      
    }
  })
  
  output$interest_plot_output <- renderPlotly({
    if (length(rv$myList) > 0) {
      format_time_axis(ggplotly(var_rate()$rate_plot), money=FALSE)
      
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
