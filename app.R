#
# Name: ubicalculator
# Date: 22/04/2018
# Author: Sam Cormack
#
# This calculator lets the user design a Universal Basic Income (UBI)
# and progressive income tax system to pay for it in New Zealand. It uses
# the 2016 tax year income distribution for New Zealand after taxes and
# existing transfers.
#
library(shiny)
library(plotly)
source("calculations.R")
inc.df = read.csv("data/income_data_processed.csv",header=TRUE)
population.df = read.csv("data/population_age.csv",header=TRUE)

## Add in population over 15 years old not present in income data
over15pop = sum(population.df$population[population.df$age_lower_bound>=15])
pop.ird = sum(inc.df$n_people)
over15.noird = over15pop - pop.ird
inc.df$n_people[[1]] = inc.df$n_people[[1]] + over15.noird

## Calculate cumulative population and proportion
inc.df$cumulative_people = cumsum(inc.df$n_people)
inc.df$cumulative_proportion = inc.df$cumulative_people/over15pop

## Initial tax rates
threshold = c(0,14000,48000,70000)
rate = c(0.105,0.175,0.3,0.33)
tax.bracket.ini = data.frame(threshold,rate)

## Calculate initial after tax incomes
tax.initial = calculateTax(inc.df$taxable_income_per_person,tax.bracket.ini)
income.at.initial = inc.df$taxable_income_per_person-tax.initial

# Define UI
ui <- fluidPage(
  titlePanel(
    h1("Design a UBI")
  ),
  
  sidebarLayout(
    position = "right",
    sidebarPanel(
      ## Parameters describing the UBI
      h2("UBI parameters"),
      numericInput("basic.income",
                   h3("Basic income amount"),
                   value = 0.0,
                   step = 100.0
      ),
      sliderInput("basic.income.phaseout", h3("Basic income phaseout rate"),
                  min = 0.0, max = 1.0, value = 0.0
      ),
      
      ## Parameters describing the tax system
      h2("Marginal tax rates"),
      uiOutput("rate1slider"),
      
      numericInput(
        "thresh2",
        h3("1st income threshold"),
        value = tax.bracket.ini$threshold[2]
      ),
      uiOutput("rate2slider"),
      
      numericInput(
        "thresh3",
        h3("2nd income threshold"),
        value = tax.bracket.ini$threshold[3]
      ),
      uiOutput("rate3slider"),
      
      numericInput(
        "thresh4",
        h3("3rd income threshold"),
        value = tax.bracket.ini$threshold[4]
      ),
      uiOutput("rate4slider")
      
      # checkboxInput("bracket5","Add another tax bracket"),
      # 
      # conditionalPanel(
      #   condition = "input.bracket5==true",
      #   numericInput(
      #     "thresh5",
      #     h3("4th income threshold"),
      #     value = 0
      #   ),
      #   uiOutput("rate5slider")
      # )
    ),
    
    ## Plot old and new after tax incomes
    mainPanel(
      
      h2("Income distribution"),
      p("Choose a level for your UBI then adjust tax levels to pay for it"),
      # plotOutput("distPlot"),
      fluidRow(
        column(7,
          plotlyOutput("distPlotlyLow")
        ),
        column(5,
          plotlyOutput("distPlotlyHigh")
        )
      ),
      fluidRow(
        column(12,offset=4,
          h2("Surplus (deficit) per person"),
          textOutput("surplusMeasure")
        )
      )
    )
  )
  
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expressions - initialise new tax brackets
 threshold.new = reactive({
   c(0,input$thresh2,input$thresh3,input$thresh4)
 })
 rate.new = reactive({
  c(input$rate1,input$rate2,input$rate3,input$rate4)
 })
 
  
  # Calculate new after tax income
  income.bt.new = reactive({
    pmax(
      inc.df$taxable_income_per_person,
      input$basic.income + (1-input$basic.income.phaseout)*inc.df$taxable_income_per_person
    )
  })
  
 
  income.at.new = reactive ({
    income.bt.new() - calculateTax(
      income.bt.new(),data.frame(threshold=threshold.new(),rate=rate.new())
      )
  })
  
  
  ## Outputs
  ## Surplus/deficit relative to initial values
  surplus = reactive({
    sum((income.at.initial-income.at.new())*inc.df$n_people)/sum(inc.df$n_people)
  })
  
  output$surplusMeasure = renderText({
    surplus()
  })
  
  ## Main plot
  # output$distPlot = renderPlot({
  #   plot(inc.df$cumulative_proportion,income.at.initial,type="l",col="blue")
  #   lines(inc.df$cumulative_proportion,income.at.new(),col="red")
  # })
 
  # output$distPlotly = renderPlotly({
  #   data = data.frame(
  #     prop=inc.df$cumulative_proportion,
  #     bt=inc.df$taxable_income_per_person,
  #     old=income.at.initial,
  #     new=income.at.new()
  #   )
  #   plt = plot_ly(data, x = ~prop) %>%
  #   add_trace(y = ~bt, name ='Before tax',type='scatter', mode = 'lines')%>%
  #   add_trace(y = ~old, name = 'Baseline',type='scatter',mode = 'lines') %>%
  #   add_trace(y = ~new, name = 'New',type='scatter', mode = 'lines',fill='tonexty') 
  #   # layout(plt,yaxis=list(type="log"))
  # })
  
  output$distPlotlyLow = renderPlotly({
    data = data.frame(
      prop=inc.df$cumulative_proportion,
      bt=inc.df$taxable_income_per_person,
      old=income.at.initial,
      new=income.at.new()
    )
    data = subset(data,prop<=0.9)
    plt = plot_ly(data, x = ~prop) %>%
      add_trace(y = ~bt, name ='Before tax',type='scatter', mode = 'lines')%>%
      add_trace(y = ~old, name = 'Baseline after tax',type='scatter',mode = 'lines') %>%
      add_trace(y = ~new, name = 'New after tax',type='scatter', mode = 'lines',fill='tonexty') 
    layout(
      plt,
      xaxis=list(range=c(0.0,0.9),title="Percentile",tickformat="%"),
      yaxis=list(autorange=TRUE,title="Income (NZD)"),
      showlegend=FALSE
    )
  })
  
  output$distPlotlyHigh = renderPlotly({
    data = data.frame(
      prop=inc.df$cumulative_proportion,
      bt=inc.df$taxable_income_per_person,
      old=income.at.initial,
      new=income.at.new()
    )
    data = subset(data,prop>=0.9)
    plt = plot_ly(data, x = ~prop) %>%
      add_trace(y = ~bt, name ='Before tax',type='scatter', mode = 'lines')%>%
      add_trace(y = ~old, name = 'Baseline after tax',type='scatter',mode = 'lines') %>%
      add_trace(y = ~new, name = 'New after tax',type='scatter', mode = 'lines',fill='tonexty') 
    layout(
      plt,
      xaxis=list(range=c(0.9,1.0),title="Percentile",tickformat="%"),
      yaxis=list(autorange=TRUE,title=FALSE)
    )
  })
  
  
  
  ## Create rate sliders
  output$rate1slider = renderUI({
    sliderInput(
      "rate1",
      h3("Lowest tax rate"),
      min = 0.0,
      max = tax.bracket.ini$rate[[2]],
      value = tax.bracket.ini$rate[[1]] #min(tax.bracket.new$rate[[1]],tax.bracket.new$rate[[2]])
    )
  })
  
  output$rate2slider = renderUI({
    sliderInput(
      "rate2",
      h3("2nd tax rate"),
      min = tax.bracket.ini$rate[[1]],
      max = tax.bracket.ini$rate[[3]],
      value = tax.bracket.ini$rate[[2]]
      # max(
      #   tax.bracket.new$rate[[1]],
      #   min(tax.bracket.new$rate[[2]],tax.bracket.new$rate[[3]])
      # )
    )
  })
  
  output$rate3slider = renderUI({
    sliderInput(
      "rate3",
      h3("3rd tax rate"),
      min = tax.bracket.ini$rate[[2]],
      max = tax.bracket.ini$rate[[4]],
      value = tax.bracket.ini$rate[[3]]
      # value = max(
      #   tax.bracket.new$rate[[2]],
      #   min(tax.bracket.new$rate[[3]],tax.bracket.new$rate[[4]])
      # )
    )
  })
  
  output$rate4slider = renderUI({
    sliderInput(
      "rate4",
      h3("4th tax rate"),
      min = tax.bracket.ini$rate[[3]],
      max = 1.0,
      value = tax.bracket.ini$rate[[4]]
      # value = max(
      #   tax.bracket.new$rate[[4]],tax.bracket.new$rate[[3]]
      # )
    )
  })
  
  # output$rate5slider = renderUI({
  #   sliderInput(
  #     "rate5",
  #     h3("5th tax rate"),
  #     min = 0.0,
  #     max = 1.0,
  #     value = 0
  #     # value = max(
  #     #   tax.bracket.new$rate[[4]],tax.bracket.new$rate[[3]]
  #     # )
  #   )
  # })
  
  ## Update rate sliders
  observeEvent(input$rate1,{
    updateSliderInput(session,inputId="rate2",min=input$rate1)
  })
  observeEvent(input$rate2,{
    updateSliderInput(session,inputId="rate1",max=input$rate2)
    updateSliderInput(session,inputId="rate3",min=input$rate2)
  })
  observeEvent(input$rate3,{
    updateSliderInput(session,inputId="rate2",max=input$rate3)
    updateSliderInput(session,inputId="rate4",min=input$rate3)
  })
  observeEvent(input$rate4,{
    updateSliderInput(session,inputId="rate3",max=input$rate4)
    # updateSliderInput(session,inputId="rate5",min=input$rate4)
  })
  
 
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
