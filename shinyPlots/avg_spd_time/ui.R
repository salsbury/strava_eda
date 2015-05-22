shinyUI(fluidPage(
  titlePanel("Time Series of Avg_Speed Run"),
  sidebarLayout(
    sidebarPanel(
      selectInput("num", "Select ID of Racer", choices = unique(runs_racer$id), selected = 1),
      dateRangeInput("dates", "Choose the range of dates.", start = "2006-01-01"),
      submitButton("Submit"),
      dataTableOutput("tab")
      ),
    mainPanel(plotOutput("plot"))
    )
  ))