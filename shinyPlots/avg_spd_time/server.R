library(shiny)
library(ggplot2)

#load data frame runs_racer.rda which contains all runs 
#for every person who participated in at least one race
# load(strava_df/runs_racer.rda)

shinyServer(function(input, output){
  output$plot <-renderPlot({
  line_plot <- runs_racer %>% filter(id == input$num, start_date >= input$dates[1], start_date <=input$dates[2]) %>% ggplot(aes(start_date, avg_speed)) + geom_line() + geom_point(aes(color = workout_type), size = 3)
    print(line_plot)
  })
  output$tab <- renderDataTable(
    runs_racer %>% filter(id == input$num, workout_type == 1) %>% select(start_date, distance),
    options = list(paging = FALSE, searching = FALSE)
  )
})