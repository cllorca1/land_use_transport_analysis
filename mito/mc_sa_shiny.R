library(data.table)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr) 
library(reshape)
library(shiny)
library(shinydashboard)
library(plotly)
library(processx)
library(leaflet)
library(sf)
library(tmap)
library(rgdal)
library(here)

mode_order = c("autoDriver","autoPassenger","train","tramOrMetro","bus", "bicycle", "walk")
mode_colors = c("autoDriver" = "#878787",
                "autoPassenger" = "#a9a9a9",
                "train" = "#789ec9",
                "tramOrMetro" = "#5c768d",
                "bus" = "#50546d",
                "bicycle" = "#87c77d",
                "walk" = "#53815b")





ui = dashboardPage(
  dashboardHeader(title = "Mode choice sensistivity analyis"),
  dashboardSidebar(
    actionButton("update", "Update", width = 100),
    sliderInput(inputId = "factorCar",
                "xCarTime",
                value = 1,
                min = 0.2,
                max = 2.8,
                step = 0.2),
    sliderInput(inputId = "factorPt",
                "xPtTime",
                value = 1,
                min = 0.2,
                max = 2.8,
                step = 0.2),
    sliderInput(inputId = "factorCarCost",
                "xCarCost",
                value = 1,
                min = 0,
                max = 5,
                step = 1),
    sliderInput(inputId = "factorPtCost",
                "xPTCost",
                value = 1,
                min = 0,
                max = 5,
                step = 1),
    sliderInput(inputId = "distance", 
                "Distance",
                value = 10,
                min = 0,
                max = 145, 
                step = 5)
    
  ),
  dashboardBody(
    tabBox(
      tabPanel(
        title = "Comparison modal share",
        plotlyOutput("comparison", width = "100%")
      ),
      tabPanel(
        title = "Modal share by distance",
        plotlyOutput("byDist", width = "100%")
      )
    )
  )
)


server = function(input, output){
  
  
  results_long = eventReactive(input$update, {
    results = read_csv("modeChoiceSensitivity.csv") %>% select(-privateAV, -sharedAV, -pooledTaxi)
    
    results_long = results %>% pivot_longer(cols = c(autoDriver,autoPassenger,bicycle,bus,train,tramOrMetro,walk), names_to  = "mode", values_to = "p")
    
    
    results_long$p = round(results_long$p,5)
    results_long$distance = round(results_long$distance,0)
    results_long$factorCar = round(results_long$factorCar,1)
    results_long$factorPt = round(results_long$factorPt,1)
    results_long$factorCarPrice = round(results_long$factorCarPrice, 1)
    results_long$factorPtPrice = round(results_long$factorPtPrice, 1)
    
    results_long$mode = factor(results_long$mode, levels = mode_order)
    results_long$income = factor(results_long$income, levels = c(2000,10000), labels = c("low income", "high income"))
    results_long
    
  })
  
  
  
  
  output$comparison = renderPlotly({
    this_results_long_reference = results_long() %>% filter(factorPt == 1,
                                                          factorPtPrice == 1,
                                                          factorCarPrice == 1,
                                                          factorCar == 1,
                                                          distance == input$distance) %>% 
      mutate(case = "A: reference")
    
    
    this_results_long_subset = results_long() %>% filter(factorPt == input$factorPt,
                                                       factorPtPrice == input$factorPtCost,
                                                       factorCarPrice == input$factorCarCost,
                                                       factorCar == input$factorCar,
                                                       distance == input$distance) %>%
      mutate(case = "B: comparison")
    
    this_results_long_subset = this_results_long_subset %>% bind_rows(this_results_long_reference)
    
    p = ggplot(this_results_long_subset, aes(x = case, y = as.numeric(p), fill = mode, color = mode)) +
      geom_bar(stat  ="identity") +
      scale_fill_manual(values = mode_colors) + scale_color_manual(values = mode_colors) +
      facet_grid(income~as.factor(purpose)) +
      ylab("Modal share") + xlab("Travel time factor (x CAR time)") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(p, height = 800, width = 1800)
    
    
  })
  
  output$byDist = renderPlotly({
    
  
    this_results_long_subset = results_long() %>% filter(factorPt == input$factorPt,
                                                         factorPtPrice == input$factorPtCost,
                                                         factorCarPrice == input$factorCarCost,
                                                         factorCar == input$factorCar) 
    
    
    
    p = ggplot(this_results_long_subset, aes(x = distance, y = as.numeric(p), fill = mode, color = mode)) +
      geom_bar(stat  ="identity") +
      scale_fill_manual(values = mode_colors) + scale_color_manual(values = mode_colors) +
      facet_grid(income~as.factor(purpose)) +
      ylab("Modal share") + xlab("Travel time factor (x CAR time)") + 
      theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(p, height = 800, width = 1800)
    
    
  })
  
  
}



shinyApp(ui, server)

