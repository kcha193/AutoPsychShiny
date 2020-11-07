

library(janitor)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(openxlsx)


# Module definition, new method
anova_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("anova_input_file"),                                                                            
              "Choose your file from Uni-Dim Rasch tab",
              accept = c(".xlsx",
                         ".xls")),
    fileInput(ns("anova_input_group"), 
              "Choose your file from containing the groups", 
              accept = c(".xlsx",
                         ".xls")),
    uiOutput(ns("select_group")),
    h4("Data"), 
    tableOutput(ns("test_tab"))
  )
}




anova_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- NS(id)
      
      anova_data <- 
        reactive({
          
        file <- input$anova_input_file
        #ext <- tools::file_ext(file$datapath)
        
        req(file)
        #validate(need(ext == "csv", "Please upload a csv file"))
        
        read.xlsx(file$datapath, sheet =  "Person Results")
      })
      
      group_data <- 
        reactive({
          
          file <- input$anova_input_group
          #ext <- tools::file_ext(file$datapath)
          
          req(file)
          #validate(need(ext == "csv", "Please upload a csv file"))
          
          read.xlsx(file$datapath, sheet = 1)
        })
      
      output$select_group <- 
        renderUI({
        
        group_dat <- 
          group_data()%>% 
          clean_names() 
        
        selectInput(ns("variable"), 
                    "Select a group variable to perform ANOVA test",
                   names(group_dat)[-1])
        })
      
      final_dat <- reactive({
        anova_dat <- anova_data() %>% 
          clean_names() %>% 
          select(participant_id, ability_theta)
        
        group_dat <- group_data()%>% 
          clean_names() %>% 
          select(participant_id, matches(input$variable))
        
        
        final_dat <- 
          anova_dat %>% left_join(group_dat)
        
        final_dat
      })
      
      output$test_tab <- renderTable({
        final_dat()
        })
      
      
    }
  )
}

# Use the module in an application
ui <- fluidPage(
  anova_UI("ANOVA")
)
server <- function(input, output, session) {
  anova_Server("ANOVA")
}
shinyApp(ui, server)