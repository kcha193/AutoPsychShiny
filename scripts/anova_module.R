

library(janitor)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(openxlsx)


# Module definition, new method
anova_1way_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(                                                                                              
        column(11,                                                                                                         
               wellPanel(h4("Data inputs"),
                         fluidRow(column(6,
                                         fileInput(ns("anova_input_file"),                                                                            
                                                   "Choose your file from Uni-Dim Rasch tab",
                                                   accept = c(".xlsx",".xls")
                                                   )
                                        ),
                                  column(6,
                                         fileInput(ns("anova_input_group"), 
                                                   "Choose your file from containing the groups", 
                                                   accept = c(".xlsx",".xls")
                                                   )
                                        )
                                  ),  # fluidRow                                                               
                                  
                                  h4("Select a group variable to perform ANOVA test"),
                                  uiOutput(ns("select_group")),
                                  h4("Diagnostic plots"),
                                  tabsetPanel(tabPanel("Box plot",
                                                       plotOutput(ns("bar_plot"))
                                                       ),
                                              tabPanel("Normality check", 
                                                       plotOutput(ns("normcheck")))
                                             ), 
                                  h4("ANOVA results"),
                                  tabsetPanel(tabPanel("ANOVA table", 
                                                       tableOutput(ns("anova_tab"))
                                                       ),
                                              tabPanel("Estimated marginal means",
                                                       tableOutput(ns("group_means"))
                                                       ),
                                              tabPanel("Tukey Pairwise comparison",
                                                       tableOutput(ns("pairwise_compare"))
                                                       )
                                              )
                                 ) # wellPanel wrapper
                       ) # column wrapper
             ) # fluidRow wrapper
  ) 
  }


anova_1way_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- NS(id)
      
      
      # Data from the the report ----------------------------------------------
      anova_data <- 
        reactive({
          
        file <- input$anova_input_file
      
        req(file)
    
        read.xlsx(file$datapath, sheet =  "Person Results")
      })
      
      
      # User input data containing the groups to compare -----------------------
      group_data <- 
        reactive({
          
          file <- input$anova_input_group

          req(file)
          
          read.xlsx(file$datapath, sheet = 1)
        })
      
      
      # Create drop-down menu for group comparison -------------------------------
      output$select_group <- 
        renderUI({
        
        group_dat <- 
          group_data() %>% clean_names() 
        
        selectInput(ns("category"), "", names(group_dat)[-1])
        })
      
      
      # Get the final data -----------------------------------------------------
      final_dat <- reactive({
        
        anova_dat <- anova_data() %>% 
          clean_names() %>% 
          select(participant_id, ability_theta)
        
        group_dat <- group_data()%>% 
          clean_names() %>% 
          select(participant_id, matches(req(input$category)))
        
        names(group_dat) <- 
          gsub(input$category, "Group", names(group_dat))
        
        final_dat <- 
          anova_dat %>% left_join(group_dat)
        
        final_dat
      })
      
      
      
      # Boxplot ----------------------------------------------------------------
      output$bar_plot <- renderPlot({
        
        ggplot(final_dat(), aes(x = Group, y = ability_theta)) +
          geom_boxplot() +
          xlab(req(input$category)) +
          ylab("Ability measure") +
          theme_classic()
      })
      
      
      
      # Fitting the ANOVA model ------------------------------------------------
      fit_anova <- 
        reactive({
          lm(ability_theta ~ Group, 
               data =  final_dat())
        })
      
      # Testing for normality plot ---------------------------------------------
      output$normcheck <- renderPlot({
        s20x::normcheck(resid(fit_anova()), s = TRUE)
      })
      

      # ANOVA results ----------------------------------------------------------
      output$anova_tab <- renderTable({
        
        tab <- anova(fit_anova()) %>% as.data.frame()
        
        tab
        
      }, rownames = TRUE, digits = 4, hover = TRUE, striped = TRUE, 
      bordered = TRUE)
      
      
      
      # Estimated marginal means -----------------------------------------------
      output$group_means <- renderTable({
        
        tab <-
          emmeans(fit_anova(), ~ Group) %>%
          as.data.frame()
        
        tab
      }, digits = 4, hover = TRUE, striped = TRUE, bordered = TRUE)
      
      
      ## Tukey Honest Significant Differences ----------------------------------
      output$pairwise_compare <- renderTable({
   
        tab <- 
          pairs(emmeans(fit_anova(), ~ Group), 
                adjust = "none") %>% 
          rbind(adjust = "tukey") %>%
          as.data.frame() 
        
        tab 
      }, digits = 4, hover = TRUE, striped = TRUE,bordered = TRUE)
    }
  )
}

# Use the module in an application
# ui <- fluidPage(
#   anova_UI("anova")
# )
# server <- function(input, output, session) {
#   anova_Server("anova")
# }
# shinyApp(ui, server)


# Module definition, new method
anova_2way_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(                                                                                              
        column(11,                                                                                                         
               wellPanel(h4("Data inputs"),
                         fluidRow(column(6,
                                         fileInput(ns("anova_input_file"),                                                                            
                                                   "Choose your file from Uni-Dim Rasch tab",
                                                   accept = c(".xlsx",".xls")
                                                   )
                                        ),
                                  column(6,
                                         fileInput(ns("anova_input_group"), 
                                                   "Choose your file from containing the groups", 
                                                   accept = c(".xlsx",".xls")
                                                   )
                                        )
                                  ),  # fluidRow                                                               
                                  
                                  h4("Select two group variables to perform ANOVA test"),
                                  fluidRow(column(6, uiOutput(ns("select_group_A"))),
                                           column(6, uiOutput(ns("select_group_B")))),
                                  h4("Diagnostic plots"),
                                  tabsetPanel(tabPanel("Box plot",
                                                       plotOutput(ns("bar_plot"))
                                                       ),
                                              tabPanel("Normality check", 
                                                       plotOutput(ns("normcheck")))
                                             ), 
                                  h4("ANOVA results"),
                                  tabsetPanel(tabPanel("ANOVA table", 
                                                       tableOutput(ns("anova_tab"))
                                                       ),
                                              tabPanel("Estimated marginal means",
                                                       tableOutput(ns("group_means"))
                                                       ),
                                              tabPanel("Tukey Pairwise comparison on Group A",
                                                       tableOutput(ns("pairwise_compareA"))
                                                       ),
                                              tabPanel("Tukey Pairwise comparison on Group B",
                                                       tableOutput(ns("pairwise_compareB"))
                                                       )
                                              )
                                 ) # wellPanel wrapper
                       ) # column wrapper
             ) # fluidRow wrapper
  ) 
  }


anova_2way_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- NS(id)
      
      
      # Data from the the report ----------------------------------------------
      anova_data <- 
        reactive({
          
        file <- input$anova_input_file
      
        req(file)
    
        read.xlsx(file$datapath, sheet =  "Person Results")
      })
      
      
      # User input data containing the groups to compare -----------------------
      group_data <- 
        reactive({
          
          file <- input$anova_input_group

          req(file)
          
          group_data <- read.xlsx(file$datapath, sheet = 1)
          
          if(ncol(group_data) < 3){ 
            stop("There are only one group variable in this input data, so cannot perform Two-Way ANOVA test.")
          } else {
            
            return(group_data)
          }
          
          
        })
      
      
      # Create drop-down menu for group comparison -------------------------------
      output$select_group_A <- 
        renderUI({
        
        group_dat <- 
          group_data() %>% clean_names() 
        
        selectInput(ns("category_A"), "Group A:", names(group_dat)[-1])
        })
      
      # Create drop-down menu for group comparison -------------------------------
      output$select_group_B <- 
        renderUI({
        
        group_dat <- 
          group_data() %>% clean_names() 

        group_left <- names(group_dat)[-1]
        
        selectInput(ns("category_B"), "Group B:", 
                    group_left[group_left != req(input$category_A)])
        })
      
      # Get the final data -----------------------------------------------------
      final_dat <- reactive({
        
        anova_dat <- anova_data() %>% 
          clean_names() %>% 
          select(participant_id, ability_theta)
        
        group_dat <- group_data()%>% 
          clean_names() %>% 
          select(participant_id,
                 matches(req(input$category_A)),
                 matches(req(input$category_B)))
        
        names(group_dat) <- 
          gsub(input$category_A, "Group_A", names(group_dat))
        
        names(group_dat) <- 
          gsub(input$category_B, "Group_B", names(group_dat))
        
        final_dat <- 
          anova_dat %>% 
          left_join(group_dat) %>% 
          mutate(Group_A = factor(Group_A),
                 Group_B = factor(Group_B))
        
        final_dat
      })
      
      
      
      # Boxplot ----------------------------------------------------------------
      output$bar_plot <- renderPlot({

        ggplot(final_dat(), aes(x = Group_A, 
                                y = ability_theta, 
                                fill = Group_B)) +
          geom_boxplot() +
          labs(y = "Ability measure", x = req(input$category_A),
               fill = req(input$category_B)) +
          theme_classic()
      })
      
      # Fitting the ANOVA model ------------------------------------------------
      fit_anova <- 
        reactive({
          lm(ability_theta ~ Group_A*Group_B, 
               data =  final_dat())
        })
      
      # Testing for normality plot ---------------------------------------------
      output$normcheck <- renderPlot({
        s20x::normcheck(resid(fit_anova()), s = TRUE)
      })
      

      # ANOVA results ----------------------------------------------------------
      output$anova_tab <- renderTable({
        
        tab <- anova(fit_anova()) %>% as.data.frame()
        
        tab
        
      }, rownames = TRUE, digits = 4, hover = TRUE, striped = TRUE, 
      bordered = TRUE)
      
      
      
      # Estimated marginal means -----------------------------------------------
      output$group_means <- renderTable({
        
        tab <-
          emmeans(fit_anova(), ~ Group_A:Group_B) %>%
          as.data.frame()
        
        tab
      }, digits = 4, hover = TRUE, striped = TRUE, bordered = TRUE)
      
      
      ## Tukey Honest Significant Differences ----------------------------------
      output$pairwise_compareA <- renderTable({
   
        tab_A <- 
          pairs(emmeans(fit_anova(), ~ Group_A|Group_B), 
                adjust = "none") %>% 
          rbind(adjust = "tukey") %>%
          as.data.frame() 
        

        tab_A
      }, digits = 4, hover = TRUE, striped = TRUE,bordered = TRUE)
   
    
      output$pairwise_compareB <- renderTable({
   
        tab_B <- 
          pairs(emmeans(fit_anova(), ~ Group_B|Group_A), 
                adjust = "none") %>% 
          rbind(adjust = "tukey") %>%
          as.data.frame() 

        tab_B
      }, digits = 4, hover = TRUE, striped = TRUE,bordered = TRUE)
    }
  )
}


# ui <- fluidPage(
#   anova_2way_UI("anova")
# )
# server <- function(input, output, session) {
#   anova_2way_Server("anova")
# }
# shinyApp(ui, server)