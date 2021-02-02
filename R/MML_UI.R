



# Module definition, new method
MML_UI <- function(id) {
  ns <- NS(id)

  tagList(
     wellPanel(
       h4("Rasch analysis tool"),
       p("This tool is useful for improving the quality of tests and developmental 
       rubrics that focus on measuring a single construct or skill, such as student 
         reading abilility."),
       p(""),
       p("The tool takes an item-response matrix (i.e., a spreadsheet of student 
          test results) and produces a detailed narrated technical report and organized 
          spreadsheets that reflect the function of the test and each question."),
       p(""),
       p("The report is based on the application of classical test theory (CTT) 
       and item-response theory (IRT; here, a unidimensional Rasch, or 1PL, model). 
       The analysis uses a specialized scoring algorithm that places estimates 
       of student ability and item difficulty on the same scale. This enables 
       educators to identify sets of questions and associated skills that students 
       might be ready to tackle with additional support."),
       p("")
              ),
    wellPanel(
       h4("1. Prepare data"),
       p("Before using the tool, ensure that your data meet the following requirements:"),
       p(""),
       p("(a) The header of the csv file (top row) includes consistent numbering 
         that includes ones and 10s columns. E.g., Item.01, Item.02,... Item.20 
         (not Item.1, Item.2,... Item.20)"),
       p(""),
       p("(b) Under the row of item descriptors (the header), item-responses may 
         include dichotomous (0, 1) or polytomous (0, 1, 2... max 9) data;"),
       p(""),
       p("(c) A column specifying student (case) identification cannot be included 
         (simply, outputs specific to students, e.g., ability and student fit estimates, 
         remain in the original order); and,"),
       p(""),
       p("(d) Some missing data (blanks) are handled by the tool, though users should 
         consider the meaning of such instances and recode if appropriate.")
               ),
       wellPanel(
       h4("2. Upload your item-response file (csv)"),
       p(""),
       fileInput(ns("input_file"), "Choose your file (.csv)", # The file is observed by the UI when it is uploaded successfully.
                  multiple = FALSE, accept = c("text/csv", "text/comma-separated-values", 
                                               "text/plain", ".csv", ".xlsx", ".xls"
                                              )
                ) %>%                                                           # Pipe into new function
       shinyInput_label_embed(icon("question-circle") %>%                       # Pipe into other function; Other icons are also possible, see ?shiny::icon or https://fontawesome.com/icons?d=gallery&q=question 
       bs_embed_tooltip(title = "If you would like to cite this tool, feel free: 
                        Courtney, M. G. R., & Xxxxx, X. (XXXX). Auto-psych: a novel 
                        shiny app for the psychometric analysis and scoring of assessment 
                        and survey data. Xxxxxxxxx and Xxxxxxxxx."
                        )
       )                                                 # End of shinyInput_labelembed function (piping occurs withing that function)
       ),
     wellPanel(
       h4("3. Specify construct and focal group"),
       p(""),
       textInput(ns("construct"), "Construct:", placeholder = "Test Topic"),
       bsTooltip(ns("construct"), "E.g., Numeracy or Literacy", "right",
                 options = list(container = "body")
                ),
    
       textInput(ns("population"), "Focal group:", placeholder = "Students"),       # 2. input: sample of interest (though modelling has population-bsed assumptions)
       bsTooltip(ns("population"), "E.g., Central School Grade 10 Students", "right",
                 options = list(container = "body")
                )
              ),
     wellPanel(
       h4("4. Specify settings for CTT analysis"),
       p(""),
       p(""),
    
       sliderInput(ns("disc.threshold"), "Flag item-total(rest) correlations lower than:",
                   min = 0, max = 1, value = 0.1, step = 0.01) %>%
       shinyInput_label_embed(icon("question-circle") %>%
       bs_embed_tooltip(title = "Items in the test that correlate negatively with 
                        the total score will be flagged automatically in red in 
                        the report. However, you can also flag items in blue that 
                        only correlated slightly with the total score in the test 
                        by selecting a lower limit here."
                        )
       ),                                                # End of shinyInput_labelembed function (piping occurs withing that function)
             
       sliderInput(ns("ci.level"), "Specify confidence interval level for item-total(rest) 
                   correlations:", min = 0.80, max = 0.99, value = 0.95, step = 0.01) %>%
       shinyInput_label_embed(icon("question-circle") %>%
                                bs_embed_tooltip(title = "Both upper and lower confidence intervals are also 
                        reported for item-total(rest) correlations. Lower CIs below 
                        zero are flagged red."
                        )
                             ),                                                 # End of shinyInput_labelembed function (piping occurs withing that function)
    
       selectizeInput(ns("NA.Delete"),
                      "If there are missing responses, for CTT statistics...",
                      choices = c("Delete cases listwise" = TRUE, 
                                  "Change missing values to zero" = FALSE)
                      ) %>%              
       shinyInput_label_embed(icon("question-circle") %>%
       bs_embed_tooltip(title = "Choose your option for handling missing values."
                       )
                             ),                                                 # End of shinyInput_labelembed function (piping occurs withing that function)
       p(""),
       p("")
               ),
     wellPanel(
       h4("5. Specify settings for Rasch analysis"),
       p(""),
    
       selectizeInput(ns("constraint"), "Constraint:", 
                      choices = c("Cases" = "cases", "Item" = "item")
                     ) %>%                           
       shinyInput_label_embed(icon("question-circle") %>%
       bs_embed_tooltip(title = "Analysis for the report includes a specialized 
                        item-response theory analysis of the data: A one-parameter 
                        logistic (Rasch-based) model is applied to the data. For 
                        this to occur, the average item difficulty level (delta) 
                        should be constrained (to zero), or the average student 
                        ability level (theta) ability should be constrained (to zero)."
                        )
       ),  # End of shinyInput_labelembed function (piping occurs withing that function)
             
       selectizeInput(ns("node.sequence"), "Assumed discretized population profile:",
                      choices = c("seq(-6,6,len=21)" = "-6,6,21", "seq(-4,4,len=21)" = "-4,4,21",
                                  "seq(-5,5,len=21)" = "-5,5,21", "seq(-7,7,len=21)" = "-7,7,21",
                                  "seq(-8,8,len=21)" = "-8,8,21", "seq(-9,9,len=21)" = "-9,9,21",
                                  "seq(-6,6,len=31)" = "-6,6,31", "seq(-6,6,len=41)" = "-6,6,41",
                                  "seq(-6,6,len=51)" = "-6,6,51", "seq(-6,6,len=61)" = "-6,6,61",
                                  "seq(-6,6,len=71)" = "-6,6,71", "seq(-6,6,len=81)" = "-6,6,81",
                                  "seq(-6,6,len=91)" = "-6,6,91", "seq(-7,7,len=21)" = "-7,7,21",
                                  "seq(-8,8,len=91)" = "-8,8,21", "seq(-9,9,len=91)" = "-9,9,21",
                                  "seq(-10,10,len=91)" = "-10,10,21"
                                    )
                      ) %>%
       shinyInput_label_embed(icon("question-circle") %>%
       bs_embed_tooltip(title = "The node sequence specifies the lowest and highest 
                        student abilities (theta; default -6 to 6) assumed to exist 
                        in the broader population of the focal group. 'Len' specifies 
                        the number of breaks, or discretized points for that broader 
                        population. Note that the default sequence is 'seq(-6, 6, 
                        len=21)' with shorter node sequences enabling faster computation."
                        )
                              ),
    
       selectizeInput(ns("conv"), "Convergence criterion", choices = c("0.0001",
                      "0.001", "0.01", "0.1")
                     ) %>%
       shinyInput_label_embed(icon("question-circle") %>%
       bs_embed_tooltip(title = "This value represents the acceptable level of tolerance 
                        for which the IRT model represent the data; larger values 
                        enable faster computation, though 0.0001 is default."
                        )
                              ),
    
       selectizeInput(ns("maxiter"), "Maximum iterations", choices = c(1000, 5000,
                      20000, 100000)
                      ) %>%
       shinyInput_label_embed(icon("question-circle") %>%
       bs_embed_tooltip(title = "This value represents the maximum number of computational 
                        steps permitted for the model to represent the data; lower 
                        values enable faster computation, though 1000 is default."
                        )
                             ),
    
       selectizeInput(ns("p.fit.threshold"), "Flag cases (persons) with person-fit statistics 
                      higher than:", choices = c(3.00, seq(2, 2.90, .10), 
                                                 seq(3.10, 5, .10))             # Sequences with 3.00 missing enable default to be set at 3.00.
                      ) %>%
       shinyInput_label_embed(icon("question-circle") %>%
       bs_embed_tooltip(title = "The higher the person-fit (outfit) values the more 
                        unusual the scoring pattern for the student. Default lower 
                        limit here is 3.0 (common) but this can be adjusted as per 
                        user preference. Here we can identify students with the 
                        most unexpected response pattern."
                        )
                              ),
       p(""),
       p("")
             ),
     wellPanel(
       h4("6. Specify graphical settings"),
       p(""),
    
       selectizeInput(ns("color.choice"), "Graphical color scheme:",
                      choices = c("Eurasian Steppe", "Deep Code", "Commercial Overreach",
                                  "Take a Trip", "Pohutukawa Beach", "Southland Coal")
                     ) %>%            
       shinyInput_label_embed(icon("question-circle") %>%
       bs_embed_tooltip(title = "Choose from six color schemes for the graphs in 
                        your preport ;-)"
                       )
                             ),
    
       selectizeInput(ns("binwidth"), "Bin width (width of WrightMap columns)",
                      choices = c(0.25, seq(.10, .20, .05), seq(.30, 1.0, .05)  # Sequences with 0.25 missing enable default to be set at 0.25.
                                 )   
                     ) %>%
       shinyInput_label_embed(icon("question-circle") %>%
       bs_embed_tooltip(title = "This specification allows for the width of the 
                        columns in the WrightMap to be specified; the WrightMap 
                        places students and questions on the same chart enabling 
                        an examination of progressive item difficulty and person 
                        ability."
                        )
                              ),
       p(""),
       p("")
                 ),
     wellPanel(
       h4("7. Include your own recommendations"),
       p(""),
    
       textAreaInput(ns("recommendations"), "Notes:", 
                     placeholder = "There are no notes for this report",
                     height = '150px'),
       bsTooltip(ns("recommendations"),
                 "These notes will be reported at the start of the PDF technical 
                 report. Make any notes you like about the original data or report 
                 itself.",
                 "right",
                 options = list(container = "body")
                ),
    
     )    # wellPanel final
  )       # taglList wrapper
}         # function wrapper