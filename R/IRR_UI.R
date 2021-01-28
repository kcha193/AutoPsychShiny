



# Module definition, new method
IRR_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    wellPanel(
      h4("Inter-Rater Reliability Tool"),
      p("This tool is particularly useful for test and rubric developers interested 
                                    in improving and validating items and rubrics that involve judgements of 
                                    student competence, for example, 'Essay Organization'."),
      p(""),
      p("The tool computes different varieties of the intra-class correlation 
                                   coefficient, which is an index of inter-rater reliability."),
      p("")
    ),
    wellPanel(
      h4("1. Prepare data"),
      p("Before using the ICC tool, ensure that your data meet the following 
                                   requirements:"),
      p(""),
      p("(a) A csv formatted spreadsheet with subject (students) as rows and 
                                   raters (or, coders) as columns (e.g., Rater_1, Rater_2, Rater_3); and,"),
      p(""),
      p("(b) The ICC tool handles missing data listwise, meaning that when 
                                   a missing value is identified, the entire row (student/case) is removed from 
                                   the analysis (the Krippendorff's alpha may be more 
                                   suitiable when missing data is present).")
    ),
    wellPanel(
      h4("2. Upload your inter-rater reliability data (csv)"),
      p(""),
      fileInput(ns("input_file"), "Choose your file (.csv)",                  # The file is observed by the UI when it is uploaded successfully.
                multiple = FALSE, accept = c("text/csv", "text/comma-separated-values, 
                                                                    text/plain", ".csv", ".xlsx", ".xls"
                )
      ) %>%                                                      # Pipe into new function
        shinyInput_label_embed(icon("question-circle") %>%                # Pipe into other function; Other icons are also possible, see ?shiny::icon or https://fontawesome.com/icons?d=gallery&q=question 
                                 bs_embed_tooltip(title = "If you would like to cite this tool, feel free: Courtney, M. G. R., & Xxxxx, X. (XXXX). Auto-psych: a novel shiny app for the psychometric analysis and scoring of assessment and survey data. Xxxxxxxxx and Xxxxxxxxx.")
        )                                          # End of shinyInput_labelembed function (piping occurs withing that function)
    ),
    wellPanel(
      h4("3. Specify the construct that the raters are measuring and identify the students"),
      p(""),
      textInput(ns("construct"), "Scored Topic:", placeholder = "ScoredTopic"),
      bsTooltip(ns("construct"), "E.g., Essay Organization", "right",
                options = list(container = "body")
      ),
      
      textInput(ns("population"), "Focal group:", placeholder = "Students"),  # 2. input: sample of interest (though modelling has population-bsed assumptions)
      bsTooltip(ns("population"), "E.g., Central School Grade 10 Students", "right",
                options = list(container = "body")
      )
    ),
    wellPanel(
      h4("4. Specify the model"),
      p(""),
      
      selectizeInput(ns("model"), "Select either the One-way or Two-way model",
                     choices = c("One-way" = "oneway", 
                                 "Two-way" = "twoway")
      ) %>%              
        shinyInput_label_embed(icon("question-circle") %>%
                                 bs_embed_tooltip(title = "(a) for One-way, raters randomly sampled for each subject; (b) for Two-way, the same raters are used across across subjects"
                                 )
        ),
      p("")
    ),
    wellPanel(
      h4("5. Select the Type"),
      p(""),
      
      selectizeInput(ns("type"), "Select either Agreement or Consistency:", 
                     choices = c("Agreement" = "agreement", 
                                 "Consistency" = "consistency")
      ) %>%                           
        shinyInput_label_embed(icon("question-circle") %>%
                                 bs_embed_tooltip(title = "(a) For agreement, IRR is characterized by agreement in absolute terms across raters; (b) for consistency, IRR is characterised by correlation in scores across raters."
                                 )
        ),                                          # End of shinyInput_labelembed function (piping occurs withing that function)
    ),
    wellPanel(
      h4("6. Select the Unit"),
      p(""),
      
      selectizeInput(ns("unit"), "Select either Agreement or Consistency:", 
                     choices = c("Single" = "single", 
                                 "Average" = "average")
      ) %>%                           
        shinyInput_label_embed(icon("question-circle") %>%
                                 bs_embed_tooltip(title = "(a) For single, each student score is not an aggregate of other scores; (b) for average, each student score is an aggregate of several scores."
                                 )
        ),                                          # End of shinyInput_labelembed function (piping occurs withing that function)
    ),
    wellPanel(
      h4("7. Select Confidence Intervals"),
      sliderInput(ns("conf.level"), 
                  "Specify confidence interval level for ICC statistic", 
                  min = 0.80, max = 0.99, value = 0.95, step = 0.01) %>%
        shinyInput_label_embed(icon("question-circle") %>%
                                 bs_embed_tooltip(title = "95% is a common confidnce interval for this statistic.")
        ),                                          # End of shinyInput_labelembed function (piping occurs withing that function)
      p("")
    ),
    wellPanel(
      h4("8. Include your own recommendations"),
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
      )                                                                                             # when conditions met in server logic), renamed as "Generate PDF report and spreadsheet'
    )     # wellPanel final
  )       # taglList wrapper
}         # function wrapper