
# Loading the libraries that are related to the Shiny interface

# Other options: bootstrap, 
# UI (front end) and server side (server side logic) shiny::fluidPage is main function

library(shiny)  
library(tuneR)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(shinyFiles)
library(shinyBS)
library(bsplus)
library(shinyWidgets)

# This line of code will set the maximum allowed file size of uploads (3MB), adjustable to computational power.
options(shiny.maxRequestSize = 3*1024^2)

shinyUI(fluidPage(
  tabsetPanel(
    tabPanel(
      "Home",
      fluid = TRUE,
      theme = shinytheme("cosmo"),
      tags$style(type="text/css",
                 "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",  # font
                 "label {font-size: 12px;}",
                 ".recalculating {opacity: 1.0;}",
                 " * {font-family: Open Sans;
                                                     font-weight: 500;
                                                     line-height: 1.1}"
      ),
      
      setBackgroundImage(src = "shiny_background5.png"),
      
      shinyjs::useShinyjs(),
      
      tags$h1("Automated Psychometrics",
              tags$img(src = "white psych.png", 
                       height = 160, 
                       width = 160, 
                       style = "float:right;margin-top:1px;"
              ),
              tags$style(HTML
                         ("h1{
                                                               font-family: 'Open Sans';
                                                               font-weight: 500;
                                                               line-height: 1.1;
                                                               font-size: 60px;
                                                               color: #FFFFFF;
                                                               }"
                         )
              )
      ),
      
      tags$h2("Dedicated to providing a link between educational research and practice.",                        # h2 header is the subtitle underneath the h1 header
              tags$style(
                HTML("h2{
                                                                font-family: 'Open Sans';
                                                                font-weight: 500;
                                                                line-height: 1.1;
                                                                font-size: 18pt;
                                                                color: #FFFFFF;
                                                                }"
                )
              )
      ),
      
      tags$h3("An application built by",                                                                      # h2 header is the subtitle underneath the h1 header
              tags$a(href = "mailto:matty_courtney@hotmail.com?Subject=Shiny%20Help", "Dr Matthew Courtney (PhD)"),
              tags$style(
                HTML("
                                                            h3{
                                                               font-family: 'Open Sans';
                                                               font-weight: 500;
                                                               line-height: 1.1;
                                                               font-size: 12pt;
                                                               color: #FFFFFF;
                                                               }
                                                           ")
              )
      )  #, keen for some sounds? Maybe user can select while wating for reports as gimmick...
      
      
      #tags$audio(src = "Zimmer.mp3", type = "audio/mp3", autoplay = NA, controls = NA
      #)
    ),
    
    
    tabPanel("Rasch (JML)", fluid = TRUE
    ),
    
    tabPanel(
      "Rasch (MML)", 
      fluid = TRUE,
      theme = shinytheme("cosmo"),                                                             # css means cascading style sheets, describing how html elements are displayed on screen
      tags$style(type="text/css",
                 "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",  # font
                 "label {font-size: 12px;}",
                 ".recalculating {opacity: 1.0;}",
                 " * {font-family: Open Sans;
                                                     font-weight: 500;
                                                     line-height: 1.1}"
      ),
      
      setBackgroundImage(src = "shiny_background5.png"),                                      # set a background image: it can be either from a URL or an image from the www folder
      
      shinyjs::useShinyjs(),                                                                  # activate javascript in the application
      
      tags$h1("Automated Rasch Analysis",
              tags$img(src = "white psych.png", 
                       height = 160, 
                       width = 160, 
                       style = "float:right;margin-top:1px;"
              ),
              tags$style(HTML
                         ("
                                                               h1{
                                                                  font-family: 'Open Sans';
                                                                  font-weight: 500;
                                                                  line-height: 1.1;
                                                                  font-size: 60px;
                                                                  color: #FFFFFF;
                                                                  }
                                                              ")
              )
      ),
      
      tags$h2("Toward valid assessments and developmental rubrics",                             # h2 header is the subtitle underneath the h1 header
              tags$style(
                HTML(
                  "
                                                               h2{
                                                                  font-family: 'Open Sans';
                                                                  font-weight: 500;
                                                                  line-height: 1.1;
                                                                  font-size: 18pt;
                                                                  color: #FFFFFF;
                                                                  }
                        
                                                              ")
              )
      ),
      
      tags$h3("An application built by",                                                                      # h2 header is the subtitle underneath the h1 header
              tags$a(href = "mailto:matty_courtney@hotmail.com?Subject=Shiny%20Help", "Dr Matthew Courtney (PhD)"),
              tags$style(
                HTML("
                                                               h3{
                                                                  font-family: 'Open Sans';
                                                                  font-weight: 500;
                                                                  line-height: 1.1;
                                                                  font-size: 12pt;
                                                                  color: #FFFFFF;
                                                                  }
                                  
                                                              ")
              )
      ),
      
      hr(),                                                                                                  # This is a basic line that creates a seperator
      
      fluidRow(                                                                                              # Start writing interface here; fluidRow is a row on the page; 
        column(10,                                                                                                           # You can also add multiple columns
               wellPanel(
                 p("Welcome to Auto-Psych, a novel online tool that allows school test developers, subject-matter experts, educational institutions, and researchers to automatically perform psychometric analyses of student 
                                                                  assessment data. The analysis provides information about the function and utility of test questions, test reliability, and the extent to which the test is suitable for the students. The tool produces a 
                                                                  detailed narrated technical report and spreadsheets based on the application of classical test theory (CTT) and item-response theory (IRT; here, a unidimensional Rasch model)."),
                 
                 p(""),
                 
                 p("Before using the tool, ensure that your data meet the following requirements:"),
                 
                 p(""),
                 
                 p("(1) The header of the csv file (top row) includes a brief description of each question (item) and should start with a letter, not a number (e.g., Item 1. Matching words and visual stimulus), very short descriptions also fine;"),
                 
                 p(""),
                 
                 p("(2) Under the row of item descriptors (the header), item-responses may include dichotomous (0,1) or polytomous (0,1,2... max 9) data;"),
                 
                 p(""),
                 
                 p("(3) A column specifying student (case) identification cannot be included (simply, outputs specific to students, e.g., ability and student fit estimates, remain in the original order); and,"),
                 
                 p(""),
                 
                 p("(4) Some missing data (blanks) are handled by the tool, though users should consider the meaning of such instances and recode if appropriate."),
                 
                 p(""),
                 
                 p("To start, upload your item-response csv file:"),
                 
                 p(""),
                 
                 fileInput("input_file",
                           "Choose your file (.csv)",
                           multiple = FALSE,
                           accept = c("text/csv",                                                                     # Only csv actually works now but we can expland to the others and sav files
                                      "text/comma-separated-values,text/plain",
                                      ".csv",
                                      ".xlsx",
                                      ".xls"
                           )
                 ) %>%                                                                                      # Pipe into new function
                   shinyInput_label_embed(                                                               # Used to create the tooltip (info button)
                     icon("question-circle") %>%                                    # Pipe into other function; Other icons are also possible, see ?shiny::icon or https://fontawesome.com/icons?d=gallery&q=question 
                       bs_embed_tooltip(title = "If you would like to cite this tool, feel free: Courtney, M. G. R., & Xxxxx, X. (XXXX). Auto-psych: a novel shiny app for the psychometric analysis and scoring of assessment and survey data. Xxxxxxxxx and Xxxxxxxxx."
                       )
                   ),
                 
                 p("After completing the upload ('Upload complete'), specify the settings for your customized report:"),
                 
                 p(""),
                 
                 p(""),
                 
                 p("1. Define your test/rubric construct and name your focal group (students):"),
                 
                 p(""),
                 
                 textInput("construct",
                           "Construct:",
                           placeholder = "Test Topic"
                 ),
                 bsTooltip("construct",
                           "E.g., Numeracy or Literacy",
                           "right",
                           options = list(container = "body")
                 ),
                 
                 textInput("population",
                           "Focal group:",
                           placeholder = "Students"
                 ),
                 bsTooltip("population",
                           "E.g., Central School Grade 10 Students",
                           "right",
                           options = list(container = "body")
                 ),
                 
                 p("2. Specify your settings for classical test theory (CTT) analysis:"),
                 
                 p(""),
                 
                 p(""),
                 
                 sliderInput("disc.threshold",
                             "Flag item-total(rest) correlations lower than:",
                             min=0,
                             max=1,
                             value=0.1,
                             step=0.01) %>%
                   shinyInput_label_embed(
                     icon("question-circle") %>%
                       bs_embed_tooltip(title = "Items in the test that correlate negatively with the total score will be flagged automatically in red in the report. However, you can also flag items in blue that only correlated slightly with the total score in the test by selecting a lower limit here."
                       )
                   ),
                 
                 sliderInput("ci.level",
                             "Specify confidence interval level for item-total(rest) correlations:",
                             min=0.80,
                             max=0.99,
                             value=0.95,
                             step=0.01) %>%
                   shinyInput_label_embed(
                     icon("question-circle") %>%
                       bs_embed_tooltip(title = "Both upper and lower confidence intervals are also reported for item-total(rest) correlations. Lower CIs below zero are flagged red."
                       )
                   ),
                 
                 selectizeInput("NA.Delete",
                                "If there are missing responses, for CTT statistics...",
                                choices = c("Delete cases listwise" = TRUE,                     
                                            "Change missing values to zero" = FALSE)) %>%              
                   shinyInput_label_embed(
                     icon("question-circle") %>%
                       bs_embed_tooltip(title = "Choose your option for handling missing values."
                       )
                   ),
                 
                 p(""),
                 
                 p(""),
                 
                 p("3. Specify your settings for Rasch analysis:"),
                 
                 p(""),
                 
                 selectizeInput("constraint",
                                "Constraint:",
                                choices = c("Item" = "item",
                                            "Cases" = "cases")
                 ) %>%                           
                   shinyInput_label_embed(
                     icon("question-circle") %>%
                       bs_embed_tooltip(title = "Analysis for the report includes a specialized item-response theory analysis of the data: A one-parameter logistic (Rasch-based) model is applied to the data. For this to occur, the average item difficulty level (delta) should be constrained (to zero), or the average student ability level (theta) ability should be constrained (to zero)."
                       )
                   ),
                 
                 selectizeInput("node.sequence",
                                "Assumed discretized population profile:",
                                choices = c("seq(-6,6,len=21)" = "-6,6,21",
                                            "seq(-4,4,len=21)" = "-4,4,21",
                                            "seq(-5,5,len=21)" = "-5,5,21",
                                            "seq(-7,7,len=21)" = "-7,7,21",
                                            "seq(-8,8,len=21)" = "-8,8,21",
                                            "seq(-9,9,len=21)" = "-9,9,21",
                                            "seq(-6,6,len=31)" = "-6,6,31",
                                            "seq(-6,6,len=41)" = "-6,6,41",
                                            "seq(-6,6,len=51)" = "-6,6,51",
                                            "seq(-6,6,len=61)" = "-6,6,61",
                                            "seq(-6,6,len=71)" = "-6,6,71",
                                            "seq(-6,6,len=81)" = "-6,6,81",
                                            "seq(-6,6,len=91)" = "-6,6,91",
                                            "seq(-7,7,len=21)" = "-7,7,21",
                                            "seq(-8,8,len=91)" = "-8,8,21",
                                            "seq(-9,9,len=91)" = "-9,9,21",
                                            "seq(-10,10,len=91)" = "-10,10,21"
                                )
                 ) %>%
                   shinyInput_label_embed(
                     icon("question-circle"
                     ) %>%
                       bs_embed_tooltip(title = "The node sequence specifies the lowest and highest student abilities (theta; default -6 to 6) assumed to exist in the broader population of the focal group. 'Len' specifies the number of breaks, or discretized points for that broader population. Note that the default sequence is 'seq(-6, 6, len=21)' with shorter node sequences enabling faster computation."
                       )
                   ),
                 
                 selectizeInput("conv",
                                "Convergence criterion",
                                choices = c("0.0001",
                                            "0.001",
                                            "0.01",
                                            "0.1")
                 ) %>%
                   shinyInput_label_embed(
                     icon("question-circle") %>%
                       bs_embed_tooltip(title = "This value represents the acceptable level of tolerance for which the IRT model represent the data; larger values enable faster computation, though 0.0001 is default."
                       )
                   ),
                 
                 selectizeInput("maxiter",
                                "Maximum iterations",
                                choices = c(1000,
                                            5000,
                                            20000,
                                            100000)) %>%
                   shinyInput_label_embed(
                     icon("question-circle") %>%
                       bs_embed_tooltip(title = "This value represents the maximum number of computational steps permitted for the model to represent the data; lower values enable faster computation, though 1000 is default."
                       )
                   ),
                 
                 selectizeInput("p.fit.threshold",
                                "Flag cases (persons) with person-fit statistics higher than:",
                                choices = c(3.00,
                                            2.00,
                                            2.10,
                                            2.20,
                                            2.30,
                                            2.40,
                                            2.50,
                                            2.60,
                                            2.70,
                                            2.80,
                                            2.90,
                                            3.10,
                                            3.20,
                                            3.30,
                                            3.40,
                                            3.50,
                                            3.60,
                                            3.70,
                                            3.80,
                                            3.90,
                                            4.00,
                                            4.10,
                                            4.20,
                                            4.30,
                                            4.40,
                                            4.50,
                                            4.60,
                                            4.70,
                                            4.80,
                                            4.90,
                                            5.00
                                )
                 ) %>%
                   shinyInput_label_embed(
                     icon("question-circle") %>%
                       bs_embed_tooltip(title = "The higher the person-fit (outfit) values the more unusual the scoring pattern for the student. Default lower limit here is 3.0 (common) but this can be adjusted as per user preference. Here we can identify students with the most unexpected response pattern."
                       )
                   ),
                 
                 p(""),
                 
                 p(""),
                 
                 p("4. Customise your graphical settings:"),
                 
                 p(""),
                 
                 selectizeInput("color.choice",
                                "Graphical color scheme:",
                                choices = c("Eurasian Steppe",
                                            "Deep Code",
                                            "Commercial Overreach",
                                            "Take a Trip",
                                            "Pohutukawa Beach",
                                            "Southland Coal"
                                )
                 ) %>%            
                   shinyInput_label_embed(
                     icon("question-circle") %>%
                       bs_embed_tooltip(title = "Choose from six color schemes for the graphs in your preport ;-)"
                       )
                   ),
                 
                 selectizeInput("binwidth",
                                "Bin width (width of WrightMap columns)",
                                choices = c(0.25,
                                            0.10,
                                            0.15,
                                            0.20,
                                            0.30,
                                            0.35,
                                            0.40,
                                            0.45,
                                            0.50,
                                            0.55,
                                            0.60,
                                            0.65,
                                            0.70,
                                            0.75,
                                            0.80,
                                            0.85,
                                            0.90,
                                            0.95,
                                            1.0
                                )
                 ) %>%
                   shinyInput_label_embed(
                     icon("question-circle") %>%
                       bs_embed_tooltip(title = "This specification allows for the width of the columns in the WrightMap to be specified; the WrightMap places students and questions on the same chart enabling an examination of progressive item difficulty and person ability."
                       )
                   ),
                 
                 p(""),
                 
                 p(""),
                 
                 p("5. Include your own recommendations:"),
                 
                 p(""),
                 
                 textAreaInput("recommendations",
                               "Notes:",
                               placeholder = "There are no notes for this report",
                               height = '150px'
                 ),
                 bsTooltip("recommendations",                                                                        # bsTooltip is another way of displaying tool tips, works with uptodate R
                           "These notes will be reported at the start of the PDF technical report. Make any notes you like about the original data or report itself.",
                           "right",
                           options = list(container = "body")
                 ),
                 
                 shinyjs::disabled(downloadButton("report",
                                                  "Generate PDF report and spreadsheet"
                 )
                 )
                 
               ) # wellPanel wrapper
        ) # column wrapper
      ) # fluidRow wrapper
    ), # Rasch tabPanel wrapper
    
    tabPanel("Many Facets Rasch", fluid = TRUE
    ),
    
    tabPanel("Multidimensional Rasch", fluid = TRUE
    ),
    
    tabPanel("Rasch Equating", fluid = TRUE
    ),
    
    tabPanel("ANOVA", fluid = TRUE
    ),
    
    tabPanel("Multiple Regression", fluid = TRUE
    ),
    
    tabPanel("Multilevel Modelling", fluid = TRUE
    ),
    
    tabPanel("EFA", fluid = TRUE
    ),
    
    tabPanel("Media", fluid = TRUE
    ),
    
    tabPanel("Contact", fluid = TRUE
    )
    
  ) # tabsetPanel wrapper
))