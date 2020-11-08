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

# Define UI for app that lets you select desired inputs

# tags$style enables nice styling
# fonts at: https://fonts.google.com and you have to find ones which are open source
# "Open Sans" is a nice option
# "simplex" is a nice theme, though "cosmo" chosen here.
# Themes at: https://rstudio.github.io/shinythemes/ 

ui <- fluidPage(
      tabsetPanel(
            tabPanel("Home", fluid = TRUE, theme = shinytheme("cosmo"),
                     tags$style(type = "text/css", 
                               "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                               "label {font-size: 12px;}", ".recalculating {opacity: 1.0;}",
                               " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                              ),
                    setBackgroundImage(src = "shiny_background8.png"),
                    shinyjs::useShinyjs(),
                    tags$h1("Automated Psychometrics",
                            tags$img(src = "white psych.png", height = 130, width = 130, 
                                    style = "float:right;margin-top:1px;"
                                    ),
                            tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                             line-height: 1.1; font-size: 60px; color: #FFFFFF;}"
                                           )
                                      )
                            ),
                    tags$h2("Toward Valid Assessments, Rubrics, and Educational Research",          # h2 header is the subtitle underneath the h1 header
                            tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                            line-height: 1.1; font-size: 18pt; color: #FFFFFF;}"
                                           )
                                      )
                            ),
                    hr(),
                    tags$h3("Chief Architect",                                                      # h2 header is the subtitle underneath the h1 header
                            tags$a(href = "mailto:matty_courtney@hotmail.com?Subject=Shiny%20Help",
                                   "Dr Matthew Courtney (PhD)"
                                   ),
                            tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 11pt;
                                                             color: #FFFFFF;}"
                                           )
                                      )
                            ),
                    tags$h3("Chief Statistician:",                                                  # h2 header is the subtitle underneath the h1 header
                            tags$a(href = "mailto:kevin.ct.chang@gmail.com?Subject=Shiny%20Help",
                                   "Dr Kevin Chang (PhD)"
                                   ),
                            tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 11pt;
                                                             color: #FFFFFF;}"
                                            )
                                       )
                            ),
                    tags$h3("Chief Psychometrician:",                                               # h2 header is the subtitle underneath the h1 header
                            tags$a(href = "mailto:chonghuachang@gmail.com?Subject=Shiny%20Help",
                                   "Dr Zhonghua Zhang (PhD)"),
                                   tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                    line-height: 1.1; font-size: 11pt;
                                                                    color: #FFFFFF;}"
                                                   )
                                              )
                                   ),
                    tags$h3("Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                            tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                                   "Dr Eric Mei (PhD)"
                                   ),
                            tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 11pt;
                                                              color: #FFFFFF;}"
                                            )
                                      )
                            ),
                    tags$h3("Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                            tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                                   "Dr Lan Ahn Nguyen Khoa (PhD)"
                                   ),
                            tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 11pt;
                                                             color: #FFFFFF;}"
                                            )
                                      )
                            ),

                    fluidRow(
                        column(11, 
                          wellPanel(
                            p("Welcome to Automated Psychometrics, a novel online tool that allows test 
                              developers, educational institutions, and researchers to automatically 
                              perform a range of psychometric analyses and statistical tests on student 
                              assessment and developmental rubric data."),
                            p(""),
                            p("The website provides automated Rasch-based analysis of student test data, 
                              uni- and multi-dimensional Rasch analysis, many facets analysis (to explore 
                              item bias, for example), and an introduction to fixed and concurrent equating. 
                              The website also provides extended options such as automated ANOVA- and 
                              regression-based analyses."),
                            p(""),
                            p("This website is the brainchild of Chief Architect and Psychometrician, 
                            Dr Mattthew Courtney (PhD). Dr Courtney's vision was to make these forms 
                            of analyses ubiquitously available to educators, test developers, educational 
                            institutions, and researchers worldwide. The intention of Dr Courtney and 
                            his team is to make the highest standard of educational assessment and research 
                            accessible to both the developed and developing world."),
                            p(""),
                            p("Use of the website is free and users may make use of the following citation:"),
                            p(""),
                            p("Courtney, M. G. R., Xxxxx, X., Xxxxx, X., & Xxxxxx, X. (XXXX). Auto-psych: 
                              a novel shiny app for the psychometric analysis and scoring of assessment
                              and survey data. The X Journal, X(X), XXX-XXX. doi. XXXXXXXXXXXXX."),
                            p(""),
                                    ),   # WelPanel
                               ),        # Column
                            ),           # fluidRow
                   ),                    # Home

           tabPanel("Uni-Dim Rasch (JML)", fluid = TRUE, theme = shinytheme("cosmo"),               # css means cascading style sheets, describing how html elements are displayed on screen
                     tags$style(type="text/css",
                                "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                "label {font-size: 12px;}", ".recalculating {opacity: 1.0;}",
                                " * {font-family: Open Sans; font-weight: 500;  line-height: 1.1}"),
                     shinyjs::useShinyjs(),                                                         # activate javascript in the application
                     tags$h1("Automated Rasch Analysis",
                             tags$img(src = "white psych.png", height = 130, width = 130, 
                                      style = "float:right;margin-top:1px;"
                                      ),
                             tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1;
                                                              font-size: 60px; color: #FFFFFF;}"
                                            )
                                       )
                            ),
                      tags$h2("Toward Valid Assessments and Developmental Rubrics",                 # h2 header is the subtitle underneath the h1 header
                              tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                                 line-height: 1.1; font-size: 18pt;
                                                                 color: #FFFFFF;}"
                                              )
                                        )
                              ),          
                      hr(),                                                                         # This is a basic line that creates a seperator
                     ),                  # Uni-Dim Rasch (JML) wrapper

            tabPanel("Uni-Dim Rasch (MML)", fluid = TRUE, theme = shinytheme("cosmo"),              # css means cascading style sheets, describing how html elements are displayed on screen
                    tags$style(type="text/css",
                               "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                               "label {font-size: 12px;}", ".recalculating {opacity: 1.0;}",
                               " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                              ),
                    shinyjs::useShinyjs(),                                                          # activate javascript in the application
                    tags$h1("Automated Rasch Analysis",
                            tags$img(src = "white psych.png", height = 130, width = 130, 
                                     style = "float:right;margin-top:1px;"
                                     ),
                            tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 60px;
                                                              color: #FFFFFF;}"
                                            )
                                       )
                            ),
                    tags$h2("Toward Valid Assessments and Developmental Rubrics",                   # h2 header is the subtitle underneath the h1 header
                            tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 18pt;
                                                             color: #FFFFFF;}"
                                            )
                                      )
                            ),
                    hr(),                                                                           # This is a basic line that creates a seperator
                    fluidRow(
                        column(10,
                         wellPanel(
                           p("This tool produces a detailed narrated technical report and spreadsheets 
                             based on the application of classical test theory (CTT) and item-response 
                             theory (IRT; here, a unidimensional Rasch model)."),
                           p(""),
                           p("Before using the tool, ensure that your data meet the following requirements:"),
                           p(""),
                           p("(1) The header of the csv file (top row) includes a brief description of 
                             each question (item) and should start with a letter, not a number (e.g., 
                             Item 1. Matching words and visual stimulus), very short descriptions also 
                             fine;"),
                           p(""),
                           p("(2) Under the row of item descriptors (the header), item-responses may 
                             include dichotomous (0,1) or polytomous (0,1,2... max 9) data;"),
                           p(""),
                           p("(3) A column specifying student (case) identification cannot be included 
                             (simply, outputs specific to students, e.g., ability and student fit estimates, 
                             remain in the original order); and,"),
                           p(""),
                           p("(4) Some missing data (blanks) are handled by the tool, though users should 
                             consider the meaning of such instances and recode if appropriate."),
                           p(""),
                           p("To start, upload your item-response csv file:"),
                           p(""),
                           fileInput("input_file", "Choose your file (.csv)",                       # The file is observed by the UI when it is uploaded successfully.
                                      multiple = FALSE, accept = c("text/csv", "text/comma-separated-values, 
                                                                   text/plain", ".csv", ".xlsx", ".xls"
                                                                  )
                                    ) %>%                                                           # Pipe into new function
                           shinyInput_label_embed(icon("question-circle") %>%                       # Pipe into other function; Other icons are also possible, see ?shiny::icon or https://fontawesome.com/icons?d=gallery&q=question 
                           bs_embed_tooltip(title = "If you would like to cite this tool, feel free: 
                                            Courtney, M. G. R., & Xxxxx, X. (XXXX). Auto-psych: a novel 
                                            shiny app for the psychometric analysis and scoring of assessment 
                                            and survey data. Xxxxxxxxx and Xxxxxxxxx."
                                            )
                                                 ),                                                 # End of shinyInput_labelembed function (piping occurs withing that function)

                           p("After completing the upload ('Upload complete'), specify the settings 
                             for your customized report:"),
                           p(""),
                           p(""),

                           p("1. Define your test/rubric construct and name your focal group 
                             (students):"),                                                         # 1. input: construct
                           p(""),
                           textInput("construct", "Construct:", placeholder = "Test Topic"),
                           bsTooltip("construct", "E.g., Numeracy or Literacy", "right",
                                     options = list(container = "body")
                                    ),

                           textInput("population", "Focal group:", placeholder = "Students"),       # 2. input: sample of interest (though modelling has population-bsed assumptions)
                           bsTooltip("population", "E.g., Central School Grade 10 Students", "right",
                                     options = list(container = "body")
                                    ),

                           p("2. Specify your settings for classical test theory (CTT) analysis:"), 
                           p(""),
                           p(""),

                           sliderInput("disc.threshold", "Flag item-total(rest) correlations lower than:",
                                       min = 0, max = 1, value = 0.1, step = 0.01) %>%
                           shinyInput_label_embed(icon("question-circle") %>%
                           bs_embed_tooltip(title = "Items in the test that correlate negatively with 
                                            the total score will be flagged automatically in red in 
                                            the report. However, you can also flag items in blue that 
                                            only correlated slightly with the total score in the test 
                                            by selecting a lower limit here."
                                            )
                                                  ),                                                # End of shinyInput_labelembed function (piping occurs withing that function)
                                 
                           sliderInput("ci.level", "Specify confidence interval level for item-total(rest) 
                                       correlations:", min = 0.80, max = 0.99, value = 0.95, step = 0.01) %>%
                           shinyInput_label_embed(icon("question-circle") %>%
                           bs_embed_tooltip(title = "Both upper and lower confidence intervals are also 
                                            reported for item-total(rest) correlations. Lower CIs below 
                                            zero are flagged red."
                                            )
                                                 ),                                                 # End of shinyInput_labelembed function (piping occurs withing that function)

                           selectizeInput("NA.Delete", "If there are missing responses, for CTT statistics...",
                                          choices = c("Delete cases listwise" = TRUE, 
                                                      "Change missing values to zero" = FALSE)
                                          ) %>%              
                           shinyInput_label_embed(icon("question-circle") %>%
                           bs_embed_tooltip(title = "Choose your option for handling missing values."
                                           )
                                                 ),                                                 # End of shinyInput_labelembed function (piping occurs withing that function)
                           p(""),
                           p(""),

                           p("3. Specify your settings for Rasch analysis:"),
                           p(""),

                           selectizeInput("constraint", "Constraint:", 
                                          choices = c("Item" = "item", "Cases" = "cases")
                                         ) %>%                           
                           shinyInput_label_embed(icon("question-circle") %>%
                           bs_embed_tooltip(title = "Analysis for the report includes a specialized 
                                            item-response theory analysis of the data: A one-parameter 
                                            logistic (Rasch-based) model is applied to the data. For 
                                            this to occur, the average item difficulty level (delta) 
                                            should be constrained (to zero), or the average student 
                                            ability level (theta) ability should be constrained (to zero)."
                                            )
                                                 ),                                                 # End of shinyInput_labelembed function (piping occurs withing that function)
                                 
                           selectizeInput("node.sequence", "Assumed discretized population profile:",
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

                           selectizeInput("conv", "Convergence criterion", choices = c("0.0001",
                                          "0.001", "0.01", "0.1")
                                         ) %>%
                           shinyInput_label_embed(icon("question-circle") %>%
                           bs_embed_tooltip(title = "This value represents the acceptable level of tolerance 
                                            for which the IRT model represent the data; larger values 
                                            enable faster computation, though 0.0001 is default."
                                            )
                                                  ),

                           selectizeInput("maxiter", "Maximum iterations", choices = c(1000, 5000,
                                          20000, 100000)
                                          ) %>%
                           shinyInput_label_embed(icon("question-circle") %>%
                           bs_embed_tooltip(title = "This value represents the maximum number of computational 
                                            steps permitted for the model to represent the data; lower 
                                            values enable faster computation, though 1000 is default."
                                            )
                                                 ),

                           selectizeInput("p.fit.threshold", "Flag cases (persons) with person-fit statistics 
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
                           p(""),

                           p("4. Customise your graphical settings:"),
                           p(""),

                           selectizeInput("color.choice", "Graphical color scheme:",
                                          choices = c("Eurasian Steppe", "Deep Code", "Commercial Overreach",
                                                      "Take a Trip", "Pohutukawa Beach", "Southland Coal")
                                         ) %>%            
                           shinyInput_label_embed(icon("question-circle") %>%
                           bs_embed_tooltip(title = "Choose from six color schemes for the graphs in 
                                            your preport ;-)"
                                           )
                                                 ),

                           selectizeInput("binwidth", "Bin width (width of WrightMap columns)",
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
                           p(""),

                           p("5. Include your own recommendations:"),
                           p(""),

                           textAreaInput("recommendations", "Notes:", 
                                         placeholder = "There are no notes for this report",
                                         height = '150px'),
                           bsTooltip("recommendations",
                                     "These notes will be reported at the start of the PDF technical 
                                     report. Make any notes you like about the original data or report 
                                     itself.",
                                     "right",
                                     options = list(container = "body")
                                    ),

                           shinyjs::disabled(downloadButton("report", "Generate PDF report and 
                                                            spreadsheet")                           # 'report' is the official name of the download button (used in UI, disabled at the start and activated...             
                                            )                                                       # when conditions met in server logic), renamed as "Generate PDF report and spreadsheet'
                                  )   # wellPanel
                              )       # column
                            )         # fluidRow
                    ),                # Rasch tabPanel

            tabPanel("Multi-Dim Rasch", fluid = TRUE),                                              # Baseline build (MC to expand unidimensional build to account for item classiciation into dimensions.

            tabPanel("Many Facets Rasch (DIF)", fluid = TRUE),                                      # Baseline build (MC, ZZ) includes csv file that takes item-response matrix with 1st column pertaining facet of interest.
                                                                                                    # function to use TAM::tam.mml.mfr with two formula options: (a) TAM, EXAMPLE 8, formulaA =~ item*facet; where 'facet' is the column name of first column of df. Expand to =~ item+item:step, and others later.
           
            tabPanel("Rasch Equating", fluid = TRUE),                                               # Baseline build for (a) concurrent calibration and (b) fixed anchor collibration proposed (MC and ZZ)

            tabPanel("ANOVA", fluid = TRUE),                                                        # Baseline build of one-way ANOVA proposed as a means of examining within- and between-group effects (KC)

            tabPanel("Multiple Regression", fluid = TRUE),

            tabPanel("MLM", fluid = TRUE),

            tabPanel("IRR", fluid = TRUE),                                                          # Baseline build ICC(1,k) or ICC(1,1), or whatever EBM thinks is most common as a means to examine inter-rater reliability in educational settings.
                                                                                                    # Maybe consider this:https://www.uvm.edu/~statdhtx/methods8/Supplements/icc/More%20on%20ICCs.pdf  and   https://www.youtube.com/watch?v=1Avl7DzKmnc
            tabPanel("CVs/Blog/Contact", fluid = TRUE)                                              # These tabs are conceptual and subject to change...
                  )      # tabsetPanel
               )         # fluidPage




# Define server logic required ----
server <- function(input, output, session){
                                                                                                    # the main  input, input$input_file (csv), is recognised when the following conditions are met.
observeEvent(input$input_file,
                {if (length(input$input_file) > 0 &&                                                # The length of the input file is above zero.
                     length(input$recommendations) > 0 &&                                           # The recommendations are always entered (this is always true due to placeholder).
                     length(input$construct) > 0 &&                                                 # same as above "Test Topic".
                     length(input$population) > 0                                                   # same as above "Students".
                    ){                                    
                      shinyjs::enable("report")                                                     # when the conditions above are met, shinyjs::enable is used to enable the report button to be clicked.
                     }else {                                                                        # otherwise, when conditions not met,
                     shinyjs::disable("report")  
                       }                                                                            # button is disabled.
                }                                             
              )
  
output$report <- downloadHandler(                                                                   # This function makes the download     
  filename = "psychometric_analysis.zip",                                                           # The zip file created
  content = function(file){
      withProgress(message = 'R Shiny Boosted Rendering',                                           # Set a progress bar because it can take some time
                    { tempdir <- tempdir()                                                          # Copy the report file to a temporary directory before processing it, in case we don't have write permissions to the current working dir (which can happen when deployed).
                      
                      tempReport <- file.path(tempdir, "Testbuild.RUNNING2.Rmd")                    # Create the filepath where the tempory rmd file resides
                      
                      file.copy("scripts/Testbuild.RUNNING2.Rmd", tempReport, overwrite = TRUE)     # Copy the rmd file from the scripts folder to the path above
                      
                      node.sequence <- as.numeric(strsplit(input$node.sequence,",")[[1]])           # The tempdir constantly changes at shinyapps.io, that is why we have to repeat this process every time.
                                                                                                    # Now we can get our inputs and use them in the .Rmd
                      params <- list(datapath = input$input_file$datapath,                          # Set up parameters to pass to Rmd document
                                                recommendations = input$recommendations,
                                                construct = input$construct,
                                                population = input$population,
                                                constraint = input$constraint,
                                                NA.Delete = input$NA.Delete,
                                                disc.threshold = as.numeric(input$disc.threshold),
                                                ci.level = as.numeric(input$ci.level),                       
                                                p.fit.threshold = as.numeric(input$p.fit.threshold),
                                                node.sequence.1 = node.sequence[1],                 # For node.sequence we have to do some cleaning on the input first:
                                                node.sequence.2 = node.sequence[2],                 # This is the part where we use the strsplit. We seperate the character on ',' and make it numeric
                                                node.sequence.3 = node.sequence[3],
                                                conv = as.numeric(input$conv),
                                                maxiter = as.numeric(input$maxiter),
                                                color.choice = input$color.choice,
                                                binwidth = as.numeric(input$binwidth),
                                                rendered_by_shiny = TRUE                            # we need rendered_by_shiny to update the progress bar
                                     )    
                                                                                                    
                      file1 <- file.path(tempdir, "report.pdf")   
                                                                                                    # Knit the document, passing in the `params` list, and eval it in a child of the global environment (this isolates the code in the document from the code in this app).
                      rmarkdown::render(tempReport,
                                        output_file = file1,
                                        params = params,
                                        envir = new.env(parent = globalenv() 
                                                        )
                                       )                                                            # file1 is the path of the PDF output
                                        
                      file2 <- file.path(tempdir, "report.xlsx")                                    # file2 is the path of the xlsx output coming from the markdown
                                        
                      files <- c(file1, file2)                                                      # combine all the files to zip them

                      zip(file, files, extras = "-j")                                               # creating a zip. You need extras = "-j" to get a clean zip, not one with the whole paths
                    }
                  )                          # withProgress
                          }                  # function file wrapper
                              )              # downloadHandler wrapper
                                          }  # server function wrapper

shinyApp(ui = ui, server = server)




