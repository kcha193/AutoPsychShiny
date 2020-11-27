
version_number <- "0.1.0"

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

source("scripts/anova_module.R")
source("scripts/MML_UI.R")
source("scripts/download_Server.R")

# UI file starts here -------------------------------------------------- 

ui <- fluidPage(
      tabsetPanel(

# Title -------------------------------------------------------------------
            tabPanel("Home", fluid = TRUE, theme = shinytheme("cosmo"),
                     tags$style(type = "text/css", 
                               "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                               "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                               " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                              ),
                    setBackgroundImage(src = "shiny_background8.png"),
                    shinyjs::useShinyjs(),
                    tags$h1("Automated Psychometrics",
                            tags$img(src = "hex5.png", height = 149, width = 135, 
                                    style = "float:right;margin-top:-18.5px;"
                                    ),
                            tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                             line-height: 1.1; font-size: 60px; color: #FFFFFF;}"
                                           )
                                      )
                            ),
                    tags$h2("Toward Valid Assessments and Educational Research",                    # h2 header is the subtitle underneath the h1 header
                            tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                            line-height: 1.1; font-size: 18pt; color: #FFFFFF;}"
                                           )
                                      )
                            ),
                    hr(),
                    tags$h3("Chief Architect:",                                                     # h2 header is the subtitle underneath the h1 header
                            tags$a(href = "mailto:matty_courtney@hotmail.com?Subject=Shiny%20Help",
                                   "Dr Matthew Courtney (PhD)"
                                   ),
                            tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 10pt;
                                                             color: #FFFFFF;}"
                                           )
                                      )
                            ),
                    tags$h3("Chief Programmer:",                                                    # h2 header is the subtitle underneath the h1 header
                            tags$a(href = "mailto:kevin.ct.chang@gmail.com?Subject=Shiny%20Help",
                                   "Dr Kevin Chang (PhD)"
                                   ),
                            tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 10pt;
                                                             color: #FFFFFF;}"
                                            )
                                       )
                            ),
                    tags$h3("Chief Psychometrician:",                                               # h2 header is the subtitle underneath the h1 header
                            tags$a(href = "mailto:chonghuachang@gmail.com?Subject=Shiny%20Help",
                                   "Dr Zhonghua Zhang (PhD)"),
                                   tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                    line-height: 1.1; font-size: 10pt;
                                                                    color: #FFFFFF;}"
                                                   )
                                              )
                                   ),
                    tags$h3("Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                            tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                                   "Dr Eric Mei (PhD)"
                                   ),
                            tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 10pt;
                                                              color: #FFFFFF;}"
                                            )
                                      )
                            ),
                    tags$h3("Contributing Psychometrician:",                                        # h2 header is the subtitle underneath the h1 header
                            tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                                   "Dr Luke Rowe (PhD)"
                                   ),
                            tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 10pt;
                                                             color: #FFFFFF;}"
                                            )
                                      )
                            ),
                    fluidRow(
                        column(11, 
                          wellPanel(
                            h4("Introduction"),
                            p("Welcome to Automated Psychometrics, a novel website that allows test 
                              developers, educational institutions, and researchers to:"),
                            p(""),
                            p("(1) Improve the quality of student assessments and developmental rubrics,"),
                            p(""),
                            p("(2) Gain valuable information about student development and learning, and,"),
                            p(""),
                            p("(3) Carry out high quality research and analytics.")
                                   ),
                          wellPanel(
                            h4("Design"),
                            p("The website provides automated Rasch-based analysis of student test data, 
                              inclusive of (1) uni-dimensional Rasch analysis, (2) many facets analysis 
                              (to explore item bias), and (3) Rasch equating for developing common ability 
                              scales across different test forms. The website also provides extended 
                              options such as (1) inter-rater reliability analysis, and (2) automated 
                              ANOVA-based procedures to explore differences in performance outcomes 
                              between groups."),
                            p("")
                                    ),
                          wellPanel(
                            h4("Team Vision"),
                            p("Together, the team draws on extensive expertise in educational 
                              and psychological assessment, statistics, quantitative research methods, 
                              statistical programming, web-design, statistical software development, 
                              teaching pedagogy, and online learning. Drawing up this multi-disciplinary 
                              skillset, the team's visions is to both promote and make high quality 
                              assessment and research  ubiquitously accessible to the developed and 
                              developing world.")
                                    ),   # WellPanel
                          wellPanel(
                            h4("Use"),
                            p("The website and all functionality was built using the open-source R programming 
                              language and received no external funding. Use of the website is free and users 
                              may make use of the following citation:"),
                            p(""),
                            p("Courtney, M. G. R., Xxxxx, X., Xxxxx, X., & Xxxxxx, X. (XXXX). Auto-psych: 
                              a novel shiny app for the psychometric analysis and scoring of assessment
                              and survey data. The X Journal, X(X), XXX-XXX. doi. XXXXXXXXXXXXX."),
                            p("")
                                   ),   # WelPanel
                               ),        # Column
                            ),           # fluidRow
                   ),                    # Home

# Uni-Dim Rasch (MML) -----------------------------------------------------


            tabPanel("Uni-Dim Rasch (MML)", 
                    fluid = TRUE, theme = shinytheme("cosmo"),                                      # css means cascading style sheets, describing how html elements are displayed on screen
                    tags$style(type="text/css",
                               "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                               "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                               " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                              ),
                    shinyjs::useShinyjs(),                                                          # activate javascript in the application
                    tags$h1("Uni-Dimensional Rasch Analysis",
                            tags$img(src = "hex5.png", height = 149, width = 135, 
                                     style = "float:right;margin-top:-18.5px;"
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
                    hr(), 
                    tags$h3("Architect:",                                                            # h2 header is the subtitle underneath the h1 header
                            tags$a(href = "mailto:matty_courtney@hotmail.com?Subject=Shiny%20Help",
                                   "Dr Matthew Courtney (PhD)"
                                  ),
                            tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 10pt;
                                                             color: #FFFFFF;}"
                                           )
                                      )
                            ),
                    tags$h3("Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                            tags$a(href = "mailto:chonghuachang@gmail.com?Subject=Shiny%20Help",
                                   "Dr Zhonghua Zhang (PhD)"),
                            tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                    line-height: 1.1; font-size: 10pt;
                                                                    color: #FFFFFF;}"
                                           )
                                      )
                           ),
                    fluidRow(
                        column(11,
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
                             might be ready to tackle with additional support. Analysts using this tool 
                             (as opposed to the JML tool) will be primarily interested in generalizing 
                             the results of the analysis to the broader population from which the sample 
                             students were drawn."),
                           p("")
                         ),
                         
                         MML_UI("MML")
                         
                        )
                    )         # fluidRow
            ),                # Rasch tabPanel



# Many-Facets Rasch (DIF) -------------------------------------------------
                                                                                                    # Baseline build (MC to expand unidimensional build to account for item classiciation into dimensions.
            tabPanel("Many-Facets Rasch (DIF)",
                     fluid = TRUE, theme = shinytheme("cosmo"),                                      # css means cascading style sheets, describing how html elements are displayed on screen
                     tags$style(type="text/css",
                                "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                               ),
                     shinyjs::useShinyjs(),                                                          # activate javascript in the application
                     tags$h1("Many-Facets Rasch Analysis",
                             tags$img(src = "hex5.png", height = 149, width = 135, 
                                      style = "float:right;margin-top:-18.5px;"
                                      ),
                             tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 60px;
                                                              color: #FFFFFF;}"
                                            )
                                       )
                             ),
                     tags$h2("Toward Unbiased Assessments and Developmental Rubrics",                   # h2 header is the subtitle underneath the h1 header
                             tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 18pt;
                                                             color: #FFFFFF;}"
                                            )
                                        )
                            ),
                     hr(), 
                     tags$h3("Architect:",                                                            # h2 header is the subtitle underneath the h1 header
                             tags$a(href = "mailto:matty_courtney@hotmail.com?Subject=Shiny%20Help",
                                    "Dr Matthew Courtney (PhD)"
                                   ),
                             tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 10pt;
                                                             color: #FFFFFF;}"
                                            )
                                       )
                            ),
                     tags$h3("Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                             tags$a(href = "mailto:chonghuachang@gmail.com?Subject=Shiny%20Help",
                                    "Dr Zhonghua Zhang (PhD)"),
                             tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                    line-height: 1.1; font-size: 10pt;
                                                                    color: #FFFFFF;}"
                                            )
                                       )
                            ),
                     fluidRow(
                        column(11,
                              wellPanel(
                                h4("Many-facets Rasch analysis tool"),
                                p("This tool extends the functionality of the Uni-Dimensional Rasch 
                                   analysis to include an examination of item (question) bias via the 
                                   application of many-facets Rasch analysis. This form of analysis 
                                   provides insight into how some questions (or developmental criteria) 
                                   might function differently across student groups."),
                                p(""),
                                p("The tool also takes an item-response matrix (i.e., a spreadsheet 
                                   of student test results). Though, the tool requires that the first 
                                   column specifies the binary facet of interest (e.g., column header 
                                   'gender'). The variable needs to be numeric with coding 1 (representing 
                                   male, for example) and 2 (representing female, for example). The 
                                   report includes and produces a detailed narrated technical report 
                                   and organized spreadsheets that reflect the function of the test and 
                                   each question, as well as a report  on item bias."),
                                p(""),
                                p("The report is based on the application of classical test theory (CTT) 
                                   and item-response theory (IRT; here, a unidimensional Rasch, or 1PL, 
                                   model, and extended many-facets analysis). The analysis uses a specialized 
                                   scoring algorithm that places estimates of student ability and item 
                                   difficulty on the same scale. This enables educators to identify sets 
                                   of questions and associated skills that students might be ready to 
                                   tackle with additional support. Analysts using this tool (as opposed 
                                   to the JML tool, in production) will be primarily interested in generalizing 
                                   the results of the analysis to the broader population from which 
                                   the sample students were drawn. Insights into potential item bias 
                                   can be helpful for checking that the scale operates in a reasonably 
                                   similar way across groups of interest."),
                                p("")
                                       ),
                              wellPanel(
                                h4("1. Prepare data"),
                                p("Before using the tool, ensure that your data meet the following requirements:"),
                                p(""),
                                p("(a) The header of the csv file (top row) includes a brief description of 
                                  each question (item) and should start with a letter, not a number (e.g., 
                                  Item 1. Matching words and visual stimulus), very short descriptions also 
                                  fine; first column needs to be facet with each cell, 1 or 2."),
                                p(""),
                                p("(b) Under the row of 'item descriptors', item-responses may 
                                  include dichotomous (0,1) or polytomous (0,1,2... max 9) data;"),
                                p(""),
                                p("(c) A column specifying student (case) identification cannot be included 
                                  (simply, outputs specific to student ordering, e.g., ability and 
                                  student fit estimates, remain in the original order); and,"),
                                p(""),
                                p("(d) Some missing item-response data (blanks) are handled by the tool, 
                                  though users should carefully consider the meaning of such instances 
                                  and recode if appropriate.")
                                        ),
                              wellPanel(
                                h4("2. Upload your item-response file (csv)"),
                                p(""),
                                fileInput("input_file", "Choose your file (.csv)",                  # The file is observed by the UI when it is uploaded successfully.
                                          multiple = FALSE, accept = c("text/csv", "text/comma-separated-values, 
                                                                   text/plain", ".csv", ".xlsx", ".xls")
                                   ) %>%                                                            # Pipe into new function
                                   shinyInput_label_embed(icon("question-circle") %>%               # Pipe into other function; Other icons are also possible, see ?shiny::icon or https://fontawesome.com/icons?d=gallery&q=question 
                                                           bs_embed_tooltip(title = "If you would like to cite this tool, feel free: Courtney, M. G. R., & Xxxxx, X. (XXXX). Auto-psych: a novel shiny app for the psychometric analysis and scoring of assessment and survey data. Xxxxxxxxx and Xxxxxxxxx.")
                                                          )                                         # End of shinyInput_labelembed function (piping occurs withing that function)
                                       ),
                              wellPanel(
                                h4("3. Specify construct and focal group"),
                                p(""),
                                textInput("construct", "Construct:", placeholder = "Test Topic"),
                                bsTooltip("construct", "E.g., Numeracy or Literacy", "right",
                                          options = list(container = "body")
                                         ),
                                textInput("population", "Focal group:", placeholder = "Students"),  # 2. input: sample of interest (though modelling has population-bsed assumptions)
                                bsTooltip("population", "E.g., Central School Grade 10 Students", "right",
                                          options = list(container = "body")
                                         )
                                       ),
                              wellPanel(
                                h4("4. Specify settings for CTT analysis"),
                                p(""),
                                p(""),
                                sliderInput("disc.threshold", "Flag item-total(rest) correlations lower than:",
                                            min = 0, max = 1, value = 0.1, step = 0.01) %>%
                                  shinyInput_label_embed(icon("question-circle") %>%
                                            bs_embed_tooltip(title = "Items in the test that correlate negatively with the total score will be flagged automatically in red in the report. However, you can also flag items in blue that only correlated slightly with the total score in the test by selecting a lower limit here."
                                                            )
                                                        ),                                          # End of shinyInput_labelembed function (piping occurs withing that function)
                                sliderInput("ci.level", "Specify confidence interval level for item-total(rest) 
                                       correlations:", min = 0.80, max = 0.99, value = 0.95, step = 0.01) %>%
                                  shinyInput_label_embed(icon("question-circle") %>%
                                            bs_embed_tooltip(title = "Both upper and lower confidence intervals are also reported for item-total(rest) correlations. Lower CIs below zero are flagged red."
                                                            )
                                                         ),                                         # End of shinyInput_labelembed function (piping occurs withing that function)
                                selectizeInput("NA.Delete", "If there are missing responses, for CTT statistics...",
                                               choices = c("Delete cases listwise" = TRUE, 
                                                           "Change missing values to zero" = FALSE)
                                               ) %>%              
                                    shinyInput_label_embed(icon("question-circle") %>%
                                                           bs_embed_tooltip(title = "Choose your option for handling missing values."
                                                           )
                                               ),                                                   # End of shinyInput_labelembed function (piping occurs withing that function)
                                p(""),
                                p("")
                                         ),
                              wellPanel(
                                h4("5. Specify settings for Rasch analysis"),
                                p(""),
                                
                                selectizeInput("constraint", "Constraint:", 
                                               choices = c("Item" = "item", "Cases" = "cases")
                                               ) %>%                           
                                    shinyInput_label_embed(icon("question-circle") %>%
                                            bs_embed_tooltip(title = "Analysis for the report includes a specialized item-response theory analysis of the data: A one-parameter logistic (Rasch-based) model is applied to the data. For this to occur, the average item difficulty level (delta) should be constrained (to zero), or the average student ability level (theta) ability should be constrained (to zero)."
                                                           )
                                                         ),                                         # End of shinyInput_labelembed function (piping occurs withing that function)
                                
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
                                                    bs_embed_tooltip(title = "The node sequence specifies the lowest and highest student abilities (theta; default -6 to 6) assumed to exist in the broader population of the focal group. 'Len' specifies the number of breaks, or discretized points for that broader population. Note that the default sequence is 'seq(-6, 6, len=21)' with shorter node sequences enabling faster computation."
                                                                    )
                                                         ),
                                
                                selectizeInput("conv", "Convergence criterion", choices = c("0.0001",
                                                                                            "0.001", "0.01", "0.1")
                                              ) %>%
                                      shinyInput_label_embed(icon("question-circle") %>%
                                            bs_embed_tooltip(title = "This value represents the acceptable level of tolerance for which the IRT model represent the data; larger values enable faster computation, though 0.0001 is default."
                                                            )
                                                            ),
                                
                                selectizeInput("maxiter", "Maximum iterations", choices = c(1000, 5000,
                                                                                            20000, 100000)
                                ) %>%
                                  shinyInput_label_embed(icon("question-circle") %>%
                                            bs_embed_tooltip(title = "This value represents the maximum number of computational steps permitted for the model to represent the data; lower values enable faster computation, though 1000 is default."
                                                            )
                                                        ),
                                
                                selectizeInput("p.fit.threshold", "Flag cases (persons) with person-fit statistics 
                                          higher than:", choices = c(3.00, seq(2, 2.90, .10), 
                                                                     seq(3.10, 5, .10))             # Sequences with 3.00 missing enable default to be set at 3.00.
                                               ) %>%
                                  shinyInput_label_embed(icon("question-circle") %>%
                                            bs_embed_tooltip(title = "The higher the person-fit (outfit) values the more unusual the scoring pattern for the student. Default lower limit here is 3.0 (common) but this can be adjusted as per user preference. Here we can identify students with the most unexpected response pattern."
                                                            )
                                                        ),
                                p(""),
                                p("")
                                       ),
                              wellPanel(
                                h4("6. Specify settings for many-facets Rasch analysis"),
                                p(""),
                                p(""),
                                sliderInput("facets.cut.logit", "Flag overall group differences in item difficulty higher than:",
                                            min = 0.1, max = 1.0, value = 0.5, step = 0.01) %>%
                                  shinyInput_label_embed(icon("question-circle") %>%
                                                           bs_embed_tooltip(title = "Items that are bias against one particular student group can be automatically flagged with this tool. A common cut-off value is 0.5 logit, (Wu, Tam, & Jen, 2016, p. 216), though users can specify their own level of practical significance here."
                                                                            )
                                                           ),                                       # End of shinyInput_labelembed function (piping occurs withing that function)
                                sliderInput("facets.cut.p", "Flag overall group differences in item difficulty with levels of 
                                             statistical significance lower than:", min = 0.001, max = 0.10, value = 0.05, step = 0.005) %>%
                                  shinyInput_label_embed(icon("question-circle") %>%
                                                           bs_embed_tooltip(title = "Items that are bias against one particular student group can be automatically flagged with this tool. A common cut-off value  is p = .05. Though, users can specify their own level of statistical significance here."
                                                                            )
                                                         ),                                           
                                p(""),
                                p("")
                                       ),
                              wellPanel(
                                h4("7. Specify graphical settings"),
                                p(""),
                                
                                selectizeInput("color.choice", "Graphical color scheme:",
                                               choices = c("Eurasian Steppe", "Deep Code", "Commercial Overreach",
                                                           "Take a Trip", "Pohutukawa Beach", "Southland Coal")
                                              ) %>%            
                                  shinyInput_label_embed(icon("question-circle") %>%
                                          bs_embed_tooltip(title = "Choose from six color schemes for the graphs in your preport ;-)")
                                                        ),
                                
                                selectizeInput("binwidth", "Bin width (width of WrightMap columns)",
                                               choices = c(0.25, seq(.10, .20, .05), seq(.30, 1.0, .05))   
                                              ) %>%
                                  shinyInput_label_embed(icon("question-circle") %>%
                                        bs_embed_tooltip(title = "This specification allows for the width of the columns in the WrightMap to be specified; the WrightMap places students and questions on the same chart enabling an examination of progressive item difficulty and person ability.")
                                                        ),
                                p(""),
                                p("")
                                       ),
                              wellPanel(
                                h4("8. Include your own recommendations"),
                                p(""),
                                
                                textAreaInput("recommendations", "Notes:", 
                                              placeholder = "There are no notes for this report",
                                              height = '150px'),
                                bsTooltip("recommendations",
                                          "These notes will be reported at the start of the PDF technical 
                                          report. Make any notes you like about the original data or 
                                          report itself.", "right",
                                          options = list(container = "body")
                                          ),
                                
                                shinyjs::disabled(downloadButton("report", "Generate PDF report and 
                                                            spreadsheet")                           # 'report' is the official name of the download button (used in UI, disabled at the start and activated...             
                                                  )                                                 # when conditions met in server logic), renamed as "Generate PDF report and spreadsheet'
                                        )   # wellPanel
                               )            # column
                             )              # fluidRow
                    ),                                                                              # Baseline DIF build (MC, ZZ).
                                                                                                    # function also uses TAM::tam.mml.mfr with two formula options: (a) TAM, EXAMPLE 8, formulaA =~ item*facet; where 'facet' is the column name of first column of df. Expand to =~ item+item:step, and others later.

# Rasch Equating ----------------------------------------------------------
           
            tabPanel("Rasch Equating",
                     fluid = TRUE, theme = shinytheme("cosmo"),                                     # css means cascading style sheets, describing how html elements are displayed on screen
                     tags$style(type="text/css",
                                "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                " * {font-family: Open Sans; font-weight: 500;  line-height: 1.1}"),
                     shinyjs::useShinyjs(),                                                         # activate javascript in the application
                     tags$h1("Rasch Equating",
                             tags$img(src = "hex5.png", height = 149, width = 135, 
                                      style = "float:right;margin-top:-18.5px;"
                             ),
                             tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1;
                                                              font-size: 60px; color: #FFFFFF;}"
                             )
                             )
                     ),
                     tags$h2("Toward Valid Unified Test Forms",                                     # h2 header is the subtitle underneath the h1 header
                             tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                                 line-height: 1.1; font-size: 18pt;
                                                                 color: #FFFFFF;}"
                                             )
                                       )
                            ),          
                     hr(), 
                     tags$h3("Architect:",                                                            # h2 header is the subtitle underneath the h1 header
                             tags$a(href = "mailto:chonghuachang@gmail.com?Subject=Shiny%20Help",
                                    "Dr Zhonghua Zhang (PhD)"
                                   ),
                             tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 10pt;
                                                             color: #FFFFFF;}"
                                             )
                                        )
                            ),
                     tags$h3("Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                             tags$a(href = "mailto:matty_courtney@hotmail.com?Subject=Shiny%20Help",
                                    "Dr Matthew Courtney (PhD)"),
                             tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                    line-height: 1.1; font-size: 10pt;
                                                                    color: #FFFFFF;}"
                                             )
                                       )
                            ),
                     tags$h3("Contributing Psychometrician:",                                       # h2 header is the subtitle underneath the h1 header
                             tags$a(href = "mailto:chonghuachang@gmail.com?Subject=Shiny%20Help",
                                    "Dr Lan Ahn Nguyen Khoa (PhD)"),
                             tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                    line-height: 1.1; font-size: 10pt;
                                                                    color: #FFFFFF;}"
                                            )
                                        )
                             ),
                     ),                                                                             # Baseline build for (a) concurrent calibration and (b) fixed anchor collibration proposed (MC and ZZ)


# ANOVA ----------------------------------------------------------

            tabPanel("ANOVA",
                     fluid = TRUE, theme = shinytheme("cosmo"),
                     tags$style(type = "text/css", 
                                "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                                ),
                     setBackgroundImage(src = "shiny_background8.png"),
                     shinyjs::useShinyjs(),                                                         # activate javascript in the application
                     tags$h1("One-Way ANOVA Analysis",
                             tags$img(src = "hex5.png", height = 149, width = 135, 
                                      style = "float:right;margin-top:-18.5px;"
                                     ),
                             tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1;
                                                              font-size: 60px; color: #FFFFFF;}"
                                            )
                                       )
                            ),
                     tags$h2("Toward Valid Examinations of Group Differences",                      # h2 header is the subtitle underneath the h1 header
                             tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                                 line-height: 1.1; font-size: 18pt;
                                                                 color: #FFFFFF;}"
                                            )
                                       )
                            ),
                     hr(),  
                     tags$h3("Architect:",                                                          # h2 header is the subtitle underneath the h1 header
                             tags$a(href = "mailto:kevin.ct.chang@gmail.com?Subject=Shiny%20Help",
                                    "Dr Kevin Chang (PhD)"
                                   ),
                             tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 10pt;
                                                             color: #FFFFFF;}"
                                            )
                                        )
                            ),
                     tags$h3("Psychometrician:",                                                    # h2 header is the subtitle underneath the h1 header
                             tags$a(href = "mailto:matty_courtney@hotmail.com?Subject=Shiny%20Help",
                                    "Dr Matthew Courtney (PhD)"
                             ),
                             tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 10pt;
                                                             color: #FFFFFF;}"
                                            )
                                        )
                            ),
                     fluidRow(
                        column(11,
                              wellPanel(
                                h4("One-Way ANOVA Tool"),
                                p("This tool provides a convenient way to examine the effect of groups,
                                such as class or school classification, on educational or personal attributes."
                                  ),
                                p(""),
                                p("On this tab, users upload their outputted spreadsheet from their 
                                Rasch analysis. In addition, users also upload another dataset that 
                                includes as many grouping variables (columns) as they like. Note that 
                                the dataset needs to include an identical number of rows as the Rasch 
                                spreadsheet as each grouping variable, e.g., gender, class, needs to 
                                correspond to the same ability estimate"
                                  )
                                       )
                               )
                              ),
                     anova_UI("anova")
                    ),                                                                              # Baseline build of one-way ANOVA proposed as a means of examining within- and between-group effects (KC)


# Inter-Rater Reliability ------------------------------------------------------
            tabPanel("Inter-Rater Reliability",
                     fluid = TRUE, theme = shinytheme("cosmo"),                                     # css means cascading style sheets, describing how html elements are displayed on screen
                     tags$style(type="text/css",
                                "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                                ),
                     shinyjs::useShinyjs(),                                                         # activate javascript in the application
                     tags$h1("Inter-Rater Reliability Analysis",
                             tags$img(src = "hex5.png", height = 149, width = 135, 
                                      style = "float:right;margin-top:-18.5px;"
                                     ),
                             tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 60px;
                                                              color: #FFFFFF;}"
                                             )
                                        )
                            ),
                     tags$h2("Toward Valid Assessments and Developmental Rubrics",                  # h2 header is the subtitle underneath the h1 header
                             tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 18pt;
                                                             color: #FFFFFF;}"
                                            )
                                       )
                            ),
                     hr(), 
                     tags$h3("Architect:",                                                          # h2 header is the subtitle underneath the h1 header
                             tags$a(href = "mailto:matty_courtney@hotmail.com?Subject=Shiny%20Help",
                                    "Dr Eric 'Bing' Mei (PhD)"
                                   ),
                             tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 10pt;
                                                             color: #FFFFFF;}"
                                            )
                                       )
                             ),
                     tags$h3("Psychometrician:",                                                    # h2 header is the subtitle underneath the h1 header
                             tags$a(href = "mailto:chonghuachang@gmail.com?Subject=Shiny%20Help",
                                    "Dr Matthew Courtney (PhD)"),
                             tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                    line-height: 1.1; font-size: 10pt;
                                                                    color: #FFFFFF;}"
                                            )
                                       )
                             ),
                     tags$h3("Statistician:",                                                    # h2 header is the subtitle underneath the h1 header
                             tags$a(href = "mailto:chonghuachang@gmail.com?Subject=Shiny%20Help",
                                    "Dr Matthew Courtney (PhD)"),
                             tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                    line-height: 1.1; font-size: 10pt;
                                                                    color: #FFFFFF;}"
                                             )
                                       )
                            ),
                     fluidRow(
                       column(11,
                              wellPanel(
                                h4("Inter-Rater Reliability Tool"),
                                p("This tool is particularly useful for test and rubric developers interested 
                                   in improving and validating items and rubrics that involve multiple 
                                   ratings for a single skill or developmental competence."),
                                p(""),
                                p("The tool computes different varieties of the intra-class correlation 
                                  coefficient, which is an index of inter-rater reliability. In addition, 
                                  an F test (for the measurement of between-rater effects) and confidence 
                                  intervals are also computed."),
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
                                  a missing value is identified, the entire row (case) is removed from 
                                  the analysis (the Krippendorff's alpha, available below, may be more 
                                  suitiable when missing data is present)")
                              ),
                              wellPanel(
                                h4("2. Upload your inter-rater reliability data (csv)"),
                                p(""),
                                fileInput("input_file", "Choose your file (.csv)",                  # The file is observed by the UI when it is uploaded successfully.
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
                                textInput("construct", "Construct:", placeholder = "Test Topic"),
                                bsTooltip("construct", "E.g., Numeracy or Literacy", "right",
                                          options = list(container = "body")
                                         ),
                                
                                textInput("population", "Focal group:", placeholder = "Students"),  # 2. input: sample of interest (though modelling has population-bsed assumptions)
                                bsTooltip("population", "E.g., Central School Grade 10 Students", "right",
                                          options = list(container = "body")
                                         )
                                        ),
                              wellPanel(
                                h4("4. Specify the model"),
                                p(""),
                                
                                selectizeInput("model", "Select either the One-way or Two-way model",
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
                                
                                selectizeInput("type", "Select either Agreement or Consistency:", 
                                               choices = c("Agreement" = "agreement", 
                                                           "Consistency" = "consistency")
                                              ) %>%                           
                                  shinyInput_label_embed(icon("question-circle") %>%
                                    bs_embed_tooltip(title = "(a) For agreement, IRR is characterized by agreement in absolute terms across raters; (b) for consistency, IRR is characterised by correlation in scores across raters."
                                                    )
                                                        ),                                          # End of shinyInput_labelembed function (piping occurs withing that function)
                                       ),
                              wellPanel(
                                h4("6. Select Confidence Intervals"),
                                sliderInput("conf.level", 
                                            "Specify confidence interval level for ICC statistic", 
                                            min = 0.80, max = 0.99, value = 0.95, step = 0.01) %>%
                                  shinyInput_label_embed(icon("question-circle") %>%
                                    bs_embed_tooltip(title = "95 is a common confidnce interval for this statistic.")
                                                        ),                                          # End of shinyInput_labelembed function (piping occurs withing that function)
                                p("")
                                       ),
                              wellPanel(
                                h4("7. Include your own recommendations"),
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
                                )                                                                   # when conditions met in server logic), renamed as "Generate PDF report and spreadsheet'
                              )   # wellPanel
                       )       # column
                     )         # fluidRow
            ),                # IRR tabPanel


# Version number
            tabPanel(paste0("autopsych Version", version_number), fluid = TRUE, theme = shinytheme("cosmo"),               # css means cascading style sheets, describing how html elements are displayed on screen
                     tags$style(type="text/css",
                                "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                                ),
                     shinyjs::useShinyjs(),                                                         # activate javascript in the application
                     tags$h1(paste0("Current Version: autopsych_", version_number),
                             tags$img(src = "hex5.png", height = 149, width = 135, 
                                      style = "float:right;margin-top:-18.5px;"
                                     ),
                             tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 60px;
                                                              color: #FFFFFF;}"
                                            )
                                        )
                            ),
                     tags$h2("Toward Valid Assessments and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                             tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 18pt;
                                                             color: #FFFFFF;}"
                                            )
                                        )
                             ),
                     hr(), 
                     fluidRow(
                       column(11,
                              wellPanel(
                                h4("Build Versions"),
                                p("This page is dedicated to providing information about the build versions 
                                  of autopsych. "),
                                p(""),
                                p("The list of builds and associated changes are provided in the tabs below:"),
                                p("")
                                        ),
                              wellPanel(
                                h4("autopsych_0.1.0"),
                                p("The autopsych_0.1.0 build version includes five main functionalities 
                                  inclusive of:"),
                                p(""),
                                p("(1) Uni-Dim Rasch (unidimensional Rasch analysis) that provides CTT 
                                  and Rasch based analysis and reporting for dichotomous and polytomous 
                                  item-response data."),
                                p(""),
                                p("(2) Many-Facets Rasch (DIF; Differential Item Functioning) analysis 
                                  that provides an extension to Uni-Dim Rasch by providing an analysis 
                                  of item-difficulty bias against student groups."),
                                p(""),
                                p("(3) Rasch Equating analysis which enables (a) concurrent and (b) 
                                  fixed anchor callibration of test forms."),
                                p(""),
                                p("(4) ANOVA (ANalysis Of VAriance) which enables one-way ANOVA analysis 
                                  of student ability estimates by groups of interest."),
                                p(""),
                                p("(5) Inter-Rater Reliability (IRR) analysis which enables an examination 
                                  of rater consistency for the same item or total score."),
                                p(""),
                                p("[Release Date: 21 November, 2020]"),
                                p(""),
                                p("Contributors: Drs Matthew Courtney, Kevin Chang, Zhonghua Zhang, Eric 
                                  'Bing' Mei, & Luke Rowe")
                                       )
                              )       # column
                             )        # fluidRow
                     ),               # autopsych Versions tabPanel


# Team -------------------------------------------------------------------------            
            tabPanel("Team",
                     tabsetPanel(tabPanel("Chief Architect", 
                                          fluidRow(column(width = 6)),
                                          theme = shinytheme("cosmo"),                                                   # css means cascading style sheets, describing how html elements are displayed on screen
                                          tags$style(type="text/css",
                                                     "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                                     "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                                     " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                                          ),
                                          shinyjs::useShinyjs(),                                                         # activate javascript in the application
                                          tags$h1("Automated Psychometrics",
                                                  tags$img(src = "hex5.png", 
                                                           height = 149, 
                                                           width = 135, 
                                                           style = "float:right;margin-top:-18.5px;"
                                                           ),
                                                  tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 60px;
                                                              color: #FFFFFF;}"
                                                                  )
                                                             )
                                                 ),
                                          tags$h2("Toward Valid Assessments and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                                                  tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 18pt;
                                                             color: #FFFFFF;}"
                                                  )
                                                  )
                                          ),
                                          hr(), 
                                          fluidRow(
                                            column(11,
                                                   wellPanel(
                                                     h4("Chief Architect:"),
                                                     p(""),
                                                     p("Dr Matthew Gordon Ray Courtney (PhD)"),
                                                     p(""),
                                                     p("As Chief Architect, Dr Courtney is the founder of the website and provides expertise in UI design, 
                                                     overall website functionality, R scripts automating technical reports, and website related research and communication.")
                                                            ),
                                                   wellPanel(
                                                     h4("Qualifications:"),
                                                     p(""),
                                                     p("Doctor of Education (specialization: Higher Education) [2015], The University of 
                                                       Auckland, New Zealand"),
                                                     p(""),                                                     
                                                     p("Master of Education (specialization: Socio-linguistics), 1st class honours [2008], 
                                                       The University of Waikato, New Zealand"),
                                                     p(""),                                                      
                                                     p("Bachelor of Teaching/Bachelor of Sports & Leisure Studies (conjoint, secondary) [2003], The University of 
                                                       Waikato, New Zealand"),
                                                     p("")
                                                   ),
                                                   wellPanel(
                                                     h4("Bio:"),
                                                     p("Dr Courtney is an applied psychometrician and R Shiny software developer 
                                                     from New Zealand. He has expertise in both classical and item-response theories, automated statistical 
                                                     analysis and reporting, web UI development, and online learning. Dr completed his PhD in Education from The 
                                                     University of Auckland in 2015. For his doctorate he made use of advanced quantitative methods to 
                                                     identify the drivers of educational commitment and learning of international university students 
                                                     across Australasia."),
                                                     p(""),
                                                     p("After completing his PhD, he spent two years 
                                                      as a Post-Doctoral Research Fellow in the Quantitative Data Analysis and Research Unit at The Faculty of 
                                                      Education and Social Work at The University of Auckland. During his time there, Dr Courtney contributed 
                                                      to multiple research projects and academic journal publications and provided consultation to staff 
                                                      and post-graduate students. Thereafter, Dr Courtney worked for over three years as a Research Fellow 
                                                      at the Assessment Research Centre, Graduate School of Education, The University of Melbourne. There 
                                                      Dr Courtney contributed to multiple state, federal, and international research projects 
                                                      which focussed on educational assessment. In addition, Dr Courtney undertook extensive training in the 
                                                      fields of classical test theory, item-response theory, and R programming under the guidance of world-renowned 
                                                      psychometrician, Professor Margaret Wu. Currently, Dr Courtney works as an Assistant Professor at the 
                                                      Nazarbayev University Graduate School of Education, a research intensive university in Kazakhstan, Central 
                                                      Asia. Dr Courtney enjoys learning new languages, introduciung post-graduate students fun topics including 
                                                      educational assessment, educational statistics, classical and modern test theories, growth modelling, 
                                                      and R programming. A list of Dr Courtney's published journal articles and statistical packages are 
                                                      provided below:"),
                                                            ),
                                                   wellPanel(
                                                     h4("ACADEMIC JOURNAL ARTICLES AND STATISTICAL PACKAGES:"),
                                                     p(""),
                                                     h4("Assessment Focus"),
                                                     p(""),
                                                     p("1. Courtney, M. G. R. (2013). Determining the number of factors to retain in EFA: Using the SPSS R-Menu 
                                                     v-2.0 to make more judicious estimations. Practical Assessment, Research & Evaluation, 18(8). 
                                                     Retrieved from http://pareonline.net/pdf/v18n8.pdf (H = 48; Q2)"),
                                                     p("2. Panadero, E., Brown, G. T., & Courtney, M. 
                                                      G. R. (2014). Teachers’ reasons for using self-assessment: A survey self-report of Spanish teachers. Assessment 
                                                      in Education: Principles, Policy & Practice, 21(4), 365-383. doi: 10.1080/0969594X.2014.919247 (H = 39; 
                                                      Q1)"),
                                                     p(""),
                                                     p("3. Langdon, F., Alexander, P. A., Tesar, M., 
                                                      Courtney, M. G. R., & Palmer, M. (2016). Induction and mentoring in early childhood educational organizations: 
                                                      embracing the complexity of teacher learning in contexts. Teaching and Teacher Education, 57, 150-160. doi: 
                                                      10.1016/j.tate.2016.03.016 (H = 114; Q1)"),
                                                     p(""),
                                                     h4("Higher Education"),
                                                     p(""),
                                                     p("4. Qanay, G., Courtney, M. G. R., & Nam, A. (in press). Supporting teacher leadership development in schools 
                                                     in Kazakhstan: a mixed-methods study. International Journal of Leadership Education (IF: 34; Q1)"),
                                                     p(""),
                                                     p("5.	Kitchen, M., Jeurissen, M., Gray, S., & Courtney, M. G. R. (2017). Teacher engagement with academic 
                                                     reading in a post-service TESOL course. Indonesian Journal of Applied Linguistics, 6(2), 260-270. doi: 
                                                     http://dx.doi.org/10.17509/ijal.v6i2 (H = 7, Q2)"),
                                                     p(""),
                                                     p("6. Courtney, M. G. R. (2018). Emerging Academic and Social Spaces: Toward a Modern Understanding of 
                                                     International Student Integration in Australasia. International Journal of Cyber Behavior, Psychology, 
                                                     and Learning, 8(3), 36-47. doi: 10.4018/IJCBPL.2018070104 (H = 14; Q4)"),
                                                     p(""),
                                                     p("7. Lee-Morgan, J., Courtney, M. G. R., & Muller, M. (2019). New Zealand Māori-Medium Teacher Education: 
                                                     An Examination of Students' Academic Confidence and Preparedness. Asia-Pacific Journal of Teacher Education. 
                                                     doi: 10.1080/1359866X.2018.1539214 (IF: 32; Q1)"),
                                                     p(""),
                                                     p("8. Lee, K., Courtney, M. G. R., McGlashan, A., Neveldsen, P., Toso, M. (2019). Initial teacher education 
                                                     students’ perceptions of technology and technology education in New Zealand. International Journal of 
                                                     Technology and Design Education. doi: 10.1007/s10798-019-09516-6 (IF: 37; Q1)"),
                                                     p(""),
                                                     h4("Youth Program Evaluation"),
                                                     p("9.	Chapman, C. M., Deane, K. L., Harré, N., Courtney, M. G. R., & Moore, J. (2017). Engagement and mentor 
                                                     support as drivers of social development in the Project K youth development program. Journal of Youth and 
                                                     Adolescence, 1-12. doi: 10.1007/s10964-017-0640-5 (H = 110; Q1)"                                        ),
                                                     p(""),
                                                     p("10.	Deane, K., Harré, N., Moore, J., & Courtney, M. G. R. (2016). The impact of the Project K Youth Development 
                                                     Program on self-efficacy: a randomized control trial. Journal of Youth and Adolescence, March 6, 1-22. doi: 
                                                     10.1007/s10964-016-0463-9 (H = 110; Q1)"),
                                                     h4("R Packages and Shiny Applications"),
                                                     p("11.	Courtney, M. G. R., & Chang, K. (2018). Dealing with non-normality: An introduction and step-by-step 
                                                     guide using R. Teaching Statistics, 40(2), 51-59. doi: https://doi.org/10.1111/test.12154 (IF: 12; Q4)"),
                                                     p(""),
                                                     p("R package details: https://cran.r-project.org/web/packages/normalr/normalr.pdf"),
                                                     p(""),
                                                     p("Online app: https://kcha193.shinyapps.io/normalr/")
                                                            ) # End wellPanel
                                                  )           # End column 11 units wide
                                                )             # End Chief Architect fluid row
                                         ),                   # End Chief Architect inner tab panel
                                 
                                 tabPanel("Chief Programmer", 
                                          fluidRow(column(width = 6)),
                                          theme = shinytheme("cosmo"),                                                   # css means cascading style sheets, describing how html elements are displayed on screen
                                          tags$style(type="text/css",
                                                     "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                                     "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                                     " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                                          ),
                                          shinyjs::useShinyjs(),                                                         # activate javascript in the application
                                          tags$h1("Automated Psychometrics",
                                                  tags$img(src = "hex5.png", 
                                                           height = 149, 
                                                           width = 135, 
                                                           style = "float:right;margin-top:-18.5px;"
                                                  ),
                                                  tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 60px;
                                                              color: #FFFFFF;}"
                                                  )
                                                  )
                                          ),
                                          tags$h2("Toward Valid Assessments and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                                                  tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 18pt;
                                                             color: #FFFFFF;}"
                                                  )
                                                  )
                                          ),
                                          hr(), 
                                          fluidRow(
                                            column(11,
                                                   wellPanel(
                                                     h4("Chief Programmer:"),
                                                     p(""),
                                                     p("Dr Kevin Chih-Tao Chang (PhD)"),
                                                     p(""),
                                                     p("As Chief Programmer, Dr Chang provides expertise in experimental methods, reactive programming, 
                                                     R Shiny software development, IT operations, and software testing; AKA R Shiny DevOps ;-)")
                                                            ),
                                                   wellPanel(
                                                     h4("Qualifications:"),
                                                     p(""),
                                                     p("Doctor of Philosophy in Statistics [2018], The University of 
                                                       Auckland, New Zealand"),
                                                     p(""),                                                     
                                                     p("Bachelor of Science with 1st class honours (specialization: bio-informatics) in [2012], The University of 
                                                       Auckland, New Zealand"),
                                                     p("")
                                                   ),
                                                   wellPanel(
                                                     h4("Bio:"),
                                                     p("Dr Chang is an experienced Data Analyst with a demonstrated history working in the research inductry. He is highly 
                                                       skilled in Research, Data Analysis, Experimental Design, and R Programming."),
                                                     p(""),
                                                     p("For his PhD, Dr Chang developed a method for optimally designing experiments 
                                                       which involve two phases, the second phases to be needed for the observations to be made, such as in proteomics 
                                                       experiments. Using a combination of theory and computing, his methods quickly find data collection protocols 
                                                       which minimize the resources required to conduct such experiments while maximizing the information that can 
                                                       be drawn from them."),
                                                     p(""),
                                                     p("After completing his PhD, he spent XXX years...Dr Chang is an  A list of Dr Chang's publications are provided below:"
                                                     ),
                                                   ),
                                                   wellPanel(
                                                     h4("ACADEMIC JOURNAL ARTICLES AND STATISTICAL PACKAGES:"),
                                                     p(""),
                                                     h4("Child and Youth Health and Wellbeing"),
                                                     p(""),
                                                     p("1. Shackleton, N., Chang, K., Lay-yee, R. et al. (2019). Microsimulation model of child and adolescent overweight: 
                                                       making use of what we already know. International Journal of Obesity, 43, 2322–2332. doi: 10.1038/s41366-019-0426-9 
                                                       (H = 218; Q1)"),
                                                     p("2. Zhao, J., Mackay, L., Chang, K., Mavoa, S., Stewart, T., Ikeda, E., Donnellan, N., & Smith, M. (2019). Visualising 
                                                     combined time use patterns of children’s zctivities and their association with weight status and neighbourhood context. 
                                                     International Journal of Environmental Research and Public Health, 16(5), 1-17. doi. 10.3390/ijerph16050897 (H = 92; Q2)"),
                                                     p(""),
                                                     p("3. Lay-Yee, R., Milne, B. J., Shackleton, N., Chang, K., & Davis, P. (2018). Preventing youth depression: Simulating the 
                                                     impact of parenting interventions. Advances in Life Course Research, 37, 15-22. doi. 10.1016/j.alcr.2018.05.001 (H = 30; Q2)"),
                                                     p(""),
                                                     h4("Psychometrics"),
                                                     p("4. Chandra, N., Chang, K., Lee, A., Shekhawat, G. S., & Searchfield, G. D. (2018). Psychometric validity, reliability, 
                                                       and responsiveness of the tinnitus functional index. Journal of the American Academy of Audiology, 29(7), 609-625. 
                                                       doi 10.3766/jaaa.16171(H = 71, Q1)"),
                                                     p(""),
                                                     h4("R Packages and Shiny Applications"),
                                                     p("5.	Courtney, M. G. R., & Chang, K. (2018). Dealing with non-normality: An introduction and step-by-step 
                                                     guide using R. Teaching Statistics, 40(2), 51-59. doi: https://doi.org/10.1111/test.12154 (IF: 12; Q4)"),
                                                     p(""),
                                                     p("R package details: https://cran.r-project.org/web/packages/normalr/normalr.pdf"),
                                                     p(""),
                                                     p("Online app: https://kcha193.shinyapps.io/normalr/"),
                                                     p(""),
                                                     p("6. Chang, K., & Ruggiero, K. (2020). Package 'infoDecompuTE."),
                                                     p(""),
                                                     p("R package details: https://cran.r-project.org/web/packages/infoDecompuTE/index.html")
                                                   ) # End wellPanel
                                            )           # End column 11 units wide
                                          )             # End Chief Architect fluid row
                                 ),
                                 
                                 tabPanel("Chief Psychometrician", 
                                          fluidRow(column(width = 6)),
                                          theme = shinytheme("cosmo"),                                                   # css means cascading style sheets, describing how html elements are displayed on screen
                                          tags$style(type="text/css",
                                                     "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                                     "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                                     " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                                                    ),
                                          shinyjs::useShinyjs(),                                                         # activate javascript in the application
                                          tags$h1("Automated Psychometrics",
                                                  tags$img(src = "hex5.png", 
                                                           height = 149, 
                                                           width = 135, 
                                                           style = "float:right;margin-top:-18.5px;"
                                                           ),
                                                  tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 60px;
                                                              color: #FFFFFF;}"
                                                                  )
                                                            )
                                                 ),
                                          tags$h2("Toward Valid Assessments and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                                                  tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 18pt;
                                                             color: #FFFFFF;}"
                                                                 )
                                                            )
                                                  ),
                                          hr(), 
                                          fluidRow(
                                            column(11,
                                                   wellPanel(
                                                     h4("Chief Psychometrician:"),
                                                     p(""),
                                                     p("Dr Zhonghua Zhang (PhD)"),
                                                     p(""),
                                                     p("As Chief Psychometrician, Dr Chang provides expertise in item response theory (IRT), R programming, test equating 
                                                     , and omputational efficiency.")
                                                             ),
                                                   wellPanel(
                                                     h4("Qualifications:"),
                                                     p(""),
                                                     p("Doctor of Philosophy in Psychology [201X], The University of 
                                                       XXXXXXX, XXXXXXXXX"),
                                                     p(""),                                                     
                                                     p("Bachelor of XXX (specialization: xxxxxxxxxxx) in [201X], The University of 
                                                       XXXXXXXXX, XXXXXXXXXXX"),
                                                     p("")
                                                             ),
                                                   wellPanel(
                                                     h4("Bio:"),
                                                     p("Dr Zhang is an experienced psychometrician and educational statistician. He is highly skilled in psychometric 
                                                     and educational research, quantitative methods, and R programming."),
                                                     p(""),
                                                     p("For his PhD, Dr Zhang"),
                                                     p(""),
                                                     p("After completing his PhD, he spent XXX years...Dr Zhang is an  A list of Dr Chang's publications are provided below:"),
                                                             ),
                                                   wellPanel(
                                                     h4("ACADEMIC JOURNAL ARTICLES:"),
                                                     p(""),
                                                     h4("Psychometrics"),
                                                     p(""),
                                                     p("1. (H = XXX; QX)"),
                                                     p(""),
                                                     p("2. (H = XXX; QX)"),
                                                     p(""),
                                                     p("3. (H = XXX; QX)"),
                                                     p(""),
                                                     h4("Educational Psychology"),
                                                     p("4. (H = XXX; QX)"),
                                                     p(""),
                                                     p("5. (H = XXX; QX)"),
                                                     p(""),
                                                     p("6. (H = XXX; QX)"),
                                                     p(""),
                                                     h4("Educational Measurement"),
                                                     p("7.  (H = XXX; QX)"),
                                                     p(""),
                                                     p("8.  (H = XXX; QX)"),
                                                     p(""),
                                                     p("9.  (H = XXX; QX)"),
                                                     p(""),
                                                     p("10.  (H = XXX; QX)"),
                                                     p(""),
                                                     p("11.  (H = XXX; QX)"),
                                                     p("")
                                                   ) # End wellPanel
                                            )           # End column 11 units wide
                                          )             # End Chief Architect fluid row
                                         ),
                                 
                                 tabPanel("Psychometrician", 
                                          fluidRow(column(width = 6)),
                                          theme = shinytheme("cosmo"),                                                   # css means cascading style sheets, describing how html elements are displayed on screen
                                          tags$style(type="text/css",
                                                     "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                                     "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                                     " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                                                     ),
                                          shinyjs::useShinyjs(),                                                         # activate javascript in the application
                                          tags$h1("Automated Psychometrics",
                                                  tags$img(src = "hex5.png", 
                                                           height = 149, 
                                                           width = 135, 
                                                           style = "float:right;margin-top:-18.5px;"
                                                           ),
                                                  tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 60px;
                                                              color: #FFFFFF;}"
                                                                  )
                                                            )
                                                  ),
                                          tags$h2("Toward Valid Assessments and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                                                  tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 18pt;
                                                             color: #FFFFFF;}"
                                                                  )
                                                             )
                                                 ),
                                          hr(), 
                                          fluidRow(
                                            column(11,
                                                   wellPanel(
                                                     h4("Psychometrician"),
                                                     p(""),
                                                     p("Dr Eric 'Bing' Mei (PhD)"),
                                                     p(""),
                                                     p("As a Psychometrician, Dr Mei Chang expertise in educational statistics, automated reporting systems, R programming, 
                                                     online learning.")
                                                            ),
                                                   wellPanel(
                                                     h4("Qualifications:"),
                                                     p(""),
                                                     p("Doctor of Education [201X], The University of Auckland, 201X"),
                                                     p(""),               
                                                     p("Master of Education [201X], The University of Auckland, 201X"),
                                                     p(""), 
                                                     p("Bachelor of XXX (specialization: xxxxxxxxxxx) in [201X], The University of 
                                                       XXXXXXXXX, XXXXXXXXXXX"),
                                                     p("")
                                                            ),
                                                   wellPanel(
                                                     h4("Bio:"),
                                                     p("Dr Mei is an experienced psychometrician and educational statistician. He has experience providing support to post-graduate 
                                                     students looking to manage data and apply multivariate statistics for educational insights."),
                                                     p(""),
                                                     p("For his PhD, Dr Mei"),
                                                     p(""),
                                                     p("After completing his PhD, he spent XXX years...Dr Mei is an  A list of Dr Mei's publications are provided below:"),
                                                             ),
                                                   wellPanel(
                                                     h4("ACADEMIC JOURNAL ARTICLES:"),
                                                     p(""),
                                                     h4("Psychometrics"),
                                                     p(""),
                                                     p("1. (H = XXX; QX)"),
                                                     p(""),
                                                     p("2. (H = XXX; QX)"),
                                                     p(""),
                                                     p("3. (H = XXX; QX)"),
                                                     p(""),
                                                     h4("Educational Psychology"),
                                                     p("4. (H = XXX; QX)"),
                                                     p(""),
                                                     p("5. (H = XXX; QX)"),
                                                     p(""),
                                                     p("6. (H = XXX; QX)"),
                                                     p(""),
                                                     h4("Educational Measurement"),
                                                     p("7.  (H = XXX; QX)"),
                                                     p(""),
                                                     p("8.  (H = XXX; QX)"),
                                                     p(""),
                                                     p("9.  (H = XXX; QX)"),
                                                     p(""),
                                                     p("10.  (H = XXX; QX)"),
                                                     p(""),
                                                     p("11.  (H = XXX; QX)"),
                                                     p("")
                                                            ) # End wellPanel
                                            )           # End column 11 units wide
                                          )             # End Chief Architect fluid row
                                         ),
                                 
                                 tabPanel("Contributing Psychometrician", 
                                          fluidRow(column(width = 6)),
                                          theme = shinytheme("cosmo"),                                                   # css means cascading style sheets, describing how html elements are displayed on screen
                                          tags$style(type="text/css",
                                                     "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                                     "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                                     " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                                          ),
                                          shinyjs::useShinyjs(),                                                         # activate javascript in the application
                                          tags$h1("Automated Psychometrics",
                                                  tags$img(src = "hex5.png", 
                                                           height = 149, 
                                                           width = 135, 
                                                           style = "float:right;margin-top:-18.5px;"
                                                  ),
                                                  tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 60px;
                                                              color: #FFFFFF;}"
                                                  )
                                                  )
                                          ),
                                          tags$h2("Toward Valid Assessments and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                                                  tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 18pt;
                                                             color: #FFFFFF;}"
                                                  )
                                                  )
                                          ),
                                          hr(), 
                                          fluidRow(
                                            column(11,
                                                   wellPanel(
                                                     h4("Contributing Psychometrician"),
                                                     p(""),
                                                     p("Dr Luke Rowe (PhD)"),
                                                     p(""),
                                                     p("As a Contributing Psychometrician, Dr Luke Rowe expertise in educational statistics, meta-analysis, online education, and R programming.")
                                                   ),
                                                   wellPanel(
                                                     h4("Qualifications:"),
                                                     p(""),
                                                     p("Doctor of Education [201X], The University of Melbourne, 201X"),
                                                     p(""),               
                                                     p("Master of Education [201X], The University of XXXXXXXX, 201X"),
                                                     p(""), 
                                                     p("Bachelor of XXX (specialization: xxxxxxxxxxx) in [201X], The University of 
                                                       XXXXXXXXX, XXXXXXXXXXX"),
                                                     p("")
                                                   ),
                                                   wellPanel(
                                                     h4("Bio:"),
                                                     p("Dr Rowe is an expert quantitative educational researcher and has worked on a number of projects devoted to 
                                                     measuring and understanding student learning, collective intelligence, and computer supported collaborative learning."),
                                                     p(""),
                                                     p("For his PhD, Dr Rowe..."),
                                                     p(""),
                                                     p("After completing his PhD, he spent XXX years...Dr Rowe is an...  A list of Dr Rowe's publications are provided below:"),
                                                   ),
                                                   wellPanel(
                                                     h4("ACADEMIC "),
                                                     p(""),
                                                     h4("Psychometrics"),
                                                     p(""),
                                                     p("1. (H = XXX; QX)"),
                                                     p(""),
                                                     p("2. (H = XXX; QX)"),
                                                     p(""),
                                                     p("3. (H = XXX; QX)"),
                                                     p(""),
                                                     h4("Educational Psychology"),
                                                     p("4. (H = XXX; QX)"),
                                                     p(""),
                                                     p("5. (H = XXX; QX)"),
                                                     p(""),
                                                     p("6. (H = XXX; QX)"),
                                                     p(""),
                                                     h4("Educational Measurement"),
                                                     p("7.  (H = XXX; QX)"),
                                                     p(""),
                                                     p("8.  (H = XXX; QX)"),
                                                     p(""),
                                                     p("9.  (H = XXX; QX)"),
                                                     p(""),
                                                     p("10.  (H = XXX; QX)"),
                                                     p(""),
                                                     p("11.  (H = XXX; QX)"),
                                                     p("")
                                                   ) # End wellPanel
                                            )           # End column 11 units wide
                                          )             # End Chief Architect fluid row
                                         )                    # End Contributing Psychometrician tabset panel
                                 ),                           # End embedded tabset panel
                     ),                                       # End main Team tabset

# Highlights -------------------------------------------------------------------            
            tabPanel("Highlights",
                     fluid = TRUE),

# Contact -------------------------------------------------------------------                
            tabPanel("Contact",
                     fluid = TRUE)
                  )      # tabsetPanel
               )         # fluidPage

# Server file starts here -------------------------------------------------- 
server <- function(input, output, session){
                                                                                # the main  input, input$input_file (csv), is recognised when the following conditions are met.

  
# ANOVA server -------------------------------------------------------------------            
  
  anova_Server("anova") # ANOVA module

# Download server -------------------------------------------------------------------            

  download_Server("MML") # download module
    

}  # server function wrapper

shinyApp(ui = ui, server = server)




