# Kevin to give review of version number below:
# It's like a global variable that makes changes throughout trail tabs

version_number <- "0.1.0"

# Ensure all necessary packages installed on machine (tidyverse covers ggplot2 and dplyr)
autopsych_pack <- c("ggrepel", "plyr", "scales", "tidyverse", "ShinyItemAnalysis", 
                    "CTT", "psychometric", "irr", "TAM", "cowplot", "openxlsx", "reshape2", "Hmisc", "xtable", 
                    "knitr", "rmarkdown", "kableExtra", "english", "shiny", "shinyjs",
                    "shinythemes", "shinyBS", "bsplus", "shinyWidgets")
packages_required <- autopsych_pack[!autopsych_pack %in% installed.packages()] 


if(length(packages_required) > 0) {
  install.packages(packages_required)
}

if(packageVersion("shiny") < "1.5.0") {
  install.packages("shiny")
}

# Ensure all necessary packages loaded loaded to library (some use package::function throughout script so do not need to be loaded)
packages_to_load <- c("scales", "shinythemes",  "shinyBS", "bsplus", "shinyWidgets")

# Ensure all necessary packages loaded to library
lapply(packages_to_load, library, character.only = TRUE)
# For list of packages, see associated publication.


# This line of code will set the maximum allowed file size of uploads (3MB), adjustable to computational power.
options(shiny.maxRequestSize = 3*1024^2)

# Define UI for app that lets you select desired inputs

# tags$style enables nice styling
# fonts at: https://fonts.google.com and you have to find ones which are open source
# "Open Sans" is a nice option
# "simplex" is a nice theme, though "cosmo" chosen here.
# Themes at: https://rstudio.github.io/shinythemes/ 

# UI file starts here -------------------------------------------------- 

# tags$head script just below makes all font white

ui <- fluidPage(tags$head(tags$style(HTML("a {color: white}"))),
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
         tags$h2("Toward Valid Assessment and Educational Research",                    # h2 header is the subtitle underneath the h1 header
                 tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                            line-height: 1.1; font-size: 18pt; color: #FFFFFF;}"
                 )
                 )
         ),
         hr(),
         tags$h3("Lead Architect:",                                                     # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:matthew.courtney@nu.edu.kz?Subject=Shiny%20Help",
                        "        Dr Matthew Courtney (PhD)"
                 ),
                 tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 10pt;
                                                             color: #FFFFFF;}"
                 )
                 )
         ),
         tags$h3("Lead Developer:",                                                    # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:kevin.ct.chang@gmail.com?Subject=Shiny%20Help",
                        "Dr Kevin Chang (PhD)"
                 ),
                 tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                             line-height: 1.1; font-size: 10pt;
                                                             color: #FFFFFF;}"
                 )
                 )
         ),
         tags$h3("Lead Psychometrician:",                                               # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                        "Dr Bing Mei (PhD)"),
                 tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                    line-height: 1.1; font-size: 10pt;
                                                                    color: #FFFFFF;}"
                 )
                 )
         ),
         tags$h3("Contributing Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:k.meissel@auckland.ac.nz?Subject=Shiny%20Help",
                        "Dr Kane Meissel (PhD)"
                 ),
                 tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 10pt;
                                                              color: #FFFFFF;}"
                 )
                 )
         ),
         tags$h3("Contributing Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                        "Dr Luke Rowe (PhD)"
                 ),
                 tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 10pt;
                                                              color: #FFFFFF;}"
                 )
                 )
         ),
         tags$h3("Contributing Mathematician:",                                        # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:laila.educator@gmail.com?Subject=Shiny%20Help",
                        "Ms Laila Issayeva (MEd)"
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
                    p("Welcome to Automated Psychometrics, a novel website that allows teachers, 
                      school assessment leaders, test developers, educational institutions, 
                      and researchers to:"),
                    p(""),
                    p("(1) Check the general quality of student assessments and 
                      developmental rubrics,"),
                    p(""),
                    p("(2) Ensure test questions and developmental criterion are not bias toward 
                      any demographic group,"),
                    p(""),
                    p("(3) Place students from different year groups on a single developmental scale via test equating,"),
                    p(""),
                    p("(4) Track student progress over a period of time via test equating,"),
                    p(""),
                    p("(5) Analyze the effect of student grouping (gender, class, school) on student ability,"),
                    p(""),
                    p("(6) Check the reliability of developmental rubrics via inter-rater reliability analysis."),
                          ),
                  wellPanel(
                    h4("Team Vision"),
                    p("The autopsych team draws on extensive expertise in educational and 
                    psychological assessment, quantitative research methods, statistical programming, 
                    web-design, teaching pedagogy, and online learning. The team collaborates to provide 
                    this app and its various features from different parts of the Asia-Pacific region 
                    including New Zealand, Australia, China, and Central Asia. The team's visions is 
                    to promote high quality assessment and research  accessible to the developed and 
                    developing world.")
                  ),   # WellPanel
                  wellPanel(
                    h4("Use"),
                    p("The website and all functionality was built using the open-source R programming 
                    language and received no external funding. The autopsych app is a free software 
                    and you can redistribute it and or modify it under the terms of the GNU GPL 3. 
                    In your work, cite as:"),
                    p(""),
                    p("Courtney, M. G. R., Xxxxx, X., Xxxxx, X., & Xxxxxx, X. (XXXX). autopsych: a 
                    novel shiny app for the psychometric analysis and scoring of assessment and 
                    survey data. The X Journal, X(X), XXX-XXX. doi. XXXXXXXXXXXXX."),
                    p("")
                  ),   # WelPanel
                  wellPanel(
                    h4("Distribution"),
                    p("In accordance with the GNU General Public License (GPL) 3:"),
                    p(""),
                    p("'If you distribute copies of such a program, whether gratis or for a fee, you must pass on to the recipients the 
                    same freedoms that you received. You must make sure that they, too, receive or can get the source code. And you 
                    must show them these terms so they know their rights' (https://www.gnu.org/licenses/gpl-3.0.en.html)"),
                    p(""),
                    p("'The intention is to enshrine the rights of users to share and build on ideas falls into the philospohical 
                      concept of common heritage (i.e., standing on the shoulders of giants). It is a bridging mechanism that 
                      encourages growth from social knowledge' https://snyk.io")
                  )
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
                 tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                        "Dr Bing Mei (PhD)"),
                 tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                    line-height: 1.1; font-size: 10pt;
                                                                    color: #FFFFFF;}"
                                )
                           )
                ),
         fluidRow(
           column(11,
                  MML_UI("MML"),
                  
                  download_UI("MML")
                  
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
         tags$h2("Toward Unbiased Test Questions and Developmental Criterion",                   # h2 header is the subtitle underneath the h1 header
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
                        "Dr Bing Mei (PhD)"),
                 tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                    line-height: 1.1; font-size: 10pt;
                                                                    color: #FFFFFF;}"
                                )
                            )
                ),
         fluidRow(
             column(11,
                  
                    FACETS_UI("FACETS"),
                  
                    download_UI("FACETS")
                  
                   )
                 )              # fluidRow
         ),                     # Baseline DIF build (MC, ZZ).


# Rasch Equating ----------------------------------------------------------
           
            tabPanel("Rasch Equating",
                     tabsetPanel(tabPanel("Fixed Anchor", 
                      fluidRow(column(width = 6)),
                      theme = shinytheme("cosmo"),
                      tags$style(type="text/css",
                                 "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                 "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                 " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                      ),
                      shinyjs::useShinyjs(),                                                         # activate javascript in the application
                      tags$h1("Fixed Anchor Test Equating",
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
                      tags$h2("Toward Valid and Comparable Test Forms",                             # h2 header is the subtitle underneath the h1 header
                              tags$style(HTML("h2{font-family: 'Open Sans'; font-weight: 500;
                                                  line-height: 1.1; font-size: 18pt; color: #FFFFFF;}"
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
                              tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                                     "Dr Bing Mei (PhD)"),
                              tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                    line-height: 1.1; font-size: 10pt;
                                                                    color: #FFFFFF;}"
                              )
                              )
                      ),
                      fluidRow(
                        column(11,
                               EQUATE_UI("EQUATE"),
                               download_UI("EQUATE")
                        )                               # End column that uses 11 width
                      )                              # End fluidRow that takes multiple panels
                     ),                                     # End Fixed Anchor tab panel that takes multiple fliudrows
                     
                     tabPanel("Concurrent", 
                              fluidRow(column(width = 6)),
                              theme = shinytheme("cosmo"),                                                   # css means cascading style sheets, describing how html elements are displayed on screen
                              tags$style(type="text/css",
                                         "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                         "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                         " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                              ),
                              shinyjs::useShinyjs(),                                                         # activate javascript in the application
                              tags$h1("Concurrent Test Equating",
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
                              tags$h2("Toward Valid and Comparable Test Forms",                   # h2 header is the subtitle underneath the h1 header
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
                                         h4("Concurrent Calibration Tool"),
                                         p(""),
                                         p("The concurrent calibration method...")
                                       ),
                                       wellPanel(
                                         h4("1. Prepare your data:"),
                                         p(""),
                                         p("(a) Do a"),
                                         p(""),
                                         p("(b) Do b"),
                                         p(""),
                                         p("(c) Do c"),
                                         p(""),
                                         p("(d) Do d"),
                                         p(""),
                                         p("(e) Do e")
                                       )    # End wellPanel
                                )           # End column 11 units wide
                              )              # End fluid row
                     )                       # End Concurrent Calibration tab panel
                     )                                # End embedded tabset panels
            ),                                         # End main Equating tabpanel

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
                                p("This tool provides a convenient way to examine the effect of student 
                                  grouping (such as student gender, class, or school classification) on 
                                  student ability or some measured personal attribute."
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
                              tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                                     "Dr Bing Mei (PhD)"
                                    ),
                              tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 10pt;
                                                              color: #FFFFFF;}"
                                             )
                                        )
                              ),
                      tags$h3("Psychometrician:",                                                    # h2 header is the subtitle underneath the h1 header
                              tags$a(href = "mailto:matthew.courtney@nu.edu.kz?Subject=Shiny%20Help",
                                     "Dr Matthew Courtney (PhD)"),
                              tags$style(HTML("h3{font-family: 'Open Sans'; font-weight: 500;
                                                                     line-height: 1.1; font-size: 10pt;
                                                                     color: #FFFFFF;}"
                                             )
                                        )
                              ),
                      fluidRow(
                        column(11,
                               IRR_UI("IRR") ,
                               download_UI("IRR")
                        )       # column
                      )         # fluidRow
             ),                # IRR tabPanel


# Version number
            tabPanel(paste0("autopsych Version ", version_number), 
                     fluid = TRUE, theme = shinytheme("cosmo"),               # css means cascading style sheets, describing how html elements are displayed on screen
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
                                  fixed anchor calibration of test forms."),
                                p(""),
                                p("(4) ANOVA (ANalysis Of VAriance) which enables one-way ANOVA analysis 
                                  of student ability estimates by groups of interest."),
                                p(""),
                                p("(5) Inter-Rater Reliability (IRR) analysis which enables an examination 
                                  of rater consistency for the same item or total score."),
                                p(""),
                                p("[Release Date: 21 November, 2020]"),
                                p(""),
                                p("Contributors: Drs Matthew Courtney, Kevin Chang, Eric 
                                  'Bing' Mei, Kane Meissel, Luke Rowe, & Ms Laila Issayeva")
                                       )
                              )       # column
                             )        # fluidRow
                     ),               # autopsych Versions tabPanel


# Team -------------------------------------------------------------------------            
            tabPanel("Team",
                     tabsetPanel(tabPanel("Lead Architect", 
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
                                   h4("Lead Architect:"),
                                   p(""),
                                   p("Dr Matthew Gordon Ray Courtney (PhD)"),
                                   p(""),
                                   p("The Lead Architect, Dr Courtney, is the founder of the website and provides expertise in psychometrics, 
                                     quantitative research methods, automation of technical reports and outputs, web UI development, and related 
                                     research and communication.")
                                          ),
                                 wellPanel(
                                   h4("Qualifications:"),
                                   p(""),
                                   p("Doctor of Education (focus: higher education, quantitative methods) [2015], The University of 
                                     Auckland, New Zealand"),
                                   p(""),                                                     
                                   p("Master of Education (focus: socio-linguistics, quantitative methods), 1st class honours [2008], 
                                     The University of Waikato, New Zealand"),
                                   p(""),                                                      
                                   p("Bachelor of Teaching/Bachelor of Sports & Leisure Studies (conjoint, secondary teaching) 
                                   [2003], The University of Waikato, New Zealand"),
                                   p("")
                                 ),
                                 wellPanel(
                                   h4("Bio:"),
                                   p("Dr Courtney is an education expert, psychometrician, and R Shiny statistical software developer 
                                   from New Zealand. He has expertise in both classical and item-response theories, automated statistical 
                                   analysis and reporting, web UI development, and online learning. Dr Courtney completed his PhD in 
                                   Education from The University of Auckland in 2015. For his doctorate he made use of advanced 
                                   quantitative methods to identify the drivers of educational commitment and learning of international 
                                   university students across Australasia."),
                                   p(""),
                                   p("After completing his PhD, he spent two years 
                                    as a Post-Doctoral Research Fellow in the Quantitative Data Analysis and Research Unit at The Faculty of 
                                    Education and Social Work at The University of Auckland. During his time there, Dr Courtney contributed 
                                    to multiple research projects and academic journal publications and provided consultation to staff 
                                    and post-graduate students. Thereafter, Dr Courtney worked for over three years as a Research Fellow 
                                    at the Assessment Research Centre, Graduate School of Education, The University of Melbourne. There 
                                    Dr Courtney contributed to multiple state, federal, and international projects which focussed on educational 
                                    measurement and assessment. At the Graduate School of Education, Dr Courtney completed extensive training in the 
                                    fields of classical test theory, item-response theory, and R statistical programming under the tutelage of 
                                    world-renowned statistician, Professor Margaret Wu. Currently, Dr Courtney works as 
                                    an Assistant Professor at the Nazarbayev University Graduate School of Education, a research intensive 
                                    university in Kazakhstan, Central Asia. Dr Courtney enjoys introduciung post-graduate students to fun topics 
                                    including educational assessment, educational statistics, classical test theory, item response theory, 
                                    growth modelling, and R programming. A list of Dr Courtney's published journal articles, R statistical 
                                    packages, and encyclopedia chapters are provided below:"),
                                          ),
                                 wellPanel(
                                   h4("ACADEMIC JOURNAL ARTICLES:"),
                                   h4("R Statistical Packages and R Shiny Applications"),
                                   p("1.	Courtney, M. G. R., & Chang, K. (2018). Dealing with non-normality: An introduction and step-by-step 
                                   guide using R. Teaching Statistics, 40(2), 51-59. doi: https://doi.org/10.1111/test.12154 (IF: 12; Q4)"),
                                   p(""),
                                   p("R package details: https://cran.r-project.org/web/packages/normalr/normalr.pdf"),
                                   p(""),
                                   p("Online app: https://kcha193.shinyapps.io/normalr/"),
                                   p(""),
                                   h4("Educational Measurement and Assessment"),
                                   p(""),
                                   p("2. Courtney, M. G. R. (2013). Determining the number of factors to retain in EFA: Using the SPSS R-Menu 
                                   v-2.0 to make more judicious estimations. Practical Assessment, Research & Evaluation, 18(8). 
                                   Retrieved from http://pareonline.net/pdf/v18n8.pdf (H = 48; Q2)"),
                                   p("3. Panadero, E., Brown, G. T., & Courtney, M. 
                                    G. R. (2014). Teachers’ reasons for using self-assessment: A survey self-report of Spanish teachers. Assessment 
                                    in Education: Principles, Policy & Practice, 21(4), 365-383. doi: 10.1080/0969594X.2014.919247 (H = 39; 
                                    Q1)"),
                                   p(""),
                                   p("4. Langdon, F., Alexander, P. A., Tesar, M., 
                                    Courtney, M. G. R., & Palmer, M. (2016). Induction and mentoring in early childhood educational organizations: 
                                    embracing the complexity of teacher learning in contexts. Teaching and Teacher Education, 57, 150-160. doi: 
                                    10.1016/j.tate.2016.03.016 (H = 114; Q1)"),
                                   p(""),
                                   h4("Higher Education"),
                                   p(""),
                                   p("5. Qanay, G., Courtney, M. G. R., & Nam, A. (2021). Supporting teacher leadership development in schools 
                                     in Kazakhstan: a mixed-methods study. International Journal of Leadership Education. doi: 
                                     10.1080/13603124.2020.1869314 (IF: 34; Q1)"),
                                   p(""),
                                   p("6.	Kitchen, M., Jeurissen, M., Gray, S., & Courtney, M. G. R. (2017). Teacher engagement with academic 
                                     reading in a post-service TESOL course. Indonesian Journal of Applied Linguistics, 6(2), 260-270. doi: 
                                     10.17509/ijal.v6i2 (H = 7, Q2)"),
                                   p(""),
                                   p("7. Courtney, M. G. R. (2018). Emerging Academic and Social Spaces: Toward a Modern Understanding of 
                                   International Student Integration in Australasia. International Journal of Cyber Behavior, Psychology, 
                                   and Learning, 8(3), 36-47. doi: 10.4018/IJCBPL.2018070104 (H = 14; Q4)"),
                                   p(""),
                                   p("8. Lee-Morgan, J., Courtney, M. G. R., & Muller, M. (2019). New Zealand Māori-Medium Teacher Education: 
                                   An Examination of Students' Academic Confidence and Preparedness. Asia-Pacific Journal of Teacher Education. 
                                   doi: 10.1080/1359866X.2018.1539214 (IF: 32; Q1)"),
                                   p(""),
                                   p("9. Lee, K., Courtney, M. G. R., McGlashan, A., Neveldsen, P., Toso, M. (2019). Initial teacher education 
                                   students’ perceptions of technology and technology education in New Zealand. International Journal of 
                                   Technology and Design Education. doi: 10.1007/s10798-019-09516-6 (IF: 37; Q1)"),
                                   p(""),
                                   h4("Youth Program Evaluation"),
                                   p("10.	Chapman, C. M., Deane, K. L., Harré, N., Courtney, M. G. R., & Moore, J. (2017). Engagement and mentor 
                                   support as drivers of social development in the Project K youth development program. Journal of Youth and 
                                   Adolescence, 1-12. doi: 10.1007/s10964-017-0640-5 (H = 110; Q1)"                                        ),
                                   p(""),
                                   p("11.	Deane, K., Harré, N., Moore, J., & Courtney, M. G. R. (2016). The impact of the Project K Youth Development 
                                   Program on self-efficacy: a randomized control trial. Journal of Youth and Adolescence, March 6, 1-22. doi: 
                                   10.1007/s10964-016-0463-9 (H = 110; Q1)")
                                           ),
                                 wellPanel(
                                   h4("ENCYCLOPEDIA CHAPTERS:"),
                                   h4("The SAGE Encyclopedia of Education Research, Measurement and Evaluation"),
                                   p("1. Courtney, M. G. R. (2018). The Repeated Measures ANOVA. In B. Frey (Ed.), The SAGE encyclopedia of education 
                                     research, measurement and evaluation (pp. 1403-1407). Thousand Oaks, CA: Sage. doi: 10.4135/9781506326139.n585"),
                                   p(""),
                                   p("2. Courtney, M. G. R. (2018). The Pearson Correlation Coefficient. In B. Frey (Ed.), The SAGE encyclopedia of 
                                     education research, measurement and evaluation (pp. 1229-1233). Thousand Oaks, CA: Sage. doi: 10.4135/9781506326139.n510"),
                                   p(""),
                                   p("3. Courtney, M. G. R. (2018). IBM SPSS Statistics. In B. Frey (Ed.), The SAGE encyclopedia of education 
                                     research, measurement and evaluation (pp. 1577-1583). Thousand Oaks, CA: Sage. doi: 10.4135/9781506326139.n655")
                                          ) # End wellPanel
                                )           # End column 11 units wide
                              )             # End Chief Architect fluid row
                       ),                   # End Chief Architect inner tab panel
               
               tabPanel("Lead Developer", 
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
                                   h4("Lead Developer:"),
                                   p(""),
                                   p("Dr Kevin Chih-Tao Chang (PhD)"),
                                   p(""),
                                   p("As Lead Developer, Dr Chang provides expertise in experimental methods, reactive programming, 
                                   R Shiny software development, IT operations, and software testing.")
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
               
               tabPanel("Lead Psychometrician", 
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
                                   h4("Lead Psychometrician:"),
                                   p(""),
                                   p("Dr Bing Mei (PhD)"),
                                   p(""),
                                   p("As a Contributing Psychometrician, Dr Bing Mei expertise in educational statistics, online education, and R programming.")
                                    ),
                                 wellPanel(
                                   h4("Qualifications:"),
                                   p(""),
                                   p("Doctor of Education [2012], The University of Auckland, 2017"),
                                   p(""),    
                                   p("Master of Arts [2006], Henan University, 2009"),
                                   p(""),  
                                   p("Bachelor of English [1999], Henan University, 2003"),
                                   p("")
                                           ),
                                 wellPanel(
                                   h4("Bio:"),
                                   p("Dr Mei is an associate professor from the School of Foreign Languages at Henan University. 
                                     His research interests include educational statistics, technology-enhanced language learning, 
                                     technology acceptance, and teacher education. His current research project focuses on the 
                                     potential use of AR/VR in the educational context."),
                                   p(""),
                                   p("After completing his PhD, he spent two years working as a project assistant and one year 
                                     working as a research fellow at the University of Auckland."),
                                   p("") ),
                                 wellPanel(
                                   h4("ACADEMIC JOURNAL ARTICLES:"),
                                   p(""),
                                   p("1. Sun, P. P., & Mei, B. (2020). Modeling preservice Chinese-as-a-second/foreign-language teachers’ 
                                     adoption of educational technology: A technology acceptance perspective. Computer Assisted 
                                     Language Learning, 0(0), 1–24. https://doi.org/10.1080/09588221.2020.1750430 (H=45, Q1)"),
                                   p(""),
                                   p("2. Mei, B. (2019a). Language teacher education and technology: Approaches and practices. 
                                     Calico Journal, 36(3), 240–242. https://doi.org/10.1558/cj.35737 (H=31, Q1)"),
                                   p(""),
                                   p("3. Mei, B. (2019b). Preparing preservice EFL teachers for CALL normalisation: A technology 
                                     acceptance perspective. System, 83, 13–24. https://doi.org/10.1016/j.system.2019.02.011 
                                     (H = 71, Q1)"),
                                   p(""),
                                   p("4. Mei, B., & Yang, S. (2019). Nurturing environmental education at the tertiary education level 
                                     in China: Can mobile augmented reality and gamification help? Sustainability, 11(16), 4292. 
                                     https://doi.org/10.3390/su11164292 (H=68, Q2)"),
                                   p(""),
                                   p("5. Teo, T., Sang, G., Mei, B., & Hoi, C. K. W. (2019). Investigating pre-service teachers’ 
                                     acceptance of Web 2.0 technologies in their future teaching: A Chinese perspective. Interactive 
                                     Learning Environments, 27(4), 530–546. https://doi.org/10.1080/10494820.2018.1489290 (H=38, Q1)"),
                                   p(""),
                                   p("6. Yang, S., & Mei, B. (2018). Understanding learners’ use of augmented reality in language 
                                     learning: Insights from a case study. Journal of Education for Teaching, 44(4), 511–513. 
                                     https://doi.org/10.1080/02607476.2018.1450937 (H=34, Q1)"),
                                   p(""),
                                   p("7. Yang, S., Mei, B., & Yue, X. (2018). Mobile augmented reality assisted chemical education: 
                                     Insights from elements 4d. Journal of Chemical Education, 95(6), 1060–1062. 
                                     https://doi.org/10.1021/acs.jchemed.8b00017(H =77, Q2)"),
                                   p(""),
                                   p("8. Mei, B., & Brown, G. T. L. (2018). Conducting Online Surveys in China. Social Science 
                                     Computer Review, 36(6), 721–734. https://doi.org/10.1177/0894439317729340 (H=67, Q1)"),
                                   p(""),
                                   p("9. Mei, B., Brown, G. T. L., & Teo, T. (2018). Toward an Understanding of Preservice 
                                     English as a Foreign Language Teachers’ Acceptance of Computer-Assisted Language 
                                     Learning 2.0 in the People’s Republic of China. Journal of Educational Computing 
                                     Research, 56(1), 74–104. https://doi.org/10.1177/0735633117700144 (H=57, Q1)"),
                                   p(""),
                                   p("10. Mei, B., & May, L. (2018). Reflective renovation: Insights from a collaborative and 
                                     active learning space project evaluation. Australasian Journal of Educational Technology, 
                                     34(6). https://doi.org/10.14742/ajet.4476 (H=43, Q1)"),
                                   p("")
                                 )    # End wellPanel
                          )           # End column 11 units wide
                        )             # End lead Architect fluid row
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
                                   p("Dr Kane Meissel (PhD)"),
                                   p(""),
                                   p("As a Contributing Psychometrician, Dr Kane Miessel has expertise in educational statistics,
                                   especially in the application of multi-level models to educational data, R programming, and 
                                   online learning.")
                                          ),
                                 wellPanel(
                                   h4("Qualifications:"),
                                   p(""),
                                   p("Doctor of Psychology [2013], The University of Auckland, 201X"),
                                   p(""),               
                                   p("Master of Education [2010], The University of Auckland, 201X"),
                                   p(""), 
                                   p("Bachelor of XXX (specialization: xxxxxxxxxxx) in [201X], The University of 
                                     XXXXXXXXX, XXXXXXXXXXX"),
                                   p("")
                                          ),
                                 wellPanel(
                                   h4("Bio:"),
                                   p("Dr Meissel is an experienced psychometrician and educational statistician. He has experience providing support to post-graduate 
                                   students looking to manage data and apply multivariate statistics for educational insights."),
                                   p(""),
                                   p("For his PhD, Dr Meissel..."),
                                   p(""),
                                   p("After completing his PhD, he spent XXX years...Dr Meissel is an... A list of Dr Mei's publications are provided below:"),
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
               
               
               
               ,
               
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
                                   p("Ms Laila Issayeva (B.Mathematics, M. Ed)"),
                                   p(""),
                                   p("As a Contributing Psychometrician, Ms Laila Issayeva has expertise in item writing, test development, psychometrics, 
                                     and a developing interest in R programming.")
                                 ),
                                 wellPanel(
                                   h4("Qualifications:"),
                                   p(""),               
                                   p("Master of Science in Educational Leadership (focus: School Leadership and 
                                     Improvement Management) [2017], Nazarbayev University, Kazakhstan"),
                                   p(""), 
                                   p("Bachelor of Education (major: Mathematics, minor: Computer Science) [2007], K. 
                                     Zhubanov Aktobe Regional University, Aktobe, Kazakhstan"),
                                   p("")
                                 ),
                                 wellPanel(
                                   h4("Bio:"),
                                   p("Ms Laila Issayeva is an experienced educator with more than 10 years of teaching Mathematics 
                                   to secondary and high school students, including teaching IGCSE, AS level, and A level. During 
                                   her teaching time, Laila was instrumental in developing lesson plans, integrated Mathematics 
                                   subject programs collaboratively with Cambridge Assessment International Education (CAIE, 
                                   Cambridge, the UK)."),
                                   p(""),
                                   p("Ms Issayeva is an Educational Measurement specialist and has good skills at developing 
                                   assessment tools, processing exams, setting standards, analyzing (CTT&amp;IRT), interpreting, and 
                                   presenting data (reports, slides). Laila has been overseeing the development and implementation 
                                   of a national Student Performance Monitoring system for Mathematics in cooperation with the 
                                   psychometricians from the Institute for Educational Measurement Cito (Cito, Arnhem, the Netherlands) 
                                   for six years. Now, she spearheads the process of shifting its format from computer-based to 
                                   computerized adaptive testing (CAT)"),
                                   p(""),
                                   p("Laila is actively involved into the process of developing, marking, and grading External Summative 
                                   Assessment Examinations accredited by CAIE. Recently, she has conceptualized and administered 
                                   functional literacy tests for Mathematics, Reading, and Sciences nationwide."), 
                                   p(""),
                                   p("Within the Master&#39;s program, Laila completed a qualitative study to explore the extent of 
                                   accessibility and applicability of Student Performance Monitoring reports for Mathematics teachers, 
                                   and eventually optimized those reports."),
                                   p(""),
                                   p("Starting from 2016, Ms Issayeva has taken part in the international annual conferences of AEA-Europe 
                                   and IAEA and has produced several papers (available on www.researchgate.net) and presentations based on 
                                   the research projects. She has co-authored several manuals and guidelines for educators to help them 
                                   develop a valid and reliable student performance monitoring system. A list of Laila Issayeva’s 
                                   research outputs is provided below:"),
                                   p("")
                                 ),
                                 wellPanel(
                                   h4("CONFERENCE PAPERS AND PRESENTATIONS:"),
                                   p(""),
                                   h4("Psychometrics"),
                                   p(""),
                                   p("Issayeva L., Rakhymbayeva Z., &amp; Temirtassov, D. (2018, October 25). The unification of a 
                                   student ability scale: first results of the psychometric research study [Presentation]. The 10th 
                                   NIS International Research-to-Practice Conference “Next Generation Schools”, Astana, Kazakhstan. 
                                   https://www.researchgate.net/publication/340816106"),
                                   p(""),
                                   p("Issayeva L., Dieteren, N., &amp; Crans, S. (2018, October 15). Curriculum sampling as a strategy 
                                   employed for a student performance monitoring system for Mathematics at Nazarbayev Intellectual 
                                   Schools [Conference paper]. The 44 th International Association for Educational Assessment 
                                   Conference “Assessment and Big Data”, Oxford, the UK. https://www.researchgate.net/publication/328293279"),
                                   p(""),
                                   p("Issayeva L., Dieteren, N., &amp; Crans, S. (2017, November 9). Assessment tool validation research 
                                   at Nazarbayev Intellectual Schools: student performance monitoring system for Mathematics [Conference paper]. 
                                   The 18th Association for Educational Assessment in Europe Conference 'Assessment cultures in a globalised world', 
                                   Prague, Czech Republic. https://www.researchgate.net/publication/335033501"),
                                   p(""),
                                   p("Issayeva, L., &amp; Temirtassov, D. (2017, October 26). Current approaches to the external assessment 
                                   of student achievement in Mathematics at Nazarbayev Intellectual Schools [Presentation]. The 9 th NIS 
                                   International Research-to-Practice Conference “Values, Wellbeing and Innovation for the Future of 
                                   Education”, Astana, Kazakhstan. https://www.researchgate.net/publication/340931566"),
                                   p(""),
                                   p("Issayeva, L., Temirtassov, D., Dieteren, N., Crans, S., &amp; Kamphuis, F. (2016, November 5). 
                                   Student performance monitoring for Mathematics as an effective instrument to adjust individual 
                                   learning paths for students and to enhance didactic tools for teachers at Nazarbayev Intellectual 
                                   Schools [Conference paper]. The 17 th Association for Educational Assessment in Europe Conference 
                                   'Social and Political underpinnings of educational assessment: Past, present and future', Limassol, 
                                   Cyprus. https://www.researchgate.net/publication/340931730"),
                                   p(""),
                                   h4("Academic Publications"),
                                   p(""),
                                   p("Issayeva, L., Temirtassov, D., Tursynova, L., &amp; Mozhayeva, O. (2019). Guidelines for item 
                                   development to conduct Student Performance Monitoring for Mathematics at Intellectual Schools. 
                                   Unpublished manuscript. ISBN 978-601-328-598-6. https://www.researchgate.net/publication/335160752"),
                                   p(""),
                                   p("Issayeva, L., Temirtassov, D., Tursynova, L., &amp; Mozhayeva, O. (2019). Guidelines for using 
                                   reports upon Student Performance Monitoring at Intellectual Schools in teaching practice. 
                                   Unpublished manuscript. ISBN 978-601-328-597-9. https://www.researchgate.net/publication/335161015"),
                                   p(""),
                                   p("Mozhayeva, O., Tursynova, L., Temirtassov, D., &amp; Issayeva, L. (2019). Conceptual framework 
                                   of a Student Performance Monitoring system at Intellectual Schools. Unpublished manuscript. 
                                   ISBN 978-601-328-596-2. https://www.researchgate.net/publication/335160822"),
                                   p(""),
                                   p("Mozhayeva, O., Tursynova, L., Temirtassov, D., Issayeva, L., &amp; Bissenov, Y. (2019). Instructions 
                                   for organizing and conducting a Student Performance Monitoring for Mathematics at Intellectual Schools. 
                                   Unpublished manuscript. ISBN 978-601-328-599-3. https://www.researchgate.net/publication/335160711"),
                                   p("")
                                 ) # End wellPanel
                          )           # End column 11 units wide
                        )             # End Chief Architect fluid row
               )                    # End Contributing Psychometrician tabset panel
               ),                           # End embedded tabset panel
   ),                                       # End main Team tabset

# Highlights -------------------------------------------------------------------            
            tabPanel("Highlights",
                     fluid = TRUE, theme = shinytheme("cosmo"),               # css means cascading style sheets, describing how html elements are displayed on screen
                     tags$style(type="text/css",
                                "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                     ),
                     shinyjs::useShinyjs(),                                                         # activate javascript in the application
                     tags$h1(paste0("Highlights"),
                             tags$img(src = "hex5.png", height = 149, width = 135, 
                                      style = "float:right;margin-top:-18.5px;"
                             ),
                             tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 60px;
                                                              color: #FFFFFF;}"
                             )
                             )
                     ),
                     tags$h2("Squad Achievements",                   # h2 header is the subtitle underneath the h1 header
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
                                h4("Achievements so Far..."),
                                p("This page is dedicated to providing information about the achievements of 
                                   the autopsych team, research, and related activities, hitherto."),
                                p("")
                                       ),
                              wellPanel(
                                h4("Endorsement of autopsych Shiny app by XXXXXXX"),
                                p("The onliine Shiny app was used and endorsed by XXXXX, with the following endorsement made by XXXX XXXXX"),
                                p(""),
                                p("Previously, .......")
                                       ),
                              wellPanel(h4("Launch of autopsych_0.1.0 on CRAN"),
                                p("The autopsych_0.1.0 build version was launched on CRAN, the Comprehensive R Archive Network on XX January,
                                  202X, making the latest build of the app available to users on their local machines."),
                                p(""),
                                p("[Release Date: 21 November, 2020]"),
                                p(""),
                                p("Contributors: Drs Matthew Courtney, Kevin Chang, Eric 
                                  'Bing' Mei, Kane Meissel, Luke Rowe, & Laila Issayeva")
                                       )
                             )       # column
                            )        # fluidRow
               ),                    # end of tabPanel

# Contact -------------------------------------------------------------------                
            tabPanel("Contact", 
                     fluid = TRUE, theme = shinytheme("cosmo"),               # css means cascading style sheets, describing how html elements are displayed on screen
                     tags$style(type="text/css",
                                "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                                "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                                " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
                     ),
                     shinyjs::useShinyjs(),                                                         # activate javascript in the application
                     tags$h1(paste0("Contact"),
                             tags$img(src = "hex5.png", height = 149, width = 135, 
                                      style = "float:right;margin-top:-18.5px;"
                             ),
                             tags$style(HTML("h1{font-family: 'Open Sans'; font-weight: 500;
                                                              line-height: 1.1; font-size: 60px;
                                                              color: #FFFFFF;}"
                             )
                             )
                     ),
                     tags$h2("To Contact autopsych...",                   # h2 header is the subtitle underneath the h1 header
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
                                h4("Enquiries:"),
                                p("For enquiries, contact:"),
                                p(""),
                                p("info@autopsych.com"),
                                p("")
                                       )
                              )       # column
                             )        # fluidRow
                     )                # Contact tabPanel end
                  )                   # tabsetPanel
               )                      # fluidPage

# Server file starts here -------------------------------------------------- 
server <- function(input, output, session){
                                                                                

  
# ANOVA server -------------------------------------------------------------------            
  
  anova_Server("anova") # ANOVA module

# Download server -------------------------------------------------------------------            

  download_Server("MML", zip_name = "pyschometric_analysis_MML.zip", type = "MML") # MML
    
  download_Server("FACETS", zip_name = "pyschometric_analysis_FACETS.zip", type = "FACETS") # FACETS

  download_Server("EQUATE", zip_name = "pyschometric_analysis_EQUATE.zip", type = "EQUATE") # EQUATE

}  # server function wrapper

shinyApp(ui = ui, server = server)




