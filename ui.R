

# # Ensure all necessary packages installed on machine (tidyverse covers ggplot2 and dplyr)
# autopsych_pack <- c("ggrepel", "plyr", "scales", "tidyverse", "ShinyItemAnalysis",
#                     "CTT", "psychometric", "irr", "TAM", "cowplot", "openxlsx", "reshape2",
#                     "Hmisc",  "NCmisc" ,"xtable", "knitr", "rmarkdown", "kableExtra",
#                     "english", "shiny", "shinyjs", "shinythemes", "shinyBS", "bsplus",
#                     "shinyWidgets", "janitor", "magrittr", "emmeans", "s20x")
# packages_required <- autopsych_pack[!autopsych_pack %in% installed.packages()]
# 
# 
# if(length(packages_required) > 0) {
#   install.packages(packages_required)
# }
# 
# if(packageVersion("shiny") < "1.5.0") {
#   install.packages("shiny")
# }
# 
# # Ensure all necessary packages loaded loaded to library (some use package::function throughout script so do not need to be loaded)
# packages_to_load <- c("scales", "shinythemes",  "shinyBS", "bsplus", "shinyWidgets",
#                       "magrittr", "ggplot2", "shiny")
# 
# # Ensure all necessary packages loaded to library
# lapply(packages_to_load, library, character.only = TRUE)
# # For list of packages, see associated publication.

version_number <- "1.0.0"


# This line of code will set the maximum allowed file size of uploads (3MB), adjustable to computational power.
# options(shiny.maxRequestSize = 3*1024^2)
options(shiny.maxRequestSize=600*1024^2)

# UI file starts here -------------------------------------------------- 

shinyUI(
    fluidPage(

      theme = shinytheme("cosmo"),
      tags$head(tags$style(HTML("a {color: #000000}"))),
      tags$style(type = "text/css",
                 "@import url('//fonts.googleapis.com/css?family=Open+Sans|Cabin:400,700');",
                 "label {font-size: 10px;}", ".recalculating {opacity: 1.0;}",
                 " * {font-family: Open Sans; font-weight: 500; line-height: 1.1}",
                 " h4 {font-family: Open Sans; font-weight: 500; line-height: 1.1}"
      ),
      tabsetPanel(

# Title -------------------------------------------------------------------
tabPanel("Home", fluid = TRUE, 
      #   setBackgroundImage(src = "shiny_background8.png"),
      #   setBackgroundColor(color = "black"),
         shinyjs::useShinyjs(),
         tags$h1("Automated Psychometrics",
                 tags$img(src = "hex5.png", height = 149, width = 135, 
                          style = "float:right;margin-top:-18.5px;"
                 ),
                 tags$style(HTML(h1_css))
         ),
         tags$h2("Toward Valid Assessment and Educational Research",                    # h2 header is the subtitle underneath the h1 header
                 tags$style(HTML(h2_css))
         ),
         hr(),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/_IL7ybAyX6k" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
         fluidRow(
           column(11, 
                  wellPanel(
                    h4("Introduction"),
                    p("Welcome to Automated Psychometrics, a novel website that allows teachers, 
                      school and university assessment experts, test developers, and researchers to:"),
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
                    web-design, teaching pedagogy, and online learning. The team collaborates from different 
                    parts of the Asia-Pacific region based on a common interest--to provide equitable access 
                    to high quality educational assessment and research in the developed and 
                    developing world."),
                    # img(src='wbphoto1.png', align = "center", width="900", height="550"),
                    # p(""),
                    # p(a("Students taking year end exams",
                    #           href = "https://search.creativecommons.org/photos/0897debb-d745-41ed-8313-75e092d3653f"),
                    #  "by",
                    #   a("World Bank Photo Collection",
                    #         href = "https://search.creativecommons.org/photos/0897debb-d745-41ed-8313-75e092d3653f"),
                    #  "is licensed under ",
                    #  a("CC-BY-NC-ND 2.0.",
                    #     href = "https://creativecommons.org/licenses/by-nc-nd/2.0/?ref=ccsearch&atype=rich"))
                           ),   # WellPanel
                  wellPanel(
                    h4("Use of autopsych App"),
                    p("The website and all functionality was built using the open-source R programming 
                    language and received no external funding. The autopsych app is a free software 
                    and you can redistribute it and or modify it under the terms of the GNU GPL 3. 
                    In your work, cite as:"),
                    p(""),
                    p("Courtney, M. G. R., Chang, K., Mei, E., Meissel, K., Rowe, L., &  Issayeva, L. B. (2021). 
                    autopsych: a novel shiny app for the psychometric analysis and scoring of assessment and survey
                    data. PLOS ONE. doi:10.1371/journal.pone.0257682"),
                    p("")
                  ),   # WelPanel
                  wellPanel(
                    h4("Distribution"),
                    p("In accordance with the GNU General Public License,",
                      a("GPL-3.0:",
                        href = "https://www.gnu.org/licenses/gpl-3.0.en.html"),
                     ),
                    p(""),
                    p("'If you distribute copies of such a program, whether gratis or for a fee, you must pass on to the recipients the 
                    same freedoms that you received. You must make sure that they, too, receive or can get the source code. And you 
                    must show them these terms so they know their rights'", a("GPL-3.0", href = "https://www.gnu.org/licenses/gpl-3.0.en.html")
                      ),
                    p(""),
                    p("'The intention is to enshrine the rights of users to share and build on ideas falls into the philospohical 
                      concept of common heritage (i.e., standing on the shoulders of giants). It is a bridging mechanism that 
                      encourages growth from social knowledge'", a("(Copyleft).", 
                                                                   href = "https://snyk.io/learn/what-is-copyleft-license/")
                    ),
                    p("")
                  ),   # WelPanel
                  wellPanel(
                    h4("Attribution for the autopsych Hex Sticker"),
                    p(""),
                    p("The autopsych hex sticker (top-right) was made with the assistance of the R Shiny", 
                      a("hexmake", 
                        href = "https://connect.thinkr.fr/hexmake/"),
                      "app."),
                    p(""),
                    p("The Greek", 
                      a("'Psi'",
                        href = "https://commons.wikimedia.org/wiki/File:Psi2.svg"),
                      "used in the hex symbol was created by",
                      a("Gdh",
                        href = "https://is.wikipedia.org/wiki/Notandi:Gdh?rdfrom=commons:User:Gdh~commonswiki"),
                      "licensed under",
                      a("CC-BY 4.0",
                        href = "https://creativecommons.org/licenses/")
                     )
                           )  # wellPanel
           ),        # Column
         )           # fluidRow

      
      
      
      
),                    # Home

# Uni-Dim Rasch (MML) -----------------------------------------------------
tabPanel("Uni-Dim Rasch (MML)", 
         fluid = TRUE,                                 # css means cascading style sheets, describing how html elements are displayed on screen
         shinyjs::useShinyjs(),                                                          # activate javascript in the application
         tags$h1("Uni-Dimensional Rasch Analysis",
                 tags$img(src = "hex5.png", height = 149, width = 135, 
                          style = "float:right;margin-top:-18.5px;"
                 ),
                  tags$style(HTML(h1_css))
         ),
         tags$h2("Toward Valid Assessments and Developmental Rubrics",                   # h2 header is the subtitle underneath the h1 header
                tags$style(HTML(h2_css))
         ),
         hr(), 
         tags$h3("Architect:",                                                            # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:matthew.courtney@nu.edu.kz?Subject=Shiny%20Help",
                        "Dr Matthew Courtney (PhD)"
                       ),
                 tags$style(HTML(h3_css))
                ),
         tags$h3("Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                        "Dr Bing Mei (PhD)"),
                 tags$style(HTML(h3_css))
                ),
         tags$h3("Contributing Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:issayeva_l@cpi.nis.edu.kz?Subject=Shiny%20Help",
                        "Ms Laila Issayeva (M.Sc)"),
                 tags$style(HTML(h3_css))
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
         fluid = TRUE,                              # css means cascading style sheets, describing how html elements are displayed on screen
         shinyjs::useShinyjs(),                                                          # activate javascript in the application
         tags$h1("Many-Facets Rasch Analysis",
                 tags$img(src = "hex5.png", height = 149, width = 135,
                          style = "float:right;margin-top:-18.5px;"
                         ),
                 tags$style(HTML(h1_css))
                ),
         tags$h2("Toward Unbiased Test Questions and Developmental Criterion",                   # h2 header is the subtitle underneath the h1 header
               tags$style(HTML(h2_css))
                ),
         hr(),
         tags$h3("Architect:",                                                            # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:matty_courtney@hotmail.com?Subject=Shiny%20Help",
                        "Dr Matthew Courtney (PhD)"
                        ),
                tags$style(HTML(h3_css))
                ),
         tags$h3("Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                        "Dr Bing Mei (PhD)"),
                 tags$style(HTML(h3_css))
                ),
         tags$h3("Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:issayeva_l@cpi.nis.edu.kz?Subject=Shiny%20Help",
                        "Ms Laila Issayeva (M.Sc)"),
                 tags$style(HTML(h3_css))
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
                              shinyjs::useShinyjs(),                                                         # activate javascript in the application
                              tags$h1("Fixed Anchor Test Equating",
                                      tags$img(src = "hex5.png", 
                                               height = 149, 
                                               width = 135, 
                                               style = "float:right;margin-top:-18.5px;"
                                      ),
                                      tags$style(HTML(h1_css))
                              ),
                              tags$h2("Toward Valid and Comparable Test Forms",                             # h2 header is the subtitle underneath the h1 header
                                     tags$style(HTML(h2_css))
                              ),
                              hr(), 
                              tags$h3("Architect:",                                                            # h2 header is the subtitle underneath the h1 header
                                      tags$a(href = "mailto:matty_courtney@hotmail.com?Subject=Shiny%20Help",
                                             "Dr Matthew Courtney (PhD)"
                                      ),
                                      tags$style(HTML(h3_css))
                              ),
                              tags$h3("Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                                      tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                                             "Dr Bing Mei (PhD)"),
                                      tags$style(HTML(h3_css))
                              ),
                              tags$h3("Psychometrician:",                                                     # h2 header is the subtitle underneath the h1 header
                                      tags$a(href = "mailto:issayeva_l@cpi.nis.edu.kz?Subject=Shiny%20Help",
                                             "Ms Laila Issayeva (M.Sc)"),
                                      tags$style(HTML(h3_css))
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
                  shinyjs::useShinyjs(),                                                         # activate javascript in the application
                  tags$h1("Concurrent Test Equating",
                          tags$img(src = "hex5.png", 
                                   height = 149, 
                                   width = 135, 
                                   style = "float:right;margin-top:-18.5px;"
                          ),
                          tags$style(HTML(h1_css))
                  ),
                  tags$h2("Toward Valid and Comparable Test Forms",                   # h2 header is the subtitle underneath the h1 header
                         tags$style(HTML(h2_css))
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
         fluid = TRUE, 
         shinyjs::useShinyjs(),                                                         # activate javascript in the application
         tags$h1("One-Way ANOVA Analysis",
                 tags$img(src = "hex5.png", height = 149, width = 135, 
                          style = "float:right;margin-top:-18.5px;"
                 ),
                 tags$style(HTML(h1_css))
         ),
         tags$h2("Toward Valid Examinations of Group Differences",                      # h2 header is the subtitle underneath the h1 header
                 tags$style(HTML(h2_css))
         ),
         hr(),  
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/iXhcokykmm4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
         tags$h3("Architect:",                                                          # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:kcha193@aucklanduni.ac.nz?Subject=Shiny%20Help",
                        "Dr Kevin Chang (PhD)"
                 ),
                 tags$style(HTML(h3_css))
         ),
         tags$h3("Psychometrician:",                                                    # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:matty_courtney@hotmail.com?Subject=Shiny%20Help",
                        "Dr Matthew Courtney (PhD)"
                 ),
                 tags$style(HTML(h3_css))
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
         fluid = TRUE, 
         shinyjs::useShinyjs(),                                                         # activate javascript in the application
         tags$h1("Inter-Rater Reliability Analysis",
                 tags$img(src = "hex5.png", height = 149, width = 135, 
                          style = "float:right;margin-top:-18.5px;"
                 ),
                 tags$style(HTML(h1_css))
         ),
         tags$h2("Toward Valid Assessment and Developmental Rubrics",                  # h2 header is the subtitle underneath the h1 header
                 tags$style(HTML(h2_css))
         ),
         hr(), 
         tags$h3("Architect:",                                                          # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:b.mei@auckland.ac.nz?Subject=Shiny%20Help",
                        "Dr Bing Mei (PhD)"
                 ),
                 tags$style(HTML(h3_css))
         ),
         tags$h3("Psychometrician:",                                                    # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:matthew.courtney@nu.edu.kz?Subject=Shiny%20Help",
                        "Dr Matthew Courtney (PhD)"),
                 tags$style(HTML(h3_css))
         ),
         tags$h3("Contributing Psychometrician:",                               # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:k.meissel@auckland.ac.nz?Subject=Shiny%20Help",
                        "Dr Kane Meissel (PhD)"),
                 tags$style(HTML(h3_css))
         ),
         tags$h3("Contributing Psychometrician:",                               # h2 header is the subtitle underneath the h1 header
                 tags$a(href = "mailto:luke.rowe@acu.edu.au?Subject=Shiny%20Help",
                        "Dr Luke Rowe (PhD)"
                 ),
         tags$style(HTML(h3_css))
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
         fluid = TRUE, 
         shinyjs::useShinyjs(),                                                         # activate javascript in the application
         tags$h1(paste0("Current Version: autopsych_", version_number),
                 tags$img(src = "hex5.png", height = 149, width = 135, 
                          style = "float:right;margin-top:-18.5px;"
                 ),
                 tags$style(HTML(h1_css))
         ),
         tags$h2("Toward Valid Assessment and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                 tags$style(HTML(h2_css))
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
                    h4("autopsych_1.0.0"),
                    p("The autopsych_1.0.0 build version includes five main functionalities 
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
                                  'Bing' Mei, Kane Meissel, Luke Rowe, & Ms Laila Issayeva.")
                  )
           )       # column
         )        # fluidRow
),               # autopsych Versions tabPanel


# Team -------------------------------------------------------------------------            
tabPanel("Team",
         tabsetPanel(tabPanel("Lead Architect", 
                              fluidRow(column(width = 6)),
                              shinyjs::useShinyjs(),                                                         # activate javascript in the application
                              tags$h1("Automated Psychometrics",
                                      tags$img(src = "hex5.png", 
                                               height = 149, 
                                               width = 135, 
                                               style = "float:right;margin-top:-18.5px;"
                                      ),
                                      tags$style(HTML(h1_css))
                              ),
                              tags$h2("Toward Valid Assessment and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                                     tags$style(HTML(h2_css))
                              ),
                              hr(), 
                              fluidRow(img(src='matt.png', 
                                           align = "center", width="340", 
                                           height="300", style = 'text-align:right;'),
                                column(11,
                                       wellPanel(
                                         h4("Lead Architect:"),
                                         p(""),
                                         p("Dr Matthew Gordon Ray Courtney (PhD)"),
                                         p(""),
                                         p("The Lead Architect, Dr Courtney, is the founder of the website and provides expertise in educational assessment, 
                                         psychometrics, quantitative research methods, automation, R Shiny UI development, and related research and 
                                           communication.")
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
                                         p("Dr Courtney is an educational assessment expert, psychometrician, and R Shiny statistical software developer 
                                   from New Zealand. He has expertise in both classical test theory (CTT) and item-response theory (IRT), automated statistical 
                                   analysis and reporting, R Shiny UI development, and online learning. 
                                   Dr Courtney completed his PhD in Education from The University of Auckland in 2015. His PhD supervisors 
                                   included Professor Gavin Brown and Laureate Professor John Hattie. For his doctorate he made use of advanced quantitative 
                                   methods to identify the drivers of key learning outcomes of international university students 
                                   in both Australia and New Zealand, and remains commited to advancing educational assessment and research in 
                                   the Asia-Pacific region."),
                                         p(""),
                                         p("After completing his PhD, he spent two years 
                                    as a Post-Doctoral Research Fellow in the Quantitative Data Analysis and Research Unit (Quant-DARE) at 
                                    The University of Auckland. During his time there, Dr Courtney contributed to multiple research projects 
                                    and academic journal articles and provided consultation to staff and post-graduate students. Thereafter, 
                                    Dr Courtney worked for over three years as a Research Fellow at the Assessment Research Centre, Graduate 
                                    School of Education, The University of Melbourne. There Dr Courtney contributed to multiple state, federal, 
                                    and international projects which focused on student assessment and educational measurement. At the Graduate 
                                    School of Education, Dr Courtney completed extensive training in the fields of classical test theory, 
                                    item-response theory, statistical programming, and automated reporting under the tutelage of 
                                    world-renowned statistician, teacher, and software developer, Professor Margaret Wu (creator, ACER ConQuest)."),
                                         p(""),
                                         p("Currently, Dr Courtney works as an Assistant Professor at the Nazarbayev University Graduate 
                                    School of Education, a research intensive university in Kazakhstan, Central Asia. He has international experience providing 
                                    staff training and technical leadership for large-scale student assessment research projects in developing nations, such as Afghanistan. 
                                    Dr Courtney leads the autopsych team to build open-source software architecture that (1) provides insights into the quality of 
                                    student assessments, (2) supports the formative use of assessment, and (3) offers an analysis of student demographic 
                                    and group conditions on student performance. He views the professional administration of valid student assessments 
                                    as essential to informing system-level educational policy and practice. Dr Courtney and his team build sustainable 
                                    (non-foreign reliant) local research capacity as a means to effect change. In terms of teaching, Dr Courtney 
                                    lectures masters and PhD students on topics related to educational assessment, quantitative research designs, 
                                    classical test theory, item response theory, structural equation modelling, latent growth modelling, and statistical 
                                    programming to measure, track, model, and understand student learning. A list of Dr Courtney's published journal articles, 
                                    R statistical packages, encyclopedia chapters, and selected commissioned work is provided below:"),
                                       ),
                                       wellPanel(
                                         h4("ACADEMIC JOURNAL ARTICLES:"),
                                         h4("R Statistical Packages and R Shiny Applications"),
                                         p("1.	Courtney, M. G. R., & Chang, K. (2018). Dealing with non-normality: An introduction and step-by-step 
                                   guide using R. Teaching Statistics, 40(2), 51-59. doi: https://doi.org/10.1111/test.12154 (IF: 13; Q2)"),
                                         p(""),
                                         p("R package details: https://cran.r-project.org/web/packages/normalr/normalr.pdf"),
                                         p(""),
                                         p("Online app: https://kcha193.shinyapps.io/normalr/"),
                                         p(""),
                                         h4("Educational Measurement and Assessment"),
                                         p(""),
                                         p("2.	Hernández-Torrano, D., & Courtney, M. G. R. (2021). Modern international large-scale assessment in 
                                           education: An integrative review and mapping of the literature. Large-Scale Assessment in Education. 
                                           doi: 10.1186/s40536-021-00109-1 (H = 12, Q1)"),
                                         p(""),
                                         p("3. Courtney, M. G. R. (2013). Determining the number of factors to retain in EFA: Using the SPSS R-Menu 
                                   v-2.0 to make more judicious estimations. Practical Assessment, Research & Evaluation, 18(8). 
                                           http://pareonline.net/pdf/v18n8.pdf (H = 52; Q2)"),
                                         p(""),
                                         p("4. Panadero, E., Brown, G. T., & Courtney, M. 
                                    G. R. (2014). Teachers’ reasons for using self-assessment: A survey self-report of Spanish teachers. Assessment 
                                    in Education: Principles, Policy & Practice, 21(4), 365-383. doi: 10.1080/0969594X.2014.919247 (H = 39; 
                                    Q1)"),
                                         p(""),
                                         p("5. Langdon, F., Alexander, P. A., Tesar, M., 
                                    Courtney, M. G. R., & Palmer, M. (2016). Induction and mentoring in early childhood educational organizations: 
                                    embracing the complexity of teacher learning in contexts. Teaching and Teacher Education, 57, 150-160. doi: 
                                    10.1016/j.tate.2016.03.016 (H = 114; Q1)"),
                                         p(""),
                                         h4("Higher Education & Collaborative Problem-Solving"),
                                         p(""),
                                         p("6. Costley, J., Courtney, M. G. R., Fanguy, M. (in press). Online collaborative note-taking behaviors, note 
                                           completeness, and course performance for a 10-week writing program. The Internet and Higher Education. 
                                           doi:10.1016/j.iheduc.2021.100831 (IF: 90; Q1)"),
                                         p(""),
                                         p("7. Qanay, G., Courtney, M. G. R., & Nam, A. (2021). Supporting teacher leadership development in schools 
                                     in Kazakhstan: a mixed-methods study. International Journal of Leadership Education. doi: 
                                     10.1080/13603124.2020.1869314 (IF: 34; Q1)"),
                                         p(""),
                                         p("8.	Kitchen, M., Jeurissen, M., Gray, S., & Courtney, M. G. R. (2017). Teacher engagement with academic 
                                     reading in a post-service TESOL course. Indonesian Journal of Applied Linguistics, 6(2), 260-270. doi: 
                                     10.17509/ijal.v6i2 (H = 9, Q2)"),
                                         p(""),
                                         p("9. Courtney, M. G. R. (2018). Emerging Academic and Social Spaces: Toward a Modern Understanding of 
                                   International Student Integration in Australasia. International Journal of Cyber Behavior, Psychology, 
                                   and Learning, 8(3), 36-47. doi: 10.4018/IJCBPL.2018070104 (H = 14; Q4)"),
                                         p(""),
                                         p("10. Lee-Morgan, J., Courtney, M. G. R., & Muller, M. (2019). New Zealand Māori-Medium Teacher Education: 
                                   An Examination of Students' Academic Confidence and Preparedness. Asia-Pacific Journal of Teacher Education. 
                                   doi: 10.1080/1359866X.2018.1539214 (IF: 32; Q1)"),
                                         p(""),
                                         p("11. Lee, K., Courtney, M. G. R., McGlashan, A., Neveldsen, P., Toso, M. (2019). Initial teacher education 
                                   students’ perceptions of technology and technology education in New Zealand. International Journal of 
                                   Technology and Design Education. doi: 10.1007/s10798-019-09516-6 (IF: 37; Q1)"),
                                         p(""),
                                         h4("Youth Program Evaluation"),
                                         p("12.	Chapman, C. M., Deane, K. L., Harré, N., Courtney, M. G. R., & Moore, J. (2017). Engagement and mentor 
                                   support as drivers of social development in the Project K youth development program. Journal of Youth and 
                                   Adolescence, 1-12. doi: 10.1007/s10964-017-0640-5 (H = 110; Q1)"                                        ),
                                         p(""),
                                         p("13.	Deane, K., Harré, N., Moore, J., & Courtney, M. G. R. (2016). The impact of the Project K Youth Development 
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
                                       ) ,
                                       wellPanel(
                                         h4("SELECTED COMMISSIONED WORK:"),
                                         p("1. Wilson, M., Wolfe, R., & Courtney, M. G. R. (Nov, 2018). Technical Review of NAPLAN Online. Federal Report 
                                           Commissioned by the Australian Education Senior Officials Committee (AESOC)."),
                                         p(""),
                                         p("2. McGaw, B., Luo, R., Collins, M., Courtney, M. G. R., & Nguyen, C. (2017). Evaluation of the External 
                                           Assessment Trials for Queensland Curriculum and Assessment Authority. Milestone reports 3 to 5: Analysis 
                                           of Test Data and Reliability."),
                                         p(""),
                                         p("3. Kushner, S., Cochise, A., Courtney, M. G. R., Sinnema, C., & Brown, G. (2016). International 
                                           Baccalaureate Primary Years Programme in Aotearoa New Zealand: A case-study in whole-school innovation. 
                                           Bethesda, MD: International Baccalaureate Organization.")
                                       )    # End wellPanel
                                )           # End column 11 units wide
                              )             # End Chief Architect fluid row
         ),                   # End Chief Architect inner tab panel
         
         tabPanel("Lead Developer", 
                  fluidRow(column(width = 6)),
                  shinyjs::useShinyjs(),                                                         # activate javascript in the application
                  tags$h1("Automated Psychometrics",
                          tags$img(src = "hex5.png", 
                                   height = 149, 
                                   width = 135, 
                                   style = "float:right;margin-top:-18.5px;"
                          ),
                          tags$style(HTML(h1_css))
                  ),
                  tags$h2("Toward Valid Assessment and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                         tags$style(HTML(h2_css))
                  ),
                  hr(), 
                  fluidRow(img(src='kevin.png', 
                               align = "center", width="320", 
                               height="300", style = 'text-align:right;'),
                    column(11,
                           wellPanel(
                             h4("Lead Developer:"),
                             p(""),
                             p("Dr Kevin Chang (PhD)"),
                             p(""),
                             p("As Lead Developer, Dr Chang provides expertise in statistics, experimental methods, reactive programming, 
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
                             p("Dr Chang is an experienced statistician with a demonstrated history working in the scientific research industry. He is highly 
                                     skilled in quantitative research methods, psychometrics, experimental design, and statistical programming."),
                             p(""),
                             p("For his PhD, Dr Chang developed a method for optimally designing experiments 
                                     which involve two phases, the second phases to be needed for the observations to be made, such as in proteomics 
                                     experiments. Using a combination of theory and computing, his methods quickly find data collection protocols 
                                     which minimize the resources required to conduct such experiments while maximizing the information that can 
                                     be drawn from them."),
                             p(""),
                             p("After completing his PhD, he spent several years as a quantitative specialist at The University of Auckland and 
                               contributed to multiple research projects and papers relating to child wellbeing and youth outcomes, psychometrics, 
                               R statististical packages, and biology and medicine. A list of Dr Chang's journal articles are provided below:"
                             ),
                           ),
                           wellPanel(
                             h4("ACADEMIC JOURNAL ARTICLES AND STATISTICAL PACKAGES:"),
                             p(""),
                             h4("Child and Youth Health and Wellbeing"),
                             p(""),
                             p("1. Shackleton, N., Chang, K., Lay-Yee, R. et al. (2019). Microsimulation model of child and adolescent overweight: 
                                     making use of what we already know. International Journal of Obesity, 43, 2322–2332. doi: 10.1038/s41366-019-0426-9 
                                     (H = 218; Q1)"),
                             p(""),
                             p("2. Zhao, J., Mackay, L., Chang, K., Mavoa, S., Stewart, T., Ikeda, E., Donnellan, N., & Smith, M. (2019). Visualising 
                                   combined time use patterns of children’s activities and their association with weight status and neighbourhood context. 
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
                             p("R package details: https://cran.r-project.org/web/packages/infoDecompuTE/index.html"),
                             p(""),
                             h4("Biology and Medicine"),
                             p(""),
                             p("7. Hoggard, M., Jacob, B., Wheeler, D., Zoing, M., Chang, K., Biswas, K., Middleditch, M., 
                               Douglas, R. G., & Taylor, M. W. (2020). Multiomic analysis identifies natural intrapatient temporal variability 
                               and changes in response to systemic corticosteroid therapy in chronic rhinosinusitis. Immunity, Inflamation and 
                               Disease, 1-18. doi.10.1002/iid3.349"),
                             p(""),
                             p("8. Zha, H. Liu, F., Ling, Z., Chang, K., Yang, J., & Li, L. (2021). Multiple bacteria associated with 
                               the more dysbiotic genitourinary microbiomes in patients with type 2 diabetes mellitus. Scientific Reports, 
                               1824. doi.s41598-021-81507-x"),
                             p(""),
                             p("9. Zha, H., Fang, Q-D., van der Reis, A., Chang, K., Yang, L-Y., Xie, J-J., Shi, D., Xu, Q-M., Li, Y-T., &
                             Li, L-J. (2020). Vital members in the gut microbiotas altered by two probiotic Bifidobacterium strains against liver 
                               damage in ratsBMC Microbiology, 20(144), 1-12. doi.10.1186/s12866-020-01827-2"),
                             p(""),
                             p("10. Zha, H., Lu, H., Wu, J., Chang, K., Wang, Q., Zhang, H., Li, J., Luo, Q., Lu, Y., & Li, L. (2020).
                               Vital Members in the More Dysbiotic Oropharyngeal Microbiotas in H7N9-Infected Patients. Frontiers in Medicince, 7(396), 1-16. 
                               doi.10.3389/fmed.2020.00396"),
                             p(""),
                             p("11. Zha, H., Chen, Y., Wu, J., Chang, K., Lu, Y., Zhang, H., Xie, J., Wang, Q., Tang, R., & Li, L. (2020). 
                             Characteristics of three microbial colonization states in the duodenum of the cirrhotic patients. Future Microbiology, 15(10). 
                               doi.10.2217/fmb-2019-0270"),
                             p(""),
                             p("12. Mackenzie, B. W., Chang, K., Zoing, M., Jain, R., Hoggard, M., Biswas, K., Douglas, R. G., & Taylor, M. W. (2019). 
                             Longitudinal study of the bacterial and fungal microbiota in the human sinuses reveals seasonal and annual changes in diversity. 
                             Scientific Reports, 9(17416). doi.s41598-019-53975-9"),
                             p(""),
                             p("13. Sutherland, K., Clatworthy, M., Chang, K., Rahardja, S., & Young, S. W. (2019). Risk Factors for Revision Anterior 
                             Cruciate Ligament Reconstruction and Frequency With Which Patients Change Surgeons. Orthopaedic Journal of Sports Medicine, 
                             7(11), 1-8. doi.10.1177/2325967119880487"),
                             p(""),
                             p("14. Sutherland, K., Clathworthy, M., Fulcher, M., Chang, K., & Young, S. W. (2019). Marked increase in the incidence of 
                               anterior cruciate ligament reconstructions in young females in New Zealand. ANZ Journal of Surgery, 89, 1151-1155. 
                               doi.10.1111/ans.15404"),
                             p(""),
                             p("15. Zha, H., Lewis, G., Waite, D. W., Wu, J., Chang, K., Dong, Y., & Jeffs, A., (2019). Bacterial communities associated 
                               with tail fan necrosis in spiny lobster, Jasus edwardsii. FEMS Microbiology Ecology, 95(6), 1-9. doi.10.1093/femsec/fiz070"),
                             p(""),
                             p("16. Ng, J., Kaur, H., Collier, T., Chang, K., Brooks, A. E. S., Allison, J. R., Brimble, M. A., Hickey, A., & Birch, N. P. 
                               (2019). Site-specific glycation of Aβ1–42 affects fibril formation and is neurotoxic. Journal of Biological Chemistry, 294(22), 
                               8806-8818. doi.10.1074/jbc.RA118.006846"),
                             p(""),
                             p("17. Biswas, K., Cavubati, R., Gunaratna, S., Hoggard, M., Waldvogal-Thurlow, S., Hong, J., Chang, K., Mackenzie, B. W., Taylor, 
                               M. W., & Douglas, R. G. (2019). Comparison of Subtyping Approaches and the Underlying Drivers of Microbial Signatures for Chronic 
                               Rhinosinusitis. mSphere, 4(1), 1-13. doi.10.1128/mSphere.00679-18"),
                             p(""),
                             p("18. Hoggard, M., Waldvogel-Thurlow, S., Zoing, M., Chang, K., Radcliff, F. J., Mackenzie, B. W., Biswas, K., Douglas, R. G.,
                               & Taylor, M. W. (2018). Frontiers in Immuniology. doi.10.3389/fimmu.2018.02065")
                           ) # End wellPanel
                    )           # End column 11 units wide
                  )             # End Chief Architect fluid row
         ),
         
         tabPanel("Lead Psychometrician", 
                  fluidRow(column(width = 6)),
                   shinyjs::useShinyjs(),                                                         # activate javascript in the application
                  tags$h1("Automated Psychometrics",
                          tags$img(src = "hex5.png", 
                                   height = 149, 
                                   width = 135, 
                                   style = "float:right;margin-top:-18.5px;"
                          ),
                          tags$style(HTML(h1_css))
                  ),
                  tags$h2("Toward Valid Assessment and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                         tags$style(HTML(h2_css))
                  ),
                  hr(), 
                  fluidRow(img(src='eric.png', 
                               align = "center", width="320", 
                               height="300", style = 'text-align:right;'),
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
                                     working as a research fellow at The University of Auckland."),
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
                  shinyjs::useShinyjs(),                                                         # activate javascript in the application
                  tags$h1("Automated Psychometrics",
                          tags$img(src = "hex5.png", 
                                   height = 149, 
                                   width = 135, 
                                   style = "float:right;margin-top:-18.5px;"
                          ),
                           tags$style(HTML(h1_css))
                  ),
                  tags$h2("Toward Valid Assessment and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                         tags$style(HTML(h2_css))
                  ),
                  hr(), 
                  fluidRow(img(src='kane.png', 
                               align = "center", width="320", 
                               height="300", style = 'text-align:right;'),
                    column(11,
                           wellPanel(
                             h4("Contributing Psychometrician:"),
                             p(""),
                             p("Dr Kane Meissel (PhD)"),
                             p(""),
                             p("As a Contributing Psychometrician, Dr Kane Meissel has expertise in educational statistics,
                               especially in the application of multi-level statistical models of longitudinal data as well 
                               as to account for student nesting in classes, schools, and jurisdictions; non-parametric effect sizes; 
                               R programming; and the application of robust statistical methods.")
                           ),
                           wellPanel(
                             h4("Qualifications:"),
                             p(""),
                             p("PhD in Education, The University of Auckland"),
                             p(""),               
                             p("Master of Science (Psychology, 1st class hons), The University of Auckland"),
                             p("")
                           ),
                           wellPanel(
                             h4("Bio:"),
                             p("Dr Kane Meissel is a Senior Lecturer in Educational Psychology in the School of Learning, 
                               Development and Professional Practice, in the Faculty of Education and Social Work. Kane’s 
                               research focuses on the use of advanced quantitative methodologies to identify and reduce 
                               educational disparities, as well as promote equity and social justice for traditionally 
                               marginalised learners. A particular focus of Kane's research is the identification of 
                               specific factors that relate to resilience among under-served learner groups."),
                             p(""),
                             p("Dr Meissel is also the Education Domain lead investigator for Growing Up in New Zealand, 
                               a major, multi-disciplinary longitudinal study following almost 7000 New Zealand children 
                               from before birth through to adulthood. The study participants reflect the contemporary 
                               diversity of Aotearoa New Zealand, and is designed to provide unique, detailed 
                               information about what shapes children and young people's early development, and 
                               aims to improve the lives of all New Zealand’s children by providing insight into 
                               how interventions can be targeted at the earliest opportunity to give every child 
                               the best start in life."),
                             p(""),
                             p("A selection of Dr Meissel's publications is provided below:")
                           ),
                           wellPanel(
                             h4("ACADEMIC JOURNAL ARTICLES:"),
                             p("1. Yao, E. S., Meissel, K., Bullen, P., Carr, P. A., Clark, T. C., & Morton, S. M. B. (2021).
                               Classifying multiple ethnic identifications: Methodological effects on child, adolescent, and 
                               adult ethnic distributions. Demographic Research, 44(21), 481-512. doi.10.4054/DemRes.2021.44.21"),
                             p("2. Rubie-Davies, C., Meissel, K., Alansari, M., Watson, P., Flint, A., & McDonald, L. (2020). 
                             Achievement and beliefs outcomes of students with high and low expectation teachers. Social 
                               Psychology of Education, 23(5), 1173-1201. doi.10.1007/s11218-020-09574-y"),
                             p("3. Meyer, F., Yao, E. S., & Meissel, K. (2020). The summer learning effect in writing in New Zealand. 
                               Reading and Writing, 33(5), 1183-1210. doi.10.1007/s11145-019-10003-6"),
                             p("4. Turnbull, S. M., Meissel, K., Locke, K., & O'Neale, D. R. J. (2020). The Impact 
                               of Science Capital on Self-Concept in Science: A Study of University Students in New Zealand. 
                               Frontiers in Education, 5(27). doi:10.3389/feduc.2020.00027"),
                             p("5. Wang, S., Rubie-Davies, C. M., & Meissel, K. (2020). The stability and trajectories of teacher 
                               expectations: Student achievement level as a moderator. Learning and Individual Differences, 78, 
                               1-10. doi:10.1016/j.lindif.2019.101819"),
                             p("6. Bullen, P, Deane, K. L., Meissel, K., & Bhatnagar, S. (2019). What constitutes globalised evidence? 
                                Cultural tensions and critical reflections of the evidence-based movement in New Zealand. 
                                International Journal of Psychology, 1-10. doi:10.1002/ijop.12574"),
                             p("7. Watson, P. W., Rubie-Davies, C. M., Meissel, K., Peterson, E. R., Flint, A., Garrett, & L., McDonald, 
                                L. (2017). Teacher gender, and expectation of reading achievement in New Zealand elementary school 
                                students: Essentially a barrier? Gender and Education, 1–20. doi:10.1080/09540253.2017.1410108"),
                             p("8. Watson, P. W., Rubie-Davies, C. M., & Meissel, K. (2019). Mathematics Self-Concept in New Zealand 
                                Elementary School Students: Evaluating Age-Related Decline. Frontiers in Psychology, 10, 1-12. 
                                doi:10.3389/fpsyg.2019.02307"),
                             p("9. Wang, S., Rubie-Davies, C. M., & Meissel, K. (2019). Instructional practices and classroom interactions 
                                of high and low expectation teachers in China. Social Psychology of Education. doi:10.1007/s11218-019-09507-4"),
                             p("10. Davies, M. J., Meissel, K. (2018). Secondary Students Use of Dialogical Discussion Practices to Foster Greater Interaction. 
                                NZ J Educ Stud 53, 209–225. doi.10.1007/s40841-018-0119-2"),
                             p("11. Deane, K. L., Meissel, K., Moore, J., & Gillham, B. (2017). Positive youth development profiles of cross-age 
                                peer mentors. Applied Developmental Science, 1–15. doi:10.1080/10888691.2017.1295810"),
                             p("12. Wang, S., Rubie-Davies, C. M., & Meissel, K. (2018). A systematic review of the teacher expectation 
                               literature over the past 30 years. Educational Research and Evaluation, 24(3-5), 124–179. 
                               doi:10.1080/13803611.2018.1548798"),
                             p("13. Davies, M., Kiemer, K., & Meissel, K. (2017). Quality Talk and dialogic teaching-an examination of a 
                             professional development programme on secondary teachers’ facilitation of student talk. British Educational 
                             Research Journal, 1-20. doi:10.1002/berj.3293"),
                             p("")
                           ) # End wellPanel
                    )           # End column 11 units wide
                  )             # End Chief Architect fluid row
         ),
         
         tabPanel("Contributing Psychometrician", 
                  fluidRow(column(width = 6)),
                  shinyjs::useShinyjs(),                                                         # activate javascript in the application
                  tags$h1("Automated Psychometrics",
                          tags$img(src = "hex5.png", 
                                   height = 149, 
                                   width = 135, 
                                   style = "float:right;margin-top:-18.5px;"
                          ),
                          tags$style(HTML(h1_css))
                  ),
                  tags$h2("Toward Valid Assessments and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                         tags$style(HTML(h2_css))
                  ),
                  hr(), 
                  fluidRow(img(src='luke.png', 
                               align = "center", width="320", 
                               height="300", style = 'text-align:right;'),
                    column(11,
                           wellPanel(
                             h4("Contributing Psychometrician"),
                             p(""),
                             p("Dr Luke Rowe (PhD)"),
                             p(""),
                             p("As a Contributing Psychometrician, Dr Luke Rowe has expertise in educational statistics, meta-analysis, online education, and R programming.")
                           ),
                           wellPanel(
                             h4("Qualifications:"),
                             p(""),
                             p("Doctor of Education [2019], The University of Melbourne"),
                             p(""),               
                           ),
                           wellPanel(
                             h4("Bio:"),
                             p("Dr Rowe is an early career academic and proficient quantitative educational researcher and has worked on 
                               a number of projects devoted to measuring and understanding student and group learning, collective 
                               intelligence, and computer supported collaborative learning. Dr Rowe has expertise in applied statistics 
                               for educational contexts (especially meta-analyses), a developing competence in R programming, a keen 
                               interest in building tools to help teachers ensure the inter-rater reliability of rubrics designed to 
                               assess student artefacts (such as essays) and demonstrated performances (e.g., sporting performances)."),
                             p(""),
                             p("For his PhD, Dr Rowe undertook a research project focused on 'collective intelligence' under the supervision of 
                               Professor John Hattie and Associate Professor John Munro at The University of Melbourne, Australia."),
                             p(""),
                             p("Dr Rowe's publications are provided below:"),
                           ),
                           wellPanel(
                             h4("ACADEMIC JOURNAL ARTICLES"),
                             p(""),
                             p("1. Rowe, L. I., Hattie, J., & Hester, R. (2021). G versus c: comparing individual and collective intelligence across two 
                                meta-analyses. Cognitive Research: Principles and Implications, 3-6(1):26. doi:10.1186/s41235-021-00285-2"),
                             p("")
                           ) # End wellPanel
                    )           # End column 11 units wide
                  )             # End Chief Architect fluid row
         )                    # End Contributing Psychometrician tabset panel
         
         
         
         ,
         
         tabPanel("Contributing Psychometrician", 
                  fluidRow(column(width = 6)),
                  shinyjs::useShinyjs(),                                                         # activate javascript in the application
                  tags$h1("Automated Psychometrics",
                          tags$img(src = "hex5.png", 
                                   height = 149, 
                                   width = 135, 
                                   style = "float:right;margin-top:-18.5px;"
                          ),
                          tags$style(HTML(h1_css))
                  ),
                  tags$h2("Toward Valid Assessments and Educational Research",                   # h2 header is the subtitle underneath the h1 header
                         tags$style(HTML(h2_css))
                  ),
                  hr(), 
                  fluidRow(img(src='laila.png', 
                               align = "center", width="320", 
                               height="300", style = 'text-align:right;'),
                    column(11,
                           wellPanel(
                             h4("Contributing Psychometrician"),
                             p(""),
                             p("Ms Laila Baudinovna Issayeva (B.Mathematics, M.Sc)"),
                             p(""),
                             p("As a Contributing Psychometrician, Ms Laila B. Issayeva has a formal background in mathematics 
                             and computer science, expertise in item writing, test development, statistical programming, 
                               and psychometrics.")
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
                                   her teaching time, Ms Issayeva  was instrumental in developing lesson plans, integrated Mathematics 
                                   subject programs collaboratively with Cambridge Assessment International Education (CAIE, 
                                   Cambridge, the UK)."),
                             p(""),
                             p("Ms Issayeva is an Educational Measurement specialist and has expertise in developing 
                                   assessment tools, processing exams, setting standards, analyzing (CTT & IRT), interpreting, and 
                                   presenting data (reports, slides). Ms Issayeva has been overseeing the development and implementation 
                                   of a national Student Performance Monitoring system for Mathematics in cooperation with the 
                                   psychometricians from the Institute for Educational Measurement Cito (Cito, Arnhem, the Netherlands) 
                                   for six years. Now, she spearheads the process of shifting its format from computer-based to 
                                   computerized adaptive testing (CAT)."),
                             p(""),
                             p("Ms Issayeva is actively involved into the process of developing, marking, and grading External Summative 
                                   Assessment Examinations accredited by CAIE. Recently, she has conceptualized and administered 
                                   functional literacy tests for Mathematics, Reading, and Sciences nationwide."), 
                             p(""),
                             p("Within the Masters program, Ms Issayeva completed a qualitative study to explore the extent of 
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
                             h4("MANUSCRIPTS, CONFERENCE PAPERS, AND PRESENTATIONS:"),
                             p(""),
                             h4("Educational Measurement and Assessment"),
                             p(""),
                             p("1. Issayeva, L. B., Temirtassov, D. K., Tursynova, L., & Mozhayeva, O. I. (2019). Guidelines for item 
                               development to conduct Student Performance Monitoring for Mathematics at Intellectual Schools. Unpublished 
                               manuscript. ISBN 978-601-328-598-6. https://www.researchgate.net/publication/335160752"),
                             p(""),
                             p("2. Issayeva, L. B., Temirtassov, D. K., Tursynova, L., & Mozhayeva, O. I. (2019). Guidelines for using 
                               reports upon Student Performance Monitoring at Intellectual Schools in teaching practice. Unpublished 
                               manuscript. ISBN 978-601-328-597-9. https://www.researchgate.net/publication/335161015"),
                             p(""),
                             p("3. Mozhayeva, O. I., Tursynova, L., Temirtassov, D. K., & Issayeva, L. B. (2019). Conceptual framework of 
                               a Student Performance Monitoring system at Intellectual Schools. Unpublished manuscript. ISBN 978-601-328-596-2. 
                               https://www.researchgate.net/publication/335160822"),
                             p(""),
                             p("4. Mozhayeva, O. I., Tursynova, L., Temirtassov, D. K., Issayeva, L. B., & Bissenov, Y. M. (2019). Instructions 
                               for organizing and conducting a Student Performance Monitoring for Mathematics at Intellectual Schools. 
                               Unpublished manuscript. ISBN 978-601-328-599-3. https://www.researchgate.net/publication/335160711"),
                             p(""),
                             p("5. Issayeva L. B., Rakhymbayeva Z. K., & Temirtassov, D. K. (2018, October 25). The unification of a student 
                               ability scale: first results of the psychometric research study [Presentation]. The 10th NIS International 
                               Research-to-Practice Conference “Next Generation Schools”, Astana, Kazakhstan. https://www.researchgate.net/publication/340816106"),
                             p(""),
                             p("6. Issayeva L. B., Dieteren, N. M. A., & Crans, S. E. (2018, October 15). Curriculum sampling as a strategy employed 
                               for a student performance monitoring system for Mathematics at Nazarbayev Intellectual Schools [Conference paper]. 
                               The 44 th International Association for Educational Assessment Conference “Assessment and Big Data”, Oxford, 
                               the UK. https://www.researchgate.net/publication/328293279"),
                             p(""),
                             p("7. Issayeva L. B., Dieteren, N. M. A., & Crans, S. E. (2017, November 9). Assessment tool validation research at 
                               Nazarbayev Intellectual Schools: student performance monitoring system for Mathematics [Conference paper]. 
                               The 18th Association for Educational Assessment in Europe Conference 'Assessment cultures in a globalised world', 
                               Prague, Czech Republic. https://www.researchgate.net/publication/335033501"),
                             p(""),
                             p("8. Issayeva, L. B., Temirtassov, D. K. (2017, October 26). Current approaches to the external assessment of 
                               student achievement in Mathematics at Nazarbayev Intellectual Schools [Presentation]. The 9 th NIS International 
                               Research-to-Practice Conference “Values, Wellbeing and Innovation for the Future of Education”, Astana, 
                               Kazakhstan. https://www.researchgate.net/publication/340931566"),
                             p(""),
                             p("9. Issayeva, L. B., Temirtassov, D. K., Dieteren, N. M. A., Crans, S. E., & Kamphuis, F. (2016, November 5). 
                               Student performance monitoring for Mathematics as an effective instrument to adjust individual learning paths 
                               for students and to enhance didactic tools for teachers at Nazarbayev Intellectual Schools [Conference paper]. 
                               The 17 th Association for Educational Assessment in Europe Conference 'Social and Political underpinnings of 
                               educational assessment: Past, present and future', Limassol, Cyprus. https://www.researchgate.net/publication/340931730"),
                             p("")
                           ) # End wellPanel
                    )           # End column 11 units wide
                  )             # End Chief Architect fluid row
         )                    # End Contributing Psychometrician tabset panel
         ),                           # End embedded tabset panel
),                                       # End main Team tabset

# Highlights -------------------------------------------------------------------            
tabPanel("Highlights",
         fluid = TRUE,
         shinyjs::useShinyjs(),                                                         # activate javascript in the application
         tags$h1(paste0("Highlights"),
                 tags$img(src = "hex5.png", height = 149, width = 135, 
                          style = "float:right;margin-top:-18.5px;"
                 ),
                  tags$style(HTML(h1_css))
         ),
         tags$h2("Squad Goals and Achievements so Far!",                   # h2 header is the subtitle underneath the h1 header
                tags$style(HTML(h2_css))
         ),
         hr(), 
         fluidRow(
           column(11,
                  wellPanel(
                    h4("This page provides information about the goals and current achievements of the autopsych team."),
                    p(""),
                    p("")
                  ),
                  wellPanel(
                    h4("Baseline testing for early reading and mathematics ability of children in Kabul, Afghanistan"),
                    p("The autopsych app was successfully applied to check the validity of piloted early child reading and 
                      numeracy assessments in Kabul, Afghanistan. Dr Courtney carried out local training for lead 
                      assessors and their teams in Kabul in April 2021. The finalized assessment was then successfully administered to 
                      all provinces across Afghanistan providing a baseline understanding of child numeracy and literacy in the region."),
                    p(""),
                    p("Ultimately, the project aims to investigate the effect of remedial academic programs and 
                    international support infrastructure on student learning."),
                    # tags$iframe(width="560", height="315", src="https://www.youtube.com/watch?v=KgNSUbD6eRs",
                    #             frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA),
                    # p(""),
                    # p("In collaboration with local partners, the autopsych team donated 15 whiteboards to a pilot school in the Kabul region."),
                    # img(src='kabul2.png', align = "center", width="900", height="550"),
                    # p("Photo courtesy of autopsych 2021"),
                   p("")
                  ),
                  wellPanel(
                    h4("Monitoring tri-lingual child literacy development in Kazakhstan"),
                    p("The autopsych app was successfully applied to check the validity of and perform fixed 
                    anchor test equating for multiple assessment instruments used to monitor early child literacy in Kazakhstan. 
                    The program involved thousands of students from 30 pilot and control schools. The goal was to 
                    examine the effects of the updated curriculum on student academic outcomes. For this project, 
                    elementary school childrens' language development in Kazakh, Russian, and Uyghur was tracked 
                    from 2015 to 2019."),
                    p(""),
                    p("Using multi-level latent growth modelling, preliminary results suggest positive effects of the 
                    updated curriculum for both urban and rural settings. In collaboration with local 
                    partners, it is expected that the results of the study will be published in an academic 
                    journal sometime soon!"),
                    p("")
                  )
           )       # column
         )        # fluidRow
),                    # end of tabPanel

# Contact -------------------------------------------------------------------                
tabPanel("Contact", 
         fluid = TRUE, 
         shinyjs::useShinyjs(),                                                         # activate javascript in the application
         tags$h1(paste0("Contact"),
                 tags$img(src = "hex5.png", height = 149, width = 135, 
                          style = "float:right;margin-top:-18.5px;"
                 ),
                  tags$style(HTML(h1_css))
         ),
         tags$h2("For enquiries, contact...",                   # h2 header is the subtitle underneath the h1 header
                tags$style(HTML(h2_css))
         ),
         hr(), 
         fluidRow(
           column(11,
                  tags$h3("Lead Architect: Dr Matthew Courtney (PhD)",                                                     # h2 header is the subtitle underneath the h1 header
                          tags$a(href = "mailto:matthew.courtney@nu.edu.kz?Subject=Shiny%20Help",
                                        "matthew.courtney@nu.edu.kz"
                                ), tags$style(HTML(h3_css))
                           )
                  ) # column
                 )  # fluidRow
)                # Contact tabPanel end
      )                   # tabsetPanel
)                      # fluidPage
)
