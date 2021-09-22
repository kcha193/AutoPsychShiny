
# Kevin, would you kindly help us complete this module?

EQUATE_UI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(                                                                             # Kevin, could you kindly assist to transform these wellPanel and Inputs to EQUATE_UI.R module file? 
      h4("Test equating"),                                                                 # I have written notes for ns() with #* below to change.
      p(""),
      p("Test equating is commonly carried out when two (or more) test forms are administered 
         to different groups of students. For example, imagine a 40 item Numeracy test (Form A) 
         is administered to a group of Grade 3 students. At the same time, another 40 item Numeracy 
         test (Form B) is administered to a group of Grade 4 students. In order for both groups of students 
         to receive a fair score on a single scale, the test designers built in some overlap where 10 
         link items (questions) are delivered in both Test Form A and B assessments (with link items 
         generally a little difficult for Form A students, and easy for Form B students). In order to 
         provide all of the students with a fair score on a single unified scale, one needs to carry 
         out test equating."),
      p(""),
      p("Test equating is also carried out when you are tracking student progress across two time periods. 
        Imagine delivering Test Form A at the start of a school year and Test Form B at the conclusion 
        of a school year. Your aim is to provide stakeholders with an understanding of the extent to which 
        each student improved for the given period. In this instance, in order to provide students with 
        a fair score for each time period on a unified scale, one needs to carry out test equating."),
      p(""),
      p("Here, we make one common and flexible form of equating, fixed-anchor equating, automatically accessible.")
    ),
    
    wellPanel(
      h4("Fixed anchor equating tool"),
      p(""),
      p("Fixed anchor equating is useful as it enables test administrators to report scores to 
         students that reflect their respective original scales. In this instance, the ability 
         scores from Form A (student theta estimates) remain unchanged. However, with fixed anchor 
         equating, student ability estimates from Form B are mapped onto the Form A test 
         so that all students' scores can be compared on a single unified scale."),
      p(""),
      p("The fixed-anchor equating tool provided here makes use of separately calibrated data 
         from Forms A and B. The tool simply takes the outputted spreadsheets from each of the 
         respective uni-dimensional Rasch analyses to (a) compare item difficulty estimates across 
         test forms, and (b) undertake the fixed equating procedure placing Form B test takers on 
         the Form A scale.")
    ),
    
    wellPanel(
      h4("1. Prepare your data:"),
      p(""),
      p("(a) Carefully prepare item-response matrices (.csv files) for Test Forms A and B ensuring 
         that the link (common) items are labelled exactly the same (in preparation for the Uni-Dim Rasch (MML) tab)."),
      p(""),
      p("(b) Carry out a uni-dimensional Rasch analysis (Uni-Dim Rasch) on the item-response data from Test 
         Form A and save the outputted spreadsheet as 'Form_A_MML_tables.xlsx'. Carry out the same analysis 
         on the item-response data from Test Form B and save as 'Form_B_MML_tables.xlsx'.")
    ),                    # End 2nd wellPanel
    wellPanel(
      
      h4("2. Upload 'Form_A_MML_tables.xlsx' and 'Form_B_MML_tables.xlsx' files:"),
      fluidRow(column(6,
                      fileInput(ns("datapath_A"),                                           #* change "input_file to" to ns("input_file") in EQUATE_UI.R file                                        
                                "Upload 'Form_A_MML_tables.xlsx':",
                                accept = c(".xlsx",".xls")
                      )
      ),             # end 1st column input
      column(6,
             fileInput(ns("datapath_B"),                                           #* change "input_file to" to ns("input_file") in EQUATE_UI.R file       
                       "Upload 'Form_B_MML_tables.xlsx':", 
                       accept = c(".xlsx",".xls")
             )
      )             # end 2nd column input
      )                   # End internal fluidRow
      
    ),                     # End WellPanel that includes dual inputs      End wellpanel for EQUATE_UI.R  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    wellPanel(
      h4("3. Specify construct and focal group"), 
      p(""),
      textInput(ns("construct"), "Construct:", placeholder = "Test Topic"),                 #* change "construct" to" to ns("construct") in EQUATE_UI.R file   
      bsTooltip(ns("construct"), "E.g., Numeracy or Literacy", "right",                     #* change "construct" to ns("construct) in EQUATE_UI.R file  
                options = list(container = "body")
      ),
      textInput(ns("population"), "Focal group:", placeholder = "Students"),                #* change "population" to" to ns("population") in EQUATE_UI.R file 
      bsTooltip(ns("population"), "E.g., Central School Grade 10 Students", "right",        #* change "population" to" to ns("population") in EQUATE_UI.R file 
                options = list(container = "body")
      )
      
    ),                      # wellPanel 3 end
    
    wellPanel(
      h4("4. Specify settings for Rasch fixed equating procedure"),                                     
      selectizeInput(ns("node.sequence"), "Assumed discretized population profile (for Form B test-takers):",
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
      
      selectizeInput(ns("conv"), "Convergence criterion", choices = c("0.0001",                       #* change "conv" to ns("conv") in EQUATE_UI.R file 
                                                                  "0.001", "0.01", "0.1")
      ) %>%
        shinyInput_label_embed(icon("question-circle") %>%
                                 bs_embed_tooltip(title = "This value represents the acceptable level of tolerance 
                                                      for which the IRT model represent the data; larger values 
                                                      enable faster computation, though 0.0001 is default."
                                 )
        ),
      
      selectizeInput(ns("maxiter"), "Maximum iterations", choices = c(1000, 5000, 20000, 100000)      #* change "maxiter" to ns("maxiter") 
      ) %>%
        shinyInput_label_embed(icon("question-circle") %>%
                                 bs_embed_tooltip(title = "This value represents the maximum number of computational 
                                                      steps permitted for the model to represent the data; lower 
                                                      values enable faster computation, though 1000 is default."
                                 )
        )
    ),
    wellPanel(
      h4("5. Include your own recommendations"),
      p(""),
      
      textAreaInput(ns("recommendations"), "Notes:",                                              #* change "recommendations" to ns("recommendations")
                    placeholder = "There are no notes for this report",
                    height = '150px'),
      bsTooltip("recommendations",                                                            #* change "recommendations" to ns("recommendations")
                "These notes will be reported at the start of the PDF technical 
                                                            report. Make any notes you like about the original data or report 
                                                            itself.",
                "right",
                options = list(container = "body")
      )
    )                      # End 7. wellPanel
  )
}