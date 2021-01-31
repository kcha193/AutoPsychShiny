

download_UI <- function(id){
  ns <- NS(id)
  
  wellPanel(
    downloadButton(ns("report"), "Generate PDF report and spreadsheet") 
    # when conditions met in server logic), renamed as "Generate PDF report and spreadsheet'
  )
}


download_Server <- function(id, 
                            zip_name,
                            type = c("MML", "FACETS", "EQUATE", "IRR")) {
  
  type <- match.arg(type)
  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- NS(id)
      
 
      output$report <- downloadHandler(                                         # This function makes the download     
        filename = zip_name,                                                    # The zip file created
        content = function(file){                                               # download handler is the main part of the application to make the RmD file. 
          withProgress(message = 'R Shiny Boosted Rendering',{                  # Set a progress bar because it can take some time
                     
                         
            tempdir <- tempdir()                                                # Copy the report file to a temporary directory before processing it, in case we don't have write permissions to the current working dir (which can happen when deployed).
            
            
            if(type == "MML"){
              
              tempReport <- file.path(tempdir, "MML.Rmd")                       # Create the filepath where the tempory rmd file resides
              file.copy("Rmd/MML.Rmd", tempReport, overwrite = TRUE)            # Copy the rmd file from the scripts folder to the path above
              
              excel_name <- "MML_tables.xlsx"
              pdf_name <- "MML_report.pdf"
              
            } else if(type == "FACETS"){
              
              tempReport <- file.path(tempdir, "FACETS.Rmd")                    # Create the filepath where the tempory rmd file resides
              file.copy("Rmd/FACETS.Rmd", tempReport, overwrite = TRUE)         # Copy the rmd file from the scripts folder to the path above
              
              excel_name <- "FACETS_tables.xlsx"
              pdf_name <- "FACETS_report.pdf"
              
            } else if(type == "EQUATE"){
              
              tempReport <- file.path(tempdir, "EQUATE.Rmd")                    # Create the filepath where the tempory rmd file resides
              file.copy("Rmd/EQUATE.Rmd", tempReport, overwrite = TRUE)         # Copy the rmd file from the scripts folder to the path above
              
              excel_name <- "EQUATE_tables.xlsx"
              pdf_name <- "EQUATE_report.pdf"
            } else if(type == "IRR"){
              tempReport <- file.path(tempdir, "IRR.Rmd")                    # Create the filepath where the tempory rmd file resides
              file.copy("Rmd/IRR.Rmd", tempReport, overwrite = TRUE)         # Copy the rmd file from the scripts folder to the path above
              
              pdf_name <- "IRR_report.pdf"
              
            }

            
            if(type != "IRR"){
              node.sequence <- as.numeric(strsplit(input$node.sequence,",")[[1]]) # The tempdir constantly changes at shinyapps.io, that is why we have to repeat this process every time.
            }
           
            
            # Now we can get our inputs and use them in the .Rmd
            
            if(type %in% c("MML", "FACETS")){
              params <- list(datapath = input$input_file$datapath,                # Set up parameters to pass to Rmd document
                             recommendations = input$recommendations,
                             construct = input$construct,
                             population = input$population,
                             constraint = input$constraint,
                             NA.Delete = input$NA.Delete,
                             disc.threshold = as.numeric(input$disc.threshold),
                             ci.level = as.numeric(input$ci.level),                       
                             p.fit.threshold = as.numeric(input$p.fit.threshold),
                             node.sequence.1 = node.sequence[1],                  # For node.sequence we have to do some cleaning on the input first:
                             node.sequence.2 = node.sequence[2],                  # This is the part where we use the strsplit. We seperate the character on ',' and make it numeric
                             node.sequence.3 = node.sequence[3],
                             conv = as.numeric(input$conv),
                             maxiter = as.numeric(input$maxiter),
                             color.choice = input$color.choice,
                             binwidth = as.numeric(input$binwidth),
                             rendered_by_shiny = TRUE,                            # we need rendered_by_shiny to update the progress bar
                             excel_name = excel_name
              )
            }
            
            if(type == "FACETS"){

              additional_params <- 
                list(
                  facets.cut.p = input$facets.cut.p, 
                  facets.cut.logit = input$facets.cut.logit,
                  excel_name = excel_name
                )
              
              params <- c(params, additional_params)
            }

            # Different set of parameters for EQUATE tab
            if(type == "EQUATE"){

               params <- list(datapath_A = input$datapath_A$datapath,                # Set up parameters to pass to Rmd document
                              datapath_B = input$datapath_B$datapath,
                              recommendations = input$recommendations,
                              construct = input$construct,
                              population = input$population,
                              node.sequence.1 = node.sequence[1],                  # For node.sequence we have to do some cleaning on the input first:
                              node.sequence.2 = node.sequence[2],                  # This is the part where we use the strsplit. We seperate the character on ',' and make it numeric
                              node.sequence.3 = node.sequence[3],
                              conv = as.numeric(input$conv),
                              maxiter = as.numeric(input$maxiter),
                              rendered_by_shiny = TRUE,                            # we need rendered_by_shiny to update the progress bar
                              excel_name = excel_name
              )
            } 
            
             # Different set of parameters for IRR tab
            if(type == "IRR"){
              
              params <- list(datapath = input$input_file$datapath,  
                             recommendations = input$recommendations,
                             construct = input$construct,
                             population = input$population,
                             model = input$model,
                             type = input$type,
                             unit = input$unit,
                             conf.level = NA,
                             rendered_by_shiny = TRUE                          # we need rendered_by_shiny to update the progress bar
              )
            }            
            
  
            file1 <- file.path(tempdir, pdf_name)   
            # Knit the document, passing in the `params` list, and eval it in a child of the global environment (this isolates the code in the document from the code in this app).
            rmarkdown::render(tempReport,
                              output_file = file1,
                              params = params,
                              envir = new.env(parent = globalenv())
            )                                                            # file1 is the path of the PDF output
            
            if(type != "IRR"){
              file2 <- file.path(tempdir, excel_name)                   # file2 is the path of the xlsx output coming from the markdown
            } else {
              file2 <- NULL
            }
            
            file_1_and_2 <- c(file1, file2)                              # combine all the files to zip them
            
            zip(file, file_1_and_2, extras = "-j")                       # creating a zip. You need extras = "-j" to get a clean zip, not one with the whole paths
          }
          )                          # withProgress
        }                  # function file wrapper
      )              # downloadHandler wrapper
    }
  )
}



     # the main  input, input$input_file (csv), is recognised when the following conditions are met.
      # observeEvent(input$input_file,{
      #   if (length(input$input_file) > 0 &&                        # The length of the input file is above zero.
      #       length(input$recommendations) > 0 &&                   # The recommendations are always entered (this is always true due to placeholder).
      #       length(input$construct) > 0 &&                         # same as above "Test Topic".
      #       length(input$population) > 0                           # same as above "Students".
      #   ){ 
      #     browser()
      #     shinyjs::enable(ns("report"))                         # when the conditions above are met, shinyjs::enable is used to enable the report button to be clicked.
      #   }else {                                                # otherwise, when conditions not met,
      #     shinyjs::disable(ns("report"))  
      #   }                                                    # button is disabled.
      # }                                             
      # )  
      
      
      # output$