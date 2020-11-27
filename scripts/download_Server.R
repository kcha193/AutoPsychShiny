


download_Server <- function(id) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- NS(id)
      
      
      output$report <- downloadHandler(                                                                   # This function makes the download     
        filename = "psychometric_analysis.zip",                                                           # The zip file created
        content = function(file){                                                                         # download handler is the main part of the application to make the RmD file. 
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
                       
                       file_1_and_2 <- c(file1, file2)                                               # combine all the files to zip them
                       
                       zip(file, file_1_and_2, extras = "-j")                                        # creating a zip. You need extras = "-j" to get a clean zip, not one with the whole paths
                       }
          )                          # withProgress
        }                  # function file wrapper
      )              # downloadHandler wrapper
    }
  )
}