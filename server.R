

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Enable the download button if you have the required inputs
  # if we don't have the required inputs, the button is not clickable
  observeEvent(input$input_file, {
    
    if (length(input$input_file) > 0 &&
        length(input$recommendations) > 0 && # because of the placeholder text, this is always satisfied
        length(input$construct) > 0 && # same as above
        length(input$population) > 0) # same as above
      shinyjs::enable("report")
    else
      shinyjs::disable("report")
  })
  
  # The function that makes the download
  output$report <- downloadHandler(
    
    filename = "psychometric_analysis.zip",
    
    content = function(file) {
      
      # Set a progress bar because it can take some time
      withProgress(message = 'R Shiny Boosted Rendering', {
        
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        
        tempdir <- tempdir()
        
        # Create the filepath where the tempory rmd file resides
        tempReport <- file.path(tempdir, "Testbuild.RUNNING2.Rmd")
        
        # Cope the rmd file from the scripts folder to the path above
        file.copy("scripts/Testbuild.RUNNING2.Rmd", tempReport, overwrite = TRUE)
        
        # The tempdir constantly changes at shinyapps.io, that why we have to repeat this process every time.
        
        # Now we can get our inputs and use them in the .Rmd
        # For node.sequence we have to do some cleaning on the input first:
        # This is the part where we use the strsplit. We seperate the character on ',' and make it numeric
        node.sequence <- as.numeric(strsplit(input$node.sequence, ",")[[1]])
        
        # Set up parameters to pass to Rmd document
        params <- list(  datapath = input$input_file$datapath,
                         recommendations = input$recommendations,
                         construct = input$construct,
                         population = input$population,
                         constraint = input$constraint,
                         NA.Delete = input$NA.Delete,
                         disc.threshold = as.numeric(input$disc.threshold),
                         ci.level = as.numeric(input$ci.level),                       
                         p.fit.threshold = as.numeric(input$p.fit.threshold),
                         node.sequence.1 = node.sequence[1],
                         node.sequence.2 = node.sequence[2],
                         node.sequence.3 = node.sequence[3],
                         conv = as.numeric(input$conv),
                         maxiter = as.numeric(input$maxiter),
                         color.choice = input$color.choice,
                         binwidth = as.numeric(input$binwidth),
                         rendered_by_shiny = TRUE) # we need rendered_by_shiny to update the progress bar
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        
        file1 <- file.path(tempdir, "report.pdf") #file1 is the path of the PDF output
        
        rmarkdown::render(tempReport, output_file = file1,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        
        file2 <- file.path(tempdir, "report.xlsx") #file2 is the path of the xlsx output coming from the markdown
        
        files <- c(file1, file2) #, file3) # combine all the files to zip them
        
        zip(file, files, extras = "-j") # creating a zip. You need extras = "-j" to get a clean zip, not one with the whole paths
        
      })
    }
  )
  
  
})