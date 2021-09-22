

# Server file starts here -------------------------------------------------- 

shinyServer(function(input, output, session){
  
  # ANOVA server -------------------------------------------------------------------            
  
  anova_Server("anova") # ANOVA module
  
  # Download server -------------------------------------------------------------------            
  
  download_Server("MML", zip_name = "pyschometric_analysis_MML.zip", type = "MML") # MML
  
  download_Server("FACETS", zip_name = "pyschometric_analysis_FACETS.zip", type = "FACETS") # FACETS
  
  download_Server("EQUATE", zip_name = "pyschometric_analysis_EQUATE.zip", type = "EQUATE") # EQUATE
  
  download_Server("IRR", zip_name = "pyschometric_analysis_IRR.zip", type = "IRR") # EQUATE
  
}  # server function wrapper
)
