# global.R

# Determine which, if any, server the app is running on

if(file.exists("/opt/shiny-server/env.txt")) {
  
  server_env <-  readLines("/opt/shiny-server/env.txt", n = 1)
  
} else {
  
  server_env <- "local"
  
}


# specify the packages to load

r_pkgs <- c("shiny",
            "shinydashboard",
            "plotly",
            "stringr",
            "DT",
            "tidyr",
            "dplyr",
            "reshape2",
            "shinyWidgets",
            "V8",
            "shinyjs",
            "ggplot2",
            "ggrepel",
            "shinyBS") 



# Require the packages from the appropriate libraries 

if(server_env=='stage'){
  
  sapply(r_pkgs, require, lib.loc = '/revr_lib/HR/TalentAnalytics/NewHire/stage', character.only = TRUE)
  
}  else if ( server_env=='prod'){
  
  sapply(r_pkgs, require, lib.loc = '/revr_lib/HR/TalentAnalytics/NewHire/prod', character.only = TRUE)
  
} else {
  
  sapply(r_pkgs, require, character.only = TRUE)
  
}



