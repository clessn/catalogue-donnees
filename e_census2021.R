# RTA list ----------------------------------------------------------------
rtamacro <- read.csv("_SharedFolder_catalogue-donnees/scrape_census/rta_macro.csv") %>% 
  ### filter for quebec only
  filter(substr(rta, 1, 1) %in% c("G", "H", "J"))
all_rtas <- rtamacro$rta

## all_rtas contains the 415 rta in Quebec

# Download all csvs -------------------------------------------------------

## Loop --------------------------------------------------------------------

for (i in 1:length(all_rtas)){
  rta <- all_rtas[i]
  message(paste0(i, " - ", rta, " starting"))
  
  file_url <- paste0("https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger/current-actuelle.cfm?Lang=F&SearchText=j3h&DGUIDlist=2021A0011", rta, "&GENDERlist=1,2,3&STATISTIClist=1,4&HEADERlist=0&FILETYPE=CSV")
  
  start_time <- Sys.time()
  
  repeat {
    tryCatch({
      download.file(file_url, destfile = paste0("_SharedFolder_catalogue-donnees/scrape_census/lake/", rta, ".csv"), method = "curl")
      
      end_time <- Sys.time()
      duration <- end_time - start_time
      
      message(paste0(i, " - ", rta, " downloaded in ", duration, " seconds"))
      break
    }, error = function(e) {
      current_time <- Sys.time()
      duration <- current_time - start_time
      
      if (duration > 60) { # 1 minute
        message(paste0(i, " - ", rta, " download failed after ", duration, " seconds. Retrying..."))
        start_time <- Sys.time()
      } else {
        message(paste0(i, " - ", rta, " download failed. Retrying..."))
      }
      
      Sys.sleep(3) # wait for 3 seconds before retrying
    })
  }
  
  message(paste0(i, " - ", rta, " ending"))
}
