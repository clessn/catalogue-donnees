# Packages ----------------------------------------------------------------
library(RSelenium)
library(tidyverse)
library(netstat)

# Start server ------------------------------------------------------------

## check available chromedriver versions
binman::list_versions("chromedriver")

### il faut que la version de chrome sur l'ordi soit parmi les available

rs_driver_object <- rsDriver(browser = c("chrome"),
                             chromever = "114.0.5735.90",
                             port = netstat::free_port())

class(rs_driver_object)

# Create client object ----------------------------------------------------

remDr <- rs_driver_object$client
class(remDr)


# Open browser --------------------------------------------------
remDr$open()

# Navigate ----------------------------------------------------------------
remDr$navigate("https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/index.cfm?Lang=F")

# Find the element -----------------------------------------------------------

## In this case, the search bar
searchbar <- remDr$findElement(using = "id", value = "SearchText")
class(searchbar)

searchbar$sendKeysToElement(list("G1R", "\uE007"))



