library(dplyr)

s3 <- paws::s3()

s3$list_buckets()
objets = s3$list_objects(Bucket = "recherche-souverainete",
                         Prefix = "raw/ces")


test <- s3$get_object(
  Bucket = "recherche-souverainete",
  Key = "raw/ces/ces1988.csv"
)

file_name4 <- "s3_download.csv"
writeBin(test$Body, con = file_name4)
csv <- read.csv(file_name4, encoding = "UTF-8")



test2 <- s3$get_object(
  Bucket = "recherche-souverainete",
  Key = "raw/omnibus/april/omnibus_4april_open.xlsx"
)

file_name4 <- "s3_download.xlsx"
writeBin(test$Body, con = file_name4)
xlsx <- readxl::read_xlsx(file_name4)
