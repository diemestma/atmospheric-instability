# Setup
rm(list=ls())
library(tidyverse)
library(thunder)

# 1. Download and Export ----

base_path <- "https://weather.uwyo.edu/wsgi/sounding?"
file_suffix <- "&id=84628&type=TEXT:CSV&src=FM35"

for (year in 1983:2021) {
  for (month in 1:12) {
    if (month < 10) month <- paste0(0,month)
    for (day in 1:31) {
      if (day < 10) day <- paste0(0,day)
      for (hour in c("00", "12")) {
        
        a <- try(read.csv(paste0(base_path, 
                                 "datetime=", year, 
                                 "-", month, 
                                 "-", day,
                                 "%20", hour, ":00:00",
                                 file_suffix)))
        
        if (class(a) != "try-error") {
          file <- paste0("E:/Datos/",year,"/",
                         year,month,day,hour,"-84628.csv")
          write.csv(a, file = file, sep = ",", row.names = FALSE)
        }
      }
    } 
  }
}



