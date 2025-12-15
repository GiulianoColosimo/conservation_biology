x <- c("calendR",
       "ggthemes",
       "knitr", "kableExtra",
       "lubridate",
       "scales",
       "tidyverse",
       "XML", "xml2")
lapply(x, require, character.only = T)
rm(x)


students <- c("Serena Liverino",     "Francesco Baldini",   "Federico Cornacchia",
              "Alessia Iacobelli",  "Laura Oliva", "Anna Battista",
              "Carlotta Cimini",     "Alessia Caretti", "Leonardo Fritz",
              "Lorenzo D'Orfeo",    "Michela Papa", "Chiara Di Ruzza",
              "Ilaria Manni", "Luca Amoruso",  "Lucrezia Tiseo", "Loris Bognanno")


paperAssign <- read.csv("paper assignment.csv", header = F)
names(paperAssign)<- c("Student", "Title", "Url", "Date")
paperAssign$Student[1:16] <- students
paperAssign$Title <- text_spec(paperAssign$Title,
                               link = paperAssign$Url,
                               format = "html",
                               color = "blue")


dates <- seq(as.Date("2025-11-01"), as.Date("2025-11-30"), by = "1 day")
events <- ifelse(format(dates, "%w") %in% c(6, 0), "Weekend", NA)
events[c(6, 10, 17)] <- "Class"
events[13] <- "Mid term 1"
events[c(3, 20, 24, 27)] <- "Class canceled"
calendR(month = 11,
        start = "M",
        special.days = events,
        special.col = c("green", "violet", "red", "gray"),
        legend.pos = "bottom",
        pdf = F, 
        day.size = 3, 
        weeknames.size = 4, 
        title = "November",
        subtitle="2025",
        text.size = 2.5,              
        text.col = "black",
        text = "14:00-16:00",
        text.pos = c(6, 10, 13, 17))
