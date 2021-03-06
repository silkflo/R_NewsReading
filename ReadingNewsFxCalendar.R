# Forex News Reading from R and limit the tradings
# Lazy Trading Course: Read news and Sentiment Analysis
# (C) 2018 Vladimir Zhbanko
# https://www.udemy.com/forex-news-and-sentiment-analysis/?couponCode=LAZYTRADE1-10


# load libraries
library(rvest)
library(lubridate)
library(tidyverse)
library(stringr)
library(magrittr)

# summary of actions
# import events list subjected to trade restrictions from xls file
# get url data columns
# create data frame

# read restricted news events as strings
restrictedEvents <- read_csv("E:/trading/Git/R_NewsReading/RestrictedEvents.csv",
                             col_names = F)

calendarEvents <- read_csv("E:/trading/Git/R_NewsReading/calendar-event-list.csv",
                           col_names = F)

# get url to access the data. URL shall be like this: "http://www.forexfactory.com/calendar.php?day=dec2.2016"
url <- paste("http://www.forexfactory.com/calendar.php?day=",
             month(Sys.Date(), label = TRUE),
             day(Sys.Date()), ".",
             year(Sys.Date()), sep = "")

# TEST URL
#url <- "http://www.forexfactory.com/calendar?day=nov2.2020"

# get the raw data from web
fxcal <- url %>% read_html() 

# get the currency column for the day
currency <- fxcal %>% html_nodes(".currency") %>% html_text()

# get the event info for the day
event <- fxcal %>% html_nodes(".calendar__event-title") %>% html_text()

# get the event time
#timeEvent <- data.frame(fxcal %>% html_nodes(".time") %>% html_text())
#sysTime <- Sys.time()%>% 
#  timeEvent$X1 <- gsub(":.*$","", timeEvent$X1)
 



# create data frame
todaysEvents <- data.frame(currency, event, stringsAsFactors = FALSE) #%>% View()

# add new column in this frame
todaysEvents$trading <- 1
flag <- 0

# scrol through the data frame todaysEvents and match the strings to content of the event column,
# if match is found write to new column "0" that will be interpreted as a NO trade
for (j in 1:nrow(restrictedEvents))
{
  # j <- 1
  matchingterm <- restrictedEvents[j, 1] %$% X1
  
  for(i in 1:nrow(todaysEvents))
  {
    # i <- 6
    if(str_detect(todaysEvents[i, 2], matchingterm) == TRUE) 
    {
      todaysEvents[i, 3] <- 0
      # when flag is 1 then no trading as macroeconomical event is detected
     
      flag <-1
      break
    }
    
  }
}

#ifelse(flag == 1 & time(sys.Time() > timeEvent + 3) )

# write the results of the all events (for user control purposes)
write.csv(todaysEvents, paste("E:/trading/Git/R_NewsReading/log/log-", Sys.Date(), ".csv", sep = ""))

# write obtained dataframe to all terminals!
#Terminal 1
write.csv(flag, "C:/Program Files (x86)/AM MT4 - Terminal 1/MQL4/Files/01_MacroeconomicEvent.csv", row.names = F)
#Terminal 2
write.csv(flag, "C:/Program Files (x86)/AM MT4 - Terminal 2/MQL4/Files/01_MacroeconomicEvent.csv", row.names = F)
#Terminal 3
write.csv(flag, "C:/Program Files (x86)/AM MT4 - Terminal 3/MQL4/Files/01_MacroeconomicEvent.csv", row.names = F)
#Terminal 4
write.csv(flag, "C:/Program Files (x86)/AM MT4 - Terminal 4/MQL4/Files/01_MacroeconomicEvent.csv", row.names = F)































