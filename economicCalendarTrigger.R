library(rvest)
library(lubridate)
library(tidyverse)
library(stringr)
library(magrittr)


#--------------DATA IMPORT-----------------#
DFevent <- try(read.csv("E:/trading/Git/R_NewsReading/calendar-event-list.csv"),silent = TRUE)
DFT2 <- try(read_csv("C:/Program Files (x86)/AM MT4 - Terminal 2/MQL4/files/OrdersResultsT2.csv",
                      col_names = c("MagicNumber", "TicketNumber", "OrderStartTime", "OrderCloseTime", "Profit", "Symbol", "OrderType"),
                      col_types = "iiccdci"),silent = TRUE)

#search for the trading currency to compare with the calendar
SymbolTraded <- c(substr(DFT2 %$% Symbol %>% unique() %>% sort(),1,3),
                  substr(DFT2 %$% Symbol %>% unique() %>% sort(),4,6))
#remove duplicate
SymbolTraded <- SymbolTraded[!duplicated(SymbolTraded)]


#----------------TABLE EDITION--------------#
# convert "Start to time format and filter high impact event
DFeventResult <- data.frame(Start = as.POSIXct( DFevent$Start, format="%m/%d/%Y %H:%M:%S" , tz = "GMT"),
                            Name = DFevent$Name,
                            Impact = DFevent$Impact,
                            Currency = DFevent$Currency)  %>%
                 filter(DFevent$Impact == "HIGH")

# change timezone to local time
# tz list : https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
DFeventResult$Start <- format(DFeventResult$Start, tz="Asia/Bangkok",usetz=TRUE)

#remove old events (can be adjusted according how long after the event have started you want keep the flag to 1)
DFeventResult <- filter(DFeventResult, DFeventResult$Start > Sys.time() - 3600)

#create a new column to compare the currency traded with the currency of the calendar
duplicateCurency <- vector("numeric", nrow(DFeventResult))
for(i in 1:nrow(DFeventResult)){
 # i <- 1
  
  for(j in 1: length(SymbolTraded)){
    # j = 3
         if(DFeventResult[i,4] == SymbolTraded[j]){
            duplicateCurency[i] <- 1
            break
          } else{
            duplicateCurency[i] <-0
          }
  }
 }

#add new column telling us which currency we are currently trading
DFeventResult$CurrencyTraded = duplicateCurency
#filter the table with the currency traded true
DFeventResult <- filter(DFeventResult,DFeventResult$CurrencyTraded == 1)


#-------FLAG RESULT--------------#
# use for loop to compare all event time with current time to send us the signal
for(i in nrow(DFeventResult)){
  # i<-1
  # 1 can be adjusted according how long before the event you want the signal stop the trading
  if(difftime(DFeventResult$Start[i],Sys.time(), units = "hours") < 1){
    flag <- 1
    break
  }else{
    flag <- 0
  }
  
}

print(flag)


#----------SEND SIGNAL IN A CSV FILE-------------#

# write flag obtained to all terminals!
#Terminal 1
write.csv(flag, "C:/Program Files (x86)/AM MT4 - Terminal 1/MQL4/Files/01_MacroeconomicEvent.csv", row.names = F)
#Terminal 2
write.csv(flag, "C:/Program Files (x86)/AM MT4 - Terminal 2/MQL4/Files/01_MacroeconomicEvent.csv", row.names = F)
#Terminal 3
write.csv(flag, "C:/Program Files (x86)/AM MT4 - Terminal 3/MQL4/Files/01_MacroeconomicEvent.csv", row.names = F)
#Terminal 4
write.csv(flag, "C:/Program Files (x86)/AM MT4 - Terminal 4/MQL4/Files/01_MacroeconomicEvent.csv", row.names = F)



