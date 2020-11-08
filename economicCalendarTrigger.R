library(rvest)
library(lubridate)
library(tidyverse)
library(stringr)
library(magrittr)
library(tidyverse)

#--------------DATA IMPORT-----------------#
DFevent <- try(read.csv("E:/trading/Git/R_NewsReading/calendar-event-list.csv"),silent = TRUE)
DFT2 <- try(read_csv("C:/Program Files (x86)/AM MT4 - Terminal 2/MQL4/files/OrdersResultsT2.csv",
                      col_names = c("MagicNumber", "TicketNumber", "OrderStartTime", "OrderCloseTime", "Profit", "Symbol", "OrderType"),
                      col_types = "iiccdci"),silent = TRUE)

# get Magic Number and symbol
DFT2MN <- DFT2 %>% group_by(MagicNumber) %>% select("MagicNumber","Symbol") %>% unique()

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


#-------FLAG RESULT--------------#
# use for loop to compare all event time with current time to send us the signal
flag  <- vector("numeric", nrow(DFT2MN))

# the 1st for loop will check any magic numbers
for(i in 1:nrow(DFT2MN)){
 # i <-1

    for(j in 1:nrow(DFeventResult)){
      # j<-1
      # 1 can be adjusted according how long before the event you want the signal stop the trading
      if(as.double(difftime(DFeventResult$Start[j],Sys.time(), units = "hours")) < 1 &&
          (substr(DFT2MN$Symbol[i],1,3) == DFeventResult$Currency[j] || substr(DFT2MN$Symbol[i],4,6) == DFeventResult$Currency[j] )){
        flag[i] <- 1 
        break
      } else{
        flag[i] <- 0
      }
    }
}

# add new column telling us which magic number shouldn't trade
DFT2MN$Flag = flag
 
# filter the table with magic number not allow to trade only
#DFT2MN <-as.data.frame(DFT2MN) %>% filter(DFT2MN$Flag == 1)


#----------SEND SIGNAL IN A CSV FILE-------------#

# write flag obtained to all terminals!

#Terminal 2
write.csv(DFT2MN, "C:/Program Files (x86)/AM MT4 - Terminal 2/MQL4/Files/01_MacroeconomicEvent.csv", row.names = F)
#Terminal 1
DFT1MN <- data.frame(DFT2MN$MagicNumber - 100, DFT2MN$Symbol,DFT2MN$Flag )
write.csv(DFT1MN, "C:/Program Files (x86)/AM MT4 - Terminal 1/MQL4/Files/01_MacroeconomicEvent.csv", row.names = F)
#Terminal 3
DFT3MN <- data.frame(DFT2MN$MagicNumber + 100, DFT2MN$Symbol,DFT2MN$Flag )
write.csv(DFT3MN, "C:/Program Files (x86)/AM MT4 - Terminal 3/MQL4/Files/01_MacroeconomicEvent.csv", row.names = F)
#Terminal 4
DFT4MN <- data.frame(DFT2MN$MagicNumber + 200, DFT2MN$Symbol,DFT2MN$Flag )
write.csv(DFT4MN, "C:/Program Files (x86)/AM MT4 - Terminal 4/MQL4/Files/01_MacroeconomicEvent.csv", row.names = F)



