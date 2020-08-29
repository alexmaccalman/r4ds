library(tidyverse)
library(lubridate)

## import server log file
server_log <- readLines('Data/Data_Sims/simulated_server_up_down/server_log.txt')

## import error data
server_log_errors <- readLines('Data/Data_Sims/simulated_server_up_down/server_log_errors.txt')

#What is the error rate?
sum(str_detect(server_log, "error"))/length(server_log)

head(server_log)

#split strings into columns. Use "[" and "]" and ": " as the delimiters. 
# Note, because [ and ] are regular expression we must but a "\\" in front of them. 
temp1 <- str_split(server_log, "\\[|\\]|(: )", simplify = TRUE)
#grab columns we want, convert to tibble and set col names
server_log_dt <- temp1[,c(2,4,5)]
server_log_dt %<>% as_tibble() %>% 
  rename("Timestamp" = V1, "ID" = V2, "Message" = V3) %>% 
  mutate(File = "server_log")

temp2 <- str_split(server_log_errors, "\\[|\\]|(: )", simplify = TRUE)
#grab columns we want, convert to tibble and set col names
server_log_errors_dt <- temp2[,c(2,4,5)]
server_log_errors_dt %<>% as_tibble() %>% 
  rename("Timestamp" = V1, "ID" = V2, "Message" = V3) %>% 
  mutate(File = "server_log_error")
  
#combine data sets
server_all <- rbind(server_log_dt, server_log_errors_dt)

#convert to timestamp to date time
server_all %<>% mutate(Timestamp = ymd_hms(Timestamp) )

# what are the unique messages types?
unique(server_log_dt[,3])
unique(server_log_errors_dt[,3])
unique(server_all[,3])
unique(server_all[,2])
server_all %<>% arrange(Timestamp) 

check <- server_all %>% 
  str_detect("[Ee]rror")


describe(server_all)
