
load("www/df.rdata")
source("www/get_results.R")
source("www/calculate_score.R")
kampoversikt$`Match Nr.` <- as.character(kampoversikt$`Match Nr.`)
kampoversikt  <- kampoversikt[order(kampoversikt$Kamptidspukt),]
kampoversikt$`Avvik 1` <- ifelse(is.na(kampoversikt$`Forventet vinner`)==TRUE, 0, kampoversikt$`Avvik 1`)

#Next match
one_hour <- 60*60
time_now <- Sys.time()+one_hour+one_hour
next_match       <- kampoversikt
next_match$until <- difftime(as.POSIXct(next_match$Kamptidspukt), time_now-one_hour-one_hour, units = "hours")
next_match       <- next_match[which(next_match$until>0),]
next_match       <- next_match[which(next_match$until==min(next_match$until)), ]
next_match$until <- next_match$until - 2
next_match       <- next_match[order(next_match$Spiller),]

temp <- unique(next_match$until)
timestamp <- paste0(gsub("\\..*", "",temp), "T ",
                    round(as.numeric(paste0("0.",gsub(".*\\.", "",temp)))*60, 0), "M")


