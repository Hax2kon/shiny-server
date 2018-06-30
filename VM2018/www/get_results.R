url <- "https://www.fifa.com/worldcup/matches/"
library(rvest)
nodes <- read_html(url) %>% html_node(xpath='/html/body/div[3]/div/div[2]/section/div[2]/div[2]') %>% 
  html_nodes(xpath='//*[@class="fi-mu-list "]')

res <- read_html(url) %>% html_node(xpath='/html/body/div[3]/div/div[2]/section/div[2]/div[2]') %>% 
  html_nodes(xpath='//*[@class="fi-mu-list "]') %>% 
  html_nodes(xpath='//*[@class="fi-s__scoreText"]') %>% html_text()
res <- trimws(gsub("\r\n", "", res))
res[grep(":", res)] <- NA

date <- read_html(url) %>% html_node(xpath='/html/body/div[3]/div/div[2]/section/div[2]/div[2]') %>% 
  html_nodes(xpath='//*[@class="fi-mu-list "]') %>% 
  html_nodes(xpath='//*[@class="fi-mu__info"]') %>% html_text()
date <- gsub(".* ([0-9][0-9] .*:00).*", "\\1", date)

real_team1 <- read_html(url) %>% html_node(xpath='/html/body/div[3]/div/div[2]/section/div[2]/div[2]') %>% 
  html_nodes(xpath='//*[@class="fi-mu-list "]') %>% 
  html_nodes(xpath='//*[@class="fi-t fi-i--4 home"]') %>% html_text()
real_team1 <- trimws(gsub("[A-Z][A-Z][A-Z]", "", gsub("\r\n", "\\1", real_team1)))

real_team2 <- read_html(url) %>% html_node(xpath='/html/body/div[3]/div/div[2]/section/div[2]/div[2]') %>% 
  html_nodes(xpath='//*[@class="fi-mu-list "]') %>% 
  html_nodes(xpath='//*[@class="fi-t fi-i--4 away"]') %>% html_text()
real_team2 <- trimws(gsub("[A-Z][A-Z][A-Z]", "", gsub("\r\n", "\\1", real_team2)))

if(length(real_team1)<length(real_team2)){
  real_team1 <- c(real_team1, "")
} else if(length(real_team2)<length(real_team1)){
  real_team2 <- c(real_team2, "")
}

date    <- date[1:length(real_team1)]
res <- res[1:length(real_team1)]
res <- data.frame(cbind(date, res, real_team1, real_team2), stringsAsFactors = FALSE)

#Fix variables
res$res1 <- gsub("-.*", "", res$res)
res$res2 <- gsub(".*-", "", res$res)

#Fix teamnames
res$real_team1 <- as.character(res$real_team1)
res$real_team2 <- as.character(res$real_team2)
res$real_team1 <- ifelse(res$real_team1 == "Korea Republic", "South Korea",
                         ifelse(res$real_team1 == "IR Iran", "Iran", res$real_team1))
res$real_team2 <- ifelse(res$real_team2 == "Korea Republic", "South Korea",
                         ifelse(res$real_team2 == "IR Iran", "Iran", res$real_team2))


#res$day  <- format(as.POSIXct(res$date, format="%d %b %Y - %H:%M"), "%Y-%m-%d")
res$time <- as.POSIXct(gsub("27-06-2018 19:00", "27-06-2018 17:00",
                            gsub("23-06-2018 21:00", "23-06-2018 18:00",
                            gsub("23-06-2018 18:00", "23-06-2018 17:00",
                                 gsub("25-06-2018 18:00", "25-06-2018 17:00",
                                      gsub("25-06-2018 21:00", "25-06-2018 20:00",
                                           gsub("8 - ", "8 ", gsub(" Jul ", "-07-", gsub(" Jun ", "-06-", res$date)))))))),
                       "%d-%m-%Y %H:%M", tz="Europe/Moscow")
attributes(res$time)$tzone <- "Etc/GMT-1"
res <- res[order(res$time, res$real_team1),]
res$match_alt <- 1:nrow(res)

res <- res[, c("real_team1", "res1", "res2", "real_team2", "time", "match_alt")]


res$time <- NULL

