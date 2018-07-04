
library(dplyr)
kampoversikt <- merge(df, res, by.x=c("match_alt"), by.y=c("match_alt"), all=TRUE)


#Fiks straffer
kampoversikt$res2[which(kampoversikt$match==51)] <- as.character(as.numeric(as.character(kampoversikt$res2[which(kampoversikt$match==51)]))+1)
kampoversikt$res1[which(kampoversikt$match==52)] <- as.character(as.numeric(as.character(kampoversikt$res1[which(kampoversikt$match==52)]))+1)
kampoversikt$res2[which(kampoversikt$match==56)] <- as.character(as.numeric(as.character(kampoversikt$res2[which(kampoversikt$match==56)]))+1)

#kampoversikt <- merge(df, res, by.x=c("team1", "team2", "day"), by.y=c("team1", "team2", "day"), all=TRUE)
kampoversikt <- kampoversikt[which(is.na(kampoversikt$match)==FALSE),]
kampoversikt$day <- NULL
kampoversikt$guess1[which(kampoversikt$guess1==99)] <- NA
kampoversikt$guess2[which(kampoversikt$guess2==99)] <- NA
kampoversikt$guess1 <- as.numeric(kampoversikt$guess1)
kampoversikt$guess2 <- as.numeric(kampoversikt$guess2)
kampoversikt$res1   <- as.numeric(kampoversikt$res1)
kampoversikt$res2   <- as.numeric(kampoversikt$res2)

kampoversikt$guessed_winner <- ifelse(kampoversikt$guess1 > kampoversikt$guess2, kampoversikt$team1,
                                      ifelse(kampoversikt$guess1 < kampoversikt$guess2, kampoversikt$team2,
                                             "Uavgjort"))
kampoversikt$guessed_second <- ifelse(kampoversikt$guess1 > kampoversikt$guess2, kampoversikt$team2,
                                      ifelse(kampoversikt$guess1 < kampoversikt$guess2, kampoversikt$team1,
                                             "Uavgjort"))
kampoversikt$real_winner    <- ifelse(kampoversikt$res1   > kampoversikt$res2  , kampoversikt$real_team1, 
                                      ifelse(kampoversikt$res1   < kampoversikt$res2  , kampoversikt$real_team2, 
                                             "Uavgjort"))
kampoversikt$real_second    <- ifelse(kampoversikt$res1   > kampoversikt$res2  , kampoversikt$real_team2, 
                                      ifelse(kampoversikt$res1   < kampoversikt$res2  , kampoversikt$real_team1, 
                                             "Uavgjort"))



#####     \\    Gjør klar til Runde2-underkonkurranse   //    #####
oversikt_runde2 <- unique(kampoversikt[grep("^S[0-9]", kampoversikt$group),c("match", "time", "res1", "res2", "real_winner")])
oversikt_runde2 <- merge(df2, oversikt_runde2, by="match", all=TRUE)


##Score
oversikt_runde2$score <- ifelse(oversikt_runde2$guessed_winner==oversikt_runde2$real_winner,
                                20 - ((oversikt_runde2$guess1 - oversikt_runde2$res1)^2) - ((oversikt_runde2$guess2 - oversikt_runde2$res2)^2),
                                0 - ((oversikt_runde2$guess1 - oversikt_runde2$res1)^2) - ((oversikt_runde2$guess2 - oversikt_runde2$res2)^2))

#Fiks tidspunkt
oversikt_runde2$time <- oversikt_runde2$time + 60*60 + 60*60 + 60*60


#####     FERDIG MED Runde2     #####

#Fix class
kampoversikt$match <- as.numeric(kampoversikt$match)


####    \\    --      Gruppespillet   --    //    #####
gruppe <- kampoversikt[grep("GROUP", kampoversikt$group),]

#Kvadrert
gruppe$kvadrert1 <- (gruppe$guess1 - gruppe$res1)^2
gruppe$kvadrert2 <- (gruppe$guess2 - gruppe$res2)^2

##Score
gruppe$score <- ifelse(gruppe$guessed_winner==gruppe$real_winner,
                           20 - gruppe$kvadrert1 - gruppe$kvadrert2,
                           0 - gruppe$kvadrert1 - gruppe$kvadrert2)
gruppe$score[which(is.na(gruppe$score)==TRUE)] <- 0


# 
#Fiks Mattis
mattisscore <- gruppe[which(gruppe$match %in% 2:5 & gruppe$player %in% c("Haakon", "Mikkel", "Sjur", "Sverre", "Olli", "Patrik")),]
mattisscore <- mattisscore %>% group_by(player) %>% mutate("cumu"=cumsum(score))
gruppe$score[which(gruppe$player=="Mattis" & gruppe$match==5)] <- median(mattisscore$cumu[which(mattisscore$match==5)])

rm(mattisscore)
gruppe$guess1[which(gruppe$player=="Mattis" & is.na(gruppe$guess1)==TRUE)] <- 99
gruppe$guess2[which(gruppe$player=="Mattis" & is.na(gruppe$guess2)==TRUE)] <- 99

##Fix latecomers
#Christian (Match 13)
gruppe$score[which(is.na(gruppe$guess1)==TRUE)] <- NA
gruppe <- gruppe[order(gruppe$player, gruppe$match),]
gruppe <- gruppe %>% group_by(player) %>% mutate("cumu"=cumsum(score))
gruppe <- data.frame(gruppe)
gruppe$score[which(gruppe$player=="Christian" & gruppe$match==13)] <- median(gruppe$cumu[which(gruppe$match==13)], na.rm=TRUE)
gruppe$score[which(gruppe$player=="Christian" & is.na(gruppe$score)==TRUE)] <- 0

#Scott (Match 14)
gruppe <- gruppe[order(gruppe$player, gruppe$match),]
gruppe <- gruppe %>% group_by(player) %>% mutate("cumu"=cumsum(score))
gruppe <- data.frame(gruppe)
gruppe$score[which(gruppe$player=="Scott" & gruppe$match==14)]     <- median(gruppe$cumu[which(gruppe$match==14)], na.rm=TRUE)
gruppe$score[which(gruppe$player=="Scott" & is.na(gruppe$score)==TRUE)] <- 0

#Ordne
gruppe$score[which(is.na(gruppe$score)==TRUE)] <- 0
gruppe$cumu <- NULL

gruppe$guess1[which(gruppe$guess1==99)] <- NA
gruppe$guess2[which(gruppe$guess2==99)] <- NA

# 
# ####    Fix latecomers
# matchmedians <- kampoversikt[which(is.na(kampoversikt$guess1)==FALSE),]
# matchmedians <- matchmedians %>% group_by(match) %>% summarise("matchmedian" = median(score, na.rm=TRUE))
# 
# for(i in 1:nrow(kampoversikt)){
#  kampoversikt$score[i] <- ifelse(is.na(kampoversikt$guess1[i]) == TRUE,
#                                  matchmedians$matchmedian[which(matchmedians$match==kampoversikt$match[i])],
#                                  kampoversikt$score[i])
# }

#####  \\  --  Åttenedels    --    //      #####
round2 <- kampoversikt[grep("^S[0-9]", kampoversikt$group),]
round2$score <- 0
sannhet <- round2[!duplicated(round2[,c("real_team1", "real_team2")]), c("real_team1","real_team2")]
sannhet <- c(sannhet$real_team1, sannhet$real_team2)
#25 poeng for å gjette riktig lag videre
round2$score <- ifelse(round2$team1 %in% sannhet, round2$score+25, round2$score)
round2$score <- ifelse(round2$team2 %in% sannhet, round2$score+25, round2$score)

#PLuss 5 poeng for å gjette riktig posisjon
round2$score <- ifelse(round2$team1 == round2$real_team1, round2$score+5, round2$score)
round2$score <- ifelse(round2$team2 == round2$real_team2, round2$score+5, round2$score)


#####  \\  --  Quartfinalen    --    //      #####
quart <- kampoversikt[grep("^Q", kampoversikt$group),]
quart$score <- 0
sannhet <- quart[!duplicated(quart[,c("real_team1", "real_team2")]), c("real_team1","real_team2")]
sannhet <- c(sannhet$real_team1, sannhet$real_team2)
#25 poeng for å gjette riktig lag videre
quart$score <- ifelse(quart$team1 %in% sannhet, quart$score+25, quart$score)
quart$score <- ifelse(quart$team2 %in% sannhet, quart$score+25, quart$score)

#PLuss 5 poeng for å gjette riktig posisjon
quart$score <- ifelse(quart$team1 == quart$real_team1, quart$score+5, quart$score)
quart$score <- ifelse(quart$team2 == quart$real_team2, quart$score+5, quart$score)


#####  \\  --  Semifinalen    --    //      #####
semi <- kampoversikt[grep("^SF", kampoversikt$group),]
semi$score <- 0
sannhet <- semi[!duplicated(semi[,c("real_team1", "real_team2")]), c("real_team1","real_team2")]
sannhet <- c(sannhet$real_team1, sannhet$real_team2)
#25 poeng for å gjette riktig lag videre
semi$score <- ifelse(semi$team1 %in% sannhet, semi$score+25, semi$score)
semi$score <- ifelse(semi$team2 %in% sannhet, semi$score+25, semi$score)

#PLuss 5 poeng for å gjette riktig posisjon
semi$score <- ifelse(semi$team1 == semi$real_team1, semi$score+5, semi$score)
semi$score <- ifelse(semi$team2 == semi$real_team2, semi$score+5, semi$score)


#####  \\  --  Bronse    --    //      #####
bronse <- kampoversikt[grep("Bronsefinale", kampoversikt$group),]
bronse$score <- 0
sannhet <- bronse[!duplicated(bronse[,c("real_team1", "real_team2")]), c("real_team1","real_team2")]
sannhet <- c(sannhet$real_team1, sannhet$real_team2)

#25 poeng for å gjette riktig lag videre
bronse$score <- ifelse(bronse$team1 %in% sannhet, bronse$score+25, bronse$score)
bronse$score <- ifelse(bronse$team2 %in% sannhet, bronse$score+25, bronse$score)

#Pluss 5 poeng for å gjette riktig posisjon
bronse$score <- ifelse(bronse$team1 == bronse$real_team1, bronse$score+5, bronse$score)
bronse$score <- ifelse(bronse$team2 == bronse$real_team2, bronse$score+50, bronse$score)

#Riktig vinner i bronsefinalen
bronse$score <- ifelse(bronse$guessed_winner == bronse$real_winner, bronse$score+5, bronse$score)


#####   \\  --  Finale   --    //    #####
finale <- kampoversikt[grep("^finale", kampoversikt$group),]
finale$score <- 0
sannhet <- finale[!duplicated(finale[,c("real_team1", "real_team2")]), c("real_team1","real_team2")]
sannhet <- c(sannhet$real_team1, sannhet$real_team2)
#25 poeng for å gjette riktig lag videre
finale$score <- ifelse(finale$team1 %in% sannhet, finale$score+25, finale$score)
finale$score <- ifelse(finale$team2 %in% sannhet, finale$score+25, finale$score)

#PLuss 5 poeng for å gjette riktig posisjon
finale$score <- ifelse(finale$team1 == finale$real_team1, finale$score+5, finale$score)
finale$score <- ifelse(finale$team2 == finale$real_team2, finale$score+5, finale$score)

#Riktig vinner og andreplass i finalen
finale$score <- ifelse(finale$guessed_winner == finale$real_winner, finale$score+200, finale$score)
finale$score <- ifelse(finale$guessed_second == finale$real_second, finale$score+100, finale$score)


#####   \\    --    Sett sammen til kampoversikt      --      //    #####
kampoversikt <- bind_rows(gruppe, round2, quart, semi, bronse, finale)


##Fix more variables
kampoversikt$time  <- kampoversikt$time + 60*60 + 60*60 + 60*60 
kampoversikt$time  <- as.character(kampoversikt$time)
kampoversikt <- kampoversikt[order(kampoversikt$player, kampoversikt$match),]
kampoversikt <- kampoversikt %>% group_by(player) %>% mutate("cumu"=cumsum(score))
kampoversikt <- data.frame(kampoversikt)


#Kampenighet
kamp_backup <- kampoversikt
kampoversikt$guessed_winner_no <- ifelse(kampoversikt$guess1>kampoversikt$guess2, 1,
                                         ifelse(kampoversikt$guess1<kampoversikt$guess2, 2, 3))
for(i in unique(kampoversikt$player)){
  kampoversikt[,paste0(i,"fasit1")]               <- kampoversikt$guess1[which(kampoversikt$player==i)]
  kampoversikt[,paste0(i,"fasit2")]               <- kampoversikt$guess2[which(kampoversikt$player==i)]
  kampoversikt[,paste0(i,"fasit_guessed_winner")] <- kampoversikt$guessed_winner_no[which(kampoversikt$player==i)]
  
  
  kampoversikt[paste0(i, "fasitavvik1")] <- (kampoversikt$guess1 - kampoversikt[, paste0(i, "fasit1")])^2
  kampoversikt[paste0(i, "fasitavvik2")] <- (kampoversikt$guess2 - kampoversikt[, paste0(i, "fasit2")])^2
  
  kampoversikt[paste0(i, "alt_score")] <- ifelse(kampoversikt$guessed_winner_no==kampoversikt[, paste0(i, "fasit_guessed_winner")],
                                        20 - kampoversikt[, paste0(i, "fasitavvik1")] - kampoversikt[, paste0(i, "fasitavvik2")],
                                        0 - kampoversikt[, paste0(i, "fasitavvik1")] - kampoversikt[, paste0(i, "fasitavvik2")])
  
  kampoversikt[paste0(i, "alt_score")] <- kampoversikt[paste0(i, "alt_score")]/20
  
  kampoversikt[which(kampoversikt$player==i), paste0(i, "alt_score")] <- NA
  
  
}
kampoversikt$guessed_winner_no <- NULL
kampoversikt[,grep("fasit", colnames(kampoversikt))] <- NULL
coefs <- grep("alt_score", colnames(kampoversikt))
kampoversikt$matchagree <- round(rowMeans(kampoversikt[, coefs], na.rm=TRUE), 2)
kampoversikt$matchagree[grep("GROUP", kampoversikt$group, invert=T)] <- 0



#Scoreboard
library(dplyr)
scoreboard <- kampoversikt %>% group_by(player) %>% summarise("Score" = sum(score, na.rm=TRUE))
colnames(scoreboard) <- c("Spiller", "Score")

#Penere navn
kampoversikt$guess1[which(is.na(kampoversikt$guess1)==TRUE)]          <- ""
kampoversikt$guess2[which(is.na(kampoversikt$guess2)==TRUE)]          <- ""
kampoversikt$res1[which(is.na(kampoversikt$res1)==TRUE)]              <- ""
kampoversikt$res2[which(is.na(kampoversikt$res2)==TRUE)]              <- ""

kampoversikt$real_team1 <- as.character(kampoversikt$real_team1)
kampoversikt$real_team2 <- as.character(kampoversikt$real_team2)
kampoversikt$real_team1[which(is.na(kampoversikt$real_team1)==TRUE)]   <- ""
kampoversikt$real_team2[which(is.na(kampoversikt$real_team2)==TRUE)]   <- ""
kampoversikt$real_winner[which(is.na(kampoversikt$real_winner)==TRUE)] <- ""


kampoversikt <- kampoversikt[,c("match", "player", "time", "group",
                                "team1", "guess1", "guess2", "team2",
                                "real_team1", "res1", "res2", "real_team2",
                                "matchagree",
                                "guessed_winner", "real_winner",
                                "kvadrert1", "kvadrert2", "score", "cumu")]

colnames(kampoversikt) <- c("Match Nr.", "Spiller", "Kamptidspukt", "Gruppe",
                            "Forventet lag 1", "Gjett lag 1",
                            "Gjett lag 2", "Forventet lag 2",
                            "Reelt lag 1", "Resultat lag 1",
                            "Resultat lag 2", "Reelt lag 2",
                            "Enighet",
                            "Forventet vinner", "Vinner",
                            "Avvik 1", "Avvik 2", "Score", "Kumulativt")




#####     \\      Lagre     //      #####
#save(scoreboard  , file="www/scoreboard.rdata")
#save(kampoversikt, file="www/kampoversikt.rdata")



#####   Enighetsmatrise
# kamp_backup$guessed_winner_no <- ifelse(kamp_backup$guess1>kamp_backup$guess2, 1,
#                                         ifelse(kamp_backup$guess1<kamp_backup$guess2, 2, 3))
# 
# par <- kamp_backup[grep("GROUP", kamp_backup$group), c("player", "match", "guessed_winner_no", "guess1", "guess2")]
# 
# for(i in unique(par$player)){
#   par[,paste0(i,"fasit1")]               <- par$guess1[which(par$player==i)]
#   par[,paste0(i,"fasit2")]               <- par$guess2[which(par$player==i)]
#   par[,paste0(i,"fasit_guessed_winner")] <- par$guessed_winner_no[which(par$player==i)]
#   
#   
#   par[paste0(i, "fasitavvik1")] <- (par$guess1 - par[, paste0(i, "fasit1")])^2
#   par[paste0(i, "fasitavvik2")] <- (par$guess2 - par[, paste0(i, "fasit2")])^2
#   
#   par[paste0(i, "alt_score")] <- ifelse(par$guessed_winner_no==par[, paste0(i, "fasit_guessed_winner")],
#                                    20 - par[, paste0(i, "fasitavvik1")] - par[, paste0(i, "fasitavvik2")],
#                                    0 - par[, paste0(i, "fasitavvik1")] - par[, paste0(i, "fasitavvik2")])
#   
#   par[paste0(i, "alt_score")] <- par[paste0(i, "alt_score")]/20
#   
#   par[which(par$player==i), paste0(i, "alt_score")] <- NA
#   
#   
# }
# par[,grep("fasit", colnames(par))] <- NULL
# coefs <- grep("alt_score", colnames(par))
# par <- par %>% group_by(player) %>% summarise_at(coefs, mean, na.rm=TRUE)
# distances <- round(data.frame(par[, 2:ncol(par)]),2)
# diag(distances) <- 1
# rownames(distances) <- par$player
# colnames(distances) <- par$player
