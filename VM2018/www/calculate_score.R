
library(dplyr)
df[which(df$player=="Patrik"),]
df$day <- format(df$time, "%Y-%m-%d")

kampoversikt <- merge(df, res, by.x=c("team1", "team2", "day"), by.y=c("team1", "team2", "day"), all=TRUE)
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
kampoversikt$real_winner    <- ifelse(kampoversikt$res1   > kampoversikt$res2  , kampoversikt$team1, 
                                      ifelse(kampoversikt$res1   < kampoversikt$res2  , kampoversikt$team2, 
                                             "Uavgjort"))


#Kvadrert
kampoversikt$kvadrert1 <- (kampoversikt$guess1 - kampoversikt$res1)^2
kampoversikt$kvadrert2 <- (kampoversikt$guess2 - kampoversikt$res2)^2

##Score
kampoversikt$score <- ifelse(kampoversikt$guessed_winner==kampoversikt$real_winner,
                           20 - kampoversikt$kvadrert1 - kampoversikt$kvadrert2,
                           0 - kampoversikt$kvadrert1 - kampoversikt$kvadrert2)
kampoversikt$score[which(is.na(kampoversikt$score)==TRUE)] <- 0

##Classes
kampoversikt$match <- as.numeric(kampoversikt$match)
# 
#Fiks Mattis
mattisscore <- kampoversikt[which(kampoversikt$match %in% 2:5 & kampoversikt$player %in% c("Haakon", "Mikkel", "Sjur", "Sverre", "Olli", "Patrik")),]
mattisscore <- mattisscore %>% group_by(player) %>% mutate("cumu"=cumsum(score))
kampoversikt$score[which(kampoversikt$player=="Mattis" & kampoversikt$match==5)] <- median(mattisscore$cumu[which(mattisscore$match==5)])

rm(mattisscore)
kampoversikt$guess1[which(kampoversikt$player=="Mattis" & is.na(kampoversikt$guess1)==TRUE)] <- 99
kampoversikt$guess2[which(kampoversikt$player=="Mattis" & is.na(kampoversikt$guess2)==TRUE)] <- 99

##Fix latecomers
#Christian (Match 13)
kampoversikt$score[which(is.na(kampoversikt$guess1)==TRUE)] <- NA
kampoversikt <- kampoversikt[order(kampoversikt$player, kampoversikt$match),]
kampoversikt <- kampoversikt %>% group_by(player) %>% mutate("cumu"=cumsum(score))
kampoversikt <- data.frame(kampoversikt)
kampoversikt$score[which(kampoversikt$player=="Christian" & kampoversikt$match==13)] <- median(kampoversikt$cumu[which(kampoversikt$match==13)], na.rm=TRUE)
kampoversikt$score[which(kampoversikt$player=="Christian" & is.na(kampoversikt$score)==TRUE)] <- 0

#Scott (Match 14)
kampoversikt <- kampoversikt[order(kampoversikt$player, kampoversikt$match),]
kampoversikt <- kampoversikt %>% group_by(player) %>% mutate("cumu"=cumsum(score))
kampoversikt <- data.frame(kampoversikt)
kampoversikt$score[which(kampoversikt$player=="Scott" & kampoversikt$match==14)]     <- median(kampoversikt$cumu[which(kampoversikt$match==14)], na.rm=TRUE)
kampoversikt$score[which(kampoversikt$player=="Scott" & is.na(kampoversikt$score)==TRUE)] <- 0

#Ordne
kampoversikt$score[which(is.na(kampoversikt$score)==TRUE)] <- 0
kampoversikt$cumu <- NULL

kampoversikt$guess1[which(kampoversikt$guess1==99)] <- NA
kampoversikt$guess2[which(kampoversikt$guess2==99)] <- NA

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
kamp_backup$guessed_winner_no <- ifelse(kamp_backup$guess1>kamp_backup$guess2, 1,
                                        ifelse(kamp_backup$guess1<kamp_backup$guess2, 2, 3))

par <- kamp_backup[grep("GROUP", kamp_backup$group), c("player", "match", "guessed_winner_no", "guess1", "guess2")]

for(i in unique(par$player)){
  par[,paste0(i,"fasit1")]               <- par$guess1[which(par$player==i)]
  par[,paste0(i,"fasit2")]               <- par$guess2[which(par$player==i)]
  par[,paste0(i,"fasit_guessed_winner")] <- par$guessed_winner_no[which(par$player==i)]
  
  
  par[paste0(i, "fasitavvik1")] <- (par$guess1 - par[, paste0(i, "fasit1")])^2
  par[paste0(i, "fasitavvik2")] <- (par$guess2 - par[, paste0(i, "fasit2")])^2
  
  par[paste0(i, "alt_score")] <- ifelse(par$guessed_winner_no==par[, paste0(i, "fasit_guessed_winner")],
                                   20 - par[, paste0(i, "fasitavvik1")] - par[, paste0(i, "fasitavvik2")],
                                   0 - par[, paste0(i, "fasitavvik1")] - par[, paste0(i, "fasitavvik2")])
  
  par[paste0(i, "alt_score")] <- par[paste0(i, "alt_score")]/20
  
  par[which(par$player==i), paste0(i, "alt_score")] <- NA
  
  
}
par[,grep("fasit", colnames(par))] <- NULL
coefs <- grep("alt_score", colnames(par))
par <- par %>% group_by(player) %>% summarise_at(coefs, mean, na.rm=TRUE)
distances <- round(data.frame(par[, 2:ncol(par)]),2)
diag(distances) <- 1
rownames(distances) <- par$player
colnames(distances) <- par$player


#
##Kampenighet
#kamp_backup <- kampoversikt
#cormat <- kampoversikt[grep("GROUP", kampoversikt$group), c("player", "match", "guess1", "guess2")]
#cormat$match <- as.factor(cormat$match)
#cormat <- data.frame(cormat)
##Reshape
#l <- reshape(cormat,
#             varying = c("guess1", "guess2"),
#             v.names = c("guess"),
#             idvar = "player",
#             timevar = "team",
#             new.row.names = seq(1, nrow(cormat)*2, 1),
#             direction="long")
#
#
##function
#matchdistance <- function(sub){
#  
#  temp            <- sub[which(sub$team==1), c("player", "match", "guess")]
#  temp            <- cbind("player"=rep(temp$player, each=nrow(temp)), "match"=rep(temp$match, each=nrow(temp)), expand.grid(temp$guess, temp$guess), "player2"=rep(temp$player, nrow(temp)))
#  temp            <- temp[which(temp$player!=temp$player2),]
#  temp$difference <- (temp$Var2 - temp$Var1)^2
#  temp1           <- temp %>% group_by(player, match) %>% summarise("diff" = mean(difference, na.rm=TRUE))
#  #aggregate(temp$difference, list(temp$player, temp$match), na.action = na.omit)
#  
#  #
#  temp            <- sub[which(sub$team==2), c("player", "match", "guess")]
#  temp            <- cbind("player"=rep(temp$player, each=nrow(temp)), "match"=rep(temp$match, each=nrow(temp)), expand.grid(temp$guess, temp$guess), "player2"=rep(temp$player, nrow(temp)))
#  temp            <- temp[which(temp$player!=temp$player2),]
#  temp$difference <- (temp$Var2 - temp$Var1)^2
#  temp2           <- temp %>% group_by(player, match) %>% summarise("diff" = mean(difference,na.rm=TRUE))
#  
#  temp            <- rbind(temp1, temp2)
#  temp            <- temp %>% group_by(player, match) %>% summarise("difference" = mean(diff,na.rm=TRUE))
#  
#  return(temp)
#}
#
##Find differences
##sub <- l[which(l$match==2),]
#difflist <- lapply(1:max(as.numeric(l$match)), function(x) matchdistance(l[which(l$match==x), ]))
#difflist <- do.call("rbind", difflist)
#colnames(difflist) <- c("player", "match", "goal_agreeableness")
#difflist$goal_agreeableness <- round(difflist$goal_agreeableness, 2)
#
#kampoversikt <- merge(kampoversikt, difflist, by = c("player","match"), all=TRUE)
# #
# 
# #####   Uenighetsmatrise
# par <- kamp_backup[grep("GROUP", kamp_backup$group), c("player", "match", "guess1", "guess2")]
# 
# #Reshape
# par <- reshape(par,
#                varying = c("guess1", "guess2"),
#                v.names = c("guess"),
#                idvar = "player",
#                timevar = "team",
#                new.row.names = seq(1, nrow(kamp_backup)*2, 1),
#                direction="long")
# 
# 
# #####
# pardistance <- function(sub){
#   
#   temp            <- sub[which(sub$team==1), c("player", "match", "guess")]
#   temp            <- cbind("player"=rep(temp$player, each=nrow(temp)), "match"=rep(temp$match, each=nrow(temp)),
#                            expand.grid(temp$guess, temp$guess), "player2"=rep(temp$player, nrow(temp)))
#   temp            <- temp[which(temp$player!=temp$player2),]
#   temp$difference <- (temp$Var2 - temp$Var1)^2
#   temp1           <- temp
#   temp1$team      <- 1
#   
#   #
#   temp            <- sub[which(sub$team==2), c("player", "match", "guess")]
#   temp            <- cbind("player"=rep(temp$player, each=nrow(temp)), "match"=rep(temp$match, each=nrow(temp)),
#                            expand.grid(temp$guess, temp$guess), "player2"=rep(temp$player, nrow(temp)))
#   temp            <- temp[which(temp$player!=temp$player2),]
#   temp$difference <- (temp$Var2 - temp$Var1)^2
#   temp2           <- temp
#   temp2$team      <- 2
#   temp            <- rbind(temp1, temp2)
#   
#   return(temp)
# }
# 
# parlist <- lapply(1:max(as.numeric(par$match)), function(x) pardistance(par[which(par$match==x), ]))
# test <- do.call("rbind", parlist)
# test$playerno  <- as.numeric(as.factor(as.character(test$player)))
# test$player2no <- as.numeric(as.factor(as.character(test$player2)))
# test <- test[which(test$playerno<test$player2no),]
# test$duo <- paste(test$player, "-", test$player2)
# test2 <- test %>% group_by(duo) %>% summarise(diff = mean(difference, na.rm=TRUE))
# 
# 
# vec2symmat <- function(invec, diag = 1, byrow = TRUE) {
#   Nrow <- ceiling(sqrt(2*length(invec)))
#   
#   if (!sqrt(length(invec)*2 + Nrow) %% 1 == 0) {
#     stop("invec is wrong length to create a square symmetrical matrix")
#   }
#   
#   mempty <- matrix(0, nrow = Nrow, ncol = Nrow)
#   mindex <- matrix(sequence(Nrow^2), nrow = Nrow, ncol = Nrow, byrow = byrow)
#   if (isTRUE(byrow)) {
#     mempty[mindex[lower.tri(mindex)]] <- invec
#     mempty[lower.tri(mempty)] <- t(mempty)[lower.tri(t(mempty))]
#   } else {
#     mempty[mindex[upper.tri(mindex)]] <- invec
#     mempty[lower.tri(mempty)] <- t(mempty)[lower.tri(t(mempty))]
#   }
#   
#   diag(mempty) <- diag
#   mempty
# }
# 
# distances <- data.frame(vec2symmat(round(test2$diff,2)))
# rownames(distances) <- levels(test$player)
# colnames(distances) <- levels(test$player)
# 
# #save(distances, file="www/uenighetsmatrise.rdata")
# 
# 
