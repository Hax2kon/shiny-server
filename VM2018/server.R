#The R-codes
library(shiny)
library(rgeos)
library(maptools)
library(DT)
library(ggplot2)



shinyServer(function(input, output) {
  
  #####     \\    -- Preperations     --      //      #####
  source("www/get_results.R")
  source("www/calculate_score.R")
  kampoversikt$`Match Nr.` <- as.character(kampoversikt$`Match Nr.`)
  kampoversikt  <- kampoversikt[order(kampoversikt$Kamptidspukt),]
  kampoversikt$`Avvik 1` <- ifelse(is.na(kampoversikt$`Forventet vinner`)==TRUE, 0, kampoversikt$`Avvik 1`)
  #kampoversikt3 <- kampoversikt[which(is.na(kampoversikt$`Avvik 1`)==FALSE), ]
  kampoversikt3 <- kampoversikt
  
  gruppescoreboard <- gruppe %>% group_by(player) %>% summarise("Score"=sum(score))
  
  
  #Today's match
  # today <- kampoversikt
  # today <- today[,c("Match Nr.", "Kamptidspukt", "Spiller", "Reelt lag 1", "Gjett lag 1", "Gjett lag 2", "Reelt lag 2", 
  #                                                 "Forventet vinner", "Resultat lag 1", "Resultat lag 2", "Enighet", "Score")]
  # today <- today[order(today$`Match Nr.`),]
  # todaytemp <- today[!duplicated(today[,c("Reelt lag 1", "Reelt lag 2")]),]
  # todaytemp$Spiller <- ""
  # todaytemp$`Gjett lag 1` <- todaytemp$`Resultat lag 1`
  # todaytemp$`Gjett lag 2` <- todaytemp$`Resultat lag 2`
  # todaytemp$Enighet <- ""
  # todaytemp$`Forventet vinner` <- ""
  # todaytemp$Score <- ""
  # today <- rbind(today, todaytemp)
  # today <- today[order(today$`Match Nr.`, today$Spiller),]
  # today$`Resultat lag 1` <- NULL
  # today$`Resultat lag 2` <- NULL
  
  today <- oversikt_kvart
  today <- today[order(today$match),]
  todaytemp <- today[!duplicated(today[,c("real_team1", "real_team2")]),]
  todaytemp$player <- ""
  todaytemp$guess1 <- todaytemp$res1
  todaytemp$guess2 <- todaytemp$res2
  todaytemp$guessed_winner <- ""
  todaytemp$score <- ""
  today <- rbind(today, todaytemp)
  today <- today[order(today$match, today$player),]
  today$res1 <- NULL
  today$res2 <- NULL
  today$time <- as.character(today$time)
  today <- today[, c("match", "time", "player", "real_team1", "guess1", "guess2", "real_team2", "guessed_winner", "score")]
  runde2tabell <- today
  today <- today[which(format(as.Date(today$time), "%Y-%m-%d") == Sys.Date()), ]
  colnames(today)        <- c("Match Nr.", "Kamptidspunkt", "Spiller", "Lag 1", "Mål lag 1", "Mål lag 2", "Lag 2", "Vinner", "Score")
  colnames(runde2tabell) <- c("Match Nr.", "Kamptidspunkt", "Spiller", "Lag 1", "Mål lag 1", "Mål lag 2", "Lag 2", "Vinner", "Score")
  # 
  # 
  # #Next match
  # one_hour <- 60*60
  # time_now <- Sys.time()+one_hour+one_hour
  # next_match       <- kampoversikt
  # next_match$until <- difftime(as.POSIXct(next_match$Kamptidspukt), time_now-one_hour-one_hour, units = "hours")
  # next_match       <- next_match[which(next_match$until>0),]
  # next_match       <- next_match[which(next_match$until==min(next_match$until)), ]
  # next_match$until <- next_match$until - 2
  # next_match       <- next_match[order(next_match$Spiller),]
  # 
  # temp <- unique(next_match$until)
  # timestamp <- paste0(gsub("\\..*", "",temp), "T ",
  #                     round(as.numeric(paste0("0.",gsub(".*\\.", "",temp)))*60, 0), "M")
  # 
  # next_match <- today[which(today$`Match Nr.` %in% unique(next_match$`Match Nr.`)),]
  # next_match$Kamptidspukt[seq(1, nrow(next_match), length(unique(kampoversikt$Spiller))+1)] <- timestamp
  # next_match$Score[seq(1, nrow(next_match), length(unique(kampoversikt$Spiller))+1)]   <- "Foreløpig score"
  # next_match$Enighet[seq(1, nrow(next_match), length(unique(kampoversikt$Spiller))+1)] <- "Enighet"
  # 
  
  #Reduce todayframe
  #today$day <- format(as.Date(today$Kamptidspukt), "%Y-%m-%d")
  #today <- today[which(format(as.Date(today$Kamptidspukt), "%Y-%m-%d") == Sys.Date()), ]
  
  #####     Scoreboard, Runde 2    #####
  scoreboard_runde2 <- oversikt_runde2 %>% group_by(player) %>% summarise("Score" = sum(score, na.rm=TRUE))
  colnames(scoreboard_runde2) <- c("Spiller", "Score")
  
  #####     Scoreboard, Kvarten    #####
  scoreboard_kvart <- oversikt_kvart %>% group_by(player) %>% summarise("Score" = sum(score, na.rm=TRUE))
  colnames(scoreboard_runde2) <- c("Spiller", "Score")
  
  #####     \\      --      Display     --      //      #####
  # choose columns to display
  output$kampoversikt2 <- DT::renderDataTable({
    DT::datatable(kampoversikt, filter="top", rownames = FALSE, 
                  options = list(pageLength = 320, scrollX=TRUE))
  })
  
  #Scoreboard
  output$scoreboard2 <- DT::renderDataTable({
    DT::datatable(scoreboard[order(scoreboard$Score, decreasing=TRUE),], options = list(orderClasses = TRUE), filter="top")
  })
  
  #Scoreboard runde2
  output$scoreboard_runde2_2 <- DT::renderDataTable({
    DT::datatable(scoreboard_runde2[order(scoreboard_runde2$Score, decreasing=TRUE),], options = list(orderClasses = TRUE), filter="top")
  })
  
  #Scoreboard kvart
  output$scoreboard_kvart2 <- DT::renderDataTable({
    DT::datatable(scoreboard_kvart[order(scoreboard_kvart$Score, decreasing=TRUE),], options = list(orderClasses = TRUE), filter="top")
  })
  
  #today
  output$todays <- DT::renderDataTable({
    DT::datatable(today, rownames = FALSE, options = list(orderClasses = TRUE, pageLength = 320)) %>%
      formatStyle("Spiller",
        target = "row",
        fontWeight = styleEqual("", "bold"),
        background = styleEqual("", "#B0BED9"))
  })
  
  output$upcoming <- DT::renderDataTable({
    DT::datatable(next_match, colnames = rep("", ncol(next_match)), rownames = rep("", nrow(next_match)),
                  options = list(dom='t',ordering=F, pageLength = 320)) %>%
      formatStyle("Spiller",
                  target = "row",
                  fontWeight = styleEqual("", "bold"),
                  background = styleEqual("", "#B0BED9"))
  })
  
  
  #####     Alle kamper, Runde 2    #####
  output$runde2tabell2 <- DT::renderDataTable({
    DT::datatable(runde2tabell, rownames = FALSE, options = list(orderClasses = TRUE, pageLength = 320)) %>%
      formatStyle("Spiller",
                  target = "row",
                  fontWeight = styleEqual("", "bold"),
                  background = styleEqual("", "#B0BED9"))
  })
  
  # 
  # #Uenighet
  # output$uenighet <- DT::renderDataTable({
  #   DT::datatable(distances)
  # })
  #Resultat - Gruppespillet
  output$res_gruppe <- DT::renderDataTable({
    DT::datatable(gruppescoreboard[order(gruppescoreboard$Score, decreasing = TRUE),])
  })
  
  
  #Poengutvikling
  output$plot1 <- renderPlot({
      ggplot(kampoversikt3, aes(as.numeric(`Match Nr.`), Score, colour = Spiller)) + 
      scale_x_continuous(limits = c(1, 64)) +
      geom_point() +
      geom_line(aes(y=Kumulativt)) +
      labs(x = "Kampnummer") +
      theme_minimal()
      
  })

  output$info <- renderDataTable({
    brushedPoints(kampoversikt3, input$plot_brush, xvar = "Kamptidspukt", yvar = "Score")
  }, 
  options = list(pageLength = 320))
  
  
  
  #Make clock
  output$currentTime <- renderText({
    # invalidateLater causes this output to automatically
    # become invalidated when input$interval milliseconds
    # have elapsed
    invalidateLater(1000)
    format(Sys.time()+60*60+60*60, format="%H:%M:%S")
  })
}
)

