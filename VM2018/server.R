#The R-codes
library(shiny)
library(rgeos)
library(maptools)
library(DT)
library(ggplot2)



shinyServer(function(input, output) {
  
  #####     \\    -- Preperations     --      //      #####
  load("www/df.rdata")
  source("www/get_results.R")
  source("www/calculate_score.R")
  kampoversikt$`Match Nr.` <- as.character(kampoversikt$`Match Nr.`)
  kampoversikt  <- kampoversikt[order(kampoversikt$Kamptidspukt),]
  kampoversikt$`Avvik 1` <- ifelse(is.na(kampoversikt$`Forventet vinner`)==TRUE, 0, kampoversikt$`Avvik 1`)
  kampoversikt3 <- kampoversikt[which(is.na(kampoversikt$`Avvik 1`)==FALSE), ]
  
  
  
  #Today's match
  today <- kampoversikt
  today <- today[,c("Match Nr.", "Kamptidspukt", "Spiller", "Reelt lag 1", "Gjett lag 1", "Gjett lag 2", "Reelt lag 2", 
                                                  "Forventet vinner", "Resultat lag 1", "Resultat lag 2", "Enighet", "Score")]
  today <- today[order(today$`Match Nr.`),]
  todaytemp <- today[!duplicated(today[,c("Reelt lag 1", "Reelt lag 2")]),]
  todaytemp$Spiller <- ""
  todaytemp$`Gjett lag 1` <- todaytemp$`Resultat lag 1`
  todaytemp$`Gjett lag 2` <- todaytemp$`Resultat lag 2`
  todaytemp$Enighet <- ""
  todaytemp$`Forventet vinner` <- ""
  todaytemp$Score <- ""
  today <- rbind(today, todaytemp)
  today <- today[order(today$`Match Nr.`, today$Spiller),]
  today$`Resultat lag 1` <- NULL
  today$`Resultat lag 2` <- NULL
  
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
  
  next_match <- today[which(today$`Match Nr.` %in% unique(next_match$`Match Nr.`)),]
  next_match$Kamptidspukt[seq(1, nrow(next_match), length(unique(kampoversikt$Spiller))+1)] <- timestamp
  next_match$Score[seq(1, nrow(next_match), length(unique(kampoversikt$Spiller))+1)]   <- "Foreløpig score"
  next_match$Enighet[seq(1, nrow(next_match), length(unique(kampoversikt$Spiller))+1)] <- "Enighet"
  
  
  #Reduce todayframe
  #today$day <- format(as.Date(today$Kamptidspukt), "%Y-%m-%d")
  today <- today[which(format(as.Date(today$Kamptidspukt), "%Y-%m-%d") == Sys.Date()), ]
  
  
  
  #####     \\      --      Display     --      //      #####
  # choose columns to display
  output$kampoversikt2 <- DT::renderDataTable({
    DT::datatable(kampoversikt[ ,input$show_vars, drop = FALSE], filter="top", rownames = FALSE, 
                  options = list(pageLength = 320))
  })
  
  #Scoreboard
  output$scoreboard2 <- DT::renderDataTable({
    DT::datatable(scoreboard[order(scoreboard$Score, decreasing=TRUE),], options = list(orderClasses = TRUE), filter="top")
  })
  
  #Scoreboard
  output$todays <- DT::renderDataTable({
    DT::datatable(today, rownames = FALSE, options = list(orderClasses = TRUE, pageLength = 320)) %>%
      formatStyle("Spiller",
        target = "row",
        fontWeight = styleEqual("", "bold"),
        background = styleEqual("", "#B0BED9"))
  })
  
  #Next match
  # one_hour <- 60*60
  # time_now <- Sys.time()+one_hour+one_hour
  # next_match$until <- difftime(as.POSIXct(next_match$Kamptidspukt), time_now-one_hour-one_hour, units = "hours")
  # next_match$until <- next_match$until - 2
  # temp <- unique(next_match$until)
  # timestamp <- paste0(gsub("\\..*", "",temp), "T ",
  #                     round(as.numeric(paste0("0.",gsub(".*\\.", "",temp)))*60, 0), "M")
  # 
  
  #Reactive winner
  #liveWinner <- reactive({
  #  ifelse(as.numeric(as.character(input$live1)) > as.numeric(as.character(input$live2)), next_match$`Reelt lag 1`, 
  #         ifelse(as.numeric(as.character(input$live1)) < as.numeric(as.character(input$live2)), next_match$`Reelt lag 2`,
  #                "Uavgjort"))
  #})
  #
  ##Reactive avvik
  #liveAvvik1 <- reactive({
  #  (as.numeric(next_match$`Gjett lag 1`) - as.numeric(as.character(input$live1)))^2
  #  
  #})
  #liveAvvik2 <- reactive({
  #  (as.numeric(next_match$`Gjett lag 2`) - as.numeric(as.character(input$live2)))^2
  #  
  #})
  ##Reactive scores
  #liveScores <- reactive({
  #  ifelse(next_match$`Forventet vinner`==liveWinner(),
  #         20 - liveAvvik1() - liveAvvik2(),
  #         0 - liveAvvik1() - liveAvvik2())
  #})
  #
  #header <- reactive({
  #  data.frame(rbind(
  #    c(""                      , "", timestamp, "", "", ""),
  #    c(""                      , unique(next_match$`Reelt lag 1`), "-", unique(next_match$`Reelt lag 2`), "", ""),
  #    c("Stilling:"             , input$live1, "-", input$live2, "Enighet", "Foreløpig score"),
  #    cbind(next_match$Spiller  , next_match$`Gjett lag 1`, rep("-", length(next_match$Spiller)), next_match$`Gjett lag 2`, next_match$Enighet, liveScores())))
  #  
  #})
    
  output$upcoming <- DT::renderDataTable({
    DT::datatable(next_match, colnames = rep("", ncol(next_match)), rownames = rep("", nrow(next_match)),
                  options = list(dom='t',ordering=F, pageLength = 320)) %>%
      formatStyle("Spiller",
                  target = "row",
                  fontWeight = styleEqual("", "bold"),
                  background = styleEqual("", "#B0BED9"))
  })
  
  
  #Uenighet
  output$uenighet <- DT::renderDataTable({
    DT::datatable(distances)
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

