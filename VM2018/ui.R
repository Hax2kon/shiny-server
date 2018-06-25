
##  Layout  ##
library(shiny);library(DT);library(ggplot2)

fluidPage(theme = "bootstrap.css",
  title = "VM 2018",
  sidebarLayout(
    sidebarPanel(width = 1.5,
      conditionalPanel(
        'input.dataset === "Kampoversikt"',
        checkboxGroupInput("show_vars", label = h3("Kolonner:"), 
                           choices = names(kampoversikt), selected = names(kampoversikt))
      ),
      conditionalPanel(
        'input.dataset === "Scoreboard"',
        helpText(h4("Foreløpig vinneroversikt"))
      ),
      conditionalPanel(
        'input.dataset === "next"',
        textInput("live1", label=h4("Mål lag 1:"), value = unique(next_match$`Resultat lag 1`)),
        textInput("live2", label=h4("Mål lag 2:"), value = unique(next_match$`Resultat lag 2`)),
        helpText(h4("Klokke: "), h4(textOutput("currentTime")))
      ),
      conditionalPanel(
        'input.dataset === "uenighet"',
        helpText(h4("Dette er et mål på hvor enige spillerne er i gruppespillet."),
                 h4("Den er basert på hvor mange poeng spillerne får gitt at de andres"),
                 h4("tipp blir resultatet."),
                 h4("Maksverdi er 1, som betyr at man får 20 poeng under alle de andres tipp"),
                 h4("Minste mulige verdi er -1, som betyr at man får -20 poeng under alle de andres tipp"))
      ),
      conditionalPanel(
        'input.dataset === "cumu"',
        helpText(h4("Denne grafen viser utviklingen"),
                 h4("i spillernes totalscore (linjer),"),
                 h4("og poengene fra enkeltkampene (prikker)."),
                 h4("Marker områder i figuren for å se"),
                 h4("informasjon om kampene bak punktene"))
      ),
      conditionalPanel(
        'input.dataset === "today"',
        helpText(h4("Dagens kamper"))
      )
    ),
    mainPanel( #width = 9.5,
      tabsetPanel(
        id = 'dataset',
        tabPanel(h3("Dagens kamper")     ,value="today"         , div(DT::dataTableOutput("todays")     , style = "font-size:150%")),
        tabPanel(h3("Neste kamp")        ,value="next"          , div(DT::dataTableOutput("upcoming")   , style = "font-size:150%")),
        tabPanel(h3("Scoreboard")        ,value="Scoreboard"    , div(DT::dataTableOutput("scoreboard2"), style = "font-size:150%")),
        tabPanel(h3("Enighetsmatrise")   ,value="uenighet"      , div(DT::dataTableOutput("uenighet")   , style = "font-size:150%")),
        tabPanel(h3("Kampoversikt")      ,value="Kampoversikt"  , DT::dataTableOutput("kampoversikt2")),
        tabPanel(h3("Poengutvikling")    ,value="cumu"          ,
                 plotOutput("plot1", brush = "plot_brush"),
                 DT::dataTableOutput("info")
                 )
        
      )
    )
  )
)
