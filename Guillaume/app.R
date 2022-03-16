#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(DT)
library(tidyverse)
library(tidyr)
library(questionr)
library(leaflet)
library(shinythemes)
library(questionr)

df <- read_csv('vgsales2.csv')
df_bis<-read_csv("vgsales.csv")
df2 <- readRDS('data_jeux.rds')
df_image<-readRDS("data_jeux_image.rds")

df_image$image<-str_replace(df_image$image, 'src="/images', 'src="https://www.mobygames.com/images')


df <- df  %>% 
  mutate(Platform = as.factor(Platform)) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Genre = as.factor(Genre)) %>% 
  mutate(Publisher = as.factor(Publisher))
df$Name<-as.character(df$Name)


df2 <- df2  %>% 
  mutate(platform = as.factor(platform)) %>% 
  mutate(Year = as.factor(Annee)) %>%
  mutate(Publisher = as.factor(Publisher))

data_NA = df_bis[,c(1:6, 7, 10:11)]
data_NA <- rename.variable(data_NA, "NA_Sales", "Sales")
data_NA$country = "Canada"

data_EU = df_bis[,c(1:6, 8, 10:11)]
data_EU <- rename.variable(data_EU, "EU_Sales", "Sales")
data_EU$country = "Austria"

data_jpn = df_bis[,c(1:6, 9:11)]
data_jpn <- rename.variable(data_jpn, "JP_Sales", "Sales")
data_jpn$country = "Japan"

data_2 <- rbind(data_EU,data_NA, data_jpn)
data_2 <- rename.variable(data_2, "country", "name" )

country<-read.csv("countries.csv", header=T)

data_complet<- inner_join(data_2, country, "name")
data_complet$name<-str_replace(data_complet$name,"Austria","Europe")
data_complet$name<-str_replace(data_complet$name,"Canada","North America")

data_complet<-data_complet%>%
  mutate(popup_info=paste("Region:",
                          name,
                          "<br/>",
                          "Jeu:",
                          Name,
                          "<br/>",
                          "Nombres de ventes:",
                          Sales,
                          "Millions" ))


# Define UI for application that draws a histogram
ui <- fluidPage(  tags$head(
  tags$style(HTML("hr {border-top: 1px solid #000000;}"))
),

# Application title
titlePanel("Video Games"),
## Page
navbarPage("Page",
           tabPanel("Graphiques/ Statistiques",
                    tabsetPanel(
                      tabPanel("Par nombre de jeux",
                               sidebarLayout(
                                 sidebarPanel(
                                   sliderInput("years",
                                               "Choix de l'année :",
                                               min = 1950,
                                               max = 2022,
                                               value = 2021)
                                   ,
                                   checkboxInput("Licence", "Jeux sous licence (Oui si coché)", value=FALSE),
                                   radioButtons("var", 
                                                label = "Choix de la variable:",
                                                choices = list("Concepteur" = "Publisher", "Console" = "platform"), 
                                                selected = "Publisher"),
                                   hr(),
                                   h4("Cliquez sur le graphique pour faire votre choix particulier :"),
                                   textInput("choixui",
                                             label=paste("Nom du choix (ne pas remplir):")
                                   ),
                                   textInput("choixui2",
                                             "Note moyenne (des jeux notés) du choix particulier:"),
                                 ),
                                 
                                 # Show a plot of the generated distribution
                                 mainPanel(
                                   h4(strong("Representation graphique:")),
                                   plotlyOutput("Hist", width = "700px", height = "300px"),
                                   h4(strong("Repartition des types de jeux en fonction du choix particulier:")),
                                   plotlyOutput("Radar", width = "400px", height = "300px")
                                   #DTOutput("myTable"),
                                 )
                               )
                      ),
                      tabPanel("Par ventes",
                                 fluidRow(column(width=6,
                                   sliderInput("years2",
                                               label= h3("Choix de l'année:"),
                                               min = 1980,
                                               max = 2017,
                                               value = 2016)),
                                   column(width=6,
                                   radioButtons("var2", 
                                                label = h3("Choix de la variable:"),
                                                choices = list(
                                                  "Concepteur" = "Publisher", 
                                                  "Genre" = "Genre", 
                                                  "Video Games" = "Name", 
                                                  "Plateforme" = "Platform"
                                                ), 
                                                selected = "Name")
                                 )),
                                 h3("Representation graphique"),
                                 # Show a plot of the generated distribution
                                 fluidRow(column(width=5,
                                   plotlyOutput("SalesHist", width = "500px", height = "300px")),
                                   column(width=4,
                                   plotlyOutput("SalesHist2", width= "500px", height= "300px"))),
                      ),
                      tabPanel("Cartographie de ventes",
                               selectInput("Nom", label=h4("Choix du jeu"), multiple = FALSE, choices = character(0)),
                               selectInput("plateform2", label = h4("Choix de la plateform"), 
                                           
                                           choices = c("--", as.list(levels(df_bis$Platform))),
                                           selected = "NES"),
                               DTOutput("Table"),
                               leafletOutput("map_vente"))
                      
                    )
           ),  
           tabPanel("Recommandation",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("type_recommandation",
                                     label = h3("Préférence par :"),
                                     choices = list("Popularité" = "Popularite", "Ventes" = "Vente"), 
                                     selected = "Popularite", 
                                     inline = TRUE),
                        sliderInput("years3",
                                    "Annee :",
                                    min = 1980,
                                    max = 2017,
                                    value = c(1980,2017)),
                        
                        selectizeInput("plateform1", 
                                       label = h3("Plateform"),
                                       choices = c(levels(df2$Platform)),
                                       multiple = TRUE,
                                       options = list(dropdownParent = 'body', 
                                                      maxitems = 'null', 
                                                      plugins = list("remove_button"),
                                                      highlight = F,
                                                      closeAfterSelect = T)
                        ),
                        selectizeInput("genre1",
                                       label = h3('Genre'),
                                       choices = c(
                                         "Action",
                                         "Aventure",
                                         "Puzzle",
                                         "Racing",
                                         "Educational",
                                         "Compilation",
                                         "Simulation",
                                         "Sports",
                                         "Strategie",
                                         "Role_play",
                                         "Edition_special"),
                                       multiple = TRUE,
                                       options = list(dropdownParent = 'body', 
                                                      maxitems = 'null', 
                                                      plugins = list("remove_button"),
                                                      highlight = F,
                                                      closeAfterSelect = T)
                        )
                      ),
                      mainPanel(
                        DTOutput("Tablo"),
                        htmlOutput("image"),
                        htmlOutput('desc'),
                        uiOutput("lienyt"),
                      )
                    ),
                    fluidRow(
                      column(4, 
                             # htmlOutput("image")
                      ),
                      column(8,
                             #htmlOutput('desc'),
                             #verbatimTextOutput('desc2')
                      )
                    )
           ),
           tabPanel("Recherche", 
                    selectInput("Nom2", "Choix du jeu", multiple = FALSE,
                                choices =c('Choisit un jeu' = "", character(0)),
                                selected=NULL),
                    uiOutput("tab")
           )
),
theme = bslib::bs_theme(bootswatch = "united"),
)
# Sidebar with a slider input for number of bins 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  thematic::thematic_shiny()
  ############################## Page recommendation ####################"
  
  observe({
    input$plateform1
    
    if(input$type_recommandation == "Vente"){
      if(is.null(input$plateform1) == FALSE){
        minimum <- min(na.omit(as.numeric(as.character(df$Year[df$Platform==input$plateform1]))))
        maximum <- max(na.omit(as.numeric(as.character(df$Year[df$Platform==input$plateform1]))))
      }else{
        minimum <- 1980
        maximum <- 2017
      }
      updateSliderInput(
        inputId = "years3",
        min = minimum,
        max = maximum,
        value = c(minimum,maximum)
      )
    }
    if(input$type_recommandation == "Popularite"){
      if(is.null(input$plateform1) == FALSE){
        minimum <- min(na.omit(as.numeric(as.character(df2$Annee[df2$platform==input$plateform1]))))
        maximum <- max(na.omit(as.numeric(as.character(df2$Annee[df2$platform==input$plateform1]))))
      }else{
        minimum <- 1950
        maximum <- 2022
      }
      updateSliderInput(
        inputId = "years3",
        min = minimum,
        max = maximum,
        value = c(minimum,maximum)
      )
    }
  })
  
  observe({
    input$type_recommandation
    
    if(input$type_recommandation == "Vente"){
      
      updateSliderInput(
        inputId = "years3",
        min = 1980,
        max = 2017,
        value = c(1980,2017)
      )
      
      updateSelectizeInput(
        inputId = "plateform1",
        choices = c(levels(df$Platform)))
      
      updateSelectizeInput(inputId = "genre1",
                           choices = c(levels(df$Genre)))
    }
    if(input$type_recommandation == "Popularite"){
      updateSliderInput(
        inputId = "years3",
        min = 1950,
        max = 2022,
        value = c(1950,2022)
      )
      
      updateSelectizeInput(
        inputId = "plateform1",
        choices = c(levels(df2$platform))
      )
      
      updateSelectizeInput(inputId = "genre1",
                           choices = c(
                             "Action" = "Action",
                             "Aventure" = "Aventure",
                             "Puzzle" = "Puzzle",
                             "Course" = "Racing",
                             "Education" = "Educational",
                             "Compilation" = "Compilation",
                             "Simulation" = "Simulation",
                             "Sports" = "Sports",
                             "Strategie" = "Strategie",
                             "Role play" = "Role_play",
                             "Edition special" = "Edition_special",
                             "DLC" = "DLC"))
    }
  })
  
  #############################################################################################
  ###################################### REACTIVE #############################################
  #############################################################################################
  
  tablo <- reactive({
    
    ####### PAR SALES ########
    if(input$type_recommandation == "Vente"){
      
      image <- paste0("<img src=\"", src="https://www.mobygames.com/images/covers/s/264995-air-hockey-android-front-cover.jpg", "\" height=\"30\" data-toggle=\"tooltip\" data-placement=\"right\" title=\"", "bijo", "\"></img>")
      
      tablo <- df %>%
        filter(as.numeric(as.character(Year)) >= input$years3[1]) %>%
        filter(as.numeric(as.character(Year)) <= input$years3[2])
      
      if(is.null(input$plateform1) == FALSE){
        tablo <- tablo %>% 
          filter(Platform == input$plateform1)
      }
      if(is.null(input$genre1) == FALSE){
        tablo <- tablo %>% 
          filter(Genre == input$genre1)
      }
      best_choice_tableau <- tablo %>% 
        slice(1:7)
      #mutate(picture = c(image, image, image)) %>% 
    }
    
    ###############PAR POPULARITE############
    if(input$type_recommandation == "Popularite"){
      
      tablo <-df2 %>%
        filter(as.numeric(as.character(Annee)) >= input$years3[1]) %>%
        filter(as.numeric(as.character(Annee)) <= input$years3[2])
      
      if(is.null(input$plateform1) == FALSE){
        tablo <- tablo %>% 
          filter(platform == input$plateform1)
      }
      
      if(is.null(input$genre1) == FALSE){
        tablo <- tablo %>% 
          filter(tablo[input$genre1] %>% rowSums() > 0)
      }
      
      best_choice_tableau <- tablo %>% 
        arrange(reviews) %>% 
        slice(1:7)
    }
    best_choice_tableau
    
  })
  ################### Page Nombre #######################################
  
  variable <- reactive({switch(input$var,
                               "Publisher" = df2$Publisher, 
                               #"Genre" = df2$Genre,
                               "platform" = df2$platform)
  })
  
  variable2 <- reactive({switch(input$var2,
                                "Publisher" = df$Publisher, 
                                "Genre" = df$Genre,
                                "Platform" = df$Platform,
                                "Name" = df$Name)
  })
  
  
  output$Hist <- renderPlotly({
    if(input$Licence==FALSE){
      hist<-fct_count(variable()[df2$Year == input$years], sort = T) %>%
        slice(1:10)  %>%
        plot_ly(
          source = "myClickSource",
          x =  ~ n,
          y =  ~ fct_reorder(f, n),
          type = "bar",
          orientation = "h") %>% layout(title=paste("Nombre de jeu par", input$var),
                                        xaxis= list(title="Nombre de jeux"),
                                        yaxis = list(title=input$var)) %>%
        config(displayModeBar=FALSE)
    }
    if(input$Licence==TRUE){
      hist<-fct_count(variable()[df2$Year == input$years & df2$Licence==1], sort = T) %>%
        slice(1:10)  %>%
        plot_ly(
          source = "myClickSource",
          x =  ~ n,
          y =  ~ fct_reorder(f, n),
          type = "bar",
          orientation = "h") %>% layout(title=paste("Nombre de jeu par", input$var),
                                        xaxis= list(title="Nombre de jeux"),
                                        yaxis = list(title=input$var)) %>%
        config(displayModeBar=FALSE)
    }
    hist
  })
  
  SelectedBar <- reactiveVal(NULL)
  
  observe({
    if(input$Licence==FALSE){
      myClicks <- event_data("plotly_click", source = "myClickSource")
      req(myClicks)
      #print(myClicks)
      SelectedBar(myClicks$y)
      #print(SelectedBar)
      updateTextInput(session, "choixui", value = SelectedBar())
      updateTextInput(session,
                      "choixui2",
                      value = paste(round(mean(df2$reviews[!is.na(df2$reviews) & variable()==SelectedBar() & df2$Year==input$years]),3), "/100")
      )
    }
    if(input$Licence==TRUE){
      myClicks <- event_data("plotly_click", source = "myClickSource")
      req(myClicks)
      #print(myClicks)
      SelectedBar(myClicks$y)
      #print(SelectedBar)
      updateTextInput(session, "choixui", value = SelectedBar())
      updateTextInput(session,
                      "choixui2",
                      value = paste(round(mean(df2$reviews[!is.na(df2$reviews) & variable()==SelectedBar() & df2$Year==input$years & df2$Licence==1]),3), "/100")
      )
    }
  })
  

  
  output$Radar <- renderPlotly({
    
    nom<-c("Puzzle", "Aventure", "Action", "Simulation", "Racing",
           "Educational", "Compilation","Sports", "Strategie", "Role_play")
    
    if (input$var=="Publisher"){
      if(input$Licence==FALSE){
        vect<-c(sum(df2$Puzzle[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Aventure[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Action[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Simulation[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Racing[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Educational[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Compilation[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Sports[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Strategie[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Role_play[df2$Publisher==SelectedBar() & df2$Year==input$years]==1))
      }
      if(input$Licence==TRUE){
        vect<-c(sum(df2$Puzzle[df2$Publisher==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Aventure[df2$Publisher==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Action[df2$Publisher==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Simulation[df2$Publisher==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Racing[df2$Publisher==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Educational[df2$Publisher==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Compilation[df2$Publisher==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Sports[df2$Publisher==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Strategie[df2$Publisher==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Role_play[df2$Publisher==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1))
      }
    }  
    if (input$var=="platform"){
      if(input$Licence==FALSE){
        vect<-c(sum(df2$Puzzle[df2$platform==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Aventure[df2$platform==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Action[df2$platform==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Simulation[df2$platform==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Racing[df2$platform==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Educational[df2$platform==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Compilation[df2$platform==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Sports[df2$platform==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Strategie[df2$platform==SelectedBar() & df2$Year==input$years]==1),
                sum(df2$Role_play[df2$platform==SelectedBar() & df2$Year==input$years]==1))
      } 
      if(input$Licence==TRUE){
        vect<-c(sum(df2$Puzzle[df2$platform==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Aventure[df2$platform==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Action[df2$platform==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Simulation[df2$platform==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Racing[df2$platform==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Educational[df2$platform==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Compilation[df2$platform==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Sports[df2$platform==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Strategie[df2$platform==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1),
                sum(df2$Role_play[df2$platform==SelectedBar() & df2$Year==input$years & df2$Licence==1]==1))
      }
    }
    
    plot_ly(
      type="scatterpolar",
      r =  vect,
      theta = nom,
      fill = "toself") %>% layout(
        polar = list(radialaxis = list(visible = T,range = c(0,max(vect)+10))
        ),showlegend = F) %>%
      config(displayModeBar=FALSE)
    
  })
  
  ############################ Page Ventes #################################"
  
  output$SalesHist <- renderPlotly({
    df %>% 
      filter(Year == input$years2) %>% 
      select(everything()) %>% 
      group_by(variable2()[df$Year == input$years2]) %>%
      summarize(Total_sales = sum(Global_Sales)) %>%
      arrange(desc(Total_sales)) %>%
      slice(1:10) %>% 
      setNames(c("Truc", "Total_sales"))  %>%
      plot_ly(
        source = "myClickSource",
        x =  ~ Total_sales,
        y =  ~ fct_reorder(Truc, Total_sales),
        type = "bar",
        orientation = "h") %>% layout(title=paste("Nombre de ventes par", input$var2),
                                      xaxis= list(title="Ventes (Millions)"),
                                      yaxis= list(title="")) %>%
      config(displayModeBar=FALSE)
    
  })
  
  SelectedBar2 <- reactiveVal(NULL)
  
  observe({
    myClicks2 <- event_data("plotly_click", source = "myClickSource")
    req(myClicks2)
    SelectedBar2(myClicks2$y)
    print(SelectedBar2())
    
    
    output$SalesHist2 <- renderPlotly({
      if(input$var2 == "Publisher"){
        new_df<-df %>% 
          filter(Year == input$years2 & Publisher== SelectedBar2()) %>% 
          select(everything()) 
        
        hist2<-new_df %>% 
          filter(Year == input$years2) %>% 
          select(everything())%>%
          group_by(new_df$Genre[new_df$Year==input$years2])%>%
          summarize(Total_sales = sum(Global_Sales)) %>%
          arrange(desc(Total_sales)) %>%
          slice(1:10) %>% 
          setNames(c("Truc", "Total_sales"))  %>%
          plot_ly(
            y =  ~ Total_sales,
            x =  ~ fct_reorder(Truc, Total_sales),
            type = "bar") %>% layout(title=paste("Ventes par genre pour le concepteur:", SelectedBar2()),
                                     xaxis= list(title="Ventes (Millions)"),
                                     yaxis= list(title="")) %>%
          config(displayModeBar=FALSE)
        #hist2
      }
      if(input$var2 == "Platform"){
        new_df<-df %>% 
          filter(Year == input$years2 & Platform== SelectedBar2()) %>% 
          select(everything()) 
        
        hist2<-new_df %>% 
          filter(Year == input$years2) %>% 
          select(everything())%>%
          group_by(new_df$Genre[new_df$Year==input$years2])%>%
          summarize(Total_sales = sum(Global_Sales)) %>%
          arrange(desc(Total_sales)) %>%
          slice(1:10) %>% 
          setNames(c("Truc", "Total_sales"))  %>%
          plot_ly(
            y =  ~ Total_sales,
            x =  ~ fct_reorder(Truc, Total_sales),
            type = "bar") %>% layout(title=paste("Ventes par genre pour la platforme:", SelectedBar2()),
                                     xaxis= list(title="Ventes (Millions)"),
                                     yaxis= list(title="")) %>%
          config(displayModeBar=FALSE)
        #hist2
      }
      if(input$var2 == "Genre"){
        new_df<-df %>% 
          filter(Year == input$years2 & Genre== SelectedBar2()) %>% 
          select(everything()) 
        
        hist2<-new_df %>% 
          filter(Year == input$years2) %>% 
          select(everything())%>%
          group_by(new_df$Name[new_df$Year==input$years2])%>%
          summarize(Total_sales = sum(Global_Sales)) %>%
          arrange(desc(Total_sales)) %>%
          slice(1:10) %>% 
          setNames(c("Truc", "Total_sales"))  %>%
          plot_ly(
            x =  ~ Total_sales,
            y =  ~ fct_reorder(Truc, Total_sales),
            type = "bar") %>% layout(title=paste("Ventes par jeu pour le genre:", SelectedBar2()),
                                     xaxis= list(title="Ventes (Millions)"),
                                     yaxis= list(title="")) %>%
          config(displayModeBar=FALSE)
        #hist2
      }
      if(input$var2 == "Name"){
        new_df<-df %>% 
          filter(Year == input$years2 & Name== SelectedBar2()) %>% 
          select(everything()) 
        
        hist2<-new_df %>% 
          filter(Year == input$years2) %>% 
          select(everything())%>%
          group_by(new_df$Platform[new_df$Year==input$years2])%>%
          summarize(Total_sales = sum(Global_Sales)) %>%
          arrange(desc(Total_sales)) %>%
          slice(1:10) %>% 
          setNames(c("Truc", "Total_sales"))  %>%
          plot_ly(
            y =  ~ Total_sales,
            x =  ~ fct_reorder(Truc, Total_sales),
            type = "bar") %>% layout(title=paste("Ventes par platforme pour le jeu:", SelectedBar2()),
                                     xaxis= list(title="Ventes (Millions)"),
                                     yaxis= list(title="")) %>%
          config(displayModeBar=FALSE)
        #hist2
      }
      hist2
    })
  })
  
  
  ################# Page Carte ##############################################
  
  updateSelectizeInput(session, "Nom",
                       choices = c("Remplace et choisit un jeu"= "",df_bis$Name),
                       server = TRUE)
  
  observe({
    
    data_complet_2<- data_complet %>% 
      filter(Name == input$Nom)
    
    choices = unique(data_complet_2$Platform)
    
    updateSelectInput(session, "plateform2", choices= choices, selected = choices[1])
  })
  
  
  output$map_vente <- renderLeaflet({
    
    leaflet(data_complet %>% 
              filter(Name == input$Nom & Platform == input$plateform2 )) %>% addProviderTiles(providers$CartoDB.DarkMatter)  %>% 
      addCircleMarkers(~longitude, ~latitude,
                       weight = 60,
                       radius= ~sqrt(Sales) * 20,
                       color= "transparent",
                       opacity= .1,
                       fillColor = "blue",
                       popup =~popup_info,
                       label=~paste(Sales, "Millions"),
                       labelOptions = labelOptions(noHide = TRUE,
                                                   textOnly = TRUE,
                                                   textsize = "10px",
                                                   style = list(color="white"))) 
    
  })
  
  output$Table <- renderDT({
    Table<-data_complet %>% 
      filter(Name == input$Nom & Platform == input$plateform2 )
    
    meilleur_tableau<-Table%>% slice(1) %>% select("Name", "Platform", "Other_Sales":"Global_Sales")%>%
      datatable(rownames = F,
                selection = "none",
                options = list(
                  select = list(style = 'os', items = 'row'), 
                  dom = 't',
                  ordering = F),
                escape = F) 
    
    meilleur_tableau
  })
  
  
  ################################# Page Recommendation ############################
  
  output$Tablo <- renderDT({
    if(input$type_recommandation == "Vente"){
      recommandation <- tablo() %>% 
        select("Name":"Publisher") %>% 
        datatable(rownames = F,
                  extensions = c('Select'),
                  selection = "single",
                  options = list(
                    select = list(style = 'os', items = 'row'), 
                    dom = 'lt',
                    ordering = F, 
                    pageLength = 3, 
                    lengthMenu = c(3, 5, 7)),
                  escape = F)
    }
    
    if(input$type_recommandation == "Popularite"){
      recommandation <- tablo() %>% 
        select(Name, Publisher, Annee, platform) %>% 
        datatable(rownames = F,
                  extensions = c('Select'),
                  selection = "single",
                  options = list(
                    select = list(style = 'os', items = 'row'), 
                    dom = 'lt',
                    ordering = F, 
                    pageLength = 3, 
                    lengthMenu = c(3, 5, 7)),
                  escape = F)
    }
    recommandation
  })
  
  output$desc <- renderText(expr = {
    # cat(paste(input$Tablo_row_last_clicked))
    # cat(paste(input$type_recommandation))
    
    if(is.null(input$Tablo_rows_selected) == FALSE){
      if(input$type_recommandation == "Popularite"){
        desc <- tablo() %>% 
          slice(input$Tablo_rows_selected) %>% 
          pull(Description)
        
      }
      if(input$type_recommandation=="Vente"){
        desc <- tablo() %>% 
          slice(input$Tablo_rows_selected) %>% 
          pull(Description)
        
      }
      paste("<h1>Description de votre jeu choisi:</h1>",
            "<p>", 
            desc %>% 
              str_remove("\\[edit description\\]") %>% 
              str_remove("\\[add description\\]") %>%
              str_to_upper() %>% 
              str_to_sentence() %>% 
              str_replace_all("\n", "<br>"), 
            "</p>")
    }
  })
  output$desc2 <- renderPrint({
    print(input$Tablo_rows_selected)
    print(input$type_recommandation)
  })
  
  
  output$image <- renderText({
    if(is.null(input$Tablo_rows_selected) == FALSE){
      if(input$type_recommandation == "Popularite"){
        nom_select<-tablo()[1][1] %>% slice(input$Tablo_rows_selected) %>%
          pull(Name)
        
        img<-df_image%>% filter(Name==nom_select)
        img<-as.vector(img)
        img<-img[2]
        test_img<-paste0(img)
        #paste0(test_img)
      }
      if(input$type_recommandation == "Vente"){
        nom_select<-tablo()[2][1] %>% slice(input$Tablo_rows_selected) %>%
          pull(Name)
        
        img<-df_image%>% filter(Name==nom_select)
        img<-as.vector(img)
        img<-img[2]
        test_img<-paste0(img)
      }
      paste0(test_img)
    }
  })
  
  
  
  output$lienyt <- renderUI({
    if(is.null(input$Tablo_rows_selected) == FALSE){
      if(input$type_recommandation == "Popularite"){
        
        valeur<-tablo()[1][1] %>% slice(input$Tablo_rows_selected) %>%
          pull(Name)
        valeur<-gsub(" ", "", valeur)
        
        url_1 <- paste0("https://www.youtube.com/results?search_query=gameplay+",valeur)
        url_2 <- a(valeur, href= url_1)
        
        #tagList(img(src = "logo_yt.png", height = 80, width = 100), url_2)
      }
      if(input$type_recommandation == "Vente"){
        
        valeur<-tablo()[2][1] %>% slice(input$Tablo_rows_selected) %>%
          pull(Name)
        valeur<-gsub(" ", "", valeur)
        
        url_1 <- paste0("https://www.youtube.com/results?search_query=gameplay+",valeur)
        url_2 <- a(valeur, href= url_1)
        
        #tagList(img(src = "logo_yt.png", height = 80, width = 100), url_2)
      }
      tagList(img(src = "logo_yt.png", height = 80, width = 100), url_2)
    } 
  })
  
  ######################### Page Recherche ###################################################
  
  updateSelectizeInput(session, "Nom2", choices = c("Remplace et choisit un jeu"= "",df2$Name),
                       selected = NULL, server = TRUE)
  
  observe({
    
    valeur_2<-input$Nom2
    valeur_2<-gsub(" ", "", valeur_2)
    
    url_3 <- paste0("https://www.youtube.com/results?search_query=gameplay+",valeur_2)
    url_4 <- a(input$Nom2, href= url_3)
    
    output$tab <- renderUI({
      tagList("Page Youtube pour un gameplay du Jeu:", url_4)
    })
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


