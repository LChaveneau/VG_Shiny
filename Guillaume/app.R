#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(showtext)
library(bslib) #theme
library(plotly)
library(DT)
library(tidyverse)
library(tidyr)
library(questionr)
library(leaflet) #carte
library(shinythemes)
library(questionr) #rename variable rapidement
library(fmsb) #RADAR chart

boxStyle ='color:black; background-color:#DE4F53; border-radius: .1em; color:white; align:right; text-align:left; display: table-cell;'
my_theme <- bs_theme(
  bg = "#e5e5e5", fg = "#0d0c0c", primary = "#dd2020",
  base_font = font_google("Press Start 2P"),
  code_font = font_google("Press Start 2P"),
  "font-size-base" = "0.55rem", "enable-rounded" = FALSE #"0.75rem"
) %>%
  bs_add_rules(
    '@import "https://unpkg.com/nes.css@latest/css/nes.min.css"'
  )

font_add_google(name = "Press Start 2P", family="ps2p")
f1 <- list(
  family = "ps2p"
)


a <- list(
  title = "SALES PER SONG",
  titlefont = f1,
  showgrid = FALSE,
  showticklabels = TRUE,
  showline=TRUE,
  tickangle = 45,
  tickfont = f1
)

df <- read_csv('vgsales3.csv')
df_bis<-read_csv("vgsales.csv")
df2 <- readRDS('data_jeux_image.rds')
df2 <- df2 %>% 
  left_join(readRDS("data_jeux_code_html.rds"), c("Name" = "Name"))

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
df2$platform <- gsub("Linux.*", "PC", df2$platform)
df2$platform <- gsub("iPhone.*", "Mobile", df2$platform)

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


create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    vect, axistype = 1,
    # Personnaliser le polygone
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Personnaliser la grille
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Personnaliser l'axe
    axislabcol = "grey", 
    # etiquettes des variables
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


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
ui <- fluidPage( theme = my_theme,
  #theme = bs_theme(
  # bg = "#e5e5e5", fg = "#0d0c0c", primary = "#dd2020",
  #base_font = font_google("Press Start 2P"),
  #code_font = font_google("Press Start 2P"),
  #"font-size-base" = "0.75rem", "enable-rounded" = FALSE
  #) %>%
  # bs_add_rules(
  #  '@import "https://unpkg.com/nes.css@latest/css/nes.min.css"'
  #),
  
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  # Application title
  titlePanel("Video Games"),
  ## Page
  navbarPage("Page",
             tabPanel("Graphics/ Statistics",
                      tabsetPanel(
                        tabPanel("By Number of games",
                                 sidebarLayout(
                                   sidebarPanel(
                                     sliderInput("years",
                                                 "Choose the year :",
                                                 min = 1950,
                                                 max = 2022,
                                                 value = 2021)
                                     ,
                                     checkboxInput("Licence", "Game under licence", value=FALSE),
                                     radioButtons("var", 
                                                  label = "Variable choices:",
                                                  choices = list("Publisher" = "Publisher", "Platform" = "platform"), 
                                                  selected = "Publisher"),
                                     hr(),
                                     h4("Click on the bar plot to make a specific choice:"),
                                     textInput("choixui",
                                               label=paste("Name of the choice:")
                                     ),
                                     textInput("choixui2",
                                               "Mean mark (of noted games) of the choice:"),
                                   ),
                                   
                                   # Show a plot of the generated distribution
                                   mainPanel(
                                     h4(strong("Graphic representation:")),
                                     plotlyOutput("Hist", width = "700px", height = "300px"),
                                     h4(strong("Repartition of game's type following your specific choice:")),
                                     plotlyOutput("Radar", width = "400px", height = "300px")
                                     #DTOutput("myTable"),
                                   )
                                 )
                        ),
                        tabPanel("By Sales",
                                 fluidRow(column(width=6,
                                                 sliderInput("years2",
                                                             label= h3("Choose the year:"),
                                                             min = 1980,
                                                             max = 2017,
                                                             value = 2016)),
                                          column(width=6,
                                                 radioButtons("var2", 
                                                              label = h3("Variable choices:"),
                                                              choices = list(
                                                                "Maker" = "Publisher", 
                                                                "Type" = "Genre", 
                                                                "Video Games" = "Name", 
                                                                "Platform" = "Platform"
                                                              ), 
                                                              selected = "Name")
                                          )),
                                 h3("Graphic representations"),
                                 # Show a plot of the generated distribution
                                 fluidRow(column(width=5,
                                                 plotlyOutput("SalesHist", width = "500px", height = "300px")),
                                          column(width=5,
                                                 plotlyOutput("SalesHist2", width= "500px", height= "300px"))),
                        ),
                        tabPanel("Sales mapping",
                                 fluidRow(column(width=5,
                                          selectInput("Nom", label=h4("Choose your game:"), multiple = FALSE, choices = character(0))),
                                          column(width=4,
                                 selectInput("plateform2", label = h4("Choose your platform"), 
                                             
                                             choices = c("--", as.list(levels(df_bis$Platform))),
                                             selected = "NES"))),
                                 DTOutput("Table"),
                                 leafletOutput("map_vente"))
                        
                      )
             ),  
             tabPanel("Recommandation",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("type_recommandation",
                                       label = h3("Preference by :"),
                                       choices = list("Popularity" = "Popularite", "Sales" = "Vente"), 
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
                          DTOutput("Tablo")
                        )
                      ),
                      fluidRow(
                        column(4, 
                               htmlOutput("image"),
                               style = "background-color: #EAE7E6"
                        ),
                        column(8,
                               htmlOutput('desc'),
                               style = "background-color: #D3D3D3",
                               uiOutput("lienyt"),
                               htmlOutput('desc2'),
                               style = boxStyle
                        )
                      )
             ),
             tabPanel("Recherche", 
                      selectInput("Nom2", "Choose your game:", multiple = FALSE,
                                  choices =c('Choisit un jeu' = "", character(0)),
                                  selected=NULL),
                      uiOutput("tab")
             )
  ),
)
# Sidebar with a slider input for number of bins 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  thematic::thematic_shiny()
  #bs_themer()
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
        slice(1:7) #%>% 
      #mutate(picture = c(image, image, image))
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
          orientation = "h") %>% layout(title=list(text=paste("Number of games by:", input$var)),
                                        font= list(color="black"),
                                        xaxis= list(title="Number of games"),
                                        yaxis = list(title=""),
                                        paper_bgcolor = "lightgrey",
                                        plot_bgcolor = "lightgrey") %>%
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
          orientation = "h") %>% layout(title=paste("Number of games by", input$var),
                                        xaxis= list(title="Number of games"),
                                        font= list(color="black"),
                                        yaxis = list(title=""),
                                        paper_bgcolor = "lightgrey",
                                        plot_bgcolor = "lightgrey") %>%
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
      myClicks <- event_data("plotly_click", source = "myClickSource", priority="event")
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
        ),showlegend = F,
        paper_bgcolor = "lightgrey",
        font= list(color="black"),
        plot_bgcolor = "lightgrey") %>%
      config(displayModeBar=FALSE)
    
    
    #vect<-t(vect)
    #colnames(vect)<-nom
    #vect<-as.data.frame(vect)
    #a<-c(0,0,0,0,0,0,0,0,0,0)
    #b<-c(max(vect)+10,max(vect)+10,max(vect)+10,max(vect)+10,max(vect)+10,max(vect)+10,max(vect)+10,max(vect)+10,max(vect)+10,max(vect)+10)
    #vect<-rbind(b, a, vect)
    #print(vect)
    
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
        source = "myClickSource2",
        x =  ~ Total_sales,
        y =  ~ fct_reorder(Truc, Total_sales),
        type = "bar",
        orientation = "h") %>% layout(title=paste("Number of sales by", input$var2),
                                      xaxis= list(title="Sales (Millions)"),
                                      yaxis= list(title=""),
                                      font= list(color="black"),
                                      paper_bgcolor = "lightgrey",
                                      plot_bgcolor = "lightgrey") %>%
      config(displayModeBar=FALSE)
    
  })
  
  SelectedBar2 <- reactiveVal(NULL)
  
  observe({
    myClicks2 <- event_data("plotly_click", source = "myClickSource2", priority = "event")
    req(myClicks2)
    SelectedBar2(myClicks2$y)
    
    
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
            type = "bar") %>% layout(title=paste("Sales by type for the maker:", SelectedBar2()),
                                     xaxis= list(title="Sales (Millions)"),
                                     yaxis= list(title=""),
                                     font= list(color="black"),
                                     paper_bgcolor = "lightgrey",
                                     plot_bgcolor = "lightgrey") %>%
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
            type = "bar") %>% layout(title=paste("Sales by type for the platform:", SelectedBar2()),
                                     xaxis= list(title="Ventes (Millions)"),
                                     yaxis= list(title=""),
                                     font= list(color="black"),
                                     paper_bgcolor = "lightgrey",
                                     plot_bgcolor = "lightgrey")%>%
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
            type = "bar") %>% layout(title=paste("Sales by games for the type:", SelectedBar2()),
                                     xaxis= list(title="Ventes (Millions)"),
                                     yaxis= list(title=""),
                                     font= list(color="black"),
                                     paper_bgcolor = "lightgrey",
                                     plot_bgcolor = "lightgrey")%>%
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
            type = "bar") %>% layout(title=paste("Sales by platform for the game:", SelectedBar2()),
                                     xaxis= list(title="Ventes (Millions)"),
                                     yaxis= list(title=""),
                                     font= list(color="black"),
                                     paper_bgcolor = "lightgrey",
                                     plot_bgcolor = "lightgrey") %>%
          config(displayModeBar=FALSE)
        #hist2
      }
      hist2
    })
  })
  
  
  ################# Page Carte ##############################################
  
  updateSelectizeInput(session, "Nom",
                       choices = c("Replace and choose a game"= "",df_bis$Name),
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
                  style = "bootstrap",
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
        select(image, Name, Publisher, Annee, platform) %>% 
        ################## Affectation de la bonne taille de l'image###############
      mutate(valeur_width = image %>%  
               str_extract("width=\"\\d+\"") %>% 
               str_extract("\\d+") %>% 
               as.numeric(),
             valeur_height = image %>%  
               str_extract("height=\"\\d+\"") %>% 
               str_extract("\\d+") %>% 
               as.numeric()) %>% 
        mutate(image = image %>% 
                 str_replace(paste0("width=\"",valeur_width,"\""), paste0("width=\"",valeur_width * 0.5,"\"")) %>% 
                 str_replace(paste0("height=\"",valeur_height,"\""), paste0("height=\"",valeur_height * 0.5,"\""))) %>% 
        ############################################################################
      select(image, Name, Publisher, Annee, platform) %>% 
        datatable(rownames = F,
                  style = "bootstrap",
                  extensions = c('Select'),
                  selection = "single",
                  options = list(
                    select = list(style = 'os', items = 'row'), 
                    dom = 'lt',
                    ordering = F, 
                    pageLength = 3, 
                    lengthMenu = c(3, 5, 7)),
                  escape = F) %>% 
        ##### Hauteur ligne #######
      formatStyle(
        0,
        target = 'row',
        lineHeight='7'
      )
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
        
        paste("<h1>Description</h1>",
              "<p>", 
              desc %>% 
                str_remove("\\[edit description\\]") %>% 
                str_remove("\\[add description\\]") %>%
                str_to_upper() %>% 
                str_to_sentence() %>% 
                str_replace_all("\n", "<br>"), 
              "</p>")
      }else{
        desc <- tablo() %>% 
          slice(input$Tablo_rows_selected) %>% 
          pull(Description)
        
        paste("<h1>Description</h1>",
              "<p>", 
              desc %>% 
                str_remove_all("\\[.*?\\]") %>% 
                str_to_upper() %>% 
                str_to_sentence() %>% 
                str_replace_all("\n", "<br>") %>%
                str_remove("Gameplay"), 
              "</p>")
      }
    }
  })
  output$desc2 <- renderText({
  })
  
  
  output$image <- renderText({
    
    if(is.null(input$Tablo_rows_selected) == FALSE){
      if(input$type_recommandation == "Popularite"){
        code_html <- tablo() %>%
          slice(input$Tablo_rows_selected) %>%
          pull(code_html)
      }
      
      if(input$type_recommandation == "Vente"){
        
        ########Code de l'ancien tableau wiki
        #            code_html <- tablo() %>%
        #              slice(input$Tablo_rows_selected) %>%
        #              pull(tableau) %>%
        #              str_replace("<table", "<center><table") %>%
        #              str_replace("</table>", "</table></center>") %>%
        #              str_replace("<a class=\"image\"", "<center><a class=\"image\"") %>%
        #              str_replace("/></a>", "/></a></center>") %>%
        #              str_replace("<div class=\"infobox-caption\".*?</div>", "") %>% 
        #              str_replace("<tbody>", "<tbody><tr valign=\"top\"><td width=\"1%\"><div id=\"coreGameCover\">") %>% 
        #              str_replace("</tbody>", "</tr></tbody>") %>% 
        #              str_replace("style=\"float: left; width: 22em;", "style=\"float: center; ; width: 45em; background-color: #FAFF52;") %>%
        #              str_replace("style=\"font-size:125%;font-style:italic;", "style=\"font-size:200%;font-style:italic; text-align: center;
        # ")          
        code_html <- tablo() %>%
          slice(input$Tablo_rows_selected) %>%
          pull(tableau)
      }
      paste0(code_html)
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
