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

df <- read_csv('vgsales.csv')
df2 <- readRDS('data_jeux.rds')

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

data_NA = df[,c(1:6, 7, 10:11)]
data_NA <- rename.variable(data_NA, "NA_Sales", "Sales")
data_NA$country = "Canada"

data_EU = df[,c(1:6, 8, 10:11)]
data_EU <- rename.variable(data_EU, "EU_Sales", "Sales")
data_EU$country = "Austria"

data_jpn = df[,c(1:6, 9:11)]
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
                               sidebarLayout(
                                 sidebarPanel(
                                   sliderInput("years2",
                                               "Annee :",
                                               min = 1980,
                                               max = 2017,
                                               value = 2016)
                                   ,
                                   radioButtons("var2", 
                                                label = h3("Choix de la variable:"),
                                                choices = list(
                                                  "Publisher" = "Publisher", 
                                                  "Genre" = "Genre", 
                                                  "Video Games" = "Name", 
                                                  "Platform" = "Platform"
                                                ), 
                                                selected = "Name")
                                 ),
                                 
                                 # Show a plot of the generated distribution
                                 mainPanel(
                                   plotOutput("SalesHist")
                                 )
                               )),
                      tabPanel("Cartographie de ventes",
                               selectInput("Nom", "Choix du jeu", multiple = FALSE, choices = character(0)),
                               selectInput("plateform2", label = h4("Choix de la plateform"), 
                                           
                                           choices = c("--", as.list(levels(df$Platform))),
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
                                         "Edition_special",
                                         "DLC"),
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
                      )
                    ),
                    fluidRow(
                      column(4, 
                             htmlOutput("image")
                      ),
                      column(8,
                             htmlOutput('desc'),
                             verbatimTextOutput('desc2')
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
theme = shinythemes::shinytheme('united')
)
# Sidebar with a slider input for number of bins 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
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
                             "Edition special" = "Edition_special"))
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
        slice(1:3)
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
        slice(1:3)
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
                                "Concepteur" = df$Publisher, 
                                "Genre" = df$Genre,
                                "Platform" = df$Platform,
                                "Name" = df$Name)
  })
  
  output$Hist <- renderPlotly({
    fct_count(variable()[df2$Year == input$years], sort = T) %>%
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
    
  })
  
  SelectedBar <- reactiveVal(NULL)
  
  observe({
    myClicks <- event_data("plotly_click", source = "myClickSource")
    req(myClicks)
    print(myClicks)
    SelectedBar(myClicks$y)
    print(SelectedBar)
    updateTextInput(session, "choixui", value = SelectedBar())
  })
  
  
  output$myTable <- renderDT({
    
    nom<-c("Action", "Aventure", "Puzzle", "Simulation", "Racing",
           "Educational", "Compilation","Sports", "Strategie", "Role_play")
    
    if (input$var=="Publisher"){
      vect<-c(sum(df2$Action[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Aventure[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Puzzle[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Simulation[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Racing[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Educational[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Compilation[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Sports[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Strategie[df2$Publisher==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Role_play[df2$Publisher==SelectedBar() & df2$Year==input$years]==1))
      
      updateTextInput(session,
                      "choixui2",
                      value = paste(round(mean(df2$reviews[!is.na(df2$reviews) & df2$Publisher==SelectedBar() & df2$Year==input$years]),3), "/100")
      )
      
    }
    if (input$var=="platform"){
      vect<-c(sum(df2$Action[df2$platform==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Aventure[df2$platform==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Puzzle[df2$platform==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Simulation[df2$platform==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Racing[df2$platform==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Educational[df2$platform==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Compilation[df2$platform==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Sports[df2$platform==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Strategie[df2$platform==SelectedBar() & df2$Year==input$years]==1),
              sum(df2$Role_play[df2$platform==SelectedBar() & df2$Year==input$years]==1))
      
      updateTextInput(session,
                      "choixui2",
                      value = paste(round(mean(df2$reviews[!is.na(df2$reviews) & df2$platform==SelectedBar() & df2$Year==input$years]),3), "/100")
      ) 
    }
    
  })
  
  
  output$Radar <- renderPlotly({
    
    nom<-c("Puzzle", "Aventure", "Action", "Simulation", "Racing",
           "Educational", "Compilation","Sports", "Strategie", "Role_play")
    
    if (input$var=="Publisher"){
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
      
      updateTextInput(session,
                      "choixui2",
                      value = paste(round(mean(df2$reviews[!is.na(df2$reviews) & df2$Publisher==SelectedBar() & df2$Year==input$years]),3), "/100")
      )
      
    }
    if (input$var=="platform"){
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
      
      updateTextInput(session,
                      "choixui2",
                      value = paste(round(mean(df2$reviews[!is.na(df2$reviews) & df2$platform==SelectedBar() & df2$Year==input$years]),3), "/100")
      ) 
    }
    
    #vect<-as.matrix(vect)
    #rownames(vect)<-nom
    #vect<-t(vect)
    #rownames(vect)<-"Valeur"
    
    #vect<-as.data.frame(vect)
    
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
  
  output$SalesHist <- renderPlot({
    df %>% 
      filter(Year == input$years2) %>% 
      select(everything()) %>% 
      group_by(variable2()[df$Year == input$years2]) %>%
      summarize(Total_sales = sum(Global_Sales)) %>%
      arrange(desc(Total_sales)) %>%
      slice(1:10) %>% 
      setNames(c("Truc", "Total_sales")) %>% 
      ggplot() + 
      aes(x=fct_reorder(Truc, Total_sales), y = Total_sales) + 
      geom_col()+ 
      xlab(label=input$var2)+
      ylab(label="Ventes totales")+
      theme_minimal()+
      ggtitle(paste("Ventes totales par", input$var2))+
      coord_flip()
  })
  
  ################# Page Carte ##############################################
  
  updateSelectizeInput(session, "Nom",
                       choices = c("Remplace et choisit un jeu"= "",df$Name),
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
      tablo() %>% 
        select("Name":"Publisher") %>% 
        datatable(rownames = F,
                  extensions = c('Select'),
                  selection = "none",
                  options = list(
                    select = list(style = 'os', items = 'row'), 
                    dom = 't',
                    ordering = F),
                  escape = F)
    }
    
    if(input$type_recommandation == "Popularite"){
      tablo() %>% 
        select(Name, Publisher, Annee, platform) %>% 
        datatable(rownames = F,
                  extensions = c('Select'),
                  selection = "none",
                  options = list(
                    select = list(style = 'os', items = 'row'), 
                    dom = 't',
                    ordering = F),
                  escape = F)
    }
  })
  
  output$desc <- renderText(expr = {
    # cat(paste(input$Tablo_row_last_clicked))
    # cat(paste(input$type_recommandation))
    
    if(is.null(input$Tablo_rows_selected) == FALSE){
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
    }
  })
  output$desc2 <- renderPrint({
    print(input$Tablo_rows_selected)
  })
 
  
  output$image <- renderText({
    if(is.null(input$Tablo_rows_selected) == FALSE){
      paste0("<img alt=\"007: James Bond - The Stealth Affair DOS Front Cover\" border=\"0\" height=\"150\" src=\"/images/covers/s/261101-007-james-bond-the-stealth-affair-dos-front-cover.jpg\" width=\"120\"/>")
    }
  })
  
  ######################### Page Recherche ###################################################
  
  updateSelectizeInput(session, "Nom2", choices = c("Remplace et choisit un jeu"= "",df2$Name),
                       selected = NULL, server = TRUE)
  
  observe({
    
    valeur<-input$Nom2
    valeur<-gsub(" ", "", valeur)
    
    url <- paste0("https://www.youtube.com/results?search_query=gameplay+",valeur)
    url_2 <- a(input$Nom2, href= url)
    
    output$tab <- renderUI({
      tagList("Page Youtube pour un gameplay du Jeu:", url_2)
    })
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# output$SalesHist <- renderPlot({
#   tablo <- df %>% 
#     filter(Year == input$years2) %>% 
#     select(everything()) %>% 
#     group_by(variable2()[df$Year == input$years2]) %>%
#     summarize(Total_sales = sum(Global_Sales))%>%
#     arrange(desc(Total_sales)) %>%
#     slice(1:10)
#   colnames(tablo) <- c("Truc", "Total_sales")
#   ggplot(tablo) + 
#     aes(x=fct_reorder(Truc, Total_sales), y = Total_sales) + 
#     geom_col() + 
#     coord_flip()
# })