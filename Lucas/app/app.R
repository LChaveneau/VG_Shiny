#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(tidyr)
library(DT)
library(fresh)
library(bslib)

#

boxStyle ='color:black; background-color:#000000; border-radius: .1em; color:white; align:right; text-align:left; display: table-cell'
my_theme <- bs_theme(
  bg = "#e5e5e5", fg = "#0d0c0c", primary = "#dd2020",
  base_font = font_google("Press Start 2P"),
  code_font = font_google("Press Start 2P"),
  "font-size-base" = "0.55rem", "enable-rounded" = FALSE #"0.75rem"
) %>%
  bs_add_rules(
    '@import "https://unpkg.com/nes.css@latest/css/nes.min.css"'
  )


df <- read_csv('vgsales3.csv')
df2 <- readRDS('data_jeux_image.rds')
df2 <- df2 %>% 
  left_join(readRDS("data_jeux_code_html2.rds"), c("Name" = "Name"))


df2 <- df2[!duplicated(cbind(df2$Name,
                             df2$platform)),]

df <- df  %>% 
  mutate(Platform = as.factor(Platform)) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Genre = as.factor(Genre)) %>% 
  mutate(Publisher = as.factor(Publisher))


df2 <- df2  %>% 
  mutate(Publisher = as.factor(Publisher)) %>% 
  mutate(Annee = as.factor(Annee)) %>% 
  mutate(platform = as.factor(platform))

df2$code_html <- as.character(df2$code_html)






# ## NOT RUN and TO DELETE AT THE END ->>>>>
# switch_vector <- function(x){switch(
#   x,
#   "Action" = df2$Action,
#   "Aventure" = df2$Aventure,
#   "Puzzle" = df2$Puzzle,
#   "Racing" = df2$Racing,
#   "Education" = df2$Educational,
#   "Compilation" = df2$Compilation,
#   "Simulation" = df2$Simulation,
#   "Sports" = df2$Sports,
#   "Strategie" = df2$Strategie,bslib
#   "Role_play" = df2$Role_play,
#   "Edition_special" = df2$Edition_special,
#   "DLC" = df2$DLC)
# }
# >>>>>>

# Define UI for application that draws a histogram
ui <- fluidPage(theme = my_theme,
                shinyjs::useShinyjs(),
  #includeCSS("www/TableStyle.css"),

    # Application title
    titlePanel(title = div(img(src="title_panel.png",  height = 43, width = 100), div("VideoGames", style = "background-color:#000000 text-shadow:0 0 2px #FFFFFF,0 0 30px #FFFFFF,0px 0px 5px #FFFFFF, 0 0 150px #FFFFFF;color:#FFFFFF;"))),
    ## Page
    navbarPage("Page",
      tabPanel("Graphiques",
        tabsetPanel(
          tabPanel("Nombre",
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("years",
                                   "Annee :",
                                   min = 1980,
                                   max = 2017,
                                   value = 2016)
                     ,
                     radioButtons("var", 
                                  label = h3("Radio buttons"),
                                  choices = list("Publisher" = "Publisher", "Genre" = "Genre"), 
                                  selected = "Genre")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                       plotOutput("Hist")
                     )
                   )
          ),
          tabPanel("Sales", 
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("years2",
                                   "Annee :",
                                   min = 1980,
                                   max = 2017,
                                   value = 2016)
                       ,
                       radioButtons("var2", 
                                    label = h3("Radio buttons"),
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
          tabPanel("I have no idea", "ODOR")
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
                                 label = h3("Platform"),
                                 choices = c(levels(df2$platform)),
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
                                   "Action" = "Action",
                                   "Adventure" = "Aventure",
                                   "Puzzle" = "Puzzle",
                                   "Racing / driving" = "Racing",
                                   "Educational" = "Educational",
                                   "Compilation" = "Compilation",
                                   "Simulation" = "Simulation",
                                   "Sports" = "Sportd=s",
                                   "Strategy / tactics" = "Strategie",
                                   "Role playing" = "Role_play",
                                   "Special edition" = "Edition_special",
                                   "DLC /add-on" = "DLC"),
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
                      style = "background-color: #EAFF91; word-wrap: break-word"
                ),
                #width: auto
                #display: table
                #position: relative;
                column(8,
                       htmlOutput('desc'),
                       style = boxStyle
              )
              ),
              fluidRow(
                column(3,
                       htmlOutput("Gameplay")
                       ),
                column(3,
                       htmlOutput("bouton_comparateur"),
                       actionBttn(
                         inputId = "bouton",
                         label = "Submit",
                         style = "jelly", 
                         color = "warning"
                       )),
                column(6,
                       htmlOutput("Comparateur "))
              )
      ),
      tabPanel("Recherche",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("years4",
                               "Annee :",
                               min = 1950,
                               max = 2022,
                               value = c(1950,2022)),
                   
                   selectizeInput("plateform2", 
                                  label = h3("Platform"),
                                  choices = c(levels(df2$platform)),
                                  multiple = TRUE,
                                  options = list(dropdownParent = 'body', 
                                                 maxitems = 'null', 
                                                 plugins = list("remove_button"),
                                                 highlight = F,
                                                 closeAfterSelect = T)
                   ),
                   selectizeInput("genre2",
                                  label = h3('Genre'),
                                  choices = c(
                                    "Action" = "Action",
                                    "Adventure" = "Aventure",
                                    "Puzzle" = "Puzzle",
                                    "Racing / driving" = "Racing",
                                    "Educational" = "Educational",
                                    "Compilation" = "Compilation",
                                    "Simulation" = "Simulation",
                                    "Sports" = "Sports",
                                    "Strategy / tactics" = "Strategie",
                                    "Role playing" = "Role_play"
                                    ),
                                  multiple = TRUE,
                                  options = list(dropdownParent = 'body', 
                                                 maxitems = 'null', 
                                                 plugins = list("remove_button"),
                                                 highlight = F,
                                                 closeAfterSelect = T)
                   ),
                   selectizeInput("Publisher",
                                  label = h3('Publisher'),
                                  choices = NULL,
                                    #c(levels(as.factor(df2$Publisher)))),
                                  multiple = TRUE,
                                  options = list(dropdownParent = 'body',
                                                  plugins = list("remove_button"),
                                                  maxOptions = 800,
                                                  highlight = T,
                                                  maxItems = 2,
                                                  closeAfterSelect = T)
                   ),
                   selectizeInput("options2",
                                  label = h3('Options'),
                                  choices = c(
                                    "DLC / add-on" = "DLC",
                                    "Special edition" = "Edition_special",
                                    "1sr person" = "Troisieme_personne",
                                    "3rd person" = "Premiere_personne",
                                    "Text" = "Text",
                                    "View: Derriere" = "Derriere",
                                    "View: Diagonal" = "Diagonal",
                                    "View: Cote" = "Cote",
                                    "View: Haut" = "Haut",
                                    "Audio" = "Audio"
                                  ),
                                  multiple = TRUE,
                                  options = list(dropdownParent = 'body', 
                                                 maxitems = 'null', 
                                                 plugins = list("remove_button"),
                                                 highlight = F,
                                                 closeAfterSelect = T)
                   )
                 ),
                 mainPanel(
                   DTOutput("Tablo2")
                 )
               ),
               fluidRow(
                 column(4,
                        htmlOutput("image2"),
                        style = "background-color: #EAE7E6; word-wrap: break-word"
                 ),
                 #width: auto
                 #display: table
                 #position: relative;
                 column(8,
                        htmlOutput('desc2'),
                        style = boxStyle
                 )
               ),
               fluidRow(
                 column(2,
                        htmlOutput("Gameplay2", style = "width: 70%")
                 ),
                 column(2,
                        htmlOutput("bouton_comparateur2"),
                        #submitButton(text = "Submit"),
                        style = "background-color:#2E8ECE")
                )
      ),
      tabPanel("About us",
               fluidRow(
                 column(2,
                        htmlOutput("P1"), 
                        style="background-color:#D3D3D3"),
                 column(8,
                        htmlOutput("contents")
                        ),
                 column(2,
                        htmlOutput("P2"),
                        style ="background-color:#D3D3D3")
                 )
               )
      )
    #theme = includeCSS("bootstrap.min.css")
    )
    # Sidebar with a slider input for number of bins 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  updateSelectizeInput(session, 
                       "Publisher", 
                       choices = c(levels(as.factor(df2$Publisher))), 
                       server = TRUE)
  
  #############################################################################################
  ###################################### EVENT REACTIVE #######################################
  #############################################################################################
  
  
  #############################################################################################
  ###################################### OBSERVE ##############################################
  #############################################################################################
  
  observe({
    input$plateform1

    if(input$type_recommandation == "Vente"){
      if(is.null(input$plateform1) == FALSE){
      minimum <- min(na.omit(as.numeric(as.character(df$Year[df$Platform %in% input$plateform1]))))
      maximum <- max(na.omit(as.numeric(as.character(df$Year[df$Platform %in% input$plateform1]))))
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
        minimum <- min(na.omit(as.numeric(as.character(df2$Annee[df2$platform %in% input$plateform1]))))
        maximum <- max(na.omit(as.numeric(as.character(df2$Annee[df2$platform %in% input$plateform1]))))
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
  
  observe({
    input$plateform2
    if(is.null(input$plateform2) == FALSE){
      minimum <- min(na.omit(as.numeric(as.character(df2$Annee[df2$platform %in% input$plateform2]))))
      maximum <- max(na.omit(as.numeric(as.character(df2$Annee[df2$platform %in% input$plateform2]))))
    }else{
      minimum <- 1950
      maximum <- 2022
    }
    updateSliderInput(
      inputId = "years4",
      min = minimum,
      max = maximum,
      value = c(minimum,maximum)
    )
  })
  
  observe({
    if(is.null(input$Tablo_rows_selected) == FALSE){
      shinyjs::show("bouton")
    } else {
      shinyjs::hide("bouton")
    }
  })
  
  observe({
    input$Tablo_rows_selected
    shinyjs::reset("bouton")
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
          filter(Platform %in% input$plateform1)
      }
      if(is.null(input$genre1) == FALSE){
        tablo <- tablo %>% 
          filter(Genre %in% input$genre1)
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
          filter(platform %in% input$plateform1)
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
  
  tablo2 <- reactive({
    tablo <-df2 %>%
      filter(as.numeric(as.character(Annee)) >= input$years4[1]) %>%
      filter(as.numeric(as.character(Annee)) <= input$years4[2])
    
    if(is.null(input$genre2) == FALSE){
      tablo <- tablo %>% 
        filter(tablo[input$genre2] %>% rowSums() == length(input$genre2))
    }
    
    if(is.null(input$options2) == FALSE){
      tablo <- tablo %>% 
        filter(tablo[input$options2] %>% rowSums() > 0)
    }
    
    if(is.null(input$Publisher) == FALSE){
      tablo <- tablo %>% 
        filter(Publisher == input$Publisher)
    }
    
    if(is.null(input$plateform2) == FALSE){
      tablo <- tablo %>% 
        filter(platform %in% input$plateform2)
    }
    tablo
  })
  
  ####################################################################################
  ####################### Reactive SWITCH ###########################################
  ####################################################################################
  
  variable <- reactive({switch(input$var,
                               "Publisher" = df$Publisher, 
                               "Genre" = df$Genre)
    })
  
  variable2 <- reactive({switch(input$var2,
                               "Publisher" = df$Publisher, 
                               "Genre" = df$Genre,
                               "Platform" = df$Platform,
                               "Name" = df$Name)
  })
  
  # ###### NOT RUN AND TO DELETE AT THE END >>>>>>
  # variable_genre <- reactive({
  #   sapply(input$genre1, switch_vector)
  # }) 
  # >>>>>>>>>>
  
  ###################################################################################
  ############################## OUTPUT #############################################
  ####################################################################################
  
  output$Hist <- renderPlot({
          fct_count(variable()[df$Year == input$years], sort = T) %>%
          slice(1:10) %>%
          ggplot() +
          aes(x=fct_reorder(f, n), y = n) +
          geom_col() +
          coord_flip()
      })

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
        geom_col() + 
        coord_flip()
    })
  
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
        
        if(is.na(desc) == TRUE){
          desc <- "Sorry! We don't have description for this video game"
        }
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
     
     output$image <- renderText({

       if(is.null(input$Tablo_rows_selected) == FALSE){
         if(input$type_recommandation == "Popularite"){
          code_html <- tablo() %>%
            slice(input$Tablo_rows_selected) %>%
            pull(code_html)
          }

         if(input$type_recommandation == "Vente"){
            code_html <- tablo() %>%
               slice(input$Tablo_rows_selected) %>%
               pull(tableau)
            if(is.na(code_html) == TRUE){
              code_html <- "<Sorry! We don't have description for this video game>"
            }
            }
         paste0(code_html)
       }
     })
     
     output$Gameplay <- renderText({
       
       if(is.null(input$Tablo_rows_selected) == FALSE){
           url <- tablo() %>%
             slice(input$Tablo_rows_selected) %>%
             pull(Name) %>% 
             str_to_lower() %>% 
             str_replace_all(" ", "+") %>%
             str_replace_all("/", "%2F") %>% 
             str_replace_all(",", "%2C") %>% 
             str_replace_all("!", "%21") %>% 
             paste( "+gameplay")
          paste0("<br>
                  </br>
                  <a target= \"_blank\" class=\"yt-simple-endpoint style-scope ytd-topbar-logo-renderer\" id=\"logo\" href=\"https://www.youtube.com/results?search_query=",url,"\" title=\"Accueil YouTube\">
                    <div class=\"style-scope ytd-topbar-logo-renderer\">
                      <h2>Trouver un gameplay sur</h2>
                      <ytd-logo class=\"style-scope ytd-topbar-logo-renderer\"><!--css-build:shady--><yt-icon id=\"logo-icon\" class=\"style-scope ytd-logo\"><svg viewBox=\"0 0 90 20\" preserveAspectRatio=\"xMidYMid meet\" focusable=\"false\" class=\"style-scope yt-icon\" style=\"pointer-events: none; display: block; width: 60%; height: 60%;\"><g viewBox=\"0 0 90 20\" preserveAspectRatio=\"xMidYMid meet\" class=\"style-scope yt-icon\"><g class=\"style-scope yt-icon\"><path d=\"M27.9727 3.12324C27.6435 1.89323 26.6768 0.926623 25.4468 0.597366C23.2197 2.24288e-07 14.285 0 14.285 0C14.285 0 5.35042 2.24288e-07 3.12323 0.597366C1.89323 0.926623 0.926623 1.89323 0.597366 3.12324C2.24288e-07 5.35042 0 10 0 10C0 10 2.24288e-07 14.6496 0.597366 16.8768C0.926623 18.1068 1.89323 19.0734 3.12323 19.4026C5.35042 20 14.285 20 14.285 20C14.285 20 23.2197 20 25.4468 19.4026C26.6768 19.0734 27.6435 18.1068 27.9727 16.8768C28.5701 14.6496 28.5701 10 28.5701 10C28.5701 10 28.5677 5.35042 27.9727 3.12324Z\" fill=\"#FF0000\" class=\"style-scope yt-icon\"></path><path d=\"M11.4253 14.2854L18.8477 10.0004L11.4253 5.71533V14.2854Z\" fill=\"white\" class=\"style-scope yt-icon\"></path></g><g class=\"style-scope yt-icon\"><g id=\"youtube-paths\" class=\"style-scope yt-icon\"><path d=\"M34.6024 13.0036L31.3945 1.41846H34.1932L35.3174 6.6701C35.6043 7.96361 35.8136 9.06662 35.95 9.97913H36.0323C36.1264 9.32532 36.3381 8.22937 36.665 6.68892L37.8291 1.41846H40.6278L37.3799 13.0036V18.561H34.6001V13.0036H34.6024Z\" class=\"style-scope yt-icon\"></path><path d=\"M41.4697 18.1937C40.9053 17.8127 40.5031 17.22 40.2632 16.4157C40.0257 15.6114 39.9058 14.5437 39.9058 13.2078V11.3898C39.9058 10.0422 40.0422 8.95805 40.315 8.14196C40.5878 7.32588 41.0135 6.72851 41.592 6.35457C42.1706 5.98063 42.9302 5.79248 43.871 5.79248C44.7976 5.79248 45.5384 5.98298 46.0981 6.36398C46.6555 6.74497 47.0647 7.34234 47.3234 8.15137C47.5821 8.96275 47.7115 10.0422 47.7115 11.3898V13.2078C47.7115 14.5437 47.5845 15.6161 47.3329 16.4251C47.0812 17.2365 46.672 17.8292 46.1075 18.2031C45.5431 18.5771 44.7764 18.7652 43.8098 18.7652C42.8126 18.7675 42.0342 18.5747 41.4697 18.1937ZM44.6353 16.2323C44.7905 15.8231 44.8705 15.1575 44.8705 14.2309V10.3292C44.8705 9.43077 44.7929 8.77225 44.6353 8.35833C44.4777 7.94206 44.2026 7.7351 43.8074 7.7351C43.4265 7.7351 43.156 7.94206 43.0008 8.35833C42.8432 8.77461 42.7656 9.43077 42.7656 10.3292V14.2309C42.7656 15.1575 42.8408 15.8254 42.9914 16.2323C43.1419 16.6415 43.4123 16.8461 43.8074 16.8461C44.2026 16.8461 44.4777 16.6415 44.6353 16.2323Z\" class=\"style-scope yt-icon\"></path><path d=\"M56.8154 18.5634H54.6094L54.3648 17.03H54.3037C53.7039 18.1871 52.8055 18.7656 51.6061 18.7656C50.7759 18.7656 50.1621 18.4928 49.767 17.9496C49.3719 17.4039 49.1743 16.5526 49.1743 15.3955V6.03751H51.9942V15.2308C51.9942 15.7906 52.0553 16.188 52.1776 16.4256C52.2999 16.6631 52.5045 16.783 52.7914 16.783C53.036 16.783 53.2712 16.7078 53.497 16.5573C53.7228 16.4067 53.8874 16.2162 53.9979 15.9858V6.03516H56.8154V18.5634Z\" class=\"style-scope yt-icon\"></path><path d=\"M64.4755 3.68758H61.6768V18.5629H58.9181V3.68758H56.1194V1.42041H64.4755V3.68758Z\" class=\"style-scope yt-icon\"></path><path d=\"M71.2768 18.5634H69.0708L68.8262 17.03H68.7651C68.1654 18.1871 67.267 18.7656 66.0675 18.7656C65.2373 18.7656 64.6235 18.4928 64.2284 17.9496C63.8333 17.4039 63.6357 16.5526 63.6357 15.3955V6.03751H66.4556V15.2308C66.4556 15.7906 66.5167 16.188 66.639 16.4256C66.7613 16.6631 66.9659 16.783 67.2529 16.783C67.4974 16.783 67.7326 16.7078 67.9584 16.5573C68.1842 16.4067 68.3488 16.2162 68.4593 15.9858V6.03516H71.2768V18.5634Z\" class=\"style-scope yt-icon\"></path><path d=\"M80.609 8.0387C80.4373 7.24849 80.1621 6.67699 79.7812 6.32186C79.4002 5.96674 78.8757 5.79035 78.2078 5.79035C77.6904 5.79035 77.2059 5.93616 76.7567 6.23014C76.3075 6.52412 75.9594 6.90747 75.7148 7.38489H75.6937V0.785645H72.9773V18.5608H75.3056L75.5925 17.3755H75.6537C75.8724 17.7988 76.1993 18.1304 76.6344 18.3774C77.0695 18.622 77.554 18.7443 78.0855 18.7443C79.038 18.7443 79.7412 18.3045 80.1904 17.4272C80.6396 16.5476 80.8653 15.1765 80.8653 13.3092V11.3266C80.8653 9.92722 80.7783 8.82892 80.609 8.0387ZM78.0243 13.1492C78.0243 14.0617 77.9867 14.7767 77.9114 15.2941C77.8362 15.8115 77.7115 16.1808 77.5328 16.3971C77.3564 16.6158 77.1165 16.724 76.8178 16.724C76.585 16.724 76.371 16.6699 76.1734 16.5594C75.9759 16.4512 75.816 16.2866 75.6937 16.0702V8.96062C75.7877 8.6196 75.9524 8.34209 76.1852 8.12337C76.4157 7.90465 76.6697 7.79646 76.9401 7.79646C77.2271 7.79646 77.4481 7.90935 77.6034 8.13278C77.7609 8.35855 77.8691 8.73485 77.9303 9.26636C77.9914 9.79787 78.022 10.5528 78.022 11.5335V13.1492H78.0243Z\" class=\"style-scope yt-icon\"></path><path d=\"M84.8657 13.8712C84.8657 14.6755 84.8892 15.2776 84.9363 15.6798C84.9833 16.0819 85.0821 16.3736 85.2326 16.5594C85.3831 16.7428 85.6136 16.8345 85.9264 16.8345C86.3474 16.8345 86.639 16.6699 86.7942 16.343C86.9518 16.0161 87.0365 15.4705 87.0506 14.7085L89.4824 14.8519C89.4965 14.9601 89.5035 15.1106 89.5035 15.3011C89.5035 16.4582 89.186 17.3237 88.5534 17.8952C87.9208 18.4667 87.0247 18.7536 85.8676 18.7536C84.4777 18.7536 83.504 18.3185 82.9466 17.446C82.3869 16.5735 82.1094 15.2259 82.1094 13.4008V11.2136C82.1094 9.33452 82.3987 7.96105 82.9772 7.09558C83.5558 6.2301 84.5459 5.79736 85.9499 5.79736C86.9165 5.79736 87.6597 5.97375 88.1771 6.32888C88.6945 6.684 89.059 7.23433 89.2707 7.98457C89.4824 8.7348 89.5882 9.76961 89.5882 11.0913V13.2362H84.8657V13.8712ZM85.2232 7.96811C85.0797 8.14449 84.9857 8.43377 84.9363 8.83593C84.8892 9.2381 84.8657 9.84722 84.8657 10.6657V11.5641H86.9283V10.6657C86.9283 9.86133 86.9001 9.25221 86.846 8.83593C86.7919 8.41966 86.6931 8.12803 86.5496 7.95635C86.4062 7.78702 86.1851 7.7 85.8864 7.7C85.5854 7.70235 85.3643 7.79172 85.2232 7.96811Z\" class=\"style-scope yt-icon\"></path></g></g></g></svg><!--css-build:shady--></yt-icon></ytd-logo>
                    </div>
                    <ytd-yoodle-renderer class=\"style-scope ytd-topbar-logo-renderer\" hidden=\"\">
                      <!--css-build:shady-->
                        <picture class=\"style-scope ytd-yoodle-renderer\">
                          <source type=\"image/webp\" class=\"style-scope ytd-yoodle-renderer\" srcset=\"\">
                            <img class=\"style-scope ytd-yoodle-renderer\" src=\"\">
                        </picture>
                    <ytd-logo class=\"style-scope ytd-yoodle-renderer\" hidden=\"\"><!--css-build:shady--><yt-icon id=\"logo-icon\" class=\"style-scope ytd-logo\"><svg viewBox=\"0 0 90 20\" preserveAspectRatio=\"xMidYMid meet\" focusable=\"false\" class=\"style-scope yt-icon\" style=\"pointer-events: none; display: block; width: 60%; height: 60%;\"><g viewBox=\"0 0 90 20\" preserveAspectRatio=\"xMidYMid meet\" class=\"style-scope yt-icon\"><g class=\"style-scope yt-icon\"><path d=\"M27.9727 3.12324C27.6435 1.89323 26.6768 0.926623 25.4468 0.597366C23.2197 2.24288e-07 14.285 0 14.285 0C14.285 0 5.35042 2.24288e-07 3.12323 0.597366C1.89323 0.926623 0.926623 1.89323 0.597366 3.12324C2.24288e-07 5.35042 0 10 0 10C0 10 2.24288e-07 14.6496 0.597366 16.8768C0.926623 18.1068 1.89323 19.0734 3.12323 19.4026C5.35042 20 14.285 20 14.285 20C14.285 20 23.2197 20 25.4468 19.4026C26.6768 19.0734 27.6435 18.1068 27.9727 16.8768C28.5701 14.6496 28.5701 10 28.5701 10C28.5701 10 28.5677 5.35042 27.9727 3.12324Z\" fill=\"#FF0000\" class=\"style-scope yt-icon\"></path><path d=\"M11.4253 14.2854L18.8477 10.0004L11.4253 5.71533V14.2854Z\" fill=\"white\" class=\"style-scope yt-icon\"></path></g><g class=\"style-scope yt-icon\"><g id=\"youtube-paths\" class=\"style-scope yt-icon\"><path d=\"M34.6024 13.0036L31.3945 1.41846H34.1932L35.3174 6.6701C35.6043 7.96361 35.8136 9.06662 35.95 9.97913H36.0323C36.1264 9.32532 36.3381 8.22937 36.665 6.68892L37.8291 1.41846H40.6278L37.3799 13.0036V18.561H34.6001V13.0036H34.6024Z\" class=\"style-scope yt-icon\"></path><path d=\"M41.4697 18.1937C40.9053 17.8127 40.5031 17.22 40.2632 16.4157C40.0257 15.6114 39.9058 14.5437 39.9058 13.2078V11.3898C39.9058 10.0422 40.0422 8.95805 40.315 8.14196C40.5878 7.32588 41.0135 6.72851 41.592 6.35457C42.1706 5.98063 42.9302 5.79248 43.871 5.79248C44.7976 5.79248 45.5384 5.98298 46.0981 6.36398C46.6555 6.74497 47.0647 7.34234 47.3234 8.15137C47.5821 8.96275 47.7115 10.0422 47.7115 11.3898V13.2078C47.7115 14.5437 47.5845 15.6161 47.3329 16.4251C47.0812 17.2365 46.672 17.8292 46.1075 18.2031C45.5431 18.5771 44.7764 18.7652 43.8098 18.7652C42.8126 18.7675 42.0342 18.5747 41.4697 18.1937ZM44.6353 16.2323C44.7905 15.8231 44.8705 15.1575 44.8705 14.2309V10.3292C44.8705 9.43077 44.7929 8.77225 44.6353 8.35833C44.4777 7.94206 44.2026 7.7351 43.8074 7.7351C43.4265 7.7351 43.156 7.94206 43.0008 8.35833C42.8432 8.77461 42.7656 9.43077 42.7656 10.3292V14.2309C42.7656 15.1575 42.8408 15.8254 42.9914 16.2323C43.1419 16.6415 43.4123 16.8461 43.8074 16.8461C44.2026 16.8461 44.4777 16.6415 44.6353 16.2323Z\" class=\"style-scope yt-icon\"></path><path d=\"M56.8154 18.5634H54.6094L54.3648 17.03H54.3037C53.7039 18.1871 52.8055 18.7656 51.6061 18.7656C50.7759 18.7656 50.1621 18.4928 49.767 17.9496C49.3719 17.4039 49.1743 16.5526 49.1743 15.3955V6.03751H51.9942V15.2308C51.9942 15.7906 52.0553 16.188 52.1776 16.4256C52.2999 16.6631 52.5045 16.783 52.7914 16.783C53.036 16.783 53.2712 16.7078 53.497 16.5573C53.7228 16.4067 53.8874 16.2162 53.9979 15.9858V6.03516H56.8154V18.5634Z\" class=\"style-scope yt-icon\"></path><path d=\"M64.4755 3.68758H61.6768V18.5629H58.9181V3.68758H56.1194V1.42041H64.4755V3.68758Z\" class=\"style-scope yt-icon\"></path><path d=\"M71.2768 18.5634H69.0708L68.8262 17.03H68.7651C68.1654 18.1871 67.267 18.7656 66.0675 18.7656C65.2373 18.7656 64.6235 18.4928 64.2284 17.9496C63.8333 17.4039 63.6357 16.5526 63.6357 15.3955V6.03751H66.4556V15.2308C66.4556 15.7906 66.5167 16.188 66.639 16.4256C66.7613 16.6631 66.9659 16.783 67.2529 16.783C67.4974 16.783 67.7326 16.7078 67.9584 16.5573C68.1842 16.4067 68.3488 16.2162 68.4593 15.9858V6.03516H71.2768V18.5634Z\" class=\"style-scope yt-icon\"></path><path d=\"M80.609 8.0387C80.4373 7.24849 80.1621 6.67699 79.7812 6.32186C79.4002 5.96674 78.8757 5.79035 78.2078 5.79035C77.6904 5.79035 77.2059 5.93616 76.7567 6.23014C76.3075 6.52412 75.9594 6.90747 75.7148 7.38489H75.6937V0.785645H72.9773V18.5608H75.3056L75.5925 17.3755H75.6537C75.8724 17.7988 76.1993 18.1304 76.6344 18.3774C77.0695 18.622 77.554 18.7443 78.0855 18.7443C79.038 18.7443 79.7412 18.3045 80.1904 17.4272C80.6396 16.5476 80.8653 15.1765 80.8653 13.3092V11.3266C80.8653 9.92722 80.7783 8.82892 80.609 8.0387ZM78.0243 13.1492C78.0243 14.0617 77.9867 14.7767 77.9114 15.2941C77.8362 15.8115 77.7115 16.1808 77.5328 16.3971C77.3564 16.6158 77.1165 16.724 76.8178 16.724C76.585 16.724 76.371 16.6699 76.1734 16.5594C75.9759 16.4512 75.816 16.2866 75.6937 16.0702V8.96062C75.7877 8.6196 75.9524 8.34209 76.1852 8.12337C76.4157 7.90465 76.6697 7.79646 76.9401 7.79646C77.2271 7.79646 77.4481 7.90935 77.6034 8.13278C77.7609 8.35855 77.8691 8.73485 77.9303 9.26636C77.9914 9.79787 78.022 10.5528 78.022 11.5335V13.1492H78.0243Z\" class=\"style-scope yt-icon\"></path><path d=\"M84.8657 13.8712C84.8657 14.6755 84.8892 15.2776 84.9363 15.6798C84.9833 16.0819 85.0821 16.3736 85.2326 16.5594C85.3831 16.7428 85.6136 16.8345 85.9264 16.8345C86.3474 16.8345 86.639 16.6699 86.7942 16.343C86.9518 16.0161 87.0365 15.4705 87.0506 14.7085L89.4824 14.8519C89.4965 14.9601 89.5035 15.1106 89.5035 15.3011C89.5035 16.4582 89.186 17.3237 88.5534 17.8952C87.9208 18.4667 87.0247 18.7536 85.8676 18.7536C84.4777 18.7536 83.504 18.3185 82.9466 17.446C82.3869 16.5735 82.1094 15.2259 82.1094 13.4008V11.2136C82.1094 9.33452 82.3987 7.96105 82.9772 7.09558C83.5558 6.2301 84.5459 5.79736 85.9499 5.79736C86.9165 5.79736 87.6597 5.97375 88.1771 6.32888C88.6945 6.684 89.059 7.23433 89.2707 7.98457C89.4824 8.7348 89.5882 9.76961 89.5882 11.0913V13.2362H84.8657V13.8712ZM85.2232 7.96811C85.0797 8.14449 84.9857 8.43377 84.9363 8.83593C84.8892 9.2381 84.8657 9.84722 84.8657 10.6657V11.5641H86.9283V10.6657C86.9283 9.86133 86.9001 9.25221 86.846 8.83593C86.7919 8.41966 86.6931 8.12803 86.5496 7.95635C86.4062 7.78702 86.1851 7.7 85.8864 7.7C85.5854 7.70235 85.3643 7.79172 85.2232 7.96811Z\" class=\"style-scope yt-icon\"></path></g></g></g></svg><!--css-build:shady--></yt-icon></ytd-logo></ytd-yoodle-renderer>
                  </a>
                 <br>
                 </br>")
       }
     })
     
     output$bouton_comparateur <- renderText({
       if(is.null(input$Tablo_rows_selected) == FALSE){
       paste0("<br></br><h2><div style = \"\"><div style = \"text-shadow:0 0 2px #FFD100,0 0 30px #FFD100,0px 0px 5px #FFD100, 0 0 150px #FFD100;color:#FFD100;\"> 
              Comparateur de prix
              </div></h2><div>Trouver les meilleurs annonces occasions de ce jeu vidéo </div></div><br></br>")
       }
       })
     
     output$Tablo2 <- renderDT({
       tablo2() %>% 
         select(Name:platform) %>% 
         datatable(rownames = F,
                          extensions = c('Select'),
                          selection = "single",
                          style = "bootstrap",
                          options = list(
                            select = list(style = 'os', items = 'row'), 
                            dom = 'tpf',
                            ordering = F,
                            pageLength = 10,
                            filter = 'top',
                            columnDefs = list(list(
                              searchable = F, 
                              targets = c(1,2,3,4)
                              ))),
                          escape = F)
     })
     
     output$desc2 <- renderText(expr = {
       # cat(paste(input$Tablo_row_last_clicked))
       # cat(paste(input$type_recommandation))
       
       if(is.null(input$Tablo2_rows_selected) == FALSE){
           desc <- tablo2() %>% 
             slice(input$Tablo2_rows_selected) %>% 
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
     output$image2 <- renderText({
       
       if(is.null(input$Tablo2_rows_selected) == FALSE){
           code_html <- tablo2() %>%
             slice(input$Tablo2_rows_selected) %>%
             pull(code_html)
           paste0(code_html)
         }
     })
     
     output$Gameplay2 <- renderText({
       
       if(is.null(input$Tablo2_rows_selected) == FALSE){
         url <- tablo2() %>%
           slice(input$Tablo2_rows_selected) %>%
           pull(Name) %>% 
           str_to_lower() %>% 
           str_replace_all(" ", "+") %>%
           str_replace_all("/", "%2F") %>% 
           str_replace_all(",", "%2C") %>% 
           str_replace_all("!", "%21") %>% 
           paste( "+gameplay")
         paste0("<br>
                  </br>
                  <a target= \"_blank\" class=\"yt-simple-endpoint style-scope ytd-topbar-logo-renderer\" id=\"logo\" href=\"https://www.youtube.com/results?search_query=",url,"\" title=\"Accueil YouTube\">
                    <div class=\"style-scope ytd-topbar-logo-renderer\">
                      <br>
                      </br>
                      <h2>Trouver un gameplay sur</h2>
                      <ytd-logo class=\"style-scope ytd-topbar-logo-renderer\"><!--css-build:shady--><yt-icon id=\"logo-icon\" class=\"style-scope ytd-logo\"><svg viewBox=\"0 0 90 20\" preserveAspectRatio=\"xMidYMid meet\" focusable=\"false\" class=\"style-scope yt-icon\" style=\"pointer-events: none; display: block; width: 70%; height: 70%;\"><g viewBox=\"0 0 90 20\" preserveAspectRatio=\"xMidYMid meet\" class=\"style-scope yt-icon\"><g class=\"style-scope yt-icon\"><path d=\"M27.9727 3.12324C27.6435 1.89323 26.6768 0.926623 25.4468 0.597366C23.2197 2.24288e-07 14.285 0 14.285 0C14.285 0 5.35042 2.24288e-07 3.12323 0.597366C1.89323 0.926623 0.926623 1.89323 0.597366 3.12324C2.24288e-07 5.35042 0 10 0 10C0 10 2.24288e-07 14.6496 0.597366 16.8768C0.926623 18.1068 1.89323 19.0734 3.12323 19.4026C5.35042 20 14.285 20 14.285 20C14.285 20 23.2197 20 25.4468 19.4026C26.6768 19.0734 27.6435 18.1068 27.9727 16.8768C28.5701 14.6496 28.5701 10 28.5701 10C28.5701 10 28.5677 5.35042 27.9727 3.12324Z\" fill=\"#FF0000\" class=\"style-scope yt-icon\"></path><path d=\"M11.4253 14.2854L18.8477 10.0004L11.4253 5.71533V14.2854Z\" fill=\"white\" class=\"style-scope yt-icon\"></path></g><g class=\"style-scope yt-icon\"><g id=\"youtube-paths\" class=\"style-scope yt-icon\"><path d=\"M34.6024 13.0036L31.3945 1.41846H34.1932L35.3174 6.6701C35.6043 7.96361 35.8136 9.06662 35.95 9.97913H36.0323C36.1264 9.32532 36.3381 8.22937 36.665 6.68892L37.8291 1.41846H40.6278L37.3799 13.0036V18.561H34.6001V13.0036H34.6024Z\" class=\"style-scope yt-icon\"></path><path d=\"M41.4697 18.1937C40.9053 17.8127 40.5031 17.22 40.2632 16.4157C40.0257 15.6114 39.9058 14.5437 39.9058 13.2078V11.3898C39.9058 10.0422 40.0422 8.95805 40.315 8.14196C40.5878 7.32588 41.0135 6.72851 41.592 6.35457C42.1706 5.98063 42.9302 5.79248 43.871 5.79248C44.7976 5.79248 45.5384 5.98298 46.0981 6.36398C46.6555 6.74497 47.0647 7.34234 47.3234 8.15137C47.5821 8.96275 47.7115 10.0422 47.7115 11.3898V13.2078C47.7115 14.5437 47.5845 15.6161 47.3329 16.4251C47.0812 17.2365 46.672 17.8292 46.1075 18.2031C45.5431 18.5771 44.7764 18.7652 43.8098 18.7652C42.8126 18.7675 42.0342 18.5747 41.4697 18.1937ZM44.6353 16.2323C44.7905 15.8231 44.8705 15.1575 44.8705 14.2309V10.3292C44.8705 9.43077 44.7929 8.77225 44.6353 8.35833C44.4777 7.94206 44.2026 7.7351 43.8074 7.7351C43.4265 7.7351 43.156 7.94206 43.0008 8.35833C42.8432 8.77461 42.7656 9.43077 42.7656 10.3292V14.2309C42.7656 15.1575 42.8408 15.8254 42.9914 16.2323C43.1419 16.6415 43.4123 16.8461 43.8074 16.8461C44.2026 16.8461 44.4777 16.6415 44.6353 16.2323Z\" class=\"style-scope yt-icon\"></path><path d=\"M56.8154 18.5634H54.6094L54.3648 17.03H54.3037C53.7039 18.1871 52.8055 18.7656 51.6061 18.7656C50.7759 18.7656 50.1621 18.4928 49.767 17.9496C49.3719 17.4039 49.1743 16.5526 49.1743 15.3955V6.03751H51.9942V15.2308C51.9942 15.7906 52.0553 16.188 52.1776 16.4256C52.2999 16.6631 52.5045 16.783 52.7914 16.783C53.036 16.783 53.2712 16.7078 53.497 16.5573C53.7228 16.4067 53.8874 16.2162 53.9979 15.9858V6.03516H56.8154V18.5634Z\" class=\"style-scope yt-icon\"></path><path d=\"M64.4755 3.68758H61.6768V18.5629H58.9181V3.68758H56.1194V1.42041H64.4755V3.68758Z\" class=\"style-scope yt-icon\"></path><path d=\"M71.2768 18.5634H69.0708L68.8262 17.03H68.7651C68.1654 18.1871 67.267 18.7656 66.0675 18.7656C65.2373 18.7656 64.6235 18.4928 64.2284 17.9496C63.8333 17.4039 63.6357 16.5526 63.6357 15.3955V6.03751H66.4556V15.2308C66.4556 15.7906 66.5167 16.188 66.639 16.4256C66.7613 16.6631 66.9659 16.783 67.2529 16.783C67.4974 16.783 67.7326 16.7078 67.9584 16.5573C68.1842 16.4067 68.3488 16.2162 68.4593 15.9858V6.03516H71.2768V18.5634Z\" class=\"style-scope yt-icon\"></path><path d=\"M80.609 8.0387C80.4373 7.24849 80.1621 6.67699 79.7812 6.32186C79.4002 5.96674 78.8757 5.79035 78.2078 5.79035C77.6904 5.79035 77.2059 5.93616 76.7567 6.23014C76.3075 6.52412 75.9594 6.90747 75.7148 7.38489H75.6937V0.785645H72.9773V18.5608H75.3056L75.5925 17.3755H75.6537C75.8724 17.7988 76.1993 18.1304 76.6344 18.3774C77.0695 18.622 77.554 18.7443 78.0855 18.7443C79.038 18.7443 79.7412 18.3045 80.1904 17.4272C80.6396 16.5476 80.8653 15.1765 80.8653 13.3092V11.3266C80.8653 9.92722 80.7783 8.82892 80.609 8.0387ZM78.0243 13.1492C78.0243 14.0617 77.9867 14.7767 77.9114 15.2941C77.8362 15.8115 77.7115 16.1808 77.5328 16.3971C77.3564 16.6158 77.1165 16.724 76.8178 16.724C76.585 16.724 76.371 16.6699 76.1734 16.5594C75.9759 16.4512 75.816 16.2866 75.6937 16.0702V8.96062C75.7877 8.6196 75.9524 8.34209 76.1852 8.12337C76.4157 7.90465 76.6697 7.79646 76.9401 7.79646C77.2271 7.79646 77.4481 7.90935 77.6034 8.13278C77.7609 8.35855 77.8691 8.73485 77.9303 9.26636C77.9914 9.79787 78.022 10.5528 78.022 11.5335V13.1492H78.0243Z\" class=\"style-scope yt-icon\"></path><path d=\"M84.8657 13.8712C84.8657 14.6755 84.8892 15.2776 84.9363 15.6798C84.9833 16.0819 85.0821 16.3736 85.2326 16.5594C85.3831 16.7428 85.6136 16.8345 85.9264 16.8345C86.3474 16.8345 86.639 16.6699 86.7942 16.343C86.9518 16.0161 87.0365 15.4705 87.0506 14.7085L89.4824 14.8519C89.4965 14.9601 89.5035 15.1106 89.5035 15.3011C89.5035 16.4582 89.186 17.3237 88.5534 17.8952C87.9208 18.4667 87.0247 18.7536 85.8676 18.7536C84.4777 18.7536 83.504 18.3185 82.9466 17.446C82.3869 16.5735 82.1094 15.2259 82.1094 13.4008V11.2136C82.1094 9.33452 82.3987 7.96105 82.9772 7.09558C83.5558 6.2301 84.5459 5.79736 85.9499 5.79736C86.9165 5.79736 87.6597 5.97375 88.1771 6.32888C88.6945 6.684 89.059 7.23433 89.2707 7.98457C89.4824 8.7348 89.5882 9.76961 89.5882 11.0913V13.2362H84.8657V13.8712ZM85.2232 7.96811C85.0797 8.14449 84.9857 8.43377 84.9363 8.83593C84.8892 9.2381 84.8657 9.84722 84.8657 10.6657V11.5641H86.9283V10.6657C86.9283 9.86133 86.9001 9.25221 86.846 8.83593C86.7919 8.41966 86.6931 8.12803 86.5496 7.95635C86.4062 7.78702 86.1851 7.7 85.8864 7.7C85.5854 7.70235 85.3643 7.79172 85.2232 7.96811Z\" class=\"style-scope yt-icon\"></path></g></g></g></svg><!--css-build:shady--></yt-icon></ytd-logo>
                    </div>
                    <ytd-yoodle-renderer class=\"style-scope ytd-topbar-logo-renderer\" hidden=\"\">
                      <!--css-build:shady-->
                        <picture class=\"style-scope ytd-yoodle-renderer\">
                          <source type=\"image/webp\" class=\"style-scope ytd-yoodle-renderer\" srcset=\"\">
                            <img class=\"style-scope ytd-yoodle-renderer\" src=\"\">
                        </picture>
                    <ytd-logo class=\"style-scope ytd-yoodle-renderer\" hidden=\"\"><!--css-build:shady--><yt-icon id=\"logo-icon\" class=\"style-scope ytd-logo\"><svg viewBox=\"0 0 90 20\" preserveAspectRatio=\"xMidYMid meet\" focusable=\"false\" class=\"style-scope yt-icon\" style=\"pointer-events: none; display: block; width: 70%; height: 70%;\"><g viewBox=\"0 0 90 20\" preserveAspectRatio=\"xMidYMid meet\" class=\"style-scope yt-icon\"><g class=\"style-scope yt-icon\"><path d=\"M27.9727 3.12324C27.6435 1.89323 26.6768 0.926623 25.4468 0.597366C23.2197 2.24288e-07 14.285 0 14.285 0C14.285 0 5.35042 2.24288e-07 3.12323 0.597366C1.89323 0.926623 0.926623 1.89323 0.597366 3.12324C2.24288e-07 5.35042 0 10 0 10C0 10 2.24288e-07 14.6496 0.597366 16.8768C0.926623 18.1068 1.89323 19.0734 3.12323 19.4026C5.35042 20 14.285 20 14.285 20C14.285 20 23.2197 20 25.4468 19.4026C26.6768 19.0734 27.6435 18.1068 27.9727 16.8768C28.5701 14.6496 28.5701 10 28.5701 10C28.5701 10 28.5677 5.35042 27.9727 3.12324Z\" fill=\"#FF0000\" class=\"style-scope yt-icon\"></path><path d=\"M11.4253 14.2854L18.8477 10.0004L11.4253 5.71533V14.2854Z\" fill=\"white\" class=\"style-scope yt-icon\"></path></g><g class=\"style-scope yt-icon\"><g id=\"youtube-paths\" class=\"style-scope yt-icon\"><path d=\"M34.6024 13.0036L31.3945 1.41846H34.1932L35.3174 6.6701C35.6043 7.96361 35.8136 9.06662 35.95 9.97913H36.0323C36.1264 9.32532 36.3381 8.22937 36.665 6.68892L37.8291 1.41846H40.6278L37.3799 13.0036V18.561H34.6001V13.0036H34.6024Z\" class=\"style-scope yt-icon\"></path><path d=\"M41.4697 18.1937C40.9053 17.8127 40.5031 17.22 40.2632 16.4157C40.0257 15.6114 39.9058 14.5437 39.9058 13.2078V11.3898C39.9058 10.0422 40.0422 8.95805 40.315 8.14196C40.5878 7.32588 41.0135 6.72851 41.592 6.35457C42.1706 5.98063 42.9302 5.79248 43.871 5.79248C44.7976 5.79248 45.5384 5.98298 46.0981 6.36398C46.6555 6.74497 47.0647 7.34234 47.3234 8.15137C47.5821 8.96275 47.7115 10.0422 47.7115 11.3898V13.2078C47.7115 14.5437 47.5845 15.6161 47.3329 16.4251C47.0812 17.2365 46.672 17.8292 46.1075 18.2031C45.5431 18.5771 44.7764 18.7652 43.8098 18.7652C42.8126 18.7675 42.0342 18.5747 41.4697 18.1937ZM44.6353 16.2323C44.7905 15.8231 44.8705 15.1575 44.8705 14.2309V10.3292C44.8705 9.43077 44.7929 8.77225 44.6353 8.35833C44.4777 7.94206 44.2026 7.7351 43.8074 7.7351C43.4265 7.7351 43.156 7.94206 43.0008 8.35833C42.8432 8.77461 42.7656 9.43077 42.7656 10.3292V14.2309C42.7656 15.1575 42.8408 15.8254 42.9914 16.2323C43.1419 16.6415 43.4123 16.8461 43.8074 16.8461C44.2026 16.8461 44.4777 16.6415 44.6353 16.2323Z\" class=\"style-scope yt-icon\"></path><path d=\"M56.8154 18.5634H54.6094L54.3648 17.03H54.3037C53.7039 18.1871 52.8055 18.7656 51.6061 18.7656C50.7759 18.7656 50.1621 18.4928 49.767 17.9496C49.3719 17.4039 49.1743 16.5526 49.1743 15.3955V6.03751H51.9942V15.2308C51.9942 15.7906 52.0553 16.188 52.1776 16.4256C52.2999 16.6631 52.5045 16.783 52.7914 16.783C53.036 16.783 53.2712 16.7078 53.497 16.5573C53.7228 16.4067 53.8874 16.2162 53.9979 15.9858V6.03516H56.8154V18.5634Z\" class=\"style-scope yt-icon\"></path><path d=\"M64.4755 3.68758H61.6768V18.5629H58.9181V3.68758H56.1194V1.42041H64.4755V3.68758Z\" class=\"style-scope yt-icon\"></path><path d=\"M71.2768 18.5634H69.0708L68.8262 17.03H68.7651C68.1654 18.1871 67.267 18.7656 66.0675 18.7656C65.2373 18.7656 64.6235 18.4928 64.2284 17.9496C63.8333 17.4039 63.6357 16.5526 63.6357 15.3955V6.03751H66.4556V15.2308C66.4556 15.7906 66.5167 16.188 66.639 16.4256C66.7613 16.6631 66.9659 16.783 67.2529 16.783C67.4974 16.783 67.7326 16.7078 67.9584 16.5573C68.1842 16.4067 68.3488 16.2162 68.4593 15.9858V6.03516H71.2768V18.5634Z\" class=\"style-scope yt-icon\"></path><path d=\"M80.609 8.0387C80.4373 7.24849 80.1621 6.67699 79.7812 6.32186C79.4002 5.96674 78.8757 5.79035 78.2078 5.79035C77.6904 5.79035 77.2059 5.93616 76.7567 6.23014C76.3075 6.52412 75.9594 6.90747 75.7148 7.38489H75.6937V0.785645H72.9773V18.5608H75.3056L75.5925 17.3755H75.6537C75.8724 17.7988 76.1993 18.1304 76.6344 18.3774C77.0695 18.622 77.554 18.7443 78.0855 18.7443C79.038 18.7443 79.7412 18.3045 80.1904 17.4272C80.6396 16.5476 80.8653 15.1765 80.8653 13.3092V11.3266C80.8653 9.92722 80.7783 8.82892 80.609 8.0387ZM78.0243 13.1492C78.0243 14.0617 77.9867 14.7767 77.9114 15.2941C77.8362 15.8115 77.7115 16.1808 77.5328 16.3971C77.3564 16.6158 77.1165 16.724 76.8178 16.724C76.585 16.724 76.371 16.6699 76.1734 16.5594C75.9759 16.4512 75.816 16.2866 75.6937 16.0702V8.96062C75.7877 8.6196 75.9524 8.34209 76.1852 8.12337C76.4157 7.90465 76.6697 7.79646 76.9401 7.79646C77.2271 7.79646 77.4481 7.90935 77.6034 8.13278C77.7609 8.35855 77.8691 8.73485 77.9303 9.26636C77.9914 9.79787 78.022 10.5528 78.022 11.5335V13.1492H78.0243Z\" class=\"style-scope yt-icon\"></path><path d=\"M84.8657 13.8712C84.8657 14.6755 84.8892 15.2776 84.9363 15.6798C84.9833 16.0819 85.0821 16.3736 85.2326 16.5594C85.3831 16.7428 85.6136 16.8345 85.9264 16.8345C86.3474 16.8345 86.639 16.6699 86.7942 16.343C86.9518 16.0161 87.0365 15.4705 87.0506 14.7085L89.4824 14.8519C89.4965 14.9601 89.5035 15.1106 89.5035 15.3011C89.5035 16.4582 89.186 17.3237 88.5534 17.8952C87.9208 18.4667 87.0247 18.7536 85.8676 18.7536C84.4777 18.7536 83.504 18.3185 82.9466 17.446C82.3869 16.5735 82.1094 15.2259 82.1094 13.4008V11.2136C82.1094 9.33452 82.3987 7.96105 82.9772 7.09558C83.5558 6.2301 84.5459 5.79736 85.9499 5.79736C86.9165 5.79736 87.6597 5.97375 88.1771 6.32888C88.6945 6.684 89.059 7.23433 89.2707 7.98457C89.4824 8.7348 89.5882 9.76961 89.5882 11.0913V13.2362H84.8657V13.8712ZM85.2232 7.96811C85.0797 8.14449 84.9857 8.43377 84.9363 8.83593C84.8892 9.2381 84.8657 9.84722 84.8657 10.6657V11.5641H86.9283V10.6657C86.9283 9.86133 86.9001 9.25221 86.846 8.83593C86.7919 8.41966 86.6931 8.12803 86.5496 7.95635C86.4062 7.78702 86.1851 7.7 85.8864 7.7C85.5854 7.70235 85.3643 7.79172 85.2232 7.96811Z\" class=\"style-scope yt-icon\"></path></g></g></g></svg><!--css-build:shady--></yt-icon></ytd-logo></ytd-yoodle-renderer>
                  </a>
                 <br>
                 </br>")
       }
     })
     
     output$bouton_comparateur2 <- renderText({
       # paste0("<br></br><h2><div style = \"background-color:#2E8ECE ;\"><div style = \"text-shadow:0 0 2px #FFD100,0 0 30px #FFD100,0px 0px 5px #FFD100, 0 0 150px #FFD100;color:#FFD100; background-color:#2E8ECE ;\"> 
       #        Comparateur de prix
       #        </div></h2><div>Trouver les meilleurs annonces occasions de ce jeu vidéo </div></div>")
     })
     
     output$P1 <- renderText({
       paste0("<div>
                <center>
                  <h2>
                    P1
                  </h2>
                  <img src=\"https://cdn02.nintendo-europe.com/media/images/08_content_images/games_6/nintendo_switch_7/nswitch_ultrastreetfighter2thefinalchallengers/CI_NSwitch_UltraStreetFighter2TheFinalChallengers_EvilRyu_Select.jpg\" title=\"selectize input in Shiny\" style=\"display: block; margin: auto ;max-width: 100%; height: auto;display: block;\">
                  <h3>Chaveneau Lucas</h3>
                </center>
                <div style = \"width = 33; font-size: 110%; font-weight: bold\">
                  Speciality:
                </div>
                <div style = \"width = 66; padding-left: 1em; padding-bottom: 0.25em;\">
                 Scraping your html-brain!
                </div>
                <div style = \"width = 33; font-size: 110%; font-weight: bold\">
                  Fatality:
                </div>
                <div style = \"width = 66; padding-left: 1em; padding-bottom: 0.25em; \">
                  Throwing some tree branch from my random forest!
                </div>
                <div style = \"width = 33; font-size: 110%; font-weight: bold\">
                  Ulti:
                </div>
                <div style = \"width = 66; padding-left: 1em; padding-bottom: 0.25em; \">
                  Hyptonize with regex!
                </div>
              
                <br>
                </br>
                
                <div style = \"width = 33; font-size: 110%; font-weight: bold\">
                  Class:
                </div>
                <div style = \"width = 66; padding-left: 1em; padding-bottom: 0.25em; \">
                  Student
                </div>
              
                <div style = \"width = 33; font-size: 110%; font-weight: bold\">
                  Division
                </div>
                <div style = \"width = 66; padding-left: 1em; padding-bottom: 0.25em; \">
                  DataScience
                </div>
                <div style = \"width = 33; font-size: 110%; font-weight: bold\">
                  Race:
                </div>
                <div style = \"width = 66; padding-left: 1em; padding-bottom: 0.25em; \">
                  Economist
                </div>
                <br>
                </br>
                <div>
                  <a target= \"_blank\" href = https://github.com/LChaveneau>
                    See more about player's background
                </div>
              </div>")
     })
}



# Run the application 
shinyApp(ui = ui, server = server)
