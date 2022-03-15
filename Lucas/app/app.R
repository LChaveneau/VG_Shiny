#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidyr)
library(DT)

df <- read_csv('vgsales2.csv')
df2 <- readRDS('data_jeux_image.rds')

df <- df  %>% 
  mutate(Platform = as.factor(Platform)) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Genre = as.factor(Genre)) %>% 
  mutate(Publisher = as.factor(Publisher))


df2 <- df2  %>% 
  mutate(Publisher = as.factor(Publisher)) %>% 
  mutate(Annee = as.factor(Annee)) %>% 
  mutate(platform = as.factor(platform))



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
#   "Strategie" = df2$Strategie,
#   "Role_play" = df2$Role_play,
#   "Edition_special" = df2$Edition_special,
#   "DLC" = df2$DLC)
# }
# >>>>>>

# Define UI for application that draws a histogram
ui <- fluidPage(  

    # Application title
    titlePanel("Video Games"),
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
                  DTOutput("Tablo")
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
               selectizeInput("Test",
                              label = h3('Test'),
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
                                          "DLC" = "DLC"),
                              multiple = TRUE,
                              options = list(dropdownParent = 'body', 
                                             maxitems = 'null', 
                                             plugins = list("remove_button"),
                                             highlight = F,
                                             closeAfterSelect = T)
                              )
               )
      ),
    theme = shinythemes::shinytheme('united')
    )
    # Sidebar with a slider input for number of bins 

# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
        paste()
      }
    }
    })
     output$desc2 <- renderPrint({
       print(input$Tablo_rows_selected)
       print(input$type_recommandation)
     })
     
     output$image <- renderText({       
       if(is.null(input$Tablo_rows_selected) == FALSE){
         if(input$type_recommandation == "Popularite"){
         image <- tablo() %>%
           slice(input$Tablo_rows_selected) %>%
           pull(image)
       }
         if(input$type_recommandation == "Vente"){
           image <- "<table class=\"infobox hproduct\" style=\"float: right; width: 22em;\"><tbody><tr><th class=\"infobox-above fn\" colspan=\"2\" style=\"font-size:125%;font-style:italic;\">Wii Sports</th></tr><tr><td class=\"infobox-image\" colspan=\"2\"><a class=\"image\" href=\"https://en.wikipedia.org/wiki/File:Wii_Sports_Europe.jpg\"><img alt='Artwork of a vertical rectangular box. The top third displays three screen shots from the game: two characters with boxing gloves fighting in a boxing ring, a character holding a bowling ball at a ball pit, and a character holding a golf at the putting green of a golf course The Wii logo is shown at the upper left corner. The center portion reads \"Wii Sports\" over five blue boxes depicting different sports equipment. The lower third displays two more screen shots from the game: a character holding a tennis racket at a Tennis court and a character swinging a baseball bat in a stadium. The PEGI \"7+\" rating is shown on the bottom left corner and the Nintendo logo is on the bottom right corner.' data-file-height=\"365\" data-file-width=\"260\" decoding=\"async\" height=\"309\" src=\"https://upload.wikimedia.org/wikipedia/en/thumb/e/e0/Wii_Sports_Europe.jpg/220px-Wii_Sports_Europe.jpg\" srcset=\"https://upload.wikimedia.org/wikipedia/en/e/e0/Wii_Sports_Europe.jpg 1.5x\" width=\"220\"/></a><div class=\"infobox-caption\">European box art depicting the game avatars, <a href=\"https://en.wikipedia.org/wiki/Mii\" title=\"Mii\">Miis</a>, playing the five sports: (clockwise from top left) <a href=\"https://en.wikipedia.org/wiki/Boxing\" title=\"Boxing\">boxing</a>, <a href=\"https://en.wikipedia.org/wiki/Bowling\" title=\"Bowling\">bowling</a>, <a href=\"https://en.wikipedia.org/wiki/Golf\" title=\"Golf\">golf</a>, <a href=\"https://en.wikipedia.org/wiki/Baseball\" title=\"Baseball\">baseball</a>, and <a href=\"https://en.wikipedia.org/wiki/Tennis\" title=\"Tennis\">tennis</a></div></td></tr><tr><th class=\"infobox-label\" scope=\"row\" style=\"white-space:nowrap;padding-right:0.65em;\"><a href=\"https://en.wikipedia.org/wiki/Video_game_developer\" title=\"Video game developer\">Developer(s)</a></th><td class=\"infobox-data\"><a class=\"mw-redirect\" href=\"https://en.wikipedia.org/wiki/Nintendo_EAD\" title=\"Nintendo EAD\">Nintendo EAD</a></td></tr><tr><th class=\"infobox-label\" scope=\"row\" style=\"white-space:nowrap;padding-right:0.65em;\"><a href=\"https://en.wikipedia.org/wiki/Video_game_publisher\" title=\"Video game publisher\">Publisher(s)</a></th><td class=\"infobox-data\"><a href=\"https://en.wikipedia.org/wiki/Nintendo\" title=\"Nintendo\">Nintendo</a></td></tr><tr><th class=\"infobox-label\" scope=\"row\" style=\"white-space:nowrap;padding-right:0.65em;\"><a class=\"mw-redirect\" href=\"https://en.wikipedia.org/wiki/Video_game_creative_director\" title=\"Video game creative director\">Director(s)</a></th><td class=\"infobox-data\"><div class=\"plainlist\"><ul><li>Keizo Ohta</li><li>Takayuki Shimamura</li><li>Yoshikazu Yamashita</li></ul></div></td></tr><tr><th class=\"infobox-label\" scope=\"row\" style=\"white-space:nowrap;padding-right:0.65em;\"><a href=\"https://en.wikipedia.org/wiki/Video_game_producer\" title=\"Video game producer\">Producer(s)</a></th><td class=\"infobox-data\"><div class=\"plainlist\"><ul><li><a href=\"https://en.wikipedia.org/wiki/Katsuya_Eguchi\" title=\"Katsuya Eguchi\">Katsuya Eguchi</a></li><li>Kiyoshi Mizuki</li></ul></div></td></tr><tr><th class=\"infobox-label\" scope=\"row\" style=\"white-space:nowrap;padding-right:0.65em;\"><a class=\"mw-redirect\" href=\"https://en.wikipedia.org/wiki/Video_game_designer\" title=\"Video game designer\">Designer(s)</a></th><td class=\"infobox-data\">Junji Morii</td></tr><tr><th class=\"infobox-label\" scope=\"row\" style=\"white-space:nowrap;padding-right:0.65em;\"><a href=\"https://en.wikipedia.org/wiki/Video_game_programmer\" title=\"Video game programmer\">Programmer(s)</a></th><td class=\"infobox-data\">Tsutomu Kaneshige</td></tr><tr><th class=\"infobox-label\" scope=\"row\" style=\"white-space:nowrap;padding-right:0.65em;\"><a class=\"mw-redirect\" href=\"https://en.wikipedia.org/wiki/Video_game_composer\" title=\"Video game composer\">Composer(s)</a></th><td class=\"infobox-data\"><a href=\"https://en.wikipedia.org/wiki/Kazumi_Totaka\" title=\"Kazumi Totaka\">Kazumi Totaka</a></td></tr><tr><th class=\"infobox-label\" scope=\"row\" style=\"white-space:nowrap;padding-right:0.65em;\">Series</th><td class=\"infobox-data\"><i><a href=\"https://en.wikipedia.org/wiki/Wii_(video_game_series)\" title=\"Wii (video game series)\">Wii</a></i></td></tr><tr><th class=\"infobox-label\" scope=\"row\" style=\"white-space:nowrap;padding-right:0.65em;\"><a href=\"https://en.wikipedia.org/wiki/Computing_platform\" title=\"Computing platform\">Platform(s)</a></th><td class=\"infobox-data\"><a href=\"https://en.wikipedia.org/wiki/Wii\" title=\"Wii\">Wii</a></td></tr><tr><th class=\"infobox-label\" scope=\"row\" style=\"white-space:nowrap;padding-right:0.65em;\">Release</th><td class=\"infobox-data\"><div class=\"plainlist\"><ul><li><span style=\"font-size:95%;\"><a href=\"https://en.wikipedia.org/wiki/North_America\" title=\"North America\">NA</a>:</span> November 19, 2006</li><li><span style=\"font-size:95%;\"><a href=\"https://en.wikipedia.org/wiki/Japan\" title=\"Japan\">JP</a>:</span> December 2, 2006</li><li><span style=\"font-size:95%;\"><a href=\"https://en.wikipedia.org/wiki/Australasia\" title=\"Australasia\">AU</a>:</span> December 7, 2006</li><li><span style=\"font-size:95%;\"><a href=\"https://en.wikipedia.org/wiki/Europe\" title=\"Europe\">EU</a>:</span> December 8, 2006</li><li><span style=\"font-size:95%;\"><a href=\"https://en.wikipedia.org/wiki/South_Korea\" title=\"South Korea\">KOR</a>:</span> April 26, 2008</li></ul></div></td></tr><tr><th class=\"infobox-label\" scope=\"row\" style=\"white-space:nowrap;padding-right:0.65em;\"><a href=\"https://en.wikipedia.org/wiki/Video_game_genre\" title=\"Video game genre\">Genre(s)</a></th><td class=\"infobox-data\"><a class=\"mw-redirect\" href=\"https://en.wikipedia.org/wiki/Sports_game\" title=\"Sports game\">Sports</a></td></tr><tr><th class=\"infobox-label\" scope=\"row\" style=\"white-space:nowrap;padding-right:0.65em;\">Mode(s)</th><td class=\"infobox-data\"><a class=\"mw-redirect\" href=\"https://en.wikipedia.org/wiki/Single-player\" title=\"Single-player\">Single-player</a>, <a href=\"https://en.wikipedia.org/wiki/Multiplayer_video_game\" title=\"Multiplayer video game\">multiplayer</a></td></tr></tbody></table>"         
           }
         paste0(image)
       }
     })
}

# Run the application 
shinyApp(ui = ui, server = server)
