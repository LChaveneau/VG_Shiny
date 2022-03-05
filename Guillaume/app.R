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
library(questionr)
library(leaflet)

df <- read_csv('vgsales.csv')

df <- df  %>% 
  mutate(Platform = as.factor(Platform)) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Genre = as.factor(Genre)) %>% 
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
  mutate(popup_info=paste("Région:",
                          name,
                          "<br/>",
                          "Jeu:",
                          Name,
                          "<br/>",
                          "Nombres de ventes:",
                          Sales,
                          "Millions" ))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Video Games Analyse"),
    
    ## Page
    navbarPage("Pages",
      tabPanel("Graphiques",
        tabsetPanel(
          tabPanel("Nombre",
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("years",
                                   "Choix de l'année:",
                                   min = 1980,
                                   max = 2017,
                                   value = 2016)
                     ,
                     radioButtons("var", 
                                  label = h3("Choix de la variable:"),
                                  choices = list("Concepteur" = "Publisher", "Type de jeu" = "Genre"), 
                                  selected = "Genre")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                       plotOutput("Hist")
                     )
                   )
          ),
          tabPanel("Ventes", 
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
                                      "Concepteur" = "Publisher", 
                                      "Type de jeu" = "Genre", 
                                      "Video Games" = "Name", 
                                      "Platforme" = "Platform"
                                      ), 
                                    selected = "Name")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                       plotOutput("SalesHist")
                     )
                   )),
          tabPanel("Cartographie de ventes",
                   textInput("Nom", "Choix du jeu:",
                             value="The Legend of Zelda"),
                   selectInput("plateform2", label = h4("Plateform"), 
                               choices = c("--", as.list(levels(df$Platform))),
                               selected = "NES"),
                   #verbatimTextOutput("test"),
                   leafletOutput("map_vente"))
        )
      ),  
      tabPanel("Recommandation",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("years3",
                              "Annee :",
                              min = 1980,
                              max = 2017,
                              value = c(1980,2017)),
                  
                  selectInput("plateform1", label = h3("Plateform"), 
                              choices = c("--", as.list(levels(df$Platform))),
                              selected = "--"),
                  selectInput("genre1", label = h3("Type"), 
                              choices = c("--", as.list(levels(df$Genre))),
                              selected = "--"),
                  
                ),
                mainPanel(
                  tableOutput("Tablo")
                  )
                )
              )
      )
    )
    # Sidebar with a slider input for number of bins 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
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
    
  

  
  
    output$Hist <- renderPlot({
          fct_count(variable()[df$Year == input$years], sort = T) %>%
          slice(1:10) %>%
          ggplot() +
          aes(x=fct_reorder(f, n), y = n) +
          geom_col() +
          coord_flip()+
        theme_minimal()+
        ylab(label="Nombres")+
        xlab(label=input$var)+
        ggtitle(paste("Nombre totale par", input$var, "de jeu"))
      })
    ## Regarder comment faire pour changer de nom de variable avec dyplr avec un vecteur sans dict. 
    ## Cela permettrait d'eviter de crÃ©er la variable tablo et de tout faire d'un seul coup
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
        xlab(label=input$var2)+
        ylab(label="Ventes totales")+
        theme_minimal()+
        ggtitle(paste("Ventes totales par", input$var2))+
        coord_flip()
    })
    
    
    #output$value <- renderText({input$caption })
    
    
    observe({
      
      data_complet_2<- data_complet %>% 
        filter(Name == input$Nom)
      
      choices = unique(data_complet_2$Platform)
      
      updateSelectInput(session, "plateform2", choices= choices, selected = choices[1])
    })
    
    
   
    
    output$map_vente <- renderLeaflet({
      
      leaflet(data_complet %>% 
                filter(Name == input$Nom & Platform == input$plateform2)) %>% addProviderTiles(providers$CartoDB.DarkMatter)  %>% 
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
    
    
    output$Tablo <- renderTable({
      
      tablo <-df %>%
        filter(as.numeric(as.character(Year)) >= input$years3[1]) %>%
        filter(as.numeric(as.character(Year)) <= input$years3[2])
      
      if(input$plateform1 != "--"){
        tablo <- tablo %>% 
          filter(Platform == input$plateform1)
        }
      if(input$genre1 != "--"){
        tablo <- tablo %>% 
          filter(Genre == input$genre1)
      }
      tablo %>% 
        slice(1:3) %>% 
        select("Name":"Publisher")
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)



