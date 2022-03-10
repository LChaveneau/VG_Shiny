library(rjson)
library(jsonlite)
library(data.table)
library(dplyr)
library(stringr)
library(questionr)
library(xtable)
library(tidyr)
library(stringr)
library(RPushbullet)

### Non nécessaire maintenant
#data<-read_json("data_lisible_1.json",simplifyVector = TRUE)
#data_2<-read_json("data_lisible_2.json",simplifyVector = TRUE)
#data_3<-read_json("data_lisible_3.json",simplifyVector = TRUE)


data_final<-rbind(data, data_2)

data_final=data_final[!duplicated(cbind(data_final$titre,
                                        data_final$platform)),]

data_final=data_final[!duplicated(cbind(data_final$titre,
                                    data_final$platform,
                                    data_final$genre,
                                    data_final$published,
                                    data_final$developed,
                                    data_final$released,
                                    data_final$perspective,
                                    data_final$gameplay,
                                    data_final$setting,
                                    data_final$narrative,
                                    data_final$edition,
                                    data_final$add_on,
                                    data_final$visual,
                                    data_final$misc)),]

#### Backup 1
 
saveRDS(data_final, file="data_jeux.rds")


data<-readRDS(file="data_jeux.rds")
data<-data[!is.na(data$platform),]
data_final_2<-rbind(data, data_3)

data_final_2=data_final_2[!duplicated(cbind(data_final_2$titre,
                                        data_final_2$platform,
                                        data_final_2$genre,
                                        data_final_2$published,
                                        data_final_2$developed,
                                        data_final_2$released,
                                        data_final_2$perspective,
                                        data_final_2$gameplay,
                                        data_final_2$setting,
                                        data_final_2$narrative,
                                        data_final_2$edition,
                                        data_final_2$add_on,
                                        data_final_2$visual,
                                        data_final_2$misc)),]

## Backup final
saveRDS(data_final_2, file="data_jeux_2.rds")

###########################################################################
###########################################################################
###########################################################################

######################################################################
######################################################################
############### Debut nettoyage ##############################################


data<-readRDS("data_jeux_2.rds")

## Platforme arrangement

data_test<-data %>% separate(platform, c("a", "b", "c", "d"), sep=",")
data_test<-rename.variable(data_test , "a", "platform")

data_b<-data_test[!is.na(data_test$b),]
data_b<-data_b[,c(1:5, 7, 10:19)]
data_b<-rename.variable(data_b , "b", "platform")

data_c<-data_test[!is.na(data_test$c),]
data_c<-data_c[,c(1:5, 8, 10:19)]
data_c<-rename.variable(data_c , "c", "platform")

data_d<-data_test[!is.na(data_test$d),]
data_d<-data_d[,c(1:5, 9:19)]
data_d<-rename.variable(data_d , "d", "platform")


data_test<-data_test[,c(1:6, 10:19)]

data_test<-rbind(data_test, data_b, data_c, data_d)
data<-data_test

### Add-on
data<-data[is.na(data$add_on),]
data<-data[,-c(4,9, 11:14)]

### Titre
data<-data[!is.na(data$titre),] #Pas de NA pour titre
data<-rename.variable(data,"titre", "Name") #Rename pour que ce soit pareil que la base de la prof


## Published

data<-data[!is.na(data$published),]


data$published<-gsub("Activision.*", "Activision", data$published)
data$published<-gsub("Ubi.*", "Ubisoft", data$published)
data$published<-gsub("SEGA.*", "SEGA", data$published)
data$published<-gsub("Sony.*", "Sony", data$published)
data$published<-gsub("KONAMI.*", "KONAMI", data$published)
data$published<-gsub("Arts.*", " Arts", data$published)
data$published<-gsub("Nintendo.*", "Nintendo", data$published)
data$published<-gsub("Amiga.*", "Amiga", data$published)
data$published<-gsub("AMC.*", "AMC", data$published)
data$published<-gsub("Amber.*", "Amber", data$published)
data$published<-gsub("Amaranth.*", "Amaranth", data$published)
data$published<-gsub("Amaro.*", "Amaro", data$published)
data$published<-gsub("Alternative.*", "Alternative", data$published)
data$published<-gsub("Altair.*", "Altair", data$published)
data$published<-gsub("Alpha .*", "Alpha", data$published)
data$published<-gsub("Alliance .*", "Alliance", data$published)
data$published<-gsub("Alien .*", "Alien", data$published)

data<-data %>% separate(published, c("Publisher", "b"), sep=",")
data<-data[,-4]

table(data$Publisher) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))



## Platform

data$platform<-gsub("^\\s+|\\s+$", "", data$platform) 
data<-data[!is.na(data$platform),]

# Remove
data<-subset(data, platform != "PlayStation Now")
data<-subset(data, platform != "Blu-ray Disc Player")
data<-subset(data, platform != "Bubble")
data<-subset(data, platform != "bada")
data<-subset(data, platform != "Gloud")
data<-subset(data, platform != "LeapTV")
data<-subset(data, platform != "N-Gage (service)")

# Modif
data$platform <- gsub("Windows.*", "PC", data$platform)
data$platform <- str_replace_all(data$platform, "Browser", "PC")
data$platform <- str_replace_all(data$platform, "iphone", "Mobile")
data$platform <- str_replace_all(data$platform, "Android", "Mobile")

table(data$platform) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

## Review

data$reviews<-round(data$reviews, 2)

## Released

data$released<-str_replace(data$released, ".*,", "")
data$released<-str_replace(data$released, " ", "")
data<-rename.variable(data,"released", "Annee")

#Licensed
data$misc<-ifelse(is.na(data$misc), 0, 1)
data<-rename.variable(data,"misc", "Licence")


## Genre

data<-data[!is.na(data$genre),]


data <- data %>% mutate(Action = str_detect(data$genre, "Action"))
data <- data %>% mutate(Aventure = str_detect(data$genre, "Adventure"))
data <- data %>% mutate(Puzzle = str_detect(data$genre, "Puzzle"))
data <- data %>% mutate(Racing = str_detect(data$genre, "Racing"))
data <- data %>% mutate(Educational = str_detect(data$genre, "Educational"))
data <- data %>% mutate(Compilation = str_detect(data$genre, "Compilation"))
data <- data %>% mutate(Simulation = str_detect(data$genre, "Simulation"))
data <- data %>% mutate(Sports = str_detect(data$genre, "Sports"))
data <- data %>% mutate(Strategie = str_detect(data$genre, "Strategy"))
data <- data %>% mutate(Role_play = str_detect(data$genre, "RPG"))
data <- data %>% mutate(Edition_special = str_detect(data$genre, "Special"))
data <- data %>% mutate(DLC = str_detect(data$genre, "DLC"))

data<-data[,-6]

##Desc

data<-rename.variable(data, "desc", 'Description')
data$Description<-gsub(".*(blurbs)","",data$Description)


## Setting

data<-rename.variable(data, "setting", "Epoque")

## Perspective

data$perspective[is.na(data$perspective)] = "pas_d'info"

data <- data %>% mutate(No_info = str_detect(data$perspective, "pas_d'info"))
data <- data %>% mutate(Troisieme_personne = str_detect(data$perspective, "3rd-person"))
data <- data %>% mutate(Premiere_personne = str_detect(data$perspective, "1st-person"))
data <- data %>% mutate(Text = str_detect(data$perspective, "Text-based"))
data <- data %>% mutate(Derriere = str_detect(data$perspective, "Behind"))
data <- data %>% mutate(Diagonal = str_detect(data$perspective, "Diagonal"))
data <- data %>% mutate(Cote = str_detect(data$perspective, "Side"))
data <- data %>% mutate(Haut = str_detect(data$perspective, "Top-down"))
data <- data %>% mutate(Audio = str_detect(data$perspective, "Audio"))

data<-data[,-6]

cols <- sapply(data, is.logical)
data[,cols] <- lapply(data[,cols], as.numeric)

saveRDS(data, "data_jeux.rds")

data <- data[order(data$Annee, decreasing=TRUE ),] 

###############################################################
###############################################################

data<-readRDS("data_jeux.rds")

library(ggplot2)
data[data$Puzzle==1,] %>% ggplot(aes(x=Puzzle, y=..count..)) + geom_bar()


