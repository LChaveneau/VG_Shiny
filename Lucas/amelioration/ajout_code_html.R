## importer data_jeux2 avec 130000 observations as data

data2 <- readRDS("data_jeux_image.rds")

data.merge <- data %>% select("titre", "published":"misc")
data.merge <- data.merge[!duplicated(data.merge$titre),]
data.test <- data2%>% 
  left_join(data.merge, c("Name" = "titre"))

creer_html<-function(Name, image, published, developed, released, platform.y, genre, perspective, gameplay, setting, narrative, misc, edition, visual){
  valeur = paste0("<h1>",Name,"</h1><br></br>","<table class=\"pct100\"><tbody><tr valign=\"top\"><td width=\"1%\"><div id=\"coreGameCover\">")
  
  if(is.na(image) == F){
    valeur = paste(
      valeur, image)
  }else{
    valeur = paste(valeur,
                   "<img alt=\"Alone in the Dark DOS Front Cover\" border=\"0\" src=\"https://lh6.googleusercontent.com/proxy/ArqMUjwLTH8fwuNnxIWgh6VJ96iZRNrC2cCd9A-0Sz_yOqz2zo0mstHMh1etS0D8Ap6kz6XQT4C5yY1YjGaDkX5zB-G_CFhFlb4y7_ePBIlknO351AHIDLVG0VsQw_KQti3uuOkqHbE=w1200-h630-p-k-no-nu\" height=\"120\" width=\"120\">")
  }
  
  valeur = paste0(valeur, "</td><td width=\"48%\"><div id=\"coreGameRelease\">")
  
  if(is.na(published) == FALSE){
    valeur = paste(
      valeur, "<div style=\"font-size: 100%; font-weight: bold;\">Published by</div><div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">",published,"</div>"
    )
  }
  if(is.na(developed) == FALSE){
    valeur = paste(valeur,
                   "<div style=\"font-size: 100%; font-weight: bold;\">Developed by</div><div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">",developed,"</div>")
  }
  
  if(is.na(released) == FALSE){
    valeur = paste(valeur,
                   "<div style=\"font-size: 100%; font-weight: bold;\">Released</div><div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">",released,"</div>")
    
  }
  
  if(is.na(platform.y) == FALSE){
    valeur = paste(valeur, 
                   "<div style=\"font-size: 100%; font-weight: bold;\">Platforms</div><div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">",platform.y,"</div>")
  }
  
  valeur = paste(
    valeur, 
    "</div></td><td width=\"48%\"><div id=\"coreGameGenre\"><div>")
  
  
  if(is.na(genre) == FALSE){
    valeur = paste(valeur,
                   "<div style=\"font-size: 100%; font-weight: bold;\">Genre</div><div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">",genre,"</div>")
  }
  
  if(is.na(gameplay) == FALSE){
    valeur = paste(valeur,
                   "<div style=\"font-size: 100%; font-weight: bold;\">Gameplay</div><div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">",gameplay,"</div>")
  }
  
  if(is.na(perspective) == FALSE){
    valeur = paste(valeur,
                   "<div style=\"font-size: 100%; font-weight: bold;\">Perspective</div><div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">",perspective,"</div>")
  }
  
  if(is.na(setting) == FALSE){
    valeur = paste(valeur,"<div style=\"font-size: 100%; font-weight: bold;\">Setting</div><div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">",setting,"</div>")
  }
  
  if(is.na(narrative) == FALSE){
    valeur = paste(valeur,"<div style=\"font-size: 100%; font-weight: bold;\">Narrative</div><div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">",narrative,"</div>")
  }
  
  if(is.na(edition) == FALSE){
    valeur = paste(valeur,"<div style=\"font-size: 100%; font-weight: bold;\">Edition</div><div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">",edition,"</div>")
  }
  
  if(is.na(visual) == FALSE){
    valeur = paste(valeur,"<div style=\"font-size: 100%; font-weight: bold;\">Visual</div>
              <div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">",visual,"</div>")
  }
  
  if(is.na(misc) == FALSE){
    valeur = paste(valeur,"<div style=\"font-size: 100%; font-weight: bold;\">Misc</div><div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">",misc,"</div>")
  }
  
  valeur = paste(valeur,"</div></div></td></tr></tbody></table>  ")
  
  return(valeur)
}

creer_html <- Vectorize(creer_html)

data_test <- data.test  %>% 
  mutate(code_html = creer_html(Name = Name, image = image, published = published, developed = developed, released = released, platform.y = platform.y, genre = genre, perspective = perspective, gameplay = gameplay, setting = setting, narrative = narrative, misc = misc, edition = edition, visual = visual))

data_test <- data_test %>% 
  select(Name, code_html)

data_final <-  data2%>% 
  left_join(data_test, c("Name" = "Name"))

saveRDS(data_final, file = "C:\\Users\\Lucas\\Documents\\M2\\S2\\Big data\\Lucas\\data\\data_jeux_image2.rds")