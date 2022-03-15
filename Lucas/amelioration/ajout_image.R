library(rjson)
library(jsonlite)
library(dplyr)

## Import de data_jeux.rds manuellement

image<-read_json("data_image.json",simplifyVector = TRUE)

image2 <- image %>% select(titre, image)

image2 <- image2[!duplicated(image2$titre),]


data <- data_jeux %>% 
  left_join(image2, c("Name" = "titre"))


saveRDS(data, file = "C:\\Users\\Lucas\\Documents\\M2\\S2\\Big data\\Lucas\\data\\data_jeux_image.rds")


data <- data %>% mutate(image = image %>%
                  str_replace('src=\"/', "src=\"https://www.mobygames.com/"))

data[1, 30]
