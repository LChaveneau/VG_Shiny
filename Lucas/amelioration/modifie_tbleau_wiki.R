library(tidyverse)

data <- read.csv("vgsales2.csv")

modifie_tableau <- 
  function(tableau){tableau %>% 
      str_replace("<div class=\"infobox-caption\".*?</div>", "") %>% 
      str_replace_all("<td class=\"infobox-data.*?>", "<div style=\"font-size: 90%; padding-left: 1em; padding-bottom: 0.25em;\">") %>% 
      str_replace_all("<th class=\"infobox-label.*?>", "<div style=\"font-size: 100%; font-weight: bold;\">") %>% 
      str_replace_all("</td>", "</div>") %>% 
      str_replace_all("</th>", "</div>") %>% 
      str_replace_all("<td", "<div") %>% 
      str_replace_all("<th", "<div") %>% 
      str_replace_all("tr>", "div>") %>% 
      str_replace_all('<a.*?>', "") %>% 
      str_replace_all("</a>", "") %>% 
      str_remove("<div><div class=\"infobox-above..*?</div></div>") %>% 
      str_replace("<div style=\"font-size: 100%; font-weight: bold;\">", "</div></td><td width=\"65%\"><div id=\"coreGameRelease\"><div style=\"font-size: 100%; font-weight: bold;\">") %>% 
      str_replace("<table.*?<tbody>", "<table class=\"pct100\"><tbody><tr valign=\"top\"><td width=\"35%\"><div id=\"coreGameCover\">") %>% 
      str_remove_all("<div><div style=\"font-size: 100%; font-weight: bold;\">(Artist|Programmer|Composer|Release|Designer|Writer).*?</div>[\\D\\d]*?</div></div></div>") %>%
      str_remove_all("<div><div style=\"font-size: 100%; font-weight: bold;\">(Artist|Programmer|Composer|Release|Designer|Writer).*?</div>[\\D\\d]*?</div></div>") %>% 
      str_replace("</tbody></table>", "</div></td></tr></tbody></table>")
  }

modifie_tableau <- Vectorize(modifie_tableau)

data <- data %>% mutate(
  tableau = paste0("<h1>",Name,"</h1><br></br>", modifie_tableau(tableau))
)

write.csv(data,"C:\\Users\\Lucas\\Documents\\M2\\S2\\Big data\\Lucas\\data\\vgsales3.csv", row.names = FALSE)