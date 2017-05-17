library(tidyverse)
library(rvest)

urls = read_csv("https://raw.githubusercontent.com/joaoarthurbm/ciframe/master/data/top/top_artistas.txt_1.songs", 
                col_names = "url_musica")

letras = urls %>% 
    group_by(url_musica) %>% 
    do(tryCatch(read_html(.$url_musica) %>% 
                    html_node("pre") %>% 
                    html_text() %>% 
                    as.tibble(), 
                error = function(e) data.frame(NA)))

letras %>% 
    select(-3) %>%
    write_csv("letras-topartistas-1.csv")
