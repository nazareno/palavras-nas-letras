library(tidyverse)
library(tidytext)

letras_raw = tibble(arquivo = list.files("letras/", full.names = T)) %>% 
    rowwise() %>% 
    do(read_csv(.$arquivo, col_types = "cc", progress = F)) %>% 
    mutate(letra = gsub("\n", " ", letra), 
           artista = strsplit(url_musica, split = "/")[[1]][4])

estilos = read_csv("artista_musica_estilo.txt", 
                   col_names = c("artista", "musica", "estilo"), 
                   col_types = "ccc") %>% 
    select(-musica)

as_letras = letras_raw %>% 
    ungroup() %>% 
    left_join(estilos, by = "artista") %>% 
    filter(!is.na(letra)) 

as_letras %>% 
    group_by(estilo) %>% 
    count() %>% 
    ggplot(aes(x = reorder(estilo, n), y = n)) + 
    geom_col() + 
    coord_flip()

# Filtramos fora a música gospel. 
palavras_raw = as_letras %>%
    filter(estilo != "gospelreligioso") %>%
    select(url_musica, estilo, letra) %>%
    unnest_tokens(word, letra) 
palavras = palavras_raw %>% 
    filter(nchar(word) > 2, 
           !grepl("[0-9+]", word))
rm(palavras_raw)

# por palavra
word_averages = palavras %>%
    group_by(url_musica) %>%
    mutate(word_position = row_number() / n()) %>%
    group_by(url_musica, word) %>% 
    summarise(word_position = median(word_position)) %>% 
    ungroup() %>% 
    group_by(word) %>%
    summarize(median_position = median(word_position),
              number = n())

write_csv(word_averages, "word_stats-all.csv")

# Por raiz (= palavra + stemming)
root_averages = palavras %>% 
    mutate(root = RTextTools::wordStem(word, "portuguese")) %>% 
    group_by(url_musica) %>%
    mutate(word_position = row_number() / n()) %>%
    group_by(url_musica, root) %>% 
    summarise(word_position = median(word_position), 
              forms = paste(unique(word), collapse = ", ")) %>% 
    ungroup() %>% 
    group_by(root) %>%
    summarize(median_position = median(word_position),
              number = n(), 
              forms = paste(unique(forms), collapse = ", "))

write_csv(root_averages, "root_stats-all.csv")
