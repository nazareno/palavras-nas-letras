library(tidyverse)
library(rvest)

get_letra = function(url_letra){
    letra_html = read_html(url_letra) %>% 
        html_node("#lyr_original") 
    
    letra = gsub("<br/>", " ", letra_html %>% as.character()) %>% 
        read_html() %>% 
        html_text()
    return(letra)    
}

arquivos = list.files("input_songs/", "song")
for(i in seq_along(arquivos)){
    input_file = paste0("input_songs/", arquivos[i])
    output_file = paste0("letras/", arquivos[i])
    if(file.exists(output_file)){
        message(output_file, " já existe")
    } else {
        message("processando ", input_file)
        urls = read_csv(input_file, col_names = "url_musica")
        letras = urls %>% 
            group_by(url_musica) %>% 
            do(tryCatch(tibble(letra = get_letra(.$url_musica)), 
                        error = function(e) data.frame(NA)))
        letras %>% 
            select(-3) %>% 
            write_csv(output_file)
    }
}

