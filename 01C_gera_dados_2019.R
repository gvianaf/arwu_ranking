
library(rvest)
library(tidyverse)
library(janitor)

# ---- ano de 2019

url <- "http://www.shanghairanking.com/ARWU2019.html"

webpage <- read_html(url)

table_data <- webpage %>% 
  html_nodes(xpath = "//*[@id='UniversityRanking']") %>% 
  map_df(html_table, fill = TRUE) %>% 
  clean_names()

# veio tudo, porém os nomes das variáveis estão ruins

names(table_data) <- c("world_rank", "institution", "location", "national_rank", "total_score", 
                       "alumni", "award", "hici", "n_s", "pub", "pcp")

# a localização está como imagem
# avaliando o link para a imagem, é possível extrair dele o país (image/flag/USA.png)

country <- webpage %>% 
  html_nodes("#UniversityRanking") %>% 
  html_nodes("td") %>% 
  html_nodes("a") %>% 
  html_nodes("img") %>% 
  # travei aqui. referência: https://stackoverflow.com/questions/34511885/convert-xml-nodeset-to-data-frame
  map(xml_attrs) %>% 
  map_df(~as.list(.))

# com essa lista de links, vou extrair apenas o país
# https://stackoverflow.com/questions/952275/regex-group-capture-in-r-with-multiple-capture-groups

siglas <- country %>% 
  map_df(~utils::strcapture("\\/.+\\/([a-zA-Z]+)", 
                            x = ., 
                            proto = list(location = character())))

siglas %>% count(location, sort = TRUE) %>% head()

# junta com a base original

arwu_2019 <- table_data %>% 
  select(-location) %>% 
  bind_cols(siglas) %>% 
  mutate(year = 2019)

# parece tudo certo
# só falta arrumar a variável de national rank, que está como como chr

saveRDS(arwu_2019, "arwu_2019.RDS")


