
library(rvest)
library(tidyverse)
library(janitor)

# ---- ano de 2018
# está em formato diferente do ano de 2019
# dividiram em duas páginas os 1000 classificados

url1 <- "http://www.shanghairanking.com/ARWU2018.html"

webpage1 <- read_html(url1)

table_data1 <- webpage1 %>% 
  html_nodes(xpath = "//*[@id='UniversityRanking']") %>% 
  map_df(html_table, fill = TRUE) %>% 
  clean_names()

# veio tudo, porém os nomes das variáveis estão ruins

names(table_data1) <- c("world_rank", "institution", "location", "national_rank", "total_score", 
                       "alumni", "award", "hici", "n_s", "pub", "pcp")


url2 <- "http://www.shanghairanking.com/ARWU2018Candidates.html"

webpage2 <- read_html(url2)

table_data2 <- webpage2 %>% 
  html_nodes(xpath = "//*[@id='UniversityRanking']") %>% 
  map_df(html_table, fill = TRUE) %>% 
  clean_names()

# a 2a lista não tem duas variáveis
# national rank & total score - ambos calculáveis

names(table_data2) <- c("world_rank", "institution", "location", "alumni", "award", "hici", "n_s", "pub", "pcp")

# junta as duas bases

table_data <- bind_rows(table_data1, table_data2)

# a localização está como imagem
# avaliando o link para a imagem, é possível extrair dele o país (image/flag/USA.png)
# primeiros 500s

country1 <- webpage1 %>% 
  html_nodes("#UniversityRanking") %>% 
  html_nodes("td") %>% 
  html_nodes("a") %>% 
  html_nodes("img") %>% 
  # travei aqui. referência: https://stackoverflow.com/questions/34511885/convert-xml-nodeset-to-data-frame
  map(xml_attrs) %>% 
  map_df(~as.list(.))

# com essa lista de links, vou extrair apenas o país
# https://stackoverflow.com/questions/952275/regex-group-capture-in-r-with-multiple-capture-groups

siglas1 <- country1 %>% 
  map_df(~utils::strcapture("\\/.+\\/([a-zA-Z]+)", 
                            x = ., 
                            proto = list(location = character())))

siglas1 %>% count(location, sort = TRUE) %>% head()

# últimos 500s

country2 <- webpage2 %>% 
  html_nodes("#UniversityRanking") %>% 
  html_nodes("td") %>% 
  # a estrutura dessa página é diferente
  # html_nodes("a") %>% 
  html_nodes("img") %>% 
  # travei aqui. referência: https://stackoverflow.com/questions/34511885/convert-xml-nodeset-to-data-frame
  map(xml_attrs) %>% 
  map_df(~as.list(.)) %>% 
  select(src)

# com essa lista de links, vou extrair apenas o país
# https://stackoverflow.com/questions/952275/regex-group-capture-in-r-with-multiple-capture-groups

siglas2 <- country2 %>% 
  map_df(~utils::strcapture("\\/.+\\/([a-zA-Z]+)", 
                            x = ., 
                            proto = list(location = character())))

siglas2 %>% count(location, sort = TRUE) %>% head()

# junta as duas bases de siglas

siglas <- bind_rows(siglas1, siglas2)

# junta com a base original

arwu_2018 <- table_data %>% 
  select(-location) %>% 
  bind_cols(siglas) %>% 
  mutate(year = 2018)

# parece tudo certo
# falta arrumar a variável de national rank e de total score

saveRDS(arwu_2018, "arwu_2018.RDS")
