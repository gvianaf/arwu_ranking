
# gera o arquivo do ARWU

options(encoding = "UTF-8")

source("01A_gera_dados_2017.R")
source("01B_gera_dados_2018.R")
source("01C_gera_dados_2019.R")


arwu <- map_df(list(arwu_2017, arwu_2018, arwu_2019), bind_rows)

# verifica os dados

glimpse(arwu)

# recalcular o total score
# http://www.shanghairanking.com/ARWU-Methodology-2019.html
# alumni 10% award 20% hici 20% n&s 20% pub 20% pcp 10%

# na verdade tem um porém aqui:
# For institutions specialized in humanities and social sciences such as London School of Economics, 
# N&S is not considered, and the weight of N&S is relocated to other indicators

sem_ns <- arwu %>% filter(is.na(n_s))

# apenas 23 universidades têm n&s como NA
# não há indícios de como essa distribuição dos 20% de N&S foi feita
# vou botar 4% em cada um dos outros indicadores

arwu <- arwu %>% 
  mutate(total_score_calc = ifelse(is.na(n_s),
                                   0.14*alumni + 0.24*award + 0.24*hici + 0.24*pub + 0.14*pcp,
                                   0.1*alumni + 0.2*award + 0.2*hici + 0.2*n_s + 0.2*pub + 0.1*pcp))

# o ranking só disponibiliza a nota das universidade que tiveram 25+ pontos
# comparando as medidas de posição, acho que é uma estimativa boa a que temos
summary(arwu$total_score)
summary(arwu[arwu$total_score_calc > 25,]$total_score_calc)

# salva o arwu global

saveRDS(arwu, "arwu.RDS")

# pega os dados BR

arwu_br <- arwu %>% 
  filter(location == "Brazil")

# cria as siglas, vou precisar
# prefiro preencher na mão mesmo

siglas <- arwu_br %>% 
  distinct(institution)

rio::export(siglas, "siglas.xlsx")

# junta com a base BR

arwu_br <- arwu_br %>% 
  left_join(rio::import("siglas_preenchido.xlsx"))

# arruma o rank nacional

arwu_br <- arwu_br %>% 
  group_by(year) %>% 
  # como o total_score tem valores iguais, adicionei critérios, com base no peso, para classificar as universidades
  arrange(desc(total_score_calc), desc(award), desc(hici), desc(n_s), desc(pub)) %>% 
  mutate(national_rank_calc = row_number())

# cria o rank federal

arwu_br <- arwu_br %>% 
  left_join(arwu_br %>% 
              filter(str_detect(institution, "Federal") | sigla == "UnB") %>% 
              # não agrupei pois já está agrupado por ano
              mutate(federal_rank = row_number()) %>% 
              select(sigla, year, federal_rank)) %>% 
  ungroup()
  
# salva o arquivo arwu BR

saveRDS(arwu_br, "arwu_br.RDS")



