######
# PACOTES
###### 

pacotes <- c("XML","RSelenium","stringr","rvest", 'xml2', 'tidyverse', 
             'ggmap', "sf","tmap","rgdal","rgeos","adehabitatHR","knitr",
             "fs","httr","leaflet",'purrr',
             "kableExtra") #,"textreadr"


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#####
# 0. FUNÇÔES
#####

scrap_ape <- function(df){
  
  html <- remDr$getPageSource()[[1]]
  pg_geral <- read_html(html)
  
  for (b in 1:21){
    #dados gerais da pagina
    geral_data <- pg_geral %>% html_nodes(paste0(".MuiGrid-grid-xl-3:nth-child(",b,")")) %>% html_text()
    
    valor <- str_extract(geral_data, "[1-9]\\d{0,2}(?:\\.\\d{3})*")
    bairro <- str_extract(geral_data, "(?<=, )\\D+(?=\\d)") 
    #area <- geral_data %>% str_extract("(?<=\\D)\\d+ m(?=\\D)") %>% str_extract("\\d+")
    #quarto <- geral_data %>% str_extract("(?<=\\D)\\d+ quart(?=\\D)") %>% str_extract("\\d+") 
    #vaga <- geral_data %>% str_extract("(?<=\\D)\\d+ vaga(?=\\D)") %>% str_extract("\\d+")
    
    #url
    #url <- pg_geral %>% html_nodes(".MuiCardActionArea-root") %>% rvest::html_attr('href')
    url <- pg_geral %>% html_nodes(paste0(".MuiGrid-grid-xl-3:nth-child(",b,")")) %>% html_nodes(".MuiCardActionArea-root") %>% rvest::html_attr('href')
    
    #loft_df<-data.frame(endereco = url, preco = valor, bairro = bairro)
    
    brk <- c(url, valor, bairro)
    
    loft_df <- loft_df %>% rbind(brk)
  }
  
  names(loft_df) <- c('endereco', 'preco', 'bairro')
  
  df <- loft_df %>% drop_na(endereco)
  return (df)
}

# SETA O NAVEGADOR
rD <- rsDriver(port = 4819L,
               ##Define a versão do Chrome que o Webdriver deve utilizar     
               chromever = '96.0.4664.45',
               ##Remove as informações do console
               verbose = F)

# CRIA O NAVEGADOR
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4819L,
  browserName = "chrome",
)

#####
# 1. WEBSCRAPPING- PÁGINA PRINCIPAL
#####

#Abre o servidor
remDr$open()

#url <- 'https://loft.com.br/venda/apartamentos/sao-paulo_sp'
#webpage <- read_html(url)

df <- data.frame()
df_t <- data.frame()
loft_df <- data.frame()
t_geral <- 0
k<-113

for (s in 1:k){
  total_pagina <- 0
  l_pag<-0
  pagina <-0
  max_it <-0
  while (pagina < 3){
    # ACESSA AO WEBPAGE
    remDr$navigate("https://loft.com.br/venda/apartamentos/sao-paulo_sp")
    
    # Clicar em bairro
    bt_bairro <- remDr$findElement(using = 'class', value = 'jss131')
    bt_bairro2 <-bt_bairro$findChildElements(using = 'class', value = 'jss132')
    bt_bairro2[[2]]$clickElement()
    #bt_bairro <- remDr$findElement(using = 'xpath', value = '//*[@id="__next"]/div/div/div/div[2]/div[2]/div/div/div[2]/div[2]/div/span[1]')
    #bt_bairro2$clickElement()
  
    ## Limpar seleção
    #bt_bairro <- remDr$findElement(using = 'xpath', value = '/html/body/div[4]/div[3]/div/div[2]/button[1]/span')
    #bt_bairro$clickElement()
    bt_bairro <- remDr$findElement(using = 'class', value = 'jss173')
    bt_bairro2 <-bt_bairro$findChildElement(using = 'class', value = 'MuiButton-label')
    bt_bairro2$clickElement()
      
    # Selecionar o bairro    
    #sel_bairro <- paste0('/html/body/div[4]/div[3]/div/div[1]/div[2]/label[', s, ']/span[1]/span[1]/input')
    #bt_bairro <- remDr$findElement(using = 'xpath', value = sel_bairro)
    #bt_bairro$clickElement()
    bt_bairro <- remDr$findElements(using = 'class', value = 'MuiFormControlLabel-root')
    bt_bairro[[s]]$clickElement()
  
    # Selecionar pesquisar
    bt_pesq <- remDr$findElements(using = 'class', value = 'MuiButton-root')
    bt_pesq[[6]]$clickElement()
  
    url_atual <- remDr$getCurrentUrl() %>% as.character()
  
    html <- remDr$getPageSource()[[1]]
    total_ape <- read_html(html)
  
    qtd_total <- total_ape %>% html_elements("section") %>% html_text() %>% str_extract("\\d+")
    total_pagina <- qtd_total[1] %>% as.numeric()
    
    pagina <- total_pagina
    
    if (max_it < 8){
      max_it <- max_it + 1
    } else if (max_it >= 8){
      pagina <- 4
    }
      # Itaim Paulista #41
      # Itaquera #42
      # Lajeado #57
      # Parelheiros #67
      # Vila Curuça #98
  }
  
  t <- ceiling(total_pagina/18)
  
  t_geral <- t_geral + total_pagina
  
  for (i in 1:t){
    remDr$navigate(paste0(url_atual, "?offset=", l_pag,"&amp;limit=18"))
    df <- scrap_ape(df)
    df_t <- rbind(df_t, df)
    l_pag <- l_pag + 18
    print(paste0('iter ', s, ' T Ape: ', total_pagina, ' DF: ', dim(df_t)[1], ' T ESP: ', t_geral,' ', i, ' / ', t))

    if ((total_pagina) == dim(df_t)[1]){
      i<- t
    }
  }
}

df_t %>%  write.csv(.,file = "loft220109.csv", row.names = FALSE)

#######
# 2. WEBSCRAPPING - DADOS DO APARTAMENTO
#######


df_ape <- read.csv("loft220109.csv")

df_ape$bairro <- df_ape$bairro %>% iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% tolower()

#Abre o servidor
remDr$open()

#a <- data.frame()
a <- read.csv("loft220113_comp.csv")

for (i in 20022:dim(df_ape)[1]){
  remDr$navigate(paste0('https://loft.com.br',df_ape$endereco[i]))
  
  #status de avanço
  st_pesq <- paste(i,'/', dim(df_ape)[1], round(i/dim(df_ape)[1], 5)*100, '%')
  print(st_pesq)
  
  html <- remDr$getPageSource()[[1]]
  pg_ape <- read_html(html)
  
  ex_pg <- pg_ape %>% html_element("h3") %>% html_text()
  
  notfound <-0
  if (!is.na(ex_pg)){
    notfound <-1
  } else if (is.na(ex_pg)){
    notfound <-0
  }
  
  if (notfound == 0){  
    # Endereço completo
    end_ape <- pg_ape %>% html_element("h1") %>% html_text() %>% 
      iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% tolower()
    end_ape
    
    # Dados gerais do apartamento
    geral_ape <- pg_ape %>% html_element("h2") %>% html_text()
    geral_ape
    
    #nome condominio
    if (is.na(geral_ape %>% str_extract("(?<=\\d)\\D+(?=\\d)"))){
      nm_cond_ape <- geral_ape %>% str_extract("(?<=)\\D+(?=\\d)") %>% 
      iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% tolower() %>%
        str_extract("(?<=)\\D+(?=\\S)") %>% str_extract("(?<=)\\D+(?=\\s)")
      nm_cond_ape <- (if (is.na(nm_cond_ape)) 'ND' else nm_cond_ape)
    } else {
      nm_cond_ape <- geral_ape %>% str_extract("(?<=\\d)\\D+(?=\\d)") %>% 
        iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% tolower() %>%
        str_extract("(?<=\\S)\\D+(?=\\S)") %>% str_extract("(?<=\\s)\\D+(?=\\s)")
      nm_cond_ape <- (if (is.na(nm_cond_ape)) 'ND' else nm_cond_ape)
    }
    nm_cond_ape
    
    if (nm_cond_ape != "ND"){
      #url condominio
      cond_url_ape <- pg_ape %>% html_elements("h2, a") %>% 
        html_elements(".MuiTypography-colorPrimary") %>% 
        rvest::html_attr('href')
      cond_url_ape
      if (identical(cond_url_ape, character(0))){
      cond_url_ape <- 'ND'
      }
    }else{
      cond_url_ape <- 'ND'
    }
    
    #andar
    andar_ape <- pg_ape %>% html_element('.MuiGrid-grid-md-3:nth-child(1)') %>% 
      html_text() %>% str_extract("\\d+") %>% as.numeric()
    andar_ape
    
    #area
    area_ape <- pg_ape %>% html_element('.MuiGrid-grid-md-3:nth-child(2)') %>% 
      html_text() %>% str_extract("(?<=)\\d+(?=\\W)") %>% as.numeric()
    area_ape
    
    #valor area
    vl_area_ape <- pg_ape %>% html_element('.MuiGrid-grid-md-3:nth-child(2)') %>% 
      html_text() %>% str_extract_all("(?<=\\W)[1-9]\\d{0,2}(?:\\.\\d{3})*(?=\\s)" )%>% as.numeric()
    
      vl_area_ape <- (if (vl_area_ape > 10) vl_area_ape else vl_area_ape*1000)
    
    vl_area_ape
    
    #dormitorio
    dorm_ape <- pg_ape %>% html_element('.MuiGrid-grid-md-3:nth-child(3)') %>% 
      html_text() %>% str_extract("(?<=)\\d+(?=\\D)") %>% as.numeric()
    dorm_ape
    
    #suite
    suite_ape <- pg_ape %>% html_element('.MuiGrid-grid-md-3:nth-child(3)') %>% 
      html_text() %>% str_extract("(?<=\\D)\\d+(?=\\D)") %>% as.numeric()
    suite_ape  <- (if (is.na(suite_ape)) 0 else 1)
    suite_ape
    
    #demais atributos
    vb4_ape <- pg_ape %>% html_element('.MuiGrid-grid-md-3:nth-child(4)') %>% html_text()
    vb5_ape <- pg_ape %>% html_element('.MuiGrid-grid-md-3:nth-child(5)') %>% html_text()
    vb6_ape <- pg_ape %>% html_element('.MuiGrid-grid-md-3:nth-child(6)') %>% html_text()
    vb7_ape <- pg_ape %>% html_element('.MuiGrid-grid-md-3:nth-child(7)') %>% html_text()
    vb8_ape <- pg_ape %>% html_element('.MuiGrid-grid-md-3:nth-child(8)') %>% html_text()
    vb9_ape <- pg_ape %>% html_element('.MuiGrid-grid-md-3:nth-child(9)') %>% html_text()
    vbg_ape <- paste0(vb4_ape, " ", vb5_ape, " ", vb6_ape, " ", vb7_ape, " ", vb8_ape, " ", vb9_ape)
    vbg_ape
    vaga_ape <- vbg_ape %>% str_extract("(?<=)\\d+ vaga(?=\\D)") %>% str_extract("\\d+")
    vaga_ape <- (if (is.na(vaga_ape)) 0 else vaga_ape)
    vaga_ape
    banheiro_ape <- vbg_ape %>% str_extract("(?<=)\\d+ Banhe(?=\\D)") %>% str_extract("\\d+")
    banheiro_ape
    distmetro_ape <- vbg_ape %>% str_extract("(?<=)\\d+m(?=\\D)") %>% str_extract("\\d+")
    distmetro_ape <- (if (is.na(distmetro_ape)) 0 else distmetro_ape)
    distmetro_ape
    mobilia_ape <- vbg_ape %>% str_extract("(?<=)mob(?=\\D)")
    mobilia_ape <- (if (is.na(mobilia_ape)) 1 else 0)
    mobilia_ape
    portaria_ape <- vbg_ape %>% str_extract("(?<=)Port(?=\\D)")
    portaria_ape <- (if (is.na(portaria_ape)) 0 else 1)
    portaria_ape
    
    ape <- c(df_ape$endereco[i], end_ape, andar_ape, area_ape, vl_area_ape, 
             dorm_ape, suite_ape, banheiro_ape, vaga_ape, mobilia_ape, 
             portaria_ape, distmetro_ape, nm_cond_ape, cond_url_ape) #end_comp, area_cm_s, area_cm_n, desc_cond)
    
    a <- a %>% rbind(ape)
  }

}

names(a) <- c('url-ape','end_comp', 'pav_ape', 'area_ape','vl_m2_ape',
              'dorm_ape','suite_ape', 'banheiro_ape', 'vaga_ape', 'mobiliado_ape',
              'portaria_ape', 'dist_metro', 'cond_nome', 'cond_url') #'end_condm','tem_ac', 'ntem_ac', 'descricao_cond')

a %>%  write.csv(.,file = "loft220114_comp.csv", row.names = FALSE)

#a <- a[-c(11408:11440),]

#####
# 2. WEBSCRAPPING- DADOS ESPECIFICOS DO CONDOMÌNIO - Output - loft220202_condominios.csv
#####

a <- read.csv("loft220114_comp.csv")

length(unique(a$cond_url))

# criando o DF para capturar os dados do condomínio
cond_df <- a %>% dplyr::select(cond_nome, cond_url)
cond_df <- cond_df %>% distinct(cond_url, .keep_all= TRUE) %>% filter(!cond_url=='ND')

#Abre o servidor
remDr$open()

b <- data.frame()

for (i in 6804:dim(cond_df)[1]){
 remDr$navigate(paste0('https://loft.com.br',cond_df$cond_url[i]))
 
 #status de avanço
 st_pesq <- paste(i,'/', dim(cond_df)[1], round(i/dim(cond_df)[1], 5)*100, '%')
 print(st_pesq)
  
 html <- remDr$getPageSource()[[1]]
 pg_cond <- read_html(html)
 
 # descrição condominio
 desc_cond <- pg_cond %>% html_elements("p") %>%
   html_text() %>% 
   iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% tolower()
 end_comp <- desc_cond[1]
 desc_cond <- desc_cond[3]
 end_comp
 desc_cond
 
 # ano construcao / torre / elevador
 yoc_cond <- pg_cond %>% html_elements(".MuiGrid-grid-xl-6") %>%
   html_text() %>% iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% tolower()
 # ano construcao
 year_cond <- yoc_cond[2] %>% str_extract("(?<=\\D)\\d+(?=\\D)") %>% as.numeric()
 year_cond
 # torre
 torre_cond <- yoc_cond[2] %>% str_extract("(?<=)to\\w+duas|to\\w+uma|to\\w+tres|to\\w+quatro|to\\w+cinco|to\\w+seis|to\\w+sete(?=\\D)")
 if (is.na(torre_cond)){
   torre_cond <- yoc_cond[2] %>% str_extract("(?<=)to\\w+(?=\\D)") %>% str_extract("(?<=\\D)\\d+(?=\\D)") %>% as.numeric()
 } else {
   torre_cond <- if(torre_cond=='torresuma'){1
     }else if(torre_cond=='torresduas'){2
     }else if(torre_cond=='torrestres'){3
     }else if(torre_cond=='torresquatro'){4
     }else if(torre_cond=='torrescinco'){5
     }else if(torre_cond=='torresseis'){6
     }else if(torre_cond=='torressete'){7
     }else{0}
 }
 torre_cond
 # elevador
 elev_cond <- yoc_cond[2] %>% tolower() %>% str_extract("(?<=)elevadorsim|elevadornao|em 1|em 2|em 3|em 4|em 5|em 6|em 7|em 8|em 9(?=\\D)")
 elev_cond <- if(elev_cond=='elevadorsim' | elev_cond=='em 1'| elev_cond=='em 2'| elev_cond=='em 3'| elev_cond=='em 4'| elev_cond=='em 5'| elev_cond=='em 6'| elev_cond=='em 7'| elev_cond=='em 8'| elev_cond=='em 9'){1
   }else{0}
 elev_cond
 
 #area comum
 #pg_cond %>% html_elements(".MuiGrid-grid-xs-12.MuiGrid-grid-md-8.MuiGrid-grid-xl-6") %>%html_text()
 
 #verifica qual é codigo em vigor para pegar as areas comuns do prédio
 codigo_areacm <- pg_cond %>% 
   html_elements(".MuiGrid-root.MuiGrid-container.MuiGrid-grid-xs-12.MuiGrid-grid-md-8.MuiGrid-grid-xl-6") %>% 
   html_elements("ul") %>%
   html_elements("li") %>%
   html_elements("span") %>%
   html_attr("class")
 
 ver_ac <- data.frame(str_split(codigo_areacm, " "))
 names(ver_ac) <- c('A','B', 'C', 'D','E','F', 'G', 'H', 'I', 'J', 'K')
 
 ver_ac <- ver_ac %>% t() %>% data.frame() %>% distinct(X5, .keep_all= FALSE)
 
 # areas comum - edificio possui
 area_cm_s <- pg_cond %>% html_elements(paste0(".", ver_ac[1, 1])) %>% html_text() %>% 
   iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% tolower() %>% toString()
 
 area_cm_s
 
 # areas comum - edificio não possui
 area_cm_n <- pg_cond %>% html_elements(paste0(".", ver_ac[2, 1])) %>% html_text() %>% 
   iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% tolower() %>% toString() #quando dá tudo cheio volta character(0)
 
 area_cm_n
 
 cond_desc <- c(cond_df$cond_url[i], end_comp, year_cond, torre_cond, elev_cond, area_cm_s, area_cm_n, desc_cond)
 
 b <- b %>% rbind(cond_desc)
 
 area_cm_s <-''
 area_cm_n <-''
 desc_cond <-''
 end_comp <-''
 
}

names(b) <- c('url_cond', 'end_condm','ano_torre', 'torres','elevador ','tem_ac', 'ntem_ac', 'descricao_cond')

b %>%  write.csv(.,file = "loft220202_condominios.csv", row.names = FALSE)

b <- read.csv("obsoleto/loft220202_condominios.csv")

# AJUSTANDO NAs
b[11,3] <- 2021
b[59,3] <- 2016
b[123,3] <- 2021
b[143,3] <- 2021
b[234,3] <- 2021
b[360,3] <- 2019
b[372,3] <- 2021
b[374,3] <- 2017
b[517,3] <- 2020
b[734,3] <- 2019
b[738,3] <- 2018
b[742,3] <- 2020
b[758,3] <- 2019
b[933,3] <- 2021
b[1048,3] <- 2016
b[1051,3] <- 2020
b[1065,3] <- 2018
b[1081,3] <- 2022
b[1157,3] <- 2018
b[1158,3] <- 2020
b[1166,3] <- 2018
b[1217,3] <- 2013
b[1435,3] <- 2021
b[1460,3] <- 2020
b[1498,3] <- 2018
b[1647,3] <- 2017
b[1736,3] <- 2019
b[1749,3] <- 2017
b[1752,3] <- 2020
b[1762,3] <- 2020
b[1901,3] <- 2019
b[1904,3] <- 2019
b[2310,3] <- 2017
b[2572,3] <- 2020
b[2891,3] <- 2016
b[2992,3] <- 2019
b[3083,3] <- 2020
b[3155,3] <- 2020
b[3167,3] <- 2019
b[3188,3] <- 2018
b[3319,3] <- 2019
b[3386,3] <- 2020
b[3459,3] <- 2021
b[3945,3] <- 2020
b[3967,3] <- 2018
b[4311,3] <- 2019
b[4446,3] <- 2018
b[4471,3] <- 2020
b[4537,3] <- 2020
b[4637,3] <- 2019
b[4639,3] <- 2019
b[4656,3] <- 2017
b[4688,3] <- 2020
b[4690,3] <- 2021
b[4694,3] <- 2020
b[4704,3] <- 2018
b[4730,3] <- 2018
b[4731,3] <- 2019
b[4732,3] <- 2020
b[4762,3] <- 2018
b[4844,3] <- 2020
b[5034,3] <- 2015
b[5068,3] <- 2017
b[5213,3] <- 2019
b[5278,3] <- 2020
b[5283,3] <- 2019
b[5303,3] <- 2018
b[5327,3] <- 2019
b[5344,3] <- 2020
b[5365,3] <- 2018
b[5368,3] <- 2020
b[5497,3] <- 2020
b[5586,3] <- 2019
b[5600,3] <- 2020
b[5701,3] <- 2020
b[5920,3] <- 2015
b[5957,3] <- 2019
b[5971,3] <- 2018
b[6084,3] <- 2018
b[6111,3] <- 2021
b[6162,3] <- 2019
b[6188,3] <- 2018
b[6195,3] <- 2018
b[6197,3] <- 2019
b[6219,3] <- 2019
b[6263,3] <- 2022
b[6361,3] <- 2021
b[6370,3] <- 2020
b[6430,3] <- 2019
b[6650,3] <- 2020
b[6668,3] <- 2019
b[6824,3] <- 2021
b[6850,3] <- 2018

b[1461,3] <- b[1461,3] %>% dplyr::na_if(1)
b[1544,3] <- 2017
b[1903,3] <- 2019
b[1931,3] <- 2019
b[3182,3] <- b[3182,3] %>% dplyr::na_if(1)
b[4808,3] <- b[4808,3] %>% dplyr::na_if(1)
b[6607,3] <- 2021
b[895,3] <- 2020
b[1072,3] <- b[1072,3] %>% dplyr::na_if(2)
b[1514,3] <- b[1514,3] %>% dplyr::na_if(2)
b[3215,3] <- 2019
b[4417,3] <- 2020
b[4795,3] <- b[4795,3] %>% dplyr::na_if(2)
b[4847,3] <- b[4847,3] %>% dplyr::na_if(2)
b[5375,3] <- b[5375,3] %>% dplyr::na_if(2)
b[5630,3] <- 2016
b[893,3] <- b[893,3] %>% dplyr::na_if(3)
b[1443,3] <- 2021
b[1557,3] <- 2011
b[5232,3] <- 2016
b[6142,3] <- 2019
b[6349,3] <- b[6349,3] %>% dplyr::na_if(3)
b[1412,3] <- b[1412,3] %>% dplyr::na_if(4)
b[1911,3] <- b[1911,3] %>% dplyr::na_if(4)
b[2059,3] <- b[2059,3] %>% dplyr::na_if(4)
b[6284,3] <- 2018
b[1556,3] <- b[1556,3] %>% dplyr::na_if(12)
b[1937,3] <- b[1937,3] %>% dplyr::na_if(12)
b[1446,3] <- b[1937,3] %>% dplyr::na_if(57)

b %>%  write.csv(.,file = "loft220211_condominios.csv", row.names = FALSE)

dplyr::na_if(1)

#####
# 2.1 AMPLIANDO AREA COMUM
#####

b <- read.csv("loft220211_condominios.csv")

#Verificando a quantidade de tipos de area comum disponível
#acomum <- b %>% distinct(tem_ac, .keep_all= FALSE) %>% iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% tolower()
#acomum <- acomum %>% str_replace_all("[^[:alnum:]]", " ")
#acomum <- acomum %>% str_split("  ")
#acomum <- unique(acomum[[1]])
#acomum <- acomum %>% str_squish()
#acomum <- unique(acomum)
##removendo os itens errados ("c", "")
#acomum <- acomum[-c(1, 10)]

#criando dt para explodir a area comum em colunas individuais
acomum <- c( "salao de festas", "espaco gourmet", "academia", "salao de jogos", "brinquedoteca",
             "playground", "churrasqueira", "quadra", "area verde", "piscina", "espaco zen",
             "lavanderia", "coworking")

area_comum <- data.frame(matrix(ncol = length(acomum), nrow = 0))
colnames(area_comum) <- acomum

nivel<-0
for (x in 1:dim(b)[1]){
  nivel <- nivel + 1
  ed_ac <- b$tem_ac[x] %>% str_split(",") %>% as.vector()
  ed_ac <- ed_ac[[1]] %>% str_squish()
  for (y in 1:length(ed_ac)){
    for (z in 1:dim(area_comum)[2]){
      if (ed_ac[y] == colnames(area_comum)[z]){
        area_comum[nivel,z] <-1
      }
    }
  }
}

#substituindo NA por 0
area_comum[is.na(area_comum)] <- 0

#removendo coluna infraestrutura
b <- b[-c(6,7)]

b <- b %>% cbind(area_comum)

b %>%  write.csv(.,file = "loft_ac_220211.csv", row.names = FALSE)

#####
# 3.0 WEBSCRAPPING - COORDENADAS GEOGRAFICAS
#####

a <- read.csv("loft220114_comp.csv")
b <- read.csv("loft220114_condominios.csv")

df_cond_end_geral <- data.frame()

# Extraindo endereços do DF-APARTAMENTOS
a$tem_numero <- a$end_comp %>% str_extract("\\d+")
end_apt_01 <- a %>% dplyr::select(end_comp, cond_url, tem_numero) %>% filter(!is.na(tem_numero))
end_apt_01 <- end_apt_01 %>% distinct(end_comp, .keep_all= TRUE) %>% filter(cond_url=='ND') %>% 
  dplyr::select(end_comp, tem_numero)
names(end_apt_01) <- c('morada', 'numero')
df_cond_end_geral <- df_cond_end_geral %>% rbind(end_apt_01)

b$tem_numero <- b$end_condm %>% str_extract("\\d+")
end_apt_02 <- b %>% dplyr::select(end_condm, tem_numero)%>%filter(!is.na(tem_numero))
names(end_apt_02) <- c('morada', 'numero')
df_cond_end_geral <- df_cond_end_geral %>% rbind(end_apt_02)

df_cond_end_geral <- df_cond_end_geral %>% distinct(morada, .keep_all= TRUE)

#######
# 3.1 Ajustando endereços que não indicaram o LAT LONG CORRETO ou NA
#######
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada

df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% str_replace_all("parque interlagos", "interlagos")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% str_replace_all("moema indios", "moema")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% str_replace_all("moema passaros", "moema")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% str_replace_all("rua do rocio, 159 . brooklin", "rua do rocio, 159 . vila olimpia")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% str_replace_all("cidade universitaria", "vila indiana")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% str_replace_all("rua jussara, 80 . cursino", "rua jucara, 80 . bosque da saude")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% str_replace_all("rua william furneau 140, sao domingos", "rua william furneau 140, vila pirituba")

df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua tupi, 171 . sumare", "rua tupi, 171 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua aracaju, 42 . sumare", "rua aracaju, 42 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua aracaju, 137 . sumare", "rua aracaju, 137 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua ceara, 45 . sumare", "rua ceara, 45 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua ceara, 101 . sumare", "rua ceara, 101 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua ceara, 449 . sumare", "rua ceara, 449 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua bahia, 70 . sumare", "rua bahia, 70 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua bahia, 116 . sumare", "rua bahia, 116 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua bahia, 388 . sumare", "rua bahia, 388 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua bahia, 450 . sumare", "rua bahia, 450 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua pernambuco, 74 . sumare", "rua pernambuco, 74 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua pernambuco, 88 . sumare", "rua pernambuco, 88 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua pernambuco, 147 . sumare", "rua pernambuco, 147 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua pernambuco, 167 . sumare", "rua pernambuco, 167 . higienopolis")
df_cond_end_geral$morada_ajust <- df_cond_end_geral$morada_ajust %>% 
  str_replace_all("rua pernambuco, 190 . sumare", "rua pernambuco, 190 . higienopolis")

#######
# 3.2 BUSCA DOS LAT / LONG
#######

#Abre o servidor
remDr$open()

remDr$navigate('https://www.google.com/maps')
df_latlong <- data.frame()
#df_latlong <- read.csv("loft220117_latlong_na.csv")

for (i in 1900:dim(df_cond_end_geral)[1]){ #df_cond_end_geral (trocado para rodar novamente os NA)
  # Clicar em pesquisa
  pesq_glmaps <- remDr$findElement(using = 'id', value = 'searchboxinput')
  pesq_glmaps$clickElement()
  pesq_glmaps$sendKeysToElement(list(df_cond_end_geral$morada_ajust[i], key = "enter"))
  
  #status de avanço
  st_pesq <- paste(i,'/', dim(df_cond_end_geral)[1], round(i/dim(df_cond_end_geral)[1], 5)*100, '%')
  print(st_pesq)
  
  # Aguardar para pegar os dados
  Sys.sleep(3)
  # Pegando URL
  url_glmaps <- remDr$getCurrentUrl() %>% as.character() %>% str_extract("(?<=)3d-\\S+(?=)")
  
  # REGEX para retirar o Lat/Long
  latitude <- url_glmaps %>% str_extract("(?<=\\D)\\S+!(?=)") %>% str_extract("(?<=)\\S+(?=\\D)")
  longitude <- url_glmaps %>% str_extract("(?<=)4d\\S+(?=)") %>% str_extract("(?<=\\D)\\S+(?=)")
  
  # Limpando a pesquisa
  pesq_glmaps <- remDr$findElement(using = 'class', value = 'sbcb_a')
  pesq_glmaps$clickElement()
  
  latlong_ <- c(df_cond_end_geral$morada_ajust[i], latitude, longitude)
  
  df_latlong <- df_latlong %>% rbind(latlong_)
}

names(df_latlong) <- c('endereco', 'latitude', 'longitude')

# verificando endereços que deram latlong NA
df_latlong_na <- df_latlong %>% filter(is.na(latitude))

##rua fernando de albuquerque, 287 . bela vista
#df_latlong[1231,2] <- "-23.5531217"
#df_latlong[1231,3] <- "-46.6588679"
##rua cayowaa, 1082 . perdizes
#df_latlong[4790,2] <- "-23.5376638"
#df_latlong[4790,3] <- "-46.6832698"
##rua jardimirim, 140 . santana
#df_latlong[5620,2] <- "-23.4852054"
#df_latlong[5620,3] <- "-46.6365671"
##rua torquato tasso, 318 . vila prudente
#df_latlong[7203,2] <- "-23.5767746"
#df_latlong[7203,3] <- "-46.5798167"

a %>% filter(end_comp=='rua bahia, 338 . sumare')
b %>% filter(end_condm=='rua pernambuco, 190 . sumare')
#df_latlong<- df_latlong %>% drop_na(latitude)

df_latlong %>%  write.csv(.,file = "loft220122_latlong.csv", row.names = FALSE)

#####
# 3.3 MAPA - SHAPEFILES
#####

df_latlong <- read.csv("loft220122_latlong.csv")

# Baixa o mapa da PDE - ZEU
shp_pde <- readOGR(dsn = "shp", layer = "03_Eixo_Existente") %>%
  st_as_sf(crs = 4326)

# Visualizar tabela
shp_pde %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Baixa o mapa da SUBPREFEITURA
shp_sub <- readOGR(dsn = "shp", layer = "SAD69-96_SHP_subprefeitura_polygon") %>%
  st_as_sf(crs = 4326)

# Visualizar tabela
shp_sub %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

#plot(shp_pde)

# inserindo coordenadas lat/long no formato st
sf_latlong <- st_as_sf(x = df_latlong, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)
#ptn_sf <- data.frame(sf_latlong$geometry)

#tm_shape(shp = sf_latlong) + 
#  tm_dots(size = 1)

#gerando mapa para visualização
tmap_mode("view")

#plotando o mapa com os pontos
tm_shape(shp = sf_latlong) + 
  tm_dots(col = "red", 
          border.col = "black", 
          size = 0.05, 
          alpha = 0.5) + 
  tm_shape(shp = shp_pde) + 
  tm_borders(lty='solid', col='black') +
  tm_shape(shp = shp_sub) + 
  tm_borders(lty='solid', col='blue')

pde_area <- st_transform(shp_pde, 4326)
sub_area <- st_transform(shp_sub, 4326)
pnts_cond <- st_transform(sf_latlong, 4326) 

pnts_intersec <- sf_latlong %>% mutate(
  zeu = st_is_within_distance( pnts_cond,pde_area, 8),
  sub_pref = st_intersects( pnts_cond,sub_area)
  )

# dummy - se pertence a ZEU ou não após o mapeamento dos pontos
pnts_intersec$zeu_ <- sapply(pnts_intersec$zeu, 
                                  function(x){ ifelse(length(x) == 0, 0, 1)})

#junção do dataframe de intersecção e o nome dos distritos
a<- data.frame(pnts_intersec)
b<- data.frame(shp_sub) #%>% select("sp_nome")
b<- b[-c(1,2,4)]

b$cod_bairro <- 1:32

a$sub_pref2 <- sapply(a$sub_pref, function(x){as.numeric(x[1])})

pnts_sub_pref <- left_join(a, b, by=c("sub_pref2"="cod_bairro"))

pnts_sub_pref <- pnts_sub_pref[-c(3, 4, 6)]

pnts_sub_pref <- left_join(pnts_sub_pref, df_latlong, by=c("endereco"="endereco"))

pnts_sub_pref <- pnts_sub_pref[-c(2)]

pnts_sub_pref %>%  write.csv(.,file = "loft_pref_220125.csv", row.names = FALSE)

#####
# 4. DATAFRAME FINAL - EMPREENDIMENTOS LANÇADOS
#####

# recuperando os DF desenvolvidos
df_ape <- read.csv("loft220109.csv")
a <- read.csv("loft220114_comp.csv")
b <- read.csv("loft_ac_220211.csv")
c <- read.csv("loft_pref_220125.csv")

#juntando a listagem geral com o detalhe dos apartamentos -> DF final
df_final <- left_join(df_ape, a, by=c("endereco"="url.ape"))
df_final <- df_final[-c(1)] # retirando coluna - 'endereco

# juntando DF final junto com o detalhe de cada condomínio -> DF FINAL
df_final <- left_join(df_final, b, by=c("cond_url"="url_cond"))
# eliminando: mobiliado_ape / portaria_ape / dist_metro / cond_ruo / descricao_cond
df_final <- df_final[-c(11, 12, 13, 15, 20)]
names(df_final)

# juntando DF final junto com o detalhe de localização (lat/long) -> DF FINAL

# tendo em vista que haviam endereços completos sem dados de condomínio, o lat long que resgatamos tb considera.
df_final$tem_numero <- df_final$end_comp %>% str_extract("\\d+") %>% as.integer()

for (i in 1:dim(df_final)[1]){
  if(!is.na(df_final$end_condm[i])){
    df_final$end_final[i] <- df_final$end_condm[i]
    } else if (is.na(df_final$tem_numero[i])){
      df_final$end_final[i] <-NA
    } else {
      df_final$end_final[i] <- df_final$end_comp[i]
    }
}
# eliminando: end_comp / end_condm / tem_numero 
df_final <- df_final[-c(3, 12, 29)]
names(df_final)

# inserindo 0 onde deu NA
df_final$end_final[is.na(df_final$end_final)] <- 0

# 15341/25405 - 60% da base possui dados de endereço para mapeamento
df_final %>% filter(end_final != 0) %>% dim()


df_final <- left_join(df_final, c, by=c("end_final"="endereco"))
# eliminando: bairro 
df_final <- df_final[-c(2)]

df_final$gi_df <- 0

names(df_final)

# padronizando nomes das colunas
loft_n_columns <- c("ape_preco","ape_pav","ape_area","ape_precom2","ape_dorm","ape_suite","ape_banheiro","ape_vaga","cond_nome",
"cond_ano","cond_n_torres","cond_elevador","salao.de.festas","espaco.gourmet","academia","salao.de.jogos",
"brinquedoteca","playground","churrasqueira","quadra","area.verde","piscina","espaco.zen","lavanderia","coworking",
"cond_end","zeu","sp_nome","latitude","longitude","gi_df")

colnames(df_final) <- loft_n_columns

df_final <- df_final %>% dplyr::select("ape_preco","ape_precom2","ape_area","ape_dorm","ape_suite","ape_banheiro","ape_vaga","cond_nome","cond_end",
"cond_ano","cond_n_torres","cond_elevador","salao.de.festas","espaco.gourmet","academia","salao.de.jogos",
"brinquedoteca","playground","churrasqueira","quadra","area.verde","piscina","espaco.zen","lavanderia",
"coworking","zeu","sp_nome","latitude","longitude","gi_df" )

df_final %>%  write.csv(.,file = "data/loft_final_R01.csv", row.names = FALSE)

df_final <- read.csv("data/loft_final_R01.csv")

xx <- df_final %>% filter(cond_ano < 1900)

colSums(is.na(df_final))
