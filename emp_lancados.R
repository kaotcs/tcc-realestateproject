pacotes <- c("stringr", 'tidyverse', 'readxl', "XML","RSelenium","stringr","rvest", 'xml2', 
             "sf","tmap","rgdal","rgeos")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#######
# 0. CONFIGURANDO SELENIUM
#######

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

#######
# 1. EMPREENDIMENTOS LANÇADOS - AJUSTE AREA COMUM - Output (gi_220222.csv)
#######

# Lendo o Dataframe
gi <- read_excel("gi/gi_df_R01.xlsx", col_names = FALSE)

#renomeando e ajustando o nome das colunas
names_ <- gi[1,] %>% iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% tolower() %>% as.vector()
colnames(gi) <- names_
gi <- gi %>% slice(-c(1))

#inserindo o item "sem area comum"
gi$infraestrutura <- gi$infraestrutura %>% replace_na("sem area comum")

#Verificando a quantidade de tipos de area comum disponível
acomum <- gi %>% distinct(infraestrutura, .keep_all= FALSE) %>% iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% tolower()
acomum <- acomum %>% str_replace_all("[^[:alnum:]]", " ")
acomum <- acomum %>% str_split("  ")
acomum <- unique(acomum[[1]])
acomum <- acomum %>% str_squish()
acomum <- unique(acomum)
#removendo os itens errados ("c", "" e "na")
acomum <- acomum[-c(1, 9)]

#criando dt para explodir a area comum em colunas individuais
acomum <- c( "salao de festas", "espaco gourmet", "academia", "salao de jogos", "brinquedoteca",
             "playground", "churrasqueira", "quadra", "area verde", "piscina", "espaco zen",
             "lavanderia", "coworking")
area_comum <- data.frame(matrix(ncol = length(acomum), nrow = 0))
colnames(area_comum) <- acomum

gi$infraestrutura <- gi$infraestrutura %>% iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% tolower()

#teste <- gi$infraestrutura[274] %>% str_split(",") %>% as.vector()
#teste <- teste[[1]] %>% str_squish()

## READEQUAÇÂO DOS NOMES - o DF possuia 86 nomes para as áreas comuns. será resumido conforme abaixo

#espaço gourmet
gi$infraestrutura <- gi$infraestrutura %>% str_replace_all( c("bar"="espaco gourmet"))

#academia
gi$infraestrutura <- gi$infraestrutura %>% str_replace_all( c("sala de ginastica"="academia",
                                                              "fitness"="academia",	
                                                              "sala de spinning"="academia",
                                                              "espaco multiuso"="academia",	
                                                              "saco boxe"="academia"))

#salao de jogos
gi$infraestrutura <- gi$infraestrutura %>% str_replace_all( c("sala de cinema"="salao de jogos",
  "home theater"="salao de jogos"))

#brinquedoteca
gi$infraestrutura <- gi$infraestrutura %>% str_replace_all( c("sala de recreacao infantil"="brinquedoteca",
                                                              "salao de festas infantil"="brinquedoteca",
                                                              "salao de festas adolescente"="brinquedoteca",
                                                              "bercario"="brinquedoteca"
                                                              ))

#churrasqueira
gi$infraestrutura <- gi$infraestrutura %>% str_replace_all( c("forno a lenha"="churrasqueira",
                                                              "lareira"="churrasqueira", 
                                                              "lareira externa"="churrasqueira"))

#quadra
gi$infraestrutura <- gi$infraestrutura %>% str_replace_all( c("quadra de tennis"="quadra",
                                                              "quadra poli esportiva"="quadra", 
                                                              "quadra gramada"="quadra",
                                                              "pista skate"="quadra",
                                                              "mini golf"="quadra",
                                                              "quadra de volei areia"="quadra",
                                                              "espiribol"="quadra",
                                                              "quadra paddle"="quadra"
                                                              ))

#area verde
gi$infraestrutura <- gi$infraestrutura %>% str_replace_all( c("jardins"="area verde",
                                                              "pista de cooper"="area verde", 
                                                              "quadra gramada"="area verde",
                                                              "trilha" ="area verde",
                                                              "lago"="area verde"
                                                              ))

#piscina
gi$infraestrutura <- gi$infraestrutura %>% str_replace_all( c("piscina adulta"="piscina",
                                                              "solarium"="piscina", 
                                                              "piscina infantil"="piscina",
                                                              "bar com piscina"="piscina",
                                                              "prainha"="piscina",
                                                              "piscina coberta"="piscina",
                                                              "piscina semi climatizada"="piscina",
                                                              "piscina climatizada"="piscina",
                                                              "piscina semi olimpica"="piscina"
                                                              ))

#espaco zen
gi$infraestrutura <- gi$infraestrutura %>% str_replace_all( c("spa"="espaco zen",
                                                              "sauna umida"="espaco zen", 
                                                              "ofuro"="espaco zen",
                                                              "sauna seca"="espaco zen",
                                                              "espaco meditacao"="espaco zen",
                                                              "sala de descanso" ="espaco zen"
                                                              ))

#lavanderia
gi$infraestrutura <- gi$infraestrutura %>% str_replace_all( c("lavanderia coletiva"="lavanderia"))

#coworking
gi$infraestrutura <- gi$infraestrutura %>% str_replace_all( c("estacao trabalho"="coworking",
                                                              "espaco cultura"="coworking", 
                                                              "lanhouse"="coworking",
                                                              "home office"="coworking",
                                                              "sala de reuniao"="coworking",
                                                              "area de conveniencia"="espaco zen"
                                                              ))

nivel<-0
for (a in 1:dim(gi)[1]){
  nivel <- nivel + 1
  ed_ac <- gi$infraestrutura[a] %>% str_split(",") %>% as.vector()
  ed_ac <- ed_ac[[1]] %>% str_squish()
  ed_ac <- unique(ed_ac)
  for (b in 1:length(ed_ac)){
    for (c in 1:dim(area_comum)[2]){
      if (ed_ac[b] == colnames(area_comum)[c]){
        area_comum[nivel,c] <-1
      }
    }
  }
}

#substituindo NA por 0
area_comum[is.na(area_comum)] <- 0

#removendo coluna infraestrutura
gi <- gi[, -7]

gi <- gi %>% cbind(area_comum)

gi %>%  write.csv(.,file = "gi/gi_220202.csv", row.names = FALSE)

#######
# 2. BUSCA DOS LAT / LONG
#######

enderecos <- paste0(gi$endereco, ' . ' ,gi$bairro) 

enderecos <- enderecos %>% iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% 
  tolower() %>% unique()

#Abre o servidor
remDr$open()

#df_latlong_lanc <- data.frame()

enderecos[4143] #<- "rua da mooca, 1290 . mooca"

#"lg arouche do, 246 . republica"
#"da mooca, 1290 . mooca"

remDr$navigate('https://www.google.com/maps')

df_latlong_lanc <- read.csv("loft220124_latlong_lanc.csv")

for (i in 3985:length(enderecos)){
  # Clicar em pesquisa
  pesq_glmaps <- remDr$findElement(using = 'id', value = 'searchboxinput')
  pesq_glmaps$clickElement()
  pesq_glmaps$sendKeysToElement(list(enderecos[i]))#, key = "enter"))
  pesq_glmaps <- remDr$findElement(using = 'id', value = 'searchbox-searchbutton')
  pesq_glmaps$clickElement()
  
  #status de avanço
  st_pesq <- paste(i,'/', length(enderecos), round(i/length(enderecos), 5)*100, '%')
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
  
  latlong_ <- c(enderecos[i], latitude, longitude)
  
  df_latlong_lanc <- df_latlong_lanc %>% rbind(latlong_)
}

#names(df_latlong_lanc) <- c('endereco', 'latitude', 'longitude')
#df_latlong_lanc$ordem <- 1:4199

# verificando NA na pesquisa do Google
df_latlong_na <- df_latlong_lanc %>% filter(is.na(latitude))

#df_latlong_lanc <- df_latlong_lanc[-c(4094),]

df_latlong_na %>%  write.csv(.,file = "ajuste_na2.csv", row.names = FALSE)

#######
# 2.1 AJUSTANDO LAT/LONG QUE NÃO VIERAM NO GOOGLE
# - o ajuste foi feito manual, tendo em vista que os endereços inseridos dava resultados dúbios para o googlemaps
#######

na_latlong <- read_excel("gi/gi_na_latlong.xlsx", col_names = FALSE)
#renomeando e ajustando o nome das colunas
names_ <- c("endereco","latitude","longitude","ordem", "google")
colnames(na_latlong) <- names_
na_latlong <- na_latlong[-1,]

#verificando REGEX antes de inserir na tabela principal
na_latlong$latitude <- na_latlong$google %>% str_extract("(?<=)3d-\\S+(?=)") %>% 
  str_extract("(?<=\\D)\\S+!(?=)") %>% #(?<=\D)\S+!4(?=)
  str_extract("(?<=)\\S+(?=\\D)")

na_latlong$longitude <- na_latlong$google %>% str_extract("(?<=)3d-\\S+(?=)") %>% 
  str_extract("(?<=)4d\\S+(?=)") %>% 
  str_extract("(?<=\\D)\\S+(?=)")

for (i in 1:dim(na_latlong)[1]){
  pos <- as.integer(na_latlong[i, 4][[1]])
  # latitude
  df_latlong_lanc[pos, 2] <- na_latlong[i, 5][[1]] %>% str_extract("(?<=)3d-\\S+(?=)") %>% 
    str_extract("(?<=\\D)\\S+!(?=)") %>% #(?<=\D)\S+!4(?=)
    str_extract("(?<=)\\S+(?=\\D)")
  # longitude
  df_latlong_lanc[pos, 3] <- na_latlong[i, 5][[1]] %>% str_extract("(?<=)3d-\\S+(?=)") %>% 
    str_extract("(?<=)4d\\S+(?=)") %>% 
    str_extract("(?<=\\D)\\S+(?=)")
}

# Ajustando endereços errados - Google indicou outros municios
#R ABDOM BATISTA, 175 -23.5376525 -46.4823949
df_latlong_lanc[14,2] <- "-23.5376525"
df_latlong_lanc[14,3] <- "-46.4823949"
#ROD RAPOSO TAVARES, 17500 -23.584052 / -46.7777138
df_latlong_lanc[1925,2] <- "-23.584052"
df_latlong_lanc[1925,3] <- "-46.7777138"

df_latlong_lanc %>%  write.csv(.,file = "gi/gi220125_latlong_lanc.csv", row.names = FALSE)

#for (i in 1:dim(na_latlong)[1]){
#  print(df_latlong_lanc[as.integer(na_latlong[i, 4][[1]]), ])
#}

#######
# 3. MAPA - SHAPEFILES
#######

df_latlong_lanc <- read.csv("gi/gi220125_latlong_lanc.csv")

# Baixa o mapa da PDE - ZEU
shp_pde <- readOGR(dsn = "shp", layer = "03_Eixo_Existente") %>%
  st_as_sf(crs = 4326)

# Baixa o mapa da SUBPREFEITURA
shp_sub <- readOGR(dsn = "shp", layer = "SAD69-96_SHP_subprefeitura_polygon") %>%
  st_as_sf(crs = 4326)

# inserindo coordenadas lat/long no formato st
sf_latlong_lanc <- st_as_sf(x = df_latlong_lanc, 
                       coords = c("longitude", "latitude"), 
                       crs = 4326)

#gerando mapa para visualização
tmap_mode("view")

#plotando o mapa com os pontos
tm_shape(shp = sf_latlong_lanc) + 
  tm_dots(col = "purple", 
          border.col = "black", 
          size = 0.05, 
          alpha = 0.5) + 
  tm_shape(shp = shp_pde) + 
  tm_borders(lty='solid', col='black') +
  tm_shape(shp = shp_sub) + 
  tm_borders(lty='solid', col='blue')

#gerando os pontos para verificar interseccao
pde_area <- st_transform(shp_pde, 4326)
sub_area <- st_transform(shp_sub, 4326)
pnts_cond <- st_transform(sf_latlong_lanc, 4326)

#novo df checando interccao
pnts_intersec_lanc <- sf_latlong_lanc %>% mutate(
  zeu = st_is_within_distance( pnts_cond,pde_area, 8),
  sub_pref = st_intersects(pnts_cond, sub_area) #1187 #289
)

# dummy - se pertence a ZEU ou não após o mapeamento dos pontos
pnts_intersec_lanc$zeu_ <- sapply(pnts_intersec_lanc$zeu, 
                                  function(x){ ifelse(length(x) == 0, 0, 1)})

#junção do dataframe de intersecção e o nome dos distritos
a<- data.frame(pnts_intersec_lanc)
b<- data.frame(shp_sub) %>% select("sp_nome")

b$cod_bairro <- 1:32

a$sub_pref2 <- sapply(a$sub_pref, function(x){as.numeric(x[1])})

pnts_sub_pref <- left_join(a, b, by=c("sub_pref2"="cod_bairro"))

pnts_sub_pref <- pnts_sub_pref[-c(2, 4, 5, 7)]

pnts_sub_pref <- left_join(pnts_sub_pref, df_latlong_lanc, by=c("endereco"="endereco"))

pnts_sub_pref <- pnts_sub_pref[-c(2, 7)]

pnts_sub_pref %>%  write.csv(.,file = "pref_220125.csv", row.names = FALSE)

#####
# 4. DATAFRAME FINAL - EMPREENDIMENTOS LANÇADOS
#####

gi <- read.csv("gi/gi_220202.csv")
pnts_sub_pref <- read.csv("pref_220125.csv")

# adicionando o campo de endereo completo
gi$n_enderecos <- paste0(gi$endereco, ' . ' ,gi$bairro) %>% iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% 
  tolower()

# apagando os campos originais
gi <- gi[-c(2,3)]

#juntando os DF
gi_final <- left_join(gi, pnts_sub_pref, by=c("n_enderecos"="endereco"))

#marcando origem para filtro
gi_final$gi_df <- 1

gi_final %>%  write.csv(.,file = "data/gi_final.csv", row.names = FALSE)

gi_final_gi <- read.csv("data/gi_final.csv")

names(gi_final_gi)

# padronizando nomes das colunas
gi_n_columns <- c("cond_nome","cond_ano","cond_n_torres","cond_elevador","ape_dorm","ape_suite","ape_banheiro","ape_vaga",
"ape_area","ape_pav","ape_preco","ape_precom2","salao.de.festas","espaco.gourmet","academia","salao.de.jogos",
"brinquedoteca","playground","churrasqueira","quadra","area.verde","piscina","espaco.zen","lavanderia","coworking",
"espaco.mulher","salao.de.festas.infantil","cond_end","zeu","sp_nome","latitude","longitude","gi_df")

colnames(gi_final_gi) <- gi_n_columns

gi_final_gi <- gi_final_gi %>% dplyr::select("ape_preco","ape_precom2","ape_area","ape_dorm","ape_suite","ape_banheiro","ape_vaga","cond_nome","cond_end",
                                       "cond_ano","cond_n_torres","cond_elevador","salao.de.festas","espaco.gourmet","academia","salao.de.jogos",
                                       "brinquedoteca","playground","churrasqueira","quadra","area.verde","piscina","espaco.zen","lavanderia",
                                       "coworking","espaco.mulher","salao.de.festas.infantil","zeu","sp_nome","latitude","longitude","gi_df" )

gi_final_gi %>%  write.csv(.,file = "data/gi_final.csv", row.names = FALSE)


xx <- gi_final_gi %>% filter(cond_ano < 2000)
