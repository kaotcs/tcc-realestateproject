#######
# 0.0 IMPORTS
#######

pacotes <- c('tidyverse', 'data.table', 'gridExtra', 'pixiedust', 'kableExtra', 'PerformanceAnalytics',
             'fastDummies', 'nortest', 'car', 'caret', 'plotly', 'jtools', 'Metrics', 'sjPlot', 'MLmetrics',
             'olsrr', 'Boruta', 'stats', 'randomForest', 'DALEX', 'snakecase', 'xgboost')

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
# 0.1 FUNCTIONS
#######

#######
# 0.2 IMPORTING DATA
#######

# DF webscrap LOF
df_raw_loft <- read.csv("data/loft_final_R01.csv")
#ajustando campo de precos em str
df_raw_loft$ape_preco <- df_raw_loft$ape_preco %>% str_replace_all("\\D","") %>% as.numeric()

# DF DB
df_raw_gi <- read.csv("data/gi_final.csv")
df_raw_gi <- df_raw_gi[-c(26,27)]
df0 <- df_raw_loft %>% rbind(df_raw_gi)

#######
# 1.0 DATA DESCRIPTION
#######

df1 <- copy(df0)

# 1.1. Rename Columns
names(df1) #ajustado nos scripts separados

# 1.2. Data Dimensions
dim(df1)

# 1.3. Data Types
str(df1)

# 1.4. Check NAs
df1_ <- data.frame(colSums(is.na(df1)))

# Tabela NAs
colnames(df1_) <- 'total_na'
df1_$features <- rownames(df1_)
rownames(df1_) <- 1:nrow(df1_)
df1_ <- df1_ %>% dplyr::select('features', 'total_na')

ggplot(df1_, aes(features, total_na)) + geom_bar(stat="identity", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text(aes(label=total_na), vjust=-0.5, hjust=0, size=3.5, angle=45) + ylab("Total NAs")

df1_ %>%
  kable() %>% kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

# 1.5. Fillout NAs
#neste ciclo vou dropar todas as colunas com NAs para rodar o modelo
df1 <- df1 %>% drop_na(ape_banheiro, ape_dorm, latitude, cond_ano, sp_nome)


# 1.6. Change Types

# changing for factors
# DADOS DO APE
df1$ape_dorm <- df1$ape_dorm %>% as.factor()
df1$ape_suite <- df1$ape_suite %>% as.factor()
df1$ape_banheiro <- df1$ape_banheiro %>% as.factor()
df1$ape_vaga <- df1$ape_vaga %>% as.factor()

# DADOS DO CONDOMINIO
#df1$cond_ano <- df1$cond_ano %>% as.factor()
df1$cond_n_torres <- df1$cond_n_torres %>% as.factor()
df1$cond_elevador <- df1$cond_elevador %>% as.factor()
# - AREAS COMUNS
df1$salao.de.festas <- df1$salao.de.festas %>% as.factor()
df1$espaco.gourmet <- df1$espaco.gourmet %>% as.factor()
df1$academia <- df1$academia %>% as.factor()
df1$salao.de.jogos <- df1$salao.de.jogos %>% as.factor()
df1$brinquedoteca <- df1$brinquedoteca %>% as.factor()
df1$playground <- df1$playground %>% as.factor()
df1$churrasqueira <- df1$churrasqueira %>% as.factor()
df1$quadra <- df1$quadra %>% as.factor()
df1$area.verde <- df1$area.verde %>% as.factor()
df1$piscina <- df1$piscina %>% as.factor()
df1$espaco.zen <- df1$espaco.zen %>% as.factor()
df1$lavanderia <- df1$lavanderia %>% as.factor()
df1$coworking <- df1$coworking %>% as.factor()
#df1$espaco.mulher <- df1$espaco.mulher %>% as.factor()
#df1$salao.de.festas.infantil <- df1$salao.de.festas.infantil %>% as.factor()

# DADOS GEOGRAFICOS
df1$zeu <- df1$zeu %>% as.factor()
df1$sp_nome <- df1$sp_nome %>% as.factor()

# ORIGEM DF
df1$gi_df <- df1$gi_df %>% as.factor()


# 1.7. Descriptive Statistical
summary(df1)

#######
# 2.0 FEATURE ENGINEERING
#######
df2 <- copy(df1)
distinct(df2, cond_ano)

# A aprovação do PDE foi realizada em 2014, no qual a vingência passou a ser 2015
# Entretanto, os primeiros projetos aprovados e entregues passaram a ocorrer meados de 2019
df2$conc_pde <- sapply(df2$cond_ano, function(x){ ifelse(as.integer(x) > 2018, 1, 0)})
df2$em_obra <- sapply(df2$cond_ano, function(x){ ifelse(as.integer(x) == 0, 1, 0)})

# Indicando quais projetos aprovados em ZEU estão inseridos na nova lei
df2$zeu_npde <- 3
for (i in 1:dim(df2)[1]){
  if (((df2$conc_pde[i] == 1) & (df2$zeu[i] == 1)) | (df2$em_obra[i] == 1) & (df2$zeu[i] == 1)){
    df2$zeu_npde[i] <-1
  } else {
    df2$zeu_npde[i] <-0
  }
}

df2 <- df2[-c(30, 31, 32)]

#distinct(df2, conc_pde)
#xx <- df2 %>% dplyr::select(zeu_npde) %>% group_by(zeu_npde) %>% summarise(total=n())

names(df2)

#######
# 3.0 FEATURE FILTERING
#######
df3 <- copy(df2)

summary(df3)

# Linhas
# Tendo em visto que alguns anúncios tiveram o valor do apartamento preenchido como zero. estes serão removidos
df3 <- df3 %>% filter(!ape_preco==0)

df3 <- df3 %>% filter(ape_preco < 13000000)

# Verificando quantidade mínima de 5 pontos para cada subprefeitura (com e sem ZEU)
aux <- df3 %>% dplyr::select(ape_dorm, sp_nome, zeu_npde) %>% group_by(zeu_npde, sp_nome, ape_dorm) %>% summarise(qtd_ = n())
aux$zona_ape_dorm <- paste0(aux$sp_nome, "_",aux$ape_dorm)
aux1_cz <- aux %>% dplyr::filter(zeu_npde==1)
aux1_sz <- aux %>% dplyr::filter(zeu_npde==0)
aux1_f <- full_join(aux1_cz, aux1_sz, by=c('zona_ape_dorm'='zona_ape_dorm'))
aux_col <- c("zeu1", "sp_nome1", "ape_dorm1", "qtd_cz", "zona_ape_dorm", 
             "zeu0","sp_nome0", "ape_dorm0", "qtd_sz")
colnames(aux1_f) <- aux_col
aux1_f[1:93, 8] <- aux1_f[1:93, 3] # o full join deixa as celulas q n deu match em branco. apenas preenchendo
aux1_f[1:93, 7] <- aux1_f[1:93, 2]

aux1_f <- aux1_f[-c(1, 2, 3, 6, 7, 8)]
aux1_f[is.na(aux1_f)] <- 0
aux1_f <- aux1_f %>% dplyr::select("zona_ape_dorm","qtd_sz", "qtd_cz")

names(aux1_f)

# SUBDIVISÃO AREA POR FAIXA DE DORMITÓRIO
aux3 <- df3 %>% dplyr::select(ape_area, ape_dorm,sp_nome, zeu_npde) %>% group_by(ape_dorm, zeu_npde, sp_nome) %>% summarise(media = mean(ape_area))
aux3$sub_dorm <- paste0(aux3$sp_nome, "_",aux3$ape_dorm)
aux3_cz <- aux3 %>% filter(zeu_npde==1)
aux3_sz <- aux3 %>% filter(zeu_npde==0)
aux3_f <- full_join(aux3_cz, aux3_sz, by='sub_dorm')

rev_nome <- c('ape_dorm_', 'zeu1', 'sp_nome_', 'area_media_zeu', 'sub_dorm',
              'ape_dorm', 'zeu0', 'sp_nome', 'area_media')

colnames(aux3_f) <- rev_nome

aux3_f[1:73, 8] <- aux3_f[1:73, 3] # o full join deixa as celulas q n deu match em branco. apenas preenchendo
aux3_f[1:73, 6] <- aux3_f[1:73, 1]
aux3_f <- aux3_f[-c(1, 2, 3, 7)]
aux3_f <- aux3_f %>% dplyr::select('sp_nome', 'ape_dorm', 'area_media', 'area_media_zeu', 'sub_dorm')

aux_area_dorm <- left_join(aux1_f, aux3_f, by=c('zona_ape_dorm'='sub_dorm'))
aux_area_dorm <- aux_area_dorm[-c(1)]
aux_area_dorm <- aux_area_dorm %>% 
  dplyr::select('sp_nome', 'ape_dorm', 'qtd_sz', 'area_media', 'qtd_cz', 'area_media_zeu')

# CRIANDO TABELA PARA VISUALIZAR
aux_area_dorm %>% dplyr::select('sp_nome', 'ape_dorm', 'qtd_sz', 'qtd_cz') %>% arrange(sp_nome, ape_dorm) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Para desenvolvimento do modelo, cada distrito deverá possuir no mínimo 5 apartamentos por tipologia tanto no ZEU
# quanto nas zonas sem ser ZEU.
# Neste sentido, serão removidos os seguintes distritos:
# - ARICANDUVA-FORMOSA-CARRAO
# - CASA VERDE-CACHOEIRINHA
# - CIDADE TIRADENTES
# - ERMELINO MATARAZZO
# - GUAIANASES
# - ITAIM PAULISTA
# - JACANA-TREMEMBE
# - PERUS
# - SAO MATEUS
# - SAO MIGUEL
# - VILA MARIA-VILA GUILHERME
df3_ <- df3 %>% filter(!((sp_nome=='ARICANDUVA-FORMOSA-CARRAO') |
                         (sp_nome=='CASA VERDE-CACHOEIRINHA') |
                         (sp_nome=='CIDADE TIRADENTES') |
                         (sp_nome=='ERMELINO MATARAZZO') |
                         (sp_nome=='GUAIANASES') |
                         (sp_nome=='ITAIM PAULISTA') |
                         (sp_nome=='JACANA-TREMEMBE') |
                         (sp_nome=='PERUS') |
                         (sp_nome=='SAO MATEUS') |
                         (sp_nome=='SAO MIGUEL') |
                         (sp_nome=='VILA MARIA-VILA GUILHERME')
                         ))
# REMOÇÂO POR FAIXA DE TIPOLOGIA
# BUTANTÃ - 4, 5 e 6 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='BUTANTA') & ((ape_dorm==4) | (ape_dorm==5) | (ape_dorm==6))))
# CAMPO LIMPO - 3, 4, 5, 6DORM
df3_ <- df3_ %>% filter(!((sp_nome=='CAMPO LIMPO') & ((ape_dorm==3) | (ape_dorm==4)| (ape_dorm==5)| (ape_dorm==6))))
# CAPELA DO SOCORRO - 1, 3, 4 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='CAPELA DO SOCORRO') & ((ape_dorm==1) | (ape_dorm==3)| (ape_dorm==4))))
# CIDADE ADEMAR - 4 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='CIDADE ADEMAR') & (ape_dorm==4)))
# FREGUESIA-BRASILANDIA - 1, 3, 4 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='FREGUESIA-BRASILANDIA') & ((ape_dorm==1) | (ape_dorm==3)| (ape_dorm==4))))
# IPIRANGA - 0, 5 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='IPIRANGA') & ((ape_dorm==0) | (ape_dorm==5))))
# ITAQUERA - 1, 3, 4  DORM
df3_ <- df3_ %>% filter(!((sp_nome=='ITAQUERA') & ((ape_dorm==1) | (ape_dorm==3) | (ape_dorm==4))))
# JABAQUARA - 4 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='JABAQUARA') & ((ape_dorm==4))))
# LAPA - 4 e 5  DORM
df3_ <- df3_ %>% filter(!((sp_nome=='LAPA') & ((ape_dorm==4) | (ape_dorm==5))))
# M'BOI MIRIM - 1, 3 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='M\'BOI MIRIM') & ((ape_dorm==1) | (ape_dorm==3))))
# MOOCA - 4, 5 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='MOOCA') & ((ape_dorm==4) | (ape_dorm==5))))
# PENHA - 4 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='PENHA') & (ape_dorm==4)))
# PINHEIROS - 5, 6, 7 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='PINHEIROS') & ((ape_dorm==5) | (ape_dorm==6) | (ape_dorm==7))))
# PIRITUBA-JARAGUA - 1, 3, 4 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='PIRITUBA-JARAGUA') & ((ape_dorm==1) | (ape_dorm==3) | (ape_dorm==4))))
# SANTANA-TUCURUVI - 4 e 5 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='SANTANA-TUCURUVI') & ((ape_dorm==4) | (ape_dorm==5))))
# SANTO AMARO - 5, 6 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='SANTO AMARO') & ((ape_dorm==5) | (ape_dorm==6))))
# SAPOPEMBA - 1, 3 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='SAPOPEMBA') & ((ape_dorm==1) | (ape_dorm==3))))
# SE - 4, 5, 6 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='SE') & ((ape_dorm==4) | (ape_dorm==5) | (ape_dorm==6))))
# VILA MARIANA - 5, 6 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='VILA MARIANA') & ((ape_dorm==5) | (ape_dorm==6))))
# VILA PRUDENTE - 4 DORM
df3_ <- df3_ %>% filter(!((sp_nome=='VILA PRUDENTE') & ((ape_dorm==4))))

#######
# 4.0 EDA
#######
df4 <- copy(df3_)
df4$zeu_npde <- df4$zeu_npde %>% as.factor
df4$cond_ano <- df4$cond_ano %>% as.factor()

str(df4)

# Análise univariada
## Variável resposta
ggplot(df4, aes(x=ape_preco)) + geom_histogram(bins=10)

## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Variáveis numéricas
nums <- dplyr::select_if(df4, is.numeric)
hist(nums$ape_precom2)
hist(nums$ape_area)

## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Variáveis categoricas


fact_ <- dplyr::select_if(df4, is.factor)

# DADOS APE
plot1 <- ggplot(fact_, aes(x=ape_dorm), Freq) + geom_bar()
plot2 <- ggplot(df4, aes(x=ape_preco, fill=ape_dorm, colour=ape_dorm)) + geom_density(alpha=0.1)
grid.arrange(plot1, plot2, ncol=2)

plot1 <- ggplot(fact_, aes(x=ape_suite), Freq) + geom_bar()
plot2 <- ggplot(df4, aes(x=ape_preco, fill=ape_suite, colour=ape_suite)) + geom_density(alpha=0.1)
grid.arrange(plot1, plot2, ncol=2)

ggplot(fact_, aes(x=ape_banheiro), Freq) + geom_bar()
ggplot(fact_, aes(x=ape_vaga), Freq) + geom_bar()
# DADOS CONDOMINIO
ggplot(fact_, aes(x=cond_ano), Freq) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #verificar condomininos com anos estranhos
ggplot(fact_, aes(x=cond_n_torres), Freq) + geom_bar() #condominios com mais de 10 torres? está estranho...
ggplot(fact_, aes(x=cond_elevador), Freq) + geom_bar()
# DADOS AREAS COMUNS
ggplot(fact_, aes(x=salao.de.festas), Freq) + geom_bar()
ggplot(fact_, aes(x=espaco.gourmet), Freq) + geom_bar()
ggplot(fact_, aes(x=academia), Freq) + geom_bar()
ggplot(fact_, aes(x=brinquedoteca), Freq) + geom_bar()
ggplot(fact_, aes(x=playground), Freq) + geom_bar()
ggplot(fact_, aes(x=churrasqueira), Freq) + geom_bar()
ggplot(fact_, aes(x=quadra), Freq) + geom_bar()
ggplot(fact_, aes(x=area.verde), Freq) + geom_bar()
ggplot(fact_, aes(x=piscina), Freq) + geom_bar()
ggplot(fact_, aes(x=espaco.zen), Freq) + geom_bar()
ggplot(fact_, aes(x=lavanderia), Freq) + geom_bar()
ggplot(fact_, aes(x=coworking), Freq) + geom_bar()
# DADOS ZEU
ggplot(fact_, aes(x=zeu), Freq) + geom_bar()

# Análise Bivariada
# --------- Gráfico de dispersão APE_AREA X APE_PRECO
ggplotly(
  df4 %>% 
    ggplot() +
    geom_point(aes(x = ape_area, y = ape_preco),
               color = "grey20", alpha = 0.6, size = 2) +
    labs(x = "Área do Apartamento",
         y = "Valor do apartamento") +
    theme_bw()
)


# --------- PREÇO MÉDIO POR SUBPREFEITURA - VALOR IMOVEL x ZEU
aux <- df4 %>% dplyr::select(ape_preco, sp_nome, zeu_npde) %>% group_by(zeu_npde, sp_nome) %>% summarise(media_sub = mean(ape_preco))
ggplot(aux, aes(x=media_sub, y=sp_nome, fill=zeu_npde, color=zeu_npde)) + geom_col(position = "dodge", width=0.5) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + ylab("Subprefeitura") + xlab("Preço Médio (R$)") + 
  scale_x_continuous(labels = scales::comma) + labs(title = "Preço do Apartamento (R$) x Distrito")



# >>> EM VALORES MÉDIOS, OS PREÇOS DO APARTAMENTO ABSOLUTO FORA DO ZEU ESTÁ MAIS CARO DO QUE AQUELE QUE ESTÁ

# --------- PREÇO MÉDIO POR SUBPREFEITURA - VALOR IMOVEL POR M2 x ZEU
aux <- df4 %>% dplyr::select(ape_precom2, sp_nome, zeu_npde) %>% group_by(zeu_npde, sp_nome) %>% summarise(media_sub = mean(ape_precom2))
ggplot(aux, aes(y=sp_nome, x=media_sub, fill=zeu_npde, color=zeu_npde)) + geom_col(position = "dodge", width=0.5) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))  + ylab("Subprefeitura") + xlab("Preço Médio (R$)") +
  labs(title = "Preço por m² (R$/m²) x Distrito")

# >>> ENTRETANTO, OS PREÇOS DO APARTAMENTO POR M2 INDICAM O OPOSTO. DESSA MANEIRA, HÁ APARTAMENTOS MAIORES FORA DO ZEU
# O QUE PROPICIA VALORES MAIS ALTOS.

# --------- PREÇO MÉDIO POR SUBPREFEITURA - AREA DO APARTAMENTO x ZEU
aux <- df4 %>% dplyr::select(ape_area, sp_nome, zeu_npde) %>% group_by(zeu_npde, sp_nome) %>% summarise(media_sub = mean(ape_area))
ggplot(aux, aes(x=media_sub, y=sp_nome, fill=zeu_npde, color=zeu_npde)) + geom_col(position = "dodge", width=0.5) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+ ylab("Subprefeitura") + xlab("Media (m²)") +
  labs(title = "Área dos apartamentos (m²) x Distrito")
# >>> PODEMOS CONFIRMAR ESTE PONTO AO ANALISAR A ÁREA MEDIA DO APARTAMENTO. EM GERAL, OS APARTAMENTOSO FORA DA ZEU
# SÃO MAIORES.

# --------- TABELA DORM X AREA
aux <- df4 %>% dplyr::select(ape_dorm, sp_nome, zeu_npde) %>% group_by(zeu_npde, sp_nome, ape_dorm) %>% summarise(qtd_ = n())
aux$zona_ape_dorm <- paste0(aux$sp_nome, "_",aux$ape_dorm)
aux1_cz <- aux %>% dplyr::filter(zeu_npde==1)
aux1_sz <- aux %>% dplyr::filter(zeu_npde==0)
aux1_f <- full_join(aux1_cz, aux1_sz, by=c('zona_ape_dorm'='zona_ape_dorm'))
aux_col <- c("zeu1", "sp_nome1", "ape_dorm1", "qtd_cz", "zona_ape_dorm", 
             "zeu0","sp_nome0", "ape_dorm0", "qtd_sz")
colnames(aux1_f) <- aux_col

aux1_f <- aux1_f[-c(1, 2, 3, 6, 7, 8)]
aux1_f <- aux1_f %>% dplyr::select("zona_ape_dorm","qtd_sz", "qtd_cz")

names(aux1_f)

# SUBDIVISÃO AREA POR FAIXA DE DORMITÓRIO
aux3 <- df4 %>% dplyr::select(ape_area, ape_dorm,sp_nome, zeu_npde) %>% group_by(ape_dorm, zeu_npde, sp_nome) %>% summarise(media = mean(ape_area))
aux3$sub_dorm <- paste0(aux3$sp_nome, "_",aux3$ape_dorm)
aux3_cz <- aux3 %>% filter(zeu_npde==1)
aux3_sz <- aux3 %>% filter(zeu_npde==0)
aux3_f <- full_join(aux3_cz, aux3_sz, by='sub_dorm')

rev_nome <- c('ape_dorm_', 'zeu1', 'sp_nome_', 'area_media_zeu', 'sub_dorm',
              'ape_dorm', 'zeu0', 'sp_nome', 'area_media')

colnames(aux3_f) <- rev_nome

aux3_f <- aux3_f %>% dplyr::select('sp_nome', 'ape_dorm', 'area_media', 'area_media_zeu', 'sub_dorm')

aux_area_dorm <- left_join(aux1_f, aux3_f, by=c('zona_ape_dorm'='sub_dorm'))
aux_area_dorm <- aux_area_dorm[-c(1)]
aux_area_dorm <- aux_area_dorm %>% 
  dplyr::select('sp_nome', 'ape_dorm', 'qtd_sz', 'area_media', 'qtd_cz', 'area_media_zeu')

sum(aux_area_dorm$qtd_sz)
sum(aux_area_dorm$qtd_cz)

aux_ad <- aux_area_dorm %>% dplyr::select(ape_dorm, area_media, area_media_zeu) %>%
  group_by(ape_dorm) %>% summarise(med_sz = mean(area_media),
                                   med_cz = mean(area_media_zeu)
                                  )
# gráfico comparativo - área média por dormitório -dividido ZEU/PDE
ggplot(aux_ad, aes(x=ape_dorm)) + 
  geom_line(aes(y = med_sz, group=1, color = "Zona ZEU - PDE anteriores"), size=1.5) + 
  geom_line(aes(y = med_cz, group=2, color="Zona ZEU - Novo PDE"), linetype="twodash", size=1.5) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  labs(title = "Dormitórios x Área Média dos apartamento - Diferenças ZEU") +
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  labs(colour="Legenda",x="Tipologias (Dorm)",y="Área Media (m²)") +
  scale_color_manual(values = c("red","steelblue"))

names(df4)

# verificar o preço ao longo dos anos - há muita disparidade nas informações tendo em vista a agraegação dos distritos.
#aux <- df4 %>% dplyr::select(ape_precom2, cond_ano, zeu_npde) %>%
#  group_by(cond_ano, zeu_npde) %>% summarise(p_medio_m2 = mean(ape_precom2))
#
#aux$cond_ano[1]
#levels(aux$cond_ano)[levels(aux$cond_ano)=='0'] <- "Em Obras"

#distinct(df4, cond_ano)
#df4$cond_ano <- as.integer(df4$cond_ano)
#xx <- df4 %>% filter(cond_ano < 1990)



# Análise Multivariada
data_num <- select_if(df4, is.numeric)
data_num <- data_num[-c(4,5)]

corrplot::corrplot(cor(data_num))
cors <- function(df) { 
  # turn all three matrices (r, n, and P into a data frame)
  M <- Hmisc::rcorr(as.matrix(df))
  # return the three data frames in a list return(Mdf)
  Mdf <- map(M, ~data.frame(.x))
}
formatted_cors <- function(df){
  cors(df) %>%
    map(~rownames_to_column(.x, var="measure1")) %>%
    map(~pivot_longer(.x, -measure1, "measure2")) %>% 
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    mutate(sig_p = ifelse(P < .05, T, F), p_if_sig = ifelse(P <.05, P, NA), r_if_sig = ifelse(P <.05, r, NA)) 
}

formatted_cors(data_num) %>% 
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Correlações\nPearson's", 
       title="Correlação Variáveis Numéricas") + 
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  geom_text() +
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(text=element_text(family="Roboto"))

chart.Correlation(data_num, histogram = TRUE)

data_char <- select_if(df4, (is.factor))
pedometrics::cramer(df4)


#######
# 5.0 DATA PREPARATION
#######
df5 <- copy(df4)

# removendo variáveis que não irão para o modelo:
df5_ <- df5[-c(8,9,11,12,28,29)]

names(df5)

summary(df5_)
# REESCALING
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
#df5_$ape_precom2 <- normalize(df5_$ape_precom2) Não aplicável variável correlacionada
df5_$ape_area <- normalize(df5_$ape_area)

# TRANSFORMATION
# TO NUMERICAL VALUES
df5_$ape_dorm <- as.numeric(levels(df5$ape_dorm))[df5$ape_dorm]
df5_$ape_suite <- as.numeric(levels(df5$ape_suite))[df5$ape_suite]
df5_$ape_banheiro <- as.numeric(levels(df5$ape_banheiro))[df5$ape_banheiro]
df5_$ape_vaga <- as.numeric(levels(df5$ape_vaga))[df5$ape_vaga]

# dados condomínio - One Hot Encoding
df5_$salao.de.festas <- as.numeric(levels(df5$salao.de.festas))[df5$salao.de.festas]
df5_$espaco.gourmet <- as.numeric(levels(df5$espaco.gourmet))[df5$espaco.gourmet]
df5_$salao.de.jogos <- as.numeric(levels(df5$salao.de.jogos))[df5$salao.de.jogos]
df5_$academia <- as.numeric(levels(df5$academia))[df5$academia]
df5_$brinquedoteca <- as.numeric(levels(df5$brinquedoteca))[df5$brinquedoteca]
df5_$playground <- as.numeric(levels(df5$playground))[df5$playground]
df5_$churrasqueira <- as.numeric(levels(df5$churrasqueira))[df5$churrasqueira]
df5_$quadra <- as.numeric(levels(df5$quadra))[df5$quadra]
df5_$espaco.zen <- as.numeric(levels(df5$espaco.zen))[df5$espaco.zen]
df5_$area.verde <- as.numeric(levels(df5$area.verde))[df5$area.verde]
df5_$piscina <- as.numeric(levels(df5$piscina))[df5$piscina]
df5_$lavanderia <- as.numeric(levels(df5$lavanderia))[df5$lavanderia]
df5_$coworking <- as.numeric(levels(df5$coworking))[df5$coworking]
df5_$zeu <- as.numeric(levels(df5$zeu))[df5$zeu]

df5_$sp_nome <- as.character(df5$sp_nome)
df5_$sp_nome <- to_snake_case(df5_$sp_nome)
df5_ <- dummy_columns(.data = df5_,
                                   select_columns = "sp_nome",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = F)

#df5_$cond_ano <- as.character(df5$cond_ano)
#df5_ <- dummy_columns(.data = df5_,
#                      select_columns = "cond_ano",
#                      remove_selected_columns = T,
#                      remove_most_frequent_dummy = F)
df5_ <- df5_[-c(2, 8)] #Excluindo "cond_ano" e "ape_area"

names(df5_)

# TARGET VARIABLE
##Para calcular o lambda de Box-Cox
#lambda_BC <- powerTransform(df5_$ape_preco) #função powerTransform do pacote car#
#lambda_BC
#
##Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
#df5_$ape_preco <- (((df5_$ape_preco ^ lambda_BC$lambda) - 1) / 
#                          lambda_BC$lambda)

df5_$ape_preco <- log(df5_$ape_preco)

hist(df5_$ape_preco)

#######
# 6.0 FEATURE SELECTION
#######
df6 <- copy(df5_)

# SPLIT TRAIN TEST SET
set.seed(45)
trainIndex <- createDataPartition(df6$ape_preco, p = .8, 
                                  list = FALSE, 
                                  times = 1)

X_Train <- df6[ trainIndex,]
X_Test  <- df6[-trainIndex,]

#######
# 7.0 MACHINE LEARNING MODELLING
#######
#Modelagem com todas as variáveis
modelo_linear <- lm(ape_preco ~ . , X_Train)
summary(modelo_linear)

tab_model(modelo_linear, digits=5)

#Shapiro-Francia: n > 30
# sf.test(modelo_linear$residuals) - Não é possível <50

summ(modelo_linear, confint = T, digits = 4, ci.width = .95)

#Histograma dos resíduos do modelo OLS linear
X_Train %>%
  mutate(residuos = modelo_linear$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_linear$residuals),
                            sd = sd(modelo_linear$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

# Separando o DF de teste da variável alvo
x_test <- X_Test[-c(1)]
y_test <- X_Test[c(1)]
y_test <- y_test$ape_preco

# convertendo para o valor original conforme valor de Box Cox
#y_test <- (((y_test * lambda_BC$lambda) + 1)) ^ (1 / lambda_BC$lambda)
# convertendo para o valor original conforme valor do logaritmo natural
y_test <- exp(y_test)

# Obtendo os valores para os dados de teste
yhat <- predict(object = modelo_linear, x_test)
yhat_ <- as.vector(yhat)
yhat_ <- exp(yhat_)

# ANálise dos erros do modelo
MAE(y_test, yhat)

MAPE(y_test, yhat)

RMSE(y_test, yhat)

R2(y_test, yhat)

# Analisando heteroestatiscidade do modelo
ols_test_breusch_pagan(modelo_linear)

# gráfico resíduos
test_modelo_lm <- copy(X_Test)
test_modelo_lm$pred <- yhat
test_modelo_lm$resd <- test_modelo_lm$ape_preco - test_modelo_lm$pred

ggplot(data = test_modelo_lm, aes(y = resd, x = ape_preco)) + geom_point(col = 'blue') + geom_abline(slope = 0)

#Plotando os resíduos
X_Train %>%
  mutate(residuos = modelo_linear$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()


#####################################
####                         CICLO 02
#####################################

##########
# Boruta as Feature Selector
#########
Boruta(ape_preco~., X_Train)->brt_ft

# Boruta Plot
plot(brt_ft, cex.axis=.7, las=2, xlab="", main="Variable Importance")

# Boruta Statistics
attStats(brt_ft)

# Selected Variables
brt_ft$finalDecision

##########
# STEPWISE as Feature Selector
##########
step_ape <- step(modelo_linear, k = 3.841459)

summary(step_ape)
tab_model(step_ape, digits=5)

ols_test_breusch_pagan(step_ape)

names(X_Train)

##########
# Random Forest -Variable Importance
##########
rf_mod <- randomForest(ape_preco ~ . , data=X_Train, ntree=100)
rf_mod

explained_rf <- explain(rf_mod, data=X_Train, y=X_Train$ape_preco)

varimps = DALEX::feature_importance(explained_rf, type='raw')

print(varimps)
plot(varimps)

explained_rf$residuals

rf_linear <- lm(formula_rf , X_Train)
summary(rf_linear)

ols_test_breusch_pagan(rf_linear)

#####################################
####                         CICLO 03 - Análise de diferentes modelos
#####################################
# https://machinelearningmastery.com/machine-learning-in-r-step-by-step/

formula_tudo = ape_preco ~ .
formula_step = ape_preco ~ . -zeu_npde -salao.de.jogos -brinquedoteca -sp_nome_campo_limpo -sp_nome_cidade_ademar
formula_rf = ape_preco ~ . -sp_nome_santo_amaro -lavanderia -zeu_npde -sp_nome_ipiranga -sp_nome_ipiranga -sp_nome_butanta -coworking -sp_nome_mooca -sp_nome_campo_limpo -sp_nome_santana_tucuruvi -sp_nome_vila_prudente -sp_nome_jabaquara -sp_nome_itaquera -sp_nome_pirituba_jaragua -sp_nome_cidade_ademar -sp_nome_m_boi_mirim -sp_nome_freguesia_brasilandia -sp_nome_capela_do_socorro -sp_nome_sapopemba

trctrl <- trainControl(method = "cv", number = 3, verboseIter = TRUE)

# 0. Linear Regression - Baseline
#########
# - Cenario Total / Boruta
lr_model <- train(formula_tudo, data = X_Train, method = "lm",
                   trControl=trctrl)

summary(lr_model)

yhat_lr <- predict(object = lr_model, x_test)
yhat_lr_ <- as.vector(yhat_lr)
yhat_lr_ <- exp(yhat_lr_)

# ANalise dos erros do modelo
MAE(y_test, yhat_lr_)
MAPE(y_test, yhat_lr_)
RMSE(y_test, yhat_lr_)
R2(y_test, yhat_lr_)

# - Stepwise - formula_step
lr_model <- train(formula_step, data = X_Train, method = "lm",
                  trControl=trctrl)

yhat_lr <- predict(object = lr_model, x_test)
yhat_lr_ <- as.vector(yhat_lr)
yhat_lr_ <- exp(yhat_lr_)

# ANalise dos erros do modelo
MAE(y_test, yhat_lr_)
MAPE(y_test, yhat_lr_)
RMSE(y_test, yhat_lr_)
R2(y_test, yhat_lr_)

# - RF - formula_rf
lr_model <- train(formula_rf, data = X_Train, method = "lm",
                  trControl=trctrl)

yhat_lr <- predict(object = lr_model, x_test)
yhat_lr_ <- as.vector(yhat_lr)
yhat_lr_ <- exp(yhat_lr_)

# ANalise dos erros do modelo
MAE(y_test, yhat_lr_)
MAPE(y_test, yhat_lr_)
RMSE(y_test, yhat_lr_)
R2(y_test, yhat_lr_)

# 1. KNN
#########
# - Cenario Total / Boruta
knn_model <- train(formula_tudo, data = X_Train, method = "knn",
                 trControl=trctrl)

yhat_knn <- predict(object = knn_model, x_test)
yhat_knn_ <- as.vector(yhat_knn)
yhat_knn_ <- exp(yhat_knn_)

# ANalise dos erros do modelo
MAE(y_test, yhat_knn_)
MAPE(y_test, yhat_knn_)
RMSE(y_test, yhat_knn_)
R2(y_test, yhat_knn_)

# - Stepwise
knn_model <- train(formula_step, data = X_Train, method = "knn",
                   trControl=trctrl)

yhat_knn <- predict(object = knn_model, x_test)
yhat_knn_ <- as.vector(yhat_knn)
yhat_knn_ <- exp(yhat_knn_)

# Analise dos erros do modelo
MAE(y_test, yhat_knn_)
MAPE(y_test, yhat_knn_)
RMSE(y_test, yhat_knn_)
R2(y_test, yhat_knn_)

# - RF
knn_model <- train(formula_rf, data = X_Train, method = "knn",
                   trControl=trctrl)

yhat_knn <- predict(object = knn_model, x_test)
yhat_knn_ <- as.vector(yhat_knn)
yhat_knn_ <- exp(yhat_knn_)

# Analise dos erros do modelo
MAE(y_test, yhat_knn_)
MAPE(y_test, yhat_knn_)
RMSE(y_test, yhat_knn_)
R2(y_test, yhat_knn_)

# Random Forest
#########
# - Cenario Total / Boruta
rf_mod <- train(formula_tudo, data = X_Train, method = "rf",
                   trControl=trctrl)

yhat_rf <- predict(object = rf_mod, x_test)
yhat_rf_ <- as.vector(yhat_rf)
yhat_rf_ <- exp(yhat_rf_)

# Analise dos erros do modelo
MAE(y_test, yhat_rf_)
MAPE(y_test, yhat_rf_)
RMSE(y_test, yhat_rf_)
R2(y_test, yhat_rf_)

# - Stepwise
rf_mod <- train(formula_step, data = X_Train, method = "rf",
                trControl=trctrl)

yhat_rf <- predict(object = rf_mod, x_test)
yhat_rf_ <- as.vector(yhat_rf)
yhat_rf_ <- exp(yhat_rf_)

# ANalise dos erros do modelo
MAE(y_test, yhat_rf_)
MAPE(y_test, yhat_rf_)
RMSE(y_test, yhat_rf_)
R2(y_test, yhat_rf_)

# - RF
rf_mod <- train(formula_rf, data = X_Train, method = "rf",
                trControl=trctrl)

yhat_rf <- predict(object = rf_mod, x_test)
yhat_rf_ <- as.vector(yhat_rf)
yhat_rf_ <- exp(yhat_rf_)

# Analise dos erros do modelo
MAE(y_test, yhat_rf_)
MAPE(y_test, yhat_rf_)
RMSE(y_test, yhat_rf_)
R2(y_test, yhat_rf_)

# XGBoost
#########
# - Cenario Total / Boruta
xgb_mod <- train(formula_tudo, data = X_Train, method = "xgbTree",
                trControl=trctrl)

yhat_xgb <- predict(object = xgb_mod, x_test)
yhat_xgb_ <- as.vector(yhat_xgb)
yhat_xgb_ <- exp(yhat_xgb_)

# Analise dos erros do modelo
MAE(y_test, yhat_xgb_)
MAPE(y_test, yhat_xgb_)
RMSE(y_test, yhat_xgb_)
R2(y_test, yhat_xgb_)

# - Stepwise
xgb_mod <- train(formula_step, data = X_Train, method = "xgbTree",
                 trControl=trctrl)

yhat_xgb <- predict(object = xgb_mod, x_test)
yhat_xgb_ <- as.vector(yhat_xgb)
yhat_xgb_ <- exp(yhat_xgb_)

# Analise dos erros do modelo
MAE(y_test, yhat_xgb_)
MAPE(y_test, yhat_xgb_)
RMSE(y_test, yhat_xgb_)
R2(y_test, yhat_xgb_)

# - RF
xgb_mod <- train(formula_rf, data = X_Train, method = "xgbTree",
                 trControl=trctrl)

yhat_xgb <- predict(object = xgb_mod, x_test)
yhat_xgb_ <- as.vector(yhat_xgb)
yhat_xgb_ <- exp(yhat_xgb_)

# Analise dos erros do modelo
MAE(y_test, yhat_xgb_)
MAPE(y_test, yhat_xgb_)
RMSE(y_test, yhat_xgb_)
R2(y_test, yhat_xgb_)

# SVM - Linear
#########
# - Cenario Total / Boruta
svm_mod <- train(formula_tudo, data = X_Train, method = "svmLinear",
                 trControl=trctrl)

yhat_svm <- predict(object = svm_mod, x_test)
yhat_svm_ <- as.vector(yhat_svm)
yhat_svm_ <- exp(yhat_svm_)

# Analise dos erros do modelo
MAE(y_test, yhat_svm_)
MAPE(y_test, yhat_svm_)
RMSE(y_test, yhat_svm_)
R2(y_test, yhat_svm_)

# - Stepwise
svm_mod <- train(formula_step, data = X_Train, method = "svmLinear",
                 trControl=trctrl)

yhat_svm <- predict(object = svm_mod, x_test)
yhat_svm_ <- as.vector(yhat_svm)
yhat_svm_ <- exp(yhat_svm_)

# Analise dos erros do modelo
MAE(y_test, yhat_svm_)
MAPE(y_test, yhat_svm_)
RMSE(y_test, yhat_svm_)
R2(y_test, yhat_svm_)

# - RF
svm_mod <- train(formula_rf, data = X_Train, method = "svmLinear",
                 trControl=trctrl)

yhat_svm <- predict(object = svm_mod, x_test)
yhat_svm_ <- as.vector(yhat_svm)
yhat_svm_ <- exp(yhat_svm_)

# Analise dos erros do modelo
MAE(y_test, yhat_svm_)
MAPE(y_test, yhat_svm_)
RMSE(y_test, yhat_svm_)
R2(y_test, yhat_svm_)

# SVM - Polynomial
#########
# - Cenario Total / Boruta
svm_mod <- train(formula_tudo, data = X_Train, method = "svmPoly",
                 trControl=trctrl)

yhat_svm <- predict(object = svm_mod, x_test)
yhat_svm_ <- as.vector(yhat_svm)
yhat_svm_ <- exp(yhat_svm_)

# Analise dos erros do modelo
MAE(y_test, yhat_svm_)
MAPE(y_test, yhat_svm_)
RMSE(y_test, yhat_svm_)
R2(y_test, yhat_svm_)

# - Stepwise
svm_mod <- train(formula_step, data = X_Train, method = "svmPoly",
                 trControl=trctrl)

yhat_svm <- predict(object = svm_mod, x_test)
yhat_svm_ <- as.vector(yhat_svm)
yhat_svm_ <- exp(yhat_svm_)

# Analise dos erros do modelo
MAE(y_test, yhat_svm_)
MAPE(y_test, yhat_svm_)
RMSE(y_test, yhat_svm_)
R2(y_test, yhat_svm_)

# - RF
svm_mod <- train(formula_rf, data = X_Train, method = "svmPoly",
                 trControl=trctrl)

yhat_svm <- predict(object = svm_mod, x_test)
yhat_svm_ <- as.vector(yhat_svm)
yhat_svm_ <- exp(yhat_svm_)

# Analise dos erros do modelo
MAE(y_test, yhat_svm_)
MAPE(y_test, yhat_svm_)
RMSE(y_test, yhat_svm_)
R2(y_test, yhat_svm_)


# NN - MLP Regression
#########
# - Cenario Total / Boruta
mlp_mod <- train(formula_tudo, data = X_Train, method = "mlp",
                 trControl=trctrl)

summary(mlp_mod)

yhat_mlp <- predict(object = mlp_mod, x_test)
yhat_mlp_ <- as.vector(yhat_mlp)
yhat_mlp_ <- exp(yhat_mlp_)

# Analise dos erros do modelo
MAE(y_test, yhat_mlp_)
MAPE(y_test, yhat_mlp_)
RMSE(y_test, yhat_mlp_)
R2(y_test, yhat_mlp_)

# - Stepwise - formula_step
mlp_mod <- train(formula_step, data = X_Train, method = "mlp",
                 trControl=trctrl)

yhat_mlp <- predict(object = mlp_mod, x_test)
yhat_mlp_ <- as.vector(yhat_mlp)
yhat_mlp_ <- exp(yhat_mlp_)

# Analise dos erros do modelo
MAE(y_test, yhat_mlp_)
MAPE(y_test, yhat_mlp_)
RMSE(y_test, yhat_mlp_)
R2(y_test, yhat_mlp_)

# - RF - formula_rf
mlp_mod <- train(formula_rf, data = X_Train, method = "mlp",
                 trControl=trctrl)

yhat_mlp <- predict(object = mlp_mod, x_test)
yhat_mlp_ <- as.vector(yhat_mlp)
yhat_mlp_ <- exp(yhat_mlp_)

# Analise dos erros do modelo
MAE(y_test, yhat_mlp_)
MAPE(y_test, yhat_mlp_)
RMSE(y_test, yhat_mlp_)
R2(y_test, yhat_mlp_)

# MODELO FINAL - XGBoost
#########
# - Cenario Total / Boruta
xgb_mod_final <- train(formula_tudo, data = X_Train, method = "xgbTree",
                 trControl=trctrl)

summary(xgb_mod_final)

yhat_xgb_final <- predict(object = xgb_mod_final, x_test)
yhat_xgb_f <- as.vector(yhat_xgb_final)
yhat_xgb_f <- exp(yhat_xgb_f)

# gráfico resíduos
test_modelo_xgb <- copy(X_Test)
test_modelo_xgb$pred <- yhat_xgb_f
test_modelo_xgb$ape_prec_conv <- exp(test_modelo_xgb$ape_preco)
test_modelo_xgb$resd <- test_modelo_xgb$ape_prec_conv - test_modelo_xgb$pred

p <- ggplot(data = test_modelo_xgb, aes(y = resd, x = ape_prec_conv)) + geom_point(col = 'blue') + geom_abline(slope = 0) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  labs(title = "Resíduos - Base de Teste") +
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  labs(colour="Legenda",x="Preço Apartamento",y="Resíduo") +
  scale_color_manual(values = c("red","steelblue"))

require(scales)
p + scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma)

# Analise dos erros do modelo
MAE(y_test, yhat_xgb_)
MAPE(y_test, yhat_xgb_)
RMSE(y_test, yhat_xgb_)
R2(y_test, yhat_xgb_)
