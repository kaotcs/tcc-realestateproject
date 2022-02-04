#######
# 0.0 IMPORTS
#######

pacotes <- c('tidyverse', 'data.table', 'gridExtra')

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
df_raw_loft <- read.csv("data/loft_final.csv")
#ajustando campo de precos em str
df_raw_loft$ape_preco <- df_raw_loft$ape_preco %>% str_replace_all("\\D","") %>% as.numeric()

# DF DB
df_raw_gi <- read.csv("data/gi_final.csv")
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
colSums(is.na(df1))

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
df1$cond_ano <- df1$cond_ano %>% as.factor()
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
df1$espaco.mulher <- df1$espaco.mulher %>% as.factor()
df1$salao.de.festas.infantil <- df1$salao.de.festas.infantil %>% as.factor()

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
# próximo ciclo

#######
# 3.0 FEATURE FILTERING
#######
df3 <- copy(df2)

# Colunas
# - remover salao.de.festas.infantil / espaco.mulher - removidos do projeto
df3 <- df3[-c(26,27)]


#######
# 4.0 EDA
#######
df4 <- copy(df3)

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

# --------- PREÇO MÉDIO POR SUBPREFEITURA - VALOR IMOVEL x ZEU
aux <- df4 %>% dplyr::select(ape_preco, sp_nome, zeu) %>% group_by(zeu, sp_nome) %>% summarise(media_sub = mean(ape_preco))
ggplot(aux, aes(x=sp_nome, y=media_sub, fill=zeu, color=zeu)) + geom_col(position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# >>> EM VALORES MÉDIOS, OS PREÇOS DO APARTAMENTO ABSOLUTO FORA DO ZEU ESTÁ MAIS CARO DO QUE AQUELE QUE ESTÁ

# --------- PREÇO MÉDIO POR SUBPREFEITURA - VALOR IMOVEL POR M2 x ZEU
aux <- df4 %>% dplyr::select(ape_precom2, sp_nome, zeu) %>% group_by(zeu, sp_nome) %>% summarise(media_sub = mean(ape_precom2))
ggplot(aux, aes(x=sp_nome, y=media_sub, fill=zeu, color=zeu)) + geom_col(position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# >>> ENTRETANTO, OS PREÇOS DO APARTAMENTO POR M2 INDICAM O OPOSTO. DESSA MANEIRA, HÁ APARTAMENTOS MAIORES FORA DO ZEU
# O QUE PROPICIA VALORES MAIS ALTOS.

# --------- PREÇO MÉDIO POR SUBPREFEITURA - AREA DO APARTAMENTO x ZEU
aux <- df4 %>% dplyr::select(ape_area, sp_nome, zeu) %>% group_by(zeu, sp_nome) %>% summarise(media_sub = mean(ape_area))
ggplot(aux, aes(x=sp_nome, y=media_sub, fill=zeu, color=zeu)) + geom_col(position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# >>> PODEMOS CONFIRMAR ESTE PONTO AO ANALISAR A ÁREA MEDIA DO APARTAMENTO. EM GERAL, OS APARTAMENTOSO FORA DA ZEU
# SÃO MAIORES.

# --------- PREÇO MÉDIO POR SUBPREFEITURA - QTD DE DORMITORIOS x ZEU
aux <- df4 %>% dplyr::select(ape_dorm, sp_nome, zeu) %>% group_by(zeu, sp_nome, ape_dorm) %>% summarise(qtd_ = n())
aux$zona_ape_dorm <- paste0(aux$sp_nome, "_",aux$ape_dorm)
aux2 <- aux %>% dplyr::filter(zeu==1)
aux2$zona_ape_dorm <- paste0(aux2$sp_nome, "_",aux2$ape_dorm)

aux <- left_join(aux, aux2, by=c('zona_ape_dorm'='zona_ape_dorm'))
aux <- aux[-c(1, 5, 6, 7, 8)]

aux_col <- c("sub_pref","ape_dorm","qtd_n_zeu","qtd_c_zeu")
colnames(aux) <- aux_col
aux[is.na(aux)] <- 0

names(aux)
# CRIANDO TABELA PARA VISUALIZAR
aux %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)


names(df4)

#######
# 5.0 DATA PREPARATION
#######

#######
# 6.0 FEATURE SELECTION
#######

#######
# 7.0 MACHINE LEARNING MODELLING
#######
