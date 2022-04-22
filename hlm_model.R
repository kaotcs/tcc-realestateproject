#######
# 0.0 IMPORTS
#######

pacotes <- c('superml', "plotly","tidyverse","reshape2","knitr","kableExtra","rgl","car",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


head(df4)

#######
# 5.0 DATA PREPARATION
#######
df5_hlm <- copy(df4)

# removendo variáveis que não irão para o modelo:
df5_hlm_ <- df5_hlm[-c(8,9,11,12,28,29)]

# REESCALING
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
#df5_$ape_precom2 <- normalize(df5_$ape_precom2) Não aplicável variável correlacionada
df5_hlm_$ape_area <- normalize(df5_hlm_$ape_area)

# TRANSFORMATION
# TO NUMERICAL VALUES
df5_hlm_$ape_dorm <- as.numeric(levels(df5_hlm_$ape_dorm))[df5_hlm_$ape_dorm]
df5_hlm_$ape_suite <- as.numeric(levels(df5_hlm_$ape_suite))[df5_hlm_$ape_suite]
df5_hlm_$ape_banheiro <- as.numeric(levels(df5_hlm_$ape_banheiro))[df5_hlm_$ape_banheiro]
df5_hlm_$ape_vaga <- as.numeric(levels(df5_hlm_$ape_vaga))[df5_hlm_$ape_vaga]

# dados condomínio - One Hot Encoding
df5_hlm_$salao.de.festas <- as.numeric(levels(df5_hlm_$salao.de.festas))[df5_hlm_$salao.de.festas]
df5_hlm_$espaco.gourmet <- as.numeric(levels(df5_hlm_$espaco.gourmet))[df5_hlm_$espaco.gourmet]
df5_hlm_$salao.de.jogos <- as.numeric(levels(df5_hlm_$salao.de.jogos))[df5_hlm_$salao.de.jogos]
df5_hlm_$academia <- as.numeric(levels(df5_hlm_$academia))[df5_hlm_$academia]
df5_hlm_$brinquedoteca <- as.numeric(levels(df5_hlm_$brinquedoteca))[df5_hlm_$brinquedoteca]
df5_hlm_$playground <- as.numeric(levels(df5_hlm_$playground))[df5_hlm_$playground]
df5_hlm_$churrasqueira <- as.numeric(levels(df5_hlm_$churrasqueira))[df5_hlm_$churrasqueira]
df5_hlm_$quadra <- as.numeric(levels(df5_hlm_$quadra))[df5_hlm_$quadra]
df5_hlm_$espaco.zen <- as.numeric(levels(df5_hlm_$espaco.zen))[df5_hlm_$espaco.zen]
df5_hlm_$area.verde <- as.numeric(levels(df5_hlm_$area.verde))[df5_hlm_$area.verde]
df5_hlm_$piscina <- as.numeric(levels(df5_hlm_$piscina))[df5_hlm_$piscina]
df5_hlm_$lavanderia <- as.numeric(levels(df5_hlm_$lavanderia))[df5_hlm_$lavanderia]
df5_hlm_$coworking <- as.numeric(levels(df5_hlm_$coworking))[df5_hlm_$coworking]
df5_hlm_$zeu <- as.numeric(levels(df5_hlm_$zeu))[df5_hlm_$zeu]

df5_hlm_$sp_nome <- as.character(df5_hlm_$sp_nome)
df5_hlm_$sp_nome <- to_snake_case(df5_hlm_$sp_nome)
#df5_hlm_ <- dummy_columns(.data = df5_hlm_,
#                      select_columns = "sp_nome",
#                      remove_selected_columns = T,
#                      remove_most_frequent_dummy = F)

label <- LabelEncoder$new()
label$fit(df5_hlm_$sp_nome)
df5_hlm_$sp_nome <- label$fit_transform(df5_hlm_$sp_nome)
#df5_hlm_$sp_nome <- label$inverse_transform(df5_hlm_$sp_nome)

df5_hlm_ <- df5_hlm_[-c(2, 8)] #Excluindo "cond_ano" e "ape_area"

names(df5_hlm_)

# TARGET VARIABLE
df5_$ape_preco <- log(df5_$ape_preco)

hist(df5_$ape_preco)

### FUNÇÂO AULA
stderr_nlme <- function(model){
  if(base::class(model) != "lme"){
    base::message("Use a lme object model from nlme package")
    stop()}
  resume <- base::summary(model)
  if(base::length(base::names(model$groups))==1){
    m.type <- "HLM2"
  } else if(base::length(base::names(model$groups))==2){
    m.type <- "HLM3"
  }
  if(m.type == "HLM2"){
    vcov_matrix <- model$apVar
    logs_sd_re <- base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re)==2){
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE Components`=base::c("Var(v0j)","Var(e)"),
                                  `Variance Estimatives`= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                                  base::exp(logs_sd_re[[2]])^2),
                                  `Std Err.`=base::c(stderr_tau00,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
    else{
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau01 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(v0j)","Var(v1j)","Var(e)"),
                                  Estimatives= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                       base::exp(logs_sd_re[[2]])^2,
                                                       base::exp(logs_sd_re[[4]])^2),
                                  Std_Err=base::c(stderr_tau00,
                                                  stderr_tau01,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                            base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                                                               base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
  if(m.type == "HLM3"){
    vcov_matrix <- model$apVar
    logs_sd_re <-  base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re) == 3){
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x3)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(t00k)","Var(v0jk)","Var(e)"),
                                  Estimatives=base::c(base::exp(logs_sd_re)[[2]]^2,
                                                      base::exp(logs_sd_re)[[1]]^2,
                                                      base::exp(logs_sd_re)[[3]]^2),
                                  Std_Err=base::c(stderr_tau_u000,
                                                  stderr_tau_r000,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    } 
    else{
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_r100 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u100 <- msm::deltamethod(~exp(x5)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x7)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE_Components`=base::c("Var(t00k)","Var(t10k)",
                                                          "Var(v0jk)","Var(v1jk)",
                                                          "Var(e)"),
                                  `Variance Estimatives`=base::c(base::exp(logs_sd_re)[[4]]^2,
                                                                 base::exp(logs_sd_re)[[5]]^2,
                                                                 base::exp(logs_sd_re)[[1]]^2,
                                                                 base::exp(logs_sd_re)[[2]]^2,
                                                                 base::exp(logs_sd_re)[[7]]^2),
                                  `Std Err.`=base::c(stderr_tau_u000,
                                                     stderr_tau_u100,
                                                     stderr_tau_r000,
                                                     stderr_tau_r100,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                            base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                                                               base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
}


################################################################################
#                              MODELO MULTINIVEL                               #
################################################################################


#Exploração visual do desempenho médio
df5_hlm_ %>%
  group_by(sp_nome) %>%
  mutate(preco_medio = mean(ape_preco, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point(aes(x = sp_nome, y = ape_preco),color = "orange", alpha = 0.5, size = 2) +
  geom_line(aes(x = sp_nome, y = preco_medio, 
                group = 1, color = "Preço Apartamento Médio"), size = 1.5) +
  scale_colour_viridis_d() +
  labs(x = "Distritos SP",
       y = "Preço Médio") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (ape_preco), com histograma
ggplotly(
  ggplot(df5_hlm_, aes(x = ape_preco)) +
    geom_density(aes(x = ape_preco), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 30) +
    theme_classic()
)

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho) por sp_nome
ggplotly(
  ggplot(df5_hlm_, aes(x = ape_preco)) +
    geom_density(aes(color = sp_nome, fill = sp_nome), 
                 position = "identity", alpha = 0.3) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_classic()
)

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (ape_preco), com histograma e por sp_nome separadamente
#(função facet_wrap)
#df5_hlm_ %>% 
#  group_by(sp_nome) %>% 
#  mutate(linhas = 1:n()) %>% 
#  mutate(x = unlist(density(ape_preco, n = max(linhas))["x"]),
#         y = unlist(density(ape_preco, n = max(linhas))["y"])) %>%
#  ggplot() +
#  geom_area(aes(x = x, y = y, group = sp_nome, fill = sp_nome), color = "black", alpha = 0.3) +
#  geom_histogram(aes(x = ape_preco, y = ..density.., fill = ape_preco), 
#                 color = "black", position = 'identity', alpha = 0.1) +
#  facet_wrap(~ ape_preco) +
#  scale_fill_viridis_d() +
#  scale_color_viridis_d() +
#  theme_bw()

#Gráfico de preço x área (OLS)
ggplotly(
  df5_hlm_ %>%
    ggplot(aes(x = ape_area, y = ape_preco)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    scale_colour_viridis_d() +
    labs(x = "Área do Apartamento",
         y = "Preço do Apartamento") +
    theme_bw()
)

#Gráfico de desempenho x horas por escola (visualização do contexto)
#NOTE QUE A PERSPECTIVA MULTINÍVEL NATURALMENTE CONSIDERA O COMPORTAMENTO
#HETEROCEDÁSTICO NOS DADOS!
ggplotly(
  df4 %>%
    ggplot(aes(x = ape_area, y = ape_preco, color = sp_nome)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    guides(color = F) +
    scale_colour_viridis_d() +
    labs(x = "Quantidade Semanal de Horas de Estudo do Aluno",
         y = "Desempenho Escolar") +
    theme_bw()
)

################################################################################
#                         ESTIMAÇÃO DO MODELO NULO HLM2                        #
################################################################################
names(df5_hlm_)
formula_geral <- ape_preco ~ ape_area + ape_suite + ape_banheiro + ape_vaga + salao.de.festas + espaco.gourmet +
  academia + salao.de.jogos + brinquedoteca + playground + churrasqueira + quadra + area.verde + piscina + espaco.zen +
  lavanderia + coworking + zeu + zeu_npde

#Estimação do modelo nulo (função lme do pacote nlme)
modelo_nulo_hlm2 <- lme(fixed = ape_preco ~ 1, 
                        random = ~ 1 | sp_nome,
                        data = df5_hlm_,
                        method = "REML")

#Parâmetros do modelo
summary(modelo_nulo_hlm2)

#Verificando a funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm2)


################################################################################
#                    COMPARAÇÃO DO HLM2 NULO COM UM OLS NULO                   #
################################################################################
#Para estimarmos o modelo OLS nulo, podemos comandar o seguinte
modelo_ols_nulo <- lm(formula = ape_preco ~ 1, 
                      data = df5_hlm_)

#Parâmetros do modelo OLS nulo
summary(modelo_ols_nulo)

#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_hlm2)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


################################################################################
#            ESTIMAÇÃO DO MODELO COM INTERCEPTOS ALEATÓRIOS HLM2               #
################################################################################

#Estimação do modelo com Interceptos Aleatórios
modelo_intercept_hlm2 <- lme(fixed = formula_geral,
                             random = ~ 1 | sp_nome,
                             data = df5_hlm_,
                             method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_hlm2)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())




################################################################################
#      ESTIMAÇÃO DO MODELO COM INTERCEPTOS E INCLINAÇÕES ALEATÓRIOS HLM2       #
################################################################################

#Estimação do modelo com Interceptos e Inclinações Aleatórios
modelo_intercept_inclin_hlm2 <- lme(fixed = formula_geral,
                                    random = ~ ape_dorm | sp_nome,
                                    data = df5_hlm_,
                                    method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_inclin_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_hlm2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

################################################################################
#      ESTIMAÇÃO DO MODELO COM INTERCEPTOS E INCLINAÇÕES ALEATÓRIOS HLM3       #
################################################################################

formula_zeu <- ape_preco ~ ape_area + ape_suite + ape_banheiro + ape_vaga + salao.de.festas + espaco.gourmet +
  academia + salao.de.jogos + brinquedoteca + playground + churrasqueira + quadra + area.verde + piscina + espaco.zen +
  lavanderia + coworking + zeu_npde


#Estimação do modelo com Interceptos e Inclinações Aleatórios
modelo_intercept_inclin_hlm3 <- lme(fixed = formula_zeu,
                                    random = ~ ape_dorm | sp_nome | zeu,
                                    data = df5_hlm_,
                                    method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_inclin_hlm3)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_hlm3)

logLik(modelo_intercept_inclin_hlm3)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


################################################################################
#                       ESTIMAÇÃO DO MODELO FINAL HLM2                         #
################################################################################

#Estimação do modelo final
modelo_final_hlm2 <- lme(fixed = desempenho ~ horas + texp + horas:texp,
                         random = ~ horas | escola,
                         data = estudante_escola,
                         method = "REML")

#Parâmetros do modelo
summary(modelo_final_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_final_hlm2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4,
         `HLM2 Modelo Final` = 5) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3",
                               "deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


#Melhor visualização dos interceptos e das inclinações aleatórios por escola,
#para o modelo final HLM2

v_final <- data.frame(modelo_final_hlm2[["coefficients"]][["random"]][["escola"]]) %>%
  rename(v00 = 1,
         v10 = 2)
v_final$escola <- c(1:10)
v_final$escola <- as.factor(v_final$escola)

v_final %>% 
  select(escola, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Para observarmos graficamente o comportamento dos valores de v0j, ou seja,
#dos interceptos aleatórios por escola, podemos comandar
random.effects(modelo_final_hlm2) %>% 
  rename(v0j = 1) %>% 
  rownames_to_column("Escola") %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(Escola) %>% 
  ggplot(aes(label = format(v0j, digits = 2), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Escola), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Escola, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Escola",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")


#Para observarmos graficamente o comportamento dos valores de v1j, ou seja
#das inclinações aleatórias por escola, podemos comandar
random.effects(modelo_final_hlm2) %>% 
  rename(v1j = 2) %>% 
  rownames_to_column("Escola") %>% 
  mutate(color_v1j = ifelse(v1j < 0, "A", "B"),
         hjust_v1j = ifelse(v1j > 0, 1.15, -0.15)) %>% 
  arrange(Escola) %>% 
  ggplot(aes(label = format(v1j, digits = 2), 
             hjust = hjust_v1j)) +
  geom_bar(aes(x = fct_rev(Escola), y = v1j, fill = color_v1j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Escola, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Escola",
       y = expression(nu[1][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

#Gerando os fitted values do modelo HLM2 Final
estudante_escola$hlm2_fitted <- predict(modelo_final_hlm2,
                                        estudante_escola)

# Visualizando os fitted values do modelo
#Visualizando os fitted values por estudante e por escola
predict(modelo_final_hlm2, level = 0:1) %>% 
  mutate(escola = gsub("^.*?\\/","",escola),
         escola = as.factor(as.numeric(escola)),
         desempenho = estudante_escola$desempenho,
         etjk = resid(modelo_final_hlm2)) %>% #função resid gera os termos etjk
  select(escola, desempenho, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

#Efetuando predições
#Exemplo: Quais os valores previstos de desempenho escolar, para dado
#aluno que estuda na escola "1", sabendo-se que ele estuda 11h semanais,
#e que a escola oferece tempo médio de experiência de seus professores
#igual a 3.6 anos?
predict(modelo_final_hlm2, level = 0:1,
        newdata = data.frame(escola = "1",
                             horas = 11,
                             texp = 3.6))

#Valores previstos do desempenho escolar em função da variável horas para o 
#modelo final HLM2 com interceptos e inclinações aleatórios
estudante_escola %>%
  mutate(fitted_escola = predict(modelo_final_hlm2, level = 1)) %>% 
  ggplot() +
  geom_point(aes(x = horas, y = fitted_escola)) +
  geom_smooth(aes(x = horas, y = fitted_escola, color = factor(escola)), 
              method = "lm", se = F) +
  scale_colour_viridis_d() +
  labs(x = "Quantidade Semanal de Horas de Estudo do Aluno",
       y = "Desempenho Escolar (Fitted Values)") +
  theme_bw()


################################################################################
#                       COMPARAÇÃO COM UM MODELO OLS                           #
################################################################################

#Elaborando um modelo OLS para fins de comparação
modelo_ols <- lm(formula = desempenho ~ horas + texp,
                 data = estudante_escola)

#Parâmetros
summary(modelo_ols)

#Comparando os LL dos modelos elaborados
data.frame(OLS = logLik(modelo_ols),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS` = 1,
         `HLM2 Modelo Final` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("darkorchid","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#LR Test
lrtest(modelo_ols, modelo_final_hlm2)

#Comparando a aderência dos fitted values dos modelos estimados
#Gerando os fitted values do modelo OLS
estudante_escola$ols_fitted <- modelo_ols$fitted.values

#Plotagem
estudante_escola %>%
  ggplot() +
  geom_smooth(aes(x = desempenho, y = ols_fitted, color = "OLS"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y= hlm2_fitted, color = "HLM2 Final"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  geom_point(aes(x = desempenho, y = ols_fitted,
                 color = "OLS")) +
  geom_point(aes(x = desempenho, y = hlm2_fitted,
                 color = "HLM2 Final"))  +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1","darkorchid")) +
  labs(x = "Desempenho", y = "Fitted Values") +
  theme_bw()


################################################################################
#                 COMPARAÇÃO COM UM MODELO OLS COM DUMMIES                     #
################################################################################

#Procedimento n-1 dummies para o contexto
estudante_escola_dummies <- dummy_cols(.data = estudante_escola,
                                       select_columns = "escola",
                                       remove_first_dummy = TRUE,
                                       remove_selected_columns = TRUE)

#Visualizando as dummies na nova base de dados 'estudante_escola_dummies'
estudante_escola_dummies %>%
  select(-hlm2_fitted,-ols_fitted, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

#Modelo OLS com dummies
modelo_ols_dummies <- lm(formula = desempenho ~ horas + texp + escola_2 +
                           escola_3 + escola_4 + escola_5 + escola_6 +
                           escola_7 + escola_8 + escola_9 + escola_10,
                         data = estudante_escola_dummies)

#Parâmetros
summary(modelo_ols_dummies)

#Procedimento stepwise
modelo_ols_dummies_step <- step(object = modelo_ols_dummies,
                                step = qchisq(p = 0.05, df = 1,
                                              lower.tail = FALSE))

#Parâmetros do modelo OLS estimado com dummies por escola a partir do
#procedimento Stepwise
summary(modelo_ols_dummies_step)

#Comparando os LL dos modelos HLM2 Final, OLs e OLS com Dummies e Stepwise
data.frame(OLS = logLik(modelo_ols),
           OLS_Dummies_Step = logLik(modelo_ols_dummies_step),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS` = 1,
         `OLS com Dummies e Stepwise` = 2,
         `HLM2 Modelo Final` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("darkorchid","maroon1","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#LR Test
lrtest(modelo_ols_dummies_step, modelo_final_hlm2)

#Comparação entre os parãmetros dos modelos (atente-se para a quantidade de
#parâmetros estimados em cada um deles!)
export_summs(modelo_ols_dummies_step, modelo_final_hlm2,
             model.names = c("OLS com Dummies", "HLM2 Final"))


#Comparando a aderência dos fitted values dos modelos HLM2 Final, OLS e
#OLS com Dummies e Stepwise
#Gerando os fitted values do modelo OLS com Dummies e Stepwise
estudante_escola$ols_step_fitted <- modelo_ols_dummies_step$fitted.values

#Gráfico para a comparação entre os fitted values dos modelos HLM2 Final, OLs e
#OLS com Dummies e Procedimento Stepwise
estudante_escola %>%
  ggplot() +
  geom_smooth(aes(x = desempenho, y = ols_step_fitted, color = "OLS com Dummies"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y= hlm2_fitted, color = "HLM2 Final"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y= ols_fitted, color = "OLS"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1", "maroon1", "darkorchid")) +
  labs(x = "Desempenho", y = "Fitted Values") +
  theme_bw()


#Comparação entre os LLs de todos os modelos estimados neste exemplo
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           OLS = logLik(modelo_ols),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           OLS_Dummies_step = logLik(modelo_ols_dummies_step),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `OLS` = 3,
         `HLM2 com Interceptos Aleatórios` = 4,
         `OLS com Dummies e Stepwise` = 5,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 6,
         `HLM2 Modelo Final` = 7) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 5) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","darkorchid","bisque4",
                               "maroon1","bisque3","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


##################################### FIM ######################################