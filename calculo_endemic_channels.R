library(tidyr)
library(dplyr)
library(tidyverse)
library(vroom)
library(mem)
library(readxl)
library(zoo)
library(Hmisc)
library(geobr)
require(geofacet)

path = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv"
dados <- vroom(file = path)


#tbla<-read_csv("MEM_VSR/dados/br_bd_diretorios_brasil_municipio.csv")

source(file = "pthreshold.functions.r")


VSR <- dados %>% 
  filter(fx_etaria == "Total", epiyear %in% c(2019, 2022, 2023, 2024)) %>%
  select(epiweek, epiyear, SG_UF_NOT, VSR, fx_etaria) %>%
  filter(SG_UF_NOT!=99)

dados.aux <- VSR %>%
  group_by(SG_UF_NOT) %>%
  group_map(~ data.prep(cases = .x$VSR,        
                        week = .x$epiweek,     
                        Year = .x$epiyear,
                        Pop = 1e5),    
            .keep = TRUE) %>%
  setNames(unique(VSR$SG_UF_NOT)) %>%
  list_rbind(names_to = 'SG_UF_NOT')

# Aplicar modelo
out <- dados.aux %>%
  group_by(SG_UF_NOT) %>%
  group_map(~ pthreshold.inla(data = .x, 
                              Year2forecast = 2025,
                              cyclic = TRUE,
                              MC=TRUE),
            .keep = TRUE) %>%
  setNames(unique(dados.aux$SG_UF_NOT)) %>%
  list_rbind(names_to = 'SG_UF_NOT')


####Summarise por DF

bands.vsr.uf <- out %>%
  filter(SG_UF_NOT!=0)%>%
  mutate(Semana.epidemiológica=week) %>%
  group_by(SG_UF_NOT, samples, Semana.epidemiológica) %>%
  summarise(values=sum(values, na.rm=T)) %>%
  ungroup()%>%
  group_by(SG_UF_NOT, Semana.epidemiológica) %>%
  summarise(`0.25quant` = quantile(values, probs = 0.25),
            `0.5quant` = quantile(values, probs = 0.5),
            `0.75quant` = quantile(values, probs = 0.75),
            `0.9quant` = quantile(values, probs = 0.9))

###Summarie por regiao

bands.vsr.regiao <- out %>%
  filter(SG_UF_NOT!=0)%>%
  mutate(regioes= floor(as.integer(SG_UF_NOT)/10),
         Semana.epidemiológica=week) %>%
  group_by(regioes, samples, Semana.epidemiológica) %>%
  summarise(values=sum(values, na.rm=T)) %>%
  ungroup()%>%
  group_by(regioes, Semana.epidemiológica) %>%
  summarise(`0.25quant` = quantile(values, probs = 0.25),
            `0.5quant` = quantile(values, probs = 0.5),
            `0.75quant` = quantile(values, probs = 0.75),
            `0.9quant` = quantile(values, probs = 0.9))%>%
  mutate(regioes=factor(regioes,
                        levels=c(1,2,5,3,4),
                        labels=c('Norte', 'Nordeste',
                                 'Centro-oeste', 'Sudeste',
                                 'Sul')))

####Salvando

bands.vsr<-out

save(bands.vsr.uf, file = "output/bands_vsr_uf.RData")
save(bands.vsr.regiao, file = "output/bands_vsr_regiao.RData")
save(bands.vsr, file = "output/bands_vsr.RData")


###FLUA A

FLU <- dados %>% 
  filter(fx_etaria == "Total", (epiyear>=2016 & epiyear<=2019) | epiyear==2023 | epiyear==2024) %>%
  select(epiweek, epiyear, SG_UF_NOT, FLU_A, fx_etaria) %>%
  filter(SG_UF_NOT!=99)

dados.aux <- FLU %>%
  group_by(SG_UF_NOT) %>%
  group_map(~ data.prep(cases = .x$FLU_A,        
                        week = .x$epiweek,     
                        Year = .x$epiyear,
                        Pop = 1e5),    
            .keep = TRUE) %>%
  setNames(unique(FLU$SG_UF_NOT)) %>%
  list_rbind(names_to = 'SG_UF_NOT')

# Aplicar modelo
out <- dados.aux %>%
  group_by(SG_UF_NOT) %>%
  group_map(~ pthreshold.inla(data = .x, 
                              Year2forecast = 2025,
                              cyclic = TRUE,
                              MC=TRUE),
            .keep = TRUE) %>%
  setNames(unique(dados.aux$SG_UF_NOT)) %>%
  list_rbind(names_to = 'SG_UF_NOT')


####Summarise por DF

bands.flu.uf <- out %>%
  filter(SG_UF_NOT!=0)%>%
  mutate(Semana.epidemiológica=week) %>%
  group_by(SG_UF_NOT, samples, Semana.epidemiológica) %>%
  summarise(values=sum(values, na.rm=T)) %>%
  ungroup()%>%
  group_by(SG_UF_NOT, Semana.epidemiológica) %>%
  summarise(`0.25quant` = quantile(values, probs = 0.25),
            `0.5quant` = quantile(values, probs = 0.5),
            `0.75quant` = quantile(values, probs = 0.75),
            `0.9quant` = quantile(values, probs = 0.9))

###Summarie por regiao

bands.flu.regiao <- out %>%
  filter(SG_UF_NOT!=0)%>%
  mutate(regioes= floor(as.integer(SG_UF_NOT)/10),
         Semana.epidemiológica=week) %>%
  group_by(regioes, samples, Semana.epidemiológica) %>%
  summarise(values=sum(values, na.rm=T)) %>%
  ungroup()%>%
  group_by(regioes, Semana.epidemiológica) %>%
  summarise(`0.25quant` = quantile(values, probs = 0.25),
            `0.5quant` = quantile(values, probs = 0.5),
            `0.75quant` = quantile(values, probs = 0.75),
            `0.9quant` = quantile(values, probs = 0.9))%>%
  mutate(regioes=factor(regioes,
                        levels=c(1,2,5,3,4),
                        labels=c('Norte', 'Nordeste',
                                 'Centro-oeste', 'Sudeste',
                                 'Sul')))

####Salvando

bands.flu<-out

save(bands.flu.uf, file = "output/bands_flu_uf.RData")
save(bands.flu.regiao, file = "output/bands_flu_regiao.RData")
save(bands.flu, file = "output/bands_flu.RData")
