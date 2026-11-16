library(zoo)
library(ggtext)
library(ggplot2)
library(rio)
library(tidyverse)
library(dplyr)
library(geobr)
library(sf)
library(ggthemes)
library(gridExtra)
library(magick)
library(geomtextpath)
library(geofacet)
library(patchwork)

info.logo <- image_read("plots/infogripe.png")

source("theme.publication.R")

load("output/bands_vsr_uf.RData")
load("output/bands_flu_uf.RData")
load("output/trajetorias_srag_por_virus_uf_sem_filtro_febre.RData")
load("output/bands_vsr_regiao.RData")
load("output/bands_flu_regiao.RData")

###Dados da pop já são baixados no script de tratamento de dados
pop <- import("https://gitlab.fiocruz.br/lsbastos/infogripe_code/-/raw/main/MEM_SRAG/populacao/projecoes_2024_tab1_idade_simples.xlsx", skip = 5)


pop_br<- pop %>% filter (!LOCAL %in% c("Norte", "Nordeste","Centro-Oeste","Sudeste","Sul", "Brasil")) %>%
  filter(SEXO=="Ambos") %>%
  select(SIGLA, CÓD., IDADE, '2023', '2024', '2025') %>%
  group_by(SIGLA, CÓD.)  %>%
  summarise(across('2023':'2025',\(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer('2023':'2025',names_to = "epiyear", values_to = "pop") %>%
  mutate(epiyear=as.double(epiyear)) %>%
  rename(DS_UF_SIGLA=SIGLA,
         SG_UF_NOT=CÓD.,
         Ano.epidemiológico=epiyear) 
##Arrumando os dados

#####Dados por UF

bands.flu.uf$virus<-"FLU_A"

canal_banda_uf<- bands.vsr.uf %>% 
  mutate(virus="VSR") %>%
  bind_rows(bands.flu.uf)%>%
  mutate(SG_UF_NOT=as.integer(SG_UF_NOT))%>%
  mutate(Ano.epidemiológico=2025)%>%
  left_join(pop_br, by=c("SG_UF_NOT", "Ano.epidemiológico")) %>%
  mutate(`0.25quant`=`0.25quant`*100000/pop,
         `0.75quant`=`0.75quant`*100000/pop,
         `0.9quant`=`0.9quant`*100000/pop,
         `0.5quant`=`0.5quant`*100000/pop) 

##################################################################
################# Incidência por regiao ###############################
##################################################################


pop_regi<- pop %>% filter (LOCAL %in% c("Norte", "Nordeste","Centro-Oeste","Sudeste","Sul")) %>%
  filter(SEXO=="Ambos") %>%
  select(LOCAL, IDADE, '2023', '2024', '2025') %>%
  group_by(LOCAL)  %>%
  summarise(across('2023':'2025',\(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer('2023':'2025',names_to = "epiyear", values_to = "pop") %>%
  mutate(epiyear=as.double(epiyear)) %>%
  rename(regioes=LOCAL,
         Ano.epidemiológico=epiyear) 


bands.flu.regiao$virus<-"FLU_A"
bands.flu.regiao$regioes <- str_replace(bands.flu.regiao$regioes, "Centro-oeste", "Centro-Oeste")
bands.vsr.regiao$regioes <- str_replace(bands.vsr.regiao$regioes, "Centro-oeste", "Centro-Oeste")

canal_banda_regi<- bands.vsr.regiao %>% 
  mutate(virus="VSR") %>%
  bind_rows(bands.flu.regiao)%>%
  mutate(Ano.epidemiológico=2025)%>%
  left_join(pop_regi, by=c("regioes", "Ano.epidemiológico")) %>%
  mutate(`0.25quant`=`0.25quant`*100000/pop,
         `0.75quant`=`0.75quant`*100000/pop,
         `0.9quant`=`0.9quant`*100000/pop,
         `0.5quant`=`0.5quant`*100000/pop) 


###Plotando

data<-canal_banda_regi %>%
  filter(virus=="FLU_A")


d1<-canal_banda_uf %>%
  filter(virus=="VSR") 

png(paste0('plots/canal_endemico_vsr.png'),height = 15, width = 12, units = 'in', res = 300)

ggplot(d1) +
  #scale_x_continuous(breaks = c(1, seq(9, 53, by = 8))) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = 0, ymax = `0.25quant`), 
              fill = 'yellow', alpha = 0.15) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = `0.25quant`, ymax = `0.75quant`), 
              fill = 'orange', alpha = 0.65) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = `0.75quant`, ymax = `0.9quant`), 
              fill = 'red', alpha = 0.35) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = `0.9quant`, ymax = Inf), 
              fill = 'darkred', alpha = 0.35) +
  
  geom_line(aes(x = Semana.epidemiológica, y = `0.5quant`, linetype = "Mediana"), color = "black") +
  geom_line(aes(x = Semana.epidemiológica, y = `0.25quant`, linetype = "P25"), color = "black") +
  geom_line(aes(x = Semana.epidemiológica, y = `0.75quant`, linetype = "P75"), color = "black") +
  geom_line(aes(x = Semana.epidemiológica, y = `0.9quant`, linetype = "P90"), color = "black") +
  scale_color_manual(values = colorblind_pal()(6)[c(4, 6, 1)]) +
  scale_linetype_manual(breaks = c('Mediana', 'P25', 'P75', 'P90'),
                        values = c(1, 2, 2, 3)) +
  # scale_x_continuous(breaks = c(1, seq(8, 52, 8)), minor_breaks = waiver()) +
  scale_x_continuous(breaks = seq(1, 52, by=9))+
  theme_Publication(base_size = 24) + 
  theme(panel.grid.minor.x = element_line(colour = "#f0f0f0", linetype = 2),
        plot.title = element_text(face = "bold", size = 18, hjust = 0),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.line = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        legend.text = element_text(size=16),
        legend.title = element_blank(),
        axis.text = element_text(size=11),
        axis.title = element_text(size=16),
        strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
        strip.text = element_text(face = "bold", size=15)
  ) +
  labs(
    x = "Semana de início de sintomas",
    y = paste0(y = "Incidência de SRAG por VSR (por 100 mil hab.)"),
    linetype = "Percentis da previsão",
    color = "Ano epidemiológico",
    fill = "Intervalos"
  ) +
  facet_geo(~DS_UF_SIGLA, grid='br_states_grid1', scale='free_y') +
  ggtitle("Anos de referência: 2019, 2022-2024")
grid::grid.raster(info.logo, x = 0.98, y = 1, just = c('right', 'top'), width = unit(1.2, 'inches'))
dev.off() 


###Canal endêmico FLU

d2<-canal_banda_uf %>%
  filter(virus=="FLU_A")  

png(paste0('plots/canal_endemico_flu.png'),height = 15, width = 12, units = 'in', res = 300)

ggplot(d2) +
  #scale_x_continuous(breaks = c(1, seq(9, 53, by = 8))) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = 0, ymax = `0.25quant`), 
              fill = 'yellow', alpha = 0.15) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = `0.25quant`, ymax = `0.75quant`), 
              fill = 'orange', alpha = 0.65) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = `0.75quant`, ymax = `0.9quant`), 
              fill = 'red', alpha = 0.35) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = `0.9quant`, ymax = Inf), 
              fill = 'darkred', alpha = 0.35) +
  
  geom_line(aes(x = Semana.epidemiológica, y = `0.5quant`, linetype = "Mediana"), color = "black") +
  geom_line(aes(x = Semana.epidemiológica, y = `0.25quant`, linetype = "P25"), color = "black") +
  geom_line(aes(x = Semana.epidemiológica, y = `0.75quant`, linetype = "P75"), color = "black") +
  geom_line(aes(x = Semana.epidemiológica, y = `0.9quant`, linetype = "P90"), color = "black") +
  scale_color_manual(values = colorblind_pal()(6)[c(4, 6, 1)]) +
  scale_linetype_manual(breaks = c('Mediana', 'P25', 'P75', 'P90'),
                        values = c(1, 2, 2, 3)) +
  # scale_x_continuous(breaks = c(1, seq(8, 52, 8)), minor_breaks = waiver()) +
  scale_x_continuous(breaks = seq(1, 52, by=9))+
  theme_Publication(base_size = 24) + 
  theme(panel.grid.minor.x = element_line(colour = "#f0f0f0", linetype = 2),
        plot.title = element_text(face = "bold", size = 18, hjust = 0),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.line = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        legend.text = element_text(size=16),
        legend.title = element_blank(),
        axis.text = element_text(size=11),
        axis.title = element_text(size=16),
        strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
        strip.text = element_text(face = "bold", size=15)
  ) +
  labs(
    x = "Semana de início de sintomas",
    y = paste0(y = "Incidência de SRAG por FLU A (por 100 mil hab.)"),
    linetype = "Percentis da previsão",
    color = "Ano epidemiológico",
    fill = "Intervalos"
  ) +
  facet_geo(~DS_UF_SIGLA, grid='br_states_grid1', scale='free_y') +
  ggtitle("Anos de referência: 2016-2019 e 2023-2024")
grid::grid.raster(info.logo, x = 0.98, y = 1, just = c('right', 'top'), width = unit(1.2, 'inches'))
dev.off() 


#####Canais endêmicos por regiao

canal_banda_regi$regioes <- factor(canal_banda_regi$regioes, 
                                   levels = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"))

png(paste0('plots/Regiao_fluA.png'),height = 12, width = 14, units = 'in', res = 300)
canal_banda_regi %>%
  filter(virus=="FLU_A") %>%
  ggplot( ) +
  #scale_x_continuous(breaks = c(1, seq(9, 53, by = 8))) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = 0, ymax = `0.25quant`), 
              fill = 'yellow', alpha = 0.15) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = `0.25quant`, ymax = `0.75quant`), 
              fill = 'orange', alpha = 0.65) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = `0.75quant`, ymax = `0.9quant`), 
              fill = 'red', alpha = 0.35) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = `0.9quant`, ymax = Inf), 
              fill = 'darkred', alpha = 0.35) +
  
  geom_line(aes(x = Semana.epidemiológica, y = `0.5quant`, linetype = "Mediana"), color = "black") +
  geom_line(aes(x = Semana.epidemiológica, y = `0.25quant`, linetype = "P25"), color = "black") +
  geom_line(aes(x = Semana.epidemiológica, y = `0.75quant`, linetype = "P75"), color = "black") +
  geom_line(aes(x = Semana.epidemiológica, y = `0.9quant`, linetype = "P90"), color = "black") +
  scale_color_manual(values = colorblind_pal()(6)[c(4, 6, 1)]) +
  scale_linetype_manual(breaks = c('Mediana', 'P25', 'P75', 'P90'),
                        values = c(1, 2, 2, 3)) +
  # scale_x_continuous(breaks = c(1, seq(8, 52, 8)), minor_breaks = waiver()) +
  scale_x_continuous(breaks = seq(1, 52, by=9))+
  theme_Publication(base_size = 24) + 
  theme(panel.grid.minor.x = element_line(colour = "#f0f0f0", linetype = 2),
        plot.title = element_text(face = "bold", size = 18, hjust = 0),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.line = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        legend.text = element_text(size=16),
        legend.title = element_blank(),
        axis.text = element_text(size=11),
        axis.title = element_text(size=16),
        strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
        strip.text = element_text(face = "bold", size=15)
  ) +
  labs(
    x = "Semana de início de sintomas",
    y = paste0(y = "Incidência de SRAG por FLU A (por 100 mil hab.)"),
    linetype = "Percentis da previsão",
    color = "Ano epidemiológico",
    fill = "Intervalos"
  ) +
  facet_wrap(~regioes, scale="free", ncol = 2)+
  ggtitle("Anos de referência: 2016-2019 e 2023-2024")  
grid::grid.raster(info.logo, x = 0.98, y = 1, just = c('right', 'top'), width = unit(1, 'inches'))
dev.off()


png(paste0('plots/Regiao_VSR.png'),height = 12, width = 14, units = 'in', res = 300)
canal_banda_regi %>%
  filter(virus=="VSR") %>%
 ggplot()+
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = 0, ymax = `0.25quant`), 
              fill = 'yellow', alpha = 0.15) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = `0.25quant`, ymax = `0.75quant`), 
              fill = 'orange', alpha = 0.65) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = `0.75quant`, ymax = `0.9quant`), 
              fill = 'red', alpha = 0.35) +
  geom_ribbon(aes(x = Semana.epidemiológica, ymin = `0.9quant`, ymax = Inf), 
              fill = 'darkred', alpha = 0.35) +
  
  geom_line(aes(x = Semana.epidemiológica, y = `0.5quant`, linetype = "Mediana"), color = "black") +
  geom_line(aes(x = Semana.epidemiológica, y = `0.25quant`, linetype = "P25"), color = "black") +
  geom_line(aes(x = Semana.epidemiológica, y = `0.75quant`, linetype = "P75"), color = "black") +
  geom_line(aes(x = Semana.epidemiológica, y = `0.9quant`, linetype = "P90"), color = "black") +
  scale_color_manual(values = colorblind_pal()(6)[c(4, 6, 1)]) +
  scale_linetype_manual(breaks = c('Mediana', 'P25', 'P75', 'P90'),
                        values = c(1, 2, 2, 3)) +
  # scale_x_continuous(breaks = c(1, seq(8, 52, 8)), minor_breaks = waiver()) +
  scale_x_continuous(breaks = seq(1, 52, by=9))+
  theme_Publication(base_size = 24) + 
  theme(panel.grid.minor.x = element_line(colour = "#f0f0f0", linetype = 2),
        plot.title = element_text(face = "bold", size = 18, hjust = 0),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.line = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        legend.text = element_text(size=16),
        legend.title = element_blank(),
        axis.text = element_text(size=11),
        axis.title = element_text(size=16),
        strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
        strip.text = element_text(face = "bold", size=15)
  ) +
  labs(
    x = "Semana de início de sintomas",
    y = paste0(y = "Incidência de SRAG por VSR (por 100 mil hab.)"),
    linetype = "Percentis da previsão",
    color = "Ano epidemiológico",
    fill = "Intervalos"
  ) +
  facet_wrap(~regioes, scale="free", ncol = 2)+
  ggtitle("Anos de referência: 2019, 2022-2024")
grid::grid.raster(info.logo, x = 0.98, y = 1, just = c('right', 'top'), width = unit(1, 'inches'))
dev.off()

####Explorando dados_flu


path = "https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv"
dados <- vroom(file = path)

df<- dados %>%
  filter (fx_etaria=="Total", epiyear >=2014) %>%
  filter(SG_UF_NOT!=0) %>%
  filter(!epiyear %in% c(2020, 2021, 2022, "2025")) %>%
  mutate(regioes=floor(as.integer(SG_UF_NOT)/10)) %>%
  mutate(regiao=factor(regioes,
                       levels=c(1,2,5,3,4),
                       labels=c('Norte', 'Nordeste',
                                'Centro-oeste', 'Sudeste',
                                'Sul'))) 

library(ISOweek)

df <- df %>%
  mutate(
    semana_iso = sprintf("%d-W%02d-1", `Ano epidemiológico`, `Semana epidemiológica`),
    data = ISOweek2date(semana_iso)
  )

df2<- df %>% filter(regiao=="Norte")


ggplot(df2, aes(x = data, y = SRAG)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Semana epidemiológica", y = "Casos")+
  facet_wrap(~ DS_UF_SIGLA, scale="free")+
  theme_bw()

