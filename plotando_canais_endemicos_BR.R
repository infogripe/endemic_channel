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

load("output/bands_flu.RData")

###Dados vírus

dados_virus <- read.csv2(file = "infogripe/Dados/InfoGripe/casos_semanais_fx_etaria_virus_sem_filtro_febre.csv")

virus_br<- dados_virus %>%
  #filter(Ano.epidemiológico%in% c(2009, 2016, 2023,2024, 2025), SG_UF_NOT==0, fx_etaria=="Total")%>%
   filter(Ano.epidemiológico>=2009, SG_UF_NOT==0, fx_etaria=="Total")%>%
   select(Ano.epidemiológico, SG_UF_NOT, Semana.epidemiológica, FLU_A)%>%
  filter(Ano.epidemiológico<2025) %>%
  filter(!Ano.epidemiológico %in% c(2021,2022))

virus_br2<- dados_virus %>%
  filter(Ano.epidemiológico==2025, SG_UF_NOT==0, fx_etaria=="Total")%>%
  select(Ano.epidemiológico, SG_UF_NOT, Semana.epidemiológica, FLU_A)

virus_br3<- dados_virus %>%
  filter(Ano.epidemiológico ==2009, SG_UF_NOT==0, fx_etaria=="Total")%>%
  select(Ano.epidemiológico, SG_UF_NOT, Semana.epidemiológica, FLU_A)


#####Dados por UF

canal_banda_BR<- bands.flu%>%
  filter(SG_UF_NOT==0)%>%
  mutate(SG_UF_NOT=as.integer(SG_UF_NOT))%>%
  rename(Semana.epidemiológica=week)%>%
  group_by(SG_UF_NOT,  samples, Semana.epidemiológica) %>%
  summarise(values=sum(values, na.rm=T)) %>%
  ungroup()%>%
  group_by(SG_UF_NOT, Semana.epidemiológica) %>%
  summarise(`0.25quant` = quantile(values, probs = 0.25),
            `0.5quant` = quantile(values, probs = 0.5),
            `0.75quant` = quantile(values, probs = 0.75),
            `0.9quant` = quantile(values, probs = 0.9)) %>%
  mutate(Ano.epidemiológico=2026)


###Plotando

#palet<-c("#d9ed92", "#b5e48c", "#92e6a7","#99d98c","#6ede8a","#76c893", "#52b69a", "#34a0a4", "#168aad", "#1a759f", "#1e6091", "#184e77")

#palete2<-c("#10451d", "#155d27", "#1a7431", "#208b3a", "#25a244", "#2dc653", "#4ad66d", "#6ede8a", "#92e6a7", "#b7efc5")

#png(paste0('plots/canal_endemico_BR.png'),height = 6, width = 12, units = 'in', res = 300)

a<- ggplot(canal_banda_BR, aes(x = Semana.epidemiológica)) +
  geom_ribbon(aes(ymin = 0, ymax = `0.5quant`, fill= "Below average, typical"),  alpha=.45) +
  # geom_ribbon(aes(ymin = `0.25quant`, ymax = `0.5quant`), fill='orange', alpha=.35) +
  geom_ribbon(aes(ymin =`0.5quant`, ymax = `0.75quant`, fill="Moderately high, fairly typical"),  alpha=.45) +
  geom_ribbon(aes(ymin=`0.75quant`, ymax=`0.9quant`, fill="Fairly high, atypical"), alpha=.55) +
  geom_ribbon(aes(ymin=`0.9quant`, ymax=Inf, fill="Exceptionally high, very atypical"),  alpha=.45) +
  scale_fill_manual(values=c("Below average, typical"='#81c3d7',
                             "Moderately high, fairly typical" = '#ffba08',
                             "Fairly high, atypical"= "#e85d04" ,
                             "Exceptionally high, very atypical"="#d00000"),
                    labels=c("Below average, \ntypical",
                             "Moderately high, \nfairly typical" ,
                             "Fairly high, \natypical" ,
                             "Exceptionally high, \nvery atypical"),
                    limits = c(  # Define a ordem desejada
                      "Below average, typical",
                      "Moderately high, fairly typical",
                      "Fairly high, atypical",
                      "Exceptionally high, very atypical"
                    ))+
  geom_line(data = virus_br,
            aes(x = Semana.epidemiológica, y = FLU_A, group = Ano.epidemiológico, 
                linetype = "Historical seasons"), 
            linewidth = 0.9, alpha=0.6, color="black"
  ) +
 # scale_color_manual(values = palet) +
  geom_line(data = virus_br2,
            aes(x = Semana.epidemiológica, y = FLU_A, linetype = "2025"), 
            linewidth = 1, colour="black")+
  scale_linetype_manual(
    values = c(
               "2025" = "solid",
               "Historical seasons" = "dashed")
  ) +
  # scale_x_continuous(breaks = c(1, seq(8, 52, 8)), minor_breaks = waiver()) +
  scale_x_continuous(breaks = seq(1, 52, by=9))+
  theme_bw()+
  #theme_Publication(base_size = 24) + 
  theme(panel.grid.minor.x = element_line(colour="#f0f0f0",
                                          linetype=2),
        legend.position = "right",
        legend.direction="vertical",
        legend.text = element_text(size=11),
        legend.title = element_blank(),
        legend.key.height = unit(1.1, "cm"),
        axis.line = element_blank(),
        strip.background=element_rect(colour = NA, fill = "#f0f0f0"),
        strip.text = element_text(face = "bold", size = 17),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title= element_text(size=12),
        plot.margin = margin(1, 1, 4, 5)) +
  labs(
    x = "week of symptoms onset",
    y = paste0(y = "Number of cases (SARI Influenza A)"),
    linetype = "Percentis da previsão",
    color = "Year",
    fill = "Intervalos"
  ) +
  annotate("text",
           x = 36.5,
           y = 2220,
           label = "2009",
           size = 5,
           hjust = 0)+
  annotate("segment",
           x = 36, xend = 32,
           y = 2200, yend = 2100,
           arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
           linewidth = 0.8,
           color = "black")

  
#grid::grid.raster(info.logo, x = 0.98, y = 1, just = c('right', 'top'), width = unit(1.2, 'inches'))
#dev.off() 



  

#####Porporção vívurs

virus_br<- dados_virus %>%
  #filter(Ano.epidemiológico%in% c(2009, 2016, 2023,2024, 2025), SG_UF_NOT==0, fx_etaria=="Total")%>%
  filter(Ano.epidemiológico==2025, SG_UF_NOT==0, fx_etaria=="Total")%>%
  select(Ano.epidemiológico, SG_UF_NOT, Semana.epidemiológica, FLU_AH1N1, FLU_AH3N2)%>%
  pivot_longer(c("FLU_AH1N1":"FLU_AH3N2"), names_to = "Subtype", values_to = "n")
  


##



b<-ggplot(virus_br, aes(x = Semana.epidemiológica, y = n, fill = Subtype)) +
  geom_bar(stat = "identity", position = "fill", alpha=.75) +
  scale_fill_manual(
    name = "Influenza A",
    labels = c(
      "FLU_AH1N1" = "A(H1N1)pdm09",
      "FLU_AH3N2" = "A(H3N2)"
    ),
    values = c(
      "FLU_AH1N1" = "#bc4b51",
      "FLU_AH3N2" = "#5b8e7d"
    )
  )+
  labs(x = "week of symptoms onset", y="Proportion")+
  theme_bw() + 
  theme(panel.grid.minor.x = element_line(colour="#f0f0f0",
                                          linetype=2),
        legend.position = "bottom",
        legend.direction="horizontal",
        legend.text = element_text(size=11),
        legend.title = element_blank(),
        legend.key.height = unit(0.6, "cm"),
        axis.line = element_blank(),
        strip.background=element_rect(colour = NA, fill = "#f0f0f0"),
        strip.text = element_text(face = "bold", size = 17),
        axis.text = element_text(size=11),
        axis.title= element_text(size=12),
        plot.margin = margin(1, 1, 4, 5)) 
  


#####Dados vacina

vacina<- read.csv("doses_influenza_2025.csv")

df<- vacina %>%
  mutate(doses = doses_acumuladas/ 1e6)

c<- ggplot( ) +
  geom_line(data = df,
            aes(x = semana, y = doses  
            ), linewidth = 0.8, color="#001d3d"
  ) +
  theme_bw() + 
  ylim(0,70)+
  geom_vline(xintercept = 17, color="darkred")+
  theme(panel.grid.minor.x = element_line(colour="#f0f0f0",
                                          linetype=2),
        legend.position = "right",
        legend.direction="vertical",
        legend.text = element_text(size=11),
        legend.title = element_blank(),
        legend.key.height = unit(0.6, "cm"),
        axis.line = element_blank(),
        strip.background=element_rect(colour = NA, fill = "#f0f0f0"),
        strip.text = element_text(face = "bold", size = 17),
        axis.text = element_text(size=11),
        axis.title= element_text(size=12),
        plot.margin = margin(1, 1, 4, 5)) +
  labs(
    x = "Week",
    y = paste0(y = "Cumulative doses (in millions)"),
  ) 


library(patchwork)


png('Figure1.png', height = 8, width = 10, units = 'in', res = 300)

(a / (b + c)) +
  plot_layout(width = c(1, 1),
              heights = c(1.3,1)) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 16, face = "bold"))

dev.off()

tiff('Figure1.tiff', height = 8, width = 10, units = 'in', res = 300)

(a / (b + c)) +
  plot_layout(width = c(1, 1),
              heights = c(1.3,1)) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 16, face = "bold"))

dev.off()


help("plot_annotation")


######Tabela resumo dados


virus_br<- dados_virus %>%
  filter(Ano.epidemiológico>=2009, SG_UF_NOT==0, fx_etaria=="Total")%>%
  #filter(Ano.epidemiológico%in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018,2019,2023,2024, 2025), SG_UF_NOT==0, fx_etaria=="Total")%>%
  select(Ano.epidemiológico, SG_UF_NOT, Semana.epidemiológica, FLU_A)%>%
  filter(Ano.epidemiológico<2026) %>%
  group_by(Ano.epidemiológico) %>%
  summarise(total=sum(FLU_A, na.rm = TRUE))%>%
  mutate(diferença_relacao2025=(28270-total)/total)


virus_br<- dados_virus %>%
  filter(Ano.epidemiológico>=2009, SG_UF_NOT==0, fx_etaria=="Total")%>%
  #filter(Ano.epidemiológico%in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018,2019,2023,2024, 2025), SG_UF_NOT==0, fx_etaria=="Total")%>%
  select(Ano.epidemiológico, SG_UF_NOT, Semana.epidemiológica, FLU_A)%>%
  filter(Ano.epidemiológico<2026) %>%
  group_by(Ano.epidemiológico) %>%
  summarise(total=sum(FLU_A, na.rm = TRUE))%>%
  mutate(diferença_relacao2025=(28270-total)/total)

teste<-   filter(Ano.epidemiológico%in% c(2021, 2022), SG_UF_NOT==0, fx_etaria=="Total")%>%
  #filter(Ano.epidemiológico%in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018,2019,2023,2024, 2025), SG_UF_NOT==0, fx_etaria=="Total")%>%
  select(Ano.epidemiológico, SG_UF_NOT, Semana.epidemiológica, FLU_A) %>%
  filter(Ano.epidemiológico<2026) %>%
  group_by(Ano.epidemiológico) %>%
  summarise(total=sum(FLU_A, na.rm = TRUE))%>%
  mutate(diferença_relacao2025=(28270-total)/total)

canal_banda_BR<- bands.flu%>%
  filter(SG_UF_NOT==0)%>%
  mutate(SG_UF_NOT=as.integer(SG_UF_NOT))%>%
  rename(Semana.epidemiológica=week)%>%
  group_by(SG_UF_NOT,  samples, Semana.epidemiológica) %>%
  summarise(values=sum(values, na.rm=T)) %>%
  ungroup()%>%
  group_by(SG_UF_NOT, Semana.epidemiológica) %>%
  summarise(`0.25quant` = quantile(values, probs = 0.25),
            `0.5quant` = quantile(values, probs = 0.5),
            `0.75quant` = quantile(values, probs = 0.75),
            `0.9quant` = quantile(values, probs = 0.9)) %>%
  pivot_longer(c(`0.25quant`:`0.9quant`), values_to = "value", names_to = "predito")%>%
  group_by(predito)%>%
  summarise(total=sum(value, na.rm = TRUE))%>%
  
  ###Dados antigos vacina
  
  df <- tibble(
    ano = 2009:2025,
    total_dose = c(
      16080123,
      15361056,
      25134553,
      26034668,
      29160815,
      35430866,
      35618994,
      39148582,
      39160569,
      40221392,
      54442433,
      50768595,
      71889287,
      62918552,
      64230887,
      57325324,
      29888056
    )
  ) %>%
  mutate(total_dosem = total_dose / 1e6)

