# L'arxiu es troba codificat en UTF-8. 
# Si apareixen caràcters estranys, anar a file --> 'reopen with encoding' i seleccionar UTF-8

rm(list=ls())

# Llibreries

library(dplyr)
library(glue)
library(ggplot2)
library(patchwork)
library(ggthemes)
library(scales)
library(sysfonts)
library(lubridate)
library(stringr)
library(slider)
library(tidyr)
library(ggrepel)
library(showtext)

# Font-Family
font_add_google("Raleway")
showtext_auto()


# Working Directory = Mateixa localització de l'script (només funciona amb RStudio)
# Si no s'utilitza RStudio, eliminar i establir-ho maualment
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Dades: casos, població

casos = read.csv(
  "https://analisi.transparenciacatalunya.cat/resource/qwj8-xpvk.csv?$select=data, regiosanitariacodi, regiosanitariadescripcio, edatrang, SUM(numcasos)&$group=data, regiosanitariacodi, regiosanitariadescripcio, edatrang&$where=resultatcoviddescripcio IN ('Positiu TAR', 'Positiu PCR') &$order=regiosanitariacodi DESC, data ASC, edatrang ASC&$limit=99999999",
  encoding = "UTF-8"
)

poblacio = read.csv(
  "https://analisi.transparenciacatalunya.cat/resource/ftq4-h9vk.csv?$select=rs_codi, rs_nom, abs_codi, abs_nom, edat, any, SUM(poblacio_oficial)&$group=rs_codi, rs_nom, abs_codi, abs_nom, edat, any&$where=any IN (2020, 2021, 2022)&$limit=9999999",
  encoding = "UTF-8"
)

relacio_abs_regio = read.csv(
  "https://analisi.transparenciacatalunya.cat/resource/xuwf-dxjd.csv?$select=regiosanitariacodi, regiosanitariadescripcio, abscodi, absdescripcio&$group=regiosanitariacodi, regiosanitariadescripcio, abscodi, absdescripcio",
  encoding = "UTF-8"
)

relacio_abs_regio = relacio_abs_regio%>%
  mutate(abscodi = str_pad(abscodi, 3, side="left", pad = "0"))


# Catàleg grups d'edat

grups_edat = data.frame(
  edat = seq(0, max(poblacio$edat), 1)
)

grups_edat = grups_edat%>%
  mutate(grup = case_when(
    edat>=0 & edat<=9 ~ "0-9",
    edat>=10 & edat<=19 ~ "10-19",
    edat>=20 & edat<=29 ~ "20-29",
    edat>=30 & edat<=39 ~ "30-39",
    edat>=40 & edat<=49 ~ "40-49",
    edat>=50 & edat<=59 ~ "50-59",
    edat>=60 & edat<=69 ~ "60-69",
    edat>=70 & edat<=79 ~ "70-79",
    edat>=80 & edat<=89 ~ "80-89",
    edat>=90 ~ "90+"
  ),
  ordre_edats = case_when(
    edat>=0 & edat<=9 ~ 1,
    edat>=10 & edat<=19 ~ 2,
    edat>=20 & edat<=29 ~ 3,
    edat>=30 & edat<=39 ~ 4,
    edat>=40 & edat<=49 ~ 5,
    edat>=50 & edat<=59 ~ 6,
    edat>=60 & edat<=69 ~ 7,
    edat>=70 & edat<=79 ~ 8,
    edat>=80 & edat<=89 ~ 9,
    edat>=90 ~ 10
  )
  
  
  )

# Poblacions

pob_cat = poblacio%>%
  group_by(any)%>%
  summarise(pob = sum(SUM_poblacio_oficial))%>%
  ungroup()%>%
  rename(AA = any)

pob_edats = poblacio%>%
  left_join(grups_edat, by = c("edat"))%>%
  group_by(any, grup, ordre_edats)%>%
  summarise(pob = sum(SUM_poblacio_oficial))%>%
  ungroup()%>%
  mutate(Id = paste0(any, "-", grup))%>%
  rename(AA = any)

pob_rs = poblacio%>%
  mutate(abs_codi = str_pad(abs_codi, width = 3, side = "left", pad = "0"))%>%
  left_join(relacio_abs_regio, by = c("abs_codi" = "abscodi"))%>%
  group_by(any, regiosanitariacodi, regiosanitariadescripcio)%>%
  summarise(pob = sum(SUM_poblacio_oficial))%>%
  ungroup()%>%
  mutate(Id = paste0(any, "-", regiosanitariacodi))%>%
  rename(AA = any,
         rs_codi = regiosanitariacodi,
         rs_desc = regiosanitariadescripcio)

# Dates de referència
casos_data_min = min( as.Date(ymd_hms(casos$data)) )
casos_data_max = max( as.Date(ymd_hms(casos$data)) )
casos_data_rang = seq(casos_data_min, casos_data_max, "day")

casos_data_max_3 = casos_data_max - 3

# Marca el principi de la sèrie a 1 de juliol de 2020. Per la sèrie completa: 2020-03-01
data_min_serie = ymd("2020-07-01")

avui = today()
avui_l = format(avui, "%Y%m%d")

#Font-size

ggplot_font_size = 40
gglabel_font_size = 10

patchwork_title = 50
patchwork_subtitle = 40
patchwork_caption = 40

left_margin = 190



# Càlcul d'indicadors


# IA14 Catalunya --> es consideren els no classificats
ia14_catalunya = casos%>%
  mutate(data = as.Date(ymd_hms(data)))%>%
  group_by(data)%>%
  summarise(casos = sum(SUM_numcasos))%>%
  ungroup()%>%
  mutate(casos14 = slide_dbl(casos, ~sum(.), .before = 13),
         AA = year(data))%>%
  left_join(pob_cat, by = "AA")%>%
  mutate(IA14 = (casos14/pob)*100000)%>%
  filter(data >= data_min_serie)

falten_anys = ifelse(TRUE %in% is.na(ia14_catalunya$IA14), TRUE, FALSE)

if (falten_anys){
  pob_mes_recent = pob_cat%>%
    filter(AA == max(AA))
  
  ia14_catalunya = ia14_catalunya%>%
    mutate(pob = ifelse(is.na(pob), pob_mes_recent$pob, pob))%>%
    mutate(IA14 = (casos14/pob)*100000)

}

variacio = ia14_catalunya$IA14[ia14_catalunya$data == casos_data_max_3] - ia14_catalunya$IA14[ia14_catalunya$data == casos_data_max_3-1]
signe_variacio = ifelse(variacio == 0, "",
                   ifelse(variacio >0, "+", ""))

ia14_catalunya = ia14_catalunya%>%
  filter(data <= casos_data_max_3)%>%
  mutate(Dif = IA14-lag(IA14))%>%
  mutate(gglabel = ifelse(data == casos_data_max_3, 
                          paste0(format(round(IA14, 2), dec = ","),
                                 " (", 
                                 signe_variacio,
                                 format(round(Dif, 2), dec = ","),
                                 ")"),
                          NA)
         )%>%
  mutate(gglabel = gsub("\\s{2,}", "", gglabel))

# IA14 per edats --> No es consideren els no classificats

ia14_edats = casos%>%
  filter(edatrang != "No classificat")%>%
  mutate(data = as.Date(ymd_hms(data)))%>%
  
  group_by(data, edatrang)%>%
  summarise(casos = sum(SUM_numcasos))%>%
  ungroup()%>%
  
  rename(dates = data)%>%
  complete(nesting(edatrang), dates = casos_data_rang)%>%
  arrange(edatrang, dates)%>%
  mutate(casos = coalesce(casos, 0))%>%
  
  group_by(edatrang)%>%
  mutate(casos14 = slide_dbl(casos, ~sum(.), .before = 13),
         AA = year(dates))%>%
  ungroup()%>%
  
  filter(dates >= data_min_serie,
         dates <= casos_data_max_3)%>%
  mutate(Id = paste0(AA, "-", edatrang))%>%
  select(-AA)%>%
  left_join(pob_edats, by = "Id")

if (falten_anys) {
  pob_edats_falten = pob_edats%>%
    filter(AA == max(AA))%>%
    mutate(AA = AA + 1)%>%
    mutate(Id = paste0(AA, "-", grup))
  
  pob_edats_complet = rbind(pob_edats, pob_edats_falten)
  
  ia14_edats = ia14_edats%>%
    select(-AA, -grup, -ordre_edats, -pob)%>%
    left_join(pob_edats_complet, by = "Id")
    
}

ia14_edats = ia14_edats%>%
  mutate(IA14 = (casos14/pob)*100000)%>%
  mutate(Dif = IA14 - lag(IA14))%>%
  mutate(signe_v = ifelse(Dif <= 0, "", "+"))%>%
  mutate(
    ggLabel = ifelse(dates == casos_data_max_3,
                     paste0(edatrang, ": ", format(round(IA14, 2), dec = ","), " (", signe_v,  format(round(Dif, 2), dec = ","), ")" ),
                     NA)
  )%>%
  mutate(ggLabel = gsub("\\s{2,}", "", ggLabel))


# IA14 per regió sanitària --> No es consideren els no classificats

ia14_regio = casos%>%
  filter(regiosanitariadescripcio != "No classificat")%>%
  mutate(data = as.Date(ymd_hms(data)))%>%
  
  group_by(data, regiosanitariacodi, regiosanitariadescripcio)%>%
  summarise(casos = sum(SUM_numcasos))%>%
  ungroup()%>%
  
  rename(dates = data)%>%
  complete(nesting(regiosanitariacodi, regiosanitariadescripcio), dates = casos_data_rang)%>%
  arrange(regiosanitariacodi, dates)%>%
  mutate(casos = coalesce(casos, 0))%>%
  
  group_by(regiosanitariacodi, regiosanitariadescripcio)%>%
  mutate(casos14 = slide_dbl(casos, ~sum(.), .before = 13),
         AA = year(dates))%>%
  ungroup()%>%
  
  filter(dates >= data_min_serie,
         dates <= casos_data_max_3)%>%
  mutate(Id = paste0(AA, "-", regiosanitariacodi))%>%
  select(-AA)%>%
  left_join(pob_rs, by = "Id")


if (falten_anys) {
  pob_rs_falten = pob_rs%>%
    filter(AA == max(AA))%>%
    mutate(AA = AA + 1)%>%
    mutate(Id = paste0(AA, "-", rs_codi))
  
  pob_rs_complet = rbind(pob_rs, pob_rs_falten)
  
  ia14_regio = ia14_regio%>%
    select(-AA, -rs_codi, -rs_desc, -pob)%>%
    left_join(pob_rs_complet, by = "Id")
  
}


ia14_regio = ia14_regio%>%
  mutate(IA14 = (casos14/pob)*100000)%>%
  mutate(
    regiosanitariadescripcio = case_when(regiosanitariadescripcio == "LLEIDA" ~ "Lleida",
                                         regiosanitariadescripcio == "CAMP DE TARRAGONA" ~ "Tarragona",
                                         regiosanitariadescripcio == "TERRES DE L'EBRE" ~ "T. de l'Ebre",
                                         regiosanitariadescripcio == "GIRONA" ~ "Girona",
                                         regiosanitariadescripcio == "CATALUNYA CENTRAL" ~ "Cat. Central",
                                         regiosanitariadescripcio == "ALT PIRINEU I ARAN" ~ "Pirineu-Aran",
                                         regiosanitariadescripcio == "METROPOLITANA SUD" ~ "Metro Sud",
                                         regiosanitariadescripcio == "METROPOLITANA NORD" ~ "Metro Nord",
                                         regiosanitariadescripcio == "BARCELONA CIUTAT" ~ "Barcelona")
  )%>%
  mutate(Dif = IA14 - lag(IA14))%>%
  mutate(signe_v = ifelse(Dif <= 0, "", "+"))%>%
  mutate(
    ggLabel = ifelse(dates == casos_data_max_3,
                     paste0(regiosanitariadescripcio, ": ", format(round(IA14, 2), dec = ","), " (", signe_v,  format(round(Dif, 2), dec = ","), ")" ),
                     NA)
  )%>%
  mutate(ggLabel = gsub("\\s{2,}", "", ggLabel))


# Gràfic IA-14 Catalunya

ia14_catalunya = ia14_catalunya%>%
  mutate(colors_variacio = ifelse(Dif<=0 | is.na(Dif), "#DDDDDD", "#FA1E0E" ))

setmanes_considerades = seq(data_min_serie, casos_data_max, "1 month")
setmanes_expand = seq(casos_data_max+1, casos_data_max+left_margin, "1 month")

vector_colors_x = c(rep("black", length(setmanes_considerades)+1),
                    rep("#F3FAFF", length(setmanes_expand)+1))


gg_ia14_cat =ggplot(data = ia14_catalunya)+
  geom_bar(aes(x = data, y = IA14),
           fill = ia14_catalunya$colors_variacio,
           stat = "identity",
           width = 1, alpha = 0.8)+
  scale_y_continuous(labels = function(x) format(x, big.mark = " "))+
  scale_x_date(breaks = "1 month",
               date_labels = "%m/%y",
               limits = c(min(ia14_catalunya$data), max(ia14_catalunya$data)+left_margin))+
  geom_text_repel(data = subset(ia14_catalunya, data == max(data)),
                  aes(x = data, y = IA14, label = gglabel),
                  direction = "y",
                  segment.linetype = 2,
                  segment.curvature = 0.15,
                  segment.color = ia14_catalunya$colors_variacio[ia14_catalunya$data==max(ia14_catalunya$data)],
                  nudge_x = 1000,
                  nudge_y = 500,
                  bg.color = "white",
                  bg.r = 0.15,
                  size = gglabel_font_size+3
  )+
  labs(x = "",
       y = "",
       title = "Catalunya")+
  theme_hc()+
  theme(axis.text.x = element_text(angle = 90, colour = vector_colors_x),
        axis.ticks.x = element_line(color = vector_colors_x),
        text = element_text(family = "Raleway", size = ggplot_font_size),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_blank(),
        legend.position = "bottom",
        title = element_text(hjust = 0,
                             color = "#1E3E52", face = "bold"))

# Gràfic IA14 per edats

gg_ia14_edats = ggplot(data = ia14_edats)+
  geom_line(aes(x = dates, y = IA14,
                color = grup))+
  scale_y_continuous(labels = function(x) format(x, big.mark = " "))+
  scale_x_date(breaks = "1 month",
               date_labels = "%m/%y",
               limits = c(min(ia14_edats$dates), max(ia14_edats$dates)+left_margin) )+
  scale_color_tableau()+
  geom_text_repel(data = subset(ia14_edats, dates == max(dates)),
                  aes(x = dates, y = IA14, label = ggLabel,
                      color = grup,
                      group = grup),
                  nudge_x = 1000,
                  direction = "y",
                  hjust = 1,
                  segment.curvature = 0.15,
                  force = 10,
                  fontface = "bold",
                  segment.linetype = 2,
                  segment.size = 0.3,
                  size = gglabel_font_size+2,
                  bg.color = "white",
                  bg.r = 0.15)+
  labs(x = "",
       y = "",
       title = "Edats")+
  theme_hc()+
  theme(axis.text.x = element_text(angle = 90, colour = vector_colors_x),
        axis.ticks.x = element_line(color = vector_colors_x),
        text = element_text(family = "Raleway", size = ggplot_font_size),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_blank(),
        legend.position = "none",
        title = element_text(hjust = 0,
                             color = "#1E3E52", face = "bold"))


# IA14 per regió sanitària

gg_ia14_regions = ggplot(data = ia14_regio)+
  geom_line(aes(x = dates, y = IA14,
                color = regiosanitariadescripcio))+
  scale_y_continuous(labels = function(x) format(x, big.mark = " "))+
  scale_x_date(breaks = "1 month",
               date_labels = "%m/%y",
               limits = c(min(ia14_regio$dates), max(ia14_regio$dates)+left_margin) )+
  scale_color_tableau()+
  geom_text_repel(data = subset(ia14_regio, dates == max(dates)),
                  aes(x = dates, y = IA14, label = ggLabel,
                      color = regiosanitariadescripcio,
                      group = regiosanitariadescripcio),
                  nudge_x = 1000,
                  direction = "y",
                  hjust = 1,
                  segment.curvature = 0.5,
                  force = 10,
                  fontface = "bold",
                  segment.linetype = 2,
                  segment.size = 0.3,
                  size = gglabel_font_size+1,
                  bg.color = "white",
                  bg.r = 0.15)+
  labs(x = "",
       y = "",
       title = "Regions Sanitàries")+
  theme_hc()+
  theme(axis.text.x = element_text(angle = 90, colour = vector_colors_x),
        axis.ticks.x = element_line(color = vector_colors_x),
        text = element_text(family = "Raleway", size = ggplot_font_size),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_blank(),
        legend.position = "none",
        title = element_text(hjust = 0,
                             color = "#1E3E52", face = "bold"))

# Composició

data_min_serie_l = format(data_min_serie, "%d/%m/%y")
casos_data_max_3_l = format(casos_data_max_3, "%d/%m/%y")


composicio = gg_ia14_cat/gg_ia14_edats/gg_ia14_regions+
  plot_annotation(title = "Incidència acumulada a 14 dies (100.000 hab.) (PCR i TAR):",
                  subtitle = glue("{data_min_serie_l} al {casos_data_max_3_l}"),
                  caption = "@marc_coca | Font: Dades Obertes | Població 2020 i 2021",
                  theme = theme(plot.title = element_text(family = "Raleway",
                                                          face = "bold",
                                                          colour = "orange",
                                                          size = patchwork_title+2),
                                plot.subtitle = element_text(size = patchwork_subtitle+2),
                                plot.background = element_rect(fill = "#F3FAFF"),
                                plot.caption = element_text(size = patchwork_caption+1),
                                plot.margin = unit(c(0.5,0.25,0.5,0.1), "cm")))

ggsave(glue("Gràfiques/{avui_l}_IA14_Cat.png"),
       #DIN A4
       width = 210, 
       height = 297, 
       units = "mm",
       dpi = 350)
