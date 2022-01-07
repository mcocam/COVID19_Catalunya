rm(list=ls())

library(ggplot2)
library(tidyverse)
library(ggrepel)
library(glue)
library(ggthemes)
library(scales)
library(patchwork)
library(lubridate)
library(ggtext)
library(sysfonts)
library(showtext)
library(ggstream)
library(patchwork)
library(grid)
library(gridtext)
library(rvest)
library(xml2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

options(scipen = 999)

font_add_google("Montserrat")
font_add_google("Merienda")

showtext_auto()




vacunesAPI = read.csv("https://analisi.transparenciacatalunya.cat/resource/tp23-dey4.csv?$select=DATA, FABRICANT, DOSI, EDAT, SUM(RECOMPTE)&$group=DATA, FABRICANT, DOSI, EDAT&$limit=99999999999&$where=FABRICANT<>'No administrada'",
                      encoding = "UTF-8")
rebuigAPI = read.csv("https://analisi.transparenciacatalunya.cat/resource/tp23-dey4.csv?$select=REGIO_CODI, DATA, FABRICANT, DOSI, EDAT, SUM(RECOMPTE)&$group=REGIO_CODI, DATA, FABRICANT, DOSI, EDAT&$limit=99999999999&$where=FABRICANT='No administrada'",
                     encoding = "UTF-8")
poblacio = read.csv("https://analisi.transparenciacatalunya.cat/resource/ftq4-h9vk.csv?$limit=99999999999&$select=any, edat, SUM(poblacio_oficial)&$group=any, edat&$where=any='2021'",
                    encoding = "UTF-8")


grupsEdat = data.frame(
  edatU = seq(1, 200, 1)
)

poblacio = poblacio%>%
  mutate(grupEdat = case_when(
    edat>=0 & edat<=11 ~ "0 a 11",
    edat>=12 & edat<=14 ~ "12 a 14",
    edat>=15 & edat<=19 ~ "15 a 19",
    edat>=20 & edat<=24 ~ "20 a 24",
    edat>=25 & edat<=29 ~ "25 a 29",
    edat>=30 & edat<=34 ~ "30 a 34",
    edat>=35 & edat<=39 ~ "35 a 39",
    edat>=40 & edat<=44 ~ "40 a 44",
    edat>=45 & edat<=49 ~ "45 a 49",
    edat>=50 & edat<=54 ~ "50 a 54",
    edat>=55 & edat<=59 ~ "55 a 59",
    edat>=60 & edat<=64 ~ "60 a 64",
    edat>=65 & edat<=69 ~ "65 a 69",
    edat>=70 & edat<=74 ~ "70 a 74",
    edat>=75 & edat<=79 ~ "75 a 79",
    edat>=80 ~ "80 o més"
    
  ),
  ordreEdat = case_when(
    edat>=0 & edat<=11 ~ 1,
    edat>=12 & edat<=14 ~ 2,
    edat>=15 & edat<=19 ~ 3,
    edat>=20 & edat<=24 ~ 4,
    edat>=25 & edat<=29 ~ 5,
    edat>=30 & edat<=34 ~ 6,
    edat>=35 & edat<=39 ~ 7,
    edat>=40 & edat<=44 ~ 8,
    edat>=45 & edat<=49 ~ 9,
    edat>=50 & edat<=54 ~ 10,
    edat>=55 & edat<=59 ~ 11,
    edat>=60 & edat<=64 ~ 12,
    edat>=65 & edat<=69 ~ 13,
    edat>=70 & edat<=74 ~ 14,
    edat>=75 & edat<=79 ~ 15,
    edat>=80 ~ 16
    
  )
  )%>%
  group_by(grupEdat, ordreEdat)%>%
  summarise(pob = sum(SUM_poblacio_oficial))%>%
  ungroup()


fabricants = data.frame(id = c("BioNTech / Pfizer"
                               ,"Moderna / Lonza"
                               ,"Oxford / AstraZeneca"
                               ,"J&J / Janssen"),
                        fShortName = c("Pfizer"
                                      ,"Moderna"
                                      ,"AstraZeneca"
                                      , "Janssen"))

edatsD2 = vacunesAPI%>%
  filter(DOSI == "2"
         ,FABRICANT!="J&J / Janssen")%>%
  group_by(EDAT)%>%
  summarise(dosisAdm2 = sum(SUM_RECOMPTE))%>%
  ungroup()

edatsJanssen =  vacunesAPI%>%
  filter(FABRICANT=="J&J / Janssen")%>%
  group_by(EDAT)%>%
  summarise(dosisAdmJanssen = sum(SUM_RECOMPTE))%>%
  ungroup()

dadesVacunesDies = vacunesAPI%>%
  left_join(poblacio, by = c("EDAT" = "grupEdat"))%>%
  left_join(fabricants, by = c("FABRICANT" = "id"))%>%
  left_join(edatsD2, by = c("EDAT"))%>%
  left_join(edatsJanssen, by = c("EDAT"))%>%
  group_by(DATA
           ,fShortName
           ,DOSI
           ,EDAT
           ,ordreEdat)%>%
  summarise(vacunesAdministrades = sum(SUM_RECOMPTE)
            ,poblacio = mean(pob)
            ,dosisAdm2 = mean(dosisAdm2)
            ,dosisAdmJanssen = mean(dosisAdmJanssen))%>%
  ungroup()%>%
  mutate(denPob1 = poblacio
         ,denPob2 = poblacio
         ,denPob3 = dosisAdm2)%>%
  mutate(percentDosi1 = ifelse(DOSI=="1", vacunesAdministrades/denPob1, NA)
         ,percentDosi2 = ifelse(DOSI=="2", vacunesAdministrades/denPob2, NA)
         ,percentDosi3 = ifelse(DOSI=="3", vacunesAdministrades/denPob3, NA))%>%
  mutate(DATA = ymd_hms(DATA))%>%
  mutate(percentD = case_when(
    !is.na(percentDosi1) ~ percentDosi1
    ,!is.na(percentDosi2) ~ percentDosi2
    ,!is.na(percentDosi3) ~ percentDosi3
  ))

dadesVacunesNow = dadesVacunesDies%>%
  group_by(fShortName
           ,DOSI
           ,EDAT
           ,ordreEdat)%>%
  summarise(vacunesAdministrades = sum(vacunesAdministrades)
            ,poblacio = mean(poblacio)
            ,dosisAdm2 = mean(dosisAdm2)
            ,dosisAdmJanssen = mean(dosisAdmJanssen))%>%
  ungroup()%>%
  mutate(denPob1 = poblacio
         ,denPob2 = poblacio
         ,denPob3 = dosisAdm2)%>%
  mutate(percentDosi1 = ifelse(DOSI=="1", vacunesAdministrades/denPob1, NA)
         ,percentDosi2 = ifelse(DOSI=="2", vacunesAdministrades/denPob2, NA)
         ,percentDosi3 = ifelse(DOSI=="3", vacunesAdministrades/denPob3, NA))%>%
  mutate(percentD = case_when(
    !is.na(percentDosi1) ~ percentDosi1
    ,!is.na(percentDosi2) ~ percentDosi2
    ,!is.na(percentDosi3) ~ percentDosi3
  ))

dadesVacunesEdats = dadesVacunesDies%>%
  group_by(DOSI
           ,EDAT
           ,ordreEdat)%>%
  summarise(vacunesAdministrades = sum(vacunesAdministrades)
            ,poblacio = mean(poblacio)
            ,dosisAdm2 = mean(dosisAdm2)
            ,dosisAdmJanssen = mean(dosisAdmJanssen))%>%
  ungroup()%>%
  mutate(denPob1 = poblacio
         ,denPob2 = poblacio
         ,denPob3 = dosisAdm2)%>%
  mutate(percentDosi1 = ifelse(DOSI=="1", vacunesAdministrades/denPob1, NA)
         ,percentDosi2 = ifelse(DOSI=="2", vacunesAdministrades/denPob2, NA)
         ,percentDosi3 = ifelse(DOSI=="3", vacunesAdministrades/denPob3, NA))%>%
  mutate(percentD = case_when(
    !is.na(percentDosi1) ~ percentDosi1
    ,!is.na(percentDosi2) ~ percentDosi2
    ,!is.na(percentDosi3) ~ percentDosi3
  ))


###
vacunesD1 = sum(dadesVacunesDies$vacunesAdministrades[dadesVacunesDies$DOSI=="1"])
vacunesD2 = sum(dadesVacunesDies$vacunesAdministrades[dadesVacunesDies$DOSI=="2"])
vacunesD3 = sum(dadesVacunesDies$vacunesAdministrades[dadesVacunesDies$DOSI=="3"])

denVacunesD1 = sum(poblacio$pob)
denVacunesD2 = sum(poblacio$pob)-sum(dadesVacunesDies$vacunesAdministrades[dadesVacunesDies$fShortName=="Janssen"])
denVacunesD3 = sum(dadesVacunesDies$vacunesAdministrades[dadesVacunesDies$DOSI=="2" 
                                                     & dadesVacunesDies$fShortName!="Janssen"])

percentD1 = vacunesD1 / denVacunesD1
percentD2 = vacunesD2 / denVacunesD2
percentD3 = vacunesD3 / denVacunesD3

##

edats_ggLabel = dadesVacunesEdats%>%
  mutate(ID = paste0(EDAT, DOSI))%>%
  rename(nD_label = vacunesAdministrades,
         pD_label = percentD)%>%
  select(ID,
         nD_label,
         pD_label)

dadesVacunes_ggEdats = dadesVacunesNow%>%
  filter(EDAT!="No classificat")%>%
  mutate(ID = paste0(EDAT, DOSI))%>%
  left_join(edats_ggLabel, by = c("ID"))%>%
  mutate(gglabel_edats = paste(
    format(nD_label, big.mark = ".", decimal.mark = ","),
    " (",
    format(round(pD_label*100, 2), big.mark = ".", decimal.mark = ","),
    "%)"
    )
  )%>%
  mutate(D = duplicated(ID))%>%
  mutate(gglabel_edats = ifelse(D == F, gglabel_edats, NA))


A = ggplot(dadesVacunes_ggEdats,
       aes(x = reorder(DOSI, -DOSI),
           y = percentD,
           fill = factor(fShortName)))+
  geom_bar(stat = "identity",
           position = "stack")+
  geom_col()+
  scale_y_continuous(labels = percent_format(),
                     limits = c(0,1))+
  scale_x_discrete(position = "top")+
  scale_fill_tableau()+
  scale_color_tableau()+
  labs(x = "", y = "", fill = "",
       title = "Edats i fabricant: 1res (1), 2nes dosis (2) i 3res dosis (3)")+
  geom_text_repel(data = dadesVacunes_ggEdats,
                  aes(x = reorder(DOSI, -DOSI), y = 0.4, label = gglabel_edats),
                  position = position_dodge(width = 0),
                  direction = "x",
                  force_pull = 10,
                  force = 0,
                  alpha = 1,
                  size = 8,
                  bg.color = "white",
                  bg.r = 0.15)+
  coord_flip()+
  theme_hc()+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(family = "Montserrat"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 20,
                                  # color = c("#4E79A7",
                                  #           "#F28E2B",
                                  #           "#E15759",
                                  #           "#76B7B2"),
                                   face = "bold"),
        axis.text = element_text(size = 20),
        strip.text = element_text(face = "bold",
                                  size = 20),
        plot.title = element_text(family = "Merienda",
                                  size = 30,
                                  face = "bold",
                                  hjust = 0.5))+
  facet_grid(reorder(EDAT, -ordreEdat)~.,
             switch = "both")


dadesVacunesDies_ggplot = dadesVacunesDies%>%
  filter(EDAT!="No classificat")%>%
  group_by(DATA,
           EDAT,
           ordreEdat,
           fShortName)%>%
  summarise(vacunesAdministrades = sum(vacunesAdministrades))%>%
  ungroup()%>%
  mutate(DATA = as.Date(DATA))

B = ggplot(dadesVacunesDies_ggplot,
       aes(x = DATA,
           y = vacunesAdministrades,
           fill = fShortName))+
  geom_stream()+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%m/%y")+
  scale_fill_tableau()+
  scale_color_tableau()+
  labs(x = "", y = "", fill = "",
       title = "Stream temporal: dosis totals")+
  facet_grid(rows = vars(reorder(EDAT, -ordreEdat)),
             scales = "free_y")+
  theme(
        text = element_text(family = "Montserrat"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(family = "Merienda",
                                  size = 30,
                                  face = "bold",
                                  hjust = 0.5),
        legend.background = element_rect(fill = "transparent"),
        axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_text(angle = 90, size=20),
        panel.grid.major.x = element_line(color = "#F3F1F5"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        strip.text = element_blank())


avui = format.Date(today(), "%d/%m/%y")
dadesFins = format.Date(max(dadesVacunesDies_ggplot$DATA), "%d/%m/%y")
pobD1 = format(denVacunesD1, big.mark = ".", decimal.mark = ",")
pobD2 = format(denVacunesD2, big.mark = ".", decimal.mark = ",")
pobD3 = format(denVacunesD3, big.mark = ".", decimal.mark = ",")

D1_text = format(vacunesD1, big.mark = ".", decimal.mark = ",")
D2_text = format(vacunesD2, big.mark = ".", decimal.mark = ",")
D3_text = format(vacunesD3, big.mark = ".", decimal.mark = ",")

P1_text = paste0(format(round(percentD1*100, 2), big.mark = ".", decimal.mark = ","), "%")
P2_text = paste0(format(round(percentD2*100, 2), big.mark = ".", decimal.mark = ","), "%")
P3_text = paste0(format(round(percentD3*100, 2), big.mark = ".", decimal.mark = ","), "%")

indexHTML = read_html("https://dadescovid.cat/")%>%
  html_table(dec = ",")

indexHTML_2 = read_html("https://dadescovid.cat/vacunacio/")%>%
  html_table(dec = ",")

pautaC = indexHTML[[5]]$`Pauta completa`
perPautaC = indexHTML_2[[1]][["% Pauta completa *"]][20]
perPautaC = format(perPautaC, big.mark = ".", decimal.mark = ",")

d1Text_label = c(glue("1res dosis: 
{D1_text}"))
d1Text = textGrob(d1Text_label,
                  gp = gpar(fontsize=40, 
                            lineheight = 0.4))

d2Text_label = c(glue("2nes dosis: 
{D2_text}"))
d2Text = textGrob(d2Text_label,
                  gp = gpar(fontsize=40, 
                            lineheight = 0.4))

d3Text_label = c(glue("3res dosis: 
{D3_text}"))
d3Text = textGrob(d3Text_label,
                  gp = gpar(fontsize=40, 
                            lineheight = 0.4))

dPText_label = c(glue("Pauta completa: {pautaC}
                      {perPautaC}% (>=12 anys)"))
dPText = textGrob(dPText_label,
                  gp = gpar(fontsize=38, 
                            lineheight = 0.4))


Z = (wrap_elements(d1Text) | wrap_elements(d2Text) | wrap_elements(d3Text) | wrap_elements(dPText)) / (A+B)+
  plot_layout(guides = "collect",
              heights = c(0.1, 1))+
  plot_annotation(
    title = "Vacunació COVID-19 a Catalunya:",
    subtitle = glue("Dosis administrades fins {dadesFins}"),
    caption = glue("**@marc_coca** <br> Els percentatges per les 1res i 2nes dosis corresponen a la població RCA oficial 2021: {pobD1} <br> Per les 3res dosis, s'utilitza la quantitat de 2nes dosis administrades: {pobD3} <br> **Font: Dades Obertes**"),
    theme = theme(legend.position = "bottom",
                  plot.title = element_text(hjust = 0.5,
                                            size = 50,
                                            family = "Merienda",
                                            color = "black",
                                            face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5,
                                            size = 40,
                                            family = "Merienda",
                                            color = "black"),
                  plot.background = element_rect(fill = alpha("#CDF0EA",0.2) ),
                  plot.caption = element_markdown(size = 25, 
                                                  lineheight = 0.5,
                                                  family = "Montserrat")
                  )
  )

Avui = format.Date(today(), "%Y%m%d")

ggsave(glue("Gràfiques/{Avui}_Vacunació.png"),
       height = 12
       ,width = 10
       ,dpi = 300)



