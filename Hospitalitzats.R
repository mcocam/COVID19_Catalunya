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
library(rvest)
library(janitor)
library(showtext)

# Font-family
font_add_google("Raleway")
showtext_auto()


# Working Directory = Mateixa localització de l'script (només funciona amb RStudio)
# Si no s'utilitza RStudio, eliminar i establir-ho maualment
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


ggplot_font_size = 40
gglabel_font_size = 13

patchwork_title = 70
patchwork_subtitle = 60
patchwork_caption = 40


# Dades UCI
UCI_html = read_html("https://dadescovid.cat/critics")%>%
  html_table(dec = ",")

DataUCI = data.frame(UCI_html)
DataUCI[1,] = c("Data", "BCN", "CT", "CC", "G", "LL", "MN", "MS", "TE", "Total")

Data_UCI_2 = DataUCI%>%
  row_to_names(row_number = 1)%>%
  mutate(Data = dmy(Data),
         Total = as.numeric(Total))


#Hosp convencional
Hosp_html = read_html("https://dadescovid.cat/diari")%>%
  html_table(dec = ",")

DataHOSP = data.frame(Hosp_html)
names(DataHOSP) = c("Data",
                    "PCRTA",
                    "PCR_fetes",
                    "TA_fets",
                    "Perc_Positives",
                    "Vacunats1",
                    "Vacunats2",
                    "Ingressats",
                    "Defuncions")

DataHOSP_2 = DataHOSP%>%
  mutate(Data = gsub('\\*', "", Data))%>%
  mutate(Data = dmy(Data),
         Ingressats = sub("\\.", "", Ingressats),
         PCRTA = sub("\\.", "", PCRTA))%>%
  mutate(Ingressats = as.numeric(Ingressats),
         PCRTA = as.numeric(PCRTA))



#Hospitalitzats

H_totals = DataHOSP_2%>%
  select(Data, Ingressats)%>%
  rename(dates = Data)%>%
  filter(dates >= dmy("01/07/2020"))%>%
  arrange(dates)%>%
  mutate(Dif = Ingressats-lag(Ingressats))%>%
  mutate(Label_GG = paste0(format(Ingressats, big.mark = " "),
                           " (", ifelse(Dif>0, paste0("+", Dif), Dif),
                           ")")
  )

UCI_totals = Data_UCI_2%>%
  select(Data, Total)%>%
  rename(dates = Data,
         UCI = Total)%>%
  filter(dates >= dmy("01/07/2020"))%>%
  arrange(dates)%>%
  mutate(Dif = UCI-lag(UCI))%>%
  mutate(Label_GG = paste0(format(UCI, big.mark = " "),
                           " (", ifelse(Dif>0, paste0("+", Dif), Dif),
                           ")")
  )

UCI_regions = Data_UCI_2%>%
  select(-Total)%>%
  rename(dates = Data)%>%
  filter(dates >= dmy("01/07/2020"))%>%
  pivot_longer(-dates, names_to = "RS", values_to = "UCI")%>%
  mutate(RS = case_when(RS == "BCN" ~ "Barcelona",
                        RS == "CT" ~ "Tarragona",
                        RS == "CC" ~ "Cat. Central",
                        RS == "G" ~ "Girona",
                        RS == "LL" ~ "Lleida",
                        RS == "MN" ~ "Metro Nord",
                        RS == "MS" ~ "Metro Sud",
                        RS == "TE" ~ "T. de l'Ebre"))%>%
  mutate(UCI = as.numeric(UCI))%>%
  arrange(RS, dates)%>%
  group_by(RS)%>%
  mutate(Dif = UCI-lag(UCI))%>%
  mutate(Label_GG = paste0(format(UCI, big.mark = " "),
                           " (", ifelse(Dif>0, paste0("+", Dif), Dif),
                           ")")
  )%>%
  ungroup()



gg_hospitalitzats = ggplot(data = H_totals,
           aes(x = dates, y = Ingressats))+
  geom_line()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " "))+
  scale_x_date(breaks = "1 month",
               date_labels = "%m/%y")+
  geom_text_repel(data = subset(H_totals, dates == max(dates)),
                  aes(x = dates, y = Ingressats, label = Label_GG),
                  direction = "y",
                  segment.curvature = 0.4,
                  segment.linetype = 2,
                  nudge_x = -1000,
                  bg.color = "white",
                  bg.r = 0.15,
                  segment.color = "#925651",
                  size = gglabel_font_size
  )+
  labs(x = "",
       y = "",
       title = "Hospitalitzats per COVID-19")+
  theme_hc()+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(family = "Raleway", size = ggplot_font_size),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_blank(),
        legend.position = "bottom",
        title = element_text(hjust = 0,
                             color = "#341B49"))

gg_uci = ggplot(data = UCI_totals,
           aes(x = dates, y = UCI))+
  geom_line()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " "))+
  scale_x_date(breaks = "1 month",
               date_labels = "%m/%y")+
  geom_text_repel(data = subset(UCI_totals, dates == max(dates)),
                  aes(x = dates, y = UCI, label = Label_GG),
                  direction = "y",
                  segment.curvature = 0.4,
                  segment.linetype = 2,
                  nudge_x = -1000,
                  bg.color = "white",
                  bg.r = 0.15,
                  segment.color = "#925651",
                  size = gglabel_font_size
  )+
  labs(x = "",
       y = "",
       title = "Pacients a UCI per COVID-19")+
  theme_hc()+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(family = "Raleway", size = ggplot_font_size),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_blank(),
        legend.position = "bottom",
        title = element_text(hjust = 0,
                             color = "#341B49"))

gg_uci_regio = ggplot(data = UCI_regions,
           aes(x = dates, y = UCI))+
  geom_line()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " "))+
  scale_x_date(breaks = "2 month",
               date_labels = "%m/%y")+
  geom_text_repel(data = subset(UCI_regions, dates == max(dates)),
                  aes(x = dates, y = UCI, label = Label_GG),
                  direction = "y",
                  segment.curvature = 0.4,
                  segment.linetype = 2,
                  nudge_x = -1000,
                  bg.color = "white",
                  bg.r = 0.15,
                  segment.color = "#925651",
                  size = gglabel_font_size
  )+
  labs(x = "",
       y = "",
       title = "Pacients a UCI per COVID-19, per Regió de l'hospital")+
  theme_hc()+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(family = "Raleway", size = ggplot_font_size),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_blank(),
        legend.position = "bottom",
        title = element_text(hjust = 0,
                             color = "#341B49"))+
  facet_wrap(~RS, scales = "free_y")


DateMin = format.Date(min(H_totals$dates), "%d/%m/%y")
DateMax = format.Date(max(H_totals$dates), "%d/%m/%y")

Z_casos = gg_hospitalitzats/gg_uci/gg_uci_regio+
  plot_annotation(title = "Hospitalitzats per COVID-19:",
                  subtitle = glue("{DateMin} al {DateMax}"),
                  caption = "@marc_coca | Font: www.dadescovid.cat",
                  theme = theme(plot.title = element_text(family = "Raleway",
                                                          face = "bold",
                                                          colour = "#8C222C",
                                                          size = patchwork_title),
                                plot.subtitle = element_text(size = patchwork_subtitle),
                                plot.background = element_rect(fill = "#FFFCF4"),
                                plot.caption = element_text(size = patchwork_caption),
                                plot.margin = unit(c(0.5,0.4,0.5,0.1), "cm")))+
  plot_layout(heights = c(1,1,2))



avui = today()
avui_l = format(avui, "%Y%m%d")

ggsave(glue("Gràfiques/{avui_l}_Hospitalitzats.png"),
       #DIN A4
       width = 210, 
       height = 297, 
       units = "mm",
       dpi = 350)
