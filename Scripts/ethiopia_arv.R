install.packages("gisr")
install.packages("Wavelength")
install.packages("glitr")

library(glitr)
library(glamr)
library(gisr)
library(Wavelength)
library(ICPIutilities)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
#library(patchwork)
library(ggtext)
library(here)
library(readxl)
library(ggplot2)
library(glitr)


data<- "Data/Ethiopia ARV MOS Map Sep-29-20-190637.xlsx"

df<-read_xlsx(data, skip=7) %>%
  rename("Aug 20"="44044...4",
         "Sep 20"="44075...5",
         "Oct 20"="44105...6",
         "Nov 20"="44136...7",
         "Dec 20"="44166...8",
         "Jan 21"="44197...9",
         "Feb 21"="44228...10",
         "Mar 21"="44256...11",
         "Apr 21"="44287...12",
         "May 21"="44317...13",
         "June 21"="44348...14",
         "July 21"="44378...15",
         "Regimen"="Product Name") %>% 
  select("Regimen", "Aug 20","Sep 20","Oct 20","Nov 20","Dec 20",
         "Jan 21","Feb 21","Mar 21","Apr 21","May 21", "June 21", "July 21") %>%
  filter(Regimen %in% c("TDF +3TC +DTG (300+ 300+ 50) mg – 30 Tablet", "TDF+ 3TC + EFV – (300 + 300 + 400)mg – Tablet",
                        "TDF+3TC+DTG(300+300+50) mg - 90 Tablet", "TDF+3TC+DTG(300+300+50) mg - 180 Tablet")) %>% 
  drop_na("Mar 21") %>% 
  pivot_longer(cols="Aug 20":"July 21", names_to="Month", values_to="MOS") %>% 
  mutate(MOS = as.numeric(MOS),
         MOS=round(MOS, digits=1),
         Regimen=case_when(
           str_detect(Regimen, "180 Tablet") ~ "TLD 180-count",
           str_detect(Regimen, "90 Tablet") ~ "TLD 90-count",
           str_detect(Regimen, "EFV") ~ "TLE",
           str_detect(Regimen, "30 Tablet") ~ "TLD 30-count",
           TRUE~"NA"),
         regimen_color=case_when(
           Regimen=="TLD 180-count" ~USAID_medblue,
           Regimen=="TLD 90-count" ~USAID_ltblue,
           Regimen=="TLD 30-count" ~USAID_blue,
           Regimen=="TLE" ~USAID_dkred,
           TRUE ~ grey40k)) %>%
  glimpse()

df %>% 
  arrange(MOS) %>% 
  mutate(Month=factor(Month, levels=c("Aug 20", "Sep 20", "Oct 20", "Nov 20", "Dec 20",
                                      "Jan 21", "Feb 21", "Mar 21", "Apr 21", "May 21", "June 21",
                                      "July 21"))) %>% 
  ggplot(aes(x=Month, y=MOS, group=regimen_color, color=regimen_color))+
  geom_line(size=1, vjust=1.25, fontface="bold")+
  geom_point(aes(fill=regimen_color,), shape=21, color=grey80k, size=12, stroke=1)+
  geom_text(aes(label=MOS), size=12/.pt, color="white", family="Source Sans Pro")+
  facet_wrap(~Regimen, nrow=2)+
  labs(x = NULL, y = "Months of Stock", color = NULL,
       title = "Months of Stock by ARV Regimen", family="Source Sans Pro")+
  si_style_xgrid()+
  theme(legend.position="none")+
  scale_color_identity()+
  scale_fill_identity()

ggsave("mos_ethiopia.png",
       height = 8,
       width = 14)
 

########################################## PEDS
df_peds<-read_xlsx(data, skip=7) %>%
  rename("Aug 20"="44044...4",
         "Sep 20"="44075...5",
         "Oct 20"="44105...6",
         "Nov 20"="44136...7",
         "Dec 20"="44166...8",
         "Jan 21"="44197...9",
         "Feb 21"="44228...10",
         "Mar 21"="44256...11",
         "Apr 21"="44287...12",
         "May 21"="44317...13",
         "June 21"="44348...14",
         "July 21"="44378...15",
         "Regimen"="Product Name") %>% 
  select("Regimen", "Aug 20","Sep 20","Oct 20","Nov 20","Dec 20",
         "Jan 21","Feb 21","Mar 21","Apr 21","May 21", "June 21", "July 21") %>%
  filter(Regimen %in% c("Lopinavir + Ritonavir – (200mg + 50mg) – Tablet", "Lopinavir 100mg+ Ritonavir 25mg Tablet",
                        "Lopinavir + Ritonavir - (40mg + 10mg) - Pellet", "Abacavir 120mg + Lamivudine 60mg Tablet"
                        )) %>% 
  drop_na("Mar 21") %>% 
  pivot_longer(cols="Aug 20":"July 21", names_to="Month", values_to="MOS") %>% 
  mutate(MOS = as.numeric(MOS),
         MOS=round(MOS, digits=1),
         Regimen=case_when(
           str_detect(Regimen, "– Tablet") ~ "LPV/r 250",
           str_detect(Regimen, "25mg") ~ "LPV/r 125",
           str_detect(Regimen, "Pellet") ~ "LPV/r 40/10 Pellet",
           str_detect(Regimen, "Abacavir 120mg") ~ "Abacavir/Lamivudine 120/60",
           TRUE~"NA"),
         regimen_color=case_when(
           Regimen=="LPV/r 250" ~USAID_medblue,
           Regimen=="LPV/r 125" ~USAID_ltblue,
           Regimen=="LPV/r 40/10 Pellet" ~USAID_blue,
           Regimen=="Abacavir/Lamivudine 120/60" ~USAID_dkred,
           TRUE ~ grey40k)) %>%
  drop_na(MOS)
  glimpse()

df_peds %>% 
  arrange(MOS) %>% 
  mutate(Month=factor(Month, levels=c("Aug 20", "Sep 20", "Oct 20", "Nov 20", "Dec 20",
                                      "Jan 21", "Feb 21", "Mar 21", "Apr 21", "May 21", "June 21",
                                      "July 21"))) %>% 
  ggplot(aes(x=Month, y=MOS, group=regimen_color, color=regimen_color))+
  geom_line(size=1, vjust=1.25, fontface="bold")+
  geom_point(aes(fill=regimen_color,), shape=21, color=grey80k, size=12, stroke=1)+
  geom_text(aes(label=MOS), size=12/.pt, color="white", family="Source Sans Pro")+
  facet_wrap(~Regimen, nrow=2)+
  labs(x = NULL, y = "Months of Stock", color = NULL,
       title = "Months of Stock by ARV Regimen - Pediatrics", family="Source Sans Pro")+
  si_style_xgrid()+
  theme(legend.position="none")+
  scale_color_identity()+
  scale_fill_identity()

ggsave("mos_ethiopia_peds.png",
       height = 8,
       width = 14)