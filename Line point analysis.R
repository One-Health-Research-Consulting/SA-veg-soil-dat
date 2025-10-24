library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(egg)
library(ggpubr)
library(RColorBrewer)
library(lubridate)


lp <- read.csv("./Vegetation Data/Clean Y3 Line Point Veg Data.csv")

classifications <- read.csv("./Vegetation Data/Line Point species names.csv")

classifications <- classifications%>%
  rename("class"="Grass...G..Sedge...S..Juncus...J..Forb...F..Typha...T")%>%
  mutate(class=replace(class, class == "F", "Forbes"))%>%
  mutate(class=replace(class, class == "G", "Grasses"))%>%
  mutate(class=replace(class, class == "J", "Juncus sp"))%>%
  mutate(class=replace(class, class == "S", "Sedges"))%>%
  mutate(class=replace(class, class == "T", "Rush"))%>%
  filter(!class == "")%>%
  mutate(Species = str_trim(Species, "both"))%>%
  distinct()%>%# There are some duplicates
  filter(!Species == "Schoeplectus decipiens")

#Typha capensis is cat tails = Rush


live <- c("Sa", "Sb", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10",
          "S11", "S12", "S13", "S14", "S15", "S16", "S17", "S18" , "S19", "S20", "S.")

dead <- c("Da", "Db", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10",
          "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18" , "D19", "D20", "D.")


lp$CH <- as.numeric(lp$CH)
lp$Farm_ID <- as.character(lp$Farm_ID)

lp <- lp%>%
  mutate(Species = str_trim(Species, "both"))%>%
  mutate(Species = replace(Species, Species == "Schoeplectus decipiens", "Schoenoplectus decipiens"))%>%
  rowwise()%>%
  mutate(total_live = sum(c_across(all_of(live))))%>%
  mutate(total_dead = sum(c_across(all_of(dead))))%>%
  mutate(total_strikes  = sum(c_across(cols = c(total_live, total_dead))))

non_plant <- c("Bare soil", "Litter", "Standing water")

all_types <- c("farm_bare", "farm_litter", "farm_water", "farm_live", "farm_dead")

farm_tots <- lp%>%
  group_by(Farm_ID)%>%
  summarise(nSpecies= n(),
            farm_bare = sum(Species == "Bare soil", na.rm = TRUE),
            farm_litter = sum(Species == "Litter", na.rm = TRUE),
            farm_water = sum(Species == "Standing water", na.rm = TRUE),
            farm_live = sum(total_live, na.rm = TRUE),
            farm_dead = sum(total_dead, na.rm = TRUE))%>%
  rowwise()%>%
  mutate(farm_strikes = sum(c_across(cols = c(farm_live, farm_dead))))%>%
  mutate(farm_pins = sum(c_across(all_of(all_types))))

sum_lp <- full_join(lp, farm_tots)



dens_sp <- sum_lp%>%
  group_by(Farm_ID, Species)%>%
  summarise(n= n(),
            density = sum(total_strikes)/mean(farm_pins))

lp_long <- lp%>%
  pivot_longer(cols = Sa:D20, 
               names_to = "strike", values_to = "count")%>%
  mutate(class_group = substring(strike, 2))%>%
  mutate(count = as.numeric(count))%>%
  group_by(Farm_ID, class_group)%>%
  summarise(strike_count = sum(count, na.rm = T))%>%#,  live_strike_count = 
  full_join(., farm_tots)%>%
  mutate(strike_density = strike_count/farm_strikes)%>%
  mutate(class_group = replace(class_group, class_group == "a", "0.5"))%>%
  mutate(class_group = replace(class_group, class_group == "b", "0.75"))%>%
  mutate(class_group = replace(class_group, class_group == ".", "0.25"))%>%
  mutate(class_group = as.numeric(class_group))


#data explore
boxplot(lp$CH)

#Plot Species by Average height
avg_h <- lp%>%
  filter(!is.na(CH))%>%
  group_by(Species)%>%
  summarise(n = n(),
            aheight = mean(CH, na.rm = T),
            sdheight = sd(CH, na.rm = T))%>%
  arrange(aheight)

avg_h_final <- 
  full_join(avg_h, classifications, by = "Species")%>%
  mutate(genus = str_sub(Species, 1,2),
         sp = gsub("^\\S+ ", "", Species),
  Species_short = paste0(genus, ". ", sp))%>%
  select(-genus, -sp)



avg_h_final <- dplyr::arrange(avg_h_final, class, aheight)
avg_h_final$Species_short <- factor(avg_h_final$Species_short, levels = unique(avg_h_final$Species_short))



avg_h_final$class <- factor(avg_h_final$class, levels = unique(avg_h_final$class))



heightxSp <- ggplot(avg_h_final, aes(y = aheight, x = Species_short, fill = class))+
  geom_col()+
  labs(x = "Species") + 
  #facet_wrap(~ class)
  theme_classic() +
  theme(axis.text = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 20))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

heightxSp

#sp_groups <- as.data.frame(unique(avg_h_final$Species))

#names(sp_groups) <- c("Species")



#write.csv(sp_groups, "Species for classification.csv", row.names = FALSE)


#ggexport(heightxSp, filename = "RVF Line Point heights by species.pdf", width=11, height=8)
#

###4 plots in by plant class
#forbs
height_F <- filter(avg_h_final, class == "Forbes")

height_F_final <- height_F%>%
  mutate(sdheight_min = if_else(aheight > sdheight, aheight - sdheight, 0))%>%
  arrange(aheight)
height_F_final$Species_short <- factor(height_F_final$Species_short, levels = unique(height_F_final$Species_short))


heightxF_final <- ggplot(height_F_final, aes(y = aheight, x = Species_short, fill = class))+
  geom_col()+
  geom_errorbar(aes(x=Species_short, ymin=sdheight_min, ymax=aheight+sdheight), width=0.4, position = position_dodge(.9)) +
  theme_classic() +
  scale_fill_manual(values=c("#C6DBEF")) +
  ylim(c(0,162)) +
  labs(y = "Height (cm)", title = "Forbes", x = "Species") + 
  theme(axis.text = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 15),
        legend.position = "none",
        plot.margin = unit(c(t=0,r=1,0,l = 1),"cm")) 

#Note, no error bar where there is just n= 1 for that species

heightxF_final
#Grasses
height_g <- filter(avg_h_final, class == "Grasses")

height_g_final <- height_g%>%
  mutate(sdheight_min = if_else(aheight > sdheight, aheight - sdheight, 0))%>%
  arrange(aheight)
height_g_final$Species_short <- factor(height_g_final$Species_short, levels = unique(height_g_final$Species_short))


heightxG_final <- ggplot(height_g_final, aes(y = aheight, x = Species_short, fill = class))+
  geom_col()+
  theme_classic() +
  geom_errorbar(aes(x=Species_short, ymin=sdheight_min, ymax=aheight+sdheight), width=0.4, position = position_dodge(.9)) +
  scale_fill_manual(values=c("#9ECAE1")) +
  labs(y = "Height (cm)", title = "Grasses", x = "Species") + 
  ylim(c(0,162)) +
  theme(axis.text = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 15),
        legend.position = "none",
        plot.margin = unit(c(t=0,r=1,0,l = 1),"cm")) 

heightxG_final

#Sedges
height_s <- filter(avg_h_final, class == "Sedges" | class == "Juncus sp" | class == "Rush")

height_s_final <- arrange(height_s, aheight)
height_s_final$Species_short <- factor(height_s_final$Species_short, levels = unique(height_s_final$Species_short))


heightxS_final <- ggplot(height_s_final, aes(y = aheight, x = Species_short, fill = class))+
  geom_col()+
  theme_classic() +
  scale_fill_manual(values=c("paleturquoise2", "plum2", "#2171B5")) +
  labs(y = "Height (cm)", title = "Sedges", x = "Species") + 
  ylim(c(0,162)) +
  geom_errorbar(aes(x=Species_short, ymin=aheight-sdheight, ymax=aheight+sdheight), width=0.4, position = position_dodge(.9)) +
  theme(axis.text = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 15),
        legend.position = "none") 

heightxS_final




avg_h_plot_Vert <- ggarrange(heightxF_final, heightxS_final, heightxG_final, ncol = 1, nrow = 3, 
                        labels = c("A", "B", "C"))


ggexport(avg_h_plot_Vert, filename = "RVF Line Point heights by veg class - multipanel plot_vertical.pdf", width=10, height=15)


#Strike density plot
stk_lp <- lp_long%>%
  mutate(Farm_IDs = str_sub(Farm_ID, 1,4))

strike_dens <- ggplot(data = stk_lp, aes(y = strike_density, x = class_group, colour = Farm_IDs))+
  geom_line()+
  labs(x = "Strike Density", y = "Strike Height", ) +
  theme_classic() +
  theme(axis.text = element_text(size = 10, colour = "black"),
        plot.margin = unit(c(t=1,r=1,b = 1,l = 1),"cm")) +
  guides(color = guide_legend(ncol=1),
         color=guide_legend(title="Farm"))


strike_dens

ggexport(strike_dens, filename = "RVF Line Point strike density by height", width=8, height=8)

###Strikes by date
#Date is not all in same format. Need to separate the components, then put them back together as a date.
lp_date <- lp%>%
  separate_wider_delim(iDate, delim = "/", names = c("day", "month", "year"))%>%
  mutate(day = str_pad(day, width = 2, side = "left", pad = "0"),
         month = str_pad(month, width = 2, side = "left", pad = "0"),
         year = str_sub(year, -2, -1),
         year = paste0("20", year),
         iDate = paste(year, month, day, sep = "-"))%>%
  select(-day, -month, -year)

lp_date$iDate <- as.Date(lp_date$iDate)

lp_long_date <- lp_date%>%
  pivot_longer(cols = Sa:D20, names_to = "strike", values_to = "count")%>%
  mutate(class_group = substring(strike, 2))%>%
  mutate(count = as.numeric(count))%>%
  #mutate(Month = month(iDate))%>%
  #mutate(Day = day(iDate))%>%
  #mutate(Year = year(iDate))%>%
  #mutate(day_Mo = paste(Day, Month, sep = "-"))%>%
  #mutate(DayMonth = format(as.Date(iDate), "%d-%m"))%>%
  group_by(Farm_ID, iDate, class_group)%>%
  summarise(strike_count = sum(count, na.rm = T))%>%#,  live_strike_count = 
  full_join(., farm_tots)%>%
  mutate(strike_density = strike_count/farm_strikes)%>%
  mutate(class_group = replace(class_group, class_group == "a", "0.5"))%>%
  mutate(class_group = replace(class_group, class_group == "b", "0.75"))%>%
  mutate(class_group = replace(class_group, class_group == ".", "0.25"))%>%
  mutate(class_group = as.numeric(class_group))


#lp_long_date$iDate <- as.Date(paste(lp_long_date$Year, lp_long_date$DayMonth, sep = "-"),  format = "%Y-%d-%m")



strike_dens_date <- ggplot(data = lp_long_date, aes(y = strike_density, x = iDate, colour = Farm_ID))+
  geom_point()+
  labs(x = "Date", y = "Strike Density") +
  theme_classic() +
  theme(axis.text = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(t=1,r=1,b = 1,l = 1),"cm")) +
  guides(color = guide_legend(ncol=1),
         color=guide_legend(title="Farm"))

strike_dens_date

lp_long_date_ht <- lp_long_date%>%
  filter(!strike_count == 0)

strike_height_date <- ggplot(data = lp_long_date_ht, aes(y = class_group, x =  iDate, colour = Farm_ID))+
  geom_point()+
  labs(y = "Strike Height", x = "Date" ) +
  theme_classic() +
  theme(axis.text = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(t=1,r=1,b = 1,l = 1),"cm")) +
  guides(color = guide_legend(ncol=1),
         color=guide_legend(title="Farm"))

strike_height_date

comb_plot <- ggarrange(strike_dens_date, strike_height_date,
                       labels = c("A", "B"),
                       nrow = 2,
                       common.legend = TRUE,
                       legend = "right")


ggexport(comb_plot, filename = "2017 RVF Line Point strike density and height by date.pdf", width=7, height=8)
