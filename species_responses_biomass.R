#Max Zaret
#e245 nutrients x consumer removal#
#Life history trade offs in grassland plant species
#2-13-2024

#####Start####
rm(list=ls())

library(tidyverse)
library(smatr)
library(nlme)
library(ggsci)
library(lsmeans)
library(ggpubr)

standard_error <- function(x) sd(x) / sqrt(length(x))

getwd()
setwd("~/Documents/Research/Nutrients_Consumer_Removal/CH III/code") #change to your wd

data <- read.csv("2019-2021 E245 Enemy Removal Aboveground Biomass.csv") #this and other data files are on github and on EDI site

#remove non-species-specific biomass from data frame (litter and moss)
data <- data %>%
  filter(Species != "Miscellaneous litter") %>%
  filter(Species != "Miscellaneous herbs") %>%
  filter(Species != "miscellaneous litter") %>%
  filter(Species != "1st Year Woody") %>%
  filter(Species != "Miscellaneous woody plants") %>%
  filter(Species != "Mosses") %>%
  mutate(Treatment = as.factor(TreatmentCode)) %>%
  mutate(Fertilizer = ifelse(FertTrt == "n", 0 ,1 ), #defining fertilizer treatment
         Fertilizer.f = as.factor(Fertilizer)) %>%
  unite("Sub", Plot:Subplot, remove=FALSE) %>% #to merge with data plan
  select(Year, Plot, Sub, Treatment, Fertilizer.f, Species, Mass.g.m.2.)

#e245 experiment design file
#need to include because it has experimental block information
plan <- read.csv("e245dataplan.csv")

data <- merge(data, plan, by=c("Treatment", "Fertilizer.f", "Plot", "Sub"))

data <- data %>%
  select(Year, Block, Plot, Sub, Treatment, Fertilizer.f, Species, Mass.g.m.2.) %>%
  arrange(Year, Plot, Sub) %>%
  filter(row_number() != 3028) #remove duplicate row

#Quickly rename Treatments to have better formated names#
data <- data %>%
  ungroup() %>%
  mutate(Treatment = case_when(Treatment == "AllPesticides" ~ "All Removal",
                               Treatment == "FoliarFungicide" ~ "Foliar Fungicide",
                               Treatment == "Fenced" ~ "Fence",
                               Treatment == "SoilDrenchFungicide" ~ "Soil Fungicide",
                               Treatment == "Insecticide" ~ "Insecticide",
                               Treatment == "Control" ~ "Control")) %>%
  mutate(Treatment = as.factor(Treatment))
                              
levels(data$Treatment)
data$Treatment <- relevel(data$Treatment, ref='All Removal')
data$Treatment <- relevel(data$Treatment, ref='Fence')
data$Treatment <- relevel(data$Treatment, ref='Insecticide')
data$Treatment <- relevel(data$Treatment, ref='Soil Fungicide')
data$Treatment <- relevel(data$Treatment, ref='Foliar Fungicide')
data$Treatment <- relevel(data$Treatment, ref='Control')

#####Nutrient Addition#####
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

nutrient <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "1")

compare <- merge(nutrient, control, by=c("Year", "Block", "Species"), all=TRUE)

#set dummy values for species that invade new plots or go extinct in other plots
#value is based on 1/2 minimum biomass observed

compare <- compare %>%
  mutate(Mass.g.m.2. = replace_na(Mass.g.m.2., 0.01) ) %>%
  mutate(control_mass = replace_na(control_mass, 0.01))

#calculate log response ratio 
Nutrient_LRR <- compare %>%
  mutate(NPK_LRR = log(Mass.g.m.2./control_mass)) %>%
  select(Year, Block, Species, NPK_LRR)

ggplot(Nutrient_LRR, aes(NPK_LRR)) +
  geom_histogram()

####Foliar Fungicide####
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

FF <- data %>%
  filter(Treatment == "Foliar Fungicide") %>%
  filter(Fertilizer.f == "0")

compare <- merge(FF, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  mutate(Mass.g.m.2. = replace_na(Mass.g.m.2., 0.01) ) %>%
  mutate(control_mass = replace_na(control_mass, 0.01))

FF_LRR <- compare %>%
  mutate(Consumer_LRR = log(Mass.g.m.2./control_mass)) %>%
  select(Year, Block, Species, Treatment, Consumer_LRR)

#distribution of LRRs
ggplot(FF_LRR, aes(Consumer_LRR)) +
  geom_histogram()


FF_set <- merge(FF_LRR, Nutrient_LRR, by=c("Year", "Block", "Species"), all=TRUE)

FF_set <- FF_set %>%
  filter(Consumer_LRR != NPK_LRR) %>% #remove comparisons that compare two dummy variables (not meaningful comparison)
  mutate(Treatment = "Foliar Fungicide",
         Treatment = as.factor(Treatment)) %>%
  select(Year, Block, Treatment, Species, Consumer_LRR, NPK_LRR)

ggplot(FF_set, aes(NPK_LRR, Consumer_LRR, col=Treatment)) +
  geom_point() +
  geom_smooth(method="lm", col="black")

summary(sma(formula = Consumer_LRR ~ NPK_LRR * Year, data=FF_set, method="SMA"))

summary(lm(Consumer_LRR ~ Species, data=FF_set))

#use Nutrient / FF sections as template for rest of code (needs to be updated)

####Fencing####
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

Fenced <- data %>%
  filter(Treatment == "Fence") %>%
  filter(Fertilizer.f == "0")

compare <- merge(Fenced, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  mutate(Mass.g.m.2. = replace_na(Mass.g.m.2., 0.01) ) %>%
  mutate(control_mass = replace_na(control_mass, 0.01))

Fenced_LRR <- compare %>%
  mutate(Consumer_LRR = log(Mass.g.m.2./control_mass)) %>%
  select(Year, Block, Species, Treatment, Consumer_LRR)

#distribution of LRRs
ggplot(Fenced_LRR, aes(Consumer_LRR)) +
  geom_histogram()


Fenced_set <- merge(Fenced_LRR, Nutrient_LRR, by=c("Year", "Block", "Species"), all=TRUE)

Fenced_set <- Fenced_set %>%
  filter(Consumer_LRR != NPK_LRR) %>%
  mutate(Treatment = "Fence",
         Treatment = as.factor(Treatment)) %>%
  select(Year, Block, Treatment, Species, Consumer_LRR, NPK_LRR)

ggplot(Fenced_set, aes(Consumer_LRR, NPK_LRR, col=Treatment)) +
  geom_point() +
  geom_smooth(method="lm")

sma(formula = NPK_LRR ~ Consumer_LRR * Year, data=Fenced_set, method="SMA")

#####Soil Fungicide#####
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

SF <- data %>%
  filter(Treatment == "Soil Fungicide") %>%
  filter(Fertilizer.f == "0")

compare <- merge(SF, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  mutate(Mass.g.m.2. = replace_na(Mass.g.m.2., 0.01) ) %>%
  mutate(control_mass = replace_na(control_mass, 0.01))

SF_LRR <- compare %>%
  mutate(Consumer_LRR = log(Mass.g.m.2./control_mass)) %>%
  select(Year, Block, Species, Treatment, Consumer_LRR)

#distribution of LRRs
ggplot(SF_LRR, aes(Consumer_LRR)) +
  geom_histogram()


SF_set <- merge(SF_LRR, Nutrient_LRR, by=c("Year", "Block", "Species"), all=TRUE)

SF_set <- SF_set %>%
  filter(Consumer_LRR != NPK_LRR) %>%
  mutate(Treatment = "Soil Fungicide",
         Treatment = as.factor(Treatment)) %>%
  select(Year, Block, Treatment, Species, Consumer_LRR, NPK_LRR)

ggplot(SF_set, aes(Consumer_LRR, NPK_LRR, col=Treatment)) +
  geom_point() +
  geom_smooth(method="lm")

sma(formula = NPK_LRR ~ Consumer_LRR * Year, data=SF_set, method="SMA")

#####Insecticide#####
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

I <- data %>%
  filter(Treatment == "Insecticide") %>%
  filter(Fertilizer.f == "0")

compare <- merge(I, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  mutate(Mass.g.m.2. = replace_na(Mass.g.m.2., 0.01) ) %>%
  mutate(control_mass = replace_na(control_mass, 0.01))

I_LRR <- compare %>%
  mutate(Consumer_LRR = log(Mass.g.m.2./control_mass)) %>%
  select(Year, Block, Species, Treatment, Consumer_LRR)

#distribution of LRRs
ggplot(I_LRR, aes(Consumer_LRR)) +
  geom_histogram()


I_set <- merge(I_LRR, Nutrient_LRR, by=c("Year", "Block", "Species"), all=TRUE)

I_set <- I_set %>%
  filter(Consumer_LRR != NPK_LRR) %>%
  mutate(Treatment = "Insecticide",
         Treatment = as.factor(Treatment)) %>%
  select(Year, Block, Treatment, Species, Consumer_LRR, NPK_LRR)

ggplot(I_set, aes(Consumer_LRR, NPK_LRR, col=Treatment)) +
  geom_point() +
  geom_smooth(method="lm")

sma(formula = NPK_LRR ~ Consumer_LRR * Year, data=I_set, method="SMA")

####AllPesticides####
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

All <- data %>%
  filter(Treatment == "All Removal") %>%
  filter(Fertilizer.f == "0")

compare <- merge(All, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  mutate(Mass.g.m.2. = replace_na(Mass.g.m.2., 0.01) ) %>%
  mutate(control_mass = replace_na(control_mass, 0.01))

All_LRR <- compare %>%
  mutate(Consumer_LRR = log(Mass.g.m.2./control_mass)) %>%
  select(Year, Block, Species, Treatment, Consumer_LRR)

#distribution of LRRs
ggplot(All_LRR, aes(Consumer_LRR)) +
  geom_histogram()


All_set <- merge(All_LRR, Nutrient_LRR, by=c("Year", "Block", "Species"), all=TRUE)

All_set <- All_set %>%
  filter(Consumer_LRR != NPK_LRR) %>%
  mutate(Treatment = "All Removal",
         Treatment = as.factor(Treatment)) %>%
  select(Year, Block, Treatment, Species, Consumer_LRR, NPK_LRR)

ggplot(All_set, aes(Consumer_LRR, NPK_LRR, col=Treatment)) +
  geom_point() +
  geom_smooth(method="lm")

sma(formula = NPK_LRR ~ Consumer_LRR * Year, data=All_set, method="SMA")

#Combine####
Consumers_set <- FF_set %>%
  rbind(Fenced_set) %>%
  rbind(SF_set) %>%
  rbind(I_set) %>%
  rbind(All_set) %>% 
  mutate(Species = as.factor(Species)) %>%
  group_by(Year, Treatment, Species) %>% #obtain species responses averaged across blocks
  summarize(Consumer_LRR = mean(Consumer_LRR, na.rm = TRUE), 
             NPK_LRR = mean(NPK_LRR, na.rm = TRUE))

levels(Consumers_set$Species)
summary(Consumers_set)

#this plot below is Fig2
ggplot(Consumers_set, aes(NPK_LRR, Consumer_LRR, col=Treatment)) +
  geom_point(alpha=0.25) +
  geom_smooth(method="lm", se=FALSE) +
  geom_hline(yintercept = c(-0.693, 0.693), lty =2) +
  geom_vline(xintercept = c(-0.693, 0.693), lty =2) +
  #geom_hline(yintercept = ( 0), lty =2) +
  #geom_vline(xintercept = ( 0), lty =2) +
  labs(x="Response to Nutrients (LRR)", y="Response to \nConsumer Removal (LRR)") +
  scale_color_locuszoom() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.20, 'cm'), #change legend key size
        legend.key.height = unit(0.20, 'cm'), #change legend key height
        legend.key.width = unit(0.20, 'cm'), #change legend key width
        legend.title = element_text( size=9, face="bold"), #change legend title font size
        legend.text = element_text(size=9)) +
  coord_fixed()

#supplmental figure showing variability across years
ggplot(Consumers_set, aes(NPK_LRR, Consumer_LRR, col=Treatment)) +
  geom_point(alpha=0.25) +
  geom_smooth(method="lm", se=FALSE) +
  geom_hline(yintercept = c(-0.693, 0.693), lty =2) +
  geom_vline(xintercept = c(-0.693, 0.693), lty =2) +
  #scale_y_continuous(limits=c(-10,10)) +
  #scale_x_continuous(limits=c(-10,10)) +
  scale_color_locuszoom() +
  coord_fixed() +
  facet_wrap(~Year) +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.20, 'cm'), #change legend key size
        legend.key.height = unit(0.20, 'cm'), #change legend key height
        legend.key.width = unit(0.20, 'cm'), #change legend key width
        legend.title = element_text( size=9, face="bold"), #change legend title font size
        legend.text = element_text(size=9))

#Does relationship between C_LRR and NPK_LRR differ by consumer type?
sma(formula = Consumer_LRR ~ NPK_LRR * Treatment, data=Consumers_set, method="SMA")
#no

#Does elevation differ by consumer type?
sma(formula = Consumer_LRR ~ NPK_LRR + Treatment, data=Consumers_set, method="SMA")
#no

#Does relationship between C_LRR and NPK_LRR vary by year of sampling?
sma(formula = Consumer_LRR ~ NPK_LRR * Year, data=Consumers_set, method="SMA")
#yes
summary(sma(formula = Consumer_LRR ~ NPK_LRR * Year, data=Consumers_set, method="SMA"))

summary(sma(formula = Consumer_LRR ~ NPK_LRR + Year, data=Consumers_set, method="SMA"))


#Summary output for each treatment
summary(sma(formula = Consumer_LRR ~ NPK_LRR * Treatment, data=Consumers_set, method="SMA")
)


summary(sma(formula = Consumer_LRR ~ NPK_LRR, data=Consumers_set, method="SMA"))
        
#Do species responses to consumer removal vary by type of consumer removal?
anova(lm(Consumer_LRR ~ Species * Treatment * Year, data=Consumers_set))

#no species respond the same regardless of treatment
#Consumer_LRR do vary by species, year, and species responses vary across years

#Does relationship between C_LRR and NPK_LRR vary by Species?
anova(lm(Consumer_LRR ~ NPK_LRR * Treatment * Species, data=Consumers_set))
summary(lm(Consumer_LRR ~ NPK_LRR * Treatment * Species, data=Consumers_set))

#distribution of species responses to consumers broadly
ggplot(Consumers_set, aes(Consumer_LRR, fill=Treatment)) +
  geom_histogram() +
  scale_fill_locuszoom() +
  labs(y="Frequency")

###Define tradeoffs#####
Consumers_set <- Consumers_set %>%
  mutate(Nutrient_Response = 
           case_when(NPK_LRR >= 0.693 ~ "nutrient only", #defining responses based on an effect if something doubles or halves in response
                     NPK_LRR <= -0.693 ~ "nutrient only",
                     NPK_LRR < 0.693 & NPK_LRR > -0.693 ~ "no response")) %>%
  mutate(Consumer_Response = 
           case_when(Consumer_LRR >= 0.693 ~ "consumer only",
                     Consumer_LRR <= -0.693 ~ "consumer only",
                     Consumer_LRR < 0.693 & Consumer_LRR > -0.693 ~ "no response")) %>%
  mutate(Response = 
           case_when(Consumer_Response == "consumer only" & Nutrient_Response == "nutrient only" ~ "tradeoff",
                     Consumer_Response == "consumer only" & Nutrient_Response == "no response"   ~ "consumer only",
                     #Consumer_Response == "consumer only" & Nutrient_Response == "absent" ~ "consumer only",
                     Consumer_Response == "no response" & Nutrient_Response == "nutrient only"   ~ "nutrient only",
                     #Consumer_Response == "absent" & Nutrient_Response == "nutrient only" ~ "nutrient only",
                     Consumer_Response == "no response" & Nutrient_Response == "no response" ~ "no response")) %>%
  #Consumer_Response == "absent" & Nutrient_Response == "no response" ~ "no response",
  #Consumer_Response == "no response" & Nutrient_Response == "absent" ~ "no response")) %>%
  mutate(Tradeoff = 
           case_when(Response == "tradeoff" & Consumer_LRR > 0 & NPK_LRR > 0 ~ "growth-defense",
                     Response == "tradeoff" & Consumer_LRR < 0 & NPK_LRR < 0 ~ "growth-defense",
                     Response == "tradeoff" & Consumer_LRR > 0 & NPK_LRR < 0 ~ "competition-defense",
                     Response == "tradeoff" & Consumer_LRR < 0 & NPK_LRR > 0 ~ "competition-defense",
                     TRUE ~ "NA")) %>%
  ungroup() %>%
  mutate(Response = as.factor(Response),
         Tradeoff = as.factor(Tradeoff))

levels(Consumers_set$Response)

Consumers_set$Response <- relevel(Consumers_set$Response, ref='tradeoff')
Consumers_set$Response <- relevel(Consumers_set$Response, ref='nutrient only')
Consumers_set$Response <- relevel(Consumers_set$Response, ref='consumer only')
Consumers_set$Response <- relevel(Consumers_set$Response, ref='no response')

#plot below is figure 3
Consumers_set %>%
  filter(Response != "NA") %>%
  ggplot(., aes(Response)) +
  geom_histogram( stat="count", aes(fill=Tradeoff)) +
  labs(y="Number of species responses", x="") +
  scale_fill_manual(breaks = c("competition-defense", "growth-defense"),
                    values = c("dark orange", "dark green", "black")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=9.5, face="bold", color="black"),
        panel.grid.major.x = element_blank()) +
  theme(legend.position = c(0.5, 0.8)) +
  theme(legend.key.size = unit(0.3, 'cm'))

#supplemental figure looking at variability across different pesticide/fence treatments
Consumers_set %>%
  filter(Response != "NA") %>%
  ggplot(., aes(Response)) +
  geom_histogram( stat="count", aes(fill=Tradeoff)) +
  labs(y="Species response", x="") +
  scale_fill_manual(breaks = c("competition-defense", "growth-defense"),
                    values = c("dark orange", "dark green", "black")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=9.5, face="bold", color="black"),
        panel.grid.major.x = element_blank()) +
  facet_wrap(~Treatment, ncol=5, nrow=1)

summary(Consumers_set$Response)

#Want to know if growth-defense is more numerious thant competition-defense responses
#create contingency table in order to do chi square tests
cont <- Consumers_set %>%
  filter(Tradeoff != "NA") %>%
  select(Year, Treatment, Species, Tradeoff) %>%
  mutate(Tradeoff = as.character(Tradeoff)) %>%
  mutate(Tradeoff = as.factor(Tradeoff))

summary(cont) 

cont <- as.data.frame(table(cont))

cont <- cont %>%
  pivot_wider(names_from = Tradeoff, values_from = Freq)

chisq.test(cont$`competition-defense`, cont$`growth-defense`)

#Functional Groups####
#read in species info sheet
species <- read.csv("cc_plant_species.csv") #find on cedar creek LTER website or github

species <- species %>%
  select(Species, Functional.group, Duration, Family)

Consumers_set <- merge(Consumers_set, species, by="Species")

Consumers_set <- Consumers_set %>%
  mutate(Functional.group =
           case_when(Species == "Cyperus sp." ~ "C4",
                     Species == "Carex sp." ~ "C3",
                     TRUE ~ Functional.group)) %>%
  mutate(Functional.group = ifelse(Functional.group == "C3", "C3 Grass",
                                   ifelse(Functional.group == "C4", "C4 Grass",
                                          ifelse(Functional.group == "F", "Forbs",
                                                 ifelse(Functional.group == "L", "Legumes",1 )))))

#Functional groups#
Consumers_set %>%
  filter(Functional.group != "1") %>% #remove few woody species#
  ggplot(., aes(NPK_LRR, Consumer_LRR, col=Functional.group)) +
  geom_point(alpha=0.25) +
  geom_smooth(method="lm", se=FALSE) +
  geom_hline(yintercept = c(-0.693, 0.693), lty =2) +
  geom_vline(xintercept = c(-0.693, 0.693), lty =2) +
  #scale_y_continuous(limits=c(-10,10)) +
  #scale_x_continuous(limits=c(-10,10)) +
  scale_color_uchicago() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.20, 'cm'), #change legend key size
        legend.key.height = unit(0.20, 'cm'), #change legend key height
        legend.key.width = unit(0.20, 'cm'), #change legend key width
        legend.title = element_text( size=9, face="bold"), #change legend title font size
        legend.text = element_text(size=9))

#below is plot for figure4
Consumers_set %>%
  filter(Functional.group != "1") %>% #remove few woody species#
  ggplot(., aes(NPK_LRR, Consumer_LRR, col=Treatment)) +
  geom_point(alpha=0.25) +
  geom_smooth(method="lm", se=FALSE) +
  #geom_hline(yintercept = c(-0.693, 0.693), lty =2) +
  #geom_vline(xintercept = c(-0.693, 0.693), lty =2) +
  geom_hline(yintercept = ( 0), lty =2) +
  geom_vline(xintercept = ( 0), lty =2) +
  #scale_y_continuous(limits=c(-10,10)) +
  #scale_x_continuous(limits=c(-10,10)) +
  labs(x="Response to Nutrients (LRR)", y="Response to \nConsumer Removal (LRR)") +
  scale_color_locuszoom() +
  coord_fixed() + 
  facet_wrap(~Functional.group) +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.20, 'cm'), #change legend key size
        legend.key.height = unit(0.20, 'cm'), #change legend key height
        legend.key.width = unit(0.20, 'cm'), #change legend key width
        legend.title = element_text( size=9, face="bold"), #change legend title font size
        legend.text = element_text(size=9))

Consumers_set2 <- Consumers_set %>%
  filter(Functional.group != "1") #remove 1 woody species

#does C_LRR and NPK_LRR relationship vary by functional group?
summary(sma(formula = Consumer_LRR ~ NPK_LRR * Functional.group, data=Consumers_set2, method="SMA"))

summary(sma(formula = Consumer_LRR ~ NPK_LRR + Functional.group, data=Consumers_set2, method="SMA", type="elevation"))

summary(lme(fixed=Consumer_LRR ~ Functional.group,
          random= ~1 | Year,
          data=Consumers_set2))

anova(lme(fixed=Consumer_LRR ~ Functional.group * Treatment,
            random= ~1 | Year,
            data=Consumers_set2))

summary(lme(fixed=NPK_LRR ~ Functional.group,
          random= ~1 | Year,
          data=Consumers_set2))

anova(lme(fixed=NPK_LRR ~ Functional.group,
          random= ~1 | Year,
          data=Consumers_set2))

lsmeans(lme(fixed=Consumer_LRR ~ Functional.group,
            random= ~1 | Year,
            data=Consumers_set2),
        pairwise ~ Functional.group, type="Tukey")

lsmeans(lme(fixed=NPK_LRR ~ Functional.group,
            random= ~1 | Year,
            data=Consumers_set2),
        pairwise ~ Functional.group, type="Tukey")

x <- Consumers_set2 %>%
  group_by(Functional.group) %>%
  summarize(mean = mean(Consumer_LRR),
            se = 1.96 * standard_error(Consumer_LRR)) %>%
  ggplot(., aes(Functional.group, mean)) +
  geom_point(size=2, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), size = 0.5, width=0.5, position = position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, lty=2) +
  labs(y="Response to \nConsumer Removal (LRR)", x="") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10, face="bold", color="black"))

y <- Consumers_set2 %>%
  group_by(Functional.group) %>%
  summarize(mean = mean(NPK_LRR),
            se = standard_error(NPK_LRR)) %>%
  ggplot(., aes(Functional.group, mean)) +
  geom_point(size=2, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), size = 0.5, width=0.5, position = position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, lty=2) +
  labs(y="Response to Nutrients (LRR)", x="") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10, face="bold", color="black"))

functional_plots <- ggarrange(x,y, ncol=2, nrow=1, align = "h")

functional_plots

Consumers_set2 %>%
  filter(Response != "NA") %>%
  ggplot(., aes(Response)) +
  geom_histogram( stat="count", aes(fill=Tradeoff)) +
  labs(y="Species response", x="") +
  scale_fill_manual(breaks = c("competition-defense", "growth-defense"),
                    values = c("dark orange", "dark green", "black")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=9.5, face="bold", color="black"),
        panel.grid.major.x = element_blank()) +
  facet_wrap(~Functional.group)

#these results not included in manuscript - looking at plant family or life histories of the plants

#Life history -- mostly perennials
ggplot(Consumers_set, aes(Consumer_LRR, NPK_LRR, col=Duration)) +
  geom_point() +
  geom_smooth(method="lm")

summary(sma(formula = Consumer_LRR ~ NPK_LRR * Duration, data=Consumers_set, method="SMA"))


#Plant Families
Fam_set <- Consumers_set %>%
  group_by(Family) %>%
  filter(n() > 15)
  #filter(Family == "Poaceae" | Family == "Asteraceae" | Family == "Cyperaceae" | Family == "Fabaceae")
  #filter(Species %in% top25percentspecies$Species) %>%
  #filter(Species != "Rhus glabra" )

View(Fam_set)

ggplot(Fam_set, aes(NPK_LRR, Consumer_LRR, col=Family)) +
  geom_point(alpha=0.1) +
  geom_smooth(method="lm", se=FALSE)
  facet_wrap(~Family)

ggplot(Fam_set, aes(NPK_LRR, Consumer_LRR, col=Family)) +
  geom_point(alpha=0.1) +
  geom_smooth(method="lm", se=FALSE)

summary(sma(formula = Consumer_LRR ~ NPK_LRR * Family, data=Fam_set, method="SMA"))

anova(lm(Consumer_LRR ~ NPK_LRR * Treatment * Family, data=Fam_set))

#diversity responses####
library(vegan)

diversity <- data %>%
  spread(Species, Mass.g.m.2.) %>%
  mutate_all(~replace(., is.na(.), 0)) 

metrics <- data %>%
  spread(Species, Mass.g.m.2.) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(richness = apply(.[,c(-6, -5, -4, -3, -2, -1)]>0,1,sum),
         invsimpson = diversity(.[,c(-6, -5, -4, -3, -2, -1)], index="invsimpson"),
         evenness = invsimpson/richness) %>%
  select(Year, Plot, Sub, richness, invsimpson, evenness)


diversity <- merge(diversity, metrics, by=c("Year", "Plot", "Sub"))
  
diversity <- diversity %>%
  select(Year, Block, Plot, Sub, Treatment, Fertilizer.f, richness, evenness, invsimpson)

fert_plots <- diversity %>%
  filter(Treatment == "Control" & Fertilizer.f == 1)

diversity_sub <- diversity %>%
  filter(Fertilizer.f == 0) %>%
  rbind(fert_plots)

##analysis###
#species richness#
hist((diversity_sub$richness))

lme1 <- lme(fixed = (richness) ~ Fertilizer.f + Treatment,
           data = diversity_sub,
           random = ~1 | Year/Block)

plot(lme1)
anova(lme1)
summary(lme1)

lsmeans(lme(fixed = (richness) ~ Fertilizer.f * Treatment,
                    data = diversity,
                    random = ~1 | Year/Block),
        pairwise ~ Fertilizer.f * Treatment, type="Tukey")

12.1/10.4

#invsimpson diversity
hist(log(diversity_sub$invsimpson))

lme1 <- lme(fixed = log(invsimpson) ~ Fertilizer.f + Treatment,
            data = diversity_sub,
            random = ~1 | Year/Block)

plot(lme1)
anova(lme1)
summary(lme1)

lsmeans(lme1 <- lme(fixed = (invsimpson) ~ Fertilizer.f * Treatment,
                    data = diversity,
                    random = ~1 | Year/Block),
        pairwise ~ Fertilizer.f * Treatment, method="Tukey")

#evenness
hist(log(diversity_sub$evenness))

lme1 <- lme(fixed = log(evenness) ~ Fertilizer.f + Treatment,
            data = diversity_sub,
            random = ~1 | Year/Block)

plot(lme1)
anova(lme1)
summary(lme1)

lsmeans(lme1 <- lme(fixed = (evenness) ~ Fertilizer.f * Treatment,
                    data = diversity,
                    random = ~1 | Year),
        pairwise ~ Fertilizer.f * Treatment, method="Tukey")


#visuals#
View(diversity)
ggplot(diversity, aes(Fertilizer.f, richness, col=Treatment)) +
  geom_boxplot() +
  scale_color_locuszoom() +
  labs(y="Species Richness", x="") +
  scale_x_discrete(labels=c("Ambient Nutrients", "Fertilized")) +
  theme(axis.text.x = element_text(size=10,  color="black", family="sans"),
        axis.title.y = element_text(size=10,  color="black", family="sans"))
  #coord_cartesian(ylim=c(1.5,8))

#diversity LRRs#####
#calculate these to visually show response to treatments
#comparing treatment plots to controls
#nutrient addition#
control <- diversity %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_richness = richness,
         control_evenness = evenness,
         control_invsimpson = invsimpson) %>%
  select(Year, Block, control_richness, control_evenness, control_invsimpson)

nutrient <- diversity %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "1")

compare <- merge(nutrient, control, by=c("Year", "Block"), all=TRUE)

#calculate log response ratio 

Nutrient_LRR <- compare %>%
  mutate(richness_LRR = log(richness/control_richness),
         evenness_LRR = log(evenness/control_evenness),
         invsimpson_LRR = log(invsimpson/control_invsimpson)) %>%
  mutate(Treatment = "Nutrients") %>%
  select(Year, Block, Plot, Sub, Treatment, richness_LRR, evenness_LRR, invsimpson_LRR)

#consumer removal#
#FF
control <- diversity %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_richness = richness,
         control_evenness = evenness,
         control_invsimpson = invsimpson) %>%
  select(Year, Block, control_richness, control_evenness, control_invsimpson)

FF <- diversity %>%
  filter(Treatment == "Foliar Fungicide") %>%
  filter(Fertilizer.f == "0")

compare <- merge(FF, control, by=c("Year", "Block"), all=TRUE)

#calculate log response ratio 
FF_LRR <- compare %>%
  mutate(richness_LRR = log(richness/control_richness),
         evenness_LRR = log(evenness/control_evenness),
         invsimpson_LRR = log(invsimpson/control_invsimpson)) %>%
  mutate(Treatment = "Foliar Fungicide") %>%
  select(Year, Block, Plot, Sub, Treatment, richness_LRR, evenness_LRR, invsimpson_LRR)

#SF#
control <- diversity %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_richness = richness,
         control_evenness = evenness,
         control_invsimpson = invsimpson) %>%
  select(Year, Block, control_richness, control_evenness, control_invsimpson)

SF <- diversity %>%
  filter(Treatment == "Soil Fungicide") %>%
  filter(Fertilizer.f == "0")

compare <- merge(SF, control, by=c("Year", "Block"), all=TRUE)

#calculate log response ratio 
SF_LRR <- compare %>%
  mutate(richness_LRR = log(richness/control_richness),
         evenness_LRR = log(evenness/control_evenness),
         invsimpson_LRR = log(invsimpson/control_invsimpson)) %>%
  mutate(Treatment = "Soil Fungicide") %>%
  select(Year, Block, Plot, Sub, Treatment, richness_LRR, evenness_LRR, invsimpson_LRR)

#Insecticide#
control <- diversity %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_richness = richness,
         control_evenness = evenness,
         control_invsimpson = invsimpson) %>%
  select(Year, Block, control_richness, control_evenness, control_invsimpson)

I <- diversity %>%
  filter(Treatment == "Insecticide") %>%
  filter(Fertilizer.f == "0")

compare <- merge(I, control, by=c("Year", "Block"), all=TRUE)

#calculate log response ratio 
I_LRR <- compare %>%
  mutate(richness_LRR = log(richness/control_richness),
         evenness_LRR = log(evenness/control_evenness),
         invsimpson_LRR = log(invsimpson/control_invsimpson)) %>%
  mutate(Treatment = "Insecticide") %>%
  select(Year, Block, Plot, Sub, Treatment, richness_LRR, evenness_LRR, invsimpson_LRR)

#Fence#
control <- diversity %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_richness = richness,
         control_evenness = evenness,
         control_invsimpson = invsimpson) %>%
  select(Year, Block, control_richness, control_evenness, control_invsimpson)

Fence <- diversity %>%
  filter(Treatment == "Fence") %>%
  filter(Fertilizer.f == "0")

compare <- merge(Fence, control, by=c("Year", "Block"), all=TRUE)

#calculate log response ratio 
Fence_LRR <- compare %>%
  mutate(richness_LRR = log(richness/control_richness),
         evenness_LRR = log(evenness/control_evenness),
         invsimpson_LRR = log(invsimpson/control_invsimpson)) %>%
  mutate(Treatment = "Fence") %>%
  select(Year, Block, Plot, Sub, Treatment, richness_LRR, evenness_LRR, invsimpson_LRR)

#All Removal#
control <- diversity %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_richness = richness,
         control_evenness = evenness,
         control_invsimpson = invsimpson) %>%
  select(Year, Block, control_richness, control_evenness, control_invsimpson) 

All <- diversity %>%
  filter(Treatment == "All Removal") %>%
  filter(Fertilizer.f == "0")

compare <- merge(All, control, by=c("Year", "Block"), all=TRUE)

#calculate log response ratio 
All_LRR <- compare %>%
  mutate(richness_LRR = log(richness/control_richness),
         evenness_LRR = log(evenness/control_evenness),
         invsimpson_LRR = log(invsimpson/control_invsimpson)) %>%
  mutate(Treatment = "All Removal") %>%
  select(Year, Block, Plot, Sub, Treatment, richness_LRR, evenness_LRR, invsimpson_LRR)

#All Removal + Fertilizer#
control <- diversity %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_richness = richness,
         control_evenness = evenness,
         control_invsimpson = invsimpson) %>%
  select(Year, Block, control_richness, control_evenness, control_invsimpson) 

All_NPK <- diversity %>%
  filter(Treatment == "All Removal") %>%
  filter(Fertilizer.f == "1")

compare <- merge(All_NPK, control, by=c("Year", "Block"), all=TRUE)

#calculate log response ratio 
All_NPK_LRR <- compare %>%
  mutate(richness_LRR = log(richness/control_richness),
         evenness_LRR = log(evenness/control_evenness),
         invsimpson_LRR = log(invsimpson/control_invsimpson)) %>%
  mutate(Treatment = "All Removal + NPKu") %>%
  filter(richness_LRR != "NA") %>%
  select(Year, Block, Plot, Sub, Treatment, richness_LRR, evenness_LRR, invsimpson_LRR)

#combine#
Combine_LRR <- Nutrient_LRR %>%
  rbind(FF_LRR) %>%
  rbind(SF_LRR) %>%
  rbind(I_LRR) %>%
  rbind(Fence_LRR) %>%
  rbind(All_LRR)
  #rbind(All_NPK_LRR)

Combine_LRR <- Combine_LRR %>%
  mutate(Treatment = as.factor(Treatment))

levels(Combine_LRR$Treatment)
#Combine_LRR$Treatment <- relevel(Combine_LRR$Treatment, ref='All Removal + NPKu')
Combine_LRR$Treatment <- relevel(Combine_LRR$Treatment, ref='All Removal')
Combine_LRR$Treatment <- relevel(Combine_LRR$Treatment, ref='Fence')
Combine_LRR$Treatment <- relevel(Combine_LRR$Treatment, ref='Insecticide')
Combine_LRR$Treatment <- relevel(Combine_LRR$Treatment, ref='Soil Fungicide')
Combine_LRR$Treatment <- relevel(Combine_LRR$Treatment, ref='Foliar Fungicide')
Combine_LRR$Treatment <- relevel(Combine_LRR$Treatment, ref='Nutrients')

x <- Combine_LRR %>%
  group_by(Treatment) %>%
  summarize(mean = mean(richness_LRR),
            ci = 1.96 * standard_error(richness_LRR)) %>%
  ggplot(., aes(Treatment, mean)) +
  geom_point(size=2, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), size = 0.5, width=0.5, position = position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, lty=2) +
  labs(y="Richness LRR", x="") +
  theme_bw() +
  theme(#axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10, color="black"),
        axis.ticks.x=element_blank(),
        axis.text.x = element_blank())

y <- Combine_LRR %>%
  group_by(Treatment) %>%
  summarize(mean = mean(evenness_LRR),
            ci = 1.96 * standard_error(evenness_LRR)) %>%
  ggplot(., aes(Treatment, mean)) +
  geom_point(size=2, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), size = 0.5, width=0.5, position = position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, lty=2) +
  labs(y="Evenness LRR", x="") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10,  color="black"))
        #axis.ticks.x=element_blank(),
        #axis.text.x = element_blank())

z <- Combine_LRR %>%
  group_by(Treatment) %>%
  summarize(mean = mean(invsimpson_LRR),
            ci = 1.96 * standard_error(invsimpson_LRR)) %>%
  ggplot(., aes(Treatment, mean)) +
  geom_point(size=2, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), size = 0.5, width=0.5, position = position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, lty=2) +
  labs(y="Inv Simpson LRR", x="") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10, color="black"))

div_plots <- ggarrange(z, ggarrange(x,y, ncol=1, nrow=2, align = "v"))

div_plots

#collapse consumer removal treatments into single variable#
Combine_LRR <- Combine_LRR %>%
  mutate(Binary = 
           case_when(Treatment == "Nutrients" ~ "N",
                     Treatment != "Nutrients" ~ "CR"))

#code below is to make figure 5
x <- Combine_LRR %>%
  group_by(Binary) %>%
  summarize(mean = mean(richness_LRR),
            ci = 1.96 * standard_error(richness_LRR)) %>%
  ggplot(., aes(Binary, mean)) +
  geom_point(size=2, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), size = 0.5, width=0.5, position = position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, lty=2) +
  scale_y_continuous(limits=c(-0.3,0.2)) +
  labs(y="Richness LRR", x="") +
  theme_bw() +
  theme(#axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10, color="black"),
    axis.ticks.x=element_blank(),
    axis.text.x = element_blank())

y <- Combine_LRR %>%
  group_by(Binary) %>%
  summarize(mean = mean(evenness_LRR),
            ci = 1.96 * standard_error(evenness_LRR)) %>%
  ggplot(., aes(Binary, mean)) +
  geom_point(size=2, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), size = 0.5, width=0.5, position = position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, lty=2) +
  scale_y_continuous(limits=c(-0.1,0.4)) +
  labs(y="Evenness LRR", x="") +
  theme_bw() +
  theme(axis.text.x = element_text(size=10,  color="black"))
#axis.ticks.x=element_blank(),
#axis.text.x = element_blank())

z <- Combine_LRR %>%
  group_by(Binary) %>%
  summarize(mean = mean(invsimpson_LRR),
            ci = 1.96 * standard_error(invsimpson_LRR)) %>%
  ggplot(., aes(Binary, mean)) +
  geom_point(size=2, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), size = 0.5, width=0.5, position = position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, lty=2) +
  labs(y="Inverse Simpson LRR", x="") +
  theme_bw() +
  theme(axis.text.x = element_text(size=10, color="black"))

div_plots <- ggarrange(z, ggarrange(x,y, ncol=1, nrow=2, align="v"))

div_plots

#Robustness####
#set LRR threshold to 1.4 (twice as high as original as in Lind et al. 2013 )
#try writing a function that... tweaks LRR threshold in increments of 0.1?
#then also gives output of proportion of responses#
Consumers_set <- Consumers_set %>%
  mutate(Nutrient_Response = 
           case_when(NPK_LRR >= 1.386 ~ "nutrient only", #defining responses based on an effect if something doubles or halves in response
                     NPK_LRR <= -1.386 ~ "nutrient only",
                     NPK_LRR < 1.386 & NPK_LRR > -1.386 ~ "no response")) %>%
  mutate(Consumer_Response = 
           case_when(Consumer_LRR >= 1.386 ~ "consumer only",
                     Consumer_LRR <= -1.386 ~ "consumer only",
                     Consumer_LRR < 1.386 & Consumer_LRR > -1.386 ~ "no response")) %>%
  mutate(Response = 
           case_when(Consumer_Response == "consumer only" & Nutrient_Response == "nutrient only" ~ "tradeoff",
                     Consumer_Response == "consumer only" & Nutrient_Response == "no response"   ~ "consumer only",
                     #Consumer_Response == "consumer only" & Nutrient_Response == "absent" ~ "consumer only",
                     Consumer_Response == "no response" & Nutrient_Response == "nutrient only"   ~ "nutrient only",
                     #Consumer_Response == "absent" & Nutrient_Response == "nutrient only" ~ "nutrient only",
                     Consumer_Response == "no response" & Nutrient_Response == "no response" ~ "no response")) %>%
  #Consumer_Response == "absent" & Nutrient_Response == "no response" ~ "no response",
  #Consumer_Response == "no response" & Nutrient_Response == "absent" ~ "no response")) %>%
  mutate(Tradeoff = 
           case_when(Response == "tradeoff" & Consumer_LRR > 0 & NPK_LRR > 0 ~ "growth-defense",
                     Response == "tradeoff" & Consumer_LRR < 0 & NPK_LRR < 0 ~ "growth-defense",
                     Response == "tradeoff" & Consumer_LRR > 0 & NPK_LRR < 0 ~ "competition-defense",
                     Response == "tradeoff" & Consumer_LRR < 0 & NPK_LRR > 0 ~ "competition-defense",
                     TRUE ~ "NA")) %>%
  ungroup() %>%
  mutate(Response = as.factor(Response),
         Tradeoff = as.factor(Tradeoff))

levels(Consumers_set$Response)

Consumers_set$Response <- relevel(Consumers_set$Response, ref='tradeoff')
Consumers_set$Response <- relevel(Consumers_set$Response, ref='nutrient only')
Consumers_set$Response <- relevel(Consumers_set$Response, ref='consumer only')
Consumers_set$Response <- relevel(Consumers_set$Response, ref='no response')

View(Consumers_set)

Consumers_set %>%
  filter(Response != "NA") %>%
  ggplot(., aes(Response)) +
  geom_histogram( stat="count", aes(fill=Tradeoff)) +
  labs(y="Number of species responses", x="") +
  scale_fill_manual(breaks = c("competition-defense", "growth-defense", ""),
                      values = c("dark orange", "dark green", "black")
                    ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=9.5, face="bold", color="black"),
        panel.grid.major.x = element_blank()) +
  theme(legend.position = c(0.5, 0.9)) +
  theme(legend.key.size = unit(0.3, 'cm')) 

Consumers_set %>%
  filter(Response != "NA") %>%
  ggplot(., aes(Response)) +
  geom_histogram( stat="count", aes(fill=Tradeoff)) +
  labs(y="Species response", x="") +
  scale_fill_manual(breaks = c("competition-defense", "growth-defense"),
                    values = c("dark orange", "dark green", "black")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=9.5, face="bold", color="black"),
        panel.grid.major.x = element_blank()) +
  facet_wrap(~Treatment, ncol=5, nrow=1)

summary(Consumers_set$Response)

#Want to know if growth-defense is more numerious thant competition-defense responses
#create contingency table in order to do chi square tests
cont <- Consumers_set %>%
  filter(Tradeoff != "NA") %>%
  select(Year, Treatment, Species, Tradeoff) %>%
  mutate(Tradeoff = as.character(Tradeoff)) %>%
  mutate(Tradeoff = as.factor(Tradeoff))

summary(cont) 

cont <- as.data.frame(table(cont))

ggplot(cont, aes(Tradeoff, Freq)) +
  geom_bar(stat="identity")

#probably just go with this chisquare test below
cont <- cont %>%
  pivot_wider(names_from = Tradeoff, values_from = Freq)

chisq.test(cont$`competition-defense`, cont$`growth-defense`, correct=FALSE)

#design ideas#####
x <- ggplot(Consumers_set, aes(NPK_LRR, Consumer_LRR)) +
  geom_point(alpha=0.0) +
  geom_hline(yintercept = c(0), lty =2) +
  geom_vline(xintercept = c(0), lty =2) +
  scale_y_continuous(limits=c(-2,2)) +
  scale_x_continuous(limits=c(-2,2)) +
  #annotate(geom="text", x=0, y=0, label="no response") +
  annotate(geom="text", x=1.1 , y =1.25, label="poor nutrient competitor \n and not well defended \n(growth-defense)") +
  annotate(geom="text", x=-1.1 , y =-1.05, label="good nutrient competitor \n  and well defended \n(growth-defense)") +
  annotate(geom="text", x=-1.1 , y =1.25, label="good nutrient competitor \n and not well defended \n(competition-defense)") +
  annotate(geom="text", x=1.1 , y =-1.05, label="poor nutrient competitor \n and  well defended \n(competition-defense)") +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
    labs(y= "Response\n-       Removing Consumers       +", x="-           Response to Nutrients       +")

x

y <- ggplot(Consumers_set, aes(NPK_LRR, Consumer_LRR)) +
  geom_point(alpha=0.0) +
  geom_hline(yintercept = c(0), lty =2) +
  geom_vline(xintercept = c(0), lty =2) +
  geom_segment(aes(x = -2, y = -2, xend = 2, yend = 2),
               arrow = arrow(length = unit(0.5, "cm"), ends="both")) +
  geom_segment(aes(x = -2, y = 2, xend = 2, yend = -2), linetype="dotdash",
               arrow = arrow(length = unit(0.5, "cm"), ends="both")) +
  #geom_abline(intercept = 0, slope =-1) +
  #geom_hline(yintercept = c(-0.693, 0.693), lty =2) +
  #geom_vline(xintercept = c(-0.693, 0.693), lty =2) +
  scale_y_continuous(limits=c(-2,2)) +
  scale_x_continuous(limits=c(-2,2)) +
  #annotate(geom="text", x=0, y=0, label="no response") +
  annotate(geom="text", x=-0.75 , y = -1.5, angle="0", size=3, label="growth-defense") +
  annotate(geom="text", x=-0.75, y =1.75, angle="0", size=3, lty=2, label="competition-defense") +
  #annotate(geom="text", x=-1.5 , y =-1.5, label="well defended and \ngood competitor") +
  #annotate(geom="text", x=-1.5 , y =1.5, label="poorly defended and \ngood competitor") +
  #annotate(geom="text", x=1.5 , y =-1.5, label="well defended and \npoor competitor") +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y= "Response\n-       Removing Consumers       +", x="-           Response to Nutrients       +")

theory_fig <- ggarrange(x,y, ncol=2, nrow=1)

theory_fig

ggsave(filename="ecology_Fig1.pdf", 
       path="/Users/mzaret/Documents/Research/Nutrients_Consumer_Removal/Ch III/code",
       width=9, height = 5)
