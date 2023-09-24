#Max Zaret
#e245 nutrients x consumer removal#
#Life history trade offs in grassland plant species
#9-8-2022#
#note that I can remove tradeoff/response code from earlier sections
#contain all of ^^^ in the "Define tradeoffs section"

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
setwd("~/Documents/Research/Nutrients_Consumer_Removal/CH III/code")

data <- read.csv("2019-2021 E245 Enemy Removal Aboveground Biomass.csv")

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

#Does relationship between C_LRR and NPK_LRR vary by Block?
sma(formula = Consumer_LRR ~ NPK_LRR * Block, data=Consumers_set, method="SMA")
#yes
summary(sma(formula = Consumer_LRR ~ NPK_LRR * Block, data=Consumers_set, method="SMA"))

summary(sma(formula = Consumer_LRR ~ NPK_LRR + Block, data=Consumers_set, method="SMA"))

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

135+48

#Functional Groups####
#read in species info sheet
species <- read.csv("cc_plant_species.csv")

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
          random= ~1 | Year/Block,
          data=Consumers_set2))

anova(lme(fixed=Consumer_LRR ~ Functional.group * Treatment,
            random= ~1 | Year/Block,
            data=Consumers_set2))

summary(lme(fixed=NPK_LRR ~ Functional.group,
          random= ~1 | Year/Block,
          data=Consumers_set2))

anova(lme(fixed=NPK_LRR ~ Functional.group,
          random= ~1 | Year/Block,
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
  facet_wrap(~Treatment)

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

#Life history -- mostly perennials
ggplot(Consumers_set, aes(Consumer_LRR, NPK_LRR, col=Duration)) +
  geom_point() +
  geom_smooth(method="lm")

summary(sma(formula = Consumer_LRR ~ NPK_LRR * Duration, data=Consumers_set, method="SMA"))


#Plant Families
View(data)

top25percentspecies <- data %>%
  group_by(Species) %>%
  summarize(biomass = mean(Mass.g.m.2.)) %>%
  filter(biomass > 8.26) %>%
  filter(Species != "Oenothera biennis")
  #ungroup() %>%
  #summarize(Q1 = quantile(biomass, probs=0.75))

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
           random = ~1 | Year/Block/Plot)

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
            random = ~1 | Year/Block/Plot)

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
            random = ~1 | Year/Block/Plot)

plot(lme1)
anova(lme1)
summary(lme1)

lsmeans(lme1 <- lme(fixed = (evenness) ~ Fertilizer.f * Treatment,
                    data = diversity,
                    random = ~1 | Year/Block),
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

Combine_LRR %>%
  group_by(Binary) %>%
  summarize(mean = mean(invsimpson_LRR),
            ci = 1.96 * standard_error(invsimpson_LRR))

lsmeans(lme(fixed=richness_LRR ~ Binary,
            random= ~1 | Year/Block,
            data=Combine_LRR),
        pairwise ~ Binary)

lsmeans(lme(fixed=evenness_LRR ~ Binary,
            random= ~1 | Year/Block,
            data=Combine_LRR),
        pairwise ~ Binary)

lsmeans(lme(fixed=invsimpson_LRR ~ Binary,
            random= ~1 | Year/Block,
            data=Combine_LRR),
        pairwise ~ Binary)

#redo lmes to match LRR calculations above (remove crossing of fert and nutrients)
control <- diversity %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0")

nutrient <- diversity %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "1") %>%
  mutate(Treatment = "Nutrients")

diversity2 <- control %>%
  rbind(nutrient) %>%
  rbind(FF) %>%
  rbind(SF) %>%
  rbind(I) %>%
  rbind(Fence) %>%
  rbind(All) %>%
  mutate(Treatment = as.factor(Treatment))

#collapse consumer removal treatments into single variable#
diversity2 <- diversity2 %>%
  mutate(Binary = 
           case_when(Treatment == "Nutrients" ~ "Nutrients",
                     Treatment == "Control" ~ "Control",
                     Treatment != "Nutrients" & Treatment != "Control" ~ "Consumer Removal")) %>%
  mutate(Binary = as.factor(Binary))

diversity2$Binary <- relevel(diversity2$Binary, ref='Consumer Removal')
diversity2$Binary <- relevel(diversity2$Binary, ref='Nutrients')
diversity2$Binary <- relevel(diversity2$Binary, ref='Control')

hist(log(diversity2$richness))

lme1 <- lme(fixed = (richness) ~ Binary,
            data = diversity2,
            random = ~1 | Year/Block)

plot(lme1)
anova(lme1)
summary(lme1)

hist(log(diversity2$evenness))

lme1 <- lme(fixed = log(evenness) ~ Binary,
            data = diversity2,
            random = ~1 | Year/Block)

plot(lme1)
anova(lme1)
summary(lme1)

hist(log(diversity2$invsimpson))

lme1 <- lme(fixed = log(invsimpson) ~ Binary,
            data = diversity2,
            random = ~1 | Year/Block)

plot(lme1)
anova(lme1)
summary(lme1)

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

#####prop competitors####
#first calculate proportion of poor competitors and poor defended species in each plot#
species_LRR <- Consumers_set %>%
  group_by(Year, Species) %>%
  summarize(Consumer_LRR = mean(Consumer_LRR),
            NPK_LRR = mean(NPK_LRR)) %>%
  mutate(Resource_Competition = 
           case_when(NPK_LRR >= 0.693 ~ "poor competitor", #defining responses based on an effect if something doubles or halves in response
                     NPK_LRR <= -0.693 ~ "good competitor",
                     NPK_LRR < 0.693 & NPK_LRR > -0.693 ~ "no response")) %>%
  mutate(Apparent_Competition = 
           case_when(Consumer_LRR >= 0.693 ~ "poor defense",
                     Consumer_LRR <= -0.693 ~ "good defense",
                     Consumer_LRR < 0.693 & Consumer_LRR > -0.693 ~ "no response"))

View(species_LRR)

ggplot(species_LRR, aes(Apparent_Competition)) +
         geom_histogram(stat="count")

#add species information to original biomass df
prop <- merge(data, species_LRR, by=c("Year", "Species"))

View(prop)

#subset to calculate biomass of species exhibiting poor competition and poor defense
competition <- prop %>% 
  group_by(Year, Block, Plot, Sub, Treatment, Fertilizer.f) %>%
  filter(Resource_Competition == "poor competitor" & Apparent_Competition == "poor defense") %>%
  summarize(poor_competitor_biomass = sum(Mass.g.m.2.))

#total biomass for a plot
total <- prop %>% 
  group_by(Year, Block, Plot, Sub, Treatment, Fertilizer.f) %>%
  summarize(total_biomass = sum(Mass.g.m.2.))

competition <- merge(competition, total, by=c("Year", "Block", "Plot", "Sub", "Treatment", "Fertilizer.f"))

#calculate proportion of biomass made up by poorly defended and poor resource competitor species
competition <- competition %>%
  mutate(prop_poor_competitor = poor_competitor_biomass/total_biomass)

#now merge data with diversity metrics#
combined <- merge(competition, diversity, by=c("Year", "Block", "Plot", "Sub", "Treatment", "Fertilizer.f"))

#look at correlations#
hist(sqrt(combined$prop_poor_competitor))

anova(lme(sqrt(prop_poor_competitor) ~ Fertilizer.f * Treatment, data=combined,
            random = ~1 | Year/Block/Plot/Sub))

summary(lm(richness ~ prop_poor_competitor, data=combined))

summary(lm(evenness ~ prop_poor_competitor, data=combined))
summary(lm(log(invsimpson) ~ prop_poor_competitor, data=combined))

ggplot(combined, aes(Fertilizer.f, prop_poor_competitor, col=Treatment)) +
  geom_boxplot() +
  scale_color_locuszoom() +
  labs(y="Proportion of poor competitors") +
  coord_cartesian(ylim=c(0,0.8))

x <- ggplot(combined, aes(prop_poor_competitor, richness, col=Treatment)) +
  #geom_smooth(method="lm", se=TRUE, col="black") +
  labs(x="Proportion of poor competitors", y="Plant Species Richness")
  scale_color_locuszoom()

x + geom_point(data=combined, aes(prop_poor_competitor, richness, col=Fertilizer.f)) +
  scale_color_locuszoom()

x + geom_point(data=combined, aes(prop_poor_competitor, richness, col=Treatment)) +
  geom_smooth(method="lm", se=FALSE) +
  scale_color_locuszoom()

#Visualize changes in diversity and proportion of poorly defended and poor resource competitors

combined <- combined %>%
  pivot_longer(cols = c("richness", "evenness", "invsimpson"),
               names_to = "diversity",
               values_to = "value")

ggplot(combined, aes(prop_poor_competitor, value, col=Treatment)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) 

#now do path analysis#
library(piecewiseSEM)

#Make nutrient addition binary#
combined <- combined %>%
  mutate(Fertilizer.n = as.numeric(as.character(Fertilizer.f)))

#Make consumer removal binary for interpretation#
combined$FF <- ifelse(combined[,5] == "AllPesticides", 0, 
                      ifelse(combined[,5] == "FoliarFungicide", 1, 
                             ifelse(combined[,5] == "Insecticide", 0, 
                                    ifelse(combined[,5] == "Control", 0,
                                           ifelse(combined[,5] == "SoilDrenchFungicide", 0,
                                                  ifelse(combined[,5] == "Fenced", 0,99))))))

combined$SF <- ifelse(combined[,5] == "AllPesticides", 0, 
                  ifelse(combined[,5] == "FoliarFungicide", 0, 
                         ifelse(combined[,5] == "Insecticide", 0, 
                                ifelse(combined[,5] == "Control", 0,
                                       ifelse(combined[,5] == "SoilDrenchFungicide", 1,
                                                     ifelse(combined[,5] == "Fenced", 0,99))))))

combined$Fenced <- ifelse(combined[,5] == "AllPesticides", 0, 
                      ifelse(combined[,5] == "FoliarFungicide", 0, 
                             ifelse(combined[,5] == "Insecticide", 0, 
                                    ifelse(combined[,5] == "Control", 0,
                                           ifelse(combined[,5] == "SoilDrenchFungicide", 0,
                                                  ifelse(combined[,5] == "Fenced", 1,99))))))

combined$I <- ifelse(combined[,5] == "AllPesticides", 0, 
                      ifelse(combined[,5] == "FoliarFungicide", 0, 
                             ifelse(combined[,5] == "Insecticide", 1, 
                                    ifelse(combined[,5] == "Control", 0,
                                           ifelse(combined[,5] == "SoilDrenchFungicide", 0,
                                                  ifelse(combined[,5] == "Fenced", 0,99))))))

combined$All <- ifelse(combined[,5] == "AllPesticides", 1, 
                      ifelse(combined[,5] == "FoliarFungicide", 0, 
                             ifelse(combined[,5] == "Insecticide", 0, 
                                    ifelse(combined[,5] == "Control", 0,
                                           ifelse(combined[,5] == "SoilDrenchFungicide", 0,
                                                  ifelse(combined[,5] == "Fenced", 0,99))))))

diversity.psem <- psem(
  lme(prop_poor_competitor ~ Fertilizer.n + FF + SF + Fenced + I + All, random= ~1 | Year/Block/Plot/Sub, data=combined, na.action=na.omit),
  #lme(prop_poor_defense ~ Fertilizer.n + Treatment, random= ~1 | Year/Block/Plot/Sub, data=combined, na.action=na.omit),
  lme(richness ~ prop_poor_competitor + Fertilizer.n + SF, random= ~1 | Year/Block/Plot/Sub, data=combined, na.action=na.omit),
  lme(evenness ~ richness + prop_poor_competitor + Fertilizer.n, random= ~1 | Year/Block/Plot/Sub, data=combined, na.action=na.omit),
  lme(invsimpson ~ richness + evenness + prop_poor_competitor + Fertilizer.n + SF, random= ~1 | Year/Block/Plot/Sub, data=combined, na.action=na.omit))

summary(diversity.psem)
plot(diversity.psem)

diversity.psem <- psem(
  lme(prop_poor_competitor ~ Fertilizer.n + FF + SF + Fenced + I + All, random= ~1 | Year/Block/Plot/Sub, data=combined, na.action=na.omit),
  lme(log(invsimpson) ~ prop_poor_competitor + Fertilizer.n + SF, random= ~1 | Year/Block/Plot/Sub, data=combined, na.action=na.omit))

summary(diversity.psem)
plot(diversity.psem)

View(combined)

###correlation by plot######
x <- FF_set %>%
  rbind(Fenced_set) %>%
  rbind(SF_set) %>%
  rbind(I_set) %>%
  rbind(All_set) %>% 
  mutate(Species = as.factor(Species)) %>%
  group_by(Year, Block, Treatment) %>%
  summarize(Consumer_LRR = mean(Consumer_LRR, na.rm = TRUE), 
         NPK_LRR = mean(NPK_LRR, na.rm = TRUE))

View(x)

y <- diversity %>%
  filter(Treatment != "Control") %>%
  group_by(Year, Block, Treatment) %>%
  summarize(invsimpson = mean(invsimpson),
            richness = mean(richness),
            evenness = mean(evenness))

z <- merge(y,x, by=c("Year", "Block", "Treatment"))

z <- z %>%
  mutate(weighted_tradeoff_correlation = tradeoff_correlation/richness)

hist((z$tradeoff_correlation))
hist(z$richness)
hist(z$evenness)
hist(z$invsimpson)

summary(lm(invsimpson ~ NPK_LRR, data=z))

ggplot(z, aes(tradeoff_correlation, invsimpson)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

###Nutrient effect on consumer LRR#####
#comparing LRR of species to consumers in fertilized plots versus unfertilized#
#Foliar Fungicide##
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "1") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

FF_NPK <- data %>%
  filter(Treatment == "Foliar Fungicide") %>%
  filter(Fertilizer.f == "1")

compare <- merge(FF_NPK, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  filter(Mass.g.m.2. != "NA") %>%
  mutate(control_mass = replace_na(control_mass, 0.005))

FF_NPK_LRR <- compare %>%
  mutate(Consumer_NPK_LRR = log(Mass.g.m.2./control_mass)) %>%
  select(Year, Block, Species, Treatment, Consumer_NPK_LRR)

#distribution of LRRs
ggplot(FF_NPK_LRR, aes(Consumer_NPK_LRR)) +
  geom_histogram()


FF_NPK_set <- merge(FF_LRR, FF_NPK_LRR, by=c("Year", "Block", "Species"), all=TRUE)

FF_NPK_set <- FF_NPK_set %>%
  mutate(Treatment = "FoliarFungicide",
         Treatment = as.factor(Treatment)) %>%
  #mutate(Consumer_LRR = replace_na(Consumer_LRR, 0.000)) %>%
  #mutate(NPK_effect = log(Consumer_NPK_LRR - Consumer_LRR)) %>%
  select(Year, Block, Treatment, Species, Consumer_LRR, Consumer_NPK_LRR)

FF_long <- FF_NPK_set %>%
  pivot_longer(cols = c("Consumer_LRR", "Consumer_NPK_LRR"), 
               names_to = "Consumer_NPK", values_to="Consumer_LRR") %>%
  mutate(Consumer_NPK = as.factor(Consumer_NPK))

levels(FF_long$Consumer_NPK)

#Fencing##
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "1") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

Fenced_NPK <- data %>%
  filter(Treatment == "Fence") %>%
  filter(Fertilizer.f == "1")

compare <- merge(Fenced_NPK, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  filter(Mass.g.m.2. != "NA") %>%
  mutate(control_mass = replace_na(control_mass, 0.005))

Fenced_NPK_LRR <- compare %>%
  mutate(Consumer_NPK_LRR = log(Mass.g.m.2./control_mass)) %>%
  select(Year, Block, Species, Treatment, Consumer_NPK_LRR)

#distribution of LRRs
ggplot(Fenced_NPK_LRR, aes(Consumer_NPK_LRR)) +
  geom_histogram()


Fenced_NPK_set <- merge(Fenced_LRR, Fenced_NPK_LRR, by=c("Year", "Block", "Species"), all=TRUE)

Fenced_NPK_set <- Fenced_NPK_set %>%
  mutate(Treatment = "Fenced",
         Treatment = as.factor(Treatment)) %>%
  #mutate(Consumer_LRR = replace_na(Consumer_LRR, 0.000)) %>%
  #mutate(NPK_effect = log(Consumer_NPK_LRR - Consumer_LRR)) %>%
  select(Year, Block, Treatment, Species, Consumer_LRR, Consumer_NPK_LRR)


ggplot(Fenced_NPK_set, aes(Consumer_LRR, Consumer_NPK_LRR, col=Treatment)) +
  geom_point() +
  geom_smooth(method="lm", col="black")

Fenced_long <- Fenced_NPK_set %>%
  pivot_longer(cols = c("Consumer_LRR", "Consumer_NPK_LRR"), 
               names_to = "Consumer_NPK", values_to="Consumer_LRR") %>%
  mutate(Consumer_NPK = as.factor(Consumer_NPK))

#Soil Fungicide##
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "1") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

SF_NPK <- data %>%
  filter(Treatment == "SoilDrenchFungicide") %>%
  filter(Fertilizer.f == "1")

compare <- merge(SF_NPK, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  filter(Mass.g.m.2. != "NA") %>%
  mutate(control_mass = replace_na(control_mass, 0.005))

SF_NPK_LRR <- compare %>%
  mutate(Consumer_NPK_LRR = log(Mass.g.m.2./control_mass)) %>%
  select(Year, Block, Species, Treatment, Consumer_NPK_LRR)

#distribution of LRRs
ggplot(SF_NPK_LRR, aes(Consumer_NPK_LRR)) +
  geom_histogram()


SF_NPK_set <- merge(SF_LRR, SF_NPK_LRR, by=c("Year", "Block", "Species"), all=TRUE)

SF_NPK_set <- SF_NPK_set %>%
  mutate(Treatment = "SoilDrenchFungicide",
         Treatment = as.factor(Treatment)) %>%
  #mutate(Consumer_LRR = replace_na(Consumer_LRR, 0.000)) %>%
  #mutate(NPK_effect = log(Consumer_NPK_LRR - Consumer_LRR)) %>%
  select(Year, Block, Treatment, Species, Consumer_LRR, Consumer_NPK_LRR)


ggplot(SF_NPK_set, aes(Consumer_LRR, Consumer_NPK_LRR, col=Treatment)) +
  geom_point() +
  geom_smooth(method="lm", col="black")

SF_long <- SF_NPK_set %>%
  pivot_longer(cols = c("Consumer_LRR", "Consumer_NPK_LRR"), 
               names_to = "Consumer_NPK", values_to="Consumer_LRR") %>%
  mutate(Consumer_NPK = as.factor(Consumer_NPK))

#Insecticide##
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "1") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

I_NPK <- data %>%
  filter(Treatment == "Insecticide") %>%
  filter(Fertilizer.f == "1")

compare <- merge(I_NPK, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  filter(Mass.g.m.2. != "NA") %>%
  mutate(control_mass = replace_na(control_mass, 0.005))

I_NPK_LRR <- compare %>%
  mutate(Consumer_NPK_LRR = log(Mass.g.m.2./control_mass)) %>%
  select(Year, Block, Species, Treatment, Consumer_NPK_LRR)

#distribution of LRRs
ggplot(I_NPK_LRR, aes(Consumer_NPK_LRR)) +
  geom_histogram()


I_NPK_set <- merge(I_LRR, I_NPK_LRR, by=c("Year", "Block", "Species"), all=TRUE)

I_NPK_set <- I_NPK_set %>%
  mutate(Treatment = "Insecticide",
         Treatment = as.factor(Treatment)) %>%
  #mutate(Consumer_LRR = replace_na(Consumer_LRR, 0.000)) %>%
  #mutate(NPK_effect = log(Consumer_NPK_LRR - Consumer_LRR)) %>%
  select(Year, Block, Treatment, Species, Consumer_LRR, Consumer_NPK_LRR)


ggplot(I_NPK_set, aes(Consumer_LRR, Consumer_NPK_LRR, col=Treatment)) +
  geom_point() +
  geom_smooth(method="lm", col="black")

I_long <- I_NPK_set %>%
  pivot_longer(cols = c("Consumer_LRR", "Consumer_NPK_LRR"), 
               names_to = "Consumer_NPK", values_to="Consumer_LRR") %>%
  mutate(Consumer_NPK = as.factor(Consumer_NPK))


#All Pesticides##
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "1") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

All_NPK <- data %>%
  filter(Treatment == "AllPesticides") %>%
  filter(Fertilizer.f == "1")

compare <- merge(All_NPK, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  filter(Mass.g.m.2. != "NA") %>%
  mutate(control_mass = replace_na(control_mass, 0.005))

All_NPK_LRR <- compare %>%
  mutate(Consumer_NPK_LRR = log(Mass.g.m.2./control_mass)) %>%
  select(Year, Block, Species, Treatment, Consumer_NPK_LRR)

#distribution of LRRs
ggplot(All_NPK_LRR, aes(Consumer_NPK_LRR)) +
  geom_histogram()


All_NPK_set <- merge(All_LRR, All_NPK_LRR, by=c("Year", "Block", "Species"), all=TRUE)

All_NPK_set <- All_NPK_set %>%
  mutate(Treatment = "AllPesticides",
         Treatment = as.factor(Treatment)) %>%
  #mutate(Consumer_LRR = replace_na(Consumer_LRR, 0.000)) %>%
  #mutate(NPK_effect = log(Consumer_NPK_LRR - Consumer_LRR)) %>%
  select(Year, Block, Treatment, Species, Consumer_LRR, Consumer_NPK_LRR)


ggplot(All_NPK_set, aes(Consumer_LRR, Consumer_NPK_LRR, col=Treatment)) +
  geom_point() +
  geom_smooth(method="lm", col="black")

All_long <- All_NPK_set %>%
  pivot_longer(cols = c("Consumer_LRR", "Consumer_NPK_LRR"), 
               names_to = "Consumer_NPK", values_to="Consumer_LRR") %>%
  mutate(Consumer_NPK = as.factor(Consumer_NPK))

#combined sets#
Consumers_NPK_set <- FF_long %>%
  rbind(Fenced_long) %>%
  rbind(SF_long) %>%
  rbind(I_long) %>%
  rbind(All_long)

levels(Consumers_NPK_set$Treatment)

ggplot(Consumers_NPK_set, aes(Consumer_NPK, Consumer_LRR, col=Treatment)) +
  geom_boxplot() +
  scale_x_discrete(labels=c("Ambient Nutrients", "Fertilized")) +
  scale_color_locuszoom() +
  theme(axis.title.x = element_blank())

ggplot(Consumers_NPK_set, aes(Treatment, Consumer_LRR, col=Consumer_NPK)) +
  geom_boxplot() +
  #scale_x_discrete(labels=c("Ambient Nutrients", "Fertilized")) +
  scale_color_locuszoom() +
  theme(axis.title.x = element_blank())

lme1 <- lme(fixed=Consumer_LRR ~ Consumer_NPK,
            data= Consumers_NPK_set,
            random = ~1 | Year/Block, na.action = na.omit)

summary(lme1)
anova(lme1)

lsmeans(lme(fixed=Consumer_LRR ~ Consumer_NPK * Treatment,
            data= Consumers_NPK_set,
            random = ~1 | Year/Block, na.action = na.omit),
        pairwise ~ Treatment, method="Tukey")

t.test(All_long$Consumer_LRR)

#add in functional group information
Consumers_NPK_set <- merge(Consumers_NPK_set, species, by="Species")

Consumers_NPK_set <- Consumers_NPK_set %>%
  mutate(Functional.group =
           case_when(Species == "Cyperus sp." ~ "C4",
                     Species == "Carex sp." ~ "C3",
                     TRUE ~ Functional.group))

#Functional groups#
Consumers_NPK_set %>%
  filter(Functional.group != "O") %>%
  filter(Functional.group != "W") %>%
  ggplot(., aes(Functional.group, Consumer_LRR, col=Consumer_NPK)) +
  geom_boxplot() +
  facet_wrap(~Treatment)

Consumers_NPK_set %>%
  filter(Functional.group != "O") %>%
  filter(Functional.group != "W") %>%
  filter(Consumer_LRR != "NA") %>%
  group_by(Consumer_NPK, Functional.group) %>%
  summarize(mean = mean(Consumer_LRR)) %>%
  ggplot(., aes(Functional.group, mean, fill=Consumer_NPK)) +
  geom_bar(stat='identity', position='dodge') +
  scale_color_locuszoom()

lm1 <- lm(Consumer_LRR ~ Consumer_NPK * Functional.group, data=Consumers_NPK_set)
summary(lm1)
anova(lm1)

lme1 <- lme(fixed = Consumer_LRR ~ Consumer_NPK * Treatment + Functional.group ,
            data=Consumers_NPK_set,
            random = ~1 | Year/Block, na.action = na.omit)
summary(lme1)
anova(lme1)

###cohens d####
#nutrient addition#
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

nutrient <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "1")

compare <- merge(nutrient, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  mutate(Mass.g.m.2. = replace_na(Mass.g.m.2., 0.01)) %>%
  mutate(control_mass = replace_na(control_mass, 0.01))

#calculate means and pooled standard deviation
means <- compare %>%
  group_by(Year, Species) %>%
  summarize(fert_mass = mean(Mass.g.m.2.),
            control_mass = mean(control_mass),
            signal = fert_mass - control_mass)

sds <- compare %>%
  pivot_longer(cols = c("Mass.g.m.2.", "control_mass"), 
               names_to = "Type", values_to="Mass") %>%
  group_by(Year, Species) %>%
  summarize(sd = sd(Mass))

output <- merge(means, sds, by=c("Year", "Species"))

NPK <- output %>%
  mutate(NPK_cohens_d = signal/sd) %>%
  select(Year, Species, NPK_cohens_d )

ggplot(NPK, aes(NPK_cohens_d)) +
  geom_histogram()

#Foliar Fungicide#
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

FF <- data %>%
  filter(Treatment == "FoliarFungicide") %>%
  filter(Fertilizer.f == "0")

compare <- merge(FF, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  mutate(Mass.g.m.2. = replace_na(Mass.g.m.2., 0.01)) %>%
  mutate(control_mass = replace_na(control_mass, 0.01))

#calculate means and pooled standard deviation
means <- compare %>%
  group_by(Year, Species) %>%
  summarize(consumer_mass = mean(Mass.g.m.2.),
            control_mass = mean(control_mass),
            signal = consumer_mass - control_mass)

sds <- compare %>%
  pivot_longer(cols = c("Mass.g.m.2.", "control_mass"), 
               names_to = "Type", values_to="Mass") %>%
  group_by(Year, Species) %>%
  summarize(sd = sd(Mass))

FF <- merge(means, sds, by=c("Year", "Species"))

FF <- FF %>%
  mutate(Consumer_cohens_d = signal/sd) %>%
  select(Year, Species, Consumer_cohens_d )

FF_set <- merge(FF, NPK, by=c("Year","Species")) 

FF_set <- FF_set %>%
  filter(Consumer_cohens_d != NPK_cohens_d) %>% #remove comparisons that compare two dummy variables (not meaningful comparison)
  mutate(Treatment = "FoliarFungicide",
         Treatment = as.factor(Treatment))

sma(formula = Consumer_cohens_d ~ NPK_cohens_d, data=FF_set, method="SMA")

#Fencing#
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

Fenced <- data %>%
  filter(Treatment == "Fenced") %>%
  filter(Fertilizer.f == "0")

compare <- merge(Fenced, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  mutate(Mass.g.m.2. = replace_na(Mass.g.m.2., 0.01)) %>%
  mutate(control_mass = replace_na(control_mass, 0.01))

#calculate means and pooled standard deviation
means <- compare %>%
  group_by(Year, Species) %>%
  summarize(consumer_mass = mean(Mass.g.m.2.),
            control_mass = mean(control_mass),
            signal = consumer_mass - control_mass)

sds <- compare %>%
  pivot_longer(cols = c("Mass.g.m.2.", "control_mass"), 
               names_to = "Type", values_to="Mass") %>%
  group_by(Year, Species) %>%
  summarize(sd = sd(Mass))

Fenced <- merge(means, sds, by=c("Year", "Species"))

Fenced <- Fenced %>%
  mutate(Consumer_cohens_d = signal/sd) %>%
  select(Year, Species, Consumer_cohens_d )

Fenced_set <- merge(Fenced, NPK, by=c("Year","Species")) 

Fenced_set <- Fenced_set %>%
  filter(Consumer_cohens_d != NPK_cohens_d) %>% #remove comparisons that compare two dummy variables (not meaningful comparison)
  mutate(Treatment = "Fenced",
         Treatment = as.factor(Treatment))

sma(formula = Consumer_cohens_d ~ NPK_cohens_d, data=Fenced_set, method="SMA")

#Soil Fungicide#
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

SF <- data %>%
  filter(Treatment == "SoilDrenchFungicide") %>%
  filter(Fertilizer.f == "0")

compare <- merge(SF, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  mutate(Mass.g.m.2. = replace_na(Mass.g.m.2., 0.01)) %>%
  mutate(control_mass = replace_na(control_mass, 0.01))

#calculate means and pooled standard deviation
means <- compare %>%
  group_by(Year, Species) %>%
  summarize(consumer_mass = mean(Mass.g.m.2.),
            control_mass = mean(control_mass),
            signal = consumer_mass - control_mass)

sds <- compare %>%
  pivot_longer(cols = c("Mass.g.m.2.", "control_mass"), 
               names_to = "Type", values_to="Mass") %>%
  group_by(Year, Species) %>%
  summarize(sd = sd(Mass))

SF <- merge(means, sds, by=c("Year", "Species"))

SF <- SF %>%
  mutate(Consumer_cohens_d = signal/sd) %>%
  select(Year, Species, Consumer_cohens_d )

SF_set <- merge(SF, NPK, by=c("Year","Species")) 

SF_set <- SF_set %>%
  filter(Consumer_cohens_d != NPK_cohens_d) %>% #remove comparisons that compare two dummy variables (not meaningful comparison)
  mutate(Treatment = "SoilDrenchFungicide",
         Treatment = as.factor(Treatment))

sma(formula = Consumer_cohens_d ~ NPK_cohens_d, data=SF_set, method="SMA")

#Insecticide#
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
  mutate(Mass.g.m.2. = replace_na(Mass.g.m.2., 0.01)) %>%
  mutate(control_mass = replace_na(control_mass, 0.01))

#calculate means and pooled standard deviation
means <- compare %>%
  group_by(Year, Species) %>%
  summarize(consumer_mass = mean(Mass.g.m.2.),
            control_mass = mean(control_mass),
            signal = consumer_mass - control_mass)

sds <- compare %>%
  pivot_longer(cols = c("Mass.g.m.2.", "control_mass"), 
               names_to = "Type", values_to="Mass") %>%
  group_by(Year, Species) %>%
  summarize(sd = sd(Mass))

I <- merge(means, sds, by=c("Year", "Species"))

I <- I %>%
  mutate(Consumer_cohens_d = signal/sd) %>%
  select(Year, Species, Consumer_cohens_d )

I_set <- merge(I, NPK, by=c("Year","Species")) 

I_set <- I_set %>%
  filter(Consumer_cohens_d != NPK_cohens_d) %>% #remove comparisons that compare two dummy variables (not meaningful comparison)
  mutate(Treatment = "Insecticide",
         Treatment = as.factor(Treatment))

sma(formula = Consumer_cohens_d ~ NPK_cohens_d, data=I_set, method="SMA")

#All Pesticides#
control <- data %>%
  filter(Treatment == "Control") %>%
  filter(Fertilizer.f == "0") %>%
  mutate(control_mass = Mass.g.m.2.) %>%
  select(Year, Block, Species, control_mass)

All <- data %>%
  filter(Treatment == "AllPesticides") %>%
  filter(Fertilizer.f == "0")

compare <- merge(All, control, by=c("Year", "Block", "Species"), all=TRUE)

compare <- compare %>%
  mutate(Mass.g.m.2. = replace_na(Mass.g.m.2., 0.01)) %>%
  mutate(control_mass = replace_na(control_mass, 0.01))

#calculate means and pooled standard deviation
means <- compare %>%
  group_by(Year, Species) %>%
  summarize(consumer_mass = mean(Mass.g.m.2.),
            control_mass = mean(control_mass),
            signal = consumer_mass - control_mass)

sds <- compare %>%
  pivot_longer(cols = c("Mass.g.m.2.", "control_mass"), 
               names_to = "Type", values_to="Mass") %>%
  group_by(Year, Species) %>%
  summarize(sd = sd(Mass))

All <- merge(means, sds, by=c("Year", "Species"))

All <- All %>%
  mutate(Consumer_cohens_d = signal/sd) %>%
  select(Year, Species, Consumer_cohens_d )

All_set <- merge(All, NPK, by=c("Year","Species")) 

All_set <- All_set %>%
  filter(Consumer_cohens_d != NPK_cohens_d) %>% #remove comparisons that compare two dummy variables (not meaningful comparison)
  mutate(Treatment = "AllPesticides",
         Treatment = as.factor(Treatment))

sma(formula = Consumer_cohens_d ~ NPK_cohens_d, data=I_set, method="SMA")

#combine#
Consumers_set <- FF_set %>%
  rbind(Fenced_set) %>%
  rbind(SF_set) %>%
  rbind(I_set) %>%
  rbind(All_set)

#cohens d
summary(sma(formula = Consumer_cohens_d ~ NPK_cohens_d * Treatment, data=Consumers_set, method="SMA"))

#significant positive relationship, no difference among treatments (likelihood ratio test)

ggplot(Consumers_set, aes(Consumer_cohens_d, NPK_cohens_d, col=Treatment)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  scale_color_locuszoom()
  facet_wrap(~Year)

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
