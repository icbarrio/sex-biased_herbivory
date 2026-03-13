#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     GENDER-BIASED HERBIVORY BY (IN)VERTEBRATE HERBIVORES
#               Isabel C Barrio (isabel@lbhi.is)
#                          18-Jan-2023 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# the dataset contains data on foraging preferences of Arctic moth caterpillars
# (Gynaephora groenlandica) and collared pikas (Ochotona collaris) assessed 
# using cafeteria trials in summer 2013

#set the working directory
setwd("C:/Users/isabel/OneDrive - Menntaský/ISABEL/CANADA/sex cafeterias/R")

#libraries---- 
#packages to be used
library(readxl)      #to import data directly from Excel
library(dplyr)       #to handle data
library(tidyverse)   #to handle data
library(ggplot2)     #to make fancy plots :)
library(lme4)        #to build GLMMs
library(lmerTest)    #to get p-values of GLMMs
library(ggpubr)      #to combine ggplots

# load datasets----
# datasets are in separate files; the first dataset contains data
# for caterpillars and the second for pikas

# for the caterpillar cafeterias, the variables in the dataset are:    
    #"caterpillar": ID code for each caterpillar used in the cafeterias
    #"age": categorical variable for larval instar: young (2 yellow spots) or old (more than 2 yellow spots)
    #"date": date when the cafeteria was run
    #"species": plant species, Salix arctica and Salix reticulata
    #           we will only use Salix arctica here
    #"position": position of the plant in the cafeteria: 1: upper left, 2: upper right, 3: lower left, 4: lower right
    #"code": ID for plant individuals
    #"gender": gender of the plant
    #"ileaves": initial number of leaves
    #"fleaves": final number of leaves
    #"c": dehydration factor; weight gained/lost by each plant species
    #"iweight": initial weight of plant
    #"fweight": final weight of plant
    #"eaten": weight (gr) eaten by the caterpillar, taking into account the dehydration factor (proportion)
    #"herbivory": code for herbivory 0-4 (not eaten to completely eaten)

caterpillars <- read_excel("cafeterias_caterpillars.xlsx", sheet= "cafeterias_caterpillars") %>% 
                  # calculate the proportion of herbivory (value between 0-1) for each plant sample
                  mutate(p.herbivory = herbivory/4,
                         no.herbivory = 4 - herbivory,
                         prop.herbivory = (ileaves-fleaves)/ileaves) %>% 
                  filter(species=="Salix arctica")

# for the pika cafeterias, the variables in the dataset are:    
    #"id": ID code for each pika used in the cafeterias
    #"area": area within the study area (greenhouses, KLM talus)
    #"date": date when the cafeteria was run
    #"trial": number of trial (3 trials per pika)
    #"plantID": ID for the plant individual
    #"sex": gender of the plant
    #"leaves.removed": number of leaves removed from the cafeteria tubes

pikas <- read_excel("cafeterias_pikas.xlsx", sheet= "cafeterias_pikas") %>%
              mutate(gender = sex) %>% 
              #calculate the proportion of herbivory (value between 0-1) 
              #as the proportion of leaves removed from each tube
              mutate(p.herbivory = leaves.removed/5,
                     leaves.remain = 5 - leaves.removed)
pikas.p <- read_excel("cafeterias_pikas.xlsx", sheet= "cafeterias_pikas") %>%
              mutate(gender = sex) %>% 
              group_by(id, gender) %>% 
                summarize(leaves.eaten=sum(leaves.removed)) %>% 
              #calculate the proportion of herbivory (value between 0-1) 
              #as the proportion of leaves removed from each tube
              mutate(p.herbivory = leaves.eaten/15,
                     leaves.remain = 15 - leaves.eaten)                  


#customised functions----
#defining the parameters for our graphs
theme_cafeterias <- function(){    #create a new theme function for the style of graphs
  theme_bw()+                  #use a predefined theme as a base
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
        plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
        legend.text = element_text(size = 12, face = "italic"),
        legend.title = element_blank(),
        legend.position = c(0.9, 0.9))
}


## caterpillars ----
# we have two options for the response variable: 1) our visual estimate 
# of herbivory on each plant sample, or 2) the weight eaten. Both variables
# yielded similar results (see older analyses) so we keep the visual estimate
# because it is easier to compare to the pika cafeterias

# nr of leaves per stem in the plat samples?
mean(caterpillars$ileaves); min(caterpillars$ileaves); max(caterpillars$ileaves)

# does plant gender have an effect? a quick visual exploration
ggplot(caterpillars, aes(x=gender, y=prop.herbivory)) +       
    geom_boxplot()+
    theme_cafeterias()

# do caterpillars respond similarly to plant gender?
ggplot(caterpillars, aes(x = gender, y = prop.herbivory)) +       
    geom_boxplot()+
    facet_wrap(~caterpillar) +
    theme_cafeterias()

# how much did caterpillars consume?
caterpillars %>% group_by(caterpillar) %>% 
                  summarise(mean.cat=mean(prop.herbivory)) %>% 
                    summarise(N=n(),
                              mean=mean(mean.cat), 
                              sd=sd(mean.cat), 
                              se=sd/sqrt(N),
                              ci=1.96*se)

# but...what does the model say?
# we'll start using the biomass eaten as the response variable
# we'll use a LINEAR MIXED MODEL,including caterpillar as a random factor
# this accounts for the non-independence between the 4 measurements for one caterpillar
GLMM.caterpillars<- glmer(cbind(herbivory, no.herbivory) ~ gender +(1|caterpillar), 
                   family=binomial, data=caterpillars)
summary(GLMM.caterpillars)
  plot(GLMM.caterpillars)
  
GLMM.caterpillars<- glmer(cbind((ileaves-fleaves), fleaves) ~ gender +(1|caterpillar), 
                   family=binomial, data=caterpillars)
summary(GLMM.caterpillars)
  plot(GLMM.caterpillars)

summary.caterpillars.eaten <- caterpillars %>%
                                group_by(gender) %>% 
                                 summarise(N=n(),
                                          mean=mean(prop.herbivory), 
                                          sd=sd(p.herbivory), 
                                          se=sd/sqrt(N),
                                          ci=1.96*se)

fig.caterpillars <- ggplot(summary.caterpillars.eaten, aes(x=gender, y=mean)) +       
                      geom_bar(position="dodge", stat="identity", fill = "#4A708B") +
                      geom_errorbar(aes(x=gender, ymin=mean-se, ymax=mean+se), 
                                    width=.1, position=position_dodge(0.9)) +
                      annotate(geom="text", x=1.5, y=0.9, label="***", size=10) +
                      scale_y_continuous(limits=c(0,1)) +
                      labs(title="a. Arctic moth caterpillars",
                           x="plant sex", y="proportion of herbivory\n") + 
                      theme_cafeterias()

#validate the assumptions of our model
res.c <-resid(GLMM.caterpillars)
#check normality of the residuals
qqnorm(res.c); qqline(res.c)  #we want to see the residuals (dots) over the solid line
                            #we can be more or less happy with that
#check homogeneity of the residuals
boxplot(res.c~caterpillars$gender); abline(h=0, col="red")
                            #we want to see the same spread for both boxes 
                            #here too, we can be more or less happy (we don't want to see huge deviations)
plot(res.c~fitted(GLMM.caterpillars)); abline(h=0, col="red")
                            #we want to see the same spread along the x-axis 
                            #the plot looks good :)
#so we can be quite happy with our model :)


##pikas----
# does plant gender have an effect? a quick visual exploration
ggplot(pikas.p, aes(x=gender, y=p.herbivory)) +       
    geom_boxplot()+
    theme_cafeterias()

# do pikas respond similarly to plant gender?
ggplot(pikas.p, aes(x=gender, y=p.herbivory)) +       
    geom_boxplot()+
    facet_wrap(~id) +
    theme_cafeterias()

# how much did pikas consume?
pikas.p %>% group_by(id) %>% 
                  summarise(mean.pika=mean(p.herbivory)) %>% 
                    summarise(N=n(),
                              mean=mean(mean.pika), 
                              sd=sd(mean.pika), 
                              se=sd/sqrt(N),
                              ci=1.96*se)

#model
GLMM.pikas<- glmer(cbind(leaves.eaten, leaves.remain) ~ gender + (1|id), 
                   family=binomial, data=pikas.p)
summary(GLMM.pikas) #no effect of plant gender
  plot(GLMM.pikas)

summary.pikas.eaten <- pikas.p %>%
                                group_by(gender) %>% 
                                 summarise(N=n(),
                                          mean=mean(p.herbivory), 
                                          sd=sd(p.herbivory), 
                                          se=sd/sqrt(N),
                                          ci=1.96*se)

fig.pikas <- ggplot(summary.pikas.eaten, aes(x=gender, y=mean)) +       
                      geom_bar(position="dodge", stat="identity", fill = "#4A708B") +
                      geom_errorbar(aes(x=gender, ymin=mean-se, ymax=mean+se), 
                                    width=.1, position=position_dodge(0.9)) +
                      scale_y_continuous(limits=c(0,1)) +
                      labs(title="b. Collared pikas",
                           x="plant sex", y="proportion of herbivory\n") + 
                      theme_cafeterias()

#validate the assumptions of our model
res.p <-resid(GLMM.pikas)
#check normality of the residuals
qqnorm(res.p); qqline(res.p)  #we want to see the residuals (dots) over the solid line
                            #we can be more or less happy with that
#check homogeneity of the residuals
boxplot(res.p~pikas.p$gender); abline(h=0, col="red")
                            #we want to see the same spread for both boxes 
                            #here too, we can be more or less happy (we don't want to see huge deviations)
plot(res.p~fitted(GLMM.pikas)); abline(h=0, col="red")
                            #we want to see the same spread along the x-axis 
                            #the plot looks good :)
#so we can be quite happy with our model :)



## Figure caterpillars vs pikas
ggarrange(fig.caterpillars, fig.pikas, 
          ncol = 2, nrow = 1)





# data from Jill Cameron on sex ratios and herbivory on Salix arctica ----

## sex ratio ----
# we have a list of all S.arctica individuals identified in two
# 30x30 m plots (upper and lower) in Pika Camp in July 2013

salix_sr <- read_excel("data_JC_Salix_arctica.xlsx", sheet= "sex_ratio")

salix_sr %>% filter(!Gender == "U") %>% 
  group_by(plot, Gender) %>% 
     summarise(n = n())
 # female to male ratio is 53:25 in the lower plot and 148:72 in the upper plot
      53/25
      148/72
 # in both cases sex ratio is ca. 2:1


## herbivory ----
# we have the number of leaves eaten (out of those sampled)
salix_herb <- read_excel("data_JC_Salix_arctica.xlsx", sheet= "herbivory") %>% 
        mutate(p.herbivory = nr.leaves.herbivorized/nr.leaves,
               leaves.no.herb = nr.leaves - nr.leaves.herbivorized) 

summary.herbivory <- salix_herb %>%
                       group_by(plot, sex) %>% 
                                 summarise(N=n(),
                                          mean=mean(p.herbivory), 
                                          sd=sd(p.herbivory), 
                                          se=sd/sqrt(N),
                                          ci=1.96*se)

fig.herbivory <- ggplot(summary.herbivory, aes(x = plot, y = mean, fill = sex)) +       
                      geom_col(position = position_dodge(), width = 0.8) +
                      geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                          width = 0.2, position = position_dodge(width = 0.8)) +
                      labs(title="Invertebrate herbivory on Salix arctica",
                           x="plot", y="proportion of herbivory\n") + 
                      theme_cafeterias()

GLMM.salix <- glmer(
  cbind(nr.leaves.herbivorized, leaves.no.herb) ~ sex + (1|individual),
  family = binomial,
  data = salix_herb,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 100000)))

summary(GLMM.salix)
  plot(GLMM.salix)


# looking at presence/absence of herbivory on Salix arctica
Salarc_pa <- salix_herb %>%
  # create 0/1 presence-absence per branch
  mutate(herbivory_present = ifelse(nr.leaves.herbivorized > 0, 1, 0)) %>%
  # check for each individual plant, if any of the branches has herbivory signs
  group_by(individual, sex, plot) %>%
    summarise(herbivory_any = any(herbivory_present == 1)) %>%
  ungroup()

Salarc_sex_summary <- Salarc_pa %>%
  group_by(plot, sex) %>%
  summarise(n_individuals = n(),
            n_with_herbivory = sum(herbivory_any),
            prop_with_herbivory = n_with_herbivory / n_individuals)

