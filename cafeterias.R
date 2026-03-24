#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     GENDER-BIASED HERBIVORY BY (IN)VERTEBRATE HERBIVORES
#               Isabel C Barrio (isabel@lbhi.is)
#                          24-Mar-2026 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# the dataset contains data on foraging preferences of Arctic moth caterpillars
# (Gynaephora groenlandica) and collared pikas (Ochotona collaris) assessed 
# using cafeteria trials in summer 2013

# libraries---- 
# packages to be used
library(readxl)      # to import data directly from Excel
library(dplyr)       # to handle data
library(tidyverse)   # to handle data
library(ggplot2)     # to make fancy plots :)
library(lme4)        # to build GLMMs
library(lmerTest)    # to get p-values of GLMMs
library(ggpubr)      # to combine ggplots


# load datasets ----
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
    #"gender": plant sex (male, female)
    #"ileaves": initial number of leaves
    #"fleaves": final number of leaves
    #"c": dehydration factor; weight gained/lost by each plant species
    #"iweight": initial weight of plant
    #"fweight": final weight of plant
    #"eaten": weight (gr) eaten by the caterpillar, taking into account the dehydration factor (proportion)
    #"herbivory": code for herbivory 0-4 (not eaten to completely eaten)
caterpillars <- read_excel("data/cafeterias_caterpillars.xlsx", sheet= "cafeterias_caterpillars") %>% 
                  # calculate the proportion of herbivory (value between 0-1) for each plant sample
                  # herbivory was recorded on a scale 0-4, so 1 would be completely eaten
                  mutate(p.herbivory = herbivory/4,
                         no.herbivory = 4 - herbivory,
                         # alternative way: proportion of initial leaves that are consumed
                         prop.herbivory = (ileaves-fleaves)/ileaves) %>% 
                  filter(species == "Salix arctica")

# for the pika cafeterias, the variables in the dataset are:    
    #"id": ID code for each pika used in the cafeterias
    #"area": area within the study area (greenhouses, KLM talus)
    #"date": date when the cafeteria was run
    #"trial": number of trial (3 trials per pika)
    #"plantID": ID for the plant individual
    #"sex": plant sex (male, female)
    #"leaves.removed": number of leaves removed from the cafeteria tubes

pikas <- read_excel("data/cafeterias_pikas.xlsx", sheet= "cafeterias_pikas") %>%
              mutate(gender = sex) %>% 
              # calculate the proportion of herbivory (value between 0-1) 
              # as the proportion of leaves removed from each tube
              mutate(p.herbivory = leaves.removed/5,
                     leaves.remain = 5 - leaves.removed)
# pooling the three subsequent cafeteria trials together (15 leaves total)
# we will use this for final analyses
pikas.p <- read_excel("data/cafeterias_pikas.xlsx", sheet= "cafeterias_pikas") %>%
              mutate(gender = sex) %>% 
              group_by(id, gender) %>% 
                summarize(leaves.eaten = sum(leaves.removed)) %>% 
              # calculate the proportion of herbivory (value between 0-1) 
              # as the proportion of leaves removed from each tube
              mutate(p.herbivory = leaves.eaten/15,
                     leaves.remain = 15 - leaves.eaten)                  


# customised functions ----
# define the parameters for our graphs
theme_cafeterias <- function(){    # create a new theme function for the style of graphs
  theme_bw()+                      # use a predefined theme as a base
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

# does plant sex have an effect? a quick visual exploration
ggplot(caterpillars, aes(x=gender, y=prop.herbivory)) +       
    geom_boxplot()+
    theme_cafeterias()

# do caterpillars respond similarly to plant sex?
ggplot(caterpillars, aes(x = gender, y = prop.herbivory)) +       
    geom_boxplot()+
    facet_wrap(~caterpillar) +
    theme_cafeterias()

# how much did caterpillars consume?
caterpillars %>% group_by(caterpillar) %>% 
                  summarise(mean.cat = mean(prop.herbivory)) %>% 
                    summarise(N = n(),
                              mean = mean(mean.cat), 
                              sd = sd(mean.cat), 
                              se = sd/sqrt(N),
                              ci = 1.96*se)

# to check differences statistically we'll use a LINEAR MIXED MODEL,
# including caterpillar as a random factor to account for the non-independence 
# between the 4 measurements for each caterpillar

# we can check the proportion of herbivory on each sample
GLMM.caterpillars<- glmer(cbind(herbivory, no.herbivory) ~ gender +(1|caterpillar), 
                   family=binomial, data=caterpillars)
summary(GLMM.caterpillars)
  plot(GLMM.caterpillars)
  
# but we can also check the number of leaves with herbivory after the trial
# we report these values in the paper, because it is more comparable to the pika trials
GLMM.caterpillars<- glmer(cbind((ileaves-fleaves), fleaves) ~ gender +(1|caterpillar), 
                   family=binomial, data=caterpillars)
summary(GLMM.caterpillars)
  plot(GLMM.caterpillars)

summary.caterpillars.eaten <- caterpillars %>%
                                group_by(gender) %>% 
                                 summarise(N = n(),
                                          mean = mean(prop.herbivory), 
                                          sd = sd(prop.herbivory), 
                                          se = sd/sqrt(N),
                                          ci = 1.96*se)

fig.caterpillars <- ggplot(summary.caterpillars.eaten, aes(x = gender, y = mean)) +       
                      geom_bar(position = "dodge", stat = "identity", fill = "#4A708B") +
                      geom_errorbar(aes(x = gender, ymin = mean-se, ymax = mean+se), 
                                    width = .1, position = position_dodge(0.9)) +
                      annotate(geom = "text", x = 1.5, y = 0.9, label = "***", size = 10) +
                      scale_y_continuous(limits = c(0,1)) +
                      labs(title = "a. Arctic moth caterpillars",
                           x = "plant sex", y = "proportion of herbivory\n") + 
                      theme_cafeterias()

# validate the assumptions of our model
res.c <- resid(GLMM.caterpillars)
# check normality of the residuals
qqnorm(res.c); qqline(res.c)  # we want to see the residuals (dots) over the solid line
                              # we can be more or less happy with that
# check homogeneity of the residuals
boxplot(res.c ~ caterpillars$gender); abline(h=0, col = "red")
                            # we want to see the same spread for both boxes 
                            # here too, we can be more or less happy (we don't want to see huge deviations)
plot(res.c ~ fitted(GLMM.caterpillars)); abline(h = 0, col = "red")
                            # we want to see the same spread along the x-axis 
                            # the plot looks good :)
# so we can be quite happy with our model :)



## pikas----
# does plant sex have an effect? a quick visual exploration
ggplot(pikas.p, aes(x = gender, y = p.herbivory)) +       
    geom_boxplot()+
    theme_cafeterias()

# do pikas respond similarly to plant sex?
ggplot(pikas.p, aes(x = gender, y = p.herbivory)) +       
    geom_boxplot()+
    facet_wrap(~id) +
    theme_cafeterias()

# how much did pikas consume?
pikas.p %>% group_by(id) %>% 
                  summarise(mean.pika = mean(p.herbivory)) %>% 
                    summarise(N = n(),
                              mean = mean(mean.pika), 
                              sd = sd(mean.pika), 
                              se = sd/sqrt(N),
                              ci = 1.96*se)

# model
GLMM.pikas<- glmer(cbind(leaves.eaten, leaves.remain) ~ gender + (1|id), 
                   family = binomial, data = pikas.p)
summary(GLMM.pikas) #no effect of plant sex
  plot(GLMM.pikas)

summary.pikas.eaten <- pikas.p %>%
                                group_by(gender) %>% 
                                 summarise(N = n(),
                                          mean = mean(p.herbivory), 
                                          sd = sd(p.herbivory), 
                                          se = sd/sqrt(N),
                                          ci = 1.96*se)

fig.pikas <- ggplot(summary.pikas.eaten, aes(x = gender, y = mean)) +       
                      geom_bar(position = "dodge", stat = "identity", fill = "#4A708B") +
                      geom_errorbar(aes(x = gender, ymin = mean-se, ymax = mean+se), 
                                    width = .1, position = position_dodge(0.9)) +
                      scale_y_continuous(limits = c(0,1)) +
                      labs(title = "b. Collared pikas",
                           x = "plant sex", y = "proportion of herbivory\n") + 
                      theme_cafeterias()

# validate the assumptions of our model
res.p <-resid(GLMM.pikas)
# check normality of the residuals
qqnorm(res.p); qqline(res.p)  # we want to see the residuals (dots) over the solid line
                              # we can be more or less happy with that
#check homogeneity of the residuals
boxplot(res.p ~ pikas.p$gender); abline(h=0, col="red")
                            # we want to see the same spread for both boxes 
                            # here too, we can be more or less happy (we don't want to see huge deviations)
plot(res.p ~ fitted(GLMM.pikas)); abline(h = 0, col="red")
                            # we want to see the same spread along the x-axis 
                            # the plot looks good :)
# so we can be quite happy with our model :)



## Figure caterpillars vs pikas
ggarrange(fig.caterpillars, fig.pikas, 
          ncol = 2, nrow = 1)
