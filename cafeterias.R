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
# datasets for caterpillars and pikas are contained in a single xls file in two tabs

# for the caterpillar cafeterias, the variables in the dataset are:    
    #"caterpillar": ID code for each caterpillar used in the cafeterias
    #"age": categorical variable for larval instar: young (2 yellow spots) or old (more than 2 yellow spots)
    #"date": date when the cafeteria was run
    #"species": plant species, Salix arctica 
    #"position": position of the plant in the cafeteria: 1: upper left, 2: upper right, 3: lower left, 4: lower right
    #"plant.code": ID for plant individuals
    #"plant.sex": plant sex (male, female)
    #"ileaves": initial number of leaves
    #"fleaves": final number of leaves
    #"c": dehydration factor; weight gained/lost by each plant species
    #"iweight": initial weight of plant
    #"fweight": final weight of plant
    #"eaten": weight (g) eaten by the caterpillar, taking into account the dehydration factor (proportion)
    #"herbivory": code for herbivory 0-4 (not eaten to completely eaten)

caterpillars <- read_excel("data/cafeterias.xlsx", sheet= "cafeterias_caterpillars") %>% 
                  # calculate herbivory as the proportion of initial leaves that are consumed
                  mutate(prop.herbivory = (ileaves-fleaves)/ileaves) 

# for the pika cafeterias, the variables in the dataset are:    
    #"pika": ID code for each pika used in the cafeterias
    #"area": area within the study area (greenhouses, KLM talus)
    #"date": date when the cafeteria was run
    #"trial": number of trial (3 trials per pika)
    #"plant.code": ID for the plant individual
    #"plant.sex": plant sex (male, female)
    #"leaves.removed": number of leaves removed from the cafeteria tubes

pikas <- read_excel("data/cafeterias_pikas.xlsx", sheet= "cafeterias_pikas") %>%
              # calculate the proportion of herbivory (value between 0-1) 
              # as the proportion of leaves removed from each tube
              mutate(p.herbivory = leaves.removed/5,
                     leaves.remain = 5 - leaves.removed)

pikas.p <- read_excel("data/cafeterias.xlsx", sheet= "cafeterias_pikas") %>%
              # pooling the three subsequent cafeteria trials together (15 leaves total)
              group_by(pika, plant.sex) %>% 
                summarize(leaves.eaten = sum(leaves.removed)) %>% ungroup() %>% 
              # calculate the proportion of herbivory (value between 0-1) 
              # as the proportion of leaves removed from each tube
              mutate(prop.herbivory = leaves.eaten/15,
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
# we also collected data on: 1) visual estimate of herbivory per plant sample (scale 0-4)
# and 2) the weight of the plant material eaten (initial - final weight corrected with desiccation factor). 
# Both variables yielded similar results (see older analyses), consistent with the proportion
# of leaves eaten, so we keep the latter because it is easier to compare to the pika cafeterias

# some descriptive stuff
# nr of leaves per stem in the plat samples?
mean(caterpillars$ileaves); min(caterpillars$ileaves); max(caterpillars$ileaves)

# does plant sex have an effect? a quick visual exploration
ggplot(caterpillars, aes(x = plant.sex, y = prop.herbivory)) +       
    geom_boxplot()+
    theme_cafeterias()

# do caterpillars respond similarly to plant sex?
ggplot(caterpillars, aes(x = plant.sex, y = prop.herbivory)) +       
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

# to check differences statistically we'll use a GENERALIZED LINEAR MIXED MODEL,
# including caterpillar as a random factor to account for the non-independence 
# between the 4 measurements (4 plant samples) for each caterpillar
# as response variable we include the number of damaged leaves and the remaining number 
# of undamaged leaves, for the models to be fully comparable to the pika trials
GLMM.caterpillars<- glmer(cbind((ileaves-fleaves), fleaves) ~ plant.sex +(1|caterpillar), 
                      family = binomial, data = caterpillars)
summary(GLMM.caterpillars)
  plot(GLMM.caterpillars)

summary.caterpillars.eaten <- caterpillars %>%
                                group_by(plant.sex) %>% 
                                 summarise(N = n(),
                                          mean = mean(prop.herbivory), 
                                          sd = sd(prop.herbivory), 
                                          se = sd/sqrt(N),
                                          ci = 1.96*se)

fig.caterpillars <- ggplot(summary.caterpillars.eaten, aes(x = plant.sex, y = mean)) +       
                      geom_bar(position = "dodge", stat = "identity", fill = "#4A708B") +
                      geom_errorbar(aes(x = plant.sex, ymin = mean-se, ymax = mean+se), 
                                    width = .1, position = position_dodge(0.9)) +
                      annotate(geom = "text", x = 1.5, y = 0.9, label = "***", size = 10) +
                      scale_y_continuous(limits = c(0,1)) +
                      labs(title = "a. Arctic moth caterpillars",
                           x = "plant sex", y = "proportion of leaves with signs of caterpillar herbivory\n") + 
                      theme_cafeterias()

# validate the assumptions of our model
res.c <- resid(GLMM.caterpillars)

par(mfrow = c(1,3))
# check normality of the residuals
qqnorm(res.c); qqline(res.c)  # we want to see the residuals (dots) over the solid line
                              # we can be more or less happy with that
# check homogeneity of the residuals
boxpl.c <- boxplot(res.c ~ caterpillars$plant.sex, xlab = "Plant sex", 
                      ylab = "Model residuals"); abline(h=0, col = "red")
                            # we want to see the same spread for both boxes 
                            # here too, we can be more or less happy (we don't want to see huge deviations)
res.c <- plot(res.c ~ fitted(GLMM.caterpillars), xlab = "Fitted values", 
                      ylab = "Model residuals"); abline(h = 0, col = "red")
                            # we want to see the same spread along the x-axis 
                            # the plot looks good :)
# so we can be quite happy with our model :)



## pikas----
# does plant sex have an effect? a quick visual exploration
ggplot(pikas.p, aes(x = plant.sex, y = prop.herbivory)) +       
    geom_boxplot()+
    theme_cafeterias()

# do pikas respond similarly to plant sex?
ggplot(pikas.p, aes(x = plant.sex, y = prop.herbivory)) +       
    geom_boxplot()+
    facet_wrap(~pika) +
    theme_cafeterias()

# how much did pikas consume?
pikas.p %>% group_by(pika) %>% 
                  summarise(mean.pika = mean(prop.herbivory)) %>% 
                    summarise(N = n(),
                              mean = mean(mean.pika), 
                              sd = sd(mean.pika), 
                              se = sd/sqrt(N),
                              ci = 1.96*se)

# model
GLMM.pikas<- glmer(cbind(leaves.eaten, leaves.remain) ~ plant.sex + (1|pika), 
                   family = binomial, data = pikas.p)
summary(GLMM.pikas) #no effect of plant sex
  plot(GLMM.pikas)
  
  
summary.pikas.eaten <- pikas.p %>%
                                group_by(plant.sex) %>% 
                                 summarise(N = n(),
                                          mean = mean(prop.herbivory), 
                                          sd = sd(prop.herbivory), 
                                          se = sd/sqrt(N),
                                          ci = 1.96*se)

fig.pikas <- ggplot(summary.pikas.eaten, aes(x = plant.sex, y = mean)) +       
                      geom_bar(position = "dodge", stat = "identity", fill = "#4A708B") +
                      geom_errorbar(aes(x = plant.sex, ymin = mean-se, ymax = mean+se), 
                                    width = .1, position = position_dodge(0.9)) +
                      scale_y_continuous(limits = c(0,1)) +
                      labs(title = "b. Collared pikas",
                           x = "plant sex", y = "proportion of leaves removed by pikas\n") + 
                      theme_cafeterias()

# validate the assumptions of our model
res.p <- resid(GLMM.pikas)
par(mfrow = c(1,3))
# check normality of the residuals
qqnorm(res.p); qqline(res.p)  # we want to see the residuals (dots) over the solid line
                              # we can be more or less happy with that
#check homogeneity of the residuals
boxplot(res.p ~ pikas.p$plant.sex, xlab = "Plant sex", 
                      ylab = "Model residuals"); abline(h=0, col="red")
                            # we want to see the same spread for both boxes 
                            # here too, we can be more or less happy (we don't want to see huge deviations)
plot(res.p ~ fitted(GLMM.pikas), xlab = "Fitted values", 
                      ylab = "Model residuals"); abline(h = 0, col="red")
                            # we want to see the same spread along the x-axis 
                            # the plot looks good :)
# so we can be quite happy with our model :)



## Figure caterpillars vs pikas
ggarrange(fig.caterpillars, fig.pikas, 
          ncol = 2, nrow = 1)
