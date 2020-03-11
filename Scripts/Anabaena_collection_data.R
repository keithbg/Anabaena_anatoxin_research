library(tidyverse)
library(ggplot2)

ana <- read_tsv("/Users/KBouma-Gregson/Documents/Special_projects/Anabaena/Anabaena_collection_data.txt") %>% 
  mutate(ELISA= as.numeric(ifelse(ELISA == "ND", 0, ELISA)),
         Species_subdom= ifelse(is.na(Species_subdom), "None", Species_subdom)) %>% 
  mutate(atx= ifelse(ELISA > 0, "Yes", "No")) %>% 
  mutate(assemblage= str_c(Species_dom, Species_subdom, sep= "-"))

ana
unique(ana$ELISA)



ggplot(data= ana) +
  geom_bar(aes(x= assemblage, fill= atx), color= "black") +
  labs(x= "Dominant - Sub-dominant species", y= "Count") +
  theme_bw()




ggplot(data= ana) +
  geom_bar(aes(x= Species_dom, fill= atx)) +
  facet_grid(Species_dom ~ Species_subdom, scales= "free_x") +
  theme_bw()



ggplot(data= ana) +
  geom_jitter(aes(x= Species_dom, y= Species_subdom, color= atx)) +
  theme_bw()
