
library(tidyverse)
library(ggplot2)


## Read in data

r16s <- read_tsv("Data/results_16S.txt") %>% 
  select(-Fasta_sequence)

r16s_rel_abund <- r16s %>%
  

metadata <- read_tsv("Data/results_metadata.txt")


tax_df <- str_split_fixed(r16s$taxonomy, ";", n= 6) %>% 
  as_tibble() %>% 
  rename(Domain= V1, Phylum= V2, Class= V3, Order= V4, Family= V5, Genus= V6) %>% 
  mutate_all(., list(~ str_replace_all(., "^.*__", ""))) %>% 
  mutate(Genus= ifelse(Genus == "", str_c(Family, "unknown", sep= "-"), Genus))


r16s_df <- cbind(r16s, tax_df) %>% 
  pivot_longer(names_to= "sampleID", values_to= "read_copies", cols= contains("W")) %>% 
  group_by(sampleID) %>% 
  mutate(rel_abund= read_copies / sum(read_copies)) %>% 
  ungroup() %>% 
  left_join(., metadata, by= c("sampleID" = "sample-id"))



## Make plots

ggplot(r16s_df, aes(x= sampleID, y= rel_abund)) +
  geom_bar(aes(fill= Family), position= "stack", stat= "identity") +
  labs(x= "Sample", y= "Relative abundance") +
  scale_y_continuous(expand= c(0, 0)) +
  facet_grid(.~Species_dom, scales= "free_x") +
  theme_classic() +
  theme(axis.text.x= element_text(angle= 90, hjust= 0.5, vjust= 0.5))


ggplot(r16s_df, aes(x= sampleID, y= rel_abund)) +
  geom_bar(aes(fill= Family), position= "stack", stat= "identity") +
  labs(x= "Sample", y= "Relative abundance") +
  scale_y_continuous(expand= c(0, 0)) +
  facet_grid(.~Species_dom, scales= "free_x") +
  theme_classic() +
  theme(axis.text.x= element_text(angle= 90, hjust= 0.5, vjust= 0.5))



