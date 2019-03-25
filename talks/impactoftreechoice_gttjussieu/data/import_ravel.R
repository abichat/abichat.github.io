library(tidyverse)
library(biomformat)

biom <- read_biom("data/ravel.biom")

otu_abund <- 
  biom %>% 
  biom_data() %>% 
  as("matrix") %>% 
  as.tibble(rownames = "OTU")

taxtable <-
  biom %>% 
  observation_metadata() %>% 
  as("matrix") %>% 
  as.tibble(rownames = "OTU") %>% 
  `colnames<-`(c("OTU", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"))

otu_abund <- 
  taxtable %>% 
  select(OTU, Genus) %>% 
  left_join(otu_abund, by = "OTU") %>% 
  filter(Genus != "NA") %>% 
  select(-OTU) %>% 
  group_by(Genus) %>% 
  summarise_all(sum) %>%
  mutate(., sum = rowSums(select(., -Genus))) %>% 
  arrange(desc(sum)) %>% 
  select(-sum) %>% 
  rename(Taxa = Genus)


taxtable <-
  taxtable %>% 
  select(-OTU, - Species) %>% 
  arrange_all()


write_csv(otu_abund, "data/data_abund.csv")
read_csv("data/data_abund.csv", col_types = cols(.default = "d", Taxa = "c"))

write_csv(taxtable, "data/data_tax.csv")
read_csv("data/data_tax.csv", col_types = "cccccc")

read_tsv("data/sample_metadata.tsv", col_types = "ccddccdd") %>% 
  rename(Sample = X1) %>% 
  select(-Depth, -RealDepth) %>% 
  write_csv("data/data_sample.csv")

read_csv("data/data_sample.csv", col_types = "ccddcc")
