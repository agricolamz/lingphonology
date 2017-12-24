setwd("/home/agricolamz/work/packages/lingphonology/lingphonology/database_creation")
library(tidyverse)
IPA <- read_tsv("https://raw.githubusercontent.com/clld/phoible/master/data/phoible-segments-features.tsv")
phoible <- read_tsv("https://github.com/clld/phoible/raw/master/data/phoible-phonemes.tsv")
phoible2 <- read_csv("http://phoible.org/parameters.csv?sEcho=1&iSortingCols=1&iSortCol_0=1&sSortDir_0=desc")
phoible3 <- read_tsv("additional_equivalence_class.csv")

phoible %>%
  distinct(GlyphID, Phoneme)%>%
  right_join(IPA, by = c("Phoneme" = "segment")) %>%
  full_join(phoible2[,c("equivalence_class", "name")], by = c("Phoneme" = "name")) %>%
  mutate(GlyphID = ifelse(is.na(GlyphID), NA,
                          paste0("http://phoible.org/parameters/", GlyphID))) %>%
  rename(url = GlyphID,
         segment = Phoneme) %>%
  select(segment, equivalence_class, tone:click, url) %>%
  arrange(url)->
  phoible

lapply(seq_along(phoible3$segment),
       function(i){
         phoible[which(phoible$segment == phoible3$segment[i]), 2] <<- phoible3$equivalence_class[i]
         })

setwd("/home/agricolamz/work/packages/lingphonology/lingphonology/data")
save(phoible, file="phoible.RData", compress='xz')
rm(list = ls())