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

# create phonological_report ----------------------------------------------
phonological_report <- readLines("report.Rmd")
setwd("/home/agricolamz/work/packages/lingphonology/lingphonology/data")
save(phonological_report, file="phonological_report.RData", compress='xz')
rm(list = ls())

# create zilo_dict --------------------------------------------------------
setwd("/home/agricolamz/work/packages/lingphonology/lingphonology/database_creation")
zilo_dict <- read_tsv("zilo_dict.csv")
setwd("/home/agricolamz/work/packages/lingphonology/lingphonology/data")
save(zilo_dict, file="zilo_dict.RData", compress='xz')

# create zilo_correspondenses ---------------------------------------------
zilo_converter <- read_delim("https://raw.githubusercontent.com/agricolamz/andi_cyrillic_to_IPA/master/andi%20cyrillic%20to%20IPA.map", skip = 16, delim = " ", col_names = FALSE)
zilo_converter <- zilo_converter[-c(1:2, 46, 237:238, 686:716), -2]
colnames(zilo_converter) <- c("transcription", "ipa")

zilo_converter %>%
  mutate(transcription = tolower(transcription),
         transcription = gsub("i|і|Ӏ|I|ӏ", "1", transcription),
         ipa = gsub("'", "ʼ", ipa)) %>%
  mutate(transcription = gsub("1", "I", transcription)) %>%
  distinct() ->
  zilo_converter

setwd("/home/agricolamz/work/packages/lingphonology/lingphonology/data")
save(zilo_converter, file="zilo_correspondences.RData", compress='xz')
