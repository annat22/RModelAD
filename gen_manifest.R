suppressPackageStartupMessages({
  library(synapser)
  library(readxl)
  library(tidyverse)
})
synLogin(silent=TRUE)

gen_manifest <- function(idkey) {
  cat("Generating manifest file from ID Key\n")  
  temp <- read_xlsx(synGet("syn20820080")$path)
  
  tab <- idkey %>%
    group_by(dataFile) %>%
    summarize(n = n())
  
  manifest.s <- tab %>%
    filter(n==1) %>%
    left_join(idkey) %>%
    mutate(isMultiSpecimen=FALSE)
  
  manifest <- tab %>%
    filter(n>1) %>%
    left_join(mutate(idkey, individualID="", specimenID="")) %>%
    distinct() %>%
    mutate(isMultiSpecimen=TRUE) %>%
    rbind(manifest.s) %>%
    unite("path", dataDir, dataFile , remove=TRUE, sep="/") %>%
    mutate(study = study) %>%
    merge(temp, all.x=TRUE, all.y=TRUE, sort=FALSE) %>%
    select(-n)
  
  return(manifest)
  }