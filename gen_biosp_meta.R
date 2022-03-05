## Generating biospecimen metadata file for AD Knowledge Portal
## Takes a character vector of either animal names or sample names
## returns metadata as data frame
## if filterValue is provided, output is filtered for that value based on filter_key
## filter_key takes "study","createdBy","type", or "line"; default is to filter by study
suppressPackageStartupMessages({
  library(synapser)
  library(readxl)
  library(tidyverse)
})
source("https://raw.github.com/TheJacksonLaboratory/ClimbR/master/climbGET.R")
synLogin(silent=TRUE)

gen_biosp_meta <- function(animalnames=NULL, samplenames=NULL, filter_key=c("study","createdBy","type","line"), filter_value=NULL) {
  
  # load template from synapse
  temp <- read_xlsx(synGet("syn12973252")$path)
  # load mapping table from synapse
  csmap <- read_csv(synGet("syn26137185")$path) %>%
    filter(synapseFile=="biospecimen") %>%
    filter(!is.na(climbField)) %>%
    mutate(climbField=if_else(climbField=="type" & synapseField=="organ", 
                              "organ", climbField))
  
  if (!is.null(animalnames)) {
    cat("Generating biospecimen metadata file from animal names\n")
    inds <- climbGET(animalnames, facet="animals", queryField = "animalName") %>%
      select(animalName, sourceMaterialKeys=materialKey)
    animalkeys <- select(inds, sourceMaterialKeys) %>% pull()
    samples <- climbGET(animalkeys, "samples", "sourceMaterialKeys")
    
    } else if (!is.null(samplenames)) {
      cat("Generating biospecimen metadata file from sample names\n")
      
      samples <- climbGET(samplenames, "samples", "sampleName")
      animalkeys <- select(samples, sourceMaterialKeys) %>% pull()
      inds <- climbGET(animalkeys, "animals", "materialKey") %>%
        select(animalName, sourceMaterialKeys=materialKey)
    }
    
  ## TODO export empty template if samples are not on climb
  
  samples <- samples %>%
    mutate(organ = type,
           samplingAgeUnits = "weeks") %>%
    left_join(inds) %>%
    select(csmap$climbField)
  
  if (!is.null(filter_value)) {
    samples <- samples %>% 
      filter(select(., matches(filter_key[1]))==as.character(filter_value))
    }
    
  meta <- merge(temp, samples, all.x=FALSE, all.y=TRUE, by.x=csmap$synapseField, by.y=csmap$climbField, sort=FALSE)
  return(meta)
  }