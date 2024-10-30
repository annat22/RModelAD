## Generating individual metadata file for AD Knowledge Portal
## Takes a character vector of animal names as stored on climb.
## returns metadata as data frame

suppressPackageStartupMessages({
  library(synapser)
  library(readxl)
  library(tidyverse)
  library(lubridate)
})
source("https://raw.github.com/TheJacksonLaboratory/ClimbR/master/climbGET.R")
synLogin(silent=TRUE)

gen_ind_meta_id <- function(climbID, workgroup=c("modelad", "marmoad")) {
  
  # load template from synapse
  temp <- read_xlsx(synGet("syn21084071")$path, sheet = 1)
  # load mapping table from synapse
  csmap0 <- read_csv(synGet("syn26137185")$path) %>%
    filter(synapseFile=="individual") %>%
    mutate(climbField=if_else(climbField=="origin" & synapseField=="individualIdSource", 
                              "individualIdSource", climbField))
  
  csmap <- csmap0 %>%
    filter(!is.na(climbField))
  
  cat("Generating individual metadata file from climbID\n")
  # get animal metadata from climb
  ind_c <- climbGET(queryValues=climbID, facet="animals", queryField="animalID") 
  inds <- ind_c %>%
    # calculate age from dateBorn and dateExit
    mutate(dateExit = as_date(ymd_hms(dateExit)),
           dateBorn = as_date(ymd_hms(dateBorn)),
           ageDeath = difftime(dateExit, dateBorn, units = "days"),
           ageDeath = round(as.double(gsub(" days", "", ageDeath))),
           ageDeathUnits = "days")

  # get lines info from climb
  lines <-  climbGET(ind_c$lineKey, "lines", "lineKey") %>%
    select(lineName=name, shortName, stock, backgroundLine) %>%
    mutate(animalId = climbID)
  
  ## TODO export empty template if animals are not on climb
  
  # get genotypes from climb
  gt_c <- climbGET(climbID, "genotypes", "animalId") 
  gts <- gt_c %>%
    mutate(genotype = paste(assay, genotype, sep="_")) %>%
    select(animalId, assay, genotype) %>%
    pivot_wider(names_from = assay, values_from = genotype) %>%
    unite("genotype", -1, sep= "; ")
  
   
  # get room info from climb
 # rooms <- climbGET(climbID, "housings", "animalId") %>%
  #  distinct(animalId, .keep_all=TRUE) %>%
   # mutate(currentLocation = gsub("^.* > .* > (.*)$", "\\1", currentLocation)) %>%
    #select(animalId, currentLocation)
  
  # combine metadata from climb and add matingID and birthID (not available via API)
  meta_c <- left_join(inds, gts) %>% 
    left_join(lines) %>% 
    mutate(birthID=NA, matingID=NA) %>%
    #left_join(rooms) %>%
    mutate(individualIdSource = origin) %>%
    select(csmap$climbField)
  
  # join metadata from climb with synapse template
  meta <- merge(temp, meta_c, all.x=FALSE, all.y=TRUE, by.x=csmap$synapseField, by.y=csmap$climbField, sort=FALSE) %>%
    select(csmap0$synapseField)
  
  return(meta)
  }