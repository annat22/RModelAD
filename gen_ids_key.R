suppressPackageStartupMessages({
  library(synapser)
  library(readxl)
  library(tidyverse)
})

gen_ids_key <- function(wdir) {
    cat("Generating ID key from data files\n")
    key <- read_csv("files_key.csv")
    idkey <- c()
    
    for (i in 1:nrow(key)) {
      ki <- key[i,]
      if (ki$isMultiSpecimen) {
        nm <- read_csv(file.path(ki$dataDir, ki$dataFile), col_types = cols(.default = "c"))
        idkey <- tibble(assay = ki$assay,
                        dataDir = ki$dataDir,
                        dataFile = ki$dataFile,
                        individualID = pull(select(nm, ki$individualID)),
                        specimenID = pull(select(nm, ki$specimenID))) %>%
          rbind(idkey)
        
      } else if (!ki$isMultiSpecimen) {
        dtf <- list.files(ki$dataDir, full.names=FALSE)
        nm <- str_split(basename(dtf), "_", simplify = TRUE) %>%
          as.matrix(nr=length(dtf))
        idkey <- tibble(assay = ki$assay, 
                        dataDir = ki$dataDir, dataFile = dtf,
                        individualID = nm[,1], specimenID = nm[,2]) %>%
          rbind(idkey)
      }
    }
  return(idkey)
  }