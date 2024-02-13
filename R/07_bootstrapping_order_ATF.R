# load required libraries
library(data.table)#
library(dplyr)
library(ggplot2)
library(tidyr)

set.seed(3)

### load species list to translate variables
spec_names <- fread("./data/species_names_translate.csv", encoding="UTF-8")

models <- readRDS("./data/all_models.rds")


atf <- c("ndets10","ndets20", "ndets30", "ndets40","ndets50",
         "ndets60", "ndets70", "ndets80", "ndets90", "ndets99",
         "avgconf", "medconf", "maxconf", "minconf")


opti_spec <- data.frame()


for (i in 1:999)
{
  myatf <- c("none", sample(atf))
  mymodels <- models
  
  for (v in myatf)
  {
    mymodels$remove <- grepl(pattern=v, mymodels$model)
    mymodels <- mymodels[mymodels$remove == F, ]
    
    opti_mods <- mymodels %>% group_by(german) %>%
      mutate(perf_rank = dense_rank(-model_performance))%>% 
      filter(perf_rank == 1) %>%
      summarise(prec = unique(prec), 
                recall = unique(recall),
                model_performance = unique(model_performance),
                remove = v) %>%
      mutate(run = i,
             rank = which(myatf == v)) %>% ungroup()
    
    #return(opti_mods)
    opti_spec <- rbind(opti_spec, opti_mods) %>% ungroup()
  }
  print(i)
}


saveRDS(opti_spec, "./data/bootstrapping_atf.rds")
