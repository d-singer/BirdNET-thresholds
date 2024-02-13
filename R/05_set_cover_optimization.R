# load required packages
library(lpSolve)
library(data.table)
library(dplyr)
library(tidyr)

# load candidate with maximum model performance per species
cand_mods <- fread("./data/candidate_models.csv", encoding="UTF-8")
cand_mods <- cand_mods %>% mutate(model_id = paste0(ifelse(ctree_ctrl == "ctree_ctrl1",
                                  "type1_", "type2_"), model))

uniqueN(cand_mods$model)

#  build matrix: models x species
species <- unique(cand_mods$german)

matrix <- cand_mods %>% select(german, model_id, rank_performance) %>%
  spread(key = model_id, value = rank_performance, fill = 0) %>% 
  select(-german) %>%
  as.matrix()

models <- colnames(matrix)

# Set up the LP problem
lp_model <- lp(direction = "min", 
               objective.in = rep(1, ncol(matrix)), 
               const.mat = matrix, 
               const.dir = rep(">=", length(species)),
               const.rhs = rep(1, length(species)))


# Extract the selected categories
opti_mods <- sort(models[which(lp_model$solution >= 1)])
opti_mods <- gsub(x=opti_mods, "type2_", "")


fwrite(as.data.frame(opti_mods), "data/opti_mods_setcover.csv")
