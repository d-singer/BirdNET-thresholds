# Load necessary libraries
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

### Load species list to translate names
spec_names <- fread("./data/species_names_translate.csv", encoding = "UTF-8")
#spec_names$scientific2 <- tolower(gsub(spec_names$scientific, pattern = " ", replacement = "_"))

### Load manually verified sample data
detsample <- fread("./data/validated_detections.csv", encoding = "UTF-8")

detsample <- detsample %>%
  select(german, conf, validation) %>%
  group_by(german) %>%
  mutate(tp_total = sum(validation))

detsample <- detsample %>% filter(german %in% sort(unique(detsample$german[detsample$validation == 1])))

### Load conditional inference tree models (type 1 and 2)
models1 <- readRDS("./data/ctrees_type1.rds")
models1$ctree_ctrl <- "ctree_ctrl1"

models2 <- readRDS("./data/ctrees_type2.rds")
models2$ctree_ctrl <- "ctree_ctrl2"

# combine model types
models <- rbind(models1, models2)

### Check the number of unique models
models1 %>%
  group_by(german) %>%
  summarise(n_mod = uniqueN(model))

models2 %>%
  group_by(german) %>%
  summarise(n_mod = uniqueN(model))


### Filter for terminal nodes with maximum precision (prec) PER MODEL (not per species)
models <- models %>% group_by(model, german, ctree_ctrl) %>%
  mutate(n_nodes = max(node),
         prec_max = max(prec),
         is_terminal = ifelse(n_nodes == 1 & is.na(rules), 1,
                              ifelse(n_nodes > 1 & is.na(rules), 0, 1)),
         is_max_prec = ifelse(prec == prec_max, 1, 0)) %>%
  filter(is_terminal == 1 & is_max_prec == 1)


### Calculate percentage of included true positives (recall)
models <- models %>% left_join(unique(detsample %>% 
          select(german, tp_total)), by = "german") %>%
          mutate(recall = n_tp / tp_total)


# calculate model performance and rank their performance within species
w = 0.75

models <- models %>% group_by(german) %>% 
  mutate(model_performance = prec * w + recall * (1 - w),
          rank_performance = dense_rank(-model_performance)) 

saveRDS(models, "./data/all_models.rds")


# filter candidate models with maximum performance per species
cand_mods <- models %>% filter(rank_performance == 1)
fwrite(cand_mods, "./data/candidate_models.csv")
saveRDS(cand_mods, "./data/candidate_models.rds")


# filter basic models
base_mods <- models %>% filter(model == "conf" & ctree_ctrl == "ctree_ctrl1")
saveRDS(base_mods, "./data/basic_models.rds")


