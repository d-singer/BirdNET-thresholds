# Load required libraries
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(partykit)

# Import human-validated detections
validated_detections <- fread("./data/validated_detections.csv", encoding = "UTF-8")

# Import aggregated time-series features
aggtimefeatures <- readRDS("./data/aggregated_timeseries_features.rds")

# Filter species with at least one human validation
species <- validated_detections %>%
  filter(validation == 1) %>%
  pull(german) %>%
  unique() %>%
  sort()

# Merge aggregated time-series features with human validations
validated_detections <- validated_detections %>%
  select(timestamp, Plot_ID, german, validation) %>%
  filter(german %in% species) %>%
  left_join(aggtimefeatures, by = c("timestamp", "Plot_ID", "german"))

# Extract predictor variable names
preds <- c("conf", colnames(validated_detections)[6:(ncol(validated_detections)-1)])


#########################
##### Type 1 models #####
#########################

# Set control parameters for type 1 conditional inference trees
ctrl1 <- ctree_control(teststat = "max", testtype = "Bonferroni", maxdepth = 1, alpha = 0.05)

# Prepare empty vectors
n <- vector()
n_tp <- vector()
prec <- vector()
spec <- vector()
node <- vector()
ctrees <- data.frame()


# Formulas of type 1 models are simply the predictor variables
models <- preds

# Set the number of cores to use (adjust as needed)
num_cores <- detectCores()

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)


# Foreach for parallel processing
ctrees_type1 <- foreach(m = models, .combine = rbind, .verbose = T) %:%
  foreach(f = species, .combine = rbind) %dopar% {
    
    
    n <- vector()
    n_tp <- vector()
    prec <- vector()
    spec <- vector()
    node <- vector()
    ctrees <- data.frame
    
    mydat <- validated_detections[validated_detections$german == f,]
    mytree <- partykit::ctree(data = mydat, formula = as.formula(paste0("validation ~ ", as.character(m))), control = ctrl1)
    
    rules <- as.data.frame(partykit:::.list.rules.party(mytree))
    rnode <- as.numeric(rownames(rules))
    rownames(rules) <- NULL
    rules <- cbind(rnode, rules)
    colnames(rules) <- c("node", "rules")
    rules$german <- f
    rules$model <- m
    
    for (x in 1:length(mytree[[1]])) {
      y <- mytree[[1]][x]$data
      prec <- c(prec, sum(y$validation[y$validation == 1]) / nrow(y))
      n <- c(n, nrow(y))
      n_tp <- c(n_tp, nrow(y[y$validation == 1, ]))
      node <- c(node, x)
      spec <- c(spec, f)
    }
    
    ctrees <- data.frame(
      german = as.factor(spec),
      prec = as.numeric(prec),
      n = as.numeric(n),
      n_tp = as.numeric(n_tp),
      node = as.numeric(node),
      model = as.character(m)
    )
    
    ctrees <- dplyr::left_join(ctrees, rules, by=c("node", "german", "model"))
    
    
    return(ctrees)
    
  }

# Close the parallel backend
stopCluster(cl)

saveRDS(ctrees_type1, "./data/ctrees_type1.rds")



#########################
##### Type 2 models #####
#########################

# Set control parameters for type 1 conditional inference trees
ctrl2 <- ctree_control(teststat = "max", testtype = "Bonferroni", maxdepth = 2, alpha = 0.05)

# Prepare empty vectors
n <- vector()
n_tp <- vector()
prec <- vector()
spec <- vector()
node <- vector()
ctrees <- data.frame()


# Formulas of type 2 models are all combinations of predictor variables
models <- vector()

for (i in 1:2){
  allcomb <- combn(preds,i)
  for (j in 1:ncol(allcomb)){
    modelX <- paste0(allcomb[,j], collapse = "+")
    models <- c(models, modelX)
  }
}

models <- sort(unique(models))

# Set the number of cores to use (adjust as needed)
num_cores <- detectCores()

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Use foreach for parallel processing
ctrees_type2 <- foreach(m = models, .combine = rbind, .verbose = T) %:%
  foreach(f = species, .combine = rbind, .verbose = T) %dopar% {
    
    
    n <- vector()
    n_tp <- vector()
    prec <- vector()
    spec <- vector()
    node <- vector()
    ctrees <- data.frame
    
    mydat <- validated_detections[validated_detections$german == f,]
    mytree <- partykit::ctree(data = mydat, formula = as.formula(paste0("validation ~ ", as.character(m))), control = ctrl2)
    
    rules <- as.data.frame(partykit:::.list.rules.party(mytree))
    rnode <- as.numeric(rownames(rules))
    rownames(rules) <- NULL
    rules <- cbind(rnode, rules)
    colnames(rules) <- c("node", "rules")
    rules$german <- f
    rules$model <- m
    #rules$rules <- ifelse(rules$rules == "", "no_rule", rules$rules)
    
    for (x in 1:length(mytree[[1]])) {
      y <- mytree[[1]][x]$data
      prec <- c(prec, sum(y$validation[y$validation == 1]) / nrow(y))
      n <- c(n, nrow(y))
      n_tp <- c(n_tp, nrow(y[y$validation == 1, ]))
      node <- c(node, x)
      spec <- c(spec, f)
    }
    
    ctrees <- data.frame(
      german = as.factor(spec),
      prec = as.numeric(prec),
      n = as.numeric(n),
      n_tp = as.numeric(n_tp),
      node = as.numeric(node),
      model = as.character(m)
    )
    
    ctrees <- dplyr::left_join(ctrees, rules, by=c("node", "german", "model"))
    
    return(ctrees)
}

# Close the parallel backend
stopCluster(cl)

saveRDS(ctrees_type2, "./data/ctrees_type2.rds")
