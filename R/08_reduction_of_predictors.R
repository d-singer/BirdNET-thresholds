# libraries required for the session

library(data.table)#
library(dplyr)
library(ggplot2)
library(tidyr)

### load species list to translate variables
spec_names <- fread("./data/species_names_translate.csv", encoding="UTF-8")


models <- readRDS("./data/all_models.rds")

##### removal of time windows

timevars <- c("none", "int12", "int11", "int10", "int09", "int08", "int07", 
              "int06", "int05", "int04","int03", "int02", "int01")

mymodels <- models

myspec <- data.frame(german= unique(mymodels$german))

opti_spec <- data.frame()

for (v in timevars)
{
  mymodels$remove <- grepl(pattern=v, mymodels$model)
  mymodels <- mymodels[mymodels$remove == F, ]
  
  opti_mods <- mymodels %>% group_by(german) %>%
    mutate(perf_rank = dense_rank(-model_performance))%>% 
    filter(perf_rank == 1) %>%
    summarise(prec = unique(prec), 
              recall = unique(recall),
              model_performance = unique(model_performance),
              remove = v)

opti_spec <- rbind(opti_spec, opti_mods)
}

nspec_highprec <- opti_spec %>% group_by(remove) %>% filter(prec >= 0.9) %>% 
  summarize(nspec_highprec = uniqueN(german)/61)

perf_all <- opti_spec %>% group_by(remove) %>% 
  summarize(mean_perf = mean(model_performance)) %>% 
  left_join(nspec_highprec, by="remove") %>% mutate(spec = "all species [n=61]") 


perf <- perf_all %>% pivot_longer(cols=c(2:3))


perf$label <- gsub(perf$remove, pattern="int01", replacement="conf. score")
perf$label <- gsub(perf$label, pattern="int02", replacement="±3 s")
perf$label <- gsub(perf$label, pattern="int03", replacement="±6 s")
perf$label <- gsub(perf$label, pattern="int04", replacement="±9 s")
perf$label <- gsub(perf$label, pattern="int05", replacement="±12 s")
perf$label <- gsub(perf$label, pattern="int06", replacement="±10 min")
perf$label <- gsub(perf$label, pattern="int07", replacement="±20 min")
perf$label <- gsub(perf$label, pattern="int08", replacement="±30 min")
perf$label <- gsub(perf$label, pattern="int09", replacement="±40 min")
perf$label <- gsub(perf$label, pattern="int10", replacement="±12 h")
perf$label <- gsub(perf$label, pattern="int11", replacement="±24 h")
perf$label <- gsub(perf$label, pattern="int12", replacement="±36 h")
perf$label <- gsub(perf$label, pattern="none", replacement="±48 h")
perf$variable <- gsub(perf$name, pattern="mean_perf", replacement="Average model performance")
perf$variable <- gsub(perf$variable, pattern="mean_prec", replacement="Mean precision")
perf$variable <- gsub(perf$variable, pattern="mean_recall", replacement="Mean recall")
perf$variable <- gsub(perf$variable, pattern="nspec_highprec", replacement="Percentage of species with high precision")

perf$variable <- factor(perf$variable, levels=c("Average model performance", "Mean precision", 
                                        "Mean recall", "Percentage of species with high precision"))
perf$label <- factor(perf$label, levels=rev(c("conf. score", "±3 s", "±6 s", "±9 s", "±12 s",
                                            "±10 min", "±20 min", "±30 min", "±40 min", 
                                            "±12 h", "±24 h", "±36 h", "±48 h")))

perf <- perf %>% arrange(spec, label)


time_plot <- ggplot(perf, aes(x=label, y=value, fill=spec, group=spec)) + 
  geom_step(color="#7F7F7F")+
  geom_point(shape=21, size=2.5)  + 
  facet_wrap(~variable, scales="free_y", nrow=2) +
  theme_bw()+ 
  #ylim(0.4, 1) + 
  xlab("Maximum time interval of ATF") + ylab("Value") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values=c("#2CA02C", "#7F7F7F")) +
  theme(legend.position = "") +
  guides(fill=guide_legend(title=""))+
  geom_vline(xintercept=12.5, lty=2, color="#7F7F7F")

time_plot   


######## removal of statistical parameters

### load variables order from bootstrapping
opti_boot <- readRDS("./data/bootstrapping_atf.rds")

atf_order <- opti_boot %>% group_by(german, remove) %>%
  filter(rank <=2)%>%
  summarise(
    mean_prec = mean(prec),
    mean_recall = mean(recall),
    mean_modperf = mean(model_performance),
    
  ) %>% group_by(german) %>% 
  mutate(rank = dense_rank(mean_modperf)) %>%
  group_by(remove) %>% 
  summarise(mean_rank = mean(rank)) %>% arrange(-mean_rank, rev(remove))

atf <- atf_order$remove

atf

mymodels <- models
opti_spec <- data.frame()

for (v in atf)
{
  mymodels$remove <- grepl(pattern=v, mymodels$model)
  mymodels <- mymodels[mymodels$remove == F, ]
  
  opti_mods <- mymodels %>% group_by(german) %>%
    mutate(perf_rank = dense_rank(-model_performance))%>% 
    filter(perf_rank == 1) %>%
    summarise(prec = unique(prec), 
              recall = unique(recall),
              model_performance = unique(model_performance),
              remove = v) 
  opti_spec <- rbind(opti_spec, opti_mods) %>% ungroup()
}

boot_all <- opti_spec %>% group_by(remove) %>% 
  summarise(mean_prec = mean(prec),
            mean_recall = mean(recall),
            mean_modperf = mean(model_performance))

boot_spec_all <- opti_spec %>% group_by(remove) %>% 
  filter(prec >= 0.9)%>% 
  summarise(nspec_highprec = n_distinct(german)/61)

boot_all <- boot_all %>% left_join(boot_spec_all, by=c("remove")) %>% arrange(mean_modperf) %>% mutate(spec = "all species [n=61]")

boot <- boot_all %>% pivot_longer(cols=c(2:5))
boot <- boot %>% filter(name %in% c("mean_modperf", "nspec_highprec"))

boot$label <- gsub(boot$remove, pattern=rev(atf)[1], replacement="conf. score")
boot$label <- gsub(boot$label, pattern=rev(atf)[2], replacement=rev(atf)[1])
boot$label <- gsub(boot$label, pattern=rev(atf)[3], replacement=rev(atf)[2])
boot$label <- gsub(boot$label, pattern=rev(atf)[4], replacement=rev(atf)[3])
boot$label <- gsub(boot$label, pattern=rev(atf)[5], replacement=rev(atf)[4])
boot$label <- gsub(boot$label, pattern=rev(atf)[6], replacement=rev(atf)[5])
boot$label <- gsub(boot$label, pattern=rev(atf)[7], replacement=rev(atf)[6])
boot$label <- gsub(boot$label, pattern=rev(atf)[8], replacement=rev(atf)[7])
boot$label <- gsub(boot$label, pattern=rev(atf)[9], replacement=rev(atf)[8])
boot$label <- gsub(boot$label, pattern=rev(atf)[10], replacement=rev(atf)[9])
boot$label <- gsub(boot$label, pattern=rev(atf)[11], replacement=rev(atf)[10])
boot$label <- gsub(boot$label, pattern=rev(atf)[12], replacement=rev(atf)[11])
boot$label <- gsub(boot$label, pattern=rev(atf)[13], replacement=rev(atf)[12])
boot$label <- gsub(boot$label, pattern=rev(atf)[14], replacement=rev(atf)[13])
boot$label <- gsub(boot$label, pattern=rev(atf)[15], replacement=rev(atf)[14])
boot$variable <- gsub(boot$name, pattern="mean_modperf", replacement="Average model performance")
boot$variable <- gsub(boot$variable, pattern="mean_prec", replacement="Mean precision")
boot$variable <- gsub(boot$variable, pattern="mean_recall", replacement="Mean recall")
boot$variable <- gsub(boot$variable, pattern="nspec_highprec", replacement="Percentage of species with high precision")

boot$variable <- factor(boot$variable, levels=c("Average model performance", "Mean precision", 
                                        "Mean recall", "Percentage of species with high precision"))

### order from bootstrapping
boot$label <- factor(boot$label, levels=c(atf[2:15], "conf. score"))

boot <- boot %>% arrange(spec, label)

atf_plot <- ggplot(boot[boot$name %in% c("mean_modperf", "nspec_highprec"),], 
                    aes(x=label, y=value, fill=spec, group=spec)) + 
  geom_step(color="#7F7F7F")+
  geom_point(shape=21, size=2.5)  + 
  facet_wrap(~variable, scales="free_y", nrow=2) +
  theme_bw()+ 
  xlab("Included statistical parameters of ATF") + ylab("Value") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values=c("#2CA02C", "#7F7F7F")) +
  theme(legend.position = "") +
  guides(fill=guide_legend(title=""))+
  geom_vline(xintercept=14.5, lty=2, color="#7F7F7F") 

atf_plot 


# png("figures/reduction_predictors.png", width=1400*4, height=900*4, res=600)
# ggpubr::ggarrange(time_plot, atf_plot, nrow = 1, ncol = 2, align = "hv", 
#                   labels = c("A", "B"), legend="none", common.legend=T)
# dev.off()


