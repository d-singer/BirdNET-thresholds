# Load necessary libraries
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsignif)


### Load species list to translate names
spec_names <- fread("./data/species_names_translate.csv", encoding = "UTF-8")
spec_names$scientific2 <- tolower(gsub(spec_names$scientific, pattern = " ", replacement = "_"))

### Load manually verified sample data
validated_detections <- fread("./data/validated_detections.csv", encoding = "UTF-8")

validated_detections <- validated_detections %>%
  select(german, conf, validation) %>%
  group_by(german) %>%
  mutate(tp_total = sum(validation))

validated_detections <- validated_detections %>% 
  filter(german %in% sort(unique(validated_detections$german[validated_detections$validation == 1])))


# Load models 
cand_mods <- readRDS("./data/candidate_models.rds")
base_mods <- readRDS("./data/basic_models.rds")
logi_para <- readRDS("./data/logistic_models.rds")

# Calculate performance of universal threshold UNI10 (confidence >= 0.1)
uni10 <- validated_detections %>% 
  filter(conf >= 0.1) %>%
  group_by(german) %>% 
  summarize(
    prec = mean(validation),
    threshold = "uni10",
    recall = sum(validation) / tp_total,
    model_performance = prec*0.75+recall*0.25) %>% 
  unique() %>% 
  merge(validated_detections %>% select(german) %>% unique(), by="german", all.y=T)

# Calculate performance of universal threshold UNI50 (confidence >= 0.5)
uni50 <- validated_detections %>% 
  filter(conf >= 0.5) %>%
  group_by(german) %>% 
  summarize(
    prec = mean(validation),
    recall = sum(validation) / tp_total,
    ) %>% unique() %>% 
  merge(validated_detections %>% select(german) %>% unique() , by="german", all.y=T) %>%
  mutate(threshold = "uni50") 
uni50$prec <- ifelse(is.na(uni50$prec), 0,uni50$prec)
uni50$recall <- ifelse(is.na(uni50$recall), 1,uni50$recall)
uni50$model_performance <- uni50$prec*0.75 + uni50$recall*0.25

# Calculate performance of universal threshold UNI90 (confidence >= 0.9)
uni90 <- validated_detections %>% 
  filter(conf >= 0.9) %>%
  group_by(german) %>% 
  summarize(
    prec = mean(validation),    
    recall = sum(validation) / tp_total,
  ) %>% unique() %>% 
  merge(validated_detections %>% select(german) %>% unique() , by="german", all.y=T) %>%
  mutate(threshold = "uni90") 
uni90$prec <- ifelse(is.na(uni90$prec), 0,uni90$prec)
uni90$recall <- ifelse(is.na(uni90$recall), 1,uni90$recall)
uni90$model_performance <- uni90$prec*0.75 + uni90$recall*0.25



## replace NA (no thresholds found by logistic regression) with values from uni10 (equals no filtering)
logi_para <- merge(uni10[,c("german", "prec")] %>% rename(precUNI10 = prec), logi_para, by="german", all.x=T) %>%
  mutate(threshold = "logi")
logi_para$prec[is.na(logi_para$prec)] <- logi_para$precUNI10[is.na(logi_para$prec)]
logi_para$recall[is.na(logi_para$recall)] <- 1
logi_para$model_performance <- logi_para$prec*0.75 + logi_para$recall*0.25
logi_para <- logi_para %>% select(-precUNI10) 


base_para <- base_mods %>% 
  mutate(threshold="base") %>% 
  select(german, prec, recall, threshold, model_performance) 

opti_para <- cand_mods %>% 
  group_by(german) %>% 
  summarize(prec = unique(prec),
            recall = unique(recall),
            threshold="opti",
            model_performance = prec*0.75+recall*0.25) %>% 
  select(german, prec, recall, threshold, model_performance)

mod_comp <- rbind(base_para, opti_para, logi_para, uni10, uni50, uni90)

mod_comp <- mod_comp %>% mutate(threshold = factor(threshold, levels=c("base", "opti", "logi", "uni10", "uni50", "uni90")))

mod_comp <- left_join(mod_comp, spec_names[,c("german", "english", "scientific")], by="german")

# define order of species  (here order according to precision of basic thresholds)
specorder <- mod_comp %>% filter(threshold == "base") %>% 
  arrange(prec) %>% select(english, german)

mod_comp$english <- factor(mod_comp$english, levels=specorder$english)
mod_comp$threshold <- factor(mod_comp$threshold, levels=c("opti", "base", "logi", "uni10", "uni50", "uni90"))

thres_names <- data.frame(threshold = c("opti", "base", "logi", "uni10", "uni50", "uni90"), 
                          type = c("CIT Optimised", "CIT Basic", "Logistic reg.", "UNI10", "UNI50", "UNI90"))

mod_comp<- left_join(mod_comp, thres_names, by="threshold") %>% arrange(threshold)

# set order of thresholds (for graph legend)
mod_comp$Threshold <- factor(mod_comp$type, levels=c("UNI10",
                                                     "UNI50",
                                                     "UNI90",
                                                     "Logistic reg.",
                                                     "CIT Basic",
                                                     "CIT Optimised"))




prec_plot <- ggplot(mod_comp, aes(x=prec, y=english, fill=Threshold, shape=Threshold, size=Threshold)) + 
  geom_point(alpha=0.7)+
  geom_vline(xintercept=0.9, lty=2) + 
  ylab("Species")+
  xlab("Precision")+
  theme_bw()+ 
  scale_shape_manual(values=c(3, 5, 4, 22, 21,24))+
  scale_size_manual(values=c(2, 2.5, 2, 2.5,4,3))+
  #scale_fill_manual(values=c("white", "white", "white", "gray55",  "black"))+
  scale_fill_manual(values=c("white", "white", "white","#D55E00","#1F77B4", "#2CA02C"))+
  scale_x_continuous(breaks=seq(0,1,0.1))+
  annotate(geom = "text", x = 0.917, y = 6.5, 
           label = "High precision", color = "black",
           angle = 90, size=3.5)+
  theme(legend.position='bottom') +
  guides(shape=guide_legend(ncol=6))


plot1 <- ggplot(mod_comp, aes(x=prec, y=Threshold, fill=Threshold)
  #                                shape=Threshold, size=Threshold)
  ) + 
  #geom_jitter(width=0, height=.25, alpha=0.45)+
  geom_boxplot(width=0.5, alpha=0.7)+
  geom_vline(xintercept=0.9, lty=2) + 
  ylab("Threshold")+
  xlab("")+
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #scale_shape_manual(values=c(3, 5, 4, 21, 24))+
  #scale_size_manual(values=c(1, 1, 1, 3,2))+
  scale_fill_manual(values=c("white", "white", "white","#D55E00", "#1F77B4", "#2CA02C"))+
  scale_x_continuous(element_blank())+
  theme(legend.position="none") +
  guides(shape=guide_legend(nrow=1)) +
  # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size=1) +
  # stat_summary(fun.data = "mean_se", geom = "point", size = 5)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

plot1

# Turn on if graphic should be written to png
# png(filename = "./figures/performance_plot_20231229.png", width=4800, height=6800, res = 600)
# ggpubr::ggarrange(plot1, prec_plot,  labels = c("A", "B"),
#           ncol = 1, nrow = 2, align="v", heights=c(0.12, 0.88))
# dev.off()

mod_comp$threshold <- factor(mod_comp$threshold, levels = c("uni10",  "uni50", "uni90", "logi", "base", "opti"))
mod_comp <- mod_comp %>% arrange(threshold) %>% mutate(nspec_highprec = ifelse(prec >= 0.9, 1,0)) 
mod_comp <- mod_comp %>% arrange(threshold, german)

fwrite(mod_comp, "./data/model_comparison.csv")

thresholds <- c("logi", "base", "uni10", "uni50", "uni90")
vars <- c("prec", "recall", "model_performance", "nspec_highprec")

comp <- vector()
p_value <- vector()
p_adjust <- vector()
signif <- vector()
var <- vector()
teststat <- vector()

wilcox_comp <- mod_comp %>% pivot_wider(id_cols =c("german"), names_from="threshold", values_from=prec)

for (v in vars)
{
  mydat <- mod_comp %>% pivot_wider(id_cols =c("german"), names_from="threshold", values_from=v)
  
  for(t in thresholds)
{
    x = mydat %>% select(opti) %>% unlist()
    y = mydat %>% select(all_of(t)) %>% unlist()

    testres <- wilcox.test(x =  x,  
                         y = y, alternative="two.sided", paired=T)

p_value <- c(p_value, testres$p.value)
comp <- c(comp, t)
var <- c(var, v)
myadj <- p.adjust(testres$p.value, method="bonferroni", n=uniqueN(thresholds))
p_adjust <- c(p_adjust, myadj )
signif <- c(signif, ifelse(myadj < 0.001, "***", 
                           ifelse(myadj <0.01, "**",
                           ifelse(myadj < 0.05, "*", "n.s."))))
}

}

res <- data.frame(comp= comp, 
                  vars = vars,
                  p_value = p_value, 
                  p_adjust = p_adjust,
                  signif = signif)

fwrite(res, "./data/wilcox_test_model_performance.csv")
