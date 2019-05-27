#I^2 tries to provide a measure of percent variation across studies due to differences not related to randomness

# load libraries
library(metafor); library(dplyr);library(ggplot2);library(data.table);library(ape); library(matrixcalc); library(dplyr); library(metafor); library(Matrix); library(MASS); library(data.table)

#all the data
all <- read.csv("data.csv", header = TRUE)

#remove the zeroes from leaf area until we know what to do with those
all <- all %>% 
  filter(var_hedges_d != "0")

table(all$response_measure)

all <- all %>%
  filter(response_measure != "area consumed")  

table(all$response_measure)

all <- all %>%
  filter(response_measure != "area")  

table(all$response_measure)

#histograms of effect sizes####
hist(all$hedges_d, breaks = 10000)
min(all$hedges_d)
max(all$hedges_d)
hist(all$var_hedges_d, breaks = 10000)
min(all$var_hedges_d)
max(all$var_hedges_d)

res <- rma(hedges_d, var_hedges_d, data = all)
overall_effect <- rma(hedges_d, var_hedges_d, mods = ~ study_type -1, data = all)
#Get rid of "-1" and pull Qb and p values from that

types <- all %>% 
  group_by(study_type) %>%
  summarise(count = n())
types

overall_effect <- setDT(
  as.data.frame(cbind(overall_effect$b,overall_effect$ci.lb,overall_effect$ci.ub)), keep.rownames = TRUE)[]
#rename rows
overall_effect$rn <- c("Natural Studies", 
                       "Biocontrol Investigations",
                       "Neoclassical - Invasive Plant",
                       "Neoclassical - Invasive Insect",
                       "Biocontrol Release")
overall_effect$samplesize <- types$count

#plot it
Studytype <- ggplot(overall_effect, aes(x=rn, y=V1)) + 
  geom_point(position=position_dodge(0.05), size = 3) +
  geom_text(aes(x= rn, y=V3+0.05, label=(types$count))) +
  ylab("Effect size (Hedges' d) ± 95% CI") +
  geom_errorbar(aes(ymin=V2, ymax=V3),
                width=0,                    # Width of the error bars
                position=position_dodge(0.5)) +
  #scale_shape_manual(values=c(15, 16, 17, 4, 18))+
  scale_y_continuous(expand = c(0, 0), limits = c(-1.5, .1))
Studytype  

Studytype +scale_colour_Publication()+ theme_Publication() +
  theme(plot.margin = margin(10, 10, 10, 50),
        axis.text.x = element_text(angle = 45, hjust = 1))
  

overall_effect











#functions for plot----
metaplot <- function(values, lowvalue, hivalue) {
  ggplot(values, aes(x=organ, y=V1, shape=type)) + 
    geom_point(position=position_dodge(0.5), size = 3) +
    geom_text(aes(x= organ, y=V3, 
                  label=sample_size), hjust = 0.5, vjust = -.3, position=position_dodge(0.5)) +
    ylab("Effect size (Hedges' d) ± 95% CI") +
    xlab("") +
    geom_errorbar(aes(ymin=V2, ymax=V3),
                  width=0,                    # Width of the error bars
                  position=position_dodge(0.5)) +
    scale_shape_manual(values=c(15, 16, 17, 4, 18))+
    geom_hline(yintercept= (0), linetype="dotted") +
    theme_bw() +
    scale_y_continuous(expand = c(0, 0), limits = c(lowvalue, hivalue)) +
    theme(axis.line = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "transparent",color = NA),
          axis.text.x=element_text(angle=0, hjust=.5),
          axis.title.x = element_text(color = "black"),
          axis.title.y = element_text(color = "black"),
          axis.text = element_text(color = "black", size = 12),
          axis.ticks = element_line(color = "black"),
          plot.title = element_text(color = "black"),
          axis.ticks.x=element_blank(),
          legend.position = ("none"))
}

metanalysis <- function(whichmetric) {
  metric <- all %>% 
    filter(response_measure == whichmetric) 
  
  overall_effect <- rma(hedges_d, var_hedges_d, data = metric)
  interaction_effect <- 
    rma(hedges_d, var_hedges_d, mods = ~ plant_organ_afflicted:study_type - 1,  data = metric)
  comparisons_interaction_spread <- setDT(
    as.data.frame(cbind(interaction_effect$b,interaction_effect$ci.lb,interaction_effect$ci.ub)), keep.rownames = TRUE)[]
  
  return(list(overall_effect, interaction_effect, comparisons_interaction_spread))
}


sum(types$count)  

funnel(res, yaxis="vinv", main = "Inverse sampling variance")

#analyses for each metric type across interaction types

#biomass####
biomass <- metanalysis("biomass")
biomass[[2]]
biomass[[3]]$organ <- c("Flower","Fruit","Leaf","Root","Seed",
                        "Stem","Whole","Leaf","Stem","Whole",
                        "Leaf","Root","Stem","Whole","Leaf",
                        "Root","Seed","Stem","Whole","Root",
                        "Seed","Stem","Whole")
biomass[[3]]$type <- c("A","A","A","A","A","A","A",
                       "B","B","B",
                       "C","C","C","C",
                       "D","D","D","D","D",
                       "E","E","E","E")
biomass[[3]]$sample_size <- (all %>% 
  filter(response_measure == "biomass") %>%
  group_by(study_type, plant_organ_afflicted) %>%
  summarise(count = n()))$count

biomass_for_plotting <- as.data.frame(biomass[[3]])

metaplot(biomass_for_plotting, -4, 2)
write.table(biomass_for_plotting, file = "biomass.csv", sep = ",")

#count----
count <- metanalysis("number of")
count[[3]]$organ <- c("Flower","Fruit","Leaf","Seed","Stem",
                      "Fruit","Flower","Leaf","Seed","Stem",
                      "Flower","Seed","Stem")
count[[3]]$type <- c("A","A","A","A","A",
                     "C",
                     "D","D","D","D",
                     "E","E","E")
count[[3]]$sample_size <- (all %>% 
                               filter(response_measure == "number of") %>%
                               group_by(study_type, plant_organ_afflicted) %>%
                               summarise(count = n()))$count
count_for_plotting <- as.data.frame(count[[3]])
count_for_plotting <- rbind(count_for_plotting, c("plant_organ_afflictedstem:study_typeb", 20, 20, 20, "Stem", "B", 1))
count_for_plotting <- as.data.frame(count_for_plotting)
count_for_plotting$V1 <- as.numeric(count_for_plotting$V1)
count_for_plotting$V2 <- as.numeric(count_for_plotting$V2)
count_for_plotting$V3 <- as.numeric(count_for_plotting$V3)

metaplot(count_for_plotting, -4, 2)
write.table(count_for_plotting, file = "count.csv", sep = ",")

#length----
length <- metanalysis("length")
length[[3]]$organ <- c("Stem","Whole","Root","Stem","Whole",
                       "Stem")
length[[3]]$type <- c("A","A",
                      "D","D","D",
                      "E")
length[[3]]$sample_size <- (all %>% 
                             filter(response_measure == "length") %>%
                             group_by(study_type, plant_organ_afflicted) %>%
                             summarise(count = n()))$count
length_for_plotting <- as.data.frame(length[[3]])
length_for_plotting <- rbind(length_for_plotting, 
                             c("typeb", 20, 20, 20, "Stem", "B", 1),
                             c("typec", 20, 20, 20, "Stem", "C", 1))
length_for_plotting <- as.data.frame(length_for_plotting)
length_for_plotting$V1 <- as.numeric(length_for_plotting$V1)
length_for_plotting$V2 <- as.numeric(length_for_plotting$V2)
length_for_plotting$V3 <- as.numeric(length_for_plotting$V3)

metaplot(length_for_plotting, -5, 2)
write.table(length_for_plotting, file = "length.csv", sep = ",")


#insect orders
insects <- filter(all, !(insect_order == "lepidoptera" & study_type == "b"))
filter(all, (insect_order == "lepidoptera" & study_type == "b"))

insects

interaction_effect <-  rma(hedges_d, var_hedges_d, mods = ~ insect_order:study_type - 1,  data = insects)
comparisons_interaction_spread <- setDT(
  as.data.frame(cbind(interaction_effect$b,interaction_effect$ci.lb,interaction_effect$ci.ub)), keep.rownames = TRUE)[]
comparisons_interaction_spread$order <- c("Coleoptera","Diptera",
                                          "Hemiptera", "Hymenoptera",
                                          "Lepidoptera","Orthoptera",
                                          "Hemiptera","Orthoptera",
                                          "Coleoptera","Hemiptera",
                                          "Lepidoptera","Orthoptera",
                                          "Coleoptera","Hemiptera",
                                          "Lepidoptera","Orthoptera",
                                          "Coleoptera","Diptera","Lepidoptera") 

comparisons_interaction_spread$type <- c("A","A","A","A","A","A",
                                         "B","B",
                                         "C","C","C","C",
                                         "D","D","D","D",
                                         "E","E","E")
comparisons_interaction_spread$count <- (insects %>% 
  group_by(study_type, insect_order) %>%
  summarise(count = n()))$count

ggplot(comparisons_interaction_spread, aes(x=order, y=V1, shape=type)) + 
  geom_point(position=position_dodge(0.5)) +
  geom_text(aes(x= order, y=V3, 
                label=(insects %>% 
                         group_by(study_type, insect_order) %>%
                         summarise(count = n()))$count), hjust = 0.5, vjust = -.5, position=position_dodge(0.5)) +
  ylab("Effect size (Hedges' d) ± 95% CI") +
  xlab("") +
  geom_errorbar(aes(ymin=V2, ymax=V3),
                width=0,                    # Width of the error bars
                position=position_dodge(0.5)) +
  scale_shape_manual(values=c(15, 16, 17, 4, 18))+
  geom_hline(yintercept= (0), linetype="dotted") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(-6, 3)) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.text.x=element_text(angle=0, hjust=.5),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        axis.text = element_text(color = "black", size = 12),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black"),
        legend.position = ("none"),
        axis.ticks.x = element_blank())
write.table(comparisons_interaction_spread, file = "insectorders.csv", sep = ",")

