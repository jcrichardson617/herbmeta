#like herbmeta but no discintion between inv and intro

#I^2 tries to provide a measure of percent variation across studies due to differences not related to randomness

# load libraries
library(metafor);library(dplyr);library(ggplot2);library(data.table);library(ape); library(matrixcalc); library(dplyr); library(metafor); library(Matrix); library(MASS); library(data.table)


#all the data
all <- read.csv("data_evo_history_only.csv", header = TRUE)

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
#hist(all$hedges_d, breaks = 10000)
#min(all$hedges_d)
#max(all$hedges_d)
#hist(all$var_hedges_d, breaks = 10000)
#min(all$var_hedges_d)
#max(all$var_hedges_d)

res <- rma(hedges_d, var_hedges_d, data = all)

overall_effect <- rma(hedges_d, var_hedges_d, mods = ~ study_type - 1, data = all)

types <- all %>% 
  group_by(study_type) %>%
  summarise(count = n())
types

overall_effect <- setDT(
  as.data.frame(cbind(overall_effect$b,overall_effect$ci.lb,overall_effect$ci.ub)), keep.rownames = TRUE)[]
#rename rows
overall_effect$rn <- c("No", "Yes")
overall_effect$samplesize <- types$count

#plot it
biocontrolYN <- ggplot(overall_effect, aes(x=rn, y=V1)) + 
  geom_point(position=position_dodge(0.5), size = 3) +
  geom_text(aes(x= rn, y=V3+0.01, label=(types$count))) +
  ylab("Effect size (Hedges' d) ± 95% CI") +
  xlab("Biocontrol") +
  geom_errorbar(aes(ymin=V2, ymax=V3),
                width=0,                    # Width of the error bars
                position=position_dodge(0.5)) +
  scale_shape_manual(values=c(15, 16, 17, 4, 18))+
  scale_y_continuous(expand = c(0, 0), limits = c(-1.1, -0.45))
  
biocontrolYN

biocontrolYN +scale_colour_Publication()+ theme_Publication() + coord_fixed(ratio = 7)

sum(types$count)  

funnel(res, yaxis="vinv", main = "Inverse sampling variance")

#analyses for each metric type across interaction types

#biomass####
all <- all %>%
  filter(plant_organ_afflicted != "flower") %>%
  filter(plant_organ_afflicted != "fruit") %>%
  droplevels()

biomass <- metanalysis("biomass")
biomass[[2]]
biomass[[3]]$organ <- c("Leaf","Root","Seed",
                        "Stem","Whole","Leaf","Root","Seed",
                        "Stem","Whole")
biomass[[3]]$type <- c("A","A","A","A","A",
                       "D","D","D","D","D")
biomass[[3]]$sample_size <- (all %>% 
                               filter(response_measure == "biomass") %>%
                               group_by(study_type, plant_organ_afflicted) %>%
                               summarise(count = n()))$count

biomass_for_plotting <- as.data.frame(biomass[[3]])

biomassnatnonnatUNLUMPED <- metaplot(biomass_for_plotting, -2.5, 0.3)
biomassnatnonnatUNLUMPED <- biomassnatnonnatUNLUMPED +  theme(legend.position = ("none")) +ylab("")
biomassnatnonnatUNLUMPED <- biomassnatnonnatUNLUMPED + scale_colour_Publication()+ theme_Publication() #+ coord_fixed(ratio = 7)
#write.table(biomass_for_plotting, file = "biomass_evo_hist.csv", sep = ",")


#lumped
all <- all %>%
  filter(plant_organ_afflicted != "flower") %>%
  filter(plant_organ_afflicted != "fruit") %>%
  filter(response_measure == "biomass") %>%
  droplevels()

overall_effect <- rma(hedges_d, var_hedges_d, mods = ~ plant_organ_afflicted - 1, data = all)

types <- all %>% 
  group_by(plant_organ_afflicted) %>%
  summarise(count = n())
types

overall_effect <- setDT(
  as.data.frame(cbind(overall_effect$b,overall_effect$ci.lb,overall_effect$ci.ub)), keep.rownames = TRUE)[]
#rename rows
overall_effect$rn <- c("Leaf", "Root", "Seed", "Stem", "Whole")
overall_effect$samplesize <- types$count

#plot it
biomassnatnonnatLUMPED <- ggplot(overall_effect, aes(x=rn, y=V1)) + 
  geom_point(position=position_dodge(0.5), size = 3) +
  geom_text(aes(x= rn, y=V3+0.05, label=(types$count))) +
  ylab("Effect size (Hedges' d) ± 95% CI") +
  xlab("Biocontrol") +
  geom_errorbar(aes(ymin=V2, ymax=V3),
                width=0,                    # Width of the error bars
                position=position_dodge(0.5)) +
  scale_shape_manual(values=c(15, 16, 17, 4, 18))+
  scale_y_continuous(expand = c(0, 0), limits = c(-1.9, -0.25))

biomassnatnonnatLUMPED <- biomassnatnonnatLUMPED +scale_colour_Publication()+ theme_Publication()

sum(types$count)  

biomassnatnonnatUNLUMPED <- biomassnatnonnatUNLUMPED + theme(axis.text.x = element_blank()) + labs(tag = "A")
biomassnatnonnatLUMPED <- biomassnatnonnatLUMPED + theme(axis.title.y=element_text(angle=90, hjust=-2.5)) + labs(tage = "B")

grid.arrange(biomassnatnonnatUNLUMPED, biomassnatnonnatLUMPED, nrow = 2)

#count----
levels(all$plant_organ_afflicted)

all <- all %>%
  filter(plant_organ_afflicted != "fruit") %>%
  filter(plant_organ_afflicted != "roots") %>%
  filter(plant_organ_afflicted != "whole organism") %>%
  droplevels()

count <- metanalysis("number of")
count[[3]]$organ <- c("Flower","Leaf","Seed","Stem",
                      "Flower","Leaf","Seed","Stem")
count[[3]]$type <- c("A","A","A","A",
                     "D","D","D","D")
count[[3]]$sample_size <- (all %>% 
                             filter(response_measure == "number of") %>%
                             group_by(study_type, plant_organ_afflicted) %>%
                             summarise(count = n()))$count
count_for_plotting <- as.data.frame(count[[3]])

countsnatnonnatUNLUMPED <- metaplot(count_for_plotting, -2.0, 1.0) + theme(legend.position = ("none"))
countsnatnonnatUNLUMPED <- countsnatnonnatUNLUMPED +scale_colour_Publication()+ theme_Publication()

countsnatnonnatUNLUMPED
write.table(count_for_plotting, file = "count_evo_histUNLUMPED.csv", sep = ",")

countsnatnonnatUNLUMPED <- countsnatnonnatUNLUMPED + 
  theme(
  axis.text.x = element_blank(),
  axis.title.y = element_blank()) + labs(tag = "A")

#lumped
# allLUMPED <- all %>%
#   filter(response_measure == "number of") %>%
#   droplevels()
#   
# overall_effect <- rma(hedges_d, var_hedges_d, mods = ~ plant_organ_afflicted - 1, data = allLUMPED)
# 
# types <- allLUMPED %>% 
#   group_by(plant_organ_afflicted) %>%
#   summarise(count = n())
# types
# 
# overall_effect <- setDT(
#   as.data.frame(cbind(overall_effect$b,overall_effect$ci.lb,overall_effect$ci.ub)), keep.rownames = TRUE)[]
# #rename rows
# overall_effect$rn <- c("Flower", "Leaf", "Seed", "Stem")
# overall_effect$samplesize <- types$count
# 
# write.table(overall_effect, file = "count_evo_histLUMPED.csv", sep = ",")
# 
# #plot it
# countsnatnonnatLUMPED <- ggplot(overall_effect, aes(x=rn, y=V1)) + 
#   geom_point(position=position_dodge(0.5), size = 3) +
#   geom_text(aes(x= rn, y=V3+0.05, label=(types$count))) +
#   ylab("Effect size (Hedges' d) ± 95% CI") +
#   xlab("Biocontrol") +
#   geom_errorbar(aes(ymin=V2, ymax=V3),
#                 width=0,                    # Width of the error bars
#                 position=position_dodge(0.5)) +
#   scale_shape_manual(values=c(15, 16, 17, 4, 18))+
#   scale_y_continuous(expand = c(0, 0), limits = c(-1.5, .5))
# 
# countsnatnonnatLUMPED <- countsnatnonnatLUMPED +scale_colour_Publication()+ theme_Publication()
# 
# countsnatnonnatLUMPED
# 
# countsnatnonnatLUMPED <- countsnatnonnatLUMPED + theme(axis.title.y=element_text(angle=90, hjust=-2.5)) + labs(tage = "B")

countsnatnonnatUNLUMPED <- countsnatnonnatUNLUMPED + coord_fixed(ratio = 0.75) + 
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_blank())

plots <- list(countsnatnonnatUNLUMPED, countsnatnonnatLUMPED)
grobs <- list()
widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)

for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}

do.call("grid.arrange", c(grobs, ncol = 1))

grid.arrange(countsnatnonnatUNLUMPED, countsnatnonnatLUMPED, ncol = 1)

#insects----
insects <- filter(all, !(insect_order == "lepidoptera" & study_type == "b"))
insects <- insects %>%
  filter(insect_order != "diptera") %>%
  filter(insect_order != "hymenoptera") %>%
  droplevels()

levels(insects$insect_order)
levels(insects$study_type)

interaction_effect <-  rma(hedges_d, var_hedges_d, mods = ~ insect_order:study_type - 1,  data = insects)
comparisons_interaction_spread <- setDT(
  as.data.frame(cbind(interaction_effect$b,interaction_effect$ci.lb,interaction_effect$ci.ub)), keep.rownames = TRUE)[]
comparisons_interaction_spread$order <- c("Coleoptera",
                                          "Hemiptera", 
                                          "Lepidoptera","Orthoptera",
                                          "Coleoptera",
                                          "Hemiptera","Lepidoptera",
                                          "Orthoptera") 

comparisons_interaction_spread$type <- c("A","A","A","A",
                                         "D","D","D","D")

comparisons_interaction_spread$count <- (insects %>% 
                                           group_by(study_type, insect_order) %>%
                                           summarise(count = n()))$count

#write.table(comparisons_interaction_spread, file = "bugsUNLUMPED.csv", sep = ",")


bugs <- ggplot(comparisons_interaction_spread, aes(x=order, y=V1, shape=type)) + 
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
  scale_y_continuous(expand = c(0, 0), limits = c(-3.1, .3)) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.text.x=element_text(angle=0, hjust=.5),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black", size = 15),
        axis.text = element_text(color = "black", size = 12),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black"),
        legend.position = ("none"),
        axis.ticks.x = element_blank())
bugs
ggsave(filename = "bugs.png")

#lumped
lumped <-  rma(hedges_d, var_hedges_d, mods = ~ insect_order - 1,  data = insects)
comparisons_lumped_spread <- setDT(
  as.data.frame(cbind(lumped$b,lumped$ci.lb,lumped$ci.ub)), keep.rownames = TRUE)[]
comparisons_lumped_spread$order <- c("Coleoptera",
                                          "Hemiptera", 
                                          "Lepidoptera","Orthoptera") 

comparisons_lumped_spread$count <- (insects %>% 
                                           group_by(insect_order) %>%
                                           summarise(count = n()))$count

write.table(comparisons_lumped_spread, file = "bugsLUMPED.csv", sep = ",")


bugs <- ggplot(comparisons_lumped_spread, aes(x=order, y=V1)) + 
  geom_point(position=position_dodge(0.5)) +
  geom_text(aes(x= order, y=V3, 
                label=(insects %>% 
                         group_by(insect_order) %>%
                         summarise(count = n()))$count), hjust = 0.5, vjust = -.5, position=position_dodge(0.5)) +
  ylab("Effect size (Hedges' d) ± 95% CI") +
  xlab("") +
  geom_errorbar(aes(ymin=V2, ymax=V3),
                width=0,                    # Width of the error bars
                position=position_dodge(0.5)) +
  scale_shape_manual(values=c(15, 16, 17, 4, 18))+
  geom_hline(yintercept= (0), linetype="dotted") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(-1.7, -0.2)) +
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
bugs

bugsLUMPED <- bugs +scale_colour_Publication()+ theme_Publication()

bugsLUMPED <- bugsLUMPED + labs(tag = "B") + theme(axis.title.y=element_text(angle=90, hjust=-2.8))

bugsUNLUMPED
bugsLUMPED

plots <- list(bugsUNLUMPED, bugsLUMPED)
grobs <- list()
widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)

for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}

do.call("grid.arrange", c(grobs, ncol = 1))
