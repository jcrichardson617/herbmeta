#all from herbmeta

#The Enemy Release Hypothesis
#There was no significant difference between the effects of herbivory in traditional biocontrol releases (non-native insects with non-native plants in invasive range) versus biocontrol investigations (native insect with native plant in native range) (JASON: we will need a Q value of this pairwise comparison here).

all <- all %>% 
  filter(study_type == "d" | study_type == 'e')

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
overall_effect$rn <- c("Nonnative Insect with Nonnative Plant*",
                       "Native Insect with Native Plant**")
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
  theme(plot.margin = margin(10, 10, 10, 50))

overall_effect


#There was no significant difference in the strength of herbivory between either native insects with non-native plants versus native insects with native plant

a_and_b <- all %>% 
  filter(study_type == "a" | study_type == "b")

overall_effect <- rma(hedges_d, var_hedges_d, mods = ~ study_type, data = a_and_b)
#Get rid of "-1" and pull Qb and p values from that

overall_effect <- setDT(
  as.data.frame(cbind(overall_effect$b,overall_effect$ci.lb,overall_effect$ci.ub)), keep.rownames = TRUE)[]
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

#between non-native insects with native plants versus native insects with native plants 

#oops did this above with a and b, use b and c for what is there

#There was a significant difference in the strength of effects of single natural enemies between native insects with native plants versus native insects and native plants as part of a biocontrol investigation in the native range

a_and_e <- all %>% 
  filter(study_type == "a" | study_type == 'e')

res <- rma(hedges_d, var_hedges_d, data = a_and_e)
overall_effect <- rma(hedges_d, var_hedges_d, mods = ~ study_type, data = a_and_e)
#Get rid of "-1" and pull Qb and p values from that

overall_effect <- setDT(
  as.data.frame(cbind(overall_effect$b,overall_effect$ci.lb,overall_effect$ci.ub)), keep.rownames = TRUE)[]
#rename rows
overall_effect$rn <- c("Nonnative Insect with Nonnative Plant*",
                       "Native Insect with Native Plant**")
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
  theme(plot.margin = margin(10, 10, 10, 50))

overall_effect

#all from herb evo hist
all <- all %>%
  filter(plant_organ_afflicted != "flower") %>%
  filter(plant_organ_afflicted != "fruit") %>%
  droplevels()

all <- all %>%
  filter(plant_organ_afflicted == "leaves")

metanalysis("biomass")
