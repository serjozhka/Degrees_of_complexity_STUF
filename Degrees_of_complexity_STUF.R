# 1. Install packages
## Install packages for measuring entropy and MI
install.packages('infotheo')
library(infotheo)

## Install a collection of packages for data manipulation (tidyverse)
install.packages('tidyverse')
library(tidyverse)

## Install data visualization packages for nice plots
install.packages('ggplot2')
library(ggplot2)
install.packages('ggpubr')
library(ggpubr)

## Install a package for nice maps
install.packages('lingtypology', dependencies = TRUE)
library(lingtypology)

## Install a package for linear regression models (instead of "lme4", which does not provide p-values)
install.packages("lmerTest")
library(lmerTest)

## Install a package for data manipulation
install.packages("dplyr")
library(dplyr)

## Install packages for reading and writing .xlsx files
install.packages("readxl")
library(readxl)

install.packages("writexl")
library(writexl)

## Install a package used to adjust the positions of labels in figures
install.packages('ggrepel')
library(ggrepel)

## Install a package with nice palettes (NB: used in answers to a reviewer, not used in the paper itself)
install.packages('colorspace')
library(colorspace)

## install a package to save html objects
install.packages('htmlwidgets')
library(htmlwidgets) 

# 2. Read the data from files

## Read the languages_preprocessed.xlsx file: info on the lgs in the BivalTyp dataset
## While pre-processing, I manually erase all columns except 
## language_no, language, language_external, expert, macroarea, family (WALS), genus (WALS), latitude, longitude, number_nominal_cases, glottocode, WO_WALS

languages <- read_excel(file.choose())
languages <- as.data.frame(languages)
no_languages <- nrow(languages)

## Read the data_for_download_preprocessed.xlsx file: 
## info valency patterns in the BivalTyp dataset
## While pre-processing, I manually erase all columns except 
## language_no, predicate_no, X, Y, locus, valency_pattern

data <- read_excel(file.choose())
data <- as.data.frame(data)

## Read the file with predicates and their numbers (predicates.xlsx). 
## Before that, manually replace all "#" with "_"

predicates <- read_excel(file.choose())
predicates <- as.data.frame(predicates)
no_predicates <- nrow(predicates)

## Read the file with the basic statistics on languages (language_stats.csv)
language_stats <- read.table(file.choose(), header = T, sep = '\t', encoding = 'UTF-8')

## Create backup copies of all raw files in case they are modified during loops or other processes

backup_data <- data
backup_languages <- languages
backup_predicates <- predicates
backup_language_stats <- language_stats

# 3. Streamline and beautify the dataframes

## Replace blanks and asterisks with 'NA's
data[data == ''] <- NA
data[data == '*'] <- NA

## add language names and predicate labels to data
data$language <- NA
data$predicate_label_en <- NA

for (i in 1:nrow(data)) 
{data$language[i] <- languages$language[which(languages$language_no == data$language_no[i])]}

for (i in 1:nrow(data)) 
{data$predicate_label_en[i] <- predicates$predicate_label_en[which(predicates$predicate_no == data$predicate_no[i])]}

# 4. Create a dataframe with an overview of patterns

patterns <- matrix (data = NA, nrow = no_predicates, ncol = no_languages)
rownames(patterns) <- predicates$predicate_label_en
colnames(patterns) <- languages$language_external

for (i in 1:nrow(data)) {
  patterns[data$predicate_no[i], data$language_no[i]] <- data$valency_pattern[i]
  }

## Create a backup copy of the patterns overview in case it is modified during loops or other processes

backup_patterns <- patterns

# 5. Prepare a full spreadsheet with data on languages
# Calculate _corrected_ entropy values (NB: do not download the values available as part of BivalTyp)
 
## Merge the dataframes with basic given infos on lgs and basic BivalTyp stats

languages_everything <- merge(x = languages, y = language_stats, by = "language_no")

## Remove redundant and unnecessary columns
languages_everything <- subset(languages_everything, select = - number.of.nominal.cases)
languages_everything <- subset(languages_everything, select = - normalised_entropy)

## Add family-based colours
family_colours <- data.frame(unique(languages_everything$family_WALS))
colnames(family_colours) <- "family_WALS"
family_colours$colours <- rainbow_hcl(nrow(family_colours))
languages_everything <- merge(x = languages_everything, y = family_colours, by = "family_WALS")
languages_everything <- languages_everything[order(languages_everything$language_no), ]

## Remove languages with many NAs, prepare raw data accordingly
limit <- 90
languages_everything <- languages_everything[-which(languages_everything$overallN < limit), ]
data <- data[data$language %in% languages_everything$language, ]

## Rename columns with raw (downloaded) entropy values
names(languages_everything)[names(languages_everything) == "entropy_nat"] <- "entropy_raw"
names(languages_everything)[names(languages_everything) == "entropy_of_intransitives_nat"] <- "entropy_intr_raw"

## Replace entropy values with corrected values calculated for subsets of fixed size 
## In this part of the code, I also set seed to make the random aspect of my
## data-generating process fully reproducible

temp2 <- vector(length = 100)
set.seed(321)

for (j in 1:nrow(languages_everything)) {
  temp1 <- data$valency_pattern[which(data$language == languages_everything$language[j])]
  temp1 <- temp1[!is.na(temp1)]
  for (i in 1:100) {
    temp2[i] <- entropy(temp1[sample(1:length(temp1), limit, replace = FALSE)])
    }
  languages_everything$entropy_corr[j] <- mean(temp2)
  }

## Replace entropy of intransitives values with corrected values 
## calculated for subsets of fixed (minimal) size 

temp2 <- vector(length = 100)
minintr <- min(languages_everything$intransitives)

set.seed(321)
for (j in 1:nrow(languages_everything)) {
  temp1 <- data$valency_pattern[which(data$language == languages_everything$language[j])]
  temp1 <- temp1[!is.na(temp1)]
  temp1 <- temp1[!temp1 == "TR"]
  for (i in 1:100) {
    temp2[i] <- entropy(temp1[sample(1:length(temp1), minintr, replace = FALSE)])
  }
  languages_everything$entropy_intr_corr[j] <- mean(temp2)
}

## Backup and write the resultant table
backup_languages_everything <- languages_everything
write_xlsx(languages_everything, path = "languages_everything.xlsx") 

# 6. Density plots for basic complexity parameters

## Create a density plot for Transitivity prominence (Figure 1)
jpeg('Density_Transitivity_prominence.jpg', width = 7, height = 4.5, units = 'in', res = 300)
ggplot(languages_everything, aes(x = transitivity_ratio)) +
  geom_density(fill = "grey", color = "black") +
  xlim(0, 1) + xlab("Transitivity prominence")
dev.off()

# Create a density plot for H (Figure 2)
jpeg('Density_H.jpg', width = 7, height = 4.5, units = 'in', res = 300)
ggplot(languages_everything, aes(x = entropy_corr)) +
  geom_density(fill = "grey", color = "black") +
  xlim(0, 4.5) + xlab("Entropy of bivalent verbs (nats)") 
dev.off()

# Create a density plot for Hintr (Figure 3)
jpeg('Density_Hintr.jpg', width = 7, height = 4.5, units = 'in', res = 300)
ggplot(languages_everything, aes(x = entropy_intr_corr)) +
  geom_density(fill = "grey", color = "black") +
  xlim(0, 3.18) + xlab("Entropy of bivalent intransitives (nats)") 
dev.off()

# 7. Add columns with the number of i) different coding patterns for X, ii) i) different coding patterns for Y

no_languages_selected <- nrow(languages_everything)
for (i in 1:no_languages_selected) {
  temp_data <- data[which(data$language == languages_everything$language[i]),]
  languages_everything$various_X[i] <- length(unique(temp_data$X[!is.na(temp_data$X)]))
  languages_everything$various_Y[i] <- length(unique(temp_data$Y[!is.na(temp_data$Y)]))
}

write.table(languages_everything, file = 'languages_everything.txt', row.names = F, col.names = T, sep = '\t')
write_xlsx(languages_everything, path = "languages_everything.xlsx") 

# 8. Some calculations on the use of non-verbal predicates and entropy
## This is based on published languages only. I employ a temporary preprocessed file data_published.xlsx

temp_data_published <- read_excel(file.choose())
temp_data_published <- as.data.frame(temp_data_published)

## Some beautification procedures
temp_data_published[temp_data_published == ''] <- NA
temp_data_published[temp_data_published == '*'] <- NA
temp_data_published$language <- NA
temp_data_published$predicate_label_en <- NA

for (i in 1:nrow(temp_data_published)) 
{temp_data_published$language[i] <- languages$language[which(languages$language_no == temp_data_published$language_no[i])]}

for (i in 1:nrow(temp_data_published)) 
{temp_data_published$predicate_label_en[i] <- predicates$predicate_label_en[which(predicates$predicate_no == temp_data_published$predicate_no[i])]}

## Calculate the prevalence of non-verbal predicates for published languages
languages_everything$no_non_verbal <- 'NA'
for (i in 1:no_languages_selected) {
  temp_data <- temp_data_published[which(temp_data_published$language == languages_everything$language[i]),]
  languages_everything$no_non_verbal[i] <- table(temp_data$non_verbal)["yes"]
}

languages_everything$no_non_verbal <- as.numeric(languages_everything$no_non_verbal)
languages_everything$ratio_non_verbal <- as.numeric(languages_everything$no_non_verbal/languages_everything$overallN)
write_xlsx(languages_everything, path = "languages_everything.xlsx") 

languages_everything_with_non_verbal <- languages_everything %>% 
  filter(!is.na(ratio_non_verbal))
  
## Create a scatterplot showing the prevalence of non-verbal predicates and Hintr (Figure 11).

jpeg('Nonverbal_Hintr_for_publication.jpg', width = 6.5, height = 4.5, units = 'in', res = 300)
ggplot(data = languages_everything_with_non_verbal,
       aes(x = ratio_non_verbal, 
           y = entropy_intr_corr,
           color = family_WALS)) +
  geom_point(size = 1.5) +  
  geom_text_repel( 
    aes(label = language_external), 
    size = 2,
    max.overlaps = Inf,
    show.legend = FALSE) +
  xlab("Prevalence of non-verbal predicates") +
  ylab("Entropy of bivalent intransitives (nats)") +
  labs(color = "Family") +
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Increase dot size in the legend
  theme(legend.position = "right",
        legend.justification = "top",
        legend.key.height = unit(0.4, "cm"))
dev.off()

# 9. Some calculations on colexification/polysemy and entropy
## Preparatory part for the analysis of polysemy patterns in some published languages
## I use the heavily Excel-preprocessed spreadsheet where I semi-manually annotated 
## each entry for whether i) it employs the same verb as any of the previous entries,
## and whether ii) it employs a different valency pattern.
## Read this pre-processed file (polysemy_raw_data.xlsx)

polysemy_raw_data <- read_excel(file.choose())
polysemy_raw_data <- as.data.frame(polysemy_raw_data)

## Some beautification procedures
polysemy_raw_data$language <- NA
polysemy_raw_data$predicate_label_en <- NA

for (i in 1:nrow(polysemy_raw_data)) 
{polysemy_raw_data$language[i] <- languages$language[which(languages$language_no == polysemy_raw_data$language_no[i])]}

for (i in 1:nrow(polysemy_raw_data)) 
{polysemy_raw_data$predicate_label_en[i] <- predicates$predicate_label_en[which(predicates$predicate_no == polysemy_raw_data$predicate_no[i])]}

## Calculate the prevalence of colexification patterns (including those with 
## distinct valency pattern) for published languages with specified verb lexemes
languages_everything$colex <- 'NA'
languages_everything$colex_with_distinct <- 'NA'

for (i in 1:no_languages_selected) {
  temp_data <- polysemy_raw_data[which(polysemy_raw_data$language == languages_everything$language[i]),]
  if (nrow(temp_data) > 0) {languages_everything$colex[i] <- sum(temp_data$same_verb == "yes")}
  if (nrow(temp_data) > 0) {languages_everything$colex_with_distinct[i] <- sum(temp_data$same_verb_and_different_pattern == "yes")}
  }

languages_everything$colex <- as.numeric(languages_everything$colex)
languages_everything$ratio_colex <- as.numeric(languages_everything$colex/languages_everything$overallN)
languages_everything$colex_with_distinct <- as.numeric(languages_everything$colex_with_distinct)
languages_everything$ratio_colex_with_distinct <- as.numeric(languages_everything$colex_with_distinct/languages_everything$overallN)

write_xlsx(languages_everything, path = "languages_everything.xlsx") 

# 10. Create maps with valency class complexity metrics (Figures 5, 6, 7, 8)

map_data('world') |> 
  filter(region != "Antarctica") |> 
  fortify() ->
  WorldData

## There might be issues with decimal signs (comma or period); in the worst scenario, replace it manually

# Option 1: produce an .html for own use
map.feature(languages = languages_everything$language, features = languages_everything$transitivity_ratio, longitude = languages_everything$longitude, latitude = languages_everything$latitude)
map_Transitivity_prominence_world <- map.feature(languages = languages_everything$language, features = languages_everything$transitivity_ratio, longitude = languages_everything$longitude, latitude = languages_everything$latitude)
saveWidget(map_Transitivity_prominence_world, file="Transitivity_prominence_world.html")

# Option 2: produce a camera-ready .png map (Figure 5 in the manuscript)
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(group = group, map_id=region),
           fill = "khaki", color = "grey80", size=0.1) +
  geom_point(data = languages_everything, aes(x = longitude, y = latitude, 
                                              color = transitivity_ratio),
             size = 3)+
  coord_sf(xlim=c(-22, 180), ylim=c(-30, 90), expand = FALSE)+
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  scale_color_gradient(low = "lightblue", high = "darkmagenta") +
  labs(x=NULL, y=NULL, color = NULL) +
  theme_void()+
  theme(plot.background=element_rect(fill="azure2", color = "azure2"))  ->
  p1
ggsave(filename = "Figure_5_Transitivity_prominence_world.png", plot = p1, width = 9, height = 7)


# Option 1: produce an .html for own use
map.feature(languages = languages_everything$language, features = languages_everything$entropy_corr, longitude = languages_everything$longitude, latitude = languages_everything$latitude)
map_H_world <- map.feature(languages = languages_everything$language, features = languages_everything$entropy_corr, longitude = languages_everything$longitude, latitude = languages_everything$latitude)
saveWidget(map_H_world, file="H_world.html")

# Option 2: produce a camera-ready .png map (Figure 6 in the manuscript)
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(group = group, map_id=region),
           fill = "khaki", color = "grey80", size=0.1) +
  geom_point(data = languages_everything, aes(x = longitude, y = latitude, 
                                              color = entropy_corr),
             size = 3)+
  coord_sf(xlim=c(-22, 180), ylim=c(-30, 90), expand = FALSE)+
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  scale_color_gradient(low = "lightblue", high = "darkmagenta") +
  labs(x=NULL, y=NULL, color = NULL) +
  theme_void()+
  theme(plot.background=element_rect(fill="azure2", color = "azure2"))  ->
  p2
ggsave(filename = "Figure_6_H_world.png", plot = p2, width = 9, height = 7)

# Option 1: produce an .html for own use
map.feature(languages = languages_everything$language, features = languages_everything$entropy_intr_corr, longitude = languages_everything$longitude, latitude = languages_everything$latitude)
map_Hintr_world <- map.feature(languages = languages_everything$language, features = languages_everything$entropy_intr_corr, longitude = languages_everything$longitude, latitude = languages_everything$latitude)
saveWidget(map_Hintr_world, file="Hintr_world.html")

# Option 2: produce camera-ready .png maps (Figures 7 and 8 in the manuscript)
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(group = group, map_id=region),
           fill = "khaki", color = "grey80", size=0.1) +
  geom_point(data = languages_everything, aes(x = longitude, y = latitude, 
                                              color = entropy_intr_corr),
             size = 3)+
  coord_sf(xlim=c(-22, 180), ylim=c(-30, 90), expand = FALSE)+
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  scale_color_gradient(low = "lightblue", high = "darkmagenta") +
  labs(x=NULL, y=NULL, color = NULL) +
  theme_void()+
  theme(plot.background=element_rect(fill="azure2", color = "azure2"))  ->
  p3
ggsave(filename = "Figure_7_Hintr_world.png", plot = p3, width = 9, height = 7)

ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(group = group, map_id=region),
           fill = "khaki", color = "grey80", size=0.1) +
  geom_point(data = languages_everything, aes(x = longitude, y = latitude, 
                                              color = entropy_intr_corr),
             size = 3)+
  coord_sf(xlim=c(39, 50), ylim=c(39, 46), expand = FALSE)+
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  scale_color_gradient(low = "lightblue", high = "darkmagenta") +
  labs(x=NULL, y=NULL, color = NULL) +
  theme_void()+
  theme(plot.background=element_rect(fill="azure2", color = "azure2")) ->
  p4
ggsave(filename = "Figure_8_Hintr_Caucasus.png", plot = p4, width = 9, height = 7)

  
# 11. Create a scatterplot with the entropy of intransitives and transitivity ratio
# Visualization for publication (Figure 4)

jpeg('TrProminence_Hintr_for_publication.jpg', width = 6.5, height = 4.5, units = 'in', res = 300)
ggplot(data = languages_everything,
       aes(x = transitivity_ratio, 
           y = entropy_intr_corr,
           color = family_WALS)) +
  geom_point(size = 1.5) +  
  geom_text_repel( 
    aes(label = language_external), 
    size = 2,
    max.overlaps = Inf,
    show.legend = FALSE) +
  xlab("Transitivity prominence") +
  ylab("Entropy of bivalent intransitives (nats)") +
  labs(color = "Family") +
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Increase dot size in the legend
  theme(legend.position = "right",
        legend.justification = "top",
        legend.key.height = unit(0.4, "cm"))
dev.off()

# 12. Explore the potential role of the number of nominal cases
## Create a scatterplot showing the relationshion between Hintr and the number of nominal cases with families shown by colors (figure 10)

jpeg('Cases_Hintr_publication.jpg', width = 6.5, height = 4.5, units = 'in', res = 300)
ggplot(data = languages_everything,
       aes(x = number_nominal_cases, 
           y = entropy_intr_corr,
           color = family_WALS)) +
  geom_point(size = 1.5) +  
  geom_text_repel( 
    aes(label = language_external), 
    size = 2,
    max.overlaps = Inf,
    show.legend = FALSE) +
  xlab("Number of nominal cases") +
  ylab("Entropy of bivalent intransitives (nats)") +
  labs(color = "Family") +
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Increase dot size in the legend
  theme(legend.position = "right",
        legend.justification = "top",
        legend.key.height = unit(0.4, "cm"))
dev.off()

# 13. Check the difference between SVO and SOV languages

pdf('WO_Hintr.pdf', width = 4.5, height = 4.5, pointsize = 4)
boxplot(data = languages_everything, entropy_intr_corr~WO_WALS, at = c (NA, 1, 2, NA), notch = T, xlab = "Word order", ylab = "Hintr (nats)") 
dev.off()

# 14. Check the difference between families
# Create a boxplot showing the range of Hintr in four families (Figure 9)

jpeg('Hintr_families.jpg', width = 4.5, height = 4.5, units = 'in', res = 300)
par(cex.axis = 0.7, cex.lab = 0.7)
boxplot(data = languages_everything, entropy_intr_corr ~ family_WALS, 
        at = c(NA, 4, NA, NA, NA, NA, 3, NA, NA, NA, NA, 1, NA, NA, NA, NA, 2, NA), 
        xlab = "Families (WALS)", ylab = "Entropy of bivalent intransitives (nats)") 
dev.off()

# 15. Linear regression models

## Create a model that predicts corrected entropy from transitivity ratio taking family as a random factor
## this is mentioned in Section 3.2 (see Table 3)
model2 <- lmer(languages_everything$entropy_corr ~ languages_everything$transitivity_ratio + (1 | languages_everything$family_WALS))
summary(model2)

## Create a model that predicts corrected entropy of intransitives from transitivity ratio 
## taking family as a random factor. This is mentioned in Section 3.2 (see Table 4)
model3 <- lmer(languages_everything$entropy_intr_corr ~ languages_everything$transitivity_ratio + (1 | languages_everything$family_WALS))
summary(model3)

## Create two models that predict corrected entropy of bivalent verbs and corrected 
## entropy of bivalent intransitives from basic word order, taking family as a random factor
## Before fitting this model, I subset the data leaving only languages with either SOV or SVO 
## these two models are mentioned in Section 4.1 (and model8 is summarized in Table 6)

temp_languages_everything <- languages_everything[languages_everything$WO_WALS %in% c('SOV', 'SVO'), ]

model8 <- lmer(entropy_corr ~ WO_WALS + (1 | family_WALS), data = temp_languages_everything)
summary(model8)

model9 <- lmer(entropy_intr_corr ~ WO_WALS + (1 | family_WALS), data = temp_languages_everything)
summary(model9)

## Create three models that predict complexity metrics from the prevalence of non-verbal predicates. 
## these models are briefly mentioned in section 5.1.

model10 <- lmer(transitivity_ratio ~ ratio_non_verbal + (1 | family_WALS), data = languages_everything)
summary(model10)
model11 <- lmer(entropy_corr ~ ratio_non_verbal + (1 | family_WALS), data = languages_everything)
summary(model11)
model12 <- lmer(entropy_intr_corr ~ ratio_non_verbal + (1 | family_WALS), data = languages_everything)
summary(model12)

## Create a model that predicts corrected entropy of bivalent verbs from the prevalence of colexifications
## this model is mentioned in section 5.4

model13 <- lmer(entropy_corr ~ ratio_colex_with_distinct + (1 | family_WALS), data = languages_everything)
summary(model13)