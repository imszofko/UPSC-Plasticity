library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggpubr)
library(ggrepel)
SGE <- read.table("~/UPSC_ThesisWork/Data/Saevar_buds_GE/GE_matrix_samples.tsv", header = T, sep = '\t', as.is = T)
EGE <- read.table("~/UPSC_ThesisWork/Data/Ekebo_wood_GE/GE_vst_mean.tsv", header = T, sep = '\t', as.is = T)
SCR <- read.table("~/UPSC_ThesisWork/Data/Saevar_buds_GE/swasp_reassignments.txt", header = T, sep = '\t', as.is = T)
SGE <- SGE %>%
  pivot_longer(cols = starts_with("n"),
               names_to = c("Samples"),
               values_to = c("Expression"))

EGE <- EGE %>% 
  pivot_longer(cols = starts_with("SWASP"),
               names_to = c("Samples"),
               values_to = c("Expression"))

SGEMeans <- SGE %>%
  group_by(Samples) %>%
  summarize(mean_expression = mean(Expression, na.rm = TRUE)) %>%
  ungroup()

EGEMeans <- EGE %>%
  group_by(Samples) %>%
  summarize(mean_expression = mean(Expression, na.rm = TRUE)) %>%
  ungroup()

EGEMeans <- EGEMeans %>% mutate(Samples = str_extract(EGEMeans$Samples, "(?<=_)\\d+"))
EGEMeans$Samples <- as.numeric(EGEMeans$Samples)

SGEMeans[c("n", "Meta.row", "Clone")] <- str_split_fixed(SGEMeans$Samples, '\\.', 3)

SEC <- read.table("~/UPSC_ThesisWork/Data/Plasticity/untransformed/Location/Ekebo_ChosenClones.txt", header = T, sep = '\t', as.is = T)
SSC <- read.table("~/UPSC_ThesisWork/Data/Plasticity/untransformed/Location/Saevar_ChosenClones.txt", header = T, sep = '\t', as.is = T)

EGEMeans <- EGEMeans %>%
  mutate(Selected = Samples %in% SEC$Clones)

SGEMeans <- SGEMeans %>%
  mutate(Selected = Clone %in% SSC$Clones)

ggplot(SGEMeans, aes(sample = mean_expression)) +
  stat_qq() +
  xlab("Theoretical Normal Quantiles")+
  ylab("Sample Quantiles")+
  stat_qq_line(col = "blue") +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))+   # Rotates x-axis labels
  ggtitle("Mean Gene Expression Distribution of Clones in Sävar")

ggplot(EGEMeans, aes(sample = mean_expression)) +
  stat_qq() +
  stat_qq_line()

+
  stat_qq() +
  stat_qq_line(col = "blue") +
  xlab("Theoretical Normal Quantiles")+
  ylab("Sample Quantiles")+
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))+   # Rotates x-axis labels
  ggtitle("Mean Gene Expression Distribution of Clones in Ekebo")

SP <-ggplot(SGEMeans, aes(sample = mean_expression)) +
  stat_qq() +
  xlab("Theoretical Quantiles")+
  ylab("Sample Quantiles")+
  stat_qq_line(col = "blue") +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))+   # Rotates x-axis labels
  ggtitle("Mean Gene Expression Distribution of Clones in Sävar")

EP <- ggplot(EGEMeans, aes(sample = mean_expression)) +
  stat_qq() +
  stat_qq_line(col = "blue") +
  xlab("Theoretical Quantiles")+
  ylab("Sample Quantiles")+
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))+   # Rotates x-axis labels
  ggtitle("Mean Gene Expression Distribution of Clones in Ekebo")


# Sort and compute quantiles
SGEMeans2 <- SGEMeans %>%
  arrange(mean_expression) %>%
  mutate(theoretical = qnorm(ppoints(n())))


SSP <- ggplot(SGEMeans2, aes(x = theoretical, y = mean_expression, color = Selected)) +
  geom_point(show.legend = F) +
  geom_smooth(method = "lm", se = FALSE, color = "pink") +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
  labs(title = "Sävar Clones Mean Gene Expression Distribution", 
       subtitle = "Selected Clones Highlighted in Red",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))  # Rotates x-axis labels

EGEMeans2 <- EGEMeans %>%
  arrange(mean_expression) %>%
  mutate(theoretical = qnorm(ppoints(n())))

SEP <- ggplot(EGEMeans2, aes(x = theoretical, y = mean_expression, color = Selected)) +
  geom_point(show.legend = F) +
  geom_smooth(method = "lm", se = FALSE, color = "pink") +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
  labs(title = "Mean Gene Expression Distribution of Clones in Ekebo", 
       subtitle = "Selected Clones Highlighted in Red",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))  # Rotates x-axis labels

visDir = "~/UPSC_ThesisWork/Writing/Thesis_Visuals/"
png(paste(visDir, "Saevar_Dist.png",sep = ""), width = 14, height = 8, units = "in", res = 250)
ggarrange(SP, SSP, labels = c("A", "B"), ncol = 2, nrow = 1)
dev.off()

png(paste(visDir, "Ekebo_Dist.png",sep = ""), width = 14, height = 8, units = "in", res = 250)
ggarrange(EP, SEP, labels = c("A", "B"), ncol = 2, nrow = 1)
dev.off()