# Vorbereitung - Datensatz einlesen & Variablen berechnen
df <- read.table ("~/Desktop/DatenMasterarbeit.csv", header=TRUE, sep=",", na.strings="NA", dec=".")

# Vorbereitung - Irritation Ausreisser entfernen
df[29,11] <- NA
df[29,29] <- NA

# Vorbereitung - Variablen berechnen
df$irritation_difference <- df$Irritation_M - df$Irritation_A
df$irritation_quotient <- (df$Irritation_M + 1) / (df$Irritation_A + 1)
df$worry_difference <- df$Worry_M - df$Worry_A
df$worry_quotient <- (df$Worry_M + 1) / (df$Worry_A + 1)
df$inconvenience_difference <- df$Inconvenience_M - df$Inconvenience_A
df$inconvenience_quotient <- (df$Inconvenience_M + 1) / (df$Inconvenience_A + 1)
df$anger_difference <- df$Anger_M - df$Anger_A
df$anger_quotient <- (df$Anger_M + 1) / (df$Anger_A + 1)

# Vorbereitung - DOCS Ausreisser entfernen
df2 <- df[df$DOCSGesamt_M < 50 & !is.na(df$DOCSGesamt_M),]

# Vorbereitung - Packages laden
library(tidyr)
library(ggplot2)
library(cowplot)
library(effectsize)

# 1. Forschungsfrage - Differenzen
mean(df$irritation_difference, na.rm = TRUE)
sd(df$irritation_difference, na.rm = TRUE)
min(df$irritation_difference, na.rm = TRUE)
max(df$irritation_difference, na.rm = TRUE)
shapiro.test(df$irritation_difference)

mean(df$worry_difference, na.rm = TRUE)
sd(df$worry_difference, na.rm = TRUE)
min(df$worry_difference, na.rm = TRUE)
max(df$worry_difference, na.rm = TRUE)
shapiro.test(df$worry_difference)

mean(df$inconvenience_difference, na.rm = TRUE)
sd(df$inconvenience_difference, na.rm = TRUE)
min(df$inconvenience_difference, na.rm = TRUE)
max(df$inconvenience_difference, na.rm = TRUE)
shapiro.test(df$inconvenience_difference)

mean(df$anger_difference, na.rm = TRUE)
sd(df$anger_difference, na.rm = TRUE)
min(df$anger_difference, na.rm = TRUE)
max(df$anger_difference, na.rm = TRUE)
shapiro.test(df$anger_difference)

# 1. Forschungsfrage - Quotienten
mean(df$irritation_quotient, na.rm = TRUE)
sd(df$irritation_quotient, na.rm = TRUE)
min(df$irritation_quotient, na.rm = TRUE)
max(df$irritation_quotient, na.rm = TRUE)
shapiro.test(df$irritation_quotient)

mean(df$worry_quotient, na.rm = TRUE)
sd(df$worry_quotient, na.rm = TRUE)
min(df$worry_quotient, na.rm = TRUE)
max(df$worry_quotient, na.rm = TRUE)
shapiro.test(df$worry_quotient)

mean(df$inconvenience_quotient, na.rm = TRUE)
sd(df$inconvenience_quotient, na.rm = TRUE)
min(df$inconvenience_quotient, na.rm = TRUE)
max(df$inconvenience_quotient, na.rm = TRUE)
shapiro.test(df$inconvenience_quotient)

mean(df$anger_quotient, na.rm = TRUE)
sd(df$anger_quotient, na.rm = TRUE)
min(df$anger_quotient, na.rm = TRUE)
max(df$anger_quotient, na.rm = TRUE)
shapiro.test(df$anger_quotient)

# 1. Forschungsfrage - Boxplot Differenzen
dfDiff <- df[,grep("difference", colnames(df))]
dfDiffL <- pivot_longer(dfDiff, cols = everything(), names_to = "Dimension", values_to = "Difference")
dfDiffL$Dimension <- forcats::as_factor(dfDiffL$Dimension)

(BoxDiff <- ggplot(dfDiffL, aes(x = Dimension, y = Difference, fill = Dimension)) +
  geom_boxplot(staplewidth = 0.25, fill = c("red", "yellow", "green", "turquoise"), na.rm = TRUE) +
  labs(title = "Differences in the Perception of Burden", x = NULL, y = "Rating Difference") +
  scale_x_discrete(labels = c("Irritation", "Worry", "Inconvenience", "Anger")) +
  coord_cartesian(ylim = c(-8, 8)) +
  scale_y_continuous(breaks = seq(-8, 8, by = 2)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)))

ggsave(plot = BoxDiff, filename = "BoxDiff.png", path = "~/Desktop/", device = "png", width = 16, height = 10, units = "cm", dpi = 300)

# 1. Forschungsfrage - Boxplot Quotienten
dfQuot <- df[,grep("quotient", colnames(df))]
dfQuotL <- pivot_longer(dfQuot, cols = everything(), names_to = "Dimension", values_to = "Quotient")
dfQuotL$Dimension <- forcats::as_factor(dfQuotL$Dimension)

(BoxQuot <- ggplot(dfQuotL, aes(x = Dimension, y = Quotient, fill = Dimension)) +
  geom_boxplot(staplewidth = 0.25, fill = c("red", "yellow", "green", "turquoise"), na.rm = TRUE) +
  labs(title = "Ratios of the Perception of Burden", x = NULL, y = "Rating Quotient") +
  scale_x_discrete(labels = c("Irritation", "Worry", "Inconvenience", "Anger")) +
  coord_cartesian(ylim = c(0, 8)) +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)))

ggsave(plot = BoxQuot, filename = "BoxQuot.png", path = "~/Desktop/", device = "png", width = 16, height = 10, units = "cm", dpi = 300)
  
# 1. Forschungsfrage - Histogramm Differenzen
HistDiff1 <- ggplot(df, aes(x = irritation_difference)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black", na.rm = TRUE) +
  labs(title = "Irritation", x = "Rating Difference", y = expression(italic(n)~"(Pairs)")) +
  coord_cartesian(xlim = c(-8, 8), ylim = c(0, 16)) +
  scale_x_continuous(breaks = seq(-8, 8, by = 2)) +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

HistDiff2 <- ggplot(df, aes(x = worry_difference)) +
  geom_histogram(binwidth = 1, fill = "yellow", color = "black", na.rm = TRUE) +
  labs(title = "Worry", x = "Rating Difference", y = expression(italic(n)~"(Pairs)")) +
  coord_cartesian(xlim = c(-8, 8), ylim = c(0, 16)) +
  scale_x_continuous(breaks = seq(-8, 8, by = 2)) +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

HistDiff3 <- ggplot(df, aes(x = inconvenience_difference)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", na.rm = TRUE) +
  labs(title = "Inconvenience", x = "Rating Difference", y = expression(italic(n)~"(Pairs)")) +
  coord_cartesian(xlim = c(-8, 8), ylim = c(0, 16)) +
  scale_x_continuous(breaks = seq(-8, 8, by = 2)) +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

HistDiff4 <- ggplot(df, aes(x = anger_difference)) +
  geom_histogram(binwidth = 1, fill = "turquoise", color = "black", na.rm = TRUE) +
  labs(title = "Anger", x = "Rating Difference", y = expression(italic(n)~"(Pairs)")) +
  coord_cartesian(xlim = c(-8, 8), ylim = c(0, 16)) +
  scale_x_continuous(breaks = seq(-8, 8, by = 2)) +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

(HistDiff <- plot_grid(HistDiff1, HistDiff2, HistDiff3, HistDiff4, nrow = 2, labels = c("A", "B", "C", "D")))

ggsave(plot = HistDiff, filename = "HistDiff.png", path = "~/Desktop/", device = "png", width = 16, height = 10, units = "cm", dpi = 300)

# 1. Forschungsfrage - Histogramm Quotienten
HistQuot1 <- ggplot(df, aes(x = irritation_quotient)) +
  geom_histogram(binwidth = 0.25, fill = "red", color = "black", na.rm = TRUE) +
  labs(title = "Irritation", x = "Rating Quotient", y = expression(italic(n)~"(Pairs)")) +
  coord_cartesian(xlim = c(0, 8), ylim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

HistQuot2 <- ggplot(df, aes(x = worry_quotient)) +
  geom_histogram(binwidth = 0.25, fill = "yellow", color = "black", na.rm = TRUE) +
  labs(title = "Worry", x = "Rating Quotient", y = expression(italic(n)~"(Pairs)")) +
  coord_cartesian(xlim = c(0, 8), ylim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

HistQuot3 <- ggplot(df, aes(x = inconvenience_quotient)) +
  geom_histogram(binwidth = 0.25, fill = "green", color = "black", na.rm = TRUE) +
  labs(title = "Inconvenience", x = "Rating Quotient", y = expression(italic(n)~"(Pairs)")) +
  coord_cartesian(xlim = c(0, 8), ylim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

HistQuot4 <- ggplot(df, aes(x = anger_quotient)) +
  geom_histogram(binwidth = 0.25, fill = "turquoise", color = "black", na.rm = TRUE) +
  labs(title = "Anger", x = "Rating Quotient", y = expression(italic(n)~"(Pairs)")) +
  coord_cartesian(xlim = c(0, 8), ylim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

(HistQuot <- plot_grid(HistQuot1, HistQuot2, HistQuot3, HistQuot4, nrow = 2, labels = c("A", "B", "C", "D")))

ggsave(plot = HistQuot, filename = "HistQuot.png", path = "~/Desktop/", device = "png", width = 16, height = 10, units = "cm", dpi = 300)

# 1. Forschungsfrage - Scatterplot
Scatt1 <- ggplot(df, aes(x = Irritation_A, y = Irritation_M)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "black") +
  scale_size_area(max_size = 4, breaks = 1:4, name = expression(italic(n)~"(Pairs)")) +
  geom_smooth(method = "lm", se = TRUE, color = "red", na.rm = TRUE) +
  labs(title = "Irritation", x = "Rating Person A", y = "Rating Person M") +
  coord_cartesian(xlim = c(0, 9), ylim = c(0, 9)) +
  scale_x_continuous(breaks = seq(0, 9, by = 2)) +
  scale_y_continuous(breaks = seq(0, 9, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

Scatt2 <- ggplot(df, aes(x = Worry_A, y = Worry_M)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "black") +
  scale_size_area(max_size = 5.6, breaks = c(1, 2, 4, 8), name = expression(italic(n)~"(Pairs)")) +
  geom_smooth(method = "lm", se = TRUE, color = "yellow", na.rm = TRUE) +
  labs(title = "Worry", x = "Rating Person A", y = "Rating Person M") +
  coord_cartesian(xlim = c(0, 9), ylim = c(0, 9)) +
  scale_x_continuous(breaks = seq(0, 9, by = 2)) +
  scale_y_continuous(breaks = seq(0, 9, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

Scatt3 <- ggplot(df, aes(x = Inconvenience_A, y = Inconvenience_M)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "black") +
  scale_size_area(max_size = 3.5, breaks = 1:3, name = expression(italic(n)~"(Pairs)")) +
  geom_smooth(method = "lm", se = TRUE, color = "green", na.rm = TRUE) +
  labs(title = "Inconvenience", x = "Rating Person A", y = "Rating Person M") +
  coord_cartesian(xlim = c(0, 9), ylim = c(0, 9)) +
  scale_x_continuous(breaks = seq(0, 9, by = 2)) +
  scale_y_continuous(breaks = seq(0, 9, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

Scatt4 <- ggplot(df, aes(x = Anger_A, y = Anger_M)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "black") +
  scale_size_area(max_size = 3.5, breaks = 1:3, name = expression(italic(n)~"(Pairs)")) +
  geom_smooth(method = "lm", se = TRUE, color = "turquoise", na.rm = TRUE) +
  labs(title = "Anger", x = "Rating Person A", y = "Rating Person M") +
  coord_cartesian(xlim = c(0, 9), ylim = c(0, 9)) +
  scale_x_continuous(breaks = seq(0, 9, by = 2)) +
  scale_y_continuous(breaks = seq(0, 9, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

(Scatt <- plot_grid(Scatt1, Scatt2, Scatt3, Scatt4, nrow = 2, labels = c("A", "B", "C", "D")))

ggsave(plot = Scatt, filename = "Scatt.png", path = "~/Desktop/", device = "png", width = 16, height = 10, units = "cm", dpi = 300)

# 1. Forschungsfrage - t-Test (paired)
t.test(df$Irritation_M, df$Irritation_A, paired = TRUE)
cohens_d(df$Irritation_M, df$Irritation_A, paired = TRUE)

t.test(df$Worry_M, df$Worry_A, paired = TRUE)
cohens_d(df$Worry_M, df$Worry_A, paired = TRUE)

t.test(df$Inconvenience_M, df$Inconvenience_A, paired = TRUE)
cohens_d(df$Inconvenience_M, df$Inconvenience_A, paired = TRUE)

t.test(df$Anger_M, df$Anger_A, paired = TRUE)
cohens_d(df$Anger_M, df$Anger_A, paired = TRUE)

# 2. Forschungsfrage - DOCS Person M
mean(df2$DOCSVerunreinigung_M, na.rm = TRUE)
sd(df2$DOCSVerunreinigung_M, na.rm = TRUE)
min(df2$DOCSVerunreinigung_M, na.rm = TRUE)
max(df2$DOCSVerunreinigung_M, na.rm = TRUE)
shapiro.test(df2$DOCSVerunreinigung_M)

mean(df2$DOCSSchaden_M, na.rm = TRUE)
sd(df2$DOCSSchaden_M, na.rm = TRUE)
min(df2$DOCSSchaden_M, na.rm = TRUE)
max(df2$DOCSSchaden_M, na.rm = TRUE)
shapiro.test(df2$DOCSSchaden_M)

mean(df2$DOCSGedanken_M, na.rm = TRUE)
sd(df2$DOCSGedanken_M, na.rm = TRUE)
min(df2$DOCSGedanken_M, na.rm = TRUE)
max(df2$DOCSGedanken_M, na.rm = TRUE)
shapiro.test(df2$DOCSGedanken_M)

mean(df2$DOCSSymmetrie_M, na.rm = TRUE)
sd(df2$DOCSSymmetrie_M, na.rm = TRUE)
min(df2$DOCSSymmetrie_M, na.rm = TRUE)
max(df2$DOCSSymmetrie_M, na.rm = TRUE)
shapiro.test(df2$DOCSSymmetrie_M)

mean(df2$DOCSGesamt_M, na.rm = TRUE)
sd(df2$DOCSGesamt_M, na.rm = TRUE)
min(df2$DOCSGesamt_M, na.rm = TRUE)
max(df2$DOCSGesamt_M, na.rm = TRUE)
shapiro.test(df2$DOCSGesamt_M)

# 2. Forschungsfrage - DOCS Person A
mean(df2$DOCSGesamt_A, na.rm = TRUE)
sd(df2$DOCSGesamt_A, na.rm = TRUE)
min(df2$DOCSGesamt_A, na.rm = TRUE)
max(df2$DOCSGesamt_A, na.rm = TRUE)
shapiro.test(df2$DOCSGesamt_A)

# 2. Forschungsfrage - Visualisierung DOCS
BoxDOCS <- ggplot(df2, aes(x = DOCSGesamt_M, y = "")) +
  geom_boxplot(staplewidth = 0.25, fill = "purple", color = "black", na.rm = TRUE) +
  labs(title = "DOCS Person M", x = "Total DOCS Score of Person M", y = NULL) +
  coord_cartesian(xlim = c(0, 35)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

HistDOCS <- ggplot(df2, aes(x = DOCSGesamt_M)) +
  geom_histogram(binwidth = 2.5, fill = "purple", color = "black", na.rm = TRUE) +
  labs(title = "DOCS Person M", x = "Total DOCS Score of Person M", y = expression(italic(n)~"(Participants)")) +
  coord_cartesian(xlim = c(0, 35), ylim = c(0, 5)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(0, 5, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

(BothDOCS <- plot_grid(BoxDOCS, HistDOCS, nrow = 1, labels = c("A", "B")))

ggsave(plot = BothDOCS, filename = "BothDOCS.png", path = "~/Desktop/", device = "png", width = 16, height = 5, units = "cm", dpi = 300)

# 2. Forschungsfrage - Scatterplot DOCS Differenzen
ScattDiff1 <- ggplot(df2, aes(x = DOCSGesamt_M, y = irritation_difference)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 2, breaks = 1:2, name = expression(italic(n)~"(Pairs)")) +
  geom_smooth(method = "lm", se = TRUE, color = "red", na.rm = TRUE) +
  labs(title = "Irritation", x = "Total DOCS Score of Person M", y = "Rating Difference") +
  coord_cartesian(xlim = c(0, 35), ylim = c(-6, 6)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(-6, 6, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ScattDiff2 <- ggplot(df2, aes(x = DOCSGesamt_M, y = worry_difference)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 2, breaks = 1:2, name = expression(italic(n)~"(Pairs)")) +
  geom_smooth(method = "lm", se = TRUE, color = "yellow", na.rm = TRUE) +
  labs(title = "Worry", x = "Total DOCS Score of Person M", y = "Rating Difference") +
  coord_cartesian(xlim = c(0, 35), ylim = c(-6, 6)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(-6, 6, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ScattDiff3 <- ggplot(df2, aes(x = DOCSGesamt_M, y = inconvenience_difference)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 2, breaks = 1:2, name = expression(italic(n)~"(Pairs)")) +
  geom_smooth(method = "lm", se = TRUE, color = "green", na.rm = TRUE) +
  labs(title = "Inconvenience", x = "Total DOCS Score of Person M", y = "Rating Difference") +
  coord_cartesian(xlim = c(0, 35), ylim = c(-6, 6)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(-6, 6, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ScattDiff4 <- ggplot(df2, aes(x = DOCSGesamt_M, y = anger_difference)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 1.4, breaks = 1:1, name = expression(italic(n)~"(Pairs)")) +
  geom_smooth(method = "lm", se = TRUE, color = "turquoise", na.rm = TRUE) +
  labs(title = "Anger", x = "Total DOCS Score of Person M", y = "Rating Difference") +
  coord_cartesian(xlim = c(0, 35), ylim = c(-6, 6)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(-6, 6, by = 2)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

(ScattDiff <- plot_grid(ScattDiff1, ScattDiff2, ScattDiff3, ScattDiff4, nrow = 2, labels = c("A", "B", "C", "D")))

ggsave(plot = ScattDiff, filename = "ScattDiff.png", path = "~/Desktop/", device = "png", width = 16, height = 10, units = "cm", dpi = 300)

# 2. Forschungsfrage - Scatterplot DOCS Quotienten
ScattQuot1 <- ggplot(df2, aes(x = DOCSGesamt_M, y = irritation_quotient)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 2, breaks = 1:2, name = expression(italic(n)~"(Pairs)")) +
  geom_smooth(method = "lm", se = TRUE, color = "red", na.rm = TRUE) +
  labs(title = "Irritation", x = "Total DOCS Score of Person M", y = "Rating Quotient") +
  coord_cartesian(xlim = c(0, 35), ylim = c(0, 5)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(0, 5, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ScattQuot2 <- ggplot(df2, aes(x = DOCSGesamt_M, y = worry_quotient)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 2, breaks = 1:2, name = expression(italic(n)~"(Pairs)")) +
  geom_smooth(method = "lm", se = TRUE, color = "yellow", na.rm = TRUE) +
  labs(title = "Worry", x = "Total DOCS Score of Person M", y = "Rating Quotient") +
  coord_cartesian(xlim = c(0, 35), ylim = c(0, 5)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(0, 5, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ScattQuot3 <- ggplot(df2, aes(x = DOCSGesamt_M, y = inconvenience_quotient)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 2, breaks = 1:2, name = expression(italic(n)~"(Pairs)")) +
  geom_smooth(method = "lm", se = TRUE, color = "green", na.rm = TRUE) +
  labs(title = "Inconvenience", x = "Total DOCS Score of Person M", y = "Rating Quotient") +
  coord_cartesian(xlim = c(0, 35), ylim = c(0, 5)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(0, 5, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ScattQuot4 <- ggplot(df2, aes(x = DOCSGesamt_M, y = anger_quotient)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 1.4, breaks = 1:1, name = expression(italic(n)~"(Pairs)")) +
  geom_smooth(method = "lm", se = TRUE, color = "turquoise", na.rm = TRUE) +
  labs(title = "Anger", x = "Total DOCS Score of Person M", y = "Rating Quotient") +
  coord_cartesian(xlim = c(0, 35), ylim = c(0, 5)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(0, 5, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

(ScattQuot <- plot_grid(ScattQuot1, ScattQuot2, ScattQuot3, ScattQuot4, nrow = 2, labels = c("A", "B", "C", "D")))

ggsave(plot = ScattQuot, filename = "ScattQuot.png", path = "~/Desktop/", device = "png", width = 16, height = 10, units = "cm", dpi = 300)

# 2. Forschungsfrage - Korrelation DOCS Differenzen
cor.test(df2$DOCSGesamt_M, df2$irritation_difference, method = "pearson", use = "complete.obs")

cor.test(df2$DOCSGesamt_M, df2$worry_difference, method = "pearson", use = "complete.obs")

cor.test(df2$DOCSGesamt_M, df2$inconvenience_difference, method = "pearson", use = "complete.obs")

cor.test(df2$DOCSGesamt_M, df2$anger_difference, method = "pearson", use = "complete.obs")

# 2. Forschungsfrage - Korrelation DOCS Quotienten
cor.test(df2$DOCSGesamt_M, df2$irritation_quotient, method = "pearson", use = "complete.obs")

cor.test(df2$DOCSGesamt_M, df2$worry_quotient, method = "pearson", use = "complete.obs")

cor.test(df2$DOCSGesamt_M, df2$inconvenience_quotient, method = "pearson", use = "complete.obs")

cor.test(df2$DOCSGesamt_M, df2$anger_quotient, method = "pearson", use = "complete.obs")

# Weitere Daten - Anzahl Probanden / Paare
length(na.omit(df$ID))

# Weitere Daten - Anzahl Probanden / Paare pro Analyse
length(na.omit(df$irritation_difference))
length(na.omit(df$worry_difference))
length(na.omit(df$inconvenience_difference))
length(na.omit(df$anger_difference))

length(na.omit(df$irritation_quotient))
length(na.omit(df$worry_quotient))
length(na.omit(df$inconvenience_quotient))
length(na.omit(df$anger_quotient))

length(na.omit(df2$DOCSVerunreinigung_M))
length(na.omit(df2$DOCSSchaden_M))
length(na.omit(df2$DOCSGedanken_M))
length(na.omit(df2$DOCSSymmetrie_M))
length(na.omit(df2$DOCSGesamt_M))

length(na.omit(df2$DOCSGesamt_A))

sum(complete.cases(df2$irritation_difference, df2$DOCSGesamt_M))
sum(complete.cases(df2$worry_difference, df2$DOCSGesamt_M))
sum(complete.cases(df2$inconvenience_difference, df2$DOCSGesamt_M))
sum(complete.cases(df2$anger_difference, df2$DOCSGesamt_M))

sum(complete.cases(df2$irritation_quotient, df2$DOCSGesamt_M))
sum(complete.cases(df2$worry_quotient, df2$DOCSGesamt_M))
sum(complete.cases(df2$inconvenience_quotient, df2$DOCSGesamt_M))
sum(complete.cases(df2$anger_quotient, df2$DOCSGesamt_M))

# Demographische Daten - Alter
mean(df$Age_M, na.rm = TRUE)
sd(df$Age_M, na.rm = TRUE)
min(df$Age_M, na.rm = TRUE)
max(df$Age_M, na.rm = TRUE)

mean(df$Age_A, na.rm = TRUE)
sd(df$Age_A, na.rm = TRUE)
min(df$Age_A, na.rm = TRUE)
max(df$Age_A, na.rm = TRUE)

mean(c(df$Age_M, df$Age_A), na.rm = TRUE)
sd(c(df$Age_M, df$Age_A), na.rm = TRUE)
min(c(df$Age_M, df$Age_A), na.rm = TRUE)
max(c(df$Age_M, df$Age_A), na.rm = TRUE)

# Demographische Daten - Geschlecht
table(df$Gender_M)
table(df$Gender_A)
table(c(df$Gender_M, df$Gender_A))

# Demographische Daten - Zivilstand
table(df$MaritalStatus_M)
table(df$MaritalStatus_A)
table(c(df$MaritalStatus_M, df$MaritalStatus_A))

# Demographische Daten - Nationalität
table(df$Nationality_M)
table(df$Nationality_A)
table(c(df$Nationality_M, df$Nationality_A))

# Demographische Daten - Höchster Bildungsabschluss
table(df$Education_M)
table(df$Education_A)
table(c(df$Education_M, df$Education_A))

# Demographische Daten - Beziehung innerhalb des Paares
table(df$Relationship_M)
table(df$Relationship_A)
table(c(df$Relationship_M, df$Relationship_A))

citation()
