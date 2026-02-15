# Vorbereitung: Datensatz einlesen & Variablen berechnen & Packages laden
df <- read.table ("~/Desktop/DatenMasterarbeit.csv", header=TRUE, sep=",", na.strings="NA", dec=".")

df$irritation_difference <- df$Irritation_A - df$Irritation_M
df$irritation_quotient <- (df$Irritation_A + 1) / (df$Irritation_M + 1)
df$worry_difference <- df$Worry_A - df$Worry_M
df$worry_quotient <- (df$Worry_A + 1) / (df$Worry_M + 1)
df$inconvenience_difference <- df$Inconvenience_A - df$Inconvenience_M
df$inconvenience_quotient <- (df$Inconvenience_A + 1) / (df$Inconvenience_M + 1)
df$anger_difference <- df$Anger_A - df$Anger_M
df$anger_quotient <- (df$Anger_A + 1) / (df$Anger_M + 1)

library(ggplot2)
library(effectsize)

# (1) Fragestellung 1 - (1) Beschreibung - (1) Differenz
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

# (1) Fragestellung 1 - (1) Beschreibung - (2) Quotient
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

# (1) Fragestellung 1 - (2) Visualisierung - (1) Differenz Box-Plot
ggplot(df, aes(x = irritation_difference, y = "")) +
  geom_boxplot(staplewidth = 0.25, fill = "red", color = "black", na.rm = TRUE) +
  labs(title = "Difference in the Perception of Irritation", x = "Irritation Rating Difference", y = NULL) +
  coord_cartesian(xlim = c(-9, 9)) +
  scale_x_continuous(breaks = seq(-9, 9, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = worry_difference, y = "")) +
  geom_boxplot(staplewidth = 0.25, fill = "yellow", color = "black", na.rm = TRUE) +
  labs(title = "Difference in the Perception of Worry", x = "Worry Rating Difference", y = NULL) +
  coord_cartesian(xlim = c(-9, 9)) +
  scale_x_continuous(breaks = seq(-9, 9, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = inconvenience_difference, y = "")) +
  geom_boxplot(staplewidth = 0.25, fill = "green", color = "black", na.rm = TRUE) +
  labs(title = "Difference in the Perception of Inconvenience", x = "Inconvenience Rating Difference", y = NULL) +
  coord_cartesian(xlim = c(-9, 9)) +
  scale_x_continuous(breaks = seq(-9, 9, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = anger_difference, y = "")) +
  geom_boxplot(staplewidth = 0.25, fill = "turquoise", color = "black", na.rm = TRUE) +
  labs(title = "Difference in the Perception of Anger", x = "Anger Rating Difference", y = NULL) +
  coord_cartesian(xlim = c(-9, 9)) +
  scale_x_continuous(breaks = seq(-9, 9, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# (1) Fragestellung 1 - (2) Visualisierung - (2) Differenz Histogramm
ggplot(df, aes(x = irritation_difference)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black", na.rm = TRUE) +
  labs(title = "Difference in the Perception of Irritation", x = "Irritation Rating Difference", y = "Number of Pairs") +
  coord_cartesian(xlim = c(-9, 9), ylim = c(0, 16)) +
  scale_x_continuous(breaks = seq(-9, 9, by = 1)) +
  scale_y_continuous(breaks = seq(0, 16, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = worry_difference)) +
  geom_histogram(binwidth = 1, fill = "yellow", color = "black", na.rm = TRUE) +
  labs(title = "Difference in the Perception of Worry", x = "Worry Rating Difference", y = "Number of Pairs") +
  coord_cartesian(xlim = c(-9, 9), ylim = c(0, 16)) +
  scale_x_continuous(breaks = seq(-9, 9, by = 1)) +
  scale_y_continuous(breaks = seq(0, 16, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = inconvenience_difference)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", na.rm = TRUE) +
  labs(title = "Difference in the Perception of Inconvenience", x = "Inconvenience Rating Difference", y = "Number of Pairs") +
  coord_cartesian(xlim = c(-9, 9), ylim = c(0, 16)) +
  scale_x_continuous(breaks = seq(-9, 9, by = 1)) +
  scale_y_continuous(breaks = seq(0, 16, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = anger_difference)) +
  geom_histogram(binwidth = 1, fill = "turquoise", color = "black", na.rm = TRUE) +
  labs(title = "Difference in the Perception of Anger", x = "Anger Rating Difference", y = "Number of Pairs") +
  coord_cartesian(xlim = c(-9, 9), ylim = c(0, 16)) +
  scale_x_continuous(breaks = seq(-9, 9, by = 1)) +
  scale_y_continuous(breaks = seq(0, 16, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# (1) Fragestellung 1 - (2) Visualisierung - (3) Quotient Box-Plot
ggplot(df, aes(x = irritation_quotient, y = "")) +
  geom_boxplot(staplewidth = 0.25, fill = "red", color = "black", na.rm = TRUE) +
  labs(title = "Ratio of the Perception of Irritation", x = "Irritation Rating Quotient", y = NULL) +
  coord_cartesian(xlim = c(0, 10)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = worry_quotient, y = "")) +
  geom_boxplot(staplewidth = 0.25, fill = "yellow", color = "black", na.rm = TRUE) +
  labs(title = "Ratio of the Perception of Worry", x = "Worry Rating Quotient", y = NULL) +
  coord_cartesian(xlim = c(0, 10)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = inconvenience_quotient, y = "")) +
  geom_boxplot(staplewidth = 0.25, fill = "green", color = "black", na.rm = TRUE) +
  labs(title = "Ratio of the Perception of Inconvenience", x = "Inconvenience Rating Quotient", y = NULL) +
  coord_cartesian(xlim = c(0, 10)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = anger_quotient, y = "")) +
  geom_boxplot(staplewidth = 0.25, fill = "turquoise", color = "black", na.rm = TRUE) +
  labs(title = "Ratio of the Perception of Anger", x = "Anger Rating Quotient", y = NULL) +
  coord_cartesian(xlim = c(0, 10)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# (1) Fragestellung 1 - (2) Visualisierung - (4) Quotient Histogramm
ggplot(df, aes(x = irritation_quotient)) +
  geom_histogram(binwidth = 0.25, fill = "red", color = "black", na.rm = TRUE) +
  labs(title = "Ratio of the Perception of Irritation", x = "Irritation Rating Quotient", y = "Number of Pairs") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 11)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 11, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = worry_quotient)) +
  geom_histogram(binwidth = 0.25, fill = "yellow", color = "black", na.rm = TRUE) +
  labs(title = "Ratio of the Perception of Worry", x = "Worry Rating Quotient", y = "Number of Pairs") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 11)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 11, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = inconvenience_quotient)) +
  geom_histogram(binwidth = 0.25, fill = "green", color = "black", na.rm = TRUE) +
  labs(title = "Ratio of the Perception of Inconvenience", x = "Inconvenience Rating Quotient", y = "Number of Pairs") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 11)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 11, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = anger_quotient)) +
  geom_histogram(binwidth = 0.25, fill = "turquoise", color = "black", na.rm = TRUE) +
  labs(title = "Ratio of the Perception of Anger", x = "Anger Rating Quotient", y = "Number of Pairs") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 11)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 11, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# (1) Fragestellung 1 - (2) Visualisierung - (5) Scatterplot
ggplot(df, aes(x = Irritation_A, y = Irritation_M)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "black") +
  scale_size_area(max_size = 4, breaks = 1:4, name = "Number of Pairs") +
  geom_smooth(method = "lm", se = TRUE, color = "red", na.rm = TRUE) +
  labs(title = "Distribution of the Perception of Irritation", x = "Irritation Rating of Person A", y = "Irritation Rating of Person M") +
  coord_cartesian(xlim = c(0, 9), ylim = c(0, 9)) +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  scale_y_continuous(breaks = seq(0, 9, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = Worry_A, y = Worry_M)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "black") +
  scale_size_area(max_size = 5.6, breaks = c(1, 2, 4, 8), name = "Number of Pairs") +
  geom_smooth(method = "lm", se = TRUE, color = "yellow", na.rm = TRUE) +
  labs(title = "Distribution of the Perception of Worry", x = "Worry Rating of Person A", y = "Worry Rating of Person M") +
  coord_cartesian(xlim = c(0, 9), ylim = c(0, 9)) +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  scale_y_continuous(breaks = seq(0, 9, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = Inconvenience_A, y = Inconvenience_M)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "black") +
  scale_size_area(max_size = 3.5, breaks = 1:3, name = "Number of Pairs") +
  geom_smooth(method = "lm", se = TRUE, color = "green", na.rm = TRUE) +
  labs(title = "Distribution of the Perception of Inconvenience", x = "Inconvenience Rating of Person A", y = "Inconvenience Rating of Person M") +
  coord_cartesian(xlim = c(0, 9), ylim = c(0, 9)) +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  scale_y_continuous(breaks = seq(0, 9, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = Anger_A, y = Anger_M)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "black") +
  scale_size_area(max_size = 3.5, breaks = 1:3, name = "Number of Pairs") +
  geom_smooth(method = "lm", se = TRUE, color = "turquoise", na.rm = TRUE) +
  labs(title = "Distribution of the Perception of Anger", x = "Anger Rating of Person A", y = "Anger Rating of Person M") +
  coord_cartesian(xlim = c(0, 9), ylim = c(0, 9)) +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  scale_y_continuous(breaks = seq(0, 9, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# (1) Fragestellung 1 - (3) Quantifizierung - (1) Korrelation
cor.test(df$Irritation_A, df$Irritation_M, method = "pearson", use = "complete.obs")

cor.test(df$Worry_A, df$Worry_M, method = "pearson", use = "complete.obs")

cor.test(df$Inconvenience_A, df$Inconvenience_M, method = "pearson", use = "complete.obs")

cor.test(df$Anger_A, df$Anger_M, method = "pearson", use = "complete.obs")

# (1) Fragestellung 1 - (3) Quantifizierung - (2) t-Test für abhängige Stichproben
t.test(df$Irritation_A, df$Irritation_M, paired = TRUE)
cohens_d(df$Irritation_A, df$Irritation_M, paired = TRUE)

t.test(df$Worry_A, df$Worry_M, paired = TRUE)
cohens_d(df$Worry_A, df$Worry_M, paired = TRUE)

t.test(df$Inconvenience_A, df$Inconvenience_M, paired = TRUE)
cohens_d(df$Inconvenience_A, df$Inconvenience_M, paired = TRUE)

t.test(df$Anger_A, df$Anger_M, paired = TRUE)
cohens_d(df$Anger_A, df$Anger_M, paired = TRUE)

# (2) Fragestellung 2 - (1) Beschreibung - (1) DOCS Person M
mean(df$DOCSVerunreinigung_M, na.rm = TRUE)
sd(df$DOCSVerunreinigung_M, na.rm = TRUE)
min(df$DOCSVerunreinigung_M, na.rm = TRUE)
max(df$DOCSVerunreinigung_M, na.rm = TRUE)
shapiro.test(df$DOCSVerunreinigung_M)

mean(df$DOCSSchaden_M, na.rm = TRUE)
sd(df$DOCSSchaden_M, na.rm = TRUE)
min(df$DOCSSchaden_M, na.rm = TRUE)
max(df$DOCSSchaden_M, na.rm = TRUE)
shapiro.test(df$DOCSSchaden_M)

mean(df$DOCSGedanken_M, na.rm = TRUE)
sd(df$DOCSGedanken_M, na.rm = TRUE)
min(df$DOCSGedanken_M, na.rm = TRUE)
max(df$DOCSGedanken_M, na.rm = TRUE)
shapiro.test(df$DOCSGedanken_M)

mean(df$DOCSSymmetrie_M, na.rm = TRUE)
sd(df$DOCSSymmetrie_M, na.rm = TRUE)
min(df$DOCSSymmetrie_M, na.rm = TRUE)
max(df$DOCSSymmetrie_M, na.rm = TRUE)
shapiro.test(df$DOCSSymmetrie_M)

mean(df$DOCSGesamt_M, na.rm = TRUE)
sd(df$DOCSGesamt_M, na.rm = TRUE)
min(df$DOCSGesamt_M, na.rm = TRUE)
max(df$DOCSGesamt_M, na.rm = TRUE)
shapiro.test(df$DOCSGesamt_M)

# (2) Fragestellung 2 - (2) Visualisierung - (1) DOCS Boxplot
ggplot(df, aes(x = DOCSGesamt_M, y = "")) +
  geom_boxplot(staplewidth = 0.25, fill = "purple", color = "black", na.rm = TRUE) +
  labs(title = "DOCS", x = "Total DOCS Score of Person M", y = NULL) +
  coord_cartesian(xlim = c(0, 80)) +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# (2) Fragestellung 2 - (2) Visualisierung - (2) DOCS Histogramm
ggplot(df, aes(x = DOCSGesamt_M)) +
  geom_histogram(binwidth = 2.5, fill = "purple", color = "black", na.rm = TRUE) +
  labs(title = "DOCS", x = "Total DOCS Score of Person M", y = "Number of Participants") +
  coord_cartesian(xlim = c(0, 80), ylim = c(0, 5)) +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +
  scale_y_continuous(breaks = seq(0, 5, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# (2) Fragestellung 2 - (2) Visualisierung - (3) DOCS Scatterplot Differenz
ggplot(df, aes(x = DOCSGesamt_M, y = irritation_difference)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 2, breaks = 1:2, name = "Number of Pairs") +
  geom_smooth(method = "lm", se = TRUE, color = "red", na.rm = TRUE) +
  labs(title = "Distribution of DOCS & Irritation Difference", x = "Total DOCS Score of Person M", y = "Irritation Rating Difference") +
  coord_cartesian(xlim = c(0, 80), ylim = c(-9, 9)) +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +
  scale_y_continuous(breaks = seq(-9, 9, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = DOCSGesamt_M, y = worry_difference)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 2, breaks = 1:2, name = "Number of Pairs") +
  geom_smooth(method = "lm", se = TRUE, color = "yellow", na.rm = TRUE) +
  labs(title = "Distribution of DOCS & Worry Difference", x = "Total DOCS Score of Person M", y = "Worry Rating Difference") +
  coord_cartesian(xlim = c(0, 80), ylim = c(-9, 9)) +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +
  scale_y_continuous(breaks = seq(-9, 9, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = DOCSGesamt_M, y = inconvenience_difference)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 2, breaks = 1:2, name = "Number of Pairs") +
  geom_smooth(method = "lm", se = TRUE, color = "green", na.rm = TRUE) +
  labs(title = "Distribution of DOCS & Inconvenience Difference", x = "Total DOCS Score of Person M", y = "Inconvenience Rating Difference") +
  coord_cartesian(xlim = c(0, 80), ylim = c(-9, 9)) +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +
  scale_y_continuous(breaks = seq(-9, 9, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = DOCSGesamt_M, y = anger_difference)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 1.4, breaks = 1:1, name = "Number of Pairs") +
  geom_smooth(method = "lm", se = TRUE, color = "turquoise", na.rm = TRUE) +
  labs(title = "Distribution of DOCS & Anger Difference", x = "Total DOCS Score of Person M", y = "Anger Rating Difference") +
  coord_cartesian(xlim = c(0, 80), ylim = c(-9, 9)) +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +
  scale_y_continuous(breaks = seq(-9, 9, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# (2) Fragestellung 2 - (2) Visualisierung - (4) DOCS Scatterplot Quotient
ggplot(df, aes(x = DOCSGesamt_M, y = irritation_quotient)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 2, breaks = 1:2, name = "Number of Pairs") +
  geom_smooth(method = "lm", se = TRUE, color = "red", na.rm = TRUE) +
  labs(title = "Distribution of DOCS & Irritation Ratio", x = "Total DOCS Score of Person M", y = "Irritation Rating Quotient") +
  coord_cartesian(xlim = c(0, 80), ylim = c(0, 10)) +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = DOCSGesamt_M, y = worry_quotient)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 2, breaks = 1:2, name = "Number of Pairs") +
  geom_smooth(method = "lm", se = TRUE, color = "yellow", na.rm = TRUE) +
  labs(title = "Distribution of DOCS & Worry Ratio", x = "Total DOCS Score of Person M", y = "Worry Rating Quotient") +
  coord_cartesian(xlim = c(0, 80), ylim = c(0, 10)) +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = DOCSGesamt_M, y = inconvenience_quotient)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 2, breaks = 1:2, name = "Number of Pairs") +
  geom_smooth(method = "lm", se = TRUE, color = "green", na.rm = TRUE) +
  labs(title = "Distribution of DOCS & Inconvenience Ratio", x = "Total DOCS Score of Person M", y = "Inconvenience Rating Quotient") +
  coord_cartesian(xlim = c(0, 80), ylim = c(0, 10)) +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = DOCSGesamt_M, y = anger_quotient)) +
  geom_point(na.rm = TRUE) +
  geom_count(na.rm = TRUE, color = "purple") +
  scale_size_area(max_size = 1.4, breaks = 1:1, name = "Number of Pairs") +
  geom_smooth(method = "lm", se = TRUE, color = "turquoise", na.rm = TRUE) +
  labs(title = "Distribution of DOCS & Anger Ratio", x = "Total DOCS Score of Person M", y = "Anger Rating Quotient") +
  coord_cartesian(xlim = c(0, 80), ylim = c(0, 10)) +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# (2) Fragestellung 2 - (3) Quantifizierung - (1) DOCS Korrelation / Regression Differenz
cor.test(df$DOCSGesamt_M, df$irritation_difference, method = "pearson", use = "complete.obs")
summary(lm(df$irritation_difference ~ df$DOCSGesamt_M))

cor.test(df$DOCSGesamt_M, df$worry_difference, method = "pearson", use = "complete.obs")
summary(lm(df$worry_difference ~ df$DOCSGesamt_M))

cor.test(df$DOCSGesamt_M, df$inconvenience_difference, method = "pearson", use = "complete.obs")
summary(lm(df$inconvenience_difference ~ df$DOCSGesamt_M))

cor.test(df$DOCSGesamt_M, df$anger_difference, method = "pearson", use = "complete.obs")
summary(lm(df$anger_difference ~ df$DOCSGesamt_M))

# (2) Fragestellung 2 - (3) Quantifizierung - (2) DOCS Korrelation / Regression Quotient
cor.test(df$DOCSGesamt_M, df$irritation_quotient, method = "pearson", use = "complete.obs")
summary(lm(df$irritation_quotient ~ df$DOCSGesamt_M))

cor.test(df$DOCSGesamt_M, df$worry_quotient, method = "pearson", use = "complete.obs")
summary(lm(df$worry_quotient ~ df$DOCSGesamt_M))

cor.test(df$DOCSGesamt_M, df$inconvenience_quotient, method = "pearson", use = "complete.obs")
summary(lm(df$inconvenience_quotient ~ df$DOCSGesamt_M))

cor.test(df$DOCSGesamt_M, df$anger_quotient, method = "pearson", use = "complete.obs")
summary(lm(df$anger_quotient ~ df$DOCSGesamt_M))

# Weitere Daten - Anzahl Probanden / Paare
length(na.omit(df$ID))

# Weitere Daten - Anzahl Probanden / Paare pro Analyse
length(na.omit(df$irritation_difference))
length(na.omit(df$worry_difference))
length(na.omit(df$inconvenience_difference))
length(na.omit(df$anger_difference))
length(na.omit(df$DOCSGesamt_M))
length(na.omit(df$DOCSGesamt_A))
sum(complete.cases(df$irritation_difference, df$DOCSGesamt_M))
sum(complete.cases(df$worry_difference, df$DOCSGesamt_M))
sum(complete.cases(df$inconvenience_difference, df$DOCSGesamt_M))
sum(complete.cases(df$anger_difference, df$DOCSGesamt_M))

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

# Demographische Daten - Höchster Bildungsstand
table(df$Education_M)
table(df$Education_A)
table(c(df$Education_M, df$Education_A))

# Demographische Daten - Beziehung innerhalb des Paares
table(df$Relationship_M)
table(df$Relationship_A)
table(c(df$Relationship_M, df$Relationship_A))

# Weitere Daten - DOCS von Person A
mean(df$DOCSGesamt_A, na.rm = TRUE)
sd(df$DOCSGesamt_A, na.rm = TRUE)
min(df$DOCSGesamt_A, na.rm = TRUE)
max(df$DOCSGesamt_A, na.rm = TRUE)
shapiro.test(df$DOCSGesamt_M)
