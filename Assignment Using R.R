#1.

# Load Libraries
library(dplyr)
library(ggplot2)
library(readr)

# Load Dataset
lung <- read.csv("C://Users//Lenovo//Desktop//Education//41//Siddikursir//lung_disease.csv")


# Basic EDA Questions

# i) Distribution of Age
summary(lung$Age)

ggplot(lung, aes(x = Age)) +
  geom_histogram(fill="skyblue", bins=20) +
  ggtitle("Distribution of Age")

# Check skewness visually
# If histogram tail extends more to one side -> skewed

# ii) Smokers vs Non-smokers
table(lung$Smoking)

prop.table(table(lung$Smoking))*100

# iii) Income group frequency
table(lung$Income)

# iv) Percentage exposed to high pollution
high_pollution <- sum(lung$Pollution == "High")
total <- nrow(lung)

(high_pollution/total)*100

# v) Overall prevalence of lung disease
prop.table(table(lung$LungDisease))*100


# Association & Crosstab


# i) Smoking vs Lung Disease
table_smoke <- table(lung$Smoking, lung$LungDisease)

table_smoke

# ii) Contingency Table Interpretation
prop.table(table_smoke, margin=1)

# iii) Pollution vs Lung Disease
table_pollution <- table(lung$Pollution, lung$LungDisease)

table_pollution

# iv) Lung disease across income groups
table_income <- table(lung$Income, lung$LungDisease)

prop.table(table_income, margin=1)

# v) Strongest relationship
# Compare proportions visually
# Statistical Testing

# i) Chi-square Test
# Smoking vs Lung Disease
chisq.test(table_smoke)

# Pollution vs Lung Disease
chisq.test(table_pollution)

# ii) Fisher Exact Test
# Use when expected cell frequency < 5

fisher.test(table_smoke)

# iii) Odds Ratio

library(epitools)

oddsratio(table_smoke)


# Correlation & Interpretation
# Convert categorical variables to numeric
lung$Smoking_num <- ifelse(lung$Smoking=="Yes",1,0)
lung$Disease_num <- ifelse(lung$LungDisease=="Yes",1,0)

# Smoking and Lung Disease
cor(lung$Smoking_num, lung$Disease_num)

# Age and Lung Disease
cor(lung$Age, lung$Disease_num)


# Logistic Regression
# Convert variables into factors
lung$Smoking <- as.factor(lung$Smoking)
lung$Pollution <- as.factor(lung$Pollution)
lung$Income <- as.factor(lung$Income)
lung$LungDisease <- as.factor(lung$LungDisease)

# Logistic Regression Model
model1 <- glm(LungDisease ~ Smoking + Age + Pollution + Income,
              data = lung,
              family = binomial)

summary(model1)

# Odds Ratios
exp(coef(model1))

# Confidence Intervals
exp(confint(model1))

# Interaction Model
model2 <- glm(LungDisease ~ Smoking*Pollution + Age + Income,
              data = lung,
              family = binomial)

summary(model2)

# Odds Ratios
exp(coef(model2))


# Visualization
# Smoking vs Lung Disease
ggplot(lung, aes(x=Smoking, fill=LungDisease)) +
  geom_bar(position="fill") +
  ylab("Proportion") +
  ggtitle("Smoking and Lung Disease")

# Pollution vs Lung Disease
ggplot(lung, aes(x=Pollution, fill=LungDisease)) +
  geom_bar(position="fill") +
  ylab("Proportion") +
  ggtitle("Pollution and Lung Disease")

############################################################
##2.

# Load Libraries
library(dplyr)
library(ggplot2)
library(car)

# Generate Data
set.seed(123)
# Assign 30 people to each program
Program <- rep(c("A", "B", "C"), each = 30)

# Simulated weight loss data
WeightLoss <- c(
  rnorm(30, mean = 5, sd = 1.5),   # Program A
  rnorm(30, mean = 7, sd = 1.8),   # Program B
  rnorm(30, mean = 9, sd = 2.0)    # Program C
)

# Create dataset
exercise_data <- data.frame(Program, WeightLoss)

# View first rows
head(exercise_data)

#Exploratory Data Analysis
# Summary statistics
exercise_data %>%
  group_by(Program) %>%
  summarise(
    Mean = mean(WeightLoss),
    SD = sd(WeightLoss),
    Min = min(WeightLoss),
    Max = max(WeightLoss)
  )

# Boxplot
ggplot(exercise_data, aes(x = Program, y = WeightLoss, fill = Program)) +
  geom_boxplot() +
  ggtitle("Weight Loss by Exercise Program") +
  theme_minimal()

#One-Way ANOVA

anova_model <- aov(WeightLoss ~ Program, data = exercise_data)

summary(anova_model)

# Step 4: Check ANOVA Assumptions

# Normality of residuals

# Histogram
hist(residuals(anova_model),
     main = "Histogram of Residuals",
     col = "lightblue")

# QQ Plot
qqnorm(residuals(anova_model))
qqline(residuals(anova_model), col = "red")

# Shapiro-Wilk Test
shapiro.test(residuals(anova_model))


#Homogeneity of Variance

leveneTest(WeightLoss ~ Program, data = exercise_data)


# Tukey HSD Test
TukeyHSD(anova_model)

#Visualization of Means

ggplot(exercise_data,
       aes(x = Program, y = WeightLoss, fill = Program)) +
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  ggtitle("Mean Weight Loss with Standard Error") +
  ylab("Weight Loss (Pounds)") +
  theme_minimal()




####################################################
#3.

# Load Libraries
library(dplyr)
library(ggplot2)
library(vcd)

# Import dataset
anemia <- read.csv("C://Users//Lenovo//Desktop//Education//41//Siddikursir//children anemia.csv")

# View structure
str(anemia)
head(anemia)

# educational.level vs Child Anemia Level

table(anemia$Highest.educational.level)
table(anemia$Anemia.level)

# Create Contingency Table
cont_table <- table(anemia$Highest.educational.level,
                    anemia$Anemia.level)

cont_table

# Row percentages
prop.table(cont_table, margin = 1)*100

# =========================================================
# Step 4: Hypothesis Formulation
# =========================================================

# Null Hypothesis (H0):
# There is NO association between mother's education
# and child anemia level.

# Alternative Hypothesis (H1):
# There IS an association between mother's education
# and child anemia level.

# Step 5: Chi-Square Test

chi_result <- chisq.test(cont_table)

chi_result

# Expected frequencies
chi_result$expected


###############################################################
#4.


# Create Contingency Table

drug_table <- matrix(c(
  40, 10,
  10, 40,
  25, 25
),
nrow = 3,
byrow = TRUE)

rownames(drug_table) <- c("Drug_A", "Drug_B", "Drug_C")
colnames(drug_table) <- c("No_Disease", "Disease")

# Display Table
drug_table


# H0: Drug treatment and disease outcome are independent.
# H1: Drug treatment and disease outcome are associated.

# Fisher's Exact Test

fisher_result <- fisher.test(drug_table)

fisher_result
# If p-value < 0.05:
# Reject H0
# This means there is a statistically significant
# association between drug treatment and disease outcome.

# Post-Hoc Pairwise Fisher Tests

# Drug A vs Drug B
table_AB <- drug_table[c(1,2), ]

fisher.test(table_AB)


# Drug A vs Drug C
table_AC <- drug_table[c(1,3), ]

fisher.test(table_AC)


# Drug B vs Drug C
table_BC <- drug_table[c(2,3), ]

fisher.test(table_BC)

# Bonferroni Correction

# Adjust alpha level
alpha <- 0.05/3

alpha

# Compare each pairwise p-value with adjusted alpha

# Contribution Plot

library(vcd)

assoc(drug_table,
      shade = TRUE,
      main = "Association Between Drug Treatment and Disease")

# Final Interpretation
# Drug A has more patients without disease,
# suggesting better treatment effectiveness.

# Drug B has more patients with disease,
# suggesting lower effectiveness.

# Drug C shows balanced outcomes.

# Significant Fisher test results indicate that
# disease outcome depends on the type of drug treatment.

##############################################################
#5.
# Load Libraries
library(dplyr)
library(ggplot2)

# Load Dataset

# Import data from URL
data <- read.csv(
  "https://stats.idre.ucla.edu/stat/data/binary.csv"
)

head(data)

# Structure of dataset
str(data)

# Convert rank into factor variable
data$rank <- as.factor(data$rank)

# Summary statistics
summary(data)

#Fit Logistic Regression Model

model <- glm(admit ~ gre + gpa + rank,
             data = data,
             family = binomial)

# Model summary
summary(model)

# Calculate Odds Ratios
exp(coef(model))

# Confidence Intervals
exp(confint(model))

# Predicted probability of admission
data$predicted_prob <- predict(model,
                               type = "response")

head(data)

# Visualization
# GRE vs Probability of Admission
ggplot(data,
       aes(x = gre,
           y = predicted_prob)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  ggtitle("GRE Score vs Probability of Admission") +
  ylab("Predicted Probability") +
  theme_minimal()

# GPA vs Probability of Admission
ggplot(data,
       aes(x = gpa,
           y = predicted_prob)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  ggtitle("GPA vs Probability of Admission") +
  ylab("Predicted Probability") +
  theme_minimal()

###############################################################
#6.
# Load Libraries
library(dplyr)
library(ggplot2)

# Import dataset
data <- read.csv(
  "https://stats.idre.ucla.edu/stat/data/poisson_sim.csv"
)

head(data)

# Structure of dataset
str(data)

# Convert program into factor variable
data$prog <- factor(data$prog,
                    levels = c(1,2,3),
                    labels = c("General",
                               "Academic",
                               "Vocational"))

# Summary statistics
summary(data)

# Distribution of awards
table(data$num_awards)

# Mean awards by program
data %>%
  group_by(prog) %>%
  summarise(
    Mean_Awards = mean(num_awards),
    Mean_Math = mean(math)
  )

# Awards by Program
ggplot(data,
       aes(x = prog,
           y = num_awards,
           fill = prog)) +
  geom_boxplot() +
  ggtitle("Number of Awards by Program Type") +
  theme_minimal()

# Math Score vs Awards
ggplot(data,
       aes(x = math,
           y = num_awards)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Math Score vs Number of Awards") +
  theme_minimal()


poisson_model <- glm(num_awards ~ prog + math,
                     family = poisson(link = "log"),
                     data = data)

# Model summary
summary(poisson_model)


# Exponentiate coefficients
exp(coef(poisson_model))

# Confidence Intervals
exp(confint(poisson_model))



# Predicted number of awards
data$predicted_awards <- predict(poisson_model,
                                 type = "response")

head(data)

# Residual Deviance
poisson_model$deviance

# Degrees of Freedom
poisson_model$df.residual

# Check Overdispersion
dispersion <- poisson_model$deviance /
  poisson_model$df.residual

dispersion

# If dispersion > 1.5:
# Overdispersion may exist.

ggplot(data,
       aes(x = math,
           y = predicted_awards,
           color = prog)) +
  geom_line() +
  ggtitle("Predicted Number of Awards") +
  ylab("Predicted Awards") +
  theme_minimal()

#############################################
#7.

# Load Libraries

library(MASS)
library(dplyr)
library(ggplot2)
library(haven)

# Import .dta dataset
data <- read_dta(
  "https://stats.idre.ucla.edu/stat/data/nb_data.dta"
)

# View first rows
head(data)

# Structure
str(data)

# Convert program into factor
data$prog <- factor(data$prog,
                    levels = c(1,2,3),
                    labels = c("General",
                               "Academic",
                               "Vocational"))

# Summary statistics
summary(data)


# Mean absences by program
data %>%
  group_by(prog) %>%
  summarise(
    Mean_Absence = mean(daysabs),
    SD_Absence = sd(daysabs),
    Mean_Math = mean(math)
  )

# Boxplot of absences by program
ggplot(data,
       aes(x = prog,
           y = daysabs,
           fill = prog)) +
  geom_boxplot() +
  ggtitle("Days Absent by Program Type") +
  ylab("Days Absent") +
  theme_minimal()

# Scatter plot
ggplot(data,
       aes(x = math,
           y = daysabs)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Math Score vs Days Absent") +
  theme_minimal()

mean(data$daysabs)

var(data$daysabs)

# If variance > mean,
# overdispersion exists,
# making Negative Binomial appropriate.


poisson_model <- glm(daysabs ~ math + prog,
                     family = poisson,
                     data = data)

summary(poisson_model)

nb_model <- glm.nb(daysabs ~ math + prog,
                   data = data)

summary(nb_model)


exp(coef(nb_model))

# Confidence Intervals
exp(confint(nb_model))


data$predicted_absence <- predict(nb_model,
                                  type = "response")

head(data)


ggplot(data,
       aes(x = math,
           y = predicted_absence,
           color = prog)) +
  geom_line() +
  ggtitle("Predicted Days Absent") +
  ylab("Predicted Absences") +
  theme_minimal()


AIC(poisson_model)

AIC(nb_model)



##############################################
#8.

# Load Libraries
library(pscl)
library(dplyr)
library(ggplot2)

# Import dataset
fish <- read.csv(
  "https://stats.idre.ucla.edu/stat/data/fish.csv"
)

# View first rows
head(fish)

# Structure of dataset
str(fish)


# Summary statistics
summary(fish)

# Frequency of fish counts
table(fish$count)

# Proportion of zero counts
mean(fish$count == 0)

# Histogram of fish counts
ggplot(fish,
       aes(x = count)) +
  geom_histogram(binwidth = 1,
                 fill = "skyblue",
                 color = "black") +
  ggtitle("Distribution of Fish Counts") +
  theme_minimal()

# Number of zeros
sum(fish$count == 0)

poisson_model <- glm(count ~ child + persons +
                       camper + livebait,
                     family = poisson,
                     data = fish)

summary(poisson_model)


zip_model <- zeroinfl(
  count ~ child + persons +
    camper + livebait |
    child + persons,
  data = fish,
  dist = "poisson"
)

summary(zip_model)


AIC(poisson_model)

AIC(zip_model)


exp(coef(zip_model))


# Predicted Fish Counts

fish$predicted_count <- predict(zip_model,
                                type = "response")

head(fish)


##################################################
#9.

library(pscl)
library(MASS)
library(dplyr)
library(ggplot2)

# Load Dataset


fish <- read.csv(
  "https://stats.idre.ucla.edu/stat/data/fish.csv"
)

# View first rows
head(fish)

# Structure
str(fish)

summary(fish)

# Frequency table
table(fish$count)

# Proportion of zeros
mean(fish$count == 0)

# Histogram of fish counts
ggplot(fish,
       aes(x = count)) +
  geom_histogram(binwidth = 1,
                 fill = "skyblue",
                 color = "black") +
  ggtitle("Distribution of Fish Counts") +
  theme_minimal()


mean(fish$count)

var(fish$count)

# If variance > mean,
# overdispersion exists.


poisson_model <- glm(
  count ~ child + persons +
    camper + livebait,
  family = poisson,
  data = fish
)

summary(poisson_model)

#Fit Negative Binomial Model


nb_model <- glm.nb(
  count ~ child + persons +
    camper + livebait,
  data = fish
)

summary(nb_model)
#Fit Zero-Inflated Negative Binomial Model


zinb_model <- zeroinfl(
  count ~ child + persons +
    camper + livebait |
    child + persons,
  dist = "negbin",
  data = fish
)

summary(zinb_model)

AIC(poisson_model)

AIC(nb_model)

AIC(zinb_model)

# Lower AIC indicates better model fit.
#Incidence Rate Ratios (IRR)


exp(coef(zinb_model))


##########################################
#10.
# Load Libraries
library(pscl)
library(dplyr)
library(ggplot2)
library(haven)

# Import .dta dataset
data <- read_dta(
  "https://stats.idre.ucla.edu/stat/data/ztp.dta"
)

# View first rows
head(data)

# Structure of dataset
str(data)

summary(data)

# Check minimum stay
min(data$stay)

# Frequency table
table(data$stay)


# Distribution of hospital stay
ggplot(data,
       aes(x = stay)) +
  geom_histogram(binwidth = 1,
                 fill = "skyblue",
                 color = "black") +
  ggtitle("Distribution of Hospital Stay") +
  theme_minimal()

# Age vs Stay
ggplot(data,
       aes(x = age,
           y = stay)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Age vs Length of Hospital Stay") +
  theme_minimal()

data$hmo <- factor(data$hmo)
data$died <- factor(data$died)

#Fit Standard Poisson Model


poisson_model <- glm(
  stay ~ age + hmo + died,
  family = poisson,
  data = data
)

summary(poisson_model)

# Fit Zero-Truncated Poisson Model

ztp_model <- zerotrunc(
  stay ~ age + hmo + died,
  dist = "poisson",
  data = data
)

summary(ztp_model)


AIC(poisson_model)

AIC(ztp_model)

exp(coef(ztp_model))

# Confidence Intervals
exp(confint(ztp_model))

# Predicted Length of Stay

data$predicted_stay <- predict(
  ztp_model,
  type = "response"
)

head(data)

# Visualization of Predictions

ggplot(data,
       aes(x = age,
           y = predicted_stay,
           color = died)) +
  geom_line() +
  ggtitle("Predicted Hospital Stay by Age") +
  ylab("Predicted Length of Stay") +
  theme_minimal()

# Residual Deviance
ztp_model$deviance


##################################################
#11.


# Load Libraries
library(pscl)
library(MASS)
library(dplyr)
library(ggplot2)
library(haven)


# Import .dta dataset
data <- read_dta(
  "https://stats.idre.ucla.edu/stat/data/ztp.dta"
)

# View first rows
head(data)

# Structure of dataset
str(data)

summary(data)

# Minimum stay
min(data$stay)

# Mean and variance
mean(data$stay)

var(data$stay)

# If variance > mean,
# overdispersion exists.

# Visualization
# Histogram of stay length
ggplot(data,
       aes(x = stay)) +
  geom_histogram(binwidth = 1,
                 fill = "skyblue",
                 color = "black") +
  ggtitle("Distribution of Hospital Stay") +
  theme_minimal()

# Age vs Stay
ggplot(data,
       aes(x = age,
           y = stay)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  ggtitle("Age vs Hospital Stay") +
  theme_minimal()

data$hmo <- factor(data$hmo)
data$died <- factor(data$died)

#Fit Standard Poisson Model

poisson_model <- glm(
  stay ~ age + hmo + died,
  family = poisson,
  data = data
)

summary(poisson_model)

# Fit Negative Binomial Model

nb_model <- glm.nb(
  stay ~ age + hmo + died,
  data = data
)

summary(nb_model)
# Fit Zero-Truncated Poisson Model

ztp_model <- zerotrunc(
  stay ~ age + hmo + died,
  dist = "poisson",
  data = data
)

summary(ztp_model)


ztnb_model <- zerotrunc(
  stay ~ age + hmo + died,
  dist = "negbin",
  data = data
)

summary(ztnb_model)


AIC(poisson_model)

AIC(nb_model)

AIC(ztp_model)

AIC(ztnb_model)

# Lower AIC indicates better fit.

# Incidence Rate Ratios (IRR)


exp(coef(ztnb_model))

# Confidence Intervals
exp(confint(ztnb_model))

#Predicted Hospital Stay


data$predicted_stay <- predict(
  ztnb_model,
  type = "response"
)

head(data)

#Visualization of Predictions

ggplot(data,
       aes(x = age,
           y = predicted_stay,
           color = died)) +
  geom_line() +
  ggtitle("Predicted Hospital Stay by Age") +
  ylab("Predicted Stay Length") +
  theme_minimal()

