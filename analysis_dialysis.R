library(rjson)

#Read CSV dataset
results <- read.csv(file.choose())

#Adding Variable to distinguished chain owned for profits from chain
#Owned non profits and non chain owned for profits and non chain owned
#Non profits
results$classification <- ""

results$classification[which(results$Profit.or.Non.Profit == "Non-Profit" & results$Chain.Owned == "Yes")] <- "Chained Non-Profit"
results$classification[which(results$Profit.or.Non.Profit == "Non-Profit" & results$Chain.Owned == "No")] <- "Non-Chained Non-Profit"
results$classification[which(results$Profit.or.Non.Profit == "Profit" & results$Chain.Owned == "Yes")] <- "Chained Profit"
results$classification[which(results$Profit.or.Non.Profit == "Profit" & results$Chain.Owned == "No")] <- "Non-Chained Profit"

#Five Star Rating One Way Anova Test For-Profit v Non-Profit
mean(results$Five.Star[results$Profit.or.Non.Profit == "Non-Profit"], na.rm = TRUE)
mean(results$Five.Star[results$Profit.or.Non.Profit == "Profit"], na.rm = TRUE)
aov_fs_fp_np <- aov(results$Five.Star ~ results$Profit.or.Non.Profit)
summary(aov_fs_fp_np)

#Five Star Rating With State Normalization
quality_w_states <- lm(results$Five.Star ~ results$classification + results$Number.of.patients.included.in.survival.summary + results$State)
summary(quality_w_states)

#Five Star Rating One Way Anova Test Chain Owned v Non Chain Owned
mean(results$Five.Star[results$Chain.Owned == "Yes"], na.rm = TRUE)
mean(results$Five.Star[results$Chain.Owned == "No"], na.rm = TRUE)
aov_fs_co_nco <- aov(results$Five.Star ~ results$Chain.Owned)
summary(aov_fs_co_nco)

#Five Star Rating One Way Anova Test on Classification
mean(results$Five.Star[results$classification == "Chained Non-Profit"], na.rm = TRUE)
mean(results$Five.Star[results$classification == "Non-Chained Non-Profit"], na.rm = TRUE)
mean(results$Five.Star[results$classification == "Chained Profit"], na.rm = TRUE)
mean(results$Five.Star[results$classification == "Non-Chained Profit"], na.rm = TRUE)

aov_fs_cl <- aov(results$Five.Star ~ results$classification)
summary(aov_fs_cl)

#Box plot for Five Star Rating
boxplot(results$Five.Star ~ results$classification, main = "Boxplot for Service Quality")

#Number of Dialysis Stations One Way Anova Test on Classification
mean(results$X..of.Dialysis.Stations[results$classification == "Chained Non-Profit"], na.rm = TRUE)
mean(results$X..of.Dialysis.Stations[results$classification == "Non-Chained Non-Profit"], na.rm = TRUE)
mean(results$X..of.Dialysis.Stations[results$classification == "Chained Profit"], na.rm = TRUE)
mean(results$X..of.Dialysis.Stations[results$classification == "Non-Chained Profit"], na.rm = TRUE)

aov_nds_cl <- aov(results$X..of.Dialysis.Stations ~ results$classification)
summary(aov_nds_cl)

#Number of Dialysis Stations One Way Anova Test on Classification
mortality <- lm(results$Mortality.Rate..Facility. ~ results$classification + results$Number.of.patients.included.in.survival.summary)
summary(mortality)

#Normalize for states
mortality_w_states <- lm(results$Mortality.Rate..Facility. ~ results$classification + results$Number.of.patients.included.in.survival.summary + results$State)
summary(mortality_w_states)

#Mortality Rate Averages
mean(results$Mortality.Rate..Facility.[results$classification == "Chained Non-Profit"], na.rm = TRUE)
mean(results$Mortality.Rate..Facility.[results$classification == "Non-Chained Non-Profit"], na.rm = TRUE)
mean(results$Mortality.Rate..Facility.[results$classification == "Chained Profit"], na.rm = TRUE)
mean(results$Mortality.Rate..Facility.[results$classification == "Non-Chained Profit"], na.rm = TRUE)

#Box plot for Mortality
boxplot(results$Mortality.Rate..Facility. ~ results$classification, main = "Boxplot for Mortality Rates")

#Select JSON file
jsonresult <- fromJSON(file = file.choose())

#Change JSON file to Data Frame
jsonresult <- as.data.frame(jsonresult)

#Box Plot Quality of Care By Number of Competitors
boxplot(jsonresult$average_rating ~ jsonresult$number_companies, main = "Quality Rating By Number of Dialysis Centers")

#Box Plot Mortality Rate By Number of Companies
boxplot(jsonresult$average_mortality ~ jsonresult$number_companies, main = "Mortality Rate By Number of Dialysis Centers")

#Linear Regression
mortality_w_competition <- lm(jsonresult$average_mortality ~ jsonresult$number_companies)
summary(mortality_w_competition)
boxplot(results$Mortality.Rate..Facility. ~ results$classification, main="Boxplot for Mortality  Quality")