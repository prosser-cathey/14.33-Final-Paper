setwd("~/Desktop/R Lesson")

# Load Data
data <- read.csv("14.33 Final Project.csv")
# Naming Columns
data$Explanation <- data$X.2
data$Answer <- data$X.3
data$Game <- data$Traveler.s.Dilemma.P.0.H..1.L...02
data$Screen <- data$no
data$id <- data$X5f52b10e0193bb5acbea65f3
data <- data[,c("Explanation", "Answer", "Game", "Screen", "id")]
# Getting rid of those who failed the screening questions or who I'm missing data for
data <- data[grepl("yes", data$Screen)==TRUE,]
data <- data[is.na(data$Answer)==FALSE,]
# Getting lower bound number out of string
data$Two <- 2*as.numeric(grepl("\\.02", data$Game)==TRUE)
data$TwoFive <- 25*as.numeric(grepl("\\.25", data$Game)==TRUE)
data$Five <- 50*as.numeric(grepl("\\.5", data$Game)==TRUE)
data$Nine <- 90*as.numeric(grepl("\\.9", data$Game)==TRUE)
data$Lower <- data$Two + data$TwoFive+data$Five+data$Nine
# Creating Diff Variable
data$Diff <- 100 - data$Lower
# Coding Cooperation
data$Answer <- as.numeric(data$Answer)
data$Cooperation <- as.numeric(data$Answer==100)
# Running Regression
lout <- lm(Cooperation ~ Diff, data = data)
summary(lout)
# Outputting to Latex
library("stargazer")
stargazer(lout)
library("stats")
confint(lout, "Diff", .95)
# Getting sample sizes
dim(data)
dim(data[data$Lower==2,])
dim(data[data$Lower==25,])
dim(data[data$Lower==50,])
dim(data[data$Lower==90,])
# Figure
library("ggplot2")
aggregate(data$Cooperation, by = list(Difference = data$Diff), mean)
ggplot(aggregate(data$Cooperation, by = list(Difference = data$Diff), mean), aes(x=Difference,y=x)) + 
  geom_point() + 
  theme_classic() +
  xlab("Difference in Bounds") +
  ylab("Cooperation Percentage") +
  ggtitle("Cooperation vs. Difference in Bounds") +
  scale_y_continuous( limits=c(0, 1))
# Total Distribution of Submissions for each group
ggplot(data = data, aes(x=Answer)) + 
  geom_histogram(bins = 100) + 
  theme_classic() +
  xlab("Submission") +
  ylab("Count of Submissions") +
  ggtitle("Distribution of Submissions by Difference in Bounds") +
  facet_wrap(vars(Diff))
# Running regression only on those who paid attention
# Adding number of words column
data$Words <- sapply(gregexpr("\\S+", data$Explanation), length)
# Regression
lou2 <- lm(Cooperation ~ Diff, data = data[data$Words>5,])
lout3 <- lm(Cooperation ~ Diff, data = data[data$Words>20,])
# Outputting to Latex
stargazer(lout, lou2, lout3, column.labels = c("All", "5 Words", "20 Words"), notes = c("This table presents the results of \\regressing cooperation on the difference in \\bounds. Note that there seems to be no effect, \\even among groups who paid close attention."))
summary(lout2)
