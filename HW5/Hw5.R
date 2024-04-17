# Loading necessary library
library(pROC)
#QUESTION - 1
# Defining the data
data <- data.frame(
  Tuple = 1:10,
  Class = c("p", "n", "p", "p", "n", "p", "n", "n", "n", "p"),
  Prob = c(0.95, 0.85, 0.78, 0.66, 0.6, 0.55, 0.53, 0.52, 0.51, 0.4)
)

# Sorting data by probability
data <- data[order(-data$Prob), ]

# Initializing empty columns
data$TP <- 0
data$FN <- 0
data$FP <- 0
data$TN <- 0
data$TPR <- 0
data$FPR <- 0

# Calculating TP, FN, FP, TN, TPR, FPR for each threshold
for (i in 1:nrow(data)) {
  threshold <- data$Prob[i]
  data$TP[i] <- sum(data$Class == "p" & data$Prob >= threshold)
  data$FN[i] <- sum(data$Class == "p" & data$Prob < threshold)
  data$FP[i] <- sum(data$Class == "n" & data$Prob >= threshold)
  data$TN[i] <- sum(data$Class == "n" & data$Prob < threshold)
  data$TPR[i] <- data$TP[i] / (data$TP[i] + data$FN[i])
  data$FPR[i] <- data$FP[i] / (data$FP[i] + data$TN[i])
}

# Drawing ROC curve
roc_curve <- roc(data$Class, data$Prob)
plot(roc_curve, main = "ROC Curve", col = "blue")

# Filling in the table with calculated values
table_filled <- cbind(data[, c("Tuple", "Class", "Prob")], data[, c("TP", "FN", "FP", "TN", "TPR", "FPR")])
rownames(table_filled) <- NULL

# Printing the filled table
print(table_filled)

#QUESTION - 2
# Calculating minimum support
min_support <- ceiling(0.2 * length(transactions)) / length(transactions)

# Finding frequent itemsets with minimum support
frequent_itemsets <- eclat(trans, parameter = list(support = min_support))

# Generating association rules
rules <- apriori(trans, parameter = list(supp = min_support, conf = 0, target = "rules"))

# Calculating confidences for the rules
confidence_a <- interestMeasure(rules, measure = "confidence", subset = "lhs %in% c('A', 'B') & rhs == 'E'")
confidence_b <- interestMeasure(rules, measure = "confidence", subset = "lhs == 'A' & rhs %in% c('B', 'E')")

# Printing the confidences
print(confidence_a)
print(confidence_b)

