dispatch_data <- read.csv("GitHub/bpmn-quality-statistics/camunda-statistics/dispatch.csv", header = TRUE)
recourse_data <- read.csv("GitHub/bpmn-quality-statistics/camunda-statistics/recourse.csv", header = TRUE)
scoring_data <- read.csv("GitHub/bpmn-quality-statistics/camunda-statistics/scoring.csv", header = TRUE)
restaurant_data <- read.csv("GitHub/bpmn-quality-statistics/camunda-statistics/restaurant.csv", header = TRUE)

dispatch_data["domain"] <- "dispatch"
recourse_data["domain"] <- "recourse"
scoring_data["domain"] <- "scoring"
restaurant_data["domain"] <- "restaurant"

data <- rbind(dispatch_data, recourse_data, scoring_data, restaurant_data)

failed_R1 <- nrow(data[data$d.R1 == 0,])
failed_R2 <- nrow(data[data$d.R2 == 0,])
failed_R3 <- nrow(data[data$d.R3 == 0,])
failed_R4 <- nrow(data[data$d.R4 == 0,])
failed_R5 <- nrow(data[data$d.R5 == 0,])

# Rules conformance (discrete)
bar_y <- c(failed_R1, failed_R2, failed_R3, failed_R4, failed_R5)
bar_x <- c("R1", "R2", "R3", "R4", "R5")

barplot <- barplot(bar_y, names.arg = bar_x, xlab = "Rule", ylab = "Models", 
        main="Discrete Criteria")
text(x = barplot, y = bar_y, label = bar_y, pos = 1, cex = 0.8)
text(x = barplot, y = bar_y, label = bar_y, pos = 3, cex = 0.8)

# Rules conformance (continuous)
boxplot_r1 <- data.frame(data["c.R1"])
boxplot_r1["RX"] <- "R1"

boxplot_r2 <- data.frame(data["c.R2"])
boxplot_r2["RX"] <- "R2"

boxplot_r3 <- data.frame(data["c.R3"])
boxplot_r3["RX"] <- "R3"

boxplot_r4 <- data.frame(data["c.R4"])
boxplot_r4["RX"] <- "R4"

boxplot_r5 <- data.frame(data["c.R5"])
boxplot_r5["RX"] <- "R5"

colnames(boxplot_r1)[1] <- "RY"
colnames(boxplot_r2)[1] <- "RY"
colnames(boxplot_r3)[1] <- "RY"
colnames(boxplot_r4)[1] <- "RY"
colnames(boxplot_r5)[1] <- "RY"

boxplot_r <- rbind(boxplot_r1, boxplot_r2, boxplot_r3, boxplot_r4, boxplot_r5)

boxplot <- boxplot(RY ~ RX, data = boxplot_r, xlab = "Rule", ylab = "Conformance", 
        main = "Continuous Criteria")

# Quality measures
boxplot_dwsm <- data.frame(data["d.wsm"])
boxplot_dwsm["QX"] <- "Discr. WSM"

boxplot_cwsm <- data.frame(data["c.wsm"])
boxplot_cwsm["QX"] <- "Cont. WSM"

boxplot_dmin <- data.frame(data["d.min"])
boxplot_dmin["QX"] <- "Discr. Pessim."

boxplot_cmin <- data.frame(data["c.min"])
boxplot_cmin["QX"] <- "Cont. Pessim."

colnames(boxplot_dwsm)[1] <- "QY"
colnames(boxplot_cwsm)[1] <- "QY"
colnames(boxplot_dmin)[1] <- "QY"
colnames(boxplot_cmin)[1] <- "QY"

boxplot_q <- rbind(boxplot_dwsm, boxplot_cwsm, boxplot_dmin, boxplot_cmin)

boxplot <- boxplot(QY ~ QX, data = boxplot_q, xlab = "Aggregate", ylab = "PMQ", 
                   main = "Quality Measures")

# Linguistic quality measures

colors <- c("green", "yellow", "orange", "red", "brown")
groups <- c("Discr. WSM", "Cont. WSM", "Discr. Pessim.", "Cont. Pessim.")
levels <- c("Good", "Well", "Satisfied", "Poor", "Bad")

values <- matrix(c(nrow(data[data$d.wsml == "Good",]), nrow(data[data$c.wsml == "Good",]), 
                   nrow(data[data$d.minl == "Good",]), nrow(data[data$c.minl == "Good",]),
                   
                   nrow(data[data$d.wsml == "Well",]), nrow(data[data$c.wsml == "Well",]), 
                   nrow(data[data$d.minl == "Well",]), nrow(data[data$c.minl == "Well",]),
                   
                   nrow(data[data$d.wsml == "Satisfied",]), nrow(data[data$c.wsml == "Satisfied",]), 
                   nrow(data[data$d.minl == "Satisfied",]), nrow(data[data$c.minl == "Satisfied",]),
                   
                   nrow(data[data$d.wsml == "Poor",]), nrow(data[data$c.wsml == "Poor",]), 
                   nrow(data[data$d.minl == "Poor",]), nrow(data[data$c.minl == "Poor",]),
                   
                   nrow(data[data$d.wsml == "Bad",]), nrow(data[data$c.wsml == "Bad",]), 
                   nrow(data[data$d.minl == "Bad",]), nrow(data[data$c.minl == "Bad",])), 
                 
                 nrow = 5, ncol = 4, byrow = TRUE)

barplot_ql <- barplot(values, main = "Linguistic Quality Measures", 
                      names.arg = groups, xlab = "Aggregate", ylab = "Models", col = colors,
                      beside = T, width=c(0.5))
text(x = barplot_ql, y = values, label = values, pos = 3, cex = 0.8)
legend("topright", legend = levels, fill = colors)
