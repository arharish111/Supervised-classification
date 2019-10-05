#Student Name : Harish Harish
# Team number : 28
# Seed value: 40
# Sample size: 2k
# Split ratio: 70/30

library(arules)
library(data.tree)
library(caret)
library(e1071)

census.df <- read.table("dataset\\census_data_original.txt",header = F,sep = ",")
for(c in colnames(census.df))
  census.df <- census.df[census.df[[c]]!="?",]

set.seed(40)
census.sample <- census.df[sample(nrow(census.df),replace = F,size = 2000),]


min.fnl <- min(census.sample$V3)
max.fnl <- max(census.sample$V3)

for(i in 1:nrow(census.sample))
{
  val <- (census.sample[i,3]-min.fnl)/(max.fnl-min.fnl)
  census.sample[i,3] <- round(val,3)
}

census.sample <- census.sample[,-5]


# census.processed <- data.frame("Age" = census.sample$V1,
#                                "Work_class" = census.sample$V2,
#                                "Fnl" = census.sample$V3,
#                                "Education" = census.sample$V4,
#                                "Edu_num" = as.factor(census.sample$V5),
#                                "Marital_status" = census.sample$V6,
#                                "Occupation" = census.sample$V7,
#                                "Relationship" = census.sample$V8,
#                                "Race" = census.sample$V9,
#                                "Sex" = census.sample$V10,
#                                "Capital_gain" = census.sample$V11,
#                                "Capital_loss" = census.sample$V12,
#                                "Hours_per_week" = census.sample$V13,
#                                "Native_country" = census.sample$V14,
#                                "Income" = census.sample$V15)

census.processed <- data.frame("Age" = census.sample$V1,
                               "Work_class" = census.sample$V2,
                               "Fnl" = census.sample$V3,
                               "Education" = census.sample$V4,
                               "Marital_status" = census.sample$V6,
                               "Occupation" = census.sample$V7,
                               "Relationship" = census.sample$V8,
                               "Race" = census.sample$V9,
                               "Sex" = census.sample$V10,
                               "Capital_gain" = census.sample$V11,
                               "Capital_loss" = census.sample$V12,
                               "Hours_per_week" = census.sample$V13,
                               "Native_country" = census.sample$V14,
                               "Income" = census.sample$V15)



train.vals <- sample(nrow(census.processed),replace = F,size = 1400)
census.train <- census.processed[train.vals,]
census.test <- census.processed[-train.vals,]


census.model <- naiveBayes(Income ~ .,data = census.train)
census.out <- predict(census.model,census.test)


census.confusion.matrix <- table(census.out, census.test$Income)
census.model.tp <- census.confusion.matrix[1]
census.model.tn <- census.confusion.matrix[4]
census.model.fp <- census.confusion.matrix[3]
census.model.fn <- census.confusion.matrix[2]
census.model.accuracy <- (census.model.tp+census.model.tn)/
  (census.model.tp+census.model.tn+census.model.fp+census.model.fn)
census.model.recall <- (census.model.tp)/(census.model.tp+census.model.fn)
census.model.precision <- (census.model.tp)/(census.model.tp+census.model.fp)
census.model.f1.score <- 2*((census.model.precision*census.model.recall)/
                              (census.model.precision+census.model.recall))
