#Student Name : Harish Harish
# Team number : 28
# Seed value: 40
# Sample size: 2k
# Split ratio: 70/30

library(arules)
library(data.tree)
library(caret)

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

#census.sample <- census.sample[,-5]


census.processed <- data.frame("Age" = census.sample$V1,
                               "Work_class" = census.sample$V2,
                               "Fnl" = census.sample$V3,
                               "Education" = census.sample$V4,
                               "Edu_num" = as.factor(census.sample$V5),
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

# census.processed <- data.frame("Age" = census.sample$V1,
#                                "Work_class" = census.sample$V2,
#                                "Fnl" = census.sample$V3,
#                                "Education" = census.sample$V4,
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

gini <- function(k,k1,k2)
{
  res <- 1 - (k1/k)^2 - (k2/k)^2
  if(is.nan(res)) return(0) else return(res)
}
gini.partition <- function(k,n,dist.values)
{
  gini.parent <- gini(k,dist.values[1],dist.values[2])
  weighted.sum.of.gini <- 0
  m <- 3
  while (n>0)
  {
    local.k <- dist.values[m]+dist.values[m+1]
    
    weighted.sum.of.gini <- weighted.sum.of.gini + 
      ((local.k/k)*(gini(local.k,dist.values[m],dist.values[m+1])))
   
    n <- n - 1
    m <- m + 2
  }
  
  gini.parent - weighted.sum.of.gini
}

choose.attribute <- function(examples)
{
  max.gain <- -1
  best.attribute <- -1
  ig <- -1
  br <- -1
  ig.vector <- -1
  c <- -1
  for(col in colnames(examples))
  {
    if(col != "Income")
    {
      census.temp <- data.frame("x"=examples[[col]],"Income"=examples$Income)
      
      if(class(census.temp[["x"]])=="factor")
      {
        ig <- get.info.gain.for.factor(census.temp)
      }
      else if(class(census.temp[["x"]])=="integer")
      {
        ig.vector <- get.info.gain.for.integer(census.temp)
        ig <- ig.vector[1]
      }
      
      if(ig > max.gain)
      {
        max.gain <- ig
        best.attribute <- col
        c <- class(census.temp[["x"]])
        if(class(census.temp[["x"]])=="integer") br <- ig.vector[2]
      }
    }
  }
  if(c=="factor")
    return(best.attribute)
  else
    return(c(best.attribute,br))
}


get.info.gain.for.integer <- function(data.integer)
{
  g <- -1
  br <- -1
  for(b in 2:4)
  {
    data.integer.disc <- try(discretizeDF(data.integer,default = list(breaks = b,method = "frequency")))
    if(is.data.frame(data.integer.disc))
    {
      ig <- get.info.gain.for.factor(data.integer.disc)
      if(ig>g) {g <- ig;br <- b}
    }
    else
      next
  }
  c(g,br)
}


get.info.gain.for.integer.fixed <- function(data.integer)
{
  g <- -1
  br <- -1
  initial.value <- summary(data.integer$x)[[1]]
  final.value <- summary(data.integer$x)[[6]]
  for(b in c((initial.value+1):(final.value-1)))
  {
    data.integer.disc <- try(discretizeDF(data.integer,default = list(breaks = c(initial.value,b,final.value),method = "fixed")))
    if(is.data.frame(data.integer.disc))
    {
      ig <- get.info.gain.for.factor(data.integer.disc)
      if(ig>g) {g <- ig;br <- b}
    }
    else
      next
  }
  c(initial.value,br,final.value)
}

get.best.split.for.fnl <- function(data.integer)
{
  g <- -1
  br <- -1
  for(b in 2:10)
  {
    data.integer.disc <- try(discretizeDF(data.integer,default = list(breaks = b,method = "frequency")))
    if(is.data.frame(data.integer.disc))
    {
      ig <- get.info.gain.for.factor(data.integer.disc)
      if(ig>g) {g <- ig;br <- b}
    }
    else
      next
  }
  #print(g)
  br
}


get.info.gain.for.factor <- function(data.disc)
{
  #print(class(data.disc[[col]]))
  i <- 3
  k.val <- nrow(data.disc)
  values <- 0
  values[1] <- sum(data.disc$Income==">50K")
  values[2] <- sum(data.disc$Income=="<=50K")
  
  #print(summary(data.disc[,col]))
  l <- unique(data.disc$x)
  num.levels <- length(l)
  for(v in l)
  {
    #print(v)
    above.50 <- sum(data.disc$x==v&data.disc$Income==">50K")
    below.50 <- sum(data.disc$x==v&data.disc$Income=="<=50K")
    values[i] <- above.50
    values[i+1] <- below.50
    #print(values)
    i <- i + 2
    #break
  }
  ig <- gini.partition(k.val,num.levels,values)
  ig
  #cat("Info gain for ",col,"is",ig,"\n")
  #length(values) <- 0
  #print(values)
}

decision.tree.learning <- function(data.train,default,node)
{
  if(is.atomic(data.train)) 
    return(default)
  else if(length(unique(data.train[,ncol(data.train)])) == 1) 
    return(unique(data.train[,ncol(data.train)]))
  else if(ncol(data.train)==1) 
    return(
      if(sum(data.train$Income==">50K")>=sum(data.train$Income=="<=50K"))
        ">50K"
      else
        "<=50K"
    )
  else
  {
    best.attribute <- choose.attribute(data.train)
    # if(length(best.attribute)>1) 
    # {
    #   switch (best.attribute[1],
    #     "Age" = data.train <- discretizeDF(data.train,methods = list(
    #         Age = list(breaks = as.integer(best.attribute[2]),method = "frequency")),
    #         default = list(method = "none")),
    #     "Capital_gain" = data.train <- discretizeDF(data.train,methods = list(
    #       Capital_gain = list(breaks = as.integer(best.attribute[2]),method = "frequency")),
    #       default = list(method = "none")),
    #     "Capital_loss" = data.train <- discretizeDF(data.train,methods = list(
    #       Capital_loss = list(breaks = as.integer(best.attribute[2]),method = "frequency")),
    #       default = list(method = "none")),
    #     "Hours_per_week" = data.train <- discretizeDF(data.train,methods = list(
    #       Hours_per_week = list(breaks = as.integer(best.attribute[2]),method = "frequency")),
    #       default = list(method = "none"))
    #   )
    #   best.attribute <- best.attribute[1]
    # }
    
    for(v.i in unique(data.train[[best.attribute]]))
    {
      data.train.best <- subset(data.train,data.train[[best.attribute]]==v.i)
      data.train.best <- data.train.best[,names(data.train.best)!=best.attribute,drop = F]
      if(is.atomic(data.train.best)) 
        m <- data.train.best
      else
      {
        if(sum(data.train.best$Income==">50K")>=sum(data.train.best$Income=="<=50K")) 
          m <- ">50K"
        else 
          m <- "<=50K"
      }
      node$mode <- default
      child <- node$AddChild(v.i)
      subtree <- decision.tree.learning(data.train.best,m,child)
      child$feature <- as.character(subtree) 
      child$mode <- m
    }
    return(best.attribute)
  }
}

age.best.split <- get.info.gain.for.integer.fixed(data.frame("x"=census.processed$Age,
                                                             "Income"=census.processed$Income))
hours.best.split <- get.info.gain.for.integer.fixed(data.frame("x"=census.processed$Hours_per_week,
                                                               "Income"=census.processed$Income))
fnl.threshold <- get.best.split.for.fnl(data.frame("x"=census.processed$Fnl,
                                                   "Income"=census.processed$Income))


#census.test <- subset(census.test,select = c(Age,Work_class,Hours_per_week,Income))
census.processed.disc <- discretizeDF(census.processed, methods = list(
  Capital_gain = list(method = "fixed", breaks = c(-1,1,Inf), 
                      labels = c("zero", "not_zero")),
  Capital_loss = list(method = "fixed", breaks = c(-1,1,Inf), 
                      labels = c("zeroL", "not_zeroL")),
  Hours_per_week = list(method = "fixed", breaks = hours.best.split, 
                        labels = c("first", "second")),
  Age = list(method = "fixed", breaks = age.best.split, 
             labels = c("firstA", "secondA")),
  Fnl = list(method = "frequency", breaks = fnl.threshold)
),
default = list(method = "none")
)

#census.test <- census.processed.disc[sample(nrow(census.processed.disc),replace = F,size = 500),]
#census.test <- subset(census.test,select = c(Age,Work_class,Hours_per_week,Income))
#census$feature <- decision.tree.learning(census.processed.disc,"<=50K",census)
#print(census,"feature")

census <- Node$new("Census")
train.vals <- sample(nrow(census.processed.disc),replace = F,size = 1400)
census.train <- census.processed.disc[train.vals,]
census.test <- census.processed.disc[-train.vals,]

ifelse(sum(census.train$Income==">50K")>=sum(census.train$Income=="<=50K"),
       train.default <- ">50K",train.default <- "<=50K")

census$feature <- decision.tree.learning(census.train,train.default,census)

p <- function(t,i,m="NA")
{
  att <- -1
  while(!t$isLeaf)
  {
    att <- as.character(i[[t$feature]])
    if(is.null(t[[att]])) 
    {
      if(is.null(t$mode))
      {
        return(m)
      }
      return(t$mode)
    }
    t <- t[[att]]
  }
  t$feature
}


predicted.income <- -1
for(i in 1:nrow(census.test))
{
  #print(i)
  test.case <- as.list(census.test[i,names(census.test)!="Income"])
  predicted.income[i] <- p(census,test.case)
}

census.result  <- data.frame("actual_income"=census.test$Income,"predicted_income"=predicted.income)
census.confusion.matrix <- confusionMatrix(census.result$predicted_income,census.result$actual_income,
                                           dnn = c("predicted_income","actual_income"))$table
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
