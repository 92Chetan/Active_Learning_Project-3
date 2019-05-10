
#Association Rule Mining with R???

# Outline:
# 1).Introduction
# 2).Association Rule Mining
# 3).Removing Redundancy
# 4).Interpreting Rules
# 5).Visualizing Association Rules
# 6).Further Readings and Online Resources


# STEP-1 : INTRODUCTION 

# SUPPORT :
# CONFIDENCE :
# LIFT :

# STEP-2 : ASSOCIATION RULE MINING

# The Titanic Dataset
# 
# The Titanic dataset in the datasets package is a 4-dimensional
# table with summarized information on the fate of passengers
# on the Titanic according to social class, sex, age and survival.

#Link to the dataset:

#http://www.rdatamining.com/data/titanic.raw.rdata

# loading the dataset 

load("./data/titanic.raw.rdata")

## draw a sample of 5 records
idx <- sample(1:nrow(titanic.raw), 5)
titanic.raw[idx, ]

summary(titanic.raw)

library (arules)

# Function apriori()
# Mine frequent itemsets, association rules or association hyperedges
# using the Apriori algorithm. The Apriori algorithm employs
# level-wise search for frequent itemsets.
# Default settings:
#  minimum support: supp=0.1
#  minimum confidence: conf=0.8
#  maximum length of rules: maxlen=10


rules.all <- apriori(titanic.raw)
rules.all

#Alternative to inspect() is to convert rules to a dataframe and then use View()
rules <- as(rules.all,"data.frame")
View(rules)
View(inspect(rules.all))

# rules with rhs containing "Survived" only
rules <- apriori(titanic.raw,
                 control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No",
                                         "Survived=Yes"),
                                   default="lhs"))


View(rules)
## keep three decimal places
quality(rules) <- round(quality(rules), digits=3)
View(quality(rules))
## order rules by lift
rules.sorted <- sort(rules, by="lift")
View(inspect(rules.sorted))

is.redundant(rules.sorted)


# STEP - 3 : Removing Redundancy

# inspect redundant rules 
View(inspect(rules.sorted[is.redundant(rules.sorted)]))
# inspect non redundant rules
View(inspect(rules.sorted[!is.redundant(rules.sorted)]))

# STEP - 4 : Interpreting Rules 

# Rules about children 

rules <- apriori(titanic.raw, control = list(verbose=F),
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(default="none", rhs=c("Survived=Yes"),
                                   lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                         "Age=Child", "Age=Adult")))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)


# STEP - 5 : vISUALIZING ASSOCAITION RULES 

library(arulesViz)
plot(rules.all)
plot(rules.all,method = "grouped")
plot(rules.all, method = "graph")
plot(rules.all, method = "graph", control = list(type = "items"))
plot(rules.all, method = "paracoord", control = list(reorder = TRUE))



































































