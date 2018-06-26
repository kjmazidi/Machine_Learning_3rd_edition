# File to set up wine-all.csv
# Using a combined red and white wine data from UCI ML Repository
# https://archive.ics.uci.edu/ml/datasets/Wine+Quality

# load wine data  6497 x 12
wine <- read.csv("wine.csv", header=FALSE, sep=';')
colnames(wine) <- c("fixed_acidity","volatile_acidity","citric_acid","residual_sugar",
                    "chlorides", "free_sulfur_dioxide", "total_sulfur_dioxide", "density",
                    "pH", "sulphates", "alcohol", "quality")
# add red/white label
wine$type = "red"
wine$type[1600:6497] <- "white"
wine$type <- factor(wine$type)
summary(wine$type)  # red 1599 # white 4898
contrasts(wine$type)
str(wine)

# write out to wine_all.csv
write.csv(wine, file="wine_all.csv", row.names = FALSE)

