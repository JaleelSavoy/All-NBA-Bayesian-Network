library(bnlearn)
library(tidyverse)

set.seed(123)
num_of_draws <- 15000000

bin.ranges.df <- read.csv("bin_ranges.csv")
bin.sizes.df <- read.csv("bin_sizes.csv")

nba_player_data_refined <- read.csv("nba_quartiles.csv", stringsAsFactors = T, row.names = "X")
nba_player_data_2020_refined <- read.csv("nba_quartiles_2020.csv", stringsAsFactors = T, row.names = "X")
nba_player_data_2020_refined <- nba_player_data_2020_refined[-1,] # remove dummmy row
nba_player_data_refined$all.nba <- as.factor(nba_player_data_refined$all.nba)

summary(nba_player_data_refined)

cols_of_interest <- c(
  "gs","ts","per","efg","usg_perc","ws","all.nba"
)

bin_labels <- c(
  "Bin 1",
  "Bin 2",
  "Bin 3",
  "Bin 4")

node.names <- names(nba_player_data_refined[, cols_of_interest])
all.nodes <- node.names[-grep("all.nba", node.names)]

denylist <- data.frame(
  from = c("all.nba"), 
  to = all.nodes)

res <- hc(nba_player_data_refined[, cols_of_interest],
          blacklist = denylist,
          score='k2')
plot(res)

strength = arc.strength(res,
                        nba_player_data_refined[, cols_of_interest],
                        criterion = "k2")
strength.plot(res, strength)
strength[order(strength$strength), ]

fittedbn <- bn.fit(res, data=nba_player_data_refined[, cols_of_interest],
                   method = "bayes",
                   iss=100)

graphviz.plot(fittedbn)
arcs(fittedbn)
print(fittedbn$all.nba$parents)

cpq_probs <- list()

for (i in bin_labels){
  for (j in bin_labels){
    cpq_probs <- append(cpq_probs,
                        cpquery(fittedbn,
                                event = (all.nba == "1"),
                                evidence = (ws  == i &
                                            per == j),
                                n=num_of_draws,
                                method = "ls")
    )
  }
}

all.nba.prob <- matrix(cpq_probs, 4)
colnames(all.nba.prob) <- c(
  "Bin 1 (ws)",
  "Bin 2 (ws)",
  "Bin 3 (ws)",
  "Bin 4 (ws)")
row.names(all.nba.prob) <- c(
  "Bin 1 (per)",
  "Bin 2 (per)",
  "Bin 3 (per)",
  "Bin 4 (per)")
all.nba.prob

nba_player_data_refined%>%
  group_by(all.nba, per, usg_perc, ws)%>%
  summarize(n=n())%>%
  kable()

nba_player_data_refined%>%
  group_by(all.nba, per)%>%
  summarize(n=n())%>%
  kable()

nba_player_data_refined%>%
  group_by(all.nba, ws)%>%
  summarize(n=n())%>%
  kable()

nba_player_data_refined%>%
  group_by(all.nba, usg_perc)%>%
  summarize(n=n())%>%
  kable()

nba_player_data_refined[which(
  nba_player_data_refined$all.nba == "0" &
    nba_player_data_refined$usg_perc == "Bin 4" &
    nba_player_data_refined$per == "Bin 3"
  ), c("player", "player_season", "season")]

nba_player_data_refined[which(
  nba_player_data_refined$all.nba == "0" &
    nba_player_data_refined$usg_perc == "Bin 3" &
    nba_player_data_refined$per == "Bin 4"
), c("player", "player_season", "season")]

cpquery(fittedbn,
        event = (all.nba == "1"),
        evidence = (per == "Bin 1" &
                    ws  == "Bin 4"),
        n=num_of_draws)

predictions <- predict(fittedbn,
                       "all.nba",
                       data = nba_player_data_2020_refined,
                       prob = TRUE)

pred_probs <- attributes(predictions)

pred.df <- data.frame(
  Player = nba_player_data_2020_refined$player,
  Prediction = predictions,
  Actual = "0"
)

all.nba.2020 <- c(
  "Giannis Antetokounmpo", 
  "LeBron James",
  "James Harden",
  "Anthony Davis",
  "Luka Doncic",
  "Kawhi Leonard",
  "Nikola Jokic",
  "Damian Lillard",
  "Chris Paul",
  "Pascal Siakam",
  "Jayson Tatum",
  "Jimmy Butler",
  "Rudy Gobert",
  "Ben Simmons",
  "Russell Westbrook"
)

for (player in all.nba.2020){
  pred.df[which(pred.df$Player == player), "Actual"] <- "1"
}

pred.df$Actual <- as.factor(pred.df$Actual)

for (NBAPlayer in 1:nrow(nba_player_data_2020_refined)){
  pred.df$PredictedProbability[NBAPlayer] <- pred_probs[["prob"]][, NBAPlayer][2]
}

pred.df <- pred.df[order(pred.df$PredictedProbability), ]

all.nba.df <- pred.df[which(pred.df$Actual == "1" | pred.df$Prediction == "1"), ]
all.nba.df <- all.nba.df[order(-all.nba.df$PredictedProbability, all.nba.df$Actual), ]

summary(all.nba.df)
View(all.nba.df)

classification.matrix <- as.matrix(table(Actual = pred.df$actuals, Predicted = pred.df$preds))

n = sum(classification.matrix)
nc = nrow(classification.matrix)
diag = diag(classification.matrix)
rowsums = apply(classification.matrix, 1, sum)
colsums = apply(classification.matrix, 2, sum)
p = rowsums / n
q = colsums / n

accuracy_score <- sum(diag) / n
precision <- diag / colsums
recall <- diag / rowsums 
f1 <- (2 * (precision * recall)) / (precision + recall) 

macroPrecision <- mean(precision)
macroRecall <- mean(recall)
macroF1 <- mean(f1)

(n / nc) * matrix(rep(p, nc), nc, nc, byrow=F)
rgAccuracy <- 1 / nc
rgPrecision <- p
rgRecall <- 0*p + 1 / nc
rgF1 <- 2 * p / (nc * p + 1)

data.frame(rgPrecision, rgRecall, rgF1)
data.frame(macroPrecision, macroRecall, macroF1)
data.frame(precision, recall, f1)
classification.matrix
