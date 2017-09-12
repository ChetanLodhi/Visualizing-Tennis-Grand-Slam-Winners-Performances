setwd("D:/R/Projects/Tennis")

# Loading R libraries
library_toload <- c("dplyr", "knitr", "ggplot2", "gplots", "RColorBrewer", "timelineS", "circlize", "fmsb")
invisible(lapply(library_toload, function(x) {suppressPackageStartupMessages(library(x, character.only=TRUE))}))

# Importing the Tennis Grand Slam Winners dataset
Slam_Winners <- read.table("Tennis-Men-Grand-Slam-Title-Winners.txt", sep="\t", stringsAsFactors = FALSE, header = TRUE)

# Fix to the tournaments data column to have same naming for the Australian Open tournament as it held twice in 1977
Slam_Winners[grep("Australian Open", Slam_Winners$TOURNAMENT), "TOURNAMENT"] = "Australian Open"

# Grouping by winner and summarising the number of wins
Slam_Top_Chart = Slam_Winners %>% group_by(WINNER) %>% summarise(NUM_WINS=n()) %>% arrange(desc(NUM_WINS))
Slam_Top_Chart$WINNER <- factor(Slam_Top_Chart$WINNER, levels = Slam_Top_Chart$WINNER[order(Slam_Top_Chart$NUM_WINS)])

# Champions with Grand Slam Singles Titles (4 or more)
top_winners_gt4 = Slam_Top_Chart %>% filter(NUM_WINS >= 4)

the_colours = c("#FF4000FF", "#FF8000FF", "#FFFF00FF", "#80FF00FF",
                "#00FF00FF", "#00FF80FF", "#00FFFFFF", "#0080FFFF",
                "#FF00FFFF", "#000000FF", "#0000FFFF")

# To plot a barplot reporting winners ordered by their number of wins
ggplot(data=top_winners_gt4, aes(x=WINNER, y=NUM_WINS, fill=NUM_WINS)) + geom_bar(stat='identity') + coord_flip() + guides(fill=FALSE) + scale_fill_gradientn(colours = the_colours)

# To plot barplot by tournaments of cahpions having more than 4 grand slams
Slam_Top_Chart_by_Trn = Slam_Winners %>% filter(WINNER %in% top_winners_gt4$WINNER) %>% group_by(TOURNAMENT, WINNER) %>% summarise(NUM_WINS=n()) %>% arrange(desc(NUM_WINS))
Slam_Top_Chart_by_Trn$NUM_WINS <- factor(Slam_Top_Chart_by_Trn$NUM_WINS)

ggplot(data=Slam_Top_Chart_by_Trn, aes(x=WINNER, y=NUM_WINS, fill=NUM_WINS)) + geom_bar(stat='identity') + coord_flip() + guides(fill=FALSE) + scale_y_discrete() + facet_grid(. ~ TOURNAMENT)

# Heatmap: To highlight how many times champions met each other on Grand Slam tournament finals
tl_rec <- 1:40
winner_runnerup <- Slam_Winners[tl_rec, c("WINNER", "RUNNER.UP")]
winner_runnerup_names <- unique(c(winner_runnerup[,1], winner_runnerup[,2]))

match_matrix <- matrix(0, nrow=length(winner_runnerup_names),
                       ncol=length(winner_runnerup_names))
colnames(match_matrix) <- winner_runnerup_names
rownames(match_matrix) <- winner_runnerup_names

for(i in 1:nrow(winner_runnerup)) {
  winner <- winner_runnerup[i, "WINNER"]
  runner_up <- winner_runnerup[i, "RUNNER.UP"]
  r <- which(rownames(match_matrix) == winner)
  c <- which(colnames(match_matrix) == runner_up)
  
  match_matrix[r,c] <- match_matrix[r,c] + 1
  match_matrix[c,r] <- match_matrix[c,r] + 1
}

diag(match_matrix) <- NA

my_palette <- colorRampPalette(c("green", "yellow", "red"))(n = 299)

col_breaks = c(seq(0, 0.99, length=100),  # for green
               seq(1, 5, length=100),     # for yellow
               seq(5.01, 10, length=100)) # for red

heatmap.2(match_matrix,
          cellnote = match_matrix,  # same data set for cell labels
          main = "Tennis Grand Slam Finalists Heatmap", # heat map title
          notecol = "black",      # change font color of cell labels to black
          density.info = "none",  # turns off density plot inside color legend
          trace = "none",         # turns off trace lines inside the heat map
          margins = c(10,10),     # widens margins around plot
          col= my_palette,       # use on color palette defined earlier
          breaks = col_breaks,    # enable color transition at specified limits
          dendrogram = "none",     # only draw a row dendrogram
          Colv = "NA")

# Dendrogram plot: To group champions based on some specific similarity metric
ch_n <- 1:20
wins <- Slam_Top_Chart[ch_n, -1]
D_Wins <- dist(wins, method = "euclidean")
hclust_fit <- hclust(D_Wins)
h_value <- 2
groups <- cutree(hclust_fit, h = h_value)
plot(hclust_fit, labels = Slam_Top_Chart$WINNER[ch_n], main = "Champions Dendrogram")
rect.hclust(hclust_fit, h = h_value, border = "blue")

# To plot Grand Slam Timeline 2013-2017: To highlight how the wins sequence happened 
year_to_date_trnm <- function(the_year, the_trnm) {
  the_date <- NULL
  if (the_trnm == "Australian Open") {
    the_date <- (paste(the_year, "-01-31", sep=""))
  } else if (the_trnm == "French Open") {
    the_date <- (paste(the_year, "-06-15", sep=""))
  } else if (the_trnm == "Wimbledon") {
    the_date <- (paste(the_year, "-07-15", sep=""))
  } else if (the_trnm == "U.S. Open") {
    the_date <- (paste(the_year, "-09-07", sep=""))
  }
  the_date
}

Slam_Winners$YEAR_DATE <- as.Date(mapply(year_to_date_trnm, Slam_Winners$YEAR, Slam_Winners$TOURNAMENT), format="%Y-%m-%d")
tl_rec <- 1:20
timelineS(Slam_Winners[tl_rec, c("WINNER", "YEAR_DATE")], main = "Grand Slam Timeline", line.color = "red", scale.font = 3,
          scale = "month", scale.format = "%Y", label.cex = 0.7, buffer.days = 120,
          labels = paste(Slam_Winners[tl_rec, "WINNER"], Slam_Winners[tl_rec, "TOURNAMENT"]))

# Chord Diagram to highlight relationship between champions (more than 10 grand slams) and tournaments wins 
top_winners_gt10 = Slam_Top_Chart %>% filter(NUM_WINS > 10)

slam_win_cnt = inner_join(Slam_Winners, top_winners_gt10) %>% select(TOURNAMENT, WINNER) %>%
  group_by(WINNER, TOURNAMENT) %>% summarise(NUM_WINS = n()) %>% arrange(TOURNAMENT, desc(NUM_WINS))

chordDiagram(slam_win_cnt)

# To highlight individual champions’ strengths and weaknesses in each specific Grand Slam tournament using radar plot
champion_radar_plot <- function(df, champion_name) {
  slam_win_cnt_chp = df %>% filter(WINNER == champion_name)
  chp_num_wins <- slam_win_cnt_chp$NUM_WINS
  l <- length(chp_num_wins)
  max_v <- 10 # choosing the same maximum value for all champions
  chp_df <- data.frame(rbind(max = rep(max_v, l), min = rep(0, l), chp_num_wins))
  colnames(chp_df) <- slam_win_cnt_chp$TOURNAMENT
  seg_n <- max_v
  radarchart(chp_df, axistype = 1, caxislabels = seq(0, max_v, 1), seg = seg_n,
             centerzero = TRUE, pcol = rgb(0.2, 0.5, 0.5, 0.9) , pfcol = rgb(0.2, 0.5, 0.5, 0.3),
             plwd = 1, cglcol = "grey", cglty = 1, axislabcol = "blue",
             vlcex = 0.8, calcex = 0.7, title = champion_name)
}

champion_radar_plot(slam_win_cnt, "Roger Federer")
champion_radar_plot(slam_win_cnt, "Rafael Nadal")
champion_radar_plot(slam_win_cnt, "Novak Djokovic")
