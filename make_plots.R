library(data.table)
library(ggplot2)
library(RColorBrewer)
#setwd("~/Documents/dagstuhl/dagstuhl_analysis")

# Import data and convert to data tables
co2 <- read.csv("./csvs/co2.csv")
co2_dt <- as.data.table(co2)
participants <- read.csv("./csvs/participants.csv")
participants_dt <- as.data.table(participants)
seminars <- read.csv("./csvs/seminars.csv")
seminars_dt <- as.data.table(seminars)

x <- participants_dt[, nrow(.SD), by=seminar_id]
setnames(x, "V1", "participants")
x <- merge(x, seminars_dt[, c("seminar_id", "year")], all.x = TRUE)

x[, seminar_id := paste("seminar", formatC(seminar_id, width=5, flag="0"), sep=" ")]

ggplot(x, aes(seminar_id, participants)) +
  geom_bar(aes(colour = participants), stat = "identity", show.legend = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle(label = "# of Participants in Dagstuhl Seminars from 2001 to 2019") +
  scale_colour_gradient(low = "white", high = "black")

ggplot(x, aes(participants)) +
  geom_line(aes(y=..y..), stat="ecdf") +
  ggtitle("CDF of participants per seminar") +
  ylab("CDF")
