library(data.table)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
#setwd("~/Documents/dagstuhl/dagstuhl_analysis")

# import data and convert to data tables
co2 <- read.csv("./csvs/co2.csv")
co2_dt <- as.data.table(co2)
participants <- read.csv("./csvs/participants.csv")
participants_dt <- as.data.table(participants)
seminars <- read.csv("./csvs/seminars.csv")
seminars_dt <- as.data.table(seminars)

# participants per seminar
x <- participants_dt[, nrow(.SD), by=seminar_id]
setnames(x, "V1", "participants")
x <- merge(x, seminars_dt[, c("seminar_id", "year")], all.x = TRUE)
x[, seminar_id := paste("seminar", formatC(seminar_id, width=5, flag="0"), sep=" ")]

# chronological
ggplot(x, aes(seminar_id, participants)) +
  geom_bar(aes(colour = participants), stat = "identity", show.legend = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle(label = "# of Participants in Dagstuhl Seminars from 2001 to 2019 (Chronological Order)") +
  scale_colour_gradient(low = "white", high = "black") +
  geom_smooth(data=x %>% group_by(seminar_id) %>% summarise(participants=sum(participants)),
              aes(x = seminar_id, y = participants, group=1),
              method = "lm", se= FALSE, color = "firebrick1", size = 1) +
  scale_y_continuous(expand = c(0.01,0.01))

# CDF
ggplot(x, aes(participants)) +
  geom_line(aes(y=..y..), stat="ecdf") +
  ggtitle("CDF of Participants per Seminar") +
  ylab("CDF") +
  scale_y_continuous(expand = c(0.01,0.01))

# co2 tons per seminar
y <- merge(participants_dt[, c("seminar_id", "country")], co2_dt, all.x=TRUE)
y <- merge(y, seminars_dt[, c("seminar_id", "year")], by.x="seminar_id", by.y="seminar_id", all.x=TRUE)
y$country <- NULL
y <- y[, .(co2=sum(tons)), by=seminar_id]
y[, seminar_id := paste("seminar", formatC(seminar_id, width=5, flag="0"), sep=" ")]

# chronological
ggplot(y, aes(seminar_id, co2)) +
  geom_bar(aes(colour = co2), stat = "identity", show.legend = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle(label = "Tons of CO² per Dagstuhl Seminar from 2001 to 2019 (Chronological Order)") +
  scale_colour_gradient(low = "white", high = "black") +
  geom_smooth(data=y %>% group_by(seminar_id) %>% summarise(co2=sum(co2)),
              aes(x = seminar_id, y = co2, group=1),
              method = "lm", se= FALSE, color = "firebrick1", size = 1) +
  scale_y_continuous(expand = c(0.01,0.01))

# by co2 tons
ggplot(y, aes(reorder(seminar_id, -co2), co2)) +
  geom_bar(aes(colour = co2), stat = "identity", show.legend = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle(label = "Tons of CO² per Dagstuhl Seminar (Ordered by Quantity)") +
  scale_colour_gradient(low = "white", high = "black") +
  xlab("seminar_id") +
  scale_y_continuous(expand = c(0.01,0.01))
