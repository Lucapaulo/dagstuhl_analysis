library(data.table)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(lubridate)
library(tidyr)
library(xts)
setwd("~/Documents/dagstuhl/dagstuhl_analysis")

# import data and convert to data tables
co2 <- read.csv("./csvs/co2.csv")
co2_dt <- as.data.table(co2)
participants <- read.csv("./csvs/participants.csv")
participants_dt <- as.data.table(participants)
seminars <- read.csv("./csvs/seminars.csv")
seminars_dt <- as.data.table(seminars)

# correct dates on seminars_dt
seminars_dt[ , "date" := lapply(.SD, gsub, pattern = "^(\\S*\\s+\\S+).*", replacement = "\\1"), .SDcols = "date"]
seminars_dt <- unite(seminars_dt, col = start_date, date, year, sep = " ")
seminars_dt[ , "start_date" := lapply(.SD, mdy), .SDcols = "start_date"]
seminars_dt[ , "start_date" := lapply(.SD, as.POSIXct), .SDcols = "start_date"]
# fix one weird value
seminars_dt[seminar_id == 10519]$start_date <- seminars_dt[seminar_id == 10519]$start_date - 284212568
seminars_dt$start_date <- make.index.unique(seminars_dt$start_date, eps = 1, drop = FALSE)

# participants per seminar
x <- participants_dt[, nrow(.SD), by=seminar_id]
setnames(x, "V1", "participants")
x <- merge(x, seminars_dt[, c("seminar_id", "start_date")], all.x = TRUE)
x[, seminar_id := paste("seminar", formatC(seminar_id, width=5, flag="0"), sep=" ")]

# participants chronological
ggplot(x, aes(start_date, participants)) +
  geom_line(aes(colour = participants), stat = "identity", show.legend = FALSE) +
  labs(title = "# of Participants in Dagstuhl Seminars from 2001 to 2019",
       x = "Year",
       y = "# of Participants") +
  scale_colour_gradient(low = "white", high = "black") +
  geom_smooth(data=x %>% group_by(start_date) %>% summarise(participants=sum(participants)),
              aes(x = start_date, y = participants, group=1),
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
y$country <- NULL
y <- y[, .(co2=sum(tons)), by=seminar_id]
y <- merge(y, seminars_dt[, c("seminar_id", "start_date")], by.x="seminar_id", by.y="seminar_id")
y[, seminar_id := paste("seminar", formatC(seminar_id, width=5, flag="0"), sep=" ")]

# co2 tons chronological
ggplot(y, aes(start_date, co2)) +
  geom_line(aes(colour = co2), stat = "identity", show.legend = FALSE) +
  labs(title = "Tons of CO² per Dagstuhl Seminar from 2001 to 2019",
       x = "Year",
       y = "Tons of CO²") +
  scale_colour_gradient(low = "white", high = "black") +
  geom_smooth(data=y %>% group_by(start_date) %>% summarise(co2=sum(co2)),
              aes(x = start_date, y = co2, group=1),
              method = "lm", se= FALSE, color = "firebrick1", size = 1) +
  scale_y_continuous(expand = c(0.01,0.01))

# seminars by co2 tons
ggplot(y, aes(reorder(seminar_id, -co2), co2)) +
  geom_bar(aes(colour = co2), stat = "identity", show.legend = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = "Tons of CO² per Dagstuhl Seminar (Ordered by Quantity)",
       x = "Seminar",
       y = "Tons of CO²") +
  scale_colour_gradient(low = "white", high = "black") +
  scale_y_continuous(expand = c(0.01,0.01))


# mixed
z <- merge(x, y)

ggplot(z, aes(x=start_date)) +
  geom_line(aes(y = participants, group = 1, colour = "Participants")) +
  geom_line(aes(y = co2, group = 1, colour = "CO²")) +
  scale_y_continuous(expand = c(0.01,0.01), sec.axis = sec_axis(~.*1, name = "Tons of CO²")) +
  scale_colour_manual(values = c("red", "dodgerblue")) +
  labs(x = "Year",
       y = "# of Participants",
       colour = "Legend",
       title = "# of Participants and Carbon Footprint in Dagstuhl Seminars") +
  geom_smooth(data=z %>% group_by(start_date) %>% summarise(co2=sum(co2)),
              aes(x = start_date, y = co2, group=1),
              method = "lm", se= FALSE, color = "red3", size = 1) +
  geom_smooth(data=z %>% group_by(start_date) %>% summarise(participants=sum(participants)),
              aes(x = start_date, y = participants, group=1),
              method = "lm", se= FALSE, color = "dodgerblue3", size = 1) +
  theme(legend.position = c(0.9, 0.9))
        