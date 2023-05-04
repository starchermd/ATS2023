#Load necessary packages
library(readr)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggthemes)
library(gridExtra)
library(ggtext)
library(shadowtext)
library(VIM)
library(ggpubr)

#Load dataset from CSV
starcherdataset <- read_csv("starcherdataset.csv")
starcherdataset$week <- mdy(starcherdataset$week)

# Evaluate missing data
aggr(starcherdataset, combined=TRUE, numbers=TRUE, prop=TRUE)

# Visualize trends
ggplot(starcherdataset) + 
  geom_point(aes(week, urbandeathrate), shape=21, fill="red") + 
  geom_point(aes(week, ruraldeathrate), shape=21, fill="blue") + 
  ggtitle("Minnesota State Mortality by Week of COVID Pandemic", subtitle = "By patients' rural or urban classification") + 
  scale_x_date(labels=date_format("%b %Y"), breaks=date_breaks("4 months")) + 
  geom_line(aes(week, urbandeathrate), linewidth=1.5, color="red") + 
  geom_line(aes(week, ruraldeathrate), linewidth=1.5, color="blue") + 
  labs(x = "Week of pandemic <br><i><span style = font-size:20pt>(with weeks of <span style = 'color: yellow3;'>excess mortality</span> emphasized)</i></span>", y = "Mortality (per 100,000 residents)", subtitle = "By patients' <span style = 'color: blue;'>rural</span> or <span style = 'color: red;'>urban</span> classification") +
  theme( 
        plot.subtitle = element_markdown(size = 24, hjust=0.5), 
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 28),
        axis.title.x = element_markdown(size = 28),
        plot.title = element_text(size = 32, hjust = 0.5)) +
  annotate("rect", xmin= as.Date("2020-04-19", "%Y-%m-%d"), xmax=as.Date("2020-06-27", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2020-08-23", "%Y-%m-%d"), xmax=as.Date("2020-09-05", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2020-09-20", "%Y-%m-%d"), xmax=as.Date("2020-10-03", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2020-10-11", "%Y-%m-%d"), xmax=as.Date("2020-10-17", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2020-10-25", "%Y-%m-%d"), xmax=as.Date("2020-10-31", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2020-11-08", "%Y-%m-%d"), xmax=as.Date("2021-01-23", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2021-05-02", "%Y-%m-%d"), xmax=as.Date("2021-05-08", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2021-05-16", "%Y-%m-%d"), xmax=as.Date("2021-05-22", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2021-06-13", "%Y-%m-%d"), xmax=as.Date("2021-06-19", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2021-08-15", "%Y-%m-%d"), xmax=as.Date("2021-08-21", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2021-09-05", "%Y-%m-%d"), xmax=as.Date("2022-03-05", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2022-05-29", "%Y-%m-%d"), xmax=as.Date("2022-06-04", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2022-06-26", "%Y-%m-%d"), xmax=as.Date("2022-07-16", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2022-07-24", "%Y-%m-%d"), xmax=as.Date("2022-08-13", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2022-09-25", "%Y-%m-%d"), xmax=as.Date("2022-10-01", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2022-09-11", "%Y-%m-%d"), xmax=as.Date("2022-09-17", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) +
  annotate("rect", xmin= as.Date("2022-10-16", "%Y-%m-%d"), xmax=as.Date("2022-10-29", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25) + 
  annotate("rect", xmin= as.Date("2022-11-06", "%Y-%m-%d"), xmax=as.Date("2022-12-03", "%Y-%m-%d"), ymin=-Inf, ymax=Inf, fill = "yellow", alpha = 0.25)

# For this graphic, the yellow areas represent time periods with excess mortality,
# the red line is the per capita mortality for urban patients and the blue line
# is the per capita mortality for rural patients.
# Rural is defined my Metropolitan Statistical Areas predetermined from the downloaded datasets.
# Patient level data was not available for this analysis.

# -------Pivotlonger to add Rural-Urban Categorical variable & Lengthen Data--------

library(dplyr) #Reloading to get "select" back
#    Lengthen mortality data
starcherlong <- pivot_longer(starcherdataset, cols = ends_with("deathrate"), names_to = "RuralUrbanCat", values_to = "mortalityrates") |>
  select(-c("ruralpop", "ruraldeaths", "urbanpop", "urbandeaths"))

# Create "Rural/Urban" factor
starcherlong$RuralUrbanCat<-as.factor(starcherlong$RuralUrbanCat)
starcherlong$RuralUrbanCat <- recode_factor(starcherlong$RuralUrbanCat, ruraldeathrate = "Rural", urbandeathrate = "Urban")

# Lengthen icubedsavg data
starcherlong$icubedsavg <- ifelse(starcherlong$RuralUrbanCat == "Rural", starcherlong$ruralicubedsavg, starcherlong$urbanicubedsavg)
starcherlong <- select(starcherlong, -c("ruralicubedsavg", "urbanicubedsavg"))

# Lengthen icuoccupancy
starcherlong$icucensus <- ifelse(starcherlong$RuralUrbanCat == "Rural", starcherlong$ruralicuoccupancy, starcherlong$urbanicuoccupancy)
starcherlong <- select(starcherlong, -c("ruralicuoccupancy", "urbanicuoccupancy"))

# Lengthen inptbeds
starcherlong$inptbeds <- ifelse(starcherlong$RuralUrbanCat == "Rural", starcherlong$ruralinptbeds, starcherlong$urbaninptbeds)
starcherlong <- select(starcherlong, -c("ruralinptbeds", "urbaninptbeds"))

#Rename column
starcherlong <- starcherlong |> rename("excesspercent" = "excessdeath%")

#------------CREATE SUBSETS OF EXCESS OR NO EXCESS----------------
excess <- starcherdataset |>
  filter(excessdeathsYN == "TRUE")
noexcess <- starcherdataset |> 
  filter(excessdeathsYN == "FALSE")

#------------ CORRELATIONS ------------
# Relationship between Urban ICU census and percent excess mortality.

cor.test(excess$urbanicuoccupancy, excess$`excessdeath%`, use="complete.obs")
summary(lm(excess$`excessdeath%`~excess$urbanicuoccupancy))

# Includes 99% confidence interval of linear model
starcherlong |> 
  filter(RuralUrbanCat == "Urban") |>
  filter(excessdeathsYN == TRUE) |>
  ggplot(aes(x=icucensus, y=excesspercent)) + 
  geom_point(color="red") +
  ggtitle("Association of <span style = 'color: red'>Urban</span> ICU Census to Excess Mortality") +
  labs(x="ICU Census (mean per hospital per day)", y="Percent of excess mortality (statewide)")+
  theme(plot.title = element_markdown(hjust=0.5, size=24),
        axis.title = element_text(size=20)) +
  geom_smooth(method=lm, level=0.99)

# Cannot evaluate Rural ICU census due to the values being so low (<1).  However,
# if you try, you will find that it does have a significant correlation.
cor.test(excess$ruralicuoccupancy, excess$`excessdeath%`, use="complete.obs")
summary(lm(excess$`excessdeath%`~excess$ruralicuoccupancy))

# Includes 99% confidence interval of linear model
starcherlong |> 
  filter(RuralUrbanCat == "Rural") |>
  filter(excessdeathsYN == TRUE) |>
  ggplot(aes(x=icucensus, y=excesspercent)) + 
  geom_point(color="blue") +
  ggtitle("Association of <span style = 'color: blue'>Rural</span> ICU Census to Excess Mortality") +
  labs(x="ICU Census (mean per hospital per day)", y="Percent of excess mortality (statewide)")+
  theme(plot.title = element_markdown(hjust=0.5, size=24),
        axis.title = element_text(size=20)) +
  geom_smooth(method=lm, level=0.99)

#------------ DATA ANALYSIS ------------

library(dplyr) #Reload deplyr to over-mask conflicting packages using "filter" function
hist(starcherdataset$ruraldeathrate)
# **MORTALITY PER 100,000**
# Rural vs urban - Overall
wilcox.test(starcherdataset$ruraldeathrate, starcherdataset$urbandeathrate)

# MORTALITY During periods WITHOUT excess mortality
wilcox.test(noexcess$ruraldeathrate, noexcess$urbandeathrate)

# MORTALITY During periods WITH EXCESS MORTALITY
wilcox.test(excess$ruraldeathrate, excess$urbandeathrate)

# MORTALITY - comparing no strain periods to strain periods
wilcox.test(excess$ruraldeathrate, noexcess$ruraldeathrate)
wilcox.test(excess$urbandeathrate, noexcess$urbandeathrate)

# Visualize mortality data
labelexcess <- c("FALSE" = "No System Strain", "TRUE" = "System Strain")

ggplot(starcherlong, aes(x = RuralUrbanCat, y= mortalityrates, color=RuralUrbanCat, fill=excessdeathsYN)) + 
  scale_fill_manual(values = c("white", "yellow2")) +
  guides(color = FALSE) +
  geom_boxplot(lwd=1.5) +
  scale_color_manual(values = c("Rural" = "blue", "Urban" = "red")) +
  facet_grid(.~excessdeathsYN, labeller = labeller(excessdeathsYN = labelexcess)) +
  labs(x = NULL, y= "Mortality (per 100,000 residents)", title = "Mortality") +
  theme_classic() +
  theme(
    plot.title = element_markdown(size=28, hjust=0.5),
    axis.text = element_text(size=22),
    axis.title.y = element_text(size=22),
    plot.subtitle = element_text(size=22, face="bold"),
    strip.text.x = element_text(size = 22),
    legend.position = "none") +
  ylim(0, 12)

ggplot(starcherlong, aes(x = RuralUrbanCat, y= mortalityrates, fill=RuralUrbanCat)) +
  geom_bar(stat="summary", fun="mean") +
  scale_fill_manual(values = c("Rural" = "blue", "Urban" = "red")) +
  facet_grid(.~excessdeathsYN, labeller = labeller(excessdeathsYN = labelexcess)) +
  labs(x = NULL, y= "Mortality (per 100,000 residents)", title = "Overall Mortality") +
  theme_classic() +
  theme(
    plot.title = element_markdown(size=28, hjust=0.5),
    axis.text = element_text(size=22),
    axis.title.y = element_text(size=22),
    plot.subtitle = element_text(size=22, face="bold"),
    strip.text.x = element_text(size = 22),
    legend.position = "none") +
  ylim(0, 6)

# **NUMBER OF ICU BEDS**
# ICU BEDS Rural vs urban
t.test(starcherdataset$ruralicubedsavg, starcherdataset$urbanicubedsavg)

# ICU BEDS During periods WITHOUT excess mortality
t.test(noexcess$ruralicubedsavg, noexcess$urbanicubedsavg)

# ICU BEDS During periods WITH EXCESS MORTALITY
t.test(excess$ruralicubedsavg, excess$urbanicubedsavg)

# ICU BEDS - comparing by rural-urban categorization
t.test(noexcess$ruralicubedsavg, excess$ruralicubedsavg)
t.test(noexcess$urbanicubedsavg, excess$urbanicubedsavg)

# Visualize ICU Beds
ggplot(starcherlong, aes(x = RuralUrbanCat, y= icubedsavg, color=RuralUrbanCat, fill=excessdeathsYN)) + 
  scale_fill_manual(values = c("white", "yellow2")) +
  guides(color = FALSE) +
  geom_boxplot(lwd=1.5) +
  scale_color_manual(values = c("Rural" = "blue", "Urban" = "red")) +
  facet_grid(.~excessdeathsYN, labeller = labeller(excessdeathsYN = labelexcess)) +
  labs(x = NULL, y= "Number of ICU Beds (per hospital, per day)", title = "Number of ICU Beds") +
  theme_classic() +
  theme(
    plot.title = element_markdown(size=28, hjust=0.5),
    axis.text = element_text(size=22),
    axis.title.y = element_text(size=22),
    plot.subtitle = element_text(size=22, face="bold"),
    strip.text.x = element_text(size = 22),
    legend.position = "none") +
  ylim(0, 15)

ggplot(starcherlong, aes(x=RuralUrbanCat, y=icubedsavg, fill=as.factor(RuralUrbanCat)))+
  geom_bar(position = "dodge", stat="summary", fun="mean") +
  facet_grid(.~excessdeathsYN, labeller = labeller(excessdeathsYN = labelexcess)) +
  labs(x = NULL, y= "Number of ICU Beds (per hospital, per day)", title = "Number of ICU Beds")+
  theme_classic()+
  scale_fill_manual(values = c("blue", "red"))+
  theme(legend.position = "none",
        axis.text = element_text(size=22),
        plot.title = element_text(size=28, hjust=0.5),
        axis.title = element_text(size=22),
        strip.text.x = element_text(size=22))+
  ylim(0, 13)

# **NUMBER OF ICU BEDS IN USE PER DAY**
# CENSUS Rural vs urban
t.test(starcherdataset$ruralicuoccupancy, starcherdataset$urbanicuoccupancy)

# CENSUS During periods WITHOUT excess mortality
t.test(noexcess$ruralicuoccupancy, noexcess$urbanicuoccupancy)

# CENSUS During periods WITH EXCESS MORTALITY
t.test(excess$ruralicuoccupancy, excess$urbanicuoccupancy)

# CENSUS Comparing within category --- strain vs not
t.test(excess$ruralicuoccupancy, noexcess$ruralicuoccupancy)
t.test(excess$urbanicuoccupancy, noexcess$urbanicuoccupancy)

# CENSUS PERCENT INCREASE
census_matrix <- matrix(c(mean(excess$ruralicuoccupancy, na.rm=TRUE), mean(excess$urbanicuoccupancy, na.rm=TRUE), mean(noexcess$ruralicuoccupancy, na.rm=TRUE), mean(noexcess$urbanicuoccupancy, na.rm=TRUE)), nrow = 2, ncol = 2)
rownames(census_matrix) <- c("Rural", "Urban")
colnames(census_matrix) <- c("Strain", "No.Strain")
census_matrix <- data.frame(census_matrix)
census_matrix <- mutate(census_matrix, RUcat = c("Rural", "Urban"))
census_matrix <- mutate(census_matrix, percentchange = (Strain/No.Strain-1)*100)
census_matrix <- mutate(census_matrix, ratio = Strain/No.Strain)
census_matrix
t.test(census_matrix$ratio)

# Visualize Census Differences
ggplot(census_matrix, aes(x=as.factor(RUcat), y=percentchange, fill=as.factor(RUcat))) +
  geom_bar(stat = "identity", width=0.6) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(size=22),
        plot.title = element_text(size=28, hjust=0.5),
        axis.title = element_text(size=22)) +
  labs(x = NULL, y= "Percent Increase (%)", title = "Increase in Daily ICU Census \nDuring System Strain") +
  ylim(0, 24) +
  geom_text(aes(label = paste(format(round(percentchange, 2)), "%")), color="white", vjust=1.5, size=8)

# Visualize Census BOXPLOT
ggplot(starcherlong, aes(x = RuralUrbanCat, y= icucensus, color=RuralUrbanCat, fill=excessdeathsYN)) + 
  scale_fill_manual(values = c("white", "yellow2")) +
  guides(color = FALSE) +
  geom_boxplot(lwd=1.5) +
  scale_color_manual(values = c("Rural" = "blue", "Urban" = "red")) +
  facet_grid(.~excessdeathsYN, labeller = labeller(excessdeathsYN = labelexcess)) +
  labs(x = NULL, y= "ICU Census (per hospital, per day)", title = "Daily ICU Census") +
  theme_classic() +
  theme(
    plot.title = element_markdown(size=28, hjust=0.5),
    axis.text = element_text(size=22),
    axis.title.y = element_text(size=22),
    plot.subtitle = element_text(size=22, face="bold"),
    strip.text.x = element_text(size = 22),
    legend.position = "none") +
  ylim(0, 12)


# **NUMBER OF ICU BEDS IN USE PER DAY, ADJUSTED FOR NUMBER OF TOTAL INPATIENT BEDS**
# ICU/INPT BEDS Rural vs urban
t.test((starcherdataset$ruralicubedsavg/starcherdataset$ruralinptbeds), (starcherdataset$urbanicubedsavg/starcherdataset$urbaninptbeds))

# ICU/INPT BEDS During periods WITHOUT excess mortality
t.test((noexcess$ruralicubedsavg/noexcess$ruralinptbeds), (noexcess$urbanicubedsavg/noexcess$urbaninptbeds))

# ICU/INPT BEDS During periods WITH EXCESS MORTALITY
t.test((excess$ruralicubedsavg/excess$ruralinptbeds), (excess$urbanicubedsavg/excess$urbaninptbeds))

# ICU/INPT Beds in RURAL areas during periods of excess vs periods without excess
t.test((excess$ruralicubedsavg/excess$ruralinptbeds), (noexcess$ruralicubedsavg/noexcess$ruralinptbeds))

# ICU/INPT Beds in URBAN areas during periods of excess vs periods without excess
t.test((excess$urbanicubedsavg/excess$urbaninptbeds), (noexcess$urbanicubedsavg/noexcess$urbaninptbeds))

# PEAK SURGE PERIODS
# When viewing the plot of mortality data, it visually appears that there is a difference
# between rural and urban mortality at the peaks of the three major surge periods (the original
# surge of April 19- June 26, 2020 -- urban predominance; the first fall surge of Sept 20, 2020 - 
# Jan 23, 2021 -- rural predominance; and the Delta wave of Aug 15, 2021 - March 5, 2022 -- rural
# predominance).

# Because of this difference, we separated out analysis of these three periods to see if those
# differences were significant.

# Using change point analysis, we can identify the specific changes points for mortality trends:
library(EnvCpt)

# Rural change points
ruralfit = envcpt(starcherdataset$ruraldeathrate)
ruralfit$summary
plot(ruralfit)
ruralfit$meancpt@cpts

# Urban change points
urbanfit = envcpt(starcherdataset$urbandeathrate)
urbanfit$summary
plot(urbanfit)
urbanfit$meancpt@cpts

# This gives the following change points:
# 7  14  31  48  81 104 150
# 7  14  33  50  80  87 104 138 145 150
# This corresponds to the following dates for the 3 big surges:
# SURGE 1:
# [7] April 12, 2020
# [14] May 31, 2020

# SURGE 2:
# [31-Rural] Sept 27, 2020
# [33-Urban] Oct 11, 2020

# [48-Rural] 01-24-2021
# [50-Urban] 02-07-2021

# SURGE 3:
# [81-Rural] 09/12/2021
# [80-Urban] 09/05/2021

# [87-Urban] 10/24/2021

# [104] 2/20/2022

# Separating out the data from the 3 main surge periods 

library(dplyr)
surge1 <- starcherdataset |>
     filter(week > as.Date("2020-04-12", "%Y-%m-%d")) |>
     filter(week < as.Date("2020-05-31", "%Y-%m-%d"))

surge2 <- starcherdataset |>
  filter(week > as.Date("2020-10-11", "%Y-%m-%d")) |>
  filter(week < as.Date("2021-01-24", "%Y-%m-%d"))


surge3 <- starcherdataset |>
  filter(week > as.Date("2021-09-12", "%Y-%m-%d")) |>
  filter(week < as.Date("2022-02-20", "%Y-%m-%d"))

# **MORTALITY PER 100,000**
# Rural vs Urban - PER SURGE PERIOD
wilcox.test(surge1$ruraldeathrate, surge1$urbandeathrate)
wilcox.test(surge2$ruraldeathrate, surge2$urbandeathrate)
mean(surge2$ruraldeathrate)
mean(surge2$urbandeathrate)

wilcox.test(surge3$ruraldeathrate, surge3$urbandeathrate)

# -----CENSUS Change per surge period----- #
library(tidyverse)

#100% Missing data on Census info - Surge 1
surge1.rural <- filter(surge1, RuralUrbanCat == "Rural")
surge1.urban <- filter(surge1, RuralUrbanCat == "Urban")
surge1_matrix <- matrix(c(mean(surge1.rural$icucensus, na.rm=TRUE), mean(surge1.urban$icucensus, na.rm=TRUE), mean(noexcess$ruralicuoccupancy, na.rm=TRUE), mean(noexcess$urbanicuoccupancy, na.rm=TRUE)), nrow = 2, ncol = 2)
rownames(surge1_matrix) <- c("Rural", "Urban")
colnames(surge1_matrix) <- c("Surge", "No.Strain")

#Census change significantly higher in urban Surge 2 (p=0.004)
surge2.rural <- filter(surge2, RuralUrbanCat == "Rural")
surge2.urban <- filter(surge2, RuralUrbanCat == "Urban")
surge2_matrix <- matrix(c(mean(surge2.rural$icucensus, na.rm=TRUE), mean(surge2.urban$icucensus, na.rm=TRUE), mean(noexcess$ruralicuoccupancy, na.rm=TRUE), mean(noexcess$urbanicuoccupancy, na.rm=TRUE)), nrow = 2, ncol = 2)
rownames(surge2_matrix) <- c("Rural", "Urban")
colnames(surge2_matrix) <- c("Surge", "No.Strain")

s2.census_matrix <- data.frame(surge2_matrix)
s2.census_matrix <- mutate(s2.census_matrix, RUcat = c("Rural", "Urban"))
s2.census_matrix <- mutate(s2.census_matrix, percentchange = (Surge/No.Strain-1)*100)
s2.census_matrix <- mutate(s2.census_matrix, ratio = Surge/No.Strain)
s2.census_matrix
t.test(s2.census_matrix$ratio)

# Census change significantly increased in rural Surge 3 (p=0.068)
# 42.6% rural vs 15.0% urban
surge3.rural <- filter(surge3, RuralUrbanCat == "Rural")
surge3.urban <- filter(surge3, RuralUrbanCat == "Urban")
surge3_matrix <- matrix(c(mean(surge3.rural$icucensus, na.rm=TRUE), mean(surge3.urban$icucensus, na.rm=TRUE), mean(noexcess$ruralicuoccupancy, na.rm=TRUE), mean(noexcess$urbanicuoccupancy, na.rm=TRUE)), nrow = 2, ncol = 2)
rownames(surge3_matrix) <- c("Rural", "Urban")
colnames(surge3_matrix) <- c("Surge", "No.Strain")

s3.census_matrix <- data.frame(surge3_matrix)
s3.census_matrix <- mutate(s3.census_matrix, RUcat = c("Rural", "Urban"))
s3.census_matrix <- mutate(s3.census_matrix, percentchange = (Surge/No.Strain-1)*100)
s3.census_matrix <- mutate(s3.census_matrix, ratio = Surge/No.Strain)
s3.census_matrix
t.test(s3.census_matrix$ratio)

# Notes regarding SENSITIVITY ANALYSES:
# 1. Including only the time periods with excess mortality within these ranges made no difference
# with regards to mortality, so opted to include the whole range.

# 2. Using either both rural dates, both urban dates, earliest/latest dates, or latest/earliest dates made no
# difference with regards to mortality.  Therefore, I followed the pattern that made the surge period
# the most "narrow" interval (latest start, earliest end).

# Bed capacity and occupancy data was not analyzed within each surge period due to a high level of
# missing data.

## -- LENGTHEN SURGE DATA FOR VISUALIZATION -- #
surge1 <- pivot_longer(surge1, cols = ends_with("deathrate"), names_to = "RuralUrbanCat", values_to = "mortalityrates") |>
  select(-c("ruralpop", "ruraldeaths", "urbanpop", "urbandeaths"))

surge1$RuralUrbanCat<-as.factor(surge1$RuralUrbanCat)
surge1$RuralUrbanCat <- recode_factor(surge1$RuralUrbanCat, ruraldeathrate = "Rural", urbandeathrate = "Urban")
surge1$icubedsavg <- ifelse(surge1$RuralUrbanCat == "Rural", surge1$ruralicubedsavg, surge1$urbanicubedsavg)
surge1 <- select(surge1, -c("ruralicubedsavg", "urbanicubedsavg"))
surge1$icucensus <- ifelse(surge1$RuralUrbanCat == "Rural", surge1$ruralicuoccupancy, surge1$urbanicuoccupancy)
surge1 <- select(surge1, -c("ruralicuoccupancy", "urbanicuoccupancy"))
surge1$inptbeds <- ifelse(surge1$RuralUrbanCat == "Rural", surge1$ruralinptbeds, surge1$urbaninptbeds)
surge1 <- select(surge1, -c("ruralinptbeds", "urbaninptbeds"))
surge1 <- surge1 |> rename("excesspercent" = "excessdeath%")

surge2 <- pivot_longer(surge2, cols = ends_with("deathrate"), names_to = "RuralUrbanCat", values_to = "mortalityrates") |>
  select(-c("ruralpop", "ruraldeaths", "urbanpop", "urbandeaths"))

surge2$RuralUrbanCat<-as.factor(surge2$RuralUrbanCat)
surge2$RuralUrbanCat <- recode_factor(surge2$RuralUrbanCat, ruraldeathrate = "Rural", urbandeathrate = "Urban")
surge2$icubedsavg <- ifelse(surge2$RuralUrbanCat == "Rural", surge2$ruralicubedsavg, surge2$urbanicubedsavg)
surge2 <- select(surge2, -c("ruralicubedsavg", "urbanicubedsavg"))
surge2$icucensus <- ifelse(surge2$RuralUrbanCat == "Rural", surge2$ruralicuoccupancy, surge2$urbanicuoccupancy)
surge2 <- select(surge2, -c("ruralicuoccupancy", "urbanicuoccupancy"))
surge2$inptbeds <- ifelse(surge2$RuralUrbanCat == "Rural", surge2$ruralinptbeds, surge2$urbaninptbeds)
surge2 <- select(surge2, -c("ruralinptbeds", "urbaninptbeds"))
surge2 <- surge2 |> rename("excesspercent" = "excessdeath%")

surge3 <- pivot_longer(surge3, cols = ends_with("deathrate"), names_to = "RuralUrbanCat", values_to = "mortalityrates") |>
  select(-c("ruralpop", "ruraldeaths", "urbanpop", "urbandeaths"))

surge3$RuralUrbanCat<-as.factor(surge3$RuralUrbanCat)
surge3$RuralUrbanCat <- recode_factor(surge3$RuralUrbanCat, ruraldeathrate = "Rural", urbandeathrate = "Urban")
surge3$icubedsavg <- ifelse(surge3$RuralUrbanCat == "Rural", surge3$ruralicubedsavg, surge3$urbanicubedsavg)
surge3 <- select(surge3, -c("ruralicubedsavg", "urbanicubedsavg"))
surge3$icucensus <- ifelse(surge3$RuralUrbanCat == "Rural", surge3$ruralicuoccupancy, surge3$urbanicuoccupancy)
surge3 <- select(surge3, -c("ruralicuoccupancy", "urbanicuoccupancy"))
surge3$inptbeds <- ifelse(surge3$RuralUrbanCat == "Rural", surge3$ruralinptbeds, surge3$urbaninptbeds)
surge3 <- select(surge3, -c("ruralinptbeds", "urbaninptbeds"))
surge3 <- surge3 |> rename("excesspercent" = "excessdeath%")

# --- VISUALIZE SURGE MORTALITY DATA --- #
ggplot(subset(surge1, !is.na(mortalityrates)), aes(x=as.factor(RuralUrbanCat), y=mortalityrates, fill=as.factor(RuralUrbanCat))) +
  geom_col(position="dodge", stat="summary", fun="mean", width=0.6) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = NULL, y= "Mortality (per 100,000 residents)", title = "Surge #1 Mortality", subtitle = "Initial Surge (April 12 - May 31, 2020)") +
  theme_classic() +
  theme(
    plot.title = element_markdown(size=28, hjust=0.5),
    axis.text = element_text(size=22),
    axis.title.y = element_text(size=22),
    plot.subtitle = element_text(size=22, face="italic", hjust=0.5),
    strip.text.x = element_text(size = 22),
    legend.position = "none") +
  ylim(0, 6)

ggplot(subset(surge2, !is.na(mortalityrates)), aes(x=as.factor(RuralUrbanCat), y=mortalityrates, fill=as.factor(RuralUrbanCat))) +
  geom_col(position="dodge", stat="summary", fun="mean", width=0.6) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = NULL, y= "Mortality (per 100,000 residents)", title = "Surge #2 Mortality", subtitle = "Fall Surge (Oct 11, 2020 - Jan 24, 2021)") +
  theme_classic() +
  theme(
    plot.title = element_markdown(size=28, hjust=0.5),
    axis.text = element_text(size=22),
    axis.title.y = element_text(size=22),
    plot.subtitle = element_text(size=22, face="italic", hjust=0.5),
    strip.text.x = element_text(size = 22),
    legend.position = "none") +
  ylim(0, 6)

ggplot(subset(surge3, !is.na(mortalityrates)), aes(x=as.factor(RuralUrbanCat), y=mortalityrates, fill=as.factor(RuralUrbanCat))) +
  geom_col(position="dodge", stat="summary", fun="mean", width=0.6) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = NULL, y= "Mortality (per 100,000 residents)", title = "Surge #3 Mortality", subtitle = "Delta Wave (Sept 12, 2021 - Feb 20, 2022)") +
  theme_classic() +
  theme(
    plot.title = element_markdown(size=28, hjust=0.5),
    axis.text = element_text(size=22),
    axis.title.y = element_text(size=22),
    plot.subtitle = element_text(size=22, face="italic", hjust=0.5),
    strip.text.x = element_text(size = 22),
    legend.position = "none") +
  ylim(0, 6)
