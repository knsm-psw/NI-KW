#!/usr/bin/env Rscript
#
library("ggplot2")
library("dplyr")
library("ggthemes")
#
#
## Space before/after \n otherwise \n is ignored
url <- "https://www.ecdc.europa.eu/en/publications-data/ \n download-todays-data-geographic-distribution-covid-19-cases-worldwide"
surl <- sprintf ("retrived %s from %s", tt, url)
  x.fontpt <- 6
y.fontpt <- 6
caption.font.size <- 6
title.font.size <- 10
stitle.font.size <- 6
mainBreaks <- "1 week"
mainColor <- "deeppink"
loessColor <- "deeppink"

## population/continent data:
c <- read.csv("ecdc_countries_names.csv", sep = ';',  header=T, na.string="NA" )

## COVID data
d <- read.csv("covid19_C.csv", sep = ';',  header=T, na.string="NA", 
   colClasses = c('factor', 'factor', 'factor', 'character', 'character', 'numeric', 'numeric'));

today <- as.Date(last(d$date))
# 14 days ago
first.day <- today - 14
tt<- format(today, "%Y-%m-%d")
fd<- format(first.day, "%Y-%m-%d")

##t <- d %>% group_by(id) %>%  top_n(1, date) %>% as.data.frame
## top_n jest obsolete w nowym dplyr
t <- d %>% group_by(id) %>%  slice_tail(n=1) %>% as.data.frame
## tylko kraje gdzie zmarło 100 i więcej
t <- t %>% select(id, totald) %>% rename(alld = totald) %>% as.data.frame
d <- left_join(d, t, by = "id")

d <- d %>% filter (alld > 99 ) %>% as.data.frame

## OECD membership data
o <- read.csv("OECD-list.csv", sep = ';',  header=T, na.string="NA" )

d$newc <- as.numeric(d$newc)
d$newd <- as.numeric(d$newd)

cat ("### Cleaning newc/newd: assign NA to negatives...\n")
d$newc[ (d$newc < 0) ] <- NA
d$newd[ (d$newd < 0) ] <- NA

## Filter last 14 days only ## ## ##
d <- d %>% filter(as.Date(date, format="%Y-%m-%d") > fd) %>% as.data.frame
## Last day
last.obs <- last(d$date)
## Format lable firstDay--lastDay (for title):
last14 <- sprintf ("%s--%s", fd, last.obs)

## Sum-up cases and deaths
t <- d %>% group_by(id) %>% summarise( tc = sum(newc, na.rm=TRUE), td = sum(newd, na.rm=TRUE)) %>% as.data.frame 
## Add country populations
t <- left_join(t, c, by = "id")
## Add OECD membership
## if membership column is not NA = OECD member
t <- left_join(t, o, by = "id") 

t$access[ (t$access > 0) ] <- 'oecd'
t$access[ (is.na(t$access)) ] <- 'non-oecd'

## Deaths/Cases ratio %%
t$totr <- t$td/t$tc * 100
## Deaths/1mln
t$tot1m <- t$td/t$popData2019 * 1000000
## Cases/1mln
t$totc1m <- t$tc/t$popData2019 * 1000000

## Drop countries with population < 5 mln
## t <- t %>% filter (popData2019 > 5000000 ) %>% as.data.frame
## Drop countries with cases < 14 (1/day)
## t <- t %>% filter (tc > 13 ) %>% as.data.frame

## PL row
dPL <- t[t$id=="PL",]
##str(dPL)
## Extract ratios for PL
totr.PL <- dPL$totr
totr.PL
## Deaths/cases
tot1m.PL <- dPL$tot1m
totc1m.PL <- dPL$totc1m

## Set scales
pScale <- seq(0,max(t$totr, na.rm=T), by=1)
mScale <- seq(0,max(t$tot1m, na.rm=T), by=10)

## Graphs
## 1. deaths/cases ratio (dot-plot)
## color=as.factor(access)): draw OECD/nonOECD with separate colors
p1 <- ggplot(t, aes(x = reorder(country, totr) )) +
  ### One group/one color:
  ##geom_point(aes(y = totr, color='navyblue'), size=1) +
  ### Groups with different colors:
  geom_point(aes(y = totr, color=as.factor(access)), size=1) +
  xlab("country") +
  ylab("%") +
  ggtitle(sprintf("Covid19 death cases ratio (%s)", last14),
    subtitle="Only countries with 100 deaths and more | pale red line = PL level") +
  theme(axis.text = element_text(size = 4)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ##theme(legend.position="none") +
  scale_color_discrete(name = "Membership") +
  ## Indicate PL-level of  d/c ratio:
  geom_hline(yintercept = totr.PL, color="red", alpha=.25, size=1) +
  labs(caption=surl) +
  scale_y_continuous(breaks=pScale) +
  ##coord_flip(ylim = c(0, 15))
   theme_wsj() +
  theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(axis.text.x  = element_text(size = x.fontpt ),  axis.text.y  = element_text(size = y.fontpt )) +
  theme( plot.subtitle=element_text(size=stitle.font.size, hjust=0), legend.title=element_text(size=8),
      plot.title=element_text(size=title.font.size, hjust=0)) +
  theme( plot.caption = element_text(size = caption.font.size)) +
  coord_flip()

ggsave(plot=p1, file="deaths_cases_ratio_14.png", height=12)

## 2. deaths/cases ratio (histogram)
#p2 <- ggplot(t, aes(x = totr) ) + 
# geom_histogram(binwidth = 0.5, fill='navyblue', alpha=.5) +
# ###
# ylab("N") +
# xlab("%") +
# ggtitle(sprintf("Covid19 deaths/cases ratio (%s)", last14),
#   subtitle="Only countries with 100 deaths and more | pale red line = PL level") +
# scale_x_continuous(breaks=pScale) +
# scale_y_continuous(breaks=seq(0, 40, by=2)) +
# geom_vline(xintercept = totr.PL, color="red", alpha=.25, size=1) +
# ##coord_cartesian(ylim = c(0, 30), xlim=c(0, 8))
# labs(caption=surl)
#
#ggsave(plot=p2, file="totr_h14.png", width=10)

## 3. deaths/1m (dot-plot)
## color=as.factor(continent)): draw continents with separate colors
p3 <- ggplot(t, aes(x = reorder(country, tot1m) )) +
  geom_point(aes(y = tot1m, color=as.factor(continent)), size =1 ) +
  xlab("") +
  ylab("deaths") +
  ggtitle(sprintf("Covid19 deaths per 1 million (%s)", last14),
    subtitle="Only countries with 100 deaths and more | red pale line = PL level") +
  theme(axis.text = element_text(size = 6)) +
  geom_hline(yintercept = tot1m.PL, color="red", alpha=.25, size=1) +
  labs(caption=surl) +
  scale_color_discrete(name = "Continent") +
  scale_y_continuous(breaks=mScale) +
 theme_wsj() +
  theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(axis.text.x  = element_text(size = x.fontpt ),  axis.text.y  = element_text(size = y.fontpt )) +
  theme( plot.subtitle=element_text(size=stitle.font.size, hjust=0), legend.title=element_text(size=8),
      plot.title=element_text(size=title.font.size, hjust=0)) +
  theme( plot.caption = element_text(size = caption.font.size)) +
  coord_flip()

ggsave(plot=p3, file="deaths_per_1mln_14.png", height=12)

##
##
p3c <- ggplot(t, aes(x = reorder(country, totc1m) )) +
  geom_point(aes(y = totc1m, color=as.factor(continent)), size =1 ) +
  xlab("") +
  ylab("deaths") +
  ggtitle(sprintf("Covid19 cases per 1 million (%s)", last14),
    subtitle="Only countries with 100 deaths and more | red pale line = PL level") +
  theme(axis.text = element_text(size = 6)) +
  geom_hline(yintercept = totc1m.PL, color="red", alpha=.25, size=1) +
  labs(caption=surl) +
  scale_color_discrete(name = "Continent") +
  ##scale_y_continuous(breaks=mScale) +
   theme_wsj() +
  theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(axis.text.x  = element_text(size = x.fontpt ),  axis.text.y  = element_text(size = y.fontpt )) +
  theme( plot.subtitle=element_text(size=stitle.font.size, hjust=0), legend.title=element_text(size=8),
      plot.title=element_text(size=title.font.size, hjust=0)) +
  theme( plot.caption = element_text(size = caption.font.size)) +
  coord_flip()

ggsave(plot=p3c, file="cases_per_1mln_14.png", height=12)

##
## 3. deaths/1m (box-plots for continents)
p4 <- ggplot(t, aes(x=as.factor(continent), y=tot1m, fill=as.factor(continent))) + 
 geom_boxplot() +
 ylab("deaths") +
 xlab("continent") +
 ggtitle(sprintf("Covid19 deaths per 1 million (%s)", last14),
    subtitle="Only countries with 100 deaths and more") +
 labs(caption=surl) +
 scale_y_continuous(breaks=mScale) +
 theme(axis.text = element_text(size = 6)) +
 theme(legend.position="none") +
  theme_wsj() +
  theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(axis.text.x  = element_text(size = x.fontpt ),  axis.text.y  = element_text(size = y.fontpt )) +
  theme( plot.subtitle=element_text(size=stitle.font.size, hjust=0), legend.title=element_text(size=8),
      plot.title=element_text(size=title.font.size, hjust=0)) +
  theme( plot.caption = element_text(size = caption.font.size))

ggsave(plot=p4, file="deaths_per_1mln_continents_c14.png", width=10)

##
ts <-  t %>% group_by(as.factor(continent)) %>% 
    summarise(Mean=mean(tot1m), Median=median(tot1m), Std=IQR(tot1m))

ts
