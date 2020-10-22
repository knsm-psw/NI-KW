#!/usr/bin/env Rscript
# Tygodniowe liczby zgonów
# Tygodniowy średni wiek
library("dplyr")
library("ggplot2")
library("scales")
library("ggpubr")
library("ggthemes")
##
u1m <- 1000
spanV <- 0.25
x.fontpt <- 8
y.fontpt <- 8
caption.font.size <- 6
title.font.size <- 10
stitle.font.size <- 8
mainBreaks <- "3 weeks"
source <- "© NI-KW  (source: www.gov.pl/web/zdrowie/ + www.gov.pl/web/koronawirus/)"
mainColor <- "deeppink"
loessColor <- "deeppink"


##
## date;woj;typ;tests
testy <- read.csv("testy_wg_wojewodztw_PL.csv", sep = ';',  header=T, na.string="NA")

last.obs <- last(testy$date)
testy <- testy %>% filter (typ == 'T') %>% filter(as.Date(date, format="%Y-%m-%d") >= as.Date(last.obs)) %>% as.data.frame

#testy

#
# date;time;woj;newc;newd;totalc;totald
przypadki <- read.csv("MZN.csv", sep = ';',  header=T, na.string="NA")
p <- przypadki %>% filter (as.Date(date, format="%Y-%m-%d") == as.Date(last.obs)) %>% as.data.frame
p  <- left_join(p, testy, by = "woj")  

#
woj <- read.csv("wojewodztwa.csv", sep = ';',  header=T, na.string="NA")

p  <- left_join(p, woj, by = "woj")  

## tests, totalc, totald, pop
p$tests1m <- p$tests  / p$pop * u1m
p$totald1m <- p$totald  / p$pop * u1m
p$totalc1m <- p$totalc  / p$pop * u1m

p


## Wykresy rozrzutu  ## ###
lm <- lm(data=p, totalc1m ~ tests1m ); 
##summary(lm)
lmc <- coef(lm);
lmr <- summary(lm)$r.squared


str(lmc)

eqspec <- sprintf ("cases = %.2f tests + %.1f (R2=%1f)", lmc["tests1m"], lmc["(Intercept)"], lmr );


pf <- ggplot(p, aes(x= tests1m, y=totalc1m)) +
 geom_point(color="steelblue", size=1) +
 ##geom_smooth(method="loess", se=F, span=spanV, color=loessColor) +
 ##scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)) +
 xlab(label="tests/1mln") +
 ylab(label="cases/1mln") +
 geom_text(data=p, aes(label=sprintf("%12.12s", woj), y= totalc1m), vjust=-1.25, size=3, alpha=.4 ) +
 geom_smooth(method="lm", se=T, size=1, color=loessColor) +
 theme_wsj() +
  theme(axis.text.x  = element_text(size = x.fontpt ),  axis.text.y  = element_text(size = y.fontpt )) +
  theme(plot.subtitle=element_text(size=stitle.font.size, hjust=0), plot.title=element_text(size=title.font.size, hjust=0)) +
  theme( plot.caption = element_text(size = caption.font.size)) +
  theme( axis.title=element_text(size=9)) +
 ggtitle(sprintf ("Tests vs Cases/Poland (Total per 1mln): %s", eqspec), subtitle=source ) 

ggsave(pf, file="tests-vs-cases.png", width=10)

### -----------------------------------------------
exit

head(d, n=20)

first.obs <- first(d$date)
period <- sprintf ("%s--%s", first.obs, last.obs)

cases <- sum(d$deaths);
max.cases <- max(d$deaths)
max.age <- max(d$meanage)

note <- sprintf ("As %% of weekly | source: twitter.com/MZ_GOV_PL)")

pf <- ggplot(d, aes(x= as.Date(date), y=weeklyc.p)) +
 geom_point(color="steelblue", size=1) +
 geom_line(color="steelblue", size=1) +
 scale_x_date( labels = date_format("%m/%d"), breaks = "2 weeks") +
 #scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)) +
 xlab(label="week") +
 ylab(label="mean age") +
 #coord_cartesian(ylim = c(0, max.age)) +
 ggtitle(sprintf ("Mean no of COVID19 cases/Poland/%s", period), subtitle=note ) 

note <- sprintf ("As %% of weekly | source: twitter.com/MZ_GOV_PL)")
pg <- ggplot(d, aes(x= as.Date(date), y=weeklyd.p)) +
 geom_point(color="steelblue", size=1) +
 geom_line(color="steelblue", size=1) +
 scale_x_date( labels = date_format("%m/%d"), breaks = "2 weeks") +
 #scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)) +
 xlab(label="week") +
 ylab(label="mean age") +
 #coord_cartesian(ylim = c(0, max.age)) +
 ggtitle(sprintf ("Mean no of COVID19 fatalities/Poland/%s", period), subtitle=note ) 

plot00 <- ggarrange(pf, pg, ncol=2, nrow=1 )
ggsave(plot=plot00, file="c19PL_daily_shares.png", width=12 )

####
e <- d %>% group_by(dow) %>% summarize( xc=mean(weeklyc.p), xd=mean(weeklyd.p))

str(e)

p.cp <- ggplot(e, aes(x = dow, y = xc )) +
    ggtitle("COVID19: Daily cases by day of week", subtitle="as %% of weekly total") +
    xlab("dow") + ylab("%") +
    geom_hline(yintercept = 100/7, color='orange') +
    scale_x_continuous (labels=c("Po", "Wt", "Sr", "Cz", "Pn", "So", "Nd"),
          breaks=c(1, 2, 3, 4, 5, 6, 7)) +
    geom_bar(position = 'dodge', stat = 'identity', fill="steelblue" )


p.dp <- ggplot(e, aes(x = dow, y = xd )) +
    ggtitle("COVID19: Daily deaths by day of week", subtitle="as %% of weekly total") +
    xlab("dow") + ylab("%") +
    geom_hline(yintercept = 100/7, color='orange') +
    scale_x_continuous (labels=c("Po", "Wt", "Sr", "Cz", "Pn", "So", "Nd"),
          breaks=c(1, 2, 3, 4, 5, 6, 7)) +
    geom_bar(position = 'dodge', stat = 'identity', fill="steelblue" )


plot11 <- ggarrange(p.cp, p.dp, ncol=2, nrow=1 )
ggsave(plot=plot11, file="c19PL_daily_shares_proc.png", width=12 )
