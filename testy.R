#!/usr/bin/env Rscript
## Narodowy Instytut K-W (NI-KW)
## (mzn_sxx wersja dla NI-KW)
##
library("dplyr")
library("tidyr")
library("ggplot2")
library("scales")
library("ggpubr")
library("ggthemes")
##
pwidth <- 12
spanV <- 0.5
m1unit <- 100 ## pop in ths so 100 not 100000
##
surl <- "© NI-KW (source: http://www.wsse.gda.pl/)"
mzurl <- "© NI-KW (source: Ministerstwo Zdrowia (@MZ_GOV_PL) · Twitter)"

mainColor <- "blueviolet"
aux1Color <- "darkblue"
loessColor <- "steelblue"
mainBreaks <- "3 weeks"

# date;tt;tp;tc;omit;nt;np;nc;tc_ratio;tp_ratio
d <- read.csv("mz_testy_daily.csv", sep = ';',  header=T, na.string="NA" )
d <- d %>% filter(as.Date(date, format="%Y-%m-%d") > "2020-08-14") %>% as.data.frame

##
d$ntproc <- d$nt / first (d$nt) * 100
d$npproc <- d$np / first (d$np) * 100
d$ncproc <- d$nc / first (d$nc) * 100

d$ntproc

d$ncproc

labTxtN <- "ciągła linia = trend loess (span=0,5)"
labTxtT <- labTxtN

last.obs <- last(d$date)
first.obs <- first(d$date)
first.day <- first(as.Date(d$date))
last.day <- last(as.Date(d$date))
fd <- format(first.day, "%Y-%m-%d")
ld <- format(last.day, "%Y-%m-%d")

max.y <- max(d$tp_ratio)
w1 <- ggplot(d, aes(x= as.Date(date), y=tp_ratio)) +
 geom_point(size=1, color=mainColor, alpha=.5) +
 geom_smooth(method="loess", se=F, span=spanV, colour = mainColor, size=1) +
 ##
 geom_point(aes(y=tc_ratio), size=1, color=aux1Color, alpha=.5) +
 geom_smooth(aes(y=tc_ratio), method="loess", se=F, span=spanV, colour = aux1Color, size=1) +
 ##
 annotate("text", x = as.Date(first.obs), y=max.y, label = labTxtN, 
  vjust = 1.2, hjust = 0, size=6, color="black", alpha=.4) +
 scale_x_date( labels = date_format("%m/%d"), breaks = "2 weeks") +
 xlab(label="") +
 ylab(label="%") +
 labs(caption=sprintf("%s", mzurl) ) +
 theme_nikw() +
 ggtitle(sprintf("Współczynniki testowani/testy/zakażenia (dane dzienne / %s--%s)", fd, ld),
  subtitle="fioletowy -- zakażenia / testy wykonane ; niebieski -- zakażenia / osoby testowane (w %)" )

##w12 <- ggarrange( w1, w2, ncol=2, nrow=1)

ggsave(plot=w1, file="covid_PL_testy_01.png", width=10)

max.y <- max(d$ncproc)
w2 <- ggplot(d, aes(x= as.Date(date), y=ntproc)) +
 geom_point(size=1, color=mainColor, alpha=.5) +
 geom_smooth(method="loess", se=F, span=spanV, colour = mainColor, size=1) +
 ##
 geom_point(aes(y=ncproc), size=1, color=aux1Color, alpha=.5) +
 geom_smooth(aes(y=ncproc), method="loess", se=F, span=spanV, colour = aux1Color, size=1) +
 ##
 annotate("text", x = as.Date(first.obs), y=max.y, label = labTxtN, 
  vjust = 1.2, hjust = 0, size=6, color="black", alpha=.4) +
 scale_x_date( labels = date_format("%m/%d"), breaks = "2 weeks") +
 xlab(label="") +
 ylab(label="%") +
 labs(caption=sprintf("%s", mzurl) ) +
 theme_nikw() +
 ggtitle(sprintf("Liczba testów i osób zakażonych (dane dzienne / %s--%s)", fd, ld), 
  subtitle="fioletowy -- testy; niebieski -- zarażeni (2020-08-15 = 100%)" )

ggsave(plot=w2, file="covid_PL_testy_02.png", width=10)
