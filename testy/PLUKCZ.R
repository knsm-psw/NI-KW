#!/usr/bin/env Rscript
#
#
library("ggplot2")
library("dplyr")
library("ggthemes")
library("scales")
library("ggpubr")

spanV <- 0.1
mainBreaks <- "2 weeks"
note <- "© NI-KW @ github.com/knsm-psw/NI-KW/testy/"

d <- read.csv("PLUKCZ_data.csv", sep = ';',  header=T, na.string="NA" );
d <- d %>% filter(as.Date(date, format="%Y-%m-%d") > "2020-08-01") %>% as.data.frame
c <- read.csv("ecdc_countries.csv", sep = ';',  header=T, na.string="NA")

d <- left_join(d, c, by="id")

## Wartości odstające usuwamy
d$nt[ (d$nt <= 0) ] <- NA

d$nt_pm <- d$nt/d$popData2019 * 1000000

print(d$nt_pm)

str(d)

pd1m <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), color = id, y=nt_pm )) +
 geom_line(aes(y=nt_pm, color = id), size=.4, alpha=.5) +
 geom_point(aes(y=nt_pm, color = id), size=.8, alpha=.5) +
 ##geom_smooth(method="loess", se=F, span=spanV, aes(y=nt )) +
 xlab(label="") +
 scale_x_date( labels = date_format("%m/%d"), breaks = mainBreaks) +
 theme_nikw() +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 labs(caption=note) +
 ggtitle("Liczba testów / 1 mln ", subtitle="")

ggsave(plot=pd1m, file="testy1.png", width=10)

####

d$ncnt <- d$nc/d$nt * 100

pd2m <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), color = id, y=ncnt )) +
 geom_line(aes(y=ncnt, color = id), size=.4, alpha=.5) +
 geom_point(aes(y=ncnt, color = id), size=.8, alpha=.5) +
 ##geom_smooth(method="loess", se=F, span=spanV, aes(y=nt )) +
 xlab(label="") +
 theme_nikw() +
 labs(caption=note) +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle("Współczynnik zarażeni/testy (%)", subtitle="")

ggsave(plot=pd2m, file="testy2.png", width=10)

t <- d %>% group_by(id) %>%  summarise(CC = max(nc, na.rm=T), DD=max(nd, na.rm=T), TT=max(nt, na.rm=T))

str(t)

d <- left_join(d, t, by="id")

d$nc1 <- d$nc/d$CC * 100
d$nd1 <- d$nd/d$DD * 100
d$nt1 <- d$nt/d$TT * 100

pd3m <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), color = id, y=ncnt )) +
 geom_line(aes(y=ncnt, color = id), size=.4, alpha=.5) +
 geom_point(aes(y=ncnt, color = id), size=.8, alpha=.5) +
 #geom_line(aes(y=nc1, color = id), size=.4, alpha=.5) +
 geom_smooth(aes(y=nc1), method="loess", se=F, span=spanV, size=1 ) +
 xlab(label="") +
 theme_nikw() +
 labs(caption=note) +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle("Współczynnik zarażeni/testy (%) a zarażeni*", 
  subtitle="* % wartości maksymalnej liczby zarażonych w analizowanym okresie (gruba linia)")

ggsave(plot=pd3m, file="testy3.png", width=10)

###
### Tylko Polska ###
###
d <- d %>% filter(id == "PL") %>% as.data.frame


pd4m <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d") )) +
 geom_line(aes(y=nc), size=.4, alpha=.5) +
 geom_point(aes(y=nc), size=.8, alpha=.5) +
 xlab(label="") +
 theme_nikw() +
 labs(caption=note) +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle("Liczba zarażonych (%)", subtitle="")

pd5m <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d") )) +
 geom_line(aes(y=nd), size=.4, alpha=.5) +
 geom_point(aes(y=nd), size=.8, alpha=.5) +
 xlab(label="") +
 theme_nikw() +
 labs(caption=note) +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle("Liczba zmarłych", subtitle="")

pd6m <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d") )) +
 geom_line(aes(y=nt), size=.4, alpha=.5) +
 geom_point(aes(y=nt), size=.8, alpha=.5) +
 xlab(label="") +
 theme_nikw() +
 labs(caption=note) +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle("Liczba testów", subtitle="")

pd7m <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d") )) +
 geom_line(aes(y=ncnt), size=.4, alpha=.5) +
 geom_point(aes(y=ncnt), size=.8, alpha=.5) +
 xlab(label="") +
 theme_nikw() +
 labs(caption=note) +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle("Współczynnik zarażeni/testy (%)", subtitle="")

pd00 <- ggarrange(pd4m,pd5m, pd6m, pd7m, ncol=2, nrow=2)
ggsave(plot=pd00, file="testy4.png", width=10)

