## Problem dane są w 2 plikach csv
## należy je połączyć i wydrukować jako wykres liniowy
library("dplyr")
library("ggplot2")
library("scales")
library("ggpubr")
library("ggthemes")

spanV <- 0.25
labTxt <- "loess smoothed trend with span = 0.25 (purple line)"
#note <- "https://github.com/hrpunio/Covid19Data/tree/master/MZ_twitter (source: twitter.com/MZ_GOV_PL (via image scrapping))"
note <- "© NI-KW github.com/knsm-psw/NI-KW (source: twitter.com/MZ_GOV_PL (image scrapped))"
x.fontpt <- 6
y.fontpt <- 6
caption.font.size <- 6
title.font.size <- 10
stitle.font.size <- 6
mainBreaks <- "1 week"
mainColor <- "deeppink"
loessColor <- "deeppink"

d <- read.csv("MZ_units_beds.csv", sep = ';',  header=T, na.string="NA");

last.obs <- last (d$date)
first.obs <- first (d$date)
fstDay <- as.Date(first.obs)

maxUU <- max(d$units)
p1 <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), y=units )) + 
 geom_smooth(method="loess", se=F, span=spanV, color=loessColor) +
 geom_point(size=.4, alpha=.5) +
 xlab(label="") +
 ylab(label="units") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 scale_x_date( labels = date_format("%m/%d"), breaks = mainBreaks) +
 annotate("text", x = fstDay, y = maxUU, label = labTxt, hjust = 0, size=3, alpha=.33) +
 theme_wsj() +
  theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(axis.text.x  = element_text(size = x.fontpt ),  axis.text.y  = element_text(size = y.fontpt )) +
  theme(plot.subtitle=element_text(size=stitle.font.size, hjust=0), plot.title=element_text(size=title.font.size, hjust=0)) +
  theme( plot.caption = element_text(size = caption.font.size)) +
 ggtitle(sprintf("COVID19i: Ventillators in use (last: %s)", last.obs), subtitle=note)


maxBB <- max(d$beds)
p2 <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), y=beds )) + 
 geom_smooth(method="loess", se=F, span=spanV, color=loessColor) +
 geom_point(size=.4, alpha=.5) +
 xlab(label="") +
 ylab(label="Beds") +
 scale_x_date( labels = date_format("%m/%d"), breaks =mainBreaks) +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 annotate("text", x = fstDay, y = maxBB, label = labTxt, hjust = 0, size=3, alpha=.33) +
 theme_wsj() +
  theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(axis.text.x  = element_text(size = x.fontpt ),  axis.text.y  = element_text(size = y.fontpt )) +
  theme(plot.subtitle=element_text(size=stitle.font.size, hjust=0), plot.title=element_text(size=title.font.size, hjust=0)) +
  theme( plot.caption = element_text(size = caption.font.size)) +
 ggtitle(sprintf("COVID19: Beds occupied (last: %s)", last.obs), subtitle=note)


maxPP <- max(d$persons)
p3 <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), y=persons )) + 
 geom_smooth(method="loess", se=F, span=spanV, color=loessColor) +
 geom_point(size=.4, alpha=.5) +
 xlab(label="") +
 ylab(label="Persons") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 scale_x_date( labels = date_format("%m/%d"), breaks = mainBreaks) +
 annotate("text", x = fstDay, y = maxPP, label = labTxt, hjust = 0, size=3, alpha=.33) +
 theme_wsj() +
  theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(axis.text.x  = element_text(size = x.fontpt ),  axis.text.y  = element_text(size = y.fontpt )) +
  theme(plot.subtitle=element_text(size=stitle.font.size, hjust=0), plot.title=element_text(size=title.font.size, hjust=0)) +
  theme( plot.caption = element_text(size = caption.font.size)) +
 ggtitle(sprintf("COVID19: Currently in quarantine (last: %s)", last.obs), subtitle=note)

pp <- ggarrange(p1,p2,p3, ncol=1, nrow=3)
ggsave(pp, file='beds_and_ventillators_nikw.png', height=11)

### ### logs
fdd <- "2020-09-20"

d <- d %>% filter(as.Date(date, format="%Y-%m-%d") > fdd) %>% as.data.frame
last.obs <- last (d$date)
first.obs <- first (d$date)
fstDay <- as.Date(first.obs)

maxUU <- max(d$units)
p1 <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), y=units )) + 
 geom_smooth(method="loess", se=F, span=spanV, color=loessColor) +
 geom_point(size=.4, alpha=.5) +
 xlab(label="") +
 ylab(label="units") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 scale_x_date( labels = date_format("%m/%d"), breaks = mainBreaks) +
 annotate("text", x = fstDay, y = maxUU, label = labTxt, hjust = 0, size=3, alpha=.33) +
 theme_wsj() +
  theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(axis.text.x  = element_text(size = x.fontpt ),  axis.text.y  = element_text(size = y.fontpt )) +
  theme(plot.subtitle=element_text(size=stitle.font.size, hjust=0), plot.title=element_text(size=title.font.size, hjust=0)) +
  theme( plot.caption = element_text(size = caption.font.size)) +
 ggtitle(sprintf("COVID19i: Ventillators in use (last: %s)", last.obs), subtitle=note)


maxBB <- max(d$beds)
p2 <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), y=beds )) + 
 geom_smooth(method="loess", se=F, span=spanV, color=loessColor) +
 geom_point(size=.4, alpha=.5) +
 xlab(label="") +
 ylab(label="Beds") +
 scale_x_date( labels = date_format("%m/%d"), breaks = mainBreaks) +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
  theme_wsj() +
  theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(axis.text.x  = element_text(size = x.fontpt ),  axis.text.y  = element_text(size = y.fontpt )) +
  theme(plot.subtitle=element_text(size=stitle.font.size, hjust=0), plot.title=element_text(size=title.font.size, hjust=0)) +
  theme( plot.caption = element_text(size = caption.font.size)) +
 annotate("text", x = fstDay, y = maxBB, label = labTxt, hjust = 0, size=3, alpha=.33) +
 ggtitle(sprintf("COVID19: Beds occupied (last: %s)", last.obs), subtitle=note)

d$lunits <- log(d$units)
maxUUl <- max(d$lunits)
p1l <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), y=lunits )) + 
 geom_smooth(method="loess", se=F, span=spanV, color=loessColor) +
 geom_point(size=.4, alpha=.5) +
 xlab(label="") +
 ylab(label="ln(units)") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 scale_x_date( labels = date_format("%m/%d"), breaks = mainBreaks) +
 annotate("text", x = fstDay, y = maxUUl, label = labTxt, hjust = 0, size=3, alpha=.33) +
 theme_wsj() +
  theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(axis.text.x  = element_text(size = x.fontpt ),  axis.text.y  = element_text(size = y.fontpt )) +
  theme(plot.subtitle=element_text(size=stitle.font.size, hjust=0), plot.title=element_text(size=title.font.size, hjust=0)) +
  theme( plot.caption = element_text(size = caption.font.size)) +
 ggtitle(sprintf("COVID19i: Ventillators in use (last: %s)", last.obs), subtitle=note)

d$lbeds <- log(d$beds)
maxBBl <- max(d$lbeds)
p2l <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), y=lbeds )) + 
 geom_smooth(method="loess", se=F, span=spanV, color=loessColor) +
 geom_point(size=.4, alpha=.5) +
 xlab(label="") +
 ylab(label="log(Beds)") +
 scale_x_date( labels = date_format("%m/%d"), breaks = "2 weeks") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 annotate("text", x = fstDay, y = maxBBl, label = labTxt, hjust = 0, size=3, alpha=.33) +
 theme_wsj() +
  theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3")) +
  theme(axis.text.x  = element_text(size = x.fontpt ),  axis.text.y  = element_text(size = y.fontpt )) +
  theme(plot.subtitle=element_text(size=stitle.font.size, hjust=0), plot.title=element_text(size=title.font.size, hjust=0)) +
  theme( plot.caption = element_text(size = caption.font.size)) +
 ggtitle(sprintf("COVID19: Beds occupied (last: %s)", last.obs), subtitle=note)

ppl <- ggarrange(p1, p2, p1l,p2l, ncol=2, nrow=2)
ggsave(ppl, file='beds_and_ventillators_logs_nikw.png', width=10)
