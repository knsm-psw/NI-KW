#!/usr/bin/env Rscript
#
library("ggplot2")
library("dplyr")
library("scales")
library("ggthemes")
library("ggpubr")
#
options(scipen = 999)

note <- "© NI-KW @ github.com/knsm-psw/NI-KW"
##
theme_nikw <- function(){
 theme_wsj() %+replace%
 theme(
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "cornsilk3"),
  ##
  axis.text.x  = element_text(size = 6 ),
  axis.text.y  = element_text(size = 6 ),
  ## margin=margin(r=, t=, b = 3, l=, unit="pt")
  plot.title=element_text(family="sans", size=14, hjust=0, margin=margin(b = 3, unit="pt")),
  plot.subtitle=element_text(family="sans", size=8, hjust=0),
  legend.title=element_text(family="sans", size=8),
  plot.caption = element_text(family="sans", size = 6)
  )
}
## https://stackoverflow.com/questions/34444295/how-to-specify-does-not-contain-in-dplyr-filter
`%notin%` = function(x,y) !(x %in% y)
##
today <- Sys.Date()
tt <- format(today, "%Y-%m-%d")
spanV <- 0.25
mainBreaks <- "4 weeks"
mainColor <- "deeppink"
loessColor <- "deeppink"
##
## Przed/po \n musi być odstęp inaczej nie działa
## url <- "https://www.ecdc.europa.eu/en/publications-data/ \n download-todays-data-geographic-distribution-covid-19-cases-worldwide"
url <- "www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"
surl <- sprintf ("retrived from %s", url)

## info o wielkości populacji i kontynencie
c <- read.csv("ecdc_countries.csv", sep = ';',  header=T, na.string="NA" )
## dane dzienne/cały świat (ECDC)
d <- read.csv("covid19_C.csv", sep = ';',  header=T, na.string="NA", 
   colClasses = c('factor', 'factor', 'factor', 'character', 'character', 'numeric', 'numeric'));
##
d <- left_join(d, c, by = "id")
# czyszczenie
# tylko kraje z ID
d <- d %>% filter(!is.na(id))
# zamień na liczby (był problem z czytaniem CSV bezpośrednio)
d$newc <- as.numeric(d$newc)
d$newd <- as.numeric(d$newd)

# Dane bezsensowne zamień na NA
# Zdarzają się ujemne bo kraje zmieniają klasyfikację/przeliczają
d$newc[ (d$newc < 0) ] <- NA
d$newd[ (d$newd < 0) ] <- NA

## zgony na 1mln
d$tot1m <- d$totald/d$popData2019 * 1000000
## Oblicz sumy dla kontynentów
## Kontynentów jest więcej niż 5:-) w zbiorze (jakieś śmieci)
continents <- c('Africa', 'America', 'Asia', 'Europe', 'Oceania')

## ogółem wg 
cont <- d %>% filter (continent %in% continents) %>% group_by(date,continent) %>% summarise(
  tc = sum(newc, na.rm=TRUE),
  td = sum(newd, na.rm=TRUE),
  tc1m = sum(newc, na.rm=TRUE)/sum(popData2019, na.rm=TRUE) * 1000000,
  td1m = sum(newd, na.rm=TRUE)/sum(popData2019, na.rm=TRUE) * 1000000
  ) %>% as.data.frame
## str(tc)

## Z ciekawości
noncont <- d %>% filter (continent %notin% continents) %>% as.data.frame
print(noncont)

pd1 <- ggplot(cont, aes(x= as.Date(date, format="%Y-%m-%d"), color = continent, y=tc)) +
 geom_point(aes(y=tc, color = continent), size=.4, alpha=.5) +
 geom_smooth(method="loess", se=F, span=spanV, aes(fill=continent, y=tc, group=continent)) +
 xlab(label="") + ylab(label="cases") + 
 scale_x_date( labels = date_format("%m/%d"), breaks = mainBreaks) +
 theme_nikw() +
 labs(caption=note) +
 ggtitle(sprintf ("COVID19: cases (%s)", last.obs), subtitle=sprintf("%s", surl))

pd2 <- ggplot(cont, aes(x= as.Date(date, format="%Y-%m-%d"), , color = continent, y=td)) +
 geom_point(aes(y=td, color = continent), size=.4, alpha=.5) +
 geom_smooth(method="loess", se=F, span=spanV, aes(fill=continent, y=td, group=continent)) +
 xlab(label="") + ylab(label="deaths") + 
 scale_x_date( labels = date_format("%m/%d"), breaks = mainBreaks) +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 theme_nikw() +
 labs(caption=note) +
 ggtitle(sprintf ("COVID19: deaths (%s)", last.obs), subtitle=sprintf("%s", surl))

ggsave(plot=pd1, file="tc_by_c.png", width=10)
ggsave(plot=pd2, file="td_by_c.png", width=10)

pd1m <- ggplot(cont, aes(x= as.Date(date, format="%Y-%m-%d"), color = continent, y=tc1m)) +
 geom_point(aes(y=tc1m, color = continent), size=.4, alpha=.5) +
 geom_smooth(method="loess", se=F, span=spanV, aes(y=tc1m )) +
 xlab(label="") + ylab(label="cases") + 
 scale_x_date( labels = date_format("%m/%d"), breaks = mainBreaks) +
 theme_nikw() +
 labs(caption=note) +
 ggtitle(sprintf ("COVID19: cases per 1mln (%s)", last.obs), subtitle=sprintf("%s", surl))

pd2m <- ggplot(cont, aes(x= as.Date(date, format="%Y-%m-%d"), color = continent, y=td1m)) +
 geom_point(aes(y=td1m, color = continent), size=.4, alpha=.5) +
 geom_smooth(method="loess", se=F, span=spanV, aes(y=td1m )) +
 xlab(label="") + ylab(label="deaths") + 
 scale_x_date( labels = date_format("%m/%d"), breaks = mainBreaks) +
 theme_nikw() +
 labs(caption=note) +
 ggtitle(sprintf ("COVID19: deaths per 1mln(%s)", last.obs), subtitle=sprintf("%s", surl))

ggsave(plot=pd1m, file="tc1m_by_c.png", width=10)
ggsave(plot=pd2m, file="td1m_by_c.png", width=10)

## koniec ##