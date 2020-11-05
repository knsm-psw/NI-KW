library("dplyr")
library("ggplot2")
library("scales")
library("ggpubr")

spanV <- 0.25
fdd <- "2020-04-30"  ## pomiń kwiecień (9 obserwacji)

d <- read.csv("pomorskie_zgony_d.csv", sep = ';',  header=T, na.string="NA")
i <- read.csv("pomorskie_zgony_i.csv", sep = ';',  header=T, na.string="NA")

d <- d %>% filter(as.Date(date, format="%Y-%m-%d") > fdd) %>% as.data.frame
i <- i %>% filter(as.Date(date, format="%Y-%m-%d") > fdd) %>% as.data.frame
str(i)

first <- first(d$date)
last <- last(d$date)
period <- sprintf ("%s--%s", first, last)
max.age <- max(d$meanagec)
cases <- sum(d$nn);

note <- sprintf ("źródło: twitter.com/MZ_GOV_PL do 9.10 | komunikaty http://www.wsse.gda.pl/ od 9.10 ")

pa <- ggplot(d, aes(x= as.Date(date), y=meanagec)) +
 geom_point(size=3, color='red', alpha=0.4) +
 ##geom_line(color="steelblue", size=.4) +
 geom_smooth(method="loess", se=F, span=spanV, color="red") +
 geom_smooth(aes(y=nn), method="loess", se=F, span=spanV, color="darkgreen") +
 geom_point(aes(y=nn), size=3, color='green', alpha=0.4) +
 geom_point(data=i, aes(x=as.Date(date), y=age), 
    size=.4, color='red', alpha=0.4) +
 annotate("text", x = as.Date(first), y=85, label = sprintf ("line: loess with %.2f span", spanV), 
  vjust = -1.2, hjust = 0, size=6, alpha=.3) +
 scale_x_date( labels = date_format("%m/%d"), breaks = "2 weeks") +
 xlab(label="") +
 ylab(label="wiek (czerwony)/zmarli (zielony)") +
 coord_cartesian(ylim = c(0, 90)) +
 scale_y_continuous(breaks=seq(0, 90, by=5)) +
 theme_nikw() +
 ggtitle(sprintf ("COVID19: kumulowany średni wiek zmarłych\noraz liczba zmarłych (Pomorskie / %s)", period), subtitle=note ) 


ggsave(plot=pa, file="pomorskie_zgony.png")
