library("dplyr")
library("ggplot2")
library("scales")
library("ggpubr")
##
pwidth <- 12
spanV <- 0.5
x.fontpt <- 6
## Pop w pliku wojewodztwa.csv jest w mln
unit1m <- 1000000
##
surl <- "https://github.com/hrpunio/Covid19Data/tree/master/MZ_twitter (source: twitter.com/MZ_GOV_PL (via image scrapping))"
mzurl <- "https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2"

d <- read.csv("testy_wg_wojewodztw_PL.csv", sep = ';',  header=T, na.string="NA" )
d$yymmdd <- as.Date(d$date, format="%Y-%m-%d")

d.new <- d %>% filter (typ =="N") %>% as.data.frame

str(d.new)

labTxtC <- "cases/day (loess smoothing with span = 0.5)"
labTxtD <- "deaths/day (loess smoothing with span = 0.5)"
last.obs <- last(d.new$date)
first.obs <- first(d.new$date)


##
wp0 <- ggplot(d.new, aes(x= yymmdd, y=tests)) + 
 geom_smooth(method="loess", se=F, span=spanV, colour = 'steelblue', size=.4) +
 geom_point(size=.4, color='steelblue', alpha=.5) +
 facet_wrap( ~woj, scales = "free_y") +
 xlab(label="") +
 scale_x_date( labels = date_format("%m/%d"), breaks = "3 weeks") +
 theme(axis.text.x  = element_text(size = x.fontpt )) +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 labs(title = sprintf("COVID19 total test (%s)", last.obs), 
      subtitle = sprintf("Source: %s",  surl), y = labTxtC, x = "")

ggsave(wp0, file="tests_weekly.png", width=pwidth)

## per 1mln/pop
w <- read.csv("wojewodztwa.csv", sep = ';',  header=T, na.string="NA" )
d <- left_join(d, w, by = "woj")
str(d)

d.total <- d %>% filter (typ == "T") %>% as.data.frame
d.total$tests1m <- d.total$tests / d.total$pop 

d.total

d.total.woj <- d.total %>% group_by(woj) %>%  slice_tail(n=1) %>% as.data.frame

d.total.woj


## global / od początku pandemii
wp1 <- ggplot(data=d.total.woj, aes(x=reorder(as.factor(woj), tests1m), y=tests1m )) +
  geom_bar(stat="identity", position=position_dodge(width=.4), width=.8,  fill="maroon", alpha=.5) +
  geom_text(aes(label=sprintf("%.1f", tests1m), y= tests1m), hjust=-.25, size=3, color="#400000" ) +
  theme(legend.position="top") +
  ylab(label="") +
  xlab(label="") +
  coord_flip() +
  ##labs(caption=sprintf("Data harvested daily from %s", mzurl)) +
  theme( plot.caption = element_text(size = 6)) +
  ggtitle(sprintf("PL: COVID19 tests per 1mln (%s)", last.obs), subtitle = sprintf("Source: %s",  surl) )

ggsave(wp1, file="tests_totals.png", width=pwidth)
