# Data source: World Health Organization
# Author: Prithul Chaturvedi <prithulc@gmail.com>
##################################################

#Importing libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Multiplot function
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##################################################

# Importing Data from source
dat <- read_csv("who_suicide_statistics.csv")
head(dat)

##################################################

# Selecting data of years after 2005
dat <- dat %>% filter(year > 2005)

##################################################
# F R A N C E
##################################################

# Getting data of France
fra <- dat %>% filter(country == "France") %>% arrange()
View(fra)

# Year-wise splitting for further statistics
fsix <- fra %>% filter(year == 2006)   %>% arrange()
fseven <- fra %>% filter(year == 2007)  %>% arrange()
feight <- fra %>% filter(year == 2008)  %>% arrange()
fnine <- fra %>% filter(year == 2009)  %>% arrange()
ften <- fra %>% filter(year == 2010)  %>% arrange()
felev <- fra %>% filter(year == 2011)  %>% arrange()
ftwel <- fra %>% filter(year == 2012)  %>% arrange()
fthir <- fra %>% filter(year == 2013)  %>% arrange()
ffour <- fra %>% filter(year == 2014)  %>% arrange()

##################################################

# Visualisations

#Population statistics
fsixp <- sum(fsix$population)
fsevenp <- sum(fseven$population)
feightp <- sum(feight$population)
fninep <- sum(fnine$population)
ftenp <- sum(ften$population)
felevp <- sum(felev$population)
ftwelp <- sum(ftwel$population)
fthirp <- sum(fthir$population)
ffourp <- sum(ffour$population)

fpop <- cbind(c(2006:2014), c(fsixp, fsevenp, feightp, fninep, ftenp, felevp,  ftwelp, fthirp, ffourp)) %>% data.frame(stringsAsFactors = T)
colnames(fpop) <- c('year', 'pop')

# Population trend
fP <- ggplot(fpop, aes(x = year, y = pop)) +
  geom_line(color = "steelblue") +  geom_point(color = "black") +
  labs(title = "Population growth of France (2006-2014)", x = "Year", y = "Population")
fP

# Suicide statistics 

# Number of suicides based on sex
fA <- ggplot(fra, aes(sex, suicides_no)) +
 ggtitle("Suicides (2006-2014)") +
 geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
 theme_minimal() +
 labs(x = "Sex", y = "Number of suicides (2006-2014)")
fA

# Number of suicides binned in age groups 
fB <- ggplot(fra, aes(age, suicides_no, fill = sex)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  theme_minimal() +
  ggtitle("Number of Suicides (2006-2014)") +
  labs(x = "Age groups", y = "Number of suicides (2006-2014)")
fB

# Year-wise suicides in France
f1 <- sum(fsix$suicides_no)
f2 <- sum(fseven$suicides_no)
f3 <- sum(feight$suicides_no)
f4 <- sum(fnine$suicides_no)
f5 <- sum(ften$suicides_no)
f6 <- sum(felev$suicides_no)
f7 <- sum(ftwel$suicides_no)
f8 <- sum(fthir$suicides_no)
f9 <- sum(ffour$suicides_no)

new_fra <- cbind(c(2006:2014),c(f1,f2,f3,f4,f5,f6,f7,f8,f9)) %>% data.frame(stringsAsFactors = T) 
colnames(new_fra) <- c("year", "total_sui")


fC <- ggplot(new_fra, aes(year, total_sui)) +
  geom_line(color = "steelblue") + geom_point(color = "black") +
  theme_minimal() + 
  ggtitle("Suicides through the years (2006-2014)") +
  labs(x = "Year", y = "Total count of suicides")
fC

# Year-wise suicides

# Getting the legend first
fplot6 <- ggplot(fsix, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2006") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

fleg <- get_legend(fplot6) %>% as_ggplot()

# Graphing
fplot6 <- ggplot(fsix, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2006") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

fplot7 <- ggplot(fseven, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2007") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

fplot8 <- ggplot(feight, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2008") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

fplot9 <- ggplot(fnine, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2009") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

fplot10 <- ggplot(ften, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2010") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

fplot11 <- ggplot(felev, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2011") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

fplot12 <- ggplot(ftwel, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2012") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

fplot13 <- ggplot(fthir, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2013") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

fplot14 <- ggplot(ffour, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2014") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

#Graph display + legend
fD <- multiplot(fplot6, fplot9, fplot12, fplot7, fplot10, fplot13, fplot8, fplot11, fplot14, cols = 3)
fleg





##################################################
# G E R M A N Y
##################################################

# Getting data of Germany
ger <- dat %>% filter(country == "Germany") %>% arrange()
View(ger)

# Year wise splitting
gsix <- ger %>% filter(year == 2006)   %>% arrange()
gseven <- ger %>% filter(year == 2007)  %>% arrange()
geight <- ger %>% filter(year == 2008)  %>% arrange()
gnine <- ger %>% filter(year == 2009)  %>% arrange()
gten <- ger %>% filter(year == 2010)  %>% arrange()
gelev <- ger %>% filter(year == 2011)  %>% arrange()
gtwel <- ger %>% filter(year == 2012)  %>% arrange()
gthir <- ger %>% filter(year == 2013)  %>% arrange()
gfour <- ger %>% filter(year == 2014)  %>% arrange()
gfif <- ger %>% filter(year == 2015) %>% arrange()

##################################################
# Visualisations

#Population statistics
gsixp <- sum(gsix$population)
gsevenp <- sum(gseven$population)
geightp <- sum(geight$population)
gninep <- sum(gnine$population)
gtenp <- sum(gten$population)
gelevp <- sum(gelev$population)
gtwelp <- sum(gtwel$population)
gthirp <- sum(gthir$population)
gfourp <- sum(gfour$population)
gfifp <- sum(gfif$population)

gpop <- cbind(c(2006:2015), c(gsixp, gsevenp, geightp, gninep, gtenp, gelevp,  gtwelp, gthirp, gfourp, gfifp)) %>% data.frame(stringsAsFactors = T)
colnames(gpop) <- c('year', 'pop')

# Population trend
gP <- ggplot(gpop, aes(x = year, y = pop)) +
  geom_line(color = "steelblue") +  geom_point(color = "black") +
  labs(title = "Population growth of Germany (2006-2015)", x = "Year", y = "Population")
gP

# Suicide statistics

# Number of suicides based on sex
gA <- ggplot(ger, aes(sex, suicides_no)) +
  ggtitle("Suicides (2006-2015)") +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  theme_minimal() +
  labs(x = "Sex", y = "Number of suicides (2006-2015)")
gA

# Number of suicides binned in age groups 
gB <- ggplot(ger, aes(age, suicides_no, fill = sex)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  theme_minimal() +
  ggtitle("Number of Suicides (2006-2015)") +
  labs(x = "Age groups", y = "Number of suicides (2006-2015)")
gB

# Year-wise suicides in France
g1 <- sum(gsix$suicides_no)
g2 <- sum(gseven$suicides_no)
g3 <- sum(geight$suicides_no)
g4 <- sum(gnine$suicides_no)
g5 <- sum(gten$suicides_no)
g6 <- sum(gelev$suicides_no)
g7 <- sum(gtwel$suicides_no)
g8 <- sum(gthir$suicides_no)
g9 <- sum(gfour$suicides_no)
g10 <- sum(gfif$suicides_no)

new_ger <- cbind(c(2006:2015),c(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10)) %>% data.frame(stringsAsFactors = T) 
colnames(new_ger) <- c("year", "total_sui")


gC <- ggplot(new_ger, aes(year, total_sui)) +
  geom_line(color = "steelblue") + geom_point(color = "black") +
  theme_minimal() + 
  ggtitle("Suicides through the years (2006-2015)") +
  labs(x = "Year", y = "Total count of suicides")
gC

# Year-wise suicides

# Getting the legend first
gplot6 <- ggplot(gsix, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2006") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

gleg <- get_legend(gplot6) %>% as_ggplot()

# Graphing

gplot6 <- ggplot(gsix, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2006") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

gplot7 <- ggplot(gseven, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2007") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

gplot8 <- ggplot(geight, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2008") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

gplot9 <- ggplot(gnine, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2009") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

gplot10 <- ggplot(gten, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2010") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

gplot11 <- ggplot(gelev, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2011") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

gplot12 <- ggplot(gtwel, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2012") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

gplot13 <- ggplot(gthir, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2013") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

gplot14 <- ggplot(gfour, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2014") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

gplot15 <- ggplot(gfif, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = 'identity') +
  labs(x = "Age", y = "Suicide count", title = "2015") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = 'none')

#Graph display + legend
gD <- multiplot(gplot6, gplot11, gplot7, gplot12, gplot8, gplot13, gplot9, gplot14, gplot10, gplot15, cols = 5)
gleg

##################################################
# COMPARITIVE STUDY

# GERMANY
gsixs <- sum(gsix$suicides_no)
gsevens <- sum(gseven$suicides_no)
geights <- sum(geight$suicides_no)
gnines <- sum(gnine$suicides_no)
gtens <- sum(gten$suicides_no)
gelevs <- sum(gelev$suicides_no)
gtwels <- sum(gtwel$suicides_no)
gthirs <- sum(gthir$suicides_no)
gfours <- sum(gfour$suicides_no)
gfifs <- sum(gfif$suicides_no)

gsui <- cbind(c("Germany","Germany","Germany","Germany","Germany","Germany","Germany","Germany","Germany","Germany"),c(2006:2015), c(gsixs, gsevens, geights, gnines, gtens, gelevs,  gtwels, gthirs, gfours, gfifs)) %>% data.frame(stringsAsFactors = T)
colnames(gsui) <- c("Country",'year', 'sui')

# FRANCE
fsixs <- sum(fsix$suicides_no)
fsevens <- sum(fseven$suicides_no)
feights <- sum(feight$suicides_no)
fnines <- sum(fnine$suicides_no)
ftens <- sum(ften$suicides_no)
felevs <- sum(felev$suicides_no)
ftwels <- sum(ftwel$suicides_no)
fthirs <- sum(fthir$suicides_no)
ffours <- sum(ffour$suicides_no)


fsui <- cbind(c("France","France","France","France","France","France","France","France","France"),c(2006:2014), c(fsixs, fsevens, feights, fnines, ftens, felevs,  ftwels, fthirs, ffours)) %>% data.frame(stringsAsFactors = T)
colnames(fsui) <- c("Country",'year', 'sui')

#Appending the data
fin_sui <- rbind(fsui,gsui) %>% data.frame(stringsAsFactors = T)
fin_sui <- fin_sui %>% group_by(Country)
View(fin_sui)
class(fin_sui$sui)

# Changing suicide number's datatype
fin_sui$sui <- as.numeric(paste(fin_sui$sui))

# Suicide trend graph
fin_sui_graph <- ggplot(data = fin_sui, aes(x  = year, y = sui, color = Country, group = Country)) +
  geom_point() + geom_line() +
  theme_minimal() + labs(x="Year",y="Suicides",title="Suicides in France(2006-2014) and Germany(2004-2015)")

fin_sui_graph

##################################################



