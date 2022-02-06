#1 
print("Sai Supreet Chivukula")
install.packages('FSA')
library('FSA')
install.packages('FSAdata')
library('FSAdata')
install.packages('magrittr')
library('magrittr')
install.packages('plyr')
library('plyr')
install.packages('dplyr')
library('dplyr')
install.packages('tidyr')
library('tidyr')
install.packages('tidyverse')
library('tidyverse')
#2
read.csv("inchBio.csv")
bio <- read.csv("inchBio.csv")
#3
headtail(bio)
str(bio)
#4
counts<- aggregate(bio$species, by= list(bio$species), FUN= length)
colnames(counts) <- c("Species", "Count")
counts
#5
unique(bio$species)
#6
tmp <- table(bio$species)
tmp
#7
tmp2 <- subset(bio,select = species)
head(tmp2, 5)
#8
w <- table(bio$species)
w
class(w)
#9
t <- as.data.frame(w)
class(t)
#10
t$Freq
#11
cSpec <- table(bio$species)
cSpec
class(cSpec)
#12
cSpecPct <- prop.table(cSpec)
is.table(cSpecPct)
print(cSpecPct)
#13
u <- data.frame(cSpecPct)
is.data.frame(u)
print(u)
#14
barplot(cSpec, ylim = c(0,250), ylab = "counts", main="Fish Count", 
        las = 1, col = "#90EE90", cex.names = .60) 
#15
barplot(cSpecPct, ylim = c(0,4), 
        main = "Fish Relative Frequency", 
        ylab = "Frequency (Count/Total)", 
        col = "lightblue", 
        las = 1, 
        cex.names = .60)
#16
d <- arrange(u,desc(Freq))
print(d)
#17
names(d)[1] <- "Species"
names(d)[2] <- "RelFreq"
print(d)
#18
d <- mutate(d, cumfreq = cumsum(RelFreq), counts= t$Freq, 
            cumcounts = cumsum(t$Freq))
print(d)
#19
def_par <- par() 
def_par
#20
par(mar=c(8,5,2,5))
pc <- barplot(d$counts,
              width = 1,
              space = .15,
              border = NA,
              axes = F,
              ylim = c(0,(3.05*max(d$counts,na.rm = T))),
              ylab = "Cumulative counts",
              names.arg = d$Species,
              cex.names = 0.70,
              main = "Species Pareto",
              las = 2)

#21
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")

#22 #23 #24
box(col = "grey62")

axis(side = 2, 
     at = c(0, d$cumcounts), 
     las = 1, 
     col.axis = "grey62", 
     col = "grey62", 
     cex.axis = 0.8)
axis(side = 4, 
     at = c(0, d$cumcounts), 
     labels = paste(c(0, round(d$cumfreq * 100)) ,
                    "%",
                    sep=""), 
     las = 1, 
     col.axis = "cyan4", 
     col = "cyan4", 
     cex.axis = 0.8)
legend("topleft", c("Chivukula"))
