#
seg.df<- read.csv("http://goo.gl/qw303p")


seg.df[, c(2, 5:7)] <- lapply(seg.df[, c(2, 5:7)], factor)

#Exploring the data
library(psych)
library(car)
library(corrplot)
library(gplots)
library(lattice)

summary(seg.df)
describe(seg.df)
str(seg.df)
pairs(seg.df)
scatterplotMatrix(seg.df)
scatterplotMatrix(formula= ~ age+income+kids, data=seg.df)

#obtaining means
mean(seg.df$income [seg.df$Segment=="Moving up" & seg.df$subscribe=="subNo"])

by(seg.df$income, seg.df$subscribe, mean)
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)

aggregate(seg.df$income, list(seg.df$Segment), mean)
aggregate(seg.df$income, list(seg.df$Segment, seg.df$subscribe), FUN=function(x)c(mean,sd))

aggregate(cbind(mean = seg.df$income, sd = seg.df$income), 
          by = list(Segment = seg.df$Segment, Subscribe = seg.df$subscribe), 
          FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(cbind(mean = seg.df$income, sd = seg.df$income), 
          by = list(Segment = seg.df$Segment, Subscribe = seg.df$subscribe), 
          FUN = function(x) {c(mean = mean(x), sd = sd(x))})

aggregate(income~Segment, data = seg.df, mean)
aggregate(income~Segment+subscribe, data = seg.df, mean)

# adding a variable with segment means

seg.income.mean <- aggregate(seg.df$income, list(seg.df$Segment), mean)
seg.income.mean
seg.df$segIncome <- seg.income.mean[seg.df$Segment, 2]

seg.income.mean[seg.df$Segment, ]
seg.income.mean[seg.df$Segment,2 ]

###5.2 Descriptives for two groups
aggregate(income~Segment+ownHome, data = seg.df, mean)
aggregate(income~Segment+ownHome+subscribe, data = seg.df, mean)
agg.data <- aggregate(income~Segment+ownHome+subscribe, data = seg.df, mean)


#to see the number of people per subgroup: 
table(seg.df$Segment, seg.df$ownHome)
with(seg.df, table(Segment, ownHome)) #another way, which temporarily attachs the database :)

with(seg.df, table(kids,Segment))

# different ways we can obtain counts (sums of values) across grouping variables

xtabs(kids~Segment, data = seg.df)
aggregate(kids~Segment, data=seg.df, sum)

seg.tab <-with(seg.df, table(kids, Segment))
seg.tab
apply(seg.tab*0:7,2,sum)

colSums (seg.tab*0:7)

#5.2.3 VISUALIZATIONS BY GROUP

# histogram by 1 factor. proportions
histogram(~subscribe | Segment, data=seg.df)

# histogram by 1 factor.counts
histogram(~subscribe | Segment, data=seg.df, type="count", layout=c(4,1), col=c("blue", "red"))
table<-with(seg.df,table(subscribe,Segment))
table
chisq.test(table)

# histogram by 2 or more factors. proportions
histogram(~subscribe | Segment + ownHome, data=seg.df)

# bar graphs from prop tables

prop<- prop.table(with(seg.df,table(subscribe, Segment)), margin=2)
prop

barchart(prop[2,],
         xlab="Proportion of Subscribed Users",
         ylab="Profile",
         col="red4")

#Y si quiero poner el porcentaje a cada barra: 

# Extraer solo la fila correspondiente a subscribe == 2 (o "Yes" según cómo esté codificado)
# Aquí asumimos que '2' significa "Yes"
subs_prop <- prop[2,]  

# Crear el gráfico con etiquetas
library(lattice)

# Crear tabla de proporciones por columna
prop <- prop.table(with(seg.df, table(subscribe, Segment)), margin = 2)

# Seleccionar proporciones de suscritos (subscribe == 2)
subs_prop <- prop[2,]

# Crear gráfico con etiquetas de porcentaje
barchart(subs_prop,
         xlab = "Proportion of Subscribed Users",
         ylab = "Segment",
         col = "red4",
         xlim = c(0, max(subs_prop) + 0.1),  # espacio para el texto
         panel = function(x, y, ...) {
           panel.barchart(x, y, ...)
           panel.text(x = x, y = y,
                      labels = paste0(round(x * 100, 1), "%"),
                      pos = 4, offset = 0.5, cex = 0.8)
         })

# Y para ordenarlo de mayor a menor porcentaje

# Reordenar subs_prop de menor a mayor
subs_prop_sorted <- subs_prop[order(subs_prop)]

# Graficar con orden
barchart(subs_prop_sorted,
         xlab = "Proportion of Subscribed Users",
         ylab = "Segment",
         col = "red4",
         xlim = c(0, max(subs_prop_sorted) + 0.1),
         panel = function(x, y, ...) {
           panel.barchart(x, y, ...)
           panel.text(x = x, y = y,
                      labels = paste0(round(x * 100, 1), "%"),
                      pos = 4, offset = 0.5, cex = 0.8)
         })

#5.2.4. VISUALIZATION BY GROUPS OF CONTINUOUS DATA

seg.mean<-aggregate(income~Segment, data=seg.df, mean)


barchart(income~Segment, data=seg.mean, col="red")

##
#for several variables
#with bar graphs
seg.income.agg<-aggregate(income ~ Segment + ownHome, data=seg.df, mean)
seg.income.agg

barchart(income~Segment, data=seg.income.agg, 
         groups= ownHome,
         auto.key=TRUE,
         par.settings=simpleTheme(col=terrain.colors(2))
        )
#
# with boxplots

boxplot(income~Segment, data=seg.df, yaxt="n", ylab="Income($k)" )
ax.seq<-seq(from=0, to =1200000, by=20000)
axis(side=2, at=ax.seq, labels=paste(ax.seq/1000, "k", sep=" "),las =1)
#using the lattice package
bwplot(Segment~income, data=seg.df, horizontal = TRUE, xlab="income")
bwplot(Segment~income | ownHome, data=seg.df, horizontal = TRUE, xlab="income")