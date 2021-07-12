
###
library(ggplot2)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
file_OD_CO <- read.csv('OD_CO_for_R1600.csv', sep = ';')


### Scatter plot
main_scatter <- ggplot(file_OD_CO, aes(x=Status, y=OD_CO)) + geom_dotplot(binaxis='y', stackdir='center')
main_scatter + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                            geom="pointrange", color="red")

### Boxplot #1
ggplot(file_OD_CO, aes(x=Status, y=OD_CO), xlab = "Samples", ylab = "OD_CO", cex.lab = 1.5, cex.axis = 1.5) + geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.75)


### Boxplot #2
p <- ggplot(file_OD_CO, aes(x=Status, y=OD_CO)) + geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.75) + theme(text = element_text(size=15))
p + stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), geom="crossbar", width=1) + labs(x="Samples", y = 'OD_CO', tag = ) 
