

#####
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
library(corrplot)

NP_corr <- read.csv('NP_corr.csv', sep = ';')
NCBI_antigens_corr <- read.csv("NCBI_antigens_corr.csv", sep = ';')

df_corr <- cbind(NP_corr, NCBI_antigens_corr)
colnames(df_corr) <- c("pure NP", "NP-RNA", 'S1', 'RBD', 'S1/S2', 'NP #1', 'NP #2')

###
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt, cex = 1.1, font = 4)
}

reg <- function(x, y, ...) {
  points(x,y, ...)
  abline(lm(y~x)) 
}


###
pairs(df_corr, upper.panel = reg, # replace HERE for panel.smooth #
      cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 1.5, font.labels = 2, lower.panel = panel.cor)


