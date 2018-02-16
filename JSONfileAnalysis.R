#Libraries--------------------------------------------------
library(rattle)# for graphic analysis
library(acepack) #required for summary
library(htmlTable) #req for summ
library(Hmisc) #rfs
library(rJava) #rfs
library(rggobi)
library(lattice)
library(ggplot2)
library(survival)
library(Formula)
library(caret)

##------------------------------------------------------------------------------------
#Load Data from JSON output based on 100 wet files from CommonCrawl searching for iPhone, Samsung, and Nokia

##---------------------------------------------------
#Correlations work for numeric variables only
crs$cor <- cor(crs$dataset[, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.
crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.
print(crs$cor)

# Graphically display the correlations.
opar <- par(cex=1)
corrplot(crs$cor, mar=c(0,0,1,0),
         method = "shade", type = "lower",
         order = "hclust", insig = "blank",
         number.cex = 3, tl.cex = 0.5, tl.srt = 45,
         title = "Iphone Correlation, Pearson Method")

par(opar)
ggcorrplot(cor(crs$cor), p.mat = cor_pmat(crs$cor), hc.order=TRUE, 
           type='lower', insig = "blank",
           tl.cex = 8)


#######################################################################
#The results were not satiisfying. Second try using Amazon Web Store.