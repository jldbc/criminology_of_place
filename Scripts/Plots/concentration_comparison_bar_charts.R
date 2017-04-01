library(reshape2)
library(cowplot)
setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/")
df1 = read.csv("concentration_comparisons_violent2.csv", stringsAsFactors=FALSE)
df2 = read.csv("concentration_comparisons_all2.csv", stringsAsFactors=FALSE)
df1[df1$X=='100pct', 'X'] = '100'
df1[df1$X=='50pct', 'X'] = '50'
df1[df1$X=='25pct', 'X'] = '25'

df2[df2$X=='100pct', 'X'] = '100'
df2[df2$X=='50pct', 'X'] = '50'
df2[df2$X=='25pct', 'X'] = '25'

### for violent crime
# melt the data frame for plotting
data.m <- melt(df1, id.vars='X')
data.m$X = factor(data.m$X, levels = c("100", "50", "25"))
# plot everything
ggplot(data.m, aes(variable, value)) +   
  geom_bar(aes(fill = X), position = "dodge", stat="identity") + labs(x = "", y="Percentage of Street Segments", 
          fill="Percentage of \nCrime Explained") + scale_y_continuous(limits = c(0,65)) 
                                                              


### for all crime 
# melt the data frame for plotting
data.m <- melt(df2, id.vars='X')
data.m$X = factor(data.m$X, levels = c("100", "50", "25"))
# plot everything
ggplot(data.m, aes(variable, value)) +   
  geom_bar(aes(fill = X), position = "dodge", stat="identity") + labs(x = "", y="Percentage of Street Segments", 
          fill="Percentage of \nCrime Explained") + scale_y_continuous(limits = c(0,65)) 





##### trying the same plots w/o the 100% level

df1 = read.csv("concentration_comparisons_violent2.csv", stringsAsFactors=FALSE)
df2 = read.csv("concentration_comparisons_all2.csv", stringsAsFactors=FALSE)
df1[df1$X=='100pct', 'X'] = '100'
df1[df1$X=='50pct', 'X'] = '50'
df1[df1$X=='25pct', 'X'] = '25'

df2[df2$X=='100pct', 'X'] = '100'
df2[df2$X=='50pct', 'X'] = '50'
df2[df2$X=='25pct', 'X'] = '25'


### for violent crime
# melt the data frame for plotting
data.m <- melt(df1[df1$X != '100',], id.vars='X')
data.m$X = factor(data.m$X, levels = c("50", "25"))
# plot everything
ggplot(data.m, aes(variable, value)) +   
  geom_bar(aes(fill = X), position = "dodge", stat="identity") + labs(x = "", y="Percentage of Street Segments", 
                                                                      fill="Percentage of \nCrime Explained") + scale_y_continuous(limits = c(0,20)) 



### for all crime 
# melt the data frame for plotting
data.m <- melt(df2[df2$X != '100',], id.vars='X')
data.m$X = factor(data.m$X, levels = c("50", "25"))
# plot everything
ggplot(data.m, aes(variable, value)) +   
  geom_bar(aes(fill = X), position = "dodge", stat="identity") + labs(x = "", y="Percentage of Street Segments", 
                                                                      fill="Percentage of \nCrime Explained") + scale_y_continuous(limits = c(0,20)) 


