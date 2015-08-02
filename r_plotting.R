library(zoo)
library(ggplot2)
library(reshape)

# Encoding: ISO 88591-1

df.temp <- read.csv("data/ExcelFormattedGISTEMPData2CSV.csv", na.strings=c("****", "***"))

df.plot <- df.temp["Year"]

# NASA Website: 4 Regions 90-24S, 24-0S, 0-24N, 24-90N 
#df.temp$GlobRMean5 <- rollmean(df.temp$Glob, 5, fill=NA)
#df.plot$NHemRMean5 <- rollmean(df.temp$NHem, 5, fill=NA)
#df.plot$SHemRMean5 <- rollmean(df.temp$SHem, 5, fill=NA)
#df.plot$N2490RMean5 <- rollmean(df.temp$X24N.90N, 5, fill=NA)
#df.plot$N240RMean5 <- rollmean(df.temp$EQU.24N, 5, fill=NA)
#df.plot$S240RMean5 <- rollmean(df.temp$X24S.EQU, 5, fill=NA)
#df.plot$S9024RMean5 <- rollmean(df.temp$X90S.24S, 5, fill=NA)

# Consolidate to two regions in a more "geographical" context (North - South)
df.plot$North5 <- rollmean(round(df.temp$X24N.90N, 1), 5, fill=NA)
df.plot$South5 <- rollmean(round((df.temp$EQU.24N+df.temp$X24S.EQU+df.temp$X90S.24S)/3,1), 5, fill=NA)

df.plot_melt <- melt(df.plot, id=c("Year"))
df.plot_melt <- na.omit(df.plot_melt)

# Colors from http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/
cbbPalette <- c("#56B4E9", "#D55E00")

# Create Plot
p <- ggplot(df.plot_melt)
p <- p+geom_line(aes(x=Year, y=value, colour=variable), size=2, stat="identity")
p <- p+geom_hline()
p <- p+scale_x_continuous(breaks = round(seq(min(df.temp$Year), 
                                             max(df.temp$Year), 
                                             by = 10), 1))
p <- p+scale_y_continuous(limits = c(-51, 100), breaks =c(-50, -25, 0, 25, 50, 75, 100))
p <- p+scale_colour_manual("Geographical Regions:", labels = c("North (24°N-90°N latitude)",
                                                               "South (90°S-24°N latitude)"),
                           values=cbbPalette)
p <- p+ylab("Temperature Anomaly (°C)")
p <- p+ggtitle("Regional Surface Temperature Anomalies 1880 - 2014 (in °C)
               \n5-Year Running Means, Base Period 1951 - 1980\n\n")
p <- p+theme(plot.title = element_text(lineheight=.8, face="bold"), legend.position="bottom")

print(p)
