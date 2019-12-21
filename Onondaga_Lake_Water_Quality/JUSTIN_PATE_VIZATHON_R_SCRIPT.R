#JUSTIN PATE
#VIZATHON PREP-CODE
library(plotly)
library(ggplot2)
library(ggpubr)
my.dir <- "C:\\School_SYR\\TERM5\\IST_719\\DATA\\"
df <- read.csv(file=paste0(my.dir, "BuoyData.csv")
                  , header = TRUE
                  , quote = "\""
                  , stringsAsFactors = FALSE)
View(df)

#define ggbar function
ggbar <- function(df_, col) {
  dfBar <- df_[c(col)]
  colnames(dfBar)[colnames(dfBar) == colnames(df_)[col]] <- 'bar'
  
  ggbp <- dfBar %>%
    group_by(bar) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = bar, y = count)) +
    geom_bar(aes(fill = bar),
             position = 'dodge',
             stat='identity') +
    # coord_flip() +
    labs(x = paste(colnames(df_)[col]),
         #y = 'Number of employees',
         title = paste(colnames(df_)[col])) +
    theme(legend.position = 'none',
          plot.title = element_text(size = 10,face = 'bold'),
          plot.subtitle  = element_text(size = 8),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 8))
  return(ggbp)
}


for (i in 1:ncol(df)) {
  print(ggbar(df, i))
}

for (i in 1:ncol(df)) {
  print(hist(df[i,]))
}
library(gam)
library(tidyr)
dfcht <- df[,-1]


ggplot(gather(dfcht),aes(value)) + geom_histogram(bins=10, col="blue")+ facet_wrap(~key, scales = 'free_x')

library(lubridate)
format(df$ï..DATE_TIME, format="%Y")


as.Date(df$ï..DATE_TIME, "%Y-%m")



#basic scatter charts
ggscatter(df, x = "ï..DATE_TIME", y = "DEPTH_m", color="SC_us_cm", size = "Chl_ug_L" )



dfmat<- data.matrix(df)
dfmat
df_heatmap <- heatmap(dfmat, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(3,3))
df_heatmap
          
          #, color = "Chl_ug_L",
          #palette = c("#00AFBB", "#E7B800", "#FC4E07") )



