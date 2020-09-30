install.packages("tidyverse")
library(tidyverse)


df<-mtcars
df[2:5, 5:7]<-NA
head(df)
summary(df)
sum(is.na(df))
colSums(is.na(df))
rowSums(is.na(df))
x<-as.integer(2.4)
typeof(x)
