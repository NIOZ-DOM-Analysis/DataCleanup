'compare_gapfilled_non-gapfilled.R

Goal: compare the non-gapfilled data with the gapfilled data
to determine the background noise and visualize the data
pregapplot could be used to make a graph, but it is super computational heavy and R doesnt like it.

Written by:
Milou Arts, NIOZ, NL, 2019

List of alterations:


'
df2 <- (df1)
df2 <- column_to_rownames(df2, var = "File Name")
df2 <- dplyr::select(df2,-c('Sample Name', 'Injection_Type'))
df2.long <-
  df2 %>% pivot_longer(cols = everything(),
                       names_to = "featurecode",
                       values_to = "relativeabundance")
setwd(dirWrite)
write.csv(df2.long, 'df2long.csv', row.names = FALSE)
df2.long <- read_csv('df2long.csv')
df2.long <- arrange(df2.long, relativeabundance)
df2.long <- rowid_to_column(df2.long, var = 'X1')

df2.pregap<-(df1.pregap)
df2.pregap<-column_to_rownames(df2.pregap, var = "File Name")
df2.pregap<-dplyr::select(df2.pregap, -c('Sample Name', 'Injection_Type'))
df2.pregap.long<-df2.pregap%>%pivot_longer( cols= everything(),names_to = "featurecode", values_to = "relativeabundance")
write.csv(df2.pregap.long, 'df2pregaplong.csv', row.names = FALSE)
df2.pregap.long<-read_csv('df2pregaplong.csv')
df2.pregap.long<-arrange(df2.pregap.long, relativeabundance)
df2.pregap.long<-rowid_to_column(df2.pregap.long, var = 'X1')

pregapplot<-dplyr::left_join(df2.long, df2.pregap.long, by= "X1")
feat.order<-dplyr::select(pregapplot, "X1")
pregapplot<-dplyr::select(pregapplot, relativeabundance.x, relativeabundance.y)
colnames(pregapplot)<-c("gapfilled", "pregap")
pregapplot<-cbind(feat.order, pregapplot)
pregap_gap<-pregapplot

split_gapfilled<-dplyr::select(pregap_gap, -pregap)
split_pregap<-dplyr::select(pregap_gap, -gapfilled)

setwd(wd.project)
setwd(dirFigs)

split_pregap<-dplyr::filter(split_pregap, pregap > 0)

#head(split_pregap)

setwd(wd.project)
#######

rm(df2, df2.long, df2.pregap, df2.pregap.long)
rm(pregap_gap, pregapplot, split_gapfilled)

