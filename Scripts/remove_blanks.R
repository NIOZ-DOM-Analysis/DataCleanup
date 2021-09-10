'remove_blanks.R

•	Remove blanks before normalisation.


Written by:
Milou Arts, NIOZ, NL, 2020

List of alterations:



'
df1.filtered_noblanks<-df1.filtered

sample_files<-dplyr::filter(orbitrapsequence, Injection_Type == "Sample")
sample_files<-sample_files$`File Name`

#select and keep the columns that are samples
options(warn=-1)
df1.filtered_noblanks<-dplyr::select(df1.filtered_noblanks, one_of(sample_files))
options(warn=0)
setwd(dirOutput)
write.csv(df1.filtered_noblanks,"df1.filtered_noblanks.csv",row.names = TRUE)



