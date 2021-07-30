'flagging_per_sample.R

•	Remove samples that you dont want in your clean up steps,
like samples you dont care to look at. This would be the situation 
where you run someones samples in your run but dont want their blanks or dumb features

•	Background features are identified and removed from all samples
if the peak area in any of the blanks is bigger then the area in the sample
 peak area in a sample. 


Written by:
Milou Arts, NIOZ, NL, 2019
Zach Quinlan, SDSU, USA, 2019
List of alterations:



'

#define the samples you want to delete in the main file
#don't understand why this not works
if(exists("samples2delete")){
df1<- dplyr::filter(df1, !Sample_Name %like any% samples2delete)
}
analysis_info$flagging.method<-"flagged and removed if $max(blank) > peak area$, otherwise $peak area - max(blank)$"

#devide the dataset in blanks and samples
InjectionType<-as.factor(df1$Injection_type)
blanks<-subset(df1, df1$Injection_type!= "Sample")
samples<-subset(df1, df1$Injection_type == "Sample")
blanks<-as.data.frame(t(blanks))
samples<-as.data.frame(t(samples))



#this part sucks because of all the steps before there are lists loaded etc and the numbers are read as characters or factors
#in order to fix it I have to save the file and re-load the files, in this way they are imported as numerical.
blanks.name<-as.data.frame(unlist(blanks[1,]))
blanks.name<-blanks.name$`unlist(blanks[1, ])`

samples.name<-as.data.frame(unlist(samples[1,]))
samples.name<-samples.name$`unlist(samples[1, ])`

colnames(blanks)<- blanks.name
colnames(samples)<-samples.name

blanks<-blanks[4:nrow(blanks),]
samples<-samples[4:nrow(samples),]
setwd(dirWrite)
write.csv(blanks,"blanks.csv",row.names = TRUE) 
write.csv(samples,"samples.csv",row.names = TRUE)

rm(blanks)
rm(samples)

blanks<-read.csv("blanks.csv", sep=",", header = TRUE, row.names = 1, check.names = TRUE, stringsAsFactors=FALSE)
samples<-read.csv("samples.csv", sep=",", header = TRUE, row.names = 1, check.names = TRUE, stringsAsFactors=FALSE)


## flagging background features and subtraction from samples ------------------------------------------------
# The idea here is to flag and remove features where max(blanks) > area in samples.
# Here we flag by 0, an not flagged is >0, this is opposite as flagging_average_all_sample
#flagged features are removed
max_blanks <- apply(blanks, 1, max)
max_blanks<-as.data.frame(max_blanks)
df.max_blanks<-as.data.frame(cbind(replicate(dim(samples)[2], max_blanks$max_blanks)))
colnames(df.max_blanks)<-colnames(samples)
rm(max_blanks)

#make empty dataframe
flagging<-as.data.frame(matrix(ncol = ncol(samples), nrow = nrow(samples)))
row.names(flagging)<-row.names(samples)
colnames(flagging)<-colnames(samples)


for(i in 1:dim(samples)[1]) {
  for(j in 1:dim(samples)[2]) {
    if (df.max_blanks[i,j] > samples[i,j]){
    flagging[i,j] <- 0 } else { flagging[i,j] <- samples[i,j] - df.max_blanks[i,j]}
  }
}

flagging$sum<-apply(flagging, 1, sum)
flagging<-tibble::rownames_to_column(flagging, 'feature')
nr.flagged<-sum(flagging$sum == 0)
nr.notflagged<-sum(flagging$sum != 0)

df1.filtered<-flagging
df1.filtered<-dplyr::filter(df1.filtered, df1.filtered$sum  > 0)
df1.filtered<-dplyr::select(df1.filtered, -sum)
df1.filtered<-tibble::column_to_rownames(df1.filtered, 'feature')

flagged<-dplyr::filter(flagging, flagging$sum == 0)
flagged<-dplyr::select(flagged, -sum)
flagged<-tibble::column_to_rownames(flagged, 'feature')

setwd(dirOutput)
write.csv(df1.filtered,"rawpeaks_no-background.csv",row.names = TRUE) 


#save again some info in the analysis info
analysis_info$nr_selected_samples<-sum(df1$Injection_type == "Sample")
analysis_info$nr_selected_blanks<-sum(df1$Injection_type != "Sample")
analysis_info$nr_selected_blank_method<-sum(df1$Injection_type == "Blank_method")
analysis_info$nr_selected_blank_instrument<-sum(df1$Injection_type == "Blank_instrument")
analysis_info$nr_flagging_pass<-nr.notflagged
analysis_info$nr_flagging_remove<-nr.flagged