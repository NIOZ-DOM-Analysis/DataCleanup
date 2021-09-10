'flagging_average_all_samples.R

•	Remove samples that you dont want in your clean up steps,
like samples you dont care to look at. This would be the situation
where you run someones samples in your run but dont want their blanks or dumb features

•	Background features are identified and removed from all samples
if the peak area in any of the blanks exceeds 50% of the
mean peak area across all samples.


Written by:
Milou Arts, NIOZ, NL, 2019
Zach Quinlan, SDSU, USA, 2019
List of alterations:



'
#define the samples you want to delete in the main file
#don't understand why this not works
if(exists("samples2delete")){
  if(is.na(samples2delete)){
  df1<- df1 %>% dplyr::filter(!is.na(Injection_Type))
}}
analysis_info$flagging.method<-"flagged and removed if $max(blanks) >= mean(peak area)*0.5$, thus over all samples"

#devide the dataset in blanks and samples
InjectionType<-as.factor(df1$Injection_Type)
blanks<-subset(df1, df1$Injection_Type!= "Sample")
samples<-subset(df1, df1$Injection_Type == "Sample")
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

blanks<-dplyr::slice(blanks, 4:nrow(blanks))
samples<-dplyr::slice(samples, 4:nrow(samples))
setwd(dirWrite)
write.csv(blanks,"blanks.csv",row.names = TRUE)
write.csv(samples,"samples.csv",row.names = TRUE)

rm(blanks)
rm(samples)

blanks<-read.csv("blanks.csv", sep=",", header = TRUE, row.names = 1, check.names = TRUE, stringsAsFactors=FALSE)
samples<-read.csv("samples.csv", sep=",", header = TRUE, row.names = 1, check.names = TRUE, stringsAsFactors=FALSE)


## flagging background features and subtraction from samples ------------------------------------------------
# The idea here is to flag and remove features where max(blanks) >= mean(samples)*0.5
# so flagged are removed, non flagged you keep.
# things are flagged with 1, not flagged = 0, this is the opposite than flagging_per_sample
blanks$max_blanks <- apply(blanks, 1, max)
samples$mean_samples <- apply (samples, 1, mean)

max_blanks<-dplyr::select(blanks, select = "max_blanks") %>% dplyr::rename("max_blanks" = "select")
mean_samples<-dplyr::select(samples, select = "mean_samples") %>% dplyr::rename("mean_samples" = "select")
flagging<-cbind(max_blanks, mean_samples)

rm(max_blanks)
rm(mean_samples)

#flag if blanks is more than half of the mean in the sample
flagging$flag<-if_else((flagging$mean_samples*0.5) < flagging$max_blanks, 1, 0, missing = NULL)
flagging<-rownames_to_column(flagging, var = "feature")

#split in flagged and not flagged
flagged<-dplyr::filter(flagging, flagging$flag == 1)
notflagged <-dplyr::filter(flagging, flagging$flag == 0)
analysis_info$nr.flagged<-nrow(flagged)
analysis_info$nr.notflagged<-nrow(notflagged)
flagged<-tibble::column_to_rownames(flagged, 'feature')

#keep the non flagged features
notflagged<-(as.character(notflagged$feature))
df1.filtered<- df1 %>% dplyr::select("File Name", one_of(notflagged))

df1.filtered<-tibble::column_to_rownames(df1.filtered, 'File Name')
df1.filtered<-as.data.frame(t(df1.filtered))
setwd(dirOutput)
write.csv(df1.filtered,"rawpeaks_no-background.csv",row.names = TRUE)

#save again some info in the analysis info
analysis_info$nr_selected_samples<-sum(df1$Injection_Type == "Sample")
analysis_info$nr_selected_blanks<-sum(df1$Injection_Type != "Sample")
analysis_info$nr_selected_blank_method<-sum(df1$Injection_Type == "Blank_method")
analysis_info$nr_selected_blank_instrument<-sum(df1$Injection_Type == "Blank_instrument")
analysis_info$nr_flagging_pass<-length(notflagged)
analysis_info$nr_flagging_remove<-nrow(flagged)

