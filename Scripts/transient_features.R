'transient_features.R

•	Remove samples transient features.
•	A transient feature is almost only lower than the set background_noise,
thus a feature needs to be above the background_noise in more than 3 (or W) samples.


Written by:
Milou Arts, NIOZ, NL, 2019
Zach Quinlan, SDSU, USA, 2019
List of alterations:


'
wd.project<-getwd()
setwd(dirOutput)
#read in the file of the rawpeaks that had the background features removed
if (exists("df1.filtered_noblanks")){
  df.trans<-df1.filtered_noblanks
}else{
df.trans<-read.csv('df1.filtered_noblanks.csv', header = TRUE, row.names = 1)
}

#create an empty matrix to fill in for every feature in how many samples (count) the area under the peak is higher than the set background noise.
df.count<-as.data.frame(matrix(ncol=1, nrow = nrow(df.trans)))
for (i in 1:dim(df.trans)[1]){
  df.count[i,1] <-sum(df.trans[i,]>background_noise)}

#Give th counts row names so we can add that column to the original table
rownames(df.count)<-rownames(df.trans)
df.filtered<-cbind(df.trans, df.count)
df.filtered<-rownames_to_column(df.filtered, 'feature')

#we now split the file into the transient features (occuring less than W times) in df.trans (W is the size of the smallest group)
#and the ones that are above the backgroud noise level more than W times (filtered)
df.trans<-dplyr::filter(df.filtered, df.filtered$V1 < W)
df.trans<-dplyr::select(df.trans, -V1)
df.filtered<-dplyr::filter(df.filtered, df.filtered$V1 >= W)
df.filtered<-dplyr::select(df.filtered, -V1)

df.trans<-column_to_rownames(df.trans, 'feature')
df.filtered<-column_to_rownames(df.filtered, 'feature')


write.csv(df.trans,"transient.feat_filtered_out.csv",row.names = TRUE)
write.csv(df.filtered,"rawpeaks_no-background_no-transientfeat.csv",row.names = TRUE)

analysis_info$nr_transient_features_removed<-nrow(df.trans)

#remove some objects we don't need anymore
rm(df.trans, df.count)
rm(flagging, blanks, samples)

#combine filtered data with metadata to create df.area but also save one without metadata but with samples in rows.
df.area<-as.data.frame(t(df.filtered))
df.area<-rownames_to_column(df.area, var = "File Name")
df.area.no.meta<-df.area

#create metadata and add.
if (!exists("full_metadata")){
  full_metadata<-dplyr::right_join(orbitrapsequence, metadata, by = "Sample Name")}
df.area<-dplyr::right_join(full_metadata, df.area, by = "File Name")
df.area<-if_na(df.area, "not applicable")

write.csv(df.area,"df.area.csv",row.names = FALSE)
write.csv(df.area.no.meta,"df.area_no.metadata.csv",row.names = FALSE)

setwd(wd.project)
