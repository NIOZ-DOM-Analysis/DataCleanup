'TICNORM_and_SQRT_by_FEATURE.R

Goal: normalize and transform by feature.


Written by:
Milou Arts, NIOZ, NL, 2019

List of alterations:

'
analysis_info$normalization_feature<-"relativized by the total ion current of each feature followed by asin(sqrt(x)) normalization."
setwd(dirOutput)

#if you normalized by sample you have a backup of your filtered data as df.1.trans.feat (and your df.filtered will be changed!))
if (exists("df1.trans.feat")){
  df2.filtered<-df1.trans.feat
} else {df2.filtered <- read.csv("rawpeaks_no-background_no-transientfeat.csv", row.names = 1)}

#Sum all raw area's under the peak to get a Total Ion Current
df2.filtered$TIC<-apply(df2.filtered, 1 ,sum)

#Normalize by TIC (area under the peak/TIC)
#define function
FUNTIC<-function (x) {
  x / df2.filtered$TIC
}
#create matrix to put data in
TICNORM.ft<-data.frame(matrix(ncol=ncol(df2.filtered), nrow = nrow(df2.filtered)))
#apply function
TICNORM.ft<-sapply(df2.filtered, FUNTIC)
#change to data frame and add rownames etc.
TICNORM.ft<-as.data.frame(TICNORM.ft)
rownames(TICNORM.ft)<-rownames(df2.filtered)
TICNORM.ft<-TICNORM.ft[,1:(ncol(TICNORM.ft)-1)]
setwd(dirOutput)
write.csv(TICNORM.ft,"TICNORM.ft.csv",row.names = TRUE) 


#create function to transform data by asin of sqare root of the normalized data.
FUNSQRT<-function(x){
  asin(sqrt(x))
}

#create matrix to put data in
ASIN_sqrt.ft<-data.frame(matrix(ncol=ncol(TICNORM.ft), nrow = nrow(TICNORM.ft)))
#apply function
ASIN_sqrt.ft<-sapply(TICNORM.ft, FUNSQRT)
#change to data frame and add rownames etc.
ASIN_sqrt.ft<-as.data.frame(ASIN_sqrt.ft)
rownames(ASIN_sqrt.ft)<-rownames(TICNORM.ft)

df.norm.ft<-as.data.frame(t(ASIN_sqrt.ft))
df.norm.ft<-rownames_to_column(df.norm.ft, "File Name")

write.csv(df.norm.ft,"df.norm_no_metadata_ft.csv",row.names = FALSE) 

#join with full metadata
if (!exists("full_metadata")){
  full_metadata<-dplyr::left_join(orbitrapsequence, metadata, by = "Sample Name")}

df.norm.ft<-dplyr::right_join(full_metadata, df.norm.ft, by = "File Name")
df.norm.ft<-if_na(df.norm.ft, "not applicable")
setwd(dirOutput)
write.csv(df.norm.ft,"df.norm.ft.csv",row.names = FALSE) 

rm(df2.filtered)


