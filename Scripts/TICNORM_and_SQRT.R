'TICNORM_and_SQRT.R

Goal: normalize by sample.


Written by:
Milou Arts, NIOZ, NL, 2019

List of alterations:

'
wd.project <- getwd()
analysis_info$normalization<-"relativized by the total ion current of each sample followed by $asin(sqrt(x))$ normalization."
setwd(dirOutput)
df1.trans.feat<-df.filtered #create backup of your data, although this should be the same as "rawpeaks_no-background_no-transientfeat.csv"
df.filtered<-as.data.frame(t(df.filtered))
df.filtered<-as.data.frame(sapply(df.filtered,as.numeric))
#Sum all raw area's under the peak to get a Total Ion Current
df.filtered$TIC<-apply(df.filtered, 1 ,sum)

#Normalize by TIC (area under the peak/TIC)
#define function
FUNTIC<-function (x) {
  x / df.filtered$TIC
}

#create matrix to put data in
TICNORM.smpl<-data.frame(matrix(ncol=ncol(df.filtered), nrow = nrow(df.filtered)))
#apply function
TICNORM.smpl<-sapply(df.filtered, FUNTIC)
#change to data frame and add rownames etc.
TICNORM.smpl<-as.data.frame(TICNORM.smpl)
rownames(TICNORM.smpl)<-rownames(df.filtered)
TICNORM.smpl<-TICNORM.smpl[,1:(ncol(TICNORM.smpl)-1)]

write.csv(TICNORM.smpl,"TICNORM.smpl.csv",row.names = TRUE)

#create function to transform data by asin of sqare root of the normalized data.
FUNSQRT<-function(x){
  asin(sqrt(x))
}

#create matrix to put data in
ASIN_sqrt_smpl<-data.frame(matrix(ncol=ncol(TICNORM.smpl), nrow = nrow(TICNORM.smpl)))
#apply function
ASIN_sqrt_smpl<-sapply(TICNORM.smpl, FUNSQRT)
#change to data frame and add rownames etc.
ASIN_sqrt_smpl<-as.data.frame(ASIN_sqrt_smpl)
rownames(ASIN_sqrt_smpl)<-rownames(TICNORM.smpl)

df.norm.smpl<-ASIN_sqrt_smpl
df.norm.smpl<-rownames_to_column(df.norm.smpl, "File Name")

write.csv(df.norm.smpl,"df.norm.smpl_no_metadata.csv",row.names = FALSE)

#now create metadata and add to the normalized and transformed data
if (!exists("full_metadata")){
  full_metadata<-dplyr::left_join(orbitrapsequence, metadata, by = "Sample Name")}
df.norm.smpl<-dplyr::right_join(full_metadata, df.norm.smpl, by = "File Name")
df.norm.smpl<-if_na(df.norm.smpl, "not applicable")

write.csv(df.norm.smpl,"df.norm.smpl.csv",row.names = FALSE)

#number of metadata columns
M<-ncol(full_metadata)+1

setwd(wd.project)
