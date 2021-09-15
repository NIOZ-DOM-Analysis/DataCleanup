'LOGNORM.R

Goal: log 10 normalize.


Written by:
Milou Arts, NIOZ, NL, 2019

List of alterations:

'
wd.project<-getwd()
setwd(dirOutput)

#create backup of your data, although this should be the same as "rawpeaks_no-background_no-transientfeat.csv"
if (!exists("df1.trans.feat")){
        df1.trans.feat<-df.filtered
        analysis_info$normalization<-"normalized by $log10(peak area)$ transformation"
        analysis_info$normalization_zeroes_replaced_by<-zeroes
} else { df.filtered<-read.csv("rawpeaks_no-background_no-transientfeat.csv", row.names = 1)
        analysis_info$normalization_2<-"normalized by $log10(peak area)$ transformation"
        analysis_info$normalization_2_zeroes_replaced_by<-zeroes }

df.filtered<-as.data.frame(t(df.filtered))

#Normalize by log 10 transformation
#define function
FUNLOG<-function (x) {
  log10(x)
}

#create matrix to put data in
LOGNORM<-data.frame(matrix(ncol=ncol(df.filtered), nrow = nrow(df.filtered)))
#apply function
LOGNORM<-sapply(df.filtered, FUNLOG)
#change to data frame and add rownames etc.
LOGNORM<-as.data.frame(LOGNORM)
rownames(LOGNORM)<-rownames(df.filtered)

write.csv(LOGNORM,"LOGNORM_raw.csv",row.names = TRUE)

#replace the 0s with the number you want. - i couldve done this before log transformation. but now i also have a raw version)
tmp<-LOGNORM
set.seed(25)
log10zeroes<-runif(sum(LOGNORM == -Inf))
log10zeroes<-log10zeroes + zeroes
log10zeroes<-log10(log10zeroes)
tmp[which(LOGNORM == -Inf, arr.ind = TRUE)]<- log10zeroes

df.norm.area<-tmp
df.norm.area<-rownames_to_column(df.norm.area, "File Name")

write.csv(df.norm.area,"df.norm.area_no_metadata.csv",row.names = FALSE)

analysis_info$nr_replaced_zeroes_lognorm<-length(log10zeroes)
rm(log10zeroes)

#now create metadata and add to the normalized and transformed data
if (!exists("full_metadata")){
  full_metadata<-dplyr::left_join(orbitrapsequence, metadata, by = "Sample Name")}
df.norm.area<-dplyr::right_join(full_metadata, df.norm.area, by = "File Name")
df.norm.area<-if_na(df.norm.area, "not applicable")

write.csv(df.norm.area,"df.norm.area.csv",row.names = FALSE)

#number of metadata columns
M<-ncol(full_metadata)+1

setwd(wd.project)