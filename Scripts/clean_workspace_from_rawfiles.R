'clean_workspace_from_rawfiles.R

Goal: clean the workspace from rawfiles so they cannot get altered

Written by:
Milou Arts, NIOZ, NL, 2019

List of alterations:


'
analysis_info<-list(total_nr_features<-nrow(rawpeakareas))
names(analysis_info)<-"total_nr_features"
analysis_info$total_nr_runs<-ncol(rawpeakareas)-4
analysis_info$nr_selected_runs<-sum(!is.na(df1$Injection_Type))
analysis_info$nr_analog_hits<-nrow(analogs_hits)
analysis_info$nr_library_hits<-nrow(library_hits)
analysis_info$Rstudio<-unlist(RStudio.Version()[3])

rm(rawpeakareas)
rm(PREgapfilled)
rm(library_hits)
rm(analogs_hits)
rm(node_info)
rm(featureID_info)
rm(df1.pregap)
rm(shared.name)

rm(df, df.pregap, feat.order, feature_info, featureID)
setwd(dirOutput)


