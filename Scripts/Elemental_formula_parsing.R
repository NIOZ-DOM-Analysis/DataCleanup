"Elemental_formula_parsing.R


Goal:  Take the sirius data and read out the formula's and calculate atom ratio's, NOSC scores, and Delta G's

Written by:
Milou Arts, NIOZ, NL, 2019

List of alterations:


"
if (exists("SIRIUS")){
  wd.project<-getwd()
    #save the made file with more info
    SIRIUS.raw<-SIRIUS
    
    #because here we are only keeping two columns and making an extra column where we add the letter Z. becuase we need to know where the formula ends.
    SIRIUS<-dplyr::select(SIRIUS, feature_nr, sirius_formula)
    SIRIUS<-dplyr::mutate(SIRIUS, sirius_formula_z = paste(sirius_formula, "Z", sep =""))
    
    
    #find position of the letter in the string
    SIRIUS$C.pos<-(lapply(strsplit(SIRIUS$sirius_formula_z, ''), function(x) which(x == 'C')))
    SIRIUS$H.pos<-(lapply(strsplit(SIRIUS$sirius_formula_z, ''), function(x) which(x == 'H')))
    SIRIUS$B.pos<-(lapply(strsplit(SIRIUS$sirius_formula_z, ''), function(x) which(x == 'B')))
    SIRIUS$N.pos<-(lapply(strsplit(SIRIUS$sirius_formula_z, ''), function(x) which(x == 'N')))
    SIRIUS$O.pos<-(lapply(strsplit(SIRIUS$sirius_formula_z, ''), function(x) which(x == 'O')))
    SIRIUS$P.pos<-(lapply(strsplit(SIRIUS$sirius_formula_z, ''), function(x) which(x == 'P')))
    SIRIUS$S.pos<-(lapply(strsplit(SIRIUS$sirius_formula_z, ''), function(x) which(x == 'S')))
    SIRIUS$Z.pos<-(lapply(strsplit(SIRIUS$sirius_formula_z, ''), function(x) which(x == 'Z')))
    
    SIRIUS<-na_if(SIRIUS, "integer(0)")
    
    SIRIUS$C.pos<-as.numeric(unlist(SIRIUS$C.pos))
    SIRIUS$H.pos<-as.numeric(unlist(SIRIUS$H.pos))
    SIRIUS$B.pos<-as.numeric(unlist(SIRIUS$B.pos))
    SIRIUS$N.pos<-as.numeric(unlist(SIRIUS$N.pos))
    SIRIUS$O.pos<-as.numeric(unlist(SIRIUS$O.pos))
    SIRIUS$P.pos<-as.numeric(unlist(SIRIUS$P.pos))
    SIRIUS$S.pos<-as.numeric(unlist(SIRIUS$S.pos))
    SIRIUS$Z.pos<-as.numeric(unlist(SIRIUS$Z.pos))
    
    
    
    #now we need to get the numbers that are after the letter, and before the next letter
    SIRIUS$C<-NA
    SIRIUS$H<-NA
    SIRIUS$B<-NA
    SIRIUS$N<-NA
    SIRIUS$O<-NA
    SIRIUS$P<-NA
    SIRIUS$S<-NA
    
    ## Find the number of Carbons
    for (i in 1:nrow(SIRIUS)){
      if (is.na(SIRIUS$C.pos[i])){
        SIRIUS$C[i] <- 0
        } else {
      if (min(SIRIUS$H.pos[i],SIRIUS$B.pos[i], SIRIUS$N.pos[i], SIRIUS$O.pos[i], SIRIUS$P.pos[i],SIRIUS$S.pos[i], SIRIUS$Z.pos[i], na.rm = TRUE )-(SIRIUS$C.pos[i]+1)==0){
        SIRIUS$C[i] <- 1
      } else {
        SIRIUS$C[i]<-substr(SIRIUS$sirius_formula_z[i], (SIRIUS$C.pos[i]+1), (min(SIRIUS$H.pos[i],SIRIUS$B.pos[i],SIRIUS$N.pos[i], SIRIUS$O.pos[i], SIRIUS$P.pos[i],SIRIUS$S.pos[i], SIRIUS$Z.pos[i], na.rm = TRUE)-1))
    }}}
    
    ## Find the number of Hydrogen atoms
    for (i in 1:nrow(SIRIUS)){
      if (is.na(SIRIUS$H.pos[i])){
        SIRIUS$H[i] <- 0
       } else {
          if (min(SIRIUS$B.pos[i], SIRIUS$N.pos[i], SIRIUS$O.pos[i], SIRIUS$P.pos[i],SIRIUS$S.pos[i], SIRIUS$Z.pos[i], na.rm = TRUE )-(SIRIUS$H.pos[i]+1)==0){
            SIRIUS$H[i] <- 1
          } else {
            SIRIUS$H[i]<-substr(SIRIUS$sirius_formula_z[i], (SIRIUS$H.pos[i]+1), (min(SIRIUS$B.pos[i], SIRIUS$N.pos[i], SIRIUS$O.pos[i], SIRIUS$P.pos[i],SIRIUS$S.pos[i], SIRIUS$Z.pos[i], na.rm = TRUE)-1))
          }}}    
    
    ## Find the number of Boor atoms
    for (i in 1:nrow(SIRIUS)){
      if (is.na(SIRIUS$B.pos[i])){
        SIRIUS$B[i] <- 0
      } else {
        if (min(SIRIUS$N.pos[i], SIRIUS$O.pos[i], SIRIUS$P.pos[i],SIRIUS$S.pos[i], SIRIUS$Z.pos[i], na.rm = TRUE )-(SIRIUS$B.pos[i]+1)==0){
          SIRIUS$B[i] <- 1
        } else {
          SIRIUS$B[i]<-substr(SIRIUS$sirius_formula_z[i], (SIRIUS$B.pos[i]+1), (min(SIRIUS$N.pos[i], SIRIUS$O.pos[i], SIRIUS$P.pos[i],SIRIUS$S.pos[i], SIRIUS$Z.pos[i], na.rm = TRUE)-1))
        }}} 
    
    ## Find the number of Nitrogen atoms
    for (i in 1:nrow(SIRIUS)){
      if (is.na(SIRIUS$N.pos[i])){
        SIRIUS$N[i] <- 0
        } else {
          if (min(SIRIUS$O.pos[i], SIRIUS$P.pos[i],SIRIUS$S.pos[i], SIRIUS$Z.pos[i], na.rm = TRUE )-(SIRIUS$N.pos[i]+1)==0){
            SIRIUS$N[i] <- 1
          } else {
            SIRIUS$N[i]<-substr(SIRIUS$sirius_formula_z[i], (SIRIUS$N.pos[i]+1), (min(SIRIUS$O.pos[i], SIRIUS$P.pos[i],SIRIUS$S.pos[i], SIRIUS$Z.pos[i], na.rm = TRUE)-1))
          }}}  
    
    ## Find the number of Oxygen atoms
    for (i in 1:nrow(SIRIUS)){
      if (is.na(SIRIUS$O.pos[i])){
        SIRIUS$O[i] <- 0
      } else {
        if (min(SIRIUS$P.pos[i],SIRIUS$S.pos[i], SIRIUS$Z.pos[i], na.rm = TRUE )-(SIRIUS$O.pos[i]+1)==0){
          SIRIUS$O[i] <- 1
        } else {
          SIRIUS$O[i]<-substr(SIRIUS$sirius_formula_z[i], (SIRIUS$O.pos[i]+1), (min(SIRIUS$P.pos[i],SIRIUS$S.pos[i], SIRIUS$Z.pos[i], na.rm = TRUE)-1))
        }}} 
    
    ## Find the number of Phosphorus atoms
    for (i in 1:nrow(SIRIUS)){
      if (is.na(SIRIUS$P.pos[i])){
        SIRIUS$P[i] <- 0
      } else {
        if (min(SIRIUS$S.pos[i], SIRIUS$Z.pos[i], na.rm = TRUE )-(SIRIUS$P.pos[i]+1)==0){
          SIRIUS$P[i] <- 1
        } else {
          SIRIUS$P[i]<-substr(SIRIUS$sirius_formula_z[i], (SIRIUS$P.pos[i]+1), (min(SIRIUS$S.pos[i], SIRIUS$Z.pos[i], na.rm = TRUE)-1))
        }}} 
    
    ## Find the number of Sulfur atoms
    for (i in 1:nrow(SIRIUS)){
      if (is.na(SIRIUS$S.pos[i])){
        SIRIUS$S[i] <- 0
      } else {
        if (SIRIUS$Z.pos[i]-(SIRIUS$S.pos[i]+1)==0){
          SIRIUS$S[i] <- 1
        } else {
          SIRIUS$S[i]<-substr(SIRIUS$sirius_formula_z[i], (SIRIUS$S.pos[i]+1), (SIRIUS$Z.pos[i]-1))
        }}} 
    
    #now make everything numeric becuase everything is character
    SIRIUS$C<-as.numeric(SIRIUS$C)
    SIRIUS$H<-as.numeric(SIRIUS$H)
    SIRIUS$B<-as.numeric(SIRIUS$B)
    SIRIUS$N<-as.numeric(SIRIUS$N)
    SIRIUS$O<-as.numeric(SIRIUS$O)
    SIRIUS$P<-as.numeric(SIRIUS$P)
    SIRIUS$S<-as.numeric(SIRIUS$S)
    SIRIUS$feature_nr<-as.character(SIRIUS$feature_nr)
    
    #now start calculating things
    SIRIUS<- dplyr::mutate(SIRIUS, 'H:C' =  H / C)
    SIRIUS<- dplyr::mutate(SIRIUS, 'O:C' =  O / C)
    SIRIUS<- dplyr::mutate(SIRIUS, 'N:C' =  N / C)
    SIRIUS<- dplyr::mutate(SIRIUS, 'NOSC' =  -((-1+(4*C)+H-(3*N)-(2*O)+(5*P)-(2*S))/C)+4)
    SIRIUS<- dplyr::mutate(SIRIUS, 'Delta_Gox' = 60.3-(28.5*NOSC))
    
    
    SIRIUS<-dplyr::full_join(df.featureID, SIRIUS, by = "feature_nr")
    setwd(dirOutput)
    write_csv(SIRIUS, "Elemental_formula_parsing.csv")
    setwd(wd.project)

} 