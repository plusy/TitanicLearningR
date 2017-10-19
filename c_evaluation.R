library(pROC)


# read ground truth -------------------------------------------------------
read_csv(FILE_CLEAN_DATA_GROUND_TRUTH) %>% 
    mutate(PassengerId = as.integer(as.character(PassengerId)),
            GTSurvived = as.integer(as.character(GTSurvived))) ->
    .df_GroundTruth


# ROC ---------------------------------------------------------------------
evaluateROC <- function(aTruthRef, aPositiveProb){
    if(length(aTruthRef) != length(aPositiveProb) ||
       all(aPositiveProb %in% c(0,1)))
        return(NULL)

    ret = roc(if (all(aTruthRef %in% c(0,1))) aTruthRef
              else factor(.df_GroundTruth$GTSurvived[which(.df_GroundTruth$PassengerId %in% aTruthRef)]),
              aPositiveProb)   

    plot(ret, 
         print.thres="best", 
         print.thres.best.method="closest.topleft")
    print(coords(ret, 
                 "best",
                 best.method="closest.topleft", 
                 ret=c("threshold", "accuracy")))
    return(ret)
}


# Accurracy -------------------------------------------------------------
evaluateAccuracy <- function(aPassagerId, aIsPositive){
    if(length(aPassagerId) != length(aIsPositive)) 
        return(NULL)
       
    data.frame(PassengerId = as.integer(as.character(aPassagerId)),
<<<<<<< HEAD
               Survived = as.integer(as.character(aIsPositive))) %>%        
=======
               Survived = as.integer(aIsPositive)) %>%        
>>>>>>> c4978355d8d88075fe2623e572fe1fad46af5884
        left_join(.df_GroundTruth, by = 'PassengerId') -> 
        ret
    
    TotalInput <- nrow(ret)
    CorrectInput <- sum(as.numeric(ret$Survived==ret$GTSurvived), na.rm = TRUE)
    Accuracy <- CorrectInput/TotalInput
    
    return(list(IsPassageIdAllValid =  (length(aIsPositive) == nrow(ret)), 
                TotalInput = TotalInput, 
                CorrectInput = CorrectInput, 
                Accuracy = Accuracy))
}
