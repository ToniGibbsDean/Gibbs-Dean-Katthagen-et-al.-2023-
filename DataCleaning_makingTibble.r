#########################################
#SPQ Data: make simple tibble
#########################################
        # Notes: 

        library(tidyverse)
        path<-"~/Dropbox/SpaceTask_Project"

        #parameters
        CorrectGameLength<-216 # no level 1

        # load data
            pfiles<-list.files(file.path(path, "Data", "SPQ", "Parameters"))
            tfiles<-list.files(file.path(path, "Data", "SPQ", "TaskData"))
            dfiles<-list.files(file.path(path, "Data", "SPQ", "Symptom_DemoData"))

            p<-lapply(file.path(path, "Data", "SPQ", "Parameters", pfiles), read.csv, header=TRUE)
            names(p)<-substr(pfiles, start=1, stop=nchar(pfiles)-4)

            t<-lapply(file.path(path, "Data", "SPQ", "TaskData", tfiles), read.csv)
            names(t)<-tfiles

            d<-lapply(file.path(path, "Data", "SPQ", "Symptom_DemoData", dfiles), read.csv)
            names(d)<-dfiles


        #combine data

            #merge task datas
                t<-rbind(t[[1]], t[[2]])

            #for all poeple with correct game length append columns with their parameter data
                IDs<-unique(t$Participant.Private.ID)
                df<-c()
                for (ID in IDs) {
                    PersonsTaskData<-t[t$Participant.Private.ID==ID,] %>%
                        filter(!is.na(level))

                    gameLength<-dim(PersonsTaskData)[1]
                    if (gameLength==CorrectGameLength) {
                        if(length(unique(PersonsTaskData$trialName))==1) {
                            PersonParameters<-p[names(p)==unique(PersonsTaskData$trialName)] [[1]]%>% filter(level!=1)
                            PersonFullData<-cbind(PersonsTaskData, PersonParameters)
                            names(PersonFullData)<-c(names(PersonsTaskData), c("Mean", "SD", "Hint", "Result", "Level", "TrialNumber")) #rename columns to avoid renaming by spreadsheet
                        } else {print("ERROR - multiple spreadsheets used by one person")}
                        df<-rbind(df,PersonFullData)
                    }
                }

            #append demos

            prolificIdKey<-rbind(d[[1]], d[[2]]) %>%
                                    filter(Question.Key=="response-1") %>%
                                    select(Participant.Private.ID, Response) %>%
                                    rename("ProlificID"="Response")

            demos<-rbind(d[[3]], d[[4]]) %>%
                    select(2:10, 13, 90, 91)
            names(demos)<-c("Country", "Language", "Age", "HeadInjury", "Any.dx", "Gender", "Education", "Employment", "Married", "Ethnicity", "ProlificID", "SPQsum")

            df_withProlificId<-left_join(df, prolificIdKey, by="Participant.Private.ID")
            tibble<-left_join(df_withProlificId, demos) %>% as_tibble


# tibble processing - data cleaning and column tidying

            #exclusions
                #record exclusions based on incomplete or repeated games
                    excludeRepeatOrIncomplete <- length(IDs) -length(unique(tibble$Participant.Private.ID))

                #exclude based on trial performance and symptom scale attention
                    #get table of all reasons for exclusion for each participant
                        ExclusionBasis<-tibble %>%
                            group_by(Participant.Private.ID) %>%
                            summarise(  nomoveX=sum(distanceMovedX==0), 
                                        nomoveY=sum(distanceMovedY==0), 
                                        nomoveXY=sum(distanceMovedX==0 & distanceMovedY==0), 
                                        overtime=sum(timeToComplete>5000), 
                                        undertime=sum(timeToComplete<1000), 
                                        zerotime=sum(timeToComplete==0)) %>%
                            summarise(  Participant.Private.ID=Participant.Private.ID, 
                                        excludeX= nomoveX>108, 
                                        excludeY=nomoveY>108, 
                                        excludeXY= nomoveXY >21, 
                                        excludeOvertime= overtime >21, 
                                        excludeUndertime= undertime >21) #, excludeZerotime= zerotime >0)
                    #extract indices of participants to exclude
                        IndicesToExclude<-ExclusionBasis %>%
                                        select(starts_with("exc")) %>%
                                        rowSums()>0
                        IdsToExclude<-ExclusionBasis$Participant.Private.ID[IndicesToExclude]
                            
        #create exclusions tracking table  
        #make main table    
                    track<-c(excludeRepeatOrIncomplete=excludeRepeatOrIncomplete, 
                            ExclusionBasis %>%  select(starts_with("exc")) %>% summarise_if( is.logical, sum))                 
                #calculate cumulative exclusions
                    CumulativeExclusionsTibble<-ExclusionBasis %>%
                    group_by(Participant.Private.ID)%>%
                        summarise(  excludex=excludeX>0,
                                    excludex_y=sum(across(excludeX: excludeY))>0, # underscore means across from column a to column c with all coolumns inbetween included
                                    excluexy=excludeXY>0,
                                    excludex_xy=sum(across(excludeX:excludeXY))>0,
                                    excludex_over=sum(across(excludeX:excludeOvertime))>0,
                                    excludex_under=sum(across(excludeX:excludeUndertime))>0,
                                    excludexyANDunder=sum(across(c(excludeXY,excludeUndertime)))>0) %>%
                        select(excludex, excludex_y, excludex_xy, excludex_over, excludex_under)
                        
                    CumulativeExclusions<-CumulativeExclusionsTibble %>%
                                            summarise_if( is.logical, sum)    

            #make final tracking table
                    track<-rbind(  track,
                            c(track["excludeRepeatOrIncomplete"], CumulativeExclusions+track$excludeRepeatOrIncomplete))
                    rownames(track)<-c("independent", "cumulative")





############################################################################################################################################################################################
#applying exclusion critiera and maknig a tibble
############################################################################################################################################################################################


            #apply exclusions
                tibble<-tibble[!tibble$Participant.Private.ID %in% IdsToExclude,]

    #column tidying
        # column na management
        tibble$success[is.na(tibble$success)]<-0
        tibble$tutorialRepeated[is.na(tibble$tutorialRepeated)]<-0

        #column additons
        tibble$PE<-abs(scale(tibble$originalResult, center = c(-0.5), scale = c(0.01)) - tibble$participantPosition)
        tibble$signedPE<-scale(tibble$originalResult, center = c(-0.5), scale = c(0.01)) - tibble$participantPosition
        tibble<-tibble %>%
            group_by(Participant.Private.ID) %>%
            mutate( signedPositionChange=lead(participantPosition)- participantPosition,
                    positionChange=abs(lead(participantPosition)- participantPosition),
                    LR = signedPositionChange/signedPE) 
        
        tibble$PerfE<-abs(scale(tibble$Mean, center = c(-0.5), scale = c(0.01))- tibble$participantPosition)
        tibble$signedPerfE<-scale(tibble$Mean, center = c(-0.5), scale = c(0.01)) - tibble$participantPosition



#save output
    write.csv(file=file.path(path, "Results", "TrackingTableSPQ.csv"), track)
    saveRDS(file=file.path(path, "IntermediateOutputs", "SPQTibble.RDS"), tibble)