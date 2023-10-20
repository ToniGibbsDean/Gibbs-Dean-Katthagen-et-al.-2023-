################################################################################################
# 1. Packages and path to project directory ######
################################################################################################
      set.seed(0.1)
  
    library(tidyverse)
    library(lme4)
    library(lmerTest)
    library(nlme)
    library(multcomp)
    library(emmeans)
    library(corrr)
    #take scientific notation off
    options(scipen=999)
    #library(sjPlot)

################################################################################################
# 2. load data
################################################################################################

    spqtibble<-readRDS("/Users/tonigibbs-dean/Documents/PhD/SpaceTaskProject/SPQ/Intermediate_Results/SPQTibble.RDS") %>%
                    filter(SPQsum != "n/a") %>% #removes 3
                    filter(level!="1") %>%
                    filter(Participant.Device.Type %in% c("computer", "mobile")) %>% #removes 3
                    mutate(spqH=SPQsum>=30)%>%
                    mutate(SDHigh=(SD==0.12)) %>%
                    mutate(level=as.factor(level)) %>%
                    mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                    mutate(highVolatility=level %in% c(4:6)) %>%
                     mutate(correctedL6trialScore = case_when(   level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                            level == 6 & trialScore>0  ~ trialScore/2,
                                            TRUE ~ as.numeric(trialScore))) %>%
                    mutate(forward=trialName %in% c("spacetask003","spacetask012"), 
                           reverse=trialName %in% c("spacetask003reversed","spacetask012reversed")) 

    
# Recode variables where needed
      spq<- spqtibble %>%
                        mutate(Ethnicity=as.character(Ethnicity)) %>%
                        mutate(Education=as.character(Education)) %>%
                        mutate(Gender=as.character(Gender)) %>%
                        mutate(Ethnicity=recode(Ethnicity, "6" = "2")) %>% 
                        mutate(Education=recode(Education, "1" = "2", "3" = "4", "5" = "6", "7" = "6"))

################################################################################################       
#create modelling dfs
################################################################################################
        df_mod<-spq %>%
            #mutate(gamers=gamingtime.quant>2) %>% 
            group_by(Participant.Private.ID, level) %>%
            mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
            summarise(meanScore=mean(trialScore), 
                    corectedScore=mean(correctedL6trialScore),
                    meanconf=mean(participantConfidence),
                    sumsuccess=sum(success/length(level)),
                    meanPE=mean(PE),
                    meanPerfE=mean(PerfE),
                    #bonus=sum(reward),
                    age=dplyr::first(Age),
                    ethnicity=as.factor(dplyr::first(Ethnicity)),
                    education=as.factor(dplyr::first(Education)),
                    employment=as.factor(dplyr::first(Employment)),
                    #country=dplyr::first(Country),
                    #language=dplyr::first(Language),
                    HeadInjury=dplyr::first(HeadInjury),
                    gender=as.factor(dplyr::first(Gender)),
                    device_type=dplyr::first(Participant.Device.Type),
                    trialName=dplyr::first(trialName),
                    spqH=as.factor(dplyr::first(spqH)),
                    wideStartSD=as.factor(dplyr::first(startSD)),
                    highVol=as.factor(dplyr::first(highVolatility)),
                    highnoise=as.factor(dplyr::first(SDHigh))) %>%
                    filter(level %in% c(3, 4, 5, 6))

        
        df_mod<- df_mod %>%
                drop_na

        df_mod_confidence<-spq %>%
                                #mutate(gamers=gamingtime.quant>2) %>% 
                                group_by(Participant.Private.ID, level, SDHigh) %>%
                                mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
                                summarise(meanScore=mean(trialScore), 
                                        meanconf=mean(participantConfidence),
                                        sumsuccess=sum(success/length(level)),
                                        meanPE=mean(PE),
                                        #bonus=sum(reward),
                                        age=dplyr::first(Age),
                                        ethnicity=as.factor(dplyr::first(Ethnicity)),
                                        education=as.factor(dplyr::first(Education)),
                                        employment=as.factor(dplyr::first(Employment)),
                                        #country=dplyr::first(Country),
                                        #language=dplyr::first(Language),
                                        HeadInjury=dplyr::first(HeadInjury),
                                        gender=as.factor(dplyr::first(Gender)),
                                        device_type=dplyr::first(Participant.Device.Type),
                                        trialName=dplyr::first(trialName),
                                        spqH=as.factor(dplyr::first(spqH)),
                                        wideStartSD=as.factor(dplyr::first(startSD)),
                                        highnoise=as.factor(dplyr::first(SDHigh))) %>%
                                        filter(level %in% c(3, 4, 5, 6))

        
        df_mod_confidence<- df_mod_confidence %>%
                                                drop_na

    #contrasts
            #exlucding L6
            # df_mod$level<-droplevels(df_mod$level)
            #  cMat <- cbind(c(1,-1,0), # 3 vs 4
            #             c(0,1,-1)) # 4 vs 5
                #        contrasts(df_mod$level) <-cMat
                #        colnames(attr(df_mod$level, "contrasts")) <- c("3v4", "4v5")
            #inc l6
                df_mod$level<-droplevels(df_mod$level)
                cMat <- cbind( c(-1,1,0,0), # 3 vs 4
                        c(0,-1,1,0), # 4 vs 5
                        c(0,0,-1,1)) #5vs 6
                contrasts(df_mod$level) <-cMat
                colnames(attr(df_mod$level, "contrasts")) <- c("3v4", "4v5", "5v6")

              #not back contrasted currently 
                df_mod_confidence$level<-droplevels(df_mod_confidence$level)
                cMat <- cbind( c(-1,1,0,0), # 3 vs 4
                        c(0,-1,1,0), # 4 vs 5
                        c(0,0,-1,1)) #5vs 6
                contrasts(df_mod_confidence$level) <-cMat
                colnames(attr(df_mod_confidence$level, "contrasts")) <- c("3v4", "4v5", "5v6")

################################################################################################
#Modelling
################################################################################################

   #######################
   #score 
   #######################
            #lowest AIC = 
            #NB when comparing with HGF stuff - remember that I have also taken out the tablet users here, but I think they
            #must still be included in the  HGF data for some reason. 

        mostsimple<-df_mod %>% 
                        lmer(corectedScore ~ 1 +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        summary(mostsimple)

        nointeraction<-df_mod %>% 
                                        lmer(corectedScore ~ 
                                                 wideStartSD + level + spqH + gender + age + education + ethnicity + 
                                                 device_type + employment + 
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)))
                                        summary(nointeraction)
                                        car::qqPlot(resid(nointeraction))
                                        scatter.smooth(residuals(nointeraction) ~ fitted(nointeraction))
               
        reduced_full_noSPQ<-df_mod %>% 
                                        #ggplot(aes(y=meanScore, x=level, fill=wideStartSD)) +
                                        #geom_boxplot() 
                                        lmer(corectedScore ~ 
                                                level + education + 
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(reduced_full_noSPQ)
                                        car::qqPlot(resid( reduced_full))
                                        scatter.smooth(residuals(reduced_full) ~ fitted(reduced_full))

        eeduced_full_nonlylevel<-df_mod %>%     #WINNING MODEL
                                        #ggplot(aes(y=meanScore, x=level, fill=wideStartSD)) +
                                        #geom_boxplot() 
                                        lmer(corectedScore ~ 
                                                level + 
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(eeduced_full_nonlylevel)
                                        car::qqPlot(resid( eeduced_full_nonlylevel))
                                        scatter.smooth(residuals(eeduced_full_nonlylevel) ~ fitted(eeduced_full_nonlylevel))


        AIC(mostsimple, 
                                nointeraction,
                                eeduced_full_nonlylevel,
                                reduced_full_noSPQ)        

        anova(mostsimple, 
                                nointeraction,
                                eeduced_full_nonlylevel,
                                reduced_full_noSPQ, test="Chisq")

        r.squaredGLMM(eeduced_full_nonlylevel)

   #######################
   #beam 
   #######################
            #lowest AIC = 
            
            mostsimple_beam<-df_mod_confidence %>% 
                                        lmer(meanconf ~ 1 +
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )
                                                                        summary(mostsimple_beam)

            nointeraction_beam_full<-df_mod_confidence %>% 
                                                lmer(meanconf ~ wideStartSD + level + spqH + gender + age + SDHigh + ethnicity 
                                                                + employment + education + device_type +
                                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                                optCtrl=list(maxfun=2e7)))
                                                summary(nointeraction_beam_full)
      
                   
            
            Interaction_full_beam<-  df_mod_confidence %>%  
                                        lmer(meanconf ~ wideStartSD + spqH*SDHigh*level + gender + age  + ethnicity 
                                                                + employment + education + device_type +
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(Interaction_full_beam)
                                        car::qqPlot(resid(Combined_score_full_beam))
                                        scatter.smooth(residuals(Combined_score_full_beam) ~ fitted(Combined_score_full_beam))

            r.squaredGLMM(MASQCombined_score_reduced)
            




            highnoiseasIV<-df_mod_confidence %>%
                                        #ggplot(aes(x=highnoise, y=meanconf, fill=spqH)) +
                                       # geom_boxplot()
                                        lmer(meanconf ~ 
                                                spqH*highnoise + level +
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(highnoiseasIV)
                                        car::qqPlot(resid( reduced_full_beam))
                                        scatter.smooth(residuals(reduced_full_beam) ~ fitted(reduced_full_beam))

                  test<-df_mod_confidence %>%
                                        #ggplot(aes(x=highnoise, y=meanconf, fill=spqH)) +
                                       # geom_boxplot()
                                        lmer(meanconf ~ 
                                                spqH:level + highnoise*level +
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(test)

           highnoiseAndLevel<-df_mod_confidence %>% ##WINNING
                                        #ggplot(aes(x=highnoise, y=meanconf, fill=spqH)) +
                                       # geom_boxplot()
                                        lmer(meanconf ~ 
                                                level*spqH + highnoise +
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(highnoiseAndLevel)
                                        car::qqPlot(resid( highnoiseAndLevel))
                                        scatter.smooth(residuals(highnoiseAndLevel) ~ fitted(highnoiseAndLevel))

           levelonlyasIV<-df_mod_confidence %>%
                                        #ggplot(aes(x=highnoise, y=meanconf, fill=spqH)) +
                                       # geom_boxplot()
                                        lmer(meanconf ~ 
                                                level*spqH  +
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(highnoiseasIV)
                                        car::qqPlot(resid( reduced_full_beam))
                                        scatter.smooth(residuals(reduced_full_beam) ~ fitted(reduced_full_beam))
            
 
        AIC(mostsimple_beam,
                        nointeraction_beam,
                        highnoiseasIV,                                             
                        highnoiseAndLevel,
                        levelonlyasIV,
                        Combined_score_full_beam)        

        anova(mostsimple_beam,
                        nointeraction_beam,
                        highnoiseasIV,                                             
                        highnoiseAndLevel,
                        levelonlyasIV,
                        Combined_score_full_beam, test="Chisq")

        r.squaredGLMM(highnoiseAndLevel)    

#######################
#Success 
#######################
            #lowest AIC = Combined_score_full_success
            
            mostsimple_success<-df_mod %>% 
                        lmer(sumsuccess ~ 1 +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        summary(mostsimple_success)

            nointeraction_success<-df_mod %>% 
                                        lmer(sumsuccess ~ 
                                                  wideStartSD + level + spqH + gender + age + education + ethnicity + 
                                                 device_type + employment + 
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)))
                                        summary(nointeraction_success)
                                        car::qqPlot(resid(nointeraction_success))
                                        scatter.smooth(residuals(nointeraction_success) ~ fitted(nointeraction_success))
                   
            reduced_full_success<-df_mod %>% #WINNING
                                        lmer(sumsuccess ~ 
                                                device_type + ethnicity + education + spqH + level +
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(reduced_full_success)
                                        car::qqPlot(resid( reduced_full_success))
                                        scatter.smooth(residuals(reduced_full_success) ~ fitted(reduced_full_success))
            
            Combined_score_full_success<-  df_mod %>%  
                                        lmer(sumsuccess ~ 
                                                wideStartSD*spqH + gender + age + education + ethnicity + 
                                                 device_type + employment + level*spqH +
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(Combined_score_full_success)
                                        car::qqPlot(resid(Combined_score_full_success))
                                        scatter.smooth(residuals(Combined_score_full_success) ~ fitted(Combined_score_full_success))

        reduced_int<-df_mod %>%   
                                        lmer(sumsuccess ~ 
                                                level*spqH +  education + ethnicity + 
                                                 device_type + (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(reduced_int)
                                        car::qqPlot(resid(reduced_int))
                                        scatter.smooth(residuals(reduced_int) ~ fitted(reduced_int))


            AIC(mostsimple_success, nointeraction_success, reduced_full_success, Combined_score_full_success, reduced_int)            
                anova(mostsimple_success, nointeraction_success, reduced_full_success, Combined_score_full_success, reduced_int, test="Chisq")

                r.squaredGLMM(ints_reduced_perfe)   
###############
#PE
###############
                df_mod %>%     
                         mutate(logmeanPE=log(meanPE)) %>%
                         ggplot(aes(x=logmeanPE)) +
                         geom_density()
                
                mostsimple_logmeanPE<-df_mod %>% 
                                mutate(logmeanPE=log(meanPE)) %>%
                                #ggplot(aes(x=logmeanPE)) +
                                #geom_density()
                                lmer(logmeanPE ~ 1 +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )
                                                                summary(mostsimple_logmeanPE)

                noints<-df_mod %>% 
                                mutate(logmeanPE=log(meanPE)) %>%
                                #ggplot(aes(x=logmeanPE)) +
                                #geom_density()
                                lmer(logmeanPE ~ level + spqH + ethnicity + employment + education + 
                                                device_type + trialName + wideStartSD +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )
                                                                summary(noints)
                
                ints<-df_mod %>% 
                                mutate(logmeanPE=log(meanPE)) %>%
                                #ggplot(aes(x=logmeanPE)) +
                                #geom_density()
                                lmer(logmeanPE ~ education + device_type + level*spqH + trialName +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )
                                                                summary(ints)

                ints_reduced<-df_mod %>% 
                                mutate(logmeanPE=log(meanPE)) %>%
                                #ggplot(aes(x=logmeanPE)) +
                                #geom_density()
                                lmer(logmeanPE ~ education + device_type + level*wideStartSD +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )
                                                                summary(ints_reduced)

              ints_reduced_2<-df_mod %>% #WINNING
                                mutate(logmeanPE=log(meanPE)) %>%
                                #ggplot(aes(x=logmeanPE)) +
                                #geom_density()
                                lmer(logmeanPE ~ education + device_type + level +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )
                                                                summary(ints_reduced_2)


                AICc(mostsimple_logmeanPE, noints, ints,ints_reduced, ints_reduced_2)

                anova(mostsimple_logmeanPE, noints, ints,ints_reduced, ints_reduced_2, test="Chisq")

                r.squaredGLMM(ints_reduced_2)    

###############
#PerfE
###############
                df_mod %>%     
                         mutate(logmeanPerfE=log(meanPerfE)) %>%
                         ggplot(aes(x=logmeanPerfE)) +
                         geom_density()
                
                mostsimple_logmeanPerfE<-df_mod %>% 
                                mutate(logmeanPerfE=log(meanPerfE)) %>%
                                #ggplot(aes(x=logmeanPE)) +
                                #geom_density()
                                lmer(logmeanPerfE ~ 1 +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )
                                                                summary(mostsimple_logmeanPerfE)

                noints_perfe<-df_mod %>% 
                                mutate(logmeanPerfE=log(meanPerfE)) %>%
                                #ggplot(aes(x=logmeanPE)) +
                                #geom_density()
                                lmer(logmeanPerfE ~ level + spqH + ethnicity + employment + education + 
                                                device_type + trialName + wideStartSD +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )
                                                                summary(noints_perfe)
                
                ints_perfe<-df_mod %>% 
                                mutate(logmeanPerfE=log(meanPerfE)) %>%
                                #ggplot(aes(x=logmeanPE)) +
                                #geom_density()
                                lmer(logmeanPerfE ~ education + device_type + level*spqH + trialName +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )
                                                                summary(ints_perfe)


              ints_reduced_perfe<-df_mod %>% #WINNING - if including int with spqh see neg trend sig for l5v4 - worse aic
                                mutate(logmeanPerfE=log(meanPerfE)) %>%
                                #ggplot(aes(x=logmeanPE)) +
                                #geom_density()
                                lmer(logmeanPerfE ~ education + device_type + level +
                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )
                                                        summary(ints_reduced_perfe)


                AICc(mostsimple_logmeanPerfE, noints_perfe, ints_perfe,ints_reduced_perfe)

                anova(mostsimple_logmeanPerfE, noints_perfe, ints_perfe,ints_reduced_perfe, test="Chisq")

                r.squaredGLMM(ints_reduced_perfe)    