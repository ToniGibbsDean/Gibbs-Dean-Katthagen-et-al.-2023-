

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
            library(corrr)s
            #take scientific notation off
            options(scipen=999)
            library(sjPlot)
            library(data.table)

                greenParaColour<-"#6fb24b"
            MASQdepColour<-"#676ed4"
            MASQanxColour<-"#b74d86"
            PDIColour<-"#b6b638"
            SPQColout<-""
            
            Comp1<-"#8951a5"
            Comp2<-"#58bf7a"
            Comp3<-"#be4a5b"
            Comp4<-"#afab4f"
            Comp5<-"#a47e3c"
            Comp6<-"#43c8ac"
            Comp7<-""
            Comp8<-""
            Comp9<-""
            
            L2Col<-"#c75f34"
            L3Col<-"#648cd5"
            L4Col<-"#cc8c33"
            L5Col<-"#588234"
            L6Col<-"#ae4837"

        path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/Genpop2/Results"

################################################################################################
# 2. load data and make any additional data cleaning exclusions 
################################################################################################
  #Definite exclusions and mutations 
    
    spqtibble<-readRDS("/Users/tonigibbs-dean/Documents/SpaceTaskProject/SPQ/Intermediate_Results/SPQTibble.RDS") %>%
                    filter(SPQsum != "n/a") %>% #removes 3
                    filter(level!="1") %>%
                    filter(Participant.Device.Type %in% c("computer", "mobile")) %>% #removes 3
                    mutate(spqH=SPQsum>=30)%>%
                    mutate(SDHigh=(SD==0.12)) %>%
                    mutate(level=as.factor(level)) %>%
                    mutate(meanchange=Mean != dplyr::lag(Mean)) %>%
                    mutate(PREmeanchange= lead(Mean != dplyr::lag(Mean))) %>%
                    mutate(POSTmeanchange= lag(Mean != dplyr::lag(Mean))) %>%
                    mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                    mutate(highVolatility=level %in% c(4:6)) %>%
                     mutate(correctedL6trialScore = case_when(   level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                            level == 6 & trialScore>0  ~ trialScore/2,
                                            TRUE ~ as.numeric(trialScore))) %>%
                    mutate(forward=trialName %in% c("spacetask003","spacetask012"), 
                           reverse=trialName %in% c("spacetask003reversed","spacetask012reversed")) 



################################################################################################
#Recoding of demographics where needed
################################################################################################
        #recoding demograhpic variables so the groups arent too small etc. :
        #Groups combined in some istances listed below:
                #gender: other specified they were female in the text option
                #Education A level with AS and other (as text says higher national diploma in both); PhD and postgrad
                #Ethnicity – issue with prefer not to say as 1 – mixed category shouild now be mixed or non specified 
                #Employment – other and prefer not to say combined 
        
        #recoding code - switch in the demos of interest 
      spq<- spqtibble %>%
                        mutate(Ethnicity=as.character(Ethnicity)) %>%
                        mutate(Education=as.character(Education)) %>%
                        mutate(Gender=as.character(Gender)) %>%
                        mutate(Ethnicity=recode(Ethnicity, "6" = "2")) %>% 
                        mutate(Education=recode(Education, "1" = "2", "3" = "4", "5" = "6", "7" = "6"))


############################################################################################################################################
# eyeballing relationship between PE and position change 
############################################################################################################################################
    x<-   spq %>%
            group_by(Participant.Private.ID, level) %>% 
            summarise(sPCmed=mean(signedPositionChange), sPEmed=mean(signedPE)) %>%

            ggplot(aes(x = sPCmed, y = sPEmed, color = level)) + #
            geom_point(size = 1, alpha = .5) +
                    #geom_smooth(method = "lm", size = 1, se = FALSE, fullrange=TRUE, alpha = .2) + 
                    xlab("Signed Position Change") + 
                    ylab("Signed PE") + 
                    theme(text = element_text(size=12)) #+ 
                    #theme(legend.position = "none") +
                    #theme(plot.title = element_text(size = 14, face = "bold")) #+
                    #facet_wrap(~level)
    x<-   spq %>%
            group_by(Participant.Private.ID, level) %>% 
            ggplot(aes(x = signedPositionChange, y = signedPE, color = level)) + #
            geom_point(size = 1, alpha = .5) +
                    #
            geom_smooth(method = "lm", size = 1, se = FALSE, fullrange=TRUE, alpha = .2) + 
                    xlab("Signed Position Change") + 
                    ylab("Signed PE") + 
                    theme(text = element_text(size=12)) #+ 
                    #theme(legend.position = "none") +
                    #theme(plot.title = element_text(size = 14, face = "bold")) #+
                    #facet_wrap(~level)
############################################################################################################################################
#create df 1
############################################################################################################################################
                                                                                  
GetBeta<-function(PE, PC) { 
             x<-lm(scale(PE) ~ scale(PC))
            return(x$coef[2])
            }
                              
        df_mod<-spq %>%
            #mutate(gamers=gamingtime.quant>2) %>% 
            group_by(Participant.Private.ID, level) %>%
            mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
             mutate(beta=GetBeta(PE= signedPE, PC=signedPositionChange)) %>%
            summarise(meanScore=mean(trialScore), 
                    beta=dplyr::first(beta),
                    corectedScore=mean(correctedL6trialScore),
                    #meanchange=dplyr::first(meanchange),
                    #PREmeanchange=dplyr::first(PREmeanchange),
                    #POSTmeanchange=dplyr::first(POSTmeanchange),
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
                     df_mod <- df_mod %>% drop_na
            # Get participant-specific LR by block

                        df_mod$level<-droplevels(df_mod$level)
                      
                cMat <- cbind( c(-1,1,0,0), # 3 vs 4
                        c(0,-1,1,0), # 4 vs 5
                        c(0,0,-1,1)) #5vs 6
                contrasts(df_mod$level) <-cMat
                colnames(attr(df_mod$level, "contrasts")) <- c("3v4", "4v5", "5v6")

############################################################################################################################################
#Modelling
############################################################################################################################################

            mostsimple<-df_mod %>% 
                        lmer(beta ~ 1 +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        summary(mostsimple)

    
             LR_fullmod<-  df_mod %>%  #no effect of apq group - some for demographics 
                                        lmer(beta ~ 
                                                 gender + age + education + ethnicity + 
                                                 device_type + employment + level*spqH +
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(LR_fullmod)
                                        car::qqPlot(resid(LR_fullmod))
                                        scatter.smooth(residuals(LR_fullmod) ~ fitted(LR_fullmod))

    
            LR_reduced_mod<-  df_mod %>%  #no effect of apq group - some for demographics 
                                        lmer(beta ~ 
                                                gender + education + ethnicity + 
                                                level + 
                                                (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )

                                        summary(LR_reduced_mod)
                                        car::qqPlot(resid(LR_reduced_mod))
                                        scatter.smooth(residuals(LR_reduced_mod) ~ fitted(LR_reduced_mod))

                 AIC(mostsimple, LR_fullmod, LR_reduced_mod)            
                  anova(mostsimple, LR_fullmod, LR_reduced_mod, test="Chisq")
  

   plot1<-   modeldf %>%
                            group_by(Participant.Private.ID, condition) %>%
                                        ggplot(aes(x=condition, y=beta, fill=isolatedChange)) +
                                        geom_boxplot()
            