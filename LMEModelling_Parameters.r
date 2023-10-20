################################################################################################################################################
# 1. Packages and path to project directory ######
################################################################################################################################################
        
    #set seed and load pacakges 
        set.seed(0.1)
    
        library(tidyverse)
        library(ggplot2)
        #library(ggplotify)
        library(nlme)
        library(lme4)
        library(lmerTest)
        library(sjPlot)
        library(patchwork)

        options(scipen=999)

        path<-"/Users/tonigibbs-dean/Documents/PHD/SpaceTaskProject/SPQ/Results"

    #load colours
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
        
        L2Col<-"#c75f34"
        L3Col<-"#648cd5"
        L4Col<-"#cc8c33"
        L5Col<-"#588234"
        L6Col<-"#ae4837"


    #read in parameters - L5 MAY CHANGE and l6 as s=now doing the error message iterations thing
    #these parameters were created using the new SPQ_HGF-creatingInputs_Responses file so I could ensure the ordering 

        L3_params_F<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_level_3.csv") 
        L4_params_F<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_level_4.csv")
        L5_params_F<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_level_5.csv")
        L6_params_F<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_level_6.csv")

        L3_params_R<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_REVERSE_level_3.csv")
        L4_params_R<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_REVERSE_level_4.csv")
        L5_params_R<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_REVERSE_level_5.csv")
        L6_params_R<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/est_withnb_REVERSE_level_6.csv")


    #read in main data frame  

            spqall<-readRDS("/Users/tonigibbs-dean/Documents/PHD/SpaceTaskProject/SPQ/Intermediate_Results/SPQTibble.RDS" ) 
            
            spqForward<-spqall %>%
                        filter(trialName %in% c('spacetask003', 'spacetask012')) 

            spqReversed<-spqall %>%
                        filter(trialName %in% c('spacetask003reversed', 'spacetask012reversed')) 


################################################################################################################################################
#joining datasets, making dataframe
################################################################################################################################################

        #get list of IDs in the order they were fed into the HGF 
        #bind ID to params
            Participant.Private.ID<-spqForward %>%
                                                group_by(Participant.Private.ID) %>%
                                                filter(level %in% c(3:6))  %>%
                                                select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                pull(1) %>%
                                                unique

            ParamsandIDs<-cbind(L3_params_F, L4_params_F, L5_params_F, L6_params_F, Participant.Private.ID)

            #forwardIDstoExc<-unique(ParamsandIDs$Participant.Private.ID[14, 3, 8, 12, 49, 48])
            
            L3fIDexc<-unique(ParamsandIDs$Participant.Private.ID[14])
            L4fIDexc<-unique(ParamsandIDs$Participant.Private.ID[c(3,8,12, 49)])
            L5fIDexc<-unique(ParamsandIDs$Participant.Private.ID[48])


            Participant.Private.ID<-spqReversed %>%
                                                group_by(Participant.Private.ID) %>%
                                                filter(level %in% c(3:6))  %>%
                                                select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                                pull(1) %>%
                                                unique
            ParamsandIDsReversed<-cbind(L3_params_R, L4_params_R, L5_params_R, L6_params_R, Participant.Private.ID)

            L4rIDexc<-unique(ParamsandIDs$Participant.Private.ID[73])
            L5rIDexc<-unique(ParamsandIDs$Participant.Private.ID[1])
            L6rIDexc<-unique(ParamsandIDs$Participant.Private.ID[c(11,39)])

            #L4r<-73
            #L5r<-1
            #L6r<-11, 39

        #left join to main data
            temp <- spqall %>%
                                        group_by(Participant.Private.ID) %>%
                                        left_join(x=spqall, y=ParamsandIDs, by="Participant.Private.ID")
                                    

            SPQwithParams <- temp %>%
                                        group_by(Participant.Private.ID) %>%
                                        left_join(x=temp, y=ParamsandIDsReversed, by="Participant.Private.ID") 
                                        
     
################################################################################################################################################
#creating modelling df
################################################################################################################################################

        #plots of parameters that arent fixed 
        
        plotting<- SPQwithParams %>%
                                group_by(Participant.Private.ID) %>%
                                #filter(level %in% c(3,4)) %>%
                                filter(SPQsum!="NA") %>%
                                #filter(Participant.Device.Type %in% c("computer", "mobile")) %>% #removes 3
                                #filter(level=="3") %>%
                                mutate(spqH=SPQsum>=30) %>%
                                mutate(startSDHigh=trialName %in% c("spacetask012", "spacetask012reversed")) %>%
                                mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                                summarise(  
                                            trialName=dplyr::first(trialName),
                                            gender=dplyr::first(Gender),
                                            age=dplyr::first(Age),
                                            device_type=dplyr::first(Participant.Device.Type),
                                            education=dplyr::first(Education),
                                            spqH=dplyr::first(spqH),
                                            startSDHigh=dplyr::first(startSDHigh),                                  
                                            L3_mux_F=dplyr::first(L3_mux_F), 
                                            L3_mux_R=dplyr::first(L3_mux_R),
                                            L3_saa_0_F=dplyr::first(L3_saa_0_F), 
                                            L3_saa_0_R=dplyr::first(L3_saa_0_R), 
                                            L3_kax_F=dplyr::first(L3_kax_F),
                                            L3_kax_R=dplyr::first(L3_kax_R),
                                            L3_kaa_F=dplyr::first(L3_kaa_F), 
                                            L3_kaa_R=dplyr::first(L3_kaa_R), 
                                            L3_be1_F=dplyr::first(L3_be1_F),
                                            L3_be1_R=dplyr::first(L3_be1_R),
                                            L3_zem_F=dplyr::first(L3_zem_F), 
                                            L3_zem_R=dplyr::first(L3_zem_R), 
                                            L3_zes_F=dplyr::first(L3_zes_F), 
                                            L3_zes_R=dplyr::first(L3_zes_R), 
                                            L4_mux_F=dplyr::first(L4_mux_F), 
                                            L4_mux_R=dplyr::first(L4_mux_R), 
                                            L4_saa_0_F=dplyr::first(L4_saa_0_F),
                                            L4_saa_0_R=dplyr::first(L4_saa_0_R),
                                            L4_kax_F=dplyr::first(L4_kax_F),
                                            L4_kax_R=dplyr::first(L4_kax_R),
                                            L4_kaa_F=dplyr::first(L4_kaa_F), # fixed
                                            L4_kaa_R=dplyr::first(L4_kaa_R), # fixed
                                            L4_be1_F=dplyr::first(L4_be1_F),
                                            L4_be1_R=dplyr::first(L4_be1_R),
                                            L4_zem_F=dplyr::first(L4_zem_F), 
                                            L4_zem_R=dplyr::first(L4_zem_R), 
                                            L4_zes_F=dplyr::first(L4_zes_F), 
                                            L4_zes_R=dplyr::first(L4_zes_R),
                                            L5_mux_F=dplyr::first(L5_mux_F), 
                                            L5_mux_R=dplyr::first(L5_mux_R), 
                                            L5_saa_0_F=dplyr::first(L5_saa_0_F),
                                            L5_saa_0_R=dplyr::first(L5_saa_0_R),
                                            L5_kax_F=dplyr::first(L5_kax_F),
                                            L5_kax_R=dplyr::first(L5_kax_R),
                                            L5_kaa_F=dplyr::first(L5_kaa_F), 
                                            L5_kaa_R=dplyr::first(L5_kaa_R), 
                                            L5_be1_F=dplyr::first(L5_be1_F),
                                            L5_be1_R=dplyr::first(L5_be1_R),
                                            L5_zem_F=dplyr::first(L5_zem_F), 
                                            L5_zem_R=dplyr::first(L5_zem_R), 
                                            L5_zes_F=dplyr::first(L5_zes_F), 
                                            L5_zes_R=dplyr::first(L5_zes_R),
                                            L6_mux_F=dplyr::first(L6_mux_F), 
                                            L6_mux_R=dplyr::first(L6_mux_R), 
                                            L6_saa_0_F=dplyr::first(L6_saa_0_F),
                                            L6_saa_0_R=dplyr::first(L6_saa_0_R),
                                            L6_kax_F=dplyr::first(L6_kax_F),
                                            L6_kax_R=dplyr::first(L6_kax_R),
                                            L6_kaa_F=dplyr::first(L6_kaa_F), 
                                            L6_kaa_R=dplyr::first(L6_kaa_R), 
                                            L6_be1_F=dplyr::first(L6_be1_F),
                                            L6_be1_R=dplyr::first(L6_be1_R),
                                            L6_zem_F=dplyr::first(L6_zem_F), 
                                            L6_zem_R=dplyr::first(L6_zem_R), 
                                            L6_zes_F=dplyr::first(L6_zes_F), 
                                            L6_zes_R=dplyr::first(L6_zes_R)) %>%                                                                                   
                                unite("L3_mux", L3_mux_F:L3_mux_R, na.rm = TRUE, remove = TRUE) %>% #SET TO FLASE if you want to see the OG cols and vals
                                unite("L3_saa", L3_saa_0_F:L3_saa_0_R, na.rm = TRUE, remove = TRUE) %>%
                                unite("L3_kax", L3_kax_F:L3_kax_R, na.rm = TRUE, remove = TRUE) %>%
                                unite("L3_kaa", L3_kaa_F:L3_kaa_R, na.rm = TRUE, remove = TRUE) %>%          
                                unite("L3_be1", L3_be1_F:L3_be1_R, na.rm = TRUE, remove = TRUE) %>%            
                                unite("L3_zem", L3_zem_F:L3_zem_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L3_zes", L3_zes_F:L3_zes_R, na.rm = TRUE, remove = TRUE) %>% 
                                unite("L4_mux", L4_mux_F:L4_mux_R, na.rm = TRUE, remove = TRUE) %>%                  
                                unite("L4_saa_0", L4_saa_0_F:L4_saa_0_R, na.rm = TRUE, remove = TRUE) %>%     
                                unite("L4_kax", L4_kax_F:L4_kax_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L4_kaa", L4_kaa_F:L4_kaa_R, na.rm = TRUE, remove = TRUE) %>% 
                                unite("L4_be1", L4_be1_F:L4_be1_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L4_zem", L4_zem_F:L4_zem_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L4_zes", L4_zes_F:L4_zes_R, na.rm = TRUE, remove = TRUE) %>%
                                unite("L5_mux", L5_mux_F:L5_mux_R, na.rm = TRUE, remove = TRUE) %>%                  
                                unite("L5_saa_0", L5_saa_0_F:L5_saa_0_R, na.rm = TRUE, remove = TRUE) %>%     
                                unite("L5_kax", L5_kax_F:L5_kax_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L5_kaa", L5_kaa_F:L5_kaa_R, na.rm = TRUE, remove = TRUE) %>% 
                                unite("L5_be1", L5_be1_F:L5_be1_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L5_zem", L5_zem_F:L5_zem_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L5_zes", L5_zes_F:L5_zes_R, na.rm = TRUE, remove = TRUE) %>%
                                unite("L6_mux", L6_mux_F:L6_mux_R, na.rm = TRUE, remove = TRUE) %>%                  
                                unite("L6_saa_0", L6_saa_0_F:L6_saa_0_R, na.rm = TRUE, remove = TRUE) %>%     
                                unite("L6_kax", L6_kax_F:L6_kax_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L6_kaa", L6_kaa_F:L6_kaa_R, na.rm = TRUE, remove = TRUE) %>% 
                                unite("L6_be1", L6_be1_F:L6_be1_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L6_zem", L6_zem_F:L6_zem_R, na.rm = TRUE, remove = TRUE) %>%           
                                unite("L6_zes", L6_zes_F:L6_zes_R, na.rm = TRUE, remove = TRUE) %>%
                                mutate( trialName=as.factor(trialName), 
                                        device_type=as.factor(device_type)) %>%
                                mutate_if(is_character, as.numeric)


########################################################################################################################################################################
# Modelling
########################################################################################################################################################################
        #Temporary data frame for modelling
                temp <- plotting %>%
                                        pivot_longer(cols = starts_with("L"), names_to = c("level", "param"), names_pattern = c("(.)_(.+)")) %>%
                                        group_by(Participant.Private.ID) %>%
                                        filter(param %in% c("be1", "kax", "kaa")) %>%
                                        mutate(level=as.factor(level)) %>%
                                        filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656")) %>% 
                                        mutate(spqH=as.factor(spqH)) %>%
                                        mutate(level=as.factor(level)) %>%
                                        mutate(param=as.factor(param)) %>% 
                                        filter(param=="be1") 
        #model selection

                simple<-   
                        lmer(value ~ 1 +
                                    (1|Participant.Private.ID) + (1|level), data=temp, REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)) )
                                                                                        

                noInt<- lmer(value ~ spqH + level +
                                                                 (1|Participant.Private.ID) + (1|level), data=temp, REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )
                
                #winning model for be1 - model did not improve b adding in other covariates                   
                Int<-     lmer(value ~ level*spqH +
                                                                (1|Participant.Private.ID) + (1|level), data=temp, REML=F,
                                                                control=lme4::lmerControl(optimizer="bobyqa", 
                                                                optCtrl=list(maxfun=2e7)) )


                AICcmodavg::AICc(simple, noInt, Int)
                anova(simple, noInt, Int, test="Chisq")


########################################################################################################################################################################
# Plot for fig 6.4 - be1 parameter difference between blocks 3 and 4 for high and low SPQ groups 
########################################################################################################################################################################

            all<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656", "4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>% #this is the final n. exlcuded
                                    #select(c(Participant.Private.ID, spqH)) %>% # just 5 and 6 cols
                                    mutate(increaseTRUE = L4_be1 > L3_be1) %>%
                                    pivot_longer(cols = starts_with("L"), names_to = c("level", "param"), names_pattern = c("(.)_(.+)")) %>% # each param is in acolumn and level is acollumn 
                                    #filter(param=="zes") %>%
                                    mutate(spqH=as.factor(spqH)) %>%
                                    mutate(level=as.factor(level)) %>%
                                    mutate(param=as.factor(param)) %>%
                                    mutate(startSDHigh=as.numeric(startSDHigh)) %>%
                                    filter(param=="be1") %>%
                                    mutate(spqNamedGroups=case_when(spqH==TRUE ~ "High SPQ",
                                                    spqH==FALSE ~ "Low SPQ" )) %>%
                                    #filter(!device_type=="tablet") %>%
                                    filter(level %in% c(3,4))

            ggplot(all, aes(y=value, x=level)) +
                                            #geom_point(aes(colour=increaseTRUE)) +
                                            geom_point(alpha=0.2)+
                                            theme_classic()+
                                                theme(axis.ticks.x = element_blank(),
                                                axis.ticks.y = element_blank()) +
                                            #geom_line(aes(group=Participant.Private.ID, colour=increaseTRUE)) +
                                            geom_line(aes(group=Participant.Private.ID), alpha=0.2) +
                                            #scale_color_manual(values=c("darkgrey", "red")) +
                                            stat_summary(fun.y=mean,geom="line",lwd=2,aes(group=1))+
                                            facet_wrap(~spqH) 
                                            facet_grid(~spqH)

                                            ggsave(all, file=file.path(path,"SPQ_be1_L3and4_nocolour.pdf"))