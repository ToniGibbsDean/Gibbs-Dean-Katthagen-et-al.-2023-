###############################################################################################################################################
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
        library(ggpubr)

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


############################################################################################################################################################
# correlation with be1 and performance metrics 
############################################################################################################################################################

                        
            make_correlation_paramAndPerf_df<-function(parameter) {     
                        
                    performmets<-     
                        spqall %>%
                                group_by(Participant.Private.ID, level) %>%
                                filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656", "4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>% #this is the final n. exlcuded
                                filter(level %in% c(3,4,5,6)) %>%
                                summarise(meanscore=mean(trialScore), meanPE=mean(PE), meanPerfE=mean(PerfE))  %>%
                                group_by(Participant.Private.ID, level)  %>%
                                    mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                                    mutate(level=as.factor(level))
                

                        corr_params<- plotting %>%
                                                filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656", "4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>% #this is the final n. exlcuded
                                                #select(c(Participant.Private.ID, spqH)) %>% # just 5 and 6 cols
                                                mutate(increaseTRUE = L4_be1 >= L3_be1) %>%
                                                pivot_longer(cols = starts_with("L"), names_to = c("level", "param"), names_pattern = c("(.)_(.+)")) %>% # each param is in acolumn and level is acollumn 
                                                filter(param==parameter) %>%
                                                group_by(Participant.Private.ID, level) %>%
                                                mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                                                #filter(param=="zes") %>%
                                                mutate(spqH=as.factor(spqH)) %>%
                                                mutate(level=as.factor(level)) %>%
                                                mutate(param=as.factor(param)) #%>%

                    df_params_perf_corr <- corr_params %>% left_join(performmets)

                return(df_params_perf_corr)


            }


            makeCorrPlots_bylevel_andOVerall_SCORE<-function(parameter, performMetric) {

                    be1_levelwise_spqwise  <-  make_correlation_paramAndPerf_df(parameter) %>%
                                                    mutate(spqNamedGroups=case_when(spqH==TRUE ~ "High SPQ",
                                                                spqH==FALSE ~ "Low SPQ" )) %>%
                                                    ggplot(aes(x=meanscore, y=value, fill=spqNamedGroups)) +
                                                    geom_point(aes(colour=spqNamedGroups),alpha=0.2) +
                                                    geom_smooth(aes(fill=spqNamedGroups, colour=spqNamedGroups), method="lm") +
                                                    stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                            label.x.npc = 0.5, label.y.npc = 0.5) +
                                                    #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                                    theme_classic() +    
                                                    theme(axis.ticks.x = element_blank(),
                                                        axis.ticks.y = element_blank(),
                                                        axis.title.y = element_blank(),
                                                        legend.title=element_blank()) +
                                                    facet_wrap(~level) +
                                                    
                                                    #facet_wrap(~level) 
                                                    #ylim(0,15)
                                                    labs(y= "Noise Belief")
                                                    #ylim(0,15)
                                                    #labs(x = "Anxiety Score")
                                    be1_levelwise_spqwise<-be1_levelwise_spqwise + scale_fill_manual(values=c(Comp1, Comp2))
                                    be1_levelwise_spqwise<-be1_levelwise_spqwise + scale_colour_manual(values=c(Comp1, Comp2))
                    
                            be1_overall <-  make_correlation_paramAndPerf_df(parameter) %>%
                                                    mutate(spqNamedGroups=case_when(spqH==TRUE ~ "High SPQ",
                                                                spqH==FALSE ~ "Low SPQ" )) %>%
                                                    ggplot(aes(x=meanscore, y=value, fill=spqNamedGroups)) +
                                                    geom_point(aes(colour=spqNamedGroups),alpha=0.2) +
                                                    geom_smooth(aes(fill=spqNamedGroups, colour=spqNamedGroups), method="lm") +
                                                    stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                            label.x.npc = 0.5, label.y.npc = 0.5) +
                                                    #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                                    theme_classic() +    
                                                    theme(axis.ticks.x = element_blank(),
                                                        axis.ticks.y = element_blank(),
                                                        axis.title.y = element_blank(),
                                                        legend.title=element_blank()) +
                                                    #facet_wrap(~level) 
                                                    #ylim(0,15)
                                                    labs(y = "Noise Belief")
                                                    #facet_wrap(~level) 
                                                    #ylim(0,15)
                                                    #labs(x = "Anxiety Score")
                                    be1_overall<-be1_overall + scale_fill_manual(values=c(Comp1, Comp2))
                                    be1_overall<-be1_overall + scale_colour_manual(values=c(Comp1, Comp2))

                                    final<-be1_overall / be1_levelwise_spqwise
                        
                        return(final)

                }



            makeCorrPlots_bylevel_andOVerall_PE<-function(parameter, performMetric) {

                    be1_levelwise_spqwise  <-  make_correlation_paramAndPerf_df(parameter) %>%
                                                    mutate(spqNamedGroups=case_when(spqH==TRUE ~ "High SPQ",
                                                                spqH==FALSE ~ "Low SPQ" )) %>%
                                                    ggplot(aes(x=meanPE, y=value, fill=spqNamedGroups)) +
                                                    geom_point(aes(colour=spqNamedGroups),alpha=0.2) +
                                                    geom_smooth(aes(fill=spqNamedGroups, colour=spqNamedGroups), method="lm") +
                                                    stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                            label.x.npc = 0.5, label.y.npc = 0.5) +
                                                    #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                                    theme_classic() +    
                                                    theme(axis.ticks.x = element_blank(),
                                                        axis.ticks.y = element_blank(),
                                                        axis.title.y = element_blank(),
                                                        legend.title=element_blank()) +
                                                    facet_wrap(~level) +
                                                    #ylim(0,15)
                                                    #labs(x = "Anxiety Score")+
                                                    #facet_wrap(~level) 
                                                    #ylim(0,15)
                                                    labs(y = "Noise Belief")
                                    be1_levelwise_spqwise<-be1_levelwise_spqwise + scale_fill_manual(values=c(Comp1, Comp2))
                                    be1_levelwise_spqwise<-be1_levelwise_spqwise + scale_colour_manual(values=c(Comp1, Comp2))
                    
                            be1_overall <-  make_correlation_paramAndPerf_df(parameter) %>%
                                                    mutate(spqNamedGroups=case_when(spqH==TRUE ~ "High SPQ",
                                                                spqH==FALSE ~ "Low SPQ" )) %>%
                                                    ggplot(aes(x=meanPE, y=value, fill=spqNamedGroups)) +
                                                    geom_point(aes(colour=spqNamedGroups),alpha=0.2) +
                                                    geom_smooth(aes(fill=spqNamedGroups, colour=spqNamedGroups), method="lm") +
                                                    stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                            label.x.npc = 0.5, label.y.npc = 0.5) +
                                                    #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                                    theme_classic() +    
                                                    theme(axis.ticks.x = element_blank(),
                                                        axis.ticks.y = element_blank(),
                                                        axis.title.y = element_blank(),
                                                        legend.title=element_blank())+
                                                    #facet_wrap(~level) 
                                                    #ylim(0,15)
                                                    labs(y = "Noise Belief")
                                    be1_overall<-be1_overall + scale_fill_manual(values=c(Comp1, Comp2))
                                    be1_overall<-be1_overall + scale_colour_manual(values=c(Comp1, Comp2))

                                    final<-be1_overall / be1_levelwise_spqwise
                        
                        return(final)

                }


#########################################################################################################################################################################
# Make plots
#########################################################################################################################################################################

            corr_PE_be1<-makeCorrPlots_bylevel_andOVerall_PE("be1") 
                        corr_PE_be1<- corr_PE_be1 + plot_annotation(tag_levels = c("A", "B"))
                        ggsave(corr_PE_be1, file=file.path(path,"corr_PE_be1.pdf"))
            corr_PE_kax<-makeCorrPlots_bylevel_andOVerall_PE("kax")
                        corr_PE_kax<- corr_PE_kax + plot_annotation(tag_levels = c("A", "B"))
                        ggsave(corr_PE_kax, file=file.path(path,"corr_PE_kax.pdf"))
            corr_PE_kaa<-makeCorrPlots_bylevel_andOVerall_PE("kaa")
                        corr_PE_kaa<- corr_PE_kaa + plot_annotation(tag_levels = c("A", "B"))
                        ggsave(corr_PE_kaa, file=file.path(path,"corr_PE_kaa.pdf"))

            corr_score_be1<-makeCorrPlots_bylevel_andOVerall_SCORE("be1")
                            corr_score_be1<- corr_score_be1 + plot_annotation(tag_levels = c("A", "B"))
                            ggsave(corr_score_be1, file=file.path(path,"corr_score_be1.pdf"))
            corr_score_kax<-makeCorrPlots_bylevel_andOVerall_SCORE("kax")
                            corr_score_kax<- corr_score_kax + plot_annotation(tag_levels = c("A", "B"))
                            ggsave(corr_score_kax, file=file.path(path,"corr_score_kax.pdf"))
            corr_score_kaa<-makeCorrPlots_bylevel_andOVerall_SCORE("kaa")
                            corr_score_kaa<- corr_score_kaa + plot_annotation(tag_levels = c("A", "B"))
                            ggsave(corr_score_kaa, file=file.path(path,"corr_score_kaa.pdf"))




###chaecking/reporting  the corelations 
    performmets<-     
            spqall %>%
            group_by(Participant.Private.ID, level) %>%
            filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656", "4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>% #this is the final n. exlcuded
            filter(level %in% c(3,4,5,6)) %>%
             summarise(meanscore=mean(trialScore), meanPE=mean(PE), meanPerfE=mean(PerfE))  %>%
              group_by(Participant.Private.ID, level)  %>%
                mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                mutate(level=as.factor(level))

            corr_params<- plotting %>%
                                    filter(!Participant.Private.ID %in% c("4025943","3984606","3981816","3932656", "4104878", "3979667", "3983506", "4242551", "3981739", "3955234")) %>% #this is the final n. exlcuded
                                    #select(c(Participant.Private.ID, spqH)) %>% # just 5 and 6 cols
                                    mutate(increaseTRUE = L4_be1 >= L3_be1) %>%
                                    pivot_longer(cols = starts_with("L"), names_to = c("level", "param"), names_pattern = c("(.)_(.+)")) %>% # each param is in acolumn and level is acollumn 
                                    filter(param=="be1") %>%
                                     group_by(Participant.Private.ID, level) %>%
                                    mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                                    #filter(param=="zes") %>%
                                    mutate(spqH=as.factor(spqH)) %>%
                                    mutate(level=as.factor(level)) %>%
                                    mutate(param=as.factor(param)) %>%
                                    filter(spqH==FALSE) %>%
                                    filter(level==5)

        df_params_perf_corr <- corr_params %>% left_join(performmets)
        
        cor.test(df_params_perf_corr$meanPE, df_params_perf_corr$value)







        overall_kax  <-  make_correlation_paramAndPerf_df("kax") %>%
                                        #mutate(slope = betaLR_6 > betaLR_5) %>%
                                        #filter(level==4) %>%
                                        ggplot(aes(x=meanscore, y=value)) +
                                        #geom_point(aes(color=slope)) +
                                        geom_point(alpha=0.2, colour=Comp3) +
                                        geom_smooth(method="lm", colour=Comp3) +
                                       # facet_wrap(~level) +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        #geom_smooth(method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.3) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.y = element_blank()) #+
                                        #facet_wrap(~level) 
                                        #ylim(0,15)
                                        #labs(x = "Anxiety Score")
                         overall_kax<-overall_kax + scale_colour_manual(values=c(Comp3))


        overall_kaa  <-  make_correlation_paramAndPerf_df("kaa") %>%
                                        #mutate(slope = betaLR_6 > betaLR_5) %>%
                                        #filter(level==4) %>%
                                        ggplot(aes(x=meanscore, y=value)) +
                                        #geom_point(aes(color=slope)) +
                                        geom_point(alpha=0.2, colour=Comp2) +
                                        geom_smooth(method="lm", colour=Comp2) +
                                       # facet_wrap(~level) +
                                        #geom_smooth(aes(color=slope), method="lm")+
                                        #geom_smooth(method="lm") +
                                        stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                                label.x.npc = 0.5, label.y.npc = 0.3) +
                                        #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                        theme_classic() +    
                                        theme(axis.ticks.x = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.y = element_blank()) #+
                                        #facet_wrap(~level) 
                                        #ylim(0,15)
                                        #labs(x = "Anxiety Score")
                         overall_kaa<-overall_kaa + scale_colour_manual(values=c(Comp2))