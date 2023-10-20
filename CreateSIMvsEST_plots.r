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

        path<-"/Users/tonigibbs-dean/Documents/SpaceTaskProject/SPQ/Results"

            #read in main data frame  

            spqall<-readRDS("/Users/tonigibbs-dean/Documents/PHD/SpaceTaskProject/SPQ/Intermediate_Results/SPQTibble.RDS" ) 

                #data re mean estimates 
            L3_realEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_REAL_meanEsts.csv")
            L3_simEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_SIM_meanEsts.csv")
            
            L4_realEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_REAL_meanEsts.csv")
            L4_simEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_SIM_meanEsts.csv")
            
            L5_realEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_REAL_meanEsts.csv")
            L5_simEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_SIM_meanEsts.csv")
            
            L6_realEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_REAL_meanEsts.csv")
            L6_simEsts<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_SIM_meanEsts.csv")


            L3_realEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_REAL_meanEsts_reversed.csv")
            L3_simEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_SIM_meanEsts_reversed.csv")
            
            L4_realEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_REAL_meanEsts_reversed.csv")
            L4_simEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_SIM_meanEsts_reversed.csv")
            
            L5_realEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_REAL_meanEsts_reversed.csv")
            L5_simEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_SIM_meanEsts_reversed.csv")
            
            L6_realEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_REAL_meanEsts_reversed.csv")
            L6_simEsts_rev<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_SIM_meanEsts_reversed.csv")


    #data re BW estimates 
            L3_realEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_REAL_BWests.csv")
            L3_simEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_SIM_BWests.csv")
            
            L4_realEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_REAL_BWests.csv")
            L4_simEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_SIM_BWests.csv")
            
            L5_realEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_REAL_BWests.csv")
            L5_simEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_SIM_BWests.csv")
            
            L6_realEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_REAL_BWests.csv")
            L6_simEsts_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_SIM_BWests.csv")


            L3_realEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_REAL_BWests_reversed.csv")
            L3_simEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L3_SIM_BWests_reversed.csv")
            
            L4_realEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_REAL_BWests_reversed.csv")
            L4_simEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L4_SIM_BWests_reversed.csv")
            
            L5_realEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_REAL_BWests_reversed.csv")
            L5_simEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L5_SIM_BWests_reversed.csv")
            
            L6_realEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_REAL_BWests_reversed.csv")
            L6_simEsts_rev_BW<-read.csv("/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF/L6_SIM_BWests_reversed.csv")

################################################################################################################################################
#functions
################################################################################################################################################

            plotting.SIMvsEST.sd<-function(ParticipantRoverPos_df, SimRoverPos_df, REAL_EST, SIM_EST, levels=c(3,4,5,6), direction) { 

                        if (direction == "forward") {
                                forward<-TRUE
                                }else if (direction == "reverse") {
                                    forward<-FALSE
                                    }else {break}
                
                # get df of trial means and sds from underlying contingencies 
                    mean_sd_df<-spqall %>%
                                                    mutate(forward= trialName %in% c("spacetask003","spacetask012" )) %>%
                                                    dplyr::filter(level %in% levels) %>%
                                                    filter(forward) %>%
                                                    mutate(PID=as.factor(Participant.Private.ID)) %>%
                                                    filter(as.numeric(PID)==1) %>%
                                                    group_by(Mean) %>%
                                                    dplyr::select(Mean, SD) %>%
                                                    rowid_to_column("ID") %>% 
                                                    mutate(SDmax=(Mean+SD/2)) %>%
                                                    mutate(SDmin=(Mean-SD/2))

                # get participant rover positions from ParticipantRoverPos_df
                    mean_sd_prp_df<-ParticipantRoverPos_df %>%
                                            rowwise() %>%
                                            summarise(median_partEsts=median(c_across(1:length(REAL_EST)))) %>%
                                            rowid_to_column("ID") %>%
                                            left_join(., mean_sd_df, "ID") %>%
                                            mutate(medianParticipantPos=(median_partEsts/100)-0.5) %>%
                                            select(!median_partEsts)

                # get sim rover positions from roverPos_df
                    mean_sd_prp_srp_df<-SimRoverPos_df %>%
                                            rowwise() %>%
                                            summarise(median_partEsts=median(c_across(1:length(REAL_EST)))) %>%
                                            rowid_to_column("ID") %>%
                                            left_join(., mean_sd_prp_df, "ID") %>%
                                            mutate(medianSimulationPos=(median_partEsts/100)-0.5) %>%
                                            select(!median_partEsts)

                # get df of median perp bws and add to previous df
                    mean_sd_prp_srp_pbw_df<-REAL_EST %>%
                                            rowwise() %>%
                                            summarise(median_partEsts=median(c_across(1:length(REAL_EST)))) %>%
                                            rowid_to_column("ID") %>%
                                            left_join(., mean_sd_prp_srp_df, "ID")

                # get df of median sim bws and add to previous df
                    preplot_df<-SIM_EST %>%
                                rowwise() %>%
                                summarise(median_simEsts=median(c_across(1:length(SIM_EST)))) %>% 
                                rowid_to_column("ID") %>%
                                left_join(., mean_sd_prp_srp_pbw_df, "ID")

                # create max and min beam positions for participants and sims
                    plot_df<-preplot_df%>%
                                mutate(partBeamMax=medianParticipantPos+median_partEsts/2,
                                       partBeamMin=medianParticipantPos-median_partEsts/2,
                                       simBeamMax=medianSimulationPos+median_simEsts/2,
                                       simBeamMin=medianSimulationPos-median_simEsts/2)



                # make plot
                     plot<-ggplot(plot_df, aes(x=ID)) +
                                                        geom_ribbon(aes(ymin=partBeamMin, ymax=partBeamMax), fill="grey", alpha=0.5) +
                                                        geom_line(aes(y=medianParticipantPos), alpha=0.8) +
                                                        geom_ribbon(aes(ymin=simBeamMin, ymax=simBeamMax), fill="orange", alpha=0.2) +
                                                        geom_line(aes(y=medianSimulationPos), alpha=0.8, colour="orange") +
                                                        geom_line(aes(y=Mean), colour="blue", linetype=2, alpha=0.5) +
                                                        geom_line(aes(y=SDmax), colour="blue", linetype="dotted", alpha=0.5) +
                                                        geom_line( aes(y=SDmin), colour="blue", linetype="dotted", alpha=0.5) +
                                                        theme_classic() +
                                                        theme( axis.ticks.x = element_blank(),
                                                                axis.ticks.y = element_blank(),
                                                                axis.title.x = element_blank(),
                                                                axis.title.y = element_blank()) +
                                                        coord_cartesian(ylim=c(-0.6,0.6)) 
                                
                    return(plot)

            }


#########################################################################################################
# Print plots sim vs ests####################################################
########################################################################################################

        L3plots_rev<-plotting.SIMvsEST.sd(ParticipantRoverPos_df= L3_realEsts, SimRoverPos_df=L3_simEsts, L3_realEsts_BW, SIM_EST=L3_simEsts_BW, 3, "forward")
        L4plots_rev<-plotting.SIMvsEST.sd(ParticipantRoverPos_df= L4_realEsts, SimRoverPos_df=L4_simEsts, L4_realEsts_BW, L4_simEsts_BW, 4, "forward")
        L5plots_rev<-plotting.SIMvsEST.sd(ParticipantRoverPos_df= L5_realEsts, SimRoverPos_df=L5_simEsts, L5_realEsts_BW, L5_simEsts_BW, 5, "forward")
        L6plots_rev<-plotting.SIMvsEST.sd(ParticipantRoverPos_df= L6_realEsts, SimRoverPos_df=L6_simEsts, L6_realEsts_BW, L6_simEsts_BW, 6, "forward")

                    simvsests_plots<-L3plots_rev/L4plots_rev/L5plots_rev/L6plots_rev
                    simVSest_plot<-simvsests_plots + plot_annotation(tag_levels = c("A", "B", "C", "D"))

                    ggsave(simVSest_plot, file=file.path(path,"simVSest_plots_SDandMean.pdf"))
