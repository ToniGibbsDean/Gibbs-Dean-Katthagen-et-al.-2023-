%% correlate simulated responses with real data and read out individually fitted parameters
load est_withnb_REVERSE_level_3.mat % load in the estimated model here
load est_withnb_level_3.mat
load est_withnb_REVERSE_level_4.mat
load est_withnb_level_4.mat
load est_withnb_REVERSE_level_5.mat
load est_withnb_level_5.mat
load est_withnb_REVERSE_level_6.mat
load est_withnb_level_6.mat


muahat = NaN(length(est(1).y),length(est));
bw_real = NaN(length(est(1).y),length(est));
bw_sim = NaN(length(est(1).y),length(est));
mean_real = NaN(length(est(1).y),length(est));
mean_sim = NaN(length(est(1).y),length(est));

for sub = 1:length(est)

    fa  = est(sub).y(:,2);
    bw_real(:,sub) = fa; 
    
    fafa  = est(sub).y(:,1);
    mean_real(:,sub) = fafa; 
    
    ta  = est(sub).optim.yhat(:,2);
    bw_sim(:,sub) = ta; 
    
    tata  = est(sub).optim.yhat(:,1);
    mean_sim(:,sub) = tata; 
    
    bla             = est(sub).traj.muahat(:,1);
    muahat(:, sub)  = bla;
    
    % store the individual parameters
    mux(sub)   = est(sub).p_prc.mux_0(1);  % initial belief about mean (close to first input)
    saa_0(sub) = est(sub).p_prc.saa_0(1);  % initial uncertainity about SD, should actually be fixed
    kax(sub)   = est(sub).p_prc.kax;       % kappa for mean learning (sometimes fixed to 0)
    kaa(sub)   = est(sub).p_prc.kaa;       % kappa for SD learning (sometimes fixed to 0)
    be1(sub)   = est(sub).p_obs.be1;       % free factor for use of noise belief
    zem(sub)   = est(sub).p_obs.zem;       % noise in mean learning
    zes(sub)   = est(sub).p_obs.zes;       % noise in SD learning
    
    clear fa bla ta fafa tata 
    
 
    

end

L3R= table(mux.', saa_0.', kax.', kaa.', be1.', zem.', zes.', 'VariableNames', {'L3_mux_R','L3_saa_0_R', 'L3_kax_R','L3_kaa_R','L3_be1_R','L3_zem_R','L3_zes_R'});
L3F= table(mux.', saa_0.', kax.', kaa.', be1.', zem.', zes.', 'VariableNames', {'L3_mux_F','L3_saa_0_F', 'L3_kax_F','L3_kaa_F','L3_be1_F','L3_zem_F','L3_zes_F'});
L4R= table(mux.', saa_0.', kax.', kaa.', be1.', zem.', zes.', 'VariableNames', {'L4_mux_R','L4_saa_0_R', 'L4_kax_R','L4_kaa_R','L4_be1_R','L4_zem_R','L4_zes_R'});
L4F= table(mux.', saa_0.', kax.', kaa.', be1.', zem.', zes.', 'VariableNames', {'L4_mux_F','L4_saa_0_F', 'L4_kax_F','L4_kaa_F','L4_be1_F','L4_zem_F','L4_zes_F'});
L5R= table(mux.', saa_0.', kax.', kaa.', be1.', zem.', zes.', 'VariableNames', {'L5_mux_R','L5_saa_0_R', 'L5_kax_R','L5_kaa_R','L5_be1_R','L5_zem_R','L5_zes_R'});
L5F= table(mux.', saa_0.', kax.', kaa.', be1.', zem.', zes.', 'VariableNames', {'L5_mux_F','L5_saa_0_F', 'L5_kax_F','L5_kaa_F','L5_be1_F','L5_zem_F','L5_zes_F'});
L6R= table(mux.', saa_0.', kax.', kaa.', be1.', zem.', zes.', 'VariableNames', {'L6_mux_R','L6_saa_0_R', 'L6_kax_R','L6_kaa_R','L6_be1_R','L6_zem_R','L6_zes_R'});
L6F= table(mux.', saa_0.', kax.', kaa.', be1.', zem.', zes.', 'VariableNames', {'L6_mux_F','L6_saa_0_F', 'L6_kax_F','L6_kaa_F','L6_be1_F','L6_zem_F','L6_zes_F'});

writetable(L3R, 'est_withnb_REVERSE_level_3.csv', 'WriteVariableNames', true) 
writetable(L3F, 'est_withnb_level_3.csv', 'WriteVariableNames', true) 
writetable(L4R, 'est_withnb_REVERSE_level_4.csv', 'WriteVariableNames', true) 
writetable(L4F, 'est_withnb_level_4.csv', 'WriteVariableNames', true) 
writetable(L5R, 'est_withnb_REVERSE_level_5.csv', 'WriteVariableNames', true) 
writetable(L5F, 'est_withnb_level_5.csv', 'WriteVariableNames', true) 
writetable(L6R, 'est_withnb_REVERSE_level_6.csv', 'WriteVariableNames', true) 
writetable(L6F, 'est_withnb_level_6.csv', 'WriteVariableNames', true) 

figure; plot(median(bw_sim,2)); hold on; plot(median(bw_real,2)); title("Median across all: real beam width (red) and simulated bw (blue)");  
figure; plot(median(mean_sim,2)); hold on; plot(median(mean_real,2)); title("Median across all: real prediction (red) and simulated prediction (blue)");  
       