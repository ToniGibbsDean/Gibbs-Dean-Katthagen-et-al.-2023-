clear all; 
close all;

%%% set the paths and load inputs
system = getenv('COMPUTERNAME');
if strcmp(system,'C15MN-PS-FO-13') % Teresa's laptop
    prefix = 'C:\users\katthagt\owncloud\';
    homepath = fullfile(prefix, 'HGF_London_Berlin\SpacetaskHGF\');
    addpath(fullfile(homepath, 'tapas\tapas-master\HGF\'));  % load tapas toolbox
    data_path =(fullfile(homepath,'INPUTS_REPONSES\'));
else
    prefix = 'T:\'; % Toni 
end

    homepath = '/Users/tonigibbs-dean/Documents/MATLAB/tapas-master/HGF'; % Toni 
    data_path =(fullfile(homepath,'INPUTS_RESPONSES/'));

% sort inputs u by level, with mean and SD for plots
input = load([data_path,'reverseinput.txt']); % same for all subjects, only load once
score = load([data_path,'trialscoreL3_6_reverse.txt']); % different for subjects

    u3(:,1)=  input(1:40);
    u3(:,2) = repmat(mean(u3(:,1,1)),40,1,1); % true underlying mean
    u3(:,3) = [repmat(std(u3(1:20,1,1)),20,1,1); repmat(std(u3(21:40,1,1)),20,1,1)]; % true underlying SD 
        
    u4(:,1) = input(41:80);
    u4(:,2) = [u4(1,1); repmat(mean(u4(2:12,1,1)),11,1,1); repmat(mean(u4(13:20,1,1)),8,1,1); repmat(mean(u4(21:30,1,1)),10,1,1); repmat(mean(u4(31:40,1,1)),10,1,1)];
    u4(:,3) = [u4(1,1); repmat(std(u4(2:12,1,1)),11,1,1); repmat(std(u4(13:20,1,1)),8,1,1); repmat(std(u4(21:30,1,1)),10,1,1); repmat(std(u4(31:40,1,1)),10,1,1)];
    
    u5(:,1) = input(81:164);
    u5(:,2) = [repmat(mean(u5(1:11,1,1)),11,1,1); repmat(mean(u5(12:21,1,1)),10,1,1); repmat(mean(u5(22:32,1,1)),11,1,1); ...
              repmat(mean(u5(33:43,1,1)),11,1,1); repmat(mean(u5(44:53,1,1)),10,1,1);  repmat(mean(u5(54:63,1,1)), 10,1,1); ...
              repmat(mean(u5(64:73,1,1)),10,1,1); repmat(mean(u5(74:end,1,1)),11,1,1)];
    u5(:,3)  = [repmat(std(u5(1:11,1,1)),11,1,1); repmat(std(u5(12:21,1,1)),10,1,1); repmat(std(u5(22:32,1,1)),11,1,1); ...
              repmat(std(u5(33:43,1,1)),11,1,1); repmat(std(u5(44:53,1,1)),10,1,1);  repmat(std(u5(54:63,1,1)), 10,1,1); ...
              repmat(std(u5(64:73,1,1)),10,1,1); repmat(std(u5(74:end,1,1)),11,1,1)];
     
    %u6(:,1)=input(165:206); % @Toni, can you adapt this for level 6 as in the levels above? 
    %u6(:,2) = [repmat(mean(u6(1:11,1,1)),11,1,1); repmat(mean(u6(11:31,1,1)),21,1,1); repmat(mean(u6(32:42,1,1)),11,1,1)];
    %u6(:,3)  = [repmat(std(u6(1:21,1,1)),21,1,1); repmat(std(u6(22:42,1,1)),21,1,1)];
            
    u6(:,1)=input(165:206); % @Toni, can you adapt this for level 6 as in the levels above? 
    u6(:,2) = [repmat(mean(u6(1:11,1,1)),11,1,1); repmat(mean(u6(12:21,1,1)),10,1,1); repmat(mean(u6(22:32,1,1)),11,1,1);...
               repmat(mean(u6(33:42,1,1)),10,1,1)]; 
    u6(:,3)  = [repmat(std(u6(1:11,1,1)),11,1,1); repmat(std(u6(12:21,1,1)),10,1,1); repmat(std(u6(22:32,1,1)),11,1,1);...        
                repmat(std(u6(33:42,1,1)),10,1,1)]; 
    
    % read out individual inputs
for sub = 1:size(score,2)
    U3(:,:,sub) = [u3, score(1:40,sub)];
    U4(:,:,sub) = [u4, score(41:80, sub)];
    U5(:,:,sub) = [u5, score(81:164, sub)];
    U6(:,:,sub) = [u6, score(165:206, sub)];
end


% responses y by level, point predictions and beam widths
Response = struct;

for level = 3:6
Response(level-2).Position  = {load(fullfile(data_path, strcat('reverseResponse_level', num2str(level), '.txt')))}; 
Response(level-2).Confidence ={load(fullfile(data_path, strcat('reverseResponseBEAM_level', num2str(level), '.txt')))};
Response(level-2).Beamwidth = (110-cell2mat(Response(level-2).Confidence))/.100;
end         
      
% Create string matrix of perception and response model combinations
          jGET_inputs = ["tapas_ehgf_jget_Level3_config_REVERSE",  "tapas_gaussian_obs_spacetask_config";
                         "tapas_ehgf_jget_Level4_config_REVERSE",  "tapas_gaussian_obs_spacetask_config";              
                         "tapas_ehgf_jget_Level5_config_REVERSE",  "tapas_gaussian_obs_spacetask_config";
                         "tapas_ehgf_jget_Level6_config_REVERSE",  "tapas_gaussian_obs_spacetask_config";];
                   
% Plot raw data effects --> in level 3 anti-proportional effects of
% variance manipulation in beamwidth response! 
% % idx = 0;  
% % for level = 3:6
% % responses(:,:,1) = cell2mat(Response(level-2).Position); 
% % responses(:,:,2) = cell2mat(Response(level-2).Beamwidth); 
% % figure; hold on; title('Means and SEM of raw data'); 
% %               
% % for i = 1:2
% % idx = idx +1
% % subplot(4,2,idx); 
% % data = responses(:,:,i)';
% % m    = mean(data,1);
% % SEM = std(data,1)/sqrt(length(data)); 
% % errorbar(1:size(data,2), m, SEM);
% % if i == 1
% %     title(strcat(('Mean prediction Level '), num2str(level)));
% %     else
% %     title(strcat(('Beamwidth Level '), num2str(level)));
% %     end
% % clear data m SEM 
% % end
% % clear responses              
% % end 
% % % explorations: best player no. 2; worst player no.12; average player no. 13
                
% % modelling loop by level and by subjects
idx = 0; 
for level = 4 % goes through levels

            P_model = char(jGET_inputs(level-2,1));
            R_model = char(jGET_inputs(level-2,2));
            
            u_r = eval(strcat('U', num2str(level)));
            responses(:,:,1) = cell2mat(Response(level-2).Position); 
            responses(:,:,2) = Response(level-2).Beamwidth; 
            
         
                for sub = 1:size(responses,2)
                       
                    u = u_r(:,:, sub);
%                     u(:,3) = tapas_sgm(zscore(u_r(:,3,sub)),1); 
                        y(:,1) = responses(1:end,sub,1);
                        y(:,2) = responses(1:end,sub,2); 
                        y(:,2) = tapas_sgm(zscore(y(:,2)),1); 
                        if level == 4
                        u(end,:) =[];
                        y(end,:) = [];
                        end
                        
                        est(sub) = tapas_fitModel(y, u, P_model, R_model);
%                         tapas_hgf_jget_spacetask_plotTraj(est(sub));
                        tapas_ehgf_jget_spacetask_plotTraj(est(sub))
                      % figure 
                       plot(est(sub).optim.yhat(:,2));
                       %hold on; plot(est(sub).y(:,2))
                       close;
                clear y 
                end            
            
                % Save models
                save(strcat("est_withnb_REVERSE_level_", num2str(level), '.mat'),'est','-mat');
                clear u
                %             end
        end
%         

% %% Compare models (uses SPM12)
% % % 
% % % % pick the est files to be compared
% % ff{1} = '\est_mua.mat';
% % ff{2} = '\est_mua_kappaufixed.mat';
% % nm=length(ff);          % number of models
% % 
% % for k = 1:nm
% %     clear est data
% %     eval(['load ' [pwd ff{k}]]);
% %     for sub = 4:length(est)
% %         F(sub,k)    = est(sub).optim.LME;
% %     end 
% %     F = F(4:end,:); % forgot the first 3 subjects while fitting... 
% % end
% % 
% % [alpha(1,:),exp_r(1,:),xp(1,:), pxp(1,:)]	 = spm_BMS(F); % Bayesian model comparison
%  
% % if groups > 1                         % group comparisons
% % [alpha(2,:),exp_r(2,:),xp(2,:), pxp(2,:)] = spm_BMS(F(~isnan(fit(1:n1)),:)); %HC matched to Sz
% % [alpha(3,:),exp_r(3,:),xp(3,:), pxp(3,:)] = spm_BMS(F(~isnan(fit(n2:end)),:)); %Sz
% % end
% 
% % family.infer = 'RFX'; % for model family comparisons 
% % family.partition = bla;
% % family.names     = {'SU', 'DU', 'iDU'}; 
% % [family1,model1] = spm_compare_families(F, family);
%                       
% 
%% correlate simulated responses with real data
load est_withnb_REVERSE_level_6.mat

neg_muahat = 0; 
muahat = NaN(length(est(1).y),77);
bw_real = NaN(length(est(1).y),77);
bw_sim = NaN(length(est(1).y),77);
mean_real = NaN(length(est(1).y),77);
mean_sim = NaN(length(est(1).y),77);

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
%     neg_muahat = neg_muahat+sum(muahat<0);
    kax(sub) = est(sub).p_prc.kax;
    kaa(sub) = est(sub).p_prc.kaa;
    warn(sub) = est(sub).optim.warn;
%     beta1(sub) = est(sub).p_obs.be1;
%     [rho(sub), pval(sub)] = corr(bw_data, bw_sim); 
%     figure
%     scatter(bw_data, bw_sim);
%     tapas_hgf_jget_plotTraj(est(sub))
%     close;
    clear fa bla ta fafa tata 
end

figure; plot(nanmedian(muahat,2));
figure; plot(nanmedian(bw_sim,2)); hold on; plot(nanmedian(bw_real,2)); title("Median across all: real beam width (red) and simulated bw (blue)");  
figure; plot(nanmedian(mean_sim,2)); hold on; plot(nanmedian(mean_real,2)); title("Median across all: real prediction (red) and simulated prediction (blue)");  
% 
% %       
% %        