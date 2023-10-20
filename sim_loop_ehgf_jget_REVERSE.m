%%% Simulation script: simulates trajectories and behavior for the space
%%% task using the eHGF JGET model (perceptual model) together with a
%%% linear regression script (response model) 
%%% by Teresa Katthagen, October 2022

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
input = load('reverseinput.txt'); % same for all subjects, only load once
score = load('trialscoreL3_6_reverse.txt'); % different for subjects


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


% % responses y by level, point predictions and beam widths, this part is
% % not needed for simulation (we're simulating the responses and do not feed them into the model)
 
% Response = struct;
% 
% for level = 3:6
% Response(level-2).Position  = {load(fullfile(data_path, strcat('forwardresponse_level', num2str(level), '.txt')))}; 
% Response(level-2).Confidence ={load(fullfile(data_path, strcat('forwardresponseBEAM_level', num2str(level), '.txt')))};
% Response(level-2).Beamwidth = (110-cell2mat(Response(level-2).Confidence))./100;
% end         
%       
% % Create string matrix of perception and response model combinations
%           jGET_inputs = ["tapas_ehgf_jget_Level3_config",  "tapas_gaussian_obs_spacetask_config";
%                          "tapas_ehgf_jget_Level4_config",  "tapas_gaussian_obs_spacetask_config";              
%                          "tapas_ehgf_jget_Level5_config",  "tapas_gaussian_obs_spacetask_config";
%                          "tapas_ehgf_jget_Level6_config",  "tapas_gaussian_obs_spacetask_config";];
                   
%% the actual simulation script starts here, the part above is the same as in the fitting loops 
nsim = 1; % number of simulations (can be much higher but then increases the output)

%%% set the range for free parameters: only give one integer if you do not
%%% want the model to go trough all solutions / combinations
omega_1_x = 5;  
omega_2_x = -3;
omega_1_a = 2;
omega_2_a = 0;
omega_u   = 0;

% build the inputs for skipping the config files  
r = struct;
ign = [];
irr = [];  


% Config structure
r = struct;
prc_fun = 'tapas_ehgf_jget_sim';
% Number of levels (minimum: 2)
n_levels = 2;
idx = 0; 
ident = [];


for level = 4 % goes through levels when set to 3:6
    if level == 3 % needs to be adapted
    p_prc.mux_0 = [46, 1];
    p_prc.sax_0 = [0.1, 0];
    p_prc.mua_0 = [0.1, -1];
    p_prc.saa_0 = [1, 1];
    p_prc.kau = 1;
    p_prc.kax = 0;
    p_prc.kaa = 1;
    p_prc.omu = 2;
    p_prc.omx = [3,   1];
    p_prc.oma = [1,    1];
 
    
    elseif level == 4
    p_prc.mux_0 = [93, 1];
    p_prc.sax_0 = [0.1, 0.01];
    p_prc.mua_0 = [1, -1];
    p_prc.saa_0 = [0.1,0.01];
    p_prc.kau = 1;
    p_prc.kax = 1;
    p_prc.kaa = 0;
    p_prc.omu = 3;
    p_prc.omx = [ 3, -3];
    p_prc.oma = [-3,  0];
   
    elseif level == 5
    p_prc.mux_0 = [21, 1];
    p_prc.sax_0 = [0.1, 0.01];
    p_prc.mua_0 = [5, -1];
    p_prc.saa_0 = [0.1, 0.01];
    p_prc.kau = 1;
    p_prc.kax = 1.5;
    p_prc.kaa = 1;
    p_prc.omu = 0;
    p_prc.omx = [3,   1];
    p_prc.oma = [-1,   -8];
       
    
    elseif level == 6 % needs to be adapted!
    p_prc.mux_0 = [21, 1];
    p_prc.sax_0 = [0.1, 0.01];
    p_prc.mua_0 = [5, -1];
    p_prc.saa_0 = [0.1, 0.01];
    p_prc.kau = 1;
    p_prc.kax = 1.5;
    p_prc.kaa = 1;
    p_prc.omu = 0;
    p_prc.omx = [3,   1];
    p_prc.oma = [-1,   -8];
    end
    
for a = 1:length(omega_1_x)
    p_prc.omx(1) = omega_1_x(a);
    
for b = 1:length(omega_2_x)
    p_prc.omx(2)=omega_2_x(b);
    
for c = 1:length(omega_1_a)
    p_prc.oma(1) = omega_1_a(c); 
    
for d = 1:length(omega_2_a)
    p_prc.oma(2) = omega_2_a(d); 
for e = 1:length(omega_u)
    p_prc.omu =omega_u(e); 
      
    p_prc.p = [      
    p_prc.mux_0,...
    p_prc.sax_0,...
    p_prc.mua_0,...
    p_prc.saa_0,...
    p_prc.kau,...
    p_prc.kax,...
    p_prc.kaa,...
    p_prc.omu,...
    p_prc.omx,...
    p_prc.oma,...
         ];
     
  % parameters for the response model 
  p_obs.be1 = 1; 
  p_obs.zem = 10;
  p_obs.zes = 0.01;
  p_obs.p = [p_obs.be1, p_obs.zem, p_obs.zes];
  u_r = eval(strcat('U', num2str(level)));
  
  if level == 4
      u_r(end,:,:) =[]; % delete the last trial in level 4, where the input suddenly changes
  end 
  
for i=1:nsim
       r(i).ign = ign;
       r(i).irr = irr;
       r(i).u = u_r(:,:, 1);
       r(i).p_prc = p_prc; 
       r(i).p_obs = p_obs; 
       r(i).y = NaN(length(r.u),2);
       r(i).c_prc.n_levels = n_levels;
       r(i).p_obs = p_obs; 

       
    % this line simulates data
    [traj, infStates, r(i)] = feval(prc_fun,r(i),r(i).p_prc.p);     
    r(i).traj = traj;
    tapas_ehgf_jget_spacetask_plotTraj(r(i))
    figure
    plot(r(i).y(:,2)); hold on; title("simulated beam width response");
    close all % I always set a breakpoint here to directly look at the trajectories and responses

    clear traj infStates 
                       
   
end
end
end
end
end
end
end 
