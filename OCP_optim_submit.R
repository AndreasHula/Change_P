#OCP_function_alt = function(y){
#  mu_o = 20
#  para_1 = abs(y[2])
#  para_2 = abs(y[3])
#  para_3 = abs(y[4])
#  OC=onlineCPD(Values[1:number_Trials], hazard_func = function(x, lambda) {     const_hazard(x, lambda = abs(y[1]))
#  }, init_params = list(list(m = mu_o, k = para_1, a 
#                             = para_2, b = para_3)) )
#
#  OC_single_track = vector("double", number_Trials+1)
#  update_index = 1
#  OC_single_track[1] =mu
#  for(i in 1:number_Trials){
#
#    if(i %in% OC$changepoint_lists$threshcps[[1]]){
#      update_index =1
#      OC_single_track[i+1] = Values[i]
#      
#    }else{
#      update_index = update_index+1
#      OC_single_track[i+1] = (update_index-1)/update_index*OC_single_track[i]+1/update_index*Values[i]
#      
#    }
#    
#    
#  }
#  OC_error = sqrt(sum( (OC_single_track[1:(length(OC_single_track)-1)]-Values[1:number_Trials] )^2))
#  
#  return(OC_error)
#}
#
#starters = c(50, 0.01,0.01, 0.0001)
#
#OCP_Run_alt = optim( starters , OCP_function_alt,  
#                       control = list(maxit = 20000) )

OCP_Run_alt = readRDS(file="optimised_ocp_alt.rds")

OC_track=mu
ptm <- proc.time()
OC=onlineCPD(Values[1:number_Trials], hazard_func = function(x, lambda) {     const_hazard(x, lambda = OCP_Run_alt$par[1])
}, init_params = list(list(m = mu, k = OCP_Run_alt$par[2], a 
                           = OCP_Run_alt$par[3], b = OCP_Run_alt$par[4])) )

OC_single_track = vector("double", number_Trials+1)
update_index = 1
OC_single_track[1] =mu
for(i in 1:number_Trials){
  ##OC_single_track[i] = currmu[[i]]
  if(i %in% OC$changepoint_lists$threshcps[[1]]){
    update_index =1
    OC_single_track[i+1] = Values[i]
    
  }else{
    update_index = update_index+1
    OC_single_track[i+1] = (update_index-1)/update_index*OC_single_track[i]+1/update_index*Values[i]
    
  }
  
  
}
OC_error = sqrt(sum( (OC_single_track[1:(length(OC_single_track)-1)]-Values[1:number_Trials] )^2))
