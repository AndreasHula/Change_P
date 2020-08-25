
set.seed(15)
#--------------------- set_up --------------------------
number_states = 15
number_Trials = 10000
reference_mean = 20
average_interval = 50
#--------------------- calc ---------------------
reevalutation_step = 1000
distribution_means = rnorm( number_states , 
                            reference_mean+rnorm(number_states ,0, 2), 
                            vector("double", number_states)+0.2)
distribution_variances = vector("double", number_states)+0.2
pois_draws = round(2*number_Trials/round(average_interval/2,0),0)
pois_parameters = rpois(pois_draws,average_interval)+1
interval_lengths = rpois(round(pois_draws), pois_parameters)
while(sum(interval_lengths)<(number_Trials+2)){
  pois_parameters = rpois(pois_draws,average_interval)+1
  interval_lengths = rpois(round(pois_draws), pois_parameters)
}
discrete_grid = 1/number_states*(0:number_states)
generators = runif(length(interval_lengths))
state_chain = vector("double", length(interval_lengths))
for(i in 1:length(interval_lengths)){
  state_chain[i] =  sum(as.numeric(generators[i]>discrete_grid))
}

Observations = {}
#Generate Data
for(i in 1:length(interval_lengths)){
  Observations = c(Observations, rnorm(interval_lengths[i],
                                       distribution_means[state_chain[i] ],
                                       distribution_variances[state_chain[i] ] ) )
}


NLL_norm = function(x,m,s){
  fun_val = -1/2*log(2*pi*s^2)-(x-m)^2/(2*s^2)
  fun_val = fun_val
    return(fun_val)
}

w0 =1
w1 = 30
mu = 20
Values = Observations
mu_multi_track = vector("double", length(Observations)+1)
mu_single_track = vector("double", number_Trials+1)
mu_single_track[1] =mu
update_index = 1
for(i in 1:number_Trials){
  update_index = update_index+1
  mu_single_track[i+1] = (update_index-1)/update_index*mu_single_track[i]+1/update_index*Values[i]
}
Basic_error = sqrt(sum( (mu_single_track[1:(length(mu_single_track)-1)]-Values[1:number_Trials] )^2))




Reference_error={}
Reference_track = vector("double", number_Trials+1)
Reference_track[1]=mu
switches = c(1, cumsum(interval_lengths)+1)
update_index = 1
for(i in 1:number_Trials ){
  if(i %in% switches){
    update_index =1
    Reference_track[i+1] = Values[i]    
    
  }else{
    update_index = update_index+1
    Reference_track[i+1] = (update_index-1)/update_index*Reference_track[i]+1/update_index*Values[i]
  }
}
Reference_error = sqrt(sum( (Reference_track[1:(length(Reference_track)-1)]-Values[1:number_Trials] )^2))


