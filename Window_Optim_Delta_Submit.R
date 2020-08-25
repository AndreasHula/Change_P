w0 = 1
w1 = 20

Delta_Crit = function(y){ #DSSM
  Measur = vector("double", w1-w0+1)+1000000
  distance_thres = abs(y)
  for(w in w0:w1){
    update_index = 1

    update_index_list=1
    mu_multi_track[1] =mu
    reference_point = 1

    for(i in 1:number_Trials){
      update_index = update_index+1
      mu_multi_track[i+1] = (update_index-1)/update_index*mu_multi_track[i]+1/update_index*Values[i]
      Change_Point = FALSE
      w_test = w
      window_elements = unique( pmax( ( (i-w_test+1):i ) ,reference_point ) )
      mu_est = replicate(length(window_elements) , mu_multi_track[i+1])
      distance = sqrt(sum( (mu_est-Values[window_elements])^2) ) 
      Change_Point = (distance > distance_thres)

      if( Change_Point ){
        reference_point = i
        update_index = 1
        mu_multi_track[i+1] =  Values[i]     
      }
      
    }
    
    Measur[w-w0+1]=sqrt(sum( (mu_multi_track[1:(number_Trials)]-Values[1:number_Trials] )^2))
  }
  Best_error = min(Measur)
  return(Best_error)
}

Op = optim( 0.4 , Delta_Crit, method = "Brent", 
           control = list(maxit = 20000) , 
           lower= 0.00001, upper = 0.9)

Delta_path = function(y, w){

  Measur = vector("double", w1-w0+1)+1000000
  distance_thres = abs(y)
  update_index = 1
    
  update_index_list=1
  mu_multi_track[1] =mu
  reference_point = 1
    
  for(i in 1:number_Trials){
    update_index = update_index+1
    mu_multi_track[i+1] = (update_index-1)/update_index*mu_multi_track[i]+1/update_index*Values[i]
    Change_Point = FALSE
    w_test = w
    window_elements = unique( pmax( ( (i-w_test+1):i ) ,reference_point ) )
    mu_est = replicate(length(window_elements) , mu_multi_track[i+1])
    distance = sqrt(sum( (mu_est-Values[window_elements])^2) ) 
    Change_Point = (distance > distance_thres)
    if( Change_Point ){
      reference_point = i
      update_index = 1
      mu_multi_track[i+1] =  Values[i]     
    }
      
  }
    
  Measur[w-w0+1]=sqrt(sum( (mu_multi_track[1:(number_Trials)]-Values[1:number_Trials] )^2))
  Best_error = min(Measur)
  return(mu_multi_track)
}

DSSM_path = Delta_path(Op$par, 5)

NLL_Crit = function(y){ #FFM
  Measur = vector("double", w1-w0+1)+1000000
  NLL_thres = abs(y)

  for(w in w0:w1){

    update_index = 1
    reference_point = 1
    update_index_list=1
    mu_multi_track[1] =mu
    mu_est = mu

    for(i in 1:number_Trials){
      update_index = update_index+1
      Change_Point = FALSE
        s_est = 0.2          
        distance =  NLL_norm(Values[i], Values[i],0.2) - NLL_norm(Values[i] ,mu_est,s_est)   
        Change_Point = (distance > NLL_thres)
        if( Change_Point ){
          reference_point = i
          update_index = 1
          mu_multi_track[i+1] =  Values[i]   
          mu_est = Values[i]
        }else{
          window_elements = unique( pmax( ( (i-w+1):i ) ,reference_point ) ) 
          mu_est = mean(Values[window_elements])
          mu_multi_track[i+1] = mu_est
        }
      
      }
    
    Measur[w-w0+1]=sqrt(sum( (mu_multi_track[1:(number_Trials)]-Values[1:number_Trials] )^2))
  }
  Best_error = min(Measur)
  return(Best_error)
}

NLL_Path = function(y,w){
  Measur = vector("double", w1-w0+1)+1000000
  NLL_thres = abs(y)
  update_index = 1
  reference_point = 1
  update_index_list=1
  mu_multi_track[1] =mu
  mu_est = mu
  for(i in 1:number_Trials){
    update_index = update_index+1
    Change_Point = FALSE
    s_est = 0.2
    distance =  NLL_norm(Values[i], Values[i],0.2) - NLL_norm(Values[i] ,mu_est,s_est)   
    Change_Point = (distance > NLL_thres)
    if( Change_Point ){
      reference_point = i
      update_index = 1
      mu_multi_track[i+1] =  Values[i]   
      mu_est = Values[i]
    }else{
      window_elements = unique( pmax( ( (i-w+1):i ) ,reference_point ) ) 
      mu_est = mean(Values[window_elements])
      mu_multi_track[i+1] = mu_est
    }
      
  }
  Measur[w-w0+1]=sqrt(sum( (mu_multi_track[1:(number_Trials)]-Values[1:number_Trials] )^2))
  return(mu_multi_track)
}

Opo = optim( 2 , NLL_Crit, method = "Brent", 
            control = list(maxit = 20000) , 
            lower= 0.00001, upper = 15)

FFM = NLL_Path(Opo$par,16)

RW_tracker = function(y){
  adapt_rate = abs(y)
  mu_multi_track[1] =mu
  
  for(i in 1:number_Trials){
    mu_multi_track[i+1] = mu_multi_track[i]+adapt_rate*(Values[i]-mu_multi_track[i])
  }
  
  Measur=sqrt(sum( (mu_multi_track[1:(number_Trials)]-Values[1:number_Trials] )^2))
  
  return(Measur)
}
Op_RW = optim( 0.5 , RW_tracker, method = "Brent", 
             control = list(maxit = 20000) , 
             lower= 0.00001, upper = 1)

RW_path = function(y){
  adapt_rate = abs(y)
  mu_multi_track[1] =mu
  
  for(i in 1:number_Trials){
    mu_multi_track[i+1] = mu_multi_track[i]+adapt_rate*(Values[i]-mu_multi_track[i])
  }
  return(mu_multi_track)
}

RW_Series=RW_path(Op_RW$par)