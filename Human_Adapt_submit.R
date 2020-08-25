w0 = 1
w1 = 20

Delta_Hum = function(y){
  Measur = vector("double", w1-w0+1)+1000000
  distance_thres = abs(y)
  for(w in w0:w1){
    update_index = 1
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



Delta_Hum_Path = function(y, extr_w, update_speed, mix_ratio ){

  up = floor(update_speed)
  Measur = vector("double", w1-w0+1)+1000000
  distance_thres = abs(y)

  for(w in w0:w1){

    update_index = up
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
        update_index = up
        mu_multi_track[i+1] = (1.0-mix_ratio)*mu_multi_track[i+1] +(mix_ratio)*Values[i]     
      }
      
    }
    
    Measur[w-w0+1]=sqrt(sum( (mu_multi_track[1:(number_Trials)]-Values[1:number_Trials] )^2))
    if(w== extr_w){
      Output_track = mu_multi_track[1:(number_Trials)]
    }

  }

  return(Output_track)
}

result_track = Delta_Hum_Path(0.9, 5, 1 , 1)
slow_adapt_track = Delta_Hum_Path(0.9, 5, 5 , 0.5)
higherthres_track = Delta_Hum_Path(2.0, 5, 1 , 1)
higherthres_slow_track = Delta_Hum_Path(2.0, 5, 5 , 0.5)
