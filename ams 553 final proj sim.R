#Simluation program
#Inventory system with a (s,S) ordering policy
#Right now, assuming S = 100. Little s is variable

#Outer initializations
all_means <- c()           #Empty data frame to store average cost
all_var <- c()             #Empty vector to store variances of each (outer) loop
min_little_s = 10          #Minimum possible restocking threshold
max_little_s = 90          #Maximum possible restocking threshold
max_runs = 100             #Max number of times for inner loop to run (for every value of little_s)
loop_runs = 0

#Loop that runs for all possible integer values of little s
#for (little_s in min_little_s:max_little_s) {
  
  avg_cost <- c()
  
  #Run loop a certain number of times for each value of little s
  for (run in 1:max_runs) {
    
    #Functions
    #Calculate order cost
    calc_order_cost <- function(z) {
      return(K + c*z)
    }
    
    #Calculate cost of unsold inventory
    calc_Inv_cost <- function(x) {
      return(h*x)
    }
    
    #Calculate penalty cost for items in backlog
    calc_penalty_cost <- function(y) {
      return (p*abs(y))
    }
    
    
    #Initializaitons
    little_s=90
    big_S = 100           #Max inventory level
    Cur_inv_level = big_S #Current inventory level - inventory system starts at full
    max_time = 200        #Max time simulation can run
    t = 0                 #Current time
    i_du = 1              #Min value for discrete uniform (order quantity)
    j_du = 50             #Max value for discrete uniform (order quantity)
    order_time = -1       #How long it takes for order to arrive (-1 means no order scheduled)
    order_status = 0      #0 = no order placed, 1 = order placed
    order_num = 0         #Keep track of number of orders
    order_cost <- c()     #vector to keep order costs
    order_wait = 5        #Wait time for order to arrive
    Inv_cost <- c()       #Keep track of inventory costs
    Penalty_cost <- c()   #Keep track of penalty costs
    Inv_level <- c()      #Keep track of inventory level across times
    K = 10                #Fixed cost per order
    c = 10                #Cost per unit per order
    h = .3                #Cost per unit for holding it in inventory
    p = 20                #Cost per unit in backlog
    num_cust = 0          #Keeps track of number of customers
    cust_incoming = 0     #Decide if need to generate (0 = no, 1 = yes)
    backlog_level <- c()  #Keep track of backlog levels (0 if none)
    alpha = .1            #Intensity for mean-reverting process
    mu = 100              #Mean of the mean-reverting process

    #Simulate over the given time period (max_time)
    for (t in 0:max_time) {
      
      #Assume next customer arrives at next integer time step
      if(cust_incoming == 0) {
        next_arrival = t + 1
        cust_incoming = 1
      }
      
      #Update inventory and order status if restocking order arrives
      if (t == order_time) {
        Cur_inv_level = Cur_inv_level + restock
        order_status = 0
        order_time = -1
      }
      
      #Process customer's order
      if (t == next_arrival) {
        #Calculate how many units he orders (discrete uniform - using inverse transform)
        cust_order = i_du + floor((j_du - i_du + 1) * runif(1,0,1))
        
        #Update inventory level
        Cur_inv_level = Cur_inv_level - cust_order
        
        #Order processed - no customers expected
        cust_incoming = 0
        num_cust = num_cust + 1
      }
      
      #Restock inventory level if below threshold AND no order already placed
      #Take into account what the cost of the next step could be -- ignore order placement if costs less
      if (Cur_inv_level < little_s && order_status==0) {
        
        #############new###########
        cnew <- alpha*(mu - c) + rnorm(100,0,1)                       #Calculate the new ordering price
        foo_cust <- c(rep(1,100))                                     #Time customers arrive - 1 per timestep
        foo_ord <- i_du + floor((j_du - i_du + 1) * runif(1,0,1))     #Amount that customer orders
        foo_inv <- Cur_inv_level - foo_ord                            #Updated inventory level
        foo_backlog <- c(rep(0,100))                                  #Backlog level
        foo_backlog[foo_inv<0] = abs(foo_inv)                         #If inventory level is negative, update backlog level
        foo_backlog_mean = mean(foo_backlog)                          #calculate mean of backlog
        foo_inv_mean = mean(foo_inv)                                  #Calculate mean of inventory
        foo_cust[foo_cust>1] = 0         #Do we need this one anymore?
        foo_ord_mean = mean(foo_cust*foo_ord)                         #Calculate mean of the orders
        
        #This should be covered by the above steps - keeping just to see what was there. Take out once confident
        #for (i in 1:100) {
        # cnew[i] = alpha*(mu - c) + rnorm(1,0,1)
        # foo_cust[i] = 1
        # foo_ord[i] = i_du + floor((j_du - i_du + 1) * runif(1,0,1))
        # foo_inv[i] = Cur_inv_level - foo_ord[i]
        # if (foo_inv[i]<0) {
        #   foo_backlog[i] = abs(foo_inv[i])
        # }
        #}
        
        ################################################################################################
        #                                                                                              #
        # Should we be multiplying mean(cnew) and c by the amount of inventory that we are purchasing? #
        #                                                                                              #
        ################################################################################################
        
        #Determine whether or not to place order
        if (calc_Inv_cost(foo_inv_mean) + mean(cnew) + p*foo_backlog_mean > c + calc_Inv_cost(Cur_inv_level)) {
          order_num = order_num + 1
          order_status = 1
          order_time = t+order_wait
          restock = min(big_S - Cur_inv_level,100)
          order_cost[order_num] = calc_order_cost(restock)
        }
      }
      
      #Update Inventory level and cost vectors
      Inv_level[t+1] = Cur_inv_level
      if(Cur_inv_level<0){
        backlog_level[t+1] = abs(Cur_inv_level)
      }
      else {
        backlog_level[t+1] = 0
      }
      Inv_cost[t+1] = calc_Inv_cost(Cur_inv_level)
      
      #Calculate penalty cost for items in backlog (if any)
      if (Cur_inv_level < 0) {
        Penalty_cost[t+1] = calc_penalty_cost(Cur_inv_level)
      }
      else
        Penalty_cost[t+1] = 0
      
      c = alpha*(mu - c) + rnorm(1,0,1)
    }
    
    #Iventory level over time
    for (i in 1:length(Inv_level)){
      if(Inv_level[i] >= 0) {
        backlog_level[i] = 0
      }
      else {
        backlog_level[i] = abs(Inv_level[i])
      } 
    }
    
    #Calculate average cost per 200 time units
    avg_cost[run] = mean(order_cost) + h*(1/max_time)*sum(Inv_level[Inv_level>0]) + p*(1/max_time)*sum(backlog_level)
  
    
    }
  
  #Calcumate the mean of the runs for each level of little_s
  all_means[loop_runs+1] = mean(avg_cost)
  
  #Calculate the variance of the runs for each level of little_s
  all_var[loop_runs+1] = var(avg_cost)
  
  loop_runs = loop_runs + 1
#}

###############################################################################
#                                                                             #
# Is everything below this irrelevant now that we're not looking at little_s? #
#                                                                             #
###############################################################################
  
  
#Combine all values of s, average costs, and variances in a single data frame
df = data.frame(min_little_s:max_little_s, all_means, all_var)
colnames(df) <- c("s", "Average cost", "Variance")

#Plot the estimated average cost at each level of little_s
plot(df$s, df$`Average cost`, xlab = "Restock Threshold", ylab = "Average Cost", main = "Average Cost vs. Threshold")

#Write data to .csv file
write.csv(df, "AMS 553 Data.csv")

#Print out mean average cost of the simulations
mean(avg_cost)