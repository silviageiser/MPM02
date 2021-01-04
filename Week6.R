

rm(list = ls(all = TRUE)) # clean the memory


#install.packages("devtools")

#devtools::install_github("PredictiveEcology/NetLogoR")

#worked now




library(NetLogoR)
library(stringr)
library(ggplot2)
library(minpack.lm)


for (i in 40:100){
  for (j in 1:20) {
    
    
    ### 1. DEFINE THE SPACE AND AGENTS ###
    
    # simulations parameters
    simtime<-100 # duration time of the simulation
    number_agents<-i
    number_pubs<-j # number of pubs (or homes. whatever) in the space named "world"
    gridSize_x<-10 # number of patches in the grid where moving agents move around
    gridSize_y<-10
    displacement_normal<-0.1 # speed of moving agents 
    displacement_pub<-0.01 # if in the pub, agents move slower and spend more time there
    plot_data_out<-numeric() # initialize variable to store data to be plotted later on
    
    # world set up, this is about the static patches
    w1 <- createWorld(minPxcor = 0, maxPxcor = gridSize_x-1, minPycor = 0, maxPycor = gridSize_y-1) # world defined by patches with coordinates Pxcor & Pycor
    x_pub<-randomPxcor(w1,number_pubs) # random pub location on the world grid
    y_pub<-randomPycor(w1,number_pubs)
    w1 <- NLset(world = w1, agents = patches(w1), val = 0) # initialize all the patches to their internal state value = 0...
    w1 <- NLset(world = w1, agents = patch(w1, x_pub, y_pub), val = 1) # ...except for the pubs with a value set to 1
    
    # agents set up, this is about the moving objects (traditionally named turtles)
    t1 <- createTurtles(n = number_agents, coords = randomXYcor(w1, n = number_agents), breed="S", color="black") # all agents are set to the state (breed) S=susceptible, colored black
    t1 <- NLset(turtles = t1, agents = turtle(t1, who = 0), var = "breed", val = "I") # agent 0 is set to I=infected (patient 0 that contaminates the others) 
    t1 <- NLset(turtles = t1, agents = turtle(t1, who = 0), var = "color", val = "red") # ... and coloured red 
    t1 <- turtlesOwn(turtles = t1, tVar = "displacement", tVal = displacement_normal) # all initially move with standard speed (normal displacement)
    
    plot(w1, axes = 0, legend = FALSE, par(bty = 'n')) # initialize graphics by displaying world patches
    points(t1, col = of(agents = t1, var = "color"), pch = 20) # initialize graphics by displaying agents
    
    
    
    
    
    ### 2. RUN THE SIMULATION TIME LOOP ###
    
    for (time in 1:simtime) { # start the simulation time loop
      
      t1 <- fd(turtles = t1, dist=t1$displacement, world = w1, torus = TRUE, out = FALSE) # each timestep move each agent forward with the fd() function, by a distance 
      t1 <- right(turtles = t1, angle = sample(-20:20, 1, replace = F)) # each timestep agents can randomly turn 20 deg right of 20 left (-20)
      
      plot(w1, axes = 0, legend = FALSE, par(bty = 'n')) # update graphics
      points(t1, col = of(agents = t1, var = "color"), pch = 20) # update graphics
      
      meet<-turtlesOn(world = w1, turtles = t1, agents = t1[of(agents = t1, var = "breed")=="I"]) # contact if multiple agents are on the same patch with function turtlesOn()
      t1 <- NLset(turtles = t1, agents = meet, var = "breed", val = "I") # get the state of the infected agent
      t1 <- NLset(turtles = t1, agents = meet, var = "color", val = "red") # and change its colour
      
      # agents that enter a pub spend more time there (have a lower displacement value)
      pub <- turtlesOn(world = w1, turtles = t1, agents = patch(w1, x_pub, y_pub)) # check if agent is on a pub patch with function turtlesOn()
      # # if enters the pub
      t1 <- NLset(turtles = t1, agents = turtle(t1, who = pub$who), var = "displacement", val = displacement_pub)
      # # if exits the pub
      t1 <- NLset(turtles = t1, agents = turtle(t1, who = t1[-(pub$who+1)]$who), var = "displacement", val = displacement_normal)
      
      
      Sys.sleep(0.1) # give some time to the computer to update all the thing graphically
      
      # store time-course data for plotting in the end
      contaminated_counter<-sum(str_count(t1$color, "red"))
      tmp_data<-c(time,contaminated_counter)
      plot_data_out<-rbind(plot_data_out, tmp_data) # store in a matrix
      
    }
    
    
    
    ### 3. PLOTTING AND FITTING SIMULATED DATA ###
    
    # perform non-linear curve fitting of the data 
    df<-as.data.frame(plot_data_out)
    names(df)<-c("time","contaminated_counter")
    x  <- df$time
    y  <- df$contaminated_counter
    
    # give initial guesses and fit with 4-parameters logistic equation (fits well S-shaped generic curves)
    model <- nlsLM(y ~ d + (a-d) / (1 + (x/c)^b) ,start = list(a = 3, b = 4, c = 600, d = 1000)) # initial guesses are set (arbitrarily) to 3,4,600,1000
    
    # make a line with the fitting model that goes through the data
    fit_x <- data.frame(x = seq(min(x),max(x),len = 100))
    fit_y <- predict(model, newdata = fit_x)
    fit_df <- as.data.frame(cbind(fit_x,fit_y))
    names(fit_df)<-c("x","y")
    fitted_function <- data.frame(x = seq(min(x),max(x),len = 100))
    lines(fitted_function$x,predict(model,fitted_function = fitted_function))
    
    # store summary statistics in a vector to be appended after each iteration to the output file
    # # put in the filename all the parameters used to set the simulation run
    simulation_run_name <- paste0("sim_",number_agents,"_",number_pubs)
    varied_params <- c(number_agents,number_pubs)
    summary_stat <- c( simulation_run_name, varied_params, as.vector(model$m$getPars()) )
    # save summary statistics of all the performed simulations in file with:
    # # Simulation ID
    # # parameters used for that simulation
    # # outcome of the curve (described by the fitting parameters)
    write.table(as.data.frame(t(summary_stat)), "./summary_stat.csv", sep = ",", col.names = FALSE, row.names=FALSE, append = TRUE) # append to pile up the different runs in a single file
    
    # graphical representation of simulation data and fitting
    #ggplot(data = df, mapping = aes(y = y, x = x)) +
    #  ggtitle(paste0("Simulation ID: \t", simulation_run_name,
    #                 "\nSimulation Params: agents=",number_agents,"; pubs=", number_pubs,
    #                 "\nCurve Fit Params: \t",toString(round(model$m$getPars(),2)))) +
    #  xlab("time") +
    #  ylab("Agents contaminated") +
    #  geom_point(data=df, aes(y=y, x=x), colour="black") +
    #  geom_line(data = fit_df, colour="red")
    
    
    
    }}