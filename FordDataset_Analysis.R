# T3_1:
  library("caret")
  # Read data:
  DF <- read.csv("Pre_Processed_DATA.csv",TRUE,",")
  
  # Split the Data into train and test:
  idx <- sample(c(TRUE, FALSE), nrow(DF), replace=TRUE, prob=c(0.8,0.2))
  train <- DF[idx, ]
  test <- DF[!idx, ]
  

  startTime1 <- Sys.time()
  
  # Trainning the model (linear regression):
  model_lm <- train(price~.,data=train,method='lm')
  model_lm
  summary(model_lm)
  
  endTime1 <- Sys.time()
  
  time_elapsed1 = endTime1 - startTime1

  
  # Testing the model:
  Output_lm <- predict(model_lm,test)
  
  # Plotting:
  plot(Output_lm, test$price,
       col = c('blue','red'),
       xlab = "Predicted Values",
       ylab = "Observed Values")
  abline(a = 0, b = 1, lwd=2,
         col = "green")
  
  
  #Evaluation metrics:
  #install.packages("Metrics")
  library(Metrics)
  # Calculate MAE:
  mae(Output_lm,test[,2])
  # Calculate MSE:
  mse(Output_lm,test[,2])
  # Calculate RMSE:
  rmse(Output_lm,test[,2])
  # Calculate R^2:
  cor(Output_lm,test[,2])^2
  



#T3_2:
  # Specifying for Cross Validation with K= 5
  train_control <- trainControl(
    method = 'cv', # k-fold cross validation
    number = 5 # number of folds
  )
  
  startTime2 <- Sys.time()
  # Trainning the model (Cross Validation, K=5):
  model_cv <- train(price~.,data=DF,method='lm',trControl = 
                      train_control)
  
  endTime2 <- Sys.time()
  
  time_elapsed2 = endTime2 - startTime2
  
  
  model_cv
  summary(model_cv)
  
  # Prediction:
  predict_cv <- predict(model_cv,test)
  summary(predict_cv)
  
  #Evaluation metrics:
  #install.packages("Metrics")
  library(Metrics)
  # Calculate MAE:
  mae(predict_cv,test[,2])
  # Calculate MSE:
  mse(predict_cv,test[,2])
  # Calculate RMSE:
  rmse(predict_cv,test[,2])
  # Calculate R^2:
  cor(predict_cv,test[,2])^2
  
  # Plotting:
  plot(predict_cv, test$price,
       col = c('blue','red'),
       xlab = "Predicted Car prices",
       ylab = "Observed Car prices")
  abline(a = 0, b = 1, lwd=2,
         col = "green")
  
  
#T3_3:
  # Run the model with less features:
    # 1: Remove the column mileage:
        DF1 <- subset (DF, select = -mileage)

        startTime3 <- Sys.time()
        # Trainning the model (without the column "mileage", CV, K=5):
        model_cv_1 <- train(price~.,data=DF1,method='lm',trControl = 
                              train_control)
        
        endTime3 <- Sys.time()
        
        time_elapsed3 = endTime3 - startTime3
        model_cv_1
        summary(model_cv_1)
        
        
        # Prediction:
        predict_cv_1 <- predict(model_cv_1,test)
        summary(predict_cv_1)
        
        #Evaluation metrics:
          #install.packages("Metrics")
          library(Metrics)
          # Calculate MAE:
          mae(predict_cv_1,test[,2])
          # Calculate MSE:
          mse(predict_cv_1,test[,2])
          # Calculate RMSE:
          rmse(predict_cv_1,test[,2])
          # Calculate R^2:
          cor(predict_cv_1,test[,2])^2
        
        
        
        # Plotting:
        plot(predict_cv_1, test$price,
             col = c('blue','red'),
             xlab = "Predicted Values",
             ylab = "Observed Values")
        abline(a = 0, b = 1, lwd=2,
               col = "green")
    
    
    #2: Remove the column mpg:
        DF2 <- subset (DF, select = -mpg)
        
        

        startTime4 <- Sys.time()
        # Training the model (without the column "mpg", CV, K=5):
        model_cv_2 <- train(price~.,data=DF2,method='lm',trControl = 
                              train_control)
        
        endTime4 <- Sys.time()
        
        time_elapsed4 = endTime4 - startTime4

        model_cv_2
        summary(model_cv_2)
        
        predict_cv_2 <- predict(model_cv_2,test)
        
        #Evaluation metrics:
          #install.packages("Metrics")
          library(Metrics)
          # Calculate MAE:
          mae(predict_cv_2,test[,2])
          # Calculate MSE:
          mse(predict_cv_2,test[,2])
          # Calculate RMSE:
          rmse(predict_cv_2,test[,2])
          # Calculate R^2:
          cor(predict_cv_2,test[,2])^2
        
        # Plotting:
        plot(predict_cv_2, test$price,
             col = c('blue','red'),
             xlab = "Predicted Values",
             ylab = "Observed Values")
        abline(a = 0, b = 1, lwd=2,
               col = "green")
    
    
    #3: change the folds into 10 instead of 5:
        train_control_1 <- trainControl(
          method = 'cv', # k-fold cross validation
          number = 10 # number of folds
        )
        
        

        startTime5 <- Sys.time()
        # Training the model (Cross Validation, K=10):
        model_cv_3 <- train(price~.,data=DF,method='lm',trControl = 
                              train_control_1)
        
        endTime5 <- Sys.time()
        
        time_elapsed5 = endTime5 - startTime5
        
        model_cv_3
        summary(model_cv_3)
        
        
        
        # prediction:
        predict_cv_3 <- predict(model_cv_3,test)
        
        #Evaluation metrics predict_cv_3:
          #install.packages("Metrics")
          library(Metrics)
          # Calculate MAE:
          mae(predict_cv_3,test[,2])
          # Calculate MSE:
          mse(predict_cv_3,test[,2])
          # Calculate RMSE:
          rmse(predict_cv_3,test[,2])
          # Calculate R^2:
          cor(predict_cv_3,test[,2])^2
        
        # Plotting predict_cv_3:
        plot(predict_cv_3, test$price,
             col = c('blue','red'),
             xlab = "Predicted Values",
             ylab = "Observed Values")
        abline(a = 0, b = 1, lwd=2,
               col = "green")
    
    
    # 4: change 'cv' to 'repeated cv':
        train_control_2 <- trainControl(
          method = 'repeatedcv', # k-fold cross validation
          number = 5 # number of folds
        )
        
        
        startTime6 <- Sys.time()
        # Training the model (Repeated Cross Validation, K=5):
        model_repeatedcv_4 <- train(price~.,data=DF,method='lm',trControl = 
                                      train_control_2,tuneLength = 5)
        
        endTime6 <- Sys.time()
        
        time_elapsed6 = endTime6 - startTime6
      
        model_repeatedcv_4
        summary(model_repeatedcv_4)
        
       
        # prediction:
        predict_repeatedcv_4  <- predict(model_repeatedcv_4 ,test)
      
        #Evaluation metrics predict_repeatedcv_4:
        #install.packages("Metrics")
        library(Metrics)
        # Calculate MAE:
        mae(predict_repeatedcv_4,test[,2])
        # Calculate MSE:
        mse(predict_repeatedcv_4,test[,2])
        # Calculate RMSE:
        rmse(predict_repeatedcv_4,test[,2])
        # Calculate R^2:
        cor(predict_repeatedcv_4,test[,2])^2
        
        
        
        
        ## Plotting predict_repeatedcv_4:
        plot(predict_repeatedcv_4, test$price,
             col = c('blue','red'),
             xlab = "Predicted Values",
             ylab = "Observed Values")
        abline(a = 0, b = 1, lwd=2,
               col = "green")
      
    

# T3-4:
    # Create Vectors for all indicators:
        MAE <- c( mae(Output_lm,test[,2]),
                   mae(predict_cv,test[,2]),
                   mae(predict_cv_1,test[,2]),
                   mae(predict_cv_2,test[,2]),
                   mae(predict_cv_3,test[,2]),
                   mae(predict_repeatedcv_4,test[,2]))
                   
        MSE <- c( mse(Output_lm,test[,2]),
                   mse(predict_cv,test[,2]),
                   mse(predict_cv_1,test[,2]),
                   mse(predict_cv_2,test[,2]),
                   mse(predict_cv_3,test[,2]),
                   mse(predict_repeatedcv_4,test[,2]))

        RMSE <- c(rmse(Output_lm,test[,2]),
                  rmse(predict_cv,test[,2]),
                  rmse(predict_cv_1,test[,2]),
                  rmse(predict_cv_2,test[,2]),
                  rmse(predict_cv_3,test[,2]),
                  rmse(predict_repeatedcv_4,test[,2]))

        R_2 <- c(cor(Output_lm,test[,2])^2,
                 cor(predict_cv,test[,2])^2,
                 cor(predict_cv_1,test[,2])^2,
                 cor(predict_cv_2,test[,2])^2,
                 cor(predict_cv_3,test[,2])^2,
                 cor(predict_repeatedcv_4,test[,2])^2)
        
        Labels <- c('Model 1: Default lm model', 'Model 2: Cross Validation k=5', 'Model 3: The feature: mileage removed','Model 4: The feature: mpg removed', 'Model 5: Cross Validation K=10','Model 6: Repeated cv K=5')

        
        # Plot MAE, MSE, RMSE and R-Squared for all models for comparison:
        library(ggplot2)
        library(stringr)
        library(ggplot2)
        library(tidyr)
        ##install.packages("cowplot")
        library("cowplot")
        library(scales)
        
        DF_MAE <- data.frame(MAE, Labels)
        # Plot MAE for all the models:
        MAE_Plot <- ggplot(DF_MAE, aes(x=Labels, y=MAE)) + 
          geom_bar(stat="identity", fill=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))+
          geom_text(aes(label=MAE), vjust=-0.5, color="black", size=2)+
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
          scale_y_continuous(limits=c(0.05,0.058),oob = rescale_none)+
          theme_minimal()
        
        
        DF_MSE <- data.frame(MSE, Labels)
        # Plot MSE for all the models:
        MSE_Plot <- ggplot(DF_MSE, aes(x=Labels, y=MSE)) + 
          geom_bar(stat="identity", fill=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))+
          geom_text(aes(label=MSE), vjust=-0.5, color="black", size=2)+
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
          scale_y_continuous(limits=c(0.0043,0.00535),oob = rescale_none)+
          theme_minimal()
        
        
        DF_RMSE <- data.frame(RMSE, Labels)
        # Plot RMSE for all the models:
        RMSE_Plot <- ggplot(DF_RMSE, aes(x=Labels, y=RMSE)) + 
          geom_bar(stat="identity", fill=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))+
          geom_text(aes(label=RMSE), vjust=-0.5, color="black", size=2)+
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
          scale_y_continuous(limits=c(0.065,0.073),oob = rescale_none)+
          theme_minimal()
        
        
        DF_R_2 <- data.frame(R_2, Labels)
        
        # Plot Rsquared for all the models:
        R_2_Plot <- ggplot(DF_R_2, aes(x=Labels, y=R_2)) + 
          geom_bar(stat = "identity", fill=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))+
          geom_text(aes(label=R_2), vjust=-0.5, color="black", size=2)+
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
          scale_y_continuous(limits=c(0.8251230,0.86),oob = rescale_none)+
          theme_minimal()
        
        
        
        ## Gathering all plots in 1 plot:
        plot_grid(MAE_Plot, MSE_Plot, RMSE_Plot, R_2_Plot , labels = "", scale = c(1, 1, 1, 1))
        
        
        
        
        
        
        # Running Time:
        # Getting the running times and the corresponding models into one DataFrame:
        Running_time_Seconds <- as.numeric(c(time_elapsed1, time_elapsed2, time_elapsed3, time_elapsed4, time_elapsed5, time_elapsed6))
        DF_running_time <- data.frame(Labels, Running_time_Seconds)
        # Printing the results:
        DF_running_time
        
        # Ploting the running time for each model:
        Time_Plot <- ggplot(DF_running_time, aes(x=Labels, y=Running_time, title("All model's running time in seconds"))) + 
          labs(y = "Running Time in Seconds (s)", x = "Models")+
          geom_bar(stat = "identity", fill=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))+
          geom_text(aes(label=Running_time_Seconds), vjust=1.6, color="white", size=2.4)+
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
          theme_minimal()
        Time_Plot
        
        
