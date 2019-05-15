#Load shiny library
library(shiny)

#Create Server
shinyServer(
  
  function(input, output) {
    # Max 500MB  file size
    options(shiny.maxRequestSize=500*1024^2)
    thedata = reactive({
      req(input$file1)
      read.csv(file = input$file1$datapath)
    })
    
    
    output$randomForestPlot = renderPlot({
      
      transactionDataset = thedata()
      
      #Instantiate row names in uploaded dataset
      rownames(transactionDataset)<-1:nrow(transactionDataset)
      rows <- sample(x=1:nrow(thedata()),size=0.7 *  nrow(transactionDataset))
      
      #Split data into test and training data
      train <- transactionDataset[rows,]
      test <-transactionDataset[! rownames(transactionDataset) %in% rows,]
      
      set.seed(42)
      
      #Create Random Forest Model
      rand<-randomForest(Class~.,data=train,replace=T,ntree=100)
      
      #Find importance of each feature in the dataset
      impo<-importance(rand)
      vars<-dimnames(impo)[[1]]
      impo<-data.frame(vars=vars,impo=as.numeric(impo[,1]))
      impo<-impo[order(impo$impo,decreasing=T),]
      
      #Plot Variable Importance
      par(mfrow=c(1,2))
      varImpPlot(rand,main='Variable Importance Plot')
      plot(rand,main='Error vs No. of trees plot')
      
      #Create prediction from data
      pred<-predict(object=rand,newdata=test)
      actual<-test$Class
      result<-data.frame(actual=actual,predicted=pred)
      
      #The column name
      column <- colnames(test)[30]

      #Output data explored
      output$exploringData <- renderText({
        paragVPN <- "The data processed by the Random Forest Model displays the importance between each feature and measures how well it predicts the classification output. The use of a VPN in recorded to determine the use of an anonymisation tool, it is then checked against the overall classification of the transaction to determine an analysis. The Error Vs No of Trees graph tunes the amount of trees that are used in the Random Forest Algorithm by analysing the out of bag error against the number of trees in the forest. In this instance, 100 trees are used in the forest."
        paragIP <- "The data processed by the Random Forest Model displays the importance between each feature and measures how well it predicts the classification output. As 'IP' is a velocity check the users IP address is studied to determine if an anomalous location is processed, it is then checked against the overall classification of the transaction to determine an analysis. The Error Vs No of Trees graph tunes the amount of trees that are used in the Random Forest Algorithm by analysing the out of bag error against the number of trees in the forest. In this instance, 100 trees are used in the forest."
        paragVirtualisation <- "The data processed by the Random Forest Model displays the importance between each feature and measures how well it predicts the classification output. As 'Virtualisation' is a velocity check the use of a virtual machine being used in the transaction is checked against the overall classification of the transaction to determine an analysis. The Error Vs No of Trees graph tunes the amount of trees that are used in the Random Forest Algorithm by analysing the out of bag error against the number of trees in the forest. In this instance, 100 trees are used in the forest."
        paragDevice <- "The data processed by the Random Forest Model displays the importance between each feature and measures how well it predicts the classification output. As 'Device' is a velocity check the device match/mismatch is checked against the overall classification of the transaction to determine an analysis. The Error Vs No of Trees graph tunes the amount of trees that are used in the Random Forest Algorithm by analysing the out of bag error against the number of trees in the forest. In this instance, 100 trees are used in the forest."
        
        parag <- paragDevice
        if("VPN" %in% colnames(test))
        {
          parag <- paragVPN
        }else if ("IP" %in% colnames(test))
        {
          parag <- paragIP
        }else if ("Virtualisation" %in% colnames(test))
        {
          parag <- paragVirtualisation
        }else if ("Device" %in% colnames(test))
        {
          parag <- paragDevice
        }
        paste(parag ,sep="")
      })
      
      
      #Create Confusion Matrix
      output$confMatrix <- renderPlot({
        
        Prediction<-predict(rand, test)
        new <- data.frame(Device=test$Class, Class=Prediction)
        
        thresh <- function(x){
          if(x>0.5) return(1) else return(0)
        }
        new$predicted <- sapply(new$Class, thresh)
        new.summ <- new %>% select(Device,predicted) %>% group_by(Device,predicted) %>% summarise(frequency=n()) %>% ungroup()
        
        if(dim(new.summ[new.summ$Device==0 & new.summ$predicted==1,])[1]==0){
          de<-data.frame(0,1,0)
          names(de)<-c("Device","predicted","frequency")
          new.summ <- rbind(new.summ, de)
        }
        if(dim(new.summ[new.summ$Device==1 & new.summ$predicted==0,])[1]==0){
          de<-data.frame(1,0,0)
          names(de)<-c("Device","predicted","frequency")
          new.summ <- rbind(new.summ, de)
        }
        
        new.summ$hcolor <- ifelse(new.summ$frequency>mean(new.summ$frequency),TRUE,FALSE)
        
          ggplot(data =  new.summ, mapping = aes(x = Device, y = predicted)) +
          geom_tile(aes(fill = frequency), colour = "white") +
          geom_text(aes(label = sprintf("%1.0f", frequency),colour=hcolor), vjust = 1) +
          scale_color_manual(values = c('FALSE' = '#08306b', 'TRUE' = '#f7fbff'), guide = "none")+
          scale_fill_gradient(high = "#08306b", low = "#f7fbff") +
          #theme(legend.position = "none")
          scale_x_continuous(position = "bottom", breaks=c(0,1)) +
          scale_y_continuous(position = "top", breaks=c(1,0)) +
          xlab(paste("Classification of ",column," Velocity ",sep=""))+
          ylab("Classification of Transactions")+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))+
          ggtitle(paste("Confusion matrix displaying genuine/fraudulent classifications and the",column,"velocity")) 
      })
      
      
      #Output Velocity Information
      output$velocityDetails <- renderText({
        textVPN <- "There is a strong correlation between the use of VPNs and the classification of genuine or fraudulent transactions. The velocity checks shows that it is more likely that a transaction will be fraudulent given the use of a VPN. The is because VPNs are commonly used by fraudsters to obfuscate their real location and disguise it as the location of the victim to bypass IP location velocity checks."
        textIP <- "There is a mild correlation between the mismatch of an IP location and the classification of genuine or fraudulent transactions. The velocity checks shows that it is more likely that a transaction is fraudulent when the cardholder's IP address is of an anomalous location. However, as cardholders may travel to distant locations to use their card, it is not a high indicator for a fraudulent transaction. Nevertheless, by completing a transaction in the hometown of the cardholder it greatly increases the chance of a genuine transaction, this is why fraudsters will use a VPN to mimic the location of the legitimate cardholder."
        textVirtualisation <- "There is a strong correlation between the use of virtualisation technologies and the classification of genuine or fraudulent transactions. The velocity check shows that it is more likely that a transaction will be fraudulent given the use of virtualisation technologies. This is because using virtual machines are commonly used by fraudsters to match their operating system with the operarting system of the victim to bypass device velocity checks."
        textDevice <- "There is a mild correlation between a device match/mismatch and the correlation of genuine or fraudulent transactions. This is because cardholers use many devices (sometimes not even their own) to complete transactions. However, using a known device increases the likelihood of a transaction being genuine, hence fraudtsers using virtualisation technologies to mimic known devices used by the cardholder."
        
        text <- textDevice
        if("VPN" %in% colnames(test))
        {
          text <- textVPN
        }else if ("IP" %in% colnames(test))
        {
          text <- textIP
        }else if ("Virtualisation" %in% colnames(test))
        {
          text <- textVirtualisation
        }else if ("Device" %in% colnames(test))
        {
          text <- textDevice
        }
        paste(text ,sep="")
      })
      
      
      #Create Accuracy Percentage
      output$accuracy <- renderPrint({
        my.accuracy <- function(df, actual, predicted){
          y <- as.vector(table(df[,predicted], df[,actual]))
          message(y)
          names(y) <- c("TN", "FP", "FN", "TP")
          acur <- (y["TP"]+y["TN"])/sum(y)*100
          return(as.numeric(acur))
        }
        Prediction<-predict(rand, test)
        new <- data.frame(Device=test$Class, Class=Prediction)
        
        thresh <- function(x){
          if(x>0.5) return(1) else return(0)
        }
        new$predicted <- sapply(new$Class, thresh)
        
        my.accuracy(new,"Device","predicted")
      })
      
      #Summary of Accuracy Output
      output$accuracyText <- renderText({
        accuracyText <- "The accuracy above details the performance of the random forest algorithm and the velocity checks. The outcome is shown to very high which demonstrates that a fusion approach is advantageous when detecting credit card fraud."
        paste(accuracyText)
      })
      
      #Summary of Model Output
      output$summary <- renderPrint({
        rand<-randomForest(Class~.,data=train,replace=T,ntree=100)
        rand
      })
      
    })
    
  }
)