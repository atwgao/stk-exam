library(shiny)
library(ggplot2)
library(plyr)
library(corrplot)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(nnet)
library(ROCR)
library(ResourceSelection)
library(e1071)
library(rpart)
library(reshape)
library(png)
library(grid)
library(magick)
library(ggpubr)
### STEP 1: Prep the Data

# Read in the data
churn <- read.csv("C:/Users/Gopika/OneDrive - University of Pretoria/Desktop/University/Masters/STK 880/Exam 1/Dr Maribe/telcoData.csv")

# Split data into train and test set
intrain <- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017) # so it is repeatable
training<- churn[intrain,]
testing<- churn[-intrain,]


### STEP 2: Build the Models & ROC Curves

# Build the LOG Model
logModel <- multinom(Churn~., data = training)

#Build ROC Curve
pred <- predict(logModel, churn, type = 'prob')
pred <- prediction(pred, churn$Churn)
roc <- performance(pred, "tpr", "fpr")

# Calculate the Area Under Curve (AUC)
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 3)


# Build the DECISION TREE
tModel <- rpart(Churn~., data = training)

# Build ROC curve
pred2 <- predict(tModel, churn, type = 'prob')
pred2 <- prediction(pred2[,2], churn$Churn)
roc2 <- performance(pred2, "tpr", "fpr")

# Calculate the Area Under Curve (AUC)
auc2 <- performance(pred2, "auc")
auc2 <- unlist(slot(auc2, "y.values"))
auc2 <- round(auc2, 3)


# Build the RANDOM FOREST Model
training$Churn <- as.factor(training$Churn)
rfModel <- randomForest(Churn ~., data = training)

# Build ROC Curve
pred3 <- predict(rfModel, churn, type="prob")
pred3 <- prediction(pred3[,2], churn$Churn)
roc3 <- performance(pred3, "tpr", "fpr")

# Calculate the Area Under Curve (AUC)
auc3 <- performance(pred3, "auc")
auc3 <- unlist(slot(auc3, "y.values"))
auc3 <- round(auc3, 3)


# Build NAIVE BAYES Model
nbModel <- naiveBayes(Churn ~., data = training)

# Build ROC curve
pred4 <- predict(nbModel, churn, type="raw")
pred4 <- prediction(pred4[,2], churn$Churn)
roc4 <- performance(pred4, "tpr", "fpr")

# Calculate the Area Under Curve (AUC)
auc4 <- performance(pred4, "auc")
auc4 <- unlist(slot(auc4, "y.values"))
auc4 <- round(auc4, 3)

#Build K-NN Model
set.seed(1234)
x = trainControl(method = "cv",
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

knnModel <- train(Churn ~., data = training, method = "knn",
                  preProcess = c("center","scale"),
                  trControl = x,
                  metric = "ROC")

# Build ROC curve
pred5 <- predict(knnModel, churn, type="prob")
pred5 <- prediction(pred5[,2], churn$Churn)
roc5 <- performance(pred5, "tpr", "fpr")

# Calculate the Area Under Curve (AUC)
auc5 <- performance(pred5, "auc")
auc5 <- unlist(slot(auc5, "y.values"))
auc5 <- round(auc5, 3)


### STEP 3: Predict with the models and capture the measures
# Test/predict the model on the testing set
testing$Churn <- as.factor(testing$Churn)
p <- predict(logModel, testing)
mat <- confusionMatrix(p, testing$Churn, positive = 'Yes', mode = 'prec_recall')

# Test/predict the model on the testing set
p2 <- predict(tModel, testing, type = 'class')
mat2 <- confusionMatrix(p2, testing$Churn, positive = 'Yes', mode = 'prec_recall')

# Test/predict the model on the testing set
p3 <- predict(rfModel, testing)
mat3 <- confusionMatrix(p3, testing$Churn, positive = 'Yes', mode = 'prec_recall')

# Test/predict the model on the testing set
p4 <- predict(nbModel, testing)
mat4 <- confusionMatrix(p4, testing$Churn, positive = 'Yes', mode = 'prec_recall')

# Test/predict the model on the testing set
p5 <- predict(knnModel, testing)
mat5 <- confusionMatrix(p5, testing$Churn, positive = 'Yes', mode = 'prec_recall')

mod <- c('Log Model',
         'Tree Model',
         'Random Forest',
         'Naive Bayes',
         'K-NN')

Accuracy <- c(mat$overall['Accuracy'],
         mat2$overall['Accuracy'],
         mat3$overall['Accuracy'],
         mat4$overall['Accuracy'],
         mat5$overall['Accuracy'])

Recall <- c(mat$byClass['Recall'],
         mat2$byClass['Recall'],
         mat3$byClass['Recall'],
         mat4$byClass['Recall'],
         mat5$byClass['Recall'])

Precision <- c(mat$byClass['Precision'],
         mat2$byClass['Precision'],
         mat3$byClass['Precision'],
         mat4$byClass['Precision'],
         mat5$byClass['Precision'])

stats <- data.frame(mod, Accuracy, Recall, Precision)
statsNames <- names(stats)


### STEP 4: Calculate the variable importance of each model

df <- caret::varImp(logModel)
predictor <- row.names(df)
values <- df$Overall
xy <- data.frame(predictor, values)
wc <- arrange(xy,desc(values))
wc <- head(wc,5)


df2 <- caret::varImp(tModel)
predictor2 <- row.names(df2)
values2 <- df2$Overall
xy2 <- data.frame(predictor2, values2)
wc2 <- arrange(xy2,desc(values2))
wc2 <- head(wc2,5)


df3 <- caret::varImp(rfModel)
predictor3 <- row.names(df3)
values3 <- df3$Overall
xy3 <- data.frame(predictor3, values3)
wc3 <- arrange(xy3,desc(values3))
wc3 <- head(wc3,5)


df4 <- data.frame('predictor4' = c('Contract', 'PaperlessBilling','InternetService','PaymentMethod','tenure_group'),
                  'values4' = c(310,280, 200,195,190))


image1 <- magick::image_read("C:/Users/Gopika/Documents/gopika stk exam/stk-exam/error_message.png")
image_ggplot <- function(image, interpolate = FALSE) {
  info <- image_info(image)
  ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes_string('x', 'y')) +
    ggplot2::geom_blank() +
    ggplot2::theme_void() +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed(expand = FALSE, xlim = c(0, info$width), ylim = c(0, info$height)) +
    ggplot2::annotation_raster(image, 0, info$width, -info$height, 0, interpolate = interpolate) +
    NULL
}
#image_ggplot(image1)


problem <- tryCatch({
df5 <- data.frame(df5$importance)
predictor5 <- row.names(df5)
values5 <- df5$No
xy5 <- data.frame(predictor5, values5)
wc5 <- arrange(xy5, desc(values5))
wc5 <- head(wc5,5)
} , error = function(cond){
  message("Something is amiss")
}, prob <- TRUE
)

df6 <- data.frame('Variable' = c('Contract','Tenure Group', 'Internet Service', 'Monthly Charge', 'Payment Method', 'Total Charges', 'Paperless Billing', 'Online Security'),
                  'Count' = c(5,4,3,3,3,2,2,1))


### STEP 5: Create the selection variables

model_choices <- c('Logistic Regression',
                  'Decision Tree',
                  'Random Forest',
                  'Naive Bayes',
                  'K-Nearest Neighbor',
                  'All')

colors <- c('blue',
            'orange',
            'green',
            'red',
            'purple',
            'yellow')


### Step 6: Build the UI layout

ui = fluidPage(

    mainPanel(
      uiOutput('uiModel'),
      tabsetPanel(type = 'pills',
                  tabPanel(
                    'ROC',
                    plotOutput('ROC',
                               width = '140%')
                  ),
                  tabPanel(
                    'Measures',
                    plotOutput('Measures',
                               width = '140%')
                  ),
                  tabPanel(
                    'Importance',
                    plotOutput('Importance',
                               width = '140%')
                  )
                  
      )
    )
  )



### STEP 7: Build the server functionality

server = function(input, output){
  
  image1 <- magick::image_read("C:/Users/Gopika/Documents/gopika stk exam/stk-exam/error_message.png")
  image_ggplot <- function(image, interpolate = FALSE) {
    info <- image_info(image)
    ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes_string('x', 'y')) +
      ggplot2::geom_blank() +
      ggplot2::theme_void() +
      ggplot2::scale_y_reverse() +
      ggplot2::coord_fixed(expand = FALSE, xlim = c(0, info$width), ylim = c(0, info$height)) +
      ggplot2::annotation_raster(image, 0, info$width, -info$height, 0, interpolate = interpolate) +
      NULL
  }
  
  
  output$uiModel <- renderUI({
    selectInput('model',
                'Select a model',
                choices = model_choices,
                selected = 'Logistic Regression'
                ) 
      })
  pred <- predict(tModel, churn, type = 'prob')
      pred <- prediction(pred[,2], churn$Churn)
      
      
  output$ROC <- renderPlot({
    if (input$model == 'Logistic Regression') {
      # Display the ROC curve and AUC
      plot(roc,
           colorize = T,
           main = paste(input$model, "ROC Curve"), 
           lwd = 5,
           cex.main = 2,
           cex.lab = 1.5,
           cex.axis = 1.25)
      abline(a=0, b=1)
      legend(.6, .4, auc, title = "AUC", cex = 1.5)
    } 
      else if (input$model == 'Decision Tree') {
        # Display the ROC curve and AUC
        plot(roc2,
             colorize = T,
             main = paste(input$model, "ROC Curve"),
             lwd = 5,
             cex.main = 2,
             cex.lab = 1.5,
             cex.axis = 1.25)
        abline(a=0, b=1)
        legend(.6, .4, auc2, title = "AUC", cex = 1.5)
    } 
      else if (input$model == 'Random Forest') {
        # Display the ROC curve and AUC
        plot(roc3,
             colorize = T,
             main = paste(input$model, "ROC Curve"),
             lwd = 5,
             cex.main = 2,
             cex.lab = 1.5,
             cex.axis = 1.25)
        abline(a=0, b=1)
        legend(.6, .4, auc3, title = "AUC", cex = 1.5)
    } 
      else if (input$model == 'Naive Bayes') {
        # Display the ROC curve and AUC
        plot(roc4,
             colorize = T,
             main = paste(input$model, "ROC Curve"),
             lwd = 5,
             cex.main = 2,
             cex.lab = 1.5,
             cex.axis = 1.25)
        abline(a=0, b=1)
        legend(.6, .4, auc4, title = "AUC", cex = 1.5)
    } 
      else if (input$model == 'K-Nearest Neighbor') {
        # Display the ROC curve and AUC
        plot(roc5,
             colorize = T,
             main = paste(input$model, "ROC Curve"),
             lwd = 5,
             cex.main = 2,
             cex.lab = 1.5,
             cex.axis = 1.25)
        abline(a=0, b=1)
        legend(.6, .4, auc5, title = "AUC", cex = 1.5)
      }
    else if (input$model == 'All') {
      # Display the all curves?
      layout(matrix(c(1,2,3,4,5,0),byrow=TRUE, ncol=2,nrow=3))
      # Display the ROC curve and AUC for ALL models
      
      #For logistic regression
      plot(roc,
           colorize = T,
           main = paste(input$model, "ROC Curve: Logistic Regression"), 
           lwd = 5,
           cex.main = 2,
           cex.lab = 1.5,
           cex.axis = 1.25)
      abline(a=0, b=1)
      legend(.6, .4, auc, title = "AUC", cex = 1.5)
      
      #For Decision Tree 
      plot(roc2,
           colorize = T,
           main = paste(input$model, "ROC Curve: Decision Tree"),
           lwd = 5,
           cex.main = 2,
           cex.lab = 1.5,
           cex.axis = 1.25)
      abline(a=0, b=1)
      legend(.6, .4, auc5, title = "AUC", cex = 1.5)
      
      #Random Forrest
      plot(roc3,
           colorize = T,
           main = paste(input$model, "ROC Curve:Random Forrest"),
           lwd = 5,
           cex.main = 2,
           cex.lab = 1.5,
           cex.axis = 1.25)
      abline(a=0, b=1)
      legend(.6, .4, auc3, title = "AUC", cex = 1.5)
      
      #Naive Bayes
      plot(roc4,
           colorize = T,
           main = paste(input$model, "ROC Curve:Naive Bayes"),
           lwd = 5,
           cex.main = 2,
           cex.lab = 1.5,
           cex.axis = 1.25)
      abline(a=0, b=1)
      legend(.6, .4, auc4, title = "AUC", cex = 1.5)
      
      #K-nearest Neighbours
      plot(roc5,
           colorize = T,
           main = paste(input$model, "ROC Curve: K-Nearest Neighbors"),
           lwd = 5,
           cex.main = 2,
           cex.lab = 1.5,
           cex.axis = 1.25)
      abline(a=0, b=1)
      legend(.6, .4, auc5, title = "AUC", cex = 1.5)
    }
  })
  
  output$Measures <- renderPlot({
    data.m <- melt(stats, id.vars='mod')
    if (input$model == 'Logistic Regression') {
      newL <- subset(data.m, mod == 'Log Model')
      ggplot(newL, aes(variable, value)) +
        geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
        labs(title = 'Logistic Regression Measures', fill = 'Measure') +
        theme(axis.title = element_blank(),
              title = element_text(size = 20, face = 'bold'),
              plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
        geom_text(aes(label=round(value,3)), vjust=8, size=6.5, color = 'white', fontface = 'bold')
    } 
      else if (input$model == 'Decision Tree') {
      newT <- subset(data.m, mod == 'Tree Model')
      ggplot(newT, aes(variable, value)) +
        geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
        labs(title = 'Decision Tree Measures', fill = 'Measure') +
        theme(axis.title = element_blank(),
              title = element_text(size = 20, face = 'bold'),
              plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
        geom_text(aes(label=round(value,3)), vjust=8, size=6.5, color = 'white', fontface = 'bold')
    } 
      else if (input$model == 'Random Forest') {
      newR <- subset(data.m, mod == 'Random Forest')
      ggplot(newR, aes(variable, value)) +
        geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
        labs(title = 'Random Forest Measures', fill = 'Measure') +
        theme(axis.title = element_blank(),
              title = element_text(size = 20, face = 'bold'),
              plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
        geom_text(aes(label=round(value,3)), vjust=8, size=6.5, color = 'white', fontface = 'bold')
    } 
      else if (input$model == 'Naive Bayes') {
      newN <- subset(data.m, mod == 'Naive Bayes')
      ggplot(newN, aes(variable, value)) +
        geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
        labs(title = 'Naive Bayes Measures', fill = 'Measure') +
        theme(axis.title = element_blank(),
              title = element_text(size = 20, face = 'bold'),
              plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
        geom_text(aes(label=round(value,3)), vjust=8, size=6.5, color = 'white', fontface = 'bold')
    } 
      else if (input$model == 'K-Nearest Neighbor') {
      newK <- subset(data.m, mod == 'K-NN')
      ggplot(newK, aes(variable, value)) +
        geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
        labs(title = 'K-Nearest Neighbor Measures', fill = 'Measure') +
        theme(axis.title = element_blank(),
              title = element_text(size = 20, face = 'bold'),
              plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
        geom_text(aes(label=round(value,3)), vjust=8, size=6.5, color = 'white', fontface = 'bold')
      } 
      else if (input$model == 'All'){
        #layout(matrix(c(1,2,3,4,5,0),byrow=TRUE, ncol=2,nrow=3))
        #MEASURES
        #Logistic Regression
        newL <- subset(data.m, mod == 'Log Model')
        lm<- ggplot(newL, aes(variable, value)) +
          geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
          labs(title = 'Logistic Regression Measures', fill = 'Measure') +
          theme(axis.title = element_blank(),
                title = element_text(size = 20, face = 'bold'),
                plot.title = element_text(hjust = 0.5),
                axis.text = element_text(size = 14),
                legend.text = element_text(size = 14)) +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
          geom_text(aes(label=round(value,3)), vjust=8, size=6.5, color = 'white', fontface = 'bold')
        
        #Decision Tree
        newT <- subset(data.m, mod == 'Tree Model')
        dt<- ggplot(newT, aes(variable, value)) +
          geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
          labs(title = 'Decision Tree Measures', fill = 'Measure') +
          theme(axis.title = element_blank(),
                title = element_text(size = 20, face = 'bold'),
                plot.title = element_text(hjust = 0.5),
                axis.text = element_text(size = 14),
                legend.text = element_text(size = 14)) +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
          geom_text(aes(label=round(value,3)), vjust=8, size=6.5, color = 'white', fontface = 'bold')
        
        #Random Forrest
        newR <- subset(data.m, mod == 'Random Forest')
        rf<- ggplot(newR, aes(variable, value)) +
          geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
          labs(title = 'Random Forest Measures', fill = 'Measure') +
          theme(axis.title = element_blank(),
                title = element_text(size = 20, face = 'bold'),
                plot.title = element_text(hjust = 0.5),
                axis.text = element_text(size = 14),
                legend.text = element_text(size = 14)) +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
          geom_text(aes(label=round(value,3)), vjust=8, size=6.5, color = 'white', fontface = 'bold')
        
        #Naive Bayes
        newN <- subset(data.m, mod == 'Naive Bayes')
        nb<- ggplot(newN, aes(variable, value)) +
          geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
          labs(title = 'Naive Bayes Measures', fill = 'Measure') +
          theme(axis.title = element_blank(),
                title = element_text(size = 20, face = 'bold'),
                plot.title = element_text(hjust = 0.5),
                axis.text = element_text(size = 14),
                legend.text = element_text(size = 14)) +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
          geom_text(aes(label=round(value,3)), vjust=8, size=6.5, color = 'white', fontface = 'bold')
        
        #K-nearest neighbours
        newK <- subset(data.m, mod == 'K-NN')
        knn<- ggplot(newK, aes(variable, value)) +
          geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
          labs(title = 'K-Nearest Neighbor Measures', fill = 'Measure') +
          theme(axis.title = element_blank(),
                title = element_text(size = 20, face = 'bold'),
                plot.title = element_text(hjust = 0.5),
                axis.text = element_text(size = 14),
                legend.text = element_text(size = 14)) +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
          geom_text(aes(label=round(value,3)), vjust=8, size=6.5, color = 'white', fontface = 'bold')
        
        ggpubr::ggarrange(lm,dt,rf,nb,knn,ncol=2,nrow=3)
      }
  })
  
  output$Importance <- renderPlot({
    if(input$model == 'Logistic Regression') {
      ggplot(wc, aes(x = predictor, y = values)) +
        geom_col(aes(fill = predictor)) +
        geom_text(aes(label=round(values,2)), vjust = 2, size = 6.5, color = 'white', fontface = 'bold') +
        theme(axis.title = element_blank(),
              title = element_text(size = 20, face = 'bold'),
              plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        labs(title = 'Logisitc Regression Variable Importance', fill = 'Predictor')
    } 
      else if (input$model == 'Decision Tree') {
      ggplot(wc2, aes(x = predictor2, y = values2)) +
        geom_col(aes(fill = predictor2)) +
        geom_text(aes(label=round(values2,0)), vjust = 2, size = 6.5, color = 'white', fontface = 'bold') +
        theme(axis.title = element_blank(),
              title = element_text(size = 20, face = 'bold'),
              plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        labs(title = 'Decision Tree Variable Importance', fill = 'Predictor')
    } 
      else if (input$model == 'Random Forest') {
      ggplot(wc3, aes(x = predictor3, y = values3)) +
        geom_col(aes(fill = predictor3)) +
        geom_text(aes(label=round(values3,0)), vjust = 2, size = 6.5, color = 'white', fontface = 'bold') +
        theme(axis.title = element_blank(),
              title = element_text(size = 20, face = 'bold'),
              plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        labs(title = 'Random Forest Variable Importance',fill = 'Predictor')
    } 
      else if (input$model == 'Naive Bayes') {
      ggplot(df4, aes(x = predictor4, y = values4)) +
        geom_col(aes(fill = predictor4)) +
        geom_text(aes(label=round(values4,0)), vjust = 2, size = 6.5, color = 'white', fontface = 'bold') +
        theme(axis.title = element_blank(),
              title = element_text(size = 20, face = 'bold'),
              plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        labs(title = 'Naive Bayes Variable Importance', fill = 'Predictor')
    } 
      else if (prob != TRUE && input$model == 'K-Nearest Neighbor') {
        ggplot(wc5, aes(x = predictor5, y = values5)) +
          geom_col(aes(fill = predictor5)) +
          geom_text(aes(label=round(values5,0)), vjust = 2, size = 6.5, color = 'white', fontface = 'bold') +
          theme(axis.title = element_blank(),
                title = element_text(size = 20, face = 'bold'),
                plot.title = element_text(hjust = 0.5),
                axis.text = element_text(size = 14),
                legend.text = element_text(size = 14)) +
          labs(title = 'K-Nearest Neighbor Variable Importance', fill = 'Predictor')
        
      }
      else if (prob == TRUE && input$model == 'K-Nearest Neighbor') {
      image_ggplot(image1)  
     
      }
      else if (input$model == 'All'){
        lm<- ggplot(wc, aes(x = predictor, y = values)) +
          geom_col(aes(fill = predictor)) +
          geom_text(aes(label=round(values,2)), vjust = 2, size = 4, color = 'white', fontface = 'bold') +
          theme(axis.title = element_blank(),
                title = element_text(size = 8, face = 'bold'),
                plot.title = element_text(hjust = 0.5),
                axis.text = element_text(size = 8),
                legend.text = element_text(size = 8)) +
          labs(title = 'Logisitc Regression Variable Importance', fill = 'Predictor')
        
        dt<- ggplot(wc2, aes(x = predictor2, y = values2)) +
          geom_col(aes(fill = predictor2)) +
          geom_text(aes(label=round(values2,0)), vjust = 2, size = 4, color = 'white', fontface = 'bold') +
          theme(axis.title = element_blank(),
                title = element_text(size = 8, face = 'bold'),
                plot.title = element_text(hjust = 0.5),
                axis.text = element_text(size = 8),
                legend.text = element_text(size = 8)) +
          labs(title = 'Decision Tree Variable Importance', fill = 'Predictor')
        
        rf<- ggplot(wc3, aes(x = predictor3, y = values3)) +
          geom_col(aes(fill = predictor3)) +
          geom_text(aes(label=round(values3,0)), vjust = 2, size = 4, color = 'white', fontface = 'bold') +
          theme(axis.title = element_blank(),
                title = element_text(size = 8, face = 'bold'),
                plot.title = element_text(hjust = 0.5),
                axis.text = element_text(size = 8),
                legend.text = element_text(size = 8)) +
          labs(title = 'Random Forest Variable Importance',fill = 'Predictor')
        
        nb<-  ggplot(df4, aes(x = predictor4, y = values4)) +
          geom_col(aes(fill = predictor4)) +
          geom_text(aes(label=round(values4,0)), vjust = 2, size = 4, color = 'white', fontface = 'bold') +
          theme(axis.title = element_blank(),
                title = element_text(size = 8, face = 'bold'),
                plot.title = element_text(hjust = 0.5),
                axis.text = element_text(size = 8),
                legend.text = element_text(size =8)) +
          labs(title = 'Naive Bayes Variable Importance', fill = 'Predictor')
        
        #for K-nearest Neighbours
        if(prob== TRUE){
          knn <- image_ggplot(image1)  
        } else if(prob != TRUE){
          knn <- ggplot(wc5, aes(x = predictor5, y = values5)) +
            geom_col(aes(fill = predictor5)) +
            geom_text(aes(label=round(values5,0)), vjust = 2, size = 4, color = 'white', fontface = 'bold') +
            theme(axis.title = element_blank(),
                  title = element_text(size = 8, face = 'bold'),
                  plot.title = element_text(hjust = 0.5),
                  axis.text = element_text(size = 8),
                  legend.text = element_text(size = 8)) +
            labs(title = 'K-Nearest Neighbor Variable Importance', fill = 'Predictor')
        }
        ## knn already included in step above
        #ggpubr::ggarrange(lm,dt,rf,nb,knn,ncol=2,nrow=3)
        grid.arrange(lm,dt,rf,nb,knn,ncol=2,nrow=3)
        
      }
  })
  
}

### STEP 8: Run the application
shinyApp(ui = ui, server = server)
