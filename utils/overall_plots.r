# Overall roc curve combining each model's roc curve
#######################################################################################################
plot_roc_all = function(){
overlay_rocs = function(roc_num,col_num){
  plot(roc_num,
       col = paste(col_num),
       lwd = 2,
       cex.main = 2,
       cex.lab = 1.5,
       cex.axis = 1.25,
       add = TRUE)  
}



  plot(roc,
       col = "2",
       lwd = 2,
       cex.main = 2,
       cex.lab = 1.5,
       cex.axis = 1.25)
  overlay_rocs(roc2,3)
  overlay_rocs(roc3,4)
  overlay_rocs(roc4,5)
  overlay_rocs(roc5,6)
  abline(a=0, b=1) 
  legend(x="bottomright",y = NA, c('Logistic Regression','Decision Tree','Random Forest','Naive Bayes','K-Nearest Neighbor'), lty = 1,col = 2:6, title = "ROC by model", cex = 1)
  legend(x="topright",y = NA, round(c(auc,auc2,auc3,auc4,auc5)/max(c(auc,auc2,auc3,auc4,auc5)),3),text.col = 2:6,  title = "Relative AUC", title.col = 1, cex = 0.85)
}

## Combining all measurements into a plot
##############################################################################################################
dat1 = as.data.frame(rbind(cbind(rep("Accuracy",5),mod,Accuracy),cbind(rep("Recall",5),mod,Recall),cbind(rep("Precision",5),mod,Precision)))
dat1 = transmute(dat1,Measure = V1,Model = mod,value = as.numeric(Accuracy))
plot_measure_all = function(){ 
  ggplot(dat1) +
  geom_bar(aes(x = Measure,y=value ,group = Model,fill = Model),  position = "dodge", stat="identity") +
  labs(title = 'Measures comparison', fill = 'Model') +
  theme(axis.title = element_blank(),
        title = element_text(size = 20, face = 'bold'),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) 
}


## Attempting a 'all' type-extension for the top 5 predictor plots
###############################################################################################################
bundle = function(wc,i_mod)cbind(rep(mod[i_mod],5),cbind(wc[,1],(wc[,2]/sum(wc[,2]))))
dat2 = as.data.frame(rbind(bundle(wc,1),bundle(wc2,2),bundle(wc3,3),bundle(df4,4)))
dat2 =data.frame(Model = dat2[,1],Predictor = dat2[,2], value = dat2[,3])


n = count(as.data.frame(unique(dat2$Predictor)))
m = count(as.data.frame(unique(dat2$Model)))
dat3 = join(data.frame(Model=sort(rep(unique(dat2$Model),n)),Predictor=rep(unique(dat2$Predictor),m)),dat2) 
dat3$value[is.na(dat3$value)] <- 0


#Shortening labels
replace_model_value = function(str,new_str)gsub(pattern = str,replacement = new_str,x = dat3$Model,ignore.case = T,perl = T)
replace_model_value("Log(.*)","Log")
dat3$Model= replace_model_value("Log(.*)","Log")
dat3$Model= replace_model_value("Tree(.*)","Tree")
dat3$Model= replace_model_value("Naive(.*)","N_Bayes")
dat3$Model= replace_model_value("Rand(.*)","RF")


plot_predictors_all = function(){
ggplot(dat3) +
  geom_bar(aes(x=Predictor, y= as.numeric(value) ,group = Predictor,fill = Predictor),  position = "dodge", stat="identity") +
  theme(axis.title = element_blank(),
        title = element_text(size = 20, face = 'bold'),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_blank()
        ) +
  labs(title = 'Model variable comparison', fill = 'Variable')+
scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+

facet_grid(col=vars(dat3$Model))
}