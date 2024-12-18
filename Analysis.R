#############GAM################
fit <- lm(NASC ~ SSS + SST + SSTG + SSH + Chl, data=Mydata)
vif(fit)
GAM1<-gam(log(NASC) ~ s(SST), family = gaussian, data = Mydata)
GAM2<-gam(log(NASC) ~ s(SSTG), family = gaussian, data = Mydata)
GAM3<-gam(log(NASC) ~ s(SSH), family = gaussian, data = Mydata)
GAM4<-gam(log(NASC) ~ s(Chl), family = gaussian, data = Mydata)
AIC1<-c(AIC(GAM1),AIC(GAM2),AIC(GAM3),AIC(GAM4))
GAM31<-gam(log(NASC) ~ s(SSH) + s(SST), family = gaussian, data = Mydata)
GAM32<-gam(log(NASC) ~ s(SSH) + s(SSTG), family = gaussian, data = Mydata)
GAM33<-gam(log(NASC) ~ s(SSH) + s(Chl), family = gaussian, data = Mydata)
AIC2<-c(AIC(GAM31),AIC(GAM32),AIC(GAM33))
GAM331<-gam(log(NASC) ~ s(SSH) + s(Chl) + s(SST), family = gaussian, data = Mydata)
GAM332<-gam(log(NASC) ~ s(SSH) + s(Chl) + s(SSTG), family = gaussian, data = Mydata)
AIC3<-c(AIC(GAM331),AIC(GAM332))
GAM3311<-gam(log(NASC) ~ s(SSH) + s(Chl) + s(SST) + s(SSTG), family = gaussian, data = Mydata)
AIC4<-c(AIC(GAM3311))
#BRT
control <- trainControl(method = "cv",
                        number = 10,
                        savePredictions = "final",
                        summaryFunction = defaultSummary) 
bag.fraction = 0.5
grid <- expand.grid(n.trees = c(1000, 2000, 3000, 4000, 5000), 
                    interaction.depth = c(1, 2, 3, 4, 5), 
                    shrinkage = c(0.0005, 0.001, 0.005, 0.01, 0.05),   
                    n.minobsinnode = c(10, 20) 
) 
train_model <- train(NASC ~ SST + SSTG + SSH + Chl, 
                     data = data,
                     method = "gbm",
                     trControl = control,
                     tuneGrid = grid,
                     metric = "RMSE", 
                     distribution = "gaussian")
print(train_model)
#############SI################
#SI_Chl
Chl = data[,"Chl"]
Chl = as.numeric(as.character(Chl))
Chl_int = (classIntervals(Chl, 10, style = "fisher"))
Chl_int[[2]][1] = Chl_int[[2]][1]-0.001
Chl_bins = (cut(Chl, breaks = Chl_int$brks))
summary(Chl_bins)
data$Chl_bins = cut(Chl, breaks = Chl_int$brks)
Chl = aggregate(data$NASC ~ Chl_bins, data = data, FUN = "mean")
colnames(Chl)[1] = "Chl_bins"
colnames(Chl)[2] = "NASC_Chl"
Chl$SI_Chl = ((Chl$NASC_Chl - min(Chl[,2]))/
                (max(Chl[,2]) - min(Chl[,2])))
data = merge(data, Chl, by = "Chl_bins", all = TRUE)
#SI_SSTG
SSTG = data[,"SSTG"]
SSTG = as.numeric(as.character(SSTG))
SSTG_int = (classIntervals(SSTG, 10, style = "fisher"))
SSTG_int[[2]][1] = SSTG_int[[2]][1]-0.001
SSTG_bins = (cut(SSTG, breaks = SSTG_int$brks))
summary(SSTG_bins)
data$SSTG_bins = cut(SSTG, breaks = SSTG_int$brks)
SSTG = aggregate(data$NASC ~ SSTG_bins, data = data, FUN = "mean")
colnames(SSTG)[1] = "SSTG_bins"
colnames(SSTG)[2] = "NASC_SSTG"
SSTG$SI_SSTG = ((SSTG$NASC_SSTG - min(SSTG[,2]))/
                  (max(SSTG[,2]) - min(SSTG[,2])))
data = merge(data, SSTG, by = "SSTG_bins", all = TRUE)
#SI_SST
SST = data[,"SST"]
SST = as.numeric(as.character(SST))
SST_int = (classIntervals(SST, 10, style = "fisher"))
SST_int[[2]][1] = SST_int[[2]][1]-0.001
SST_bins = (cut(SST, breaks = SST_int$brks))
summary(SST_bins)
data$SST_bins = cut(SST, breaks = SST_int$brks)
SST = aggregate(data$NASC ~ SST_bins, data = data, FUN = "mean")
colnames(SST)[1] = "SST_bins"
colnames(SST)[2] = "NASC_SST"
SST$SI_SST = ((SST$NASC_SST - min(SST[,2]))/
                (max(SST[,2]) - min(SST[,2])))
data = merge(data, SST, by = "SST_bins", all = TRUE)
#SI_SSH
SSH = data[,"SSH"]
SSH = as.numeric(as.character(SSH))
SSH_int = (classIntervals(SSH, 10, style = "fisher"))
SSH_int[[2]][1] = SSH_int[[2]][1]-0.001
SSH_bins = (cut(SSH, breaks = SSH_int$brks))
summary(SSH_bins)
data$SSH_bins = cut(SSH, breaks = SSH_int$brks)
SSH = aggregate(data$NASC ~ SSH_bins, data = data, FUN = "mean")
colnames(SSH)[1] = "SSH_bins"
colnames(SSH)[2] = "NASC_SSH"
SSH$SI_SSH = ((SSH$NASC_SSH - min(SSH[,2]))/
                (max(SSH[,2]) - min(SSH[,2])))
data = merge(data, SSH, by = "SSH_bins", all = TRUE)
#sort SI tables
Chl = Chl[order(Chl$Chl_bins),]
SSTG = SSTG[order(SSTG$SSTG_bins),]
SST = SST[order(SST$SST_bins),]
SSH = SSH[order(SSH$SSH_bins),]
data = data[,c( "Lat", "Lon", "NASC", "SST", "SSTG", "SSH", "Chl", "SI_SST", "SI_SSTG", "SI_SSH", "SI_Chl")]
SSTG_bins = as.character(SSTG_bins)
Chl_bins = as.character(Chl_bins)
SST_bins = as.character(SST_bins)
SSH_bins = as.character(SSH_bins)
a = lapply(strsplit(as.character(SSTG$SSTG_bins), split = ","), "[", 1)
b = lapply(strsplit(as.character(Chl$Chl_bins), split = ","), "[", 1)
d = lapply(strsplit(as.character(SST$SST_bins), split = ","), "[", 1)
e = lapply(strsplit(as.character(SSH$SSH_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a))) 
bb = as.numeric(sub("\\(","", unlist(b)))
dd = as.numeric(sub("\\(","", unlist(d)))
ee = as.numeric(sub("\\(","", unlist(e)))
SSTG_axis = aa 
Chl_axis = bb
SST_axis = dd
SSH_axis = ee
table(SSTG_axis)
table(Chl_axis)
table(SST_axis)
table(SSH_axis)
#####SSTG########
newSSTG<-nls(SI_SSTG ~exp(-a*(SSTG-b)^2), data = data, start = list(a =1, b = 2))
summary_newSSTG <- summary(newSSTG)
estimated_a <- summary_newSSTG$coefficients["a", "Estimate"]
estimated_b <- summary_newSSTG$coefficients["b", "Estimate"]
#####Chl########
newChl<-nls(SI_Chl ~exp(-a*(Chl-b)^2), data = data, start = list(a =1, b = 2))
summary_newChl <- summary(newChl)
estimated_a <- summary_newChl$coefficients["a", "Estimate"]
estimated_b <- summary_newChl$coefficients["b", "Estimate"]
#####SST########
newSST<-nls(SI_SST ~exp(-a*(SST-b)^2), data = data, start = list(a =1, b = 2))
summary_newSST <- summary(newSST)
estimated_a <- summary_newSST$coefficients["a", "Estimate"]
estimated_b <- summary_newSST$coefficients["b", "Estimate"]
#####SSH########
newSSH<-nls(SI_SSH ~exp(-a*(SSH-b)^2), data = data, start = list(a =1, b = 2))
summary_newSSH <- summary(newSSH)
estimated_a <- summary_newSSH$coefficients["a", "Estimate"]
estimated_b <- summary_newSSH$coefficients["b", "Estimate"]
##################HSI################## 
data$AMM_HSI <- 0.2687*data$SI_SST+0.0780*data$SI_SSTG+0.4623*data$SI_SSH+0.1910*data$SI_Chl
data$GMM_HSI <- (data$SI_SST^0.2687)*(data$SI_SSTG^0.0780)*(data$SI_SSH^0.4623)*(data$SI_Chl^0.1910)