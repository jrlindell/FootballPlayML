#preparing data
str(`2013Data`)
levels(Val$Side.of.Field)<-levels(Train$Side.of.Field)
levels(ValTwo$Side.of.Field)->levels(TrainTwo$Side.of.Field)
levels(Final2013data$Side.of.Field)->levels(Final2014data$Side.of.Field)
as.numeric(as.character(Train$Play.Percent.of.Goal))

randomForest(Run~Tm+Opp+Down+ToGo+Side.of.Field+Tm.Score+Opp.Score+Time.Under+Absolute.Score.Differential+Team.Game.Location+Yard.Line+Touchdown.Count+as.numeric(Play.Percent.of.Goal)+First.Down.Count+Goal.To.Go+Success.Count,data=`2013Data`)

#2013 is test set, 2014 is validation set
fosball <- randomForest(Run~Tm+Opp+Down+ToGo+Side.of.Field+Tm.Score+Opp.Score+Time.Under+Absolute.Score.Differential+Team.Game.Location+Yard.Line+Touchdown.Count+First.Down.Count+Goal.To.Go+Success.Count,data=Train)

pred.ball=predict(fosball,Val)
class.football=ifelse(pred.ball>.4209,1,0)
percent<-table(class.football,Val$Run)
(percent[1,1]+percent[2,2])/31943
#68.9%

firstdownval<-Val[ which(Val$Down==1),]
pred.firstdown=predict(fosball,firstdownval)
class.firstdown=ifelse(pred.firstdown>.5,1,0)
percentfirstdown<-table(class.firstdown,firstdownval$Run)
(percentfirstdown[1,1]+percentfirstdown[2,2])/nrow(firstdownval)
#60.3%

seconddownval<-Val[ which(Val$Down==2),]
pred.seconddown=predict(fosball,seconddownval)
class.seconddown=ifelse(pred.seconddown>.5,1,0)
percentseconddown<-table(class.seconddown,seconddownval$Run)
(percentseconddown[1,1]+percentseconddown[2,2])/nrow(seconddownval)
#65.4%

thirddownval<-Val[ which(Val$Down==3),]
pred.thirddown=predict(fosball,thirddownval)
class.thirddown=ifelse(pred.thirddown>.5,1,0)
percentthirddown<-table(class.thirddown,thirddownval$Run)
(percentthirddown[1,1]+percentthirddown[2,2])/nrow(thirddownval)
#82.5%

fourthdownval<-Val[ which(Val$Down==4),]
pred.fourthdown=predict(fosball,fourthdownval)
class.fourthdown=ifelse(pred.fourthdown>.5,1,0)
percentfourthdown<-table(class.fourthdown,fourthdownval$Run)
(percentfourthdown[1,1]+percentfourthdown[2,2])/nrow(fourthdownval)
#79%

Packersval<-Val[ which(Val$Tm=="Packers"),]
pred.Packers=predict(fosball,Packersval)
class.Packers=ifelse(pred.Packers>.5,1,0)
percentPackers<-table(class.Packers,Packersval$Run)
(percentPackers[1,1]+percentPackers[2,2])/nrow(Packersval)

PackersTrain<-Train[ which(Train$Tm=="Packers"),]
fosball.Packers <- randomForest(Run~Tm+Opp+Down+ToGo+Side.of.Field+Tm.Score+Opp.Score+Time.Under+Absolute.Score.Differential+Team.Game.Location+Yard.Line+Touchdown.Count+First.Down.Count+Goal.To.Go+Success.Count,data=PackersTrain)
Packersval<-Val[ which(Val$Tm=="Packers"),]
pred.Packers=predict(fosball.Packers,Packersval)
class.Packers=ifelse(pred.Packers>.5,1,0)
percentPackers<-table(class.Packers,Packersval$Run)
(percentPackers[1,1]+percentPackers[2,2])/nrow(Packersval)


#uses 2014 as training set, 2013 as validation, predicts all play types
PlayPredictor <- randomForest(Play~Tm+Opp+Down+ToGo+Side.of.Field+Tm.Score+Opp.Score+Time.Under+Absolute.Score.Differential+Team.Game.Location+Yard.Line+Touchdown.Count+First.Down.Count+Goal.To.Go+Success.Count,data=TrainTwo)
pred.play=predict(PlayPredictor,ValTwo)
Pred<-table(pred.play,ValTwo$Play)
(Pred[1,1]+Pred[2,2]+Pred[3,3]+Pred[4,4]+Pred[5,5])/nrow(ValTwo)
#45.3%

downval12<-ValTwo[ which(ValTwo$Down==1),]
pred.play1=predict(PlayPredictor,downval12)
Pred1<-table(pred.play1,downval12$Play)
(Pred1[1,1]+Pred1[2,2]+Pred1[3,3]+Pred1[4,4]+Pred1[5,5])/nrow(downval12)
#37.9%

downval22<-ValTwo[ which(ValTwo$Down==2),]
pred.play2=predict(PlayPredictor,downval22)
Pred2<-table(pred.play2,downval22$Play)
(Pred2[1,1]+Pred2[2,2]+Pred2[3,3]+Pred2[4,4]+Pred2[5,5])/nrow(downval22)
#45.8%

downval32<-ValTwo[ which(ValTwo$Down==3),]
pred.play3=predict(PlayPredictor,downval32)
Pred3<-table(pred.play3,downval32$Play)
(Pred3[1,1]+Pred3[2,2]+Pred1[3,3]+Pred3[4,4]+Pred3[5,5])/nrow(downval32)
#58.4%

downval42<-ValTwo[ which(ValTwo$Down==4),]
pred.play4=predict(PlayPredictor,downval42)
Pred4<-table(pred.play4,downval42$Play)
(Pred4[1,1]+Pred4[2,2]+Pred4[3,3]+Pred4[4,4]+Pred4[5,5])/nrow(downval42)
#50.2%

importance(PlayPredictor)


test.err=double(15)
for (m in 1:15){
  fp.rffootball2014 <- randomForest(Run~Tm+Opp+Down+ToGo+Side.of.Field+Tm.Score+Opp.Score+Time.Under+Absolute.Score.Differential+Team.Game.Location+Yard.Line+Touchdown.Count+Goal.To.Go+Success.Count+Interception.Count+Fumble.Count+Sack.Count+Down*ToGo,data=Final2014data,mtry=m)
  pred.rffootball2014=predict(fp.rffootball2014,Final2013data)
  class.rffootball2014=ifelse(pred.rffootball2014>.5,1,0)
  test.football<-table(class.rffootball2014,Final2013data$Run)
  test.err[m] = (test.football[1,1]+test.football[2,2])/nrow(Final2013data)
}

fp.rffootball2014 <- randomForest(Run~Tm+Opp+Down+ToGo+Side.of.Field+Tm.Score+Opp.Score+Time.Under+Absolute.Score.Differential+Team.Game.Location+Yard.Line+Touchdown.Count+Goal.To.Go+Winning+Tied+Interception.Count+Fumble.Count+Sack.Count+Down*ToGo,data=Final2014datav2,mtry=11)
pred.rffootball2014=predict(fp.rffootball2014,Final2013data)
class.rffootball2014=ifelse(pred.rffootball2014>.48,1,0)
test.football<-table(class.rffootball2014,Final2013data$Run)
(test.football[1,1]+test.football[2,2])/nrow(Final2013data)

pred.rffootball2014=predict(fp.rffootball2014,Final2014datav2)
class.rffootball2014=ifelse(pred.rffootball2014>.48,1,0)
test.football<-table(class.rffootball2014,Final2014datav2$Run)
(test.football[1,1]+test.football[2,2])/nrow(Final2014datav2)

fp.rffootballall <- randomForest(Run~Tm+Opp+Down+ToGo+Side.of.Field+Tm.Score+Opp.Score+Time.Under+Absolute.Score.Differential+Team.Game.Location+Yard.Line+Touchdown.Count+Goal.To.Go+Winning+Tied+Interception.Count+Fumble.Count+Sack.Count+Down*ToGo,data=NFLdatav2,mtry=11)
pred.rffootballall=predict(fp.rffootballall,Final2014datav2)
class.rffootballall=ifelse(pred.rffootballall>.48,1,0)
test.footballall<-table(class.rffootballall,Final2014datav2$Run)
(test.footballall[1,1]+test.footballall[2,2])/nrow(Final2014datav2)

pred.rffootballall=predict(fp.rffootballall,Final2013data)
class.rffootballall=ifelse(pred.rffootballall>.48,1,0)
test.footballall<-table(class.rffootballall,Final2013data$Run)
(test.footballall[1,1]+test.footballall[2,2])/nrow(Final2013data)


rfnflfinal <- randomForest(Run~Tm+Opp+Down+ToGo+Side.of.Field+Tm.Score+Opp.Score+Time.Under+Absolute.Score.Differential+Team.Game.Location+Yard.Line+Touchdown.Count+Goal.To.Go+Winning+Tied+Interception.Count+Fumble.Count.1+Sack.Count+Down*ToGo,data=FinalNflDatav2,mtry=11)
pred.rfnflfinal=predict(rfnflfinal,FinalNflDatav2)
class.rfnflfinal=ifelse(pred.rfnflfinal>.48,1,0)
test.rfnflfinal<-table(class.rfnflfinal,FinalNflDatav2$Run)

Result<-rbind(FinalNflDatav2,Probs)


pred.rfnflfinal=predict(rfnflfinal,FinalNflDatav2[2,])
class.rfnflfinal=ifelse(pred.rfnflfinal>.48,1,0)
test.rfnflfinal<-table(class.rfnflfinal,FinalNflDatav2$Run)


#plots
plot(yardplot)
plot(rfnflfinal,log="y")
varImpPlot(rfnflfinal)





FinalNflDatav2<-rbind(Final2013[,c("Run","Tm","Opp","Down","ToGo","Side.of.Field","Tm.Score","Opp.Score","Time.Under","Absolute.Score.Differential","Team.Game.Location","Yard.Line","Touchdown.Count","First.Down.Count","Goal.To.Go","Success.Count","Interception.Count","Fumble.Count.1","Sack.Count","Winning","Tied")],Final2014[,c("Run","Tm","Opp","Down","ToGo","Side.of.Field","Tm.Score","Opp.Score","Time.Under","Absolute.Score.Differential","Team.Game.Location","Yard.Line","Touchdown.Count","First.Down.Count","Goal.To.Go","Success.Count","Interception.Count","Fumble.Count.1","Sack.Count","Winning","Tied")])