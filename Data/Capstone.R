Train2 = read.csv("C:/Users/C16Jacob.Lindell/Desktop/School/Econ 465/Final Project/Data/HW4 2013 NFL Play-by-Play Data.csv")

amlm <- step(lm(Run~., data = Train2), direction = "both")
summary(amlm)$coef



lm<-lm(Run~., data = Train2)
summary(lm)$coef

##HW 10
glm.fit<-glm(Run~.,data=Train2,family=binomial)
summary(glm.fit)


amlm <- step(lm(Run~-Tm-Opp, data = Train2), direction = "both")
summary(amlm)$coef



amlm <- step(lm(Run~Down + ToGo + Side.of.Field + Tm.Score + Opp.Score + 
                  Time.Under + Absolute.Score.Differential + Team.Game.Location + 
                  Yard.Line + Touchdown.Count + Play.Percent.of.Goal + First.Down.Count + 
                  Goal.To.Go + Success.Count + Pass + Run.Left + Run.Middle + 
                  Run.Right + Short, data = Train2), direction = "both")
summary(amlm)$coef
