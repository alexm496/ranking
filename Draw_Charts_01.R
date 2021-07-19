#font_add_google("EB Garamond")

showtext_auto()
curveDF1 <- data.frame(SkillDifference=25*(-24:24),f="Logistic")
curveDF2 <- data.frame(SkillDifference=25*(-24:24),f="Normal CDF")
c <- log(10)/400
curveDF1$Probability <- 1/(1+exp(-c*curveDF1$SkillDifference))
curveDF2$Probability <- pnorm(curveDF2$SkillDifference,0,sqrt(pi^2/(3*c^2)))

curveDF <- rbind(curveDF1,curveDF2)
myPlot <- ggplot(data=curveDF,aes(x=SkillDifference,y=Probability,col=f))+
  geom_line()+scale_color_manual(values=c("navy","gold"))+
  theme_classic()+theme(text=element_text(size=35,  family="EB Garamond"))

myPlot