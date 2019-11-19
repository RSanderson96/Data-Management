library(ProjectTemplate)
load.project()


Leaving_SurveyDF = data.frame(Reasons = unique(cyber.security.7.leaving.survey.responses$leaving_reason, incomparables= FALSE))

Reasons.Function = function(x){
LeavingSurvey = x
R = unique(LeavingSurvey$leaving_reason, incomparables = FALSE)
L = length(R)

Reasons_Left = vector()
for(i in 1:L){
Reasons = LeavingSurvey%>% filter(leaving_reason == R[i])
Total = nrow(Reasons)
Reasons_Left[i] = Total}

return(Reasons_Left)
}

Reasons4 =Reasons.Function(cyber.security.4.leaving.survey.responses)
Reasons5 =Reasons.Function(cyber.security.5.leaving.survey.responses)
Reasons6 =Reasons.Function(cyber.security.6.leaving.survey.responses)
Reasons7 =Reasons.Function(cyber.security.7.leaving.survey.responses)

Combined_Leaving_Survey = cbind(Leaving_SurveyDF, Reasons4, Reasons5, Reasons6, Reasons7)

Total = c(sum(Reasons4), sum(Reasons5), sum(Reasons6), sum(Reasons7))


Combined_Leaving_SurveyTotal = rbind(Combined_Leaving_Survey, Total )
Combined_Leaving_SurveyTotal$Reasons = c("Other", 
                                    "The Course required more time Than I Realised", 
                                    "I prefer not to say"," I don't have enough time",
                                    "The Course was too hard", "The Course wasn't what I expected", 
                                    "The Course won't help me reach my goals", "The Course was too easy", "Total")

One = ggplot(data = Combined_Leaving_Survey, aes(x = Reasons, y = Total))
OneGraph = One + geom_col (aes(x = Reasons, y = Reasons4))
OneGraph

Two = ggplot(data = Combined_Leaving_Survey, aes(x = Reasons, y = Total))
TwoGraph = Two + geom_col (aes(x = Reasons, y = Reasons5))
TwoGraph

Three = ggplot(data = Combined_Leaving_Survey, aes(x = Reasons, y = Total))
ThreeGraph = Three + geom_col (aes(x = Reasons, y = Reasons6))
OneGraph

Four = ggplot(data = Combined_Leaving_Survey, aes(x = Reasons, y = Total))
FourGraph = Four + geom_col (aes(x = Reasons, y = Reasons7))
FourGraph
