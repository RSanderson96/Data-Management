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

Combined_Leaving_Survey = rbind(Combined_Leaving_Survey, Total )


