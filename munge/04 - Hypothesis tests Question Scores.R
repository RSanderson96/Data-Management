#Making the dataframe
Results.function = function(y){
Answers= y %>% filter(correct == "true") %>% count(learner_id) %>% arrange(-n)
#how many correct answers for each learner ID?
Attempts =y %>% count(learner_id) %>% arrange(-n)
#How many attempts for each learner ID?
Results = merge(Answers, Attempts, by = "learner_id", all = TRUE)
Results = Results[rowSums(is.na(Results)) == 0,]
Results = Results%>% rename(Answers = n.x, Attempts = n.y )
Score = ((Results$Answers)/(Results$Attempts)*100)
Results = cbind(Results, Score)
Results = Results[-c(1), ]
return(Results)}

Results1 = Results.function(cyber.security.1.question.response)
Results2 = Results.function(cyber.security.2.question.response)
Results3 = Results.function(cyber.security.3.question.response)
Results4 = Results.function(cyber.security.4.question.response)
Results5 = Results.function(cyber.security.5.question.response)
Results6 = Results.function(cyber.security.6.question.response)
Results7 = Results.function(cyber.security.7.question.response)

cache("Results1")
cache("Results2")
cache("Results3")
cache("Results4")
cache("Results5")
cache("Results6")
cache("Results7")

MeanScore = c((mean(Results1$Score)), (mean(Results2$Score)),
              (mean(Results3$Score)), (mean(Results4$Score)),
              (mean(Results5$Score)), (mean(Results6$Score)),
              (mean(Results7$Score)))
cache("MeanScore")

Standard_Deviation = c((sd(Results1$Score)), (sd(Results2$Score)),
                       (sd(Results3$Score)), (sd(Results4$Score)),
                       (sd(Results5$Score)), (sd(Results6$Score)),
                       (sd(Results7$Score)))
cache("Standard_Deviation")

N = c((nrow(Results1)), (nrow(Results2)),
        (nrow(Results3)), (nrow(Results4)),
        (nrow(Results5)), (nrow(Results6)),
        (nrow(Results7)))
cache("N")

Resultsdf = data.frame(Course = c(1:7), n = N, Average = MeanScore, SD = Standard_Deviation)
cache("Resultsdf")


#Testing - Ho: A2=A3, H1: A2!=A3
#incomplete data -hard decision to work out what to test! Varied number of attempts
#n for all courses >30 - apply CLT
#mean score for A2 = 63.28 - how do the other courses vary?
sqrt(1027)
Course3Pvalue = pnorm(63.46, mean = 63.28, sd = 16.56791/sqrt(1027))
Course4Pvalue = pnorm(60.89, mean = 63.28, sd = 17.213841/sqrt(1140))
Course5Pvalue = pnorm(62.39, mean = 63.28, sd = 16.80533/sqrt(1122))
Course6Pvalue = pnorm(62.37, mean = 63.28, sd = 17.35376/sqrt(683))
Course7Pvalue = pnorm(63.19, mean = 63.28, sd = 17.15847/sqrt(616))

Pvalues = data.frame("3" = Course3Pvalue, "4" = Course4Pvalue, 
                     "5" = Course5Pvalue, "6" = Course6Pvalue,
                     "7" = Course7Pvalue)

cache("Pvalues")
#all values fall very comfortably within a 90% Hypothesis test - see limited impact of videos on the overall score for any course run


(60.89-63.28)/(17.21/sqrt(1140))
alpha = .025
z.alpha = qnorm(1−alpha)
−z.alpha               # critical value 
Test_Stat = c(z.alpha, -z.alpha)
Test_Stat



pnorm(97.95, 100, (6.19/sqrt(30)))