#How many times did an ID not complete a step?

#How many times did an ID not complete a step?

LeavingResponses.function = function(x,y) {
  #List the incomplete learner IDs
  Incomplete_Quantity= x %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)
  
  library(ProjectTemplate)
  #Are these learner IDs in the leaving response file?
  
  LeaversDF = merge(y, Incomplete_Quantity, by = "learner_id", all=TRUE)
  
  #how many have a reason and a not completed?
  
  LeaversDF[is.na(LeaversDF)] <- 0 #beware warnings will appear!
  
  Both = LeaversDF %>% filter(n != 0) %>% filter(id != 0 ) %>% arrange (-n)
  NnotR = LeaversDF %>% filter(n != 0) %>% filter (id == "0") %>% arrange(-n)
  RnotN = LeaversDF %>% filter(n=="0") %>% filter (id != "0") 
  
  ReasonAndIncomplete = nrow(Both)
  IncompleteNoReason = nrow(NnotR)
  ReasonNotIncomplete = nrow(RnotN)
  
  Incomplete = c(nrow(Both),nrow(NnotR), nrow(RnotN))
  
  return(Incomplete)}

Incomplete4 = LeavingResponses.function(cyber.security.4.step.activity, cyber.security.4.leaving.survey.responses)
Incomplete5 = LeavingResponses.function(cyber.security.5.step.activity, cyber.security.5.leaving.survey.responses)
Incomplete6 = LeavingResponses.function(cyber.security.6.step.activity, cyber.security.6.leaving.survey.responses)
Incomplete7 = LeavingResponses.function(cyber.security.7.step.activity, cyber.security.7.leaving.survey.responses)
cache("Incomplete4")
cache("Incomplete5")
cache("Incomplete6")
cache("Incomplete7")

DF = data.frame(Both = Incomplete4 [1], Incomplete_No_Reason = Incomplete4 [2], Reason_Not_Incomplete = Incomplete4 [3])
IncompleteReasonsDF = rbind(DF,Incomplete5, Incomplete6, Incomplete7)
IncompleteReasonsDF = cbind(c(4:7), IncompleteReasonsDF)
IncompleteReasonsDF = IncompleteReasonsDF%>% rename(Course_Run = "c(4:7)")
Total = c(sum(Reasons4), sum(Reasons5), sum(Reasons6), sum(Reasons7))
Total_Leavers = data.frame(Course_run = c(4:7), Total = Total )
IncompleteReasonsDF = cbind(IncompleteReasonsDF, Total_Leavers$Total )
IncompleteReasonsDF = IncompleteReasonsDF%>% rename(Total_Survey_Responses = "Total_Leavers$Total")
IncompleteReasonsDF = cbind(IncompleteReasonsDF, c(nrow(Incomplete_Quantity4), nrow(Incomplete_Quantity5),nrow(Incomplete_Quantity6),
                                                   nrow(Incomplete_Quantity7)))
IncompleteReasonsDF = IncompleteReasonsDF = IncompleteReasonsDF%>% rename(Total_Unfinished = "c(nrow(Incomplete_Quantity4), nrow(Incomplete_Quantity5), nrow(Incomplete_Quantity6), ")

cache("IncompleteReasonsDF")

