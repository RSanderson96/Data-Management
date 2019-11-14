library(ProjectTemplate)
load.project()

#How many times did an ID not complete a step?

LeavingResponses.function = function(x,y) {

Incomplete_Quantity= x %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)

#Are these learner IDs in the leaving response file?

LeaversDF = merge(y, Incomplete_Quantity, by = "learner_id", all=TRUE)

#how many have a reason and a not completed?

LeaversDF[is.na(LeaversDF)] <- 0 #beware warnings will appear!

Both = LeaversDF %>% filter(n != "") %>% filter(id != "" ) %>% arrange (-n)
NnotR = LeaversDF %>% filter(n != "") %>% filter (id == "0") %>% arrange(-n)
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

DF = data.frame(Both = Incomplete4 [1], Incomplete_No_Reason = Incomplete4 [2], Reason_Not_Incomplete = Incomplete4 [3])
IncompleteDF = rbind(DF,Incomplete5, Incomplete6, Incomplete7)
                                                 