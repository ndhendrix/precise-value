make_test_pattern()
make_treat_prob()
make_age_pattern()

source(here("src", "precise_value_base_values.R"))
source(here("src", "precise_value_r_model.R"))
results_list <- precise_value()

outcomes <- results_list[[1]]
CEA_results_discounted<-results_list[[2]]
CEA_results_undiscounted<-results_list[[3]]
ADE_results_clo<-results_list[[4]]
ADE_results_war<-results_list[[5]]

#####the number of alerts#####
##(1) total # of alerts
n_alerts<-sum(outcomes$clo_n_alert+outcomes$war_n_alert)
n_alerts

##(2) # of alerts for clo
n_alerts_clo<-sum(outcomes$clo_n_alert)
n_alerts_clo

##(3) # of alerts for war
n_alerts_war<-sum(outcomes$war_n_alert)
n_alerts_war

#####CEA results, undiscounted#####
#(1) costs
sum(CEA_results_undiscounted$no_alert_cost)
sum(CEA_results_undiscounted$alert_cost_total)

#(2) QALYs
sum(CEA_results_undiscounted$no_alert_qaly)
sum(CEA_results_undiscounted$alert_qaly)

#(3) Inc costs
inc_cost_undiscounted<-round(sum(CEA_results_undiscounted$alert_cost_total)-sum(CEA_results_undiscounted$no_alert_cost),2)
inc_cost_undiscounted

#(4) Inc QALYs
inc_qaly_undiscounted<-round(sum(CEA_results_undiscounted$alert_qaly)-sum(CEA_results_undiscounted$no_alert_qaly),2)
inc_qaly_undiscounted

#(5) ICER
ICER_undiscounted<-inc_cost_undiscounted/inc_qaly_undiscounted
ICER_undiscounted

#####CEA results, discounted#####
sum(CEA_results_discounted$alert_n)

#(1) costs
sum(CEA_results_discounted$no_alert_cost)
sum(CEA_results_discounted$alert_cost_total)

#(2) QALYs
sum(CEA_results_discounted$no_alert_qaly)
sum(CEA_results_discounted$alert_qaly)

#(3) Inc costs
inc_cost_discounted<-round(sum(CEA_results_discounted$alert_cost_total)-sum(CEA_results_discounted$no_alert_cost),2)
inc_cost_discounted

#(4) Inc QALYs
inc_qaly_discounted<-round(sum(CEA_results_discounted$alert_qaly)-sum(CEA_results_discounted$no_alert_qaly),5)
inc_qaly_discounted

#(5) ICER
ICER_discounted<-(sum(CEA_results_discounted$alert_cost_total)-sum(CEA_results_discounted$no_alert_cost))/(sum(CEA_results_discounted$alert_qaly)-sum(CEA_results_discounted$no_alert_qaly))
ICER_discounted


#####Costs per alerts#####
#(1) total costs
inc_cost_undiscounted/n_alerts
inc_cost_discounted/n_alerts

#(2) medical costs
inc_med_cost_undiscounted<-sum(CEA_results_undiscounted$alert_cost_medical)-sum(CEA_results_undiscounted$no_alert_cost)
inc_med_cost_undiscounted

inc_med_cost_discounted<-sum(CEA_results_discounted$alert_cost_medical)-sum(CEA_results_discounted$no_alert_cost)
inc_med_cost_discounted

inc_med_cost_undiscounted/n_alerts
inc_med_cost_discounted/n_alerts

#(3) admin costs
inc_adm_cost_undiscounted<-sum(CEA_results_undiscounted$alert_cost_admin)
inc_adm_cost_undiscounted

inc_adm_cost_discounted<-sum(CEA_results_discounted$alert_cost_admin)
inc_adm_cost_discounted

inc_adm_cost_undiscounted/n_alerts
inc_adm_cost_discounted/n_alerts


#####Inc. costs per QALY#####
##(1) inc. medical costs per QALY
inc_med_cost_undiscounted/inc_qaly_undiscounted
inc_med_cost_discounted/inc_qaly_discounted

##(2) inc. admin costs per QALY
inc_adm_cost_undiscounted/inc_qaly_undiscounted
inc_adm_cost_discounted/inc_qaly_discounted



#####ADE clopidogrel#####
ADE_result_clo_report<-data.frame(matrix(nrow=3,ncol=11))
colnames(ADE_result_clo_report)<-c("Intervention","Non-fatal MI","stent thrombosis","Nonfatal intracranial",
                                   "Nonfatal extracranial","CABG bleeding","minor bleeding",
                                   "CABG revascularization","PCI revascularization","CV death","non CV death")
ADE_result_clo_report[,1]<-c("No Alerts","Alerts","Incremental")

#(1) Non-fatal MI
ADE_result_clo_report[1,2]<-round(ADE_results_clo[,1],3)
ADE_result_clo_report[2,2]<-round(ADE_results_clo[,2],3)
ADE_result_clo_report[3,2]<-round(ADE_results_clo[,2],3)-round(ADE_results_clo[,1],3)

#(2) stent thrombosis
ADE_result_clo_report[1,3]<-round(ADE_results_clo[,3],3)
ADE_result_clo_report[2,3]<-round(ADE_results_clo[,4],3)
ADE_result_clo_report[3,3]<-round(ADE_results_clo[,4],3)-round(ADE_results_clo[,3],3)

#(3) Nonfatal intracranial
ADE_result_clo_report[1,4]<-round(ADE_results_clo[,5],3)
ADE_result_clo_report[2,4]<-round(ADE_results_clo[,6],3)
ADE_result_clo_report[3,4]<-round(ADE_results_clo[,6],3)-round(ADE_results_clo[,5],3)

#(4) Nonfatal extracranial
ADE_result_clo_report[1,5]<-round(ADE_results_clo[,7],3)
ADE_result_clo_report[2,5]<-round(ADE_results_clo[,8],3)
ADE_result_clo_report[3,5]<-round(ADE_results_clo[,8],3)-round(ADE_results_clo[,7],3)

#(5) CABG bleeding
ADE_result_clo_report[1,6]<-round(ADE_results_clo[,9],3)
ADE_result_clo_report[2,6]<-round(ADE_results_clo[,10],3)
ADE_result_clo_report[3,6]<-round(ADE_results_clo[,10],3)-round(ADE_results_clo[,9],3)

#(6) minor bleeding
ADE_result_clo_report[1,7]<-round(ADE_results_clo[,11],3)
ADE_result_clo_report[2,7]<-round(ADE_results_clo[,12],3)
ADE_result_clo_report[3,7]<-round(ADE_results_clo[,12],3)-round(ADE_results_clo[,11],3)

#(7) CABG revascularization
ADE_result_clo_report[1,8]<-round(ADE_results_clo[,13],3)
ADE_result_clo_report[2,8]<-round(ADE_results_clo[,14],3)
ADE_result_clo_report[3,8]<-round(ADE_results_clo[,14],3)-round(ADE_results_clo[,13],3)

#(8) PCI revascularization
ADE_result_clo_report[1,9]<-round(ADE_results_clo[,15],3)
ADE_result_clo_report[2,9]<-round(ADE_results_clo[,16],3)
ADE_result_clo_report[3,9]<-round(ADE_results_clo[,16],3)-round(ADE_results_clo[,15],3)

#(9) CV death
ADE_result_clo_report[1,10]<-round(ADE_results_clo[,17],3)
ADE_result_clo_report[2,10]<-round(ADE_results_clo[,18],3)
ADE_result_clo_report[3,10]<-round(ADE_results_clo[,18],3)-round(ADE_results_clo[,17],3)

#(10) non CV death
ADE_result_clo_report[1,11]<-round(ADE_results_clo[,19],3)
ADE_result_clo_report[2,11]<-round(ADE_results_clo[,20],3)
ADE_result_clo_report[3,11]<-round(ADE_results_clo[,20],3)-round(ADE_results_clo[,19],3)

#####ADE warfarin#####
#(1) Bleeding
round(ADE_results_war[,1],3)
round(ADE_results_war[,2],3)
round(ADE_results_war[,2],3)-round(ADE_results_war[,1],3)

#(2) clot
round(ADE_results_war[,3],3)
round(ADE_results_war[,4],3)
round(ADE_results_war[,4],3)-round(ADE_results_war[,3],3)

