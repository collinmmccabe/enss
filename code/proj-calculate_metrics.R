emp_el <- readRDS("./data/Effective_el_emp")
emp_el <- list(emp_el[[1]], emp_el[[3]], emp_el[[8]], emp_el[[9]], emp_el[[10]], emp_el[[12]], emp_el[[14]], emp_el[[16]], emp_el[[20]], emp_el[[21]], emp_el[[22]], emp_el[[23]], emp_el[[30]], emp_el[[26]], emp_el[[29]], emp_el[[13]], emp_el[[34]], emp_el[[38]], emp_el[[39]], emp_el[[41]])

emp_el_w <- readRDS("./data/Effective_el_emp_w")
emp_el_w <- list(emp_el_w[[1]], emp_el_w[[3]], emp_el_w[[8]], emp_el_w[[9]], emp_el_w[[10]], emp_el_w[[12]], emp_el_w[[14]], emp_el_w[[16]], emp_el_w[[20]], emp_el_w[[21]], emp_el_w[[22]], emp_el_w[[23]], emp_el_w[[30]], emp_el_w[[26]], emp_el_w[[29]], emp_el_w[[13]], emp_el_w[[34]], emp_el_w[[38]], emp_el_w[[39]], emp_el_w[[41]])

unweighted_metrics <- calculate_metrics(emp_el)
weighted_metrics <- calculate_metrics(emp_el_w)

ens_SI <- c(7, 36, 20, 9, 11, 43, 13, 16, 16, 31, 36, 20, 34, 25, 11, 10, 24, 10, 9, 15)
ens_SIR <- c(8, 22, 18, 10, 13, 21, 13, 16, 17, 26, 26, 15, 35, 22, 13, 11, 21, 12, 10, 12)
ens_SI_w <- c(9, 75, 43, 9, 15, 57, 43, 20, 20, 53, 79, 70, 37, 32, 16, 12, 27, 16, 10, 16)
ens_SIR_w <- c(7, 23, 18, 9, 12, 26, 13, 15, 16, 26, 28, 17, 30, 22, 12, 9, 18, 11, 9, 10)

SI_AICc <- AICc_ens_metrics(ens_SI, unweighted_metrics)
SIR_AICc <- AICc_ens_metrics(ens_SIR, unweighted_metrics)
SI_w_AICc <- AICc_ens_metrics(ens_SI_w, weighted_metrics)
SIR_w_AICc <- AICc_ens_metrics(ens_SIR_w, weighted_metrics)
