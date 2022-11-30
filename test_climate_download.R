




test <- download_chelsa_trace21k(save.location = here::here(), 
                                 parameter = "bio", 
                                 model =  "CHELSA_TraCE21k", 
                                 bio.var = c(1,5), 
                                 time.var = c(20:18)
)



bio.var.wanted = c(1, 6, 12, 15, 18, 19)
time.var.wanted = c(20:-200)
parameter.choosen = c("bio", "tasmin")

paleoclimate <- download_chelsa_trace21k(save.location = here::here(), 
                                 parameter = parameter.choosen, 
                                 model =  "CHELSA_TraCE21k", 
                                 bio.var = bio.var.wanted, 
                                 time.var = time.var.wanted 
)