library(data.table)



# Data read --------------------------------------------------------------------
dropshop <- fread("data/compiled_zwiftinsider - dropshop.csv")
speedtest <- fread("data/compiled_zwiftinsider - speedtest.csv")

# - Overwrite model with preferred name (as per manufacturer)
dropshop[pref_name!="", model:=pref_name]

# - Join datasets
main <- speedtest[, -c("test_part")][dropshop, on="item"]




# Get Speed data relative to baseline (zwift aero + 32mm) ----------------------
base_tempus <- main[component=="frame" & make=="Zwift" & model=="Aero", tempus]
base_alp    <- main[component=="frame" & make=="Zwift" & model=="Aero", alp]

main[, tempus:=tempus-base_tempus]
main[, alp:=alp-base_alp]



# Split and cross datasets -----------------------------------------------------
frames <- main[!is.na(tempus) & !is.na(alp) & component=="frame" & grepl("road|tt", class),
               .("frame"=paste(make, model), class, "drops_f"=drops, "level_f"=level, 
                 "tempus_f"=tempus, "alp_f"=alp)]

wheels <- main[!is.na(tempus) & !is.na(alp) & component=="wheel" & class=="road",
               .("wheel"=paste(make, model), "drops_w"=drops, "level_w"=level, 
                 "tempus_w"=tempus, "alp_w"=alp)]

bikes <- setkey(wheels[,c(k=1,.SD)],k)[frames[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL][,
           .(class, "drops"=drops_f+drops_w, "level"=max(level_f, level_w), 
             "tempus"=tempus_f+tempus_w, "alp"=alp_f+alp_w), by=.(frame, wheel)]


# - Add base times back on to get total time
bikes[, tempus:=tempus+base_tempus]
bikes[, alp:=alp+base_alp]



# Add TRON ---------------------------------------------------------------------
bikes <- rbind(bikes, data.table(frame="TRON", wheel="", class="road", 
                                 drops=0, level=0, tempus=3027, alp=2938))



# Write out data ---------------------------------------------------------------
fwrite(bikes, "data/speed_data.txt", sep="\t")


