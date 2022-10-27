######################################################################################################################### 
##AUTHOR: HARSA YUNILMAN
#########################################################################################################################

## Extract payroll data
data <- read.csv("payroll_pekerja_bebas.csv")

## Some function and data clean-ups
options(scipen = 999)



######################################################################################################################### 
##MANUAL INPUT
######################################################################################################################### 


## Set tax rates
kurung_pajak <- c(50000000,250000000, 500000000, 5000000000)
tarif_pajak <- c(0.05, 0.15, 0.25, 0.30, 0.35)
ptkp_tahun <- c(54000000, 4500000, 4500000)
ptkp_bulan <- c(4500000, 375000, 375000)
ptkp_harian <- c(150000, 12500, 12500)


kurung_bebas <- c(450000,4500000,10200000)

## Set tax classifications and period
masa_pajak <- 9
kode_objek_pajak <- "21-100-03"
pembetulan <- 0
tahun_pajak <- 2016

month_end <- as.Date("2016-09-30")
year_start <- as.Date("2016-01-31")
year_end <- as.Date("2016-12-31")



######################################################################################################################### 
##PENGHITUNGAN PPH 21 YANG HARUS DIPOTONG
######################################################################################################################### 

## Calculate average wage per day

wages_month <- data$JUMLAH.KUMULATIF.UPAH

work_days <- data$JUMLAH.HARI.KERJA

wages_day <- c(wages_month / work_days)


## Determine the tax bracket based on total wage for the month and average wage per day

tax_bracket <- ifelse(data$MASA.PEMBAYARAN == "Bulanan", 4, 
                      ifelse(wages_day <= kurung_bebas[1] & wages_month <= kurung_bebas[2], 1,
                             ifelse(wages_day > kurung_bebas[1] & wages_month <= kurung_bebas[2], 2,
                                    ifelse(wages_month <= kurung_bebas[3], 3, 4)
                                    )
                             )
                      )

## Based on tax bracket, calculate the tax base

penghasilan_bruto <- ifelse(tax_bracket == 4, wages_month * 12, wages_month)


deductible_amount_1 <- ifelse(tax_bracket == 1, penghasilan_bruto, 0)

deductible_amount_2 <- ifelse(tax_bracket == 2, kurung_bebas[1] * work_days, 0)

deductible_amount_3 <- ifelse(tax_bracket == 3, 
                             ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="TK", ptkp_harian[1] * work_days, 0) +
                             ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="K", (ptkp_harian[1]+ptkp_harian[2]) * work_days, 0) +
                             ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="HB", ptkp_harian[1] * work_days, 0) +
                               
                             ifelse(data$JENIS.KELAMIN=="W" & data$STATUS.KAWIN=="TK", ptkp_harian[1] * work_days, 0) +
                             ifelse(data$JENIS.KELAMIN=="W" & data$STATUS.KAWIN=="K" & data$STATUS.KELUARGA=="", ptkp_harian[1] * work_days, 0) +
                             ifelse(data$JENIS.KELAMIN=="W" & data$STATUS.KAWIN=="K" & data$STATUS.KELUARGA=="STK", (ptkp_harian[1]+ptkp_harian[2]) * work_days, 0) +
                             ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="HB", ptkp_harian[1] * work_days, 0) +
                               
                             ptkp_harian[3] * work_days * data$STATUS...JUMLAH.TANGGUNGAN.KELUARGA.UNTUK.PTKP, 0
                             )

deductible_amount_4 <- ifelse(tax_bracket == 4,
                              ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="TK", ptkp_tahun[1], 0) +
                              ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="K", ptkp_tahun[1]+ptkp_tahun[2], 0) +
                              ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="HB", ptkp_tahun[1], 0) +
                                
                              ifelse(data$JENIS.KELAMIN=="W" & data$STATUS.KAWIN=="TK", ptkp_tahun[1], 0) +
                              ifelse(data$JENIS.KELAMIN=="W" & data$STATUS.KAWIN=="K" & data$STATUS.KELUARGA=="", ptkp_tahun[1], 0) +
                              ifelse(data$JENIS.KELAMIN=="W" & data$STATUS.KAWIN=="K" & data$STATUS.KELUARGA=="STK", ptkp_tahun[1]+ptkp_tahun[2], 0) +
                              ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="HB", ptkp_tahun[1], 0) +
                                
                              ptkp_tahun[3]*data$STATUS...JUMLAH.TANGGUNGAN.KELUARGA.UNTUK.PTKP, 0
                              )

deductible_amount_total <- c(deductible_amount_1 + 
                             deductible_amount_2 + 
                             deductible_amount_3 + 
                             deductible_amount_4 +
                             data$BIAYA.WAJIB
                             )

tax_base <- c(penghasilan_bruto - deductible_amount_total)


pph_total <- ifelse(tax_bracket == 1, 0, 
                    ifelse(tax_bracket == 2, tarif_pajak[1] * tax_base,
                           ifelse(tax_bracket == 3, tarif_pajak[1] * tax_base,
                                         ifelse(tax_base > kurung_pajak[4], 
                                                
                                                kurung_pajak[1]*tarif_pajak[1]/12+
                                                  (kurung_pajak[2] - kurung_pajak[1])*tarif_pajak[2]/12 + 
                                                  (kurung_pajak[3]-kurung_pajak[2])*tarif_pajak[3]/12 + 
                                                  (kurung_pajak[4]-kurung_pajak[3])*tarif_pajak[4]/12 + 
                                                  (tax_base - kurung_pajak[4])*tarif_pajak[5]/12,
                                                
                                                ifelse(tax_base > kurung_pajak[3],
                                                       
                                                       kurung_pajak[1]*tarif_pajak[1]/12+
                                                         (kurung_pajak[2] - kurung_pajak[1])*tarif_pajak[2]/12 + 
                                                         (kurung_pajak[3]-kurung_pajak[2])*tarif_pajak[3]/12 + 
                                                         (tax_base - kurung_pajak[3])*tarif_pajak[4]/12,                   
                                                       
                                                       ifelse(tax_base > kurung_pajak[2], 
                                                              
                                                              kurung_pajak[1]*tarif_pajak[1]/12+
                                                                (kurung_pajak[2] - kurung_pajak[1])*tarif_pajak[2]/12 + 
                                                                (tax_base - kurung_pajak[2])*tarif_pajak[3]/12,                           
                                                              
                                                              ifelse(tax_base > kurung_pajak[1], 
                                                                     
                                                                     kurung_pajak[1]*tarif_pajak[1]/12+
                                                                       (tax_base - kurung_pajak[1])*tarif_pajak[2]/12, 
                                                                     
                                                                     tax_base*tarif_pajak[1]/12
                                                                     )
                                                              )
                                                       )
                                                )
                                  )
                           )
                    )

pph_total_npwp <- ifelse (data$NPWP=="", 1.2 * pph_total, pph_total)

pph_terutang <- c(pph_total_npwp - data$PPh.PASAL.21.YANG.TELAH.DIPOTONG.DI.MASA.PAJAK.YANG.SAMA)



######################################################################################################################### 
##REPORT
#########################################################################################################################

npwp_status <- ifelse(data$NPWP == "", "Ya", "Tidak")

tax_highest_rate <- ifelse(tax_bracket == 1, NA,
                       ifelse(tax_bracket == 2, tarif_pajak[1],
                              ifelse(tax_bracket == 3, tarif_pajak[1],
                                  ifelse(tax_base > kurung_pajak[4], tarif_pajak[5],
                                     ifelse(tax_base > kurung_pajak[3], tarif_pajak[4],
                                            ifelse(tax_base > kurung_pajak[2], tarif_pajak[3],
                                                   ifelse(tax_base > kurung_pajak[1], tarif_pajak[2],
                                                          tarif_pajak[1])
                                                   )
                                            )
                                        )
                                     )
                              )
                       )

pekerja_bebas <-  data.frame(masa_pajak, 
                              tahun_pajak, 
                              pembetulan, 
                              data$NPWP, 
                              data$NAMA, 
                              kode_objek_pajak, 
                              penghasilan_bruto, 
                              npwp_status,
                              tax_highest_rate,
                              pph_terutang, 
                              data$KODE.NEGARA.DOMISILI
                              )


pekerja_bebas <- setNames(pekerja_bebas, c("Masa Pajak",
                                          "Tahun Pajak",
                                          "Pembetulan", 
                                          "NPWP", 
                                          "Nama", 
                                          "Kode Pajak", 
                                          "Jumlah Bruto", 
                                          "Tidak Ber-NPWP",
                                          "Tarif",
                                          "Jumlah PPh", 
                                          "Kode Negara"
                                          )
                          )
