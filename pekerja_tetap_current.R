######################################################################################################################### 
##AUTHOR: HARSA YUNILMAN
#########################################################################################################################

## Extract payroll data
data <- read.csv("payroll_pekerja_tetap_current.csv")
data <- transform(data, "TANGGAL.MULAI.BEKERJA" = as.Date(data[,1]))
data <- transform(data, "AWAL.MASA.PENSIUN" = as.Date(data[,2]))

## Some function and data clean-ups
options(scipen = 999)

elapsed_months <- function(end_date, start_date) { 
                      ed <- as.POSIXlt(end_date)
                      sd <- as.POSIXlt(start_date)
                      12 * (ed$year - sd$year) + (ed$mon - sd$mon)
                  }


######################################################################################################################### 
##MANUAL INPUT
######################################################################################################################### 


## Set tax rates
kurung_pajak <- c(50000000,250000000, 500000000, 5000000000)
tarif_pajak <- c(0.05, 0.15, 0.25, 0.30, 0.35)
ptkp <- c(54000000, 4500000, 4500000)
jabatan_tarif <- c(0.05,6000000)
pensiun_tarif <- c(0.05,2400000)

## Set tax classifications and period
masa_pajak <- 9
kode_objek_pajak <- "21-100-01"
pembetulan <- 0
tahun_pajak <- 2016

month_end <- as.Date("2016-09-30")
year_start <- as.Date("2016-01-31")
year_end <- as.Date("2016-12-31")



######################################################################################################################### 
##PENGHITUNGAN PPH 21 YANG HARUS DIPOTONG ATAS PENGHASILAN TERATUR
######################################################################################################################### 


## Calculate the number of months the employee has worked since the beginning of tax year
months_working <- ifelse( data$TANGGAL.MULAI.BEKERJA > year_start, 
                          elapsed_months(month_end, data$TANGGAL.MULAI.BEKERJA)+1, 
                          elapsed_months(month_end, year_start)+1 )


## Penghasilan bruto teratur for the month
bruto_teratur_sebulan <- c(data$GAJI.PENSIUN.ATAU.THT.JHT 
                      + data$TUNJANGAN.PPh 
                      + data$TUNJANGAN.LAINNYA 
                      + data$OVERTIME 
                      + data$JAMINAN.KECELAKAAN.KERJA..JKK.
                      + data$JAMINAN.KEMATIAN..JKM.
                      + data$JAMINAN.PEMELIHARAAN.KESEHATAN..JPK.
                      + data$HONORARIUM.DAN.IMBALAN.LAIN.SEJENISNYA
                      + data$PREMI.ASURANSI.YANG.DIBAYAR.PEMBERI.KERJA
                      + data$PENERIMAAN.DALAM.BENTUK.NATURA.DAN.KENIKMATAN.LAINNYA.YANG.DIKENAKAN.PEMOTONGAN.PPh.PASAL.21)


## Biaya jabatan for the month
jbt_teratur_sebulan <- pmin(jabatan_tarif[1]*bruto_teratur_sebulan, jabatan_tarif[2]/12)

## Deductible costs for the month
biaya_sebulan <- c(data$IURAN.PENSIUN 
                   + data$IURAN.JAMINAN.HARI.TUA
                   + data$IURAN.TUNJANGAN.HARI.TUA)

## Neto teratur for the month
neto_teratur_sebulan <- pmax(bruto_teratur_sebulan - jbt_teratur_sebulan - biaya_sebulan,0)


## Number of months during which the employee is expected to work for the calendar year
months_to_end <- pmin(elapsed_months(year_end, data$TANGGAL.MULAI.BEKERJA)+1, 12)

## Annualization factor based on the time the employee becomes a taxable person during the calendar year
faktor_teratur <-   ifelse(data$MASA.KEWAJIBAN.PAJAK.SUBJEKTIF=="SATP" & data$AWAL.MASA.PENSIUN > year_end, months_to_end, 
                           ifelse(data$MASA.KEWAJIBAN.PAJAK.SUBJEKTIF == "SATP" & data$AWAL.MASA.PENSIUN < year_end, 
                                  pmin(elapsed_months(data$AWAL.MASA.PENSIUN, data$TANGGAL.MULAI.BEKERJA), elapsed_months(data$AWAL.MASA.PENSIUN, year_start)), 12)
                           )
  

## Annualized penghasilan neto teratur
neto_teratur_setahun <- pmax(neto_teratur_sebulan*faktor_teratur)

## PTKP
ptkp         <- ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="TK", ptkp[1], 0) +
                ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="K", ptkp[1]+ptkp[2], 0) +
                ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="HB", ptkp[1], 0) +
                
                ifelse(data$JENIS.KELAMIN=="W" & data$STATUS.KAWIN=="TK", ptkp[1], 0) +
                ifelse(data$JENIS.KELAMIN=="W" & data$STATUS.KAWIN=="K" & data$STATUS.KELUARGA=="", ptkp[1], 0) +
                ifelse(data$JENIS.KELAMIN=="W" & data$STATUS.KAWIN=="K" & data$STATUS.KELUARGA=="STK", ptkp[1]+ptkp[2], 0) +
                ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="HB", ptkp[1], 0) +
  
                ptkp[3]*data$STATUS...JUMLAH.TANGGUNGAN.KELUARGA.UNTUK.PTKP

## Tax base for penghasilan teratur
kp_teratur <- pmax(c(neto_teratur_setahun -  ptkp),0)
  
## PPh for the tax year
pph_teratur_setahun <-  ifelse(kp_teratur > kurung_pajak[4], 
                               
                               kurung_pajak[1]*tarif_pajak[1]+
                              (kurung_pajak[2] - kurung_pajak[1])*tarif_pajak[2] + 
                              (kurung_pajak[3]-kurung_pajak[2])*tarif_pajak[3] + 
                              (kurung_pajak[4]-kurung_pajak[3])*tarif_pajak[4] + 
                              (kp_teratur - kurung_pajak[4])*tarif_pajak[5],
                               
                        ifelse(kp_teratur > kurung_pajak[3],
                               
                               kurung_pajak[1]*tarif_pajak[1]+
                              (kurung_pajak[2] - kurung_pajak[1])*tarif_pajak[2] + 
                              (kurung_pajak[3]-kurung_pajak[2])*tarif_pajak[3] + 
                              (kp_teratur - kurung_pajak[3])*tarif_pajak[4],                   
                               
                        ifelse(kp_teratur > kurung_pajak[2], 
                               
                               kurung_pajak[1]*tarif_pajak[1]+
                              (kurung_pajak[2] - kurung_pajak[1])*tarif_pajak[2] + 
                              (kp_teratur - kurung_pajak[2])*tarif_pajak[3],                           
                               
                        ifelse(kp_teratur > kurung_pajak[1], 
                          
                               kurung_pajak[1]*tarif_pajak[1]+
                              (kp_teratur - kurung_pajak[1])*tarif_pajak[2], 
                        
                        kp_teratur*tarif_pajak[1]     
                              )
                            )
                          )
                        )
                               
## PPh for the month
pph_teratur_sebulan <- c(pph_teratur_setahun/faktor_teratur)



######################################################################################################################### 
##PENGHITUNGAN PPH 21 YANG HARUS DIPOTONG ATAS PENGHASILAN TIDAK TERATUR
#########################################################################################################################                     


## Penghasilan bruto tidak teratur paid or due this month
bruto_tdteratur <- c(data$TANTIEM..BONUS..GRATIFIKASI..JASA.PRODUKSI.DAN.THR)

## Annualization factor based on the time the employee becomes a taxable person during the calendar year
faktor_tdteratur <- ifelse(data$MASA.KEWAJIBAN.PAJAK.SUBJEKTIF=="SATP", 1, 12/months_working)

## Annualized penghasilan bruto tidak teratur
bruto_tdteratur_setahun <- c(bruto_tdteratur * faktor_tdteratur)


## Annualized penghasilan bruto teratur
bruto_teratur_setahun <- c(bruto_teratur_sebulan * faktor_teratur)

## Annualized penghasilan bruto total
bruto_total_setahun <- c(bruto_teratur_setahun + bruto_tdteratur_setahun)

## Annualized deductible costs
biaya_total_setahun <- c(biaya_sebulan * faktor_teratur)

## Biaya jabatan for the annualized penghasilan bruto total
jbt_total_setahun <- pmin(jabatan_tarif[1] * bruto_total_setahun, jabatan_tarif[2])

## Penghasilan neto total
neto_total_setahun <- pmax(bruto_total_setahun - jbt_total_setahun - biaya_total_setahun,0)

## Tax base for penghasilan total
kp_total <- pmax(neto_total_setahun -  ptkp,0)

## PPh total for the year
pph_total_setahun <-  ifelse(kp_total > kurung_pajak[4], 
                               
                               kurung_pajak[1]*tarif_pajak[1]+
                              (kurung_pajak[2] - kurung_pajak[1])*tarif_pajak[2] + 
                              (kurung_pajak[3]-kurung_pajak[2])*tarif_pajak[3] + 
                              (kurung_pajak[4]-kurung_pajak[3])*tarif_pajak[4] + 
                              (kp_total - kurung_pajak[4])*tarif_pajak[5],
                               
                      ifelse(kp_total > kurung_pajak[3],
                                      
                              kurung_pajak[1]*tarif_pajak[1]+
                              (kurung_pajak[2] - kurung_pajak[1])*tarif_pajak[2] + 
                              (kurung_pajak[3]-kurung_pajak[2])*tarif_pajak[3] + 
                              (kp_total - kurung_pajak[3])*tarif_pajak[4],                   
                                      
                      ifelse(kp_total > kurung_pajak[2], 
                                             
                              kurung_pajak[1]*tarif_pajak[1]+
                              (kurung_pajak[2] - kurung_pajak[1])*tarif_pajak[2] + 
                              (kp_total - kurung_pajak[2])*tarif_pajak[3],                           
                                             
                      ifelse(kp_total > kurung_pajak[1], 
                                                    
                              kurung_pajak[1]*tarif_pajak[1]+
                              (kp_total - kurung_pajak[1])*tarif_pajak[2], 
                                                    
                      kp_total*tarif_pajak[1]     
                      
                            )
                          )
                        )
                      )



## PPh on penghasian tidak teratur paid or due this month
pph_tdteratur <- pmax(pph_total_setahun - pph_teratur_setahun,0)

## Total PPh payable for the month
pph_terutang <- ifelse (data$NPWP=="", 
                        1.2*(pph_teratur_sebulan + pph_tdteratur), 
                        pph_teratur_sebulan + pph_tdteratur
                        )


######################################################################################################################### 
##GENERATE REPORT
#########################################################################################################################
                                                  

jumlah_bruto <- c(bruto_teratur_sebulan + bruto_tdteratur)

pekerja_tetap_current <-  data.frame(masa_pajak, 
                          tahun_pajak, 
                          pembetulan, 
                          data$NPWP, 
                          data$NAMA, 
                          kode_objek_pajak, 
                          jumlah_bruto, 
                          pph_terutang, 
                          data$KODE.NEGARA.DOMISILI
                          )


pekerja_tetap_current <- setNames(pekerja_tetap_current, c("Masa Pajak",
                                                           "Tahun Pajak",
                                                           "Pembetulan", 
                                                           "NPWP", 
                                                           "Nama", 
                                                           "Kode Pajak", 
                                                           "Jumlah Bruto", 
                                                           "Jumlah PPh", 
                                                           "Kode Negara"
                                                          )
                                  )