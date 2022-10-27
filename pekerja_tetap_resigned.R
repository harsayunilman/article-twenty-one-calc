######################################################################################################################### 
##AUTHOR: HARSA YUNILMAN
#########################################################################################################################

## Extract payroll data
data <- read.csv("payroll_pekerja_tetap_resigned.csv")
data <- transform(data, "TANGGAL.MULAI.BEKERJA" = as.Date(data[,1]))

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
masa_pajak <- 5
kode_objek_pajak <- "21-100-01"
pembetulan <- 0
tahun_pajak <- 2016

month_end <- as.Date("2016-05-31")
year_start <- as.Date("2016-01-31")
year_end <- as.Date("2016-12-31")



######################################################################################################################### 
##PENGHITUNGAN PPH 21 YANG HARUS DIPOTONG ATAS PENGHASILAN YANG DITERIMA DARI AWAL TAHUN PAJAK S.D. AKHIR TAHUN PAJAK
######################################################################################################################### 


## Calculate the number of months the employee has worked since the beginning of tax year
months_working <- ifelse( data$TANGGAL.MULAI.BEKERJA > year_start, 
                          elapsed_months(month_end, data$TANGGAL.MULAI.BEKERJA)+1, 
                          elapsed_months(month_end, year_start)+1 )


## Penghasilan bruto teratur for the year
bruto_teratur_setahun <- c(data$GAJI.PENSIUN.ATAU.THT.JHT 
                           + data$TUNJANGAN.PPh 
                           + data$TUNJANGAN.LAINNYA 
                           + data$OVERTIME 
                           + data$JAMINAN.KECELAKAAN.KERJA..JKK.
                           + data$JAMINAN.KEMATIAN..JKM.
                           + data$JAMINAN.PEMELIHARAAN.KESEHATAN..JPK.
                           + data$HONORARIUM.DAN.IMBALAN.LAIN.SEJENISNYA
                           + data$PREMI.ASURANSI.YANG.DIBAYAR.PEMBERI.KERJA
                           + data$PENERIMAAN.DALAM.BENTUK.NATURA.DAN.KENIKMATAN.LAINNYA.YANG.DIKENAKAN.PEMOTONGAN.PPh.PASAL.21)

## Penghasilan bruto tidak teratur paid thoughout the year
bruto_tdteratur_setahun <- c(data$TANTIEM..BONUS..GRATIFIKASI..JASA.PRODUKSI.DAN.THR)

## Penghasilan bruto total
bruto_total_setahun <- c(bruto_teratur_setahun + bruto_tdteratur_setahun)

## Annualization factor based on the time the employee becomes a taxable person during the calendar year
faktor_pengetahunan <- ifelse(data$MASA.KEWAJIBAN.PAJAK.SUBJEKTIF=="SATP", 1, 12 / months_working)

## Annualized penghasilan bruto
bruto_total_disetahunkan <- c(bruto_total_setahun * faktor_pengetahunan)

## Biaya jabatan for the month
jbt_total_disetahunkan <- ifelse(data$MASA.KEWAJIBAN.PAJAK.SUBJEKTIF=="SATP", 
                                 pmin(jabatan_tarif[1] * bruto_total_setahun, jabatan_tarif[2] * months_working / 12),
                                 pmin(jabatan_tarif[1] * bruto_total_disetahunkan, jabatan_tarif[2])
)

## Deductible costs for the year
biaya_total_setahun <- c(data$IURAN.PENSIUN 
                         + data$IURAN.JAMINAN.HARI.TUA
                         + data$IURAN.TUNJANGAN.HARI.TUA)

## Annualized deductible costs
biaya_total_disetahunkan <- c(biaya_total_setahun * faktor_pengetahunan)

## Neto teratur for year
neto_total_disetahunkan <- pmax(bruto_total_disetahunkan - jbt_total_disetahunkan - biaya_total_disetahunkan,0)

## PTKP
ptkp         <- ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="TK", ptkp[1], 0) +
  ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="K", ptkp[1]+ptkp[2], 0) +
  ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="HB", ptkp[1], 0) +
  
  ifelse(data$JENIS.KELAMIN=="W" & data$STATUS.KAWIN=="TK", ptkp[1], 0) +
  ifelse(data$JENIS.KELAMIN=="W" & data$STATUS.KAWIN=="K" & data$STATUS.KELUARGA=="", ptkp[1], 0) +
  ifelse(data$JENIS.KELAMIN=="W" & data$STATUS.KAWIN=="K" & data$STATUS.KELUARGA=="STK", ptkp[1]+ptkp[2], 0) +
  ifelse(data$JENIS.KELAMIN=="L" & data$STATUS.KAWIN=="HB", ptkp[1], 0) +
  
  ptkp[3]*data$STATUS...JUMLAH.TANGGUNGAN.KELUARGA.UNTUK.PTKP

## Tax base
kp_total_disetahunkan <- pmax(c(neto_total_disetahunkan -  ptkp),0)

## PPh for the tax year
pph_total_disetahunkan <-  ifelse(kp_total_disetahunkan > kurung_pajak[4], 
                                  
                                  kurung_pajak[1]*tarif_pajak[1]+
                                    (kurung_pajak[2] - kurung_pajak[1])*tarif_pajak[2] + 
                                    (kurung_pajak[3]-kurung_pajak[2])*tarif_pajak[3] + 
                                    (kurung_pajak[4]-kurung_pajak[3])*tarif_pajak[4] + 
                                    (kp_total_disetahunkan - kurung_pajak[4])*tarif_pajak[5],
                                  
                                  ifelse(kp_total_disetahunkan > kurung_pajak[3],
                                         
                                         kurung_pajak[1]*tarif_pajak[1]+
                                           (kurung_pajak[2] - kurung_pajak[1])*tarif_pajak[2] + 
                                           (kurung_pajak[3]-kurung_pajak[2])*tarif_pajak[3] + 
                                           (kp_total_disetahunkan - kurung_pajak[3])*tarif_pajak[4],                   
                                         
                                         ifelse(kp_total_disetahunkan > kurung_pajak[2], 
                                                
                                                kurung_pajak[1]*tarif_pajak[1]+
                                                  (kurung_pajak[2] - kurung_pajak[1])*tarif_pajak[2] + 
                                                  (kp_total_disetahunkan - kurung_pajak[2])*tarif_pajak[3],                           
                                                
                                                ifelse(kp_total_disetahunkan > kurung_pajak[1], 
                                                       
                                                       kurung_pajak[1]*tarif_pajak[1]+
                                                         (kp_total_disetahunkan - kurung_pajak[1])*tarif_pajak[2], 
                                                       
                                                       kp_total_disetahunkan*tarif_pajak[1]     
                                                )
                                         )
                                  )
)

## Actual PPh for the year
pph_total <- c(pph_total_disetahunkan / faktor_pengetahunan)

## PPh due for the month of December
pph_resigned <- ifelse (data$NPWP=="", 
                        1.2*(pph_total - data$PPh.PASAL.21.YANG.TELAH.DIPOTONG.MASA.SEBELUMNYA), 
                        pph_total - data$PPh.PASAL.21.YANG.TELAH.DIPOTONG.MASA.SEBELUMNYA
)



######################################################################################################################### 
##GENERATE REPORT
#########################################################################################################################


pekerja_tetap_resigned <-  data.frame(masa_pajak, 
                                      tahun_pajak, 
                                      pembetulan, data$NPWP, 
                                      data$NAMA, 
                                      kode_objek_pajak, 
                                      bruto_total_setahun, 
                                      pph_resigned, 
                                      data$KODE.NEGARA.DOMISILI
)


pekerja_tetap_resigned <- setNames(pekerja_tetap_resigned, c("Masa Pajak",
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
