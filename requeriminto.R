setwd("C:/Users/dzapata/Documents/TAREA/29_AGO") 
library(haven)  
library(dplyr) 
library(readxl)
rm(list=ls())
append00<-read_dta("append00.dta")
append01<-read_dta("append01.dta")
append02<-read_dta("append02.dta")
append03<-read_dta("append03.dta")
append04<-read_dta("append04.dta")
append05<-read_dta("append05.dta")
append06<-read_dta("append06.dta")
append07<-read_dta("append07.dta")
append08<-read_dta("append08.dta")
append09<-read_dta("append09.dta")
append10<-read_dta("append10.dta") 
append11<-read_dta("append11.dta")
append12<-read_dta("append12.dta")
append13<-read_dta("append13.dta")
append14<-read_dta("append14.dta")
append15<-read_dta("append15.dta") 
append16<-read_dta("append16.dta")
append17<-read_dta("append17.dta")
append<-list(append00=append00,append01=append01,append02=append02,
             append03=append03,append04=append04,append05=append05,
             append06=append06,append07=append07,append08=append08,
             append09=append09,append10=append10,append11=append11,
             append12=append12,append13=append13,append14=append14,
             append15=append15,append16=append16,append17=append17)  
sapply(append, colnames)
sapply(append, class)

temp<-lapply(append,select,c("fob_dol3","pos_ara3" ,"pais3", "kilo_ne3", "mes","depto_proc"))

# Correlativa mineros, no mineros y sectores
nansec<-read_xlsx("NAN_SEC.xlsx") 
colnames(nansec)
MNME<-lapply(temp,merge,y=nansec,by.x="pos_ara3",by.y="NANDINA")  
MNME1<-lapply(MNME,subset,Descripción=="Mineros")  

# Correlativa paises
correlativapaises<-read_xlsx("correlativacorregida.xlsx")
colnames(correlativapaises)
COUNTRY<-lapply(temp,merge,y=correlativapaises,by="pais3")  
COUNTRY1<-lapply(COUNTRY,subset,nombre=="ALEMANIA")  


# Correlativa de Totpart: Cuci, Ciiu
totpart<-read_dta("correlativa_nueva1.dta")
colnames(totpart)
CUCI<-lapply(temp,merge,y=totpart,by.x="pos_ara3", by.y="nandina")  
CUCI1<-lapply(CUCI,subset,ciiu_rev__4_a_c=="0142")  

# Correlativa de departamento
depto<-read_dta("dpto.dta")
colnames(depto)
DEPTO<-lapply(temp,merge,y=depto,by="depto_proc") 
DEPTO1<-lapply(DEPTO,subset,nombredepartamento=="BOGOTA")  












