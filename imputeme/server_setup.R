 

  

#Set up server


#First install fundamental packages
sudo apt-get update
sudo apt-get install r-base-dev
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install default-jdk


#Then install R-packages
sudo -i
R
install.packages("shiny")
install.packages("rmarkdown")
install.packages("openxlsx")
install.packages("nlme")
install.packages("R.utils")
install.packages("mailR")
q()
y
exit



#Then install shiny server
sudo apt-get install gdebi-core
wget http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.3.0.403-amd64.deb
sudo gdebi shiny-server-1.3.0.403-amd64.deb
y

#then listen on port 80
sudo vi /etc/shiny-server/shiny-server.conf
#also add run_as ubuntu  (!!!!)


#Then install git
sudo apt-get install git


#Then get imputation related programs (suggest to alter name at some point?)
mkdir impute_dir
cd impute_dir


#get impute2
wget https://mathgen.stats.ox.ac.uk/impute/impute_v2.3.2_x86_64_static.tgz
gunzip impute_v2.3.2_x86_64_static.tgz
tar -xvf impute_v2.3.2_x86_64_static.tar


#get the reference from 1kgenomes
wget https://mathgen.stats.ox.ac.uk/impute/ALL_1000G_phase1integrated_v3_impute.tgz
gunzip ALL_1000G_phase1integrated_v3_impute.tgz
tar xf ALL_1000G_phase1integrated_v3_impute.tar
rm ALL_1000G_phase1integrated_v3_impute.tar 
wget https://mathgen.stats.ox.ac.uk/impute/ALL_1000G_phase1integrated_v3_annotated_legends.tgz
gunzip ALL_1000G_phase1integrated_v3_annotated_legends.tgz
tar xf ALL_1000G_phase1integrated_v3_annotated_legends.tar
rm ALL_1000G_phase1integrated_v3_annotated_legends.tar 
mv ALL_1000G_phase1integrated_v3_annotated_legends/* ALL_1000G_phase1integrated_v3_impute/
rmdir ALL_1000G_phase1integrated_v3_annotated_legends


#In R - get samples
url<- "ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20110521/supporting/phase1_samples_integrated_20101123.ped"
download.file(url,basename(url))
d<-read.table(basename(url),stringsAsFactors=F,sep="\t",comment.char="", quote="",fill=T,header=T)
head(d)
d[,"group"]<-d[,"Population"]
d<-d[,c("Individual.ID","Population","group","Gender")]
colnames(d)<-c("sample","population","group","sex")
set.seed(42)
d<-d[sample(rownames(d),2184/2),]
write.table(d,file="sample.reference.txt",sep="\t",row.names=F,col.names=T,quote=F)
# make a sample file


#Get shapeit
wget https://mathgen.stats.ox.ac.uk/genetics_software/shapeit/shapeit.v2.r837.GLIBCv2.12.Linux.static.tgz
tar zxvf shapeit.v2.r837.GLIBCv2.12.Linux.static.tgz

#Get gtools
wget http://www.well.ox.ac.uk/~cfreeman/software/gwas/gtool_v0.7.5_x86_64.tgz
tar zxvf gtool_v0.7.5_x86_64.tgz


#get server
cd /srv/shiny-server/
sudo git clone https://github.com/lassefolkersen/gene-surfer





#setup rsa
# run ssh-keygen on impute.me server (as user ubuntu). 
# Also append id_rsa.pub from all the nodes to the hub
ssh-keygen -t rsa -b 4096 -C "impute_me_keys"



#setup misc_files
mkdir /home/ubuntu/misc_files


#Setup configuration file
vi /home/ubuntu/misc_files/configuration.R
# maxImputations <- 1
# maxImputationsInQueue <- 3
# serverRole <- "Hub"
# hubAddress <- "54.187.201.139"


#Setup accepted emails
vi /home/ubuntu/misc_files/accepted_emails.txt
# lassefolkersen@mail.com


#make a folder for cron logs
mkdir /home/ubuntu/misc_files/cron_logs
