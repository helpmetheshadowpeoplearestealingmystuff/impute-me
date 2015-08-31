 

  

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
make a sample file


#Get shapeit
wget https://mathgen.stats.ox.ac.uk/genetics_software/shapeit/shapeit.v2.r837.GLIBCv2.12.Linux.static.tgz
tar zxvf shapeit.v2.r837.GLIBCv2.12.Linux.static.tgz

#Get gtools
wget http://www.well.ox.ac.uk/~cfreeman/software/gwas/gtool_v0.7.5_x86_64.tgz
tar zxvf gtool_v0.7.5_x86_64.tgz

#This part can probably be skipped now
# git clone https://github.com/johnlees/23andme-impute
# mv 23andme-impute/* .
# wget http://search.cpan.org/CPAN/authors/id/T/TO/TOMHUGHES/IO-Zlib-1.10.tar.gz
# gzip -d IO-Zlib-1.10.tar.gz
# tar xvf IO-Zlib-1.10.tar
# cd IO-Zlib-1.10
# perl Makefile.PL PREFIX=~/impute_dir/IO-zlib
# make
# make test
# make install
# export PERL5LIB=~/impute_dir/IO-zlib/share/perl5/



	
	# use lib '~/impute_dir/IO-zlib';
# gunzip genome_Lasse_Folkersen_Full_20140731040800.txt.gz
	
# perl ~/impute_dir/impute_genome.pl -i ~/impute_dir/genome_Lasse_Folkersen_Full_20140731040800.txt -g ~/impute_dir/ALL_1000G_phase1integrated_v3_impute/ -o this_output -p > ~/impute_dir/scriptFile


# gzip: /home/people/lasfol/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chrY_impute.legend.gz:
	
	
	
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr1_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr1_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr1_impute.legend.gz -g this_output.chr1.gen -int 40e6 45e6 -Ne 20000 -o tmp_impute2.chr1.9 -phase -allow_large_regions





# 

# 	
# 	
# 	
# 	
# 	
# 	
# 	
# 	
# #Testing the impute
# cut --delimiter=" " -f 1-8 /home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/Example/example.chr22.study.gens > /home/ubuntu/misc_files/test_genome.gen
# 
# 
# 	
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g /home/ubuntu/misc_files/test_genome.gen -int 1 21596808 -Ne 20000 -o tmp_impute2.chr22.1 -phase -allow_large_regions
# 
# 
# impute2 \
# -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz \
# -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz \
# -g /home/ubuntu/example_output/imputation_folder_id_562283631/id_562283631.chr22.gen \
# -int 5e6 10e6 -Ne 20000 -o tmp_impute2.chr22.2 -phase -allow_large_regions
# 	
# 
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g id_562283631.chr22.gen -int 25e6 26e6 -Ne 20000 -o xtmp_impute2.chr22.6 -phase -allow_large_regions
# 
# 
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g /home/ubuntu/misc_files/test_genome.gen -int 16050308 16050712 -Ne 20000 -o yytmp_impute2.chr22.6 -phase -allow_large_regions
# #This here actually works - so when you force SNPs known to be in reference to be in the gen file, 
# 
# # zcat /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz | head -n 100
# 
# 
# 
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g test_genome2.gen -int 17026094 17195269 -Ne 20000 -o ctmp_impute2.chr22.6 -phase -allow_large_regions
# 
# 
# 
# 
# 
# 
# wget https://mathgen.stats.ox.ac.uk/impute/1000GP_Phase3.tgz
# tar zxvf 1000GP_Phase3.tgz
# 
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g test_genome2.gen -int 17026094 17195269 -Ne 20000 -o ctmp_impute2.chr22.6 -phase -allow_large_regions
# #this is the ALL_1000G version, on a short piece of 23andme genotypes. It runs with no error, but doesn't impute anything.
# 
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g /home/ubuntu/misc_files/test_genome.gen -int 16050308 16050712 -Ne 20000 -o yytmp_impute2.chr22.6 -phase -allow_large_regions
# #This here actually works - so when you force SNPs known to be in reference to be in the gen file, 
# 
# 
# 
# 
# impute2 \
# -m /home/ubuntu/new_version/1000GP_Phase3/genetic_map_chr22_combined_b37.txt \
# -h /home/ubuntu/new_version/1000GP_Phase3/1000GP_Phase3_chr22.hap.gz \
# -l /home/ubuntu/new_version/1000GP_Phase3/1000GP_Phase3_chr22.legend.gz \
# -g /home/ubuntu/misc_files/test_genome.gen \
# -int 16050308 16050712 -Ne 20000 -o wmp_impute2.chr22.6 -phase -allow_large_regions
# #This is the forced in example SNPs - but now it doesn't work?!
# 
# 
# 
# 
# impute2 \
# -m /home/ubuntu/new_version/1000GP_Phase3/genetic_map_chr22_combined_b37.txt \
# -h /home/ubuntu/new_version/1000GP_Phase3/1000GP_Phase3_chr22.hap.gz \
# -l /home/ubuntu/new_version/1000GP_Phase3/1000GP_Phase3_chr22.legend.gz \
# -g /home/ubuntu/misc_files/id_562283631.chr22.gen  \
# -int 16114244 17661178 -Ne 20000 -o qmp_impute2.chr22.6 -phase -allow_large_regions
# #This is the 23andme data - it does not work
# 
# 
# 
# cut --delimiter=" " -f 1-8 \
# /home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/Example/example.chr22.study.gens \
# > /home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/Example/example.chr22.study.8col.gens
# 
# 
# impute2 \
# -m /home/ubuntu/new_version/1000GP_Phase3/genetic_map_chr22_combined_b37.txt \
# -h /home/ubuntu/new_version/1000GP_Phase3/1000GP_Phase3_chr22.hap.gz \
# -l /home/ubuntu/new_version/1000GP_Phase3/1000GP_Phase3_chr22.legend.gz \
# -g /home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/Example/example.chr22.study.8col.gens  \
# -int 22014144 22016144 -Ne 20000 -o qmp_impute2.chr22.6 -phase -allow_large_regions
# #This is the example day
# 
# less /home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/Example/example.chr22.study.8col.gens
# 
# vi /home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/Example/example.chr22.study.8col.gens SNP_A-4277437 rs9941935 20345144 A G 1 0 0
# 
# zcat /home/ubuntu/new_version/1000GP_Phase3/1000GP_Phase3_chr22.legend.gz | grep rs9941935
# # rs9941935:22015144:A:G 22015144 A G Biallelic_SNP 0.57034795763994 0.351585014409222 0.0188492063492063 0.342942345924453 0.217791411042945 0.314496805111821
# 
# #ok so try to replace with 22015144
# #this works, actually. So check if it's a position problem
# 
# 
# 
# 
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g test_genome2.gen -int 17026094 17195269 -Ne 20000 -o cctmp_impute2.chr22.6 -phase -allow_large_regions
# 
# 
# 
# grep rs9605903 test_genome2.gen
# # SNP9 rs9605903 17054720 C T 1 0 0
# 
# zcat /home/ubuntu/new_version/1000GP_Phase3/1000GP_Phase3_chr22.legend.gz | grep rs9605903
# # rs9605903:17054720:T:C 17054720 T C Biallelic_SNP 0.0408472012102874 0.262247838616715 0.236111111111111 0.259443339960239 0.193251533742331 0.184504792332268
# 
# zcat /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz | grep rs9605903
# # rs9605903 17054720 T C SNP LOWCOV 0.9924 0.0569 0.2569 0.2395 0.2691 0.0569 0.2569 0.2395 0.2691
# 
# 
# 
# #but still it becomes .25 .50 .25....
# 
# head -n 100 id_562283631.chr22.gen >id_562283631.chr22_first_100.gen
# 
# #try to permutate more stuff
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g id_562283631.chr22_first_100.gen  -int 17026094 17195269 -Ne 20000 -o ccctmp_impute2.chr22.6 -phase -allow_large_regions
# 
# 
# 
# vi id_562283631.chr22_first_100.gen
# 
# #UCSC
# # rs9605903 at chr22:17054470-17054970
# 
# grep rs9605903 id_562283631.chr22_first_100.gen
# 
# zcat /home/ubuntu/new_version/1000GP_Phase3/1000GP_Phase3_chr22.legend.gz | grep rs9605903
# # rs9605903:17054720:T:C 17054720 T C Biallelic_SNP 0.0408472012102874 0.262247838616715 0.236111111111111 0.259443339960239 0.193251533742331 0.184504792332268
# 
# 
# 
# wget https://mathgen.stats.ox.ac.uk/genetics_software/shapeit/shapeit.v2.r837.GLIBCv2.12.Linux.static.tgz
# 
# 
# 
# 
# 
# #try to permutate more stuff
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g id_562283631.chr22_first_100.gen  -int 17026094 17195269 -Ne 20000 -o test_x
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #Ok try over with the example data
# cut --delimiter=" " -f 1-8 /home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/Example/example.chr22.study.gens > /home/ubuntu/misc_files/test_genome.gen
# 
# 
# 
# grep rs12484060 id_562283631.chr22.gen
# 
# 
# test_genome3.gen
# SNP_A-2185542 rs4821116 20303319 A G 0 0 1
# SNP1338 rs1210829 20308800 C A 0 0 1
# SNP1339 rs9606323 20312668 G A 0 0 1
# SNP1340 rs6518604 20323211 A T 0 0 1
# SNP_A-1970719 rs861844 20336219 G T 0 1 0
# SNP_A-2265815 rs16989505 20344764 A G 1 0 0
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g test_genome3.gen -int 20303310 20344770 -Ne 20000 -o test_y
# #works
# 
# vi test_genome4.gen
# SNP1338 rs1210829 20308800 C A 0 0 1
# SNP1339 rs9606323 20312668 G A 0 0 1
# SNP1340 rs6518604 20323211 A T 0 0 1
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g test_genome4.gen -int 20303310 20344770 -Ne 20000 -o test_z
# #This works as well?!?! (it was only 23andme SNPs?)
# 
# 
# 
# 
# #Try to take some more
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g id_562283631.chr22.gen -int 20303310 20344770 -Ne 20000 -o test_t
# #this doesn't work
# 
# 
# vi id_562283631.chr22.gen
# 
# vi test_genome5.gen
# SNP1264 rs2286928 20098521 G A 0 0 1
# SNP1265 rs417309 20098544 G A 0 0 1
# SNP1266 rs720012 20098582 A G 1 0 0
# SNP1267 rs3757 20099331 G A 0 0 1
# SNP1268 rs1633445 20100596 T A 0 0 1
# SNP1269 rs885980 20102090 C T 1 0 0
# SNP1270 rs2238798 20107729 A G 1 0 0
# SNP1271 rs11705021 20109677 G A 0 0 1
# SNP1272 rs175169 20117345 G T 1 0 0
# SNP1274 rs175174 20127554 A G 1 0 0
# SNP1275 rs8137258 20135961 T A 0 0 1
# SNP1276 rs649737 20144663 G A 0 0 1
# SNP1277 rs175185 20145121 A G 1 0 0
# SNP1278 rs17817803 20145526 C T 1 0 0
# SNP1279 rs9605074 20146533 G A 0 0 1
# SNP1280 rs588536 20148118 G A 0 0 1
# SNP1281 rs373747 20155192 C T 1 0 0
# SNP1282 rs28384 20156415 C T 1 0 0
# SNP1283 rs455127 20160205 C T 1 0 0
# SNP1284 rs1153418 20161424 C T 1 0 0
# SNP1285 rs175194 20163070 A G 1 0 0
# SNP1286 rs385773 20164589 A G 1 0 0
# SNP1287 rs175197 20165632 C T 1 0 0
# SNP1288 rs175199 20168295 G T 1 0 0
# SNP1289 rs658073 20170996 A T 0 0 1
# SNP1290 rs10212087 20171251 C A 0 0 1
# SNP1291 rs617427 20171368 C T 1 0 0
# SNP1292 rs75766 20174853 A T 0 0 1
# SNP1293 rs701446 20179255 C T 1 0 0
# SNP1294 rs696885 20181136 G T 1 0 0
# SNP1295 rs696884 20183509 A G 1 0 0
# SNP1296 rs613930 20185119 A C 1 0 0
# SNP1297 rs625704 20185457 A G 1 0 0
# SNP1298 rs655656 20187575 C T 1 0 0
# SNP1299 rs672570 20189077 C T 1 0 0
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g test_genome5.gen -int 20098520 20344770 -Ne 20000 -o test_c
# #this doesn't work
# 
# 
# 
# 
# 
# 
# vi test_genome6.gen
# SNP1275 rs8137258 20135961 T A 0 0 1
# SNP1287 rs175197 20165632 C T 1 0 0
# SNP1295 rs696884 20183509 A G 1 0 0
# SNP1299 rs672570 20189077 C T 1 0 0
# 
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -g test_genome6.gen -int 20098520 20344770 -Ne 20000 -o test_v
# 
# #ok, this works again. So it could really be a problem with the phasing
# 
# 
# 
# /home/ubuntu/impute_dir/bin/shapeit
# 
# # shapeit --input-bed gwas.bed gwas.bim gwas.fam \
# # --input-map genetic_map.txt \
# # --output-max gwas.phased.haps gwas.phased.sample
# 
# 
# #Ok need plink as well
# cd ~/impute_dir
# wget http://pngu.mgh.harvard.edu/~purcell/static/bin/plink140618/plink_linux_x86_64.zip
# 
# 
# # /home/ubuntu/impute_dir/plink --23file",fileNameUnzip,firstName,lastName,"--out",prefix")
# 
# /home/ubuntu/impute_dir/plink --noweb --23file /home/ubuntu/example_output/imputation_folder_id_562283631/id_562283631_raw_data.txt name1 name2 --recode --out plinkoutx2
# 
# 
# 
# for chr in $(seq 1 22); do
# ~/impute_dir/plink --file plinkoutx2 \
# --chr $chr \
# --recode \
# --out plinkoutx2_$chr \
# --exclude mysnps.txt;
# done
# 
# 
# /home/ubuntu/impute_dir/bin/shapeit \
# --input-ped plinkoutx2_22.ped plinkoutx2_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz reference.sample
# -O shapeitxout1
# 
# 
# 
# 
# url<- "ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20110521/supporting/phase1_samples_integrated_20101123.ped"
# download.file(url,basename(url))
# d<-read.table(basename(url),stringsAsFactors=F,sep="\t",comment.char="", quote="",fill=T,header=T)
# head(d)
# d[,"group"]<-d[,"Population"]
# d<-d[,c("Individual.ID","Population","group","Gender")]
# colnames(d)<-c("sample","population","group","sex")
# set.seed(42)
# d<-d[sample(rownames(d),2184/2),]
# write.table(d,file="sample.reference.txt",sep="\t",row.names=F,col.names=T,quote=F)
# #make a sample file
# 
# 
# # grep 51063820 plinkoutx2_22.map
# #remove i5012765
# # grep 51065593 plinkoutx2_22.map
# #remove i5012767
# 
# /home/ubuntu/impute_dir/bin/shapeit -check \
# --input-ped plinkoutx2_22.ped plinkoutx2_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz /home/ubuntu/misc_files/sample.reference.txt \
# --output-log W_test
# 
# cut -f 4 W_test.snp.strand > exclude_snps_here
# 
# ~/impute_dir/plink --file plinkoutx2_22 \
# --recode \
# --out plinkoutxx_22 \
# --exclude exclude_snps_here
# 
# 
# /home/ubuntu/impute_dir/bin/shapeit -check \
# --input-ped plinkoutxx_22.ped plinkoutxx_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz /home/ubuntu/misc_files/sample.reference.txt \
# --output-log R_test
# 
# 
# 
# 
# /home/ubuntu/impute_dir/bin/shapeit \
# --input-ped plinkoutxx_22.ped plinkoutxx_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz /home/ubuntu/misc_files/sample.reference.txt \
# -O T_test
# 
# 
# 
# 
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -known_haps_g T_test.haps -int 20098520 20344770 -Ne 20000 -o test_qt
# # THis worked! Wauw
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #More general
# 
# /home/ubuntu/impute_dir/plink --noweb --23file /home/ubuntu/example_output/imputation_folder_id_562283631/id_562283631_raw_data.txt name1 name2 --recode --out plinkoutx2
# 
# 
# 
# for chr in $(seq 1 22); do
# ~/impute_dir/plink --file plinkoutx2 \
# --chr $chr \
# --recode \
# --out plinkoutx2_$chr \
# --exclude mysnps.txt;
# done
# 
# 
# /home/ubuntu/impute_dir/bin/shapeit \
# --input-ped plinkoutx2_22.ped plinkoutx2_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz reference.sample
# -O shapeitxout1
# 
# 
# 
# 
# url<- "ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20110521/supporting/phase1_samples_integrated_20101123.ped"
# download.file(url,basename(url))
# d<-read.table(basename(url),stringsAsFactors=F,sep="\t",comment.char="", quote="",fill=T,header=T)
# head(d)
# d[,"group"]<-d[,"Population"]
# d<-d[,c("Individual.ID","Population","group","Gender")]
# colnames(d)<-c("sample","population","group","sex")
# set.seed(42)
# d<-d[sample(rownames(d),2184/2),]
# write.table(d,file="sample.reference.txt",sep="\t",row.names=F,col.names=T,quote=F)
# #make a sample file
# 
# 
# # grep 51063820 plinkoutx2_22.map
# #remove i5012765
# # grep 51065593 plinkoutx2_22.map
# #remove i5012767
# 
# /home/ubuntu/impute_dir/bin/shapeit -check \
# --input-ped plinkoutx2_22.ped plinkoutx2_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz /home/ubuntu/misc_files/sample.reference.txt \
# --output-log W_test
# 
# cut -f 4 W_test.snp.strand > exclude_snps_here
# 
# ~/impute_dir/plink --file plinkoutx2_22 \
# --recode \
# --out plinkoutxx_22 \
# --exclude exclude_snps_here
# 
# 
# /home/ubuntu/impute_dir/bin/shapeit -check \
# --input-ped plinkoutxx_22.ped plinkoutxx_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz /home/ubuntu/misc_files/sample.reference.txt \
# --output-log R_test
# 
# 
# 
# 
# /home/ubuntu/impute_dir/bin/shapeit \
# --input-ped plinkoutxx_22.ped plinkoutxx_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz /home/ubuntu/misc_files/sample.reference.txt \
# -O T_test
# 
# 
# 
# 
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -known_haps_g T_test.haps -int 20098520 20344770 -Ne 20000 -o test_qt
# # THis worked! Wauw
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
