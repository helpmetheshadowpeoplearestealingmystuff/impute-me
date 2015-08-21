



#Set up server
# qsub -I -W group_list=allelic_imbalance -l nodes=1:ppn=1,mem=32gb,walltime=360000
sudo apt-get update
sudo apt-get install r-base-dev
sudo apt-get install libcurl4-openssl-dev

sudo -i
R
install.packages("shiny")
install.packages("rmarkdown")
install.packages("openxlsx")
install.packages("nlme")
install.packages("R.utils")
install.packages("mail")

q()
y
exit


sudo apt-get install gdebi-core
wget http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.3.0.403-amd64.deb
sudo gdebi shiny-server-1.3.0.403-amd64.deb
y

#then listen on port 80
sudo vi /etc/shiny-server/shiny-server.conf


sudo apt-get install git




mkdir impute_dir
cd impute_dir

wget https://mathgen.stats.ox.ac.uk/impute/impute_v2.3.2_x86_64_static.tgz
gunzip impute_v2.3.2_x86_64_static.tgz
tar -xvf impute_v2.3.2_x86_64_static.tar
export PATH=$PATH:~/impute_dir/impute_v2.3.2_x86_64_static >> >> ~/.bashrc


# wget https://mathgen.stats.ox.ac.uk/impute/1000GP_Phase3.tgz
# gunzip 1000GP_Phase3.tgz
# tar -xvf 1000GP_Phase3.tar

wget https://mathgen.stats.ox.ac.uk/impute/ALL_1000G_phase1integrated_v3_impute.tgz
gunzip ALL_1000G_phase1integrated_v3_impute.tgz
tar xf ALL_1000G_phase1integrated_v3_impute.tar
rm ALL_1000G_phase1integrated_v3_impute.tar 


wget https://mathgen.stats.ox.ac.uk/impute/ALL_1000G_phase1integrated_v3_annotated_legends.tgz
gunzip ALL_1000G_phase1integrated_v3_annotated_legends.tgz
# mv ALL_1000G_phase1integrated_v3_annotated_legends.tar ALL_1000G_phase1integrated_v3_impute/
tar xf ALL_1000G_phase1integrated_v3_annotated_legends.tar
rm ALL_1000G_phase1integrated_v3_annotated_legends.tar 
mv ALL_1000G_phase1integrated_v3_annotated_legends/* ALL_1000G_phase1integrated_v3_impute/
rmdir ALL_1000G_phase1integrated_v3_annotated_legends

git clone https://github.com/johnlees/23andme-impute
mv 23andme-impute/* .



wget http://search.cpan.org/CPAN/authors/id/T/TO/TOMHUGHES/IO-Zlib-1.10.tar.gz
gzip -d IO-Zlib-1.10.tar.gz
tar xvf IO-Zlib-1.10.tar
cd IO-Zlib-1.10
 
perl Makefile.PL PREFIX=~/impute_dir/IO-zlib
make
make test
make install
export PERL5LIB=~/impute_dir/IO-zlib/share/perl5/


# use lib '~/impute_dir/IO-zlib';
gunzip genome_Lasse_Folkersen_Full_20140731040800.txt.gz
	
perl ~/impute_dir/impute_genome.pl -i ~/impute_dir/genome_Lasse_Folkersen_Full_20140731040800.txt -g ~/impute_dir/ALL_1000G_phase1integrated_v3_impute/ -o this_output -p > ~/impute_dir/scriptFile


# gzip: /home/people/lasfol/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chrY_impute.legend.gz:
	
	
	
impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr1_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr1_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr1_impute.legend.gz -g this_output.chr1.gen -int 40e6 45e6 -Ne 20000 -o tmp_impute2.chr1.9 -phase -allow_large_regions
