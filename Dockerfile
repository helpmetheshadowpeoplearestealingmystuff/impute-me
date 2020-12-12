FROM rocker/shiny:4.0.1
MAINTAINER Lasse Folkersen, lassefolkersen@impute.me

ARG DEBIAN_FRONTEND=noninteractive

# Install easy R-packages
RUN R -e "install.packages(c( \
'DT', \
'igraph',  \
'jsonlite', \
'mailR', \
'openxlsx', \
'R.utils', \
'visNetwork' \
),dependencies=TRUE, repos = 'http://cran.rstudio.com/')"

#Install basic non-problematic apt-get apps
RUN apt-get update && apt-get -y install \
wget \
git \
sed \
tar \
unzip \
vim \
curl \
vcftools

#Install kandinsky package, plus the remotes-package that is needed to get it
RUN R -e "install.packages(c( \
'remotes' \
),dependencies=TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('gsimchoni/kandinsky')"

#Install the mailR, plus the java environment required for it
RUN apt-get -y update && apt-get install -y \
default-jdk \
r-cran-rjava 
RUN R -e "install.packages(c( \
'rJava', \
'mailR' \
),dependencies=TRUE, repos = 'http://cran.rstudio.com/')"

#Install plotly - takes forever and often fails in non-deterministic ways. 
#All it affects is the 3D-plotting of ancestry. Doesn't affect calculations at all.
# RUN R -e "install.packages(c( \
# 'plotly' \
# ),dependencies=TRUE, repos = 'http://cran.rstudio.com/')"

#configure the shiny_server.conf
RUN sed -i 's=run_as shiny=run_as ubuntu=' /etc/shiny-server/shiny-server.conf && \
sed -i 's=site_dir /srv/shiny-server=site_dir /home/ubuntu/srv/impute-me/=' /etc/shiny-server/shiny-server.conf && \
sed -i 's=log_dir /var/log/shiny-server=log_dir /home/ubuntu/logs/shiny=' /etc/shiny-server/shiny-server.conf && \
sed -i 's=directory_index on=directory_index off=' /etc/shiny-server/shiny-server.conf

#Install cron stuff using the supercronic app (needs root to install, then can run as user)
ENV SUPERCRONIC_URL=https://github.com/aptible/supercronic/releases/download/v0.1.11/supercronic-linux-amd64 \
SUPERCRONIC=supercronic-linux-amd64 \
SUPERCRONIC_SHA1SUM=a2e2d47078a8dafc5949491e5ea7267cc721d67c
RUN curl -fsSLO "$SUPERCRONIC_URL" \
&& echo "${SUPERCRONIC_SHA1SUM}  ${SUPERCRONIC}" | sha1sum -c - \
&& chmod +x "$SUPERCRONIC" \
&& mv "$SUPERCRONIC" "/usr/local/bin/${SUPERCRONIC}" \
&& ln -s "/usr/local/bin/${SUPERCRONIC}" /usr/local/bin/supercronic


#The impute.me server runs as ubuntu default user, so the docker should too
RUN useradd ubuntu
RUN mkdir /home/ubuntu
RUN chown ubuntu /home/ubuntu && \
chown ubuntu /var/lib/shiny-server
USER ubuntu
WORKDIR /home/ubuntu

#Create the directory stucture
RUN mkdir /home/ubuntu/srv && \
mkdir /home/ubuntu/logs && \
mkdir /home/ubuntu/logs/cron_logs && \
mkdir /home/ubuntu/logs/shiny && \
mkdir /home/ubuntu/logs/submission && \
mkdir /home/ubuntu/misc_files && \
mkdir /home/ubuntu/data && \
mkdir /home/ubuntu/programs && \
mkdir /home/ubuntu/prs_dir && \
mkdir /home/ubuntu/imputations

#get impute2
WORKDIR /home/ubuntu/programs
RUN wget https://mathgen.stats.ox.ac.uk/impute/impute_v2.3.2_x86_64_static.tgz && \
gunzip impute_v2.3.2_x86_64_static.tgz && \
tar -xvf impute_v2.3.2_x86_64_static.tar && \
rm impute_v2.3.2_x86_64_static.tar

#get the reference from 1kgenomes
WORKDIR /home/ubuntu/programs
RUN wget https://mathgen.stats.ox.ac.uk/impute/ALL_1000G_phase1integrated_v3_impute.tgz && \
gunzip ALL_1000G_phase1integrated_v3_impute.tgz && \
tar xf ALL_1000G_phase1integrated_v3_impute.tar && \
rm ALL_1000G_phase1integrated_v3_impute.tar  && \
wget https://mathgen.stats.ox.ac.uk/impute/ALL_1000G_phase1integrated_v3_annotated_legends.tgz && \
gunzip ALL_1000G_phase1integrated_v3_annotated_legends.tgz && \
tar xf ALL_1000G_phase1integrated_v3_annotated_legends.tar && \
rm ALL_1000G_phase1integrated_v3_annotated_legends.tar  && \
mv ALL_1000G_phase1integrated_v3_annotated_legends/* ALL_1000G_phase1integrated_v3_impute/  && \
rmdir ALL_1000G_phase1integrated_v3_annotated_legends

#link to the X-chr
WORKDIR /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute
RUN ln -s genetic_map_chrX_nonPAR_combined_b37.txt genetic_map_chrX_combined_b37.txt && \
ln -s ALL_1000G_phase1integrated_v3_chrX_nonPAR_impute.hap.gz ALL_1000G_phase1integrated_v3_chrX_impute.hap.gz && \
ln -s ALL_1000G_phase1integrated_v3_chrX_nonPAR_impute.legend.gz ALL_1000G_phase1integrated_v3_chrX_impute.legend.gz

#get shapeit2 v2.r904 (trying to make it work with shapeit4 as well, but seems difficult)
WORKDIR /home/ubuntu/programs
RUN wget https://mathgen.stats.ox.ac.uk/genetics_software/shapeit/shapeit.v2.r904.glibcv2.17.linux.tar.gz && \
tar -zxvf shapeit.v2.r904.glibcv2.17.linux.tar.gz && \
rm shapeit.v2.r904.glibcv2.17.linux.tar.gz

#Get gtools
WORKDIR /home/ubuntu/programs
RUN wget http://www.well.ox.ac.uk/~cfreeman/software/gwas/gtool_v0.7.5_x86_64.tgz && \
tar zxvf gtool_v0.7.5_x86_64.tgz && \
rm gtool_v0.7.5_x86_64.tgz

#Get plink (1.9)
WORKDIR /home/ubuntu/programs
RUN wget http://s3.amazonaws.com/plink1-assets/plink_linux_x86_64_20200103.zip && \
unzip plink_linux_x86_64_20200103.zip

#Get plink (2.0) #needed for prs freq-correction (and ideally for everything at some point in the future, but it's still too unstable to do that)
WORKDIR /home/ubuntu/programs
RUN mkdir plink2 && \
cd plink2 && \
wget http://s3.amazonaws.com/plink2-assets/alpha2/plink2_linux_avx2.zip && \
unzip plink2_linux_avx2.zip && \
rm plink2_linux_avx2.zip


#Initiate the priority queue
RUN touch /home/ubuntu/misc_files/fast_queue_emails.txt

# Create the configuration file (just blank template. Will run, but won't mail stuff)
RUN echo "maxImputations <- 1" > /home/ubuntu/misc_files/configuration.R && \
echo "maxImputationsInQueue <- 200" >> /home/ubuntu/misc_files/configuration.R  && \
echo "serverRole <- 'Hub'" >> /home/ubuntu/misc_files/configuration.R && \
echo "hubAddress <- 'SOMEIPADRESSMANDATORYFORNODERUNNING'" >> /home/ubuntu/misc_files/configuration.R && \
echo "email_password <- ''" >> /home/ubuntu/misc_files/configuration.R && \
echo "email_address <- ''" >> /home/ubuntu/misc_files/configuration.R && \
echo "routinely_delete_this <- c('link','data')"  >> /home/ubuntu/misc_files/configuration.R && \
echo "paypal <- 'https://www.paypal.me/lfolkersenimputeme/5'" >> /home/ubuntu/misc_files/configuration.R && \
echo "bulk_node_count <- 1"  >> /home/ubuntu/misc_files/configuration.R && \
echo "error_report_mail <- ''" >> /home/ubuntu/misc_files/configuration.R && \
echo "seconds_wait_before_start <- 0" >> /home/ubuntu/misc_files/configuration.R && \
echo "running_as_docker <- TRUE" >> /home/ubuntu/misc_files/configuration.R

#Create the accepted emails list (just put "any")
RUN echo "email   imputeok" > /home/ubuntu/misc_files/accepted_emails.txt && \
echo "any   TRUE" >> /home/ubuntu/misc_files/accepted_emails.txt

#Set ll to give long lists
RUN echo "alias ll='ls -lh'" > /home/ubuntu/.bashrc

#Customize the R opening slightly, by load ingfunctions.R as default.
#not important for pipeline running, but nice to have when operating
#and debugging inside the container.
RUN echo ".First <- function(){" > /home/ubuntu/.Rprofile && \
echo "cat('\n   Welcome to impute.me!\n\n')" >> /home/ubuntu/.Rprofile && \
echo "source('/home/ubuntu/srv/impute-me/functions.R')" >> /home/ubuntu/.Rprofile && \
echo "}" >> /home/ubuntu/.Rprofile

#Write a crontab to open using supercronic, once the docker is running
RUN echo "*/10 * * * * Rscript /home/ubuntu/srv/impute-me/imputeme/imputation_cron_job.R > /home/ubuntu/logs/cron_logs/\`date +\%Y\%m\%d\%H\%M\%S\`-impute-cron.log 2>&1" > /home/ubuntu/misc_files/supercronic.txt && \
echo "*/18 * * * * Rscript /home/ubuntu/srv/impute-me/imputeme/vcf_handling_cron_job.R > /home/ubuntu/logs/cron_logs/\`date +\%Y\%m\%d\%H\%M\%S\`-vcf-cron.log 2>&1" >> /home/ubuntu/misc_files/supercronic.txt

#clone the main github repo
RUN git clone https://github.com/lassefolkersen/impute-me.git /home/ubuntu/srv/impute-me/
  
#reset workdir
WORKDIR /home/ubuntu

#final command
CMD shiny-server


