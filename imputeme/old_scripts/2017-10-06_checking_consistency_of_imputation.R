

new<-read.table("C:/Users/FOLK/Documents/Work/Bioinformatics/new/id_823J76y50.cached.all_gwas.gz",stringsAsFactors = F, header=T,row.names=1)
old<-read.table("C:/Users/FOLK/Documents/Work/Bioinformatics/old/id_823J76y50.cached.all_gwas.gz",stringsAsFactors = F, header=T,row.names=1)


r<-intersect(rownames(new),rownames(old))


d<-data.frame(row.names=r,new=new[r,],old=old[r,])


#NA-mismatch
nrow(d)
12571

sum(is.na(d[,"new"]))
248

sum(is.na(d[,"old"]))
148

sum(apply(is.na(d),1,sum)==2)
75

d[apply(is.na(d),1,sum)==1,]
#seems fairly random when it is NA in one place and when it is in another





#outright-mismatch
d1<-d[!is.na(d[,"new"]) & !is.na(d[,"old"]),]


d2<-d1[d1[,"new"] != d1[,"old"],]
nrow(d2)
455

#that's too much


# we investigate one (rs5760748)
#this is how it looks in new in gen
# 22 rs5760747 25334869 G A 1 0 0
# --- rs144293611 25334885 G A 1 0 0
# --- rs5760748 25334977 C T 1 0 0
# --- rs11913792 25335033 C T 1 0 0
# --- rs193227671 25335058 G C 1 0 0
# --- rs184935645 25335089 G A 1 0 0
# 22 rs5996794 25335093 C T 1 0 0

#this is how it looks in old in gen
# --- rs5760747 25334869 G A 0.003 0.996 0
# --- rs144293611 25334885 G A 1 0 0
# --- rs5760748 25334977 C T 0.003 0.996 0
# --- rs11913792 25335033 C T 1 0 0
# --- rs193227671 25335058 G C 1 0 0
# --- rs184935645 25335089 G A 1 0 0
# --- rs5996794 25335093 C T 0.003 0.996 0



#so it seems like two more are measured in the new?! Check rs5760747 and rs5996794

grep rs5760748 id_823J76y50_raw_data.txt 
grep 25334869 id_823J76y50_raw_data.txt 
grep 25335093 id_823J76y50_raw_data.txt 


#OK - so the verdict here is that they are NOT measured in the new data set. Maybe they have been force inserted somehow in the merge step. In other words - we need to do more checkups for that :-(






#2017-10-09 continuing evaluation after re-run with deletion filters off

# We look at the rs5760747 rs5996794 and rs5760748 in chr22 (the last SNP is just a grep-working-check)
# 

grep 'rs5760747\|rs5996794\|rs5760748\|rs13328684' step_1_id_823J76y50.map
#none found in input

grep 'rs5760747\|rs5996794\|rs5760748\|rs147574439' step_2_id_823J76y50_chr22.bim 
#none found in step2

#check this once it is ready - presumably it will give that at least 2 or 3 are found - given that we check the fam
grep 'rs5760747\|rs5996794\|rs5760748\|rs147574439' step_2m_chr22.bim
