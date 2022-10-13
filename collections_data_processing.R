setwd("~/Dropbox/NHMLA/Collections/collections_analytics/")

library(tidyverse)
library(phytools)
library(cowplot)

bird_raw <- read_csv("Ornithology 20181001.csv")

prum_tree <- read.nexus("prum_tree.nex")

peters <- read_csv("PETERS_DATABASE_version_1.csv")

mbf_hm_translation <- read_csv("MBF_to_HM_translation.csv")

peters <- peters %>%
  mutate(Species_sub = gsub(" ","_",Species))

#Grab all Peters genus names, drop any duplicates
peters_names <- peters %>%
  select(Species_sub,Order,Family,Suborder,Subfamily,Species) %>%
  separate(Species,by=" ",into=c("Genus"),extra="drop",remove = F) %>%
  distinct(Genus,.keep_all = T)
  
#Grab all HM genus names, drop any duplicates
hm_names <- mbf_hm_translation %>%
  select(ORDER_HM,FAMILY_HM,Genus_HM,species_HM) %>%
  distinct(Genus_HM,.keep_all = T)

#############Create phylogenies for plotting###################
###Old Peters Way
#Combine Peters tax info with Prum tree labels, change any old names to those in Peters
# prum_labels <- prum_tree$tip.label %>%
#   as.tibble %>%
#   separate(value,into=c("Genus"),extra="drop",remove=F) %>%
#   mutate(Genus = if_else(Genus == "Mesitornis","Mesoenas",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Ardeotis","Choriotis",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Chroicocephalus","Larus",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Phoebastria","Diomedea",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Urocolius","Colius",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Buccanodon","Pogoniulus",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Ibycter","Daptrius",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Caracara","Polyborus",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Barnardius","Platycercus",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Poecile","Parus",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Cryptopipo","Chloropipo",Genus)) %>%
#   left_join(peters_names,by=c("Genus" = "Genus"))
# #Grab representative species from each family
# prum_unique_family <- prum_labels %>%
#   distinct(Family,.keep_all=TRUE) %>%
#   select(value)
# 
# #Create list of species from duplicate families to drop
# prum_unique_family_to_drop <- prum_labels %>%
#   anti_join(prum_unique_family) %>%
#   pull(value)
# 
# #Create family tree
# prum_tree_fam <- drop.tip(prum_tree,prum_unique_family_to_drop)
# 
# #Replace species name tip labels with Families
# prum_tree_fam$tip.label <- prum_tree_fam$tip.label %>%
#   as.tibble %>%
#   separate(value,into=c("Genus"),extra="drop",remove=F) %>%
#   mutate(Genus = if_else(Genus == "Mesitornis","Mesoenas",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Ardeotis","Choriotis",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Chroicocephalus","Larus",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Phoebastria","Diomedea",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Urocolius","Colius",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Buccanodon","Pogoniulus",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Ibycter","Daptrius",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Caracara","Polyborus",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Barnardius","Platycercus",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Poecile","Parus",Genus)) %>%
#   mutate(Genus = if_else(Genus == "Cryptopipo","Chloropipo",Genus)) %>%
#   left_join(peters_names,by=c("Genus" = "Genus")) %>%
#   pull(Family)
# 
# #Grab representative species from each order
# prum_unique_order <- prum_labels %>%
#   distinct(Order,.keep_all=TRUE) %>%
#   select(value)
# 
# #Create list of species from duplicate families to drop
# prum_unique_order_to_drop <- prum_labels %>%
#   anti_join(prum_unique_order) %>%
#   pull(value)
# 
# #Create order tree
# prum_tree_order <- drop.tip(prum_tree,prum_unique_order_to_drop)



############# New way with Howard and Moore ##########
prum_labels <- prum_tree$tip.label %>%
  as.tibble %>%
  separate(value,into=c("Genus"),extra="drop",remove=F) %>%
  mutate(Genus = if_else(Genus == "Megalaima","Psilopogon",Genus)) %>%
  mutate(Genus = if_else(Genus == "Opisthocomus","Ophistocomus",Genus)) %>%
  mutate(Genus = if_else(Genus == "Oceanodroma","Hydrobates",Genus)) %>%
  mutate(Genus = if_else(Genus == "Rynchops","Rhynchops",Genus)) %>%
  left_join(hm_names,by=c("Genus"="Genus_HM"))

#Grab representative species from each family
prum_unique_family <- prum_labels %>%
  distinct(FAMILY_HM,.keep_all=TRUE) %>%
  select(value)

#Create list of species from duplicate families to drop
prum_unique_family_to_drop <- prum_labels %>%
  anti_join(prum_unique_family) %>%
  pull(value)

#Create family tree
prum_tree_fam <- drop.tip(prum_tree,prum_unique_family_to_drop)

#Replace species name tip labels with Families
prum_tree_fam$tip.label <- prum_tree_fam$tip.label %>%
  as.tibble %>%
  separate(value,into=c("Genus"),extra="drop",remove=F) %>%
  mutate(Genus = if_else(Genus == "Megalaima","Psilopogon",Genus)) %>%
  mutate(Genus = if_else(Genus == "Opisthocomus","Ophistocomus",Genus)) %>%
  mutate(Genus = if_else(Genus == "Oceanodroma","Hydrobates",Genus)) %>%
  mutate(Genus = if_else(Genus == "Rynchops","Rhynchops",Genus))%>%
  left_join(hm_names,by=c("Genus"="Genus_HM")) %>%
  pull(FAMILY_HM)


####Create an order tree
#Grab representative species from each order
prum_unique_order<- prum_labels %>%
  distinct(ORDER_HM,.keep_all=TRUE) %>%
  select(value)

#Create list of species from duplicate families to drop
prum_unique_order_to_drop <- prum_labels %>%
  anti_join(prum_unique_order) %>%
  pull(value)

#Create family tree
prum_tree_order <- drop.tip(prum_tree,prum_unique_order_to_drop)

#Replace species name tip labels with Families
prum_tree_order$tip.label <- prum_tree_order$tip.label %>%
  as.tibble %>%
  separate(value,into=c("Genus"),extra="drop",remove=F) %>%
  mutate(Genus = if_else(Genus == "Megalaima","Psilopogon",Genus)) %>%
  mutate(Genus = if_else(Genus == "Opisthocomus","Ophistocomus",Genus)) %>%
  mutate(Genus = if_else(Genus == "Oceanodroma","Hydrobates",Genus)) %>%
  mutate(Genus = if_else(Genus == "Rynchops","Rhynchops",Genus))%>%
  left_join(hm_names,by=c("Genus"="Genus_HM")) %>%
  pull(ORDER_HM)






#Rename columns to remove spaces
bird_raw <- bird_raw %>%
  dplyr::rename(DB_Internal_Rec_No=`DB Internal Rec No`, Catalog_Number = `Catalog Number`, Other_Mus_No = `Other Mus No`, Rec_Type = `Rec Type`, Field_No = `Field No`, LAF_No = `LAF No`, Spec_Nat = `Spec Nat`, Weight_g = `Weight g`, Date_Coll = `Date Coll`, Elevation_m = `Elevation m`, Coll_Remarks = `Coll Remarks`, Tis_Date = `Tis Date`, Tis_Liver = `Tis Liver`,Tis_Heart = `Tis Heart`, Tis_Muscle = `Tis Muscle`, Accn_Number = `Accn Number`, Taxon_Rank = `Taxon Rank`, Province_State = `Province State`, District_Country = `District County`, Nearest_Named_Place = `Nearest Named Place`, Precise_Location = `Precise Location`, Coord_Sys = `Coord Sys`, Coord_Uncertainty_m = `Coord Uncertainty m`, Locality_Remarks = `Locality Remarks`, Latitude_DMS = `Latitude DMS`, Latitude_Dec = `Latitude Dec`, Longitude_DMS = `Longitude DMS`, Longitude_Dec = `Longitude Dec`)

#Translate specimen categories
bird_raw <- bird_raw %>%
  mutate(alcoholic=case_when(Spec_Nat == "AL" ~ "complete",
                             Spec_Nat == "FA" ~ "complete",
                             Spec_Nat == "AC" ~ "partial",
                             Spec_Nat == "AO" ~ "partial",
                             Spec_Nat == "SA" ~ "partial"),
         skeleton=case_when(Spec_Nat == "SN" ~ "complete",
                            Spec_Nat == "FB" ~ "complete",
                            Spec_Nat == "PS" ~ "partial",
                            Spec_Nat == "SO" ~ "partial",
                            Spec_Nat == "SB" ~ "partial",
                            Spec_Nat == "KB" ~ "partial",
                            Spec_Nat == "MS" ~ "partial",
                            Spec_Nat == "SK" ~ "partial"),
         skin=case_when(Spec_Nat == "SS" ~ "study_skin",
                        Spec_Nat == "KB" ~ "study_skin",
                        Spec_Nat == "SB" ~ "study_skin",
                        Spec_Nat == "SA" ~ "study_skin",
                        Spec_Nat == "FB" ~ "flat_skin",
                        Spec_Nat == "FS" ~ "flat_skin",
                        Spec_Nat == "FA" ~ "flat_skin",
                        Spec_Nat == "WO" ~ "flat_skin"),
         body_mount=case_when(Spec_Nat == "BM" ~ "mount",
                              Spec_Nat == "MS" ~ "mount"))

#Translate NHM bird data into modern taxonomy
bird_raw <- bird_raw %>%
  left_join(mbf_hm_translation,by=c("Genus"="Genus_MBF","Species"="Species_MBF")) %>%
  distinct(DB_Internal_Rec_No,.keep_all=T)

#Write file with list of genera according to modern taxonomy
bird_raw %>%
  filter(skin == "study_skin") %>%
  distinct(Genus_HM,.keep_all=T) %>%
  select(ORDER_HM,FAMILY_HM,Genus_HM) %>%
  filter(!is.na(Genus_HM)) %>%
  write_csv("bird_collection_study_skin_genera_HM.csv")

bird_raw %>%
  filter(skin == "study_skin") %>%
  distinct(Genus_HM,species_HM,.keep_all=T) %>%  
  select(ORDER_HM,FAMILY_HM,Genus_HM,species_HM) %>%
  filter(!is.na(Genus_HM)) %>%
  write_csv("bird_collection_study_skin_genus_species_HM.csv")



#How many specimens?
bird_raw %>%
  filter(!is.na(Catalog_Number)) %>%
  summarize(N_Specimens=n())

#How many tissue samples?
bird_raw %>%
  filter(!is.na(LAF_No)) %>%
  summarize(N_Tissues = n())

bird_raw %>%
  group_by(Spec_Nat) %>%
  summarize(N_Specimens = n())

#How many specimens in each category?
bird_raw %>%
  filter(!is.na(skin)) %>%
  summarize(n_ss=n())
bird_raw %>%
  filter(!is.na(skeleton)) %>%
  summarize(n_skels=n())
bird_raw %>%
  filter(!is.na(alcoholic)) %>%
  summarize(n_alcoholic=n())

skin_breakdown <- bird_raw %>%
  filter(!is.na(skin)) %>%
  mutate(ss="skin") %>%
  ggplot(aes(x=ss,fill=skin)) +
  geom_bar(position = position_stack()) +
  theme_bw() +
  scale_fill_manual(
    values = c("#77AADD","#4477AA"),
    name="",
    labels=c("study_skin"="study skin","flat_skin"="flat skin")
  ) +
  xlab("") +
  theme(legend.position="bottom",
        axis.text.x=element_blank(),
        text = element_text(size=18),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  labs(title="skins")

skel_breakdown <- bird_raw %>%
  filter(!is.na(skeleton)) %>%
  mutate(sk="skeleton") %>%
  ggplot(aes(x=sk,fill=factor(skeleton,levels = c("partial","complete")))) +
  geom_bar(position = position_stack()) +
  theme_bw() +
  scale_fill_manual(
    values = c("#88CCAA","#117744"),
    name=""
  ) +
  xlab("") +
  theme(legend.position="bottom",
        axis.text.x=element_blank(),
        text = element_text(size=18),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  labs(title="skeletons")

fluid_breakdown <- bird_raw %>%
  filter(!is.na(alcoholic)) %>%
  mutate(ss="alcoholic") %>%
  ggplot(aes(x=ss,fill=factor(alcoholic,levels = c("partial","complete")))) +
  geom_bar(position = position_stack()) +
  theme_bw() +
  scale_fill_manual(
    values = c("#DDCC77","#999933"),
    name=""
  ) +
  xlab("") +
  theme(legend.position="bottom",
        axis.text.x=element_blank(),
        text = element_text(size=18),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  labs(title="fluids")

tissue_breakdown <- bird_raw %>%
  filter(!is.na(LAF_No)) %>%
  mutate(ss="tissue") %>%
  ggplot(aes(x=ss,fill=ss)) +
  geom_bar(position = position_stack()) +
  theme_bw() +
  scale_fill_manual(
    values = c("#CC6677"),
    name=""
  ) +
  xlab("") +
  theme(legend.position="bottom",
        axis.text.x=element_blank(),
        text = element_text(size=18),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  labs(title="tissues")

pdf("Specimen_Count_Breakdown.pdf",width=8,height=8)
plot_grid(skin_breakdown,skel_breakdown,fluid_breakdown,tissue_breakdown,nrow = 2)
dev.off()



########Specimen acquisition through time
skin_acq <- bird_raw %>%
  separate(Date_Coll, into=c("Day","Month","Year"),remove = F,extra="drop") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Year)) %>%
  filter(Year != 0, Year <= 2018) %>%
  filter(!is.na(skin)) %>%
  group_by(Year) %>%
  summarize(count=n()) %>%
  ggplot(aes(Year,count)) +
  geom_area(fill="#4477AA") +
  theme_bw() +
  theme(text = element_text(size=18)) +
  labs(title="skins") +
  xlim(c(1850,2018))

skel_acq <- bird_raw %>%
    separate(Date_Coll, into=c("Day","Month","Year"),remove = F,extra="drop") %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(!is.na(Year)) %>%
    filter(Year != 0, Year <= 2018) %>%
    filter(!is.na(skeleton)) %>%
    group_by(Year) %>%
    summarize(count=n()) %>%
    ggplot(aes(Year,count)) +
    geom_area(fill="#117733") +
    theme_bw() +
    theme(text = element_text(size=18)) +
    labs(title="skeletons") +
  xlim(c(1850,2018))

fluid_acq <- bird_raw %>%
  separate(Date_Coll, into=c("Day","Month","Year"),remove = F,extra="drop") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Year)) %>%
  filter(Year != 0, Year <= 2018) %>%
  filter(!is.na(alcoholic)) %>%
  group_by(Year) %>%
  summarize(count=n()) %>%
  ggplot(aes(Year,count)) +
  geom_area(fill="#DDCC77") +
  theme_bw() +
  theme(text = element_text(size=18)) +
  labs(title="fluids") +
  xlim(c(1850,2018))

tissue_acq <- bird_raw %>%
  separate(Date_Coll, into=c("Day","Month","Year"),remove = F,extra="drop") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Year)) %>%
  filter(Year != 0, Year <= 2018) %>%
  filter(!is.na(LAF_No)) %>%
  group_by(Year) %>%
  summarize(count=n()) %>%
  ggplot(aes(Year,count)) +
  geom_area(fill="#CC6677") +
  theme_bw() +
  theme(text = element_text(size=18)) +
  labs(title="tissues") +
  xlim(c(1850,2018))

pdf("Specimen_Aquisition_Through_Time_AllPreps.pdf",width=7,height=10)
plot_grid(skin_acq,skel_acq,fluid_acq,tissue_acq,nrow=4,align="v")
dev.off()


bird_raw %>%
  separate(Date_Coll, into=c("Day","Month","Year"),remove = F) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Year)) %>%
  filter(Year != 0, Year <= 2018) %>%
  ggplot(aes(Year)) +
  stat_ecdf() +
  theme_bw() +
  ylab("proportion of specimens")




#How many species in each family and order of birds?
family_sizes <- mbf_hm_translation %>%
  distinct(Genus_HM,species_HM,.keep_all=T) %>%
  group_by(FAMILY_HM) %>%
  summarize(n_sp = n())

order_sizes <- mbf_hm_translation %>%
  distinct(Genus_HM,species_HM,.keep_all=T) %>%
  group_by(ORDER_HM) %>%
  summarize(n_sp = n())


#######Family plots
family_sizes <- mbf_hm_translation %>%
  distinct(Genus_HM,species_HM,.keep_all=T) %>%
  group_by(FAMILY_HM) %>%
  summarize(species = n())

family_count_skin <- bird_raw %>%
  filter(!is.na(skin)) %>%
  distinct(Genus_HM,species_HM,.keep_all=T) %>%
  filter(Species != "Sp", Species != "Sp.") %>%
  group_by(FAMILY_HM) %>%
  summarize(skin=n()) %>%
  filter(!is.na(FAMILY_HM))
family_count_skel <- bird_raw %>%
  filter(!is.na(skeleton)) %>%
  distinct(Genus_HM,species_HM,.keep_all=T) %>%
  filter(Species != "Sp", Species != "Sp.") %>%
  group_by(FAMILY_HM) %>%
  summarize(skeleton=n()) %>%
  filter(!is.na(FAMILY_HM))
family_count_fluid <- bird_raw %>%
  filter(!is.na(alcoholic)) %>%
  distinct(Genus_HM,species_HM,.keep_all=T) %>%
  filter(Species != "Sp", Species != "Sp.") %>%
  group_by(FAMILY_HM) %>%
  summarize(fluid=n()) %>%
  filter(!is.na(FAMILY_HM))
family_count_tissue <- bird_raw %>%
  filter(!is.na(LAF_No)) %>%
  distinct(Genus_HM,species_HM,.keep_all=T) %>%
  filter(Species != "Sp", Species != "Sp.") %>%
  group_by(FAMILY_HM) %>%
  summarize(tissue=n()) %>%
  filter(!is.na(FAMILY_HM))

all_family_counts <- family_sizes %>%
  left_join(family_count_skin) %>%
  left_join(family_count_skel) %>%
  left_join(family_count_fluid) %>%
  left_join(family_count_tissue) %>%
  gather(key="prep",value="N",species,skin,skeleton,fluid,tissue) %>%
  arrange(FAMILY_HM) %>%
  mutate(plot_num=c(rep(1:5,each=200),rep(6,times=195))) 

spec_cols <- c("#4477AA","#117733","#DDCC77","#CC6677","grey")
names(spec_cols) <- c("skin","skeleton","fluid","tissue","species")

family_plot1 <- all_family_counts %>%
  filter(plot_num==1) %>%
  ggplot(aes(FAMILY_HM,N,fill=factor(prep,levels=c("species","skin","skeleton","fluid","tissue")))) +
  geom_bar(stat="identity",position="dodge") +
  scale_fill_manual(values=spec_cols,name="preparation") +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5),
        plot.margin=unit(rep(0,4),"lines")) +
  xlab("")+
  ylim(0,380)
family_plot2 <- all_family_counts %>%
  filter(plot_num==2) %>%
  ggplot(aes(FAMILY_HM,N,fill=factor(prep,levels=c("species","skin","skeleton","fluid","tissue")))) +
  geom_bar(stat="identity",position="dodge") +
  scale_fill_manual(values=spec_cols,name="preparation") +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5),
       plot.margin=unit(rep(0,4),"lines")) +
  xlab("")+
  ylim(0,380)
family_plot3 <- all_family_counts %>%
  filter(plot_num==3) %>%
  ggplot(aes(FAMILY_HM,N,fill=factor(prep,levels=c("species","skin","skeleton","fluid","tissue")))) +
  geom_bar(stat="identity",position="dodge") +
  scale_fill_manual(values=spec_cols,name="preparation") +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5),
        plot.margin=unit(rep(0,4),"lines")) +
  xlab("")+
  ylim(0,380)
family_plot4 <- all_family_counts %>%
  filter(plot_num==4) %>%
  ggplot(aes(FAMILY_HM,N,fill=factor(prep,levels=c("species","skin","skeleton","fluid","tissue")))) +
  geom_bar(stat="identity",position="dodge") +
  scale_fill_manual(values=spec_cols,name="preparation") +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5),
        plot.margin=unit(rep(0,4),"lines")) +
  xlab("")+
  ylim(0,380)
family_plot5 <- all_family_counts %>%
  filter(plot_num==5) %>%
  ggplot(aes(FAMILY_HM,N,fill=factor(prep,levels=c("species","skin","skeleton","fluid","tissue")))) +
  geom_bar(stat="identity",position="dodge") +
  scale_fill_manual(values=spec_cols,name="preparation") +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5),
        plot.margin=unit(rep(0,4),"lines")) +
  xlab("")+
  ylim(0,380)
family_plot6 <- all_family_counts %>%
  filter(plot_num==6) %>%
  ggplot(aes(FAMILY_HM,N,fill=factor(prep,levels=c("species","skin","skeleton","fluid","tissue")))) +
  geom_bar(stat="identity",position="dodge") +
  scale_fill_manual(values=spec_cols,name="preparation") +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5),
        plot.margin=unit(rep(0,4),"lines")) +
  xlab("family")+
  ylim(0,380)


pdf("family_counts_all.pdf",width=10,height=18)
plot_grid(family_plot1,family_plot2,family_plot3,family_plot4,family_plot5,family_plot6,nrow=6)
dev.off()



ssp_family_prop <- ss_family_count %>%
  full_join(family_sizes) %>%
  mutate(prop_ss = n_ss/n_sp) %>%
  arrange(desc(prop_ss)) %>%
  mutate(prop_ss = if_else(prop_ss>1,1,prop_ss)) %>%
  mutate(prop_ss = if_else(is.na(prop_ss),0,prop_ss)) %>%
  mutate(prop_not_ss = 1-prop_ss) %>%
  mutate(missing_ss_sp = n_sp-n_ss)

ssp_family_prop_df <- ssp_family_prop %>%
  select(prop_ss,prop_not_ss,FAMILY_HM) %>%
  as.data.frame %>%
  column_to_rownames("FAMILY_HM")

pdf("Family_SS_Prop_plot.pdf")
plotTree.barplot(prum_tree_fam,ssp_family_prop_df[prum_tree_fam$tip.label,],args.plotTree = list(fsize=0.3),lwd=2)
dev.off()


ssp_family_Nsp_df <- ssp_family_prop %>%
  mutate(n_missing=n_sp-n_ss) %>%
  mutate(n_missing=if_else(n_missing<0,0,as.double(n_missing))) %>%
  select(n_ss,n_missing,FAMILY_HM) %>%
  as.data.frame %>%
  column_to_rownames("FAMILY_HM")

pdf("Family_SS_N_SP_plot.pdf")
plotTree.barplot(prum_tree_fam,ssp_family_Nsp_df,args.plotTree = list(fsize=0.3),lwd=2)
dev.off()



#######Order plots

spec_cols <- c("#4477AA","#117733","#DDCC77","#CC6677")
names(spec_cols) <- c("skin","skeleton","alcoholic","tissue")

ss_order_count <- bird_raw %>%
  filter(!is.na(skin)) %>%
  distinct(Genus_HM,species_HM,.keep_all=T) %>%
  group_by(ORDER_HM) %>%
  summarize(n_ss=n())
ss_order_prop <- ss_order_count %>%
  full_join(order_sizes,by=c("ORDER_HM"="ORDER_HM")) %>%
  mutate(prop_ss = n_ss/n_sp) %>%
  mutate(prop_ss = if_else(prop_ss>1,1,prop_ss)) %>%
  mutate(prop_ss = if_else(is.na(prop_ss),0,prop_ss)) %>%
  mutate(prop_not_ss = 1-prop_ss)

ss_order_prop_df <- ss_order_prop %>%
  select(prop_ss,prop_not_ss,ORDER_HM) %>%
  filter(!is.na(ORDER_HM)) %>%
  as.data.frame %>%
  column_to_rownames("ORDER_HM")

pdf("Order_SS_Prop_plot.pdf",width=3,height=7)
plotTree.barplot(prum_tree_order,ss_order_prop_df,args.plotTree = list(fsize=0.5),lwd=2,args.barplot = list(col=c(spec_cols["skin"],"grey94")))
dev.off()

#Plot skeletons
skel_order_count <- bird_raw %>%
  filter(!is.na(skeleton)) %>%
  distinct(Genus_HM,species_HM,.keep_all=T) %>%
  group_by(ORDER_HM) %>%
  summarize(n_ss=n())
skel_order_prop <- skel_order_count %>%
  full_join(order_sizes,by=c("ORDER_HM"="ORDER_HM")) %>%
  mutate(prop_ss = n_ss/n_sp) %>%
  mutate(prop_ss = if_else(prop_ss>1,1,prop_ss)) %>%
  mutate(prop_ss = if_else(is.na(prop_ss),0,prop_ss)) %>%
  mutate(prop_not_ss = 1-prop_ss)

skel_order_prop_df <- skel_order_prop %>%
  select(prop_ss,prop_not_ss,ORDER_HM) %>%
  filter(!is.na(ORDER_HM)) %>%
  as.data.frame %>%
  column_to_rownames("ORDER_HM")

pdf("Order_Skel_Prop_plot.pdf",width=3,height=7)
plotTree.barplot(prum_tree_order,skel_order_prop_df,args.plotTree = list(fsize=0.5),lwd=2,args.barplot = list(col=c(spec_cols["skeleton"],"grey94")))
dev.off()


#Plot alcoholics
alc_order_count <- bird_raw %>%
  filter(!is.na(alcoholic)) %>%
  distinct(Genus_HM,species_HM,.keep_all=T) %>%
  group_by(ORDER_HM) %>%
  summarize(n_ss=n())
alc_order_prop <- alc_order_count %>%
  full_join(order_sizes,by=c("ORDER_HM"="ORDER_HM")) %>%
  mutate(prop_ss = n_ss/n_sp) %>%
  mutate(prop_ss = if_else(prop_ss>1,1,prop_ss)) %>%
  mutate(prop_ss = if_else(is.na(prop_ss),0,prop_ss)) %>%
  mutate(prop_not_ss = 1-prop_ss)

alc_order_prop_df <- alc_order_prop %>%
  select(prop_ss,prop_not_ss,ORDER_HM) %>%
  filter(!is.na(ORDER_HM)) %>%
  as.data.frame %>%
  column_to_rownames("ORDER_HM")

pdf("Order_Alc_Prop_plot.pdf",width=3,height=7)
plotTree.barplot(prum_tree_order,alc_order_prop_df,args.plotTree = list(fsize=0.5),lwd=2,args.barplot = list(col=c(spec_cols["alcoholic"],"grey94")))
dev.off()


#Plot tissues
tis_order_count <- bird_raw %>%
  filter(!is.na(LAF_No)) %>%
  distinct(Genus_HM,species_HM,.keep_all=T) %>%
  group_by(ORDER_HM) %>%
  summarize(n_ss=n())
tis_order_prop <- tis_order_count %>%
  full_join(order_sizes,by=c("ORDER_HM"="ORDER_HM")) %>%
  mutate(prop_ss = n_ss/n_sp) %>%
  mutate(prop_ss = if_else(prop_ss>1,1,prop_ss)) %>%
  mutate(prop_ss = if_else(is.na(prop_ss),0,prop_ss)) %>%
  mutate(prop_not_ss = 1-prop_ss)

tis_order_prop_df <- tis_order_prop %>%
  select(prop_ss,prop_not_ss,ORDER_HM) %>%
  filter(!is.na(ORDER_HM)) %>%
  as.data.frame %>%
  column_to_rownames("ORDER_HM")

pdf("Order_Tis_Prop_plot.pdf",width=3,height=7)
plotTree.barplot(prum_tree_order,tis_order_prop_df,args.plotTree = list(fsize=0.5),lwd=2,args.barplot = list(col=c(spec_cols["tissue"],"grey94")))
dev.off()






#Plot the actual order numbers
ssp_order_Nsp_df <- ss_order_prop %>%
  filter(!is.na(ORDER_HM)) %>%
  mutate(n_missing=n_sp-n_ss) %>%
  mutate(n_missing=if_else(n_missing<0,0,as.double(n_missing))) %>%
  select(n_ss,n_missing,ORDER_HM) %>%
  as.data.frame %>%
  column_to_rownames("ORDER_HM")

pdf("Order_SS_N_SP_plot.pdf")
plotTree.barplot(prum_tree_order,ssp_order_Nsp_df,args.plotTree = list(fsize=0.7),lwd=2)
dev.off()



skel_counts <- bird_raw %>%
  filter(!is.na(skeleton)) %>%
  group_by(FAMILY_HM,Genus_HM,species_HM) %>%
  summarize(n_sp = n()) %>%
  arrange(desc(n_sp))





bird_num_country <- bird_raw %>%
  group_by(Country) %>%
  summarize(N = n()) %>%
  arrange(desc(N)) %>%
  mutate(Country = if_else(Country == "UNITED STATES","USA",Country)) %>%
  mutate(Country = if_else(Country == "CENTRAL AFRICAN REP", "CENTRAL AFRICAN REPUBLIC",Country)) %>%
  mutate(Country = if_else(Country == "CONGO REPUBLIC", "DEMOCRATIC REPUBLIC OF THE CONGO",Country))  %>%
  mutate(Country = if_else(Country == "SOUTH WEST AFRICA", "NAMIBIA",Country)) %>%
  mutate(Country = if_else(Country == "UNITED KINGDOM", "UK",Country)) %>%
  mutate(Country = if_else(Country == "SURINAM", "SURINAME",Country)) %>%
  mutate(Country = if_else(Country == "RHODESIA", "ZIMBABWE",Country)) %>%
  mutate(Country = if_else(Country == "CAMEROUN", "CAMEROON",Country)) %>%
  mutate(Country = if_else(Country == "BURMA", "MYANMAR",Country)) %>%
  mutate(Country = if_else(Country == "REP OF CONGO", "REPUBLIC OF CONGO",Country)) %>%
  mutate(Country = if_else(Country == "USSR", "RUSSIA",Country)) %>%
  mutate(Country = if_else(Country == "ANTARCTIC PENINSULA", "ANTARCTICA",Country)) %>%
  mutate(Country = if_else(Country == "NORTH VIETNAM", "VIETNAM",Country)) %>%
  mutate(Country = if_else(Country == "GALAPAGOS IS", "ECUADOR",Country)) %>%
  mutate(Country = if_else(Country == "SOUTH VIETNAM", "VIETNAM",Country)) %>%
  mutate(Country = if_else(Country == "NEW GUINEA", "PAPUA NEW GUINEA",Country)) %>%
  mutate(Country = if_else(Country == "BISMARCK ARCH", "PAPUA NEW GUINEA",Country)) %>%
  mutate(Country = if_else(Country == "HAWAIIAN IS", "USA",Country)) %>%
  mutate(Country = if_else(Country == "SIAM (=THAILAND)", "THAILAND",Country)) %>%
  mutate(Country = if_else(Country == "HOLLAND", "NETHERLANDS",Country)) %>%
  mutate(Country = if_else(Country == "ENGLAND", "UK",Country))


#Create a tibble of localities with more than 40 species not depicted on map
bird_num_ocean <- bird_raw %>%
  filter(is.na(Country)) %>%
  filter(!is.na(Ocean)) %>%
  group_by(Ocean) %>%
  summarize(N = n()) %>%
  arrange(desc(N)) %>%
  select(region=Ocean,N) %>%
  mutate(region=str_to_title(region)) %>%
  filter(N>40) %>%
  mutate(region=if_else(region=="North Pacific","North Pacific Ocean",region)) %>%
  mutate(region=if_else(region=="South Pacific","South Pacific Ocean",region)) %>%
  print(n=30)

#What is rep for specimens with no country?
bird_no_country <- bird_raw %>%
  filter(is.na(Country)) %>%
  group_by(Continent) %>%
  summarize(N = n()) %>%
  arrange(desc(N))

#Check out grouping by country
bird_raw %>%
  group_by(Country) %>%
  summarize(N=n()) %>%
  arrange(desc(N))

#Places on the map with no specimens
map_data('world') %>%
  mutate(region = toupper(region)) %>%
  full_join(bird_num_country,by=c("region" = "Country")) %>%
  filter(is.na(N)) %>%
  distinct(region)

#Which regions don't have a match for map data?
not_on_map <- map_data('world') %>%
  as.tibble %>%
  mutate(region = toupper(region)) %>%
  full_join(bird_num_country,by=c("region" = "Country")) %>%
  filter(!is.na(N), is.na(long)) %>%
  filter(!is.na(region)) %>%
  mutate(region = str_to_title(region)) %>%
  select(region,N) %>%
  filter(N>40)

#Create combo tibble of regions to depict not represented on map
counts_sep_plot <- bind_rows(bird_num_ocean,not_on_map) %>%
  arrange((N))

#Prep map data for plotting
map_data_upper <- map_data('world') %>%
  mutate(region = toupper(region)) %>%
  left_join(bird_num_country,by=c("region" = "Country"))

#Raw values
gg <- ggplot() + geom_map(data=map_data_upper, map=map_data_upper, aes(map_id=region, x=long, y = lat, fill=N))
gg <- gg + scale_fill_gradient(low = "white", high = "navy", guide = "colourbar", na.value="white")
gg+ coord_equal()

#log-transformed values
gg <- ggplot() + geom_map(data=map_data_upper, map=map_data_upper, aes(map_id=region, x=long, y = lat, fill=log10(N)))
gg <- gg + scale_fill_gradient(low = "white", high = "black", guide = "colourbar", na.value="grey97", name="# specimens",limits=c(1,5))
gg+ coord_equal() +
  theme_map()
ggsave("log_scale_specimen_world_map.pdf",width=16,height=8)

#Plot regions not shown on plot
counts_sep_plot %>%
  mutate(dummy=10) %>%
  ggplot(aes(dummy,factor(region,levels=(counts_sep_plot %>% pull(region))),col=log10(N))) +
  geom_count(stat="identity",size=8,shape="square") + 
  scale_color_gradient(low = "white", high = "black", guide = "colourbar", na.value="grey97", name="# specimens",limits=c(1,5)) +
  xlab("") +
  ylab("") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size=18))
ggsave("additional_localities_specimen_density.pdf",width=4.5,height=6)


#Plot by specimen types
bird_num_country_skin <- bird_raw %>%
  filter(!is.na(skin)) %>%
  group_by(Country) %>%
  summarize(N = n()) %>%
  arrange(desc(N)) %>%
  mutate(Country = if_else(Country == "UNITED STATES","USA",Country)) %>%
  mutate(Country = if_else(Country == "CENTRAL AFRICAN REP", "CENTRAL AFRICAN REPUBLIC",Country)) %>%
  mutate(Country = if_else(Country == "CONGO REPUBLIC", "DEMOCRATIC REPUBLIC OF THE CONGO",Country))  %>%
  mutate(Country = if_else(Country == "SOUTH WEST AFRICA", "NAMIBIA",Country)) %>%
  mutate(Country = if_else(Country == "UNITED KINGDOM", "UK",Country)) %>%
  mutate(Country = if_else(Country == "SURINAM", "SURINAME",Country)) %>%
  mutate(Country = if_else(Country == "RHODESIA", "ZIMBABWE",Country)) %>%
  mutate(Country = if_else(Country == "CAMEROUN", "CAMEROON",Country)) %>%
  mutate(Country = if_else(Country == "BURMA", "MYANMAR",Country)) %>%
  mutate(Country = if_else(Country == "REP OF CONGO", "REPUBLIC OF CONGO",Country)) %>%
  mutate(Country = if_else(Country == "USSR", "RUSSIA",Country)) %>%
  mutate(Country = if_else(Country == "ANTARCTIC PENINSULA", "ANTARCTICA",Country)) %>%
  mutate(Country = if_else(Country == "NORTH VIETNAM", "VIETNAM",Country)) %>%
  mutate(Country = if_else(Country == "GALAPAGOS IS", "ECUADOR",Country)) %>%
  mutate(Country = if_else(Country == "SOUTH VIETNAM", "VIETNAM",Country)) %>%
  mutate(Country = if_else(Country == "NEW GUINEA", "PAPUA NEW GUINEA",Country)) %>%
  mutate(Country = if_else(Country == "BISMARCK ARCH", "PAPUA NEW GUINEA",Country)) %>%
  mutate(Country = if_else(Country == "HAWAIIAN IS", "USA",Country)) %>%
  mutate(Country = if_else(Country == "SIAM (=THAILAND)", "THAILAND",Country)) %>%
  mutate(Country = if_else(Country == "HOLLAND", "NETHERLANDS",Country)) %>%
  mutate(Country = if_else(Country == "ENGLAND", "UK",Country))

map_data_upper_skin <- map_data('world') %>%
  mutate(region = toupper(region)) %>%
  left_join(bird_num_country_skin,by=c("region" = "Country"))

#log-transformed values
ggplot() + geom_map(data=map_data_upper_skin, map=map_data_upper_skin, aes(map_id=region, x=long, y = lat, fill=log10(N))) + 
  scale_fill_gradient(low = "white", high = "#4477AA", guide = "colourbar", na.value="grey97", name="",limits=c(1,5)) + 
  coord_equal() +
  theme_map()
ggsave("log_scale_specimen_world_map_skin.pdf",width=8,height=4)


#Skeletons
bird_num_country_skel <- bird_raw %>%
  filter(!is.na(skeleton)) %>%
  group_by(Country) %>%
  summarize(N = n()) %>%
  arrange(desc(N)) %>%
  mutate(Country = if_else(Country == "UNITED STATES","USA",Country)) %>%
  mutate(Country = if_else(Country == "CENTRAL AFRICAN REP", "CENTRAL AFRICAN REPUBLIC",Country)) %>%
  mutate(Country = if_else(Country == "CONGO REPUBLIC", "DEMOCRATIC REPUBLIC OF THE CONGO",Country))  %>%
  mutate(Country = if_else(Country == "SOUTH WEST AFRICA", "NAMIBIA",Country)) %>%
  mutate(Country = if_else(Country == "UNITED KINGDOM", "UK",Country)) %>%
  mutate(Country = if_else(Country == "SURINAM", "SURINAME",Country)) %>%
  mutate(Country = if_else(Country == "RHODESIA", "ZIMBABWE",Country)) %>%
  mutate(Country = if_else(Country == "CAMEROUN", "CAMEROON",Country)) %>%
  mutate(Country = if_else(Country == "BURMA", "MYANMAR",Country)) %>%
  mutate(Country = if_else(Country == "REP OF CONGO", "REPUBLIC OF CONGO",Country)) %>%
  mutate(Country = if_else(Country == "USSR", "RUSSIA",Country)) %>%
  mutate(Country = if_else(Country == "ANTARCTIC PENINSULA", "ANTARCTICA",Country)) %>%
  mutate(Country = if_else(Country == "NORTH VIETNAM", "VIETNAM",Country)) %>%
  mutate(Country = if_else(Country == "GALAPAGOS IS", "ECUADOR",Country)) %>%
  mutate(Country = if_else(Country == "SOUTH VIETNAM", "VIETNAM",Country)) %>%
  mutate(Country = if_else(Country == "NEW GUINEA", "PAPUA NEW GUINEA",Country)) %>%
  mutate(Country = if_else(Country == "BISMARCK ARCH", "PAPUA NEW GUINEA",Country)) %>%
  mutate(Country = if_else(Country == "HAWAIIAN IS", "USA",Country)) %>%
  mutate(Country = if_else(Country == "SIAM (=THAILAND)", "THAILAND",Country)) %>%
  mutate(Country = if_else(Country == "HOLLAND", "NETHERLANDS",Country)) %>%
  mutate(Country = if_else(Country == "ENGLAND", "UK",Country))

map_data_upper_skel <- map_data('world') %>%
  mutate(region = toupper(region)) %>%
  left_join(bird_num_country_skel,by=c("region" = "Country"))

#log-transformed values
ggplot() + geom_map(data=map_data_upper_skel, map=map_data_upper_skel, aes(map_id=region, x=long, y = lat, fill=log10(N))) + 
  scale_fill_gradient(low = "white", high = "#117733", guide = "colourbar", na.value="grey97", name="",limits=c(1,5)) + 
  coord_equal() +
  theme_map()
ggsave("log_scale_specimen_world_map_skel.pdf",width=8,height=4)


#Plot by specimen types
bird_num_country_fluid <- bird_raw %>%
  filter(!is.na(alcoholic)) %>%
  group_by(Country) %>%
  summarize(N = n()) %>%
  arrange(desc(N)) %>%
  mutate(Country = if_else(Country == "UNITED STATES","USA",Country)) %>%
  mutate(Country = if_else(Country == "CENTRAL AFRICAN REP", "CENTRAL AFRICAN REPUBLIC",Country)) %>%
  mutate(Country = if_else(Country == "CONGO REPUBLIC", "DEMOCRATIC REPUBLIC OF THE CONGO",Country))  %>%
  mutate(Country = if_else(Country == "SOUTH WEST AFRICA", "NAMIBIA",Country)) %>%
  mutate(Country = if_else(Country == "UNITED KINGDOM", "UK",Country)) %>%
  mutate(Country = if_else(Country == "SURINAM", "SURINAME",Country)) %>%
  mutate(Country = if_else(Country == "RHODESIA", "ZIMBABWE",Country)) %>%
  mutate(Country = if_else(Country == "CAMEROUN", "CAMEROON",Country)) %>%
  mutate(Country = if_else(Country == "BURMA", "MYANMAR",Country)) %>%
  mutate(Country = if_else(Country == "REP OF CONGO", "REPUBLIC OF CONGO",Country)) %>%
  mutate(Country = if_else(Country == "USSR", "RUSSIA",Country)) %>%
  mutate(Country = if_else(Country == "ANTARCTIC PENINSULA", "ANTARCTICA",Country)) %>%
  mutate(Country = if_else(Country == "NORTH VIETNAM", "VIETNAM",Country)) %>%
  mutate(Country = if_else(Country == "GALAPAGOS IS", "ECUADOR",Country)) %>%
  mutate(Country = if_else(Country == "SOUTH VIETNAM", "VIETNAM",Country)) %>%
  mutate(Country = if_else(Country == "NEW GUINEA", "PAPUA NEW GUINEA",Country)) %>%
  mutate(Country = if_else(Country == "BISMARCK ARCH", "PAPUA NEW GUINEA",Country)) %>%
  mutate(Country = if_else(Country == "HAWAIIAN IS", "USA",Country)) %>%
  mutate(Country = if_else(Country == "SIAM (=THAILAND)", "THAILAND",Country)) %>%
  mutate(Country = if_else(Country == "HOLLAND", "NETHERLANDS",Country)) %>%
  mutate(Country = if_else(Country == "ENGLAND", "UK",Country))

map_data_upper_fluid <- map_data('world') %>%
  mutate(region = toupper(region)) %>%
  left_join(bird_num_country_fluid,by=c("region" = "Country"))

#log-transformed values
ggplot() + geom_map(data=map_data_upper_fluid, map=map_data_upper_fluid, aes(map_id=region, x=long, y = lat, fill=log10(N))) + 
  scale_fill_gradient(low = "white", high = "#DDCC77", guide = "colourbar", na.value="grey97", name="",limits=c(1,5)) + 
  coord_equal() +
  theme_map()
ggsave("log_scale_specimen_world_map_fluid.pdf",width=8,height=4)



#Plot by specimen types
bird_num_country_tissue <- bird_raw %>%
  filter(!is.na(LAF_No)) %>%
  group_by(Country) %>%
  summarize(N = n()) %>%
  arrange(desc(N)) %>%
  mutate(Country = if_else(Country == "UNITED STATES","USA",Country)) %>%
  mutate(Country = if_else(Country == "CENTRAL AFRICAN REP", "CENTRAL AFRICAN REPUBLIC",Country)) %>%
  mutate(Country = if_else(Country == "CONGO REPUBLIC", "DEMOCRATIC REPUBLIC OF THE CONGO",Country))  %>%
  mutate(Country = if_else(Country == "SOUTH WEST AFRICA", "NAMIBIA",Country)) %>%
  mutate(Country = if_else(Country == "UNITED KINGDOM", "UK",Country)) %>%
  mutate(Country = if_else(Country == "SURINAM", "SURINAME",Country)) %>%
  mutate(Country = if_else(Country == "RHODESIA", "ZIMBABWE",Country)) %>%
  mutate(Country = if_else(Country == "CAMEROUN", "CAMEROON",Country)) %>%
  mutate(Country = if_else(Country == "BURMA", "MYANMAR",Country)) %>%
  mutate(Country = if_else(Country == "REP OF CONGO", "REPUBLIC OF CONGO",Country)) %>%
  mutate(Country = if_else(Country == "USSR", "RUSSIA",Country)) %>%
  mutate(Country = if_else(Country == "ANTARCTIC PENINSULA", "ANTARCTICA",Country)) %>%
  mutate(Country = if_else(Country == "NORTH VIETNAM", "VIETNAM",Country)) %>%
  mutate(Country = if_else(Country == "GALAPAGOS IS", "ECUADOR",Country)) %>%
  mutate(Country = if_else(Country == "SOUTH VIETNAM", "VIETNAM",Country)) %>%
  mutate(Country = if_else(Country == "NEW GUINEA", "PAPUA NEW GUINEA",Country)) %>%
  mutate(Country = if_else(Country == "BISMARCK ARCH", "PAPUA NEW GUINEA",Country)) %>%
  mutate(Country = if_else(Country == "HAWAIIAN IS", "USA",Country)) %>%
  mutate(Country = if_else(Country == "SIAM (=THAILAND)", "THAILAND",Country)) %>%
  mutate(Country = if_else(Country == "HOLLAND", "NETHERLANDS",Country)) %>%
  mutate(Country = if_else(Country == "ENGLAND", "UK",Country))

map_data_upper_tissue <- map_data('world') %>%
  mutate(region = toupper(region)) %>%
  left_join(bird_num_country_tissue,by=c("region" = "Country"))

#log-transformed values
ggplot() + geom_map(data=map_data_upper_tissue, map=map_data_upper_tissue, aes(map_id=region, x=long, y = lat, fill=log10(N))) + 
  scale_fill_gradient(low = "white", high = "#CC6677", guide = "colourbar", na.value="grey97", name="",limits=c(1,5)) + 
  coord_equal() +
  theme_map()
ggsave("log_scale_specimen_world_map_tissue.pdf",width=8,height=4)




######Plot US densities by state
state_counts <- bird_raw %>%
  filter(Country == "UNITED STATES") %>%
  group_by(Province_State) %>%
  summarize(N = n()) %>%
  arrange(desc(N)) %>%
  print(n=50)

map_data_upper_states <- map_data('state') %>%
  mutate(region = toupper(region)) %>%
  left_join(state_counts,by=c("region" = "Province_State"))

#log-transformed values
ggplot() + geom_map(data=map_data_upper_states, map=map_data_upper_states, aes(map_id=region, x=long, y = lat, fill=log10(N))) + 
  scale_fill_gradient(low = "white", high = "black", guide = "colourbar", na.value="grey97", name="",limits=c(1,5)) + 
  coord_equal() +
  theme_map()
ggsave("log_scale_specimen_US_mainland.pdf",width=8,height=5)







#Skeleton counts by species
bird_raw %>%
  filter(!is.na(skeleton)) %>%
  group_by(Genus_HM,species_HM) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  print(n=30)
  





#Tissues
bird_tissues <- bird_raw %>%
  filter(!is.na(LAF_No)) 

bird_tissues %>%
  group_by(Genus,Species) %>%
  summarize(n_specimens = n()) %>%
  arrange(desc(n_specimens)) %>%
  print(n=50)


bird_tissues %>%
  filter(Genus == "Carpodacus", Species == "mexicanus") %>%
  select(Nearest_Named_Place,Precise_Location) %>%
  print(n=35)

bird_tissues %>%
  filter(Genus == "Carpodacus", Species == "mexicanus") %>%
  write_csv("Carpodacus_mexicanus_tissues.csv")

bird_tissues %>%
  filter(Genus == "Psaltriparus", Species == "minimus") %>%
  select(Nearest_Named_Place,Precise_Location) %>%
  print(n=35)

bird_tissues %>%
  filter(Genus == "Psaltriparus", Species == "minimus") %>%
  write_csv("Psaltriparus_minimus_tissues.csv")

theme_set(theme_bw(16))
LA <- get_map('los angeles')
qmap('los angeles',zoom=9)
