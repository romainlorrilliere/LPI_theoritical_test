#!/bin/sh
# Les lignes commançant par #SBATCH sont interprétées par SLURM.

# Nombre de Noeud
#SBATCH --nodes=1

# Durée !!!!!!!!
#SBATCH --time=120-00:00:00

# Nombre de processeur par noeud
#SBATCH --ntasks-per-node=1

# Nom du job
#SBATCH --job-name=sO_EPfull

# Quantité de RAM par noeud
#SBATCH --mem=2G

# Quel type de machine demander (type_1 ou type_2) # Ne rien changer
#SBATCH --partition=type_2
# --nodelist=node-28

# Chargement des modules
 module load userspace/tr17.10
 module load biology
 module load udunits
# 
module load lapack/3.7.1
module load gcc/7.2.0
# 
 module load jags/gcc72/4.3.0
 module load proj/8.0.0
 module load geos/3.10.0
 module load gdal/3.2.2
# 
module load R/4.2.2

# Commandes et scripte à lancer
# Rscript cl_lpi_test_1.r
Rscript cl_test_1.r
