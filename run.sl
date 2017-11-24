#!/bin/bash
#SBATCH -p debug
#SBATCH -N 1 
#SBATCH -C haswell
#SBATCH -t 00:10:00
#SBATCH -o btio_debug.txt
#DW jobdw capacity=1289GiB access_mode=striped type=scratch pool=sm_pool

RUNS=(1) # Number of runs
OUTDIR=/global/cscratch1/sd/khl7265/FS_64_8M/btio
BBDIR=${DW_JOB_STRIPED}btio
NN=${SLURM_NNODES}
let NP=NN*1
#let NP=NN*32 

DIMX=512
DIMY=512
DIMZ=512
NITR=1 # 1 Itr = 5 GiB

echo "mkdir -p ${OUTDIR}"
mkdir -p ${OUTDIR}
echo "mkdir -p ${BBDIR}"
mkdir -p ${BBDIR}

for i in ${RUNS[@]}
do
    for u in blocking nonblocking
    do
        for v in coll indep
        do
            let IO_METHOD=2
            if [ "x${u}" = "xnonblocking" ]; then
                let IO_METHOD=IO_METHOD+1
            fi
            if [ "x${v}" = "xindep" ]; then
                let IO_METHOD=IO_METHOD+2
            fi
            
            # Ncmpi
            if [ "x${v}" = "xcoll" ]; then
                echo "rm -f ${OUTDIR}/*"
                rm -f ${OUTDIR}/*

                echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
                m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data
                srun -n ${NP} ./btio

                echo "#%$: io_driver: ncmpi"
                echo "#%$: number_of_nodes: ${NN}"
                echo "#%$: number_of_procs: ${NP}"
                echo "#%$: io_mode: ${u}_${v}"

                echo "ls -lah ${OUTDIR}"
                ls -lah ${OUTDIR}

                echo '-----+-----++------------+++++++++--+---'
            fi

            # Bb
            if [ "x${u}" = "xblocking" ] && [ "x${v}" = "xcoll" ]; then
                export PNETCDF_HINTS="nc_dw_driver=enable;nc_dw_del_on_close=disable;nc_dw_overwrite=enable;nc_dw_dirname=${BBDIR}"

                echo "rm -f ${OUTDIR}/*"
                rm -f ${OUTDIR}/*
                echo "rm -f ${BBDIR}/*"
                rm -f ${BBDIR}/*
                
                echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
                m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data
                srun -n ${NP} ./btio

                echo "#%$: io_driver: bb"     
                echo "#%$: number_of_nodes: ${NN}"
                echo "#%$: number_of_procs: ${NP}"
                echo "#%$: io_mode: ${u}_${v}"
                
                echo "ls -lah ${OUTDIR}"
                ls -lah ${OUTDIR}
                if ["${NP}" -lt 33]; then
                    echo "ls -lah ${BBDIR}"
                    ls -lah ${BBDIR}
                fi                     

                echo '-----+-----++------------+++++++++--+---'

                unset PNETCDF_HINTS
            fi

            # Bb_shared
            if [ "x${u}" = "xblocking" ] && [ "x${v}" = "xcoll" ]; then
                export PNETCDF_HINTS="nc_dw_driver=enable;nc_dw_del_on_close=disable;nc_dw_overwrite=enable;nc_dw_sharedlog=enable;nc_dw_dirname=${BBDIR}"

                echo "rm -f ${OUTDIR}/*"
                rm -f ${OUTDIR}/*
                echo "rm -f ${BBDIR}/*"
                rm -f ${BBDIR}/*
                
                echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
                m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data
                srun -n ${NP} ./btio

                echo "#%$: io_driver: bb_shared"     
                echo "#%$: number_of_nodes: ${NN}"
                echo "#%$: number_of_procs: ${NP}"
                echo "#%$: io_mode: ${u}_${v}"
                
                echo "ls -lah ${OUTDIR}"
                ls -lah ${OUTDIR}
                if ["${NP}" -lt 33]; then
                    echo "ls -lah ${BBDIR}"
                    ls -lah ${BBDIR}
                fi

                echo '-----+-----++------------+++++++++--+---'

                unset PNETCDF_HINTS
            fi

            # Staging
            if [ "x${u}" = "xblocking" ] && [ "x${v}" = "xcoll" ]; then
                export stageout_bb_path="${BBDIR}"
                export stageout_pfs_path="${OUTDIR}"
            fi

            echo "rm -f ${OUTDIR}/*"
            rm -f ${OUTDIR}/*
            echo "rm -f ${BBDIR}/*"
            rm -f ${BBDIR}/*
            
            echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${BBDIR} inputbt.m4 > inputbt.data"
                m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${BBDIR} inputbt.m4 > inputbt.data
            srun -n ${NP} ./btio

            echo "#%$: io_driver: stage"
            echo "#%$: number_of_nodes: ${NN}"
            echo "#%$: number_of_procs: ${NP}"
            echo "#%$: io_mode: ${u}_${v}"

            echo "ls -lah ${OUTDIR}"
            ls -lah ${OUTDIR}
            echo "ls -lah ${BBDIR}"
            ls -lah ${BBDIR}
            
            echo '-----+-----++------------+++++++++--+---'
            
            if [ "x${u}" = "xblocking" ] && [ "x${v}" = "xcoll" ]; then
                unset stageout_bb_path
                unset stageout_pfs_path
            fi
        done
    done

    echo '--++---+----+++-----++++---+++--+-++--+---'
done

echo "BB Info: "
module load dws
sessID=$(dwstat sessions | grep $SLURM_JOBID | awk '{print $1}')
echo "session ID is: "${sessID}
instID=$(dwstat instances | grep $sessID | awk '{print $1}')
echo "instance ID is: "${instID}
echo "fragments list:"
echo "frag state instID capacity gran node"
dwstat fragments | grep ${instID}


