#!/bin/bash
#COBALT -t 3
#COBALT -n 1
#COBALT --attrs mcdram=cache:numa=quad:ssds=required:ssd_size=16
#COBALT -A ecp-testbed-01
#COBALT -q debug-flat-quad
#COBALT -o btio_1.txt
#COBALT -e btio_1.txt

echo "Starting Cobalt job script"
RUNS=(1) # Number of runs
OUTDIR=/projects/radix-io/khou/FS_64_8M/ior
BBDIR=/local/scratch
PPN=4
NN=${COBALT_JOBSIZE}
let NP=NN*PPN
DIMX=512
DIMY=512
DIMZ=512
NITR=1 # 1 Itr = 5 GiB

export n_nodes=$COBALT_JOBSIZE
export n_mpi_ranks_per_node=${PPN}
export n_mpi_ranks=$(($n_nodes * $n_mpi_ranks_per_node))
export n_openmp_threads_per_rank=1
export n_hyperthreads_per_core=1

echo "mkdir -p ${OUTDIR}"
mkdir -p ${OUTDIR}

for i in ${RUNS[@]}
do
    for u in blocking nonblocking
    do
        for v in coll indep
        do
            COLL=
            NB=
            if [ "x${v}" = "xcoll" ]; then
                COLL=-c
            fi
            if [ "x${u}" = "xnonblocking" ]; then
                NB=-y
            fi

            # Ncmpi
            if [ "x${v}" = "xcoll" ]; then
                echo "rm -f ${OUTDIR}/*"
                rm -f ${OUTDIR}/*

                echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
                m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data
                aprun -n ${NP} -N ${PPN} ./btio

                echo "#%$: io_driver: ncmpi"
                echo "#%$: platform: theta"
                echo "#%$: number_of_nodes: ${NN}"
                echo "#%$: number_of_procs: ${NP}"
                echo "#%$: io_mode: ${u}_${v}"

                echo "ls -lah ${OUTDIR}"
                ls -lah ${OUTDIR}
                
                echo '-----+-----++------------+++++++++--+---'
            fi

            # Dw
            if [ "x${u}" = "xblocking" ] && [ "x${v}" = "xcoll" ]; then
                echo "rm -f ${OUTDIR}/*"
                rm -f ${OUTDIR}/*

                echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
                m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data
                aprun -n ${NP} -N ${PPN} -e PNETCDF_HINTS="nc_dw_driver=enable;nc_dw_del_on_close=disable;nc_dw_overwrite=enable;nc_dw_dirname=${BBDIR}" ./btio

                echo "#%$: io_driver: dw"     
                echo "#%$: platform: theta"
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
            fi

            # Dw shared
            if [ "x${u}" = "xblocking" ] && [ "x${v}" = "xcoll" ]; then
                echo "rm -f ${OUTDIR}/*"
                rm -f ${OUTDIR}/*

                echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
                m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data
                aprun -n ${NP} -N ${PPN} -e PNETCDF_HINTS="nc_dw_driver=enable;nc_dw_del_on_close=disable;nc_dw_overwrite=enable;nc_dw_sharedlog=enable;nc_dw_dirname=${BBDIR}" ./btio

                echo "#%$: io_driver: dw_shared"     
                echo "#%$: platform: theta"
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
            fi
        done
    done
    echo '--++---+----+++-----++++---+++--+-++--+---'
done
