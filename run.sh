#!/bin/bash
#COBALT -t 5
#COBALT -n 1
#COBALT --attrs mcdram=cache:numa=quad:ssds=required:ssd_size=16
#COBALT -A ecp-testbed-01
#COBALT -q debug-flat-quad
#COBALT -o btio_1.txt
#COBALT -e btio_1.txt

export n_nodes=$COBALT_JOBSIZE
export n_mpi_ranks_per_node=${PPN}
export n_mpi_ranks=$(($n_nodes * $n_mpi_ranks_per_node))
export n_openmp_threads_per_rank=1
export n_hyperthreads_per_core=1

RUNS=(1) # Number of runs
OUTDIR=/projects/radix-io/khou/FS_64_8M/btio
BBDIR=/local/scratch
PPN=4
#PPN=64
NN=${COBALT_JOBSIZE}
let NP=NN*PPN

DIMX=32
DIMY=32
DIMZ=32
NITR=1 # 1 Itr = 5 GiB

echo "mkdir -p ${OUTDIR}"
mkdir -p ${OUTDIR}

TSTARTTIME=`date +%s.%N`

for i in ${RUNS[@]}
do
    # Ncmpio    
    echo "========================== NCMPI =========================="
    >&2 echo "========================== NCMPI =========================="

    echo "#%$: io_driver: ncmpi"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: blocking_coll"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*
    
    let IO_METHOD=2
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    aprun -n ${NP} -N ${PPN} ./btio

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    
    echo '-----+-----++------------+++++++++--+---'

    # Ncmpio NB

    echo "========================== NCMPI NB ALL =========================="
    >&2 echo "========================== NCMPI NB ALL =========================="
    
    echo "#%$: io_driver: ncmpi"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: nonblocking_coll_all"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*
    
    let IO_METHOD=13
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    aprun -n ${NP} -N ${PPN} ./btio

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    
    echo '-----+-----++------------+++++++++--+---'

    # Ncmpio NB

    echo "========================== NCMPI NB =========================="
    >&2 echo "========================== NCMPI NB =========================="
    
    echo "#%$: io_driver: ncmpi"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: nonblocking_coll"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*
    
    let IO_METHOD=3
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    aprun -n ${NP} -N ${PPN} ./btio

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    
    echo '-----+-----++------------+++++++++--+---' 

    # BB LPP P

    echo "========================== BB LPP P ALL =========================="
    >&2 echo "========================== BB LPP P ALL =========================="

    echo "#%$: io_driver: bb_lpp_private_all"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: blocking_coll"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*

    let IO_METHOD=12
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    aprun -n ${NP} -N ${PPN} -e PNETCDF_HINTS="nc_burst_buf=enable;nc_burst_buf_del_on_close=disable;nc_burst_buf_overwrite=enable;nc_burst_buf_dirname=${BBDIR}" ./btio
    
    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}

    echo '-----+-----++------------+++++++++--+---'

    # BB LPN S

    echo "========================== BB LPN P ALL =========================="
    >&2 echo "========================== BB LPN P ALL =========================="

    echo "#%$: io_driver: bb_lpn_private_all"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: blocking_coll"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*

    let IO_METHOD=12
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    aprun -n ${NP} -N ${PPN} -e PNETCDF_HINTS="nc_burst_buf=enable;nc_burst_buf_del_on_close=disable;nc_burst_buf_shared_logs=enable;nc_burst_buf_overwrite=enable;nc_burst_buf_dirname=${BBDIR}" ./btio

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}

    echo '-----+-----++------------+++++++++--+---'

    # BB LPP P

    echo "========================== BB LPP P =========================="
    >&2 echo "========================== BB LPP P =========================="

    echo "#%$: io_driver: bb_lpp_private"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: blocking_coll"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*

    let IO_METHOD=2
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    aprun -n ${NP} -N ${PPN} -e PNETCDF_HINTS="nc_burst_buf=enable;nc_burst_buf_del_on_close=disable;nc_burst_buf_overwrite=enable;nc_burst_buf_dirname=${BBDIR}" ./btio
    
    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}

    echo '-----+-----++------------+++++++++--+---'

    # BB LPN S

    echo "========================== BB LPN P =========================="
    >&2 echo "========================== BB LPN P =========================="

    echo "#%$: io_driver: bb_lpn_private"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: blocking_coll"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*

    let IO_METHOD=2
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    aprun -n ${NP} -N ${PPN} -e PNETCDF_HINTS="nc_burst_buf=enable;nc_burst_buf_del_on_close=disable;nc_burst_buf_shared_logs=enable;nc_burst_buf_overwrite=enable;nc_burst_buf_dirname=${BBDIR}" ./btio

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}

    echo '-----+-----++------------+++++++++--+---'
done

ENDTIME=`date +%s.%N`
TIMEDIFF=`echo "$ENDTIME - $TSTARTTIME" | bc | awk -F"." '{print $1"."$2}'`
echo "-------------------------------------------------------------"
echo "total_exe_time: $TIMEDIFF"
echo "-------------------------------------------------------------"



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
