#!/bin/bash
#SBATCH -p debug
#SBATCH -N 1 
#SBATCH -C haswell
#SBATCH -t 00:20:00
#SBATCH -o btio_1_%j.txt
#SBATCH -e btio_1_%j.err
#SBATCH -L SCRATCH
#SBATCH -A m844
#SBATCH --gres=craynetwork:2
#DW jobdw capacity=640GiB access_mode=striped type=scratch
#DW jobdw capacity=640GiB access_mode=private type=scratch
RUNS=(1) # Number of runs
OUTDIR=/global/cscratch1/sd/khl7265/FS_64_8M/btio
NN=${SLURM_NNODES}
let NP=NN*4
#let NP=NN*32 

DIMX=32
DIMY=32
DIMZ=32
NITR=1 # 1 Itr = 5 GiB

# Make sure BB stripe count is correct
srun -n 1 /global/homes/k/khl7265/sc ${DW_JOB_STRIPED}/test.bin 64
ret="$(sacct -o exitcode -n -j ${SLURM_JOB_ID}.0)"
# remove leading whitespace characters
ret="${ret#"${ret%%[![:space:]]*}"}"
# remove trailing whitespace characters
ret="${ret%"${ret##*[![:space:]]}"}"
if [[ "x${ret}" != "x0:0" ]]; then
    echo "BB stripe count mismatch, quit"
    exit 0
fi

echo "mkdir -p ${OUTDIR}"
mkdir -p ${OUTDIR}
echo "rm -rf ${DW_JOB_STRIPED}"
rm -rf ${DW_JOB_STRIPED}

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

    srun -n ${NP} -t 3 ./btio

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

    srun -n ${NP} -t 3 ./btio

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

    srun -n ${NP} -t 3 ./btio

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

    export PNETCDF_HINTS="nc_burst_buf=enable;nc_burst_buf_del_on_close=disable;nc_burst_buf_overwrite=enable;nc_burst_buf_dirname=${DW_JOB_PRIVATE}"

    let IO_METHOD=12
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    srun -n ${NP} -t 3 ./btio
    
    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`
    
    unset PNETCDF_HINTS

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    echo "ls -lah ${DW_JOB_PRIVATE}"
    ls -lah ${DW_JOB_PRIVATE}

    echo '-----+-----++------------+++++++++--+---'

    # BB LPP S

    echo "========================== BB LPP S ALL =========================="
    >&2 echo "========================== BB LPP S ALL =========================="

    echo "#%$: io_driver: bb_lpp_striped_all"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: blocking_coll"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*
    echo "rm -f ${DW_JOB_STRIPED}/*"
    rm -f ${DW_JOB_STRIPED}/*

    export PNETCDF_HINTS="nc_burst_buf=enable;nc_burst_buf_del_on_close=disable;nc_burst_buf_overwrite=enable;nc_burst_buf_dirname=${DW_JOB_STRIPED}"

    let IO_METHOD=12
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    srun -n ${NP} -t 3 ./btio

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    unset PNETCDF_HINTS

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    if [[ "${NP}" -lt 33 ]]; then
        echo "ls -lah ${DW_JOB_STRIPED}"
        ls -lah ${DW_JOB_STRIPED}
    fi

    echo '-----+-----++------------+++++++++--+---'

    # BB LPN S

    echo "========================== BB LPN S ALL =========================="
    >&2 echo "========================== BB LPN S ALL =========================="

    echo "#%$: io_driver: bb_lpn_striped_all"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: blocking_coll"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*
    echo "rm -f ${DW_JOB_STRIPED}/*"
    rm -f ${DW_JOB_STRIPED}/*

    export PNETCDF_HINTS="nc_burst_buf=enable;nc_burst_buf_del_on_close=disable;nc_burst_buf_overwrite=enable;nc_burst_buf_shared_logs=enable;nc_burst_buf_dirname=${DW_JOB_STRIPED}"

    let IO_METHOD=12
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    srun -n ${NP} -t 3 ./btio

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    unset PNETCDF_HINTS

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    if [[ "${NP}" -lt 33 ]]; then
        echo "ls -lah ${DW_JOB_STRIPED}"
        ls -lah ${DW_JOB_STRIPED}
    fi

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

    export PNETCDF_HINTS="nc_burst_buf=enable;nc_burst_buf_del_on_close=disable;nc_burst_buf_overwrite=enable;nc_burst_buf_dirname=${DW_JOB_PRIVATE}"

    let IO_METHOD=2
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    srun -n ${NP} -t 3 ./btio
    
    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`
    
    unset PNETCDF_HINTS

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    echo "ls -lah ${DW_JOB_PRIVATE}"
    ls -lah ${DW_JOB_PRIVATE}

    echo '-----+-----++------------+++++++++--+---'

    # BB LPP S

    echo "========================== BB LPP S =========================="
    >&2 echo "========================== BB LPP S =========================="

    echo "#%$: io_driver: bb_lpp_striped"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: blocking_coll"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*
    echo "rm -f ${DW_JOB_STRIPED}/*"
    rm -f ${DW_JOB_STRIPED}/*

    export PNETCDF_HINTS="nc_burst_buf=enable;nc_burst_buf_del_on_close=disable;nc_burst_buf_overwrite=enable;nc_burst_buf_dirname=${DW_JOB_STRIPED}"

    let IO_METHOD=2
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    srun -n ${NP} -t 3 ./btio

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    unset PNETCDF_HINTS

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    if [[ "${NP}" -lt 33 ]]; then
        echo "ls -lah ${DW_JOB_STRIPED}"
        ls -lah ${DW_JOB_STRIPED}
    fi

    echo '-----+-----++------------+++++++++--+---'

    # BB LPN S

    echo "========================== BB LPN S =========================="
    >&2 echo "========================== BB LPN S =========================="

    echo "#%$: io_driver: bb_lpn_striped"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: blocking_coll"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*
    echo "rm -f ${DW_JOB_STRIPED}/*"
    rm -f ${DW_JOB_STRIPED}/*

    export PNETCDF_HINTS="nc_burst_buf=enable;nc_burst_buf_del_on_close=disable;nc_burst_buf_overwrite=enable;nc_burst_buf_shared_logs=enable;nc_burst_buf_dirname=${DW_JOB_STRIPED}"

    let IO_METHOD=2
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    srun -n ${NP} -t 3 ./btio

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    unset PNETCDF_HINTS

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    if [[ "${NP}" -lt 33 ]]; then
        echo "ls -lah ${DW_JOB_STRIPED}"
        ls -lah ${DW_JOB_STRIPED}
    fi

    echo '-----+-----++------------+++++++++--+---'
    
    # Staging

    echo "========================== Stage =========================="
    >&2 echo "========================== Stage =========================="

    echo "#%$: io_driver: stage"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_procs: ${NP}"
    echo "#%$: io_mode: ${u}_${v}"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*
    echo "rm -f ${DW_JOB_STRIPED}/*"
    rm -f ${DW_JOB_STRIPED}/*

    export stageout_bb_path="${DW_JOB_STRIPED}"
    export stageout_pfs_path="${OUTDIR}"

    let IO_METHOD=2
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${DW_JOB_STRIPED} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${DW_JOB_STRIPED} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    srun -n ${NP} -t 3 ./btio

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    unset stageout_bb_path
    unset stageout_pfs_path

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    echo "ls -lah ${DW_JOB_STRIPED}"
    ls -lah ${DW_JOB_STRIPED}
    
    echo '-----+-----++------------+++++++++--+---'
    
    # Staging Indep

    echo "========================== On BB Indep =========================="
    >&2 echo "========================== On BB Indep =========================="

    echo "#%$: io_driver: on_bb"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: blocking_indep"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*
    echo "rm -f ${DW_JOB_STRIPED}/*"
    rm -f ${DW_JOB_STRIPED}/*

    #export stageout_bb_path="${DW_JOB_STRIPED}"
    #export stageout_pfs_path="${OUTDIR}"

    let IO_METHOD=4
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${DW_JOB_STRIPED} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${DW_JOB_STRIPED} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    srun -n ${NP} -t 3 ./btio

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    #unset stageout_bb_path
    #unset stageout_pfs_path

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    echo "ls -lah ${DW_JOB_STRIPED}"
    ls -lah ${DW_JOB_STRIPED}

    echo '-----+-----++------------+++++++++--+---'

    # LogFS

    echo "========================== Logfs =========================="
    >&2 echo "========================== Logfs =========================="
    
    echo "#%$: io_driver: logfs"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: blocking_coll"

    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*
    
    export PNETCDF_HINTS="logfs_replayonclose=true;logfs_info_logbase=${DW_JOB_PRIVATE};logfs_flushblocksize=268435456"

    let IO_METHOD=2
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=logfs:${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=logfs:${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`

    srun -n ${NP} -t 3 ./btio_logfs

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    unset PNETCDF_HINTS

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    
    echo '-----+-----++------------+++++++++--+---'

    # Data Elevator

    echo "========================== DE =========================="
    >&2 echo "========================== DE =========================="

    echo "#%$: io_driver: de"
    echo "#%$: number_of_nodes: ${NN}"
    echo "#%$: number_of_proc: ${NP}"
    echo "#%$: io_mode: blocking_coll"
    
    echo "rm -f ${OUTDIR}/*"
    rm -f ${OUTDIR}/*
    echo "rm -f ${DW_JOB_STRIPED}/*"
    rm -f ${DW_JOB_STRIPED}/*

    let IO_METHOD=2
    echo "m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data"
    m4 -D io_method=${IO_METHOD} -D n_itr=${NITR} -D dim_x=${DIMX} -D dim_y=${DIMY} -D dim_z=${DIMZ} -D out_dir=${OUTDIR} inputbt.m4 > inputbt.data

    STARTTIME=`date +%s.%N`
    
    srun -n ${NP} -t 3 --mem=60000 --gres=craynetwork:1 ./btio_de &
    srun -n ${NP} -t 3 --mem=60000 --gres=craynetwork:1 /global/homes/k/khl7265/local/dataelevator/bin/dejob -i -a &
    wait

    ENDTIME=`date +%s.%N`
    TIMEDIFF=`echo "$ENDTIME - $STARTTIME" | bc | awk -F"." '{print $1"."$2}'`

    echo "#%$: exe_time: $TIMEDIFF"

    echo "ls -lah ${OUTDIR}"
    ls -lah ${OUTDIR}
    echo "ls -lah ${DW_JOB_STRIPED}"
    ls -lah ${DW_JOB_STRIPED}

    echo '-----+-----++------------+++++++++--+---'

    #let IO_METHOD=2
    #if [ "x${u}" = "xnonblocking" ]; then
    #    let IO_METHOD=IO_METHOD+1
    #fi
    #if [ "x${v}" = "xindep" ]; then
    #    let IO_METHOD=IO_METHOD+2
    #fi
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

ENDTIME=`date +%s.%N`
TIMEDIFF=`echo "$ENDTIME - $TSTARTTIME" | bc | awk -F"." '{print $1"."$2}'`
echo "-------------------------------------------------------------"
echo "total_exe_time: $TIMEDIFF"
echo "-------------------------------------------------------------"

