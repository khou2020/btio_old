!
!  Copyright (C) 2013, Northwestern University
!  See COPYRIGHT notice in top-level directory.
!
!  $Id: bt.f90 3451 2015-11-09 03:46:47Z wkliao $

      !----< main >----------------------------------------------------
      program main
      use mpi
      use header
      use mpiio_m
      use pnetcdf_m
      implicit none

      character io_mode
      character(LEN=128) filename, cmd
      integer i, err, argc, iargc, fstatus, io_method
      integer*8 n3
      integer striping_factor, striping_unit
      double precision navg, t, t1, t2, t3, t4, tmax

      call MPI_Init(err)
      call MPI_Comm_size(MPI_COMM_WORLD, nprocs, err)
      call MPI_Comm_rank(MPI_COMM_WORLD, rank, err)

      ! check if number of processes is a square number
      ncells = dint(dsqrt(dble(nprocs) + 0.00001d0))

      if (nprocs .NE. ncells*ncells) then
         print*, 'Number of processes must be a square number. exit ...'
         goto 999
      endif

      root = 0

      !---------------------------------------------------------------------
      !      Root reads input file (if it exists) else takes
      !      defaults from parameters
      !---------------------------------------------------------------------
      if (rank .eq. root) then
         ! take filename from command-line argument if there is any
         call getarg(0, cmd)
         argc = IARGC()
         if (argc .GT. 1) then
            print*,'Usage: ',trim(cmd),' [filename]'
            niter = -1
            goto 777
         endif
         filename = 'inputbt.data'
         if (argc .EQ. 1) call getarg(1, filename)

         open(unit=2,file=trim(filename),status='old', iostat=fstatus)

         if (fstatus .eq. 0) then
 233        FORMAT(' Reading from input file ',A)
            write(*,233) trim(filename)
            read (2,*) io_mode
            read (2,*) io_method
            read (2,*) niter
            read (2,*) grid_points(1), grid_points(2), grid_points(3)
            read (2,'(a)') dir_path
            close(2)
         else
 234        format(' No input file inputbt.data. Exiting ...')
            write(*,234) 
            niter = -1
            goto 777
         endif
      endif

 777  call MPI_Bcast(io_mode,     1, MPI_CHARACTER, root, MPI_COMM_WORLD, err)
      call MPI_Bcast(io_method,   1, MPI_INTEGER,   root, MPI_COMM_WORLD, err)
      call MPI_Bcast(niter,       1, MPI_INTEGER,   root, MPI_COMM_WORLD, err)
      call MPI_Bcast(grid_points, 3, MPI_INTEGER8,  root, MPI_COMM_WORLD, err)
      call MPI_Bcast(dir_path,  128, MPI_CHARACTER, root, MPI_COMM_WORLD, err)

      if (niter .EQ. -1) goto 999

      call allocate_variables

      call make_set

      !---------------------------------------------------------------------
      !      Synchronize before placing time stamp
      !---------------------------------------------------------------------
      call MPI_Barrier(MPI_COMM_WORLD, err)

      t1 = MPI_Wtime()
      if (io_method .LT. 2) then ! 0: collective I/O, 1: independent I/O
         err = mpiio_setup(io_mode)
      else
         err = pnetcdf_setup(io_mode, io_method)
      endif
      if (err .EQ. 0) goto 999

      t2 = MPI_Wtime()

      num_io = 0
      do i=1, niter
         if (io_mode .EQ. 'w') then
            if (io_method .LT. 2) then ! 0: collective I/O, 1: independent I/O
               call mpiio_write(io_method)
            else
               call pnetcdf_write
            endif
         else
            if (io_method .LT. 2) then ! 0: collective I/O, 1: independent I/O
               call mpiio_read(io_method)
            else
               call pnetcdf_read
            endif
         endif
      end do

      t3 = MPI_Wtime()

      if (io_method .LT. 2) then ! 0: collective I/O, 1: independent I/O
         call mpiio_cleanup
      else
         call pnetcdf_cleanup
      endif

      call deallocate_variables

      t4 = MPI_Wtime()
      t = t4 - t1
      call MPI_Reduce(t, tmax, 1, MPI_DOUBLE_PRECISION, MPI_MAX,  &
                      root, MPI_COMM_WORLD, err)
      
      call report_io_performance(t, t2 - t1, t3 - t2, t4 - t3, rank, nprocs, root, io_method)

      if ( rank .eq. root ) then
         striping_factor = 0
         striping_unit   = 0
         call get_file_striping(info_used, striping_factor, striping_unit)
         call print_io_hints(info_used)

         n3 = grid_points(1)*grid_points(2)*grid_points(3)

         navg = n3 * 5 * 8       ! I/O amount per write/read
         navg = navg * num_io    ! I/O amount for all write/read

2000  format('#%$: ', A, ': ', A)
2001  format('#%$: ', A, ': ', F16.2)
2002  format('#%$: ', A, ': ', I13)


         print 2002,' number_of_cells', ncells
         print 2002,' number_of_iterations', niter

         if (io_mode .EQ. 'w') then
            print 2000,'io_operation', 'write'
         else
            print 2000,'io_operation', 'read'
         endif
         print 2002,'n_proc', nprocs
         print 2002,'size_x', grid_points(1)
         print 2002,'size_y', grid_points(2)
         print 2002,'size_z', grid_points(3)
         print 2002,'n_itr', niter
         
         print 2001,'io_size', navg

         if (io_method > 1) then ! 0: collective I/O, 1: independent I/O
            print 2000,'api', 'mpiio'
         else
            print 2000,'api', 'pnetcdf'  
         endif

         print 2000,'file_name',trim(dir_path)
         print 2002,'stripe_count',striping_factor
         print 2002,'stripe_size',striping_unit

      endif
      if (info_used .NE. MPI_INFO_NULL) &
         call MPI_Info_free(info_used, err)

 999  err = nf90mpi_mpi_finalize()

      end program main

      !---------------------------------------------------------------------------
      ! print I/O performance numbers
      !---------------------------------------------------------------------------
      subroutine report_io_performance(btio_time, init_time, rw_time, close_time, rank, NumPEs, root, io_method)
            use pnetcdf
            use mpi
            integer root, NumPEs, io_method, rank
            double precision btio_time, init_time, rw_time, close_time

            ! local variables
            integer ierr
            integer(kind=MPI_OFFSET_KIND) malloc_size, sum_size
            integer(kind=MPI_OFFSET_KIND) dw_data, dw_meta, dw_buffer
            integer(kind=MPI_OFFSET_KIND) dw_meta_all, dw_data_all, dw_buffer_all
            double precision time_io_max, time_io_min, time_io_mean, time_io_var
            double precision time_init_max, time_init_min, time_init_mean, time_init_var
            double precision time_rw_max, time_rw_min, time_rw_mean, time_rw_var
            double precision time_close_max, time_close_min, time_close_mean, time_close_var
            double precision dw_time(13), dw_time_max(13), dw_time_min(13), dw_time_mean(13), dw_time_var(13)
            double precision var(13), total_max, total_min, total_mean, total_var
            double precision time_staging

            err = nfmpi_inq_bb_time( dw_time(1), dw_time(2), dw_time(3), dw_time(4), dw_time(5), dw_time(6))
            err = nfmpi_inq_bb_time_put(dw_time(7), dw_time(8), dw_time(9))
            err = nfmpi_inq_bb_time_flush(dw_time(10), dw_time(11), dw_time(12), dw_time(13))
            err = nfmpi_inq_bb_size(dw_data, dw_meta, dw_buffer)

            call MPI_Reduce(btio_time, time_io_max, 1, MPI_DOUBLE_PRECISION, MPI_max, root, MPI_COMM_WORLD, ierr)
            call MPI_Reduce(btio_time, time_io_min, 1, MPI_DOUBLE_PRECISION, MPI_min, root, MPI_COMM_WORLD, ierr)
            call MPI_Allreduce(btio_time, time_io_mean, 1, MPI_DOUBLE_PRECISION, MPI_sum, MPI_COMM_WORLD, ierr)
            time_io_mean = time_io_mean / NumPEs
            var(1) = (btio_time - time_io_mean) * (btio_time - time_io_mean)
            call MPI_Reduce(var(1), time_io_var, 1, MPI_DOUBLE_PRECISION, MPI_sum, root, MPI_COMM_WORLD, ierr)
            time_io_var = time_io_var / NumPEs

            call MPI_Reduce(init_time, time_init_max, 1, MPI_DOUBLE_PRECISION, MPI_max, root, MPI_COMM_WORLD, ierr)
            call MPI_Reduce(init_time, time_init_min, 1, MPI_DOUBLE_PRECISION, MPI_min, root, MPI_COMM_WORLD, ierr)
            call MPI_Allreduce(init_time, time_init_mean, 1, MPI_DOUBLE_PRECISION, MPI_sum, MPI_COMM_WORLD, ierr)
            time_init_mean = time_init_mean / NumPEs
            var(1) = (init_time - time_init_mean) * (init_time - time_init_mean)
            call MPI_Reduce(var(1), time_init_var, 1, MPI_DOUBLE_PRECISION, MPI_sum, root, MPI_COMM_WORLD, ierr)
            time_io_var = time_io_var / NumPEs

            call MPI_Reduce(rw_time, time_rw_max, 1, MPI_DOUBLE_PRECISION, MPI_max, root, MPI_COMM_WORLD, ierr)
            call MPI_Reduce(rw_time, time_rw_min, 1, MPI_DOUBLE_PRECISION, MPI_min, root, MPI_COMM_WORLD, ierr)
            call MPI_Allreduce(rw_time, time_rw_mean, 1, MPI_DOUBLE_PRECISION, MPI_sum, MPI_COMM_WORLD, ierr)
            time_rw_mean = time_rw_mean / NumPEs
            var(1) = (rw_time - time_rw_mean) * (rw_time - time_rw_mean)
            call MPI_Reduce(var(1), time_rw_var, 1, MPI_DOUBLE_PRECISION, MPI_sum, root, MPI_COMM_WORLD, ierr)
            time_rw_var = time_rw_var / NumPEs

            call MPI_Reduce(close_time, time_close_max, 1, MPI_DOUBLE_PRECISION, MPI_max, root, MPI_COMM_WORLD, ierr)
            call MPI_Reduce(close_time, time_close_min, 1, MPI_DOUBLE_PRECISION, MPI_min, root, MPI_COMM_WORLD, ierr)
            call MPI_Allreduce(close_time, time_close_mean, 1, MPI_DOUBLE_PRECISION, MPI_sum, MPI_COMM_WORLD, ierr)
            time_close_mean = time_close_mean / NumPEs
            var(1) = (close_time - time_close_mean) * (close_time - time_close_mean)
            call MPI_Reduce(var(1), time_close_var, 1, MPI_DOUBLE_PRECISION, MPI_sum, root, MPI_COMM_WORLD, ierr)
            time_close_var = time_close_var / NumPEs

            call MPI_Reduce(dw_time, dw_time_max, 13, MPI_DOUBLE_PRECISION, MPI_max, root, MPI_COMM_WORLD, ierr)
            call MPI_Reduce(dw_time, dw_time_min, 13, MPI_DOUBLE_PRECISION, MPI_min, root, MPI_COMM_WORLD, ierr)
            call MPI_Allreduce(dw_time, dw_time_mean, 13, MPI_DOUBLE_PRECISION, MPI_sum, MPI_COMM_WORLD, ierr)
            do 180 i = 1, 13
                  dw_time_mean(i) = dw_time_mean(i) / NumPEs
                  var(i) = (dw_time(i) - dw_time_mean(i)) * (dw_time(i) - dw_time_mean(i))
180         continue
            call MPI_Reduce(var, dw_time_var, 13, MPI_DOUBLE_PRECISION, MPI_sum, root, MPI_COMM_WORLD, ierr)
            do 190 i = 1, 13
                  dw_time_var(i) = dw_time_var(i) / NumPEs
190         continue

            call MPI_Reduce(dw_meta, dw_meta_all, 1, MPI_OFFSET, MPI_SUM, root, MPI_COMM_WORLD, ierr)
            call MPI_Reduce(dw_data, dw_data_all, 1, MPI_OFFSET, MPI_SUM, root, MPI_COMM_WORLD, ierr)
            call MPI_Reduce(dw_buffer, dw_buffer_all, 1, MPI_OFFSET, MPI_max, root, MPI_COMM_WORLD, ierr)

            if (rank .EQ. root) then
            
                  call nfmpi_stage_out_env(time_staging)

1007  format(A,A)
1008  format('#%$: ', A, ': ', F16.2)
1009  format('#%$: ', A, ': ', I13)
1010  format('#%$: ', A, ': ', A)
1011  format('#%$: ', A, ': ', F16.4)

                  
                  print 1009,' number_of_processes', NumPEs

                  if ((io_method .EQ. 3) .OR. (io_method .EQ. 5)) then
                        print 1010,' nonblocking_io', '1'
                  else
                        print 1010,' nonblocking_io', '0'
                  endif
                  if (io_method > 3) then
                        print 1010,' indep_io','1' 
                  else
                        print 1010,' indep_io', '0'
                  endif

                  print 1008,' init_time_max        ', time_init_max
                  print 1008,' init_time_min        ', time_init_min
                  print 1008,' init_time_mean        ', time_init_mean
                  print 1008,' init_time_var        ', time_init_var

                  print 1008,' rw_time_max        ', time_rw_max
                  print 1008,' rw_time_min        ', time_rw_min
                  print 1008,' rw_time_mean        ', time_rw_mean
                  print 1008,' rw_time_var        ', time_rw_var

                  print 1008,' close_time_max        ', time_close_max
                  print 1008,' close_time_min        ', time_close_min
                  print 1008,' close_time_mean        ', time_close_mean
                  print 1008,' close_time_var        ', time_close_var

                  print 1008,' btio_time_max        ', time_io_max
                  print 1008,' btio_time_min        ', time_io_min
                  print 1008,' btio_time_mean        ', time_io_mean
                  print 1008,' btio_time_var        ', time_io_var

                  print 1008,' total_time_max        ', time_io_max + time_staging
                  print 1008,' total_time_min        ', time_io_min + time_staging
                  print 1008,' total_time_mean        ', time_io_mean + time_staging
                  print 1008,' total_time_var        ', time_io_var

                  print 1008,' stage_time       ', time_staging

                  print 1008,' dw_total_time_max       ', dw_time_max(1)
                  print 1008,' dw_create_time_max       ', dw_time_max(2)
                  print 1008,' dw_enddef_time_max       ', dw_time_max(3)
                  print 1008,' dw_put_time_max       ', dw_time_max(4)
                  print 1008,' dw_flush_time_max       ', dw_time_max(5)
                  print 1008,' dw_close_time_max       ', dw_time_max(6)
                  print 1008,' dw_put_data_wr_time_max       ', dw_time_max(7)
                  print 1008,' dw_put_meta_wr_time_max       ', dw_time_max(8)
                  print 1008,' dw_put_num_wr_time_max       ', dw_time_max(9)
                  print 1008,' dw_flush_replay_time_max       ', dw_time_max(10)
                  print 1008,' dw_flush_data_rd_time_max       ', dw_time_max(11)
                  print 1008,' dw_flush_put_time_max       ', dw_time_max(12)
                  print 1008,' dw_flush_wait_time_max       ', dw_time_max(13)

                  print 1008,' dw_total_time_min       ', dw_time_min(1)
                  print 1008,' dw_create_time_min       ', dw_time_min(2)
                  print 1008,' dw_enddef_time_min       ', dw_time_min(3)
                  print 1008,' dw_put_time_min       ', dw_time_min(4)
                  print 1008,' dw_flush_time_min       ', dw_time_min(5)
                  print 1008,' dw_close_time_min       ', dw_time_min(6)
                  print 1008,' dw_put_data_wr_time_min       ', dw_time_min(7)
                  print 1008,' dw_put_meta_wr_time_min       ', dw_time_min(8)
                  print 1008,' dw_put_num_wr_time_min       ', dw_time_min(9)
                  print 1008,' dw_flush_replay_time_min       ', dw_time_min(10)
                  print 1008,' dw_flush_data_rd_time_min       ', dw_time_min(11)
                  print 1008,' dw_flush_put_time_min       ', dw_time_min(12)
                  print 1008,' dw_flush_wait_time_min       ', dw_time_min(13)

                  print 1008,' dw_total_time_mean       ', dw_time_mean(1)
                  print 1008,' dw_create_time_mean       ', dw_time_mean(2)
                  print 1008,' dw_enddef_time_mean       ', dw_time_mean(3)
                  print 1008,' dw_put_time_mean       ', dw_time_mean(4)
                  print 1008,' dw_flush_time_mean       ', dw_time_mean(5)
                  print 1008,' dw_close_time_mean       ', dw_time_mean(6)
                  print 1008,' dw_put_data_wr_time_mean       ', dw_time_mean(7)
                  print 1008,' dw_put_meta_wr_time_mean       ', dw_time_mean(8)
                  print 1008,' dw_put_num_wr_time_mean       ', dw_time_mean(9)
                  print 1008,' dw_flush_replay_time_mean       ', dw_time_mean(10)
                  print 1008,' dw_flush_data_rd_time_mean       ', dw_time_mean(11)
                  print 1008,' dw_flush_put_time_mean       ', dw_time_mean(12)
                  print 1008,' dw_flush_wait_time_mean       ', dw_time_mean(13)

                  print 1011,' dw_total_time_var       ', dw_time_var(1)
                  print 1011,' dw_create_time_var       ', dw_time_var(2)
                  print 1011,' dw_enddef_time_var       ', dw_time_var(3)
                  print 1011,' dw_put_time_var       ', dw_time_var(4)
                  print 1011,' dw_flush_time_var       ', dw_time_var(5)
                  print 1011,' dw_close_time_var       ', dw_time_var(6)
                  print 1011,' dw_put_data_wr_time_var       ', dw_time_var(7)
                  print 1011,' dw_put_meta_wr_time_var       ', dw_time_var(8)
                  print 1011,' dw_put_num_wr_time_var       ', dw_time_var(9)
                  print 1011,' dw_flush_replay_time_var       ', dw_time_var(10)
                  print 1011,' dw_flush_data_rd_time_var       ', dw_time_var(11)
                  print 1011,' dw_flush_put_time_var       ', dw_time_var(12)
                  print 1011,' dw_flush_wait_time_var       ', dw_time_var(13)

                  print 1009,' dw_metadata_size       ', dw_meta_all
                  print 1009,' dw_data_size       ', dw_data_all
                  print 1009,' dw_flush_buffer_size       ', dw_buffer_all
            endif
      end subroutine report_io_performance
