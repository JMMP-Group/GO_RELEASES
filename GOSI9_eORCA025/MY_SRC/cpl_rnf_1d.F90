MODULE cpl_rnf_1d
   !!======================================================================
   !!                       ***  MODULE  cpl_rnf_1d  ***
   !! Ocean forcing:  River runoff passed from the atmosphere using
   !!                 1D array. One value per river.
   !!=====================================================================
   !! History : ?.?  ! 2018-01 (D. Copsey) Initial setup
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   cpl_rnf_1d_init : runoffs initialisation
   !!---------------------------------------------------------------------- 

#if defined key_oasis3
   USE mod_oasis                    ! OASIS3-MCT module
#endif
   USE timing          ! Timing
   USE in_out_manager  ! I/O units
   USE lib_mpp         ! MPP library
   USE iom
   USE dom_oce         ! Domain sizes (for grid box area e1e2t)
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE lib_fortran,    ONLY: DDPDD
   
   IMPLICIT NONE
   PRIVATE
   
   PUBLIC   cpl_rnf_1d_init     ! routine called in nemo_init
   PUBLIC   cpl_rnf_1d_to_2d      ! routine called in sbccpl.F90
   
   TYPE, PUBLIC ::   RIVERS_DATA     !: Storage for river outflow data
      INTEGER, ALLOCATABLE, DIMENSION(:,:)    ::   river_number       !: River outflow number
      REAL(wp), ALLOCATABLE, DIMENSION(:)     ::   river_area         ! 1D array listing areas of each river outflow (m2)
      COMPLEX(wp), ALLOCATABLE, DIMENSION(:)  ::   river_area_c       ! Comlex version of river_area for use in bit reproducible sums (m2)
   END TYPE RIVERS_DATA
   
   TYPE(RIVERS_DATA), PUBLIC, TARGET :: rivers  !: River data
   
   INTEGER, PUBLIC            :: nn_cpl_river   ! Maximum number of rivers being passed through the coupler
   INTEGER, PUBLIC            :: runoff_id      ! OASIS coupling id used in oasis_get command
   LOGICAL                    :: ln_print_river_info  ! Diagnostic prints of river coupling information
   
CONTAINS

   SUBROUTINE cpl_rnf_1d_init
      !!----------------------------------------------------------------------
      !!                    ***  SUBROUTINE cpl_rnf_1d_init  ***
      !!                     
      !! ** Purpose : - Read in file for river outflow numbers.
      !!                Calculate 2D area of river outflow points.
      !!                Called from nemo_init (nemogcm.F90).
      !!
      !!----------------------------------------------------------------------
      !! namelist variables
      !!-------------------
      CHARACTER(len=200)                        ::   file_riv_number             !: Filename for river numbers
      INTEGER                                   ::   ios                 ! Local integer output status for namelist read
      INTEGER                                   ::   inum
      INTEGER                                   ::   ii, jj, rr          !: Loop indices
      INTEGER                                   ::   max_river
      REAL(wp), DIMENSION(jpi,jpj)              ::   river_number        ! 2D array containing the river outflow numbers
      
      NAMELIST/nam_cpl_rnf_1d/file_riv_number, nn_cpl_river, ln_print_river_info
      !!----------------------------------------------------------------------

      IF( ln_timing ) CALL timing_start('cpl_rnf_1d_init')
      
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'cpl_rnf_1d_init : initialization of river runoff coupling'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'
      IF(lwp) CALL flush(numout)
      
      REWIND(numnam_cfg)
      
      ! Read the namelist
      READ  ( numnam_ref, nam_cpl_rnf_1d, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_cpl_rnf_1d in reference namelist' )
      READ  ( numnam_cfg, nam_cpl_rnf_1d, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'nam_cpl_rnf_1d in configuration namelist' )
      IF(lwm .AND. nprint > 2) WRITE ( numond, nam_cpl_rnf_1d )

      !                                               ! Parameter control and print
      IF(lwp) WRITE(numout,*) '  '
      IF(lwp) WRITE(numout,*) '          Namelist nam_cpl_rnf_1d : Coupled runoff using 1D array'
      IF(lwp) WRITE(numout,*) '             Input file that contains river numbers = ',file_riv_number
      IF(lwp) WRITE(numout,*) '             Maximum number of rivers to couple = ',nn_cpl_river
      IF(lwp) WRITE(numout,*) '             Print river information = ',ln_print_river_info
      IF(lwp) WRITE(numout,*) ' '
      IF(lwp) CALL flush(numout)

      ! Assign space for river numbers
      ALLOCATE( rivers%river_number( jpi, jpj ) )
      
      ! Read the river numbers from netcdf file
      CALL iom_open (file_riv_number , inum )
      CALL iom_get  ( inum, jpdom_data, 'river_number', river_number )
      CALL iom_close( inum )
      
      ! Convert from a real array to an integer array
      max_river=0
      DO ii = 1, jpi
        DO jj = 1, jpj
          rivers%river_number(ii,jj) = INT(river_number(ii,jj))
          IF ( rivers%river_number(ii,jj) > max_river ) THEN
            max_river = rivers%river_number(ii,jj)
          END IF
        END DO
      END DO
      
      ! Print out the largest river number
      IF ( ln_print_river_info .AND. lwp) THEN
         WRITE(numout,*) 'Maximum river number in input file = ',max_river
         CALL flush(numout)
      END IF
      
      ! Get the area of each river outflow
      ALLOCATE( rivers%river_area( nn_cpl_river ) )
      ALLOCATE( rivers%river_area_c( nn_cpl_river ) )
      rivers%river_area_c(:) = CMPLX( 0.e0, 0.e0, wp )
      DO ii = nldi, nlei      
        DO jj = nldj, nlej
          IF ( tmask_i(ii,jj) > 0.5 ) THEN  ! This makes sure we are not at a duplicated point (at north fold or east-west cyclic point)
            IF ( rivers%river_number(ii,jj) > 0 .AND. rivers%river_number(ii,jj) <= nn_cpl_river ) THEN
              ! Add the area of each grid box (e1e2t) into river_area_c using DDPDD which should maintain bit reproducibility (needs to be checked)
              CALL DDPDD( CMPLX( e1e2t(ii,jj), 0.e0, wp ), rivers%river_area_c(rivers%river_number(ii,jj) ) )
            END IF
          END IF
        END DO
      END DO
      
      ! Use mpp_sum to add together river areas on other processors
      CALL mpp_sum( 'cpl_rnf_1d', rivers%river_area_c )
      
      ! Convert from complex number to real
      rivers%river_area(:) = REAL(rivers%river_area_c(:),wp)
      
      IF ( ln_print_river_info .AND. lwp) THEN
        WRITE(numout,*) 'Area of rivers 1 to 10 are ',rivers%river_area(1:10)
        WRITE(numout,*) 'Maximum river area = ',MAXVAL(rivers%river_area)
        WRITE(numout,*) 'Minimum river area = ',MINVAL(rivers%river_area)
        WRITE(numout,*) 'Sum of river areas = ',SUM(rivers%river_area)
        CALL flush(numout)
      END IF

      IF ( MINVAL(rivers%river_area) <= 0 ) THEN
         WRITE(numout,*) 'ERROR: There is at least one river with a river outflow area of zero. Please check your file_riv_number file'
         WRITE(numout,*) 'that all the allocated river numbers are at ocean points as defined by the bathymetry file with no river'
         WRITE(numout,*) 'numbers within the north fold or wraparound points.'
         DO rr = 1,nn_cpl_river
           IF ( rivers%river_area(rr) <= 0 ) THEN
             WRITE(numout,*) 'The first river with this problem is river number ',rr
             CALL ctl_stop ( 'STOP', 'ERROR: There is at least one river with a river outflow area of zero. See stdout.')
           END IF
         END DO
      END IF
      
   END SUBROUTINE cpl_rnf_1d_init
   
   SUBROUTINE cpl_rnf_1d_to_2d( runoff_1d )
   
      !!----------------------------------------------------------------------
      !!                    ***  SUBROUTINE cpl_rnf_1d_to_2d  ***
      !!                     
      !! ** Purpose : - Convert river outflow from 1D array (passed from the
      !!                atmosphere) to the 2D NEMO runoff field.
      !!                Called from sbc_cpl_ice_flx (sbccpl.F90).
      !!
      !!----------------------------------------------------------------------
      
      REAL                   , INTENT(in   ) ::   runoff_1d(nn_cpl_river)    ! River runoff. One value per river.
      
      INTEGER  ::   ii, jj, rr                 ! Loop indices
      LOGICAL  ::   found_nan
            
      ! Convert the 1D total runoff per river to 2D runoff flux by
      ! dividing by the area of each runoff zone.
      DO ii = 1, jpi
        DO jj = 1, jpj
          IF ( rivers%river_number(ii,jj) > 0 .AND. rivers%river_number(ii,jj) <= nn_cpl_river .AND. tmask_i(ii,jj) > 0.5 ) THEN
            rnf(ii,jj) = runoff_1d(rivers%river_number(ii,jj)) / rivers%river_area(rivers%river_number(ii,jj))
          ELSE
            rnf(ii,jj) = 0.0
          END IF
            
        END DO
      END DO

      IF ( ln_print_river_info ) THEN
        WRITE(numout,*) 'SUM of river runoff on 1D array = ',SUM(runoff_1d)
        WRITE(numout,*) 'SUM of river runoff on 2D array = ',SUM(rnf)
        found_nan = .false.
        DO rr = 1, nn_cpl_river
          IF (rr <= 10) THEN
            WRITE(numout,*) 'River number ',rr,' has total runoff=',runoff_1d(rr),' and area=',rivers%river_area(rr),' making runoff flux=',runoff_1d(rr)/rivers%river_area(rr)
          END IF

          IF (runoff_1d(rr) /= runoff_1d(rr)) THEN
            IF (.NOT. found_nan) THEN
              WRITE(numout,*) 'WARNING: NAN found at river number ',rr
              found_nan = .true.
            END IF
          END IF

        END DO
      END IF     


   END SUBROUTINE cpl_rnf_1d_to_2d

END MODULE cpl_rnf_1d
