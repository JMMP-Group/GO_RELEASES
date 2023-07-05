MODULE icbclv
   !!======================================================================
   !!                       ***  MODULE  icbclv  ***
   !! Icebergs:  calving routines for iceberg calving
   !!======================================================================
   !! History : 3.3.1 !  2010-01  (Martin&Adcroft) Original code
   !!            -    !  2011-03  (Madec)          Part conversion to NEMO form
   !!            -    !                            Removal of mapping from another grid
   !!            -    !  2011-04  (Alderson)       Split into separate modules
   !!            -    !  2011-05  (Alderson)       budgets into separate module
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   icb_clv_flx   : transfer input flux of ice into iceberg classes
   !!   icb_clv       : calve icebergs from stored ice
   !!----------------------------------------------------------------------
   USE par_oce        ! NEMO parameters
   USE dom_oce        ! NEMO ocean domain
   USE phycst         ! NEMO physical constants
   USE lib_mpp        ! NEMO MPI library, lk_mpp in particular
   USE lbclnk         ! NEMO boundary exchanges for gridded data

   USE icb_oce        ! iceberg variables
   USE icbdia         ! iceberg diagnostics
   USE icbutl         ! iceberg utility routines
   USE icb_oce        ! iceberg parameters 

   USE sbc_oce        ! for icesheet freshwater input variables 
   USE in_out_manager 
   USE iom 

   IMPLICIT NONE
   PRIVATE

   PUBLIC   icb_clv_flx  ! routine called in icbstp.F90 module
   PUBLIC   icb_clv      ! routine called in icbstp.F90 module

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id$
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE icb_clv_flx( kt )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE icb_clv_flx  ***
      !!
      !! ** Purpose :   accumulate ice available for calving into class arrays
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt
      !
      REAL(wp)      ::   zcalving_used, zdist, zfact
      REAL(wp), DIMENSION(1)      ::   zgreenland_calving_sum, zantarctica_calving_sum 
      LOGICAL       ::   ll_write
      INTEGER       ::   jn, ji, jj                    ! loop counters
      INTEGER       ::   imx                           ! temporary integer for max berg class
      LOGICAL, SAVE ::   ll_first_call = .TRUE.
      !!----------------------------------------------------------------------
      !
      ! Adapt calving flux and calving heat flux from coupler for use here
      ! Use interior mask: so no bergs in overlap areas and convert from km^3/year to kg/s
      ! this assumes that input is given as equivalent water flux so that pure water density is appropriate

      zfact = ( (1000._wp)**3 / ( NINT(rday) * nyear_len(1) ) ) * rn_rho_bergs
      berg_grid%calving(:,:) = src_calving(:,:) * zfact * tmask_i(:,:) * tmask(:,:,1)

      ! Heat in units of W/m2, and mask (just in case)
      berg_grid%calving_hflx(:,:) = src_calving_hflx(:,:) * tmask_i(:,:) * tmask(:,:,1)

      IF( lk_oasis) THEN
        ! nn_coupled_iceshelf_fluxes uninitialised unless lk_oasis=true
        IF( nn_coupled_iceshelf_fluxes .gt. 0 ) THEN
          ll_write = ((MOD( kt, sn_cfctl%ptimincr ) == 0) .OR. ( kt == nitend )) .AND. lwp .AND. ((nn_print>0))
          ! Adjust total calving rates so that sum of iceberg calving and iceshelf melting in the northern
          ! and southern hemispheres equals rate of increase of mass of greenland and antarctic ice sheets
          ! to preserve total freshwater conservation in coupled models without an active ice sheet model.

           zgreenland_calving_sum(1) = SUM( berg_grid%calving(:,:) * greenland_icesheet_mask(:,:) )
           IF( lk_mpp ) CALL mpp_sum( 'icbclv', zgreenland_calving_sum )
           WHERE( greenland_icesheet_mask(:,:) == 1.0 )                                                                                 &
          &    berg_grid%calving(:,:) = berg_grid%calving(:,:) * greenland_icesheet_mass_rate_of_change * rn_greenland_calving_fraction &
          &                                     / ( zgreenland_calving_sum(1) + 1.0e-10_wp )

           ! check
           IF(ll_write) WRITE(numout, *) 'Greenland iceberg calving climatology (kg/s) : ',zgreenland_calving_sum(1)
           zgreenland_calving_sum(1) = SUM( berg_grid%calving(:,:) * greenland_icesheet_mask(:,:) )
           IF( lk_mpp ) CALL mpp_sum( 'icbclv', zgreenland_calving_sum )
           IF(ll_write) WRITE(numout, *) 'Greenland iceberg calving adjusted value (kg/s) : ',zgreenland_calving_sum(1)

           zantarctica_calving_sum(1) = SUM( berg_grid%calving(:,:) * antarctica_icesheet_mask(:,:) )
           IF( lk_mpp ) CALL mpp_sum( 'icbclv', zantarctica_calving_sum )
           WHERE( antarctica_icesheet_mask(:,:) == 1.0 )                                                                              &
           berg_grid%calving(:,:) = berg_grid%calving(:,:) * antarctica_icesheet_mass_rate_of_change * rn_antarctica_calving_fraction &
          &                           / ( zantarctica_calving_sum(1) + 1.0e-10_wp )

           ! check
           IF(ll_write) WRITE(numout, *) 'Antarctica iceberg calving climatology (kg/s) : ',zantarctica_calving_sum(1)
           zantarctica_calving_sum(1) = SUM( berg_grid%calving(:,:) * antarctica_icesheet_mask(:,:) )
           IF( lk_mpp ) CALL mpp_sum( 'icbclv', zantarctica_calving_sum )
           IF(ll_write) WRITE(numout, *) 'Antarctica iceberg calving adjusted value (kg/s) : ',zantarctica_calving_sum(1)

        ENDIF
      ENDIF
   
      CALL iom_put( 'berg_calve', berg_grid%calving(:,:) )


      IF( ll_first_call .AND. .NOT. l_restarted_bergs ) THEN      ! This is a hack to simplify initialization
         ll_first_call = .FALSE.
         !do jn=1, nclasses
         !  where (berg_grid%calving==0.) berg_grid%stored_ice(:,:,jn)=0.
         !end do
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               IF( berg_grid%calving(ji,jj) /= 0._wp )                                          &    ! Need units of J
                  berg_grid%stored_heat(ji,jj) = SUM( berg_grid%stored_ice(ji,jj,:) ) *         &    ! initial stored ice in kg
                     &                   berg_grid%calving_hflx(ji,jj) * e1e2t(ji,jj) / berg_grid%calving(ji,jj)   ! J/s/m2 x m^2 
                     !                                                                                             ! = J/s/calving in kg/s
            END DO
         END DO
      ENDIF

      ! assume that all calving flux must be distributed even if distribution array does not sum
      ! to one - this may not be what is intended, but it's what you've got
      DO jj = 1, jpj
         DO ji = 1, jpi
            imx = berg_grid%maxclass(ji,jj)
            zdist = SUM( rn_distribution(1:nclasses) ) / SUM( rn_distribution(1:imx) )
            DO jn = 1, imx
               berg_grid%stored_ice(ji,jj,jn) = berg_grid%stored_ice(ji,jj,jn)     &
                  &                           + berg_dt * berg_grid%calving(ji,jj) * rn_distribution(jn) * zdist
            END DO
         END DO
      END DO

      ! before changing the calving, save the amount we're about to use and do budget
      zcalving_used = SUM( berg_grid%calving(:,:) )
      berg_grid%tmp(:,:) = berg_dt * berg_grid%calving_hflx(:,:) * e1e2t(:,:) * tmask_i(:,:)
      berg_grid%stored_heat (:,:) = berg_grid%stored_heat (:,:) + berg_grid%tmp(:,:)
      CALL icb_dia_income( kt,  zcalving_used, berg_grid%tmp )
      !
   END SUBROUTINE icb_clv_flx


   SUBROUTINE icb_clv( kt )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE icb_clv  ***
      !!
      !! ** Purpose :   This routine takes a stored ice field and calves to the ocean,
      !!                so the gridded array stored_ice has only non-zero entries at selected
      !!                wet points adjacent to known land based calving points
      !!
      !! ** method  : - Look at each grid point and see if there's enough for each size class to calve
      !!                If there is, a new iceberg is calved.  This happens in the order determined by
      !!                the class definition arrays (which in the default case is smallest first)
      !!                Note that only the non-overlapping part of the processor where icebergs are allowed
      !!                is considered
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt
      INTEGER       ::   ji, jj, jn   ! dummy loop indices
      INTEGER       ::   icnt, icntmax
      TYPE(iceberg) ::   newberg
      TYPE(point)   ::   newpt
      REAL(wp)      ::   zday, zcalved_to_berg, zheat_to_berg
      !!----------------------------------------------------------------------
      !
      icntmax = 0
      zday    = REAL(nday_year,wp) + REAL(nsec_day,wp)/86400.0_wp
      !
      DO jn = 1, nclasses
         DO jj = nicbdj, nicbej
            DO ji = nicbdi, nicbei
               !
               icnt = 0
               !
               DO WHILE (berg_grid%stored_ice(ji,jj,jn) >= rn_initial_mass(jn) * rn_mass_scaling(jn) )
                  !
                  newpt%lon = glamt(ji,jj)         ! at t-point (centre of the cell)
                  newpt%lat = gphit(ji,jj)
                  newpt%xi  = REAL( mig(ji), wp )
                  newpt%yj  = REAL( mjg(jj), wp )
                  !
                  newpt%uvel = 0._wp               ! initially at rest
                  newpt%vvel = 0._wp
                  !                                ! set berg characteristics
                  newpt%mass           = rn_initial_mass     (jn)
                  newpt%thickness      = rn_initial_thickness(jn)
                  newpt%width          = first_width         (jn)
                  newpt%length         = first_length        (jn)
                  newberg%mass_scaling = rn_mass_scaling     (jn)
                  newpt%mass_of_bits   = 0._wp                          ! no bergy
                  !
                  newpt%year   = nyear
                  newpt%day    = zday
                  newpt%heat_density = berg_grid%stored_heat(ji,jj) / berg_grid%stored_ice(ji,jj,jn)   ! This is in J/kg
                  !
                  CALL icb_utl_incr()
                  newberg%number(:) = num_bergs(:)
                  !
                  CALL icb_utl_add( newberg, newpt )
                  !
                  zcalved_to_berg = rn_initial_mass(jn) * rn_mass_scaling(jn)           ! Units of kg
                  !                                ! Heat content
                  zheat_to_berg           = zcalved_to_berg * newpt%heat_density             ! Units of J
                  berg_grid%stored_heat(ji,jj) = berg_grid%stored_heat(ji,jj) - zheat_to_berg
                  !                                ! Stored mass
                  berg_grid%stored_ice(ji,jj,jn) = berg_grid%stored_ice(ji,jj,jn) - zcalved_to_berg
                  !
                  icnt = icnt + 1
                  !
                  CALL icb_dia_calve(ji, jj, jn,  zcalved_to_berg, zheat_to_berg )
               END DO
               icntmax = MAX( icntmax, icnt )
            END DO
         END DO
      END DO
      !
      DO jn = 1, nclasses
         CALL lbc_lnk( 'icbclv', berg_grid%stored_ice(:,:,jn), 'T', 1._wp )
      END DO
      CALL lbc_lnk( 'icbclv', berg_grid%stored_heat, 'T', 1._wp )
      !
      IF( nn_verbose_level > 0 .AND. icntmax > 1 )   WRITE(numicb,*) 'icb_clv: icnt=', icnt,' on', narea
      !
   END SUBROUTINE  icb_clv

   !!======================================================================
END MODULE icbclv
