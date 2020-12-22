MODULE module_drv

   USE module_domain, ONLY : domain, get_ijk_from_grid
   USE module_configure, ONLY : grid_config_rec_type

   IMPLICIT NONE
   PRIVATE

   PUBLIC drv_allocate 
   PUBLIC drv_rcv
   PUBLIC drv_store_input

#ifdef WRF_USE_REGESM 
   LOGICAL, PARAMETER, PUBLIC :: drv_on = .TRUE.
#else
   LOGICAL, PARAMETER, PUBLIC :: drv_on = .FALSE.
#endif

#if ( RWORDSIZE == 8 )
   TYPE IMPORT_FLD
     REAL, ALLOCATABLE, DIMENSION(:,:) :: sst 
   END TYPE IMPORT_FLD
#else
   TYPE IMPORT_FLD
     REAL(kind=8), ALLOCATABLE, DIMENSION(:,:) :: sst    
   END TYPE IMPORT_FLD
#endif

   PUBLIC :: IMPORT_FLD
   TYPE(IMPORT_FLD), PUBLIC :: wrf_import

CONTAINS

   SUBROUTINE drv_allocate(grid)
     !!-------------------------------------------------------------------
     !!             ***  SUBROUTINE cpl_allocate  ***
     !!
     !! ** Purpose : Allocate temporary arrays to exchange data with coupler 
     !!--------------------------------------------------------------------
     TYPE(domain), INTENT(INOUT) :: grid
     !
     INTEGER :: ips,ipe,jps,jpe,kps,kpe ! domain dimension on 1 processor
     INTEGER :: ims,ime,jms,jme,kms,kme ! memory domain dimension on 1 processor 
     INTEGER :: ids,ide,jds,jde,kds,kde ! domain dimension
     !!--------------------------------------------------------------------
#if (EM_CORE == 1)
      CALL get_ijk_from_grid( grid, ids, ide, jds, jde, kds, kde, &
         &                          ims, ime, jms, jme, kms, kme, &
         &                          ips, ipe, jps, jpe, kps, kpe  )
!
      IF (.NOT. ALLOCATED(wrf_import%sst)) THEN
        ALLOCATE(wrf_import%sst(ips:ipe,jps:jpe))
        wrf_import%sst(ips:ipe,jps:jpe) = 1.0E20
      END IF 
#endif 
   END SUBROUTINE drv_allocate

   SUBROUTINE drv_rcv( kdomwrf, cdsuffix,            &
      &                ids, ide, jds, jde, kds, kde, &
      &                ims, ime, jms, jme, kms, kme, &
      &                ips, ipe, jps, jpe, kps, kpe, &
      &                max_edom, pdatacpl, pdataobs)
      !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE drv_rcv  ***
      !!
      !! ** Purpose :   receiving data from driver
      !!--------------------------------------------------------------------
      INTEGER,                                                   INTENT(IN   ) :: kdomwrf     ! wrf domain index
      CHARACTER(*),                                              INTENT(IN   ) :: cdsuffix    ! field name suffix
      INTEGER,                                                   INTENT(IN   ) :: ids,ide,jds,jde,kds,kde
      INTEGER,                                                   INTENT(IN   ) :: ims,ime,jms,jme,kms,kme
      INTEGER,                                                   INTENT(IN   ) :: ips,ipe,jps,jpe,kps,kpe
      INTEGER,                                                   INTENT(IN   ) :: max_edom    ! max number of external model domains
      REAL, DIMENSION(ims:ime,             jms:jme),           INTENT(  OUT) :: pdatacpl    ! coupling data
      REAL, DIMENSION(ims:ime,             jms:jme), OPTIONAL, INTENT(IN   ) :: pdataobs    ! observed data to be merged
      !
      INTEGER :: jext                                ! external domain index
      INTEGER :: ifldid                              ! field index
      REAL, DIMENSION(ips:ipe,jps:jpe) :: zdata   ! data received from the coupler
      REAL, DIMENSION(ips:ipe,jps:jpe) :: pcplmask    ! coupling mask
      !!--------------------------------------------------------------------

      DO jext = 1, max_edom
         IF( jext == 1 ) THEN
            pcplmask(ips:ipe,jps:jpe) = 0.0
            WHERE (wrf_import%sst(ips:ipe,jps:jpe) < 1.0E10)
              pcplmask(ips:ipe,jps:jpe) = 1.0
            END WHERE
            IF (PRESENT(pdataobs)) THEN
              pdatacpl(ips:ipe,jps:jpe) = pdataobs(ips:ipe,jps:jpe)*(1.0-pcplmask(ips:ipe,jps:jpe))
            ELSE
              pdatacpl(ips:ipe,jps:jpe) = 0.0
            END IF
            pdatacpl(ips:ipe,jps:jpe) = pdatacpl(ips:ipe,jps:jpe)+wrf_import%sst(ips:ipe,jps:jpe)*pcplmask(ips:ipe,jps:jpe)
         END IF
      END DO
     
   END SUBROUTINE drv_rcv 

   SUBROUTINE drv_store_input( grid, config_flags )
      !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE drv_store_input  ***
      !!
      !! ** Purpose : Store input data that will be merged later with data
      !received from the coupler
      !!--------------------------------------------------------------------
      TYPE(domain)                , INTENT(INOUT) :: grid
      TYPE (grid_config_rec_type) , INTENT(IN   ) :: config_flags
      !
      INTEGER :: ips,ipe,jps,jpe,kps,kpe ! domain dimension on 1 processor
      INTEGER :: ims,ime,jms,jme,kms,kme ! memory domain dimension on 1 processor 
      INTEGER :: ids,ide,jds,jde,kds,kde ! domain dimension
      LOGICAL :: llmust_store
      INTEGER :: jext          ! local loop indicees     
      !!--------------------------------------------------------------------

#if (EM_CORE == 1)
      CALL get_ijk_from_grid( grid, ids, ide, jds, jde, kds, kde, &
         &                          ims, ime, jms, jme, kms, kme, &
         &                          ips, ipe, jps, jpe, kps, kpe  )
      
      ! take care of variables read in AUXINPUT4... 
      ! AUXINPUT4 was just read if:
      ! 1) We asked (legally) for an AUXINPUT4 input AND this is the first time
      ! step AFTER an auxinput4_alarm was ringing
      ! OR
      ! 2) This is the first time step
      IF( ( config_flags%auxinput4_interval .NE. 0 .AND. config_flags%io_form_auxinput4 .NE. 0 .AND. grid%just_read_auxinput4 ) &
         .OR. grid%itimestep .EQ. 1 ) THEN
         
         ! store SST in SST_INPUT
         grid%sst_input(ips:ipe,jps:jpe) = grid%sst(ips:ipe,jps:jpe)   ! store SST into SST_INPUT 
         
         grid%just_read_auxinput4 = .FALSE.  ! the work as been done and not me done again until we reread data from AUXINPUT4
      
      END IF
#endif      
 
   END SUBROUTINE drv_store_input

END MODULE module_drv
