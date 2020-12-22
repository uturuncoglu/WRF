! Various dummy type definitions and routines for the sole purpose of 
! mimicking newer MYESMF interface features without necessarily implementing 
! them.  

MODULE MYESMF_Stubs

   IMPLICIT NONE

   PRIVATE

! Bogus typedefs
   TYPE MYESMF_Grid
      INTEGER :: dummy
   END TYPE

   TYPE MYESMF_GridComp
      INTEGER :: dummy
   END TYPE

   TYPE MYESMF_State
      INTEGER :: dummy
   END TYPE

   TYPE MYESMF_VM
      INTEGER :: dummy
   END TYPE

   TYPE MYESMF_MsgType
      INTEGER :: mtype
   END TYPE
   TYPE(MYESMF_MsgType), PARAMETER  ::      &
      MYESMF_LOG_INFO  =   MYESMF_MsgType(1), &
      MYESMF_LOG_WARNING = MYESMF_MsgType(2), &
      MYESMF_LOG_ERROR =   MYESMF_MsgType(3)

   TYPE MYESMF_LOG
      INTEGER :: dummy
   END TYPE

   LOGICAL, private, save :: initialized = .false.

   PUBLIC MYESMF_Grid, MYESMF_GridComp, MYESMF_State, MYESMF_VM
   PUBLIC MYESMF_Initialize, MYESMF_Finalize, MYESMF_IsInitialized
   PUBLIC MYESMF_LogWrite, MYESMF_LOG, MYESMF_MsgType
   PUBLIC MYESMF_LOG_INFO, MYESMF_LOG_WARNING, MYESMF_LOG_ERROR

CONTAINS


! NOOP
   SUBROUTINE MYESMF_Initialize( vm, defaultcalkind, rc )
      USE myesmf_basemod
      USE myesmf_calendarmod
      TYPE(MYESMF_VM),           INTENT(IN   ), OPTIONAL :: vm
      TYPE(MYESMF_CalendarType), INTENT(IN   ), OPTIONAL :: defaultcalkind
      INTEGER,                 INTENT(  OUT), OPTIONAL :: rc

      TYPE(MYESMF_CalendarType) :: defaultCalType
      INTEGER :: status

      IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
      ! Initialize the default time manager calendar
      IF ( PRESENT(defaultcalkind) )THEN
         defaultCalType = defaultcalkind
      ELSE
         defaultCalType = MYESMF_CAL_NOLEAP
      END IF
      allocate( defaultCal )
      defaultCal = MYESMF_CalendarCreate( calendarType=defaultCalType, &
                        rc=status)

      ! initialize tables in time manager
      CALL initdaym

      IF (status .ne. MYESMF_SUCCESS) THEN
          PRINT *, "Error initializing the default time manager calendar"
          RETURN
      END IF
      initialized = .true.

      IF ( PRESENT( rc ) ) rc = MYESMF_SUCCESS
   END SUBROUTINE MYESMF_Initialize


   FUNCTION MYESMF_IsInitialized()
      LOGICAL MYESMF_IsInitialized
      MYESMF_IsInitialized = initialized
   END FUNCTION MYESMF_IsInitialized


! NOOP
   SUBROUTINE MYESMF_Finalize( rc )
      USE myesmf_basemod
      INTEGER, INTENT(  OUT), OPTIONAL :: rc
#if (defined SPMD) || (defined COUP_CSM)
#include <mpif.h>
#endif
      LOGICAL :: flag
      INTEGER :: ier

      IF ( PRESENT( rc ) ) rc = MYESMF_SUCCESS
#if (defined SPMD) || (defined COUP_CSM)
      CALL MPI_Finalized( flag, ier )
      IF ( ier .ne. mpi_success )THEN
        IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
      END IF
      IF ( .NOT. flag ) THEN
        CALL MPI_Finalize( ier ) 
        IF ( ier .ne. mpi_success )THEN
          IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
        END IF
      END IF
#endif
   END SUBROUTINE MYESMF_Finalize

! NOOP
   SUBROUTINE MYESMF_LogWrite( msg, MsgType, line, file, method, log, rc )
      USE myesmf_basemod
      CHARACTER(LEN=*), INTENT(IN) :: msg
      TYPE(MYESMF_MsgType), INTENT(IN) :: msgtype
      INTEGER, INTENT(IN), OPTIONAL :: line
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: method
      TYPE(MYESMF_LOG),TARGET,OPTIONAL :: log
      INTEGER, INTENT(OUT),OPTIONAL :: rc
      IF ( PRESENT( rc ) ) rc = MYESMF_SUCCESS
   END SUBROUTINE MYESMF_LogWrite


END MODULE MYESMF_Stubs


