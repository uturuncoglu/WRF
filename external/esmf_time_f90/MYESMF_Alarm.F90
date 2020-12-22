!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA license.
!
!==============================================================================
!
!     MYESMF Alarm Module
      module MYESMF_AlarmMod
!
!==============================================================================
!
! This file contains the Alarm class definition and all Alarm class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <MYESMF_TimeMgr.inc>

!===============================================================================
!BOPI
!
! !MODULE: MYESMF_AlarmMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Alarm}
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from MYESMF base class
      use MYESMF_BaseMod

      ! associated derived types
      use MYESMF_TimeIntervalMod, only : MYESMF_TimeInterval, &
                                       MYESMF_TimeIntervalAbsValue
      use MYESMF_TimeMod,         only : MYESMF_Time

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
     private
!------------------------------------------------------------------------------
!     ! MYESMF_Alarm
!
!     ! F90 class type to match C++ Alarm class in size only;
!     !  all dereferencing within class is performed by C++ implementation

! internals for MYESMF_Alarm
      type MYESMF_AlarmInt
        type(MYESMF_TimeInterval) :: RingInterval
        type(MYESMF_Time)  :: RingTime
        type(MYESMF_Time)  :: PrevRingTime
        type(MYESMF_Time)  :: StopTime
        integer :: ID
        integer :: AlarmMutex
        logical :: Ringing
        logical :: Enabled
        logical :: RingTimeSet
        logical :: RingIntervalSet
        logical :: StopTimeSet
      end type

! Actual public type:  this bit allows easy mimic of "deep" MYESMF_AlarmCreate
! in MYESMF 2.1.0+.  Note that MYESMF_AlarmCreate is in a separate module to avoid 
! cyclic dependence.  
! NOTE:  DO NOT ADD NON-POINTER STATE TO THIS DATA TYPE.  It emulates MYESMF 
!        shallow-copy-masquerading-as-reference-copy insanity.  
      type MYESMF_Alarm
        type(MYESMF_AlarmInt), pointer :: alarmint
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public MYESMF_Alarm
      public MYESMF_AlarmInt   ! needed on AIX but not PGI
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public MYESMF_AlarmDestroy
      public MYESMF_AlarmSet
      public MYESMF_AlarmGet
!      public MYESMF_AlarmGetRingInterval
!      public MYESMF_AlarmSetRingInterval
!      public MYESMF_AlarmGetRingTime
!      public MYESMF_AlarmSetRingTime
!      public MYESMF_AlarmGetPrevRingTime
!      public MYESMF_AlarmSetPrevRingTime
!      public MYESMF_AlarmGetStopTime
!      public MYESMF_AlarmSetStopTime
      public MYESMF_AlarmEnable
      public MYESMF_AlarmDisable
      public MYESMF_AlarmRingerOn
      public MYESMF_AlarmRingerOff
      public MYESMF_AlarmIsRinging
!      public MYESMF_AlarmCheckRingTime
      public operator(==)
 
! Required inherited and overridden MYESMF_Base class methods

!      public MYESMF_AlarmRead
!      public MYESMF_AlarmWrite
      public MYESMF_AlarmValidate
      public MYESMF_AlarmPrint

! !PRIVATE MEMBER FUNCTIONS:
      private MYESMF_AlarmEQ
!EOPI

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator(==)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure MYESMF_AlarmEQ

! !DESCRIPTION:
!     This interface overloads the == operator for the {\tt MYESMF\_Alarm} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_AlarmSet - Initializes an alarm

! !INTERFACE:
      subroutine MYESMF_AlarmSet(alarm, RingTime, RingInterval, PrevRingTime, &
                               StopTime, Enabled, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      type(MYESMF_Time), intent(in), optional :: RingTime, PrevRingTime
      type(MYESMF_TimeInterval), intent(in), optional :: RingInterval
      type(MYESMF_Time), intent(in), optional :: StopTime
      logical, intent(in), optional :: Enabled
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes an {\tt MYESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to initialize
!     \item[{[RingTime]}]
!          Optional ring time for one-shot or first repeating alarm
!     \item[{[RingInterval]}]
!          Optional ring interval for repeating alarms
!     \item[{[StopTime]}]
!          Optional stop time for repeating alarms
!     \item[Enabled]
!          Alarm enabled/disabled
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.1, TMG4.7
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%RingTimeSet = .FALSE.
        alarm%alarmint%RingIntervalSet = .FALSE.
        alarm%alarmint%StopTimeSet = .FALSE.
        IF ( PRESENT( RingInterval ) ) THEN
          ! force RingInterval to be positive
          alarm%alarmint%RingInterval = &
            MYESMF_TimeIntervalAbsValue( RingInterval )
          alarm%alarmint%RingIntervalSet = .TRUE.
        ENDIF
        IF ( PRESENT( PrevRingTime ) ) THEN
          alarm%alarmint%PrevRingTime = PrevRingTime
        ENDIF
        IF ( PRESENT( RingTime ) ) THEN
          alarm%alarmint%RingTime = RingTime
          alarm%alarmint%RingTimeSet = .TRUE.
        ENDIF
        IF ( PRESENT( StopTime ) ) THEN
          alarm%alarmint%StopTime = StopTime
          alarm%alarmint%StopTimeSet = .TRUE.
        ENDIF
        alarm%alarmint%Enabled = .TRUE.
        IF ( PRESENT( Enabled ) ) THEN
          alarm%alarmint%Enabled = Enabled
        ENDIF
        IF ( PRESENT( rc ) ) THEN
          rc = MYESMF_SUCCESS
        ENDIF
        alarm%alarmint%Ringing = .FALSE.
        alarm%alarmint%Enabled = .TRUE.
      ELSE
        IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
      ENDIF

      end subroutine MYESMF_AlarmSet



! Deallocate memory for MYESMF_Alarm
      SUBROUTINE MYESMF_AlarmDestroy( alarm, rc )
         TYPE(MYESMF_Alarm), INTENT(INOUT) :: alarm
         INTEGER,          INTENT(  OUT), OPTIONAL :: rc
         IF ( ASSOCIATED( alarm%alarmint ) ) THEN
           DEALLOCATE( alarm%alarmint )
         ENDIF
         ! TBH:  ignore deallocate errors, for now
         IF ( PRESENT( rc ) ) rc = MYESMF_SUCCESS
      END SUBROUTINE MYESMF_AlarmDestroy



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_AlarmGetRingInterval - Get an alarm's ring interval
!
! !INTERFACE:
      subroutine MYESMF_AlarmGetRingInterval(alarm, RingInterval, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(in) :: alarm
      type(MYESMF_TimeInterval), intent(out) :: RingInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt MYESMF\_Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.7
!EOP
      RingInterval = alarm%alarmint%RingInterval

      end subroutine MYESMF_AlarmGetRingInterval
 
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_AlarmSetRingInterval - Set an alarm's ring interval
!
! !INTERFACE:
      subroutine MYESMF_AlarmSetRingInterval(alarm, RingInterval, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(out) :: alarm
      type(MYESMF_TimeInterval), intent(in) :: RingInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt MYESMF\_Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'MYESMF_AlarmSetRingInterval not supported' )
      end subroutine MYESMF_AlarmSetRingInterval

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmGetRingTime - Get an alarm's time to ring
!
! !INTERFACE:
      subroutine MYESMF_AlarmGetRingTime(alarm, RingTime, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(in) :: alarm
      type(MYESMF_Time), intent(out) :: RingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt MYESMF\_Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring time
!     \item[RingTime]
!          The {\tt MYESMF\_Alarm}'s ring time
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'MYESMF_AlarmGetRingTime not supported' )
      end subroutine MYESMF_AlarmGetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmSetRingTime - Set an alarm's time to ring
!
! !INTERFACE:
      subroutine MYESMF_AlarmSetRingTime(alarm, RingTime, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(out) :: alarm
      type(MYESMF_Time), intent(in) :: RingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt MYESMF\_Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring time
!     \item[RingTime]
!          The {\tt MYESMF\_Alarm}'s ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.1, TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'MYESMF_AlarmSetRingTime not supported' )
      end subroutine MYESMF_AlarmSetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmGet - Get an alarm's parameters -- compatibility with MYESMF 2.0.1
!
! !INTERFACE:
      subroutine MYESMF_AlarmGet(alarm, PrevRingTime, RingInterval, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(in) :: alarm
      type(MYESMF_Time), intent(out), optional :: PrevRingTime
      type(MYESMF_TimeInterval), intent(out), optional :: RingInterval
      integer, intent(out), optional :: rc
      integer :: ierr

! !DESCRIPTION:
!     Get an {\tt MYESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the previous ring time
!     \item[PrevRingTime]
!          The {\tt MYESMF\_Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP

      ierr = MYESMF_SUCCESS

      IF ( PRESENT(PrevRingTime) ) THEN
        CALL MYESMF_AlarmGetPrevRingTime(alarm, PrevRingTime, rc=ierr)
      ENDIF
      IF ( PRESENT(RingInterval) ) THEN
        CALL MYESMF_AlarmGetRingInterval(alarm, RingInterval, rc=ierr)
      ENDIF

      IF ( PRESENT(rc) ) THEN
        rc = ierr
      ENDIF

      end subroutine MYESMF_AlarmGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmGetPrevRingTime - Get an alarm's previous ring time
!
! !INTERFACE:
      subroutine MYESMF_AlarmGetPrevRingTime(alarm, PrevRingTime, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(in) :: alarm
      type(MYESMF_Time), intent(out) :: PrevRingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt MYESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the previous ring time
!     \item[PrevRingTime]
!          The {\tt MYESMF\_Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        PrevRingTime = alarm%alarmint%PrevRingTime
        IF ( PRESENT( rc ) ) rc = MYESMF_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
      ENDIF
      end subroutine MYESMF_AlarmGetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmSetPrevRingTime - Set an alarm's previous ring time
!
! !INTERFACE:
      subroutine MYESMF_AlarmSetPrevRingTime(alarm, PrevRingTime, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(out) :: alarm
      type(MYESMF_Time), intent(in) :: PrevRingTime
      integer, intent(out), optional :: rc
   
! !DESCRIPTION:
!     Set an {\tt MYESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the previous ring time
!     \item[PrevRingTime]
!          The {\tt MYESMF\_Alarm}'s previous ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'MYESMF_AlarmSetPrevRingTime not supported' )
      end subroutine MYESMF_AlarmSetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmGetStopTime - Get an alarm's stop time
!
! !INTERFACE:
      subroutine MYESMF_AlarmGetStopTime(alarm, StopTime, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(in) :: alarm
      type(MYESMF_Time), intent(out) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt MYESMF\_Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the stop time
!     \item[StopTime]
!          The {\tt MYESMF\_Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'MYESMF_AlarmGetStopTime not supported' )
      end subroutine MYESMF_AlarmGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmSetStopTime - Set an alarm's stop time
!
! !INTERFACE:
      subroutine MYESMF_AlarmSetStopTime(alarm, StopTime, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(out) :: alarm
      type(MYESMF_Time), intent(in) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt MYESMF\_Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the stop time
!     \item[StopTime]
!          The {\tt MYESMF\_Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'MYESMF_AlarmSetStopTime not supported' )
      end subroutine MYESMF_AlarmSetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_AlarmEnable - Enables an alarm

! !INTERFACE:
      subroutine MYESMF_AlarmEnable(alarm, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Enables an {\tt MYESMF\_Alarm} to function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to enable
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Enabled = .TRUE.
        IF ( PRESENT( rc ) ) rc = MYESMF_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
      ENDIF
      end subroutine MYESMF_AlarmEnable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_AlarmDisable - Disables an alarm

! !INTERFACE:
      subroutine MYESMF_AlarmDisable(alarm, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Disables an {\tt MYESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to disable
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Enabled = .FALSE.
        IF ( PRESENT( rc ) ) rc = MYESMF_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
      ENDIF
      end subroutine MYESMF_AlarmDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmRingerOn - Turn on an alarm


! !INTERFACE:
      subroutine MYESMF_AlarmRingerOn(alarm, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn on an {\tt MYESMF\_Alarm}; sets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn on
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.6
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%Enabled ) THEN
          alarm%alarmint%Ringing = .TRUE.
          IF ( PRESENT( rc ) ) rc = MYESMF_SUCCESS
        ELSE
          alarm%alarmint%Ringing = .FALSE.
          IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
      ENDIF

      end subroutine MYESMF_AlarmRingerOn

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmRingerOff - Turn off an alarm

! !INTERFACE:
      subroutine MYESMF_AlarmRingerOff(alarm, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn off an {\tt MYESMF\_Alarm}; unsets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn off   
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.6
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Ringing = .FALSE.
        IF ( alarm%alarmint%Enabled ) THEN
          IF ( PRESENT( rc ) ) rc = MYESMF_SUCCESS
        ELSE
          IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
      ENDIF
      end subroutine MYESMF_AlarmRingerOff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmIsRinging - Check if alarm is ringing

! !INTERFACE:
      function MYESMF_AlarmIsRinging(alarm, rc)
!
! !RETURN VALUE:
      logical :: MYESMF_AlarmIsRinging

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(in) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt MYESMF\_Alarm} is ringing.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check for ringing state  
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%Enabled ) THEN
          MYESMF_AlarmIsRinging = alarm%alarmint%Ringing
          IF ( PRESENT( rc ) ) rc = MYESMF_SUCCESS
        ELSE
          MYESMF_AlarmIsRinging = .FALSE.
          IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
      ENDIF
      end function MYESMF_AlarmIsRinging

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_AlarmCheckRingTime - Method used by a clock to check whether to trigger an alarm
!
! !INTERFACE:
      function MYESMF_AlarmCheckRingTime(alarm, ClockCurrTime, positive, rc)
!
! !RETURN VALUE:
      logical :: MYESMF_AlarmCheckRingTime
!
! !ARGUMENTS:
      type(MYESMF_Alarm), intent(inout) :: alarm
      type(MYESMF_Time), intent(in) :: ClockCurrTime
      integer, intent(in) :: positive
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Main method used by a {\tt MYESMF\_Clock} to check whether to trigger
!     the {\tt MYESMF\_Alarm} 
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check if time to ring   
!     \item[ClockCurrTime]
!          The {\tt MYESMF\_Clock}'s current time
!     \item[positive]
!          Whether to check ring time in the positive or negative direction
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4, TMG4.6
!EOP
      CALL wrf_error_fatal( 'MYESMF_AlarmCheckRingTime not supported' )
      MYESMF_AlarmCheckRingTime = .FALSE.  ! keep compilers happy
      end function MYESMF_AlarmCheckRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmEQ - Compare two alarms for equality
!
! !INTERFACE:
      function MYESMF_AlarmEQ(alarm1, alarm2)
!
! !RETURN VALUE:
      logical :: MYESMF_AlarmEQ

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(in) :: alarm1
      type(MYESMF_Alarm), intent(in) :: alarm2

! !DESCRIPTION:
!     Compare two alarms for equality; return true if equal, false otherwise
!     Maps to overloaded (==) operator interface function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm1]
!          The first {\tt MYESMF\_Alarm} to compare
!     \item[alarm2]
!          The second {\tt MYESMF\_Alarm} to compare
!     \end{description}
!
! !REQUIREMENTS:  
!EOP
      CALL wrf_error_fatal( 'MYESMF_AlarmEQ not supported ' )
      MYESMF_AlarmEQ = .FALSE.       ! keep compilers happy
      end function MYESMF_AlarmEQ

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the MYESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_AlarmRead - restores an alarm

! !INTERFACE:
      subroutine MYESMF_AlarmRead(alarm, RingInterval, RingTime, &
                           PrevRingTime, StopTime, Ringing, &
                           Enabled, ID, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(out) :: alarm
      type(MYESMF_TimeInterval), intent(in) :: RingInterval
      type(MYESMF_Time), intent(in) :: RingTime
      type(MYESMF_Time), intent(in) :: PrevRingTime
      type(MYESMF_Time), intent(in) :: StopTime
      logical, intent(in) :: Ringing
      logical, intent(in) :: Enabled
      integer, intent(in) :: ID
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt MYESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to restore
!     \item[RingInterval]
!          The ring interval for repeating alarms
!     \item[RingTime]
!          Ring time for one-shot or first repeating alarm
!     \item[PrevRingTime]
!          The {\tt MYESMF\_Alarm}'s previous ring time
!     \item[StopTime]
!          Stop time for repeating alarms
!     \item[Ringing]
!          The {\tt MYESMF\_Alarm}'s ringing state
!     \item[Enabled]
!          {\tt MYESMF\_Alarm} enabled/disabled
!     \item[ID]
!          The {\tt MYESMF\_Alarm}'s ID
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'MYESMF_AlarmRead not supported' )
      end subroutine MYESMF_AlarmRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_AlarmWrite - saves an alarm

! !INTERFACE:
      subroutine MYESMF_AlarmWrite(alarm, RingInterval, RingTime, &
                            PrevRingTime, StopTime, Ringing, &
                            Enabled, ID, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(in) :: alarm
      type(MYESMF_TimeInterval), intent(out) :: RingInterval
      type(MYESMF_Time), intent(out) :: RingTime
      type(MYESMF_Time), intent(out) :: PrevRingTime
      type(MYESMF_Time), intent(out) :: StopTime
      logical, intent(out) :: Ringing
      logical, intent(out) :: Enabled
      integer, intent(out) :: ID
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt MYESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to save
!     \item[RingInterval]
!          Ring interval for repeating alarms
!     \item[RingTime]
!          Ring time for one-shot or first repeating alarm
!     \item[PrevRingTime]
!          The {\tt MYESMF\_Alarm}'s previous ring time
!     \item[StopTime]
!          Stop time for repeating alarms
!     \item[Ringing]
!          The {\tt MYESMF\_Alarm}'s ringing state
!     \item[Enabled]
!          {\tt MYESMF\_Alarm} enabled/disabled
!     \item[ID]
!          The {\tt MYESMF\_Alarm}'s ID
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'MYESMF_AlarmWrite not supported' )
      end subroutine MYESMF_AlarmWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmValidate - Validate an Alarm's properties

! !INTERFACE:
      subroutine MYESMF_AlarmValidate(alarm, opts, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(in) :: alarm
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt MYESMF\_Alarm}'s properties
!
!     The arguments are:  
!     \begin{description}
!     \item[alarm]
!          {\tt MYESMF\_Alarm} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'MYESMF_AlarmValidate not supported' )
      end subroutine MYESMF_AlarmValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AlarmPrint - Print out an Alarm's properties

! !INTERFACE:
      subroutine MYESMF_AlarmPrint(alarm, opts, rc)

! !ARGUMENTS:
      type(MYESMF_Alarm), intent(in) :: alarm
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt MYESMF\_Alarm}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          {\tt MYESMF\_Alarm} to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'MYESMF_AlarmPrint not supported' )
      end subroutine MYESMF_AlarmPrint

!------------------------------------------------------------------------------

      end module MYESMF_AlarmMod
