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
!     MYESMF Alarm-Clock Module
      module MYESMF_AlarmClockMod
!
!==============================================================================
!
! This file contains the AlarmCreate method.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <MYESMF_TimeMgr.inc>

!===============================================================================
!BOPI
!
! !MODULE: MYESMF_AlarmClockMod
!
! !DESCRIPTION:
! Separate module that uses both MYESMF_AlarmMod and MYESMF_ClockMod.  
! Separation is needed to avoid cyclic dependence.  
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Alarm}
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit MYESMF_Alarm and MYESMF_Clock
      use MYESMF_AlarmMod, only : MYESMF_Alarm, MYESMF_AlarmSet
      use MYESMF_ClockMod, only : MYESMF_Clock, MYESMF_ClockAddAlarm

      ! associated derived types
      use MYESMF_TimeIntervalMod, only : MYESMF_TimeInterval
      use MYESMF_TimeMod,         only : MYESMF_Time

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
     private
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public MYESMF_AlarmCreate

!==============================================================================

      contains

!==============================================================================


! Create MYESMF_Alarm using MYESMF 2.1.0+ semantics
      FUNCTION MYESMF_AlarmCreate( clock, RingTime, RingInterval, &
                                 StopTime, Enabled, rc )

        ! return value
        type(MYESMF_Alarm) :: MYESMF_AlarmCreate
        ! !ARGUMENTS:
        type(MYESMF_Clock), intent(inout), optional :: clock
        type(MYESMF_Time), intent(in), optional :: RingTime
        type(MYESMF_TimeInterval), intent(in), optional :: RingInterval
        type(MYESMF_Time), intent(in), optional :: StopTime
        logical, intent(in), optional :: Enabled
        integer, intent(out), optional :: rc
        ! locals
        type(MYESMF_Alarm) :: alarmtmp
         ! TBH:  ignore allocate errors, for now
        ALLOCATE( alarmtmp%alarmint )
        CALL MYESMF_AlarmSet( alarmtmp,                  &
                            RingTime=RingTime,         &
                            RingInterval=RingInterval, &
                            StopTime=StopTime,         &
                            Enabled=Enabled,           &
                            rc=rc )
        IF ( PRESENT ( clock ) ) THEN
          CALL MYESMF_ClockAddAlarm( clock, alarmtmp, rc )
        ENDIF
        MYESMF_AlarmCreate = alarmtmp
      END FUNCTION MYESMF_AlarmCreate


!------------------------------------------------------------------------------

      end module MYESMF_AlarmClockMod
