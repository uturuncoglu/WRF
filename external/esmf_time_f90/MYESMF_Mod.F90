! TBH:  This version is for use with the MYESMF library embedded in the WRF 
! TBH:  distribution.  
MODULE MYESMF_Mod
   USE myesmf_alarmmod
   USE myesmf_basemod
   USE myesmf_basetimemod
   USE myesmf_calendarmod
   USE myesmf_clockmod
   USE myesmf_fractionmod
   USE myesmf_timeintervalmod
   USE myesmf_timemod
   USE myesmf_alarmclockmod
   USE myesmf_stubs   ! add new dummy interfaces and typedefs here as needed
#include <MYESMF_TimeMgr.inc>
   INTEGER, PARAMETER :: MYESMF_MAX_ALARMS=MAX_ALARMS
!
END MODULE MYESMF_Mod
