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
!     MYESMF Calendar Module
      module MYESMF_CalendarMod
!
!==============================================================================
!
! This file contains the Calendar class definition and all Calendar class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <MYESMF_TimeMgr.inc>

!==============================================================================
!BOPI
! !MODULE: MYESMF_CalendarMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class { \tt ESMC\_Calendar} implementation
!
! See {\tt ../include/ESMC\_Calendar.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from MYESMF base class
      use MYESMF_BaseMod

      ! inherit from base time class
      use MYESMF_BaseTimeMod

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------



      INTEGER, PARAMETER :: MONTHS_PER_YEAR = 12
      INTEGER, PARAMETER :: mday(MONTHS_PER_YEAR)   &
                          = (/31,28,31,30,31,30,31,31,30,31,30,31/)
      INTEGER, PARAMETER :: mdayleap(MONTHS_PER_YEAR) &
                          = (/31,29,31,30,31,30,31,31,30,31,30,31/)
      INTEGER, DIMENSION(365) :: daym
      INTEGER, DIMENSION(366) :: daymleap
      INTEGER :: mdaycum(0:MONTHS_PER_YEAR)
      INTEGER :: mdayleapcum(0:MONTHS_PER_YEAR)
      TYPE(MYESMF_BaseTime), TARGET :: monthbdys(0:MONTHS_PER_YEAR)
      TYPE(MYESMF_BaseTime), TARGET :: monthbdysleap(0:MONTHS_PER_YEAR)


!------------------------------------------------------------------------------
!     ! MYESMF_CalendarType
!
!     ! F90 "enum" type to match C++ ESMC_CalendarType enum

      type MYESMF_CalendarType
      private
        integer :: caltype
      end type

      type(MYESMF_CalendarType), parameter :: &
                               MYESMF_CAL_GREGORIAN =  MYESMF_CalendarType(1), &
                               MYESMF_CAL_JULIAN =     MYESMF_CalendarType(2), &
                           ! like Gregorian, except Feb always has 28 days
                               MYESMF_CAL_NOLEAP =     MYESMF_CalendarType(3), & 
                           ! 12 months, 30 days each
                               MYESMF_CAL_360DAY =     MYESMF_CalendarType(4), & 
                           ! user defined
                               MYESMF_CAL_GENERIC =    MYESMF_CalendarType(5), &
                           ! track base time seconds only
                               MYESMF_CAL_NOCALENDAR = MYESMF_CalendarType(6)

!------------------------------------------------------------------------------
!     ! MYESMF_Calendar
!
!     ! F90 class type to match C++ Calendar class in size only;
!     !  all dereferencing within class is performed by C++ implementation
!
!------------------------------------------------------------------------------
!
!     ! MYESMF_DaysPerYear
!
      type MYESMF_DaysPerYear
      private
        integer :: D        ! whole days per year
! Fractional days-per-year are not yet used in this implementation.  
!        integer :: Dn       ! fractional days per year numerator
!        integer :: Dd       ! fractional days per year denominator
      end type              ! e.g. for Venus, D=0, Dn=926, Dd=1000
!
!------------------------------------------------------------------------------
!     ! MYESMF_Calendar
!
!
      type MYESMF_Calendar
      private
        type(MYESMF_CalendarType) :: Type
! TBH:  When NO_DT_COMPONENT_INIT is set, code that uses F95 compile-time 
! TBH:  initialization of components of derived types is not included.  
! TBH:  Some older compilers, like PGI 5.x do not support this F95 feature.  
#ifdef NO_DT_COMPONENT_INIT
        logical :: Set
#else
        logical :: Set = .false.
#endif
        integer, dimension(MONTHS_PER_YEAR) :: DaysPerMonth
        integer :: SecondsPerDay
        integer :: SecondsPerYear
        type(MYESMF_DaysPerYear) :: DaysPerYear
      end type

!------------------------------------------------------------------------------
! !PUBLIC DATA:
   TYPE(MYESMF_Calendar), public, save, pointer :: defaultCal   ! Default Calendar


!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public MONTHS_PER_YEAR
      public mday
      public mdayleap
      public monthbdys
      public monthbdysleap
      public daym
      public daymleap
      public mdaycum
      public mdayleapcum
      public MYESMF_CalendarType
      public MYESMF_CAL_GREGORIAN, MYESMF_CAL_NOLEAP, &
             MYESMF_CAL_360DAY, MYESMF_CAL_NOCALENDAR
!      public MYESMF_CAL_JULIAN
!      public MYESMF_CAL_GENERIC
      public MYESMF_Calendar

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public MYESMF_CalendarCreate

! Required inherited and overridden MYESMF_Base class methods

      public MYESMF_CalendarInitialized ! Only in this implementation, intended
                                      ! to be private within MYESMF methods
!EOPI

!==============================================================================

      contains


!==============================================================================
!BOP
! !IROUTINE: MYESMF_CalendarCreate - Create a new MYESMF Calendar of built-in type

! !INTERFACE:
      ! Private name; call using MYESMF_CalendarCreate()
      function MYESMF_CalendarCreate(name, calendartype, rc)

! !RETURN VALUE:
      type(MYESMF_Calendar) :: MYESMF_CalendarCreate

! !ARGUMENTS:
      character (len=*),       intent(in),  optional :: name
      type(MYESMF_CalendarType), intent(in)            :: calendartype
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Creates and sets a {\tt calendar} to the given built-in
!     {\tt MYESMF\_CalendarType}. 
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt MYESMF\_CalendarCreate()}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          The name for the newly created calendar.  If not specified, a
!          default unique name will be generated: "CalendarNNN" where NNN
!          is a unique sequence number from 001 to 999.
!     \item[calendartype]
!          The built-in {\tt MYESMF\_CalendarType}.  Valid values are:
!            {\tt MYESMF\_CAL\_360DAY}, {\tt MYESMF\_CAL\_GREGORIAN},
!            {\tt MYESMF\_CAL\_JULIANDAY}, {\tt MYESMF\_CAL\_NOCALENDAR}, and
!            {\tt MYESMF\_CAL\_NOLEAP}.
!          See the "Time Manager Reference" document for a description of
!          each calendar type.
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      type(MYESMF_DaysPerYear) :: dayspy

      if ( present(rc) ) rc = MYESMF_FAILURE
! Calendar type is hard-coded.  Use MYESMF library if more flexibility is 
! needed.  
#ifdef NO_LEAP_CALENDAR
      if ( calendartype%caltype  /= MYESMF_CAL_NOLEAP%caltype ) then
         write(6,*) 'Not a valid calendar type for this implementation'
         write(6,*) 'This implementation only allows MYESMF_CAL_NOLEAP'
         write(6,*) 'calender type set to     = ', calendartype%caltype
         write(6,*) 'NO_LEAP calendar type is = ', MYESMF_CAL_NOLEAP%caltype
         return
      end if
      MYESMF_CalendarCreate%Type = MYESMF_CAL_NOLEAP
#else
      if ( calendartype%caltype  /= MYESMF_CAL_GREGORIAN%caltype ) then
         write(6,*) 'Not a valid calendar type for this implementation'
         write(6,*) 'This implementation only allows MYESMF_CAL_GREGORIAN'
         write(6,*) 'calender type set to     = ', calendartype%caltype
         write(6,*) 'GREGORIAN calendar type is = ', MYESMF_CAL_GREGORIAN%caltype
         return
      end if
      MYESMF_CalendarCreate%Type = MYESMF_CAL_GREGORIAN
#endif
! This is a bug on some systems -- need initial value set by compiler at 
! startup.  
! However, note that some older compilers do not support compile-time 
! initialization of data members of Fortran derived data types.  For example, 
! PGI 5.x compilers do not support this F95 feature.  See 
! NO_DT_COMPONENT_INIT.  
      MYESMF_CalendarCreate%Set = .true.
      MYESMF_CalendarCreate%SecondsPerDay = SECONDS_PER_DAY
! DaysPerYear and SecondsPerYear are incorrect for Gregorian calendars...  
      dayspy%D = size(daym)
!TBH:  TODO:  Replace DaysPerYear and SecondsPerYear with methods 
!TBH:  TODO:  since they only make sense for the NO_LEAP calendar!  
      MYESMF_CalendarCreate%DaysPerYear = dayspy
      MYESMF_CalendarCreate%SecondsPerYear = MYESMF_CalendarCreate%SecondsPerDay &
                                       * dayspy%D
!TBH:  TODO:  use mdayleap for leap-year calendar
      MYESMF_CalendarCreate%DaysPerMonth(:) = mday(:)

      if ( present(rc) ) rc = MYESMF_SUCCESS

      end function MYESMF_CalendarCreate


!==============================================================================
!BOP
! !IROUTINE: MYESMF_CalendarInitialized - check if calendar was created

! !INTERFACE:
      function MYESMF_CalendarInitialized(calendar)

! !RETURN VALUE:
      logical MYESMF_CalendarInitialized

! !ARGUMENTS:
      type(MYESMF_Calendar), intent(in)            :: calendar

! !DESCRIPTION:
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
! Note that return value from this function will be bogus for older compilers 
! that do not support compile-time initialization of data members of Fortran 
! derived data types.  For example, PGI 5.x compilers do not support this F95 
! feature.  At the moment, the call to this fuction is #ifdefd out when the 
! leap-year calendar is used so this is not an issue for WRF (see 
! NO_DT_COMPONENT_INIT).  
        MYESMF_CalendarInitialized = calendar%set

     end function MYESMF_CalendarInitialized

      end module MYESMF_CalendarMod
