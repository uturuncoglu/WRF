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
!     MYESMF Time Module
      module MYESMF_TimeMod
!
!==============================================================================
!
! This file contains the Time class definition and all Time class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <MYESMF_TimeMgr.inc>

!==============================================================================
!BOPI
! !MODULE: MYESMF_TimeMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Time} implementation
!
! See {\tt ../include/ESMC\_Time.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from MYESMF base class
      use MYESMF_BaseMod

      ! inherit from base time class
      use MYESMF_BaseTimeMod

      ! associated derived types
      use MYESMF_TimeIntervalMod
      use MYESMF_CalendarMod
      use MYESMF_Stubs

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! MYESMF_Time
!
!     ! F90 class type to match C++ Time class in size only;
!     !  all dereferencing within class is performed by C++ implementation

     type MYESMF_Time
       type(MYESMF_BaseTime) :: basetime           ! inherit base class
       ! time instant is expressed as year + basetime
       integer :: YR
       type(MYESMF_Calendar), pointer :: calendar  ! associated calendar
     end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public MYESMF_Time
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public MYESMF_TimeGet
      public MYESMF_TimeSet

! Required inherited and overridden MYESMF_Base class methods

      public MYESMF_TimeCopy

! !PRIVATE MEMBER FUNCTIONS:

      private MYESMF_TimeGetDayOfYear
      private MYESMF_TimeGetDayOfYearInteger

! Inherited and overloaded from MYESMF_BaseTime

      ! NOTE:  MYESMF_TimeInc, MYESMF_TimeDec, MYESMF_TimeDiff, MYESMF_TimeEQ, 
      !        MYESMF_TimeNE, MYESMF_TimeLT, MYESMF_TimeGT, MYESMF_TimeLE, and 
      !        MYESMF_TimeGE are PUBLIC only to work around bugs in the 
      !        PGI 5.1-x compilers.  They should all be PRIVATE.  

      public operator(+)
      public MYESMF_TimeInc

      public operator(-)
      public MYESMF_TimeDec
      public MYESMF_TimeDec2
      public MYESMF_TimeDiff

      public operator(.EQ.)
      public MYESMF_TimeEQ

      public operator(.NE.)
      public MYESMF_TimeNE

      public operator(.LT.)
      public MYESMF_TimeLT

      public operator(.GT.)
      public MYESMF_TimeGT

      public operator(.LE.)
      public MYESMF_TimeLE

      public operator(.GE.)
      public MYESMF_TimeGE

!EOPI

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface MYESMF_TimeGetDayOfYear

! !PRIVATE MEMBER FUNCTIONS:
      module procedure MYESMF_TimeGetDayOfYearInteger

! !DESCRIPTION:
!     This interface overloads the {\tt MYESMF\_GetDayOfYear} method
!     for the {\tt MYESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(+)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure MYESMF_TimeInc, MYESMF_TimeInc2

! !DESCRIPTION:
!     This interface overloads the + operator for the {\tt MYESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface assignment (=)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure MYESMF_TimeCopy

! !DESCRIPTION:
!     This interface overloads the = operator for the {\tt MYESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(-)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure MYESMF_TimeDec, MYESMF_TimeDec2

! !DESCRIPTION:
!     This interface overloads the - operator for the {\tt MYESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(-)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure MYESMF_TimeDiff

! !DESCRIPTION:
!     This interface overloads the - operator for the {\tt MYESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.EQ.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure MYESMF_TimeEQ

! !DESCRIPTION:
!     This interface overloads the .EQ. operator for the {\tt MYESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.NE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure MYESMF_TimeNE

! !DESCRIPTION:
!     This interface overloads the .NE. operator for the {\tt MYESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.LT.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure MYESMF_TimeLT

! !DESCRIPTION:
!     This interface overloads the .LT. operator for the {\tt MYESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.GT.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure MYESMF_TimeGT

! !DESCRIPTION:
!     This interface overloads the .GT. operator for the {\tt MYESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.LE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure MYESMF_TimeLE

! !DESCRIPTION:
!     This interface overloads the .LE. operator for the {\tt MYESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.GE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure MYESMF_TimeGE

! !DESCRIPTION:
!     This interface overloads the .GE. operator for the {\tt MYESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================
!
! Generic Get/Set routines which use F90 optional arguments
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_TimeGet - Get value in user-specified units

! !INTERFACE:
      subroutine MYESMF_TimeGet(time, YY, YRl, MM, DD, D, Dl, H, M, S, Sl, MS, &
                              US, NS, d_, h_, m_, s_, ms_, us_, ns_, Sn, Sd, &
                              dayOfYear, dayOfYear_r8, dayOfYear_intvl,      &
                              timeString, rc)

! !ARGUMENTS:
      type(MYESMF_Time), intent(in) :: time
      integer, intent(out), optional :: YY
      integer(MYESMF_KIND_I8), intent(out), optional :: YRl
      integer, intent(out), optional :: MM
      integer, intent(out), optional :: DD
      integer, intent(out), optional :: D
      integer(MYESMF_KIND_I8), intent(out), optional :: Dl
      integer, intent(out), optional :: H
      integer, intent(out), optional :: M
      integer, intent(out), optional :: S
      integer(MYESMF_KIND_I8), intent(out), optional :: Sl
      integer, intent(out), optional :: MS
      integer, intent(out), optional :: US
      integer, intent(out), optional :: NS
      double precision, intent(out), optional :: d_
      double precision, intent(out), optional :: h_
      double precision, intent(out), optional :: m_
      double precision, intent(out), optional :: s_
      double precision, intent(out), optional :: ms_
      double precision, intent(out), optional :: us_
      double precision, intent(out), optional :: ns_
      integer, intent(out), optional :: Sn
      integer, intent(out), optional :: Sd
      integer, intent(out), optional :: dayOfYear
      ! dayOfYear_r8 = 1.0 at 0Z on 1 January, 1.5 at 12Z on
      ! 1 January, etc.
      real(MYESMF_KIND_R8), intent(out), optional :: dayOfYear_r8
      character (len=*), intent(out), optional :: timeString
      type(MYESMF_TimeInterval), intent(out), optional :: dayOfYear_intvl
      integer, intent(out), optional :: rc

      type(MYESMF_TimeInterval) :: day_step
      integer :: ierr

! !DESCRIPTION:
!     Get the value of the {\tt MYESMF\_Time} in units specified by the user
!     via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers
!     to maintain precision. Hence, user-specified floating point values are
!     converted internally from integers.
!
!     See {\tt ../include/ESMC\_BaseTime.h and ../include/ESMC\_Time.h} for
!     complete description.
!     
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[{[YY]}]
!          Integer year CCYR (>= 32-bit)
!     \item[{[YRl]}]
!          Integer year CCYR (large, >= 64-bit)
!     \item[{[MM]}]
!          Integer month 1-12
!     \item[{[DD]}]
!          Integer day of the month 1-31
!     \item[{[D]}]
!          Integer Julian days (>= 32-bit)
!     \item[{[Dl]}]
!          Integer Julian days (large, >= 64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (>= 32-bit)
!     \item[{[Sl]}]
!          Integer seconds (large, >= 64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.1, TMG2.5.1, TMG2.5.6
!EOP
      TYPE(MYESMF_Time) :: begofyear
      INTEGER :: year, month, dayofmonth, hour, minute, second
      REAL(MYESMF_KIND_R8) :: rsec

      ierr = MYESMF_SUCCESS

      IF ( PRESENT( YY ) ) THEN
        YY = time%YR
      ENDIF
      IF ( PRESENT( MM ) ) THEN
        CALL timegetmonth( time, MM )
      ENDIF
      IF ( PRESENT( DD ) ) THEN
        CALL timegetdayofmonth( time, DD )
      ENDIF
!
!$$$ Push HMS down into MYESMF_BaseTime from EVERYWHERE
!$$$ and THEN add MYESMF scaling behavior when other args are present...  
      IF ( PRESENT( H ) ) THEN
        H = mod( time%basetime%S, SECONDS_PER_DAY ) / SECONDS_PER_HOUR
      ENDIF
      IF ( PRESENT( M ) ) THEN
        M = mod( time%basetime%S, SECONDS_PER_HOUR) / SECONDS_PER_MINUTE
      ENDIF
      IF ( PRESENT( S ) ) THEN
        S = mod( time%basetime%S, SECONDS_PER_MINUTE )
      ENDIF
      ! TBH:  HACK to allow DD and S to behave as in MYESMF 2.1.0+ when 
      ! TBH:  both are present and H and M are not.  
      IF ( PRESENT( S ) .AND. PRESENT( DD ) ) THEN
        IF ( ( .NOT. PRESENT( H ) ) .AND. ( .NOT. PRESENT( M ) ) ) THEN
          S = mod( time%basetime%S, SECONDS_PER_DAY )
        ENDIF
      ENDIF
      IF ( PRESENT( MS ) ) THEN
        IF ( time%basetime%Sd /= 0 ) THEN
          MS = NINT( ( time%basetime%Sn*1.0D0 / time%basetime%Sd*1.0D0 ) * 1000.0D0 )
        ELSE
          MS = 0
        ENDIF
      ENDIF
      IF ( PRESENT( Sd ) .AND. PRESENT( Sn ) ) THEN
        Sd = time%basetime%Sd
        Sn = time%basetime%Sn
      ENDIF
      IF ( PRESENT( dayOfYear ) ) THEN
        CALL MYESMF_TimeGetDayOfYear( time, dayOfYear, rc=ierr )
      ENDIF
      IF ( PRESENT( dayOfYear_r8 ) ) THEN
        ! 64-bit IEEE 754 has 52-bit mantisssa -- only need 25 bits to hold 
        ! number of seconds in a year...  
        rsec = REAL( time%basetime%S, MYESMF_KIND_R8 )
        IF ( time%basetime%Sd /= 0 ) THEN
          rsec = rsec + ( REAL( time%basetime%Sn, MYESMF_KIND_R8 ) / &
                          REAL( time%basetime%Sd, MYESMF_KIND_R8 ) )
        ENDIF
        dayOfYear_r8 = rsec / REAL( SECONDS_PER_DAY, MYESMF_KIND_R8 )
        ! start at 1
        dayOfYear_r8 = dayOfYear_r8 + 1.0_MYESMF_KIND_R8
      ENDIF
      IF ( PRESENT( timeString ) ) THEN
        ! This duplication for YMD is an optimization that avoids calling 
        ! timegetmonth() and timegetdayofmonth() when it is not needed.  
        year = time%YR
        CALL timegetmonth( time, month )
        CALL timegetdayofmonth( time, dayofmonth )
!$$$ push HMS down into MYESMF_BaseTime
        hour = mod( time%basetime%S, SECONDS_PER_DAY ) / SECONDS_PER_HOUR
        minute = mod( time%basetime%S, SECONDS_PER_HOUR) / SECONDS_PER_MINUTE
        second = mod( time%basetime%S, SECONDS_PER_MINUTE )
        CALL MYESMFold_TimeGetString( year, month, dayofmonth, &
                                    hour, minute, second, timeString )
      ENDIF
      IF ( PRESENT( dayOfYear_intvl ) ) THEN
        year = time%YR
        CALL MYESMF_TimeSet( begofyear, yy=year, mm=1, dd=1, s=0, &
                           calendar=time%calendar, rc=ierr )
        IF ( ierr == MYESMF_FAILURE)THEN
           rc = ierr
           RETURN
        END IF
        CALL MYESMF_TimeIntervalSet( day_step, d=1, s=0, rc=ierr )
        dayOfYear_intvl = time - begofyear + day_step
      ENDIF

      IF ( PRESENT( rc ) ) THEN
        rc = ierr
      ENDIF

      end subroutine MYESMF_TimeGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_TimeSet - Initialize via user-specified unit set

! !INTERFACE:
      subroutine MYESMF_TimeSet(time, YY, YRl, MM, DD, D, Dl, H, M, S, Sl, &
                              MS, US, NS, d_, h_, m_, s_, ms_, us_, ns_, &
                              Sn, Sd, calendar, rc)

! !ARGUMENTS:
      type(MYESMF_Time), intent(inout) :: time
      integer, intent(in), optional :: YY
      integer(MYESMF_KIND_I8), intent(in), optional :: YRl
      integer, intent(in), optional :: MM
      integer, intent(in), optional :: DD
      integer, intent(in), optional :: D
      integer(MYESMF_KIND_I8), intent(in), optional :: Dl
      integer, intent(in), optional :: H
      integer, intent(in), optional :: M
      integer, intent(in), optional :: S
      integer(MYESMF_KIND_I8), intent(in), optional :: Sl
      integer, intent(in), optional :: MS
      integer, intent(in), optional :: US
      integer, intent(in), optional :: NS
      double precision, intent(in), optional :: d_
      double precision, intent(in), optional :: h_
      double precision, intent(in), optional :: m_
      double precision, intent(in), optional :: s_
      double precision, intent(in), optional :: ms_
      double precision, intent(in), optional :: us_
      double precision, intent(in), optional :: ns_
      integer, intent(in), optional :: Sn
      integer, intent(in), optional :: Sd
      type(MYESMF_Calendar), intent(in), target, optional :: calendar
      integer, intent(out), optional :: rc
      ! locals
      INTEGER :: ierr

! !DESCRIPTION:
!     Initializes a {\tt MYESMF\_Time} with a set of user-specified units
!     via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers
!     to maintain precision. Hence, user-specified floating point values are
!     converted internally to integers.
!
!     See {\tt ../include/ESMC\_BaseTime.h and ../include/ESMC\_Time.h} for
!     complete description.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to initialize
!     \item[{[YY]}]
!          Integer year CCYR (>= 32-bit)
!     \item[{[YRl]}]
!          Integer year CCYR (large, >= 64-bit)
!     \item[{[MM]}]
!          Integer month 1-12
!     \item[{[DD]}]
!          Integer day of the month 1-31
!     \item[{[D]}]
!          Integer Julian days (>= 32-bit)
!     \item[{[Dl]}]
!          Integer Julian days (large, >= 64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (>= 32-bit)
!     \item[{[Sl]}]
!          Integer seconds (large, >= 64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[cal]}]
!          Associated {\tt Calendar}
!     \item[{[tz]}]
!          Associated timezone (hours offset from GMT, e.g. EST = -5)
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
!  PRINT *,'DEBUG:  BEGIN MYESMF_TimeSet()'
!$$$ push this down into MYESMF_BaseTime constructor
      time%basetime%S  = 0
      time%basetime%Sn = 0
      time%basetime%Sd = 0

      IF ( PRESENT( rc ) ) rc = MYESMF_FAILURE
      time%YR = 0
      IF ( PRESENT( YY ) ) THEN
!  PRINT *,'DEBUG:  MYESMF_TimeSet():  YY = ',YY
        time%YR = YY
      ENDIF
      IF ( PRESENT( MM ) ) THEN
!  PRINT *,'DEBUG:  MYESMF_TimeSet():  MM = ',MM
        CALL timeaddmonths( time, MM, ierr )
        IF ( ierr == MYESMF_FAILURE ) THEN
          IF ( PRESENT( rc ) ) THEN
            rc = MYESMF_FAILURE
            RETURN
          ENDIF
        ENDIF
!  PRINT *,'DEBUG:  MYESMF_TimeSet():  back from timeaddmonths'
      ENDIF
      IF ( PRESENT( DD ) ) THEN
!$$$ no check for DD in range of days of month MM yet
!$$$ Must separate D and DD for correct interface!
!  PRINT *,'DEBUG:  MYESMF_TimeSet():  DD = ',DD
        time%basetime%S = time%basetime%S + &
          ( SECONDS_PER_DAY * INT( (DD-1), MYESMF_KIND_I8 ) )
      ENDIF
!$$$ push H,M,S,Sn,Sd,MS down into MYESMF_BaseTime constructor
      IF ( PRESENT( H ) ) THEN
!  PRINT *,'DEBUG:  MYESMF_TimeSet():  H = ',H
        time%basetime%S = time%basetime%S + &
          ( SECONDS_PER_HOUR * INT( H, MYESMF_KIND_I8 ) )
      ENDIF
      IF ( PRESENT( M ) ) THEN
!  PRINT *,'DEBUG:  MYESMF_TimeSet():  M = ',M
        time%basetime%S = time%basetime%S + &
          ( SECONDS_PER_MINUTE * INT( M, MYESMF_KIND_I8 ) )
      ENDIF
      IF ( PRESENT( S ) ) THEN
!  PRINT *,'DEBUG:  MYESMF_TimeSet():  S = ',S
        time%basetime%S = time%basetime%S + &
          INT( S, MYESMF_KIND_I8 )
      ENDIF
      IF ( PRESENT( Sn ) .AND. ( .NOT. PRESENT( Sd ) ) ) THEN
        CALL wrf_error_fatal( &
          "MYESMF_TimeSet:  Must specify Sd if Sn is specified")
      ENDIF
      IF ( PRESENT( Sd ) .AND. PRESENT( MS ) ) THEN
        CALL wrf_error_fatal( &
          "MYESMF_TimeSet:  Must not specify both Sd and MS")
      ENDIF
      time%basetime%Sn = 0
      time%basetime%Sd = 0
      IF ( PRESENT( MS ) ) THEN
!  PRINT *,'DEBUG:  MYESMF_TimeSet():  MS = ',MS
        time%basetime%Sn = MS
        time%basetime%Sd = 1000_MYESMF_KIND_I8
      ELSE IF ( PRESENT( Sd ) ) THEN
!  PRINT *,'DEBUG:  MYESMF_TimeSet():  Sd = ',Sd
        time%basetime%Sd = Sd
        IF ( PRESENT( Sn ) ) THEN
!  PRINT *,'DEBUG:  MYESMF_TimeSet():  Sn = ',Sn
          time%basetime%Sn = Sn
        ENDIF
      ENDIF
      IF ( PRESENT(calendar) )THEN
!  PRINT *,'DEBUG:  MYESMF_TimeSet():  using passed-in calendar'
! Note that the ugly hack of wrapping the call to MYESMF_CalendarInitialized() 
! inside this #ifdef is due to lack of support for compile-time initialization 
! of components of Fortran derived types.  Some older compilers like PGI 5.1-x 
! do not support this F95 feature.  In this case we only lose a safety check.  
#ifndef NO_DT_COMPONENT_INIT
        IF ( .not. MYESMF_CalendarInitialized( calendar ) )THEN
           call wrf_error_fatal( "Error:: MYESMF_CalendarCreate not "// &
                                 "called on input Calendar")
        END IF
#endif
        time%Calendar => calendar
      ELSE
!  PRINT *,'DEBUG:  MYESMF_TimeSet():  using default calendar'
        IF ( .not. MYESMF_IsInitialized() )THEN
           call wrf_error_fatal( "Error:: MYESMF_Initialize not called")
        END IF
        time%Calendar => defaultCal
      END IF

!  PRINT *,'DEBUG:  MYESMF_TimeSet():  calling normalize_time()'
!$$$DEBUG
!IF ( time%basetime%Sd > 0 ) THEN
!  PRINT *,'DEBUG MYESMF_TimeSet() before normalize:  S,Sn,Sd = ', &
!    time%basetime%S, time%basetime%Sn, time%basetime%Sd
!ENDIF
!$$$END DEBUG
      CALL normalize_time( time )
!$$$DEBUG
!IF ( time%basetime%Sd > 0 ) THEN
!  PRINT *,'DEBUG MYESMF_TimeSet() after normalize:  S,Sn,Sd = ', &
!    time%basetime%S, time%basetime%Sn, time%basetime%Sd
!ENDIF
!$$$END DEBUG

!  PRINT *,'DEBUG:  MYESMF_TimeSet():  back from normalize_time()'
      IF ( PRESENT( rc ) ) THEN
        rc = MYESMF_SUCCESS
      ENDIF

      end subroutine MYESMF_TimeSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMFold_TimeGetString - Get time instant value in string format

! !INTERFACE:
      subroutine MYESMFold_TimeGetString( year, month, dayofmonth, &
                                        hour, minute, second, TimeString )

! !ARGUMENTS:
      integer, intent(in) :: year
      integer, intent(in) :: month
      integer, intent(in) :: dayofmonth
      integer, intent(in) :: hour
      integer, intent(in) :: minute
      integer, intent(in) :: second
      character*(*), intent(out) :: TimeString
! !DESCRIPTION:
!     Convert {\tt MYESMF\_Time}'s value into ISO 8601 format YYYY-MM-DDThh:mm:ss
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to convert
!     \item[TimeString]
!          The string to return
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.4.7
!EOP

!PRINT *,'DEBUG:  MYESMF_TimePrint():  YR,S,Sn,Sd = ',time%YR,time%basetime%S,time%basetime%Sn,time%basetime%Sd
!PRINT *,'DEBUG:  MYESMF_TimePrint():  year = ',year
!PRINT *,'DEBUG:  MYESMF_TimePrint():  month, dayofmonth = ',month,dayofmonth
!PRINT *,'DEBUG:  MYESMF_TimePrint():  hour = ',hour
!PRINT *,'DEBUG:  MYESMF_TimePrint():  minute = ',minute
!PRINT *,'DEBUG:  MYESMF_TimePrint():  second = ',second

!$$$here...  add negative sign for YR<0
!$$$here...  add Sn, Sd ??
#ifdef PLANET
      write(TimeString,FMT="(I4.4,'-',I5.5,'_',I2.2,':',I2.2,':',I2.2)") &
             year,dayofmonth,hour,minute,second
#else
      write(TimeString,FMT="(I4.4,'-',I2.2,'-',I2.2,'_',I2.2,':',I2.2,':',I2.2)") &
             year,month,dayofmonth,hour,minute,second
#endif

      end subroutine MYESMFold_TimeGetString

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_TimeGetDayOfYearInteger - Get time instant's day of the year as an integer value
!
! !INTERFACE:
      subroutine MYESMF_TimeGetDayOfYearInteger(time, DayOfYear, rc)
!
! !ARGUMENTS:
      type(MYESMF_Time), intent(in) :: time
      integer, intent(out) :: DayOfYear
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the day of the year the given {\tt MYESMF\_Time} instant falls on
!     (1-365).  Returned as an integer value
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[DayOfYear]
!          The {\tt MYESMF\_Time} instant's day of the year (1-365)
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      ! requires that time be normalized
!$$$ bug when Sn>0?  test
!$$$ add tests
      DayOfYear = ( time%basetime%S / SECONDS_PER_DAY ) + 1
      IF ( PRESENT( rc ) ) rc = MYESMF_SUCCESS
      end subroutine MYESMF_TimeGetDayOfYearInteger

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_TimeInc - Increment time instant with a time interval
!
! !INTERFACE:
      function MYESMF_TimeInc(time, timeinterval)
!
! !RETURN VALUE:
      type(MYESMF_Time) :: MYESMF_TimeInc
!
! !ARGUMENTS:
      type(MYESMF_Time), intent(in) :: time
      type(MYESMF_TimeInterval), intent(in) :: timeinterval
! !LOCAL:
      integer   :: rc
!
! !DESCRIPTION:
!     Increment {\tt MYESMF\_Time} instant with a {\tt MYESMF\_TimeInterval},
!     return resulting {\tt MYESMF\_Time} instant
!
!     Maps overloaded (+) operator interface function to
!     {\tt MYESMF\_BaseTime} base class
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The given {\tt MYESMF\_Time} to increment
!     \item[timeinterval]
!          The {\tt MYESMF\_TimeInterval} to add to the given {\tt MYESMF\_Time}
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP

      ! copy MYESMF_Time specific properties (e.g. calendar, timezone) 
      MYESMF_TimeInc = time

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeSum(time, timeinterval, MYESMF_TimeInc)

      end function MYESMF_TimeInc
!
! this is added for certain compilers that don't deal with commutativity
!
      function MYESMF_TimeInc2(timeinterval, time)
      type(MYESMF_Time) :: MYESMF_TimeInc2
      type(MYESMF_Time), intent(in) :: time
      type(MYESMF_TimeInterval), intent(in) :: timeinterval
      MYESMF_TimeInc2 = MYESMF_TimeInc( time, timeinterval )
      end function MYESMF_TimeInc2
!

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_TimeDec - Decrement time instant with a time interval
!
! !INTERFACE:
      function MYESMF_TimeDec(time, timeinterval)
!
! !RETURN VALUE:
      type(MYESMF_Time) :: MYESMF_TimeDec
!
! !ARGUMENTS:
      type(MYESMF_Time), intent(in) :: time
      type(MYESMF_TimeInterval), intent(in) :: timeinterval
! !LOCAL:
      integer   :: rc
!
! !DESCRIPTION:
!     Decrement {\tt MYESMF\_Time} instant with a {\tt MYESMF\_TimeInterval},
!     return resulting {\tt MYESMF\_Time} instant
!
!     Maps overloaded (-) operator interface function to
!     {\tt MYESMF\_BaseTime} base class
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The given {\tt MYESMF\_Time} to decrement
!     \item[timeinterval]
!          The {\tt MYESMF\_TimeInterval} to subtract from the given
!          {\tt MYESMF\_Time}
!     \end{description}
!     
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP

      ! copy MYESMF_Time specific properties (e.g. calendar, timezone) 
      MYESMF_TimeDec = time

      ! call ESMC_BaseTime base class function
       call c_ESMC_BaseTimeDec(time, timeinterval, MYESMF_TimeDec)

      end function MYESMF_TimeDec

!
! this is added for certain compilers that don't deal with commutativity
!
      function MYESMF_TimeDec2(timeinterval, time)
      type(MYESMF_Time) :: MYESMF_TimeDec2
      type(MYESMF_Time), intent(in) :: time
      type(MYESMF_TimeInterval), intent(in) :: timeinterval
      MYESMF_TimeDec2 = MYESMF_TimeDec( time, timeinterval )
      end function MYESMF_TimeDec2
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_TimeDiff - Return the difference between two time instants
!
! !INTERFACE:
      function MYESMF_TimeDiff(time1, time2)
!
! !RETURN VALUE:
      type(MYESMF_TimeInterval) :: MYESMF_TimeDiff
!
! !ARGUMENTS:
      type(MYESMF_Time), intent(in) :: time1
      type(MYESMF_Time), intent(in) :: time2
! !LOCAL:
      integer :: rc

! !DESCRIPTION:
!     Return the {\tt MYESMF\_TimeInterval} difference between two
!     {\tt MYESMF\_Time} instants
!
!     Maps overloaded (-) operator interface function to
!     {\tt MYESMF\_BaseTime} base class
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          The first {\tt MYESMF\_Time} instant
!     \item[time2]
!          The second {\tt MYESMF\_Time} instant
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      CALL MYESMF_TimeIntervalSet( MYESMF_TimeDiff, rc=rc )
      call c_ESMC_BaseTimeDiff(time1, time2, MYESMF_TimeDiff)

      end function MYESMF_TimeDiff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_TimeEQ - Compare two times for equality
!
! !INTERFACE:
      function MYESMF_TimeEQ(time1, time2)
!
! !RETURN VALUE:
      logical :: MYESMF_TimeEQ
!
! !ARGUMENTS:
      type(MYESMF_Time), intent(in) :: time1
      type(MYESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if both given {\tt MYESMF\_Time} instants are equal, false
!     otherwise.  Maps overloaded (==) operator interface function to
!     {\tt MYESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! invoke C to C++ entry point for MYESMF_BaseTime base class function
      call c_ESMC_BaseTimeEQ(time1, time2, MYESMF_TimeEQ)

      end function MYESMF_TimeEQ

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_TimeNE - Compare two times for non-equality
!
! !INTERFACE:
      function MYESMF_TimeNE(time1, time2)
!
! !RETURN VALUE:
      logical :: MYESMF_TimeNE
!
! !ARGUMENTS:
      type(MYESMF_Time), intent(in) :: time1
      type(MYESMF_Time), intent(in) :: time2

! !DESCRIPTION:
!     Return true if both given {\tt MYESMF\_Time} instants are not equal, false
!     otherwise.  Maps overloaded (/=) operator interface function to
!     {\tt MYESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeNE(time1, time2, MYESMF_TimeNE)

      end function MYESMF_TimeNE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_TimeLT - Time instant 1 less than time instant 2 ?
!
! !INTERFACE:
      function MYESMF_TimeLT(time1, time2)
!
! !RETURN VALUE:
      logical :: MYESMF_TimeLT
!
! !ARGUMENTS:
      type(MYESMF_Time), intent(in) :: time1
      type(MYESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if first {\tt MYESMF\_Time} instant is less than second
!     {\tt MYESMF\_Time} instant, false otherwise.  Maps overloaded (<)
!     operator interface function to {\tt MYESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLT(time1, time2, MYESMF_TimeLT)

      end function MYESMF_TimeLT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_TimeGT - Time instant 1 greater than time instant 2 ?
!
! !INTERFACE:
      function MYESMF_TimeGT(time1, time2)
!
! !RETURN VALUE:
      logical :: MYESMF_TimeGT
!
! !ARGUMENTS:
      type(MYESMF_Time), intent(in) :: time1
      type(MYESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if first {\tt MYESMF\_Time} instant is greater than second
!     {\tt MYESMF\_Time} instant, false otherwise.  Maps overloaded (>) operator
!     interface function to {\tt MYESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGT(time1, time2, MYESMF_TimeGT)

      end function MYESMF_TimeGT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_TimeLE - Time instant 1 less than or equal to time instant 2 ?
!
! !INTERFACE:
      function MYESMF_TimeLE(time1, time2)
!
! !RETURN VALUE:
      logical :: MYESMF_TimeLE
!
! !ARGUMENTS:
      type(MYESMF_Time), intent(in) :: time1
      type(MYESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if first {\tt MYESMF\_Time} instant is less than or equal to
!     second {\tt MYESMF\_Time} instant, false otherwise.  Maps overloaded (<=)
!     operator interface function to {\tt MYESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLE(time1, time2, MYESMF_TimeLE)

      end function MYESMF_TimeLE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: MYESMF_TimeGE - Time instant 1 greater than or equal to time instant 2 ?
!
! !INTERFACE:
      function MYESMF_TimeGE(time1, time2)
!
! !RETURN VALUE:
      logical :: MYESMF_TimeGE
!
! !ARGUMENTS:
      type(MYESMF_Time), intent(in) :: time1
      type(MYESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if first {\tt MYESMF\_Time} instant is greater than or equal to
!     second {\tt MYESMF\_Time} instant, false otherwise.  Maps overloaded (>=)
!     operator interface function to {\tt MYESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGE(time1, time2, MYESMF_TimeGE)

      end function MYESMF_TimeGE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_TimeCopy - Copy a time-instance

! !INTERFACE:
      subroutine MYESMF_TimeCopy(timeout, timein)

! !ARGUMENTS:
      type(MYESMF_Time), intent(out) :: timeout
      type(MYESMF_Time), intent(in) :: timein

! !DESCRIPTION:
!     Copy a time-instance to a new instance.
!
!     \item[{[rc]}]
!          Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      timeout%basetime = timein%basetime
      timeout%YR       = timein%YR
      timeout%Calendar => timein%Calendar

      end subroutine MYESMF_TimeCopy

      end module MYESMF_TimeMod
