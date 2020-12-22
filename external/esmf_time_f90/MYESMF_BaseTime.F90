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
!     MYESMF BaseTime Module
      module MYESMF_BaseTimeMod
!
!==============================================================================
!
! This file contains the BaseTime class definition and all BaseTime class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES

#include <MYESMF_TimeMgr.inc>
!
!===============================================================================
!BOPI
! !MODULE: MYESMF_BaseTimeMod - Base MYESMF time definition 
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! This module serves only as the common Time definition inherited
! by {\tt MYESMF\_TimeInterval} and {\tt MYESMF\_Time}
!
! See {\tt ../include/ESMC\_BaseTime.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      use MYESMF_BaseMod    ! MYESMF Base class
      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! MYESMF_BaseTime
!
!     ! Base class type to match C++ BaseTime class in size only;
!     !  all dereferencing within class is performed by C++ implementation

      type MYESMF_BaseTime
        integer(MYESMF_KIND_I8) :: S   ! whole seconds
        integer(MYESMF_KIND_I8) :: Sn  ! fractional seconds, numerator
        integer(MYESMF_KIND_I8) :: Sd  ! fractional seconds, denominator
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public MYESMF_BaseTime
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! overloaded operators
      public operator(+)
      private MYESMF_BaseTimeSum
      public operator(-)
      private MYESMF_BaseTimeDifference
      public operator(/)
      private MYESMF_BaseTimeQuotI
      private MYESMF_BaseTimeQuotI8
      public operator(.EQ.)
      private MYESMF_BaseTimeEQ
      public operator(.NE.)
      private MYESMF_BaseTimeNE
      public operator(.LT.)
      private MYESMF_BaseTimeLT
      public operator(.GT.)
      private MYESMF_BaseTimeGT
      public operator(.LE.)
      private MYESMF_BaseTimeLE
      public operator(.GE.)
      private MYESMF_BaseTimeGE

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
      interface operator(+)
        module procedure MYESMF_BaseTimeSum
      end interface
      interface operator(-)
        module procedure MYESMF_BaseTimeDifference
      end interface
      interface operator(/)
        module procedure MYESMF_BaseTimeQuotI,MYESMF_BaseTimeQuotI8
      end interface
      interface operator(.EQ.)
        module procedure MYESMF_BaseTimeEQ
      end interface
      interface operator(.NE.)
        module procedure MYESMF_BaseTimeNE
      end interface
      interface operator(.LT.)
        module procedure MYESMF_BaseTimeLT
      end interface
      interface operator(.GT.)
        module procedure MYESMF_BaseTimeGT
      end interface
      interface operator(.LE.)
        module procedure MYESMF_BaseTimeLE
      end interface
      interface operator(.GE.)
        module procedure MYESMF_BaseTimeGE
      end interface


!==============================================================================

      contains

!==============================================================================


! Add two basetimes
      FUNCTION MYESMF_BaseTimeSum( basetime1, basetime2 )
        TYPE(MYESMF_BaseTime) :: MYESMF_BaseTimeSum
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime2
        ! locals
        INTEGER (MYESMF_KIND_I8) :: Sn1, Sd1, Sn2, Sd2, lcd
!  PRINT *,'DEBUG:  BEGIN MYESMF_BaseTimeSum()'
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  basetime1%S = ',basetime1%S
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  basetime1%Sn = ',basetime1%Sn
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  basetime1%Sd = ',basetime1%Sd
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  basetime2%S = ',basetime2%S
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  basetime2%Sn = ',basetime2%Sn
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  basetime2%Sd = ',basetime2%Sd
        MYESMF_BaseTimeSum   = basetime1
        MYESMF_BaseTimeSum%S = MYESMF_BaseTimeSum%S + basetime2%S
        Sn1 = basetime1%Sn
        Sd1 = basetime1%Sd
        Sn2 = basetime2%Sn
        Sd2 = basetime2%Sd
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  Sn1 = ',Sn1
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  Sd1 = ',Sd1
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  Sn2 = ',Sn2
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  Sd2 = ',Sd2
        IF      ( ( Sd1 .EQ. 0 ) .AND. ( Sd2 .EQ. 0 ) ) THEN
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  no fractions'
          MYESMF_BaseTimeSum%Sn = 0
          MYESMF_BaseTimeSum%Sd = 0
        ELSE IF ( ( Sd1 .NE. 0 ) .AND. ( Sd2 .EQ. 0 ) ) THEN
          MYESMF_BaseTimeSum%Sn = Sn1
          MYESMF_BaseTimeSum%Sd = Sd1
        ELSE IF ( ( Sd1 .EQ. 0 ) .AND. ( Sd2 .NE. 0 ) ) THEN
          MYESMF_BaseTimeSum%Sn = Sn2
          MYESMF_BaseTimeSum%Sd = Sd2
        ELSE IF ( ( Sd1 .NE. 0 ) .AND. ( Sd2 .NE. 0 ) ) THEN
          CALL compute_lcd( Sd1 , Sd2 , lcd )
          MYESMF_BaseTimeSum%Sd = lcd
          MYESMF_BaseTimeSum%Sn = (Sn1 * lcd / Sd1) + (Sn2 * lcd / Sd2)
        ENDIF
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  MYESMF_BaseTimeSum%S = ',MYESMF_BaseTimeSum%S
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  MYESMF_BaseTimeSum%Sn = ',MYESMF_BaseTimeSum%Sn
!  PRINT *,'DEBUG:  MYESMF_BaseTimeSum():  MYESMF_BaseTimeSum%Sd = ',MYESMF_BaseTimeSum%Sd
        CALL normalize_basetime( MYESMF_BaseTimeSum )
!  PRINT *,'DEBUG:  END MYESMF_BaseTimeSum()'
      END FUNCTION MYESMF_BaseTimeSum


! Subtract two basetimes
      FUNCTION MYESMF_BaseTimeDifference( basetime1, basetime2 )
        TYPE(MYESMF_BaseTime) :: MYESMF_BaseTimeDifference
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime2
        ! locals
        TYPE(MYESMF_BaseTime) :: neg2

        neg2%S  = -basetime2%S
        neg2%Sn = -basetime2%Sn
        neg2%Sd =  basetime2%Sd

        MYESMF_BaseTimeDifference = basetime1 + neg2

      END FUNCTION MYESMF_BaseTimeDifference


! Divide basetime by 8-byte integer
      FUNCTION MYESMF_BaseTimeQuotI8( basetime, divisor )
        TYPE(MYESMF_BaseTime) :: MYESMF_BaseTimeQuotI8
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime
        INTEGER(MYESMF_KIND_I8), INTENT(IN) :: divisor
        ! locals
        INTEGER(MYESMF_KIND_I8) :: d, n, dinit

!PRINT *,'DEBUG MYESMF_BaseTimeQuotI8() A:  S,Sn,Sd = ', &
!  basetime%S,basetime%Sn,basetime%Sd
!PRINT *,'DEBUG MYESMF_BaseTimeQuotI8() A:  divisor = ', divisor
        IF ( divisor == 0_MYESMF_KIND_I8 ) THEN
          CALL wrf_error_fatal( 'MYESMF_BaseTimeQuotI8:  divide by zero' )
        ENDIF

!$$$ move to default constructor
        MYESMF_BaseTimeQuotI8%S  = 0
        MYESMF_BaseTimeQuotI8%Sn = 0
        MYESMF_BaseTimeQuotI8%Sd = 0

        ! convert to a fraction and divide by multipling the denonminator by 
        ! the divisor
        IF ( basetime%Sd == 0 ) THEN
          dinit = 1_MYESMF_KIND_I8
        ELSE
          dinit = basetime%Sd
        ENDIF
        n = basetime%S * dinit + basetime%Sn
        d = dinit * divisor
!PRINT *,'DEBUG MYESMF_BaseTimeQuotI8() B:  n,d = ',n,d
        CALL simplify( n, d, MYESMF_BaseTimeQuotI8%Sn, MYESMF_BaseTimeQuotI8%Sd )
!PRINT *,'DEBUG MYESMF_BaseTimeQuotI8() C:  S,Sn,Sd = ', &
!  MYESMF_BaseTimeQuotI8%S,MYESMF_BaseTimeQuotI8%Sn,MYESMF_BaseTimeQuotI8%Sd
        CALL normalize_basetime( MYESMF_BaseTimeQuotI8 )
!PRINT *,'DEBUG MYESMF_BaseTimeQuotI8() D:  S,Sn,Sd = ', &
!  MYESMF_BaseTimeQuotI8%S,MYESMF_BaseTimeQuotI8%Sn,MYESMF_BaseTimeQuotI8%Sd
      END FUNCTION MYESMF_BaseTimeQuotI8

! Divide basetime by integer
      FUNCTION MYESMF_BaseTimeQuotI( basetime, divisor )
        TYPE(MYESMF_BaseTime) :: MYESMF_BaseTimeQuotI
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime
        INTEGER, INTENT(IN) :: divisor
        IF ( divisor == 0 ) THEN
          CALL wrf_error_fatal( 'MYESMF_BaseTimeQuotI:  divide by zero' )
        ENDIF
        MYESMF_BaseTimeQuotI = basetime / INT( divisor, MYESMF_KIND_I8 )
      END FUNCTION MYESMF_BaseTimeQuotI


! .EQ. for two basetimes
      FUNCTION MYESMF_BaseTimeEQ( basetime1, basetime2 )
        LOGICAL :: MYESMF_BaseTimeEQ
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        MYESMF_BaseTimeEQ = ( retval .EQ. 0 )
      END FUNCTION MYESMF_BaseTimeEQ


! .NE. for two basetimes
      FUNCTION MYESMF_BaseTimeNE( basetime1, basetime2 )
        LOGICAL :: MYESMF_BaseTimeNE
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        MYESMF_BaseTimeNE = ( retval .NE. 0 )
      END FUNCTION MYESMF_BaseTimeNE


! .LT. for two basetimes
      FUNCTION MYESMF_BaseTimeLT( basetime1, basetime2 )
        LOGICAL :: MYESMF_BaseTimeLT
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        MYESMF_BaseTimeLT = ( retval .LT. 0 )
      END FUNCTION MYESMF_BaseTimeLT


! .GT. for two basetimes
      FUNCTION MYESMF_BaseTimeGT( basetime1, basetime2 )
        LOGICAL :: MYESMF_BaseTimeGT
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        MYESMF_BaseTimeGT = ( retval .GT. 0 )
      END FUNCTION MYESMF_BaseTimeGT


! .LE. for two basetimes
      FUNCTION MYESMF_BaseTimeLE( basetime1, basetime2 )
        LOGICAL :: MYESMF_BaseTimeLE
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        MYESMF_BaseTimeLE = ( retval .LE. 0 )
      END FUNCTION MYESMF_BaseTimeLE


! .GE. for two basetimes
      FUNCTION MYESMF_BaseTimeGE( basetime1, basetime2 )
        LOGICAL :: MYESMF_BaseTimeGE
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(MYESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        MYESMF_BaseTimeGE = ( retval .GE. 0 )
      END FUNCTION MYESMF_BaseTimeGE


      end module MYESMF_BaseTimeMod
