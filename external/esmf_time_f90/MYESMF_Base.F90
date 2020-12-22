!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA license.
!
! MYESMF Base Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! module definition

      module MYESMF_BaseMod
 
!BOP
! !MODULE: MYESMF_BaseMod - Base class for all MYESMF classes
!
! !DESCRIPTION:
!
! The code in this file implements the Base defined type
!  and functions which operate on all types.  This is an
!  interface to the actual C++ base class implementation in the ../src dir.
!
! See the MYESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
!    Global integer parameters, used frequently

      integer, parameter :: MYESMF_SUCCESS = 0, MYESMF_FAILURE = -1
      integer, parameter :: MYESMF_MAXSTR = 128
      integer, parameter :: MYESMF_MAXDIM = 7, &
                            MYESMF_MAXDECOMPDIM=3, &
                            MYESMF_MAXGRIDDIM=2
     
      integer, parameter :: MYESMF_MAJOR_VERSION = 2
      integer, parameter :: MYESMF_MINOR_VERSION = 1
      integer, parameter :: MYESMF_REVISION      = 1
      integer, parameter :: MYESMF_PATCHLEVEL    = 0
      character(32), parameter :: MYESMF_VERSION_STRING = "2.1.1"

!------------------------------------------------------------------------------
!
      type MYESMF_Status
      private
          integer :: status
      end type

      type(MYESMF_Status), parameter :: MYESMF_STATE_UNINIT = MYESMF_Status(1), &
                                      MYESMF_STATE_READY = MYESMF_Status(2), &
                                      MYESMF_STATE_UNALLOCATED = MYESMF_Status(3), &
                                      MYESMF_STATE_ALLOCATED = MYESMF_Status(4), &
                                      MYESMF_STATE_BUSY = MYESMF_Status(5), &
                                      MYESMF_STATE_INVALID = MYESMF_Status(6)
 
!------------------------------------------------------------------------------
!
      type MYESMF_Pointer
      private
          integer*8 :: ptr
      end type

      type(MYESMF_Pointer), parameter :: MYESMF_NULL_POINTER = MYESMF_Pointer(0), &
                                       MYESMF_BAD_POINTER = MYESMF_Pointer(-1)


!------------------------------------------------------------------------------
!
      !! TODO: I believe if we define an assignment(=) operator to convert
      !!   a datatype into integer, then we could use the type and kind as
      !!   targets in a select case() statement and make the contents private.
      !!   (see pg 248 of the "big book")
      type MYESMF_DataType
      !!private
          integer :: dtype
      end type

      type(MYESMF_DataType), parameter :: MYESMF_DATA_INTEGER = MYESMF_DataType(1), &
                                        MYESMF_DATA_REAL = MYESMF_DataType(2), &
                                        MYESMF_DATA_LOGICAL = MYESMF_DataType(3), &
                                        MYESMF_DATA_CHARACTER = MYESMF_DataType(4)

!------------------------------------------------------------------------------

      integer, parameter :: &
                   MYESMF_KIND_I1 = selected_int_kind(2), &
                   MYESMF_KIND_I2 = selected_int_kind(4), &
                   MYESMF_KIND_I4 = selected_int_kind(9), &
                   MYESMF_KIND_I8 = selected_int_kind(14), &
                   MYESMF_KIND_R4 = selected_real_kind(3,25), &
                   MYESMF_KIND_R8 = selected_real_kind(6,45), &
                   MYESMF_KIND_C8 = selected_real_kind(3,25), &
                   MYESMF_KIND_C16 = selected_real_kind(6,45)

!------------------------------------------------------------------------------

      type MYESMF_DataValue
      private
          type(MYESMF_DataType) :: dt
          integer :: rank
          ! how do you do values of all types here ? TODO
          ! in C++ i'd do a union w/ overloaded access funcs
          integer :: vi
          !integer, dimension (:), pointer :: vip
          !real :: vr
          !real, dimension (:), pointer :: vrp
          !logical :: vl
          !logical, pointer :: vlp
          !character (len=MYESMF_MAXSTR) :: vc
          !character, pointer :: vcp
      end type

!------------------------------------------------------------------------------
!
      type MYESMF_Attribute
      private
          character (len=MYESMF_MAXSTR) :: attr_name
          type (MYESMF_DataType) :: attr_type
          type (MYESMF_DataValue) :: attr_value
      end type

!------------------------------------------------------------------------------
!
      !! TODO: this should be a shallow object, with a simple init() and
      !!  get() function, and the contents should go back to being private.
      type MYESMF_AxisIndex
!     !!private
          integer :: l
          integer :: r
          integer :: max
          integer :: decomp
          integer :: gstart
      end type

      !! TODO: same comment as above.
      type MYESMF_MemIndex
!     !!private
          integer :: l
          integer :: r
          integer :: str
          integer :: num
      end type

!------------------------------------------------------------------------------
!
      type MYESMF_BasePointer
      private
          integer*8 :: base_ptr
      end type

      integer :: global_count = 0

!------------------------------------------------------------------------------
!
!     ! WARNING: must match corresponding values in ../include/ESMC_Base.h
      type MYESMF_Logical
      private
          integer :: value
      end type

      type(MYESMF_Logical), parameter :: MYESMF_TF_UNKNOWN  = MYESMF_Logical(1), &
                                       MYESMF_TF_TRUE     = MYESMF_Logical(2), &
                                       MYESMF_TF_FALSE    = MYESMF_Logical(3)

!------------------------------------------------------------------------------
!
      type MYESMF_Base
      private
         integer :: ID
         integer :: ref_count
         type (MYESMF_Status) :: base_status
         character (len=MYESMF_MAXSTR) :: name
     end type

! !PUBLIC TYPES:

      public MYESMF_STATE_INVALID
!      public MYESMF_STATE_UNINIT, MYESMF_STATE_READY, &
!             MYESMF_STATE_UNALLOCATED, MYESMF_STATE_ALLOCATED, &
!             MYESMF_STATE_BUSY

      public MYESMF_DATA_INTEGER, MYESMF_DATA_REAL, &
             MYESMF_DATA_LOGICAL, MYESMF_DATA_CHARACTER

      public MYESMF_KIND_I1, MYESMF_KIND_I2, MYESMF_KIND_I4, MYESMF_KIND_I8, & 
             MYESMF_KIND_R4, MYESMF_KIND_R8, MYESMF_KIND_C8, MYESMF_KIND_C16

      public MYESMF_NULL_POINTER, MYESMF_BAD_POINTER


      public MYESMF_FAILURE, MYESMF_SUCCESS
      public MYESMF_MAXSTR
      public MYESMF_MAXDIM, MYESMF_MAXDECOMPDIM, MYESMF_MAXGRIDDIM
     
      public MYESMF_MAJOR_VERSION, MYESMF_MINOR_VERSION, MYESMF_REVISION
      public MYESMF_VERSION_STRING 

      public MYESMF_Status, MYESMF_Pointer, MYESMF_DataType
      public MYESMF_DataValue, MYESMF_Attribute
!      public MYESMF_MemIndex
!      public MYESMF_BasePointer
      public MYESMF_Base

      public MYESMF_AxisIndex, MYESMF_AxisIndexGet
!      public MYESMF_AxisIndexInit
      public MYESMF_Logical
!      public MYESMF_TF_TRUE, MYESMF_TF_FALSE

! !PUBLIC MEMBER FUNCTIONS:
!
! !DESCRIPTION:
!     The following routines apply to any type in the system.  
!     The attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
!   Base class methods
!      public MYESMF_BaseInit
   
!      public MYESMF_BaseGetConfig
!      public MYESMF_BaseSetConfig

!      public MYESMF_BaseGetInstCount

!      public MYESMF_BaseSetID
!      public MYESMF_BaseGetID

!      public MYESMF_BaseSetRefCount
!      public MYESMF_BaseGetRefCount

!      public MYESMF_BaseSetStatus
!      public MYESMF_BaseGetStatus

!   Virtual methods to be defined by derived classes
!      public MYESMF_Read
!      public MYESMF_Write
!      public MYESMF_Validate
!      public MYESMF_Print

!  Attribute methods
      public MYESMF_AttributeSet
      public MYESMF_AttributeGet
      public MYESMF_AttributeGetCount
      public MYESMF_AttributeGetbyNumber
      public MYESMF_AttributeGetNameList
      public MYESMF_AttributeSetList
      public MYESMF_AttributeGetList
      public MYESMF_AttributeSetObjectList
      public MYESMF_AttributeGetObjectList
      public MYESMF_AttributeCopy
      public MYESMF_AttributeCopyAll
 
!  Misc methods
      public MYESMF_SetName
      public MYESMF_GetName
      public MYESMF_SetPointer
      public MYESMF_SetNullPointer
      public MYESMF_GetPointer

!  Print methods for calling by higher level print functions
!  (they have little formatting other than the actual values)
      public MYESMF_StatusString, MYESMF_DataTypeString

!  Overloaded = operator functions
      public operator(.eq.), operator(.ne.), assignment(=)
!
!
!EOP

!------------------------------------------------------------------------------

! overload .eq. & .ne. with additional derived types so you can compare 
!  them as if they were simple integers.
 

interface operator (.eq.)
 module procedure MYESMF_sfeq
 module procedure MYESMF_dteq
 module procedure MYESMF_pteq
 module procedure MYESMF_tfeq
 module procedure MYESMF_aieq
end interface

interface operator (.ne.)
 module procedure MYESMF_sfne
 module procedure MYESMF_dtne
 module procedure MYESMF_ptne
 module procedure MYESMF_tfne
 module procedure MYESMF_aine
end interface

interface assignment (=)
 module procedure MYESMF_dtas
 module procedure MYESMF_ptas
end interface

!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
! function to compare two MYESMF_Status flags to see if they're the same or not

function MYESMF_sfeq(sf1, sf2)
 logical MYESMF_sfeq
 type(MYESMF_Status), intent(in) :: sf1, sf2

 MYESMF_sfeq = (sf1%status .eq. sf2%status)
end function

function MYESMF_sfne(sf1, sf2)
 logical MYESMF_sfne
 type(MYESMF_Status), intent(in) :: sf1, sf2

 MYESMF_sfne = (sf1%status .ne. sf2%status)
end function

!------------------------------------------------------------------------------
! function to compare two MYESMF_DataTypes to see if they're the same or not

function MYESMF_dteq(dt1, dt2)
 logical MYESMF_dteq
 type(MYESMF_DataType), intent(in) :: dt1, dt2

 MYESMF_dteq = (dt1%dtype .eq. dt2%dtype)
end function

function MYESMF_dtne(dt1, dt2)
 logical MYESMF_dtne
 type(MYESMF_DataType), intent(in) :: dt1, dt2

 MYESMF_dtne = (dt1%dtype .ne. dt2%dtype)
end function

subroutine MYESMF_dtas(intval, dtval)
 integer, intent(out) :: intval
 type(MYESMF_DataType), intent(in) :: dtval

 intval = dtval%dtype
end subroutine


!------------------------------------------------------------------------------
! function to compare two MYESMF_Pointers to see if they're the same or not

function MYESMF_pteq(pt1, pt2)
 logical MYESMF_pteq
 type(MYESMF_Pointer), intent(in) :: pt1, pt2

 MYESMF_pteq = (pt1%ptr .eq. pt2%ptr)
end function

function MYESMF_ptne(pt1, pt2)
 logical MYESMF_ptne
 type(MYESMF_Pointer), intent(in) :: pt1, pt2

 MYESMF_ptne = (pt1%ptr .ne. pt2%ptr)
end function

subroutine MYESMF_ptas(ptval, intval)
 type(MYESMF_Pointer), intent(out) :: ptval
 integer, intent(in) :: intval

 ptval%ptr = intval
end subroutine

!------------------------------------------------------------------------------
! function to compare two MYESMF_Logicals to see if they're the same or not
! also need assignment to real f90 logical?

function MYESMF_tfeq(tf1, tf2)
 logical MYESMF_tfeq
 type(MYESMF_Logical), intent(in) :: tf1, tf2

 MYESMF_tfeq = (tf1%value .eq. tf2%value)
end function

function MYESMF_tfne(tf1, tf2)
 logical MYESMF_tfne
 type(MYESMF_Logical), intent(in) :: tf1, tf2

 MYESMF_tfne = (tf1%value .ne. tf2%value)
end function

!------------------------------------------------------------------------------
! function to compare two MYESMF_AxisIndex to see if they're the same or not

function MYESMF_aieq(ai1, ai2)
 logical MYESMF_aieq
 type(MYESMF_AxisIndex), intent(in) :: ai1, ai2

 MYESMF_aieq = ((ai1%l .eq. ai2%l) .and. &
              (ai1%r .eq. ai2%r) .and. &
              (ai1%max .eq. ai2%max) .and. &
              (ai1%decomp .eq. ai2%decomp) .and. &
              (ai1%gstart .eq. ai2%gstart))

end function

function MYESMF_aine(ai1, ai2)
 logical MYESMF_aine
 type(MYESMF_AxisIndex), intent(in) :: ai1, ai2

 MYESMF_aine = ((ai1%l .ne. ai2%l) .or. &
              (ai1%r .ne. ai2%r) .or. &
              (ai1%max .ne. ai2%max) .or. &
              (ai1%decomp .ne. ai2%decomp) .or. &
              (ai1%gstart .ne. ai2%gstart))

end function

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Base methods
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_BaseInit - initialize a Base object
!
! !INTERFACE:
      subroutine MYESMF_BaseInit(base, rc)
!
! !ARGUMENTS:
      type(MYESMF_Base) :: base                 
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Set initial state on a Base object.
!
!     \begin{description}
!     \item [base]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized 
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [{[rc]}]
!           Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP

      logical :: rcpresent                          ! Return code present   

!     !Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = MYESMF_FAILURE
      endif

      global_count = global_count + 1
      base%ID = global_count
      base%ref_count = 1
      base%base_status = MYESMF_STATE_READY
      base%name = "undefined"

      if (rcpresent) rc = MYESMF_SUCCESS

      end subroutine MYESMF_BaseInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_SetName - set the name of this object
!
! !INTERFACE:
      subroutine MYESMF_SetName(anytype, name, namespace, rc)
!
! !ARGUMENTS:
      type(MYESMF_Base) :: anytype                 
      character (len = *), intent(in), optional :: name   
      character (len = *), intent(in), optional :: namespace
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Associate a name with any object in the system.
!
!     \begin{description}
!     \item [anytype]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized 
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [[name]]
!           Object name.  An error will be returned if a duplicate name 
!           is specified.  If a name is not given a unique name will be
!           generated and can be queried by the {\tt MYESMF_GetName} routine.
!     \item [[namespace]]
!           Object namespace (e.g. "Application", "Component", "Grid", etc).
!           If given, the name will be checked that it is unique within
!           this namespace.  If not given, the generated name will be 
!           unique within this namespace.  If namespace is not specified,
!           a default "global" namespace will be assumed and the same rules
!           for names will be followed.
!     \item [[rc]]
!           Return code; equals {\tt MYESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
! 

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3
      logical :: rcpresent                          ! Return code present   
      character (len = MYESMF_MAXSTR) :: ournamespace ! Namespace if not given
      character (len = MYESMF_MAXSTR) :: defaultname  ! Name if not given
      integer, save :: seqnum = 0       ! HACK - generate uniq names
                                        ! but not coordinated across procs

!     !Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = MYESMF_FAILURE
      endif

!     ! TODO: this code should generate a unique name if a name
!     !   is not given.  If a namespace is given, the name has to
!     !   be unique within that namespace.  Example namespaces could
!     !   be: Applications, Components, Fields/Bundles, Grids.
!      
!     ! Construct a default namespace if one is not given
      if( present(namespace) ) then
          if( namespace .eq. "" ) then
              ournamespace = "global"
          else
              ournamespace = namespace
          endif
      else
              ournamespace = "global"
      endif

!     ! Construct a default name if one is not given
      if( present(name) ) then
         if( name .eq. "" ) then
            write(defaultname, 20) trim(ournamespace), seqnum
20          format(A,I3.3)
            seqnum = seqnum + 1
            anytype%name = defaultname
         else
            anytype%name = name
         endif
      else
         write(defaultname, 20) trim(ournamespace), seqnum
         seqnum = seqnum + 1
         anytype%name = defaultname
      endif

      if (rcpresent) rc = MYESMF_SUCCESS

      end subroutine MYESMF_SetName

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_GetName - get the name of this object
!
! !INTERFACE:
      subroutine MYESMF_GetName(anytype, name, rc)
!
! !ARGUMENTS:
      type(MYESMF_Base), intent(in) :: anytype             ! any MYESMF object/type
      character (len = *), intent(out) :: name           ! object/type name
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Return the name of any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      name = anytype%name
      if (present(rc)) rc = MYESMF_SUCCESS

      end subroutine MYESMF_GetName


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AttributeSet - set attribute on an MYESMF type
!
! !INTERFACE:
      subroutine MYESMF_AttributeSet(anytype, name, value, rc)
!
! !ARGUMENTS:
      type(MYESMF_Base), intent(in) :: anytype             ! any MYESMF type
      character (len = *), intent(in) :: name            ! attribute name
      type(MYESMF_DataValue), intent(in) :: value              ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Associate a (name,value) pair with any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      end subroutine MYESMF_AttributeSet


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  MYESMF_AttributeGet - get attribute from an MYESMF type
!
! !INTERFACE:
      subroutine MYESMF_AttributeGet(anytype, name, type, value, rc)
!
! !ARGUMENTS:
      type(MYESMF_Base), intent(in) :: anytype           ! any MYESMF type
      character (len = *), intent(in) :: name          ! attribute name
      type(MYESMF_DataType), intent(out) :: type             ! all possible data types
      type(MYESMF_DataValue), intent(out) :: value           ! attribute value
      integer, intent(out), optional :: rc             ! return code

!
! !DESCRIPTION:

!
!EOP
! !REQUIREMENTS:  FLD1.5.1, FLD1.5.3

      end subroutine MYESMF_AttributeGet


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  MYESMF_AttributeGetCount - get an MYESMF object's number of attributes
!
! !INTERFACE:
      subroutine MYESMF_AttributeGetCount(anytype, count, rc)
!
! !ARGUMENTS:
      type(MYESMF_Base), intent(in) :: anytype             ! any MYESMF type
      integer, intent(out) :: count                      ! attribute count
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Returns number of attributes present.

!
!EOP
! !REQUIREMENTS:  FLD1.7.5

      end subroutine MYESMF_AttributeGetCount


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  MYESMF_AttributeGetbyNumber - get an MYESMF object's attribute by num ber
!
! !INTERFACE:
      subroutine MYESMF_AttributeGetbyNumber(anytype, number, name, type, value, rc)
!
! !ARGUMENTS:
      type(MYESMF_Base), intent(in) :: anytype             ! any MYESMF type
      integer, intent(in) :: number                      ! attribute number
      character (len = *), intent(in) :: name            ! attribute name
      type(MYESMF_DataType), intent(out) :: type               ! all possible data types
      type(MYESMF_DataValue), intent(out) :: value             ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Allows the caller to get attributes by number instead of by name.
! This can be useful in iterating through all attributes in a loop.
!
!EOP
! !REQUIREMENTS: 

      end subroutine MYESMF_AttributeGetbyNumber


!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  MYESMF_AttributeGetNameList - get an MYESMF object's attribute name list
!
! !INTERFACE:
      subroutine MYESMF_AttributeGetNameList(anytype, count, namelist, rc)
!
! !ARGUMENTS:
      type(MYESMF_Base), intent(in) :: anytype             ! any MYESMF type
      integer, intent(out) :: count                      ! attribute count
      character (len = *), dimension (:), intent(out) :: namelist   ! attribute names
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Return a list of all attribute names without returning the values.

!
!EOP
! !REQUIREMENTS:  FLD1.7.3

      end subroutine MYESMF_AttributeGetNameList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  MYESMF_AttributeSetList - set an MYESMF object's attributes 
!
! !INTERFACE:
      subroutine MYESMF_AttributeSetList(anytype, namelist, valuelist, rc)

!
! !ARGUMENTS:
      type(MYESMF_Base), intent(in) :: anytype             ! any MYESMF type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(MYESMF_DataValue), dimension (:), intent(in) :: valuelist      ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set multiple attributes on an object in one call.  Depending on what is
! allowed by the interface, all attributes may have to have the same type.
!
!EOP
! !REQUIREMENTS:  (none.  added for completeness)

      end subroutine MYESMF_AttributeSetList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  MYESMF_AttributeGetList - get an MYESMF object's attributes
!
! !INTERFACE:
      subroutine MYESMF_AttributeGetList(anytype, namelist, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(MYESMF_Base), intent(in) :: anytype             ! any MYESMF type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(MYESMF_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(MYESMF_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get multiple attributes from an object in a single call.

!
!EOP
! !REQUIREMENTS:  FLD1.7.4

      end subroutine MYESMF_AttributeGetList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  MYESMF_AttributeSetObjectList - set an attribute on multiple MYESMF objects 
!
! !INTERFACE:
      subroutine MYESMF_AttributeSetObjectList(anytypelist, name, value, rc)
!
! !ARGUMENTS:
      type(MYESMF_Base), dimension (:), intent(in) :: anytypelist     ! list of any MYESMF types
      character (len = *), intent(in) :: name            ! attribute name
      type(MYESMF_DataValue), dimension (:), intent(in) :: value          ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set the same attribute on multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine MYESMF_AttributeSetObjectList


!-------------------------------------------------------------------------
!BOP
!
!
! !IROUTINE:  MYESMF_AttributeGetObjectList - get an attribute from multiple MYESMF objects 
!
! !INTERFACE:
      subroutine MYESMF_AttributeGetObjectList(anytypelist, name, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(MYESMF_Base), dimension (:), intent(in) :: anytypelist     ! list of any MYESMF types
      character (len = *), intent(in) :: name            ! attribute name
      type(MYESMF_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(MYESMF_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get the same attribute name from multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine MYESMF_AttributeGetObjectList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  MYESMF_AttributeCopy - copy an attribute between two objects
!
! !INTERFACE:
      subroutine MYESMF_AttributeCopy(name, source, destination, rc)
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name            ! attribute name
      type(MYESMF_Base), intent(in) :: source              ! any MYESMF type
      type(MYESMF_Base), intent(in) :: destination         ! any MYESMF type
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! The specified attribute associated with the source object is
! copied to the destination object.  << does this assume overwriting the
! attribute if it already exists in the output or does this require yet
! another arg to say what to do with collisions? >>


!
!EOP
! !REQUIREMENTS:  FLD1.5.4

      end subroutine MYESMF_AttributeCopy


!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_AttributeCopyAll - copy attributes between two objects

!
! !INTERFACE:
      subroutine MYESMF_AttributeCopyAll(source, destination, rc)
!
! !ARGUMENTS:
      type(MYESMF_Base), intent(in) :: source              ! any MYESMF type
      type(MYESMF_Base), intent(in) :: destination         ! any MYESMF type
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! All attributes associated with the source object are copied to the
! destination object.  Some attributes will have to be considered
! {\tt read only} and won't be updated by this call.  (e.g. an attribute
! like {\tt name} must be unique and therefore can't be duplicated.)

!
!EOP
! !REQUIREMENTS:  FLD1.5.4

      end subroutine MYESMF_AttributeCopyAll

!=========================================================================
! Misc utility routines, perhaps belongs in a utility file?
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_AxisIndexInit - initialize an AxisIndex object

!
! !INTERFACE:
      subroutine MYESMF_AxisIndexInit(ai, l, r, max, decomp, gstart, rc)
!
! !ARGUMENTS:
      type(MYESMF_AxisIndex), intent(inout) :: ai
      integer, intent(in) :: l, r, max, decomp, gstart
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Set the contents of an AxisIndex type.

!
!EOP
! !REQUIREMENTS:

      ai%l = l
      ai%r = r
      ai%max = max
      ai%decomp = decomp
      ai%gstart = gstart

      if (present(rc)) rc = MYESMF_SUCCESS

      end subroutine MYESMF_AxisIndexInit

!BOP
!
!IROUTINE:  ESMC_AxisIndexInit - initialize an AxisIndex object

!
! !INTERFACE:
      subroutine MYESMF_AxisIndexGet(ai, l, r, max, decomp, gstart, rc)
!
! !ARGUMENTS:
      type(MYESMF_AxisIndex), intent(inout) :: ai
      integer, intent(out), optional :: l, r, max, decomp, gstart
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Get the contents of an AxisIndex type.

!
!EOP
! !REQUIREMENTS:

      if (present(l)) l = ai%l
      if (present(r)) r = ai%r
      if (present(max)) max = ai%max
      if (present(decomp)) decomp = ai%decomp
      if (present(gstart)) gstart = ai%gstart

      if (present(rc)) rc = MYESMF_SUCCESS

      end subroutine MYESMF_AxisIndexGet

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  MYESMF_SetPointer - set an opaque value

!
! !INTERFACE:
      subroutine MYESMF_SetPointer(ptype, contents, rc)
!
! !ARGUMENTS:
      type(MYESMF_Pointer) :: ptype 
      integer*8, intent(in) :: contents
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      ptype%ptr = contents
      if (present(rc)) rc = MYESMF_SUCCESS

      end subroutine MYESMF_SetPointer

!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  MYESMF_SetNullPointer - set an opaque value

!
! !INTERFACE:
      subroutine MYESMF_SetNullPointer(ptype, rc)
!
! !ARGUMENTS:
      type(MYESMF_Pointer) :: ptype 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      integer*8, parameter :: nullp = 0

      ptype%ptr = nullp
      if (present(rc)) rc = MYESMF_SUCCESS

      end subroutine MYESMF_SetNullPointer
!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  MYESMF_GetPointer - get an opaque value 
!  
! !INTERFACE: 
      function MYESMF_GetPointer(ptype, rc) 
!
! !RETURN VALUE:
      integer*8 :: MYESMF_GetPointer

! !ARGUMENTS:
      type(MYESMF_Pointer), intent(in) :: ptype 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Get the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      MYESMF_GetPointer = ptype%ptr
      if (present(rc)) rc = MYESMF_SUCCESS

      end function MYESMF_GetPointer

!------------------------------------------------------------------------- 
! misc print routines
!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  MYESMF_StatusString - Return status as a string
!  
! !INTERFACE: 
      subroutine MYESMF_StatusString(status, string, rc)
!
! !ARGUMENTS:
      type(MYESMF_Status), intent(in) :: status
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a status variable as a string.

!
!EOP
! !REQUIREMENTS:

      if (status .eq. MYESMF_STATE_UNINIT) string = "Uninitialized"
      if (status .eq. MYESMF_STATE_READY) string = "Ready"
      if (status .eq. MYESMF_STATE_UNALLOCATED) string = "Unallocated"
      if (status .eq. MYESMF_STATE_ALLOCATED) string = "Allocated"
      if (status .eq. MYESMF_STATE_BUSY) string = "Busy"
      if (status .eq. MYESMF_STATE_INVALID) string = "Invalid"
 
      if (present(rc)) rc = MYESMF_SUCCESS

      end subroutine MYESMF_StatusString

!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  MYESMF_DataTypeString - Return DataType as a string
!  
! !INTERFACE: 
      subroutine MYESMF_DataTypeString(datatype, string, rc)
!
! !ARGUMENTS:
      type(MYESMF_DataType), intent(in) :: datatype
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a datatype variable as a string.

!
!EOP
! !REQUIREMENTS:

      if (datatype .eq. MYESMF_DATA_INTEGER) string = "Integer"
      if (datatype .eq. MYESMF_DATA_REAL) string = "Real"
      if (datatype .eq. MYESMF_DATA_LOGICAL) string = "Logical"
      if (datatype .eq. MYESMF_DATA_CHARACTER) string = "Character"
 
      if (present(rc)) rc = MYESMF_SUCCESS

      end subroutine MYESMF_DataTypeString

!-------------------------------------------------------------------------
!
!-------------------------------------------------------------------------
! put Print and Validate skeletons here - but they should be
!  overridden by higher level more specialized functions.
!-------------------------------------------------------------------------

      end module MYESMF_BaseMod
