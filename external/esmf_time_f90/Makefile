# To build this by itself, use the make target myesmf_time_f90_only
# from the top-level WRF Makefile.  
# > cd ../..
# configure
# make myesmf_time_f90_only

.SUFFIXES: .F90 .o .f

# get rid of single quotes after comments
# WARNING:  This will break if a quoted string is followed by a comment that has
#           a single quote.
SED_FTN = sed -e "/\!.*'/s/'//g"

RM = /bin/rm -f
AR = ar
#RANLIB	= ranlib
RANLIB	= echo
FGREP = fgrep -iq

OBJS = MYESMF_Alarm.o MYESMF_BaseTime.o MYESMF_Clock.o MYESMF_Time.o \
        Meat.o MYESMF_Base.o MYESMF_Calendar.o MYESMF_Fraction.o   \
        MYESMF_TimeInterval.o MYESMF_Stubs.o MYESMF_Mod.o \
        module_symbols_util.o \
	module_utility.o MYESMF_AlarmClock.o

default: libmyesmf_time.a

tests: Test1_MYESMF.exe Test1_WRFU.exe

libmyesmf_time.a : $(OBJS)
	$(RM) libmyesmf_time.a
	if [ "$(AR)" != "lib.exe" ] ; then \
          $(AR) ru libmyesmf_time.a $(OBJS) ; \
        else \
          $(AR) /out:libmyesmf_time.a $(OBJS) ; \
        fi
	$(RANLIB) libmyesmf_time.a

Test1_MYESMF.f : Test1.F90
	$(RM) Test1_MYESMF.b Test1_MYESMF.f
	cp Test1.F90 Test1_MYESMF.b
	$(CPP) -I. Test1_MYESMF.b > Test1_MYESMF.f

Test1_MYESMF.exe : libmyesmf_time.a Test1_MYESMF.o
	$(FC) -o Test1_MYESMF.exe Test1_MYESMF.o libmyesmf_time.a

Test1_WRFU.f : Test1.F90
	$(RM) Test1_WRFU.b Test1_WRFU.f
	sed -e "s/MYESMF_Mod/module_utility/g" -e "s/MYESMF_/WRFU_/g" Test1.F90 > Test1_WRFU.b
	$(CPP) -I. Test1_WRFU.b > Test1_WRFU.f

Test1_WRFU.exe : libmyesmf_time.a Test1_WRFU.o
	$(FC) -o Test1_WRFU.exe Test1_WRFU.o libmyesmf_time.a

.F90.o :
	$(RM) $@
	$(SED_FTN) $*.F90 > $*.b
	$(CPP) -I. $*.b > $*.f
	$(RM) $*.b
	@ if echo $(CPP) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi
	$(FC) -o $@ -c $*.f

.F90.f :
	$(RM) $@
	$(SED_FTN) $*.F90 > $*.b
	$(CPP) -I. $*.b > $*.f
	$(RM) $*.b
	@ if echo $(CPP) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi

.f.o :
	$(RM) $@
	$(RM) $*.b
	$(FC) -c $*.f

clean : testclean

testclean: 
	$(RM) *.b *.f *.o *.obj *.i libmyesmf_time.a *.mod Test1*.exe

superclean: testclean
	$(RM) Test1*.out make_tests.out

# DEPENDENCIES : only dependencies after this line 

#$$$ update dependencies!  

MYESMF_Alarm.o : MYESMF_Base.o MYESMF_Time.o MYESMF_TimeInterval.o
MYESMF_BaseTime.o : MYESMF_Base.o
MYESMF_Clock.o : MYESMF_Base.o MYESMF_Time.o MYESMF_TimeInterval.o MYESMF_Alarm.o
MYESMF_AlarmClock.o : MYESMF_Alarm.o MYESMF_Clock.o MYESMF_Time.o MYESMF_TimeInterval.o
MYESMF_Time.o : MYESMF_Base.o MYESMF_BaseTime.o MYESMF_TimeInterval.o MYESMF_Calendar.o \
              MYESMF_Stubs.o
MYESMF_Base.o :
MYESMF_Calendar.o : MYESMF_Base.o MYESMF_BaseTime.o
MYESMF_Fraction.o :
MYESMF_TimeInterval.o : MYESMF_Base.o MYESMF_BaseTime.o MYESMF_Calendar.o MYESMF_Fraction.o
MYESMF_Mod.o : MYESMF_Alarm.o MYESMF_BaseTime.o MYESMF_Clock.o MYESMF_Time.o \
        MYESMF_Base.o MYESMF_Calendar.o MYESMF_Fraction.o    \
        MYESMF_TimeInterval.o MYESMF_Stubs.o MYESMF_AlarmClock.o
Meat.o : MYESMF_Alarm.o MYESMF_BaseTime.o MYESMF_Clock.o MYESMF_Time.o \
        MYESMF_Base.o MYESMF_Calendar.o MYESMF_Fraction.o    \
        MYESMF_TimeInterval.o
MYESMF_Stubs.o : MYESMF_Base.o MYESMF_Calendar.o
module_utility.o :  module_symbols_util.o
module_symbols_util.o :  MYESMF_Mod.o
Test1.o :  module_utility.o

