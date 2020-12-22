#!/bin/bash

lst=`ls -al *.F90 *.inc Makefile | awk '{print $9}'`
for i in $lst
do
  echo ":::: $i --> ${i/ESMF/MYESMF} ::::"
  sed -i -- 's/ESMF/MYESMF/g' $i
  sed -i -- 's/esmf/myesmf/g' $i
  mv $i ${i/ESMF/MYESMF} 
done
