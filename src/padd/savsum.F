*
**************************************************************
*
*  PHOENIX Version 2.1.4
*  Copyright (C) 2016 Wolfgang Sturhahn
*
*  This file is part of PHOENIX-2.1.4.
*
*  PHOENIX-2.1.4 is free software:
*  You can redistribute it and/or modify it under the terms
*  of the GNU General Public License as published by the
*  Free Software Foundation
*
*  PHOENIX-2.1.4 is distributed in the hope that it will be
*  useful, but WITHOUT ANY WARRANTY; without even the implied
*  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*  PURPOSE. See the GNU General Public License for details.
*
*  A copy of the GNU General Public License should have been
*  provided along with PHOENIX-2.1.4.
*  If not, see <http://www.gnu.org/licenses/>.
*
**************************************************************
*
      Subroutine SAVSUM(NERR)
*
* purpose :  save the sum of all data.
* date    :  November 7, 1997
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    XOUT       = name of output file
*    XCC        = comment char. in output file
*    XRES       = name of resolution file
*    BREXT      = .TRUE. if resolution file was used
*    DASM       = left/right asymmetry of resolution function
*    DPCF       = power coefficient of internally generated
*                  resolution function
*    DSX(*)     = energy values
*    DSY(*)     = sum of counts
*    BPEAK      = if .FALSE. no peak fitting will be performed
*
* export variables :
*    NERR       = error code
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
*
      Character*4  XEXT
      Character*72 XFILE,XF3
*
      Save XFILE
*
*
*:  set error code
*
      NERR = 0
*
      If(Index(XOPT,',').eq.0) Then
       BCSV = .false.
       XF3  = XFORM3
       XEXT = '.dat'
      Else
       BCSV = .true.
       XF3  = XFORM3C
       XEXT = '.csv'
      Endif
*
*:  open output file
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_sum'//XEXT)
      Open(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
*:  write comments to the output file
*
      If(BCSV) Then
       Write(LUNOUT,2000,IOSTAT=IERR,ERR=999) XDATE,XTIME,XZONE
      Else
       Write(LUNOUT,2001,IOSTAT=IERR,ERR=999) XCC,
     +    XCC,XDATE,XCC,XTIME,XZONE,XCC
      Endif
*
 2000 Format('Date of conversion : ',A,
     +           ',Time of conversion : ',A,1X,A)
 2001 Format(A/A,'  Date of conversion : ',A/
     +         A,'  Time of conversion : ',A,1X,A/A)
*
      If(BPEAK) Then
       If(BREXT) Then
        If(BCSV) Then
         Write(LUNOUT,2002,IOSTAT=IERR,ERR=999)
     +            XRES(1:LENX(XRES,' ')),DASM
        Else
         Write(LUNOUT,2003,IOSTAT=IERR,ERR=999) XCC,
     +            XRES(1:LENX(XRES,' ')),XCC,DASM,XCC
        Endif
       Else
        If(BCSV) Then
         Write(LUNOUT,2004,IOSTAT=IERR,ERR=999) DASM,DPCF
        Else
         Write(LUNOUT,2005,IOSTAT=IERR,ERR=999) XCC,XCC,
     +            DASM,DPCF,XCC
        Endif
       Endif
       If(BCSV) Then
        Write(LUNOUT,2008,IOSTAT=IERR,ERR=999)
       Else
        Write(LUNOUT,2009,IOSTAT=IERR,ERR=999) XCC
       Endif
*
       Do  I = 1,NCL
        If(BDS(I)) Then
         If(BCSV) Then
          If(Dabs(DTBH(I)).le.2.D0*DDTBH(I)) XINFO(I)(4:5) = 'ok'
          If(Dabs(DTBH(I)).gt.2.D0*DDTBH(I)) XINFO(I)(5:5) = '?'
          If(Dabs(DTBH(I)).gt.3.D0*DDTBH(I)) XINFO(I)(4:4) = '?'
         Else
          If(Dabs(DTBH(I)).le.2.D0*DDTBH(I)) XINFO(I)(7:8) = 'ok'
          If(Dabs(DTBH(I)).gt.2.D0*DDTBH(I)) XINFO(I)(8:8) = '?'
          If(Dabs(DTBH(I)).gt.3.D0*DDTBH(I)) XINFO(I)(7:7) = '?'
         Endif
         L = LENX(XINFO(I),' ')
         Write(LUNOUT,'(A)',IOSTAT=IERR,ERR=999) XINFO(I)(1:L)
        Endif
       EndDo
      Else
       If(BCSV) Then
        Write(LUNOUT,2006,IOSTAT=IERR,ERR=999)
     +       DE0P1,DE0P2,DE0T1,DE0T2,DE0TA
       Else
        Write(LUNOUT,2007,IOSTAT=IERR,ERR=999)
     +       XCC,XCC,DE0P1,DE0P2,XCC,DE0T1,DE0T2,DE0TA,XCC
       Endif
      Endif
*
 2002 Format('Fit function for dominant peak from file "',A,'"'
     +       ',asymmetry factor :',F6.2)
 2003 Format(A,' Fit function for dominant peak from file "',A,'".'/
     +       A,'  asymmetry factor :',F6.2/A)
*
 2004 Format('Fit function for dominant peak is of exponential type,'
     +         'asymmetry factor :',F6.2,',shape coefficient :',F6.2)
 2005 Format(A,' Fit function for dominant peak is of exponential type'/
     +   A,'  asymmetry factor :',F6.2,3X,'shape coefficient :',F6.2/A)
*
 2006 Format('The zero-energy is fixed by,crystal positions :',2F11.2,
     +       ',crystal temperatures :',3F11.2)
 2007 Format(A,' The zero-energy is fixed by'/
     +       A,4X,'crystal positions    :',2F11.2/
     +       A,4X,'crystal temperatures :',3F11.2/A)
*
 2008 Format(',detailed imbalance,Chi^2,peak,FWHM,position,',
     +           '%background,input file')
 2009 Format(A,'   det.bal.  Chi^2    peak    FWHM    pos.  %bkgr.',
     +           2X,'input file')
*
      If(BCSV) Then
       Write(LUNOUT,*,IOSTAT=IERR,ERR=999)
      Else
       Write(LUNOUT,'(A)',IOSTAT=IERR,ERR=999) XCC
      Endif
*
*:  write the sum to the output file
*
      B = .true.
      Call SAVHDR(LUNOUT,B,'energy (meV)','counts')
*
      Do  K = 1,KDS
       Write(LUNOUT,XF3,IOSTAT=IERR,ERR=999)
     +            DSX(K),DSY(K),Dsqrt(DSDY(K))
      EndDo
*
      Close(UNIT=LUNOUT)
*
      Return
*
*:  IO error exits
*
  998 Continue
      NERR  = 6901
      NCOM1 = IERR
      XMSG1 = XFILE
      Return
*
  999 Continue
      NERR  = 6902
      NCOM1 = IERR
      XMSG1 = XFILE
*
*
      End
