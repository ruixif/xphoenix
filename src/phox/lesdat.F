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
      Subroutine LESDAT(NERR)
*
* purpose :  read a formatted data file;
*            FORTRAN unit <LUNINP> is used
* date    :  November 22, 1997
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    XMES    = name of data file and options
*    LUNMES  = FORTRAN unit used for reading data
*
* export variables :
*    NERR    = error code
*    XMES    = name of data file, options stripped
*    KD      = number of data points in res. file
*    DMX(*)  = x-values
*    DMY(*)  = y-values
*    DMDY()  = y-errors^2
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
*
      Character*1  XTAB
      Character*72 XP(PCMAX+1)
      Character*300 XLINE
*
*
*:  set error code
*
      NERR = 0
*
*:  determine column numbers and input file name;
*:  defaults are x-col=1, y-col=2, y-err-col=not present
*
      Call XPARSE(XMES,' ',5,XP)
      XMES = XP(1)
*
      Read(XP(2),*,ERR=100,END=100) ICX
      GoTo 101
  100 Continue
      ICX = 1
*
  101 Continue
      Read(XP(3),*,ERR=102,END=102) ICY
      GoTo 103
  102 Continue
      ICY = 2
*
  103 Continue
      Read(XP(4),*,ERR=104,END=104) ICDY
      GoTo 105
  104 Continue
      ICDY = 0
*
  105 Continue
      ICC = MAX0(ICX,ICY,ICDY)
*
      If(ICC.gt.PCMAX) Then
       NERR = 2801
       Return
      Endif
*
      If(ICX.lt.1 .or. ICY.lt.1 .or. ICDY.lt.0) Then
       NERR = 2802
       Return
      Endif
*
*:  open input file
*
      Open(UNIT=LUNMES,FILE=XMES,STATUS='OLD',ERR=999,IOSTAT=IERR)
*
      L = LENX(XMES,' ')
      Write(6,2000) XMES(1:L)
 2000 Format(' -- reading data file',12X,'''',A,'''')
*
*:  read data
*
      KL = Len(XLINE)
      KD = 0
      XTAB = Char(9)
*
  200 Continue
       Read(LUNMES,'(A)',ERR=201,END=201) XLINE
       Call XADJ(XLINE,' ')
       Call XADJ(XLINE,XTAB)
       Call XADJ(XLINE,',')
*
*:  skip invalid lines
*
       If(XLINE(1:1).eq.' ') GoTo 200
       If(Index('+-0123456789.',XLINE(1:1)).eq.0) GoTo 200
*
*:  csv files
*
       If(Index(XLINE,',').ne.0) Then
        Do  K = 1,KL-1
         If(XLINE(K:K+1).eq.',,') Then
          XLINE(K+1:KL) = '0'//XLINE(K+1:KL-1)
         Endif
        EndDo
        If(XLINE(1:1).eq.',') Then
         XLINE = '0'//XLINE(1:KL-1)
        Endif
        Call XPARSE(XLINE,',',ICC+1,XP)
        GoTo 202
       Endif
*
*:  tab-separated-value files
*
       If(Index(XLINE,XTAB).ne.0) Then
        Call XPARSE(XLINE,XTAB,ICC+1,XP)
        GoTo 202
       Endif
*
*:  space-separated-value files; this is the default
*
       Call XPARSE(XLINE,' ',ICC+1,XP)
*
  202  Continue
       Read(XP(ICX),*,ERR=201,END=201) DMXR
       Read(XP(ICY),*,ERR=201,END=201) DMYR
       If(ICDY.LE.0) Then
        DMDYR = Dmax1(DMYR,1.D0)
       Else
        Read(XP(ICDY),*,ERR=201,END=201) DMDYR
        DMDYR = DMDYR**2
       Endif
       KD = KD + 1
*
       DMX(KD)  = DMXR
       DMY(KD)  = DMYR
       DMDY(KD) = DMDYR
*
       If(KD.eq.PDATA) GoTo 201
      GoTo 200
*
  201 Continue
      Close(LUNMES)
*
*:  too few data values
*
      If(KD.LE.10) Then
       NERR = 2803
      Endif
*
      Return
*
*:  IO error exit
*
  999 Continue
      NERR  = 2901
      NCOM1 = IERR
*
*
      End
