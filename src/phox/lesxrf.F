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
      Subroutine LESXRF(NERR)
*
* purpose :  read external resolution function using
*            FORTRAN unit <LUNINP>; modify the shape;
*            bin resolution function.
* date    :  February 6, 1997
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    XRES       = name of resolution file
*    LUNINP     = FORTRAN unit used for reading data
*
* export variables :
*    NERR       = error code
*    BREXT      = .TRUE. if an external resolution function
*                   was defined
*    KRD        = number of data points in res. file
*    DRRX(*)    = x-value, resolution function
*    DRRY(*)    = y-value, resolution function
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
*
*
      Character*1  XTAB
      Character*72 XP(PCMAX),XPP(4)
      Character*300 XLINE
*
*
*:  reset error code
*
      NERR = 0
*
*:  investigate the type of resolution function to use
*
      Call XPARSE(XRES,' ',5,XP)
*
*:  read error on the first position, assume file name
*
      Read(XP(1),*,ERR=101,END=101) DPCF
      If(DPCF.lt.0.1D0 .or. DPCF.gt.10.D0) Then
       NERR = 4801
      Endif
      BREXT = .false.
*
      Return
*
*
*:  an external resolution function was requested
*
  101 Continue
      DPCF  = 1.D0
      BREXT = .true.
*
*:  look for "PROCESS" keyword
*
      BPROC = .false.
      NPROC = 0
      Do  N = 2,4
       Call XTOUP(XP(N))
       If(XP(N)(1:7).eq.'PROCESS') Then
        BPROC  = .True.
        NPROC  = N
        Exit
       Endif
      EndDo
*
*:  analyze process options
*
      If(NPROC.ne.0) Then
       DPROC1 = -1.D0
       DPROC2 = -1.D0
       Call XPARSE(XP(N),':',3,XPP)
       If(XPP(2)(1:1).ne.' ') Then
        Read(XPP(2),*,ERR=102,END=102) DPRC
        DPROC1 = DPRC
       Endif
       If(XPP(3)(1:1).ne.' ') Then
        Read(XPP(3),*,ERR=102,END=102) DPRC
        DPROC2 = DPRC
       Endif
      Endif
  102 Continue
*
*:  determine column numbers and input file name;
*:  defaults are x-col=1, y-col=2;
*
      XRES = XP(1)
*
      ICX = 1
      ICY = 2
*
      If(NPROC.eq.2) Then
       Read(XP(3),*,ERR=200,END=200) ICX
       GoTo 201
  200  Continue
       ICX = 1
  201  Continue
       Read(XP(4),*,ERR=202,END=202) ICY
       GoTo 203
  202  Continue
       ICY = 2
  203  Continue
      Endif
*
      If(NPROC.eq.3) Then
       Read(XP(2),*,ERR=300,END=300) ICX
       GoTo 301
  300  Continue
       ICX = 1
  301  Continue
       Read(XP(4),*,ERR=302,END=302) ICY
       GoTo 303
  302  Continue
       ICY = 2
  303  Continue
      Endif
*
      If(NPROC.eq.4 .or. NPROC.eq.0) Then
       Read(XP(2),*,ERR=400,END=400) ICX
       GoTo 401
  400  Continue
       ICX = 1
  401  Continue
       Read(XP(3),*,ERR=402,END=402) ICY
       GoTo 403
  402  Continue
       ICY = 2
  403  Continue
      Endif
*
      ICC = Max0(ICX,ICY)
*
      If(ICC.gt.PCMAX) Then
       NERR = 4802
       XMSG = XRES
       Return
      Endif
*
      If(ICX.lt.1 .or. ICY.lt.1) Then
       NERR = 4803
       XMSG = XRES
       Return
      Endif
*
*:  open input file
*
      Open(UNIT=LUNINP,FILE=XRES,STATUS='OLD',ERR=999,IOSTAT=IERR)
*
      L = LENX(XRES,' ')
      Write(6,2000) XRES(1:L)
 2000 Format(' -- reading resolution file',6X,'''',A,'''')
*
*:  read data
*
      KL  = Len(XLINE)
      KRD = 0
      XTAB = Char(9)
*
  500 Continue
       Read(LUNINP,'(A)',ERR=501,END=501) XLINE
       Call XADJ(XLINE,' ')
       Call XADJ(XLINE,XTAB)
       Call XADJ(XLINE,',')
*
*:  skip invalid lines
*
       If(XLINE(1:1).eq.' ') GoTo 500
       If(Index('+-0123456789.',XLINE(1:1)).eq.0) GoTo 500
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
        GoTo 502
       Endif
*
*:  tab-separated-value files
*
       If(Index(XLINE,XTAB).ne.0) Then
        Call XPARSE(XLINE,XTAB,ICC+1,XP)
        GoTo 502
       Endif
*
*:  space-separated-value files; this is the default
*
       Call XPARSE(XLINE,' ',ICC+1,XP)
*
  502  Continue
       Read(XP(ICX),*,ERR=501,END=501) DMXR
       Read(XP(ICY),*,ERR=501,END=501) DMYR
       KRD = KRD + 1
*
       DRRX(KRD) = DMXR
       DRRY(KRD) = DMYR
       If(KRD.eq.PDATA) GoTo 501
      GoTo 500
*
  501 Continue
      Close(Unit=LUNINP)
*
*:  too few data values
*
      If(KRD.le.10) Then
       NERR = 4804
       XMSG = XRES
       Return
      Endif
*
*:  process resolution function:
*:   remove background; cut off far tails;
*:   apply asymmetry coefficient to x-scale;
*:   linearize x-scale
*
      Call MDFYRF
*
      Return
*
*:  error exits
*
  999 Continue
      NERR  = 4901
      NCOM1 = IERR
      XMSG  = XRES
*
*
      End
