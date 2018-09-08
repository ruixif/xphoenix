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
      Subroutine LESPAD(NERR)
*
* purpose :  read start parameters using
*            FORTRAN unit <LUNINP>
* date    :  April 8, 1996
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    LUNINP   = FORTRAN unit used for reading data
*    XDIR     = directory where executable resides
*
* export variables :
*    NCL         = total number of input files
*    DOPENG      = nuclear transition energy (keV)
*    DTEMP       = temperature of sample (K)
*    DLNCS       = lattice constant (m), monochromator
*    DTHEX       = thermal expansion coeff. (1/K), monochromator
*    DTHEXA      = thermal expansion coeff. (1/K), analyzer
*    MILINX(*,1) = Miller indices of 1.crystal
*    MILINX(*,2) = Miller indices of 2.crystal
*    DENSCL      = linear scaling for energy axis
*    DENQC       = quadratic scaling for energy axis (1.E-6/meV)
*    DENSHF      = shift of energy axis
*    KINTPL      = interpolation scheme used for adding data sets
*                   0 :  quadratic interpolation (default)
*                   1 :  linear interpolation
*                   2 :  binning, bin size given by first data set
*    XOUT        = file name, summed data
*    XOPT        = output options
*    XCC         = comment character used in output file
*    DRANGE      = range for fit of dominant peak if BPEAK=.TRUE.
*    DCRNGE      = start position for fit of dominant peak if nonzero
*                            and BPEAK=.TRUE.
*    BPEAK       = if DRANGE=0 no peak fitting will be performed
*                   and BPEAK=.FALSE. otherwise BPEAK=.TRUE.
*    DBGR        = inelastic background under elastic peak
*    DASM        = left/right asymmetry of resolution function
*    XRES        = name of resolution file or shape coefficient
*                   of internally generated resolution function
*    DE0P1       = position of 1.crystal (microrad) if BPEAK=.FALSE.
*    DE0P2       = position of 2.crystal (microrad) if BPEAK=.FALSE.
*    DE0T1       = temperature of 1.crystal (K) if BPEAK=.FALSE.
*    DE0T2       = temperature of 2.crystal (K) if BPEAK=.FALSE.
*    DE0TA       = temperature of analyzer crystal (K) if BPEAK=.FALSE.
*    XPLGIN      = name plugin for specific data type of input files
*    XINTYP      = upper-case version of XPLGIN
*    XMES(*)     = file names + column specifiers, measured data
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
      Character*1   X
      Character*200 XLINES(PLINES),XWORK(2*PLINES),XP(5),
     +              XFULL,XD1,XL
*
*:  set error code
*
      NERR = 0
*
*:  read file, exit on error
*
      Inquire(Unit=LUNINP,Name=XL)
      L = LENX(XL,' ')
      Write(6,2000) XL(1:L)
 2000 Format(' -- reading standard input file  ''',A,'''')
*
*:  read file, exit on error
*
      N = 0
      Do  I = 1,PLINES
       Read(LUNINP,'(A)',END=200,ERR=999,IOSTAT=IERR) XLINES(I)
       N = N + 1
      EndDo
  200 Continue
*
*:  strip comment lines, comment char. '*'
*
      Call XSTRIP(N,N1,'*',XLINES)
*
*:  remove empty lines
*
      Call XRMEL(N1,NZ,' ',XLINES)
*
*:  interpret command lines (command lines start with '@')
*:  execute substitutions
*:  strip command lines
*
      Call XINTPR(NZ,NLC,'@',XLINES,XWORK,NSUB)
*
      NLC = NZ - NLC
      NZ  = Min0(NZ,PDS+14)
*
*:  read valid data
*
      Call CREAD(XLINES(1) ,' ','::','R8',R,DOPENG,K,X)
*
      If(DOPENG.le.0.0) Then
       NERR = 1801
       Return
      Endif
*
*:  temperature
*
      Call CREAD(XLINES(2) ,' ','::','R8',R,DTEMP,K,X)
*
      If(DTEMP.lt.0.0D0) Then
       NERR = 1802
       Return
      Endif
      DTEMP = Dmax1(1.D-3,DTEMP)
*
*:  lattice constant
*
      Call CREAD(XLINES(3) ,' ','::','R8',R,DLCNS,K,X)
*
      If(DLCNS.le.0.0D0) Then
       NERR = 1803
       Return
      Endif
      DLCNS = 1.D-10*DLCNS
*
*:  thermal expansion
*
      Call CREAD(XLINES(4) ,' ','::','A ',R,D,K,XL)
*
      DTHEX  = 0.D0
      DTHEXA = 0.D0
      Call XPARSE(XL,' ',3,XP)
      Read(XP(1),*,ERR=100,END=100) DTHEX
  100 Read(XP(2),*,ERR=101,END=101) DTHEXA
  101 Continue
      If(DTHEXA.eq.0.D0) DTHEXA = DTHEX
*
*:  Miller indices and rotation directions
*
      Call CREAD(XLINES(5) ,' ','::','A ',R,D,K,XL)
      Call XPARSE(XL,' ',5,XP)
*
      MILINX(1,1) = 0
      MILINX(2,1) = 0
      MILINX(3,1) = 0
      DROTD1      = 1.D0
      Read(XP(1),*,ERR=102,END=102) MILINX(1,1)
  102 Read(XP(2),*,ERR=103,END=103) MILINX(2,1)
  103 Read(XP(3),*,ERR=104,END=104) MILINX(3,1)
  104 Read(XP(4),*,ERR=105,END=105) DROTD1
  105 Continue
*
      Call CREAD(XLINES(6) ,' ','::','A ',R,D,K,XL)
      Call XPARSE(XL,' ',5,XP)
*
      MILINX(1,2) = 0
      MILINX(2,2) = 0
      MILINX(3,2) = 0
      DROTD2      = 1.D0
      Read(XP(1),*,ERR=106,END=106) MILINX(1,2)
  106 Read(XP(2),*,ERR=107,END=107) MILINX(2,2)
  107 Read(XP(3),*,ERR=108,END=108) MILINX(3,2)
  108 Read(XP(4),*,ERR=109,END=109) DROTD2
  109 Continue
*
*:  energy rescaling and shift applied to output data
*:  interpolation scheme, 
*
      Call CREAD(XLINES(7) ,' ','::','A ',R,D,K,XL)
      DENSCL = 1.D0
      DENQC  = 0.D0
      DENSHF = 0.D0
      Call XPARSE(XL,' ',4,XP)
      Read(XP(1),*,ERR=110,END=110) DENSCL
  110 Read(XP(2),*,ERR=111,END=111) DENQC
  111 Read(XP(3),*,ERR=112,END=112) DENSHF
  112 Continue
*
      KINTPL = 0
      Call XTOUP(XP(4))
      If(XP(4)(1:1).eq.'L') KINTPL = 1
      If(XP(4)(1:1).eq.'B') KINTPL = 2
*
      If(DENSCL.eq.0.0D0) Then
       NERR = 1804
       Return
      Endif
*
*:  name of output file, comment character
*
      Call CREAD(XLINES(8) ,' ','::','A ',R,D,K,XOUT)
      Call XPARSE(XOUT,' ',2,XP)
      XOUT = XP(1)(1:72)
      XOPT = XP(2)(1:72)
      Call XTOUP(XOPT)
*
      Call CREAD(XLINES(9) ,' ','::','A ',R,D,K,XCC)
*
*:  range and center for the fit
*
      Call CREAD(XLINES(10),' ','::','A ',R,D,K,XL)
*
      DRANGE = 0.D0
      DCRNGE = 0.D0
      Call XPARSE(XL,' ',3,XP)
      Read(XP(1),*,ERR=113,END=113) DRANGE
  113 Read(XP(2),*,ERR=114,END=114) DCRNGE
  114 Continue
*
*:  inelastic background, peak asymmetry, resolution file
*
*:  if the "fit range" is zero or negative
*:   the zero-energy is derived from crystal
*:   positions and temperatures
*
      If(DRANGE.gt.0.0D0) Then
       BPEAK = .true.
       Call CREAD(XLINES(11),' ','::','R8',R,DBGR,K,X)
       Call CREAD(XLINES(12),' ','::','R8',R,DASM,K,X)
       Call CREAD(XLINES(13),' ','::','A ',R,D,K,XRES)
*
       DBGR = DBGR*1.D-2
       If(DASM.lt.0.1D0 .or. DASM.gt.10.D0) Then
        NERR = 1805
        Return
       Endif
      Else
       BPEAK = .false.
       DBGR  =   0.D0
       DASM  =   1.D0
       DE0P1 =   0.D0
       DE0T1 = 300.D0
       DE0P2 =   0.D0
       DE0T2 = 300.D0
       DE0TA = 300.D0
*
       Call CREAD(XLINES(11),' ','::','A ',R,D,K,XL)
       Call XPARSE(XL,' ',3,XP)
       Read(XP(1),*,ERR=115,END=115) DE0P1
  115  Read(XP(2),*,ERR=116,END=116) DE0T1
  116  Continue
       DE0P1 = DROTD1*DE0P1
*
       Call CREAD(XLINES(12),' ','::','A ',R,D,K,XL)
       Call XPARSE(XL,' ',3,XP)
       Read(XP(1),*,ERR=117,END=117) DE0P2
  117  Read(XP(2),*,ERR=118,END=118) DE0T2
  118  Continue
       DE0P2 = DROTD2*DE0P2
*
       Call CREAD(XLINES(13),' ','::','R8',R,DE0TA,K,X)
      Endif
*
*:  name of plugin for data read
*
      Call CREAD(XLINES(14),' ','::','A ',R,D,K,XPLGIN)
*
*:  ASCII is the default plugin
*
      If(XPLGIN(1:1).eq.' ') Then
       XPLGIN = 'ASCII'
      Endif
*
*:  non-native data types:
*:   check if plugin exists
*:   publish full name of plugin
*
      XINTYP = XPLGIN
      Call XTOUP(XINTYP)
      If(XINTYP(1:5).ne.'ASCII') Then
       If(Index(XVERSION,'@1').eq.0) Then
        L   = Lenx(XDIR,' ')
        XD1 = XDIR(1:L-4)
        Call XMERGE(XD1,'/share/PHOENIX-'//XVERSION)
        Call XMERGE(XD1,'/plugins/')
        XFULL = XD1
        Call XMERGE(XFULL,XPLGIN)
        Inquire(File=XFULL,Exist=B)
       Else
        XD1 = 
     + '/Users/wolfgang/SoftwareDevelopment/PHOENIX-devel/plugins/'
        XFULL = XD1
        Call XMERGE(XFULL,XPLGIN)
        Inquire(File=XFULL,Exist=B)
       Endif
*
       If(.not.B) Then
        XDIR = XD1
        NERR = 1806
        Return
       Endif
*
       XPLGIN = XFULL
      Endif
*
*:  data files
*
      NCL = NZ - 14
      Do  I = 1,NCL
       Call CREAD(XLINES(I+14),' ','::','A ',R,D,K,XMES(I))
      EndDo
*
*:  analysis of file names and expansion if range is specified
*
      Call FILEXP
*
      Return
*
*:  IO error exit
*
  999 Continue
      NERR  = 1901
      NCOM1 = IERR
      NCOM2 = N + 1
*
*
      End
