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
      Subroutine LESPST(NERR)
*
* purpose :  read start parameters using
*            FORTRAN unit <LUNINP>
* date    :  December 20, 2010
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    LUNINP   = FORTRAN unit used for reading data
*
* export variables :
*    DGAMMA    = nuclear transition energy (keV)
*    DECOIL    = nuclear recoil energy (meV)
*    DTMP1     = begin of temperature range (K)
*    DTMP2     = end of temperature range (K)
*    NTMP      = number of temperature points
*    DENG1     = begin of energy range (meV)
*    DENG2     = end of energy range (meV)
*    NENG      = number of energy points
*    XDOS      = file names, PDOS
*    XOUT      = file prefix, output data
*    XOPT      = print options, output data
*    DFWHM     = FWHM of resolution function
*    DSHP      = shape coefficient of resolution function
*    DASM      = left/right asymmetry of resolution function
*    DELPST    = strength of elastic peak
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<psth.h>
*
      Character*1  X
      Character*72 XLINES(PLINES),XWORK(2*PLINES),XP(15),XPP(15),
     +             XTEMP,XENG,XELP
*
*
*:  reset error code
*
      NERR = 0
*
*:  read file, exit on error
*
      Inquire(Unit=LUNINP,Name=XELP)
      L = LENX(XELP,' ')
      Write(6,2000) XELP(1:L)
 2000 Format(' -- reading standard input file  ''',A,'''')
*
      N = 0
      Do  I = 1,PLINES
       Read(LUNINP,'(A)',END=100,ERR=999,IOSTAT=IERR) XLINES(I)
       N  = N + 1
      EndDo
  100 Continue
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
*:  read valid data
*
      Call CREAD(XLINES(1),' ','::','R8',R,DGAMMA,K,X)
      Call CREAD(XLINES(2),' ','::','R8',R,DECOIL,K,X)
      Call CREAD(XLINES(3),' ','::','A ',R,D,K,XDOS)
      Call CREAD(XLINES(4),' ','::','A ',R,D,K,XTEMP)
      Call CREAD(XLINES(5),' ','::','A ',R,D,K,XENG)
      Call CREAD(XLINES(6),' ','::','A ',R,D,K,XOUT)
*
*:  evaluate or modify some input parms
*
      Call XADJ(XTEMP,' ')
      Call XPARSE(XTEMP,' ',4,XP)
      DTMP1 = 0.D0
      If(XP(1)(1:1).ne.' ') Then
       Read(XP(1),*) DTMP1
      Endif
      DTMP2 = DTMP1
      NTMP  = 1
      If(XP(2)(1:1).ne.' ') Then
       Read(XP(2),*) DTMP2
       If(XP(3)(1:1).ne.' ') Read(XP(3),*) NTMP
      Endif
      If(DTMP2.lt.DTMP1) Then
       D     = DTMP1
       DTMP1 = DTMP2
       DTMP2 = D
      Endif
*
      Call XADJ(XENG,' ')
      Call XPARSE(XENG,' ',4,XP)
      DENG1 = 0.D0
      If(XP(1)(1:1).ne.' ') Then
       Read(XP(1),*) DENG1
      Endif
      DENG2 = DENG1
      NENG  = 1
      If(XP(2)(1:1).ne.' ') Then
       Read(XP(2),*) DENG2
       If(XP(3)(1:1).ne.' ') Read(XP(3),*) NENG
      Endif
      If(DENG2.lt.DENG1) Then
       D     = DENG1
       DENG1 = DENG2
       DENG2 = D
      Endif
*
*:  collect save options
*
      Call XPARSE(XOUT,' ',15,XP)
      XOUT = XP(1)
      Call XFILL(XOPT,1,Len(XOPT),' ')
      KK = 1
      Do  K = 2,14
       If(XP(K)(1:1).eq.' ') Exit
       Call XPARSE(XP(K),'.',15,XPP)
       Do  J = 1,14
        If(XPP(J)(1:1).eq.' ') Exit
        XOPT(KK:KK) = XPP(J)(1:1)
        KK = KK+1
       EndDo
      EndDo
      Call XTOUP(XOPT)
*
*:  invalid input parameters
*
      If(DGAMMA.le.0.D0) Then
       NERR = 1801
       Return
      Endif
*
      If(DECOIL.le.0.D0) Then
       NERR = 1802
       Return
      Endif
*
      If(DTMP1.le.0.D0) Then
       NERR = 1803
       Return
      Endif
*
      If(DTMP2.le.0.D0) Then
       NERR = 1803
       Return
      Endif
*
      If(NTMP.le.0) Then
       NERR = 1804
       Return
      Endif
*
      If(NENG.le.0) Then
       NERR = 1805
       Return
      Endif
*
      If(Index(XOUT,' ').le.1) Then
       NERR = 1806
       Return
      Endif
*
      If(Index(XOPT,'S').eq.0)  Return
*
      Call CREAD(XLINES(7),' ','::','R8',R,DFWHM,K,X)
      Call CREAD(XLINES(8),' ','::','R8',R,DSHP,K,X)
      Call CREAD(XLINES(9),' ','::','R8',R,DASM,K,X)
      Call CREAD(XLINES(10),' ','::','A ',R,D,K,XELP)
*
      Call XADJ(XELP,' ')
      Call XPARSE(XELP,' ',4,XP)
      DELPST = 0.D0
      If(XP(1)(1:1).ne.' ') Then
       Read(XP(1),*) DELPST
      Endif
      DINTCT = 0.D0
      DBGRD  = 0.D0
      If(XP(2)(1:1).ne.' ') Then
       Read(XP(2),*) DINTCT
       If(XP(3)(1:1).ne.' ') Read(XP(3),*) DBGRD
       DINTCT = Dmax1(1.D2,DINTCT)
       DBGRD  = Dmax1(0.D0,DBGRD)
      Endif
*
      If(DFWHM.gt.0.01) Then
       If(DSHP.lt.0.1) Then
        NERR = 1807
        Return
       Endif
*
       If(DASM.lt.0.1) Then
        NERR = 1808
        Return
       Endif
      Endif
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
