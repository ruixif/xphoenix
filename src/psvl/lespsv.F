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
      Subroutine LESPSV(NERR)
*
* purpose :  read start parameters using
*            FORTRAN unit <LUNINP>
* date    :  April 2, 2006
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    LUNINP   = FORTRAN unit used for reading data
*
* export variables :
*    DMASS      = mass number of resonant isotope
*    DPRES      = pressure of material (GPa)
*    XEOS       = EOS type
*    DRHO       = sample density at p=0 (g/cm^3)
*    DDRHO      = error of density
*    DKB        = bulk modulus at p=0 (GPa)
*    DDKB       = error of bulk modulus
*    DKBP       = pressure derivative of bulk mod. at p=0
*    DDKBP      = error of bulk modulus derivative
*    DWS1       = fit range beginning, start value
*    DWS2       = fit range beginning, end value
*    NWS        = number of fit range beginning values
*    DWW1       = fit range width, start value
*    DWW2       = fit range width, end value
*    NWW        = number of fit range width values
*    XOPT1      = description of fit parameter A1
*    XOPT2      = description of fit parameter A2
*    XMES       = input file with DOS values
*    XOUT       = prefix for output files
*    XOPT       = output format options
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<psvl.h>
*
      Character*1  X
      Character*2  XX
      Character*72 XLINES(PLINES),XWORK(2*PLINES),XP(4),XL0
*
*
*:  reset error code
*
      NERR = 0
*
*:  read file, exit on error
*
      Inquire(Unit=LUNINP,Name=XL0)
      L = LENX(XL0,' ')
      Write(6,2000) XL0(1:L)
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
      Call CREAD(XLINES(1),' ','::','R8',R,DMASS,K,X)
      Call CREAD(XLINES(2),' ','::','R8',R,DPRES,K,X)
      Call CREAD(XLINES(3),' ','::','A ',R,D,K,XEOS)
      Call XTOUP(XEOS)
*
*:  error checks on parameters
*
      If(DMASS.LE.0.D0) Then
       NERR = 1801
       Return
      Endif
*
      If(DPRES.LT.0.D0) Then
       NERR = 1802
       Return
      Endif
*
      If(DPRES.ne.0.D0) Then
       XX = XEOS(1:2)
       If(XX.ne.'BM'.and. XX.ne.'VI' .and. XX.ne.'LO') Then
        XEOS = 'BM'
        NERR = 1701
        Call ERRCHK(NERR,*101)
  101   Continue
       Endif
      Endif
*
*:  read valid data
*
      Call CREAD(XLINES(4),' ','::','A ',R,D,K,XL0)
      Call XADJ(XL0,' ')
      Call XPARSE(XL0,' ',4,XP)
      Read(XP(1),*) DRHO
      If(XP(2)(1:1).ne.' ') Then
       Read(XP(2),*) DDRHO
      Else
       DDRHO = 0.D0
      Endif
*
      Call CREAD(XLINES(5),' ','::','A ',R,D,K,XL0)
      Call XADJ(XL0,' ')
      Call XPARSE(XL0,' ',4,XP)
      Read(XP(1),*) DKB
      If(XP(2)(1:1).ne.' ') Then
       Read(XP(2),*) DDKB
      Else
       DDKB = 0.D0
      Endif
*
      Call CREAD(XLINES(6),' ','::','A ',R,D,K,XL0)
      Call XADJ(XL0,' ')
      Call XPARSE(XL0,' ',4,XP)
      Read(XP(1),*) DKBP
      If(XP(2)(1:1).ne.' ') Then
       Read(XP(2),*) DDKBP
      Else
       DDKBP = 0.D0
      Endif
*
*:  error checks on parameters
*
      If(DRHO.le.0.D0 .or. DDRHO.lt.0.D0) Then
       NERR = 1803
       Return
      Endif
*
      If(DKB.le.0.D0 .or. DDKB.lt.0.D0) Then
       NERR = 1804
       Return
      Endif
*
      If(DPRES.ne.0.D0) Then
       If(DKBP.le.0.D0 .or. DDKBP.lt.0.D0) Then
        NERR = 1805
        Return
       Endif
      Endif
*
*:  read valid data
*
      Call CREAD(XLINES(7),' ','::','R8',R,DWS1,K,X)
*
      Call CREAD(XLINES(8),' ','::','A ',R,D,K,XL0)
      Call XADJ(XL0,' ')
      Call XPARSE(XL0,' ',4,XP)
      Read(XP(1),*) DWW1
      If(XP(2)(1:1).ne.' ') Then
       Read(XP(2),*) DWW2
      Else
       DWW2 = DWW1
       NWW  = 1
      Endif
      If(XP(3)(1:1).ne.' ') Then
       Read(XP(3),*) NWW
      Else
       NWW  = 1
      Endif
      If(DWW1.eq.DWW2) NWW = 1
      NWW = Min0(NWW,PSVL)
*
      Call CREAD(XLINES(9),' ','::','A ',R,D,K,XOPT1)
      Call CREAD(XLINES(10),' ','::','A ',R,D,K,XOPT2)
      Call XTOUP(XOPT1)
      Call XTOUP(XOPT2)
*
      Call CREAD(XLINES(11),' ','::','A ',R,D,K,XMES)
      Call CREAD(XLINES(12),' ','::','A ',R,D,K,XOUT)
      Call XPARSE(XOUT,' ',2,XP)
      XOUT = XP(1)
      XOPT = XP(2)
      Call XTOUP(XOPT)
*
      Return
*
*:  IO error exit
*
  999 Continue
      NERR = 1901
      MSG1 = IERR
      MSG2 = N + 1
*
*
      End
