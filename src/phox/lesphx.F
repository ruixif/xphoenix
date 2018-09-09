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
      Subroutine LESPHX(NERR)
*
* purpose :  read start parameters using
*            FORTRAN unit <LUNINP>
* date    :  November 15, 1994
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    LUNINP   = FORTRAN unit used for reading data
*
* export variables :
*    DGAMMA   = nuclear transition energy (keV)
*    DECOIL   = nuclear recoil energy (meV)
*    DTEMP    = sample temperature
*    XMES     = file names, measured data
*    XDBG     = background/normalization information
*    XOUT     = file prefix, output data
*    XOPT     = print options, output data
*    DRANGE   = fit range for removal of elastic peak
*    DCRNGE   = convolution range around removed elastic peak
*    DRNGE0   = center of fit range for removal of elastic peak
*    DBGR     = inelastic background under elastic peak
*    DASM     = left/right asymmetry of resolution function
*    XRES     = name of resolution file or shape coefficient
*                  of internally generated resolution function
*    BDCNV    = .TRUE. if deconvolution was requested
*    DIFL     = fraction of FFT data to use if deconvolution
*                  was requested
*    NF       = power coefficient for smoothing of FFT data
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
*
      Character*1  X
      Character*72 XLINES(PLINES),XWORK(2*PLINES),XP(3),XL
*
*
*:  reset error code
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
      N = 0
      Do  I = 1,PLINES
       Read(LUNINP,'(A)',END=100,ERR=999,IOSTAT=IERR) XLINES(I)
       N = N + 1
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
*:  two few input lines
*
      If(NLC.lt.12) Then
       NERR = 1801
       Return
      Endif
*
*:  read valid data
*
      Call CREAD(XLINES(1),' ','::','R8',R,DGAMMA,K,X)
      Call CREAD(XLINES(2),' ','::','R8',R,DECOIL,K,X)
*
      If(DGAMMA.le.0.0) Then
       NERR = 1802
       Return
      Endif
*
      If(DECOIL.le.0.0) Then
       NERR = 1803
       Return
      Endif
*
      Call CREAD(XLINES(3),' ','::','R8',R,DTEMP,K,X)
*add temp control here
      If(DTEMP.lt.0.0D0) Then
       NERR = 1804
       Return
      Endif
*
      Call CREAD(XLINES(4),' ','::','A ',R,D,K,XMES)
      Call CREAD(XLINES(5),' ','::','A ',R,D,K,XDBG)
*add background control here
      
      
      Call CREAD(XLINES(6),' ','::','A ',R,D,K,XOUT)
      If(Index(XOUT,' ').le.1) Then
       NERR = 1805
       Return
      Endif
*
      Call XPARSE(XOUT,' ',2,XP)
      XOUT = XP(1)
      XOPT = XP(2)
      Call XTOUP(XOPT)
      I = Index(XOPT,',')
      If(I.ne.0 .and. Index(XOPT,'G').ne.0) Then
       XOPT(I:I) = ' '
      Endif
*
      Call CREAD(XLINES(7),' ','::','A ',R,D,K,XL)
      DRANGE = 0.D0
      DCRNGE = 0.D0
      DRNGE0 = 0.D0
      Call XPARSE(XL,' ',3,XP)
      Read(XP(1),*) DRANGE
      If(XP(2)(1:1).ne.' ') Read(XP(2),*) DCRNGE
      If(XP(3)(1:1).ne.' ') Read(XP(3),*) DRNGE0
*
      If(DRANGE.lt.0.0D0) Then
       NERR = 1806
       Return
      Endif
*
      If(DCRNGE.lt.0.0D0) Then
       NERR = 1807
       Return
      Endif
*
      Call CREAD(XLINES(8),' ','::','R8',R,DBGR,K,X)
      DBGR = DBGR*1.D-2
*
      Call CREAD(XLINES(9),' ','::','R8',R,DASM,K,X)
      If(DASM.lt.0.1D0 .or. DASM.gt.10.D0) Then
       NERR = 1808
       Return
      Endif
*
      Call CREAD(XLINES(10),' ','::','A ',R,D,K,XRES)
      Call CREAD(XLINES(11),' ','::','A ',R,D,K,XL)
      BDCNV = XL(1:1).eq.'Y' .or. XL(1:1).eq.'y'
*
      DIFL = 0.D0
      NF   = 0

      
      
      If(BDCNV) Then
       Call CREAD(XLINES(12),' ','::','A ',R,D,K,XL)
       Call XPARSE(XL,' ',2,XP)
       If(XP(1)(1:1).ne.' ') Read(XP(1),*) DIFL
       If(XP(2)(1:1).ne.' ') Read(XP(2),*) NF
       If(DIFL.lt.0.01D0 .or. DIFL.gt.2.D0) Then
        NERR = 1809
        Return
       Endif
       If(NF.lt.0) Then
        NERR = 1810
        Return
       Endif
      Else
       Call CREAD(XLINES(12),' ','::','R8',R,DIFL,K,X)
       If(DIFL.lt.0.D0) Then
        NERR = 1811
        Return
       Endif
      Endif
*add temp stepsize change here
      Call CREAD(XLINES(13),' ','::','R8',R,DTEMPEND,K,X)
      Call CREAD(XLINES(14),' ','::','R8',R,DTEMPSTEP,K,X)
      Call CREAD(XLINES(15),' ','::','R8',R,DDBGEND,K,X)
      Call CREAD(XLINES(16),' ','::','R8',R,DDBGSTEP,K,X)
      

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
