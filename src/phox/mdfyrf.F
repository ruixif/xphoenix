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
      Subroutine MDFYRF
*
* Purpose :  manipulate and bin resolution function
* Date    :  December 6, 1997
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    BPROC      = if true, process file further
*    DPROC1     = tail region level
*    DPROC2     = noise level cutoff
*    KRD        = number of data points in res. file
*    DRRX(*)    = x-value, resolution function
*    DRRY(*)    = y-value, resolution function
*    DASM       = left/right asymmetry of resolution function
*
* export variables :
*    KRD        = number of data points in res. file, modified
*    DRRX(*)    = x-value, resolution function, modified
*    DRRY(*)    = y-value, resolution function, modified
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
*
      Dimension  DXH(PDATA),DYH(PDATA),DYH1(PDATA),DYAV(PDATA)
*
*
*:  increase number of data points by factor of 2
*:  create increasing x-values
*
      KRD9 = Min0(PDATA,2*KRD)
      Call BINEQ1(DRRX,DRRY,DXH,DYH,KRD,KRD9,K0,1)
      KRD = KRD9
*
*: save info to common area for printout in gfit
*
      DRESCL = DXH(1)
      DRESCR = DXH(KRD)
*
      DBRF = 0.D0
      If(BPROC) Then
*
*:  peak value
*
       KPKRF = 1
       DPKRF = DYH(1)
       Do I = 2,KRD
        If(DYH(I).gt.DPKRF) Then
         DPKRF = DYH(I)
         KPKRF = I
        Endif
       EndDo
*
*:  background level
*
       KRDN  = KRD/4
       DBRFN = 0.D0
       Do  I = 1,KRDN
        DBRFN = DBRFN + DYH(I)
       EndDo
       DBRFN = DBRFN/Dfloat(KRDN)
*
       KRDP  = KRD/4
       DBRFP = 0.D0
       Do  I = KRD-KRDP+1,KRD
        DBRFP = DBRFP + DYH(I)
       EndDo
       DBRFP = DBRFP/Dfloat(KRDP)
*
       DBRF = Dmin1(DBRFN,DBRFP)
*
*:  smooth tail regions
*
       If(DPROC1.gt.0.D0) Then
        DCRF = DPROC1*(DPKRF-DBRF)
        KRF1 = KPKRF
        Do  I = KPKRF,1,-1
         If(DYH(I)-DBRF.lt.DCRF) Then
          KRF1 = I
          Exit
         Endif
        EndDo
*
        KRF2 = KPKRF
        Do  I = KPKRF,KRD
         If(DYH(I)-DBRF.lt.DCRF) Then
          KRF2 = I
          Exit
         Endif
        EndDo
*
        Do  I = 1,KRD
         If(I.gt.KRF1 .and. I.lt.KRF2) Then
          DYH1(I) = 0.5D0*(DYH(KRF1) + DYH(KRF2))
         Else
          DYH1(I) = DYH(I)
         Endif
        EndDo
*
        Call DRUNAV(DYH1,DYAV,KRD,51)
*
        Do I = 1,KRD
         If(I.gt.KRF1 .and. I.lt.KRF2) Then
          DYH(I) = DYH(I)
         Else
          DYH(I) = DYAV(I)
         Endif
        EndDo
       Endif
*
*:  subtract background
*
       Do  I = 1,KRD
        DYH(I) = DYH(I) - DBRF
       EndDo
*
*:  tail cutoff
*
       If(DPROC2.lt.0.D0) Then
        DPROC2 = Dmax1(3.D0,2.D0*DBRF)/(DPKRF - DBRF)
       Endif
*
       DBRFC = DPROC2*(DPKRF - DBRF)
*
*:  find cut-offs
*
       ICUTL = 1
       Do  I = KPKRF,1,-1
        If(DYH(I).lt.DBRFC) Then
         ICUTL = I+1
         Exit
        Endif
       EndDo
*
       ICUTR = KRD
       Do  I = KPKRF,KRD
        If(DYH(I).lt.DBRFC) Then
         ICUTR = I-1
         Exit
        Endif
       EndDo
*
*:  shuffle data
*
       II = 0
       Do  I = ICUTL,ICUTR
        II = II+1
        DXH(II) = DXH(I)
        DYH(II) = DYH(I)
       EndDo
       KRD = II
*
*: save info to common area for printout in gfit
*
       DRESCL = DXH(ICUTL)
       DRESCR = DXH(ICUTR)
      Endif
*
*:  normalize to maximum y-value
*
      DYMAX = DNORM1(DYH,KRD,KMAX,3)
*
*: save info to common area for printout in gfit
*
      DRESMX = DYMAX
      DRESBK = DBRF
*
*:  find FWHM of resolution function
*
      DW1 = 0.D0
      Do  I = KMAX+1,KRD
       If(DYH(I).lt.0.5D0) Then
        DF  = (0.5D0 - DYH(I-1))/(DYH(I) - DYH(I-1))
        DW1 = DF*DXH(I) + (1.D0-DF)*DXH(I-1)
        Exit
       Endif
      EndDo
*
      DW2 = 0.D0
      Do  I = KMAX-1,1,-1
       If(DYH(I).lt.0.5D0) Then
        DF  = (0.5D0 - DYH(I+1))/(DYH(I) - DYH(I+1))
        DW2 = DF*DXH(I) + (1.D0-DF)*DXH(I+1)
        Exit
       Endif
      EndDo
*
      DFWHM = Dabs(DW1 - DW2)
*
*:  scale x-values to FWHM,
*:  stretch negative and positive parts of resolution function
*
      DFP = 2.D0/(1.D0+DASM)
      DFM = DASM*DFP
*
      Do  I = KMAX-1,1,-1
       DXH(I) = (DFM*(DXH(I) - DXH(KMAX)) + DXH(KMAX))/DFWHM
      EndDo
*
      Do  I = KMAX+1,KRD
       DXH(I) = (DFP*(DXH(I) - DXH(KMAX)) + DXH(KMAX))/DFWHM
      EndDo
*
      DXH(KMAX) = DXH(KMAX)/DFWHM
*
*:  create equidistant binning
*
      Call BINEQ1(DXH,DYH,DRRX,DRRY,KRD,KRD,K0,3)
*
*:  calculate first moment of normalized res. function
*
      DM0 = 0.D0
      DM1 = 0.D0
      Do  I = 1,KRD
       DM0 = DM0 + DRRY(I)
       DM1 = DM1 + DRRY(I)*DRRX(I)
      EndDo
*
*
      End
