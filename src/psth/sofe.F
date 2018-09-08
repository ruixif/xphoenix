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
      Subroutine SOFE(DENG1L,DENG2L,NENGL)
*
*
* purpose :  calculate phonon excitation probabilities from
*            the phonon density of states
* date    :  December 20, 2010
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DTMP1      = temperature
*    DFLM(1)    = Lamb-Moessbauer factor for DTMP1
*    DENG1      = begin of energy range (meV)
*    DENG2      = end of energy range (meV)
*    NENG       = number of energy points
*    KD         = number of data points
*    DMX(*)     = energy scale
*    DMY(*)     = phonon density of states
*    DMDY(*)    = error^2 of phonon density of states
*
* export variables :
*    JMAX       = highest-order of phonon contributions
*    DSX(*)     = energy scale
*    DSY(*,0)   = phonon excitation probability
*    DSY(*,J)   = J-th order phonon contribution
*    DENG1L     = begin of energy range (meV), corrected
*    DENG2L     = end of energy range (meV), corrected
*    NENGL      = number of energy points, corrected
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<psth.h>
*
      DIMENSION   DGF(PFFT),DRF(PFFT),EFT0(PFFT),EFT1(PFFT),
     +            EFTN(PFFT),EFTR(PFFT),NWORK(PFFT),DWORK(PFFT),
     +            DHY(PFFT,0:PORDER),DH1(2),DH2(2),DP(PFIT)
*
*
      NERR = 0
*
      DBETA = 1.D0/(DBLTZ*DTMP1)
      DFLML = DFLM(1)
*
*:  binary exponent for FFT
*
      DLNFLM = -Dlog(DFLML)
      DSINT  = DFLML/(1.D0-DFLML)
      J1 = Idnint(0.5D0*(Dsqrt(1.D0+4.D0*DLNFLM**2)-1.D0))
      J1 = Max0(1,J1)
      JMAX = 0
      Do  J = 1,PORDER
       JMAX = J
       DSINT = DSINT*DLNFLM/Dfloat(J)
       If(J.ge.J1 .and. DSINT.le.1.D-3) Exit
      EndDo
*
      NEXP  = Idint(Dlog(Dfloat(KD*JMAX))/Dlog(2.D0)) + 1
      KDF   = 2**NEXP
*
      If(KDF.gt.PFFT) Then
       KDF  = PFFT
       NEXP = Idnint(Dlog(Dfloat(PFFT))/Dlog(2.D0))
      Endif
      KDF2 = KDF/2
*
*:  generating function; assume DMX(1)=0
*
      DGRSUM = 0.D0
      Do 2  K = 2,KDF2
       If(K.le.KD) Then
        DE = Dexp(-DBETA*DMX(K))
        DF = DMX(K)*(1.D0 - DE)/DECOIL
        DGF(K) = DMY(K)/DF
        DGRSUM = DGRSUM + DGF(K)
       Else
        DGF(K) = 0.D0
       Endif
    2 Continue
*
      Do 3  K = 2,KDF2+1
       If(K.le.KD) Then
        DE = Dexp(-DBETA*DMX(K))
        DGF(KDF-K+2) = DE*DGF(K)
        DGRSUM = DGRSUM + DGF(KDF-K+2)
       Else
        DGF(KDF-K+2) = 0.D0
       Endif
    3 Continue
*
*:  plug the hole at E=0 which is necessary for
*:   proper integrals of n-phonon contributions
*
      DGF(1) = 0.5D0*(DGF(2) + DGF(KDF))
*
*:  Fourier transform of generating function
*
*:  Fourier transformation
*:   f(y) = Pi/2 * Sum( g(X)*exp(-i*x*y)dx )
*:  Note also
*:   Sum( |f(y)|**2 dy ) = Sum( |g(x)|**2 dx ).
*
      Do 4  K = 1,KDF
       EFT0(K) = Dcmplx(DGF(K),0.D0)
    4 Continue
*
      IMODE = 0
      CALL EFFT(EFT0,NEXP,DWORK,NWORK,IMODE)
*
*:  one-phonon contribution
*
      KDF2 = KDF/2
      DDE  = DMX(2) - DMX(1)
      DSCALE = 1.D3*DFLML/DDE
      Do 5  K = 1,KDF2
       DSY(K,1)      = DSCALE*DGF(K+KDF2)
       DSY(KDF2+K,1) = DSCALE*DGF(K)
       DSX(K)        = Dfloat(K-KDF2-1)*DDE
       DSX(KDF2+K)   = Dfloat(K-1)*DDE
       EFTN(K)      = EFT0(K)
       EFTN(K+KDF2) = EFT0(K+KDF2)
    5 Continue
*
*:  higher-order phonon contributions
*
      IMODE = 1
      DFF = Dsqrt(Dfloat(KDF))
      Do 6  J = 2,JMAX
       Do 7  K = 1,KDF
        EFTN(K) = DFF*EFTN(K)*EFT0(K)/Dfloat(J)
        EFT1(K) = EFTN(K)
    7  Continue
*
       Call EFFT(EFT1,NEXP,DWORK,NWORK,IMODE)
*
       Do 8  K = 1,KDF2
        DSY(K,J)      = DSCALE*Dreal(EFT1(K+KDF2))
        DSY(KDF2+K,J) = DSCALE*Dreal(EFT1(K))
    8  Continue
    6 Continue
*
*:  define resolution function
*
      If(DFWHM.lt.0.01D0) Then
       Do 20 K = 1,KDF
        EFTR(K) = (1.D0,0.D0)
   20  Continue
      Else
       BREXT = .False.
       DP(1) = 1.D0
       DP(2) = DLN2*(2.D0/DFWHM)**DSHP
       DP(3) = 0.D0
       DP(6) = DASM
       DP(7) = DSHP
*
       DDX = Dabs(DMX(2)-DMX(1))
       Do 21  K = 1,KDF/2
        DX = Dfloat(K-1)*DDX
        DRF(K) = DTH(-1,DX,0.D0,DP,DH1,DH2,0,BREXT)
   21  Continue
*
       Do 22  K = 1,KDF/2-1
        DX = -Dfloat(K)*DDX
        DRF(KDF-K+1) = DTH(-1,DX,0.D0,DP,DH1,DH2,0,BREXT)
   22  Continue
*
*:  normalize
*
       DNR = Dnorm1(DRF,KDF,N,5)
*
*:  Fourier transform of resolution function
*
       DN = Dsqrt(Dfloat(KDF))
       Do 23  K = 1,KDF
        EFTR(K) = Dcmplx(DN*DRF(K),0.D0)
   23  Continue
*
       IMODE = 0
       CALL EFFT(EFTR,NEXP,DWORK,NWORK,IMODE)
      Endif
*
*:  all phonon contributions
*
      Do 9  K = 1,KDF
       EFT1(K) = EFTR(K)*(Cdexp(DFF*EFT0(K)) - 1.D0)/DFF
    9 Continue
*
      IMODE = 1
      Call EFFT(EFT1,NEXP,DWORK,NWORK,IMODE)
*
      DSYMAX = -1.D99
      Do 10  K = 1,KDF2
       DSY(K,0)      = DSCALE*Dreal(EFT1(K+KDF2))
       DSY(KDF2+K,0) = DSCALE*Dreal(EFT1(K))
       DSYMAX = Dmax1(DSYMAX,DSY(KDF2+K,0))
   10 Continue
*
*:  determine relevant energy range
*
      If(DENG1.eq.DENG2 .or. NENG.le.0) Then
       KCUT = KDF2-1
       DCUT = 1.D-6*DSYMAX
       Do 11  K = KDF2+1+KD,KDF
        If(DSY(K,0).le.DCUT) Then
         KCUT = K-KDF2-1
         GoTo 101
        Endif
   11  Continue
  101  Continue
*
*:  shuffle values;energy scale
*
       DDE = DMX(2) - DMX(1)
       DE0 = -DDE*Dfloat(KDF2)
       K1  = KDF2-KCUT+1
       K2  = KDF2+KCUT+1
       KK  = 0
       Do 12  K = K1,K2
        KK = KK+1
        Do 13  J = 0,JMAX
         DSY(KK,J) = DSY(K,J)
   13   Continue
        DSX(KK) = DE0 + Dfloat(K-1)*DDE
   12  Continue
       NENGL = KK
      Else
*
*:  define energy range
*
       DDME = DMX(2) - DMX(1)
       DE1  = -DDME*Dfloat(KDF2)
       DE2  =  DDME*Dfloat(KDF2)
*
       BCH = .False.
       DDE = (DENG2-DENG1)/Dfloat(NENG-1)
*
       If(DENG1.lt.DE1) Then
        DENG1L = DE1
        BCH    = .True.
       Else
        DENG1L = DENG1
       Endif
*
       If(DENG2.gt.DE2) Then
        DENG2L = DE2
        BCH    = .True.
       Else
        DENG2L = DENG2
       Endif
*
       NENGL  = Idnint((DENG2L-DENG1L)/DDE) + 1
       If(NENGL.gt.PFFT) Then
        DF     = Dabs(DENG1L)/(DENG2L-DENG1L)
        DENG1L = DENG1L +        DF*Dfloat(NENGL-PFFT)*DDE
        DENG2L = DENG2L - (1.D0-DF)*Dfloat(NENGL-PFFT)*DDE
        NENGL  = Idnint((DENG2L-DENG1L)/DDE) + 1
        BCH    = .True.
       Endif
       DENG2L = DENG1L + Dfloat(NENGL-1)*DDE
*
       If(BCH) Then
        NERR  = 3701
        Call ERRCHK(NERR,*201)
  201   Continue
       Endif
*
*:  interpolation
*
       Do 14  K = 1,NENGL
        DSX(K) = DENG1L + Dfloat(K-1)*DDE
        KK  = Idnint((DSX(K) - DE1)/DDME)
        KK1 = Max0(1,KK)
        KK2 = Min0(KDF,KK+1)
        DME = DE1 + Dfloat(KK-1)*DDME
        Do 15  J = 0,JMAX
         DHY(K,J) = DSPLL(DSX(K),DME,     DSY(KK1,J),
     +                           DME+DDME,DSY(KK2,J) )
   15   Continue
   14  Continue
*
       Do 16  J = 0,JMAX
        Do 17  K = 1,NENGL
         DSY(K,J) = DHY(K,J)
   17   Continue
   16  Continue
      Endif
*
*:  generate elastic peak
*
      If(DELPST.gt.0.D0) Then
       BREXT = .False.
       DP(1) = 1.D0
       DP(2) = DLN2*(2.D0/DFWHM)**DSHP
       DP(3) = 0.D0
       DP(6) = DASM
       DP(7) = DSHP
*
       DNR = 0.D0
       DNS = 0.D0
       Do 18  K = 1,NENGL
        DRF(K) = DTH(-1,DSX(K),0.D0,DP,DH1,DH2,0,BREXT)
        DNR = DNR + DRF(K)
        DNS = DNS + DSY(K,0)
   18  Continue
*
       DFAC = DELPST* DNS/DNR * DFLML/(1.D0-DFLML)
       Do 19  K = 1,NENGL
        DSYP(K) = DSY(K,0) + DFAC*DRF(K)
   19  Continue
      Endif
*
*
      End
