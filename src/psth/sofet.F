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
      Subroutine SOFET(DENG1L,DENG2L,NENGL)
*
*
* purpose :  calculate phonon excitation probabilities from
*            the phonon density of states for many temperatures
* date    :  February 26, 2013
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DTMP1      = begin of temperature range (K)
*    DTMP2      = end of temperature range (K)
*    NTMP       = number of temperature points
*    DFLM(*)    = Lamb-Moessbauer factors
*    DENG1      = begin of energy range (meV)
*    DENG2      = end of energy range (meV)
*    NENG       = number of energy points
*    KD         = number of data points
*    DMX(*)     = energy scale (meV)
*    DMY(*)     = phonon density of states
*    KDF        = nr. of data points for FFT
*    NEXP       = lb(KDF)
*    KCUT       = cutoff for energy index
*
* export variables :
*    DSY(*,*)   = phonon excitation probability functions
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
     +            EFTR(PFFT),NWORK(PFFT),DWORK(PFFT),
     +            DHY(PFFT),DH1(2),DH2(2),DP(PFIT)
*
*
      KDF2 = KDF/2
      DFF  = Dsqrt(Dfloat(KDF))
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
       Do 21  K = 1,KDF2
        DX = Dfloat(K-1)*DDX
        DRF(K) = DTH(-1,DX,0.D0,DP,DH1,DH2,0,BREXT)
   21  Continue
*
       Do 22  K = 1,KDF2-1
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
*
*:  generating function; assume DMX(1)=0
*
      DDTMP = (DTMP2-DTMP1)/Dfloat(NTMP-1)
      Do 2  N = 1,NTMP-1
       DTMP  = DTMP1 + Dfloat(N)*DDTMP
       DBETA = 1.D0/(DBLTZ*DTMP)
*
       DGRSUM = 0.D0
       Do 3  K = 2,KDF2
        If(K.le.KD) Then
         DE = Dexp(-DBETA*DMX(K))
         DF = DMX(K)*(1.D0 - DE)/DECOIL
         DGF(K) = DMY(K)/DF
         DGRSUM = DGRSUM + DGF(K)
        Else
         DGF(K) = 0.D0
        Endif
    3  Continue
*
       Do 4  K = 2,KDF2+1
        If(K.le.KD) Then
         DE = Dexp(-DBETA*DMX(K))
         DGF(KDF-K+2) = DE*DGF(K)
         DGRSUM = DGRSUM + DGF(KDF-K+2)
        Else
         DGF(KDF-K+2) = 0.D0
        Endif
    4  Continue
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
       Do 5  K = 1,KDF
        EFT0(K) = Dcmplx(DGF(K),0.D0)
    5  Continue
*
       IMODE = 0
       Call EFFT(EFT0,NEXP,DWORK,NWORK,IMODE)
*
*:  all phonon contributions
*
       Do 6  K = 1,KDF
        EFT1(K) = EFTR(K)*(Cdexp(DFF*EFT0(K)) - 1.D0)/DFF
    6  Continue
*
       IMODE = 1
       Call EFFT(EFT1,NEXP,DWORK,NWORK,IMODE)
*
       DSCALE = 1.D3*DFLM(N+1)/(DMX(2)-DMX(1))
       Do 7  K = 1,KDF2
        DSYT(K,N)      = DSCALE*Dreal(EFT1(K+KDF2))
        DSYT(KDF2+K,N) = DSCALE*Dreal(EFT1(K))
    7  Continue
*
*:  determine relevant energy range
*
       If(DENG1.eq.DENG2 .or. NENG.le.0) Then
*
*:  shuffle values;energy scale
*
        DDE = DMX(2) - DMX(1)
        DE0 = -DDE*Dfloat(KDF2)
        K1  = KDF2-KCUT+1
        K2  = KDF2+KCUT+1
        KK  = 0
        Do 8  K = K1,K2
         KK = KK+1
         DSYT(KK,N) = DSYT(K,N)
    8   Continue
        NENGL = KK
       Else
*
*:  define energy range
*
        DDME = DMX(2) - DMX(1)
        DE1  = -DDME*Dfloat(KDF2)
        DE2  =  DDME*Dfloat(KDF2)
*
        DDE = (DENG2-DENG1)/Dfloat(NENG-1)
        If(DENG1.lt.DE1) Then
         DENG1L = DE1
        Else
         DENG1L = DENG1
        Endif
*
        If(DENG2.gt.DE2) Then
         DENG2L = DE2
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
        Endif
        DENG2L = DENG1L + Dfloat(NENGL-1)*DDE
*
*:  interpolation
*
        Do 9  K = 1,NENGL
         KK  = Idnint((DSX(K) - DE1)/DDME)
         KK1 = Max0(1,KK)
         KK2 = Min0(KDF,KK+1)
         DME = DE1 + Dfloat(KK-1)*DDME
         DHY(K) = DSPLL(DSX(K),DME,     DSYT(KK1,N),
     +                         DME+DDME,DSYT(KK2,N) )
    9   Continue
*
        Do 10  K = 1,NENGL
         DSYT(K,N) = DHY(K)
   10   Continue
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
        Do 11  K = 1,NENGL
         DRF(K) = DTH(-1,DSX(K),0.D0,DP,DH1,DH2,0,BREXT)
         DNR = DNR + DRF(K)
         DNS = DNS + DSYT(K,N)
   11   Continue
*
        DFAC = DELPST* DNS/DNR * DFLM(N+1)/(1.D0-DFLM(N+1))
        Do 12  K = 1,NENGL
         DSYT(K,N) = DSYT(K,N) + DFAC*DRF(K)
   12   Continue
       Endif
*
    2 Continue
*
*
      End
