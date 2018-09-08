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
      SUBROUTINE DOSFTS
*
*
* purpose :  calculate the Greens function from the
*             phonon density of states
* date    :  March 26, 2011
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DMX(*)     = energy scale
*    DMY(*)     = phonon density of states
*    DMDY(*)    = phonon density of states errors^2
*    KD         = number of data points
*
* export variables :
*    DGFX(*)    = time scale in (ps)
*    DGFY(*)    = Greens function in (pm)
*    DGFDY(*)   = Greens function errors (pm)
*    KDG        = number of function points
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<psth.h>
*
      DIMENSION   EFT1(PFFT),EFT2(PFFT),NWORK(PFFT),DWORK(PFFT)
*
*
*:  binary exponent for FFT
*
      NEXP  = Idint(Dlog(Dfloat(KD))/Dlog(2.D0)) + 3
      KDF   = 2**NEXP
*
      If(KDF.gt.PFFT) Then
       KDF  = PFFT
       NEXP = Idnint(Dlog(Dfloat(PFFT))/Dlog(2.D0))
      Endif
*
*:   fill array, padd with zeroes
*:   EFT1(*)  density of states divided by energy
*:   EFT2(*)  DOS errors^2 divided by energy^2
*:   assume DMX(1)=0
*
      EFT1(1) = (0.D0,0.D0)
      EFT2(1) = (0.D0,0.D0)
      Do 1  I = 2,KD
       EFT1(I) = Dcmplx(DMY(I)/DMX(I),0.D0)
       EFT2(I) = Dcmplx(DMDY(I)/DMX(I)**2,0.D0)
    1 Continue
*
      Do 2  I = KD+1,KDF
       EFT1(I) = (0.D0,0.D0)
       EFT2(I) = (0.D0,0.D0)
    2 Continue
*
*:  Fourier transformation
*:   f(y) = Pi/2 * Sum( g(X)*exp(-i*x*y)dx )
*:  Note also
*:   Sum( |f(y)|**2 dy ) = Sum( |g(x)|**2 dx ).
*
      IMODE = 0
      Call EFFT(EFT1,NEXP,DWORK,NWORK,IMODE)
      Call EFFT(EFT2,NEXP,DWORK,NWORK,IMODE)
*
*:  time axis in ps
*
      KDG = Min0(KD,KDF/2)
      DDT = D2PI*DHQUER*1.D15/((DMX(2)-DMX(1))*Dfloat(KDF))
      DN  = 6.D9*DHQUER*DCLIC*DECOIL/DGAMMA*Dsqrt(Dfloat(KDF))
      Do 3  I = 1,KDG
       DGFX(I)  = Dfloat(I-1)*DDT
       DGFY(I)  = DN*Dreal(EFT1(I))
       DGFDY(I) = DN*Dreal(Cdsqrt(EFT2(2*I)))
    3 Continue
*
*
      End
