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
      Subroutine FTS(NIT,NITE)
*
*
* purpose :  decomposition of inelastic absorption probability
*            into 1,2,3-phonon contributions;
*            the resolution function is used to deconvolute the
*            data before decomposition takes place;
*            after decomposition data is convoluted with
*            resolution function if requested.
* date    :  January 7, 1995
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    NIT        = if 0 first iteration
*    NITE       = total number of iterations
*    KPD1       = x-scale index for zero energy, i.e., DSX(KPD1)=0
*    DSX(*)     = energy scale
*    DRY(*)     = resolution function, normalized by area=1
*    DSY(*)     = absorption probability, elastic peak removed
*    DSDY(*)    = statistical errors of DSY(*) squared
*    DFFAC      = Lamb-MB factor
*
* export variables :
*    DMSR(*)     = correction factors of moments DMS(*)
*    DCNY(*,1)   = one phonon contribution
*    DCNY(*,2)   = two phonon contribution
*    DCNY(*,3)   = three+ phonon contribution
*    DCOVAR(*,*) = covariance matrix of DCNY(*,1)
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
*
      Dimension   DCTOFF(PFFT),EFT0(PFFT),EFT1(PFFT),
     +            EFT2(PFFT),EFT3(PFFT),EFT4(PFFT),
     +            NWORK(PFFT),DWORK(PFFT)
*
*
*:  binary exponent for FFT
*
      KDEFF = MAX0(KD,2*KPD1+1,2*(KD-KPD1)+1)
      NEXP  = Ifix(ALOG(FLOAT(KDEFF))/ALOG(2.)) + 1
      KDF   = 2**NEXP
*
      If(DFLOAT(KDF-KD).LT.0.4D0*DFLOAT(KDEFF)) Then
       NEXP = NEXP + 1
       KDF  = 2*KDF
      Endif
*
      IF(KDF.GT.PFFT) Then
       KDF  = PFFT
       NEXP = NINT(ALOG(FLOAT(PFFT))/ALOG(2.))
      Endif
*
*:   fill arrays, expanded by estimated multiphonon contributions
*
*:   EFT0(*)  resolution function, normalized
*:   EFT1(*)  scaled data without elastic peak divided by f-factor
*:   EFT4(*)  Gaussian with FWHM=DIFL for smoothing
*
      Do  I = 1,KDF/2
       II = I+KPD1-1
       If(I.LE.KD-KPD1+1) Then
        EFT0(I) = DCMPLX(DRY(II),0.D0)
        EFT1(I) = DCMPLX(DSY(II)/DFFAC,0.D0)
*
        If(DIFL.NE.0.D0) Then
         DXX = 4.D0*DLN2*(DSX(II)/DIFL)**2
         If(DXX.LT.20.D0) Then
          EFT4(I) = DCMPLX(DEXP(-DXX),0.D0)
         Else
          EFT4(I) = (0.D0,0.D0)
         Endif
        ElseIf(I.EQ.1) Then
          EFT4(I) = (1.D0,0.D0)
         Else
          EFT4(I) = (0.D0,0.D0)
        Endif
       Else
        EFT0(I) = (0.D0,0.D0)
        EFT4(I) = (0.D0,0.D0)
*
*:  estimate the distant multiphonon contributions
*
        If(NIT.EQ.0) Then
         EFT1(I) = (0.D0,0.D0)
        Else
         II = I + KDF/2
         EE = EFT2(II) + EFT3(II)
         Do  J = 1,9
          EE = EE + EFT2(II+J) + EFT3(II+J)
         EndDo
         EFT1(I) = 0.1D0*EE
        Endif
       Endif
      EndDo
*
      Do  I = KDF/2+1,KDF
       II = I-KDF+KPD1-1
       If(II.GE.1) Then
        EFT0(I) = DCMPLX(DRY(II),0.D0)
        EFT1(I) = DCMPLX(DSY(II)/DFFAC,0.D0)
*
        If(DIFL.NE.0.D0) Then
         DXX = 4.D0*DLN2*(DSX(II)/DIFL)**2
         If(DXX.LT.20.D0) Then
          EFT4(I) = DCMPLX(Dexp(-DXX),0.D0)
         Else
          EFT4(I) = (0.D0,0.D0)
         Endif
        Else
         EFT4(I) = (0.D0,0.D0)
        Endif
       Else
        EFT0(I) = (0.D0,0.D0)
        EFT4(I) = (0.D0,0.D0)
*
*:  estimate the distant multiphonon contributions
*
        If(NIT.eq.0) Then
         EFT1(I) = (0.D0,0.D0)
        Else
         II = I - KDF/2
         EE = EFT2(II) + EFT3(II)
         Do  J = 1,9
          EE = EE + EFT2(II+J) + EFT3(II+J)
         EndDo
         EFT1(I) = 0.1D0*EE
        Endif
       Endif
      EndDo
*
*:  Fourier transformation
*:   f(y) = Pi/2 * Sum( g(X)*exp(-i*x*y)dx )
*:  Note also
*:   Sum( |f(y)|**2 dy ) = Sum( |g(x)|**2 dx ).
*
      IMODE = 0
      Call EFFT(EFT0,NEXP,DWORK,NWORK,IMODE)
      Call EFFT(EFT1,NEXP,DWORK,NWORK,IMODE)
      Call EFFT(EFT4,NEXP,DWORK,NWORK,IMODE)
*
*:  define cut-off function to
*:  manipulate the FT of the data
*
*:  reduce the high frequency components according
*:   to the rule that the physical meaningful part
*:   in the data-FT has to drop faster than the FT
*:   of the resolution function
*
      Do  I = 1,KDF
       D0 = Cdabs(EFT0(I)/EFT0(1))
       D1 = Cdabs(EFT1(I)/EFT1(1))
       DCTOFF(I) = Dmin1(D0/D1,1.D0)
      EndDo
*
*:  decompose and deconvolute data
*
      DFAC = 1.D0/DSQRT(DFLOAT(KDF))
*
*
*:  DIFL is a reduction factor to suppress
*:   random high frequency components
*
      If(BDCNV) Then
       KPAD = Idnint(DFLOAT(KDF/2)*DIFL)
       Do  I = 1,KDF
        If(I.ge.KPAD+1 .and. I.le.KDF-KPAD) Then
         EFT0(I) = (0.D0,0.D0)
         EFT1(I) = (0.D0,0.D0)
         EFT2(I) = (0.D0,0.D0)
         EFT3(I) = (0.D0,0.D0)
        Else
*
         EE0     = EFT1(I)/DFAC
         EFT1(I) = EFT1(I)*DCTOFF(I)
         EE      = EFT1(I)/EFT0(I)
*
*:  1-phonon contribution
*
         EFT1(I) = CDLOG(1.D0 + EE)
*
*:  2-phonon contribution
*
         EFT2(I) = 0.5D0*EFT1(I)*EFT1(I)
*
*:  higher order phonon contributions
*
         EFT3(I) = EE - EFT1(I) - EFT2(I)
*
*:  derivative of S_1
*
         EFT0(I) = 1.D0/(1.D0 + EE0)
*
*:  apply filter which is a parabola of order NF
*
         If(I.lt.KPAD) Then
          DX = DFLOAT(I-1)/DFLOAT(KPAD)
         Else
          DX = DFLOAT(KDF-I+1)/DFLOAT(KPAD)
         Endif
*
         DF = DFLTR(0,DX,NF)*DFAC
         EFT0(I) = DF*EFT0(I)
         EFT1(I) = DF*EFT1(I)
         EFT2(I) = DF*EFT2(I)
         EFT3(I) = DF*EFT3(I)
        Endif
       EndDo
      Else
*
*:  smoothing factors
*
       DSM1 = 1.D0
       DSM2 = 2.D0
       DSM3 = 3.D0
       ENFC = 1.D0/EFT4(1)
*
*:  decomposition but no deconvolution
*
       Do  I = 1,KDF
        EE0     = EFT1(I)/DFAC
        EFT1(I) = EFT1(I)*DCTOFF(I)
        EE      = EFT1(I)/EFT0(I)
*
*:  1-phonon contribution, S_1
*
        EFT1(I) = Cdlog(1.D0 + EE)
*
*:  2-phonon contribution, S_2
*
        EFT2(I) = 0.5D0*EFT1(I)*EFT1(I)
*
*:  higher order phonon contributions
*
        EFT3(I) = EE - EFT1(I) - EFT2(I)
*
*:  undo deconvolution, optionally smooth with Gaussian
*
        EE1 = ENFC*EFT4(I)
*
        EFT1(I) = EFT1(I)*EFT0(I)*EE1**DSM1
        EFT2(I) = EFT2(I)*EFT0(I)*EE1**DSM2
        EFT3(I) = EFT3(I)*EFT0(I)*EE1**DSM3
*
*:  derivative of S_1
*
        EFT0(I) = 1.D0/(1.D0 + EE0)
       EndDo
      Endif
*
*
*:  inverse Fourier transformation
*
      IMODE = 1
      Call EFFT(EFT0,NEXP,DWORK,NWORK,IMODE)
      Call EFFT(EFT1,NEXP,DWORK,NWORK,IMODE)
      Call EFFT(EFT2,NEXP,DWORK,NWORK,IMODE)
      Call EFFT(EFT3,NEXP,DWORK,NWORK,IMODE)
*
*:  center arrays
*
      Do  I = 1, KDF/2
       II       = KDF/2 + I
       EF       = EFT0(I)*DFAC
       EFT0(I)  = EFT0(II)*DFAC
       EFT0(II) = EF
       EF       = EFT1(I)
       EFT1(I)  = EFT1(II)
       EFT1(II) = EF
       EF       = EFT2(I)
       EFT2(I)  = EFT2(II)
       EFT2(II) = EF
       EF       = EFT3(I)
       EFT3(I)  = EFT3(II)
       EFT3(II) = EF
      EndDo
*
*:  calculate correction factors DMSR(*) of moments DMS(*), i.e.,
*:   the refined value of the Nth moment is given by DMS(N)*DMSR(N)
*
      M = 3
      If(DFFAC.gt.0.1D0) Then
       KBG = KDF/2 - KPD1
       KED = KBG + KD
       DDE = DSX(2) - DSX(1)
       Do  J = 0,M
        DMSR(J) = 0.D0
        DN      = 0.D0
        Do  I = 1,KDF
         If(I.LE.KBG) Then
          DE = DSX(1) + DFLOAT(I-KBG)*DDE
          DMSR(J) = DMSR(J) + DREAL(EFT2(I)+EFT3(I))*DE**J
          DN      = DN      + DREAL(EFT1(I)+EFT2(I)+EFT3(I))*DE**J
          Cycle
         Endif
         If(I.GE.KED+1) Then
          DE = DSX(KD) + DFLOAT(I-KED)*DDE
          DMSR(J) = DMSR(J) + DREAL(EFT2(I)+EFT3(I))*DE**J
          DN      = DN      + DREAL(EFT1(I)+EFT2(I)+EFT3(I))*DE**J
          Cycle
         Endif
         DN = DN + DREAL(EFT1(I)+EFT2(I)+EFT3(I))*DSX(I-KBG)**J
        EndDo
        If(J.EQ.0) DN = DN/(1.D0 - DFFAC)
        DMSR(J) = 1.D0 + DMSR(J)/DN
       EndDo
      Else
       Do  J = 0,M
        DMSR(J) = 1.D0
       EndDo
      Endif
*
*:  save results
*
      If(NIT.eq.NITE) Then
       KOFF = KDF/2 - KPD1 + 1
       Do  I = 1,KD
        DCNY(I,1) = DREAL(EFT1(I+KOFF))
        DCNY(I,2) = DREAL(EFT2(I+KOFF))
        DCNY(I,3) = DREAL(EFT3(I+KOFF))
       EndDo
      Endif
*
*:  calculate covariance matrix of S_1 for E>0
*
      If(NIT.eq.0) Then
       KOFF = KDF/2 + 1
       Do  I = KPD1,KD
        II = I - KPD1 + 1
        Do  J = KPD1,I
         JJ = J - KPD1 + 1
         DCOVAR(II,JJ) = 0.D0
         Do  K = 1,KD
          KI = KOFF + I - K
          If(KI.lt.1 .or. KI.gt.KDF) Cycle
*
          KJ = KOFF + J - K
          If(KJ.lt.1 .or. KJ.gt.KDF) Cycle
*
          DCOVAR(II,JJ) = DCOVAR(II,JJ) + DSDY(K)*
     +                      Dreal(EFT0(KI))*Dreal(EFT0(KJ))
         EndDo
        EndDo
       EndDo
      Endif
*
*
      End
