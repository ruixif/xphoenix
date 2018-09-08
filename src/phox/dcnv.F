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
      Subroutine DCNV
*
* purpose :  decompose data that was measured
*            with a certain resolution function;
*            the FourierLog formalism is used.
* date    :  January 7, 1995
* author  :  Wolfgang Sturhahn                                          
*
*
* import variables :
*    KDBP       = x-scale index for DRY(*)<DBLIM*DSY(*)
*    KD         = nr. of data points
*    DSX(*)     = energy scale
*    DSY(*)     = absorption probability, elastic peak removed
*    DSDY(*)    = statistical errors of DSY(*) squared
*    DRY(*)     = resolution function
*    DFFAC      = Lamb-MB factor
*    DEKIN      = kinetic energy per atom
*    DMPHE      = mean phonon energy
*
* export variables :
*    KPD1        = x-scale index for zero energy, i.e., DSX(KPD1)=0
*    DSX(*)      = shifted energy scale, i.e., DSX(KPD1)=0 exists
*    DRY(*)      = resolution function, normalized by area=1
*    DCY(*)      = phonon density of states
*    DCDY(*)     = statistical errors of phonon density of states
*    DCOVAR(*,*) = covariance matrix of phonon density of states
*    DCNY(*,1)   = one phonon contribution
*    DCNY(*,2)   = two phonon contribution
*    DCNY(*,3)   = three phonon contribution
*    DFFAC       = refined value of Lamb-Mb factor
*    DEKIN       = refined value of kinetic energy per atom
*    DMPHE       = refined value of mean phonon energy
*    DMSR(N)     = correction factor of moment DMS(N)
*                                                                       
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*                                                                       
#include<phoenix.h>
#include<phox.h>
#include<xterm.h>
*
      Dimension  DH(PDATA),DH1(PDATA),DCO(PDATA,PDATA),
     +           DVC(PDATA,PDATA),DWORK(PDATA,PDATA)
*
*:  bin data on a x-scale that contains x=0
*
      Call BINEQ1(DSX,DSY,DMX,DMY,KD,KD,KPD1,2)
      Call DCOPY1(DMY,DSY,KD)
*
      Call BINEQ1(DSX,DSDY,DMX,DMY,KD,KD,KPD1,6)
      Call DCOPY1(DMY,DSDY,KD)
*
      Call BINEQ1(DSX,DRY,DMX,DMY,KD,KD,KPD1,2)
      Call DCOPY1(DMY,DRY,KD)
      Call DCOPY1(DMX,DSX,KD)
*
*:  area-normalize the resolution function
*
      DN = DNORM1(DRY,KD,K,5)
*
*:  calculate 1,2,3-phonon contributions with FFT algorithm
*:  provide also refinements of the moments DMS(*)
*
*:  this procedure will be reiterated NITE times
*
      NITE = 1
      NIT  = 0
  100 Continue
      Call FTS(NIT,NITE)
*
*:  update data and reiterate only
*:  if the f-factor is sufficiently large
*
      If(DFFAC.gt.0.1D0) Then
       NIT = NIT + 1
*
       If(NIT.LE.NITE) Then
        DFFAC8 = DFFAC
        DFFAC  = 1.D0 - DECOIL*DMS(0)*DMSR(0)/(DMS(1)*DMSR(1))
*
        DNN    = (1.D0 - DFFAC)/(1.D0 - DFFAC8)
        Do K=1,KD
         DSY(K) = DSY(K)*DNN
        EndDo
        GoTo 100
       Endif
      Endif
*
*:  use the refinements of the moments to recalculate
*:   f-factor, kinetic energy, and mean phonon energy
*
      DFFAC8 = DFFAC
      DKIN8  = DKIN
      DMPHE8 = DMPHE
*
*:  reconstruct the area of the elastic peak
*
      DSCLE = DECOIL/(DMSR(1)*DMS(1))
      DPEAK = 1.D0/DSCLE - DMS(0)*DMSR(0)
*
*:  calculate the f-factor
*
      DFFAC  = DPEAK*DSCLE
      DDFFAC = DDFFAC*DFFAC/DFFAC8
*
*:  kinetic energy per MB-atom
*
      DKN   = 0.25/DECOIL
      DKIN  = DMS(2)*DMSR(2) - DMS(1)*DMSR(1)*DECOIL
      DKIN  = DKN*DKIN*DSCLE
      DDKIN = DDKIN*DKIN/DKIN8
*
*:  mean force constant
*
      DEN    = 5.D2*DELCH*(DGAMMA/(DECOIL*DHQUER*DCLIC))**2
      DMPHE  = DMS(3)*DMSR(3) - 3.0*DMS(2)*DMSR(2)*DECOIL +
     +         2.0*DMS(1)*DMSR(1)*DECOIL**2
      DMPHE  = DEN*DMPHE*DSCLE
      DDMPHE = DDMPHE*DMPHE/DMPHE8
*
*:  isotope fractionation, i.e., ln(Beta)*M/DeltaM
*
      DLNB  = 3.D1*(DBETA*DKIN - 0.5D0)
      DDLNB = 3.D1*DBETA*DDKIN
*
*:  high-T approximation for isotope fractionation
*
      DLNBN  = 0.25D-2*DECOIL/DELCH*(DBETA*DHQUER*DCLIC/DGAMMA)**2
      DLNBH  = DLNBN*DMPHE
      DDLNBH = DLNBN*DDMPHE
*
      Write(LUNPTL,2000) '','',
     +              DFFAC,DDFFAC,DKIN,DDKIN,DMPHE,DDMPHE,
     +              DLNB,DDLNB,DLNBH,DDLNBH
      Write(6,2000) XT_MAG,XT_DEF,DFFAC,DDFFAC,DKIN,DDKIN,
     +              DMPHE,DDMPHE,DLNB,DDLNB,DLNBH,DDLNBH
*
 2000 Format(2X,A,'Quantities derived after refinement -->',A/
     + 4X,'Lamb-Moessbauer factor : ',F8.4,'  +-',F8.4/
     + 4X,'kinetic energy / atom  : ',F8.4,'  +-',F8.4,' meV'/
     + 4X,'mean force constant    :', F9.4,'  +-',F8.4,' N/m'/
     + 4X,'isotope fractionation  :', F9.4,'  +-',F8.4,' perMille/%'/
     + 4X,'high T isotope frac.   :', F9.4,'  +-',F8.4,' perMille/%'/)
*
*
*:  calculate partial phonon-DOS in 1/eV/at.vol.
*
*:  at low temperatures
*:   the energy-loss side of the data is ignored
*
      If( Dmax1(-DSX(1),DSX(KD))*DBETA.gt.15.D0 ) Then
       BLT  = .true.
      Else
       BLT  = .false.
      Endif
*
      DH(1)  = 0.D0
      DCY(1) = 0.D0
      DNORM  = 3.D3/(DECOIL*(DSX(2)-DSX(1)))
*
      Do  K = KPD1+1,KD
       KK  = K-KPD1+1
       KM  = KPD1-KK+1
       DXX = DBETA*DSX(K)
       If(DXX.gt.20.D0) Then
        DE = 0.D0
       Else
        DE = Dexp(-DXX)
       Endif
*
       DTNH   = (1.D0 - DE)/(1.D0 + DE)
       DH(KK) = DNORM*DSX(K)*(1.D0 - DE)
*
       If(BLT) Then
        D1  = DH(KK)
        DS1 = DCNY(K,1)
       Else
        D1 = DNORM*DTNH*DSX(K)
        If(KM.le.0) Then
         DS1  = DCNY(K,1)*(1.D0 + DE)
        Else
         DS1  = DCNY(K,1) + DCNY(KM,1)
        Endif
       Endif
*
       DCY(KK) = D1*DS1
      EndDo
*
*:  calculate covariance matrix of DOS
*
      Do  I = 1,KD-KPD1+1
       Do  J = 1,I
        DCOVAR(I,J) = DH(I)*DH(J)*DCOVAR(I,J)
       EndDo
      EndDo
*
*:  analyze inconsistencies
*:  re-normalize the DOS
*
      Call CNSTC(DOSF,DOSDF,DOSK,DOSDK,DOSM,DOSDM)
*
*:  calculate several quantities from the DOS
*
*:   Lamb-Moessbauer temperature, i.e., T_LM = -T/ln(f_LM) for T->inf
*
      DCT   = 0.D0
      DCDT  = 0.D0
      DCCT  = 0.D0
      Do  K = 2,KD-KPD1+1
       KK   = K+KPD1-1
       D1   = 1.D0/(DSX(KK)*DSX(KK))
       DCT  = DCT  + DCY(K)*D1
       DCDT = DCDT + DCOVAR(K,K)*D1*D1
       Do  J = 2,K-1
        JJ   = J+KPD1-1
        D2   = 1.D0/(DSX(JJ)*DSX(JJ))
        DCDT = DCDT + 2.D0*DCOVAR(K,J)*D1*D2
       EndDo
*
       DCCT = DCCT - DCY(K)*D1/DSX(KK)
      EndDo
*
      D1   = 1.D0/(DSX(KPD1+1)*DSX(KPD1+1))
      DCT  = DCT  + 0.5D0*DCY(2)*D1
      DCDT = DCDT + 0.5D0*DCOVAR(2,2)*D1*D1
*
*:  apply corrections caused by resolution function
*
      DCT = DCT - DCCT*DMR(1)*2.D0
*
      DRT  = Dsqrt(DCDT)/DCT
      DIFC = 2.D-6*DECOIL/DELCH*(DHQUER*DCLIC/DGAMMA)**2
     +              *DCT*(DSX(2)-DSX(1))
      DIFC = 1.D0/DIFC
      DDIFC = DRT*DIFC
      DCT  = 0.5D0*DNORM/(DBLTZ*DCT)
      DCDT = DCT*DRT
*
*:   recoilless fraction at T=0
*
      DF0   = 0.D0
      DFD0  = 0.D0
      DFC0  = 0.D0
      Do  K = 2,KD-KPD1+1
       KK   = K+KPD1-1
       D1   = 1.D0/DSX(KK)
       DF0  = DF0  + DCY(K)*D1
       DFD0 = DFD0 + DCOVAR(K,K)*D1*D1
       Do  J = 2,K-1
        JJ   = J+KPD1-1
        D2   = 1.D0/DSX(JJ)
        DFD0 = DFD0 + 2.D0*DCOVAR(K,J)*D1*D2
       EndDo
*
       DFC0 = DFC0 - DCY(K)*D1*D1
      EndDo
*
*:  apply corrections caused by resolution function
*
      DF0 = DF0 - DFC0*DMR(1)
*
      DF0  = Dexp(-DF0/DNORM)
      DFD0 = DF0*Dsqrt(DFD0)/DNORM
*
*:   kinatic energy/atom at T=0
*
      DK0   = 0.D0
      DKD0  = 0.D0
      DKC0  = 0.D0
      Do  K = 2,KD-KPD1+1
       KK   = K+KPD1-1
       D1   = DSX(KK)
       DK0  = DK0  + DCY(K)*D1
       DKD0 = DKD0 + DCOVAR(K,K)*D1*D1
       Do  J = 2,K-1
        JJ   = J+KPD1-1
        D2   = DSX(JJ)
        DKD0 = DKD0 + 2.D0*DCOVAR(K,J)*D1*D2
       EndDo
*
       DKC0 = DKC0 + DCY(K)
      EndDo
*
*:  apply corrections caused by resolution function
*
      DK0 = DK0 - DKC0*DMR(1)
*
      DNN  = (DSX(2)-DSX(1))*1.D-3
      DK0  = 0.25D0*DNN*DK0/3.D0
      DKD0 = 0.25D0*DNN*Dsqrt(DKD0)/3.D0
*
*:   specific heat in k_B/atom
*:   vibrational entropy in k_B/atom
*
      Do  K = 2,KD-KPD1+1
       KK     = K+KPD1-1
       DH(K)  = Dexp(-0.5D0*DBETA*DSX(KK))
       DH1(K) = Dlog(1.D0/DH(K) - DH(K))
      EndDo
*
      DCV   = 0.D0
      DDCV  = 0.D0
      DCCV  = 0.D0
      DEV   = 0.D0
      DDEV  = 0.D0
      DCEV  = 0.D0
      Do  K = 2,KD-KPD1+1
       KK   = K+KPD1-1
*
       DE   = DH(K)*DH(K)
       D1   = DSX(KK)*DSX(KK)/((1.D0 - DE )*(1.D0/DE - 1.D0))
       DCV  = DCV  + DCY(K)*D1
       DDCV = DDCV + DCOVAR(K,K)*D1*D1
*
       DSH  = 0.5D0*(1.D0/DH(K) - DH(K))
       DCH  = 0.5D0*(1.D0/DH(K) + DH(K))
       D3   = 0.5D0*DBETA*DSX(KK)*DCH/DSH - DH1(K)
       DEV  = DEV  + DCY(K)*D3
       DDEV = DDEV + DCOVAR(K,K)*D3*D3
*
       Do  J = 2,K-1
        JJ   = J+KPD1-1
        DE   = DH(J)*DH(J)
        D2   = DSX(JJ)*DSX(JJ)/((1.D0 - DE )*(1.D0/DE - 1.D0))
        DDCV = DDCV + 2.D0*DCOVAR(K,J)*D1*D2
*
        DSH1 = 0.5D0*(1.D0/DH(J) - DH(J))
        DCH1 = 0.5D0*(1.D0/DH(J) + DH(J))
        D4   = 0.5D0*DBETA*DSX(JJ)*DCH1/DSH1 - DH1(J)
        DDEV = DDEV + 2.D0*DCOVAR(K,J)*D3*D4
       EndDo
*
       DCCV = DCCV + DCY(K)*D1*(2.D0/DSX(KK)-DBETA*DCH/DSH)
       DCEV = DCEV - DCY(K)*0.25D0*DSX(KK)*(DBETA/DSH)**2
      EndDo
*
*:  apply corrections caused by resolution function
*
      DCV = DCV - DCCV*DMR(1)
      DEV = DEV - DCEV*DMR(1)
*
      DCV  = DNN*DCV*DBETA**2
      DDCV = DNN*Dsqrt(DDCV)*DBETA**2
*
      DEV  = DNN*DEV
      DDEV = DNN*Dsqrt(DDEV)
*
*:  calculate ln(beta)
*
      DLNB   = 3.D1*(DOSK*DBETA - 0.5D0)
      DDLNB  = 3.D1*DOSDK*DBETA
*
      DNN    = 0.25D-2*DECOIL/DELCH*(DHQUER*DCLIC/DGAMMA)**2
      DLNBH  = DNN*DOSM*DBETA**2
      DDLNBH = DNN*DOSDM*DBETA**2
*
      Write(LUNPTL,2001) '','',DOSF,DOSDF,DOSK,DOSDK,DOSM,DOSDM,
     +              DF0,DFD0,DK0,DKD0,DCV,DDCV,DEV,DDEV,DIFC,DDIFC,
     +              DCT,DCDT,DLNB,DDLNB,DLNBH,DDLNBH
      Write(6,2001) XT_MAG,XT_DEF,DOSF,DOSDF,DOSK,DOSDK,DOSM,DOSDM,
     +              DF0,DFD0,DK0,DKD0,DCV,DDCV,DEV,DDEV,DIFC,DDIFC,
     +              DCT,DCDT,DLNB,DDLNB,DLNBH,DDLNBH
*
 2001 Format(2X,A,'Quantities calculated from the partial DOS -->',A/
     + 4X,'Lamb-Moessbauer factor',8X,':',F9.4,' +-',F8.4/
     + 4X,'kinetic energy',16X,':',F9.4,' +-',F8.4,' meV/atom'/
     + 4X,'mean force constant',11X,':',F9.4,' +-',F8.4,' N/m'/
     + 4X,'Lamb-Moessbauer factor at T=0 :',F9.4,' +-',F8.4/
     + 4X,'kinetic energy',9X,'at T=0 :',F9.4,' +-',F8.4,' meV/atom'/
     + 4X,'vibrational specific heat     :',
     +                       F9.4,' +-',F8.4,' k_B/atom'/
     + 4X,'vibrational entropy',11X,':',F9.4,' +-',F8.4,' k_B/atom'/
     + 4X,'resilience'         ,20X,':',F9.4,' +-',F8.4,' N/m'/
     + 4X,'Lamb-Moessbauer temperature   :',F9.1,' +-',F8.2,' K'/
     + 4X,'isotope fractionation',9X,':',F9.4,' +-',F8.4,' perMille/%'/
     + 4X,'high T isotope frac.',10X,':',F9.4,' +-',F8.4,' perMille/%'/)
*
*:  decorrelate errors of DOS
*
      If(Index(XOPT,'X').ne.0) Then
       KM = KD-KPD1+1
       Do  I = 1,KM
        Do  J = 1,I-1
         DCO(I,J) = DCOVAR(I,J)
         DCO(J,I) = DCOVAR(I,J)
        EndDo
        DCO(I,I) = DCOVAR(I,I)
       EndDo
*
       Write(6,2002) XT_MAG,XT_DEF
 2002  Format(3X,A,'Decorrelation of DOS errors -->',A/
     +          5X,'Iteration',5X,'Correlation')
*
       Call DRSEV(PDATA,KM,DCO,DVC,1.D-10,20,NERR,0,DWORK)
       Write(6,*)
*
       DCDY(1) = 0.D0
       Do  I = 2,KM
        DCDY(I) = Dsqrt(DCO(I,I))
       EndDo
*
      Else
       KM = KD-KPD1+1
       Do  I = 1,KM
        DCDY(I) = Dsqrt(DCOVAR(I,I))
       EndDo
      Endif
*
*
      End
