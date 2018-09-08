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
      Subroutine CNSTC(DOSF,DOSDF,DOSK,DOSDK,DOSM,DOSDM)
*
* purpose :  perform tests on consistency involving the DOS,
*            and previouly calculated moments from the raw data;
*            update the normalization of the DOS
* date    :  March 14, 1997
* author  :  Wolfgang Sturhahn                                          
*
*
* import variables :
*    KPD1    = index corresponding to x=0
*    KPD2    = index corresponding to maximum useful x-value
*    DSX(*)  = x-value of data
*    DSY(*)  = y-value of data
*    DCY(*)  = DOS
*    DCDY(*) = errors of DOS
*    KDBN    = DSX(KDBN) is the beginning of the central area
*    KDBP    = DSX(KDBP) is the end of the central area
*    DSCLE   = scaling factor of inelastic spectrum
*    DFFAC   = f-factor from area of inelastic spectrum
*    DDFFAC  = error of f-factor
*    DKIN    = kinetic energy from 2nd moment of inelastic spectrum
*    DDKIN   = error of kinetic energy
*    DMPHE   = force constant from 3rd moment of inelastic spectrum
*    DDMPHE  = error of force constant
*
* export variables :
*    DMD(*)  = moments of the data
*    DMR(*)  = moments of res.fct.
*    DOSF    = f-factor from DOS
*    DOSK    = kinetic energy from DOS
*    DOSM    = force constant from DOS
*                                                                       
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*                                                                       
#include<phoenix.h>
#include<phox.h>
#include<xterm.h>
*
      Dimension  DCYAV(PDATA),DHH1(PDATA),DHH2(PDATA),
     +            DSP(PDATA),DSPP(PDATA)
*
      Character*20 XMSGN,XMSGN1,XMSGF,XMSGK,XMSGM,XMSGG,XMSGS,
     +             XLMSGN,XLMSGN1,XLMSGF,XLMSGK,XLMSGM,XLMSGG,XLMSGS
*
*
*:  calculate slope and curvature of positive spectrum
*:  exclude the central part
*
      KS = 2*Max0(KPD1-KDBN,KDBP-KPD1)
      KE = Min0(KD-2,2*KPD1-1) - KPD1
*
      DH   = DSX(2) - DSX(1)
      KDBL = 0
      Do  K = KS,KE
       KP = KPD1 + K
       KM = KPD1 - K
       DSDY(KP) = Dabs(DSDY(KP))
       DSDY(KM) = Dabs(DSDY(KM))
*
       If(DBETA*DSX(KP).gt.5.D0) Exit
       DW   = Dexp(DBETA*DSX(KP))
       DPLS = DSY(KP)
       DMNS = DSY(KM)
*
       DDPL = DPLS + DMNS
       DDD  = Dsqrt(DSDY(KP) + DSDY(KM))
       If(DDD.gt.0.3D0*DDPL) Exit
*
       KDBL = KDBL + 1
*
*:  first order derivative
*
       DHH1(KDBL) = 0.5D0*(DSY(KP+1) - DSY(KP-1))/DH
*
*:  second order derivative
*
       DHH2(KDBL) = (-DSY(KP+2) + 16.D0*DSY(KP+1) - 30.D0*DSY(KP) +
     +        16.D0*DSY(KP-1) - DSY(KP-2))/(12.D0*DH*DH)
      EndDo
*
*:  average the derivatives
*
      DFWHM = 2.D0*(DLN2/DP(2))**(1.D0/DP(7))
      NAVE  = Min0(Idnint(DFWHM/DH),KD/20)
*
      Call DRUNAV(DHH1,DSP,KDBL,NAVE)
      Call DRUNAV(DHH2,DSPP,KDBL,NAVE)
*
*:  detailed balance of the data
*:  exclude the central part
*
      DS0  = 0.D0
      DS1  = 0.D0
      DS2  = 0.D0
      DS3  = 0.D0
      DS4  = 0.D0
      DS5  = 0.D0
      DS6  = 0.D0
      KDBL = 0
      Do  K = KS,KE
       KP = KPD1 + K
       KM = KPD1 - K
       DSDY(KP) = Dabs(DSDY(KP))
       DSDY(KM) = Dabs(DSDY(KM))
*
       If(DBETA*DSX(KP).gt.5.D0) Exit
       DW   = Dexp(DBETA*DSX(KP))
       DPLS = DSY(KP)
       DMNS = DSY(KM)
*
       DDPL = DPLS + DMNS
       DDMS = DPLS - DMNS
       DDD  = Dsqrt(DSDY(KP) + DSDY(KM))
       If(DDD.gt.0.3D0*DDPL) Exit
*
       D1   = DPLS - DW*DMNS
       D2   = DPLS + DW*DMNS
*
       DS0 = DS0 + DW*DMNS
       DS1 = DS1 + D1
       DS2 = DS2 + D2
       DS3 = DS3 + DW*DMNS
       DS4 = DS4 + DSDY(KP)
       DS5 = DS5 + DPLS
       DS6 = DS6 + DW**2*DSDY(KM)
       If(K.eq.0) Then
        DS0 = DS0 + DFFAC
        DS2 = DS2 + DFFAC
        DS3 = DS3 + DFFAC
        DS5 = DS5 + DFFAC
       Endif
*
*:  thermal asymmetry;
*:  include correction for resolution function
*
       KDBL = KDBL + 1
       DDBX(KDBL)  = 0.5D0*DBETA*DSX(KP)
*
*:  first order correction
*
       DK1D = DMR(1)*(DSP(KDBL)*(1.D0+1.D0/DW) - DBETA*DSY(KM))
       DK1N = DMR(1)*(DSP(KDBL)*(1.D0-1.D0/DW) + DBETA*DSY(KM))
*
*:  second order correction
*:  these are twice as large as they should be but works better
*
       DK2D = DMR(2)*(DSPP(KDBL)*(1.D0+1.D0/DW) -
     +             2.D0*DBETA*DSP(KDBL)/DW + DBETA*DBETA*DSY(KM))
       DK2N = DMR(2)*(DSPP(KDBL)*(1.D0-1.D0/DW) +
     +             2.D0*DBETA*DSP(KDBL)/DW - DBETA*DBETA*DSY(KM))
*
       DDPL = DDPL - DK1D - DK2D
       If(DDD.ge.0.30D0*DDPL) Then
        KDBL = KDBL - 1
       Else
        DDMS = DDMS - DK1N - DK2N
        DDBY(KDBL)  = DDMS/DDPL
        DDBDY(KDBL) = 2.D0*Dsqrt(
     +               DSDY(KP)*DMNS**2 + DSDY(KM)*DPLS**2)/DDPL**2
*
        DQFCT(KDBL) = 4.D0*(0.5D0 - DSP(KDBL)/(DBETA*DSY(KP)))
       Endif
      EndDo
*
*:  quadratic fit of thermal asymmetry
*
      Call DBFIT(DDBX,DDBY,DDBDY,DQFCT,KDBL,
     +             DEC0,DEC1,DEC2,DVEC0,DVEC1,DVEC2,DVEC12,DCHI0,DCHI2)
*
*:  average energy correction and error
*
      DDEAV2 = 0.D0
      DDEAV3 = 0.D0
      DDEAV4 = 0.D0
      Do  K = 1,KD
       DDEAV2 = DDEAV2 + DSX(K)**2
       DDEAV3 = DDEAV3 + DSX(K)**3
       DDEAV4 = DDEAV4 + DSX(K)**4
      EndDo
      DAVEC  = Dsqrt(DEC1**2 + 2.D0*DEC1*DEC2*DBETA*DDEAV3/DDEAV2 +
     +                 DDEAV4/DDEAV2*(DBETA*DEC2)**2)
      DDAVEC = Dsqrt(DVEC1 + 2.D0*DVEC12*DBETA*DDEAV3/DDEAV2 +
     +                 DVEC2*DDEAV4/DDEAV2*DBETA**2)
*
      If(DS2.eq.0.D0) Then
       DE  = 0.D0
       DDE = 0.9999D0
      Else
*
*:  correction to detailed balance due to finite energy resolution
*
       DCS = DMR(1)*(-DS0*DBETA + (DSY(KPD1+KS) -
     +        DSY(KPD1-KS)*Dexp(DBETA*DSX(KPD1-KS)))/(DSX(2)-DSX(1)) )
       KPS = KS + KPD1
       KMS = KPD1 - KS
       DWS = Dexp(DBETA*DSX(KPS))
*
       DCS11 = DMR(1)*(DSY(KPS) - DWS*DSY(KMS))
       DCS21 = 0.5D0*DMR(2)*(DSP(1)*(1 + DWS) - DBETA*DWS*DSY(KMS))
*
       DCS12 = DMR(1)*(DSY(KPS) + DWS*DSY(KMS))
       DCS22 = 0.5D0*DMR(2)*(DSP(1)*(1 - DWS) + DBETA*DWS*DSY(KMS))
*
       DS1 = DS1 + DCS11 + DCS21
       DS2 = DS2 + DCS12 + DCS22
*
       DE  = DS1/DS2
       DDE = 2.D0*Dsqrt(DS3**2*DS4 + DS5**2*DS6)/DS2**2
      Endif
*
*:  analyze inconsistencies
*
      DSIGF = (DE/DDE)**2
      If(DSIGF.gt.7.D0) Then
       DSIGF = 1.D2
      Else
       DSIGF = 1.D2*(1.D0 - Dexp(-DSIGF))
      Endif
      If(DSIGF.gt.DSGLM2) Then
       XMSGN  = XT_RED//'concerning'//XT_DEF
       XLMSGN =  'concerning'
      ElseIf(DSIGF.gt.DSGLM1) Then
        XMSGN  = XT_CYN//'acceptable'//XT_DEF
        XLMSGN = 'acceptable'
       Else
        XMSGN  = XT_GRN//'    ok'//XT_DEF
        XLMSGN = '    ok'
      Endif
*
      DSIGF1 = (DAVEC/DDAVEC)**2
      If(DSIGF1.gt.7.D0) Then
       DSIGF1 = 1.D2
      Else
       DSIGF1 = 1.D2*(1.D0 - Dexp(-DSIGF1))
      Endif
      If(DSIGF1.gt.DSGLM2) Then
       XMSGN1  = XT_RED//'concerning'//XT_DEF
       XLMSGN1 = 'concerning'
      ElseIf(DSIGF1.gt.DSGLM1) Then
        XMSGN1  = XT_CYN//'acceptable'//XT_DEF
        XLMSGN1 = 'acceptable'
       Else
        XMSGN1  = XT_GRN//'    ok'//XT_DEF
        XLMSGN1 = '    ok'
      Endif
*
      Write(LUNPTL,2000) '','',1.D2*DE,1.D2*DDE,Dabs(DE/DDE),XLMSGN,
     +              1.D2*DAVEC,1.D2*DDAVEC,Dabs(DAVEC/DDAVEC),XLMSGN1
      Write(6,2000) XT_MAG,XT_DEF,1.D2*DE,1.D2*DDE,Dabs(DE/DDE),XMSGN,
     +              1.D2*DAVEC,1.D2*DDAVEC,Dabs(DAVEC/DDAVEC),XMSGN1
*
 2000 Format(2X,A,'Consistency tests using the refined data -->',A/
     + 7X,'tested quantity',9X,'%deviation',3X,'norm.dev.   status'/
     + 4X,'detailed balance',7X,F7.2,' +-',F5.2,F8.2,4X,A/
     + 4X,'energy/temp. calib.',4X,F7.2,' +-',F5.2,F8.2,4X,A/)
*
*
*:  average DOS
*
      DFWHM = 2.D0*(DLN2/DP(2))**(1.D0/DP(7))
      JAV   = Idnint(3.D0*DFWHM/(DSX(2)-DSX(1)))
*
      Do  K = 1,KD-KPD1+1
       DCYAV(K) = 0.D0
       JJ       = 0
       Do  J = -JAV,JAV
        If(K.gt.-J .and. K.lt.KD-KPD1+2-J) Then
         DCYAV(K) = DCYAV(K) + DCY(K+J)
         JJ = JJ + 1
        Endif
       EndDo
       DCYAV(K) = DCYAV(K)/Dfloat(JJ)
      EndDo
*
*:  normalization and negativity of DOS
*
      DOSNG  = 0.D0
      DOSDNG = 0.D0
      DOSN   = 0.D0
      DOSDN  = 0.D0
      Do  K = 1,KD-KPD1+1
       If(DCYAV(K).lt.0.D0) Then
        DOSNG  = DOSNG  + Dabs(DCYAV(K))
        DOSDNG = DOSDNG + DCOVAR(K,K)
        Do  J = 1,K-1
         If(DCYAV(J).lt.0.D0) Then
          DOSDNG = DOSDNG + 2.D0*DCOVAR(K,J)
         Endif
        EndDo
       Endif
*
       DOSN  = DOSN  + DCY(K)
       DOSDN = DOSDN + DCOVAR(K,K)
       Do  J = 1,K-1
        DOSDN = DOSDN + 2.D0*DCOVAR(K,J)
       EndDo
      EndDo
*
*:  re-normalization of DOS
*
      DNN    = (DSX(2)-DSX(1))/3.D3
*
      DOSNG  = 0.5D0*DOSNG*DNN
      DOSDNG = 0.5D0*Dsqrt(2.D0*DOSDNG)*DNN
*
      DOSN   = DOSN*DNN
      DOSDN  = Dsqrt(DOSDN)*DNN
*
      DOSN2 = DOSN*DOSN
      Do  K = 1,KD-KPD1+1
       DCY(K)  = DCY(K)/DOSN
       DCDY(K) = DCDY(K)/DOSN
       Do  J = 1,K
        DCOVAR(K,J) = DCOVAR(K,J)/DOSN2
       EndDo
      EndDo
*
*
*:  other contractions of the DOS
*
      Do  K = KPD1+1,KD
       KK       = K-KPD1+1
       DE       = Dexp(-DBETA*DSX(K))
       DHH1(KK) = (1.D0 + DE)/(1.D0 - DE)
      EndDo
*
*
*:  recoilless fraction
*
      DOSF   = 0.D0
      DOSDF  = 0.D0
      DOSCF  = 0.D0
      DOSCF2 = 0.D0
      Do  K = 2,KD-KPD1+1
       KK    = K+KPD1-1
       D1    = DHH1(K)/DSX(KK)
       DOSF  = DOSF  + DCY(K)*D1
       DOSDF = DOSDF + DCOVAR(K,K)*D1*D1
       Do  J = 2,K-1
        JJ    = J+KPD1-1
        D2    = DHH1(J)/DSX(JJ)
        DOSDF = DOSDF + 2.D0*DCOVAR(K,J)*D1*D2
       EndDo
*
       D0     = 0.5D0*DBETA*(1.D0 - DHH1(K)**2)
       DOSCF  = DOSCF - DCY(K)*(D0 + D1)/DSX(KK)
       DOSCF2 = DOSCF2 + DCY(K)*D0*(2.D0/DSX(KK)-DBETA*DHH1(K))/DSX(KK)
      EndDo
*
      D1     = DHH1(2)/DSX(KPD1+1)
      DOSF   = DOSF  + 0.5D0*DCY(2)*D1
      DOSDF  = DOSDF + 0.5D0*DCOVAR(2,2)*D1*D1
*
*:  apply correction originating from resolution function
*
      DOSF  = DOSF - DOSCF*DMR(1) - 0.5D0*DOSCF2*DMR(2)
*
      DOSF  = Dexp(-DOSF*DECOIL*DNN)
      DOSDF = DOSF*Dsqrt(DOSDF)*DNN*DECOIL
*
*
*:  kinetic energy
*
      DOSK   = 0.D0
      DOSDK  = 0.D0
      DOSCK  = 0.D0
      DOSCK2 = 0.D0
      Do  K = 2,KD-KPD1+1
       KK     = K+KPD1-1
       D1     = DHH1(K)*DSX(KK)
       DOSK   = DOSK   + DCY(K)*D1
       DOSDK  = DOSDK  + DCOVAR(K,K)*D1*D1
       Do  J = 2,K-1
        JJ    = J+KPD1-1
        D2    = DHH1(J)*DSX(JJ)
        DOSDK = DOSDK + 2.D0*DCOVAR(K,J)*D1*D2
       EndDo
*
       D0     = 0.5D0*DBETA*(1.D0 - DHH1(K)**2)
       DOSCK  = DOSCK  + DCY(K)*(D0*DSX(KK) + DHH1(K))
       DOSCK2 = DOSCK2 + DCY(K)*D0*(2.D0 - DBETA*DSX(KK)*DHH1(K))
      EndDo
*
*:  apply corrections originating from resolution function
*
      DOSK  = DOSK - DOSCK*DMR(1) - 0.5D0*DOSCK2*DMR(2)
*
      DOSK  = 0.25D0*DOSK*DNN
      DOSDK = 0.25D0*Dsqrt(DOSDK)*DNN
*
*
*:  mean force constant
*
      DOSM   = 0.D0
      DOSDM  = 0.D0
      DOSCM  = 0.D0
      Do  K = 2,KD-KPD1+1
       KK     = K+KPD1-1
       D1     = DSX(KK)*DSX(KK)
       DOSM   = DOSM   + DCY(K)*D1
       DOSDM  = DOSDM  + DCOVAR(K,K)*D1*D1
       Do  J = 2,K-1
        JJ    = J+KPD1-1
        D2    = DSX(JJ)*DSX(JJ)
        DOSDM = DOSDM + 2.D0*DCOVAR(K,J)*D1*D2
       EndDo
*
       DOSCM = DOSCM + DCY(K)*DSX(KK)
      EndDo
*
*:  apply corrections originating from resolution function
*
      DOSM  = DOSM - 2.D0*DOSCM*DMR(1) - 3.D0*DMR(2)/DNN
*
      DNN   = DNN*5.D2*DELCH*(DGAMMA/(DHQUER*DCLIC))**2/DECOIL
      DOSM  = DOSM*DNN
      DOSDM = Dsqrt(DOSDM)*DNN
*
*
*:  analyze inconsistencies
*
      DSIGFG = (DOSNG/DOSDNG)**2
      DSUM   = DSIGFG
      If(DSIGFG.GT.7.D0) Then
       DSIGFG = 1.D2
      Else
       DSIGFG = 1.D2*(1.D0 - DEXP(-DSIGFG))
      Endif
      If(DSIGFG.GT.DSGLM2) Then
       XMSGG  = XT_RED//'concerning'//XT_DEF
       XLMSGG = 'concerning'
      ElseIf(DSIGFG.GT.DSGLM1) Then
        XMSGG  = XT_CYN//'acceptable'//XT_DEF
        XLMSGG = 'acceptable'
       Else
        XMSGG  = XT_GRN//'    ok'//XT_DEF
        XLMSGG = '    ok'
      Endif
*
      DSIGFN = ((DOSN-1.D0)/DOSDN)**2
      DSUM   = DSUM + DSIGFN
      If(DSIGFN.GT.7.D0) Then
       DSIGFN = 1.D2
      Else
       DSIGFN = 1.D2*(1.D0 - DEXP(-DSIGFN))
      Endif
      If(DSIGFN.GT.DSGLM2) Then
       XMSGN  = XT_RED//'concerning'//XT_DEF
       XLMSGN = 'concerning'
      ElseIf(DSIGFN.GT.DSGLM1) Then
        XMSGN  = XT_CYN//'acceptable'//XT_DEF
        XLMSGN = 'acceptable'
       Else
        XMSGN  = XT_GRN//'    ok'//XT_DEF
        XLMSGN = '    ok'
      Endif
*
      DOSDFT = DOSDF**2 + DDFFAC**2
      DSIGFF = (DOSF-DFFAC)**2/DOSDFT
      DOSDFT = DSQRT(DOSDFT)
      DSUM   = DSUM + DSIGFF
      If(DSIGFF.GT.7.D0) Then
       DSIGFF = 1.D2
      Else
       DSIGFF = 1.D2*(1.D0 - DEXP(-DSIGFF))
      Endif
      If(DSIGFF.GT.DSGLM2) Then
       XMSGF  = XT_RED//'concerning'//XT_DEF
       XLMSGF = 'concerning'
      ElseIf(DSIGFF.GT.DSGLM1) Then
        XMSGF  = XT_CYN//'acceptable'//XT_DEF
        XLMSGF = 'acceptable'
       Else
        XMSGF  = XT_GRN//'    ok'//XT_DEF
        XLMSGF = '    ok'
      Endif
*
      DOSDKT = DOSDK**2+DDKIN**2
      DSIGFK = (DOSK-DKIN)**2/DOSDKT
      DOSDKT = DSQRT(DOSDKT)
      DSUM   = DSUM + DSIGFK
      If(DSIGFK.GT.7.D0) Then
       DSIGFK = 1.D2
      Else
       DSIGFK = 1.D2*(1.D0 - DEXP(-DSIGFK))
      Endif
      If(DSIGFK.GT.DSGLM2) Then
       XMSGK  = XT_RED//'concerning'//XT_DEF
       XLMSGK = 'concerning'
      ElseIf(DSIGFK.GT.DSGLM1) Then
        XMSGK  = XT_CYN//'acceptable'//XT_DEF
        XLMSGK = 'acceptable'
       Else
        XMSGK  = XT_GRN//'    ok'//XT_DEF
        XLMSGK = '    ok'
      Endif
*
      DOSDMT = DOSDM**2+DDMPHE**2
      DSIGFM = (DOSM-DMPHE)**2/DOSDMT
      DOSDMT = DSQRT(DOSDMT)
      DSUM   = DSUM + DSIGFM
      If(DSIGFM.GT.7.D0) Then
       DSIGFM = 1.D2
      Else
       DSIGFM = 1.D2*(1.D0 - DEXP(-DSIGFM))
      Endif
      If(DSIGFM.GT.DSGLM2) Then
       XMSGM  = XT_RED//'concerning'//XT_DEF
       XLMSGM = 'concerning'
      ElseIf(DSIGFM.GT.DSGLM1) Then
        XMSGM  = XT_CYN//'acceptable'//XT_DEF
        XLMSGM = 'acceptable'
       Else
        XMSGM  = XT_GRN//'    ok'//XT_DEF
        XLMSGM = '    ok'
      Endif
*
      DSUM = 0.2D0*DSUM
      If(DSUM.gt.7.D0) Then
       DSIGFS = 1.D2
      Else
       DSIGFS = 1.D2*(1.D0 - DEXP(-DSUM))
      Endif
      If(DSIGFS.gt.DSGLM2) Then
       XMSGS  = XT_RED//'concerning'//XT_DEF
       XLMSGS = 'concerning'
      ElseIf(DSIGFS.GT.DSGLM1) Then
        XMSGS  = XT_CYN//'acceptable'//XT_DEF
        XLMSGS = 'acceptable'
       Else
        XMSGS  = XT_GRN//'    ok'//XT_DEF
        XLMSGS = '    ok'
      Endif
      DSUM = Dsqrt(DSUM)
*
      Write(LUNPTL,2001) '','',
     +       1.D2*DOSNG/DOSN,1.D2*DOSDNG/DOSN,Dabs(DOSNG/DOSDNG),XLMSGG,
     +       1.D2*(DOSN-1.D0),1.D2*DOSDN,Dabs((DOSN-1.D0)/DOSDN),XLMSGN,
     +       1.D2*(DOSF/DFFAC-1.D0),1.D2*DOSDFT/DFFAC,
     +                              Dabs((DOSF-DFFAC)/DOSDFT),XLMSGF,
     +       1.D2*(DOSK/DKIN-1.D0) ,1.D2*DOSDKT/DKIN,
     +                              Dabs((DOSK-DKIN)/DOSDKT),XLMSGK,
     +       1.D2*(DOSM/DMPHE-1.D0),1.D2*DOSDMT/DMPHE,
     +                              Dabs((DOSM-DMPHE)/DOSDMT),XLMSGM,
     +       DSUM,XLMSGS
      Write(6,2001) XT_MAG,XT_DEF,
     +       1.D2*DOSNG/DOSN,1.D2*DOSDNG/DOSN,Dabs(DOSNG/DOSDNG),XMSGG,
     +       1.D2*(DOSN-1.D0),1.D2*DOSDN,Dabs((DOSN-1.D0)/DOSDN),XMSGN,
     +       1.D2*(DOSF/DFFAC-1.D0),1.D2*DOSDFT/DFFAC,
     +                              Dabs((DOSF-DFFAC)/DOSDFT),XMSGF,
     +       1.D2*(DOSK/DKIN-1.D0) ,1.D2*DOSDKT/DKIN,
     +                              Dabs((DOSK-DKIN)/DOSDKT),XMSGK,
     +       1.D2*(DOSM/DMPHE-1.D0),1.D2*DOSDMT/DMPHE,
     +                              Dabs((DOSM-DMPHE)/DOSDMT),XMSGM,
     +       DSUM,XMSGS
*
*
 2001 Format(2X,A,'Consistency tests using the partial DOS -->',A/
     +  7X,'tested quantity',9X,'%deviation',3X,'norm.dev.   status'/
     +  4X,'negativity of DOS',6X,F7.2,' +-',F5.2,F8.2,4X,A/
     +  4X,'norm of DOS',12X,F7.2,' +-',F5.2,F8.2,4X,A/
     +  4X,'Lamb-Moessbauer factor',1X,F7.2,' +-',F5.2,F8.2,4X,A/
     +  4X,'kinetic energy / atom',2X,F7.2,' +-',F5.2,F8.2,4X,A/
     +  4X,'mean force constant',4X,F7.2,' +-',F5.2,F8.2,4X,A/
     +  4X,'rms average --------------------------->',F6.2,4X,A/)
*
*
      End
