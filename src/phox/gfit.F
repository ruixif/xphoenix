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
      Subroutine GFIT(NFIT,NERR)
*
* purpose :  fit resolution function to elastic peak
* date    :  December 29, 1997
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    NFIT       = number of fit parameters
*    KD         = number of data points
*    DMX(*)     = x-value of data
*    DMY(*)     = y-value of data
*    DMDY(*)    = y-error^2 of data
*    DRANGE     = fit range
*    DP(3)      = position of elastic peak, start value
*    DP(4)      = background scaling factor, start value
*    DP(6)      = asymmetry coefficient
*    DP(7)      = shape coefficient
*    BPT        = if .TRUE. print results to protocol file
*
* export variables :
*    DP(1)   = scaling factor
*    DP(2)   = ln2*(2/FWHM)**DP(7)
*    DP(3)   = position of elastic peak
*    DP(4)   = background scaling factor
*    KMAX    = index of peak position
*    K1,K2   = indices defining the fit range
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
#include<xterm.h>
*
      Dimension DTY(PDATA,0:PFIT),DK(PFIT,PFIT),DM(PFIT,PFIT),
     +          DDZ(PFIT),DD2Z(PFIT,PFIT)
*
*
*:  define start values for fit parameters
*:  ======================================
*
*:   DP(1) =  scaling factor
*:   DP(2) =  ln2*(2/FWHM)**DP(7)
*:   DP(3) =  position of elastic peak
*:   DP(4) =  background scaling factor
*
*:  position of elastic peak, DP(3)
*:  defined by the largest y-value or by start value
*
      If(DP(3).eq.0.D0) Then
       DMYMAX = 0.0
       KMAX   = 1
       Do  K = 1,KD
        If(DMY(K).le.DMYMAX) Cycle
        DMYMAX = DMY(K)
        KMAX   = K
       EndDo
*      DP(3) = DMX(KMAX)
       DP(3) = (DMX(KMAX-1)*DMY(KMAX-1)+DMX(KMAX)*DMY(KMAX)+
     +   DMX(KMAX+1)*DMY(KMAX+1))/(DMY(KMAX-1)+DMY(KMAX)+DMY(KMAX+1))
      Else
       KMAX = 1
       Do While (DMX(KMAX).lt.DP(3))
        KMAX = KMAX + 1
       EndDo
      Endif
*
*:  define fit range in terms of indices
*
      KR = Idnint(DRANGE/Dabs(DMX(2)-DMX(1)))
      KR = Max0(1,KR)
*
      K1 = Max0(KMAX-KR,1)
      K2 = Min0(KMAX+KR,KD)
      If(K2-K1+1.le.NFIT) Then
       NERR = 9801
       Return
      Endif
*
*:  peak height, DP(1)
*
      DP1M = -1.D0
      Do  K = K1,K2
       If(DMY(K).gt.DP1M) Then
        KMAX = K
        DP1M = DMY(K)
       Endif
      EndDo
*
*:  update fit range indices
*
      K1 = Max0(KMAX-KR,1)
      K2 = Min0(KMAX+KR,KD)
*
*: peak height, DP(1)
*
      DP(1) = DMY(KMAX) - 0.5D0*(DMY(K1) + DMY(K2))
*
*:  position of elastic peak, DP(3)
*
      DP(3) = DMX(KMAX)
*
*:  inelastic background, DP(4)
*
      If(NFIT.gt.3) Then
       DXX   = DBETA*(DMX(K2) - DP(3))
       DP(4) = 0.5D0*(DMY(K1)+DMY(K2))*(1.D0 - Dexp(-DXX))/(DXX*DP(1))
      Endif
*
*:  width of central peak, DP(2)
*
*:  for small peaks statistical fluctuations make
*:   determination of width from data unreliable
*
      If(Dsqrt(DMDY(KMAX))/DP(1).lt.0.15D0) Then
       DP(2) = DLN2/(Dabs(0.25D0*(DMX(K2) - DMX(KMAX))))**DP(7)
      Else
       DHPH = (0.5D0 + DP(4))*DP(1)
       KW   = 0
       Do  K = KMAX+1,KD
        If(DMY(K).le.DHPH) Then
         KW = K - KMAX
         Exit
        Endif
       EndDo
       KW    = Max0(KW,1)
       DP(2) = DLN2/(Dabs(DMX(KMAX+KW) - DMX(KMAX)))**DP(7)
      Endif
*
*:  print header
*
      If(BREXT) Then
       If(BPROC) Then
        Write(LUNPTL,1997) '','',
     +                 DRESMX,DRESBK,DRESCL,DRESCR,
     +                 DMX(K1)-DMX(KMAX),DMX(K2)-DMX(KMAX),DMX(KMAX),
     +                 DP(1),2.D0*(DLN2/DP(2))**(1.D0/DP(7)),
     +                 DP(3),DP(4)*1.D2,DP(6)
        Write(6,1997) XT_MAG,XT_DEF,
     +                 DRESMX,DRESBK,DRESCL,DRESCR,
     +                 DMX(K1)-DMX(KMAX),DMX(K2)-DMX(KMAX),DMX(KMAX),
     +                 DP(1),2.D0*(DLN2/DP(2))**(1.D0/DP(7)),
     +                 DP(3),DP(4)*1.D2,DP(6)
       Else
        Write(LUNPTL,1998) '','',
     +                 DRESMX,DRESCL,DRESCR,
     +                 DMX(K1)-DMX(KMAX),DMX(K2)-DMX(KMAX),DMX(KMAX),
     +                 DP(1),2.D0*(DLN2/DP(2))**(1.D0/DP(7)),
     +                 DP(3),DP(4)*1.D2,DP(6)
        Write(6,1998) XT_MAG,XT_DEF,
     +                 DRESMX,DRESCL,DRESCR,
     +                 DMX(K1)-DMX(KMAX),DMX(K2)-DMX(KMAX),DMX(KMAX),
     +                 DP(1),2.D0*(DLN2/DP(2))**(1.D0/DP(7)),
     +                 DP(3),DP(4)*1.D2,DP(6)
       Endif
      Else
       Write(LUNPTL,1999) '','',
     +                 DMX(K1)-DMX(KMAX),DMX(K2)-DMX(KMAX),DMX(KMAX),
     +                 DP(1),2.D0*(DLN2/DP(2))**(1.D0/DP(7)),
     +                 DP(3),DP(4)*1.D2,DP(6),DP(7)
       Write(6,1999) XT_MAG,XT_DEF,
     +                 DMX(K1)-DMX(KMAX),DMX(K2)-DMX(KMAX),DMX(KMAX),
     +                 DP(1),2.D0*(DLN2/DP(2))**(1.D0/DP(7)),
     +                 DP(3),DP(4)*1.D2,DP(6),DP(7)
      Endif
*
 1997  Format(2X,A,'Fit of the elastic peak -->',A/
     +  4X,'shape function processed from file'/
     +  6X,'peak              : ',1PE10.2/
     +  6X,'background        : ',0PF6.2/
     +  6X,'valid range (meV) :',F7.2,' to',F6.2/
     +  4X,'fit range (meV) : ',F6.2,' to',F6.2,'  centered at',F6.2/
     +  14X,'Chi^2    peak   FWHM   pos.  %bgr.  asm.'/
     +  4X,'start:',9X,1PE9.2,0PF6.2,F7.3,F7.3,F6.2)
 1998  Format(2X,A,'Fit of the elastic peak -->',A/
     +  4X,'shape function unprocessed from file'/
     +  6X,'peak              : ',1PE10.2/
     +  6X,'valid range (meV) :',0PF7.2,' to',F6.2/
     +  4X,'fit range (meV) : ',F6.2,' to',F6.2,'  centered at',F6.2/
     +  14X,'Chi^2    peak   FWHM   pos.  %bgr.  asm.'/
     +  4X,'start:',9X,1PE9.2,0PF6.2,F7.3,F7.3,F6.2)
 1999  Format(2X,A,'Fit of the elastic peak -->',A/
     +  4X,'shape function internal, exponential type'/
     +  4X,'fit range (meV) : ',F6.2,' to',F6.2,'  centered at',F6.2/
     +  14X,'Chi^2    peak   FWHM   pos.  %bgr.  asm. shape'/
     +  4X,'start:',9X,1PE9.2,0PF6.2,F7.3,F7.3,F6.2,F6.2)
*
*
*:  start the iteration
*
      NDF  = K2 - K1 + 1 - NFIT
      BNCV = .true.
      Do  I = 1,PITERA
*
*:  calculate theory and its derivatives, where
*:   DTH is the external theory function
*
       Do  N = 0,NFIT
        Do  K = K1,K2
         DTY(K,N) = DTH(N,DMX(K),DBETA,DP,DRRX,DRRY,KRD,BREXT)
        EndDo
       EndDo
*
*:  calculate Chi**2
*
       DZ8 = DZ
       DZ  = 0.0
       Do  K = K1,K2
        DZ = DZ + (DTY(K,0) - DMY(K))**2 /DMDY(K)
       EndDo
       If(I.eq.1) DZ8 = 2.D0*DZ
*
*:  calculate 1.derivative of Chi**2
*:  and its relative change in the iteration process
*
       DCG = 0.0
       Do  N = 1,NFIT
        DDZ(N) = 0.0
        Do  K = K1,K2
         DDZ(N) = DDZ(N) + (DTY(K,0) - DMY(K))*DTY(K,N)/DMDY(K)
        EndDo
        DDZ(N) = 2.0*DDZ(N)
        DCG    = DCG + DDZ(N)*DDP(N)
       EndDo
       DCG = DCG/DZ
*
*:  test convergence
*
       If(I.ne.1 .and. Dabs(DCG).le.DCVG) Then
        BNCV = .false.
        Exit
       Endif
*
*:  calculate 2.derivative of Chi**2
*
       Do  NC = 1,NFIT
        Do  NR = 1,NFIT
         DD2Z(NR,NC) = 0.0
         Do  K = K1,K2
          DD2Z(NR,NC) = DD2Z(NR,NC) + DTY(K,NR)*DTY(K,NC)/DMDY(K)
         EndDo
         DD2Z(NR,NC) = 2.0*DD2Z(NR,NC)
        EndDo
       EndDo
*
*:  calculate parameter corrections using the
*:    Levenberg-Marquardt method
*
       Do  NR = 1,NFIT
        DK(NR,1) = DDZ(NR)
        Do  NC = 1,NFIT
         DM(NR,NC) = DD2Z(NR,NC)
        EndDo
       EndDo
*
       DLVMQ = 1.D0 + 0.01D0*Dmax1(DZ8-DZ,0.D0)/Dfloat(NDF)
       Do  NR = 1,NFIT
        DM(NR,NR) = DLVMQ*DM(NR,NR)
       EndDo
*
       Call DLGS(PFIT,DM,DK,NFIT,1,DDET)
*
*:  limit parameter changes to stabilize convergence
*
       If(Dabs(DK(2,1)).gt.0.1D0)   DK(2,1) = Dsign(0.1D0,DK(2,1))
       If(Dabs(DK(3,1)).gt.0.5D0)   DK(3,1) = Dsign(0.5D0,DK(3,1))
       If(Dabs(DK(4,1)).gt.0.005D0) DK(4,1) = Dsign(0.005D0,DK(4,1))
*
*
*:  update parameter values
*
       Do  N = 1,NFIT
        DDP(N) = -DK(N,1)
        DP(N)  =  DP(N) + DDP(N)
       EndDo
*
      EndDo
*
      If(BREXT) Then
       Write(6,2000) DZ/Float(NDF),
     +            DP(1),2.D0*(DLN2/DP(2))**(1.D0/DP(7)),
     +            DP(3),DP(4)*1.D2,DP(6)
       Write(LUNPTL,2000) DZ/Float(NDF),DP(1),
     +            2.D0*(DLN2/DP(2))**(1.D0/DP(7)),
     +            DP(3),DP(4)*1.D2,DP(6)
      Else
       Write(6,2001) DZ/Float(NDF),
     +            DP(1),2.D0*(DLN2/DP(2))**(1.D0/DP(7)),
     +            DP(3),DP(4)*1.D2,DP(6),DP(7)
       Write(LUNPTL,2001) DZ/Float(NDF),DP(1),
     +            2.D0*(DLN2/DP(2))**(1.D0/DP(7)),
     +            DP(3),DP(4)*1.D2,DP(6),DP(7)
      Endif
*
 2000 Format(4X,'final:',F9.2,1PE9.2,0PF6.2,F7.3,F7.3,F6.2)
 2001 Format(4X,'final:',F9.2,1PE9.2,0PF6.2,F7.3,F7.3,F6.2,F6.2)
*
      If(BNCV) Then
       Write(6,2002) XT_RED,XT_DEF
       Write(LUNPTL,2002) '',''
      Endif
 2002 Format(4X,A,'--> no convergence...',A)
*
*:  calculate errors of fit parameters
*
      Do  NC = 1, NFIT
       Do  NR = 1, NFIT
        DM(NR,NC) = DD2Z(NR,NC)
        DK(NR,NC) = 0.D0
       EndDo
       DK(NC,NC) = DZ
      EndDo
*
      Call DLGS(PFIT,DM,DK,NFIT,NFIT,DDET)
*
      Do  N = 1, NFIT
       DDP(N) = Dsqrt(Dabs(DK(N,N)))
      EndDo
*
*:  print errors of results
*
      If(NFIT.eq.4) Then
       Write(LUNPTL,2003) 
     +   Dsqrt(DZ)/Float(NDF),DDP(1),
     +   2.D0*(DLN2/DP(2))**(1.D0/DP(7))*DDP(2)/(DP(2)*DP(7)),
     +   DDP(3),DDP(4)*1.D2
       Write(6,2003)
     +   Dsqrt(DZ)/Float(NDF),DDP(1),
     +   2.D0*(DLN2/DP(2))**(1.D0/DP(7))*DDP(2)/(DP(2)*DP(7)),
     +   DDP(3),DDP(4)*1.D2
      Else
       Write(LUNPTL,2004)
     +   Dsqrt(DZ)/Float(NDF),DDP(1),
     +   2.D0*(DLN2/DP(2))**(1.D0/DP(7))*DDP(2)/(DP(2)*DP(7)),
     +   DDP(3)
       Write(6,2004)
     +   Dsqrt(DZ)/Float(NDF),DDP(1),
     +   2.D0*(DLN2/DP(2))**(1.D0/DP(7))*DDP(2)/(DP(2)*DP(7)),
     +   DDP(3)
      Endif
*
 2003 Format(4X,'errors:',F8.2,1PE9.2,0PF6.2,F7.3,F7.3/)
 2004 Format(4X,'errors:',F8.2,1PE9.2,0PF6.2,F7.3/)
*
*
      End
