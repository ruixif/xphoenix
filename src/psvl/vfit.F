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
      Subroutine VFIT(PD,KD,DX,DY,DDY,BVALID,
     +                 NTYPE,PMAX,DP,DPMIN,DPMAX,DDPMAX,DDP,BFIT,
     +                 NITMAX,DCVG,LUN,DCHI,NERR)
*
* purpose :  optimize a set of parameters that are arguments to
*             the fit FUNCTION DTHRY by minimization of the
*             mean square deviation of the data set (DX,DY)
*             with repect to this theory
* date    :  March 25, 2006
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    PD        = max number of data points
*    KD        = total number of data points
*    DX(*)     = x-value of data
*    DY(*)     = y-value of data
*    DDY(*)    = y-error^2 of data
*    BVALID(*) = ignore data points for which BVALID=.FALSE.
*    NTYPE     = type of theory function to use
*    PMAX      = maximum number of paramters
*    DP(*)     = start values for parameters
*    DPMIN(*)  = lower bound for parameters
*    DPMAX(*)  = upper bound for parameters
*    DDPMAX(*) = maximal relative change per iteration step
*    BFIT(*)   = fit only parameters for which BFIT=.TRUE.
*    NITMAX    = maximum number of iterations in fit loop
*    DCVG      = convergence criterium
*    LUN       = Fortran unit for printout
*
* export variables :
*    DP(*)   = fitted parameters
*    DDP(*)  = errors of fitted parameters
*    DCHI    = Chi^2 value
*    NERR    = error code
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
*
      Parameter(PCODE = 99)
*
      Dimension DX(*),DY(*),DDY(*),BVALID(*),
     +          DP(*),DPMIN(*),DPMAX(*),DDPMAX(*),DDP(*),BFIT(*),
     +          IPX(0:PMAX),DTY(PD,0:PMAX),
     +          DDZ(PMAX),DD2Z(PMAX,PMAX),
     +          DK(PMAX,PMAX),DM(PMAX,PMAX)
*
*
      NERR = 0
*
*:  number of valid data points
*
      KDV = 0
      K1  = KD
      K2  = 0
      Do  K = 1,KD
       If(BVALID(K)) Then
        KDV = KDV + 1
        K1  = Min0(K,K1)
        K2  = Max0(K,K2)
       Endif
      EndDo
*
*:  no valid data, return
*
      If(KDV.eq.0) Then
       NERR = PCODE*1000 + 700
       Return
      Endif
*
*:  total number of parameters requested by theory function
*
      NP = Idnint(DTHRY(NTYPE,DX,DP,-1))
*
*:  no parameters, return
*
      If(NP.le.0) Then
       NERR = PCODE*1000 + 701
       Return
      Endif
*
*:  too many parameters, return
*
      If(NP.gt.PMAX) Then
       NERR = PCODE*1000 + 702
       Return
      Endif
*
*:  determine total number of fit paramters
*:  index fit paramters in parameter vector
*:  initialize DDP array (parameter errors)
*:  check parm start values
*
      NF     = 0
      IPX(0) = 0
      Do  N = 1,NP
       DDP(N) = 0.D0
       If(BFIT(N)) Then
        NF      = NF + 1
        IPX(NF) = N
        DP(N)   = Dmin1(DP(N),DPMAX(N))
        DP(N)   = Dmax1(DP(N),DPMIN(N))
       Endif
      EndDo
*
*:  too few data points for fitting, return
*
      If(NF.ge.KDV) Then
       NERR = PCODE*1000 + 703
       Return
      Endif
*
*:  nothing to fit, calculate Chi^2 only, return
*
      If(NF.eq.0) Then
       DZ  = 0.0
       Do  K = 1,KD
        If(BVALID(K)) Then
         DZ  = DZ + (DTHRY(NTYPE,DX(K),DP,0) - DY(K))**2 /DDY(K)
        Endif
       EndDo
       DCHI = DZ/Dfloat(KDV)
      Endif
*
*:  silent operation if BPRT=.FALSE.
*
      BPRT = LUN.gt.0 .and. LUN.lt.100
*
*:  print actual values of parameters
*
      If(BPRT) Then
       Write(LUN,2000) DX(K1),DX(K2),NTYPE,NP,NF,KDV
       Write(LUN,2001) DCHI,(DP(N),N=1,NP)
      Endif
*
 2000  Format(/2X,'Fit range',F5.1,' to',F5.1,' meV -->'/
     +        3X,'Type of fit function        :  ',I2/
     +        3X,'Number of parameters        :  ',I2/
     +        3X,'Number of fit parameters    :  ',I2/
     +        3X,'Number of valid data points :',I4//
     +        6X,'Chi^2  quality',5X,'p1',8X,'p2',8X,'p3...')
 2001  Format(3X,F8.2,9X,1P,5D10.2)
*
      If(NF.eq.0) Return
*
*:  start the iteration
*
      DZ    = 0.0
      BDONE = .FALSE.
      Do  I = 1,NITMAX
*
*:  calculate theory and its derivatives, where
*:   DTHRY is the external theory function
*
       Do  N = 0,NF
        Do  K = 1,KD
         If(BVALID(K)) DTY(K,N) = DTHRY(NTYPE,DX(K),DP,IPX(N))
        EndDo
       EndDo
*
*:  calculate Chi^2 (not normalized)
*
       DZ = 0.0
       Do  K = 1,KD
        If(BVALID(K)) Then
         DZ = DZ + (DTY(K,0) - DY(K))**2 /DDY(K)
        Endif
       EndDo
*
       DCHI8 = DCHI
       DCHI  = DZ/Dfloat(KDV-NF)
       If(I.eq.1) DCHI8 = 2.D0*DCHI
*
*:  calculate 1.derivative of Chi^2
*:  and its relative change in the iteration process
*
       DCG = 0.0
       Do  N = 1,NF
        DDZ(N) = 0.0
        Do  K = 1,KD
         If(BVALID(K)) Then
          DDZ(N) = DDZ(N) + (DTY(K,0) - DY(K))*DTY(K,IPX(N))/DDY(K)
         Endif
        EndDo
        DDZ(N) = 2.D0*DDZ(N)
        DCG    = DCG + DDZ(N)*DDP(IPX(N))
       EndDo
       DCG = DCG/DZ
*
*:  print actual values of parameters
*
       If(BPRT) Then
        Write(LUN,2002) DCHI,DCG,(DP(N),N=1,NP)
       Endif
 2002  Format(3X,F8.2,1P,D9.1,5D10.2)
*
*:  test convergence
*
       If(I.ne.1 .and. Dabs(DCG).le.DCVG) Then
        BDONE = .true.
        Exit
       Endif
*
*:  calculate approximate 2.derivative of Chi^2
*
       Do  NC = 1,NF
        Do  NR = 1,NF
         DD2Z(NR,NC) = 0.0
         Do  K = 1,KD
          If(BVALID(K)) Then
           DD2Z(NR,NC) = DD2Z(NR,NC) + DTY(K,NR)*DTY(K,NC)/DDY(K)
          Endif
         EndDo
         DD2Z(NR,NC) = 2.0*DD2Z(NR,NC)
        EndDo
       EndDo
*
*:  calculate parameter corrections using the
*:    Levenberg-Marquardt method
*
       Do  NR = 1,NF
        DK(NR,1) = DDZ(NR)
        Do  NC = 1,NF
         DM(NR,NC) = DD2Z(NR,NC)
        EndDo
       EndDo
*
       DLVMQ = 1.D0 + 0.1D0*Dmax1(DCHI8-DCHI,0.D0)
       Do  NR = 1,NF
        DM(NR,NR) = DLVMQ*DM(NR,NR)
       EndDo
*
       Call DLGS(PMAX,DM,DK,NF,1,DDET)
*
*:  update parameter values
*:  apply boundaries
*:  limit amount of change
*
       Do  N = 1,NF
        DP9 = DP(IPX(N)) - DK(N,1)
        DP9 = Dmax1(DP9,DPMIN(IPX(N)))
        DP9 = Dmin1(DP9,DPMAX(IPX(N)))
*
        DDEFF = DP9 - DP(IPX(N))
        If(Dabs(DDEFF).gt.DDPMAX(IPX(N))*Dabs(DP(IPX(N)))) Then
         DP9 = DP(IPX(N)) + DDPMAX(IPX(N))*DDEFF
        Endif
*
        DDP(IPX(N)) = DP9 - DP(IPX(N))
        DP(IPX(N))  = DP9
       EndDo
*
      EndDo
*
*:  calculate errors of fit parameters
*
      Do  NC = 1,NF
       Do  NR = 1,NF
        DM(NR,NC) = DD2Z(NR,NC)
        DK(NR,NC) = 0.D0
       EndDo
       DK(NC,NC) = 2.D0*Dmax1(DCHI,1.D0)
      EndDo
*
*:  calculate parameter errors
*
      Call DLGS(PMAX,DM,DK,NF,NF,DDET)
*
      Do  N = 1,NF
       DDP(IPX(N)) = Dsqrt(Dabs(DK(N,N)))
      EndDo
*
*:  print errors of results
*
      If(BPRT) Then
       DDCHI = Dsqrt(DZ)/Dfloat(KDV-NF)
       Write(LUN,2003) DDCHI,(DDP(N),N=1,NP)
      Endif
*
 2003 Format(4X,'+-',F5.2,9X,1P,5D10.2/)
*
*:  abnormal exit
*
      If(.not.BDONE) Then
       NERR = PCODE*1000 + 100
       If(BPRT) Write(LUN,2004)
 2004  Format(5X,'no convergence...'/)
      Endif
*
*
      End
