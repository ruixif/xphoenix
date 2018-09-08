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
      SUBROUTINE DBFIT(DX,DY,DDY,DF,KD,
     +                   DP0,DP1,DP2,DV0,DV1,DV2,DV12,DCHI0,DCHI2)
*
* purpose :  fit to the input data with special quadratic function
* date    :  January 7, 2011
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DX(*)    = x-values
*    DY(*)    = y-values
*    DDY(*)   = y-errors
*    DF(*)    = y-values smoothed
*    KD       = number of values in above arrays
*
* export variables :
*    DP0      = correction coefficient from linear fit
*    DP1      = linear correction coefficient from quadratic fit
*    DP2      = quadratic correction coefficient from quadratic fit
*    DV0      = variance of DP0
*    DV1      = variance of DP1
*    DV2      = variance of DP2
*    DV12     = correlated variance of DP1 and DP2
*    DCHI0    = Chi^2 of linear fit
*    DCHI2    = Chi^2 of quadratic fit
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
*
      Dimension  DX(*),DY(*),DDY(*),DF(*)
*
*
      DP0   = 0.D0
      DV0   = 1.D99
      DP1   = 0.D0
      DV1   = 1.D99
      DP2   = 0.D0
      DV2   = 1.D99
      DV12  = 1.D99
      DCHI2 = 0.D0
*
      IF(KD.lt.3) Return
*
*:  calculate weighted sums
*
      DSPB = 0.D0
      DSQB = 0.D0
      DSP  = 0.D0
      DSP2 = 0.D0
      DSQ  = 0.D0
      DSQ2 = 0.D0
      DSPQ = 0.D0
      Do 1  K = 1,KD
       DT = Dtanh(DX(K))
       DB = (DY(K) - DT)/(1.D0 - DT*DT)
       DP = -DX(K)
       DQ = DF(K)*DX(K)*DX(K)
       DW = ((1.D0 - DT*DT)/DDY(K))**2
*
       DSPB = DSPB + DP*DB*DW
       DSQB = DSQB + DQ*DB*DW
       DSP  = DSP  + DP*DW
       DSP2 = DSP2 + DP*DP*DW
       DSQ  = DSQ  + DQ*DW
       DSQ2 = DSQ2 + DQ*DQ*DW
       DSPQ = DSPQ + DP*DQ*DW
    1 Continue
*
*:  parameters
*
      DP0 = DSPB/DSP2
      DET = DSP2*DSQ2 - DSPQ*DSPQ
      DP1 = (DSPB*DSQ2 - DSQB*DSPQ)/DET
      DP2 = (DSQB*DSP2 - DSPB*DSPQ)/DET
*
*:  Chi^2
*
      DCHI0 = 0.D0
      DCHI2 = 0.D0
      Do 2  K = 1,KD
       DT = Dtanh(DX(K))
       DB = (DY(K) - DT)/(1.D0 - DT*DT)
       DP = -DX(K)
       DQ = DF(K)*DX(K)*DX(K)
       DW = ((1.D0 - DT*DT)/DDY(K))**2
*
       DCHI0 = DCHI0 + DW*(DB - DP1*DP)**2
       DCHI2 = DCHI2 + DW*(DB - DP1*DP - DP2*DQ)**2
    2 Continue
      DCHI0 = DCHI0/Dfloat(KD-1)
      DCHI2 = DCHI2/Dfloat(KD-2)
*
*:  variances
*
      DV0  =  DCHI0/DSP2
      DV1  =  DCHI2*DSQ2/DET
      DV2  =  DCHI2*DSP2/DET
      DV12 = -DCHI2*DSPQ/DET
*
*
      END
