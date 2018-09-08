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
*  Free Software Foundation.
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
      Function DSP_COLOR(RX11,RTMP1,RTMP2,RTMP)
*
* purpose :  return thermal color allocation
* date    :  April 9, 2013
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    RX11     = X11 device pointer
*    RTMP1    = lowest temperature
*    RTMP2    = highest temperature
*    RTMP     = temperature
*
* export variables :
*    DSP_COLOR = allocated color pointer
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      REAL*4  DSP_COLOR
*
*
      If(RTMP1.eq.RTMP2) Then
       RR = 1.0
       RG = 0.66
       RB = 0.33
       GoTo 100
      Endif
*
*: reference temperatures for red, green, blue
*
      RRT = 4.*RTMP1
      RGT = 2.*(RTMP1 + RTMP2)
      RBT = 4.*RTMP2
*
*:  Planck distribution
*
      RR = Exp(-RRT/RTMP)*RRT**3
      RG = Exp(-RGT/RTMP)*RGT**3
      RB = Exp(-RBT/RTMP)*RBT**3
*
      RM = Amin1(RR,RG,RB)
      RS = RR + RG + RB
      RR = (RR-RM)/RS
      RG = (RG-RM)/RS
      RB = (RB-RM)/RS
*
      RS = RR + RG + RB
      RR = RR/RS
      RG = RG/RS
      RB = RB/RS
*
      RM = Amax1(RR,RG,RB)
      RR = RR + 1. - RM
      RG = RG + 1. - RM
      RB = RB + 1. - RM
*
*:  allocate color
*
  100 Continue
      DSP_COLOR = G2_INK(RX11,RR,RG,RB)
*
*
      End
