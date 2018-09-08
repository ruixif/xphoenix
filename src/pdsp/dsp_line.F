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
      Subroutine DSP_LINE(RX11,RCOLOR,RLT,MS,RX0,RY0,RX1,RY1)
*
* purpose :  draw a dotted line using G2 graphics tools
* date    :  November 20, 2012
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    RX11     = X11 device pointer
*    RCOLOR   = line color
*    RLT      = line thickness
*    MS       = 0,1,2,3 line style solid,dashed,dotted,dash-dotted
*    RX0,RY0  = starting point for line
*    RX1,RY1  = end point for line
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Dimension  RD(4)
*
      If(RLT.le.0.0) Return
*
*: set color and line width
*
      Call G2_PEN(RX11,RCOLOR)
      Call G2_SET_LINE_WIDTH(RX11,RLT)
*
      If(MS.le.0) Then
       Call G2_SET_DASH(RX11,0.0,RD)
       Call G2_LINE(RX11,RX0,RY0,RX1,RY1)
       Return
      Endif
*
      If(MS.eq.1) Then
       RD(1) = 10.
       RD(2) = 5.
       Call G2_SET_DASH(RX11,2.0,RD)
       Call G2_LINE(RX11,RX0,RY0,RX1,RY1)
       Return
      Endif
*
      If(MS.eq.2) Then
       RR = Sqrt((RX1-RX0)**2 + (RY1-RY0)**2)
       RC = (RX1-RX0)/RR
       RS = (RY1-RY0)/RR
       Do 1  I = 0,Nint(RR),3
        RX = RX0 + Float(I)*RC
        RY = RY0 + Float(I)*RS
        Call G2_PLOT(RX11,RX,RY)
    1  Continue
       Return
      Endif
*
      If(MS.eq.3) Then
       RD(1) = 8.
       RD(2) = 3.
       RD(3) = 2.
       RD(4) = 3.
       Call G2_SET_DASH(RX11,4.0,RD)
       Call G2_LINE(RX11,RX0,RY0,RX1,RY1)
       Return
      Endif
*
*
      End
