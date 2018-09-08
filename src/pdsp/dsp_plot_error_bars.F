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
      Subroutine DSP_PLOT_ERROR_BARS(RX11,RC,RLT,MS,RG,RX,RY,
     +                      RDX,RDY,KD,RXMIN,RXMAX,RYMIN,RYMAX)
*
* Purpose :  plot 2d data error bars in g2 graphics window
* Date    :  December 14, 2012
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    RX11        = X11 device pointer
*    RC          = line color
*    RLT         = line thickness
*    MS          = 0,1,2,3 line style solid,dashed,dotted,dash-dotted
*    RG(1),RG(2) = lower left corner of frame
*    RG(3),RG(4) = upper right corner of frame
*    RX(*)       = x-data
*    RY(*)       = y-data
*    RDX(*)      = x-errors
*    RDY(*)      = y-errors
*    KD          = nr. of data points
*    RXMIN,RXMAX = min/max x-value for plot
*    RYMIN,RYMAX = min/max y-value for plot
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Dimension  RG(*),RX(*),RY(*),RDX(*),RDY(*)
*
      RX1 = RG(1)
      RY1 = RG(2)
      RX2 = RG(3)
      RY2 = RG(4)
*
*: create plot
*
      RFX = (RX2-RX1)/(RXMAX-RXMIN)
      RFY = (RY2-RY1)/(RYMAX-RYMIN)
      Do 1  K = 1,KD
       If(RX(K).lt.RXMIN .or. RX(K).gt.RXMAX) GoTo 1
       If(RY(K).lt.RYMIN .or. RY(K).gt.RYMAX) GoTo 1
*
       If(RDX(K).gt.0.0) Then
        RXX1 = RX1 + RFX*(RX(K)-RDX(K)-RXMIN)
        RXX1 = Amax1(RX1,RXX1)
        RXX1 = Amin1(RX2,RXX1)
        RXX2 = RX1 + RFX*(RX(K)+RDX(K)-RXMIN)
        RXX2 = Amax1(RX1,RXX2)
        RXX2 = Amin1(RX2,RXX2)
        RYY  = RY1 + RFY*(RY(K)-RYMIN)
        Call DSP_LINE(RX11,RC,RLT,MS,RXX1,RYY,RXX2,RYY)
       Endif
*
       If(RDY(K).gt.0.0) Then
        RYY1 = RY1 + RFY*(RY(K)-RDY(K)-RYMIN)
        RYY1 = Amax1(RY1,RYY1)
        RYY1 = Amin1(RY2,RYY1)
        RYY2 = RY1 + RFY*(RY(K)+RDY(K)-RYMIN)
        RYY2 = Amax1(RY1,RYY2)
        RYY2 = Amin1(RY2,RYY2)
        RXX  = RX1 + RFX*(RX(K)-RXMIN)
        Call DSP_LINE(RX11,RC,RLT,MS,RXX,RYY1,RXX,RYY2)
       Endif
    1 Continue
*
*
      End
