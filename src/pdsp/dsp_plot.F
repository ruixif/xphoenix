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
      Subroutine DSP_PLOT(RX11,RC,RLT,MS,RG,RX,RY,KD,PD,
     +                      RXMIN,RXMAX,RYMIN,RYMAX)
*
* Purpose :  plot 2d data set in g2 graphics window
* Date    :  December 14, 2012
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    RX11        = X11 device pointer
*    RLT         = line thickness / symbol size
*    RC          = symbol/line color
*    RG(1),RG(2) = lower left corner of frame
*    RG(3),RG(4) = upper right corner of frame
*    MS          = 0,1,2,3,4> style line,dashed-line,dotted-line,
*                              dash-dotted-line,symbols
*    RX(*)       = x-data
*    RY(*)       = y-data
*    KD          = nr. of data points
*    RXMIN,RXMAX = min/max x-value for plot
*    RYMIN,RYMAX = min/max y-value for plot
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
*
      Dimension  RG(*),RX(*),RY(*),RP(2*PD),RD(4)
*
      RX1 = RG(1)
      RY1 = RG(2)
      RX2 = RG(3)
      RY2 = RG(4)
*
*:  set color
*
      Call G2_PEN(RX11,RC)
*
*:  scaling factors
*
      RFX = (RX2-RX1)/(RXMAX-RXMIN)
      RFY = (RY2-RY1)/(RYMAX-RYMIN)
*
*: create line plot
*
      If(MS.lt.4) Then
       If(MS.le.0) Then
        Call G2_SET_DASH(RX11,0.0,RD)
       ElseIf(MS.eq.1) Then
        RD(1) = 10.
        RD(2) = 5.
        Call G2_SET_DASH(RX11,2.0,RD)
       ElseIf(MS.eq.2) Then
        RD(1) = 2.
        RD(2) = 3.
        Call G2_SET_DASH(RX11,2.0,RD)
       ElseIf(MS.eq.3) Then
        RD(1) = 8.
        RD(2) = 3.
        RD(3) = 2.
        RD(4) = 3.
        Call G2_SET_DASH(RX11,4.0,RD)
       Endif
       Call G2_SET_LINE_WIDTH(RX11,RLT)
*
       KK = 0
       Do  K = 1,KD
        If(RX(K).lt.RXMIN .or. RX(K).gt.RXMAX) Cycle
        If(RY(K).lt.RYMIN .or. RY(K).gt.RYMAX) Cycle
*
        KK = KK + 1
        RP(2*KK-1) = RX1 + RFX*(RX(K)-RXMIN)
        RP(2*KK)   = RY1 + RFY*(RY(K)-RYMIN)
        If(KK.eq.PD) Exit
       EndDo
*
       RPP = Float(KK)
       Call G2_POLY_LINE(RX11,RPP,RP)
      EndIf
*
*:  create symbol plots
*
*:  circles
*
      If(MS.eq.4) Then
       Call G2_SET_DASH(RX11,0.0,RD)
       Do  K = 1,KD
        If(RX(K).lt.RXMIN .or. RX(K).gt.RXMAX) Cycle
        If(RY(K).lt.RYMIN .or. RY(K).gt.RYMAX) Cycle
*
        RXX = RX1 + RFX*(RX(K)-RXMIN)
        RYY = RY1 + RFY*(RY(K)-RYMIN)
        If(RLT.le.0.0) Then
         Call G2_PLOT(RX11,RXX,RYY)
        Else
         Call G2_FILLED_CIRCLE(RX11,RXX,RYY,RLT)
        Endif
       EndDo
      Endif
*
*:  rectangles
*
      If(MS.eq.5) Then
       Call G2_SET_DASH(RX11,0.0,RD)
       Do  K = 1,KD
        If(RX(K).lt.RXMIN .or. RX(K).gt.RXMAX) Cycle
        If(RY(K).lt.RYMIN .or. RY(K).gt.RYMAX) Cycle
*
        RXX = RX1 + RFX*(RX(K)-RXMIN)
        RYY = RY1 + RFY*(RY(K)-RYMIN)
        If(RLT.le.0.0) Then
         Call G2_PLOT(RX11,RXX,RYY)
        Else
         RXX1 = RXX - RLT
         RYY1 = RYY - RLT
         RXX2 = RXX + RLT
         RYY2 = RYY + RLT
         Call G2_FILLED_RECTANGLE(RX11,RXX1,RYY1,RXX2,RYY2)
        Endif
       EndDo
      Endif
*
*:  triangles up
*
      If(MS.eq.6) Then
       Call G2_SET_DASH(RX11,0.0,RD)
       Do  K = 1,KD
        If(RX(K).lt.RXMIN .or. RX(K).gt.RXMAX) Cycle
        If(RY(K).lt.RYMIN .or. RY(K).gt.RYMAX) Cycle
*
        RXX = RX1 + RFX*(RX(K)-RXMIN)
        RYY = RY1 + RFY*(RY(K)-RYMIN)
        If(RLT.le.0.0) Then
         Call G2_PLOT(RX11,RXX,RYY)
        Else
         RXX1 = RXX
         RYY1 = RYY + 2.0*RLT
         RXX2 = RXX - 1.7321*RLT
         RYY2 = RYY - RLT
         RXX3 = RXX + 1.7321*RLT
         RYY3 = RYY - RLT
         Call G2_FILLED_TRIANGLE(RX11,RXX1,RYY1,RXX2,RYY2,RXX3,RYY3)
        Endif
       EndDo
      Endif
*
*:  triangles down
*
      If(MS.eq.7) Then
       Call G2_SET_DASH(RX11,0.0,RD)
       Do  K = 1,KD
        If(RX(K).lt.RXMIN .or. RX(K).gt.RXMAX) Cycle
        If(RY(K).lt.RYMIN .or. RY(K).gt.RYMAX) Cycle
*
        RXX = RX1 + RFX*(RX(K)-RXMIN)
        RYY = RY1 + RFY*(RY(K)-RYMIN)
        If(RLT.le.0.0) Then
         Call G2_PLOT(RX11,RXX,RYY)
        Else
         RXX1 = RXX
         RYY1 = RYY - 2.0*RLT
         RXX2 = RXX - 1.7321*RLT
         RYY2 = RYY + RLT
         RXX3 = RXX + 1.7321*RLT
         RYY3 = RYY + RLT
         Call G2_FILLED_TRIANGLE(RX11,RXX1,RYY1,RXX2,RYY2,RXX3,RYY3)
        Endif
       EndDo
      Endif
*
*
      End
