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
      Subroutine DSP_XAXIS(RX11,RCOLOR,RLT,RFSZ,
     +                      RG,RMIN,RMAX,NTICK,ML,MTL,MT,MG,XAXIS)
*
* Purpose :  create frame and axis labeling in g2 graphics window
* Date    :  December 14, 2012
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    RX11        = X11 device pointer
*    RCOLOR      = frame color
*    RLT         = line thickness
*    RFSZ        = font size for labels
*    RG(1),RG(2) = lower left corner of frame
*    RG(3),RG(4) = upper right corner of frame
*    RMIN        = minimum x-value
*    RMAX        = maximum x-value
*    NTICK       = maximum nr. of ticks
*    ML          = 0,1,2,3 axis label on none,bottom,top,both
*    MTL         = 0,1,2,3 tick labels on none,bottom,top,both
*    MT          = 0,1,2,3 ticks on none,bottom,top,both
*    MG          = 0,1 grid lines no,yes
*    XAXIS       = x-axis label
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Character*(*) XAXIS
      Character*30  XFORM,XLABEL
*
      Dimension  RG(*)
*
      External  LENX
*
      RX1    = RG(1)
      RY1    = RG(2)
      RX2    = RG(3)
      RY2    = RG(4)
      RXSIZE = RX2 - RX1
      RYSIZE = RY2 - RY1
      RANGE  = RMAX - RMIN
*
*:  style settings
*
      BLT = .false.
      BLB = .false.
      If(ML.eq.1 .or. ML.eq.3) BLB = .true.
      If(ML.eq.2 .or. ML.eq.3) BLT = .true.
*
      BTLT = .false.
      BTLB = .false.
      If(MTL.eq.1 .or. MTL.eq.3) BTLB = .true.
      If(MTL.eq.2 .or. MTL.eq.3) BTLT = .true.
*
      BTT = .false.
      BTB = .false.
      If(MT.eq.1 .or. MT.eq.3) BTB = .true.
      If(MT.eq.2 .or. MT.eq.3) BTT = .true.
*
      BGR = MG.gt.0
*
*:  create x-axis label
*
      RX = RX1 + 0.5*RXSIZE - 0.2*Float(LENX(XAXIS,' '))*RFSZ
      If(BLB) Then
       If(BTLB) Then
        RY = RY1 - 2.5*RFSZ
       Else
        RY = RY1 - 1.2*RFSZ
       Endif
       Call DSP_TEXT(RX11,RCOLOR,RFSZ,0,RX,RY,XAXIS)
      Endif
*
      If(BLT) Then
       If(BTLT) Then
        RY = RY2 + 1.5*RFSZ
       Else
        RY = RY2 + 0.4*RFSZ
       Endif
       Call DSP_TEXT(RX11,RCOLOR,RFSZ,0,RX,RY,XAXIS)
      Endif
*
*:  tick marks and labels
*
*:  step size
*
      RH1 = RANGE/Float(NTICK)
      NHL = Int(Alog10(Abs(RH1)))
      If(RH1.le.1.0)  NHL = NHL-1
      RH1 = 10.0**Amod(Alog10(Abs(RH1)),1.0)
      If(NHL.lt.0)  RH1 = RH1*10.0
      STP = 1.0
      If(RH1.ge.1.0)  STP = 2.0
      If(RH1.ge.2.0)  STP = 5.0
      If(RH1.ge.5.0)  STP = 10.0
      If(NHL.lt.0) Then
       STP = STP/10**Iabs(NHL)
      Else
       STP = STP*10**NHL
      Endif
*
*:  accuracy for small step size
*
      If(STP.lt.1.0) Then
       NL2 = Iabs(Int(Alog10(Abs(STP)))) + 1
       NL1 = Int(Alog10(Amax1(Abs(RMIN),Abs(RMAX)))) + 1
       If(Abs(Amod(Alog10(Abs(STP)),1.0)).lt.0.001) Then
        NL2 = NL2 - 1
       Endif
       If(NL2.lt.0) Then
        NL2 = Max0(NL2,Iabs(NL1))
        NL1 = NL2 + 2
       Else
        NL1 = NL2 + NL1 + 2
       Endif
      Else
       NL1 = 3
       NL2 = 1
      Endif
*
      If(RMIN.ge.0.0) Then
       RTX0 = RXSIZE*(STP-Amod(RMIN,STP))/RANGE
       L0   = Nint((RMIN-Amod(RMIN,STP))/STP) + 1
      Else
       RTX0 = -RXSIZE*Amod(RMIN,STP)/RANGE
       L0   = Nint((RMIN-Amod(RMIN,STP))/STP)
      Endif
      RDTX = RXSIZE*STP/RANGE
      LMAX = Nint(RANGE/STP) + 1
      Do 1  L = 1,LMAX
       RTMX = RX1 + RTX0 + Float(L-1)*RDTX
       If(RTMX.gt.RX2 .or. RTMX.lt.RX1) GoTo 1
       If(BTB)  Call DSP_LINE(RX11,RCOLOR,RLT,0,RTMX,RY1,RTMX,RY1+10.0)
       If(BTT)  Call DSP_LINE(RX11,RCOLOR,RLT,0,RTMX,RY2,RTMX,RY2-10.0)
       If(BGR)  Call DSP_LINE(RX11,RCOLOR,RLT,2,
     +                         RTMX,RY1+12.0,RTMX,RY2-12.0)
*
       If(STP.ge.1.0) Then
        NLABEL = Nint(STP)*(L-1+L0)
        If(NLABEL.eq.0) Then
         XLABEL = '0'
         RTMXD  = 0.2*RFSZ
        Else
         NLL = Int(Alog10(Float(Iabs(NLABEL)))) + 1
         If(NLABEL.lt.0)  NLL = NLL+1
         Write(XFORM,2001) NLL
         RTMXD = 0.2*Float(NLL)*RFSZ
         Write(XLABEL,XFORM) NLABEL
        Endif
       Else
        RLABEL = STP*Float(L-1+L0)
        If(RLABEL.eq.0) Then
         XLABEL = '0'
         RTMXD = 0.2*RFSZ
        Else
         If(RLABEL.lt.0.0) Then
          NLL1 = NL1 + 1
         Else
          NLL1 = NL1
         Endif
         Write(XFORM,2002) NLL1,NL2
         Write(XLABEL,XFORM) RLABEL
         RTMXD = 0.2*Float(NLL1)*RFSZ
        Endif
       Endif
*
       If(BTLB) Call DSP_TEXT(RX11,RCOLOR,0.9*RFSZ,0,
     +                         RTMX-RTMXD,RY1-1.2*RFSZ,XLABEL)
       If(BTLT) Call DSP_TEXT(RX11,RCOLOR,0.9*RFSZ,0,
     +                         RTMX-RTMXD,RY2+0.2*RFSZ,XLABEL)
    1 Continue
*
 2001 Format('(I',I2.2,')')
 2002 Format('(F',I2.2,'.',I2.2,')')
*
*
      End
