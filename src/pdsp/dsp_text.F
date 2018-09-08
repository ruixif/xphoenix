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
      Subroutine DSP_TEXT(RX11,RCOLOR,RFSZ,MS,RX0,RY0,XTEXT)
*
* purpose :  place text using G2 graphics tools
* date    :  December 15, 2012
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    RX11     = X11 device pointer
*    RCOLOR   = text color
*    RFSZ     = font size
*    MS       = 0,1 display text horizontally,vertically
*    RX0,RY0  = position for text
*    XTEXT    = text
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Character*(*) XTEXT
      Character*200 XT
*
      External  LENX
*
*: set color and font size
*
      Call G2_PEN(RX11,RCOLOR)
      Call G2_SET_FONT_SIZE(RX11,RFSZ)
*
*:  analyze placement commands
*
      RX = RX0
      RY = RY0
*
      L  = Len(XTEXT)
      LT = Len(XT)
      LM = Min0(L,LT)
      XT = XTEXT(1:LM)
*
      Do While (Index(XT,'\').ne.0)
       IB = Index(XT,'\')
       I1 = Index(XT,'}')
       Read(XT(IB+3:I1-1),*,End=100,Err=100) RD
       If(XT(IB+1:IB+2).eq.'h{') RX = RX + RD
       If(XT(IB+1:IB+2).eq.'v{') RY = RY - RD
  100  Continue
       XT(1:LT-I1) = XT(I1+1:LT)
      EndDo
*
*:  place text
*
      If(MS.le.0) Then
       Call G2_STRING(RX11,RX,RY,XT)
      Else
       LA = LENX(XT,' ')
       Do 1  L = 1,LA
        Call G2_STRING(RX11,RX,RY,XT(L:L))
        RY = RY - 0.85*RFSZ
    1  Continue
      Endif
*
*
      End
