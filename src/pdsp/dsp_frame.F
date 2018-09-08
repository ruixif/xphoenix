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
      Subroutine DSP_FRAME(RX11,RCOLOR,RLT,RFSZ,RG,XTITLE)
*
* Purpose :  create frame and axis labeling in g2 graphics window
* Date    :  December 14, 2012
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    RX11        = X11 device pointer
*    RLT         = line thickness
*    RCOLOR      = frame color
*    RFSZ        = font size for frame descriptor
*    RG(1),RG(2) = lower left corner of frame
*    RG(3),RG(4) = upper right corner of frame
*    XTITLE      = frame descriptor
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Character*(*) XTITLE
*
      Dimension  RG(*)
*
      RX1 = RG(1)
      RY1 = RG(2)
      RX2 = RG(3)
      RY2 = RG(4)
*
*: create frame
*
      Call G2_PEN(RX11,RCOLOR)
      Call G2_SET_LINE_WIDTH(RX11,RLT)
      Call G2_RECTANGLE(RX11,RX1,RY1,RX2,RY2)
*
*:  create frame description
*
      RX = RX2 - 100.
      RY = RY2 + 0.2*RFSZ
      Call DSP_TEXT(RX11,RCOLOR,RFSZ,0,RX,RY,XTITLE)
*
*
      End
