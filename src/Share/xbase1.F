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
      Subroutine XBASE1(XIN,XOUT)
*
* purpose :  remove leading directory name
* date    :  October 28, 2016
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    XIN   = file name with directory
*
* export variables :
*    XOUT  = file name, leading directories removed
*
*
#include<implici.h>
*
      Character*(*) XIN,XOUT
*
      LIN  = Len(XIN)
      LOUT = Len(XOUT)
      Call XFILL(XOUT,1,LOUT,' ')
      L = Min0(LIN,LOUT)
*
*:  nothing to remove
*
      If(Index(XIN,'/').eq.0) Then
       XOUT(1:L) = XIN(1:L)
       Return
      Endif
*
*:  position of the '/'
*
      LSLASH = 0
      Do  J = LIN,1,-1
       If(XIN(J:J).eq.'/') Then
        LSLASH = J
        Exit
       Endif
      EndDo
*
*:  remove dirname
*
      If(LSLASH.lt.L) Then
       XOUT(1:L-LSLASH) = XIN(LSLASH+1:L)
      Endif
*
*
      END
