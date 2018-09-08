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
       Subroutine SAVHDR(LUN,BM,XX,XY)
*
* purpose :  output a generic header
* date    :  April 13, 2016
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    LUN    = logical unit number
*    BM     = .true. if errors present
*    XX     = x-axis label
*    XY     = y-axis label
*    XOPT   = list of output options
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
      Character*(*) XX,XY
*
*
      If(Index(XOPT,'G').ne.0) Then
       Write(LUN,2000) XDATE,XTIME,XZONE,XX,XY
       If(BM) Then
        Write(LUN,2001)
       Else
        Write(LUN,2002)
       Endif
      Endif
*
 2000 Format('@page size 600,600'/
     +       '@view 0.12,0.1,0.97,0.93'/
     +       '@subtitle "PHOENIX module padd, ',A,' at ',A,1X,A,'"'/
     +       '@xaxes scale Normal'/
     +       '@yaxes scale Normal'/
     +       '@xaxis label "',A,'"'/
     +       '@yaxis label "',A,'"')
 2001 Format('@s0 errorbar size 0.0'/'@type xydy')
 2002 Format('@type xy')
*
      If(Index(XOPT,',').ne.0) Then
       If(BM) Then
        Write(LUN,2003) XDATE,XTIME,XZONE,XX,XY,XY
       Else
        Write(LUN,2004) XDATE,XTIME,XZONE,XX,XY
       Endif
      Endif
*
 2003 Format('PHOENIX module padd; ',A,' at ',A,1X,A/
     +         A,',',A,',error of ',A)
 2004 Format('PHOENIX module padd; ',A,' at ',A,1X,A/A,',',A)
*
*
      End
