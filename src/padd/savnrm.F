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
      Subroutine SAVNRM(NERR)
*
* purpose :  save normalization data to file
*             '<output file prefix>_nrm.<ext>'
* date    :  November 7, 1997
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    XOUT   = output file prefix
*
* export variables :
*    NERR   = error code
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
      Character*4  XEXT
      Character*72 XFILE,XF2
*
      Save XFILE
*
*
*:  set error code
*
      NERR = 0
*
      If(Index(XOPT,',').eq.0) Then
       XF2  = XFORM2
       XEXT = '.dat'
      Else
       XF2  = XFORM2C
       XEXT = '.csv'
      Endif
*
*:  open file
*
      Inquire(LUNTMP+1,OPENED=BOPEN)
      If(BOPEN) Then
       Write(LUNTMP+1,*)
      Else
       XFILE = XOUT
       Call XMERGE(XFILE,'_nrm'//XEXT)
       Open(UNIT=LUNTMP+1,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       B = .false.
       Call SAVHDR(LUNTMP+1,B,'energy (meV)','normalization function')
      Endif
*
*:  save data
*
      Do  K = 1,KD
       Write(LUNTMP+1,XF2,IOSTAT=IERR,ERR=999) DMX(K),DMN(K)
      EndDo
*
      Return
*
*:  IO error exits
*
  998 Continue
      NERR  = 7701
      NCOM1 = IERR
      XMSG1 = XFILE
      Return
*
  999 Continue
      NERR  = 7702
      NCOM1 = IERR
      XMSG1 = XFILE
*
*
      End
