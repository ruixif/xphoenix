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
      Subroutine SAVDAT(NERR)
*
*
* purpose :  save calculated data
* date    :  Ocotber 25, 2014
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    K1      = index, beginning of fit range
*    K2      = index, end of fit range
*    DSX(*)  = energy scale
*    DSY(*)  = Debye sound velocities from input data
*    DSDY(*) = error**2 of Debye sound velocities
*    DSVT(*) = fitted Debye sound velocities
*    DDSVT(*) = error of fitted Debye sound velocities
*
* export variables :
*    NERR  = error code
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<psvl.h>
*
      Character*4   XEXT
      Character*72  XFILE,XF2,XF3
*
*
      NERR = 0
*
      If(Index(XOPT,',').eq.0) Then
       XF2  = XFORM2
       XF3  = XFORM3
       XEXT = '.dat'
      Else
       XF2  = XFORM2C
       XF3  = XFORM3C
       XEXT = '.csv'
      Endif
*
*:  save Debye sound velocities
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_vel'//XEXT)
      Open(UNIT=LUNTMP,FILE=XFILE,IOSTAT=IERR,ERR=999)
*
      B = .true.
      Call SAVHDR(LUNTMP,B,'energy (meV)',
     +                 'pseudo Debye sound velocity (km/s)')
*
      Do  K = 2,KVEL
       Write(LUNTMP,XF3) DSX(K),1.D-3*DSY(K),1.D-3*Dsqrt(DSDY(K))
      EndDo
      Close(UNIT=LUNTMP)
*
*:  save fit results
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_vet'//XEXT)
      Open(UNIT=LUNTMP,FILE=XFILE,IOSTAT=IERR,ERR=999)
*
      B = .false.
      Call SAVHDR(LUNTMP,B,'energy (meV)',
     +                 'fits to pseudo Debye sound vel. (km/s)')
*
      Do  J = 1,NWW
       Do  K = 1,KIX(J)
        Write(LUNTMP,XF2) DSX(K),1.D-3*DTTY(K,J)
       EndDo
       If(J.ne.NWW) Write(LUNTMP,*)
      EndDo
      Close(UNIT=LUNTMP)
*
*:  save chi^2 values
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_chi'//XEXT)
      Open(UNIT=LUNTMP,FILE=XFILE,IOSTAT=IERR,ERR=999)
*
      Call SAVHDR(LUNTMP,B,'fit window width (meV)','chi^2 values')
*
      Do  J = 1,NWW
       Write(LUNTMP,XF2) DWW(J),DCHIT(J)
      EndDo
      Close(UNIT=LUNTMP)
*
*:  save sound velocities
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_vld'//XEXT)
      Open(UNIT=LUNTMP,FILE=XFILE,IOSTAT=IERR,ERR=999)
*
      B = .true.
      Call SAVHDR(LUNTMP,B,'fit window width (meV)',
     +                 'Debye sound velocity (km/s)')
*
      Do  J = 1,NWW
       Write(LUNTMP,XF3) DWW(J),1.D-3*DSVT(J),1.D-3*DDSVT(J)
      EndDo
      Close(UNIT=LUNTMP)
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_vlp'//XEXT)
      Open(UNIT=LUNTMP,FILE=XFILE,IOSTAT=IERR,ERR=999)
*
      Call SAVHDR(LUNTMP,B,'fit window width (meV)',
     +                      'p-wave sound velocity (km/s)')
*
      Do  J = 1,NWW
       Write(LUNTMP,XF3) DWW(J),1.D-3*DSVPT(J),1.D-3*DDSVPT(J)
      EndDo
      Close(UNIT=LUNTMP)
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_vls'//XEXT)
      Open(UNIT=LUNTMP,FILE=XFILE,IOSTAT=IERR,ERR=999)
*
      Call SAVHDR(LUNTMP,B,'fit window width (meV)',
     +                      's-wave sound velocity (km/s)')
*
      Do  J = 1,NWW
       Write(LUNTMP,XF3) DWW(J),1.D-3*DSVST(J),1.D-3*DDSVST(J)
      EndDo
      Close(UNIT=LUNTMP)
*
      Return
*
*:  error exits
*
  999 Continue
      NERR = 3901
      XMSG = XFILE
      MSG1 = IERR
*
*
      End
